module HsBindgen.Frontend.Pass.Slice (
    sliceDecls
  ) where

import Data.Foldable qualified as Foldable
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Clang.HighLevel.Types
import HsBindgen.C.Predicate
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.NonSelectedDecls
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Type.PrelimDeclId
import HsBindgen.Frontend.Pass.Slice.IsPass
import HsBindgen.Frontend.Pass.Sort.IsPass
import HsBindgen.Language.C qualified as C

-- | A declaration directly selected by the selection predicate.
type Root = NsPrelimDeclId

-- | A declaration indirectly selected because it is the transitive dependency
-- of a 'Root'.
type TransitiveDependency = NsPrelimDeclId

sliceDecls ::
     IsMainFile
  -> Config Slice
  -> C.TranslationUnit Sort
  -> (C.TranslationUnit Slice, [Msg Slice])
sliceDecls isMainFile SliceConfig{..} unitSort = case sliceConfigProgramSlicing of
  DisableProgramSlicing -> (unitSlice, [])
  -- When program slicing is enabled, we select all declarations while parsing.
  -- Instead, we apply the selection predicate here, and also select all
  -- transitive dependencies.
  EnableProgramSlicing ->
    let useDeclGraph :: UseDeclGraph
        useDeclGraph = declUseDecl $ C.unitAnn $ unitSlice

        -- All parsed declarations.
        decls :: [C.Decl Slice]
        decls = C.unitDecls unitSlice

        matchDecl :: C.Decl Slice -> Bool
        matchDecl decl = match isMainFile loc qid sliceConfigPredicate
          where
            loc :: SingleLoc
            loc = C.declLoc $ C.declInfo decl

            qid :: QualPrelimDeclId
            qid = declQualPrelimDeclId decl

        matchedDeclarations   :: [C.Decl Slice]
        unmatchedDeclarations :: [C.Decl Slice]
        (matchedDeclarations, unmatchedDeclarations) = partition matchDecl decls

        selectedRoots :: [Root]
        selectedRoots = map declNsPrelimDeclId matchedDeclarations

        -- NOTE: We traverse the use-decl graph N times, where N is the number
        -- of roots. We could track the transitives of multiple roots in a
        -- single traversal.
        --
        -- See issue https://github.com/well-typed/hs-bindgen/issues/517.
        getTransitives :: Root -> (Root, Set TransitiveDependency)
        getTransitives root = (root, UseDeclGraph.getTransitiveDeps useDeclGraph root)

        rootToTransitiveDependencies :: [(Root, Set TransitiveDependency)]
        rootToTransitiveDependencies = map getTransitives selectedRoots

        transitiveDependencies :: Set TransitiveDependency
        transitiveDependencies = Foldable.foldl'
          (<>) Set.empty (map snd rootToTransitiveDependencies)

        selectedDeclarationIds :: Set NsPrelimDeclId
        selectedDeclarationIds = Set.union
                                 (Set.fromList selectedRoots)
                                 transitiveDependencies

        -- NOTE: Careful, we need to maintain the order of declarations so that
        -- children come before parents. 'partition' does that for us.
        selectedDeclarations, nonSelectedDecls :: [C.Decl Slice]
        (selectedDeclarations, nonSelectedDecls) =
            partition
              ((`Set.member` selectedDeclarationIds) . declNsPrelimDeclId)
              decls

        nonSelectedDecls' :: NonSelectedDecls
        nonSelectedDecls' = Foldable.foldl' insertNonSelected
          (declNonSelected $ C.unitAnn unitSort) nonSelectedDecls

        declMeta' :: DeclMeta
        declMeta' = (C.unitAnn unitSlice) { declNonSelected = nonSelectedDecls'}

        sliceMsgs :: [Msg Slice]
        sliceMsgs = getSliceMsgs
                      transitiveDependencies
                      selectedDeclarations
                      unmatchedDeclarations
                      rootToTransitiveDependencies
     in
      ( unitSlice {
            C.unitDecls = selectedDeclarations
          , C.unitAnn = declMeta'
          }
      , sliceMsgs )
  where
    unitSlice :: C.TranslationUnit Slice
    unitSlice = coercePass unitSort

    insertNonSelected :: NonSelectedDecls -> C.Decl Slice -> NonSelectedDecls
    insertNonSelected nonSelectedDecls decl = case declQualPrelimDeclId decl of
      QualPrelimDeclIdNamed name kind ->
        insert
          (C.QualName name kind)
          (singleLocPath (C.declLoc (C.declInfo decl)))
          nonSelectedDecls
      -- Refer to 'recordNonSelectedDecl'.
      QualPrelimDeclIdAnon{}    -> nonSelectedDecls
      QualPrelimDeclIdBuiltin{} -> nonSelectedDecls

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

type TransitiveDependencyToRoots = Map TransitiveDependency (Set Root)

getSliceMsgs
  :: Set NsPrelimDeclId
  -> [C.Decl Slice]
  -> [C.Decl Slice]
  -> [(Root, Set TransitiveDependency)]
  -> [Msg Slice]
getSliceMsgs transitiveDependencies
             selectedDeclarations
             unmatchedDeclarations
             rootToTransitiveDependencies
  = errorMsgs ++ skipMsgs ++ selectMsgs
  where
    unavailableTransitiveDeps :: Set NsPrelimDeclId
    unavailableTransitiveDeps =
      transitiveDependencies `Set.difference`
        (Set.fromList $ map declNsPrelimDeclId selectedDeclarations)

    errorMsgs :: [Msg Slice]
    errorMsgs = map TransitiveDependencyUnavailable $
      Set.toList unavailableTransitiveDeps

    skipMsgs :: [Msg Slice]
    skipMsgs = map (Skipped . C.declInfo) unmatchedDeclarations

    -- We have a map from root to transitive dependencies. However, to report why
    -- something was selected, we need a map from each transitive dependency to its
    -- roots.
    transitiveDependencyToRoots :: TransitiveDependencyToRoots
    transitiveDependencyToRoots = Foldable.foldl'
      addRootWithTransitiveDependencies Map.empty rootToTransitiveDependencies

    selectMsgs :: [Msg Slice]
    selectMsgs = map (Selected . uncurry TransitiveDependencyOf) $
     Map.toList transitiveDependencyToRoots

addRootWithTransitiveDependencies ::
     TransitiveDependencyToRoots
  -> (Root, Set TransitiveDependency)
  -> TransitiveDependencyToRoots
addRootWithTransitiveDependencies mp (root, transitiveDeps) =
  Foldable.foldl' addTransitiveDependency mp transitiveDeps
  where addTransitiveDependency
          :: TransitiveDependencyToRoots
          -> TransitiveDependency
          -> TransitiveDependencyToRoots
        addTransitiveDependency m transitiveDep
          -- Do not add the root itself as transitive dependency because it
          -- leads to weird trace messages:
          --
          -- > Selected X because it is a transitive dependency of X.
          | root == transitiveDep = m
          | otherwise             = Map.alter addRoot transitiveDep m

        addRoot :: Maybe (Set Root) -> Maybe (Set Root)
        addRoot Nothing      = Just $ Set.singleton root
        addRoot (Just roots) = Just $ Set.insert root roots
