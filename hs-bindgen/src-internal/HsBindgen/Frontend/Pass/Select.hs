module HsBindgen.Frontend.Pass.Select (
    selectDecls
  ) where

import Data.Foldable qualified as Foldable
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Clang.HighLevel.Types
import HsBindgen.C.Predicate (IsMainHeader)
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.NonSelectedDecls
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Pass.Sort.IsPass

-- | A declaration directly selected by the selection predicate.
type Root = C.NsPrelimDeclId

-- | A declaration indirectly selected because it is the transitive dependency
-- of a 'Root'.
type TransitiveDependency = C.NsPrelimDeclId

selectDecls ::
     IsMainHeader
  -> Config Select
  -> C.TranslationUnit ResolveBindingSpec
  -> (C.TranslationUnit Select, [Msg Select])
selectDecls isMainHeader SelectConfig{..} unitRBS =
    case selectConfigProgramSlicing of
      DisableProgramSlicing ->
        let matchedDecls :: [C.Decl Select]
            matchedDecls = filter matchDecl decls
         in (unitSelect { C.unitDecls = matchedDecls }, [])

      -- When program slicing is enabled, we select all declarations while
      -- parsing.  Instead, we apply the selection predicate here, and also
      -- select all transitive dependencies.
      EnableProgramSlicing ->
        let matchedDeclarations   :: [C.Decl Select]
            unmatchedDeclarations :: [C.Decl Select]
            (matchedDeclarations, unmatchedDeclarations) =
              partition matchDecl decls

            selectedRoots :: [Root]
            selectedRoots = map C.declOrigNsPrelimDeclId matchedDeclarations

            -- NOTE: We traverse the use-decl graph N times, where N is the
            -- number of roots.  We could track the transitives of multiple
            -- roots in a single traversal.
            --
            -- See issue https://github.com/well-typed/hs-bindgen/issues/517.
            getTransitives :: Root -> (Root, Set TransitiveDependency)
            getTransitives root =
              (root, UseDeclGraph.getTransitiveDeps useDeclGraph root)

            rootToTransitiveDependencies :: [(Root, Set TransitiveDependency)]
            rootToTransitiveDependencies = map getTransitives selectedRoots

            transitiveDependencies :: Set TransitiveDependency
            transitiveDependencies =
              Foldable.foldl'
                (<>)
                Set.empty
                (map snd rootToTransitiveDependencies)

            selectedDeclarationIds :: Set C.NsPrelimDeclId
            selectedDeclarationIds =
              Set.union (Set.fromList selectedRoots) transitiveDependencies

            -- NOTE: Careful, we need to maintain the order of declarations so
            -- that children come before parents.  'partition' does that for us.
            selectedDeclarations, nonSelectedDecls :: [C.Decl Select]
            (selectedDeclarations, nonSelectedDecls) =
                partition
                  ( (`Set.member` selectedDeclarationIds)
                  . C.declOrigNsPrelimDeclId
                  )
                  decls

            nonSelectedDecls' :: NonSelectedDecls
            nonSelectedDecls' = Foldable.foldl' insertNonSelected
              (declNonSelected $ C.unitAnn unitRBS) nonSelectedDecls

            declMeta' :: DeclMeta
            declMeta' =
              (C.unitAnn unitSelect) { declNonSelected = nonSelectedDecls' }

            selectMsgs :: [Msg Select]
            selectMsgs =
              getSelectMsgs
                transitiveDependencies
                selectedDeclarations
                unmatchedDeclarations
                rootToTransitiveDependencies
        in
          ( unitSelect {
                C.unitDecls = selectedDeclarations
              , C.unitAnn = declMeta'
              }
          , selectMsgs )
  where
    unitSelect :: C.TranslationUnit Select
    unitSelect = coercePass unitRBS

    decls :: [C.Decl Select]
    decls = C.unitDecls unitSelect

    matchDecl :: C.Decl Select -> Bool
    matchDecl decl =
      Predicate.matchSelect
        isMainHeader
        (C.declLoc $ C.declInfo decl)
        (C.declQualDeclId decl)
        selectConfigPredicate

    useDeclGraph :: UseDeclGraph
    useDeclGraph = declUseDecl $ C.unitAnn unitSelect

    insertNonSelected :: NonSelectedDecls -> C.Decl Select -> NonSelectedDecls
    insertNonSelected nonSelectedDecls decl =
      insert
        (C.declQualName decl)
        (singleLocPath (C.declLoc (C.declInfo decl)))
        nonSelectedDecls

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

type TransitiveDependencyToRoots = Map TransitiveDependency (Set Root)

getSelectMsgs
  :: Set C.NsPrelimDeclId
  -> [C.Decl Select]
  -> [C.Decl Select]
  -> [(Root, Set TransitiveDependency)]
  -> [Msg Select]
getSelectMsgs transitiveDependencies
              selectedDeclarations
              unmatchedDeclarations
              rootToTransitiveDependencies
  = errorMsgs ++ excludeMsgs ++ selectMsgs
  where
    unavailableTransitiveDeps :: Set C.NsPrelimDeclId
    unavailableTransitiveDeps =
      transitiveDependencies `Set.difference`
        (Set.fromList $ map C.declOrigNsPrelimDeclId selectedDeclarations)

    errorMsgs :: [Msg Select]
    errorMsgs = map SelectTransitiveDependencyUnavailable $
      Set.toList unavailableTransitiveDeps

    excludeMsgs :: [Msg Select]
    excludeMsgs = map (SelectExcluded . C.declInfo) unmatchedDeclarations

    -- We have a map from root to transitive dependencies.  However, to report
    -- why something was selected, we need a map from each transitive dependency
    -- to its roots.
    transitiveDependencyToRoots :: TransitiveDependencyToRoots
    transitiveDependencyToRoots = Foldable.foldl'
      addRootWithTransitiveDependencies Map.empty rootToTransitiveDependencies

    selectMsgs :: [Msg Select]
    selectMsgs = map (SelectSelected . uncurry TransitiveDependencyOf) $
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
