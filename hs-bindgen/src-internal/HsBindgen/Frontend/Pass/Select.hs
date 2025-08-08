module HsBindgen.Frontend.Pass.Select (
    selectDecls
  ) where

import Data.List (partition)
import Data.Set (Set)
import Data.Set qualified as Set

import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Pass.Sort.IsPass
import HsBindgen.Frontend.Predicate qualified as Predicate

-- | A declaration directly selected by the selection predicate.
type Root = C.NsPrelimDeclId

-- | A declaration indirectly selected because it is the transitive dependency
-- of a 'Root'.
type TransitiveDependency = C.NsPrelimDeclId

selectDecls ::
     Predicate.IsMainHeader
  -> Predicate.IsInMainHeaderDir
  -> Config Select
  -> C.TranslationUnit ResolveBindingSpec
  -> (C.TranslationUnit Select, [Msg Select])
selectDecls isMainHeader isInMainHeaderDir SelectConfig{..} unitRBS =
    case selectConfigProgramSlicing of
      DisableProgramSlicing ->
        let matchedDecls :: [C.Decl Select]
            matchedDecls = filter matchDecl decls

            selectMsgs :: [Msg Select]
            selectMsgs = map (SelectSelected . C.declInfo) matchedDecls
         in (unitSelect { C.unitDecls = matchedDecls }, selectMsgs)

      EnableProgramSlicing ->
        let matchedDecls, unmatchedDecls :: [C.Decl Select]
            (matchedDecls, unmatchedDecls) = partition matchDecl decls

            selectedRoots :: [Root]
            selectedRoots = map C.declOrigNsPrelimDeclId matchedDecls

            transitiveDeps :: Set TransitiveDependency
            transitiveDeps =
              UseDeclGraph.getTransitiveDeps useDeclGraph selectedRoots

            -- NOTE: Careful, we need to maintain the order of declarations so
            -- that children come before parents.  'filter' does that for us.
            selectedDecls :: [C.Decl Select]
            selectedDecls =
              filter
                ((`Set.member` transitiveDeps) . C.declOrigNsPrelimDeclId)
                decls

            selectMsgs :: [Msg Select]
            selectMsgs =
              getSelectMsgs transitiveDeps selectedDecls unmatchedDecls
        in (unitSelect { C.unitDecls = selectedDecls }, selectMsgs)
  where
    unitSelect :: C.TranslationUnit Select
    unitSelect = coercePass unitRBS

    decls :: [C.Decl Select]
    decls = C.unitDecls unitSelect

    matchDecl :: C.Decl Select -> Bool
    matchDecl decl =
      Predicate.matchSelect
        isMainHeader
        isInMainHeaderDir
        (C.declLoc $ C.declInfo decl)
        (C.declQualDeclId decl)
        selectConfigPredicate

    useDeclGraph :: UseDeclGraph
    useDeclGraph = declUseDecl $ C.unitAnn unitSelect

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

getSelectMsgs
  :: Set C.NsPrelimDeclId
  -> [C.Decl Select]
  -> [C.Decl Select]
  -> [Msg Select]
getSelectMsgs transitiveDeps selectedDecls unmatchedDecls =
    errorMsgs ++ excludeMsgs ++ selectMsgs
  where
    unavailableTransitiveDeps :: Set C.NsPrelimDeclId
    unavailableTransitiveDeps =
      transitiveDeps `Set.difference`
        (Set.fromList $ map C.declOrigNsPrelimDeclId selectedDecls)

    errorMsgs, excludeMsgs, selectMsgs :: [Msg Select]
    errorMsgs = map SelectTransitiveDependencyUnavailable $
      Set.toList unavailableTransitiveDeps
    excludeMsgs = map (SelectExcluded . C.declInfo) unmatchedDecls
    selectMsgs  = map (SelectSelected . C.declInfo) selectedDecls
