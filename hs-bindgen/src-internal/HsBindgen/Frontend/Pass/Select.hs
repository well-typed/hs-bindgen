module HsBindgen.Frontend.Pass.Select (
    selectDecls
  ) where

import Data.Foldable qualified as Foldable
import Data.List (partition)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set

import Clang.HighLevel.Types

import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.Util.Tracer

type MatchFun = C.QualPrelimDeclId -> SingleLoc -> C.Availability -> Bool

selectDecls ::
     IsMainHeader
  -> IsInMainHeaderDir
  -> Set C.QualName
  -> Config Select
  -> C.TranslationUnit ResolveBindingSpecs
  -> (C.TranslationUnit Select, [Msg Select])
selectDecls
  isMainHeader
  isInMainHeaderDir
  declsWithExternalBindingSpec
  SelectConfig{..}
  unit =
    let matchedDecls, unmatchedDecls :: [C.Decl Select]
        (matchedDecls, unmatchedDecls) = partition matchDecl decls

        selectedRoots :: [C.QualPrelimDeclId]
        selectedRoots = map C.declOrigQualPrelimDeclId matchedDecls

        transitiveDeps :: Set C.QualPrelimDeclId
        transitiveDeps = case selectConfigProgramSlicing of
          DisableProgramSlicing ->
            Set.empty
          EnableProgramSlicing ->
            UseDeclGraph.getTransitiveDeps unit.unitAnn.declUseDecl selectedRoots

        selectedDecls :: [C.Decl Select]
        selectedDecls = case selectConfigProgramSlicing of
          DisableProgramSlicing ->
            matchedDecls
          EnableProgramSlicing ->
            -- NOTE: Careful, we need to maintain the order of declarations so
            -- that children come before parents. 'filter' does that for us.
            filter ((`Set.member` transitiveDeps) . C.declOrigQualPrelimDeclId) decls

        -- TODO_PR: Differentiate between required transitive dependencies that
        -- we did not attempt to parse (e.g., parse predicate not matched) or we
        -- have failed to parse; and transitive dependencies we have parsed
        -- successfully, but that have vanished in a subsequent pass and do not
        -- show up in the list of declarations provided to this pass.
        unavailableTransitiveDeps :: Set C.QualPrelimDeclId
        unavailableTransitiveDeps = Set.empty
        -- unavailableTransitiveDeps =
        --   transitiveDeps `Set.difference`
        --     (Set.fromList $ map C.declOrigNsPrelimDeclId selectedDecls)

    in if Set.null unavailableTransitiveDeps
       then ( unitSelectWith selectedDecls
            ,    getSelectMsgs            selectedRoots  selectedDecls
              ++ getExcludeMsgs           transitiveDeps unmatchedDecls
              ++ getDelayedParseMsgs      selectedDecls  index
              ++ getParseNotAttemptedMsgs match          index
              ++ getParseFailedMsgs       match          index
            )
       else panicPure $ errorMsgWith unavailableTransitiveDeps
  where
    errorMsgWith :: Set C.QualPrelimDeclId -> String
    errorMsgWith xs = unlines $
        "Unavailable transitive dependencies: "
      : map (show . prettyForTrace) (Set.toList xs)
      ++ [
           "This is an indication that declarations have been removed after parse."
         , "Please remove unsupported declarations in the parse pass, and not later!"
         ]

    decls :: [C.Decl Select]
    decls = map coercePass unit.unitDecls

    index :: DeclIndex
    index = unit.unitAnn.declIndex

    unitSelectWith :: [C.Decl Select] -> C.TranslationUnit Select
    unitSelectWith xs = C.TranslationUnit {
            C.unitDecls = xs
          , C.unitIncludeGraph = unit.unitIncludeGraph
          , C.unitAnn = unit.unitAnn
          }

    matchDecl :: Id p ~ C.DeclId => C.Decl p -> Bool
    matchDecl decl =
      match
        (C.declOrigQualPrelimDeclId decl)
        (C.declLoc $ C.declInfo decl)
        (C.declAvailability $ C.declInfo decl)

    match :: MatchFun
    match = \declId -> go declId declId
      where
        -- We compare the use sites of anonymous declarations with the original
        -- @declId@, so we can detect cycles involving anonymous declarations in
        -- the use-decl graph. We believe these cycles can not exist.
        go originalDeclId declId loc availability = case declId of
            C.QualPrelimDeclIdNamed name kind ->
              matchSelect
                isMainHeader
                isInMainHeaderDir
                (singleLocPath loc)
                (C.QualName name kind)
                availability
                selectConfigPredicate
            -- Apply the select predicate to the use site.
            anon@(C.QualPrelimDeclIdAnon{}) -> matchAnon anon
            -- Never select builtins.
            C.QualPrelimDeclIdBuiltin _ -> False
          where
            matchAnon :: C.QualPrelimDeclId -> Bool
            matchAnon anon =
              case DeclUseGraph.getUseSites unit.unitAnn.declDeclUse anon of
                [x] ->
                  matchUseSite $ fst x
                []  ->
                  -- Unused anonymous declarations are removed in the @NameAnon@
                  -- pass. Here we are using the decl-use graph to find use
                  -- sites, and so we still can encounter unused anonymous
                  -- declarations.
                  False
                xs  ->
                  panicPure $
                    "anonymous declaration with multiple use sites: "
                    ++ show anon ++ " used by " ++ show xs

            matchUseSite :: C.QualPrelimDeclId -> Bool
            matchUseSite declIdUseSite
              | declIdUseSite == originalDeclId =
                  panicPure $
                    "unexpected cycle involving anonymous declaration: "
                      ++ show originalDeclId
              | otherwise =
              case DeclIndex.lookup declIdUseSite index of
                Nothing   -> panicPure "did not find declaration"
                Just decl -> match
                               declIdUseSite
                               (C.declLoc $ C.declInfo decl)
                               (C.declAvailability $ C.declInfo decl)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

getSelectMsgs
  :: [C.QualPrelimDeclId]
  -> [C.Decl Select]
  -> [Msg Select]
getSelectMsgs selectedRootsIds selectedDecls = selectMsgs
  where
    isRoot :: C.Decl Select -> Bool
    isRoot x =
      Set.member (C.declOrigQualPrelimDeclId x) (Set.fromList selectedRootsIds)

    selectMsgs :: [Msg Select]
    selectMsgs = Foldable.foldl' addMsgs [] selectedDecls

    addMsgs :: [Msg Select] -> C.Decl Select -> [Msg Select]
    addMsgs msgs decl =
      let info         = decl.declInfo
          selectReason = if isRoot decl then SelectionRoot else TransitiveDependency
          selectDepr   = [ SelectDeprecated info | isDeprecated info ]
      in  SelectSelectStatus (Selected selectReason) info : selectDepr ++ msgs

    isDeprecated :: C.DeclInfo Select -> Bool
    isDeprecated info = case C.declAvailability info of
      C.Deprecated -> True
      _            -> False

getExcludeMsgs ::
     Set C.QualPrelimDeclId
  -> [C.Decl Select]
  -> [Msg Select]
getExcludeMsgs transitiveDeps unmatchedDecls = excludeMsgs
  where
    unselectedDecls :: [C.Decl Select]
    unselectedDecls =
      filter
        ((`Set.notMember` transitiveDeps) . C.declOrigQualPrelimDeclId)
        unmatchedDecls

    excludeMsgs :: [Msg Select]
    excludeMsgs =
      map (SelectSelectStatus NotSelected . C.declInfo) unselectedDecls

getDelayedParseMsgs :: [C.Decl Select] -> DeclIndex -> [Msg Select]
getDelayedParseMsgs selectedDecls index =
  concatMap getMsgs selectedDecls
  where
    getMsgs :: C.Decl Select -> [Msg Select]
    getMsgs decl = map (toSelectMsg decl)
      $ DeclIndex.lookupDelayedParseMsgs (C.declOrigQualPrelimDeclId decl) index

    toSelectMsg :: C.Decl Select -> DelayedParseMsg -> Msg Select
    toSelectMsg decl = SelectParse decl.declInfo

getParseNotAttemptedMsgs :: MatchFun -> DeclIndex -> [Msg Select]
getParseNotAttemptedMsgs match =
  Foldable.foldl' (Foldable.foldl' addMsg) []
    . DeclIndex.getParseOmissions
  where
    addMsg ::
         [SelectMsg]
      -> ParseOmission
      -> [SelectMsg]
    addMsg xs (ParseOmission i l a r) =
      [ SelectParseNotAttempted i l r
      | match i l a
      ] ++ xs

getParseFailedMsgs :: MatchFun -> DeclIndex -> [Msg Select]
getParseFailedMsgs match =
  Foldable.foldl' (Foldable.foldl' addMsg) []
    . DeclIndex.getParseFailures
  where
    addMsg ::
         [SelectMsg]
      -> ParseFailure
      -> [SelectMsg]
    addMsg xs (ParseFailure i l a msgs) =
      [ SelectParseFailed i l msg
      | match i l a
      , msg <- NonEmpty.toList msgs
      ] ++ xs
