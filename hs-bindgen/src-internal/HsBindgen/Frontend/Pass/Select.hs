module HsBindgen.Frontend.Pass.Select (
    selectDecls
  ) where

import Data.List (partition)
import Data.Set qualified as Set

import Clang.HighLevel.Types

import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Pass.Sort.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.Util.Tracer

selectDecls ::
     IsMainHeader
  -> IsInMainHeaderDir
  -> Config Select
  -> C.TranslationUnit ResolveBindingSpecs
  -> (C.TranslationUnit Select, [Msg Select])
selectDecls isMainHeader isInMainHeaderDir SelectConfig{..} unitRBS =
    let matchedDecls, unmatchedDecls :: [C.Decl Select]
        (matchedDecls, unmatchedDecls) = partition matchDecl decls

        selectedRoots :: [C.QualPrelimDeclId]
        selectedRoots = map C.declOrigQualPrelimDeclId matchedDecls

        transitiveDeps :: Set C.QualPrelimDeclId
        transitiveDeps = case selectConfigProgramSlicing of
          DisableProgramSlicing ->
            Set.empty
          EnableProgramSlicing ->
            UseDeclGraph.getTransitiveDeps useDeclGraph selectedRoots

        selectedDecls :: [C.Decl Select]
        selectedDecls = case selectConfigProgramSlicing of
          DisableProgramSlicing ->
            matchedDecls
          EnableProgramSlicing ->
            -- NOTE: Careful, we need to maintain the order of declarations so
            -- that children come before parents. 'filter' does that for us.
            filter ((`Set.member` transitiveDeps) . C.declOrigQualPrelimDeclId) decls

        selectMsgs :: [Msg Select]
        selectMsgs =
          getSelectMsgs selectedRoots transitiveDeps selectedDecls unmatchedDecls

        -- TODO_PR: Differentiate between required transitive dependencies that
        -- we did not attempt to parse (e.g., parse predicate not matched) or we
        -- have failed to parse; and transitive dependencies we have parsed
        -- successfully, but that have vanished in a subsequent pass and do not
        -- show up in the list of declarations provided to this pass.
        unavailableTransitiveDeps :: Set C.QualPrelimDeclId
        unavailableTransitiveDeps = Set.empty
        -- -- TODO_PR: Old definition:
        --
        -- unavailableTransitiveDeps =
        --   transitiveDeps `Set.difference`
        --     (Set.fromList $ map C.declOrigNsPrelimDeclId selectedDecls)

    in if Set.null unavailableTransitiveDeps
       -- TODO_PR
       -- then ( unitSelect { C.unitDecls = selectedDecls }
       --      , delayedParseMsgs ++ selectMsgs
       --      )
       then ( unitSelect { C.unitDecls = selectedDecls }
            , selectMsgs
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

    unitSelect :: C.TranslationUnit Select
    unitSelect = C.TranslationUnit {
            C.unitDecls = map coercePass unitRBS.unitDecls
          , C.unitIncludeGraph = unitRBS.unitIncludeGraph
          , C.unitAnn = unitRBS.unitAnn
          }

    decls :: [C.Decl Select]
    decls = C.unitDecls unitSelect

    useDeclGraph :: UseDeclGraph
    useDeclGraph = unitRBS.unitAnn.declUseDecl

    matchDecl :: C.Decl Select -> Bool
    matchDecl decl = isSelected $
      match
        (C.declOrigQualPrelimDeclId decl)
        (C.declLoc $ C.declInfo decl)
        (C.declAvailability $ C.declInfo decl)

    match :: C.QualPrelimDeclId -> SingleLoc -> C.Availability -> SelectStatus
    match = \declId -> go declId declId
      where
        -- We compare the use sites of anonymous declarations with the original
        -- @declId@, so we can detect cycles involving anonymous declarations in
        -- the use-decl graph. We believe these cycles can not exist.
        go originalDeclId declId loc availability = case declId of
            C.QualPrelimDeclIdNamed name kind ->
                if matchSelect
                     isMainHeader
                     isInMainHeaderDir
                     (singleLocPath loc)
                     (C.QualName name kind)
                     availability
                     selectConfigPredicate
                then Selected SelectionRoot
                else NotSelected
            -- Apply the select predicate to the use site.
            anon@(C.QualPrelimDeclIdAnon{}) -> matchAnon anon
            -- Never select builtins.
            C.QualPrelimDeclIdBuiltin _ -> NotSelected
          where
            declUseGraph :: DeclUseGraph
            declUseGraph = declDeclUse (C.unitAnn unitRBS)

            index :: DeclIndex
            index = declIndex (C.unitAnn unitRBS)

            matchAnon :: C.QualPrelimDeclId -> SelectStatus
            matchAnon anon =
              case DeclUseGraph.getUseSites declUseGraph anon of
                [x] ->
                  matchUseSite $ fst x
                []  ->
                  panicPure "anonymous declaration without use site"
                xs  ->
                  panicPure $
                    "anonymous declaration with multiple use sites" ++ show xs

            matchUseSite :: C.QualPrelimDeclId -> SelectStatus
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

    -- matchKey :: Key -> SelectStatus
    -- matchKey (ParseMsgKey loc declId declKind declAvailability) =
    --   let qualDeclId = C.QualDeclId {
    --         qualDeclIdName   = C.declIdName declId
    --       , qualDeclIdOrigin = C.declIdOrigin declId
    --       , qualDeclIdKind   = declKind
    --       }
    --   in match (C.qualDeclIdToQualPrelimDeclId qualDeclId) loc declAvailability


{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

getSelectMsgs
  :: [C.QualPrelimDeclId]
  -> Set C.QualPrelimDeclId
  -> [C.Decl Select]
  -> [C.Decl Select]
  -> [Msg Select]
getSelectMsgs selectedRootsIds transitiveDeps selectedDecls unmatchedDecls =
    (excludeMsgs ++ selectRootMsgs ++ selectTransMsgs)
  where
    unselectedDecls :: [C.Decl Select]
    unselectedDecls =
      filter
        ((`Set.notMember` transitiveDeps) . C.declOrigQualPrelimDeclId)
        unmatchedDecls

    isRoot :: C.Decl Select -> Bool
    isRoot x = Set.member (C.declOrigQualPrelimDeclId x) (Set.fromList selectedRootsIds)

    -- | Strict transitive dependencies are not selection roots.
    selectedRoots, transitiveDepsStrict :: [C.Decl Select]
    (selectedRoots, transitiveDepsStrict) = partition isRoot selectedDecls

    excludeMsgs, selectRootMsgs, selectTransMsgs :: [Msg Select]
    excludeMsgs =
      map (SelectSelectStatus NotSelected . C.declInfo) unselectedDecls
    selectRootMsgs =
      map (SelectSelectStatus (Selected SelectionRoot) . C.declInfo) selectedRoots
    selectTransMsgs =
      map (SelectSelectStatus (Selected TransitiveDependency) . C.declInfo) transitiveDepsStrict

    -- TODO_PR.
-- getDelayedParseMsgs ::
--      SortParseStatus
--   -> (SingleLoc -> C.QualDeclId -> C.Availability -> SelectStatus)
--   -> Map (C.QualName) [Msg Select]
-- getDelayedParseMsgs parseStatus match =
--   Map.mapWithKey getMsgs $ unNameAnonParseStatus parseStatus
--   where
--     getMsgs :: C.QualName -> (C.NameOrigin, ParseStatusValue) -> [Msg Select]
--     getMsgs qualName (origin, ParseStatusValue{..} ) =
--       let qualDeclId = C.QualDeclId {
--               qualDeclIdName   = C.qualNameName qualName
--             , qualDeclIdOrigin = origin
--             , qualDeclIdKind   = C.qualNameKind qualName
--             }
--       in case match psvSingleLoc qualDeclId psvAvailability of
--         True  -> case psvDeclStatus of
--           ParseNotAttempted r ->
--             [ SelectParseNotAttempted qualName origin psvSingleLoc r ]
--           ParseSucceeded msgs ->
--             [ SelectParse qualName origin psvSingleLoc m | m <- msgs ]
--           ParseFailed (msg :| msgs) ->
--             [ SelectParseFailed qualName origin psvSingleLoc m | m<- msg : msgs ]
--         False -> []

    -- TODO_PR.
    -- -- The set of desired selected keys is a super-set of the set of actually
    -- -- selected keys, in which some keys may be missing because parsing has
    -- -- failed.
    -- desiredSelectedKeys :: Set Key
    -- desiredSelectedKeys = Set.filter match allKeys

    -- -- Failed declarations are declarations we desired to select, but that we
    -- -- failed to reify during parse/reification. See the documentation of
    -- -- 'ParseMsgs'.
    -- failedKeys :: Set Key
    -- failedKeys = desiredSelectedKeys Set.\\ selectedKeys

    -- selectedMsgs :: Map Key [DelayedParseMsg]
    -- selectedMsgs = Map.restrictKeys msgs selectedKeys

    -- failedMsgs :: Map Key [DelayedParseMsg]
    -- failedMsgs = Map.restrictKeys msgs failedKeys

isSelected :: SelectStatus -> Bool
isSelected NotSelected        = False
isSelected (Selected _reason) = True
