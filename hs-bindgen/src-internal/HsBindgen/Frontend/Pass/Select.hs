module HsBindgen.Frontend.Pass.Select (
    selectDecls
  ) where

import Data.List (partition)
import Data.Map qualified as Map
import Data.Set qualified as Set

import Clang.HighLevel.Types

import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.External (QualDeclId (qualDeclIdOrigin))
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Pass.Sort.IsPass
import HsBindgen.Frontend.Predicate qualified as Predicate
import HsBindgen.Imports
import HsBindgen.Util.Tracer

selectDecls ::
     Predicate.IsMainHeader
  -> Predicate.IsInMainHeaderDir
  -> Config Select
  -> C.TranslationUnit ResolveBindingSpec
  -> (C.TranslationUnit Select, [Msg Select])
selectDecls isMainHeader isInMainHeaderDir SelectConfig{..} unitRBS =
    let matchedDecls, unmatchedDecls :: [C.Decl Select]
        (matchedDecls, unmatchedDecls) = partition matchDecl decls

        selectedRoots :: [C.NsPrelimDeclId]
        selectedRoots = map C.declOrigNsPrelimDeclId matchedDecls

        transitiveDeps :: Set C.NsPrelimDeclId
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
            filter ((`Set.member` transitiveDeps) . C.declOrigNsPrelimDeclId) decls

        parseMsgs :: [Msg Select]
        parseMsgs = getDelayedParseMsgs' selectedDecls

        selectMsgs :: [Msg Select]
        unavailableTransitiveDeps :: Set C.NsPrelimDeclId
        (selectMsgs, unavailableTransitiveDeps) =
          getSelectMsgs selectedRoots transitiveDeps selectedDecls unmatchedDecls

    in if Set.null unavailableTransitiveDeps
       then ( unitSelect { C.unitDecls = selectedDecls }
            , parseMsgs ++ selectMsgs
            )
       else panicPure $ errorMsgWith unavailableTransitiveDeps
  where
    errorMsgWith :: Set C.NsPrelimDeclId -> String
    errorMsgWith xs = unlines $
        "Unavailable transitive dependencies: "
      : map (show . prettyForTrace) (Set.toList xs)
      ++ [
           "This is an indication that declarations have been removed after parse."
         , "Please remove unsupported declarations in the parse pass, and not later!"
         ]

    unitSelect :: C.TranslationUnit Select
    unitSelect =
      let C.TranslationUnit{..} = unitRBS
      in  C.TranslationUnit {
            C.unitDecls = map coercePass unitDecls
          , C.unitIncludeGraph = unitIncludeGraph
          , C.unitAnn = SelectDeclMeta {
                selectDeclIndex     = declIndex unitAnn
              , selectDeclUseDecl   = declUseDecl unitAnn
              , selectDeclDeclUse   = declDeclUse unitAnn
              , selectDeclNonParsed = declNonParsed unitAnn
              }
          }

    decls :: [C.Decl Select]
    decls = C.unitDecls unitSelect

    ann :: DeclMeta ResolveBindingSpec
    ann = C.unitAnn unitRBS

    useDeclGraph :: UseDeclGraph
    useDeclGraph = declUseDecl ann

    match :: SingleLoc -> C.QualDeclId -> C.Availability -> Bool
    match loc qualDeclId availability =
      Predicate.matchSelect
        isMainHeader
        isInMainHeaderDir
        (singleLocPath loc)
        qualDeclId
        availability
        selectConfigPredicate

    matchDecl :: C.Decl Select -> Bool
    matchDecl decl =
      match
        (C.declLoc $ C.declInfo decl)
        (C.declQualDeclId decl)
        (C.declAvailability $ C.declInfo decl)

    matchKey :: Key -> Bool
    matchKey (ParseMsgKey loc declId declKind declAvailability) =
      let qualDeclId = C.QualDeclId {
            qualDeclIdName   = C.declIdName declId
          , qualDeclIdOrigin = C.declIdOrigin declId
          , qualDeclIdKind   = declKind
          }
      in match loc qualDeclId declAvailability

    getDelayedParseMsgs' :: [C.Decl Select] -> [Msg Select]
    getDelayedParseMsgs' = getDelayedParseMsgs ann matchKey

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

getSelectMsgs
  :: [C.NsPrelimDeclId]
  -> Set C.NsPrelimDeclId
  -> [C.Decl Select]
  -> [C.Decl Select]
  -> ([Msg Select], Set C.NsPrelimDeclId)
getSelectMsgs selectedRootsIds transitiveDeps selectedDecls unmatchedDecls =
    (excludeMsgs ++ selectRootMsgs ++ selectTransMsgs, unavailableTransitiveDeps)
  where
    unavailableTransitiveDeps :: Set C.NsPrelimDeclId
    unavailableTransitiveDeps =
      transitiveDeps `Set.difference`
        (Set.fromList $ map C.declOrigNsPrelimDeclId selectedDecls)

    unselectedDecls :: [C.Decl Select]
    unselectedDecls =
      filter
        ((`Set.notMember` transitiveDeps) . C.declOrigNsPrelimDeclId)
        unmatchedDecls

    isRoot :: C.Decl Select -> Bool
    isRoot x = Set.member (C.declOrigNsPrelimDeclId x) (Set.fromList selectedRootsIds)

    -- | Strict transitive dependencies are not selection roots.
    selectedRoots, transitiveDepsStrict :: [C.Decl Select]
    (selectedRoots, transitiveDepsStrict) = partition isRoot selectedDecls

    excludeMsgs, selectRootMsgs, selectTransMsgs :: [Msg Select]
    excludeMsgs =
      map (SelectNotSelected                   . C.declInfo) unselectedDecls
    selectRootMsgs =
      map (SelectSelected SelectionRoot        . C.declInfo) selectedRoots
    selectTransMsgs =
      map (SelectSelected TransitiveDependency . C.declInfo) transitiveDepsStrict

type Key = ParseMsgKey Select

getDelayedParseMsgs ::
     DeclMeta ResolveBindingSpec
  -> (ParseMsgKey Select -> Bool)
  -> [C.Decl Select]
  -> [Msg Select]
getDelayedParseMsgs meta match decls =
        map (uncurry SelectParse)  (flatten selectedMsgs)
     ++ map (uncurry SelectFailed) (flatten failedMsgs)
  where
    msgs :: Map Key [DelayedParseMsg]
    msgs = unParseMsgs $ coerceParseMsgs $ declParseMsgs meta

    allKeys :: Set Key
    allKeys = Map.keysSet msgs

    declToKey :: C.Decl Select -> Key
    declToKey C.Decl{declInfo, declKind} =
      ParseMsgKey
        (C.declLoc declInfo)
        (C.declId declInfo)
        (C.declKindNameKind declKind)
        (C.declAvailability declInfo)

    selectedKeys :: Set Key
    selectedKeys = Set.fromList $ map declToKey decls

    -- The set of desired selected keys is a super-set of the set of actually
    -- selected keys, in which some keys may be missing because parsing has
    -- failed.
    desiredSelectedKeys :: Set Key
    desiredSelectedKeys = Set.filter match allKeys

    -- Failed declarations are declarations we desired to select, but that we
    -- failed to reify during parse/reification. See the documentation of
    -- 'ParseMsgs'.
    failedKeys :: Set Key
    failedKeys = desiredSelectedKeys Set.\\ selectedKeys

    selectedMsgs :: Map Key [DelayedParseMsg]
    selectedMsgs = Map.restrictKeys msgs selectedKeys

    failedMsgs :: Map Key [DelayedParseMsg]
    failedMsgs = Map.restrictKeys msgs failedKeys

    flatten :: Map a [b] -> [(a, b)]
    flatten = concatMap (\(k, xs) -> map (k,) xs) . Map.toList
