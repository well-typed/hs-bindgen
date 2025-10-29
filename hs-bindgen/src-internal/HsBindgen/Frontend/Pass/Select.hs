module HsBindgen.Frontend.Pass.Select (
    selectDecls
  ) where

import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set ((\\))
import Data.Set qualified as Set

import Clang.HighLevel.Types

import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex (..))
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
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

-- Identifier of declaration.
type DeclId = C.QualPrelimDeclId

-- Declaration itself.
type Decl = C.Decl Select

-- Match function to find selection roots.
type Match = DeclId -> SingleLoc -> C.Availability -> Bool

-- | We have to treat with two notions of availability here:
--
-- 1. A declaration can be available because it is in the list of declarations
--    in the translation unit.
--
-- 2. A declaration is available if the declaration itself and all of its
--    transitive dependencies are available.
--
-- @TransitiveAvailability@ deals with the second type.
data TransitiveAvailability =
    TransitivelyAvailable
  | TransitivelyUnavailable UnavailabilityReason

selectDecls ::
     IsMainHeader
  -> IsInMainHeaderDir
  -> Set C.Name
  -> Set C.QualName
  -> Config Select
  -> C.TranslationUnit ResolveBindingSpecs
  -> (C.TranslationUnit Select, [Msg Select])
selectDecls
  isMainHeader
  isInMainHeaderDir
  failedMacros
  declsWithExternalBindingSpecs
  SelectConfig{..}
  unit =
    let -- Identifiers of selection roots.
        rootIds :: Set DeclId
        rootIds = Foldable.foldl' addMatch Set.empty index.succeeded
          where
           addMatch :: Set DeclId -> ParseSuccess -> Set DeclId
           addMatch xs (ParseSuccess decl _) =
             let info = decl.declInfo
                 qualPrelimDeclId = C.declQualPrelimDeclId decl
                 isSelected = match qualPrelimDeclId info.declLoc info.declAvailability
             in  if isSelected then
                   Set.insert qualPrelimDeclId xs
                 else xs

        -- Identifiers of transitive dependencies (without roots), and all
        -- selected declarations.
        transIds, selectedIds :: Set DeclId
        (transIds, selectedIds) = case selectConfigProgramSlicing of
          DisableProgramSlicing ->
            (Set.empty, rootIds)
          EnableProgramSlicing ->
            let rootAndTransIds =
                  UseDeclGraph.getTransitiveDeps useDeclGraph $
                    Set.toList rootIds
            in  (rootAndTransIds \\ rootIds, rootAndTransIds)

        -- Fold available declarations and collect selected declarations and
        -- trace messages.
        Acc {
            selDecls = selDeclsReversed
          , tIds     = unavailableTransIds
          , msgs     = selectStatusMsgs
          } =
          foldDecls
            getTransitiveAvailability
            rootIds
            transIds
            availableDecls

        unitSelect :: C.TranslationUnit Select
        unitSelect = C.TranslationUnit {
                C.unitDecls = reverse selDeclsReversed
              , C.unitIncludeGraph = unit.unitIncludeGraph
              , C.unitAnn = unit.unitAnn
              }

    in (    unitSelect
       ,    selectStatusMsgs
         ++ getUnavailableTransMsgs  unavailableTransIds
         ++ getDelayedParseMsgs      selectedIds index
         ++ getParseNotAttemptedMsgs match isIgnored index
         ++ getParseFailureMsgs      match isIgnored index
       )
  where
    availableDecls :: [Decl]
    availableDecls = map coercePass unit.unitDecls

    -- Identifiers of declarations that are unavailable because we did not
    -- attempt to parse them or because they depend on declarations we did not
    -- attempt to parse.
    transitivelyUnavailableIdsDueToNotAttemptedParses :: Set DeclId
    transitivelyUnavailableIdsDueToNotAttemptedParses =
        DeclUseGraph.getUseSitesTransitively
          declUseGraphWithoutIgnoredDecls
          (Set.toList $ Map.keysSet index.notAttempted)

    -- Identifiers of declarations that are unavailable because we failed to
    -- parse them or because they depend on declarations we failed to parse.
    transitivelyUnavailableIdsDueToFailedParses :: Set DeclId
    transitivelyUnavailableIdsDueToFailedParses =
        DeclUseGraph.getUseSitesTransitively
          declUseGraphWithoutIgnoredDecls
          (Set.toList $ Map.keysSet index.failed)

    getTransitiveAvailability :: C.QualPrelimDeclId -> TransitiveAvailability
    getTransitiveAvailability declId =
      case ( Set.member declId transitivelyUnavailableIdsDueToNotAttemptedParses
           , Set.member declId transitivelyUnavailableIdsDueToFailedParses ) of
        (False, False) -> TransitivelyAvailable
        (True,  False) -> TransitivelyUnavailable UnavailableParseNotAttempted
        (_,     True)  -> TransitivelyUnavailable UnavailableParseFailed

    -- TODO_PR: Remove.
    hasExternalBindingSpec :: DeclId -> Bool
    hasExternalBindingSpec = \case
      C.QualPrelimDeclIdNamed n k ->
        Set.member (C.QualName n k) declsWithExternalBindingSpecs
      _otherwise -> False

    -- TODO_PR: Remove this and check failing golden tests.
    isFailedMacro :: DeclId -> Bool
    isFailedMacro = \case
      C.QualPrelimDeclIdNamed n C.NameKindOrdinary ->
        Set.member n failedMacros
      _otherwise -> False

    -- TODO_PR: We have to determine if a declaration we determine to be
    -- transitively unavailable can actually be ignored because it is handled by
    -- other means (e.g., binding specifications, or it is a macro that we have
    -- failed to parse).
    isIgnored :: DeclId -> Bool
    isIgnored x = hasExternalBindingSpec x || isFailedMacro x

    getUnavailableTransMsgs :: Set DeclId -> [Msg Select]
    getUnavailableTransMsgs =
      map TransitiveDependencyUnavailable
        . Set.toList
        . Set.filter (not . isIgnored)

    index :: DeclIndex
    index = unit.unitAnn.declIndex

    declUseGraph :: DeclUseGraph
    declUseGraph = unit.unitAnn.declDeclUse

    declUseGraphWithoutIgnoredDecls :: DeclUseGraph
    declUseGraphWithoutIgnoredDecls =
      DeclUseGraph.filterNodes (not . isIgnored) declUseGraph

    useDeclGraph :: UseDeclGraph
    useDeclGraph = unit.unitAnn.declUseDecl

    match :: Match
    match = \declId -> go declId declId
      where
        -- We compare the use sites of anonymous declarations with the original
        -- @declId@, so we can detect cycles involving anonymous declarations in
        -- the use-decl graph. We believe these cycles can not exist.
        go origDeclId declId loc availability = case declId of
            C.QualPrelimDeclIdNamed name kind ->
              matchSelect
                isMainHeader
                isInMainHeaderDir
                (singleLocPath loc)
                (C.QualName name kind)
                availability
                selectConfigPredicate
            -- Apply the select predicate to the use site.
            anon@(C.QualPrelimDeclIdAnon{}) -> matchAnon origDeclId anon
            -- Never select builtins.
            C.QualPrelimDeclIdBuiltin _ -> False

        matchAnon :: DeclId -> DeclId -> Bool
        matchAnon origDeclId anon =
          case DeclUseGraph.getUseSites unit.unitAnn.declDeclUse anon of
            [(declId, _)] -> matchUseSite origDeclId declId
            -- Unused anonymous declarations are removed in the @NameAnon@
            -- pass. Here we are using the decl-use graph to find use sites,
            -- and so we still can encounter unused anonymous declarations.
            []            -> False
            xs            -> panicPure $
              "anonymous declaration with multiple use sites: "
              ++ show anon ++ " used by " ++ show xs

        matchUseSite :: DeclId -> DeclId -> Bool
        matchUseSite origDeclId declIdUseSite
          | declIdUseSite == origDeclId = panicPure $
              "unexpected cycle involving anonymous declaration: "
              ++ show origDeclId
          | otherwise =
          case DeclIndex.lookup declIdUseSite index of
            Nothing   -> panicPure "did not find declaration"
            Just decl -> match
                           declIdUseSite
                           (C.declLoc $ C.declInfo decl)
                           (C.declAvailability $ C.declInfo decl)

{-------------------------------------------------------------------------------
  Fold declarations
-------------------------------------------------------------------------------}

data Acc = Acc
  { -- | Accumulated selected declarations that are transitively available.
    selDecls :: [Decl],
    -- TODO_PR: After fixing the DeclIndex etc., this should be empty.
    --
    -- Track the full set of declaration ids (roots and strict transitives).
    --
    -- | Identifiers of transitive dependencies yet to be selected. After the
    -- fold, this set contains identifiers of transitive dependencies that were
    -- _not_ found in the list of declarations of the translation unit. These
    -- declarations are _unvailable_, and may lead to error traces if they are
    -- not handled by other means (e.g., by binding specifications).
    tIds :: Set DeclId,
    -- Accumulated trace messages.
    msgs :: [Msg Select]
  }

-- | Fold over the list of declarations in the translation unit (see @Acc@).
foldDecls ::
    (DeclId -> TransitiveAvailability)
  -> Set DeclId
  -> Set DeclId
  -> [Decl]
  -> Acc
foldDecls getTransitiveAvailability rootIds transIds decls =
    Foldable.foldl' aux (Acc [] transIds []) decls
  where
    aux :: Acc -> Decl -> Acc
    aux Acc{..} decl =
      let declId    = C.declOrigQualPrelimDeclId decl
          selDecls' = decl : selDecls
          -- We check three conditions:
          isSelectionRoot        = Set.member declId rootIds
          isTransitiveDependency = lookupAndDelete declId tIds
          transitiveAvailability = getTransitiveAvailability declId
      in
      case ( isSelectionRoot
           , isTransitiveDependency
           , transitiveAvailability ) of
        -- Declaration is a selection root.
        (True, Nothing, TransitivelyAvailable) ->
           Acc selDecls' tIds (getSelMsgs SelectionRoot decl ++ msgs)
        (True, Nothing, TransitivelyUnavailable r) ->
          let msg = TransitiveDependencyOfDeclarationUnavailable
                      SelectionRoot r decl
          in  Acc selDecls tIds (msg : msgs)
        -- Declaration is a transitive dependency.
        (False, Just tIds', TransitivelyAvailable) ->
          Acc selDecls' tIds' (getSelMsgs TransitiveDependency decl ++ msgs)
        (False, Just tIds', TransitivelyUnavailable r) ->
          let msg = TransitiveDependencyOfDeclarationUnavailable
                      TransitiveDependency r decl
          in  Acc selDecls tIds' (msg : msgs)
        -- Declaration is not selected.
        (False, Nothing, _) ->
          let msg = SelectStatusInfo NotSelected decl
          in  Acc selDecls tIds (msg : msgs)
        -- Declaration is a selection root and a transitive dependency. This
        -- should be impossible and we consider it a bug.
        (True, Just _, _) ->
          panicPure $
            "Declaration is selection root and transitive dependency: "
            ++ show decl.declInfo

    getSelMsgs :: SelectReason -> C.Decl Select -> [Msg Select]
    getSelMsgs selectReason decl =
      let selectDepr   = [ SelectDeprecated decl | isDeprecated decl.declInfo ]
      in  SelectStatusInfo (Selected selectReason) decl : selectDepr

    isDeprecated :: C.DeclInfo Select -> Bool
    isDeprecated info = case C.declAvailability info of
      C.Deprecated -> True
      _            -> False

{-------------------------------------------------------------------------------
  Parse trace messages
-------------------------------------------------------------------------------}

getDelayedParseMsgs :: Set DeclId -> DeclIndex -> [Msg Select]
getDelayedParseMsgs selIds index = concatMap getMsgs $ Set.toList selIds
  where
    getMsgs :: DeclId -> [Msg Select]
    getMsgs k = map SelectParseSuccess $ DeclIndex.lookupAttachedParseMsgs k index

getParseNotAttemptedMsgs :: Match -> (DeclId -> Bool) -> DeclIndex -> [Msg Select]
getParseNotAttemptedMsgs match isIgnored =
  Foldable.foldl' (Foldable.foldl' addMsg) [] . notAttempted
  where
    addMsg :: [SelectMsg] -> ParseNotAttempted -> [SelectMsg]
    addMsg xs (ParseNotAttempted i l a r) =
      [ SelectParseNotAttempted i l r
      | match i l a
      , not $ isIgnored i
      ] ++ xs

getParseFailureMsgs :: Match -> (DeclId -> Bool) -> DeclIndex -> [Msg Select]
getParseFailureMsgs match isIgnored =
  Foldable.foldl' (Foldable.foldl' addMsg) [] . failed
  where
    addMsg :: [SelectMsg] -> ParseFailure -> [SelectMsg]
    addMsg xs (ParseFailure i l a msgs) =
      [ SelectParseFailure msg
      | match i l a
      , not $ isIgnored i
      , msg <- NonEmpty.toList msgs
      ] ++ xs

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- Return @Just@ the new set if the element was deleted, otherwise return
-- @Nothing@.
lookupAndDelete :: Ord a => a -> Set a -> Maybe (Set a)
lookupAndDelete x xs = Set.alterF (\b -> if b then Just False else Nothing) x xs
