module HsBindgen.Frontend.Pass.Select (
    selectDecls
  ) where

import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set ((\\))
import Data.Set qualified as Set

import Clang.HighLevel.Types

import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.DeclDB (Deactivation (..), DeclDB, Entry (..),
                                  Inactive (..), InactiveInfo (..),
                                  InactiveReason (..), UnusableReason (..))
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.MacroFallback (FallbackMacro (..))
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass (TypecheckedMacro (..))
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.SortMsgs
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.IR.Translation
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Data types
-------------------------------------------------------------------------------}

-- Declaration itself.
type Decl l = C.Decl l Select

type In = TranslateTypes

-- | Internal data type! This data type
-- 'HsBindgen.Frontend.Pass.Select.Unselectable' refers to declarations that are
-- not selectable _from the perspective of `hs-bindgen`_; and, in particular,
-- not from the perspective of the user (they can change the select predicate).
--
-- Also, (and in contrast to usability, see 'Entry'), selectability _is
-- concerned with transitivity_. All transitive dependencies of a selectable
-- declaration must also be selectable.
data Unselectable =
    -- | We (i.e., `hs-bindgen`) can not select a declaration selected because
    --   it or one of its dependencies is unusable.
    Unselectable Inactive
    -- | We (i.e., `hs-bindgen`) can not select a declaration because one of its
    --   dependencies has not been selected by the user.
  | UnselectableDependencyNotSelected
  deriving stock (Show)

-- | We have to treat with two notions of usability here:
--
-- 1. A declaration can be usable because it is in the list of declarations
--    attached to the translation unit.
--
-- 2. A declaration is available if the declaration itself and all of its
--    transitive dependencies are available.
--
-- @TransitiveSelectability@ deals with the second type.
--
-- (We avoid the term available, because it is overloaded with Clang's
-- CXAvailabilityKind).
data TransitiveSelectability =
    TransitivelySelectable
    -- | For each transitive dependency, we try to give the root cause of
    -- unavailability.
    --
    -- We should use a "non-empty" map here.
  | TransitivelyUnselectable (Map C.DeclId Unselectable)
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Select
-------------------------------------------------------------------------------}

selectDecls ::
     forall l. HasCallStack
  => IsMainHeader
  -> IsInMainHeaderDir
  -> SelectConfig
  -> C.TranslationUnit l TranslateTypes
  -> (C.TranslationUnit l Select, [AnnMsg Select])
selectDecls isMainHeader isInMainHeaderDir config unit =
    let -- Directly match the selection predicate on the 'DeclDB', obtaining
        -- information about succeeded _and failed_ selection roots.
        selectionRoots :: Map C.DeclId (Entry l In)
        selectionRoots = selectEntries match index

        -- Identifiers of selection roots. Some of them may be unavailable
        -- (i.e., not in the 'succeeded' map, and hence, not in the list of
        -- declarations attached to the translation unit).
        rootIds :: Set C.DeclId
        rootIds = Map.keysSet selectionRoots

        -- Identifiers of transitive dependencies including selection roots.
        rootAndTransDepIds :: Set C.DeclId
        rootAndTransDepIds = UseDeclGraph.getTransitiveDeps useDeclGraph rootIds

        -- Identifiers of transitive dependencies excluding selection roots.
        strictTransDepIds :: Set C.DeclId
        strictTransDepIds = rootAndTransDepIds \\ rootIds

        -- Identifiers of all selected declarations.
        selectedIds :: Set C.DeclId
        -- Identifiers of (additional) transitive dependencies selected due to
        -- program slicing. This is the only point where we differentiate
        -- between selection with or without program slicing.
        additionalSelectedTransDepIds :: Set C.DeclId
        (selectedIds, additionalSelectedTransDepIds) = case config.programSlicing of
          DisableProgramSlicing -> (rootIds        , Set.empty)
          EnableProgramSlicing  -> (rootAndTransDepIds, strictTransDepIds)

        additionalSelectedTransDeps :: Map C.DeclId (Entry l In)
        additionalSelectedTransDeps =
          Map.restrictKeys index.entries additionalSelectedTransDepIds

        notSelected :: Map C.DeclId (Entry l In)
        notSelected = Map.withoutKeys index.entries selectedIds

        getTransitiveSelectability :: C.DeclId -> TransitiveSelectability
        getTransitiveSelectability x
          | Map.null unusabilityReasons = TransitivelySelectable
          | otherwise                   = TransitivelyUnselectable unusabilityReasons
          where
            transDeps :: Set C.DeclId
            transDeps =
              UseDeclGraph.getStrictTransitiveDeps
                useDeclGraph
                (Set.singleton x)

            unusables :: Map C.DeclId Unselectable
            unusables =
              Map.mapMaybe (fmap Unselectable . DeclDB.getUnusable) $
                Map.restrictKeys index.entries transDeps

            nonselected :: Map C.DeclId Unselectable
            nonselected  =
              Map.fromSet (const UnselectableDependencyNotSelected) $
                transDeps \\ selectedIds

            unusabilityReasons :: Map C.DeclId Unselectable
            unusabilityReasons =
              Map.unionWith
                getMostNaturalUnselectable
                unusables
                nonselected

            -- Get the reason that is most useful to the user about why a
            -- declaration is unselectable.
            getMostNaturalUnselectable ::
              Unselectable -> Unselectable -> Unselectable
            getMostNaturalUnselectable l r =
              case (l,r) of
                (UnselectableDependencyNotSelected, Unselectable u) ->
                  case u of
                    InactiveDecl InactiveInfo{reason = NotAttempted}        -> r
                    InactiveDecl InactiveInfo{reason = Deactivated Omitted} -> r
                    _otherReason                                            -> l
                (Unselectable u, UnselectableDependencyNotSelected) ->
                  case u of
                    InactiveDecl InactiveInfo{reason = NotAttempted}        -> l
                    InactiveDecl InactiveInfo{reason = Deactivated Omitted} -> l
                    _otherReason                                            -> r
                (_, _) -> l

        selectDecl :: Decl l -> Maybe (Decl l)
        selectDecl =
          selectDeclWith
            selectedIds
            getTransitiveSelectability

        db' :: DeclDB l Select
        db' =
          runIdentity $
            DeclDB.traverseActive DeclDB.AnyOrder
              (\decl -> pure $ case selectDecl (coercePass decl) of
                  Just decl' -> DeclDB.Advanced decl'
                  Nothing    -> DeclDB.Deselect)
              index

        unitSelect :: C.TranslationUnit l Select
        unitSelect = C.fromDeclDB unit.includeGraph db'

        selectMsgs :: [AnnMsg Select]
        selectMsgs =
          getSelectMsgs
            rootIds
            additionalSelectedTransDepIds
            getTransitiveSelectability
            index

        -- If there were no predicate matches we issue a warning to the user.
        noDeclarationsMatchedMsg :: [AnnMsg Select]
        noDeclarationsMatchedMsg = [
            withCallStack C.WithLocationInfo{
                loc = C.LocationUnavailable
              , msg = SelectNoDeclarationsMatched
              }
          | Set.null rootIds
          ]

        selectionRootMsgs :: [AnnMsg Select]
        selectionRootMsgs = getDelayedMsgsSelectionRoots index selectionRoots

        -- Number of selected macros that hs-bindgen dropped.
        droppedMacroCount :: Int
        droppedMacroCount =
          length $ filter (\m -> isDroppedMacro m.traceMsg.msg) selectionRootMsgs

        -- Single summary of dropped macros.
        macrosDroppedSummaryMsg :: [AnnMsg Select]
        macrosDroppedSummaryMsg = [
            withCallStack C.WithLocationInfo{
                loc = C.LocationUnavailable
              , msg = SelectMacrosDropped droppedMacroCount
              }
          | droppedMacroCount > 0
          ]

        -- Selected macros that fall back
        macroFallbackMsgs :: [AnnMsg Select]
        macroFallbackMsgs = [
            withCallStack C.WithLocationInfo{
                loc = C.declIdLocationInfo decl.id.cName [decl.info.loc]
              , msg = SelectMacroFallback raw.failure
              }
          | decl <- DeclDB.active DeclDB.AnyOrder db'
          , C.DeclMacro (MacroRaw raw) <- [decl.kind]
          ]

        -- Single summary of fallback macros.
        macrosFallbackSummaryMsg :: [AnnMsg Select]
        macrosFallbackSummaryMsg = [
            withCallStack C.WithLocationInfo{
                loc = C.LocationUnavailable
              , msg = SelectMacrosFallback (length macroFallbackMsgs)
              }
          | not (null macroFallbackMsgs)
          ]

        msgs :: [AnnMsg Select]
        msgs =
          concat [
              selectMsgs
            , selectionRootMsgs
            , macroFallbackMsgs
            , getDelayedMsgsAdditionalSelectedTransDeps index additionalSelectedTransDeps
            , getBugMsgsNotSelected index notSelected
            , getBugMsgsInactive index
            , noDeclarationsMatchedMsg
            ]

    in  ( unitSelect
        , concat [
              sortSelectMsgs unit.includeGraph msgs
            , macrosDroppedSummaryMsg
            , macrosFallbackSummaryMsg
            ]
        )
  where
    index :: DeclDB l In
    index = unit.db

    useDeclGraph :: UseDeclGraph
    useDeclGraph = UseDeclGraph.fromDeclUseGraph index.declUseGraph

    match :: Match
    match name loc availability =
      matchSelect
        isMainHeader
        isInMainHeaderDir
        (singleLocPath loc)
        name
        availability
        config.selectionPredicate

{-------------------------------------------------------------------------------
  Filter list of declarations
-------------------------------------------------------------------------------}

selectDeclWith ::
  -- | Selected declaration IDs.
     Set C.DeclId
  -> (C.DeclId -> TransitiveSelectability)
  -> Decl l
  -> Maybe (Decl l)
selectDeclWith
  selectedIds
  getTransitiveSelectability
  decl =
    case (isSelected, transitiveSelectability) of
      (True, TransitivelySelectable) -> Just decl
      _otherwise                     -> Nothing
  where
    declId :: C.DeclId
    declId = decl.id.cName

    isSelected :: Bool
    isSelected = Set.member declId selectedIds

    transitiveSelectability :: TransitiveSelectability
    transitiveSelectability = getTransitiveSelectability declId

{-------------------------------------------------------------------------------
  Select traces
-------------------------------------------------------------------------------}

getSelectMsgs ::
     forall l. HasCallStack
  => Set C.DeclId
  -- ^ Selection roots.
  -> Set C.DeclId
  -- ^ Additionally selected transitive dependencies (non-empty when program
  --   slicing is enabled).
  ->(C.DeclId -> TransitiveSelectability)
  -> DeclDB l In
  -> [AnnMsg Select]
getSelectMsgs
  rootIds
  additionalSelectedTransDepIds
  getTransitiveSelectability
  declDB
  = concatMap (uncurry aux) $ DeclDB.toList declDB
  where
    aux ::
         HasCallStack
      => C.DeclId
      -> Entry l In
      -> [AnnMsg Select]
    aux =
      getSelectMsgsDeclId
        getTransitiveSelectability
        declDB
        rootIds
        additionalSelectedTransDepIds

getSelectMsgsDeclId ::
    HasCallStack
  => (C.DeclId -> TransitiveSelectability)
  -> DeclDB l In
  -- | Selection roots.
  -> Set C.DeclId
  -- | Additionally selected transitive dependencies (non-empty when program
  --   slicing is enabled).
  -> Set C.DeclId
  -> C.DeclId
  -> Entry l In
  -> [AnnMsg Select]
getSelectMsgsDeclId
  getTransitiveSelectability
  declDB
  rootIds
  additionalSelectedTransDepIds
  declId
  entry
  = case ( isSelectedRoot
         , isAdditionalSelectedTransDep
         , transitiveSelectability ) of
      -- Declaration is a selection root.
      (True, False, TransitivelySelectable) ->
        getMsgsFor SelectionRoot
      (True, False, TransitivelyUnselectable rs) ->
        maybeToList $ getUnavailMsg SelectionRoot rs
      -- Declaration is an additionally selected transitive dependency.
      (False, True, TransitivelySelectable) ->
        getMsgsFor TransitiveDependency
      (False, True, TransitivelyUnselectable rs) ->
        maybeToList $ getUnavailMsg TransitiveDependency rs
      -- Declaration is not selected. An external declaration has no location
      -- to report.
      (False, False, _) -> case entry of
        External{}  -> []
        _otherEntry -> [withLoc $ SelectStatusInfo NotSelected]
      -- Declaration is a selection root and a transitive dependency. This
      -- should be impossible and we consider it a bug.
      (True, True, _) ->
        panicPure $
          "Declaration is selection root and transitive dependency: "
          ++ show (withLoc declId)
  where
    -- We check three conditions:
    isSelectedRoot = Set.member declId rootIds
    -- These are also always strict transitive dependencies.
    isAdditionalSelectedTransDep =
      Set.member declId additionalSelectedTransDepIds
    transitiveSelectability = getTransitiveSelectability declId

    -- An unusable declaration is not selected: as a selection root, it reports
    -- why ('getDelayedMsgsSelectionRoots'); as a transitive dependency, its
    -- users do.
    getMsgsFor :: HasCallStack => SelectReason -> [AnnMsg Select]
    getMsgsFor selectReason
      | isJust (DeclDB.getUnusable entry) = []
      | otherwise = concat [
            [ withLoc $ SelectStatusInfo (Selected selectReason) ]
          , [ withLoc $ SelectDeprecated selectReason | isDeprecated ]
          ]

    withLoc :: HasCallStack => a -> WithCallStack (C.WithLocationInfo a)
    withLoc x = withCallStack C.WithLocationInfo{
                  loc = C.declIdLocationInfo declId $
                          foldMap C.declLocsToList $
                            DeclDB.entryToLoc entry
                , msg = x
                }

    getUnavailMsg ::
         HasCallStack
      => SelectReason
      -> Map C.DeclId Unselectable
      -> Maybe (AnnMsg Select)
    getUnavailMsg selectReason unavailReasons =
        if null msgs then
          Nothing
        else
          Just $ withLoc $ TransitiveDependenciesMissing selectReason msgs
      where
        msgs = [
               case r of
                 Unselectable u ->
                   TransitiveDependencyUnusable i u
                 UnselectableDependencyNotSelected ->
                   TransitiveDependencyNotSelected i locs
             | (i, r) <- Map.toList unavailReasons
             , let locs = case DeclDB.lookupLoc i declDB of
                     Nothing -> []
                     Just xs -> C.declLocsToList xs
             ]

    isDeprecated :: Bool
    isDeprecated = DeclDB.entryToAvailability entry == C.Deprecated

{-------------------------------------------------------------------------------
  Delayed traces
-------------------------------------------------------------------------------}

mkActiveMessages ::
     HasCallStack
  => DeclDB l In
  -> C.DeclId
  -> C.Decl l In
  -> [AnnMsg Select]
mkActiveMessages declDB declId decl =
    map (mkAnnMsg . SelectDelayedMsg) $ DeclDB.lookupDelayedMsgs declId declDB
  where
    mkAnnMsg :: HasCallStack => SelectMsg -> AnnMsg Select
    mkAnnMsg msg = withCallStack C.WithLocationInfo{
          loc = C.declIdLocationInfo declId [decl.info.loc]
        , msg = msg
        }

mkSquashedMessage ::
     HasCallStack
  => C.DeclId
  -> DeclDB.SquashedTypedef
  -> AnnMsg Select
mkSquashedMessage declId x = withCallStack C.WithLocationInfo{
      loc = C.declIdLocationInfo declId [x.info.loc]
    , msg = SelectMangleNamesSquashed x
    }

mkUnusableMessage :: HasCallStack => C.DeclId -> InactiveInfo -> AnnMsg Select
mkUnusableMessage declId x = withCallStack C.WithLocationInfo{
      loc = C.declIdLocationInfo declId [x.info.loc]
    , msg = SelectUnusable x.reason
    }

-- | The delayed messages of a selected inactive declaration, but for Bug-level
-- ones ('getBugMsgsInactive')
--
-- An omitted declaration reports none: the user asked for it.
mkInactiveMessages ::
     HasCallStack
  => DeclDB l In
  -> C.DeclId
  -> InactiveInfo
  -> [AnnMsg Select]
mkInactiveMessages declDB declId x = case x.reason of
    Deactivated Omitted -> []
    _otherReason        -> [
        withCallStack C.WithLocationInfo{
            loc = C.declIdLocationInfo declId [x.info.loc]
          , msg = SelectDelayedMsg msg
          }
      | msg <- DeclDB.lookupDelayedMsgs declId declDB
      , getDefaultLogLevel msg /= Bug
      ]

getDelayedMsgsSelectionRoots ::
     forall l. HasCallStack
  => DeclDB l In
  -> Map C.DeclId (Entry l In)
     -- ^ Selection roots
  -> [AnnMsg Select]
getDelayedMsgsSelectionRoots declDB roots =
    concatMap (uncurry aux) $ Map.toList roots
  where
    aux ::
         HasCallStack
      => C.DeclId
      -> Entry l In
      -> [AnnMsg Select]
    aux declId = \case
      Active decl ->
        mkActiveMessages declDB declId decl
      External{} -> []
      -- Only after @Select@
      Deselected{} -> []
      -- We do not emit the delayed messages of squashed entries; instead we
      -- issue a notice that the @typedef@ was squashed.
      Squashed x ->
        List.singleton $ mkSquashedMessage declId x
      Inactive (InactiveDecl x) ->
          mkUnusableMessage declId x
        : mkInactiveMessages declDB declId x
      Inactive (InactiveConflict c) ->
        List.singleton $ withCallStack C.WithLocationInfo{
            loc = C.declIdLocationInfo declId $
              NonEmpty.toList $ C.conflictToList c
          , msg = SelectConflict
          }

getDelayedMsgsAdditionalSelectedTransDeps ::
     forall l. HasCallStack
  => DeclDB l In
  -> Map C.DeclId (Entry l In)
     -- ^ Additionally selected transitive dependencies
  -> [AnnMsg Select]
getDelayedMsgsAdditionalSelectedTransDeps declDB transDeps =
    concatMap (uncurry aux) $ Map.toList transDeps
  where
    aux ::
         HasCallStack
      => C.DeclId
      -> Entry l In
      -> [AnnMsg Select]
    aux declId = \case
      Active decl ->
        mkActiveMessages declDB declId decl
      External{} -> []
      -- Only after @Select@
      Deselected{} -> []
      -- We do not emit the delayed messages of squashed entries; instead we
      -- issue a notice that the @typedef@ was squashed.
      Squashed x ->
        [mkSquashedMessage declId x]
      -- Messages for unusable transitive dependencies are already attached to
      -- the traces of the reverse transitive dependencies.
      Inactive _ -> []

-- NOTE: We emit delayed BUG-level parse messages even for declarations that are
-- not selected. We do not have a test for this; please ensure delayed BUG-level
-- parse messages are emitted for all declarations also in the future.
getBugMsgsNotSelected ::
     forall l. HasCallStack
  => DeclDB l In
  -> Map C.DeclId (Entry l In)
     -- ^ Declarations not selected
  -> [AnnMsg Select]
getBugMsgsNotSelected declDB notSelected =
    concatMap (uncurry aux) $ Map.toList notSelected
  where
    aux ::
         HasCallStack
      => C.DeclId
      -> Entry l In
      -> [AnnMsg Select]
    aux declId = \case
      Active decl ->
        let isBugLevel x = getDefaultLogLevel x == Bug
        in  filter isBugLevel $ mkActiveMessages declDB declId decl
      External{}   -> []
      Squashed{}   -> []
      Deselected{} -> []
      Inactive (InactiveDecl x) ->
        [ mkUnusableMessage declId x | isBug x.reason ]
      Inactive InactiveConflict{} -> []

    isBug :: InactiveReason -> Bool
    isBug = \case
      NotAttempted             -> False
      ParseFailed err          -> getDefaultLogLevel err == Bug
      Deactivated (Unusable r) -> getDefaultLogLevel r == Bug
      Deactivated Omitted      -> False

-- | Bug-level delayed messages of every entry that is not active, whether
-- selected or not
--
-- Active entries report theirs with their other delayed messages.
getBugMsgsInactive ::
     forall l. HasCallStack
  => DeclDB l In
  -> [AnnMsg Select]
getBugMsgsInactive declDB =
    concatMap (uncurry aux) $ DeclDB.toList declDB
  where
    aux ::
         HasCallStack
      => C.DeclId
      -> Entry l In
      -> [AnnMsg Select]
    aux declId = \case
      Active{} -> []
      entry    -> [
          withCallStack C.WithLocationInfo{
              loc = C.declIdLocationInfo declId $
                maybe [] C.declLocsToList (DeclDB.entryToLoc entry)
            , msg = SelectDelayedMsg msg
            }
        | msg <- DeclDB.lookupDelayedMsgs declId declDB
        , getDefaultLogLevel msg == Bug
        ]

-- | Does this selection-root message represent a macro that hs-bindgen dropped?
--
isDroppedMacro :: SelectMsg -> Bool
isDroppedMacro = \case
    SelectUnusable reason -> case reason of
      Deactivated (Unusable UnusableMacro{})       -> True
      -- A parse failure counts as a dropped macro iff it is a macro parse
      -- failure.
      ParseFailed p                                -> isMacroParseFailure p
      NotAttempted                                 -> False
      Deactivated Omitted                          -> False
      Deactivated (Unusable UnusableMangleNames{}) -> False
    _otherwise                                     -> False
  where
    isMacroParseFailure :: DelayedParseMsg -> Bool
    isMacroParseFailure = \case
      ParseMacroDefinitionNoMacroName{} -> True
      ParseMacroEmpty{}                 -> False
      ParseMacroErrorRead{}             -> True
      _otherwise                        -> False

{-------------------------------------------------------------------------------
  Sort traces
-------------------------------------------------------------------------------}

sortSelectMsgs :: IncludeGraph -> [AnnMsg Select] -> [AnnMsg Select]
sortSelectMsgs includeGraph msgs =
    bugMsgs ++ sortMsgs order msgs
  where
    -- Compute the include order once.
    order :: IncludeGraph.IncludeOrder
    order = IncludeGraph.toIncludeOrder includeGraph

    -- We do not know where to sort a message whose source is not in the include
    -- graph, but a mis-sorted message is no reason to abort: report the bug and
    -- sort the message to the back.
    bugMsgs :: [AnnMsg Select]
    bugMsgs = [
        withCallStack C.WithLocationInfo{
            loc = C.LocationUnavailable
          , msg = SelectSourceNotInIncludeGraph path
          }
      | path <- Set.toList unknownPaths
      ]

    unknownPaths :: Set RealPath
    unknownPaths = Set.fromList [
        path
      | msg <- msgs
      , Just loc <- [msgLoc msg]
      , C.InHeader path <- [singleLocPath loc]
      , IncludeGraph.lookupIncludeOrder order (C.InHeader path)
          == IncludeGraph.NotInIncludeGraph
      ]

{-------------------------------------------------------------------------------
  Apply the selection predicate
-------------------------------------------------------------------------------}

-- Match function to find selection roots.
type Match = C.DeclName -> SingleLoc C.DeclPath -> C.Availability -> Bool

-- | The entries that match the select predicate. Do not include anything
--   external nor omitted.
selectEntries ::
     Match
  -> DeclDB l In
  -> Map C.DeclId (Entry l In)
selectEntries predicate declDB =
    Map.filterWithKey matchEntry declDB.entries
  where
    matchEntry :: C.DeclId -> Entry l In -> Bool
    matchEntry declId entry =
        case entryInfo entry of
          Nothing ->
            -- External or omittted declarations have already been removed from
            -- the list of declarations
            False
          Just (locs, availability) ->
            if declId.isUnnamed
              then matchUnnamed declId
              else Foldable.any
                     (\loc -> predicate declId.name loc availability)
                     (C.declLocsToList locs)

    matchDeclId :: C.DeclId -> Bool
    matchDeclId declId =
        case DeclDB.lookupEntry declId declDB of
          Just entry -> matchEntry declId entry
          Nothing    -> panicPure $ "Unknown declaration ID: " ++ show declId

    -- Extract info from 'Entry' needed to match against the selection predicate
    --
    -- Returns 'Nothing' for external or omitted declarations.
    -- Returns 'Just _' for squashed declarations. Those can still be selected.
    -- Returns multiple locations only for conflicts.
    entryInfo ::
         Entry l In
      -> Maybe (C.DeclLocs, C.Availability)
    entryInfo entry = case entry of
        External{} ->
          Nothing
        Inactive (InactiveDecl InactiveInfo{reason = Deactivated Omitted}) ->
          Nothing
        _otherwise -> do
          locs <- DeclDB.entryToLoc entry
          pure (locs, DeclDB.entryToAvailability entry)

    -- We match unnamed declarations based on their use sites.
    --
    -- Looking at the use sites means we treat unnamed declarations in a very
    -- similar way to auxiliary declarations. It's however worth spelling out
    -- why this is possible. There are two main classes of unnamed
    -- declarations. The first is unnamed declarations inside of typedefs:
    --
    -- > typedef struct { .. } foo;
    --
    -- Selecting the typedef but not the struct makes no sense; selecting the
    -- struct but not the typedef does, but is already captured by support for
    -- squashing typedefs.
    --
    -- The second class is unnamed declarations inside structs or unions:
    --
    -- > struct outer {
    -- >    struct { .. } field1, field2;
    -- >    struct { .. } field3;
    -- > }
    -- >
    --
    -- Like for typedefs, it doesn't really make sense to select the outer
    -- struct but not the inner struct. It could /in principle/ make sense to
    -- select the inner but not the outer, but it seems a pretty rare use case,
    -- and not a big deal if the outer struct will "come along" even if only the
    -- inner struct is needed.
    matchUnnamed :: C.DeclId -> Bool
    matchUnnamed unnamed =
       case DeclUseGraph.getUseSites declDB.declUseGraph unnamed of
         (declId, _) :_ ->
           -- The only way that unnamed declarations can have multiple use
           -- sites is when multiple fields are defined together:
           --
           -- > struct foo {
           -- >   struct { .. } x, y, z;
           -- > }
           --
           -- From the perspective of selection, any of these will do.
           matchDeclId declId
         [] ->
           -- An unnamed declaration can have no use sites if those
           -- dependencies were removed from the graph because the parent
           -- declaration is opaqued using a prescriptive binding specification.
           False
