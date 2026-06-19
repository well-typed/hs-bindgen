module HsBindgen.Frontend.Pass.Select (
    selectDecls
  ) where

import Data.List (sortBy)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Ord (comparing)
import Data.Set ((\\))
import Data.Set qualified as Set

import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex, Entry (..),
                                              Success (..), Unusable (..),
                                              Usable (..))
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict qualified as Conflict
import HsBindgen.Frontend.Pass.EnrichComments.IsPass (EnrichComments)
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Data types
-------------------------------------------------------------------------------}

-- Declaration itself.
type Decl l = C.Decl l Select

-- | Internal data type! This data type 'HsBindgen.Frontend.Pass.Select.Unselectable' refers to declarations
-- that are not selectable _from the perspective of `hs-bindgen`_; and, in
-- particular, not from the perspective of the user (they can change the select
-- predicate).
--
-- Also, (and in contrast to 'Usable'/'Unusable'), selectability _is concerned
-- with transitivity_. All transitive dependencies of a selectable declaration
-- must also be selectable.
data Unselectable =
    -- | We (i.e., `hs-bindgen`) can not select a declaration selected because
    --   it or one of its dependencies is unusable.
    Unselectable Unusable
    -- | We (i.e., `hs-bindgen`) can not select a declaration because one of its
    --   dependencies has not been selected by the user.
  | UnselectableNotSelected
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
  -> C.TranslationUnit l AdjustTypes
  -> (C.TranslationUnit l Select, [AnnMsg Select])
selectDecls isMainHeader isInMainHeaderDir config unit =
    let -- Directly match the selection predicate on the 'DeclIndex', obtaining
        -- information about succeeded _and failed_ selection roots.
        selectionRootsIndex :: DeclIndex l
        selectionRootsIndex = selectDeclIndex unit.meta.declUseGraph match index

        -- Identifiers of selection roots. Some of them may be unavailable
        -- (i.e., not in the 'succeeded' map, and hence, not in the list of
        -- declarations attached to the translation unit).
        rootIds :: Set C.DeclId
        rootIds = DeclIndex.keysSet selectionRootsIndex

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

        additionalSelectedTransDepsIndex :: DeclIndex l
        additionalSelectedTransDepsIndex = DeclIndex.restrictKeys index additionalSelectedTransDepIds

        notSelectedIndex :: DeclIndex l
        notSelectedIndex = DeclIndex.withoutKeys index selectedIds

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
              Unselectable <$> DeclIndex.getUnusables index transDeps

            nonselected :: Map C.DeclId Unselectable
            nonselected  =
              Map.fromSet (const UnselectableNotSelected) $
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
            getMostNaturalUnselectable l r = case (l,r) of
              (UnselectableNotSelected, Unselectable u         ) ->
                case u of
                  UnusableParseNotAttempted{} -> r
                  UnusableOmitted{}           -> r
                  _otherReason                -> l
              (Unselectable u         , UnselectableNotSelected) ->
                case u of
                  UnusableParseNotAttempted{} -> l
                  UnusableOmitted{}           -> l
                  _otherReason                -> r
              (_, _) -> l

        selectDecl :: Decl l -> Maybe (Decl l)
        selectDecl =
          selectDeclWith
            selectedIds
            getTransitiveSelectability

        unitDecls :: [Decl l]
        unitDecls = map coercePass unit.decls

        selectedUnitDecls  :: [Decl l]
        selectedUnitDecls = mapMaybe selectDecl unitDecls

        selectMsgs :: [AnnMsg Select]
        selectMsgs =
          getSelectMsgs
            rootIds
            additionalSelectedTransDepIds
            getTransitiveSelectability
            index

        unitSelect :: C.TranslationUnit l Select
        unitSelect = C.TranslationUnit {
                decls        = selectedUnitDecls
              , includeGraph = unit.includeGraph
              , meta         = unit.meta
              }

        -- If there were no predicate matches we issue a warning to the user.
        noDeclarationsMatchedMsg :: [AnnMsg Select]
        noDeclarationsMatchedMsg = [
            withCallStack C.WithLocationInfo{
                loc = C.LocationUnavailable
              , msg = SelectNoDeclarationsMatched
              }
          | Set.null rootIds
          ]

        msgs :: [AnnMsg Select]
        msgs =
          concat [
              selectMsgs
            , getDelayedMsgsSelectionRoots selectionRootsIndex
            , getDelayedMsgsAdditionalSelectedTransDeps additionalSelectedTransDepsIndex
            , getDelayedMsgsNotSelected notSelectedIndex
            , noDeclarationsMatchedMsg
            ]

    in  ( unitSelect
        , sortSelectMsgs unit.includeGraph msgs
        )
  where
    index :: DeclIndex l
    index = unit.meta.declIndex

    useDeclGraph :: UseDeclGraph
    useDeclGraph = unit.meta.useDeclGraph

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
    declId = decl.info.id.cName

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
  -> DeclIndex l
  -> [AnnMsg Select]
getSelectMsgs
  rootIds
  additionalSelectedTransDepIds
  getTransitiveSelectability
  declIndex
  = concatMap (uncurry aux) $ DeclIndex.toList declIndex
  where
    aux :: HasCallStack => C.DeclId -> Entry l -> [AnnMsg Select]
    aux =
      getSelectMsgsDeclId
        getTransitiveSelectability
        declIndex
        rootIds
        additionalSelectedTransDepIds

getSelectMsgsDeclId ::
    HasCallStack
  => (C.DeclId -> TransitiveSelectability)
  -> DeclIndex l
  -- | Selection roots.
  -> Set C.DeclId
  -- | Additionally selected transitive dependencies (non-empty when program
  --   slicing is enabled).
  -> Set C.DeclId
  -> C.DeclId
  -> Entry l
  -> [AnnMsg Select]
getSelectMsgsDeclId
  getTransitiveSelectability
  declIndex
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
      -- Declaration is not selected.
      (False, False, _) ->
        [withLoc $ SelectStatusInfo NotSelected]
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

    getMsgsFor :: HasCallStack => SelectReason -> [AnnMsg Select]
    getMsgsFor selectReason = concat [
          [ withLoc $ SelectStatusInfo (Selected selectReason) ]
        , [ withLoc $ SelectDeprecated selectReason | isDeprecated ]
        ]

    loc :: [SingleLoc]
    loc = DeclIndex.entryToLoc entry

    withLoc :: HasCallStack => a -> WithCallStack (C.WithLocationInfo a)
    withLoc x = withCallStack C.WithLocationInfo{
                  loc = C.declIdLocationInfo declId loc
                , msg = x
                }

    getUnavailMsg ::
         HasCallStack
      => SelectReason
      -> Map C.DeclId Unselectable -> Maybe (AnnMsg Select)
    getUnavailMsg selectReason unavailReasons =
        if null msgs then
          Nothing
        else
          Just $ withLoc $ TransitiveDependenciesMissing selectReason msgs
      where
        msgs = [
               case r of
                 Unselectable u ->
                   TransitiveDependencyUnusable
                     i
                     u
                     (DeclIndex.lookupLoc i declIndex)
                 UnselectableNotSelected ->
                   TransitiveDependencyNotSelected
                     i
                     (DeclIndex.lookupLoc i declIndex)
             | (i, r) <- Map.toList unavailReasons
             ]

    isDeprecated :: Bool
    isDeprecated = DeclIndex.entryToAvailability entry == C.Deprecated

{-------------------------------------------------------------------------------
  Delayed traces
-------------------------------------------------------------------------------}

mkSuccessMessages ::
     HasCallStack
  => C.DeclId
  -> Success l EnrichComments
  -> [AnnMsg Select]
mkSuccessMessages declId success = concat [
      fmap (mkAnnMsg . SelectDelayedParseMsg)
        delayedParseMsgs
    , fmap (mkAnnMsg . SelectDelayedPrepareReparseMsg)
        delayedPrepareReparseMsgs
    ]
  where
    DeclIndex.Success _ delayedParseMsgs delayedPrepareReparseMsgs = success

    mkAnnMsg :: HasCallStack => SelectMsg -> AnnMsg Select
    mkAnnMsg msg = withCallStack C.WithLocationInfo{
          loc = C.declIdLocationInfo declId [success.decl.info.loc]
        , msg = msg
        }

getDelayedMsgsSelectionRoots :: HasCallStack => DeclIndex l -> [AnnMsg Select]
getDelayedMsgsSelectionRoots = concatMap (uncurry aux) . DeclIndex.toList
  where
    aux :: HasCallStack => C.DeclId -> Entry l -> [AnnMsg Select]
    aux declId = \case
      UsableE e -> case e of
        UsableSuccess success -> mkSuccessMessages declId success
        UsableExternal   -> []
        -- Parse messages are unavailable for squashed entries. We are OK with
        -- this; instead we have issued a notice that the @typedef@ was squashed.
        UsableSquashed x ->
          [ withCallStack C.WithLocationInfo{
                loc = C.declIdLocationInfo declId [x.typedefLoc]
              , msg = SelectMangleNamesSquashed x
              }
          ]
      UnusableE e -> case e of
        UnusableParseNotAttempted loc xs ->
          [ withCallStack C.WithLocationInfo{
                loc = C.declIdLocationInfo declId [loc]
              , msg = SelectParseNotAttempted x
              }
          | x <- NonEmpty.toList xs
          ]
        UnusableParseFailure loc x -> List.singleton $ withCallStack C.WithLocationInfo{
            loc = C.declIdLocationInfo declId [loc]
          , msg = SelectParseFailure x
          }
        UnusableConflict x -> List.singleton $ withCallStack C.WithLocationInfo{
            loc = C.declIdLocationInfo declId (Conflict.toList x)
          , msg = SelectConflict
          }
        UnusableMangleNamesFailure loc x -> List.singleton $ withCallStack C.WithLocationInfo{
            loc = C.declIdLocationInfo declId [loc]
          , msg = SelectMangleNamesFailure x
          }
        UnusableTypecheckMacrosError loc err -> List.singleton$ withCallStack C.WithLocationInfo{
            loc = C.declIdLocationInfo declId [loc]
          , msg = SelectMacroTypecheckFailure err
          }
        UnusableOmitted{} ->
          []

getDelayedMsgsAdditionalSelectedTransDeps ::
     HasCallStack
  => DeclIndex l
  -> [AnnMsg Select]
getDelayedMsgsAdditionalSelectedTransDeps = concatMap (uncurry aux) . DeclIndex.toList
  where
    aux :: HasCallStack => C.DeclId -> Entry l -> [AnnMsg Select]
    aux declId = \case
      UsableE e -> case e of
        UsableSuccess success -> mkSuccessMessages declId success
        UsableExternal   -> []
        -- Parse messages are unavailable for squashed entries. We are OK with
        -- this; instead we have issued a notice that the @typedef@ was squashed.
        UsableSquashed x ->
          [ withCallStack C.WithLocationInfo{
                loc = C.declIdLocationInfo declId [x.typedefLoc]
              , msg = SelectMangleNamesSquashed x
              }
          ]
      -- Messages for unusable transitive dependencies are already attached to
      -- the traces of the reverse transitive dependencies.
      UnusableE _ -> []

-- NOTE: We emit delayed BUG-level parse messages even for declarations that are
-- not selected. We do not have a test for this; please ensure delayed BUG-level
-- parse messages are emitted for all declarations also in the future.
getDelayedMsgsNotSelected :: HasCallStack => DeclIndex l -> [AnnMsg Select]
getDelayedMsgsNotSelected = concatMap (uncurry aux) . DeclIndex.toList
  where
    aux :: HasCallStack => C.DeclId -> Entry l -> [AnnMsg Select]
    aux declId = \case
      UsableE e -> case e of
        UsableSuccess success ->
          let isBugLevel x = getDefaultLogLevel x == Bug
          in  filter isBugLevel $ mkSuccessMessages declId success
        UsableExternal -> []
        UsableSquashed{} -> []
      UnusableE e -> case e of
        UnusableParseNotAttempted{} -> []
        UnusableParseFailure loc x -> case getDefaultLogLevel x of
          Bug ->
            List.singleton $ withCallStack C.WithLocationInfo{
                loc = C.declIdLocationInfo declId [loc]
              , msg = SelectDelayedParseMsg x
              }
          _otherLvl -> []
        UnusableConflict{} -> []
        UnusableMangleNamesFailure{} -> []
        UnusableTypecheckMacrosError loc err -> case getDefaultLogLevel err of
          Bug ->
            List.singleton $ withCallStack C.WithLocationInfo{
                loc = C.declIdLocationInfo declId [loc]
              , msg = SelectMacroTypecheckFailure err
              }
          _otherLvl -> []
        UnusableOmitted{} -> []

{-------------------------------------------------------------------------------
  Sort traces
-------------------------------------------------------------------------------}

compareByOrder :: Map SourcePath Int -> SourcePath -> SourcePath -> Ordering
compareByOrder xs x y =
  let ix = lookupUnsafe x
      iy = lookupUnsafe y
  in  compare ix iy
  where
    lookupUnsafe z = case Map.lookup z xs of
      Nothing -> panicPure $ "Unknown source path: " <> show z
      Just v  -> v

compareSingleLocs :: Map SourcePath Int -> SingleLoc -> SingleLoc -> Ordering
compareSingleLocs xs x y =
    case compareByOrder xs (singleLocPath x) (singleLocPath y) of
      LT -> LT
      EQ -> comparing getLineCol x y
      GT -> GT
  where
    getLineCol :: SingleLoc -> (Int, Int)
    getLineCol z = (singleLocLine z, singleLocColumn z)

compareMsgs :: Map SourcePath Int -> AnnMsg Select -> AnnMsg Select -> Ordering
compareMsgs orderMap x y =
  case (C.locationInfoLocs x.traceMsg.loc, C.locationInfoLocs y.traceMsg.loc) of
    (lx : __, ly : _) -> compareSingleLocs orderMap lx ly
    -- Sort messages not attached to a declaration to the back.
    ([] , _ ) -> GT
    (_  , []) -> LT

sortSelectMsgs :: IncludeGraph -> [AnnMsg Select] -> [AnnMsg Select]
sortSelectMsgs includeGraph = sortBy (compareMsgs orderMap)
  where
    -- Compute the order map once.
    orderMap :: Map SourcePath Int
    orderMap = IncludeGraph.toOrderMap includeGraph

{-------------------------------------------------------------------------------
  Apply the selection predicate
-------------------------------------------------------------------------------}

-- Match function to find selection roots.
type Match = C.DeclName -> SingleLoc -> C.Availability -> Bool

-- | Limit the declaration index to those entries that match the select
--   predicate. Do not include anything external nor omitted.
selectDeclIndex :: DeclUseGraph -> Match -> DeclIndex l -> DeclIndex l
selectDeclIndex declUseGraph p declIndex =
    DeclIndex.filter matchEntry declIndex
  where
    matchEntry :: C.DeclId -> Entry l -> Bool
    matchEntry declId entry =
        case entryInfo entry of
          Nothing ->
            -- External or omittted declarations have already been removed from
            -- the list of declarations
            False
          Just (locs, availability) ->
            if declId.isAnon
              then matchAnon declId
              else or [p declId.name loc availability| loc <- locs]

    matchDeclId :: C.DeclId -> Bool
    matchDeclId declId =
        case DeclIndex.lookupEntry declId declIndex of
          Just entry -> matchEntry declId entry
          Nothing    -> panicPure $ "Unknown declaration ID: " ++ show declId

    -- Extract info from 'Entry' needed to match against the selection predicate
    --
    -- Returns 'Nothing' for external or omitted declarations.
    -- Returns 'Just _' for squashed declarations. Those can still be selected.
    -- Returns multiple locations only for conflicts.
    entryInfo :: Entry l -> Maybe ([SingleLoc], C.Availability)
    entryInfo = \case
        UsableE e -> case e of
          UsableSuccess success ->
            let info = success.decl.info
            in Just ([info.loc], info.availability)
          UsableExternal ->
            Nothing
          UsableSquashed x ->
            Just ([x.typedefLoc], C.Available)
        UnusableE e -> case e of
          UnusableParseNotAttempted loc _ ->
            Just ([loc], C.Available)
          UnusableParseFailure loc _ ->
            Just ([loc], C.Available)
          UnusableConflict conflict ->
            Just (Conflict.toList conflict, C.Available)
          UnusableMangleNamesFailure loc _ ->
            Just ([loc], C.Available)
          UnusableTypecheckMacrosError loc _ ->
            Just ([loc], C.Available)
          UnusableOmitted{} ->
            Nothing

    -- We match anonymous declarations based on their use sites.
    --
    -- Looking at the use sites means we treat anonymous declarations in a very
    -- similar way to auxiliary declarations. It's however worth spelling out
    -- why this is possible. There are two main classes of anonymous
    -- declarations. The first is anonymous declarations inside of typedefs:
    --
    -- > typedef struct { .. } foo;
    --
    -- Selecting the typedef but not the struct makes no sense; selecting the
    -- struct but not the typedef does, but is already captured by support for
    -- squashing typedefs.
    --
    -- The second class is anonymous declarations inside structs or unions:
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
    matchAnon :: C.DeclId -> Bool
    matchAnon anon =
       case DeclUseGraph.getUseSites declUseGraph anon of
         (declId, _) :_ ->
           -- The only way that anonymous declarations can have multiple use
           -- sites is when multiple fields are defined together:
           --
           -- > struct foo {
           -- >   struct { .. } x, y, z;
           -- > }
           --
           -- From the perspective of selection, any of these will do.
           matchDeclId declId
         [] ->
           -- An anonymous declaration can have no use sites if those
           -- dependencies were removed from the graph because the parent
           -- declaration is opaqued using a prescriptive binding specification.
           False
