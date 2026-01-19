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
                                              Unusable (..), Usable (..))
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict qualified as Conflict
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.HandleMacros.Error
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Data types
-------------------------------------------------------------------------------}

-- Declaration itself.
type Decl = C.Decl Select

-- | Internal data type! This data type 'Unselectable' refers to declarations
-- that are not selectable _from the perspective of `hs-bindgen`_; and, in
-- particular, not from the perspective of the user (they can change the select
-- predicate).
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
  | TransitivelyUnselectable (Map DeclId Unselectable)
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Select
-------------------------------------------------------------------------------}

selectDecls ::
     IsMainHeader
  -> IsInMainHeaderDir
  -> SelectConfig
  -> C.TranslationUnit MangleNames
  -> (C.TranslationUnit Select, [Msg Select])
selectDecls isMainHeader isInMainHeaderDir config unit =
    let -- Directly match the select predicate on the 'DeclIndex', obtaining
        -- information about succeeded _and failed_ selection roots.
        selectedIndex :: DeclIndex
        selectedIndex = selectDeclIndex unit.ann.declUseGraph match index

        -- Identifiers of selection roots. Some of them may be unavailable
        -- (i.e., not in the 'succeeded' map, and hence, not in the list of
        -- declarations attached to the translation unit).
        rootIds :: Set DeclId
        rootIds = DeclIndex.keysSet selectedIndex

        -- Identifiers of transitive dependencies including selection roots.
        rootAndTransIds :: Set DeclId
        rootAndTransIds =
          UseDeclGraph.getTransitiveDeps useDeclGraph $
            Set.toList rootIds

        -- Identifiers of transitive dependencies excluding selection roots.
        strictTransIds :: Set DeclId
        strictTransIds = rootAndTransIds \\ rootIds

        -- Identifiers of all selected declarations.
        selectedIds :: Set DeclId
        -- Identifiers of (additional) transitive dependencies selected due to
        -- program slicing. This is the only point where we differentiate
        -- between selection with or without program slicing.
        additionalSelectedTransIds :: Set DeclId
        (selectedIds, additionalSelectedTransIds) = case config.programSlicing of
          DisableProgramSlicing -> (rootIds        , Set.empty)
          EnableProgramSlicing  -> (rootAndTransIds, strictTransIds)

        getTransitiveSelectability :: DeclId -> TransitiveSelectability
        getTransitiveSelectability x
          | Map.null unusabilityReasons = TransitivelySelectable
          | otherwise                   = TransitivelyUnselectable unusabilityReasons
          where
            transDeps :: Set DeclId
            transDeps = UseDeclGraph.getStrictTransitiveDeps useDeclGraph [x]

            unusables :: Map DeclId Unselectable
            unusables =
              Unselectable <$> DeclIndex.getUnusables index transDeps

            nonselected :: Map DeclId Unselectable
            nonselected  =
              Map.fromSet (const UnselectableNotSelected) $
                transDeps \\ selectedIds

            unusabilityReasons :: Map DeclId Unselectable
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

        selectDecl :: Decl -> Maybe Decl
        selectDecl =
          selectDeclWith
            selectedIds
            getTransitiveSelectability

        unitDecls :: [Decl]
        unitDecls = map coercePass unit.decls

        selectedUnitDecls  :: [Decl]
        selectedUnitDecls = mapMaybe selectDecl unitDecls

        selectMsgs :: [Msg Select]
        selectMsgs =
          getSelectMsgs
            rootIds
            additionalSelectedTransIds
            getTransitiveSelectability
            index

        unitSelect :: C.TranslationUnit Select
        unitSelect = C.TranslationUnit {
                decls        = selectedUnitDecls
              , includeGraph = unit.includeGraph
              , ann          = unit.ann
              }

        -- If there were no predicate matches we issue a warning to the user.
        noDeclarationsMatchedMsg :: [Msg Select]
        noDeclarationsMatchedMsg = [
            WithLocationInfo{
                loc = LocationUnavailable
              , msg = SelectNoDeclarationsMatched
              }
          | Set.null rootIds
          ]

        msgs :: [Msg Select]
        msgs =
          concat [
              selectMsgs
            , getDelayedMsgs selectedIndex
            , noDeclarationsMatchedMsg
            ]

    in  ( unitSelect
        , sortSelectMsgs unit.includeGraph msgs
        )
  where
    index :: DeclIndex
    index = unit.ann.declIndex

    useDeclGraph :: UseDeclGraph
    useDeclGraph = unit.ann.useDeclGraph

    match :: Match
    match name loc availability = parsed && selected
      where
        -- We have parsed some declarations that are required for scoping but
        -- that actually do not match the parse predicate. We want to avoid
        -- selecting these declarations. This is only problematic if the select
        -- predicate is wider than the parse predicate (rarely the case).
        parsed, selected :: Bool
        parsed =
          matchParse
            isMainHeader
            isInMainHeaderDir
            (singleLocPath loc)
            config.parsePredicate
        selected =
          matchSelect
            isMainHeader
            isInMainHeaderDir
            (singleLocPath loc)
            name
            availability
            config.selectPredicate

{-------------------------------------------------------------------------------
  Filter list of declarations
-------------------------------------------------------------------------------}

selectDeclWith ::
  -- | Selected declaration IDs.
     Set DeclId
  -> (DeclId -> TransitiveSelectability)
  -> Decl
  -> Maybe Decl
selectDeclWith
  selectedIds
  getTransitiveSelectability
  decl =
    case (isSelected, transitiveSelectability) of
      (True, TransitivelySelectable) -> Just decl
      _otherwise                     -> Nothing
  where
    declId :: DeclId
    declId = decl.info.id.cName

    isSelected :: Bool
    isSelected = Set.member declId selectedIds

    transitiveSelectability :: TransitiveSelectability
    transitiveSelectability = getTransitiveSelectability declId

{-------------------------------------------------------------------------------
  Select traces
-------------------------------------------------------------------------------}

getSelectMsgs ::
  -- | Selection roots.
     Set DeclId
  -- | Additionally selected transitive dependencies (non-empty when program
  --   slicing is enabled).
  -> Set DeclId
  ->(DeclId -> TransitiveSelectability)
  -> DeclIndex
  -> [Msg Select]
getSelectMsgs
  rootIds
  additionalSelectedTransIds
  getTransitiveSelectability
  declIndex
  = concatMap (uncurry aux) $ DeclIndex.toList declIndex
  where
    aux :: DeclId -> Entry -> [Msg Select]
    aux =
      getSelectMsgsDeclId
        getTransitiveSelectability
        declIndex
        rootIds
        additionalSelectedTransIds

getSelectMsgsDeclId ::
    (DeclId -> TransitiveSelectability)
  -> DeclIndex
  -- | Selection roots.
  -> Set DeclId
  -- | Additionally selected transitive dependencies (non-empty when program
  --   slicing is enabled).
  -> Set DeclId
  -> DeclId
  -> Entry
  -> [Msg Select]
getSelectMsgsDeclId
  getTransitiveSelectability
  declIndex
  rootIds
  additionalSelectedTransIds
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
      Set.member declId additionalSelectedTransIds
    transitiveSelectability = getTransitiveSelectability declId

    getMsgsFor :: SelectReason -> [Msg Select]
    getMsgsFor selectReason = concat [
          [ withLoc $ SelectStatusInfo (Selected selectReason) ]
        , [ withLoc $ SelectDeprecated selectReason | isDeprecated ]
        ]

    loc :: [SingleLoc]
    loc = DeclIndex.entryToLoc entry

    withLoc :: a -> WithLocationInfo a
    withLoc x = WithLocationInfo{
                  loc = declIdLocationInfo declId loc
                , msg = x
                }

    getUnavailMsg :: SelectReason -> Map DeclId Unselectable -> Maybe (Msg Select)
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

getDelayedMsgs :: DeclIndex -> [Msg Select]
getDelayedMsgs = concatMap (uncurry aux) . DeclIndex.toList
  where
    aux :: DeclId -> Entry -> [Msg Select]
    aux declId = \case
      UsableE e -> case e of
        UsableSuccess success ->
          [ WithLocationInfo{
                loc = declIdLocationInfo declId [success.decl.info.loc]
              , msg = SelectParseSuccess x
              }
          | x <- success.delayedParseMsgs
          ]
        UsableExternal   -> []
      UnusableE e -> case e of
        UnusableParseNotAttempted loc xs ->
          [ WithLocationInfo{
                loc = declIdLocationInfo declId [loc]
              , msg = SelectParseNotAttempted x
              }
          | x <- NonEmpty.toList xs
          ]
        UnusableParseFailure loc x -> List.singleton WithLocationInfo{
            loc = declIdLocationInfo declId [loc]
          , msg = SelectParseFailure x
          }
        UnusableConflict x -> List.singleton WithLocationInfo{
            loc = declIdLocationInfo declId (Conflict.toList x)
          , msg = SelectConflict
          }
        UnusableMangleNamesFailure loc x -> List.singleton WithLocationInfo{
            loc = declIdLocationInfo declId [loc]
          , msg = SelectMangleNamesFailure x
          }
        UnusableFailedMacro x -> List.singleton WithLocationInfo{
            loc = declIdLocationInfo x.name [x.loc]
          , msg = SelectMacroFailure x.macroError
          }
        UnusableOmitted{} ->
          []
      -- Parse messages are unavailable for squashed entries. We are OK with
      -- this; instead we have issued a notice in 'MangleNames' that the typedef
      -- was squashed.
      SquashedE{} -> []

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
      Nothing -> panicPure $ "unknown source path: " <> show z
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

compareMsgs :: Map SourcePath Int -> Msg Select -> Msg Select -> Ordering
compareMsgs orderMap x y =
  case (locationInfoLocs x.loc, locationInfoLocs y.loc) of
    (lx : __, ly : _) -> compareSingleLocs orderMap lx ly
    -- Sort messages not attached to a declaration to the back.
    ([] , _ ) -> GT
    (_  , []) -> LT

sortSelectMsgs :: IncludeGraph -> [Msg Select] -> [Msg Select]
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
selectDeclIndex :: DeclUseGraph -> Match -> DeclIndex -> DeclIndex
selectDeclIndex declUseGraph p declIndex =
    DeclIndex.filter matchEntry declIndex
  where
    matchEntry :: DeclId -> Entry -> Bool
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

    matchDeclId :: DeclId -> Bool
    matchDeclId declId =
        case DeclIndex.lookupEntry declId declIndex of
          Just entry -> matchEntry declId entry
          Nothing    -> panicPure $ "unknown " ++ show declId

    -- Extract info from 'Entry' needed to match against the selection predicate
    --
    -- Returns 'Nothing' for external or omitted declarations.
    -- Returns 'Just _' for squashed declarations. Those can still be selected.
    -- Returns multiple locations only for conflicts.
    entryInfo :: Entry -> Maybe ([SingleLoc], C.Availability)
    entryInfo = \case
        UsableE e -> case e of
          UsableSuccess success ->
            let info = success.decl.info
            in Just ([info.loc], info.availability)
          UsableExternal ->
            Nothing
        UnusableE e -> case e of
          UnusableParseNotAttempted loc _ ->
            Just ([loc], C.Available)
          UnusableParseFailure loc _ ->
            Just ([loc], C.Available)
          UnusableConflict conflict ->
            Just (Conflict.toList conflict, C.Available)
          UnusableMangleNamesFailure loc _ ->
            Just ([loc], C.Available)
          UnusableFailedMacro failedMacro ->
            Just ([failedMacro.loc], C.Available)
          UnusableOmitted{} ->
            Nothing
        SquashedE e -> Just ([e.typedefLoc], C.Available)

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
    matchAnon :: DeclId -> Bool
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
           -- Unused anonymous declarations have already been removed in the
           -- @AssignAnonIds@ pass.
           panicPure "Unexpected unused anonymous declaration"
