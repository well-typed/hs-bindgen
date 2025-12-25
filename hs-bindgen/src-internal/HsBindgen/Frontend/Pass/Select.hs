module HsBindgen.Frontend.Pass.Select (
    selectDecls
  ) where

import Data.List (sortBy)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
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
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict qualified as Conflict
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.HandleMacros.Error
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
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
    UnselectableBecauseUnusable Unusable
    -- | We (i.e., `hs-bindgen`) can not select a declaration because one of its
    --   dependencies has not been selected by the user.
  | TransitiveDependencyNotSelected
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
     IsMainHeader
  -> IsInMainHeaderDir
  -> SelectConfig
  -> C.TranslationUnit ResolveBindingSpecs
  -> (C.TranslationUnit Select, [Msg Select])
selectDecls
  isMainHeader
  isInMainHeaderDir
  SelectConfig{..}
  C.TranslationUnit{..} =
    let -- Directly match the select predicate on the 'DeclIndex', obtaining
        -- information about succeeded _and failed_ selection roots.
        selectedIndex :: DeclIndex
        selectedIndex = selectDeclIndex unitAnn.declDeclUse match index

        -- Identifiers of selection roots. Some of them may be unavailable
        -- (i.e., not in the 'succeeded' map, and hence, not in the list of
        -- declarations attached to the translation unit).
        rootIds :: Set C.DeclId
        rootIds = DeclIndex.keysSet selectedIndex

        -- Identifiers of transitive dependencies including selection roots.
        rootAndTransIds :: Set C.DeclId
        rootAndTransIds =
          UseDeclGraph.getTransitiveDeps useDeclGraph $
            Set.toList rootIds

        -- Identifiers of transitive dependencies excluding selection roots.
        strictTransIds :: Set C.DeclId
        strictTransIds = rootAndTransIds \\ rootIds

        -- Identifiers of all selected declarations.
        selectedIds :: Set C.DeclId
        -- Identifiers of (additional) transitive dependencies selected due to
        -- program slicing. This is the only point where we differentiate
        -- between selection with or without program slicing.
        additionalSelectedTransIds :: Set C.DeclId
        (selectedIds, additionalSelectedTransIds) = case selectConfigProgramSlicing of
          DisableProgramSlicing -> (rootIds        , Set.empty)
          EnableProgramSlicing  -> (rootAndTransIds, strictTransIds)

        getTransitiveSelectability :: C.DeclId -> TransitiveSelectability
        getTransitiveSelectability x
          | Map.null unusabilityReasons = TransitivelySelectable
          | otherwise                   = TransitivelyUnselectable unusabilityReasons
          where
            transDeps :: Set C.DeclId
            transDeps = UseDeclGraph.getStrictTransitiveDeps useDeclGraph [x]

            unusables :: Map C.DeclId Unselectable
            unusables =
              UnselectableBecauseUnusable <$> DeclIndex.getUnusables index transDeps

            nonselected :: Map C.DeclId Unselectable
            nonselected  =
              Map.fromSet (const TransitiveDependencyNotSelected) $
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
              (TransitiveDependencyNotSelected, UnselectableBecauseUnusable u  ) ->
                case u of
                  UnusableParseNotAttempted{} -> r
                  UnusableOmitted{}           -> r
                  _otherReason                -> l
              (UnselectableBecauseUnusable u  , TransitiveDependencyNotSelected) ->
                case u of
                  UnusableParseNotAttempted{} -> l
                  UnusableOmitted{}           -> l
                  _otherReason                -> r
              (_, _) -> l

        selectDecl :: Decl -> (Maybe Decl, [Msg Select])
        selectDecl =
          selectDeclWith
            getTransitiveSelectability
            index
            rootIds
            additionalSelectedTransIds

        availableDecls :: [Decl]
        availableDecls = map coercePass unitDecls

        -- Fold available declarations and collect selected declarations and
        -- trace messages.
        unitDecls'  :: [Decl]
        selectMsgs :: [Msg Select]
        (unitDecls', selectMsgs) =
          bimap catMaybes concat $ unzip $
            map selectDecl availableDecls

        unitSelect :: C.TranslationUnit Select
        unitSelect = C.TranslationUnit {
                C.unitDecls = unitDecls'
              , C.unitIncludeGraph = unitIncludeGraph
              , C.unitAnn = unitAnn
              }

        msgs :: [Msg Select]
        msgs =
             selectMsgs
          ++ getDelayedMsgs selectedIndex
          -- If there were no predicate matches we issue a warning to the user.
          ++ [ WithLocationInfo{
                   loc = LocationUnavailable
                 , msg = SelectNoDeclarationsMatched
                 }
             | Set.null rootIds
             ]

    in  ( unitSelect
        , sortSelectMsgs unitIncludeGraph msgs
        )
  where
    index :: DeclIndex
    index = unitAnn.declIndex

    useDeclGraph :: UseDeclGraph
    useDeclGraph = unitAnn.declUseDecl

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
            selectConfigParsePredicate
        selected =
          matchSelect
            isMainHeader
            isInMainHeaderDir
            (singleLocPath loc)
            name
            availability
            selectConfigPredicate

{-------------------------------------------------------------------------------
  Fold declarations
-------------------------------------------------------------------------------}

selectDeclWith ::
    (C.DeclId -> TransitiveSelectability)
  -> DeclIndex
  -- | Selection roots.
  -> Set C.DeclId
  -- | Additionally selected transitive dependencies (non-empty when program
  --   slicing is enabled).
  -> Set C.DeclId
  -> Decl
  -> (Maybe Decl, [Msg Select])
selectDeclWith
  getTransitiveSelectability
  declIndex
  rootIds
  additionalSelectedTransIds
  decl =
    case ( isSelectedRoot
         , isAdditionalSelectedTransDep
         , transitiveSelectability ) of
      -- Declaration is a selection root.
      (True, False, TransitivelySelectable) ->
        (Just decl, getSelMsgs SelectionRoot)
      (True, False, TransitivelyUnselectable rs) ->
        (Nothing, getUnavailMsgs SelectionRoot rs)
      -- Declaration is an additionally selected transitive dependency.
      (False, True, TransitivelySelectable) ->
        (Just decl, getSelMsgs TransitiveDependency)
      (False, True, TransitivelyUnselectable rs) ->
        (Nothing, getUnavailMsgs TransitiveDependency rs)
      -- Declaration is not selected.
      (False, False, _) ->
        let selectMsg = WithLocationInfo{
                loc = declLocationInfo decl
              , msg = SelectStatusInfo NotSelected
              }
        in (Nothing, [selectMsg])
      -- Declaration is a selection root and a transitive dependency. This
      -- should be impossible and we consider it a bug.
      (True, True, _) ->
        panicPure $
          "Declaration is selection root and transitive dependency: "
          ++ show decl.declInfo
  where
    declId :: C.DeclId
    declId = decl.declInfo.declId

    -- We check three conditions:
    isSelectedRoot = Set.member declId rootIds
    -- These are also always strict transitive dependencies.
    isAdditionalSelectedTransDep =
      Set.member declId additionalSelectedTransIds
    transitiveSelectability = getTransitiveSelectability declId

    getSelMsgs :: SelectReason -> [Msg Select]
    getSelMsgs selectReason = concat [
          [ WithLocationInfo{
                loc = declLocationInfo decl
              , msg = SelectStatusInfo (Selected selectReason)
              }
          ]
        , [ WithLocationInfo{
                loc = declLocationInfo decl
              , msg = SelectDeprecated
              }
          | isDeprecated decl.declInfo
          ]
        ]

    getUnavailMsgs :: SelectReason -> Map C.DeclId Unselectable -> [Msg Select]
    getUnavailMsgs selectReason unavailReason =
      [ WithLocationInfo{
            loc = declLocationInfo decl
          , msg =
              case r of
                UnselectableBecauseUnusable u ->
                  TransitiveDependencyOfDeclarationUnusable
                    selectReason
                    i
                    u
                    (DeclIndex.lookupLoc i declIndex)
                TransitiveDependencyNotSelected ->
                  TransitiveDependencyOfDeclarationNotSelected
                    selectReason
                    i
                    (DeclIndex.lookupLoc i declIndex)
          }
      | (i, r) <- Map.toList unavailReason
      ]

    isDeprecated :: C.DeclInfo Select -> Bool
    isDeprecated info = case C.declAvailability info of
      C.Deprecated -> True
      _            -> False

{-------------------------------------------------------------------------------
  Parse trace messages
-------------------------------------------------------------------------------}

declLocationInfo :: Decl -> LocationInfo
declLocationInfo decl =
    declIdLocationInfo decl.declInfo.declId [decl.declInfo.declLoc]

getDelayedMsgs :: DeclIndex -> [Msg Select]
getDelayedMsgs = concatMap (uncurry getSelectMsg) . DeclIndex.toList
  where
    getSelectMsg :: C.DeclId -> Entry -> [Msg Select]
    getSelectMsg declId = \case
      UsableE e -> case e of
        UsableSuccess success ->
          [ WithLocationInfo{
                loc = declIdLocationInfo declId [success.decl.declInfo.declLoc]
              , msg = SelectParseSuccess x
              }
          | x <- success.delayedParseMsgs
          ]
        UsableExternal   -> []
        UsableSquashed{} -> []
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
            loc = declIdLocationInfo declId (Conflict.getLocs x)
          , msg = SelectConflict
          }
        UnusableFailedMacro x -> List.singleton WithLocationInfo{
            loc = declIdLocationInfo x.name [x.loc]
          , msg = SelectMacroFailure x.macroError
          }
        UnusableOmitted{} ->
          []

{-------------------------------------------------------------------------------
  Sort messages
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
    matchEntry :: C.DeclId -> Entry -> Bool
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
          Nothing    -> panicPure $ "unknown " ++ show declId

    -- Extract info from 'Entry' needed to match against the selection predicate
    --
    -- Returns 'Nothing' for external or omitted declarations.
    -- Returns multiple locations only for conflicts.
    entryInfo :: Entry -> Maybe ([SingleLoc], C.Availability)
    entryInfo = \case
        UsableE e -> case e of
          UsableSuccess success ->
            let info = success.decl.declInfo
            in Just ([info.declLoc], info.declAvailability)
          UsableExternal ->
            Nothing
          UsableSquashed{} ->
            Nothing
        UnusableE e -> case e of
          UnusableParseNotAttempted loc _ ->
            Just ([loc], C.Available)
          UnusableParseFailure loc _ ->
            Just ([loc], C.Available)
          UnusableConflict x ->
            Just (getLocs x, C.Available)
          UnusableFailedMacro failedMacro ->
            Just ([failedMacro.loc], C.Available)
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
           -- Unused anonymous declarations have already been removed in the
           -- @AssignAnonIds@ pass.
           panicPure "Unexpected unused anonymous declaration"
