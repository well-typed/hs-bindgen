module HsBindgen.Frontend.Pass.Select (
    selectDecls
  ) where

import Data.List (sortBy)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Set ((\\))
import Data.Set qualified as Set

import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex, Entry (..), Match,
                                              Unusable (..), Usable (..))
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.HandleMacros.Error
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Data types
-------------------------------------------------------------------------------}

-- Identifier of declaration.
type DeclId = C.DeclId AssignAnonIds

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
  | TransitivelyUnselectable (Map DeclId Unselectable)
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Select
-------------------------------------------------------------------------------}

selectDecls ::
     IsMainHeader
  -> IsInMainHeaderDir
  -> Config Select
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
        selectedIndex = DeclIndex.selectDeclIndex match index

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
        (selectedIds, additionalSelectedTransIds) = case selectConfigProgramSlicing of
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
              UnselectableBecauseUnusable <$> DeclIndex.getUnusables index transDeps

            nonselected :: Map DeclId Unselectable
            nonselected  =
              Map.fromSet (const TransitiveDependencyNotSelected) $
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
          ++ [ SelectNoDeclarationsMatched | Set.null rootIds ]

    in  ( unitSelect
        , sortSelectMsgs unitIncludeGraph msgs
        )
  where
    index :: DeclIndex
    index = unitAnn.declIndex

    useDeclGraph :: UseDeclGraph
    useDeclGraph = unitAnn.declUseDecl

    match :: Match
    match = go
      where
        go :: DeclId -> SingleLoc -> C.Availability -> Bool
        go declId loc availability =
            let -- We have parsed some declarations that are required for
                -- scoping but that actually do not match the parse predicate.
                -- We want to avoid selecting these declarations. This is only
                -- problematic if the select predicate is wider than the parse
                -- predicate (which is seldom the case).
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
                    declId.name
                    availability
                    selectConfigPredicate
            in parsed && selected

{-------------------------------------------------------------------------------
  Fold declarations
-------------------------------------------------------------------------------}

selectDeclWith ::
    (DeclId -> TransitiveSelectability)
  -> DeclIndex
  -- | Selection roots.
  -> Set DeclId
  -- | Additionally selected transitive dependencies (non-empty when program
  --   slicing is enabled).
  -> Set DeclId
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
        (Nothing, [SelectStatusInfo decl NotSelected])
      -- Declaration is a selection root and a transitive dependency. This
      -- should be impossible and we consider it a bug.
      (True, True, _) ->
        panicPure $
          "Declaration is selection root and transitive dependency: "
          ++ show decl.declInfo
  where
    declId :: DeclId
    declId = coercePass decl.declInfo.declId

    -- We check three conditions:
    isSelectedRoot = Set.member declId rootIds
    -- These are also always strict transitive dependencies.
    isAdditionalSelectedTransDep =
      Set.member declId additionalSelectedTransIds
    transitiveSelectability = getTransitiveSelectability declId

    getSelMsgs :: SelectReason -> [Msg Select]
    getSelMsgs selectReason =
      let selectDepr   = [ SelectDeprecated decl | isDeprecated decl.declInfo ]
      in  SelectStatusInfo decl (Selected selectReason) : selectDepr

    getUnavailMsgs :: SelectReason -> Map DeclId Unselectable -> [Msg Select]
    getUnavailMsgs selectReason unavailReason =
      [ case r of
          UnselectableBecauseUnusable u ->
            TransitiveDependencyOfDeclarationUnusable
              decl selectReason i u (DeclIndex.lookupLoc i declIndex)
          TransitiveDependencyNotSelected ->
            TransitiveDependencyOfDeclarationNotSelected
              decl selectReason i   (DeclIndex.lookupLoc i declIndex)
      | (i, r) <- Map.toList unavailReason
      ]

    isDeprecated :: C.DeclInfo Select -> Bool
    isDeprecated info = case C.declAvailability info of
      C.Deprecated -> True
      _            -> False

{-------------------------------------------------------------------------------
  Parse trace messages
-------------------------------------------------------------------------------}

getDelayedMsgs :: DeclIndex -> [Msg Select]
getDelayedMsgs = concatMap (uncurry getSelectMsg) . DeclIndex.toList
  where
    getSelectMsg :: C.DeclId AssignAnonIds -> Entry -> [SelectMsg]
    getSelectMsg declId = \case
      UsableE e -> case e of
        UsableSuccess success ->
          map (SelectParseSuccess declId success.decl.declInfo.declLoc) $
            success.delayedParseMsgs
        UsableExternal  -> []
      UnusableE e -> case e of
        UnusableParseNotAttempted loc xs ->
          map (SelectParseNotAttempted declId loc) $ NonEmpty.toList xs
        UnusableParseFailure loc x ->
          [SelectParseFailure declId loc x]
        UnusableConflict x ->
          [SelectConflict x]
        UnusableFailedMacro x ->
          [SelectMacroFailure x]
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

getSingleLoc :: Msg Select -> Maybe SingleLoc
getSingleLoc = \case
  SelectStatusInfo d _                                   -> fromD d
  TransitiveDependencyOfDeclarationUnusable    d _ _ _ _ -> fromD d
  TransitiveDependencyOfDeclarationNotSelected d _ _   _ -> fromD d
  SelectDeprecated d                                     -> fromD d
  SelectParseSuccess _declId loc _                       -> Just loc
  SelectParseNotAttempted _declId loc _                  -> Just loc
  SelectParseFailure _declId loc _                       -> Just loc
  SelectConflict c                                       -> Just $ getMinimumLoc c
  SelectMacroFailure failedMacro                         -> Just $ failedMacro.loc
  SelectNoDeclarationsMatched                            -> Nothing
  where
    fromD = Just . C.declLoc . C.declInfo

compareMsgs :: Map SourcePath Int -> Msg Select -> Msg Select -> Ordering
compareMsgs orderMap x y =
  case (getSingleLoc x, getSingleLoc y) of
    (Just lx, Just ly) -> compareSingleLocs orderMap lx ly
    -- Sort messages not attached to a declaration to the back.
    (Nothing, _      ) -> GT
    (_      , Nothing) -> LT

sortSelectMsgs :: IncludeGraph -> [Msg Select] -> [Msg Select]
sortSelectMsgs includeGraph = sortBy (compareMsgs orderMap)
  where
    -- Compute the order map once.
    orderMap :: Map SourcePath Int
    orderMap = IncludeGraph.toOrderMap includeGraph
