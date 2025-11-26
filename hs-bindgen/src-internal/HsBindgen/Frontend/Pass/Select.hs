module HsBindgen.Frontend.Pass.Select (
    selectDecls
  ) where

import Data.List (sortBy)
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
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.HandleMacros.Error
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Data types
-------------------------------------------------------------------------------}

-- Identifier of declaration.
type DeclId = C.QualPrelimDeclId

-- Declaration itself.
type Decl = C.Decl Select

-- | We have to treat with two notions of usability here:
--
-- 1. A declaration can be usable because it is in the list of declarations
--    attached to the translation unit.
--
-- 2. A declaration is available if the declaration itself and all of its
--    transitive dependencies are available.
--
-- @TransitiveUsability@ deals with the second type.
--
-- (We avoid the term available, because it is overloaded with Clang's
-- CXAvailabilityKind).
data TransitiveUsability =
    TransitivelyUsable
    -- | For each transitive dependency, we try to give the root cause of
    -- unavailability.
    --
    -- We should use a "non-empty" map here.
  | TransitivelyUnusable (Map DeclId UnusabilityReason)
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Select
-------------------------------------------------------------------------------}

selectDecls ::
     HasCallStack
  => IsMainHeader
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

        getTransitiveAvailability :: DeclId -> TransitiveUsability
        getTransitiveAvailability x
          | Map.null unusabilityReasons = TransitivelyUsable
          | otherwise                   = TransitivelyUnusable unusabilityReasons
          where
            -- We check the transitive usability of declarations in the list of
            -- declarations attached to the translation unit (these have been
            -- successfully reified). That is, there is no need to process
            -- strict transitive dependencies only (i.e., to remove 'x' from
            -- 'transDeps').
            transDeps :: Set DeclId
            transDeps = UseDeclGraph.getTransitiveDeps useDeclGraph [x]

            unusables :: Map DeclId UnusabilityReason
            unusables = UnusableR <$> DeclIndex.getUnusables index transDeps

            nonselected :: Map DeclId UnusabilityReason
            nonselected  =
              Map.fromSet (const UnusableNotSelected) $
                transDeps \\ selectedIds

            unusabilityReasons :: Map DeclId UnusabilityReason
            unusabilityReasons =
              Map.unionWith
                getMostNaturalUnusabilityReason
                unusables
                nonselected

            getMostNaturalUnusabilityReason ::
              UnusabilityReason -> UnusabilityReason -> UnusabilityReason
            getMostNaturalUnusabilityReason l r = case (l,r) of
              (UnusableNotSelected, UnusableR u        ) -> case u of
                UnusableParseNotAttempted _ -> r
                _otherReason                -> l
              (UnusableR u        , UnusableNotSelected) -> case u of
                UnusableParseNotAttempted _ -> l
                _otherReason                -> r
              (_                  , _                  ) -> l

        selectDecl :: Decl -> (Maybe Decl, [Msg Select])
        selectDecl =
          selectDeclWith
            getTransitiveAvailability
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
    match = \declId -> go declId declId
      where
        -- We compare the use sites of anonymous declarations with the original
        -- @declId@, so we can detect cycles involving anonymous declarations in
        -- the use-decl graph. We believe these cycles can not exist.
        go origDeclId declId loc availability = case declId of
            C.QualPrelimDeclIdNamed name kind ->
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
                      (C.QualName name kind)
                      availability
                      selectConfigPredicate
              in parsed && selected
            -- Apply the select predicate to the use site.
            anon@(C.QualPrelimDeclIdAnon{}) -> matchAnon origDeclId anon
            -- Never select builtins.
            C.QualPrelimDeclIdBuiltin _ -> False

        matchAnon :: DeclId -> DeclId -> Bool
        matchAnon origDeclId anon =
          case DeclUseGraph.getUseSites unitAnn.declDeclUse anon of
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
            -- TODO https://github.com/well-typed/hs-bindgen/issues/1273:
            -- Implement trace messages stating why we deselect the anonymous
            -- declarations (e.g., Is the use site an external declaration?).
            Nothing   -> False
            Just decl -> match
                           declIdUseSite
                           (C.declLoc $ C.declInfo decl)
                           (C.declAvailability $ C.declInfo decl)

{-------------------------------------------------------------------------------
  Fold declarations
-------------------------------------------------------------------------------}

selectDeclWith ::
    (DeclId -> TransitiveUsability)
  -> DeclIndex
  -- | Selection roots.
  -> Set DeclId
  -- | Additionally selected transitive dependencies (non-empty when program
  --   slicing is enabled).
  -> Set DeclId
  -> Decl
  -> (Maybe Decl, [Msg Select])
selectDeclWith
  getTransitiveAvailability
  declIndex
  rootIds
  additionalSelectedTransIds
  decl =
    case ( isSelectedRoot
         , isAdditionalSelectedTransDep
         , transitiveAvailability ) of
      -- Declaration is a selection root.
      (True, False, TransitivelyUsable) ->
        (Just decl, getSelMsgs SelectionRoot)
      (True, False, TransitivelyUnusable rs) ->
        (Nothing, getUnavailMsgs SelectionRoot rs)
      -- Declaration is an additionally selected transitive dependency.
      (False, True, TransitivelyUsable) ->
        (Just decl, getSelMsgs TransitiveDependency)
      (False, True, TransitivelyUnusable rs) ->
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
    declId    = C.declOrigQualPrelimDeclId decl
    -- We check three conditions:
    isSelectedRoot = Set.member declId rootIds
    -- These are also always strict transitive dependencies.
    isAdditionalSelectedTransDep =
      Set.member declId additionalSelectedTransIds
    transitiveAvailability = getTransitiveAvailability declId

    getSelMsgs :: SelectReason -> [Msg Select]
    getSelMsgs selectReason =
      let selectDepr   = [ SelectDeprecated decl | isDeprecated decl.declInfo ]
      in  SelectStatusInfo decl (Selected selectReason) : selectDepr

    getUnavailMsgs :: SelectReason -> Map DeclId UnusabilityReason -> [Msg Select]
    getUnavailMsgs selectReason unavailReason =
      [ TransitiveDependencyOfDeclarationUnavailable
          decl selectReason i r (DeclIndex.lookupLoc i declIndex)
      | (i, r) <- Map.toList unavailReason ]

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
    getSelectMsg :: DeclId -> DeclIndex.Entry -> [SelectMsg]
    getSelectMsg declId = \case
      UsableE e -> case e of
        UsableSuccess s -> map SelectParseSuccess $ psAttachedMsgs s
        -- TODO_PR: This case will only happen when we actually match the select
        -- predicate on external declarations.
        UsableExternal  -> [SelectUseExternal declId]
      UnusableE e -> case e of
        UnusableParseNotAttempted x -> [SelectParseNotAttempted x]
        UnusableParseFailure x      -> [SelectParseFailure x]
        UnusableConflict x          -> [SelectConflict x]
        UnusableFailedMacro x       -> [SelectMacroFailure x]
        -- TODO_PR: This case will only happen when we actually match the select
        -- predicate on omitted declarations.
        UnusableOmitted x           -> [SelectOmitted x]

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
  -- TODO_PR: Location info for externals?
  SelectUseExternal{}                                    -> Nothing
  TransitiveDependencyOfDeclarationUnavailable d _ _ _ _ -> fromD d
  SelectDeprecated d                                     -> fromD d
  SelectParseSuccess m                                   -> fromM m
  SelectParseNotAttempted (ParseNotAttempted m)          -> fromM m
  SelectParseFailure      (ParseFailure      m)          -> fromM m
  SelectConflict c                                       -> Just $ getMinimumLoc c
  SelectMacroFailure      (FailedMacro       m)          -> fromM m
  -- TODO_PR: Location info for omissions?
  SelectOmitted{}                                        -> Nothing
  SelectNoDeclarationsMatched                            -> Nothing
  where
    fromD = Just . C.declLoc . C.declInfo
    fromM = Just . loc

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
