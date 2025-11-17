module HsBindgen.Frontend.Pass.Select (
    selectDecls
  ) where

import Data.Map.Strict qualified as Map
import Data.Set ((\\))
import Data.Set qualified as Set

import Clang.HighLevel.Types

import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex (..), Match,
                                              selectDeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
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

{-------------------------------------------------------------------------------
  Data types
-------------------------------------------------------------------------------}

-- Identifier of declaration.
type DeclId = C.QualPrelimDeclId

-- Declaration itself.
type Decl = C.Decl Select

-- | We have to treat with two notions of availability here:
--
-- 1. A declaration can be available because it is in the list of declarations
--    attached to the translation unit.
--
-- 2. A declaration is available if the declaration itself and all of its
--    transitive dependencies are available.
--
-- @TransitiveAvailability@ deals with the second type.
data TransitiveAvailability =
    TransitivelyAvailable
    -- | For each transitive dependency, we try to give the root cause of
    -- unavailability.
    --
    -- We should use a "non-empty" map here.
  | TransitivelyUnavailable (Map DeclId UnavailabilityReason)
  deriving stock (Show, Eq, Ord)

instance Semigroup TransitiveAvailability where
  TransitivelyAvailable      <> x                    = x
  x                         <> TransitivelyAvailable = x
  TransitivelyUnavailable x <> TransitivelyUnavailable y =
    TransitivelyUnavailable $ Map.unionWith min x y

instance Monoid TransitiveAvailability where
  mempty = TransitivelyAvailable

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
    let -- | Directly match the select predicate on the 'DeclIndex', obtaining
        -- information about succeeded _and failed_ selection roots.
        selectedIndex :: DeclIndex
        selectedIndex = selectDeclIndex match index

        -- | Identifiers of selection roots. Some of them may be unavailable
        -- (i.e., not in the 'succeeded' map, and hence, not in the list of
        -- declarations attached to the translation unit).
        rootIds :: Set DeclId
        rootIds = DeclIndex.keysSet selectedIndex

        -- | Identifiers of transitive dependencies including selection roots.
        rootAndTransIds :: Set DeclId
        rootAndTransIds =
          UseDeclGraph.getTransitiveDeps useDeclGraph $
            Set.toList rootIds

        -- | Identifiers of transitive dependencies excluding selection roots.
        strictTransIds :: Set DeclId
        strictTransIds = rootAndTransIds \\ rootIds

        -- | Identifiers of all selected declarations.
        selectedIds :: Set DeclId
        -- | Identifiers of (additional) transitive dependencies selected due to
        -- | program slicing.
        additionalSelectedTransIds :: Set DeclId
        (selectedIds, additionalSelectedTransIds) = case selectConfigProgramSlicing of
          DisableProgramSlicing -> (rootIds        , Set.empty)
          EnableProgramSlicing  -> (rootAndTransIds, strictTransIds)

        getTransitiveAvailability :: DeclId -> TransitiveAvailability
        getTransitiveAvailability x = mconcat [
              availabilityFromSet notAttemptedTransDeps UnavailableParseNotAttempted
            , availabilityFromSet failedTransDeps       UnavailableParseFailed
            , availabilityFromSet failedMacrosTransDeps UnavailableHandleMacrosFailed
            , availabilityFromSet nonselectedTransDeps  UnavailableNotSelected
            ]
          where
            -- We only check the transitive availability of declarations in the
            -- list of declarations attached to the translation unit. That is,
            -- there is no need to process strict transitive dependencies only
            -- (i.e., to remove 'x' from 'transDeps').
            transDeps :: Set DeclId
            transDeps = UseDeclGraph.getTransitiveDeps useDeclGraph [x]

            notAttemptedTransDeps, failedTransDeps :: Set DeclId
            failedMacrosTransDeps, nonselectedTransDeps :: Set DeclId
            notAttemptedTransDeps = Set.intersection transDeps notAttempted
            failedTransDeps       = Set.intersection transDeps failed
            failedMacrosTransDeps = Set.intersection transDeps failedMacros
            nonselectedTransDeps  = transDeps \\ selectedIds

            availabilityFromSet ::
                     Set DeclId
                  -> UnavailabilityReason
                  -> TransitiveAvailability
            availabilityFromSet xs r =
              if Set.null xs then
                TransitivelyAvailable
              else
                TransitivelyUnavailable $ Map.fromSet (const r) xs

        selectDecl :: Decl -> (Maybe Decl, [Msg Select])
        selectDecl =
          selectDeclWith getTransitiveAvailability rootIds additionalSelectedTransIds

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

    in  (    unitSelect
        ,    selectMsgs
          -- If there were no predicate matches we issue a warning to the user.
          ++ [ SelectNoDeclarationsMatched | Set.null rootIds ]
          ++ getDelayedMsgs      selectedIndex
        )
  where
    notAttempted, failed, failedMacros :: Set DeclId
    notAttempted = Map.keysSet index.notAttempted
    failed       = Map.keysSet index.failed
    failedMacros = Map.keysSet index.failedMacros

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
    (DeclId -> TransitiveAvailability)
  -- | Selection roots.
  -> Set DeclId
  -- | Additionally selected transitive dependencies (non-empty when program
  --   slicing is enabled).
  -> Set DeclId
  -> Decl
  -> (Maybe Decl, [Msg Select])
selectDeclWith getTransitiveAvailability rootIds additionalSelectedTransIds decl =
    case ( isSelectedRoot
         , isAdditionalSelectedTransDep
         , transitiveAvailability ) of
      -- Declaration is a selection root.
      (True, False, TransitivelyAvailable) ->
        (Just decl, getSelMsgs SelectionRoot)
      (True, False, TransitivelyUnavailable rs) ->
        (Nothing, getUnavailMsgs SelectionRoot rs)
      -- Declaration is an additionally selected transitive dependency.
      (False, True, TransitivelyAvailable) ->
        (Just decl, getSelMsgs TransitiveDependency)
      (False, True, TransitivelyUnavailable rs) ->
        (Nothing, getUnavailMsgs TransitiveDependency rs)
      -- Declaration is not selected.
      (False, False, _) ->
        (Nothing, [SelectStatusInfo NotSelected decl])
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
      in  SelectStatusInfo (Selected selectReason) decl : selectDepr

    getUnavailMsgs :: SelectReason -> Map DeclId UnavailabilityReason -> [Msg Select]
    getUnavailMsgs selectReason unavailReason =
      [ TransitiveDependencyOfDeclarationUnavailable selectReason r decl
      | r <- Map.toList unavailReason ]

    isDeprecated :: C.DeclInfo Select -> Bool
    isDeprecated info = case C.declAvailability info of
      C.Deprecated -> True
      _            -> False

{-------------------------------------------------------------------------------
  Parse trace messages
-------------------------------------------------------------------------------}

getDelayedMsgs :: DeclIndex -> [Msg Select]
getDelayedMsgs DeclIndex{..} = concat [
    getMsgss (map SelectParseSuccess . psAttachedMsgs) succeeded
  , getMsgs   SelectParseNotAttempted                  notAttempted
  , getMsgs   SelectParseFailure                       failed
  , getMsgs   SelectMacroFailure                       failedMacros
  ]
  where
    getMsgs :: (a -> b) -> Map k a -> [b]
    getMsgs f = map f . Map.elems

    getMsgss :: (a -> [b]) -> Map k a -> [b]
    getMsgss f = concatMap f . Map.elems
