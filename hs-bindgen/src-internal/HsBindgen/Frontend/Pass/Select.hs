module HsBindgen.Frontend.Pass.Select (
    selectDecls
  ) where

import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Semigroup.Compat (Semigroup (..))
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

{-------------------------------------------------------------------------------
  Data types
-------------------------------------------------------------------------------}

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
  | TransitivelyUnavailable (NonEmpty UnavailabilityReason)
  deriving stock (Show, Eq, Ord)

instance Semigroup TransitiveAvailability where
  TransitivelyAvailable      <> xs                    = xs
  xs                         <> TransitivelyAvailable = xs
  TransitivelyUnavailable xs <> TransitivelyUnavailable ys =
    TransitivelyUnavailable $ xs <> ys

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

        availableDecls :: [Decl]
        availableDecls = map coercePass unit.unitDecls

        -- Fold available declarations and collect selected declarations and
        -- trace messages.
        Acc {
            selDecls  = selDeclsReversed
          , remaining = unavailableIds
          , msgs      = selectStatusMsgs
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
         -- If there were no predicate matches we issue a warning to the user.
         -- Only warn if there were successfully parsed declarations to select from.
         ++ [ SelectNoDeclarationsMatched
            | Set.null rootIds
            , not (Map.null index.succeeded)
            ]
         ++ getUnavailableMsgs       unavailableIds
         ++ getDelayedParseMsgs      selectedIds index
         ++ getParseNotAttemptedMsgs match index
         ++ getParseFailureMsgs      match index
       )
  where
    getUseSitesTransitively :: Set DeclId -> Set DeclId
    getUseSitesTransitively =
      DeclUseGraph.getUseSitesTransitively declUseGraph . Set.toList

    notAttempted, failed, failedMacros :: Set DeclId
    notAttempted = Map.keysSet index.notAttempted
    failed       = Map.keysSet index.failed
    failedMacros = Map.keysSet index.failedMacros

    -- | Identifiers of declarations that are unavailable because we did not
    -- attempt to parse them or because they depend on declarations we did not
    -- attempt to parse.
    transitivelyUnavailableIdsNotAttemptedParses :: Set DeclId
    transitivelyUnavailableIdsNotAttemptedParses =
      getUseSitesTransitively notAttempted

    -- | Identifiers of declarations that are unavailable because we failed to
    -- parse them or because they depend on declarations we failed to parse.
    transitivelyUnavailableIdsFailedParses :: Set DeclId
    transitivelyUnavailableIdsFailedParses =
        getUseSitesTransitively failed

    -- | Identifiers of declarations that are unavailable because we failed to
    -- parse or type-check the macros or because they depend on declarations for
    -- which we failed to parse or type-check the macros.
    transitivelyUnavailableIdsFailedMacros :: Set DeclId
    transitivelyUnavailableIdsFailedMacros =
        getUseSitesTransitively failedMacros

    getUnavailableTransitiveDeps ::
      Set DeclId -> Set DeclId -> DeclId -> Set DeclId
    getUnavailableTransitiveDeps unavailableRoots unavailableTransitiveDependencies declId
      | Set.member declId unavailableTransitiveDependencies =
          let dependencies = UseDeclGraph.getTransitiveDeps useDeclGraph [declId]
          in  dependencies `Set.intersection` unavailableRoots
      | otherwise = Set.empty

    getTransitiveAvailabilityWith ::
         Set DeclId -> Set DeclId -> (DeclId -> UnavailabilityReason)
      -> DeclId -> TransitiveAvailability
    getTransitiveAvailabilityWith unavailableRoots unavailableTransitiveDependencies reason declId =
      let roots = getUnavailableTransitiveDeps unavailableRoots unavailableTransitiveDependencies declId
      in case NonEmpty.nonEmpty $ Set.toList roots of
        Nothing -> TransitivelyAvailable
        Just xs -> TransitivelyUnavailable $ fmap reason xs

    getTransitiveAvailability :: DeclId -> TransitiveAvailability
    getTransitiveAvailability x = sconcat $
      getNotAttempted x :| [getParseFailed x , getMacroFailed x]
      where
        getNotAttempted = getTransitiveAvailabilityWith
          notAttempted
          transitivelyUnavailableIdsNotAttemptedParses
          UnavailableParseNotAttempted
        getParseFailed = getTransitiveAvailabilityWith
          failed
          transitivelyUnavailableIdsFailedParses
          UnavailableParseFailed
        getMacroFailed = getTransitiveAvailabilityWith
          failedMacros
          transitivelyUnavailableIdsFailedMacros
          UnavailableHandleMacrosFailed

    getUnavailableMsgs :: Set DeclId -> [Msg Select]
    getUnavailableMsgs = map SelectDeclarationUnavailable . Set.toList

    index :: DeclIndex
    index = unit.unitAnn.declIndex

    declUseGraph :: DeclUseGraph
    declUseGraph = unit.unitAnn.declDeclUse

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
            -- TODO https://github.com/well-typed/hs-bindgen/issues/1273: Be
            -- more rigorous in determining the selection status of the use site
            -- when it is not in the `succeeded` map of the `DeclIndex`.
            Nothing   -> False
            Just decl -> match
                           declIdUseSite
                           (C.declLoc $ C.declInfo decl)
                           (C.declAvailability $ C.declInfo decl)

{-------------------------------------------------------------------------------
  Fold declarations
-------------------------------------------------------------------------------}

data Acc = Acc
  { -- | Selected declarations that are transitively available.
    selDecls :: [Decl],
    -- | Identifiers of declarations yet to be selected. After the fold, this
    -- set contains identifiers that were _not_ found in the list of
    -- declarations of the translation unit. These declarations are
    -- _unvailable_, and lead to error traces.
    remaining :: Set DeclId,
    -- | Trace messages.
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
    Foldable.foldl' aux (Acc [] (rootIds `Set.union` transIds) []) decls
  where
    aux :: Acc -> Decl -> Acc
    aux Acc{..} decl =
      let declId    = C.declOrigQualPrelimDeclId decl
          selDecls' = decl : selDecls
          remaining' = Set.delete declId remaining
          -- We check three conditions:
          isSelectionRoot        = Set.member declId rootIds
          isTransitiveDependency = Set.member declId transIds
          transitiveAvailability = getTransitiveAvailability declId
      in
      case ( isSelectionRoot
           , isTransitiveDependency
           , transitiveAvailability ) of
        -- Declaration is a selection root.
        (True, False, TransitivelyAvailable) ->
           Acc selDecls' remaining' (getSelMsgs SelectionRoot decl ++ msgs)
        (True, False, TransitivelyUnavailable rs) ->
          let ms = [ TransitiveDependencyOfDeclarationUnavailable
                       SelectionRoot r decl | r <- toList rs ]
          in  Acc selDecls remaining' (ms ++ msgs)
        -- Declaration is a transitive dependency.
        (False, True, TransitivelyAvailable) ->
          Acc selDecls' remaining' (getSelMsgs TransitiveDependency decl ++ msgs)
        (False, True, TransitivelyUnavailable rs) ->
          let ms = [ TransitiveDependencyOfDeclarationUnavailable
                       TransitiveDependency r decl | r <- toList rs ]
          in  Acc selDecls remaining' (ms ++ msgs)
        -- Declaration is not selected.
        (False, False, _) ->
          let msg = SelectStatusInfo NotSelected decl
          in  Acc selDecls remaining' (msg : msgs)
        -- Declaration is a selection root and a transitive dependency. This
        -- should be impossible and we consider it a bug.
        (True, True, _) ->
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

getParseNotAttemptedMsgs :: Match -> DeclIndex -> [Msg Select]
getParseNotAttemptedMsgs match =
  Foldable.foldl' (Foldable.foldl' addMsg) [] . notAttempted
  where
    addMsg :: [SelectMsg] -> ParseNotAttempted -> [SelectMsg]
    addMsg xs (ParseNotAttempted i l a r) =
      [ SelectParseNotAttempted i l r
      | match i l a
      ] ++ xs

getParseFailureMsgs :: Match -> DeclIndex -> [Msg Select]
getParseFailureMsgs match =
  Foldable.foldl' (Foldable.foldl' addMsg) [] . failed
  where
    addMsg :: [SelectMsg] -> ParseFailure -> [SelectMsg]
    addMsg xs (ParseFailure i l a msgs) =
      [ SelectParseFailure msg
      | match i l a
      , msg <- NonEmpty.toList msgs
      ] ++ xs
