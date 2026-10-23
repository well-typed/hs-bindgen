-- | Declaration index
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
-- > import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
module HsBindgen.Frontend.Analysis.DeclIndex (
    DeclIndex -- opaque
    -- * Entry
  , UsableEntry(..)
  , UnusableEntry(..)
  , UnusableReason(..)
  , Success(..)
  , Squashed(..)
  , Entry(..)
  , entryToLoc
  , entryToAvailability
    -- * Construction
  , fromParseResults
    -- * Filter
  , filter
  , restrictKeys
  , withoutKeys
    -- * Query parse successes
  , lookup
  , getDecls
    -- * Other queries
  , lookupEntry
  , toList
  , lookupLoc
  , lookupAmbiguity
  , unusableToLoc
  , keysSet
  , getOmitted
  , getSquashed
  , getUnusables
    -- * Support for delayed messages
  , registerDelayedMsg
    -- * Support for macro failures
  , registerUnhandledMacro
    -- * Support for binding specifications
  , registerOmittedDeclarations
  , registerExternalDeclarations
    -- * Support for name mangle failures
  , registerSquashedDeclarations
  , registerMangleNamesFailure
  ) where

import Prelude hiding (filter, lookup)

import Control.Monad.State
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.HighLevel.Types

import HsBindgen.Frontend.Analysis.DeclIndex.ResolveMacro
import HsBindgen.Frontend.DelayedMsg
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.EnrichComments.IsPass
import HsBindgen.Frontend.Pass.MangleNames.Error
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.Predicate (IsMainHeader)
import HsBindgen.Imports hiding (toList)
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass (IsPass)
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Macro.Error
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro
import HsBindgen.Macro.UniqueExpansion qualified as UniqueExpansion
import HsBindgen.Macro.UniqueExpansion.Types qualified as UniqueExpansion
import HsBindgen.Util.Tracer

type In  = EnrichComments
type Out = ConstructTranslationUnit

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | The declaration index, parameterized by the pass at which the indexed
-- declarations are represented.
--
-- The public 'DeclIndex' fixes this to 'ConstructTranslationUnit' (macros
-- resolved). The polymorphic form is used internally to build the index at
-- 'EnrichComments' before resolving macros (see 'fromParseResults').
--
-- The declaration index indexes C types (not Haskell types); as such, it
-- contains declarations in the source code, and never contains external
-- declarations.
--
-- When we replace a declaration by an external one while resolving binding
-- specifications, it is not deleted from the declaration index but reclassified
-- as 'UsableExternal'. In the "HsBindgen.Frontend.Analysis.UseDeclGraph",
-- dependency edges from use sites to the replaced declaration are deleted,
-- because the use sites now depend on the external Haskell type.
--
-- For example, assume the C code
--
-- @
-- typedef struct {    // D1
--   int x;
-- } foo_t;            // D2
--
-- typedef foo_t foo;  // D3
-- @
--
-- - D1 declares an untagged @struct@
-- - D2 declares the typedef @foo_t@ depending on D1
-- - D3 declares a typedef @foo@ depending on D2
--
-- The use-decl graph is
--
-- D1 <- D2 <- D3
--
-- Further, assume we have an external binding specification for D2. After
-- resolving external binding specifications, the use-decl graph will be
--
-- D1 <- D2    D3
--      (R2) <-|
--
-- The edge from D3 to D2 was removed, since D3 now depends on a Haskell type
-- R3, which is not part of the use-decl graph.
data DeclIndex l = DeclIndex {
      map             :: Map C.DeclId (Entry l)
      -- | Macros whose expansion depends on where they are invoked
      --
      -- See "HsBindgen.Macro.UniqueExpansion". Ambiguity is independent of the
      -- entries: a macro may be ambiguous whether or not it is usable, and
      -- replacing or removing its entry does not change the verdict.
    , ambiguousMacros :: Set UniqueExpansion.Name
    }
  deriving stock (Show, Generic)

{-------------------------------------------------------------------------------
  Entry
-------------------------------------------------------------------------------}

-- | Usable declaration
--
-- At each stage in the `hs-bindgen` pipeline, a "usable" declaration (i.e.,
-- 'UsableEntry') is a declaration we think we can generate bindings for. Passes
-- may replace "usable" declarations with
--
-- - other "usable" declarations such as external declarations
--   ("ResolveBindingSpecs") or squashed declarations (in "MangleNames"); or
--
-- - "unusable" declarations (i.e, 'UnusableEntry'), for example when the
--   macro language gives up on a macro that does not fall back
--   ("ApplyMacroFallback") or name mangling fails ("MangleNames").
--
-- At the end of the `hs-bindgen` pipeline, we can generate bindings for
-- "usable" declarations.
--
-- However, usability is not concerned with _transitivity_. Usable declarations
-- may have unusable transitive dependencies. Even though we can generate
-- bindings for usable declarations, they may not be functional because they
-- miss transitive dependencies.
--
-- (We avoid the term available, because it is overloaded with Clang's
-- CXAvailabilityKind).
data UsableEntry l =
    -- | Includes the macros the macro language gave up on, from
    -- @ConstructTranslationUnit@ until @ApplyMacroFallback@ decides; fallback
    -- macros stay here for good, with their body from before typechecking.
    --
    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1580>: keep such
    -- macros pending until @ApplyMacroFallback@.
    UsableSuccess (Success l Out)
  | UsableExternal C.DeclLocs
    -- Squashed declarations are always "usable" because we only squash
    -- declaration in the list of declarations attached to the declaration unit.
  | UsableSquashed Squashed
  deriving stock (Show, Generic)

usableToLoc :: UsableEntry l -> C.DeclLocs
usableToLoc = \case
    UsableSuccess  x    -> C.DeclLoc x.decl.info.loc
    UsableExternal locs -> locs
    UsableSquashed x    -> C.DeclLoc x.typedefLoc

-- | Unusable declaration
--
-- A declaration is unusable if we cannot generate bindings for it.
--
-- See 'UsableEntry'.
--
-- (We avoid the term available, because it is overloaded with Clang's
-- CXAvailabilityKind).
data UnusableEntry =
    UnusableReason (SingleLoc C.DeclPath) UnusableReason
  | UnusableConflict C.Conflict
  deriving stock (Show, Generic)

instance PrettyForTrace UnusableEntry where
  prettyForTrace = \case
    UnusableReason   _loc unusable -> prettyForTrace unusable
    UnusableConflict      conflict -> prettyForTrace conflict

data UnusableReason =
    UnusableUnavailable
  | UnusableOmitted
  | UnusableParseFailure           DelayedParseMsg
  | UnusableMacroTypecheckFailure  MacroTypecheckError
  | UnusableMacroResolutionFailure MacroResolutionError
  | UnusableMangleNamesFailure     MangleNamesError
  deriving stock (Show, Generic)

instance PrettyForTrace UnusableReason where
  prettyForTrace = \case
    UnusableUnavailable{} ->
      "Declaration is 'unavailable' on this platform"
    UnusableOmitted{} ->
      "Omitted by prescriptive binding specification"
    UnusableParseFailure{} ->
      "Parse failed"
    UnusableMangleNamesFailure{} ->
      "Name mangler failure"
    UnusableMacroTypecheckFailure{} ->
      "Macro type-checking failed"
    UnusableMacroResolutionFailure{} ->
      "Macro name resolution failed"

unusableToLoc :: UnusableEntry -> C.DeclLocs
unusableToLoc = \case
  UnusableReason   loc _ -> C.DeclLoc          loc
  UnusableConflict     c -> C.DeclLocsConflict c

data Success l p = Success {
    decl        :: C.Decl l p
    -- | In the order the passes issued them
  , delayedMsgs :: [DelayedMsg]
  }
  deriving stock (Generic)

deriving stock instance ( IsPass p
                        , Macro.HasTypes l
                        ) => Show (Success l p)

data Squashed = Squashed {
    -- | The location of the squashed typedef (i.e., _not_ the target)
    typedefLoc   :: SingleLoc C.DeclPath
  , targetNameC  :: C.DeclId
  , targetNameHs :: Hs.Name Hs.NsTypeConstr
  }
  deriving stock (Show, Generic)

-- | Entry of declaration index
data Entry l =
    UsableEntry   (UsableEntry l)
  | UnusableEntry UnusableEntry
  deriving stock (Show, Generic)

entryToLoc :: Entry l -> C.DeclLocs
entryToLoc = \case
  UnusableEntry e -> unusableToLoc e
  UsableEntry   e -> usableToLoc e

entryToAvailability :: Entry l -> C.Availability
entryToAvailability = \case
    UsableEntry e   -> case e of
      UsableSuccess success -> success.decl.info.availability
      UsableExternal{}      -> C.Available
      UsableSquashed{}      -> C.Available
    UnusableEntry{} -> C.Available

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Construct the declaration index, resolving macro names.
--
-- Macro resolution needs the set of all declaration IDs, which is only known
-- once all parse results are in. We therefore first resolve the macros in each
-- successful declaration ('resolveMacros'), and then build the index from the
-- resolved results ('buildIndex'), detecting conflicts even with macros that we
-- cannot resolve.
--
-- The analysis of the macro definitions decides whether a macro redefinition
-- is a conflict, and is recorded in the index as the set of ambiguous macros.
fromParseResults ::
     forall l. Macro.HasTypes l
  => Macro.Lang l
  -> UniqueExpansion.Analysis
  -> IsMainHeader
  -> [ParseResult l In]
  -> DeclIndex l
fromParseResults macroLang macroAnalysis isMainHeader parseResults = DeclIndex{
      map             = buildIndex macroAnalysis isMainHeader $
                          resolveMacros macroLang declIds parseResults
    , ambiguousMacros = UniqueExpansion.ambiguous macroAnalysis
    }
  where
    declIds :: Set C.DeclId
    declIds = getDeclIds parseResults

    getDeclIds :: [ParseResult l In] -> Set C.DeclId
    getDeclIds = Set.fromList . map (.id)

{-------------------------------------------------------------------------------
  Macro resolution
-------------------------------------------------------------------------------}

parseResultToEntry :: ParseResult l Out -> Entry l
parseResultToEntry result = case result.classification of
    ParseResultSuccess r ->
      UsableEntry $ UsableSuccess (parseSuccessToSuccess r)
    ParseResultUnavailable ->
      UnusableEntry $ UnusableReason result.loc UnusableUnavailable
    ParseResultFailure r ->
      UnusableEntry $ UnusableReason result.loc $ UnusableParseFailure r

parseSuccessToSuccess :: ParseSuccess l Out -> Success l Out
parseSuccessToSuccess success = Success {
      decl        = success.decl
    , delayedMsgs = map DelayedParse success.delayedParseMsgs
    }

-- | Resolve macro names in every successful declaration.
--
-- Macros whose names cannot be resolved remain successes; the failure is
-- recorded in the macro (see "HsBindgen.Frontend.MacroFallback").
resolveMacros ::
     forall l.
     Macro.Lang l
  -> Set C.DeclId
  -> [ParseResult l In]
  -> [ParseResult l Out]
resolveMacros macroLang allDeclIds = map (over #classification resolveClassification)
  where
    resolveClassification :: ParseClassification l In -> ParseClassification l Out
    resolveClassification = \case
      ParseResultSuccess success ->
        ParseResultSuccess ParseSuccess{
          decl             = resolveMacroWith macroLang allDeclIds success.decl
        , delayedParseMsgs = success.delayedParseMsgs
        }
      ParseResultUnavailable ->
        ParseResultUnavailable
      ParseResultFailure x ->
        ParseResultFailure x

{-------------------------------------------------------------------------------
  Build from resolved macros
-------------------------------------------------------------------------------}

-- This function checks for conflicts between ordinary declarations and macro
-- declarations, which must be done specially because they are in separate
-- namespaces. This is done here because we need to detect conflicts even with
-- macros the macro language gives up on, which are thrown out in the
-- @ApplyMacroFallback@ pass unless they fall back.
buildIndex ::
     forall l. Macro.HasTypes l
  => UniqueExpansion.Analysis
  -> IsMainHeader
  -> [ParseResult l Out]
  -> Map C.DeclId (Entry l)
buildIndex macroAnalysis isMainHeader results =
    execState (mapM_ aux results) Map.empty
  where
    aux :: ParseResult l Out -> State (Map C.DeclId (Entry l)) ()
    aux new = modify' $ \index ->
      let declId :: C.DeclId
          declId = new.id

          mConflict :: Maybe (IsConflict l)
          mConflict = Foldable.asum [
              do
                old <- Map.lookup declId index
                pure $ checkIsConflict macroAnalysis isMainHeader new (declId, old)
            , case declId.name.kind of
                C.NameKindOrdinary -> do
                  let altDeclId = C.DeclId{
                          name   = C.DeclName{
                              text = declId.name.text
                            , kind = C.NameKindMacro
                            }
                        , isUnnamed = False
                        }
                  old <- Map.lookup altDeclId index
                  pure $ checkIsConflict macroAnalysis isMainHeader new (altDeclId, old)
                C.NameKindMacro    -> do
                  let altDeclId = C.DeclId{
                          name   = C.DeclName{
                              text = declId.name.text
                            , kind = C.NameKindOrdinary
                            }
                        , isUnnamed = False
                        }
                  old <- Map.lookup altDeclId index
                  pure $ checkIsConflict macroAnalysis isMainHeader new (altDeclId, old)
                C.NameKindTagged{} -> Nothing
            ]
      in  case Map.lookup declId index of
            -- The same identifier at the same location is the same
            -- declaration, seen twice; e.g., Parse emits a struct defined
            -- inline in a variable or function declaration twice.
            Just old | entryToLoc old == C.DeclLoc new.loc ->
              index
            _otherwise -> case mConflict of
              Nothing ->
                Map.insert declId (parseResultToEntry new) index
              Just (Redefinition entry) ->
                Map.insert declId entry index
              Just (SingleConflict ids conflict) ->
                Map.union
                  ( Map.fromList
                      [ (x, UnusableEntry (UnusableConflict conflict))
                      | x <- Set.toList ids
                      ]
                  )
                  index

{-------------------------------------------------------------------------------
  Conflicts
-------------------------------------------------------------------------------}

data IsConflict l =
    Redefinition (Entry l)
  | SingleConflict (Set C.DeclId) C.Conflict

-- | Check whether a new declaration conflicts with an existing entry
--
-- A macro that is defined more than once is not a conflict if the analysis of
-- the macro definitions deems the redefinition benign. Two ordinary
-- declarations are not a conflict if they are successfully parsed to the same
-- kind of declaration.
checkIsConflict ::
     forall l. Macro.HasTypes l
  => UniqueExpansion.Analysis
  -> IsMainHeader
  -> ParseResult l Out
  -> (C.DeclId, Entry l)
  -> IsConflict l
checkIsConflict macroAnalysis isMainHeader new (oldId, old)
    | C.NameKindMacro <- newId.name.kind
    , C.NameKindMacro <- oldId.name.kind
    = case (redefinition, old) of
        -- E.g., the macro conflicts with an ordinary declaration
        (_, UnusableEntry UnusableConflict{}) -> singleConflict
        (UniqueExpansion.Benign, _)           -> Redefinition macroRedefinition
        (UniqueExpansion.NotBenign, _)        -> singleConflict
    | ParseResultSuccess newSuccess <- new.classification
    , UsableEntry (UsableSuccess oldSuccess) <- old
    , newSuccess.decl.kind == oldSuccess.decl.kind
    = Redefinition $ UsableEntry $ UsableSuccess $
        mergeRedefinition keep (parseSuccessToSuccess newSuccess) oldSuccess
    -- If a declaration fails to parse, and is itself repeated in the source
    -- code, we will report this as a conflict, rather than with the parse
    -- failure. Fixing this is difficult: in the absence of a parse result, we
    -- cannot detect that we are dealing with the same declaration repeated
    -- twice.
    | otherwise
    = singleConflict
  where
    newId :: C.DeclId
    newId = new.id

    newLoc :: SingleLoc C.DeclPath
    newLoc = new.loc

    redefinition :: UniqueExpansion.Redefinition
    redefinition =
      UniqueExpansion.redefinition macroAnalysis $
        UniqueExpansion.Name newId.name.text

    -- The definitions are identical, so the macro language fared the same with
    -- both.
    macroRedefinition :: Entry l
    macroRedefinition = case (parseResultToEntry new, old) of
      ( UsableEntry (UsableSuccess newSuccess)
        , UsableEntry (UsableSuccess oldSuccess) ) ->
          UsableEntry $ UsableSuccess $
            mergeRedefinition keep newSuccess oldSuccess
      (newEntry, _) -> case keep of
        KeepNew -> newEntry
        KeepOld -> old

    keep :: Keep
    keep = keepRedefinition isMainHeader newLoc old

    singleConflict :: IsConflict l
    singleConflict =
      let conflict :: C.Conflict
          conflict = case old of
            UnusableEntry (UnusableConflict c) ->
              C.conflictInsert c newLoc
            _otherwise ->
              C.conflictFromList $
                newLoc : C.declLocsToList (entryToLoc old)
      in SingleConflict (Set.fromList [newId, oldId]) conflict

-- TODO <https://github.com/well-typed/hs-bindgen/issues/2282>
--
-- We should "keep" more information in a redefinition so we can apply the
-- select predicate to any of the declarations.

-- | Which of two definitions of the same declaration to keep
data Keep = KeepNew | KeepOld

-- | Keep the new definition if it is in a main header and the old one is not
--
-- The location of the kept definition decides whether the default selection
-- predicate selects it, and its header determines the generated @#include@.
keepRedefinition :: IsMainHeader -> SingleLoc C.DeclPath -> Entry l -> Keep
keepRedefinition isMainHeader newLoc old
    | inMainHeader newLoc
    , not $ any inMainHeader $ C.declLocsToList (entryToLoc old)
    = KeepNew
    | otherwise
    = KeepOld
  where
    inMainHeader :: SingleLoc C.DeclPath -> Bool
    inMainHeader = maybe False isMainHeader . C.declPathRealPath . singleLocPath

-- | Merge two definitions, keeping the delayed messages of both
mergeRedefinition :: Keep -> Success l Out -> Success l Out -> Success l Out
mergeRedefinition keep newSuccess oldSuccess =
    -- TODO <https://github.com/well-typed/hs-bindgen/issues/2099?
    --
    -- We should make sure that we do not report delayed messages multiple
    -- times (e.g., use 'Data.List.nub').
    let kept = case keep of
          KeepNew -> newSuccess
          KeepOld -> oldSuccess
    in kept{delayedMsgs = newSuccess.delayedMsgs ++ oldSuccess.delayedMsgs}

{-------------------------------------------------------------------------------
  Filter
-------------------------------------------------------------------------------}

filter ::
     (C.DeclId -> Entry l -> Bool)
  -> DeclIndex l
  -> DeclIndex l
filter p = #map %~ Map.filterWithKey p

restrictKeys :: DeclIndex l -> Set C.DeclId -> DeclIndex l
restrictKeys index xs = index & #map %~ (`Map.restrictKeys` xs)

withoutKeys :: DeclIndex l -> Set C.DeclId -> DeclIndex l
withoutKeys index xs = index & #map %~ (`Map.withoutKeys` xs)

{-------------------------------------------------------------------------------
  Query parse successes
-------------------------------------------------------------------------------}

-- | Lookup parse success.
lookup :: C.DeclId -> DeclIndex l -> Maybe (C.Decl l Out)
lookup declId index = case Map.lookup declId index.map of
  Nothing                          -> Nothing
  Just (UsableEntry (UsableSuccess x)) -> Just $ x.decl
  _                                -> Nothing

-- | Get all parse successes.
getDecls :: DeclIndex l -> [C.Decl l Out]
getDecls index = mapMaybe toDecl $ Map.elems index.map
  where
    toDecl = \case
      UsableEntry (UsableSuccess x) -> Just x.decl
      _otherEntries             -> Nothing

{-------------------------------------------------------------------------------
  Other queries
-------------------------------------------------------------------------------}

-- | Lookup an entry of a declaration index.
lookupEntry :: C.DeclId -> DeclIndex l -> Maybe (Entry l)
lookupEntry x index = Map.lookup x index.map

-- | Get all entries of a declaration index.
toList :: DeclIndex l -> [(C.DeclId, Entry l)]
toList index = Map.toList index.map

-- | Get the source locations of a declaration.
--
-- 'Nothing' indicates the declaration was /not found/.
lookupLoc :: C.DeclId -> DeclIndex l -> Maybe C.DeclLocs
lookupLoc d i = entryToLoc <$> lookupEntry d i

-- | Does the expansion of a macro depend on where it is invoked?
--
-- An identifier that is not defined as a macro is
-- 'UniqueExpansion.Unambiguous'.
lookupAmbiguity :: UniqueExpansion.Name -> DeclIndex l -> UniqueExpansion.Ambiguity
lookupAmbiguity name index
    | name `Set.member` index.ambiguousMacros =
        UniqueExpansion.Ambiguous
    | otherwise =
        UniqueExpansion.Unambiguous

-- | Get the identifiers of all declarations in the index.
keysSet :: DeclIndex l -> Set C.DeclId
keysSet index = Map.keysSet index.map

-- | Get omitted entries.
getOmitted :: DeclIndex l -> Map C.DeclId RealPath
getOmitted index = Map.mapMaybe toOmitted index.map
  where
    toOmitted :: Entry l -> Maybe RealPath
    toOmitted = \case
      UsableEntry   (UsableSquashed e) ->
        lookupEntry e.targetNameC index >>= toOmitted
      UsableEntry{} ->
        Nothing
      UnusableEntry (UnusableReason loc UnusableOmitted) ->
        C.declPathRealPath (singleLocPath loc)
      UnusableEntry{} ->
        Nothing

-- | Get squashed entries.
--
-- TODO <https://github.com/well-typed/hs-bindgen/issues/1549>
-- We may no longer need `getSquashed` once we properly record lists of aliases.
getSquashed ::
     DeclIndex l
  -> Set C.DeclId
  -> Map C.DeclId (RealPath, Hs.Name Hs.NsTypeConstr)
getSquashed index targets = Map.mapMaybe onlySquashedTargetingSet index.map
  where
    onlySquashedTargetingSet ::
         Entry l
      -> Maybe (RealPath, Hs.Name Hs.NsTypeConstr)
    onlySquashedTargetingSet = \case
      UsableEntry (UsableSquashed e) ->
        if Set.member e.targetNameC targets then
          (, e.targetNameHs) <$> C.declPathRealPath (singleLocPath e.typedefLoc)
        else
          Nothing
      _otherwise  -> Nothing

-- | Restrict the declaration index to unusable declarations in a given set.
getUnusables :: DeclIndex l -> Set C.DeclId -> Map C.DeclId UnusableEntry
getUnusables index = Map.mapMaybe onlyUnusable . (.map) . restrictKeys index
  where
    onlyUnusable :: Entry l -> Maybe UnusableEntry
    onlyUnusable = \case
      UsableEntry   _ -> Nothing
      UnusableEntry e -> Just e

{-------------------------------------------------------------------------------
  Support for delayed messages
-------------------------------------------------------------------------------}

-- | Append a delayed message to an existing 'UsableSuccess' entry.
--
-- Has no effect if the declaration is not a success
registerDelayedMsg ::
     (C.DeclId, DelayedMsg)
  -> DeclIndex l
  -> DeclIndex l
registerDelayedMsg (declId, msg) =
    #map %~ Map.adjust addMsg declId
  where
    addMsg :: Entry l -> Entry l
    addMsg (UsableEntry (UsableSuccess ps)) =
      UsableEntry $ UsableSuccess ps{delayedMsgs = ps.delayedMsgs ++ [msg]}
    addMsg entry = entry

{-------------------------------------------------------------------------------
  Support for macro failures
-------------------------------------------------------------------------------}

-- | Register a macro the macro language gave up on, and that does not fall back
--
-- The reason is the one of the stage that gave up. Until this point, the macro
-- was a 'UsableSuccess'.
--
-- TODO <https://github.com/well-typed/hs-bindgen/issues/1580>: keep such
-- macros pending until @ApplyMacroFallback@.
registerUnhandledMacro ::
     DeclIndex l
  -> (C.Decl l TypecheckMacros, MacroLangFailure)
  -> DeclIndex l
registerUnhandledMacro index (decl, failure) = index
    & #map %~ Map.insert decl.id (UnusableEntry $ UnusableReason decl.info.loc reason)
  where
    reason :: UnusableReason
    reason = case failure of
      FailedParse     err -> UnusableParseFailure (ParseMacroErrorParse err)
      FailedResolve   err -> UnusableMacroResolutionFailure err
      FailedTypecheck err -> UnusableMacroTypecheckFailure err

{-------------------------------------------------------------------------------
  Support for binding specifications
-------------------------------------------------------------------------------}

registerOmittedDeclarations ::
     Map C.DeclId (SingleLoc C.DeclPath)
  -> DeclIndex l
  -> DeclIndex l
registerOmittedDeclarations xs =
    #map %~ Map.union (toOmitted <$> xs)
  where
    toOmitted loc = UnusableEntry $ UnusableReason loc UnusableOmitted

registerExternalDeclarations ::
     [(C.DeclId, C.DeclLocs)]
  -> DeclIndex l
  -> DeclIndex l
registerExternalDeclarations xs index = Foldable.foldl' insert index xs
  where
    insert :: DeclIndex l -> (C.DeclId, C.DeclLocs) -> DeclIndex l
    insert index' (declId, locs) =
      index' & #map %~ Map.insert declId (UsableEntry $ UsableExternal locs)

{-------------------------------------------------------------------------------
  Support for mangle names
-------------------------------------------------------------------------------}

registerSquashedDeclarations ::
     Map C.DeclId Squashed
  -> DeclIndex l
  -> DeclIndex l
registerSquashedDeclarations xs =
    #map %~ Map.union (UsableEntry . UsableSquashed <$> xs)

registerMangleNamesFailure ::
     Map C.DeclId (SingleLoc C.DeclPath, MangleNamesError)
  -> DeclIndex l
  -> DeclIndex l
registerMangleNamesFailure xs =
    #map %~ Map.union (toEntry <$> xs)
  where
    toEntry (loc, err) =
      UnusableEntry $ UnusableReason loc $ UnusableMangleNamesFailure err
