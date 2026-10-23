-- | Declaration database
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.DeclDB (DeclDB)
-- > import HsBindgen.Frontend.DeclDB qualified as DeclDB
module HsBindgen.Frontend.DeclDB (
    DeclDB -- constructor hidden
      ( entries
      , delayedMsgs
      )
    -- * Entry
  , Entry(..)
  , SquashedTypedef(..)
  , SquashTarget(..)
  , Inactive(..)
  , InactiveInfo(..)
  , InactiveReason(..)
  , Deactivation(..)
  , UnusableReason(..)
  , entryToLoc
  , entryToAvailability
  , inactiveToLoc
  , getUnusable
    -- * Construction
  , fromParseResults
    -- * Transitions
  , deactivate
  , squash
  , externalize
  , addDelayedMsgs
    -- * Filter
  , filter
  , restrictKeys
  , withoutKeys
    -- * Query active declarations
  , lookup
  , getDecls
    -- * Other queries
  , lookupEntry
  , lookupDelayedMsgs
  , toList
  , lookupLoc
  , lookupAmbiguity
  , keysSet
  , getOmitted
  , getSquashed
  ) where

import Prelude hiding (filter, lookup)

import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.HighLevel.Types

import HsBindgen.BindingSpec (ResolvedExtBinding)
import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.DeclDB.ResolveMacro
import HsBindgen.Frontend.DelayedMsg
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.EnrichComments.IsPass
import HsBindgen.Frontend.Pass.MangleNames.Error
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Predicate (IsMainHeader)
import HsBindgen.Imports hiding (toList)
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass (IsPass, coercePass)
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

-- | The declaration database: an entry for every declaration in the source
--
-- Passes change the kind of an entry, but do not remove it; the test suite
-- checks the transitions (@Test.HsBindgen.Frontend.DeclDB.checkTransition@).
--
-- When we replace a declaration by an external one while resolving binding
-- specifications, its entry becomes 'External'. In the
-- "HsBindgen.Frontend.Analysis.UseDeclGraph", dependency edges from use sites
-- to the replaced declaration are deleted, because the use sites now depend on
-- the external Haskell type.
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
data DeclDB l p = DeclDB {
      entries         :: Map C.DeclId (Entry l p)
      -- | Delayed messages of any entry, in the order the passes issued them
    , delayedMsgs     :: Map C.DeclId (NonEmpty DelayedMsg)
      -- | Macros whose expansion depends on where they are invoked
      --
      -- See "HsBindgen.Macro.UniqueExpansion". Ambiguity is independent of the
      -- entries: a macro may be ambiguous whether or not it is usable, and
      -- replacing or removing its entry does not change the verdict.
    , ambiguousMacros :: Set UniqueExpansion.Name
    }
  deriving stock (Generic)

deriving stock instance (IsPass p, Macro.HasTypes l) => Show (DeclDB l p)

{-------------------------------------------------------------------------------
  Entry
-------------------------------------------------------------------------------}

-- | What we generate for a declaration
--
-- Every entry but 'Inactive' is usable. Usability is not concerned with
-- transitivity: a usable declaration may have unusable transitive
-- dependencies; "HsBindgen.Frontend.Pass.Select" computes the consequences.
data Entry l p =
    -- | We generate a binding, from the declaration at pass @p@
    --
    -- Includes the macros the macro language gave up on, from
    -- @ConstructTranslationUnit@ until @ApplyMacroFallback@ decides; fallback
    -- macros stay here for good, with their body from before typechecking.
    --
    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1580>: keep such
    -- macros pending until @ApplyMacroFallback@.
    Active   (C.Decl l p)
    -- | We generate the target: a typedef squashed into it
  | Squashed SquashedTypedef
    -- | Defined elsewhere: an external binding replaces the declaration
  | External ResolvedExtBinding
    -- | Nothing to generate
  | Inactive Inactive
  deriving stock (Generic)

deriving stock instance (IsPass p, Macro.HasTypes l) => Show (Entry l p)

data SquashedTypedef = SquashedTypedef{
      info   :: C.DeclInfo
    , target :: SquashTarget
    }
  deriving stock (Show, Generic)

-- | The declaration a typedef was squashed into
data SquashTarget = SquashTarget{
      c  :: C.DeclId
    , hs :: Hs.Name Hs.NsTypeConstr
    }
  deriving stock (Show, Eq, Generic)

data Inactive =
    InactiveDecl     InactiveInfo
    -- | Several declarations share this name
  | InactiveConflict C.Conflict
  deriving stock (Show, Generic)

instance PrettyForTrace Inactive where
  prettyForTrace = \case
    InactiveDecl     inactive -> prettyForTrace inactive.reason
    InactiveConflict conflict -> prettyForTrace conflict

data InactiveInfo = InactiveInfo{
      info   :: C.DeclInfo
    , reason :: InactiveReason
    }
  deriving stock (Show, Generic)

-- | Why we don't have the declaration
data InactiveReason =
    -- | At construction
    ParseFailed DelayedParseMsg
    -- | At construction; unavailable, see 'C.DeclInfo' availability
  | NotAttempted
    -- | By a later pass
  | Deactivated Deactivation
  deriving stock (Show, Generic)

instance PrettyForTrace InactiveReason where
  prettyForTrace = \case
    ParseFailed{} ->
      "Parse failed"
    NotAttempted ->
      "Declaration is 'unavailable' on this platform"
    Deactivated deactivation ->
      prettyForTrace deactivation

-- | Why a later pass stopped generating the declaration
data Deactivation =
    Unusable UnusableReason
    -- | By a prescriptive binding specification
  | Omitted
  | Deselected
  deriving stock (Show, Eq, Generic)

instance PrettyForTrace Deactivation where
  prettyForTrace = \case
    Unusable reason -> prettyForTrace reason
    Omitted         -> "Omitted by prescriptive binding specification"
    Deselected      -> "Not selected"

data UnusableReason =
    -- | By @ApplyMacroFallback@, for a macro that does not fall back
    UnusableMacro       MacroLangFailure
  | UnusableMangleNames MangleNamesError
  deriving stock (Show, Eq, Generic)

instance PrettyForTrace UnusableReason where
  prettyForTrace = \case
    UnusableMangleNames{} ->
      "Name mangler failure"
    UnusableMacro failure -> case failure of
      FailedParse{}     -> "Parse failed"
      FailedResolve{}   -> "Macro name resolution failed"
      FailedTypecheck{} -> "Macro type-checking failed"

instance IsTrace Level UnusableReason where
  getDefaultLogLevel = \case
    UnusableMacro       err -> getDefaultLogLevel err
    UnusableMangleNames err -> getDefaultLogLevel err
  getSource = \case
    UnusableMacro       err -> getSource err
    UnusableMangleNames err -> getSource err
  getTraceId = \case
    UnusableMacro       err -> getTraceId err
    UnusableMangleNames err -> getTraceId err

-- | 'Nothing' only for 'External'
entryToLoc :: Entry l p -> Maybe C.DeclLocs
entryToLoc = \case
    Active   decl     -> Just $ C.DeclLoc decl.info.loc
    Squashed squashed -> Just $ C.DeclLoc squashed.info.loc
    External _        -> Nothing
    Inactive inactive -> Just $ inactiveToLoc inactive

-- | 'C.Available' where we do not know: for conflicts and externals
entryToAvailability :: Entry l p -> C.Availability
entryToAvailability = \case
    Active decl                       -> decl.info.availability
    Squashed squashed                 -> squashed.info.availability
    External{}                        -> C.Available
    Inactive (InactiveDecl inactive)  -> inactive.info.availability
    Inactive InactiveConflict{}       -> C.Available

inactiveToLoc :: Inactive -> C.DeclLocs
inactiveToLoc = \case
    InactiveDecl     inactive -> C.DeclLoc inactive.info.loc
    InactiveConflict conflict -> C.DeclLocsConflict conflict

-- | Why the entry is unusable, if it is; 'Nothing' for 'Deselected', which
--   arises after @Select@, its only reader
getUnusable :: Entry l p -> Maybe Inactive
getUnusable = \case
    Inactive inactive -> case inactive of
      InactiveDecl InactiveInfo{reason = Deactivated Deselected} -> Nothing
      _otherwise                                                 -> Just inactive
    Active{}   -> Nothing
    Squashed{} -> Nothing
    External{} -> Nothing

-- | For panic messages
entryKind :: Entry l p -> String
entryKind = \case
    Active{}   -> "active"
    Squashed{} -> "squashed"
    External{} -> "external"
    Inactive{} -> "inactive"

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Construct the declaration database, resolving macro names.
--
-- Macro resolution needs the set of all declaration IDs, which is only known
-- once all parse results are in. We therefore first resolve the macros in each
-- successful declaration ('resolveMacros'), and then build the database from
-- the resolved results ('buildDB'), detecting conflicts even with macros that
-- we cannot resolve.
--
-- The analysis of the macro definitions decides whether a macro redefinition
-- is a conflict, and is recorded in the database as the set of ambiguous
-- macros.
fromParseResults ::
     forall l. Macro.HasTypes l
  => Macro.Lang l
  -> UniqueExpansion.Analysis
  -> IsMainHeader
  -> [ParseResult l In]
  -> DeclDB l Out
fromParseResults macroLang macroAnalysis isMainHeader parseResults =
    buildDB macroAnalysis isMainHeader $
      resolveMacros macroLang declIds parseResults
  where
    declIds :: Set C.DeclId
    declIds = Set.fromList $ map parseResultId parseResults

{-------------------------------------------------------------------------------
  Macro resolution
-------------------------------------------------------------------------------}

parseResultToEntry :: ParseResult l Out -> Entry l Out
parseResultToEntry = \case
    ParseResultSuccess r ->
      Active r.decl
    ParseResultUnavailable r ->
      Inactive $ InactiveDecl InactiveInfo{
          info   = r.info
        , reason = NotAttempted
        }
    ParseResultFailure r ->
      Inactive $ InactiveDecl InactiveInfo{
          info   = r.info
        , reason = ParseFailed r.msg
        }

parseResultMsgs :: ParseResult l Out -> [DelayedMsg]
parseResultMsgs = \case
    ParseResultSuccess r     -> map DelayedParse r.delayedParseMsgs
    ParseResultUnavailable{} -> []
    ParseResultFailure{}     -> []

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
resolveMacros macroLang allDeclIds = map resolveResult
  where
    resolveResult :: ParseResult l In -> ParseResult l Out
    resolveResult = \case
      ParseResultSuccess success ->
        ParseResultSuccess ParseSuccess{
          decl             = resolveMacroWith macroLang allDeclIds success.decl
        , delayedParseMsgs = success.delayedParseMsgs
        }
      ParseResultUnavailable notAttempted ->
        ParseResultUnavailable (coercePass notAttempted)
      ParseResultFailure failure ->
        ParseResultFailure (coercePass failure)

{-------------------------------------------------------------------------------
  Build from resolved macros
-------------------------------------------------------------------------------}

-- This function checks for conflicts between ordinary declarations and macro
-- declarations, which must be done specially because they are in separate
-- namespaces. This is done here because we need to detect conflicts even with
-- macros the macro language gives up on, which are thrown out in the
-- @ApplyMacroFallback@ pass unless they fall back.
buildDB ::
     forall l. Macro.HasTypes l
  => UniqueExpansion.Analysis
  -> IsMainHeader
  -> [ParseResult l Out]
  -> DeclDB l Out
buildDB macroAnalysis isMainHeader = Foldable.foldl' aux DeclDB{
      entries         = Map.empty
    , delayedMsgs     = Map.empty
    , ambiguousMacros = UniqueExpansion.ambiguous macroAnalysis
    }
  where
    aux :: DeclDB l Out -> ParseResult l Out -> DeclDB l Out
    aux db new =
      let declId :: C.DeclId
          declId = parseResultId new

          checkWith :: C.DeclId -> Maybe (IsConflict l)
          checkWith oldId = do
            old <- Map.lookup oldId db.entries
            pure $ checkIsConflict macroAnalysis isMainHeader new (oldId, old)

          altDeclId :: C.NameKind -> C.DeclId
          altDeclId kind = C.DeclId{
                name      = C.DeclName{text = declId.name.text, kind = kind}
              , isUnnamed = False
              }

          mConflict :: Maybe (IsConflict l)
          mConflict = Foldable.asum [
              checkWith declId
            , case declId.name.kind of
                C.NameKindOrdinary -> checkWith (altDeclId C.NameKindMacro)
                C.NameKindMacro    -> checkWith (altDeclId C.NameKindOrdinary)
                C.NameKindTagged{} -> Nothing
            ]

          -- A conflict keeps the messages of its members, a parse failure
          -- included.
          conflictMember :: C.Conflict -> DeclDB l Out -> C.DeclId -> DeclDB l Out
          conflictMember conflict db' x =
            let newMsgs
                  | x == declId =
                      parseResultMsgs new ++ parseFailedMsg (parseResultToEntry new)
                  | otherwise =
                      []
                oldMsgs = case Map.lookup x db'.entries of
                  Nothing  -> []
                  Just old -> lookupDelayedMsgs x db' ++ parseFailedMsg old
            in  setEntry x
                  (Inactive (InactiveConflict conflict))
                  (newMsgs ++ oldMsgs)
                  db'
      in  case Map.lookup declId db.entries of
            -- The same identifier at the same location is the same
            -- declaration, seen twice; e.g., Parse emits a struct defined
            -- inline in a variable or function declaration twice.
            Just old | entryToLoc old == Just (C.DeclLoc (parseResultInfo new).loc) ->
              db
            _otherwise -> case mConflict of
              Nothing ->
                setEntry declId (parseResultToEntry new) (parseResultMsgs new) db
              Just (Redefinition entry) ->
                mergeRedefinition declId entry (parseResultMsgs new) db
              Just (SingleConflict ids conflict) ->
                Foldable.foldl' (conflictMember conflict) db ids

parseFailedMsg :: Entry l p -> [DelayedMsg]
parseFailedMsg = \case
    Inactive (InactiveDecl InactiveInfo{reason = ParseFailed msg}) ->
      [DelayedParse msg]
    _otherwise ->
      []

-- | Set an entry and its delayed messages
setEntry :: C.DeclId -> Entry l p -> [DelayedMsg] -> DeclDB l p -> DeclDB l p
setEntry declId entry msgs db = db{
      entries     = Map.insert declId entry db.entries
    , delayedMsgs = case msgs of
        []     -> Map.delete declId db.delayedMsgs
        m : ms -> Map.insert declId (m :| ms) db.delayedMsgs
    }

{-------------------------------------------------------------------------------
  Conflicts
-------------------------------------------------------------------------------}

data IsConflict l =
    -- | The entry to keep
    Redefinition (Entry l Out)
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
  -> (C.DeclId, Entry l Out)
  -> IsConflict l
checkIsConflict macroAnalysis isMainHeader new (oldId, old)
    | C.NameKindMacro <- newId.name.kind
    , C.NameKindMacro <- oldId.name.kind
    = case (redefinition, old) of
        -- E.g., the macro conflicts with an ordinary declaration
        (_, Inactive InactiveConflict{}) -> singleConflict
        -- The definitions are identical, so the macro language fared the same
        -- with both.
        (UniqueExpansion.Benign, _)      -> Redefinition kept
        (UniqueExpansion.NotBenign, _)   -> singleConflict
    | ParseResultSuccess newSuccess <- new
    , Active oldDecl <- old
    , newSuccess.decl.kind == oldDecl.kind
    = Redefinition kept
    -- If a declaration fails to parse, and is itself repeated in the source
    -- code, we will report this as a conflict, rather than with the parse
    -- failure. Fixing this is difficult: in the absence of a parse result, we
    -- cannot detect that we are dealing with the same declaration repeated
    -- twice.
    | otherwise
    = singleConflict
  where
    newId :: C.DeclId
    newId = parseResultId new

    newLoc :: SingleLoc C.DeclPath
    newLoc = (parseResultInfo new).loc

    redefinition :: UniqueExpansion.Redefinition
    redefinition =
      UniqueExpansion.redefinition macroAnalysis $
        UniqueExpansion.Name newId.name.text

    kept :: Entry l Out
    kept = case keepRedefinition isMainHeader newLoc old of
      KeepNew -> parseResultToEntry new
      KeepOld -> old

    singleConflict :: IsConflict l
    singleConflict =
      let conflict :: C.Conflict
          conflict = case old of
            Inactive (InactiveConflict c) ->
              C.conflictInsert c newLoc
            _otherwise ->
              C.conflictFromList $
                newLoc : foldMap C.declLocsToList (entryToLoc old)
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
keepRedefinition :: IsMainHeader -> SingleLoc C.DeclPath -> Entry l p -> Keep
keepRedefinition isMainHeader newLoc old
    | inMainHeader newLoc
    , not $ any inMainHeader $ foldMap C.declLocsToList (entryToLoc old)
    = KeepNew
    | otherwise
    = KeepOld
  where
    inMainHeader :: SingleLoc C.DeclPath -> Bool
    inMainHeader = maybe False isMainHeader . C.declPathRealPath . singleLocPath

-- | Merge two definitions, keeping the delayed messages of both
mergeRedefinition ::
     C.DeclId
  -> Entry l Out
     -- ^ The definition to keep
  -> [DelayedMsg]
     -- ^ The messages of the new definition
  -> DeclDB l Out
  -> DeclDB l Out
mergeRedefinition declId kept newMsgs db =
    -- TODO <https://github.com/well-typed/hs-bindgen/issues/2099?
    --
    -- We should make sure that we do not report delayed messages multiple
    -- times (e.g., use 'Data.List.nub').
    setEntry declId kept (newMsgs ++ lookupDelayedMsgs declId db) db

{-------------------------------------------------------------------------------
  Transitions
-------------------------------------------------------------------------------}

-- | Stop generating active declarations
deactivate ::
     HasCallStack
  => Map C.DeclId Deactivation
  -> DeclDB l p
  -> DeclDB l p
deactivate = transition "deactivate" $ \deactivation -> \case
    Active decl -> Just $ Inactive $ InactiveDecl InactiveInfo{
        info   = decl.info
      , reason = Deactivated deactivation
      }
    _otherwise  -> Nothing

-- | Squash active typedefs into their targets
squash ::
     HasCallStack
  => Map C.DeclId SquashTarget
  -> DeclDB l p
  -> DeclDB l p
squash = transition "squash" $ \target -> \case
    Active decl -> Just $ Squashed SquashedTypedef{
        info   = decl.info
      , target = target
      }
    _otherwise  -> Nothing

-- | Replace active or inactive declarations by external bindings
externalize ::
     HasCallStack
  => Map C.DeclId ResolvedExtBinding
  -> DeclDB l p
  -> DeclDB l p
externalize = transition "externalize" $ \binding -> \case
    Active{}   -> Just $ External binding
    Inactive{} -> Just $ External binding
    _otherwise -> Nothing

-- | Panics if an identifier has no entry or the step rejects the entry
transition ::
     forall a l p. HasCallStack
  => String
  -> (a -> Entry l p -> Maybe (Entry l p))
  -> Map C.DeclId a
  -> DeclDB l p
  -> DeclDB l p
transition name step xs db =
    db{entries = Map.foldlWithKey' aux db.entries xs}
  where
    aux :: Map C.DeclId (Entry l p) -> C.DeclId -> a -> Map C.DeclId (Entry l p)
    aux entries declId x = case Map.lookup declId entries of
      Nothing ->
        panicPure $ name ++ ": unknown declaration " ++ show declId
      Just entry -> case step x entry of
        Nothing ->
          panicPure $
            name ++ ": unexpected " ++ entryKind entry ++ " entry "
              ++ show declId
        Just entry' ->
          Map.insert declId entry' entries

-- | Append delayed messages, in list order
--
-- Precondition: every 'C.DeclId' has an entry.
addDelayedMsgs ::
     HasCallStack
  => [(C.DeclId, DelayedMsg)]
  -> DeclDB l p
  -> DeclDB l p
addDelayedMsgs msgs db =
    db{delayedMsgs = Foldable.foldl' aux db.delayedMsgs msgs}
  where
    aux ::
         Map C.DeclId (NonEmpty DelayedMsg)
      -> (C.DeclId, DelayedMsg)
      -> Map C.DeclId (NonEmpty DelayedMsg)
    aux acc (declId, msg)
      | Map.member declId db.entries =
          Map.insertWith (\new old -> old <> new) declId (msg :| []) acc
      | otherwise =
          panicPure $ "addDelayedMsgs: unknown declaration " ++ show declId

{-------------------------------------------------------------------------------
  Filter
-------------------------------------------------------------------------------}

filter ::
     (C.DeclId -> Entry l p -> Bool)
  -> DeclDB l p
  -> DeclDB l p
filter p db = restrictKeys db $ Map.keysSet $ Map.filterWithKey p db.entries

restrictKeys :: DeclDB l p -> Set C.DeclId -> DeclDB l p
restrictKeys db xs = db{
      entries     = Map.restrictKeys db.entries     xs
    , delayedMsgs = Map.restrictKeys db.delayedMsgs xs
    }

withoutKeys :: DeclDB l p -> Set C.DeclId -> DeclDB l p
withoutKeys db xs = db{
      entries     = Map.withoutKeys db.entries     xs
    , delayedMsgs = Map.withoutKeys db.delayedMsgs xs
    }

{-------------------------------------------------------------------------------
  Query active declarations
-------------------------------------------------------------------------------}

-- | Lookup an active declaration.
lookup :: C.DeclId -> DeclDB l p -> Maybe (C.Decl l p)
lookup declId db = case Map.lookup declId db.entries of
    Just (Active decl) -> Just decl
    _otherwise         -> Nothing

-- | Get all active declarations.
getDecls :: DeclDB l p -> [C.Decl l p]
getDecls db = [decl | Active decl <- Map.elems db.entries]

{-------------------------------------------------------------------------------
  Other queries
-------------------------------------------------------------------------------}

-- | Lookup an entry.
lookupEntry :: C.DeclId -> DeclDB l p -> Maybe (Entry l p)
lookupEntry x db = Map.lookup x db.entries

-- | The delayed messages of a declaration, in the order the passes issued them
lookupDelayedMsgs :: C.DeclId -> DeclDB l p -> [DelayedMsg]
lookupDelayedMsgs x db = foldMap Foldable.toList $ Map.lookup x db.delayedMsgs

-- | Get all entries.
toList :: DeclDB l p -> [(C.DeclId, Entry l p)]
toList db = Map.toList db.entries

-- | Get the source locations of a declaration.
--
-- 'Nothing' indicates the declaration was /not found/, or is 'External'.
lookupLoc :: C.DeclId -> DeclDB l p -> Maybe C.DeclLocs
lookupLoc d db = lookupEntry d db >>= entryToLoc

-- | Does the expansion of a macro depend on where it is invoked?
--
-- An identifier that is not defined as a macro is
-- 'UniqueExpansion.Unambiguous'.
lookupAmbiguity :: UniqueExpansion.Name -> DeclDB l p -> UniqueExpansion.Ambiguity
lookupAmbiguity name db
    | name `Set.member` db.ambiguousMacros =
        UniqueExpansion.Ambiguous
    | otherwise =
        UniqueExpansion.Unambiguous

-- | Get the identifiers of all declarations.
keysSet :: DeclDB l p -> Set C.DeclId
keysSet db = Map.keysSet db.entries

-- | Get omitted entries, and typedefs squashed into omitted entries.
getOmitted :: DeclDB l p -> Map C.DeclId RealPath
getOmitted db = Map.mapMaybe toOmitted db.entries
  where
    toOmitted :: Entry l p -> Maybe RealPath
    toOmitted = \case
      Squashed squashed ->
        lookupEntry squashed.target.c db >>= toOmitted
      Inactive (InactiveDecl inactive@InactiveInfo{reason = Deactivated Omitted}) ->
        C.declPathRealPath (singleLocPath inactive.info.loc)
      _otherwise ->
        Nothing

-- | Get squashed entries.
--
-- TODO <https://github.com/well-typed/hs-bindgen/issues/1549>
-- We may no longer need `getSquashed` once we properly record lists of aliases.
getSquashed ::
     DeclDB l p
  -> Set C.DeclId
  -> Map C.DeclId (RealPath, Hs.Name Hs.NsTypeConstr)
getSquashed db targets = Map.mapMaybe onlySquashedTargetingSet db.entries
  where
    onlySquashedTargetingSet ::
         Entry l p
      -> Maybe (RealPath, Hs.Name Hs.NsTypeConstr)
    onlySquashedTargetingSet = \case
      Squashed squashed
        | Set.member squashed.target.c targets ->
            (, squashed.target.hs) <$>
              C.declPathRealPath (singleLocPath squashed.info.loc)
      _otherwise ->
        Nothing
