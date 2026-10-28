-- | Declaration database
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.DeclDB (DeclDB)
-- > import HsBindgen.Frontend.DeclDB qualified as DeclDB
--
-- = Orderings
--
-- The declaration database does not store any particular 'Order': each
-- traversal states the order it needs. We distinguish three orderings:
--
-- * Sequence order: the order in which @libclang@ visits the declarations,
--   and in which 'HsBindgen.Frontend.Pass.Parse.parseDecls' returns them.
--   @libclang@ visits the macros first and the remaining declarations in
--   source order, so sequence order is /not/ source order.
--
-- * Source order: roughly, how the declarations appear in the C source.
--   @Parse@ records it in 'C.DeclInfo' (@sourceOrderIndex@), sorting on
--   source position with @clang_isBeforeInTranslationUnit@ (accurate, but
--   requires Clang >= 20.1). @annSortKey@ (in
--   "HsBindgen.Frontend.Analysis.DeclUseGraph.Query") computes a best-effort
--   source order without that API; it can be inaccurate, e.g. when a header
--   includes another header part-way through its own declarations.
--
-- * Dependency order: topological over the dependency edges; if @A@ uses @B@,
--   then @B@ comes before @A@. 'DependencyBy' follows the 'NeedsShape' edges
--   and orders the entries that do not depend on each other by a key, such as
--   @annSortKey@; 'AllDependencies' follows every edge.
module HsBindgen.Frontend.DeclDB (
    DeclDB -- constructor hidden
      ( entries
      , delayedMsgs
      , declUseGraph
      )
    -- * Entry
  , Entry(..)
  , SquashTarget(..)
  , Inactive(..)
  , InactiveReason(..)
  , Deactivation(..)
  , UnusableReason(..)
  , entryToLoc
  , entryToDeprecation
  , inactiveToLoc
  , getUnusable
    -- * Construction
  , fromParseResults
    -- * Traversal
  , Order(..)
  , PassResult(..)
  , traverseActive
  , active
    -- * Transitions
  , externalize
  , addDelayedMsgs
    -- * Dependency edges
  , deleteDeps
  , insertDepsOfDecl
    -- * Query active declarations
  , lookup
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

import Prelude hiding (lookup)

import Data.Digraph qualified as Digraph
import Data.Foldable qualified as Foldable
import Data.Functor.Const (Const (..))
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Set qualified as Set

import Clang.HighLevel.Types

import HsBindgen.BindingSpec (ResolvedExtBinding)
import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Analysis (Dependency (..))
import HsBindgen.Frontend.Analysis.DeclUseGraph.Construction qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.DeclUseGraph.Definition (DeclUseGraph (..))
import HsBindgen.Frontend.DeclDB.ResolveMacro
import HsBindgen.Frontend.DelayedMsg
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.EnrichComments.IsPass
import HsBindgen.Frontend.Pass.MangleNames.Error
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass (ReparseMacroExpansions)
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

-- | The declaration database: an entry for every declaration in the C source
--
-- Passes change the kind of an entry, but do not remove it; the test suite
-- checks the transitions (@Test.HsBindgen.Frontend.DeclDB.checkTransition@).
--
-- When we replace a declaration by an external one while resolving binding
-- specifications, its entry becomes 'External'. In the
-- "HsBindgen.Frontend.Analysis.UseDeclGraph", all edges of the replaced
-- declaration are deleted ('externalize').
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
-- D1    D2    D3
--      (R2) <--|
--
-- The edges from and to D2 were removed; D2 is still in the use-decl graph; D3
-- now depends on the Haskell type R2, which is not part of the use-decl graph.
data DeclDB l p = DeclDB {
      entries         :: Map C.DeclId (Entry l p)
      -- | Delayed messages of any entry, in the order the passes issued them
    , delayedMsgs     :: Map C.DeclId (NonEmpty DelayedMsg)
      -- | Dependency edges, from the dependency to the user
      --
      -- Edges follow the bodies of the declarations. Edges change when
      -- externalising, opaquing or reparsing, and when macros fail. Edges do
      -- not change when an entry is deactivated.
      --
      -- Its vertices are the keys of 'entries'.
      --
      -- The reverse use-decl graph is not stored: see
      -- 'HsBindgen.Frontend.Analysis.UseDeclGraph.fromDeclUseGraph'.
    , declUseGraph    :: DeclUseGraph
      -- | Macros whose expansion depends on where they are invoked
      --
      -- See "HsBindgen.Macro.UniqueExpansion". Ambiguity is independent of the
      -- entries: a macro may be ambiguous whether or not it is usable, and
      -- replacing or removing its entry does not change the verdict.
    , ambiguousMacros :: Set UniqueExpansion.Name
    }

deriving stock instance (IsPass p, Macro.HasTypes l) => Show (DeclDB l p)

{-------------------------------------------------------------------------------
  Entry
-------------------------------------------------------------------------------}

-- | Information kept about each C declaration
--
-- Only v'Inactive' entries are unusable. Usability is not concerned with
-- transitivity: a usable declaration may have unusable transitive
-- dependencies (see "HsBindgen.Frontend.Pass.Select").
data Entry l p =
    -- | We intend to generate a binding
    --
    -- Before the pass @ApplyMacroFallback@, this may also include macros the
    -- macro language gave up on because we may fall back to generating a raw
    -- macro binding.
    Active   (C.Decl l p)
    -- | We generate a binding for the squash target only
  | Squashed C.DeclInfo SquashTarget
    -- | Defined elsewhere: an external binding replaces the declaration
  | External ResolvedExtBinding
    -- | Usable, but deselected
  | Deselected C.DeclInfo
    -- | We do not intend to generate a binding
  | Inactive Inactive
  deriving stock (Generic)

deriving stock instance (IsPass p, Macro.HasTypes l) => Show (Entry l p)

-- | The declaration a typedef was squashed into
data SquashTarget = SquashTarget{
      c  :: C.DeclId
    , hs :: Hs.Name Hs.NsTypeConstr
    }
  deriving stock (Show, Eq, Generic)

data Inactive =
    InactiveDecl     C.DeclInfo InactiveReason
    -- | Several declarations share this name
  | InactiveConflict C.Conflict
  deriving stock (Show, Generic)

instance PrettyForTrace Inactive where
  prettyForTrace = \case
    InactiveDecl     _ reason -> prettyForTrace reason
    InactiveConflict conflict -> prettyForTrace conflict

-- | Why we don't have the declaration
data InactiveReason =
    -- | At construction
    ParseFailed  DelayedParseMsg
    -- | At construction
  | NotAttempted NotAttemptedReason
    -- | By a later pass
  | Deactivated  Deactivation
  deriving stock (Show, Generic)

instance PrettyForTrace InactiveReason where
  prettyForTrace = \case
    ParseFailed{} ->
      "Parse failed"
    NotAttempted reason ->
      prettyForTrace reason
    Deactivated deactivation ->
      prettyForTrace deactivation

-- | Reason why we do not generate a binding for a declaration
data Deactivation =
    Unusable UnusableReason
    -- | Applied by a prescriptive binding specification
  | Omitted
  deriving stock (Show, Eq, Generic)

instance PrettyForTrace Deactivation where
  prettyForTrace = \case
    Unusable reason -> prettyForTrace reason
    Omitted         -> "Omitted by prescriptive binding specification"

data UnusableReason =
    -- | Apply by @ApplyMacroFallback@, for a macro that does not fall back
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
    Squashed info _   -> Just $ C.DeclLoc info.loc
    External _        -> Nothing
    Deselected info   -> Just $ C.DeclLoc info.loc
    Inactive inactive -> Just $ inactiveToLoc inactive

-- | Fall back to 'C.NotDeprecated' for conflicts and externals
entryToDeprecation :: Entry l p -> C.Deprecation
entryToDeprecation = \case
    Active decl                       -> decl.info.deprecation
    Squashed info _                   -> info.deprecation
    External{}                        -> C.NotDeprecated
    Deselected info                   -> info.deprecation
    Inactive (InactiveDecl info _)    -> info.deprecation
    Inactive InactiveConflict{}       -> C.NotDeprecated

inactiveToLoc :: Inactive -> C.DeclLocs
inactiveToLoc = \case
    InactiveDecl     info _   -> C.DeclLoc info.loc
    InactiveConflict conflict -> C.DeclLocsConflict conflict

-- | Why the entry is unusable, if it is
getUnusable :: Entry l p -> Maybe Inactive
getUnusable = \case
    Inactive inactive -> Just inactive
    Active{}          -> Nothing
    Squashed{}        -> Nothing
    External{}        -> Nothing
    Deselected{}      -> Nothing

-- | For trace messages
entryKind :: Entry l p -> String
entryKind = \case
    Active{}     -> "active"
    Squashed{}   -> "squashed"
    External{}   -> "external"
    Deselected{} -> "deselected"
    Inactive{}   -> "inactive"

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
      Inactive $ InactiveDecl r.info (NotAttempted r.reason)
    ParseResultFailure r ->
      Inactive $ InactiveDecl r.info (ParseFailed r.msg)

parseResultMsgs :: ParseResult l Out -> [DelayedParseMsg]
parseResultMsgs = \case
    ParseResultSuccess r     -> r.delayedParseMsgs
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
buildDB macroAnalysis isMainHeader =
    finish . Foldable.foldl' aux Build{
        entries   = Map.empty
      , parseMsgs = Map.empty
      }
  where
    finish :: Build l -> DeclDB l Out
    finish build = withGraph DeclDB{
          entries         = build.entries
        , delayedMsgs     = fmap (fmap DelayedParse) build.parseMsgs
          -- Built once the entries are complete
        , declUseGraph    = DeclUseGraph Digraph.empty
        , ambiguousMacros = UniqueExpansion.ambiguous macroAnalysis
        }

    withGraph :: DeclDB l Out -> DeclDB l Out
    withGraph db = db{
          declUseGraph =
            DeclUseGraph.construct
              (keysSet db)
              (active AnyOrder db)
        }

    aux :: Build l -> ParseResult l Out -> Build l
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

          -- A conflict keeps the messages of its members for Bug-level
          -- reporting.
          conflictMember :: C.Conflict -> Build l -> C.DeclId -> Build l
          conflictMember conflict db' x =
            let newMsgs
                  | x == declId =
                      parseResultMsgs new ++ parseFailedMsg (parseResultToEntry new)
                  | otherwise =
                      []
                oldMsgs = case Map.lookup x db'.entries of
                  Nothing  -> []
                  Just old -> lookupBuildMsgs x db' ++ parseFailedMsg old
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

parseFailedMsg :: Entry l p -> [DelayedParseMsg]
parseFailedMsg = \case
    Inactive (InactiveDecl _ (ParseFailed msg)) ->
      [msg]
    _otherwise ->
      []

-- | The database under construction
data Build l = Build {
      entries   :: Map C.DeclId (Entry l Out)
      -- | Delayed messages: only @Parse@ has issued any so far
    , parseMsgs :: Map C.DeclId (NonEmpty DelayedParseMsg)
    }

lookupBuildMsgs :: C.DeclId -> Build l -> [DelayedParseMsg]
lookupBuildMsgs x build = foldMap Foldable.toList $ Map.lookup x build.parseMsgs

-- | Set an entry and its delayed messages
setEntry :: C.DeclId -> Entry l Out -> [DelayedParseMsg] -> Build l -> Build l
setEntry declId entry msgs db = Build{
      entries   = Map.insert declId entry db.entries
    , parseMsgs = case msgs of
        []     -> Map.delete declId db.parseMsgs
        m : ms -> Map.insert declId (m :| ms) db.parseMsgs
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
-- We should keep more information in a redefinition so we can apply the select
-- predicate to any of the declarations.

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

-- | Merge two definitions, keeping the delayed messages of both, each once
--
-- Two definitions of the same declaration often carry the same message, e.g.
-- two tentative definitions @int i; int i;@.
mergeRedefinition ::
     C.DeclId
  -> Entry l Out
     -- ^ The definition to keep
  -> [DelayedParseMsg]
     -- ^ The messages of the new definition
  -> Build l
  -> Build l
mergeRedefinition declId kept newMsgs db =
    setEntry declId kept (oldMsgs ++ filter (`notElem` oldMsgs) newMsgs) db
  where
    oldMsgs :: [DelayedParseMsg]
    oldMsgs = lookupBuildMsgs declId db

{-------------------------------------------------------------------------------
  Traversal
-------------------------------------------------------------------------------}

-- | The order of a traversal
data Order l p =
    AnyOrder
    -- | Topological over every edge, dependencies first; among entries that do
    -- not depend on each other, no order is promised.
  | AllDependencies
    -- | Topological over the 'NeedsShape' edges only, dependencies first. The
    -- key function is used to break ties (the lesser key @k@ comes first). If
    -- keys are equal, no order is promised.
  | forall k. Ord k => DependencyBy (Entry l p -> k)

-- | The outcome of processing one active declaration in a pass
data PassResult l p =
    Advanced   (C.Decl l p)
    -- ^ Still active, advanced to pass @p@
  | Squash     SquashTarget
    -- ^ Happens only in @MangleNames@
  | Deselect
    -- ^ Happens only in @Select@
  | Deactivate Deactivation

-- | Advance the active declarations; other entries are rewrapped unchanged
--
-- Keeps every dependency edge.
traverseActive ::
     forall f l p p'. (HasCallStack, Applicative f)
  => Order l p
  -> (C.Decl l p -> f (PassResult l p'))
  -> DeclDB l p
  -> f (DeclDB l p')
traverseActive order f db = case order of
    AnyOrder         -> traverseAll (const step) db
    AllDependencies  -> inOrder $ Digraph.sortBy compare db.declUseGraph.graph
    DependencyBy key -> inOrder $ Digraph.sortBy (comparing (key . entry)) shapeDependencies
  where
    step :: Entry l p -> f (Entry l p')
    step = \case
      Active   decl     -> advance decl <$> f decl
      Squashed info t   -> pure $ Squashed info t
      External binding  -> pure $ External binding
      Deselected info   -> pure $ Deselected info
      Inactive inactive -> pure $ Inactive inactive

    advance :: C.Decl l p -> PassResult l p' -> Entry l p'
    advance decl = \case
      Advanced decl' -> Active decl'
      Squash target  -> Squashed decl.info target
      Deselect -> Deselected decl.info
      Deactivate deactivation ->
        Inactive $ InactiveDecl decl.info (Deactivated deactivation)

    shapeDependencies :: Digraph.Digraph Dependency C.DeclId
    shapeDependencies = Digraph.filterEdges (== NeedsShape) db.declUseGraph.graph

    entry :: C.DeclId -> Entry l p
    entry declId = case Map.lookup declId db.entries of
      Just x  -> x
      Nothing -> panicPure $ "traverseActive: vertex without entry " ++ show declId

    inOrder :: [C.DeclId] -> f (DeclDB l p')
    inOrder declIds
      | length declIds /= Map.size db.entries =
          panicPure "traverseActive: the vertices are not the keys"
      | otherwise =
          withEntries db . Map.fromList <$>
            traverse (\declId -> (declId,) <$> step (entry declId)) declIds

-- | The active declarations
active :: Order l p -> DeclDB l p -> [C.Decl l p]
active order = getConst . traverseActive order collect
  where
    collect :: C.Decl l p -> Const [C.Decl l p] (PassResult l p)
    collect decl = Const [decl]

-- | Advance every entry
--
-- Internal use only: the signature permits any transition.
traverseAll ::
     Applicative f
  => (C.DeclId -> Entry l p -> f (Entry l p'))
  -> DeclDB l p
  -> f (DeclDB l p')
traverseAll f db = withEntries db <$> Map.traverseWithKey f db.entries

-- | Replace the entries
--
-- Precondition: the keys are unchanged.
withEntries :: DeclDB l p -> Map C.DeclId (Entry l p') -> DeclDB l p'
withEntries db entries = DeclDB{
      entries         = entries
    , delayedMsgs     = db.delayedMsgs
    , declUseGraph    = db.declUseGraph
    , ambiguousMacros = db.ambiguousMacros
    }

{-------------------------------------------------------------------------------
  Transitions
-------------------------------------------------------------------------------}

-- | Replace active or inactive declarations by external bindings
--
-- Deletes all edges of the externalised declarations: the binding is defined
-- elsewhere, and its users refer to it by 'C.TypeExtBinding'. Panics if an
-- identifier has no entry, or its entry is squashed or external.
externalize ::
     forall l p. HasCallStack
  => Map C.DeclId ResolvedExtBinding
  -> DeclDB l p
  -> DeclDB l p
externalize bindings db
    | not (Set.null unknown) =
        panicPure $ "externalize: unknown declarations " ++ show unknown
    | otherwise =
        deleteEdges . runIdentity $ traverseAll (\declId -> pure . step declId) db
  where
    deleteEdges :: DeclDB l p -> DeclDB l p
    deleteEdges db' = db'{
          declUseGraph =
              DeclUseGraph.deleteRevDeps ids
            $ DeclUseGraph.deleteDeps ids
            $ db'.declUseGraph
        }

    ids :: Set C.DeclId
    ids = Map.keysSet bindings

    unknown :: Set C.DeclId
    unknown = Set.difference ids (Map.keysSet db.entries)

    step :: C.DeclId -> Entry l p -> Entry l p
    step declId entry = case Map.lookup declId bindings of
      Nothing      -> entry
      Just binding -> case entry of
        Active{}   -> External binding
        Inactive{} -> External binding
        _otherwise ->
          panicPure $
            "externalize: unexpected " ++ entryKind entry ++ " entry "
              ++ show declId

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
  Dependency edges
-------------------------------------------------------------------------------}

-- | Delete the dependency edges of the given declarations
--
-- For example, when a type is opaqued, it no longer has dependencies.
deleteDeps :: Set C.DeclId -> DeclDB l p -> DeclDB l p
deleteDeps declIds db = db{
      declUseGraph = DeclUseGraph.deleteDeps declIds db.declUseGraph
    }

-- | Insert the dependency edges of a declaration
--
-- Panics if the declaration or one of its dependencies has no entry.
insertDepsOfDecl ::
     HasCallStack
  => C.Decl l ReparseMacroExpansions
  -> DeclDB l p
  -> DeclDB l p
insertDepsOfDecl decl db = db{
      declUseGraph = DeclUseGraph.insertDepsOfDecl decl db.declUseGraph
    }

{-------------------------------------------------------------------------------
  Query active declarations
-------------------------------------------------------------------------------}

-- | Lookup an active declaration.
lookup :: C.DeclId -> DeclDB l p -> Maybe (C.Decl l p)
lookup declId db = case Map.lookup declId db.entries of
    Just (Active decl) -> Just decl
    _otherwise         -> Nothing

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
      Squashed _ target ->
        lookupEntry target.c db >>= toOmitted
      Inactive (InactiveDecl info (Deactivated Omitted)) ->
        C.declPathRealPath (singleLocPath info.loc)
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
      Squashed info target
        | Set.member target.c targets ->
            (, target.hs) <$>
              C.declPathRealPath (singleLocPath info.loc)
      _otherwise ->
        Nothing
