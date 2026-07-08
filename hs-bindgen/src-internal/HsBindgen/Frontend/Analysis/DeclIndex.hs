-- | Declaration index
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
-- > import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
module HsBindgen.Frontend.Analysis.DeclIndex (
    DeclIndex -- opaque
    -- * Entry
  , Usable(..)
  , Unusable(..)
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
  , lookupUnusableLoc
  , keysSet
  , getOmitted
  , getSquashed
  , getUnusables
    -- * Support for macro failures
  , registerMacroTypecheckFailure
  , registerDelayedParseMsg
    -- * Support for the @PrepareReparse@ pass
  , registerDelayedPrepareReparseMsg
    -- * Support for the @ReparseMacroExpansions@ pass
  , registerDelayedReparseMacroExpansionsMsg
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
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Optics.Core (traverseOf)

import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex.ResolveMacro
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict (Conflict)
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict qualified as Conflict
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.EnrichComments.IsPass
import HsBindgen.Frontend.Pass.MangleNames.Error
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass.Msg
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass.Msg (DelayedReparseMacroExpansionsMsg)
import HsBindgen.Imports hiding (toList)
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass (IsPass)
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Macro.Error
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro
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
-- - D1 declares an anonymous @struct@
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
      map :: Map C.DeclId (Entry l)
    }
  deriving stock (Show, Generic)

{-------------------------------------------------------------------------------
  Entry
-------------------------------------------------------------------------------}

-- | Usable declaration
--
-- At each stage in the `hs-bindgen` pipeline, a 'Usable' declaration is a
-- declaration we think we can generate bindings for. Passes may replace
-- 'Usable' declarations with
--
-- - other 'Usable' declarations such as external declarations
--   ("ResolveBindingSpecs") or squashed declarations (in "MangleNames"); or
--
-- - 'Unusable' declarations, for example when macro typechecking fails
--   ("TypecheckMacros") or name mangling fails ("MangleNames").
--
-- At the end of the `hs-bindgen` pipeline, we can generate bindings for
-- 'Usable' declarations.
--
-- However, usability is not concerned with _transitivity_. Usable declarations
-- may have unusable transitive dependencies. Even though we can generate
-- bindings for usable declarations, they may not be functional because they
-- miss transitive dependencies.
--
-- (We avoid the term available, because it is overloaded with Clang's
-- CXAvailabilityKind).
data Usable l =
      UsableSuccess (Success l Out)
      -- TODO <https://github.com/well-typed/hs-bindgen/issues/1577>
      -- This should have a SingleLoc.
    | UsableExternal
      -- Squashed declarations are always "usable" because we only squash
      -- declaration in the list of declarations attached to the declaration
      -- unit.
    | UsableSquashed Squashed
    deriving stock (Show, Generic)

usableToLoc :: Usable l -> Maybe SingleLoc
usableToLoc = \case
    UsableSuccess  x -> Just x.decl.info.loc
    UsableExternal   -> Nothing
    UsableSquashed x -> Just x.typedefLoc

-- | Unusable declaration
--
-- A declaration is unusable if we cannot generate bindings for it.
--
-- See 'Usable'.
--
-- (We avoid the term available, because it is overloaded with Clang's
-- CXAvailabilityKind).
data Unusable =
      UnusableParseUnavailable       SingleLoc
    | UnusableParseFailure           SingleLoc DelayedParseMsg
    | UnusableConflict               Conflict
    | UnusableMangleNamesFailure     SingleLoc MangleNamesError
    | UnusableTypecheckMacrosError   SingleLoc MacroTypecheckError
    | UnusableMacroResolutionFailure SingleLoc MacroResolutionError

      -- | Omitted by prescriptive binding specifications
    | UnusableOmitted            SingleLoc
    deriving stock (Show, Generic)

instance PrettyForTrace Unusable where
  prettyForTrace = \case
    UnusableParseUnavailable{} ->
      "Declaration unavailable"
    UnusableParseFailure{} ->
      "Parse failed"
    UnusableConflict{} ->
      "Conflicting declarations"
    UnusableMangleNamesFailure{} ->
      "Name mangler failure"
    UnusableTypecheckMacrosError{} ->
      "Macro type-checking failed"
    UnusableMacroResolutionFailure{} ->
      "Macro name resolution failed"
    UnusableOmitted{} ->
      "Omitted by prescriptive binding specification"

unusableToLoc :: Unusable -> [SingleLoc]
unusableToLoc = \case
    UnusableParseUnavailable loc         -> [loc]
    UnusableParseFailure loc _           -> [loc]
    UnusableConflict conflict            -> Conflict.toList conflict
    UnusableMangleNamesFailure loc _     -> [loc]
    UnusableTypecheckMacrosError loc _   -> [loc]
    UnusableMacroResolutionFailure loc _ -> [loc]
    UnusableOmitted loc                  -> [loc]

data Success l p = Success {
    decl                              :: C.Decl l p
  , delayedParseMsgs                  :: [DelayedParseMsg]
  , delayedPrepareReparseMsgs         :: [DelayedPrepareReparseMsg]
  , delayedReparseMacroExpansionsMsgs :: [DelayedReparseMacroExpansionsMsg]
  }
  deriving stock (Generic)

deriving stock instance ( IsPass p
                        , Macro.HasTypes l
                        ) => Show (Success l p)

data Squashed = Squashed {
    -- | The location of the squashed typedef (i.e., _not_ the target)
    typedefLoc   :: SingleLoc
  , targetNameC  :: C.DeclId
  , targetNameHs :: Hs.Name Hs.NsTypeConstr
  }
  deriving stock (Show, Generic)

-- | Entry of declaration index
data Entry l =
    UsableE   (Usable l)
  | UnusableE Unusable
  deriving stock (Show, Generic)

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1577>
-- This should return NonEmpty
entryToLoc :: Entry l -> [SingleLoc]
entryToLoc = \case
  (UnusableE e) -> unusableToLoc e
  (UsableE   e) -> maybeToList $ usableToLoc e

entryToAvailability :: Entry l -> C.Availability
entryToAvailability = \case
    UsableE e   -> case e of
      UsableSuccess success -> success.decl.info.availability
      UsableExternal        -> C.Available
      UsableSquashed{}      -> C.Available
    UnusableE{} -> C.Available

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: DeclIndex l
empty = DeclIndex Map.empty

-- | Construct the declaration index, resolving macro names.
--
-- Macro resolution needs the set of all declaration IDs, which is only known
-- once the whole index has been built. We therefore build the index in two
-- stages: first 'buildIndex' constructs the index at 'EnrichComments' (so that
-- conflict detection can compare macro bodies, and so that we detect conflicts
-- even with macros that we cannot resolve), then 'resolveMacros' resolves each
-- successful declaration into the final index fixed to the
-- 'ConstructTranslationUnit' pass.
fromParseResults ::
     forall l. Macro.HasTypes l
  => Macro.Lang l
  -> [ParseResult l In]
  -> DeclIndex l
fromParseResults macroLang parseResults =
    buildIndex $ resolveMacros macroLang declIds parseResults
  where
    declIds :: Set C.DeclId
    declIds = getDeclIds parseResults

    getDeclIds :: [ParseResult l In] -> Set C.DeclId
    getDeclIds = Set.fromList . map (.id)

{-------------------------------------------------------------------------------
  Macro resolution
-------------------------------------------------------------------------------}

-- | Result of resolving macro names in a single parse result.
data ResolvedResult l =
    -- | Macro names were resolved successfully (or the declaration was not a
    -- macro, or was not a parse success to begin with).
    Resolved (ParseResult l Out)
    -- | Macro names could not be resolved.
  | Unresolved C.DeclId SingleLoc MacroResolutionError

resolvedResultId :: ResolvedResult l -> C.DeclId
resolvedResultId = \case
    Resolved result       -> result.id
    Unresolved declId _ _ -> declId

resolvedResultLoc :: ResolvedResult l -> SingleLoc
resolvedResultLoc = \case
    Resolved result    -> result.loc
    Unresolved _ loc _ -> loc

resolvedResultToEntry :: ResolvedResult l -> Entry l
resolvedResultToEntry = \case
    Resolved result      -> parseResultToEntry result
    Unresolved _ loc err -> UnusableE $ UnusableMacroResolutionFailure loc err
  where
    parseResultToEntry :: ParseResult l Out -> Entry l
    parseResultToEntry result = case result.classification of
      ParseResultSuccess r ->
        UsableE $ UsableSuccess (parseSuccessToSuccess r)
      ParseResultUnavailable ->
        UnusableE $ UnusableParseUnavailable result.loc
      ParseResultFailure r ->
        UnusableE $ UnusableParseFailure result.loc r

    parseSuccessToSuccess :: ParseSuccess l Out -> Success l Out
    parseSuccessToSuccess success = Success {
          decl = success.decl
        , delayedParseMsgs = success.delayedParseMsgs
        , delayedPrepareReparseMsgs = []
        , delayedReparseMacroExpansionsMsgs = []
        }

-- | Resolve macro names in every successful declaration.
--
-- A declaration whose macro names cannot be resolved becomes 'Unresolved', which
-- 'buildIndex' turns into an 'UnusableMacroResolutionFailure' entry.
resolveMacros ::
     forall l.
     Macro.Lang l
  -> Set C.DeclId
  -> [ParseResult l In]
  -> [ResolvedResult l]
resolveMacros macroLang allDeclIds = map resolveParseResult
  where
    resolveParseResult :: ParseResult l In -> ResolvedResult l
    resolveParseResult result =
      case traverseOf #classification resolveParseClassification result of
        Right resolved -> Resolved resolved
        Left  err      -> Unresolved result.id result.loc err

    resolveParseClassification ::
         ParseClassification l In
      -> Either MacroResolutionError (ParseClassification l Out)
    resolveParseClassification = \case
      ParseResultSuccess success ->
        case resolveMacroWith macroLang allDeclIds success.decl of
          Right resolvedDecl -> Right $
            ParseResultSuccess ParseSuccess{
              decl             = resolvedDecl
            , delayedParseMsgs = success.delayedParseMsgs
            }
          Left err -> Left err
      ParseResultUnavailable ->
        Right $ ParseResultUnavailable
      ParseResultFailure x ->
        Right $ ParseResultFailure x

{-------------------------------------------------------------------------------
  Build from resolved macros
-------------------------------------------------------------------------------}

-- This function checks for conflicts between ordinary declarations and macro
-- declarations, which must be done specially because they are in separate
-- namespaces. This is done here because we need to detect conflicts even with
-- macros that we cannot typecheck, which are thrown out in the
-- @TypecheckMacros@ pass.
buildIndex :: forall l. Macro.HasTypes l => [ResolvedResult l] -> DeclIndex l
buildIndex results = flip execState empty $ mapM_ aux results
  where
    aux :: ResolvedResult l -> State (DeclIndex l) ()
    aux new = modify' $ \index -> DeclIndex $
      let declId :: C.DeclId
          declId = resolvedResultId new

          mConflict :: Maybe (IsConflict l)
          mConflict = Foldable.asum [
              do
                old <- Map.lookup declId index.map
                pure $ checkIsConflict new (declId, old)
            , case declId.name.kind of
                C.NameKindOrdinary -> do
                  let altDeclId = C.DeclId{
                          name   = C.DeclName{
                              text = declId.name.text
                            , kind = C.NameKindMacro
                            }
                        , isAnon = False
                        }
                  old <- Map.lookup altDeclId index.map
                  pure $ checkIsConflict new (altDeclId, old)
                C.NameKindMacro    -> do
                  let altDeclId = C.DeclId{
                          name   = C.DeclName{
                              text = declId.name.text
                            , kind = C.NameKindOrdinary
                            }
                        , isAnon = False
                        }
                  old <- Map.lookup altDeclId index.map
                  pure $ checkIsConflict new (altDeclId, old)
                C.NameKindTagged{} -> Nothing
            ]
      in  case mConflict of
            Nothing ->
              Map.insert declId (resolvedResultToEntry new) index.map
            Just (Redefinition success) ->
              Map.insert declId (UsableE $ UsableSuccess success) index.map
            Just (SingleConflict ids conflict) ->
              Map.union
                ( Map.fromList
                    [ (x,UnusableE $ UnusableConflict conflict)
                    | x <- Set.toList ids
                    ]
                )
                index.map

{-------------------------------------------------------------------------------
  Conflicts
-------------------------------------------------------------------------------}

data IsConflict l =
    Redefinition (Success l ConstructTranslationUnit)
  | SingleConflict (Set C.DeclId) Conflict

checkIsConflict ::
     Macro.HasTypes l
  => ResolvedResult l
  -> (C.DeclId, Entry l)
  -> IsConflict l
checkIsConflict new (oldId, old) = case new of
    Resolved new' -> case (new'.classification, old) of
      (ParseResultSuccess newSuccess, UsableE (UsableSuccess oldSuccess))
        | newSuccess.decl.kind == oldSuccess.decl.kind ->
          -- TODO <https://github.com/well-typed/hs-bindgen/issues/2099?
          --
          -- We should make sure that we do not report delayed messages multiple
          -- times (e.g., use 'Data.List.nub').
          let delayedParseMsgs =
                newSuccess.delayedParseMsgs ++ oldSuccess.delayedParseMsgs
              delayedPrepareReparseMsgs =
                if not (null oldSuccess.delayedPrepareReparseMsgs) then
                  panicPure "expected empty prepare reparse messages"
                else
                  []
          in Redefinition $ oldSuccess{
                 delayedParseMsgs          = delayedParseMsgs
               , delayedPrepareReparseMsgs = delayedPrepareReparseMsgs
               }
      _otherwise -> singleConflict
    _otherwise -> singleConflict
  where
    singleConflict =
      let newLoc = resolvedResultLoc new
          conflict :: Conflict
          conflict = case old of
            UnusableE (UnusableConflict c) ->
              Conflict.insert c newLoc
            _otherwise ->
              Conflict.fromList $ newLoc : entryToLoc old
      in SingleConflict (Set.fromList [resolvedResultId new, oldId]) conflict

{-------------------------------------------------------------------------------
  Filter
-------------------------------------------------------------------------------}

filter ::
     (C.DeclId -> Entry l -> Bool)
  -> DeclIndex l
  -> DeclIndex l
filter p (DeclIndex entries) = DeclIndex (Map.filterWithKey p entries)

restrictKeys :: DeclIndex l -> Set C.DeclId -> DeclIndex l
restrictKeys index xs = DeclIndex $ Map.restrictKeys index.map xs

withoutKeys :: DeclIndex l -> Set C.DeclId -> DeclIndex l
withoutKeys index xs = DeclIndex $ Map.withoutKeys index.map xs

{-------------------------------------------------------------------------------
  Query parse successes
-------------------------------------------------------------------------------}

-- | Lookup parse success.
lookup :: C.DeclId -> DeclIndex l -> Maybe (C.Decl l Out)
lookup declId (DeclIndex i) = case Map.lookup declId i of
  Nothing                          -> Nothing
  Just (UsableE (UsableSuccess x)) -> Just $ x.decl
  _                                -> Nothing

-- | Get all parse successes.
getDecls :: DeclIndex l -> [C.Decl l Out]
getDecls index = mapMaybe toDecl $ Map.elems index.map
  where
    toDecl = \case
      UsableE (UsableSuccess x) -> Just x.decl
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
lookupLoc :: C.DeclId -> DeclIndex l -> [SingleLoc]
lookupLoc d i = case lookupEntry d i of
  Nothing -> []
  Just e  -> entryToLoc e

-- | Get the source locations of an unusable declaration.
lookupUnusableLoc :: C.DeclId -> DeclIndex l -> [SingleLoc]
lookupUnusableLoc d i = case lookupEntry d i of
  Just (UnusableE  e) -> unusableToLoc e
  _otherwise          -> []

-- | Get the identifiers of all declarations in the index.
keysSet :: DeclIndex l -> Set C.DeclId
keysSet index = Map.keysSet index.map

-- | Get omitted entries.
getOmitted :: DeclIndex l -> Map C.DeclId SourcePath
getOmitted index = Map.mapMaybe toOmitted index.map
  where
    toOmitted :: Entry l -> Maybe SourcePath
    toOmitted = \case
      UsableE (UsableSquashed e) -> lookupEntry e.targetNameC index >>= toOmitted
      UsableE{}                  -> Nothing
      UnusableE e                -> case e of
        UnusableOmitted sloc -> Just sloc.singleLocPath
        _otherEntry          -> Nothing

-- | Get squashed entries.
--
-- TODO <https://github.com/well-typed/hs-bindgen/issues/1549>
-- We may no longer need `getSquashed` once we properly record lists of aliases.
getSquashed ::
     DeclIndex l
  -> Set C.DeclId
  -> Map C.DeclId (SourcePath, Hs.Name Hs.NsTypeConstr)
getSquashed index targets = Map.mapMaybe onlySquashedTargetingSet index.map
  where
    onlySquashedTargetingSet ::
         Entry l
      -> Maybe (SourcePath, Hs.Name Hs.NsTypeConstr)
    onlySquashedTargetingSet = \case
      UsableE (UsableSquashed e) ->
        if Set.member e.targetNameC targets then
          Just (e.typedefLoc.singleLocPath, e.targetNameHs)
        else
          Nothing
      _otherwise  -> Nothing

-- | Restrict the declaration index to unusable declarations in a given set.
getUnusables :: DeclIndex l -> Set C.DeclId -> Map C.DeclId Unusable
getUnusables index = Map.mapMaybe onlyUnusable . (.map) . restrictKeys index
  where
    onlyUnusable :: Entry l -> Maybe Unusable
    onlyUnusable = \case
      UsableE   _ -> Nothing
      UnusableE e -> Just e

{-------------------------------------------------------------------------------
  Support for delayed parse messages
-------------------------------------------------------------------------------}

-- | Append a delayed parse message to an existing 'UsableSuccess' entry.
--
-- Has no effect if the declaration is not a parse success (e.g., if it is a
-- parse failure or a macro failure).
registerDelayedParseMsg ::
     (C.DeclId, DelayedParseMsg)
  -> DeclIndex l
  -> DeclIndex l
registerDelayedParseMsg (declId, msg) (DeclIndex i) = DeclIndex $
    Map.adjust addMsg declId i
  where
    addMsg :: Entry l -> Entry l
    addMsg (UsableE (UsableSuccess ps)) =
      UsableE $ UsableSuccess ps {
          HsBindgen.Frontend.Analysis.DeclIndex.delayedParseMsgs =
            ps.delayedParseMsgs ++ [msg]
        }
    addMsg entry = entry

{-------------------------------------------------------------------------------
  Support for macro failures
-------------------------------------------------------------------------------}

registerMacroTypecheckFailure
  :: (C.DeclId, SingleLoc, MacroTypecheckError) -> DeclIndex l -> DeclIndex l
registerMacroTypecheckFailure (declId, loc, err) (DeclIndex i) = DeclIndex $
    Map.insert declId (UnusableE $ UnusableTypecheckMacrosError loc err) i

{-------------------------------------------------------------------------------
  Support for @PrepareReparse@ pass
-------------------------------------------------------------------------------}

-- | Append a delayed @PrepareReparse@ message to an existing 'UsableSuccess'
-- entry.
--
-- Has no effect if the declaration is not a success
registerDelayedPrepareReparseMsg ::
     (C.DeclId, DelayedPrepareReparseMsg)
  -> DeclIndex l
  -> DeclIndex l
registerDelayedPrepareReparseMsg (declId, msg) (DeclIndex i) = DeclIndex $
    Map.adjust addMsg declId i
  where
    addMsg :: Entry l -> Entry l
    addMsg (UsableE (UsableSuccess ps)) =
      UsableE $ UsableSuccess ps{
          delayedPrepareReparseMsgs = ps.delayedPrepareReparseMsgs ++ [msg]
        }
    addMsg entry = entry


{-------------------------------------------------------------------------------
  Support for @ReparseMacroExpansions@ pass
-------------------------------------------------------------------------------}

-- | Append a delayed @ReparseMacroExpansions@ message to an existing
-- 'UsableSuccess' entry.
--
-- Has no effect if the declaration is not a success
registerDelayedReparseMacroExpansionsMsg ::
     (C.DeclId, DelayedReparseMacroExpansionsMsg)
  -> DeclIndex l
  -> DeclIndex l
registerDelayedReparseMacroExpansionsMsg (declId, msg) (DeclIndex i) = DeclIndex $
    Map.adjust addMsg declId i
  where
    addMsg :: Entry l -> Entry l
    addMsg (UsableE (UsableSuccess ps)) =
      UsableE $ UsableSuccess ps{
          delayedReparseMacroExpansionsMsgs = ps.delayedReparseMacroExpansionsMsgs ++ [msg]
        }
    addMsg entry = entry

{-------------------------------------------------------------------------------
  Support for binding specifications
-------------------------------------------------------------------------------}

registerOmittedDeclarations ::
     Map C.DeclId SingleLoc
  -> DeclIndex l
  -> DeclIndex l
registerOmittedDeclarations xs index = DeclIndex $
    Map.union (UnusableE . UnusableOmitted <$> xs) index.map

registerExternalDeclarations :: Set C.DeclId -> DeclIndex l -> DeclIndex l
registerExternalDeclarations xs index = Foldable.foldl' insert index xs
  where
    insert :: DeclIndex l -> C.DeclId -> DeclIndex l
    insert (DeclIndex i) x =
      DeclIndex $ Map.insert x (UsableE UsableExternal) i

{-------------------------------------------------------------------------------
  Support for mangle names
-------------------------------------------------------------------------------}

registerSquashedDeclarations ::
     Map C.DeclId Squashed
  -> DeclIndex l
  -> DeclIndex l
registerSquashedDeclarations xs index = DeclIndex $
    Map.union (UsableE . UsableSquashed <$> xs) index.map

registerMangleNamesFailure ::
     Map C.DeclId (SingleLoc, MangleNamesError)
  -> DeclIndex l
  -> DeclIndex l
registerMangleNamesFailure xs index = DeclIndex $
    Map.union (UnusableE . uncurry UnusableMangleNamesFailure <$> xs) index.map
