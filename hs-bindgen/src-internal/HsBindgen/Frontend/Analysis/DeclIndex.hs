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
import Data.List.NonEmpty ((<|))
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set

import C.Expr.Syntax qualified as CExpr

import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.Errors
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict (Conflict)
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict qualified as Conflict
import HsBindgen.Frontend.Pass.EnrichComments.IsPass
import HsBindgen.Frontend.Pass.MangleNames.Error
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.TypecheckMacros.Error
import HsBindgen.Imports hiding (toList)
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Index of all declarations
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
data DeclIndex = DeclIndex {
      map :: Map DeclId Entry
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
data Usable =
      UsableSuccess (ParseSuccess EnrichComments)
      -- TODO <https://github.com/well-typed/hs-bindgen/issues/1577>
      -- This should have a SingleLoc.
    | UsableExternal
      -- Squashed declarations are always "usable" because we only squash
      -- declaration in the list of declarations attached to the declaration
      -- unit.
    | UsableSquashed Squashed
    deriving stock (Show, Generic)

usableToLoc :: Usable -> Maybe SingleLoc
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
      -- Historically, there were more reasons for not attempting a parse,
      -- that's why we store a non-empty list of reasons. We could remove this
      -- indirection.
      UnusableParseNotAttempted   SingleLoc (NonEmpty ParseNotAttempted)
    | UnusableParseFailure        SingleLoc DelayedParseMsg
    | UnusableConflict            Conflict
    | UnusableMangleNamesFailure  SingleLoc MangleNamesError
    | UnusableMacroTypecheckError SingleLoc MacroTypecheckError

      -- | Omitted by prescriptive binding specifications
    | UnusableOmitted            SingleLoc
    deriving stock (Show, Generic)

instance PrettyForTrace Unusable where
  prettyForTrace = \case
    UnusableParseNotAttempted{} ->
      "Parse not attempted"
    UnusableParseFailure{} ->
      "Parse failed"
    UnusableConflict{} ->
      "Conflicting declarations"
    UnusableMangleNamesFailure{} ->
      "Name mangler failure"
    UnusableMacroTypecheckError{} ->
      "Macro type-checking failed"
    UnusableOmitted{} ->
      "Omitted by prescriptive binding specification"

unusableToLoc :: Unusable -> [SingleLoc]
unusableToLoc = \case
    UnusableParseNotAttempted loc _   -> [loc]
    UnusableParseFailure loc _        -> [loc]
    UnusableConflict conflict         -> Conflict.toList conflict
    UnusableMangleNamesFailure loc _  -> [loc]
    UnusableMacroTypecheckError loc _ -> [loc]
    UnusableOmitted loc               -> [loc]

data Squashed = Squashed {
    -- | The location of the squashed typedef (i.e., _not_ the target)
    typedefLoc   :: SingleLoc
  , targetNameC  :: DeclId
    -- | 'Nothing' if target declaration is not in the list of declarations
    -- (e.g., it was not parsed).
  , targetNameHs :: Maybe (Hs.Name Hs.NsTypeConstr)
  }
  deriving stock (Show, Generic)

-- | Entry of declaration index
data Entry =
    UsableE   Usable
  | UnusableE Unusable
  deriving stock (Show, Generic)

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1577>
-- This should return NonEmpty
entryToLoc :: Entry -> [SingleLoc]
entryToLoc = \case
  (UnusableE e) -> unusableToLoc e
  (UsableE   e) -> maybeToList $ usableToLoc e

entryToAvailability :: Entry -> C.Availability
entryToAvailability = \case
    UsableE e   -> case e of
      UsableSuccess success -> success.decl.info.availability
      UsableExternal        -> C.Available
      UsableSquashed{}      -> C.Available
    UnusableE{} -> C.Available

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: DeclIndex
empty = DeclIndex Map.empty

-- This function checks for conflicts between ordinary declarations and macro
-- declarations, which must be done specially because they are in separate
-- namespaces. This is done here because we need to detect conflicts even with
-- macros that we cannot typecheck, which are thrown out in the
-- @TypecheckMacros@ pass.
fromParseResults :: [ParseResult EnrichComments] -> DeclIndex
fromParseResults results = flip execState empty $ mapM_ aux results
  where
    aux :: ParseResult EnrichComments -> State DeclIndex ()
    aux new = modify' $ \index -> DeclIndex $
      let mConflict :: Maybe (Either Entry ((DeclId, DeclId), Entry))
          mConflict = Foldable.asum [
              Left <$> Map.lookup new.id index.map
            , fmap Right $ case new.id.name.kind of
                CNameKindOrdinary ->
                  let altDeclId = DeclId{
                          name   = new.id.name{ kind = CNameKindMacro }
                        , isAnon = False
                        }
                  in  ((new.id, altDeclId),) <$> Map.lookup altDeclId index.map
                CNameKindTagged{} -> Nothing
                CNameKindMacro    ->
                  let altDeclId = DeclId{
                          name   = new.id.name{ kind = CNameKindOrdinary }
                        , isAnon = False
                        }
                  in  ((altDeclId, new.id),) <$> Map.lookup altDeclId index.map
            ]
      in  case mConflict of
            -- No conflict
            Nothing ->
              Map.insert new.id (parseResultToEntry new) index.map
            -- Conflict in same namespace
            Just (Left entry) ->
              handleConflict new.id new entry index.map
            -- Conflict between ordinary and macro
            Just (Right ((ordinaryDeclId, macroDeclId), entry)) ->
              -- Store the conflict for both declaration IDs, the ordinary kind,
              -- and the macro kind.
              handleConflict ordinaryDeclId  new entry $
              handleConflict macroDeclId     new entry $
              index.map

    handleConflict ::
         DeclId
      -> ParseResult EnrichComments
      -> Entry
      -> Map DeclId Entry
      -> Map DeclId Entry
    handleConflict declId new = \case
      UsableE oldUsable -> case oldUsable of
        UsableSuccess oldSuccess -> case new.classification of
          ParseResultSuccess newSuccess
            -- Redeclaration with the same definition can happen with opaque
            -- structs, for example.  We stick with the first declaration.
            | sameDefinition oldSuccess.decl.kind newSuccess.decl.kind -> id
            -- Redeclaration of macros cannot be supported.
            | otherwise ->
                let conflict = UnusableE $ UnusableConflict $
                      Conflict.between oldSuccess.decl.info.loc new.loc
                in  Map.insert declId conflict
          ParseResultNotAttempted{} -> id
          ParseResultFailure{} -> Map.insert declId (parseResultToEntry new)
        UsableExternal   -> panicPure "Unexpected UsableExternal"
        UsableSquashed s -> panicPure $ "Unexpected Squashed: " <> show s
      UnusableE oldUnusable -> case oldUnusable of
        UnusableParseNotAttempted loc nasOld
          | ParseResultNotAttempted naNew <- new.classification ->
              Map.insert declId $
                UnusableE (UnusableParseNotAttempted loc (naNew <| nasOld))
          | otherwise -> Map.insert declId $ parseResultToEntry new
        UnusableParseFailure{} -> id
        UnusableConflict c -> Map.insert declId . UnusableE . UnusableConflict $
          Conflict.insert c new.loc
        UnusableMangleNamesFailure _ x ->
          panicPure $
            "Unexpected UnusableMangleNamesFailure: " <> show x
        UnusableMacroTypecheckError loc err ->
          panicPure $
            "Unexpected UnusableMacroTypecheckError: " <> show loc <> " " <> show err
        UnusableOmitted x ->
          panicPure $
            "Unexpected UnusableOmitted: " <> show x

    parseResultToEntry :: ParseResult EnrichComments -> Entry
    parseResultToEntry result = case result.classification of
      ParseResultSuccess r ->
        UsableE $ UsableSuccess r
      ParseResultNotAttempted r ->
        UnusableE $ UnusableParseNotAttempted result.loc $ r :| []
      ParseResultFailure r ->
        UnusableE $ UnusableParseFailure result.loc r

    sameDefinition ::
         C.DeclKind EnrichComments
      -> C.DeclKind EnrichComments
      -> Bool
    sameDefinition a b = case (a, b) of
      (C.DeclMacro macroA, C.DeclMacro macroB) ->
         CExpr.sameMacro macroA.parsedMacro macroB.parsedMacro
      _otherwise                               -> a == b

{-------------------------------------------------------------------------------
  Filter
-------------------------------------------------------------------------------}

filter :: (DeclId -> Entry -> Bool) -> DeclIndex -> DeclIndex
filter p (DeclIndex entries) = DeclIndex (Map.filterWithKey p entries)

restrictKeys :: DeclIndex -> Set DeclId -> DeclIndex
restrictKeys index xs = DeclIndex $ Map.restrictKeys index.map xs

withoutKeys :: DeclIndex -> Set DeclId -> DeclIndex
withoutKeys index xs = DeclIndex $ Map.withoutKeys index.map xs

{-------------------------------------------------------------------------------
  Query parse successes
-------------------------------------------------------------------------------}

-- | Lookup parse success.
lookup :: DeclId -> DeclIndex -> Maybe (C.Decl EnrichComments)
lookup declId (DeclIndex i) = case Map.lookup declId i of
  Nothing                          -> Nothing
  Just (UsableE (UsableSuccess x)) -> Just $ x.decl
  _                                -> Nothing

-- | Get all parse successes.
getDecls :: DeclIndex -> [C.Decl EnrichComments]
getDecls index = mapMaybe toDecl $ Map.elems index.map
  where
    toDecl = \case
      UsableE (UsableSuccess x) -> Just x.decl
      _otherEntries             -> Nothing

{-------------------------------------------------------------------------------
  Other queries
-------------------------------------------------------------------------------}

-- | Lookup an entry of a declaration index.
lookupEntry :: DeclId -> DeclIndex -> Maybe Entry
lookupEntry x index = Map.lookup x index.map

-- | Get all entries of a declaration index.
toList :: DeclIndex -> [(DeclId, Entry)]
toList index = Map.toList index.map

-- | Get the source locations of a declaration.
lookupLoc :: DeclId -> DeclIndex -> [SingleLoc]
lookupLoc d i = case lookupEntry d i of
  Nothing -> []
  Just e  -> entryToLoc e

-- | Get the source locations of an unusable declaration.
lookupUnusableLoc :: DeclId -> DeclIndex -> [SingleLoc]
lookupUnusableLoc d i = case lookupEntry d i of
  Just (UnusableE  e) -> unusableToLoc e
  _otherwise          -> []

-- | Get the identifiers of all declarations in the index.
keysSet :: DeclIndex -> Set DeclId
keysSet index = Map.keysSet index.map

-- | Get omitted entries.
getOmitted :: DeclIndex -> Map DeclId SourcePath
getOmitted index = Map.mapMaybe toOmitted index.map
  where
    toOmitted :: Entry -> Maybe SourcePath
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
     DeclIndex
  -> Set DeclId
  -> Map DeclId (SourcePath, Hs.Name Hs.NsTypeConstr)
getSquashed index targets = Map.mapMaybe onlySquashedTargetingSet index.map
  where
    onlySquashedTargetingSet :: Entry -> Maybe (SourcePath, Hs.Name Hs.NsTypeConstr)
    onlySquashedTargetingSet = \case
      UsableE (UsableSquashed e) ->
        case (e.targetNameHs, Set.member e.targetNameC targets) of
          (Just nameHs, True) -> Just (e.typedefLoc.singleLocPath, nameHs)
          _otherwise          -> Nothing
      _otherwise  -> Nothing

-- | Restrict the declaration index to unusable declarations in a given set.
getUnusables :: DeclIndex -> Set DeclId -> Map DeclId Unusable
getUnusables index = Map.mapMaybe onlyUnusable . (.map) . restrictKeys index
  where
    onlyUnusable :: Entry -> Maybe Unusable
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
registerDelayedParseMsg :: (DeclId, DelayedParseMsg) -> DeclIndex -> DeclIndex
registerDelayedParseMsg (declId, msg) (DeclIndex i) = DeclIndex $
    Map.adjust addMsg declId i
  where
    addMsg :: Entry -> Entry
    addMsg (UsableE (UsableSuccess ps)) =
      UsableE $ UsableSuccess ps{
          delayedParseMsgs = ps.delayedParseMsgs ++ [msg]
        }
    addMsg entry = entry

{-------------------------------------------------------------------------------
  Support for macro failures
-------------------------------------------------------------------------------}

registerMacroTypecheckFailure
  :: (DeclId, SingleLoc, MacroTypecheckError) -> DeclIndex -> DeclIndex
registerMacroTypecheckFailure (declId, loc, err) (DeclIndex i) = DeclIndex $
    Map.insert declId (UnusableE $ UnusableMacroTypecheckError loc err) i

{-------------------------------------------------------------------------------
  Support for binding specifications
-------------------------------------------------------------------------------}

registerOmittedDeclarations :: Map DeclId SingleLoc -> DeclIndex -> DeclIndex
registerOmittedDeclarations xs index = DeclIndex $
    Map.union (UnusableE . UnusableOmitted <$> xs) index.map

registerExternalDeclarations :: Set DeclId -> DeclIndex -> DeclIndex
registerExternalDeclarations xs index = Foldable.foldl' insert index xs
  where
    insert :: DeclIndex -> DeclId -> DeclIndex
    insert (DeclIndex i) x =
      DeclIndex $ Map.insert x (UsableE UsableExternal) i

{-------------------------------------------------------------------------------
  Support for mangle names
-------------------------------------------------------------------------------}

registerSquashedDeclarations :: Map DeclId Squashed -> DeclIndex -> DeclIndex
registerSquashedDeclarations xs index = DeclIndex $
    Map.union (UsableE . UsableSquashed <$> xs) index.map

registerMangleNamesFailure ::
  Map DeclId (SingleLoc, MangleNamesError) -> DeclIndex -> DeclIndex
registerMangleNamesFailure xs index = DeclIndex $
    Map.union (UnusableE . uncurry UnusableMangleNamesFailure <$> xs) index.map
