{-# LANGUAGE OverloadedLabels #-}

-- | Declaration index
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
-- > import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
module HsBindgen.Frontend.Analysis.DeclIndex (
    Usable(..)
  , Unusable(..)
  , Entry(..)
  , DeclIndex
    -- * Construction
  , fromParseResults
    -- * Query parse successes
  , lookup
  , (!)
  , getDecls
    -- * Other queries
  , lookupEntry
  , toList
  , lookupLoc
  , lookupUnusableLoc
  , keysSet
  , getOmitted
    -- * Support for macro failures
  , registerMacroFailures
    -- * Support for binding specifications
  , registerOmittedDeclarations
  , registerExternalDeclarations
    -- * Support for selection
  , Match
  , selectDeclIndex
  , getUnusables
  ) where

import Prelude hiding (lookup)

import Control.Monad.State
import Data.Foldable qualified as Foldable
import Data.Function
import Data.Map.Strict qualified as Map
import Optics.Core (over)

import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.Errors
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict
import HsBindgen.Frontend.Pass.HandleMacros.Error
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports hiding (toList)
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Usable declaration
--
-- A declaration is usable if we successfully reified the declaration or if it
-- is external.
--
-- (We avoid the term available, because it is overloaded with Clang's
-- CXAvailabilityKind).
data Usable =
      UsableSuccess ParseSuccess
      -- TODO https://github.com/well-typed/hs-bindgen/issues/1273: Attach
      -- information required to match the select predicate also to external
      -- declarations.
    | UsableExternal
    deriving stock (Show, Generic)

-- | Unusable declaration
--
-- A declaration is unusable if we did not reify the declaration. We can not
-- generate bindings for unusable declarations.
--
-- (We avoid the term available, because it is overloaded with Clang's
-- CXAvailabilityKind).
data Unusable =
      UnusableParseNotAttempted ParseNotAttempted
    | UnusableParseFailure      ParseFailure
    | UnusableConflict          ConflictingDeclarations
    | UnusableFailedMacro       FailedMacro
      -- TODO https://github.com/well-typed/hs-bindgen/issues/1273: Attach
      -- information required to match the select predicate also to omitted
      -- declarations.
    | UnusableOmitted           (C.QualName, SourcePath)
    deriving stock (Show, Generic)

instance PrettyForTrace Unusable where
  prettyForTrace = \case
    UnusableParseNotAttempted{} ->
      "parse not attempted: (!) adjust parse predicate"
    UnusableParseFailure{} ->
      "parse failed"
    UnusableConflict{} ->
      "conflicting declarations"
    UnusableFailedMacro{} ->
      "macro parsing or type-checking failed"
    UnusableOmitted{} ->
      "omitted by prescriptive binding specification"

-- | Entry of declaration index
data Entry = UsableE Usable | UnusableE Unusable
    deriving stock (Show, Generic)

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
newtype DeclIndex = DeclIndex {
      unDeclIndex   :: Map C.QualPrelimDeclId Entry
    }
  deriving stock (Show, Generic)

emptyIndex :: DeclIndex
emptyIndex = DeclIndex Map.empty

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromParseResults :: [ParseResult] -> DeclIndex
fromParseResults results = flip execState emptyIndex $ mapM_ aux results
  where
    aux :: ParseResult -> State DeclIndex ()
    aux new = modify' $
        DeclIndex . Map.alter (handleDeclaration declId new) declId . unDeclIndex
      where
        declId :: C.QualPrelimDeclId
        declId = getQualPrelimDeclId new

    handleDeclaration ::
      C.QualPrelimDeclId -> ParseResult -> Maybe Entry -> Maybe Entry
    handleDeclaration declId new = \case
      Nothing -> Just $ parseResultToEntry new
      -- We remove duplicates with /different/ values and store them as
      -- 'ConflictingDeclarations'. We could detect and handle some but not all
      -- of these duplicates; for now, we remove them all.
      --
      -- Duplicates may arise, for example, if a declaration is redefined by a
      -- macro; for other kinds of declarations, clang will have reported an
      -- error already.
      --
      -- There are cases where one declaration is an actual C construct like a
      -- variable declaration, but the new declaration is a macro of the same
      -- name that simply defers to the C construct. This is apparently a valid
      -- pattern, which for example occurs in @stdio.h@:
      --
      -- > typedef int FILE;
      -- > extern FILE *const stdin;
      -- > #define stdin (stdin)
      --
      -- Note that in examples like this, we will always "succeed" in parsing
      -- the macro, because proper macro handling does not happen until after
      -- the @DeclIndex@ has been built (at this point the macro is merely a
      -- list of tokens). So whether the macro is something we can handle or not
      -- is irrelevant at this point.
      Just oldDuplicate -> Just $ case oldDuplicate of
        UsableE oldUsable -> case oldUsable of
          UsableSuccess oldParseSuccess
            -- Redeclaration but with the same definition. This can happen, for
            -- example for opaque structs. We stick with the first but add the
            -- parse messages of the second.
            | ParseResultSuccess newParseSuccess <- new
            , sameParseResult (ParseResultSuccess oldParseSuccess) new ->
                UsableE $ UsableSuccess $ over #psAttachedMsgs
                  (++ newParseSuccess.psAttachedMsgs)
                  oldParseSuccess
            | otherwise ->
                newConflict oldParseSuccess.psDecl.declInfo.declLoc
          -- TODO_PR: Should not happen because we have no external entries yet.
          -- Panic?
          UsableExternal -> UsableE UsableExternal
        UnusableE oldUnusable -> case oldUnusable of
          (UnusableParseNotAttempted r)
            | sameParseResult (ParseResultNotAttempted r) new ->
                oldDuplicate
            | otherwise ->
                newConflict (unParseNotAttempted r).loc
          UnusableParseFailure r
            | sameParseResult (ParseResultFailure r) new ->
                oldDuplicate
            | otherwise ->
                newConflict (unParseFailure r).loc
          UnusableConflict c -> addConflict c
          -- TODO_PR: Should not happen because we have no failed macros yet.
          -- Panic?
          UnusableFailedMacro r -> UnusableE $ UnusableFailedMacro r
          -- TODO_PR: Should not happen because we have no omitted entries yet.
          -- Panic?
          UnusableOmitted o -> UnusableE $ UnusableOmitted o
      where
        newLoc :: SingleLoc
        newLoc = case new of
          ParseResultSuccess       ParseSuccess{psDecl} -> psDecl.declInfo.declLoc
          ParseResultNotAttempted (ParseNotAttempted m) -> m.loc
          ParseResultFailure      (ParseFailure m)      -> m.loc

        addConflict :: ConflictingDeclarations -> Entry
        addConflict c =
          UnusableE $ UnusableConflict $
            addConflictingLoc c newLoc

        newConflict :: SingleLoc -> Entry
        newConflict oldLoc =
          UnusableE $ UnusableConflict $
            conflictingDeclarations declId oldLoc newLoc

        parseResultToEntry :: ParseResult -> Entry
        parseResultToEntry = \case
          ParseResultSuccess      r -> UsableE   $ UsableSuccess             r
          ParseResultNotAttempted r -> UnusableE $ UnusableParseNotAttempted r
          ParseResultFailure      r -> UnusableE $ UnusableParseFailure      r

sameParseResult :: ParseResult -> ParseResult -> Bool
sameParseResult a b = case (a, b) of
  (ParseResultSuccess x1      , ParseResultSuccess x2) ->
    sameDefinition x1.psDecl.declKind x2.psDecl.declKind
  (ParseResultNotAttempted x1 , ParseResultNotAttempted x2) -> x1 == x2
  (ParseResultFailure x1      , ParseResultFailure x2)      -> x1 == x2
  (_                          , _)                          -> False

sameDefinition :: C.DeclKind Parse -> C.DeclKind Parse -> Bool
sameDefinition a b =
    case (a, b) of
      (C.DeclMacro macroA, C.DeclMacro macroB) ->
        sameMacro macroA macroB
      _otherwise ->
        a == b

sameMacro :: UnparsedMacro -> UnparsedMacro -> Bool
sameMacro = (==) `on` (map tokenSpelling . unparsedTokens)

{-------------------------------------------------------------------------------
  Query parse successes
-------------------------------------------------------------------------------}

-- | Lookup parse success.
lookup :: C.QualPrelimDeclId -> DeclIndex -> Maybe (C.Decl Parse)
lookup qualPrelimDeclId (DeclIndex i) = case Map.lookup qualPrelimDeclId i of
  Nothing                          -> Nothing
  Just (UsableE (UsableSuccess x)) -> Just $ x.psDecl
  _                                -> Nothing

-- | Unsafe! Get parse success.
(!) :: HasCallStack => DeclIndex -> C.QualPrelimDeclId -> C.Decl Parse
(!) declIndex qualPrelimDeclId =
    fromMaybe (panicPure $ "Unknown key: " ++ show qualPrelimDeclId) $
       lookup qualPrelimDeclId declIndex

-- | Get all parse successes.
getDecls :: DeclIndex -> [C.Decl Parse]
getDecls = mapMaybe toDecl . Map.elems . unDeclIndex
  where
    toDecl = \case
      UsableE (UsableSuccess x) -> Just x.psDecl
      _otherEntries             -> Nothing

{-------------------------------------------------------------------------------
  Other queries
-------------------------------------------------------------------------------}

-- | Lookup an entry of a declaration index.
lookupEntry :: C.QualPrelimDeclId -> DeclIndex -> Maybe Entry
lookupEntry x = Map.lookup x . unDeclIndex

-- | Get all entries of a declaration index.
toList :: DeclIndex -> [(C.QualPrelimDeclId, Entry)]
toList = Map.toList . unDeclIndex

-- | Get the source location of a declaration.
lookupLoc :: C.QualPrelimDeclId -> DeclIndex -> Maybe SingleLoc
lookupLoc d (DeclIndex i) = case Map.lookup d i of
  Nothing            -> Nothing
  Just (UsableE e)   -> case e of
    UsableSuccess x -> Just $ x.psDecl.declInfo.declLoc
    UsableExternal  -> Nothing
  Just (UnusableE e) -> case e of
    UnusableParseNotAttempted (ParseNotAttempted x) -> Just $ x.loc
    UnusableParseFailure (ParseFailure x)           -> Just $ x.loc
    -- TODO_PR: What should we do for conflicting declarations? Return the
    -- minimum location (relevant for sorting select messages), return all
    -- locations, or return none? See also 'lookupUnusableLoc' below.
    UnusableConflict x                              -> Just $ getMinimumLoc x
    UnusableFailedMacro (FailedMacro x)             -> Just $ x.loc
    UnusableOmitted{}                               -> Nothing

-- TODO_PR: We use this function when looking for missing declarations during
-- resolution of external binding specifications. Do we want to get all
-- locations for conflicting declarations here?
lookupUnusableLoc :: C.QualPrelimDeclId -> DeclIndex -> [SingleLoc]
lookupUnusableLoc d (DeclIndex i) = case Map.lookup d i of
  Nothing            -> []
  Just (UsableE _)   -> []
  Just (UnusableE e) -> case e of
    UnusableParseNotAttempted (ParseNotAttempted x) -> [x.loc]
    UnusableParseFailure (ParseFailure x)           -> [x.loc]
    UnusableConflict x                              -> getLocs x
    UnusableFailedMacro (FailedMacro x)             -> [x.loc]
    UnusableOmitted{}                               -> []

-- | Get the identifiers of all declarations in the index.
keysSet :: DeclIndex -> Set C.QualPrelimDeclId
keysSet = Map.keysSet . unDeclIndex

-- | Get omitted entries.
getOmitted :: DeclIndex -> Map C.QualPrelimDeclId (C.QualName, SourcePath)
getOmitted = Map.mapMaybe toOmitted . unDeclIndex
  where
    toOmitted :: Entry -> Maybe (C.QualName, SourcePath)
    toOmitted = \case
      UsableE _ -> Nothing
      UnusableE e -> case e of
        UnusableOmitted x -> Just x
        _otherEntry       -> Nothing

{-------------------------------------------------------------------------------
  Support for macro failures
-------------------------------------------------------------------------------}

registerMacroFailures :: [FailedMacro] -> DeclIndex -> DeclIndex
registerMacroFailures xs index = Foldable.foldl' insert index xs
  where
    insert :: DeclIndex -> FailedMacro -> DeclIndex
    insert (DeclIndex i) x =
      DeclIndex $ Map.insert (unFailedMacro x).declId (UnusableE $ UnusableFailedMacro x) i

{-------------------------------------------------------------------------------
  Support for selection
-------------------------------------------------------------------------------}

-- Match function to find selection roots.
type Match = C.QualPrelimDeclId -> SingleLoc -> C.Availability -> Bool

selectDeclIndex :: Match -> DeclIndex -> DeclIndex
selectDeclIndex p = DeclIndex . Map.filter matchEntry . unDeclIndex
  where
    matchEntry :: Entry -> Bool
    matchEntry = \case
      UsableE e -> case e of
        UsableSuccess (ParseSuccess i d _) ->
          p i d.declInfo.declLoc d.declInfo.declAvailability
        -- TODO_PR: We should match external declarations.
        UsableExternal ->
          False
      UnusableE e -> case e of
        UnusableParseNotAttempted (ParseNotAttempted m) ->
          matchMsg m
        UnusableParseFailure (ParseFailure m) ->
          matchMsg m
        UnusableConflict x ->
          or [p (getDeclId x) l C.Available | l <- getLocs x ]
        UnusableFailedMacro (FailedMacro m) ->
          matchMsg m
        -- TODO_PR: We should match omitted declarations.
        UnusableOmitted{} ->
          False

    matchMsg :: AttachedParseMsg a -> Bool
    matchMsg m = p m.declId m.loc m.availability

-- | Restrict the declaration index to unusable declarations in a given set.
getUnusables :: DeclIndex -> Set C.QualPrelimDeclId -> Map C.QualPrelimDeclId Unusable
getUnusables (DeclIndex i) xs = Map.mapMaybe retainUnusable $ Map.restrictKeys i xs
  where
    retainUnusable :: Entry -> Maybe Unusable
    retainUnusable = \case
      UsableE   _ -> Nothing
      UnusableE x -> Just x

{-------------------------------------------------------------------------------
  Supprot for binding specifications
-------------------------------------------------------------------------------}

registerOmittedDeclarations ::
  Map C.QualPrelimDeclId (C.QualName, SourcePath) -> DeclIndex  -> DeclIndex
registerOmittedDeclarations xs =
      DeclIndex . Map.union (UnusableE . UnusableOmitted <$> xs) . unDeclIndex

registerExternalDeclarations :: Set C.QualPrelimDeclId -> DeclIndex  -> DeclIndex
registerExternalDeclarations xs index = Foldable.foldl' insert index xs
  where
    insert :: DeclIndex -> C.QualPrelimDeclId -> DeclIndex
    insert (DeclIndex i) x =
      DeclIndex $ Map.insert x (UsableE UsableExternal) i
