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
  , DeclIndex -- opaque
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
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map

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
      UnusableParseNotAttempted (NonEmpty ParseNotAttempted)
    | UnusableParseFailure      ParseFailure
    | UnusableConflict          ConflictingDeclarations
    | UnusableFailedMacro       FailedMacro
      -- TODO https://github.com/well-typed/hs-bindgen/issues/1273: Attach
      -- information required to match the select predicate also to omitted
      -- declarations.
      -- | Omitted by prescriptive binding specifications
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
      unDeclIndex :: Map C.PrelimDeclId Entry
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
        DeclIndex . Map.alter (Just . handleParseResult declId new) declId . unDeclIndex
      where
        declId :: C.PrelimDeclId
        declId = getParseResultDeclId new

    handleParseResult ::
      C.PrelimDeclId -> ParseResult -> Maybe Entry -> Entry
    handleParseResult declId new = \case
      Nothing -> parseResultToEntry new
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
      Just old -> case old of
        UsableE oldUsable -> case oldUsable of
          UsableSuccess oldParseSuccess -> case new of
            ParseResultSuccess newParseSuccess
              -- Redeclaration but with the same definition. This can happen, for
              -- example for opaque structs. We stick with the first declaration.
              | sameDefinition oldParseSuccess.psDecl.declKind newParseSuccess.psDecl.declKind ->
                old
              | otherwise ->
                newConflict oldParseSuccess.psDecl.declInfo.declLoc
            ParseResultNotAttempted _ -> old
            ParseResultFailure _      -> parseResultToEntry new
          UsableExternal ->
            panicPure "handleParseResult: usable external"
        UnusableE oldUnusable -> case oldUnusable of
          (UnusableParseNotAttempted nasOld)
            | ParseResultNotAttempted naNew <- new ->
                UnusableE $ UnusableParseNotAttempted $ naNew <| nasOld
            | otherwise ->
                parseResultToEntry new
          UnusableParseFailure _ -> old
          UnusableConflict c     -> addConflicts c
          UnusableFailedMacro x  ->
            panicPure $ "handleParseResult: unusable failed macro" <> show x
          UnusableOmitted     x  ->
            panicPure $ "handelParseResult: unusable omitted" <> show x
      where
        newLoc :: SingleLoc
        newLoc = getParseResultLoc new

        addConflicts :: ConflictingDeclarations  -> Entry
        addConflicts c =
          UnusableE $ UnusableConflict $
            addConflictingLoc c newLoc

        newConflict :: SingleLoc -> Entry
        newConflict oldLoc =
          UnusableE $ UnusableConflict $
            conflictingDeclarations declId oldLoc newLoc

        parseResultToEntry :: ParseResult -> Entry
        parseResultToEntry = \case
          ParseResultSuccess      r -> UsableE   $ UsableSuccess               r
          ParseResultNotAttempted r -> UnusableE $ UnusableParseNotAttempted $ r :| []
          ParseResultFailure      r -> UnusableE $ UnusableParseFailure        r

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
lookup :: C.PrelimDeclId -> DeclIndex -> Maybe (C.Decl Parse)
lookup qualPrelimDeclId (DeclIndex i) = case Map.lookup qualPrelimDeclId i of
  Nothing                          -> Nothing
  Just (UsableE (UsableSuccess x)) -> Just $ x.psDecl
  _                                -> Nothing

-- | Unsafe! Get parse success.
(!) :: HasCallStack => DeclIndex -> C.PrelimDeclId -> C.Decl Parse
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
lookupEntry :: C.PrelimDeclId -> DeclIndex -> Maybe Entry
lookupEntry x = Map.lookup x . unDeclIndex

-- | Get all entries of a declaration index.
toList :: DeclIndex -> [(C.PrelimDeclId, Entry)]
toList = Map.toList . unDeclIndex

-- | Get the source locations of a declaration.
lookupLoc :: C.PrelimDeclId -> DeclIndex -> [SingleLoc]
lookupLoc d (DeclIndex i) = case Map.lookup d i of
  Nothing            -> []
  Just (UsableE e)   -> case e of
    UsableSuccess x -> [x.psDecl.declInfo.declLoc]
    UsableExternal  -> []
  Just (UnusableE e) -> unusableToLoc e

-- | Get the source locations of an unusable declaration.
lookupUnusableLoc :: C.PrelimDeclId -> DeclIndex -> [SingleLoc]
lookupUnusableLoc d (DeclIndex i) = case Map.lookup d i of
  Nothing            -> []
  Just (UsableE _)   -> []
  Just (UnusableE e) -> unusableToLoc e

unusableToLoc :: Unusable -> [SingleLoc]
unusableToLoc = \case
    UnusableParseNotAttempted xs ->
      [x.loc | (ParseNotAttempted x) <- NonEmpty.toList xs]
    UnusableParseFailure (ParseFailure x) -> [x.loc]
    UnusableConflict x                    -> getLocs x
    UnusableFailedMacro (FailedMacro x)   -> [x.loc]
    UnusableOmitted{}                     -> []

-- | Get the identifiers of all declarations in the index.
keysSet :: DeclIndex -> Set C.PrelimDeclId
keysSet = Map.keysSet . unDeclIndex

-- | Get omitted entries.
getOmitted :: DeclIndex -> Map C.PrelimDeclId (C.QualName, SourcePath)
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
type Match = C.PrelimDeclId -> SingleLoc -> C.Availability -> Bool

-- | Limit the declaration index to those entries that match the select
--   predicate. Do not include anything external nor omitted.
selectDeclIndex :: Match -> DeclIndex -> DeclIndex
selectDeclIndex p = DeclIndex . Map.filter matchEntry . unDeclIndex
  where
    matchEntry :: Entry -> Bool
    matchEntry = \case
      UsableE e -> case e of
        UsableSuccess (ParseSuccess i d _) ->
          p i d.declInfo.declLoc d.declInfo.declAvailability
        UsableExternal ->
          False
      UnusableE e -> case e of
        UnusableParseNotAttempted xs ->
          any (matchMsg . unParseNotAttempted) xs
        UnusableParseFailure (ParseFailure m) ->
          matchMsg m
        UnusableConflict x ->
          or [p (getDeclId x) l C.Available | l <- getLocs x ]
        UnusableFailedMacro (FailedMacro m) ->
          matchMsg m
        UnusableOmitted{} ->
          False

    matchMsg :: AttachedParseMsg a -> Bool
    matchMsg m = p m.declId m.loc m.availability

-- | Restrict the declaration index to unusable declarations in a given set.
getUnusables :: DeclIndex -> Set C.PrelimDeclId -> Map C.PrelimDeclId Unusable
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
  Map C.PrelimDeclId (C.QualName, SourcePath) -> DeclIndex  -> DeclIndex
registerOmittedDeclarations xs =
      DeclIndex . Map.union (UnusableE . UnusableOmitted <$> xs) . unDeclIndex

registerExternalDeclarations :: Set C.PrelimDeclId -> DeclIndex  -> DeclIndex
registerExternalDeclarations xs index = Foldable.foldl' insert index xs
  where
    insert :: DeclIndex -> C.PrelimDeclId -> DeclIndex
    insert (DeclIndex i) x =
      DeclIndex $ Map.insert x (UsableE UsableExternal) i
