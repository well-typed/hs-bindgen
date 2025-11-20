{-# LANGUAGE OverloadedLabels #-}

-- | Declaration index
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
-- > import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
module HsBindgen.Frontend.Analysis.DeclIndex (
    DeclIndex(..)
    -- * Construction
  , DeclIndexError(..)
  , fromParseResults
    -- * Query
  , lookup
  , (!)
  , lookupAttachedParseMsgs
  , getDecls
  , getLoc
  , keysSet
    -- * Support for selection
  , Match
  , selectDeclIndex
  ) where

import Prelude hiding (lookup)

import Control.Applicative (asum)
import Control.Monad.State
import Data.Function
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Optics.Core (over, set, (%))
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types
import Clang.Paths (SourcePath)

import HsBindgen.Errors
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.HandleMacros.Error
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
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
-- as 'external'. In the "HsBindgen.Frontend.Analysis.UseDeclGraph", dependency
-- edges from use sites to the replaced declaration are deleted, because the use
-- sites now depend on the external Haskell type.
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
--
-- The records
-- - 'succeeded', 'notAttempted', and 'failed' store parse results;
-- - 'failedMacros' stores failed macros; and
-- - 'omitted' and 'external' store binding specifications.
data DeclIndex = DeclIndex {
      succeeded    :: !(Map C.QualPrelimDeclId ParseSuccess)
    , notAttempted :: !(Map C.QualPrelimDeclId ParseNotAttempted)
    , failed       :: !(Map C.QualPrelimDeclId ParseFailure)
    , failedMacros :: !(Map C.QualPrelimDeclId HandleMacrosParseMsg)
    , omitted      :: !(Map C.QualPrelimDeclId (C.QualName, SourcePath))
      -- TODO https://github.com/well-typed/hs-bindgen/issues/1273: Attach
      -- information required to match the select predicate also to external
      -- declarations.
    , external     :: !(Set C.QualPrelimDeclId)
    }
  deriving stock (Show, Generic)

emptyIndex :: DeclIndex
emptyIndex =
   DeclIndex Map.empty Map.empty Map.empty Map.empty Map.empty Set.empty

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Construction state (internal type)
data PartialIndex = PartialIndex{
      index  :: DeclIndex
    , errors :: !(Map C.QualPrelimDeclId DeclIndexError)
    }
  deriving (Generic)

fromParseResults :: HasCallStack => [ParseResult] -> (DeclIndex, [DeclIndexError])
fromParseResults results =
      fromPartialIndex
    . flip execState (PartialIndex emptyIndex Map.empty)
    $ mapM_ aux results
  where
    fromPartialIndex :: PartialIndex -> (DeclIndex, [DeclIndexError])
    fromPartialIndex (PartialIndex i e) =
      -- We assert that no key is used twice. Duplicate keys may arise, for
      -- example, if a declaration is redefined by a macro:
      --
      -- @
      -- extern FILE *const stderr;
      -- #define stderr (stderr)
      -- @
      --
      -- We can detect and handle some but not all of these duplicates; for now,
      -- we remove them all.
      let succeededIds    = Map.keysSet i.succeeded
          notAttemptedIds = Map.keysSet i.notAttempted
          failedIds       = Map.keysSet i.failed

          cap = Set.intersection

          dups :: Set C.QualPrelimDeclId
          dups = Set.unions [
              succeededIds    `cap` notAttemptedIds
            , succeededIds    `cap` failedIds
            , notAttemptedIds `cap` failedIds
            ]

          succeededDups    = i.succeeded    `Map.restrictKeys` dups
          notAttemptedDups = i.notAttempted `Map.restrictKeys` dups
          failedDups       = i.failed       `Map.restrictKeys` dups

          succeededMsgs, notAttemptedMsgs, failedMsgs :: [DeclIndexError]
          succeededMsgs =
            map (SharedKey . ParseResultSuccess)      $ Map.elems succeededDups
          notAttemptedMsgs =
            map (SharedKey . ParseResultNotAttempted) $ Map.elems notAttemptedDups
          failedMsgs =
            map (SharedKey . ParseResultFailure)      $ Map.elems failedDups

          succeededNoDups    = i.succeeded    `Map.withoutKeys` dups
          notAttemptedNoDups = i.notAttempted `Map.withoutKeys` dups
          failedNoDups       = i.failed       `Map.withoutKeys` dups

          iNoDups = i {
              succeeded    = succeededNoDups
            , notAttempted = notAttemptedNoDups
            , failed       = failedNoDups
            }
      in ( iNoDups
         , Map.elems e ++ succeededMsgs ++ notAttemptedMsgs ++ failedMsgs )

    aux :: ParseResult -> State PartialIndex ()
    aux parse = modify' $ \oldIndex@PartialIndex{..} ->
        if Map.member qualPrelimDeclId errors then
          -- Ignore further definitions of the same ID after an error
          oldIndex
        else case parse of
          ParseResultSuccess x ->
              let (succeeded', mErr) = flip runState Nothing $
                     Map.alterF
                       (insert x)
                       qualPrelimDeclId
                       index.succeeded
              in PartialIndex{
                  index  = set #succeeded succeeded' index
                , errors = case mErr of
                    Nothing  -> errors
                    Just err -> Map.insert qualPrelimDeclId err errors
                }
          ParseResultNotAttempted x ->
            over
              ( #index % #notAttempted )
              ( insertFailure qualPrelimDeclId x )
              oldIndex
          ParseResultFailure x ->
            over
              ( #index % #failed )
              ( insertFailure qualPrelimDeclId x )
              oldIndex
      where
        qualPrelimDeclId :: C.QualPrelimDeclId
        qualPrelimDeclId = getQualPrelimDeclId parse

    insert ::
         ParseSuccess
      -> Maybe ParseSuccess
      -> State (Maybe DeclIndexError) (Maybe ParseSuccess)
    insert new mOld = state $ \_ ->
        case mOld of
          Nothing ->
              -- The normal case: no previous declaration exists
              success new

          Just old
            | sameDefinition new.psDecl.declKind old.psDecl.declKind ->
                -- Redeclaration but with the same definition. This can happen,
                -- for example for opaque structs. We stick with the first but
                -- add the parse messages of the second.
                success $ over #psAttachedMsgs (++ new.psAttachedMsgs) old

            | otherwise ->
                -- Redeclaration with a /different/ value. This is only legal
                -- for macros; for other kinds of declarations, clang will have
                -- reported an error already.
                --
                -- TODO: there are cases where one declaration is an actual C
                -- construct like a variable declaration, but the new
                -- declaration is a macro of the same name that simply defers to
                -- the C construct. This is apparently a valid pattern, which
                -- for example occurs in @stdio.h@:
                --
                -- > typedef int FILE;
                -- > extern FILE *const stdin;
                -- > #define stdin  (stdin)
                --
                -- See issue #1155.
                failure $ Redeclaration{
                    redeclarationId  = C.declQualPrelimDeclId $ new.psDecl
                  , redeclarationOld = old.psDecl.declInfo.declLoc
                  , redeclarationNew = new.psDecl.declInfo.declLoc
                  }
     where
       -- No errors; set (or replace) value in the map
       success :: a -> (Maybe a, Maybe DeclIndexError)
       success x = (Just x, Nothing)

       -- In case of an error, /remove/ the value from the map
       failure :: e -> (Maybe a, Maybe e)
       failure err = (Nothing, Just err)

    -- For failures, we just stick with the first failure.
    insertFailure :: Ord k => k -> a -> Map k a -> Map k a
    insertFailure key x =
      Map.alter ( \case
        Nothing -> Just x
        Just x' -> Just x'
        ) key

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
  Construction errors
-------------------------------------------------------------------------------}

data DeclIndexError =
    Redeclaration {
        redeclarationId  :: C.QualPrelimDeclId
      , redeclarationOld :: SingleLoc
      , redeclarationNew :: SingleLoc
      }
  | SharedKey {
        sharedKeyParseResult :: ParseResult
      }
  deriving stock (Show)

instance PrettyForTrace DeclIndexError where
  prettyForTrace = \case
    Redeclaration{..} -> PP.hcat [
        prettyForTrace (C.Located redeclarationOld redeclarationId)
      , " was redeclared at "
      , PP.showToCtxDoc redeclarationNew
      , ". No binding generated."
      ]
    SharedKey{..} ->
      PP.hang "Duplicate in declaration index detected:" 2 $
        prettyForTrace sharedKeyParseResult

instance IsTrace Level DeclIndexError where
  getDefaultLogLevel = \case
      Redeclaration{} -> Warning
      SharedKey{}     -> Warning
  getSource  = const HsBindgen
  getTraceId = const "decl-index"

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

lookup :: C.QualPrelimDeclId -> DeclIndex -> Maybe (C.Decl Parse)
lookup qualPrelimDeclId =
  fmap psDecl . Map.lookup qualPrelimDeclId . succeeded

(!) :: HasCallStack => DeclIndex -> C.QualPrelimDeclId -> C.Decl Parse
(!) declIndex qualPrelimDeclId =
    fromMaybe (panicPure $ "Unknown key: " ++ show qualPrelimDeclId) $
       lookup qualPrelimDeclId declIndex

lookupAttachedParseMsgs :: C.QualPrelimDeclId -> DeclIndex -> [AttachedParseMsg DelayedParseMsg]
lookupAttachedParseMsgs qualPrelimDeclId =
  maybe [] psAttachedMsgs . Map.lookup qualPrelimDeclId . succeeded

getDecls :: DeclIndex -> [C.Decl Parse]
getDecls = map psDecl . Map.elems . succeeded

-- Get the source location of a declaration (if available)
getLoc :: C.QualPrelimDeclId -> DeclIndex -> Maybe SingleLoc
-- Use a classical pattern match here in case more records are added to
-- 'DeclIndex'.
--
-- 'asum' is safe because list is non-empty.
getLoc x (DeclIndex ss na fa fm _om _ex) = asum [
      (C.declLoc . C.declInfo. psDecl) <$> Map.lookup x ss
    , (loc . unParseNotAttempted)      <$> Map.lookup x na
    , (loc . unParseFailure)           <$> Map.lookup x fa
    , (loc . unHandleMacrosParseMsg)   <$> Map.lookup x fm
      -- TODO: We do not have a location for `omitted` nor `external`.
    ]

keysSet :: DeclIndex -> Set C.QualPrelimDeclId
keysSet DeclIndex{..} = Set.unions [
      Map.keysSet succeeded
    , Map.keysSet notAttempted
    , Map.keysSet failed
    , Map.keysSet failedMacros
    -- TODO https://github.com/well-typed/hs-bindgen/issues/1301: Also add
    -- 'omitted' here and deal with the 'omitted' case in selection.
    ]

{-------------------------------------------------------------------------------
  Support for selection
-------------------------------------------------------------------------------}

-- Match function to find selection roots.
type Match = C.QualPrelimDeclId -> SingleLoc -> C.Availability -> Bool

selectDeclIndex :: Match -> DeclIndex -> DeclIndex
selectDeclIndex p DeclIndex{..} = DeclIndex {
      succeeded    = Map.filter matchSuccess      succeeded
    , notAttempted = Map.filter matchNotAttempted notAttempted
    , failed       = Map.filter matchFailed       failed
    , failedMacros = Map.filter matchFailedMacros failedMacros
    , omitted
    , external
    }
  where
    matchMsg :: AttachedParseMsg a -> Bool
    matchMsg m = p m.declId m.loc m.availability

    matchSuccess :: ParseSuccess -> Bool
    matchSuccess (ParseSuccess i d _) =
      p i d.declInfo.declLoc d.declInfo.declAvailability

    matchNotAttempted :: ParseNotAttempted -> Bool
    matchNotAttempted (ParseNotAttempted m) = matchMsg m

    matchFailed :: ParseFailure -> Bool
    matchFailed (ParseFailure m) = matchMsg m

    matchFailedMacros :: HandleMacrosParseMsg -> Bool
    matchFailedMacros (HandleMacrosParseMsg m) = matchMsg m
