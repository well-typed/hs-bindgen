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
  , keysSet
    -- * Support for selection
  , Match
  , selectDeclIndex
  ) where

import Prelude hiding (lookup)

import Control.Monad.State
import Data.Function
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Optics.Core (over, set, (%))
import Text.SimplePrettyPrint (hcat, showToCtxDoc)

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
      -- We assert that no key is used twice. This assertion is not strictly
      -- necessary, and we may want to remove it in the future.
      let ss = Map.keysSet i.succeeded
          os = Map.keysSet i.notAttempted
          fs = Map.keysSet i.failed
          is = Set.intersection
          sharedKeys = Set.unions [is ss os, is ss fs, is os fs]
      in  if sharedKeys == Set.empty then
            (i, Map.elems e)
          else
            panicPure $
              "DeclIndex.fromParseResults: shared keys: " <> show sharedKeys

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
              ( alter qualPrelimDeclId x )
              oldIndex
          ParseResultFailure x ->
            over
              ( #index % #failed )
              ( alter qualPrelimDeclId x )
              oldIndex
      where
        qualPrelimDeclId :: C.QualPrelimDeclId
        qualPrelimDeclId = getQualPrelimDeclId parse

    alter :: (Ord k, Show a, Eq a) => k -> a -> Map k a -> Map k a
    alter key x =
      Map.alter ( \case
        Nothing -> Just x
        Just x'
          -- TODO https://github.com/well-typed/hs-bindgen/issues/1283: We parse
          -- declarations multiple times.
          | x == x'   -> Just x
          -- TODO_PR: We also encounter conflicting values. In particular for
          -- the `hsb_complex_test`, `cimag` is defined twice (function and
          -- macro).
          -- | otherwise -> panicPure $ "conflicting values: " <> show (x, x')
          | otherwise -> traceShow ("conflicting values: " <> show (x,x')) $ Just x
        ) key

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
  deriving stock (Show, Eq)

instance PrettyForTrace DeclIndexError where
  prettyForTrace Redeclaration{..} = hcat [
        prettyForTrace redeclarationId
      , " declared at "
      , showToCtxDoc redeclarationOld
      , " was redeclared at "
      , showToCtxDoc redeclarationNew
      , ". No binding generated."
      ]

instance IsTrace Level DeclIndexError where
  getDefaultLogLevel = \case
      -- Redeclarations can only happen for macros, so we issue a warning,
      -- rather than an error.
      Redeclaration{} -> Warning
  getSource  = const HsBindgen
  getTraceId = const "decl-index-error"

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

keysSet :: DeclIndex -> Set C.QualPrelimDeclId
keysSet DeclIndex{..} = Set.unions [
      Map.keysSet succeeded
    , Map.keysSet notAttempted
    , Map.keysSet failed
    , Map.keysSet failedMacros
    -- TODO https://github.com/well-typed/hs-bindgen/issues/799: Also add
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
