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
  ) where

import Prelude hiding (lookup)

import Control.Monad.State
import Data.Function
import Data.List.NonEmpty ((<|))
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
data DeclIndex = DeclIndex {
      succeeded    :: !(Map C.QualPrelimDeclId ParseSuccess)
    , notAttempted :: !(Map C.QualPrelimDeclId (NonEmpty ParseNotAttempted))
    , failed       :: !(Map C.QualPrelimDeclId (NonEmpty ParseFailure))
    , failedMacros :: !(Map C.QualPrelimDeclId HandleMacrosFailure)
    , omitted      :: !(Map C.QualName SourcePath)
      -- TODO https://github.com/well-typed/hs-bindgen/issues/1273: Attach
      -- information required to match the select predicate also to external
      -- declarations.
    , external     :: !(Set C.QualName)
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

fromParseResults :: [ParseResult] -> (DeclIndex, [DeclIndexError])
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

    alter :: Ord k => k -> a -> Map k (NonEmpty a) -> Map k (NonEmpty a)
    alter key x =
      Map.alter (\case
        Nothing -> Just $ x :| []
        Just xs -> Just $ x <| xs) key

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
