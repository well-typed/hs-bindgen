{-# LANGUAGE OverloadedLabels #-}

-- | Declaration index
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
-- > import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
module HsBindgen.Frontend.Analysis.DeclIndex (
    DeclIndex -- opaque
  , getNotAttempted
  , getFailed
    -- * Construction
  , DeclIndexError(..)
  , fromParseResults
    -- * Query
  , lookup
  , (!)
  , lookupDelayedParseMsgs
  , lookupNotAttempted
  , getDecls
  ) where

import Prelude hiding (lookup)

import Control.Monad.State
import Data.Function
import Data.Map.Strict qualified as Map
import Optics.Core (_1, over, set, view, (%))
import Text.SimplePrettyPrint (hcat, showToCtxDoc)

import Clang.HighLevel.Types

import HsBindgen.Errors
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Index of all declarations
data DeclIndex = DeclIndex {
      succeeded ::
        !(Map C.QualPrelimDeclId (C.Decl Parse, [DelayedParseMsg]))
    , notAttempted ::
        !(Map C.QualPrelimDeclId (SingleLoc, C.Availability, ParseOmissionReason))
    , failed  ::
        !(Map C.QualPrelimDeclId (SingleLoc, C.Availability, NonEmpty DelayedParseMsg))
    }
  deriving stock (Show, Generic)

emptyIndex :: DeclIndex
emptyIndex = DeclIndex Map.empty Map.empty Map.empty

getNotAttempted :: DeclIndex -> Map C.QualPrelimDeclId (SingleLoc, C.Availability, ParseOmissionReason)
getNotAttempted = notAttempted

getFailed :: DeclIndex -> Map C.QualPrelimDeclId (SingleLoc, C.Availability, NonEmpty DelayedParseMsg)
getFailed = failed

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
    fromPartialIndex p = (p.index , Map.elems p.errors
      )

    aux :: ParseResult -> State PartialIndex ()
    aux parse = modify' $ \oldIndex@PartialIndex{..} ->
        if Map.member qualPrelimDeclId errors then
          -- Ignore further definitions of the same ID after an error
          oldIndex
        else case parse of
          ParseSucceeded{..} ->
              let (succeeded', mErr) = flip runState Nothing $
                     Map.alterF
                       (insert psDecl psDelayedMsgs)
                       qualPrelimDeclId
                       index.succeeded
              in PartialIndex{
                  index  = set #succeeded succeeded' index
                , errors = case mErr of
                    Nothing  -> errors
                    Just err -> Map.insert qualPrelimDeclId err errors
                }
          ParseNotAttempted{..} ->
            let val = (pnaSingleLoc, pnaAvailability, pnaParseOmissionReason)
            in  over
                  ( #index % #notAttempted )
                  ( Map.insert qualPrelimDeclId val )
                  oldIndex
          ParseFailed{..} ->
            let val = (pfSingleLoc, pfAvailability, pfDelayedParseMsgs)
            in  over
                  ( #index % #failed )
                  ( Map.insert qualPrelimDeclId val )
                  oldIndex
      where
        qualPrelimDeclId :: C.QualPrelimDeclId
        qualPrelimDeclId = getQualPrelimDeclId parse

    insert ::
         C.Decl Parse
      -> [DelayedParseMsg]
      -> Maybe (C.Decl Parse, [DelayedParseMsg])
      -> State (Maybe DeclIndexError) (Maybe (C.Decl Parse, [DelayedParseMsg]))
    insert new newMsgs mOld = state $ \_ ->
        case mOld of
          Nothing ->
              -- The normal case: no previous declaration exists
              success (new, newMsgs)

          Just (old, oldMsgs)
            | sameDefinition (C.declKind new) (C.declKind old) ->
                -- Redeclaration but with the same definition. This can happen,
                -- for example for opaque structs. We stick with the first but
                -- add the parse messages of the second.
                success (old, oldMsgs ++ newMsgs)

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
                    redeclarationId  = C.declQualPrelimDeclId new
                  , redeclarationOld = C.declLoc $ C.declInfo old
                  , redeclarationNew = C.declLoc $ C.declInfo new
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
  fmap fst . Map.lookup qualPrelimDeclId . succeeded


(!) :: HasCallStack => DeclIndex -> C.QualPrelimDeclId -> C.Decl Parse
(!) declIndex qualPrelimDeclId =
    fromMaybe (panicPure $ "Unknown key: " ++ show qualPrelimDeclId) $
       lookup qualPrelimDeclId declIndex

lookupDelayedParseMsgs :: C.QualPrelimDeclId -> DeclIndex -> [DelayedParseMsg]
lookupDelayedParseMsgs qualPrelimDeclId =
  maybe [] snd . Map.lookup qualPrelimDeclId . succeeded

lookupNotAttempted :: C.QualPrelimDeclId -> DeclIndex -> Maybe SingleLoc
lookupNotAttempted qualPrelimDeclId =
  fmap (view _1) . Map.lookup qualPrelimDeclId . notAttempted

getDecls :: DeclIndex -> [C.Decl Parse]
getDecls = map fst . Map.elems . succeeded
