{-# LANGUAGE OverloadedLabels #-}

-- | Monad for parsing declarations
--
-- Intended for unqualified import (unless context is unambiguous).
--
-- > import HsBindgen.Frontend.Pass.Parse.Decl.Monad (ParseDecl)
-- > import HsBindgen.Frontend.Pass.Parse.Decl.Monad qualified as ParseDecl
module HsBindgen.Frontend.Pass.Parse.Decl.Monad (
    -- * Definition
    ParseDecl
  , Env (..)
  , run
    -- * Functionality
    -- ** "Reader"
  , getTranslationUnit
  , evalGetMainHeadersAndInclude
  , evalPredicate
    -- ** "State"
  , recordMacroExpansionAt
  , checkHasMacroExpansion
    -- ** Logging
  , recordImmediateTrace
    -- ** Errors
  , unknownCursorKind
    -- * Utility: dispatching
  , dispatch
  , dispatchWithArg
  ) where

import Data.IORef
import Data.Set qualified as Set
import Data.Text qualified as Text

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Eff
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.ProcessIncludes (GetMainHeadersAndInclude)
import HsBindgen.Frontend.RootHeader (HashIncludeArg, RootHeader)
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition

  We are careful to distinguish between the state that the computation can
  depend on ('MacroExpansions') and the additional output that generate during
  parsing but that cannot otherwise affect the computation ('IncludeGraph').
-------------------------------------------------------------------------------}

-- | Monad used during folding
type ParseDecl = Eff ParseDeclMonad

data ParseDeclMonad a

-- | Support for 'ParseDecl' (internal type, not exported)
data ParseSupport = ParseSupport {
      parseEnv   :: Env              -- ^ Reader
    , parseState :: IORef ParseState -- ^ State
    }

type instance Support ParseDeclMonad = ParseSupport

run :: Env -> ParseDecl a -> IO a
run env f = do
    support <- ParseSupport env <$> newIORef initParseState
    unwrapEff f support

{-------------------------------------------------------------------------------
  "Reader"
-------------------------------------------------------------------------------}

data Env = Env {
      envUnit                     :: CXTranslationUnit
    , envRootHeader               :: RootHeader
    , envIsMainHeader             :: IsMainHeader
    , envIsInMainHeaderDir        :: IsInMainHeaderDir
    , envGetMainHeadersAndInclude :: GetMainHeadersAndInclude
    , envPredicate                :: Boolean ParsePredicate
    , envTracer                   :: Tracer (Msg Parse)
    }

getTranslationUnit :: ParseDecl CXTranslationUnit
getTranslationUnit = wrapEff $ \ParseSupport{parseEnv} ->
    return (envUnit parseEnv)

evalGetMainHeadersAndInclude ::
     SourcePath
  -> ParseDecl (NonEmpty HashIncludeArg, HashIncludeArg)
evalGetMainHeadersAndInclude path = wrapEff $ \ParseSupport{parseEnv} ->
    either panicIO return $ (envGetMainHeadersAndInclude parseEnv) path

evalPredicate :: C.DeclInfo Parse -> ParseDecl Bool
evalPredicate info = wrapEff $ \ParseSupport{parseEnv} -> pure $
    matchParse
      (envIsMainHeader parseEnv)
      (envIsInMainHeaderDir parseEnv)
      (singleLocPath (C.declLoc info))
      (envPredicate parseEnv)

{-------------------------------------------------------------------------------
  "State"
-------------------------------------------------------------------------------}

data ParseState = ParseState {
      -- | Where did clang expand macros?
      --
      -- Declarations with expanded macros need to be reparsed.
      stateMacroExpansions        :: Set SingleLoc
    }
  deriving (Generic)

initParseState :: ParseState
initParseState = ParseState{
      stateMacroExpansions        = Set.empty
    }

recordMacroExpansionAt :: SingleLoc -> ParseDecl ()
recordMacroExpansionAt loc = do
    wrapEff $ \ParseSupport{parseState} ->
      modifyIORef parseState $ \st -> st{
          stateMacroExpansions = Set.insert loc (stateMacroExpansions st)
        }

checkHasMacroExpansion :: Range SingleLoc -> ParseDecl Bool
checkHasMacroExpansion extent = do
    wrapEff $ \ParseSupport{parseState} ->
      aux extent . stateMacroExpansions <$> readIORef parseState
  where
    aux :: Range SingleLoc -> Set SingleLoc -> Bool
    aux range expansions = or [
        -- Do a quick O(log n) check first, for the common case that the macro
        -- is right at the start of the range. For example, this would capture
        -- cases such as
        --
        -- > #define T int
        -- >
        -- > struct ExampleStruct {
        -- >   T field;
        -- > };
        Set.member (rangeStart range) expansions

        -- If that fails, do a O(n) scan through all macro expansions
      , any (\e -> fromMaybe False (rangeContainsLoc range e)) expansions
      ]

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

-- | Directly emit a parse message that can not be attached to a declaration,
-- usually because not enough information about the declaration is available.
recordImmediateTrace :: C.DeclInfo Parse -> ImmediateParseMsg -> ParseDecl ()
recordImmediateTrace declInfo msg = wrapEff $ \ParseSupport{parseEnv} ->
    traceWith (envTracer parseEnv) WithLocationInfo{
        loc = prelimDeclIdLocationInfo declInfo.declId [declInfo.declLoc]
      , msg
      }

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

unknownCursorKind :: MonadIO m => CXCursorKind -> CXCursor -> m x
unknownCursorKind kind curr = do
    loc      <- HighLevel.clang_getCursorLocation' curr
    spelling <- clang_getCursorKindSpelling (simpleEnum kind)
    panicIO $ concat [
        "Unknown cursor of kind "
      , show kind
      , " ("
      , Text.unpack spelling
      , ") at "
      , show loc
      ]

{-------------------------------------------------------------------------------
  Utility: dispatching based on the cursor kind
-------------------------------------------------------------------------------}

dispatch :: MonadIO m => CXCursor -> (CXCursorKind -> m a) -> m a
dispatch curr k = do
    mKind <- fromSimpleEnum <$> clang_getCursorKind curr
    case mKind of
      Right kind -> k kind
      Left  i    -> panicIO $ "Unrecognized CXCursorKind " ++ show i

dispatchWithArg ::
     MonadIO m
  => CXCursor
  -> (CXCursorKind -> CXCursor -> m a)
  -> m a
dispatchWithArg x f = dispatch x $ \kind -> f kind x
