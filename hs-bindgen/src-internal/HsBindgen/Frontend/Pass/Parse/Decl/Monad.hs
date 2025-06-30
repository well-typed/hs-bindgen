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
  , evalGetMainHeader
  , evalPredicate
    -- ** "State"
  , recordMacroExpansionAt
  , checkHasMacroExpansion
  , recordNonSelectedDecl
    -- ** Logging
  , recordTrace
    -- ** Errors
  , unknownCursorKind
    -- * Utility: dispatching
  , dispatch
  , dispatchFold
  ) where

import Data.IORef
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Stack

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import HsBindgen.C.Predicate (IsMainFile, Predicate)
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Eff
import HsBindgen.Errors
import HsBindgen.Frontend.NonSelectedDecls (NonSelectedDecls)
import HsBindgen.Frontend.NonSelectedDecls qualified as NonSelectedDecls
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Type.DeclId
import HsBindgen.Frontend.ProcessIncludes (GetMainHeader)
import HsBindgen.Frontend.RootHeader (RootHeader)
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.C.Name (QualName)
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

run :: Env -> ParseDecl a -> IO (NonSelectedDecls, a)
run env f = do
    support <- ParseSupport env <$> newIORef initParseState
    x <- unwrapEff f support
    (, x) . stateNonSelectedDecls <$> readIORef (parseState support)

{-------------------------------------------------------------------------------
  "Reader"
-------------------------------------------------------------------------------}

data Env = Env {
      envUnit          :: CXTranslationUnit
    , envRootHeader    :: RootHeader
    , envIsMainFile    :: IsMainFile
    , envGetMainHeader :: GetMainHeader
    , envPredicate     :: Predicate
    , envTracer        :: Tracer IO ParseMsg
    }

getTranslationUnit :: ParseDecl CXTranslationUnit
getTranslationUnit = wrapEff $ \ParseSupport{parseEnv} ->
    return (envUnit parseEnv)

evalGetMainHeader :: SourcePath -> ParseDecl CHeaderIncludePath
evalGetMainHeader path = wrapEff $ \ParseSupport{parseEnv} ->
    return $ (envGetMainHeader parseEnv) path

evalPredicate :: SingleLoc -> Maybe QualName -> ParseDecl (Either Predicate.SkipReason ())
evalPredicate loc mQualName = wrapEff $ \ParseSupport{parseEnv} -> do
    let matchResult =
          Predicate.match (envIsMainFile parseEnv) (envPredicate parseEnv) loc mQualName
    case matchResult of
      Right ()    -> return ()
      Left reason -> traceWith (envTracer parseEnv) (Skipped reason)
    return matchResult

{-------------------------------------------------------------------------------
  "State"
-------------------------------------------------------------------------------}

data ParseState = ParseState {
      -- | Where did clang expand macros?
      --
      -- Declarations with expanded macros need to be reparsed.
      stateMacroExpansions :: Set SingleLoc

      -- | Non-selected declarations
      --
      -- We need to track which header each omitted declaration is declared in
      -- so that we can resolve external bindings.
    , stateNonSelectedDecls :: NonSelectedDecls
    }

initParseState :: ParseState
initParseState = ParseState{
      stateMacroExpansions  = Set.empty
    , stateNonSelectedDecls = NonSelectedDecls.empty
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

recordNonSelectedDecl :: CXCursor -> ParseDecl ()
recordNonSelectedDecl curr = do
    mNameKind <- dispatch curr $ return . C.toNameKindFromCXCursorKind
    case mNameKind of
      Just nameKind -> getDeclId curr >>= \case
        DeclNamed cname -> do
          let cQualName = C.QualName cname nameKind
          sourcePath <-
            singleLocPath <$> HighLevel.clang_getCursorLocation' curr
          wrapEff $ \ParseSupport{parseState} ->
            modifyIORef parseState $ \st -> st{
                stateNonSelectedDecls =
                  NonSelectedDecls.insert cQualName sourcePath $
                    stateNonSelectedDecls st
              }
        -- We __do not track unselected anonymous declarations__. If we want to
        -- use descriptive binding specification with anonymous declarations, we
        -- __must__ select these declarations.
        DeclAnon{} -> return ()
      -- We intentionally do selection as part of parsing, rather than a
      -- separate step: if the user does not select certain declarations
      -- (perhaps because they live deep in the bowels of some system
      -- libraries), we also do not need to parse them. Since 'recordSource' is
      -- called on all declarations, selected or not, we must ensure that we
      -- don't error out on such unsupported cases.
      Nothing -> return ()

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

recordTrace :: HasCallStack => ParseMsg -> ParseDecl ()
recordTrace trace = wrapEff $ \ParseSupport{parseEnv} ->
    traceWith (envTracer parseEnv) trace

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

unknownCursorKind :: (MonadIO m, HasCallStack) => CXCursorKind -> Fold m x
unknownCursorKind kind = simpleFold $ \curr -> do
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

dispatchFold ::
     MonadUnliftIO m
  => CXCursor
  -> (CXCursorKind -> Fold m a)
  -> m (Next m a)
dispatchFold x f = dispatch x $ \kind -> runFold (f kind) x
