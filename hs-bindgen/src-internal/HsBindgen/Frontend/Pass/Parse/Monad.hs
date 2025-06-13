module HsBindgen.Frontend.Pass.Parse.Monad (
    -- * Definition
    M
  , ParseEnv (..)
  , runParseMonad
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
  , ParseTrace(..)
  , recordTrace
  ) where

import Control.Tracer (Tracer)
import Data.IORef
import Data.Set qualified as Set
import GHC.Stack

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import HsBindgen.C.Predicate (Predicate, IsMainFile)
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Eff
import HsBindgen.Frontend.NonSelectedDecls (NonSelectedDecls)
import HsBindgen.Frontend.NonSelectedDecls qualified as NonSelectedDecls
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.ProcessIncludes (GetMainHeader)
import HsBindgen.Frontend.RootHeader (RootHeader)
import HsBindgen.Frontend.Util.Fold (dispatch)
import HsBindgen.Imports
import HsBindgen.Language.C
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition

  We are careful to distinguish between the state that the computation can
  depend on ('MacroExpansions') and the additional output that generate during
  parsing but that cannot otherwise affect the computation ('IncludeGraph').
-------------------------------------------------------------------------------}

-- | Monad used during folding
type M = Eff ParseMonad

data ParseMonad a

-- | Support for 'M' (internal type, not exported)
data ParseSupport = ParseSupport {
      parseEnv   :: ParseEnv           -- ^ Reader
    , parseState :: IORef ParseState   -- ^ State
    }

type instance Support ParseMonad = ParseSupport

runParseMonad :: ParseEnv -> M a -> IO (NonSelectedDecls, a)
runParseMonad env f = do
    support <- ParseSupport env <$> newIORef initParseState
    x <- unwrapEff f support
    (, x) . stateNonSelectedDecls <$> readIORef (parseState support)

{-------------------------------------------------------------------------------
  "Reader"
-------------------------------------------------------------------------------}

data ParseEnv = ParseEnv {
      envUnit          :: CXTranslationUnit
    , envRootHeader    :: RootHeader
    , envIsMainFile    :: IsMainFile
    , envGetMainHeader :: GetMainHeader
    , envPredicate     :: Predicate
    , envTracer        :: Tracer IO (TraceWithCallStack ParseTrace)
    }

getTranslationUnit :: M CXTranslationUnit
getTranslationUnit = wrapEff $ \ParseSupport{parseEnv} ->
    return (envUnit parseEnv)

evalGetMainHeader :: SourcePath -> M CHeaderIncludePath
evalGetMainHeader path = wrapEff $ \ParseSupport{parseEnv} ->
    return $ (envGetMainHeader parseEnv) path

evalPredicate :: CXCursor -> M (Either Predicate.SkipReason ())
evalPredicate curr = wrapEff $ \ParseSupport{parseEnv} -> do
    matchResult <-
      Predicate.match (envIsMainFile parseEnv) (envPredicate parseEnv) curr
    case matchResult of
      Right ()    -> return ()
      Left reason -> traceWithCallStack (envTracer parseEnv) (Skipped reason)
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

recordMacroExpansionAt :: SingleLoc -> M ()
recordMacroExpansionAt loc = do
    wrapEff $ \ParseSupport{parseState} ->
      modifyIORef parseState $ \st -> st{
          stateMacroExpansions = Set.insert loc (stateMacroExpansions st)
        }

checkHasMacroExpansion :: Range SingleLoc -> M Bool
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

recordNonSelectedDecl :: CXCursor -> M ()
recordNonSelectedDecl curr = do
    mNamespace <- dispatch curr $ return . \case
      CXCursor_MacroDefinition -> Just NamespaceMacro
      CXCursor_StructDecl      -> Just NamespaceStruct
      CXCursor_UnionDecl       -> Just NamespaceUnion
      CXCursor_TypedefDecl     -> Just NamespaceTypedef
      CXCursor_EnumDecl        -> Just NamespaceEnum
      -- We intentionally do selection as part of parsing, rather than a
      -- separate step: if the user does not select certain declarations
      -- (perhaps because they live deep in the bowels of some system
      -- libraries), we also do not need to parse them. Since 'recordSource' is
      -- called on all declarations, selected or not, we must ensure that we
      -- don't error out on such unsupported cases.
      _kind                    -> Nothing
    case mNamespace of
      Just namespace -> getDeclId curr >>= \case
        DeclNamed cname -> do
          sourcePath <-
            singleLocPath <$> HighLevel.clang_getCursorLocation' curr
          wrapEff $ \ParseSupport{parseState} -> do
            modifyIORef parseState $ \st -> st{
                stateNonSelectedDecls =
                  NonSelectedDecls.insert (cname, namespace) sourcePath $
                    stateNonSelectedDecls st
              }
        DeclAnon{} -> return ()
      Nothing -> return ()

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

data ParseTrace =
    -- | We skipped over a declaration
    Skipped Predicate.SkipReason

    -- | Struct with implicit fields
    --
    -- We record the name of the struct that has the implicit fields.
  | UnsupportedImplicitFields {
        unsupportedImplicitFieldsId  :: DeclId
      , unsupportedImplicitFieldsLoc :: SingleLoc
      }
  deriving stock (Show)

instance PrettyTrace ParseTrace where
  prettyTrace = \case
      Skipped reason ->
          prettyTrace reason
      UnsupportedImplicitFields {..} -> concat [
          "Unsupported implicit field with ID "
        , show unsupportedImplicitFieldsId
        , " at "
        , show unsupportedImplicitFieldsLoc
        ]

instance HasDefaultLogLevel ParseTrace where
  getDefaultLogLevel = \case
      Skipped reason              -> getDefaultLogLevel reason
      UnsupportedImplicitFields{} -> Error

instance HasSource ParseTrace where
    getSource = const HsBindgen

recordTrace :: HasCallStack => ParseTrace -> M ()
recordTrace trace = wrapEff $ \ParseSupport{parseEnv} ->
    traceWithCallStack (envTracer parseEnv) trace
