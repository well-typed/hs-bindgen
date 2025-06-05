module HsBindgen.Frontend.Pass.Parse.Monad (
    -- * Definition
    M
  , ParseEnv (..)
  , runParseMonad
    -- * Functionality
    -- ** "Reader"
  , getTranslationUnit
  , evalPredicate
    -- ** "State"
  , updateMainHeader
  , getMainHeader
  , recordMacroExpansionAt
  , checkHasMacroExpansion
    -- ** Logging
  , ParseLog(..)
  , recordTrace
  ) where

import Control.Tracer (Tracer)
import Data.IORef
import Data.Set qualified as Set
import GHC.Stack

import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import HsBindgen.C.Predicate (Predicate, IsMainFile)
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Eff
import HsBindgen.Errors
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Frontend.RootHeader (RootHeader)
import HsBindgen.Imports
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

runParseMonad :: ParseEnv -> M a -> IO a
runParseMonad env f = do
    support <- ParseSupport env <$> newIORef initParseState
    unwrapEff f support

{-------------------------------------------------------------------------------
  "Reader"
-------------------------------------------------------------------------------}

data ParseEnv = ParseEnv {
      envUnit       :: CXTranslationUnit
    , envRootHeader :: RootHeader
    , envIsMainFile :: IsMainFile
    , envPredicate  :: Predicate
    , envTracer     :: Tracer IO (TraceWithCallStack ParseLog)
    }

getTranslationUnit :: M CXTranslationUnit
getTranslationUnit = wrapEff $ \ParseSupport{parseEnv} ->
    return (envUnit parseEnv)

evalPredicate :: CXCursor -> M Bool
evalPredicate curr = wrapEff $ \ParseSupport{parseEnv} -> do
    isMatch <- Predicate.match
                 (envIsMainFile parseEnv)
                 (envPredicate  parseEnv)
                 curr
    case isMatch of
      Right ()     -> return True
      Left  reason -> do
        traceWithCallStack (envTracer parseEnv) callStack $ Skipped reason
        return False

{-------------------------------------------------------------------------------
  "State"
-------------------------------------------------------------------------------}

data ParseState = ParseState {
      -- | Where did clang expand macros?
      --
      -- Declarations with expanded macros need to be reparsed.
      stateMacroExpansions :: Set SingleLoc

      -- | Current main header
      --
      -- This is exclusively used to set 'functionHeader'.
    , stateMainHeader :: Maybe CHeaderIncludePath
    }

initParseState :: ParseState
initParseState = ParseState{
      stateMacroExpansions = Set.empty
    , stateMainHeader      = Nothing
    }

-- | Update the main header, when necessary
--
-- Should only be called on 'CXCursor_InclusionDirective'.
updateMainHeader :: CXCursor -> M ()
updateMainHeader curr = do
    wrapEff $ \ParseSupport{parseEnv, parseState} ->
      ifFromRootHeader_ (envRootHeader parseEnv) curr $ \path ->
        modifyIORef parseState $ \st -> st{stateMainHeader = Just path}

getMainHeader :: M CHeaderIncludePath
getMainHeader = wrapEff $ \ParseSupport{parseState} -> do
     state <- readIORef parseState
     case stateMainHeader state of
       Just header -> return header
       Nothing     ->
         -- This should not happen: we only ask for the main header when we
         -- are generating a function, which must ultimately come from one of
         -- the main headers.
         panicIO "No main header"

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

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

data ParseLog =
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

instance PrettyTrace ParseLog where
  prettyTrace = \case
      Skipped reason ->
          prettyTrace reason
      UnsupportedImplicitFields {..} -> concat [
          "Unsupported implicit field with ID "
        , show unsupportedImplicitFieldsId
        , " at "
        , show unsupportedImplicitFieldsLoc
        ]

instance HasDefaultLogLevel ParseLog where
  getDefaultLogLevel = \case
      Skipped reason              -> getDefaultLogLevel reason
      UnsupportedImplicitFields{} -> Error

instance HasSource ParseLog where
    getSource = const HsBindgen

recordTrace :: HasCallStack => ParseLog -> M ()
recordTrace trace = wrapEff $ \ParseSupport{parseEnv} ->
    traceWithCallStack (envTracer parseEnv) callStack trace
