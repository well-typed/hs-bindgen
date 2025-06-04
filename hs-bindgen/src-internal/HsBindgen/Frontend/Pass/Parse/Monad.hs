module HsBindgen.Frontend.Pass.Parse.Monad (
    -- * Definition
    M
  , ParseEnv (..)
  , runParseMonad
    -- * Functionality
    -- ** "Reader"
  , getTranslationUnit
  , getPredicate
  , getMainHeaders
    -- ** "Writer"
  , modifyIncludeGraph
    -- ** "State"
  , setMainHeader
  , getMainHeader
  , recordMacroExpansionAt
  , checkHasMacroExpansion
    -- ** Logging
  , ParseLog(..)
  , recordTraceWithCallStack
  ) where

import Control.Tracer (Tracer)
import Data.IORef
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Stack (CallStack)

import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import HsBindgen.C.Predicate (Predicate)
import HsBindgen.Eff
import HsBindgen.Frontend.Graph.Includes (IncludeGraph)
import HsBindgen.Frontend.Graph.Includes qualified as IncludeGraph
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.Util.Tracer
import HsBindgen.Errors

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
      parseEnv         :: ParseEnv           -- ^ Reader
    , parseExtraOutput :: IORef IncludeGraph -- ^ Writer (ish)
    , parseState       :: IORef ParseState   -- ^ State
    }

type instance Support ParseMonad = ParseSupport

runParseMonad :: ParseEnv -> M a -> IO (a, IncludeGraph)
runParseMonad env f = do
    support <- ParseSupport env
                 <$> newIORef IncludeGraph.empty
                 <*> newIORef initParseState
    result  <- unwrapEff f support
    (result,) <$> readIORef (parseExtraOutput support)

{-------------------------------------------------------------------------------
  "Reader"
-------------------------------------------------------------------------------}

data ParseEnv = ParseEnv {
      envUnit        :: CXTranslationUnit
    , envPredicate   :: Predicate
    , envMainHeaders :: Map SourcePath CHeaderIncludePath
    , envTracer      :: Tracer IO (TraceWithCallStack ParseLog)
    }

getTranslationUnit :: M CXTranslationUnit
getTranslationUnit = wrapEff $ \ParseSupport{parseEnv} ->
    pure $ envUnit parseEnv

getPredicate :: M Predicate
getPredicate = wrapEff $ pure . envPredicate . parseEnv

getMainHeaders :: M (Map SourcePath CHeaderIncludePath)
getMainHeaders = wrapEff $ pure . envMainHeaders . parseEnv

{-------------------------------------------------------------------------------
  "Writer"
-------------------------------------------------------------------------------}

modifyIncludeGraph :: (IncludeGraph -> IncludeGraph) -> M ()
modifyIncludeGraph f = wrapEff $ \ParseSupport{parseExtraOutput} ->
    modifyIORef parseExtraOutput f

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

setMainHeader :: CHeaderIncludePath -> M ()
setMainHeader includePath = wrapEff $ \ParseSupport{parseState} ->
    modifyIORef parseState $ \st -> st{
        stateMainHeader = Just includePath
      }

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
    -- | Executed @#include@
    RegisterInclude {
        registerIncludeIncludingFile :: SourcePath
      , registerIncludeIncludedFile :: SourcePath
      }

    -- | Clang builtin
  | SkippedBuiltIn Text

    -- | Definition skipped due to user's selection predicate
  | SkippedPredicate {
        skippedName   :: Text
      , skippedLoc    :: SingleLoc
      , skippedReason :: Text
      }

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
    RegisterInclude h i -> concat [
        "Registering include: "
      , show h
      , " includes "
      , show i
      ]
    SkippedBuiltIn x -> concat [
        "Skipped built-in: "
      , show x
      ]
    SkippedPredicate{..} -> Text.unpack $ mconcat [
        "Skipped "
      , skippedName
      , " at "
      , Text.pack (show skippedLoc)
      , ": "
      , skippedReason
      ]
    UnsupportedImplicitFields {..} -> concat [
        "Unsupported implicit field with ID "
      , show unsupportedImplicitFieldsId
      , " at "
      , show unsupportedImplicitFieldsLoc
      ]

instance HasDefaultLogLevel ParseLog where
  getDefaultLogLevel = \case
    RegisterInclude {}           -> Debug
    SkippedBuiltIn {}            -> Debug
    SkippedPredicate {}          -> Info
    UnsupportedImplicitFields {} -> Error

instance HasSource ParseLog where
  getSource = const HsBindgen

recordTraceWithCallStack :: CallStack -> ParseLog -> M ()
recordTraceWithCallStack stack trace = wrapEff $ \ParseSupport{parseEnv} ->
  traceWithCallStack (envTracer parseEnv) stack trace


