module HsBindgen.Frontend.Pass.Parse.Monad (
    -- * Definition
    M
  , ParseLog(..)
  , ParseEnv (..)
  , runParseMonad
    -- * Functionaltiy
  , liftIO
  , modifyIncludeGraph
  , getTranslationUnit
  , getPredicate
  , getMainSourcePaths
  , recordTraceWithCallStack
  , recordMacroExpansionAt
  , checkHasMacroExpansion
  ) where

import Control.Monad.IO.Class
import Control.Tracer (Tracer)
import Data.IORef
import Data.Set qualified as Set
import GHC.Stack (CallStack)

import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths (SourcePath)
import Data.Text qualified as Text
import HsBindgen.C.Predicate (Predicate)
import HsBindgen.Eff
import HsBindgen.Frontend.Graph.Includes (IncludeGraph)
import HsBindgen.Frontend.Graph.Includes qualified as IncludeGraph
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.Util.Tracer (HasDefaultLogLevel (getDefaultLogLevel),
                              HasSource (getSource), Level (Debug, Error, Info),
                              PrettyTrace (prettyTrace), Source (HsBindgen),
                              TraceWithCallStack, traceWithCallStack)

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
      parseEnv         :: ParseEnv              -- ^ Reader
    , parseExtraOutput :: IORef IncludeGraph    -- ^ Writer (ish)
    , parseState       :: IORef (Set SingleLoc) -- ^ State
    }

data ParseLog =
      RegisterInclude {
        registerIncludeIncludingFile :: SourcePath
      , registerIncludeIncludedFile :: SourcePath
      }
    | SkippedBuiltIn Text
    | SkippedPredicate {
        skippedName   :: Text
      , skippedLoc    :: MultiLoc
      , skippedReason :: Text
      }
      -- | Struct with implicit fields
      --
      -- We record the name of the struct that has the implicit fields.
    | UnsupportedImplicitFields {
        unsupportedImplicitFieldsId  :: DeclId
      , unsupportedImplicitFieldsLoc :: MultiLoc
      }
  deriving stock (Show)

instance PrettyTrace ParseLog where
  prettyTrace = \case
    RegisterInclude h i -> "Registering include: " <> show h <> " includes " <> show i
    SkippedBuiltIn x ->
      "Skipped built-in: " <> show x
    SkippedPredicate {..} ->
      Text.unpack $ Text.concat [ "Skipped "
                                , skippedName
                                , " at "
                                , Text.pack (show skippedLoc)
                                , ": "
                                , skippedReason
                                ]
    UnsupportedImplicitFields {..} ->
      concat [ "Unsupported implicit field with ID "
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

data ParseEnv = ParseEnv {
    envUnit            :: CXTranslationUnit
  , envPredicate       :: Predicate
  , envMainSourcePaths :: Set SourcePath
  , envTracer          :: Tracer IO (TraceWithCallStack ParseLog)
  }

type instance Support ParseMonad = ParseSupport

runParseMonad :: ParseEnv -> M a -> IO (a, IncludeGraph)
runParseMonad env f = do
    support <- ParseSupport env
                 <$> newIORef IncludeGraph.empty
                 <*> newIORef Set.empty
    result  <- unwrapEff f support
    (result,) <$> readIORef (parseExtraOutput support)

modifyIncludeGraph :: (IncludeGraph -> IncludeGraph) -> M ()
modifyIncludeGraph f = wrapEff $ \ParseSupport{parseExtraOutput} ->
    modifyIORef parseExtraOutput f

getTranslationUnit :: M CXTranslationUnit
getTranslationUnit = wrapEff $ \ParseSupport{parseEnv} ->
    pure $ envUnit parseEnv

getPredicate :: M Predicate
getPredicate = wrapEff $ pure . envPredicate . parseEnv

getMainSourcePaths :: M (Set SourcePath)
getMainSourcePaths = wrapEff $ pure . envMainSourcePaths . parseEnv

recordTraceWithCallStack :: CallStack -> ParseLog -> M ()
recordTraceWithCallStack stack trace = wrapEff $ \ParseSupport{parseEnv} ->
  traceWithCallStack (envTracer parseEnv) stack trace

recordMacroExpansionAt :: SingleLoc -> M ()
recordMacroExpansionAt loc = do
    wrapEff $ \ParseSupport{parseState} ->
      modifyIORef parseState $ Set.insert loc

checkHasMacroExpansion :: Range SingleLoc -> M Bool
checkHasMacroExpansion extent = do
    wrapEff $ \ParseSupport{parseState} ->
      aux extent <$> readIORef parseState
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
