{-# LANGUAGE Arrows #-}

-- | Logging
--
-- Indended for unqualified import.
module HsBindgen.Util.Tracer (
    -- * Tracer definition and main API
    Tracer -- opaque
  , Contravariant(..)
  , traceWith
  , simpleTracer
  , nullTracer
  , natTracer
    -- * Data types and typeclasses useful for tracing
  , Level (..)
  , PrettyForTrace (..)
  , HasDefaultLogLevel (..)
  , Source (..)
  , HasSource (..)
  , Verbosity (..)
    -- * Tracer configuration
  , ShowTimeStamp (..)
  , ShowCallStack (..)
  , TracerConfig (..)
  , AnsiColor (..)
  , Report
  , OutputConfig (..)
  , outputConfigQ
  , CustomLogLevel (..)
    -- * Tracers
  , withTracerStdOut
  , withTracerCustom
  , fatalError
  , withTracerCustom'
  ) where

import Control.Applicative (ZipList (ZipList, getZipList))
import Control.Monad (MonadPlus (mplus), when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Tracer (Contravariant (..))
import Control.Tracer qualified as ContraTracer
import Data.Default (Default (..))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.Format (FormatTime)
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)
import Language.Haskell.TH (Q, reportError, reportWarning)
import System.Console.ANSI (Color (..), ColorIntensity (Vivid),
                            ConsoleIntensity (BoldIntensity),
                            ConsoleLayer (Foreground),
                            SGR (SetColor, SetConsoleIntensity),
                            hSupportsANSIColor, setSGRCode)
import System.Exit (exitFailure)
import System.IO (Handle, hPutStrLn, stdout)

import Text.SimplePrettyPrint

{-------------------------------------------------------------------------------
  Definition and main API

  The definition of 'Tracer' is opaque.
-------------------------------------------------------------------------------}

newtype Tracer m a = Wrap {
      unwrap :: ContraTracer.Tracer m (MsgWithCallStack a)
    }

-- | We pair every trace message with a callstack for easier debugging
--
-- This is an internal type.
data MsgWithCallStack a = MsgWithCallStack {
      msgCallStack        :: CallStack
    , msgWithoutCallStack :: a
    }
  deriving stock (Show, Functor)

instance Monad m => Contravariant (Tracer m) where
  contramap f = Wrap . contramap (fmap f) . unwrap

traceWith :: (Monad m, HasCallStack) => Tracer m a -> a -> m ()
traceWith tracer =
      ContraTracer.traceWith (unwrap tracer)
    . MsgWithCallStack callStack

-- | Simple tracer that 'ContraTracer.emit's every message
simpleTracer :: Applicative m => (a -> m ()) -> Tracer m a
simpleTracer f = simpleWithCallStack (f . msgWithoutCallStack)

-- | Generalization of 'simpleWithCallStack'
--
-- This is internal API.
simpleWithCallStack :: Applicative m => (MsgWithCallStack a -> m ()) -> Tracer m a
simpleWithCallStack =
      Wrap
    . ContraTracer.Tracer
    . ContraTracer.emit

-- | See 'ContraTracer.squelchUnless'
squelchUnless :: Monad m => (a -> Bool) -> Tracer m a -> Tracer m a
squelchUnless p =
      Wrap
    . ContraTracer.squelchUnless (p . msgWithoutCallStack)
    . unwrap

nullTracer :: Monad m => Tracer m a
nullTracer = Wrap ContraTracer.nullTracer

natTracer :: (forall x. m x -> n x) -> (Tracer m a -> Tracer n a)
natTracer f =
      Wrap
    . ContraTracer.natTracer f
    . unwrap

{-------------------------------------------------------------------------------
  Data types and type classes useful for tracing
-------------------------------------------------------------------------------}

-- | Log or verbosity level.
--
-- Careful, the derived 'Ord' instance is used when determining if a trace
-- should be emitted, or not.
data Level =
    -- | Useful only for debugging hs-bindgen itself
    Debug

    -- | Regular progress, perhaps useful for debugging hs-bindgen config
    --
    -- E.g.: "Why why `foo` not selected?"
  | Info

    -- | Normal but significant condition the user should be aware of
    --
    -- E.g.: "binding global variable may result in duplicate symbols"
  | Notice

    -- | We may produce incomplete bindings
  | Warning

    -- | We are unable to produce /any/ bindings at all
  | Error
  deriving stock (Show, Eq, Ord)

alignLevel :: Level -> String
alignLevel = \case
  Debug   -> "Debug  "
  Info    -> "Info   "
  Notice  -> "Notice "
  Warning -> "Warning"
  Error   -> "Error  "

getColorForLevel :: Level -> Color
getColorForLevel = \case
  Debug   -> White
  Info    -> Green
  Notice  -> Yellow
  Warning -> Yellow
  Error   -> Red

-- | Convert values to textual representations used in traces.
class PrettyForTrace a where
  prettyForTrace :: a -> CtxDoc

-- | Get default (or suggested) log level of values used in traces.
class HasDefaultLogLevel a where
  getDefaultLogLevel :: a -> Level

-- | Possible sources of traces. The 'Source' is shown by default in traces, and
-- so should be useful to users of @hs-bindgen@.
data Source = Libclang | HsBindgen
  deriving stock (Show, Eq)

alignSource :: Source -> String
alignSource = \case
  Libclang  -> "Libclang "
  HsBindgen -> "HsBindgen"

-- | Get source or context of values used in traces.
class HasSource a where
  getSource :: a -> Source

newtype Verbosity = Verbosity { unwrapVerbosity :: Level }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Tracer configuration
-------------------------------------------------------------------------------}

data ShowTimeStamp = EnableTimeStamp | DisableTimeStamp
  deriving stock (Show, Eq)

data ShowCallStack = EnableCallStack | DisableCallStack
  deriving stock (Show, Eq)

-- | Configuration of tracer.
data TracerConfig = TracerConfig {
    tVerbosity     :: !Verbosity
  , tShowTimeStamp :: !ShowTimeStamp
  , tShowCallStack :: !ShowCallStack
  }
  deriving stock (Show, Eq)

instance Default TracerConfig where
  def = TracerConfig
    { tVerbosity      = (Verbosity Info)
    , tShowTimeStamp  = DisableTimeStamp
    , tShowCallStack  = DisableCallStack
    }

data AnsiColor = EnableAnsiColor | DisableAnsiColor
  deriving stock (Show, Eq)

-- | The report function needs to know about the log level. For example, with
-- Template Haskell, errors and warnings are treated differently compared to
-- debug and info messages.
type Report m = Level -> String -> m ()

data OutputConfig m =
    OutputHandle {
      _outputHandle           :: Handle
      -- | 'Nothing': Automatically determine ANSI color support by examining
      -- the 'Handle'; see 'getAnsiColor'.
    , _outputAnsiColorSetting :: Maybe AnsiColor
    }
  | OutputReport {
      _outputReport    :: Report m
    , _outputAnsiColor :: AnsiColor
    }

instance Default (OutputConfig m) where
  def = OutputHandle stdout Nothing

-- | Output configuration suitable for compile-time code generation with
-- Template Haskell.
--
-- Propagate warnings and errors to GHC.
--
-- Report traces with other log levels to `stdout`.
outputConfigQ :: OutputConfig Q
outputConfigQ = OutputReport report DisableAnsiColor
  where report = \case
          Warning -> reportWarning
          Error   -> reportError
          _level  -> liftIO . putStrLn

-- | Sometimes, we want to change log levels. For example, we want to suppress
-- specific traces in tests.
--
-- The custom log level function takes a trace and either returns 'Just' a
-- custom log level, or 'Nothing', if it does not customize the log level of the
-- trace.
newtype CustomLogLevel a = CustomLogLevel { getCustomLogLevel :: a -> Maybe Level }

-- | NOTE: Custom log levels on the left-hand-side overrule custom log levels on
-- the right-hand-side.
instance Semigroup (CustomLogLevel a) where
  (CustomLogLevel l) <> (CustomLogLevel r) =
    CustomLogLevel $ \x -> l x `mplus` r x

instance Monoid (CustomLogLevel a) where
  mempty = CustomLogLevel $ const Nothing

{-------------------------------------------------------------------------------
  Tracers
-------------------------------------------------------------------------------}

-- | Run an action with a tracer writing to 'stdout'.
--
-- Use ANSI colors, if available.
--
-- Return 'Nothing' if an 'Error' trace was emitted.
withTracerStdOut ::
     ( MonadIO m
     , PrettyForTrace a
     , HasDefaultLogLevel a
     , HasSource a
     )
  => TracerConfig
  -> (Tracer m a -> m b)
  -> m (Maybe b)
withTracerStdOut = withTracerCustom def mempty

-- | Run an action with a tracer using a custom report function.
--
-- Return 'Nothing' if an 'Error' trace was emitted.
withTracerCustom
  :: forall m a b. (MonadIO m, PrettyForTrace a, HasDefaultLogLevel a, HasSource a)
  => OutputConfig m
  -> CustomLogLevel a
  -> TracerConfig
  -> (Tracer m a -> m b)
  -> m (Maybe b)
withTracerCustom outputConfig customLogLevel tracerConf action =
  nothingOnError (withTracerCustom' outputConfig customLogLevel tracerConf action)

-- | Report that errors happened and exit with failure.
fatalError :: MonadIO m => m a
fatalError = liftIO $ do
  putStrLn "An error happened (see above)"
  exitFailure

-- | Run an action with a tracer using a custom report function.
--
-- Return the maximum log level of traces.
--
-- We do not export this function from the public interface, but use it in
-- tests.
withTracerCustom' :: forall m a b.
  (MonadIO m, PrettyForTrace a, HasDefaultLogLevel a,  HasSource a)
  => OutputConfig m
  -> CustomLogLevel a
  -> TracerConfig
  -> (Tracer m a -> m b)
  -> m (b, Level)
withTracerCustom' outputConfig customLogLevel tracerConf action = do
  (report, ansiColor) <- getReportAndAnsiColor
  withIORef Debug $ \ref ->
    action $ mkTracer ansiColor customLogLevel ref tracerConf report
  where
    getReportAndAnsiColor :: m (Report m, AnsiColor)
    getReportAndAnsiColor = case outputConfig of
      OutputHandle handle ansiColorSetting -> do
        ansiColor <- case ansiColorSetting of
          Nothing -> getAnsiColor handle
          Just x  -> pure x
        let report _lvl = liftIO . hPutStrLn handle
        pure (report, ansiColor)
      OutputReport report ansiColor -> pure (report, ansiColor)

{-------------------------------------------------------------------------------
  Internal helpers
-------------------------------------------------------------------------------}

-- | Create a tracer emitting traces to a provided function @report@.
--
-- The traces provide additional information about
-- - the time,
-- - the log level, and
-- - the source.
mkTracer :: forall m a.
     ( MonadIO m
     , PrettyForTrace a
     , HasDefaultLogLevel a
     , HasSource a
     )
  => AnsiColor
  -> CustomLogLevel a
  -> IORef Level
  -> TracerConfig
  -> Report m
  -> Tracer m a
mkTracer ansiColor customLogLevel maxLogLevelRef (TracerConfig {..}) report =
  squelchUnless isLogLevelHighEnough $ simpleWithCallStack $ traceAction
  where
    isLogLevelHighEnough :: a -> Bool
    isLogLevelHighEnough trace = getLogLevel trace >= unwrapVerbosity tVerbosity

    -- Log format:
    -- [OPTIONAL TIMESTAMP] [LEVEL] [SOURCE] Message.
    --   Indent subsequent lines.
    --   OPTION CALL STACK.
    traceAction :: MsgWithCallStack a -> m ()
    traceAction MsgWithCallStack {..} = do
      updateMaxLogLevel level
      time <- case tShowTimeStamp of
        DisableTimeStamp -> pure Nothing
        EnableTimeStamp -> Just <$> liftIO getCurrentTime
      mapM_ (report level) $ getZipList $ formatLines time level source <*> traces
      when (tShowCallStack == EnableCallStack) $
        report level $ renderCtxDoc (indentContext 2 context)
          $ string $ prettyCallStack msgCallStack
      where level = getLogLevel msgWithoutCallStack
            source = getSource msgWithoutCallStack
            traces = ZipList $ lines
              $ renderCtxDoc context
              $ prettyForTrace msgWithoutCallStack

    context :: Context
    context = mkContext 120

    updateMaxLogLevel :: Level -> m ()
    updateMaxLogLevel level = liftIO $ modifyIORef maxLogLevelRef $ max level

    getLogLevel :: a -> Level
    getLogLevel x = case getCustomLogLevel customLogLevel x of
      Nothing  -> getDefaultLogLevel x
      Just lvl -> lvl

    showTime :: FormatTime t => t -> String
    showTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%3QZ"

    prependTime :: FormatTime t => t -> String -> String
    prependTime time x = '[' : showTime time <> "] " <> x

    prependLevel :: Level -> String -> String
    prependLevel level x =
      withColor ansiColor level ('[' : alignLevel level <> "] ") <> x

    prependSource :: Level -> Source -> String -> String
    prependSource level source x =
      withColor ansiColor level ('[' : alignSource source <> "] ") <> x

    prependLabels :: Maybe UTCTime -> Level -> Source -> String -> String
    prependLabels mTime level source =
      maybePrependTime . prependLevel level . prependSource level source
      where maybePrependTime = case mTime of
              Nothing   -> id
              Just time -> prependTime time

    formatLines :: Maybe UTCTime -> Level -> Source -> ZipList (String -> String)
    formatLines mTime level source = ZipList $
      prependLabels mTime level source : repeat indent

    indent :: String -> String
    indent = ("  " <>)

-- | Render a string in bold and a specified color.
--
-- Careful, the applied ANSI code suffix also resets all other activated formatting.
withColor :: AnsiColor -> Level -> String -> String
withColor DisableAnsiColor _     = id
withColor EnableAnsiColor    level = withColor' (getColorForLevel level)
  where
    withColor' :: Color -> String -> String
    withColor' color x = setColor <> x <> resetColor
      where
        setColor :: String
        setColor = setSGRCode [ SetColor Foreground Vivid color
                              , SetConsoleIntensity BoldIntensity ]

        resetColor :: String
        resetColor = setSGRCode []

nothingOnError :: MonadIO m => m (a, Level) -> m (Maybe a)
nothingOnError k = k >>= \case
  (_, Error) -> pure $ Nothing
  (r, _    ) -> pure $ Just r

withIORef :: MonadIO m => b -> (IORef b -> m a) -> m (a, b)
withIORef initialValue action = do
  ref <- liftIO $ newIORef initialValue
  actionResult <- action ref
  refResult <- liftIO $ readIORef ref
  pure (actionResult, refResult)

getAnsiColor :: MonadIO m => Handle -> m AnsiColor
getAnsiColor handle = do
    supportsAnsiColor <- liftIO $ hSupportsANSIColor handle
    pure $ if supportsAnsiColor then EnableAnsiColor else DisableAnsiColor
