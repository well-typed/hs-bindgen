{-# LANGUAGE Arrows #-}

-- | Logging
--
-- Indended for unqualified import.
module HsBindgen.Util.Tracer (
  -- | Data types and typeclasses useful for tracing
    Level (..)
  , PrettyTrace (..)
  , HasDefaultLogLevel (..)
  , Source (..)
  , HasSource (..)
  , Verbosity (..)
  -- | Tracer configuration
  , AnsiColor (..)
  , ShowTimeStamp (..)
  , ShowCallStack (..)
  , TracerConf (..)
  , defaultTracerConf
  , CustomLogLevel (..)
  -- | Trace with call stack
  , TraceWithCallStack (..)
  , traceWithCallStack
  , useTrace
  -- | Tracers
  , withTracerStdOut
  , withTracerFile
  , withTracerCustom
  ) where

import Control.Applicative (ZipList (ZipList, getZipList))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Tracer (Contravariant (contramap), Tracer (Tracer), emit,
                       squelchUnless, traceWith)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.Format (FormatTime)
import GHC.Stack (HasCallStack, callStack, CallStack, prettyCallStack)
import System.Console.ANSI (Color (..), ColorIntensity (Vivid),
                            ConsoleIntensity (BoldIntensity),
                            ConsoleLayer (Foreground),
                            SGR (SetColor, SetConsoleIntensity),
                            hSupportsANSIColor, setSGRCode)
import System.IO (IOMode (AppendMode), hPutStrLn, stdout, withFile)

{-------------------------------------------------------------------------------
  Data types and type classes useful for tracing
-------------------------------------------------------------------------------}

-- | Log or verbosity level.
--
-- Careful, the derived 'Ord' instance is used when determining if a trace
-- should be emitted, or not.
data Level = Debug | Info | Warning | Error
  deriving stock (Show, Eq, Ord)

alignLevel :: Level -> String
alignLevel = \case
  Debug   -> "Debug  "
  Info    -> "Info   "
  Warning -> "Warning"
  Error   -> "Error  "

getColorForLevel :: Level -> Color
getColorForLevel = \case
  Debug   -> White
  Info    -> Green
  Warning -> Yellow
  Error   -> Red

-- | Convert values to textual representations used in traces.
class PrettyTrace a where
  -- TODO: Use 'Text' (issue #650).
  prettyTrace :: a -> String

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

data AnsiColor = EnableAnsiColor | DisableAnsiColor
  deriving stock (Show, Eq)

data ShowTimeStamp = EnableTimeStamp | DisableTimeStamp
  deriving stock (Show, Eq)

data ShowCallStack = EnableCallStack | DisableCallStack
  deriving stock (Show, Eq)

-- | Configuration of tracer.
data TracerConf = TracerConf {
    tVerbosity     :: !Verbosity
  , tShowTimeStamp :: !ShowTimeStamp
  , tShowCallStack :: !ShowCallStack
  }
  deriving stock (Show, Eq)

defaultTracerConf :: TracerConf
defaultTracerConf = TracerConf
  { tVerbosity      = (Verbosity Info)
  , tShowTimeStamp  = DisableTimeStamp
  , tShowCallStack  = DisableCallStack
  }

-- | Sometimes, we want to change log levels. For example, we want to suppress
-- specific traces in tests.
data CustomLogLevel a = DefaultLogLevel | CustomLogLevel (a -> Level)

{-------------------------------------------------------------------------------
  Tracers
-------------------------------------------------------------------------------}

-- | Create a tracer emitting traces to a provided function @report@.
--
-- The traces provide additional information about
-- - the time,
-- - the log level, and
-- - the source.
mkTracer :: forall m a. (MonadIO m, PrettyTrace a, HasDefaultLogLevel a, HasSource a)
  => AnsiColor
  -> TracerConf
  -> CustomLogLevel a
  -> IORef Level
  -> (String -> m ())
  -> Tracer m (TraceWithCallStack a)
mkTracer ansiColor (TracerConf {..}) customLogLevel maxLogLevelRef report =
  squelchUnless (isLogLevelHighEnough . tTrace) $ Tracer $ emit $ traceAction
  where
    isLogLevelHighEnough :: a -> Bool
    isLogLevelHighEnough trace = getLogLevel trace >= unwrapVerbosity tVerbosity

    -- Log format:
    -- [OPTIONAL TIMESTAMP] [LEVEL] [SOURCE] Message.
    --   Indent subsequent lines.
    --   OPTION CALL STACK.
    traceAction :: TraceWithCallStack a -> m ()
    traceAction TraceWithCallStack {..} = do
      updateMaxLogLevel level
      time <- case tShowTimeStamp of
        DisableTimeStamp -> pure Nothing
        EnableTimeStamp -> Just <$> liftIO getCurrentTime
      mapM_ report $ getZipList $ formatLines time level source <*> traces
      when (tShowCallStack == EnableCallStack) $
        mapM_ (report . indent) $ lines $ prettyCallStack tCallStack
      where level = getLogLevel tTrace
            source = getSource tTrace
            traces = ZipList $ lines $ prettyTrace tTrace

    updateMaxLogLevel :: Level -> m ()
    updateMaxLogLevel level = liftIO $ modifyIORef maxLogLevelRef $ max level

    getLogLevel :: a -> Level
    getLogLevel x = case customLogLevel of
      DefaultLogLevel  -> getDefaultLogLevel x
      CustomLogLevel f -> f x

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

-- | Run an action with a tracer writing to 'stdout'. Use ANSI colors, if available.
--
-- Also return the maximum log level of traces.
withTracerStdOut :: MonadIO m => (PrettyTrace a, HasDefaultLogLevel a, HasSource a)
  => TracerConf
  -> CustomLogLevel a
  -> (Tracer m (TraceWithCallStack a) -> m b)
  -> m (b, Level)
withTracerStdOut tracerConf customLogLevel action = do
  supportsAnsiColor <- liftIO $ hSupportsANSIColor stdout
  let ansiColor = if supportsAnsiColor then EnableAnsiColor else DisableAnsiColor
  withIORef Debug $ \ref ->
    action $ mkTracer ansiColor tracerConf customLogLevel ref (liftIO . putStrLn)

-- | Run an action with a tracer writing to a file. Do not use ANSI colors.
--
-- Also return the maximum log level of traces.
withTracerFile
  :: (PrettyTrace a, HasDefaultLogLevel a, HasSource a)
  => FilePath
  -> TracerConf
  -> CustomLogLevel a
  -> (Tracer IO (TraceWithCallStack a) -> IO b)
  -> IO (b, Level)
withTracerFile file tracerConf customLogLevel action =
  withFile file AppendMode $ \handle ->
    withIORef Debug $ \ref ->
      action $
        mkTracer DisableAnsiColor tracerConf customLogLevel ref (hPutStrLn handle)

-- | Run an action with a tracer using a custom report function.
--
-- Also return the maximum log level of traces.
withTracerCustom
  :: forall m a b. (MonadIO m, PrettyTrace a, HasDefaultLogLevel a, HasSource a)
  => AnsiColor
  -> TracerConf
  -> CustomLogLevel a
  -> (String -> m ())
  -> (Tracer m (TraceWithCallStack a) -> m b)
  -> m (b, Level)
withTracerCustom ansiColor tracerConf customLogLevel report action =
  withIORef Debug $ \ref ->
    action $ mkTracer ansiColor tracerConf customLogLevel ref report

{-------------------------------------------------------------------------------
  Trace with call stack
-------------------------------------------------------------------------------}

data TraceWithCallStack a = TraceWithCallStack { tTrace     :: a
                                               , tCallStack :: CallStack }

instance Functor TraceWithCallStack where
  fmap f trace = trace { tTrace = f (tTrace trace) }

traceWithCallStack :: (Monad m, HasCallStack)
  => Tracer m (TraceWithCallStack a) -> a -> m ()
traceWithCallStack tracer trace =
  traceWith tracer (TraceWithCallStack trace callStack)

-- | Use, for example, to specialize a tracer with a call stack.
--
-- > useDiagnostic :: Tracer IO (TraceWithCallStack Trace)
-- >               -> Tracer IO (TraceWithCallStack Diagnostic)
-- > useDiagnostic = useTrace TraceDiagnostic
useTrace :: (Contravariant c, Functor f)  => (b -> a) -> c (f a) -> c (f b)
useTrace = contramap . fmap

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

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

withIORef :: MonadIO m => b -> (IORef b -> m a) -> m (a, b)
withIORef initialValue action = do
  ref <- liftIO $ newIORef initialValue
  actionResult <- action ref
  refResult <- liftIO $ readIORef ref
  pure (actionResult, refResult)
