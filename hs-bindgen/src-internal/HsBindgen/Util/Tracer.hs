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
  , ErrorTraceException (..)
    -- * Tracer configuration
  , AnsiColor (..)
  , ShowTimeStamp (..)
  , ShowCallStack (..)
  , TracerConf (..)
  , CustomLogLevel (..)
    -- * Tracers
  , withTracerStdOut
  , withTracerFile
  , withTracerCustom
  , withTracerCustom'
  ) where

import Control.Applicative (ZipList (ZipList, getZipList))
import Control.Exception (Exception (..), throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Tracer (Contravariant (..))
import Control.Tracer qualified as ContraTracer
import Data.Default (Default (..))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.Format (FormatTime)
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)
import System.Console.ANSI (Color (..), ColorIntensity (Vivid),
                            ConsoleIntensity (BoldIntensity),
                            ConsoleLayer (Foreground),
                            SGR (SetColor, SetConsoleIntensity),
                            hSupportsANSIColor, setSGRCode)
import System.IO (Handle, IOMode (AppendMode), hPutStrLn, stdout, withFile)

import HsBindgen.Errors (hsBindgenExceptionFromException,
                         hsBindgenExceptionToException)
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

data ErrorTraceException = ErrorTraceException

instance Show ErrorTraceException where
  show _ = "An error happened while generating bindings (see above)"

instance Exception ErrorTraceException where
  toException = hsBindgenExceptionToException
  fromException = hsBindgenExceptionFromException

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

instance Default TracerConf where
  def = TracerConf
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

-- | Run an action with a tracer writing to 'stdout'.
--
-- Use ANSI colors, if available.
--
-- Throw exception after performing action if an 'Error' trace was emitted.
withTracerStdOut ::
     ( MonadIO m
     , PrettyForTrace a
     , HasDefaultLogLevel a
     , HasSource a
     )
  => TracerConf
  -> CustomLogLevel a
  -> (Tracer m a -> m b)
  -> m b
withTracerStdOut tracerConf customLogLevel action = do
  ansiColor <- getAnsiColor stdout
  withExceptionOnError $ \ref ->
    action $ mkTracer ansiColor tracerConf customLogLevel ref (liftIO . putStrLn)

-- | Run an action with a tracer writing to a file. Do not use ANSI colors.
--
-- Throw exception after performing action if an 'Error' trace was emitted.
withTracerFile
  :: (PrettyForTrace a, HasDefaultLogLevel a, HasSource a)
  => FilePath
  -> TracerConf
  -> CustomLogLevel a
  -> (Tracer IO a -> IO b)
  -> IO b
withTracerFile file tracerConf customLogLevel action =
  withFile file AppendMode $ \handle ->
    withExceptionOnError $ \ref ->
      action $
        mkTracer DisableAnsiColor tracerConf customLogLevel ref (hPutStrLn handle)

-- | Run an action with a tracer using a custom report function.
--
-- Throw exception after performing action if an 'Error' trace was emitted.
withTracerCustom
  :: forall m a b. (MonadIO m, PrettyForTrace a, HasDefaultLogLevel a, HasSource a)
  => AnsiColor
  -> TracerConf
  -> CustomLogLevel a
  -> (String -> m ())
  -> (Tracer m a -> m b)
  -> m b
withTracerCustom ansiColor tracerConf customLogLevel report action =
  exceptionOnError (withTracerCustom' ansiColor tracerConf customLogLevel report action)

-- | Run an action with a tracer using a custom report function.
--
-- Do not throw exception on 'Error' traces; instead return the maximum log
-- level of traces.
withTracerCustom' :: (MonadIO m, PrettyForTrace a, HasDefaultLogLevel a,  HasSource a)
  => AnsiColor
  -> TracerConf
  -> CustomLogLevel a
  -> (String -> m ())
  -> (Tracer m a -> m b)
  -> m (b, Level)
withTracerCustom' ansiColor tracerConf customLogLevel report action =
  withIORef Debug $ \ref ->
    action $ mkTracer ansiColor tracerConf customLogLevel ref report

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
  -> TracerConf
  -> CustomLogLevel a
  -> IORef Level
  -> (String -> m ())
  -> Tracer m a
mkTracer ansiColor (TracerConf {..}) customLogLevel maxLogLevelRef report =
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
      mapM_ report $ getZipList $ formatLines time level source <*> traces
      when (tShowCallStack == EnableCallStack) $
        report $ renderCtxDoc (indentContext 2 context)
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

exceptionOnError :: MonadIO m => m (a, Level) -> m a
exceptionOnError k = k >>= \case
  (_, Error) -> liftIO $ throwIO ErrorTraceException
  (r, _    ) -> pure r

withExceptionOnError :: MonadIO m => (IORef Level -> m a) -> m a
withExceptionOnError action = exceptionOnError (withIORef Debug action)

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
