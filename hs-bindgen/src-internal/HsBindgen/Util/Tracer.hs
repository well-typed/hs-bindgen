{-# LANGUAGE Arrows #-}

-- | Logging
--
-- Indended for unqualified import.
module HsBindgen.Util.Tracer (
    -- * Tracer definition and main API
    Tracer -- opaque
  , traceWith
  , simpleTracer
  , nullTracer
    -- * Data types and typeclasses useful for tracing
  , PrettyForTrace (..)
  , Level (..)
  , SafeLevel (..)
  , Source (..)
  , TraceId (..)
  , IsTrace (..)
  , Verbosity (..)
  , IsUserRequested (..)
  , userRequestedIf
    -- * Tracer configuration
  , ShowTimeStamp (..)
  , ShowCallStack (..)
  , AnsiColor (..)
  , Report
  , OutputConfig (..)
  , outputConfigTH
  , CustomLogLevel (..)
  , TracerConfig (..)
    -- * Tracers
  , withTracer
  , TracerState(..)
  , withTracer'
  , checkTracerState
    -- * Errors
  , TraceException (..)
  , FileSystemException (..)
    -- * Safe tracers
  , withTracerSafe
  , SafeTrace(..)
    -- * Re-exports
  , Contravariant(..)
  ) where

import Control.Exception (Exception (..))
import Control.Tracer (Contravariant (..))
import Control.Tracer qualified as ContraTracer
import Data.Bool (bool)
import Data.Data (Typeable)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.Format (FormatTime)
import GHC.Generics as GHC
import GHC.Stack (CallStack, callStack, prettyCallStack)
import Language.Haskell.TH (reportError, reportWarning, runQ)
import System.Console.ANSI (Color (..), ColorIntensity (Vivid),
                            ConsoleIntensity (BoldIntensity),
                            ConsoleLayer (Foreground),
                            SGR (SetColor, SetConsoleIntensity),
                            hSupportsANSIColor, setSGRCode)
import System.IO (Handle, hPutStr, stderr)
import Text.SimplePrettyPrint (Context, CtxDoc)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Errors
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition and main API

  The definition of 'Tracer' is opaque.
-------------------------------------------------------------------------------}

newtype Tracer a = Wrap {
      unwrap :: ContraTracer.Tracer IO (MsgWithCallStack a)
    }

-- | We pair every trace message with a callstack for easier debugging
--
-- This is an internal type.
data MsgWithCallStack a = MsgWithCallStack {
      msgCallStack        :: CallStack
    , msgWithoutCallStack :: a
    }
  deriving stock (Show, Functor)

instance Contravariant Tracer where
  contramap f = Wrap . contramap (fmap f) . unwrap

traceWith :: (MonadIO m, HasCallStack) => Tracer a -> a -> m ()
traceWith tracer =
      liftIO
    . ContraTracer.traceWith (unwrap tracer)
    . MsgWithCallStack callStack

-- | Simple tracer that 'ContraTracer.emit's every message
simpleTracer :: (a -> IO ()) -> Tracer a
simpleTracer f = simpleWithCallStack (f . msgWithoutCallStack)

-- | Generalization of 'simpleWithCallStack'
--
-- This is internal API.
simpleWithCallStack :: (MsgWithCallStack a -> IO ()) -> Tracer a
simpleWithCallStack =
      Wrap
    . ContraTracer.Tracer
    . ContraTracer.emit

-- | See 'ContraTracer.squelchUnless'
squelchUnless :: (a -> Bool) -> Tracer a -> Tracer a
squelchUnless p =
      Wrap
    . ContraTracer.squelchUnless (p . msgWithoutCallStack)
    . unwrap

nullTracer :: Tracer a
nullTracer = Wrap ContraTracer.nullTracer

{-------------------------------------------------------------------------------
  Data types and type classes useful for tracing
-------------------------------------------------------------------------------}

-- | Convert values to textual representations used in traces.
class PrettyForTrace a where
  prettyForTrace :: a -> CtxDoc
  default prettyForTrace :: (Generic a, GPrettyForTrace (Rep a)) => a -> CtxDoc
  prettyForTrace = gPrettyForTrace'

class GPrettyForTrace (r :: Type -> Type) where
  gPrettyForTrace :: r x -> CtxDoc

instance GPrettyForTrace r => GPrettyForTrace (M1 tag meta r) where
  gPrettyForTrace (M1 x) = gPrettyForTrace x

instance (GPrettyForTrace r1, GPrettyForTrace r2) => GPrettyForTrace (r1 :+: r2) where
  gPrettyForTrace (L1 x) = gPrettyForTrace x
  gPrettyForTrace (R1 x) = gPrettyForTrace x

instance PrettyForTrace a => GPrettyForTrace (K1 tag a) where
  gPrettyForTrace (K1 x) = prettyForTrace x

gPrettyForTrace' :: (GHC.Generic a, GPrettyForTrace (GHC.Rep a)) => a -> CtxDoc
gPrettyForTrace' = gPrettyForTrace .  GHC.from

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
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

instance Default Level where
  def = Notice

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

-- | Safe log or verbosity level to be used by the backend.
--
-- We intentionally limit 'SafeLevel' to debug and info messages. Notices,
-- warnings and errors should be handled in the frontend.
--
-- Traces with safe log levels can not abort the program.
data SafeLevel = SafeDebug | SafeInfo | SafeNotice
  deriving stock (Show, Eq, Ord, Bounded)

fromSafeLevel :: SafeLevel -> Level
fromSafeLevel = \case
  SafeDebug  -> Debug
  SafeInfo   -> Info
  SafeNotice -> Notice

toSafeLevel :: Level -> Maybe SafeLevel
toSafeLevel = \case
  Debug  -> Just SafeDebug
  Info   -> Just SafeInfo
  Notice -> Just SafeNotice
  _      -> Nothing

-- | Possible sources of traces. The 'Source' is shown by default in traces, and
-- so should be useful to users of @hs-bindgen@. At the moment, we only
-- distinguish between 'Libclang' and 'HsBindgen'.
data Source = Libclang | HsBindgen
  deriving stock (Show, Eq)

alignSource :: Source -> String
alignSource = \case
  Libclang  -> "Libclang "
  HsBindgen -> "HsBindgen"

newtype TraceId = TraceId { unTraceId :: String }
  deriving stock (Show, Eq, Ord)
  deriving (IsString, Semigroup, Monoid) via String

class PrettyForTrace a => IsTrace l a | a -> l where
  -- | Get default (or suggested) log level of a trace.
  getDefaultLogLevel :: a -> l
  default getDefaultLogLevel :: (Generic a, GIsTrace l (Rep a)) => a -> l
  getDefaultLogLevel = gGetDefaultLogLevel'
  -- | Get source or context of trace.
  getSource :: a -> Source
  default getSource :: (Generic a, GIsTrace l (Rep a)) => a -> Source
  getSource = gGetSource'
  -- | The trace identifier does not necessarily have to be unique. For example,
  -- an non-unique identifier may be used to collect multiple traces in a single
  -- category.
  --
  -- The trace identifier usually starts with a letter and contains only letters
  -- and dashes (e.g., @my-trace-id@). A good default is the constructor name in
  -- kebab-case, leaving out potential "-trace" or "-msg" suffixes.
  getTraceId :: a -> TraceId
  default getTraceId :: (Generic a, GIsTrace l (Rep a)) => a -> TraceId
  getTraceId = gGetTraceId'

class GIsTrace l (r :: Type -> Type) | r -> l where
  gGetDefaultLogLevel :: r x -> l
  gGetSource          :: r x -> Source
  gGetTraceId         :: r x -> TraceId

instance GIsTrace l r => GIsTrace l (M1 tag meta r) where
  gGetDefaultLogLevel (M1 x) = gGetDefaultLogLevel x
  gGetSource          (M1 x) = gGetSource          x
  gGetTraceId         (M1 x) = gGetTraceId         x

instance (GIsTrace l r1, GIsTrace l r2) => GIsTrace l (r1 :+: r2) where
  gGetDefaultLogLevel (L1 x) = gGetDefaultLogLevel x
  gGetDefaultLogLevel (R1 x) = gGetDefaultLogLevel x
  gGetSource          (L1 x) = gGetSource          x
  gGetSource          (R1 x) = gGetSource          x
  gGetTraceId         (L1 x) = gGetTraceId         x
  gGetTraceId         (R1 x) = gGetTraceId         x

instance IsTrace l a => GIsTrace l (K1 tag a) where
  gGetDefaultLogLevel (K1 x) = getDefaultLogLevel x
  gGetSource          (K1 x) = getSource          x
  gGetTraceId         (K1 x) = getTraceId         x

gGetDefaultLogLevel' :: (GHC.Generic a, GIsTrace l (GHC.Rep a)) => a -> l
gGetDefaultLogLevel' = gGetDefaultLogLevel . GHC.from

gGetSource' :: (GHC.Generic a, GIsTrace l (GHC.Rep a)) => a -> Source
gGetSource' = gGetSource . GHC.from

gGetTraceId' :: (GHC.Generic a, GIsTrace l (GHC.Rep a)) => a -> TraceId
gGetTraceId' = gGetTraceId . GHC.from

newtype Verbosity = Verbosity { unwrapVerbosity :: Level }
  deriving stock (Show, Eq)
  deriving Default via Level

-- | User requested status
--
-- The log level of some trace messages depend on if the user explicitly
-- requested something or not.
data IsUserRequested = UserRequested | NotUserRequested
  deriving stock (Show, Eq)

userRequestedIf :: Bool -> IsUserRequested
userRequestedIf = bool NotUserRequested UserRequested

{-------------------------------------------------------------------------------
  Tracer configuration
-------------------------------------------------------------------------------}

data AnsiColor = EnableAnsiColor | DisableAnsiColor
  deriving stock (Show, Eq)

data ShowTimeStamp = EnableTimeStamp | DisableTimeStamp
  deriving stock (Show, Eq)

data ShowCallStack = EnableCallStack | DisableCallStack
  deriving stock (Show, Eq)

-- | We provide both, the trace itself and the formatted message. For example, a
-- report function may want to store the trace only, and ignore the formatted
-- message.
--
-- Further, a report function may need to know about the log level. For example,
-- with Template Haskell, errors and warnings are treated differently compared
-- to debug and info messages. NOTE: Traces do have default log levels, but we
-- need the custom log level. This is sub-optimal. We could use newtype wrappers
-- to directly change the log level, and not provide the custom log level
-- separately here.
--
-- The report function has access to the typed trace @a@, and the formatted
-- trace. The formatted trace also possibly contains the time stamp, the call
-- stack, or other information.
type Report a = Level -> a -> String -> IO ()

data OutputConfig a =
    OutputHandle {
      _outputHandle           :: Handle
      -- | 'Nothing': Automatically determine ANSI color support by examining
      -- the 'Handle'; see 'getAnsiColor'.
    , _outputAnsiColorSetting :: Maybe AnsiColor
    }
  | OutputCustom {
      _outputReport    :: Report a
    , _outputAnsiColor :: AnsiColor
    }

instance Contravariant OutputConfig where
  contramap f = \case
    OutputHandle{..} -> OutputHandle{..}
    OutputCustom{..} -> OutputCustom{
        _outputReport = \level -> _outputReport level . f
      , ..
      }

-- | The default tracer configuration
--
-- - writes to 'stderr', and
-- - uses ANSI colors, if available.
instance Default (OutputConfig a) where
  def = OutputHandle stderr Nothing

-- | Output configuration suitable for compile-time code generation with
-- Template Haskell.
--
-- Propagate warnings and errors to GHC.
--
-- Report traces with other log levels to `stdout`.
outputConfigTH :: OutputConfig a
outputConfigTH = OutputCustom report DisableAnsiColor
  where
    report :: Report a
    report level _ = case level of
      -- NOTE: In general, 'runQ' is a bad idea, but it supports 'reportWarning'
      -- and 'reportError'.
      Warning -> runQ . reportWarning
      Error   -> runQ . reportError
      _level  -> putStr

-- | Sometimes, we want to change log levels. For example, we want to suppress
-- specific traces in tests.
--
-- The custom log level function takes a trace and returns a function
-- customizing the log level.
newtype CustomLogLevel l a = CustomLogLevel { unCustomLogLevel :: a -> l -> l }

-- | First apply the left custom log level, then the right one.
instance Semigroup (CustomLogLevel l a) where
  (CustomLogLevel left) <> (CustomLogLevel right) =
    CustomLogLevel $ \trc -> right trc . left trc

instance Monoid (CustomLogLevel l a) where
  mempty = CustomLogLevel $ const id

instance Contravariant (CustomLogLevel l) where
  contramap f (CustomLogLevel g) = CustomLogLevel $ g . f

-- | Configuration of tracer.
data TracerConfig l a = TracerConfig {
    tVerbosity      :: !Verbosity
  , tOutputConfig   :: !(OutputConfig a)
  , tCustomLogLevel :: !(CustomLogLevel l a)
  , tShowTimeStamp  :: !ShowTimeStamp
  , tShowCallStack  :: !ShowCallStack
  }
  deriving (Generic)

instance Contravariant (TracerConfig l) where
  contramap f tracerConfig =
    tracerConfig {
      tOutputConfig   = contramap f $ tOutputConfig   tracerConfig
    , tCustomLogLevel = contramap f $ tCustomLogLevel tracerConfig
    }

instance Default (TracerConfig l a) where
  def = TracerConfig
    { tVerbosity      = def
    , tOutputConfig   = def
    , tCustomLogLevel = mempty
    , tShowTimeStamp  = DisableTimeStamp
    , tShowCallStack  = DisableCallStack
    }

{-------------------------------------------------------------------------------
  Tracers
-------------------------------------------------------------------------------}

-- | Run an action with a tracer.
--
-- The default tracer configuration
--
-- - writes to 'stdout', and
-- - uses ANSI colors, if available.
--
-- Return 'Nothing' if an 'Error' trace was emitted.
withTracer :: forall m a b. (MonadIO m , IsTrace Level a)
  => TracerConfig Level a
  -> (Tracer a -> IORef (TracerState a) -> m b)
  -> m (Either (TraceException a) b)
withTracer tracerConf action = leftOnError <$> (withTracer' tracerConf action)

-- | Internal. The tracer stores the maximum log level of emitted traces and all
-- emitted error traces.
data TracerState a = TracerState {
      tracerMaxLevel :: Level
    , tracerErrors   :: [a]
    }

emptyTracerState :: TracerState a
emptyTracerState = TracerState Debug []

-- | Run an action with a tracer.
--
-- Return the maximum log level of traces.
--
-- We do not export this function from the public interface, but use it in
-- tests.
--
withTracer' :: forall m a b. (MonadIO m, IsTrace Level a)
  => TracerConfig Level a
  -> (Tracer a -> IORef (TracerState a) -> m b)
  -> m (b, TracerState a)
withTracer' TracerConfig{..} action = do
  (report, ansiColor) <- getOutputConfig
  withIORef emptyTracerState $ \ref ->
    action (mkTracer
              tCustomLogLevel
              ref
              tVerbosity
              ansiColor
              tShowCallStack
              tShowTimeStamp
              report)
           ref
  where
    getOutputConfig :: m (Report a, AnsiColor)
    getOutputConfig = case tOutputConfig of
      OutputHandle handle ansiColorSetting -> do
        ansiColor <- case ansiColorSetting of
          Nothing -> getAnsiColor handle
          Just x  -> pure x
        let report _lvl _trace = liftIO . hPutStr handle
        pure (report, ansiColor)
      OutputCustom report ansiColor -> pure (report, ansiColor)

{-------------------------------------------------------------------------------
  Fatal error
-------------------------------------------------------------------------------}

data TraceException a = TraceException [a]
  deriving Show

instance (Show a, Typeable a) => Exception (TraceException a) where
    toException   = hsBindgenExceptionToException
    fromException = hsBindgenExceptionFromException
    -- We only display errors in tests.
    displayException (TraceException _) = "An error happened (see above)"


data FileSystemException = FileAlreadyExistsException FilePath
  deriving Show

instance Exception FileSystemException where
  toException   = hsBindgenExceptionToException
  fromException = hsBindgenExceptionFromException
  displayException (FileAlreadyExistsException path) = unlines
    [ "Output file already exists: " ++ path
    , ""
    , "Use --overwrite-files to allow overwriting existing files, or delete the file manually."
    ]

{-------------------------------------------------------------------------------
  Safe tracer
-------------------------------------------------------------------------------}

-- | Run an action with a safe tracer using a custom output configuration.
--
-- Always returns a result (if the action does not panic).
--
-- See 'SafeLevel'.
withTracerSafe :: forall m a b. (MonadIO m, IsTrace SafeLevel a)
  => TracerConfig SafeLevel a
  -> (Tracer a -> m b)
  -> m b
withTracerSafe tracerConf action =
    fst <$> withTracer' tracerConf' (\t _ -> action' t)
  where
    action' :: Tracer (SafeTrace a) -> m b
    action' = action . contramap SafeTrace

    toCustomLogLevelUnsafe :: CustomLogLevel SafeLevel c -> CustomLogLevel Level c
    toCustomLogLevelUnsafe (CustomLogLevel f) = CustomLogLevel $ \trc lvl ->
      -- NOTE: Only customize safe levels, and do not change unsafe levels.
      -- However, the latter case should never happen!
      case toSafeLevel lvl of
        Just safeLvl -> fromSafeLevel $ f trc safeLvl
        Nothing      -> lvl

    customLogLevel :: CustomLogLevel Level (SafeTrace a)
    customLogLevel =
      toCustomLogLevelUnsafe $
        contramap getSafeTrace $
        tCustomLogLevel tracerConf

    tracerConf' :: TracerConfig Level (SafeTrace a)
    tracerConf' = tracerConf {
        tOutputConfig   = contramap getSafeTrace $ tOutputConfig tracerConf
      , tCustomLogLevel = customLogLevel
      }

newtype SafeTrace a = SafeTrace { getSafeTrace :: a }
  deriving (Show, Eq, Generic)

instance PrettyForTrace a => PrettyForTrace (SafeTrace a) where
  prettyForTrace = prettyForTrace . getSafeTrace

instance IsTrace SafeLevel a => IsTrace Level (SafeTrace a) where
  getDefaultLogLevel = fromSafeLevel . getDefaultLogLevel . getSafeTrace
  getSource          = getSource                          . getSafeTrace
  getTraceId         = getTraceId                         . getSafeTrace

{-------------------------------------------------------------------------------
  Internal helpers
-------------------------------------------------------------------------------}

-- | Create a tracer emitting traces to a provided function @report@.
--
-- The traces provide additional information about
-- - the time,
-- - the log level, and
-- - the source.
mkTracer :: forall a. (IsTrace Level a)
  => CustomLogLevel Level a
  -> IORef (TracerState a)
  -> Verbosity
  -> AnsiColor
  -> ShowCallStack
  -> ShowTimeStamp
  -> Report a
  -> Tracer a
mkTracer
  customLogLevel
  tracerStateRef
  verbosity
  ansiColor
  showCallStack
  showTimeStamp
  report =
    squelchUnless isLogLevelHighEnough $ simpleWithCallStack $ traceAction
  where
    isLogLevelHighEnough :: a -> Bool
    isLogLevelHighEnough trace = getLogLevel trace >= unwrapVerbosity verbosity

    traceAction :: MsgWithCallStack a -> IO ()
    traceAction MsgWithCallStack {..} = do
      liftIO $ modifyIORef' tracerStateRef $
        updateTracerState level msgWithoutCallStack
      msgTrace <- formatTrace ansiColor showTimeStamp level msgWithoutCallStack
      let msgStack = prettyCallStack msgCallStack
          components = msgTrace : [ msgStack | showCallStack == EnableCallStack ]
      report level msgWithoutCallStack $ unlines components
      where
        level :: Level
        level = getLogLevel msgWithoutCallStack

    updateTracerState :: Level -> a -> TracerState a -> TracerState a
    updateTracerState level trace TracerState{..} = case level of
      Error  -> TracerState Error                      (trace:tracerErrors)
      _other -> TracerState (max level tracerMaxLevel) tracerErrors

    getLogLevel :: a -> Level
    getLogLevel x = unCustomLogLevel customLogLevel x (getDefaultLogLevel x)

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
        setColor = setSGRCode [
            SetColor Foreground Vivid color
          , SetConsoleIntensity BoldIntensity
          ]
        resetColor :: String
        resetColor = setSGRCode []

leftOnError :: (b, TracerState a) -> Either (TraceException a) b
leftOnError = \case
  (_, TracerState Error errors) -> Left  (TraceException errors)
  (r, _                  )      -> Right r

-- | Check a TracerState IORef for errors.
--
-- Returns 'Just' a TraceException if errors were emitted (Error level traces),
-- otherwise returns 'Nothing'.
--
-- This is useful for checking for errors after forcing lazy computations that
-- trace to a tracer whose state is captured in the IORef.
--
checkTracerState :: MonadIO m => IORef (TracerState a) -> m (Maybe (TraceException a))
checkTracerState ref = do
  tracerState <- liftIO $ readIORef ref
  pure $ case tracerState of
    TracerState Error errors -> Just (TraceException errors)
    _                        -> Nothing

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

-- Format a trace message.
type Format m a = AnsiColor -> ShowTimeStamp -> Level -> a -> m String

-- Log format:
-- [OPTIONAL TIMESTAMP] [LEVEL] [SOURCE] Message.
--   Indent subsequent lines.
--   OPTION CALL STACK.
formatTrace :: (MonadIO m, IsTrace Level a) => Format m a
formatTrace ansiColor showTimeStamp level trace = do
    mTime <- case showTimeStamp of
      DisableTimeStamp -> pure Nothing
      EnableTimeStamp -> Just <$> liftIO getCurrentTime
    pure $ formatLine mTime $ PP.renderCtxDoc context $ prettyForTrace trace
  where
    context :: Context
    context = PP.mkContext 120

    source :: Source
    source = getSource trace

    traceId :: TraceId
    traceId = getTraceId trace

    showTime :: FormatTime t => t -> String
    showTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%3QZ"

    prependTime :: FormatTime t => t -> String -> String
    prependTime time x = "[" <> showTime time <> "] " <> x

    prependLevel :: String -> String
    prependLevel x =
      withColor ansiColor level ("[" <> alignLevel level <> "]") <> " " <> x

    prependSource :: String -> String
    prependSource x =
      withColor ansiColor level ("[" <> alignSource source <> "]") <> " " <> x

    prependTraceId :: String -> String
    prependTraceId x =
      withColor ansiColor level ("[" <> unTraceId traceId <> "]") <> " " <> x

    prependLabels :: Maybe UTCTime -> String -> String
    prependLabels mTime =
      maybePrependTime . prependLevel . prependSource . prependTraceId
      where maybePrependTime = case mTime of
              Nothing   -> id
              Just time -> prependTime time

    formatLine :: Maybe UTCTime -> String -> String
    formatLine mTime = prependLabels mTime
