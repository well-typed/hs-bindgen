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
  , OutputHandle (..)
  , OutputCustom (..)
  , outputConfigTH
  , CustomLogLevel (..)
  , applyCustomLogLevel
  , TracerConfig (..)
    -- * Tracers
  , withTracer
  , TracerState(..)
  , checkTracerState
  , AnErrorHappened(..)
    -- * Safe tracers
  , withTracerSafe
  , SafeTrace(..)
    -- * Re-exports
  , Contravariant(..)
    -- * Test infrastructure
  , withTracerUnsafe
  ) where

import Control.Tracer (Contravariant (..))
import Control.Tracer qualified as ContraTracer
import Data.Bool (bool)
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

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition and main API

  The definition of 'Tracer' is opaque.
-------------------------------------------------------------------------------}

newtype Tracer e = Wrap (ContraTracer.Tracer IO (MsgWithCallStack e))

unwrap :: Tracer e -> ContraTracer.Tracer IO (MsgWithCallStack e)
unwrap (Wrap tracer) = tracer

-- | We pair every trace message with a callstack for easier debugging
--
-- This is an internal type.
data MsgWithCallStack e = MsgWithCallStack {
      callStack :: CallStack
    , traceMsg  :: e
    }
  deriving stock (Show, Functor)

instance Contravariant Tracer where
  contramap f = Wrap . contramap (fmap f) . unwrap

traceWith :: (MonadIO m, HasCallStack) => Tracer e -> e -> m ()
traceWith tracer =
      liftIO
    . ContraTracer.traceWith (unwrap tracer)
    . MsgWithCallStack callStack

-- | Simple tracer that 'ContraTracer.emit's every message
simpleTracer :: (e -> IO ()) -> Tracer e
simpleTracer f = simpleWithCallStack (f . (.traceMsg))

-- | Generalization of 'simpleWithCallStack'
--
-- This is internal API.
simpleWithCallStack :: (MsgWithCallStack e -> IO ()) -> Tracer e
simpleWithCallStack =
      Wrap
    . ContraTracer.Tracer
    . ContraTracer.emit

-- | See 'ContraTracer.squelchUnless'
squelchUnless :: (e -> Bool) -> Tracer e -> Tracer e
squelchUnless p =
      Wrap
    . ContraTracer.squelchUnless (p . (.traceMsg))
    . unwrap

nullTracer :: Tracer e
nullTracer = Wrap ContraTracer.nullTracer

{-------------------------------------------------------------------------------
  Data types and type classes useful for tracing
-------------------------------------------------------------------------------}

-- | Convert values to textual representations used in traces.
class PrettyForTrace e where
  prettyForTrace :: e -> CtxDoc
  default prettyForTrace :: (Generic e, GPrettyForTrace (Rep e)) => e -> CtxDoc
  prettyForTrace = gPrettyForTrace'

class GPrettyForTrace (r :: Type -> Type) where
  gPrettyForTrace :: r x -> CtxDoc

instance GPrettyForTrace r => GPrettyForTrace (M1 tag meta r) where
  gPrettyForTrace (M1 x) = gPrettyForTrace x

instance (GPrettyForTrace r1, GPrettyForTrace r2) => GPrettyForTrace (r1 :+: r2) where
  gPrettyForTrace (L1 x) = gPrettyForTrace x
  gPrettyForTrace (R1 x) = gPrettyForTrace x

instance PrettyForTrace e => GPrettyForTrace (K1 tag e) where
  gPrettyForTrace (K1 x) = prettyForTrace x

gPrettyForTrace' :: (GHC.Generic e, GPrettyForTrace (GHC.Rep e)) => e -> CtxDoc
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

newtype TraceId = TraceId { id :: String }
  deriving stock (Show, Eq, Ord)
  deriving (IsString, Semigroup, Monoid) via String

class PrettyForTrace e => IsTrace l e | e -> l where
  -- | Get default (or suggested) log level of a trace.
  getDefaultLogLevel :: e -> l
  default getDefaultLogLevel :: (Generic e, GIsTrace l (Rep e)) => e -> l
  getDefaultLogLevel = gGetDefaultLogLevel'
  -- | Get source or context of trace.
  getSource :: e -> Source
  default getSource :: (Generic e, GIsTrace l (Rep e)) => e -> Source
  getSource = gGetSource'
  -- | The trace identifier does not necessarily have to be unique. For example,
  -- an non-unique identifier may be used to collect multiple traces in a single
  -- category.
  --
  -- The trace identifier usually starts with a letter and contains only letters
  -- and dashes (e.g., @my-trace-id@). A good default is the constructor name in
  -- kebab-case, leaving out potential "-trace" or "-msg" suffixes.
  getTraceId :: e -> TraceId
  default getTraceId :: (Generic e, GIsTrace l (Rep e)) => e -> TraceId
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

gGetDefaultLogLevel' :: (GHC.Generic e, GIsTrace l (GHC.Rep e)) => e -> l
gGetDefaultLogLevel' = gGetDefaultLogLevel . GHC.from

gGetSource' :: (GHC.Generic e, GIsTrace l (GHC.Rep e)) => e -> Source
gGetSource' = gGetSource . GHC.from

gGetTraceId' :: (GHC.Generic e, GIsTrace l (GHC.Rep e)) => e -> TraceId
gGetTraceId' = gGetTraceId . GHC.from

newtype Verbosity = Verbosity { level :: Level }
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
type Report e = Level -> e -> String -> IO ()

data OutputConfig e =
    OutputConfigHandle OutputHandle
  | OutputConfigCustom (OutputCustom e)

data OutputHandle = OutputHandle{
      handle :: Handle

      -- | ANSI color support
      --
      -- 'Nothing': Automatically determine ANSI color support by examining the
      -- 'Handle'; see 'getAnsiColor'.
    , ansiColor :: Maybe AnsiColor
    }

data OutputCustom e = OutputCustom{
      report    :: Report e
    , ansiColor :: AnsiColor
    }

instance Contravariant OutputConfig where
  contramap f = \case
      OutputConfigHandle handle -> OutputConfigHandle handle
      OutputConfigCustom custom -> OutputConfigCustom (contramap f custom)

instance Contravariant OutputCustom where
  contramap f outputCustom = outputCustom{
        report = \level -> outputCustom.report level . f
      }

-- | The default tracer configuration
--
-- - writes to 'stderr', and
-- - uses ANSI colors, if available.
instance Default (OutputConfig e) where
  def = OutputConfigHandle def

instance Default OutputHandle where
  def = OutputHandle{
        handle    = stderr
      , ansiColor = Nothing
      }

-- | Output configuration suitable for compile-time code generation with
-- Template Haskell.
--
-- Propagate warnings and errors to GHC.
--
-- Report traces with other log levels to `stdout`.
outputConfigTH :: OutputConfig e
outputConfigTH = OutputConfigCustom OutputCustom{
      report
    , ansiColor = DisableAnsiColor
    }
  where
    report :: Report e
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
newtype CustomLogLevel l e = CustomLogLevel (e -> l -> l)

applyCustomLogLevel :: CustomLogLevel l e -> e -> l -> l
applyCustomLogLevel (CustomLogLevel f) = f

-- | First apply the left custom log level, then the right one.
instance Semigroup (CustomLogLevel l e) where
  (CustomLogLevel left) <> (CustomLogLevel right) =
    CustomLogLevel $ \trc -> right trc . left trc

instance Monoid (CustomLogLevel l e) where
  mempty = CustomLogLevel $ const id

instance Contravariant (CustomLogLevel l) where
  contramap f (CustomLogLevel g) = CustomLogLevel $ g . f

-- | Configuration of tracer.
data TracerConfig l e = TracerConfig{
      verbosity      :: Verbosity
    , outputConfig   :: OutputConfig e
    , customLogLevel :: CustomLogLevel l e
    , showTimeStamp  :: ShowTimeStamp
    , showCallStack  :: ShowCallStack
    }
  deriving (Generic)

instance Contravariant (TracerConfig l) where
  contramap f config = config {
        outputConfig   = contramap f config.outputConfig
      , customLogLevel = contramap f config.customLogLevel
      }

instance Default (TracerConfig l e) where
  def = TracerConfig{
        verbosity      = def
      , outputConfig   = def
      , customLogLevel = mempty
      , showTimeStamp  = DisableTimeStamp
      , showCallStack  = DisableCallStack
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
withTracer :: forall m e a. (MonadIO m , IsTrace Level e)
  => TracerConfig Level e
  -> (Tracer e -> m a)
  -> m (Either AnErrorHappened a)
withTracer tracerConf action = withTracerUnsafe tracerConf action'
  where
    action' :: Tracer e -> IORef TracerState -> m (Either AnErrorHappened a)
    action' tracer ref = do
      r <- action tracer
      s <- liftIO $ readIORef ref
      case s of
        TracerState Error -> pure $ Left AnErrorHappened
        _                 -> pure $ Right r

-- | Internal. The tracer stores the maximum log level of emitted traces and all
-- emitted error traces.
data TracerState = TracerState {
      tracerMaxLevel :: Level
    }

defTracerState :: TracerState
defTracerState = TracerState Debug

-- | Run an action with a tracer.
--
-- The caller is responsible for checking for errors in the tracer state.
--
-- Used in tests.
withTracerUnsafe :: forall m e a. (MonadIO m, IsTrace Level e)
  => TracerConfig Level e
  -> (Tracer e -> IORef TracerState -> m a)
  -> m a
withTracerUnsafe config action = do
  (report, ansiColor) <- getOutputConfig
  fmap fst $ withIORef defTracerState $ \ref ->
    action (mkTracer
              config.customLogLevel
              ref
              config.verbosity
              ansiColor
              config.showCallStack
              config.showTimeStamp
              report)
           ref
  where
    getOutputConfig :: m (Report e, AnsiColor)
    getOutputConfig = case config.outputConfig of
        OutputConfigHandle outputHandle -> do
          ansiColor <- case outputHandle.ansiColor of
            Nothing -> getAnsiColor outputHandle.handle
            Just x  -> pure x
          let report _lvl _trace = liftIO . hPutStr outputHandle.handle
          pure (report, ansiColor)
        OutputConfigCustom outputCustom ->
          pure (outputCustom.report, outputCustom.ansiColor)

{-------------------------------------------------------------------------------
  Trace error
-------------------------------------------------------------------------------}

-- | Evidence that an error has happened.
--
-- We do not store/report the actual errors, because we have emitted them
-- previously.
data AnErrorHappened = AnErrorHappened
    deriving stock (Show)

instance PrettyForTrace AnErrorHappened where
  prettyForTrace AnErrorHappened = "An error happened (see above)"

{-------------------------------------------------------------------------------
  Safe tracer
-------------------------------------------------------------------------------}

-- | Run an action with a safe tracer using a custom output configuration.
--
-- Always returns a result (if the action does not panic).
--
-- See 'SafeLevel'.
withTracerSafe :: forall m e a. (MonadIO m, IsTrace SafeLevel e)
  => TracerConfig SafeLevel e
  -> (Tracer e -> m a)
  -> m a
withTracerSafe config action =
    withTracerUnsafe tracerConf' (\t _ -> action' t)
  where
    action' :: Tracer (SafeTrace e) -> m a
    action' = action . contramap SafeTrace

    toCustomLogLevelUnsafe :: CustomLogLevel SafeLevel c -> CustomLogLevel Level c
    toCustomLogLevelUnsafe (CustomLogLevel f) = CustomLogLevel $ \trc lvl ->
      -- NOTE: Only customize safe levels, and do not change unsafe levels.
      -- However, the latter case should never happen!
      case toSafeLevel lvl of
        Just safeLvl -> fromSafeLevel $ f trc safeLvl
        Nothing      -> lvl

    customLogLevel :: CustomLogLevel Level (SafeTrace e)
    customLogLevel =
      toCustomLogLevelUnsafe $
        contramap (.trace) config.customLogLevel

    tracerConf' :: TracerConfig Level (SafeTrace e)
    tracerConf' = config {
        outputConfig   = contramap (.trace) config.outputConfig
      , customLogLevel = customLogLevel
      }

newtype SafeTrace e = SafeTrace { trace :: e }
  deriving (Show, Eq, Generic)

instance PrettyForTrace e => PrettyForTrace (SafeTrace e) where
  prettyForTrace = prettyForTrace . (.trace)

instance IsTrace SafeLevel e => IsTrace Level (SafeTrace e) where
  getDefaultLogLevel = fromSafeLevel . getDefaultLogLevel . (.trace)
  getSource          = getSource                          . (.trace)
  getTraceId         = getTraceId                         . (.trace)

{-------------------------------------------------------------------------------
  Internal helpers
-------------------------------------------------------------------------------}

-- | Create a tracer emitting traces to a provided function @report@.
--
-- The traces provide additional information about
-- - the time,
-- - the log level, and
-- - the source.
mkTracer :: forall e. (IsTrace Level e)
  => CustomLogLevel Level e
  -> IORef TracerState
  -> Verbosity
  -> AnsiColor
  -> ShowCallStack
  -> ShowTimeStamp
  -> Report e
  -> Tracer e
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
    isLogLevelHighEnough :: e -> Bool
    isLogLevelHighEnough trace = getLogLevel trace >= verbosity.level

    traceAction :: MsgWithCallStack e -> IO ()
    traceAction msg = do
      liftIO $ modifyIORef' tracerStateRef $ updateTracerState level
      msgTrace <- formatTrace ansiColor showTimeStamp level msg.traceMsg
      let msgStack = prettyCallStack msg.callStack
          components = msgTrace : [ msgStack | showCallStack == EnableCallStack ]
      report level msg.traceMsg $ unlines components
      where
        level :: Level
        level = getLogLevel msg.traceMsg

    updateTracerState :: Level -> TracerState -> TracerState
    updateTracerState level (TracerState maxLevel) =
      TracerState $ max level maxLevel

    getLogLevel :: e -> Level
    getLogLevel x = applyCustomLogLevel customLogLevel x (getDefaultLogLevel x)

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

-- | Check a TracerState IORef for errors.
--
-- This is useful for checking for errors after forcing lazy computations that
-- trace to a tracer whose state is captured in the IORef.
checkTracerState :: MonadIO m => IORef TracerState -> m (Maybe AnErrorHappened)
checkTracerState ref = do
  tracerState <- liftIO $ readIORef ref
  pure $ case tracerState of
    TracerState Error -> Just AnErrorHappened
    _                 -> Nothing

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
type Format m e = AnsiColor -> ShowTimeStamp -> Level -> e -> m String

-- Log format:
-- [OPTIONAL TIMESTAMP] [LEVEL] [SOURCE] Message.
--   Indent subsequent lines.
--   OPTION CALL STACK.
formatTrace :: (MonadIO m, IsTrace Level e) => Format m e
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
      withColor ansiColor level ("[" <> traceId.id <> "]") <> " " <> x

    prependLabels :: Maybe UTCTime -> String -> String
    prependLabels mTime =
      maybePrependTime . prependLevel . prependSource . prependTraceId
      where maybePrependTime = case mTime of
              Nothing   -> id
              Just time -> prependTime time

    formatLine :: Maybe UTCTime -> String -> String
    formatLine mTime = prependLabels mTime
