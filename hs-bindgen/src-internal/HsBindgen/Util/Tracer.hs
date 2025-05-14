{-# LANGUAGE Arrows #-}

-- | Logging
--
-- Indended for unqualified import.
module HsBindgen.Util.Tracer (
  -- | Data types and typeclasses useful for tracing
    Level (..)
  , AnsiColor (..)
  , PrettyTrace (..)
  , HasLogLevel (..)
  , HasSource (..)
  , Verbosity (..)
  -- | Tracers
  , mkTracer
  , withTracerStdOut
  , withTracerFile
  ) where

import Control.Tracer (Tracer (Tracer), emit, squelchUnless)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.Format (FormatTime)
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
  deriving (Show, Eq, Ord)

data AnsiColor = WithAnsiColor | WithoutAnsiColor

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

-- | Render a string in bold and a specified color.
--
-- Careful, the applied ANSI code suffix also resets all other activated formatting.
withColor :: Color -> String -> String
withColor color x = setColor <> x <> resetColor
  where
    setColor :: String
    setColor = setSGRCode [ SetColor Foreground Vivid color
                          , SetConsoleIntensity BoldIntensity ]

    resetColor :: String
    resetColor = setSGRCode []

withAnsiCodes :: AnsiColor -> Level -> String -> String
withAnsiCodes WithoutAnsiColor _     = id
withAnsiCodes WithAnsiColor    level = withColor (getColorForLevel level)

-- | Convert values to textual representations used in traces.
class PrettyTrace a where
  -- TODO: Use 'Text' (issue #650).
  prettyTrace :: a -> String

-- | Get log level of values used in traces.
class HasLogLevel a where
  getLogLevel :: a -> Level

-- | Get source or context of values used in traces.
class HasSource a where
  getSource :: a -> String

data Verbosity = Verbosity !Level | Quiet
  deriving (Show, Eq)

{-------------------------------------------------------------------------------
  Tracers
-------------------------------------------------------------------------------}

-- | Create a tracer emitting traces to a provided function @report@.
--
-- The traces provide additional information about
-- - the time,
-- - the log level, and
-- - the source.
mkTracer :: (PrettyTrace a, HasLogLevel a, HasSource a)
  => AnsiColor -> Verbosity -> (String -> IO ()) -> Tracer IO a
mkTracer ansiCodes verbosity report =
  squelchUnless isLogLevelHighEnough $ Tracer $ emit prettyReport
  where
    isLogLevelHighEnough :: HasLogLevel a => a -> Bool
    isLogLevelHighEnough x = case verbosity of
      Quiet -> False
      Verbosity v -> getLogLevel x >= v

    showTime :: FormatTime t => t -> String
    showTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%3QZ"

    prependTime :: FormatTime t => t -> String -> String
    prependTime time x = '[' : showTime time <> "] " <> x

    prependLevel :: Level -> String -> String
    prependLevel level x = withAnsiCodes ansiCodes level ('[' : alignLevel level <> "] ") <> x

    prependSource :: String -> String -> String
    prependSource source x = '[' : source <> "] " <> x

    colorLine :: Level -> String -> String
    colorLine level = case ansiCodes of
      WithAnsiColor | level `elem` [Warning, Error] -> withAnsiCodes ansiCodes level
      _otherwise                                    -> id

    formatLine :: UTCTime -> Level -> String -> String -> String
    formatLine time level source = prependTime time
      . prependLevel level . prependSource source . colorLine level

    -- Log format:
    -- [TIMESTAMP] [LEVEL] [SOURCE/CONTEXT] MESSAGE
    -- [2025-05-16 05:45:19.391Z] [Info   ] [Source] Skipped "hs-bindgen-c-example.h" at "hs-bindgen-root.h:1:1": Not from the main file
    prettyReport :: (PrettyTrace a, HasLogLevel a, HasSource a) => a -> IO ()
    prettyReport trace = do
      time <- getCurrentTime
      -- TODO #647: We apply the prefix line-wise. I prefer this behavior, but
      -- do you agree?
      mapM_ (report . formatLine time level source) $ lines $ prettyTrace trace
      where level = getLogLevel trace
            source = getSource trace

-- | Run an action with a tracer writing to 'stdout'. Use ANSI colors, if available.
withTracerStdOut :: (PrettyTrace a, HasLogLevel a, HasSource a)
  => Verbosity -> (Tracer IO a -> IO b) -> IO b
withTracerStdOut verbosity action = do
  supportsAnsiColor <- hSupportsANSIColor stdout
  let ansiColor = if supportsAnsiColor then WithAnsiColor else WithoutAnsiColor
  action $ mkTracer ansiColor verbosity putStrLn

-- | Run an action with a tracer writing to a file. Do not use ANSI colors.
withTracerFile
  :: (PrettyTrace a, HasLogLevel a, HasSource a)
  => FilePath -> Verbosity -> (Tracer IO a -> IO b) -> IO b
withTracerFile file verbosity action = withFile file AppendMode $ \handle ->
  let tracer = mkTracer WithoutAnsiColor verbosity (hPutStrLn handle)
  in action tracer
