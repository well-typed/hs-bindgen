module Test.Internal.Tracer
  ( withAnsiColor
  , withTracerTest
  , withTracerTestCustom
  ) where

import Control.Tracer (Tracer)
import Data.Bool (bool)
import HsBindgen.Lib
import System.Console.ANSI (hSupportsANSIColor)
import System.IO (stdout)
import Test.Internal.Trace (degradeKnownTraces)
import Test.Tasty (TestTree, askOption, withResource)
import Test.Tasty.Ingredients.ConsoleReporter (UseColor (..))

withAnsiColor :: (IO AnsiColor -> TestTree) -> TestTree
withAnsiColor k =
  askOption (\(useColor :: UseColor) ->
    withResource (getAnsiColor useColor) doNothing k)
  where doNothing = const $ pure ()

        getAnsiColor :: UseColor -> IO AnsiColor
        getAnsiColor useColor = do
          supportsAnsiColor <- hSupportsANSIColor stdout
          pure $ case useColor of
                  Never -> DisableAnsiColor
                  Always -> EnableAnsiColor
                  Auto -> bool DisableAnsiColor EnableAnsiColor supportsAnsiColor

-- | Tracer to 'stdout'.
--
-- We use @IO AnsiColor@ since Tasty provides resources as IO actions.
withTracerTest :: IO AnsiColor -> (Tracer IO (TraceWithCallStack Trace) -> IO a) -> IO a
withTracerTest = withTracerTestCustom putStrLn

-- | Tracer with custom report function.
withTracerTestCustom
  :: (String -> IO ())
  -> IO AnsiColor
  -> (Tracer IO (TraceWithCallStack Trace) -> IO a)
  -> IO a
withTracerTestCustom report getAnsiColor action = do
  ansiColor <- getAnsiColor
  withTracerCustom ansiColor tracerConf degradeKnownTraces report action
  where tracerConf = defaultTracerConf { tVerbosity = Verbosity Warning }
