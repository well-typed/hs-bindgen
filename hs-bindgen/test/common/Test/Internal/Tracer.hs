{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Internal.Tracer
  ( withAnsiColor
  , withTracerTest
  , withTracerTestCustom
    -- Writer
  , withWriterTracer
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Tracer (Tracer (..), emit)
import Data.Bool (bool)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
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

-- | Run an action with a tracer that collects all trace messages.
--
-- The last trace will be the first in the list.
--
-- This would be much nicer using the 'Writer' monad, but we fix the monad to
-- 'IO' in many cases.
--
-- > mkWriterTracer :: MonadWriter [a] m => Tracer m a
-- > mkWriterTracer = Tracer $ emit (tell . singleton)
--
-- > withWriterTracer :: (MonadWriter [a] m1, Monad m2) => (Tracer m1 a -> m2 b) -> m2 (b, [a])
-- > withWriterTracer action = runWriterT (WriterT $ (, []) <$> (action mkWriterTracer))
withWriterTracer :: forall m a b. MonadIO m => (Tracer m a -> m b) -> m (b, [a])
withWriterTracer action = do
  tracesRef <- liftIO $ newIORef []
  actionRes <- action $ mkWriterTracer tracesRef
  traces <- liftIO $ readIORef tracesRef
  pure (actionRes, traces)

mkWriterTracer :: MonadIO m => IORef [a] -> Tracer m a
mkWriterTracer tracesRef = Tracer $ emit addTrace
  where addTrace trace = liftIO $ modifyIORef' tracesRef (\xs -> trace : xs)
