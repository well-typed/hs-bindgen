module Test.HsBindgen.Util.Tracer
  ( tests
  ) where

import Data.Proxy (Proxy (Proxy))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import HsBindgen.Lib

import Test.Internal.Tasty (assertException)
import Test.Internal.Tracer (TraceExpectation (Expected),
                             TraceExpectationException, customTracePredicate,
                             defaultTracePredicate, withTracePredicate)

data TestTrace = TestDebug String | TestInfo String | TestWarning String | TestError String

instance PrettyTrace TestTrace where
  prettyTrace = \case
    TestDebug x   -> x
    TestInfo  x   -> x
    TestWarning x -> x
    TestError x   -> x

instance HasDefaultLogLevel TestTrace where
  getDefaultLogLevel = \case
    TestDebug   _ -> Debug
    TestInfo    _ -> Info
    TestWarning _ -> Warning
    TestError   _ -> Error

instance HasSource TestTrace where
  getSource = const HsBindgen

assertMaxLevel :: [TestTrace] -> Level -> Assertion
assertMaxLevel = assertMaxLevelWithCustomLogLevel DefaultLogLevel

assertMaxLevelWithDegrade :: [TestTrace] -> Level -> Assertion
assertMaxLevelWithDegrade =
  assertMaxLevelWithCustomLogLevel (CustomLogLevel $ const Info)

assertMaxLevelWithCustomLogLevel
  :: CustomLogLevel TestTrace -> [TestTrace] -> Level -> Assertion
assertMaxLevelWithCustomLogLevel customLogLevel traces expectedLevel = do
  lvl <- testTracerIO customLogLevel traces
  lvl @?= expectedLevel

testTracerIO :: CustomLogLevel TestTrace -> [TestTrace] -> IO Level
testTracerIO customLogLevel traces = do
  let noOutput _ = pure ()
      tracerConf = defaultTracerConf { tVerbosity = Verbosity Debug }
      -- NB: Use and test the tracer functionality provided by @hs-bindgen:lib@,
      -- and not by the tests (e.g., 'withTracePredicate').
      withTracer = withTracerCustom' DisableAnsiColor tracerConf customLogLevel noOutput
  (_, maxLogLevel) <- withTracer $ \tracer -> do
    mapM_ (traceWithCallStack tracer) traces
  pure maxLogLevel

tests :: TestTree
tests = testGroup "HsBindgen.Util.Tracer"
  [ testGroup "DefaultLogLevel"
    [ testCase "none"    $ assertMaxLevel [] Debug
    , testCase "warning" $ assertMaxLevel [wn] Warning
    , testCase "error"   $ assertMaxLevel [er] Error
    , testCase "error1"  $ assertMaxLevel [wn, er] Error
    , testCase "error2"  $ assertMaxLevel [wn, er, wn] Error
    , testCase "error3"  $ assertMaxLevel [er, wn] Error
    ]
  , testGroup "CustomLogLevel"
    [ testCase "none"     $ assertMaxLevelWithDegrade [] Debug
    , testCase "warning"  $ assertMaxLevelWithDegrade [wn] Info
    , testCase "warning1" $ assertMaxLevelWithDegrade [db, wn] Info
    , testCase "warning2" $ assertMaxLevelWithDegrade [wn, db] Info
    , testCase "warning3" $ assertMaxLevelWithDegrade [db, wn, db] Info
    , testCase "error"    $ assertMaxLevelWithDegrade [er] Info
    , testCase "error1"   $ assertMaxLevelWithDegrade [wn, er] Info
    , testCase "error2"   $ assertMaxLevelWithDegrade [wn, er, wn] Info
    , testCase "error3"   $ assertMaxLevelWithDegrade [er, wn] Info
    ]
  , testGroup "ExceptionOnError"
    [ testCase "exception" $
        assertException "Expected ErrorTraceException" (Proxy :: Proxy ErrorTraceException) $ do
          let noOutput _ = pure ()
              tracerConf = defaultTracerConf { tVerbosity = Verbosity Debug }
              withTracer = withTracerCustom DisableAnsiColor tracerConf DefaultLogLevel noOutput
          withTracer $ \tracer -> do
            traceWithCallStack tracer er
    ]
  , testGroup "withTracePredicate"
    [ testCase "ok-debug" $
          withTracePredicate defaultTracePredicate $ \tracer ->
            traceWithCallStack tracer db
    , testCase "ok-info" $
          withTracePredicate defaultTracePredicate $ \tracer ->
            traceWithCallStack tracer info
    , testCase "!ok-warning" $
        assertException "Expected TraceExpectationException" proxy $
          withTracePredicate defaultTracePredicate $ \tracer ->
            traceWithCallStack tracer wn
    , testCase "!ok-error" $
        assertException "Expected TraceExpectationException" proxy $
          withTracePredicate defaultTracePredicate $ \tracer ->
            traceWithCallStack tracer er
    , testCase "ok-custom-warning" $
        withTracePredicate (customTracePredicate ["Warning"] expectWar) $
          \tracer -> do
            traceWithCallStack tracer wn
    , testCase "ok-custom-error" $
        withTracePredicate (customTracePredicate ["Error"] expectErr) $
          \tracer -> do
            traceWithCallStack tracer er
    , testCase "!ok-custom-too-many" $
        assertException "Expected TraceExpectationException" proxy $
          withTracePredicate (customTracePredicate ["Warning"] expectWar) $
            \tracer -> do
              traceWithCallStack tracer wn
              traceWithCallStack tracer wn
    , testCase "!ok-custom-too-few" $
        assertException "Expected TraceExpectationException" proxy $
          withTracePredicate (customTracePredicate ["Warning"] expectWar) $
            \tracer -> do
              traceWithCallStack tracer db
    ]
  ]
  where db        = TestDebug   "Debug message."
        info      = TestInfo    "Info message."
        wn        = TestWarning "Warning!"
        er        = TestError   "Error!"
        proxy     = Proxy :: Proxy (TraceExpectationException TestTrace)
        expectWar = \case
          TestWarning _   -> Just (Expected "Warning")
          _otherTrace     -> Nothing
        expectErr = \case
          TestError _   -> Just (Expected "Error")
          _otherTrace   -> Nothing
