module Test.HsBindgen.Unit.Tracer (tests) where

import Data.Proxy (Proxy (Proxy))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import HsBindgen.Lib
import HsBindgen.Util.Tracer (withTracerCustom')
import Text.SimplePrettyPrint (string)

import Test.Common.HsBindgen.TracePredicate
import Test.Common.Util.Tasty

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.HsBindgen.Unit.Tracer" [
      testGroup "DefaultLogLevel" [
          testCase "none"    $ assertMaxLevel [] Debug
        , testCase "warning" $ assertMaxLevel [wn] Warning
        , testCase "error"   $ assertMaxLevel [er] Error
        , testCase "error1"  $ assertMaxLevel [wn, er] Error
        , testCase "error2"  $ assertMaxLevel [wn, er, wn] Error
        , testCase "error3"  $ assertMaxLevel [er, wn] Error
        ]
    , testGroup "CustomLogLevel" [
          testCase "none"     $ assertMaxLevelWithDegrade [] Debug
        , testCase "warning"  $ assertMaxLevelWithDegrade [wn] Info
        , testCase "warning1" $ assertMaxLevelWithDegrade [db, wn] Info
        , testCase "warning2" $ assertMaxLevelWithDegrade [wn, db] Info
        , testCase "warning3" $ assertMaxLevelWithDegrade [db, wn, db] Info
        , testCase "error"    $ assertMaxLevelWithDegrade [er] Info
        , testCase "error1"   $ assertMaxLevelWithDegrade [wn, er] Info
        , testCase "error2"   $ assertMaxLevelWithDegrade [wn, er, wn] Info
        , testCase "error3"   $ assertMaxLevelWithDegrade [er, wn] Info
        ]
    , testGroup "LeftOnError" [
          testCase "left" $ do
              let noOutput _ = pure ()
                  tracerConf = def { tVerbosity = Verbosity Debug }
                  withTracer = withTracerCustom DisableAnsiColor tracerConf DefaultLogLevel noOutput
              res <- withTracer $ \tracer -> do traceWith tracer er
              res @?= Nothing
        ]
    , testGroup "withTracePredicate" [
          testCase "ok-debug" $
              withTracePredicate defaultTracePredicate $ \tracer ->
                traceWith tracer db
        , testCase "ok-info" $
              withTracePredicate defaultTracePredicate $ \tracer ->
                traceWith tracer info
        , testCase "!ok-warning" $
            assertException "Expected TraceExpectationException" proxy $
              withTracePredicate defaultTracePredicate $ \tracer ->
                traceWith tracer wn
        , testCase "!ok-error" $
            assertException "Expected TraceExpectationException" proxy $
              withTracePredicate defaultTracePredicate $ \tracer ->
                traceWith tracer er
        , testCase "ok-custom-warning" $
            withTracePredicate expectWar $
              \tracer -> do
                traceWith tracer wn
        , testCase "ok-custom-error" $
            withTracePredicate expectErr $
              \tracer -> do
                traceWith tracer er
        , testCase "!ok-custom-too-many" $
            assertException "Expected TraceExpectationException" proxy $
              withTracePredicate expectWar $
                \tracer -> do
                  traceWith tracer wn
                  traceWith tracer wn
        , testCase "!ok-custom-too-few" $
            assertException "Expected TraceExpectationException" proxy $
              withTracePredicate expectWar $
                \tracer -> do
                  traceWith tracer db
        ]
    ]
  where
    db        = TestDebug   "Debug message."
    info      = TestInfo    "Info message."
    wn        = TestWarning "Warning!"
    er        = TestError   "Error!"
    proxy     = Proxy :: Proxy (TraceExpectationException TestTrace)
    expectWar = singleTracePredicate $ \case
      TestWarning _ -> Just $ Expected ()
      _otherTrace   -> Nothing
    expectErr = singleTracePredicate $ \case
      TestError _   -> Just $ Expected ()
      _otherTrace   -> Nothing

{-------------------------------------------------------------------------------
  Internal: infrastructure for generating test traces
-------------------------------------------------------------------------------}

data TestTrace =
    TestDebug String
  | TestInfo String
  | TestWarning String
  | TestError String

instance PrettyForTrace TestTrace where
  prettyForTrace = \case
    TestDebug x   -> string x
    TestInfo  x   -> string x
    TestWarning x -> string x
    TestError x   -> string x

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
      tracerConf = def { tVerbosity = Verbosity Debug }
      -- NB: Use and test the tracer functionality provided by @hs-bindgen:lib@,
      -- and not by the tests (e.g., 'withTracePredicate').
      withTracer = withTracerCustom' DisableAnsiColor tracerConf customLogLevel noOutput
  (_, maxLogLevel) <- withTracer $ \tracer -> do
    mapM_ (traceWith tracer) traces
  pure maxLogLevel

