module Test.HsBindgen.Unit.Tracer (tests) where

import Data.Either (isLeft)
import Data.Proxy (Proxy (Proxy))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?), (@?=))

import HsBindgen.Lib
import HsBindgen.Util.Tracer

import Text.SimplePrettyPrint qualified as PP

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
              let noOutput :: Applicative m => Report m a
                  noOutput _ _ _ = pure ()
                  tracerConf   = def {
                      tVerbosity    = Verbosity Debug
                    , tOutputConfig = OutputCustom noOutput DisableAnsiColor
                    }
              res <- withTracer tracerConf $ \tracer -> do traceWith tracer er
              isLeft res @? "isLeft"
        ]
    , testGroup "withTracePredicate" [
          testCase "ok-debug" $
              withTracePredicate defaultTracePredicate $ \tracer ->
                traceWith tracer db
        , testCase "ok-info" $
              withTracePredicate defaultTracePredicate $ \tracer ->
                traceWith tracer info
        , testCase "!ok-notice" $
            assertException "Expected TraceExpectationException" proxy $
              withTracePredicate defaultTracePredicate $ \tracer ->
                traceWith tracer notice
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
    notice    = TestNotice  "Notice message."
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
  | TestNotice String
  | TestWarning String
  | TestError String
  deriving stock (Show, Eq)

instance PrettyForTrace TestTrace where
  prettyForTrace = \case
    TestDebug   x -> PP.string x
    TestInfo    x -> PP.string x
    TestNotice  x -> PP.string x
    TestWarning x -> PP.string x
    TestError   x -> PP.string x

instance HasDefaultLogLevel TestTrace where
  getDefaultLogLevel = \case
    TestDebug   _ -> Debug
    TestInfo    _ -> Info
    TestNotice  _ -> Notice
    TestWarning _ -> Warning
    TestError   _ -> Error

instance HasSource TestTrace where
  getSource = const HsBindgen

assertMaxLevel :: [TestTrace] -> Level -> Assertion
assertMaxLevel = assertMaxLevelWithCustomLogLevel mempty

assertMaxLevelWithDegrade :: [TestTrace] -> Level -> Assertion
assertMaxLevelWithDegrade =
  assertMaxLevelWithCustomLogLevel (CustomLogLevel $ const $ Just Info)

assertMaxLevelWithCustomLogLevel
  :: CustomLogLevel Level TestTrace -> [TestTrace] -> Level -> Assertion
assertMaxLevelWithCustomLogLevel customLogLevel traces expectedLevel = do
  lvl <- testTracerIO customLogLevel traces
  lvl @?= expectedLevel

testTracerIO :: CustomLogLevel Level TestTrace -> [TestTrace] -> IO Level
testTracerIO customLogLevel traces = do
  let noOutput :: Applicative m => Report m a
      noOutput _ _ _ = pure ()
      tracerConfig = def {
          tVerbosity      = Verbosity Debug
        , tOutputConfig   = OutputCustom noOutput DisableAnsiColor
        , tCustomLogLevel = customLogLevel
        }
  -- NB: Use and test the tracer functionality provided by @hs-bindgen:lib@,
  -- and not by the tests (e.g., 'withTracePredicate').
  (_, TracerState maxLogLevel _) <- withTracer' tracerConfig $ \tracer -> do
    mapM_ (traceWith tracer) traces
  pure maxLogLevel

