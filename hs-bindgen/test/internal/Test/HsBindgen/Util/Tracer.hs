module Test.HsBindgen.Util.Tracer
  ( tests
  ) where

import GHC.Stack (callStack)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import HsBindgen.Lib

data TestTrace = TestDebug String | TestWarning String | TestError String

instance PrettyTrace TestTrace where
  prettyTrace = \case
    TestDebug x   -> x
    TestWarning x -> x
    TestError x   -> x

instance HasDefaultLogLevel TestTrace where
  getDefaultLogLevel = \case
    TestDebug   _ -> Debug
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
      withTracer = withTracerCustom DisableAnsiColor tracerConf customLogLevel noOutput
  (_, maxLogLevel) <- withTracer $ \tracer -> do
    mapM_ (traceWithCallStack tracer callStack) traces
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
  ]
  where db = TestDebug   "Debug message."
        wn = TestWarning "Be careful!"
        er = TestError   "Error!"
