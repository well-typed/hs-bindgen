{-# OPTIONS_GHC -Wno-orphans #-}

module Test.HsBindgen.Unit.Tracer (tests) where

import Data.Data (Typeable)
import Data.Either (isLeft)
import Data.IORef (readIORef)
import Data.Proxy (Proxy (Proxy))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?), (@?=))
import Test.Tasty.QuickCheck (Arbitrary (..), CoArbitrary, Fun, Function, Gen,
                              Property, elements, pattern Fn, testProperty,
                              (===))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Util.Tracer

import Test.Common.HsBindgen.Trace.Predicate
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
        , testGroup "semigroup" [
            testProperty "const-log-level-last-wins"   prop_constLevelLastWins
          , testProperty "adapt-log-level-associative" prop_adaptLevelAssociative
          , testProperty "adapt-log-level-unit"        prop_adaptLevelUnit
          ]
        ]
    , testGroup "LeftOnError" [
          testCase "left" $ do
              let tracerConf = quietTracerConfig {
                      verbosity = Verbosity Debug
                    }
              res <- withTracer tracerConf $ \tracer -> do traceWith tracer er
              isLeft res @? "isLeft"
        ]
    , testGroup "withTracePredicate" [
          testCase "ok-debug" $
              withPred defaultTracePredicate $ \tracer ->
                traceWith tracer db
        , testCase "ok-info" $
              withPred defaultTracePredicate $ \tracer ->
                traceWith tracer info
        , testCase "!ok-notice" $
            assertException "Expected TraceExpectationException" proxy $
              withPred defaultTracePredicate $ \tracer ->
                traceWith tracer notice
        , testCase "!ok-warning" $
            assertException "Expected TraceExpectationException" proxy $
              withPred defaultTracePredicate $ \tracer ->
                traceWith tracer wn
        , testCase "!ok-error" $
            assertException "Expected TraceExpectationException" proxy $
              withPred defaultTracePredicate $ \tracer ->
                traceWith tracer er
        , testCase "ok-custom-warning" $
            withPred expectWar $
              \tracer -> do
                traceWith tracer wn
        , testCase "ok-custom-error" $
            withPred expectErr $
              \tracer -> do
                traceWith tracer er
        , testCase "!ok-custom-too-many" $
            assertException "Expected TraceExpectationException" proxy $
              withPred expectWar $
                \tracer -> do
                  traceWith tracer wn
                  traceWith tracer wn
        , testCase "!ok-custom-too-few" $
            assertException "Expected TraceExpectationException" proxy $
              withPred expectWar $
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

    withPred :: (IsTrace Level a, Typeable a, Show a) =>
      TracePredicate a -> (Tracer a -> IO b) -> IO b
    withPred = withTracePredicate noReport

    noReport :: a -> IO ()
    noReport = const $ pure ()


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

instance IsTrace Level TestTrace where
  getDefaultLogLevel = \case
    TestDebug   _ -> Debug
    TestInfo    _ -> Info
    TestNotice  _ -> Notice
    TestWarning _ -> Warning
    TestError   _ -> Error
  getSource  = const HsBindgen
  getTraceId = const "test"

assertMaxLevel :: [TestTrace] -> Level -> Assertion
assertMaxLevel = assertMaxLevelWithCustomLogLevel mempty

alwaysLevel :: Level -> CustomLogLevel Level a
alwaysLevel level = CustomLogLevel $ const . const level

assertMaxLevelWithDegrade :: [TestTrace] -> Level -> Assertion
assertMaxLevelWithDegrade =
  assertMaxLevelWithCustomLogLevel $ alwaysLevel Info

assertMaxLevelWithCustomLogLevel
  :: CustomLogLevel Level TestTrace -> [TestTrace] -> Level -> Assertion
assertMaxLevelWithCustomLogLevel customLogLevel traces expectedLevel = do
  lvl <- testTracerIO customLogLevel traces
  lvl @?= expectedLevel

testTracerIO :: CustomLogLevel Level TestTrace -> [TestTrace] -> IO Level
testTracerIO customLogLevel traces = do
  let tracerConfig :: TracerConfig Level TestTrace
      tracerConfig = quietTracerConfig {
          verbosity      = Verbosity Debug
        , customLogLevel = customLogLevel
        }
  -- NB: Use and test the tracer functionality provided by @hs-bindgen:lib@,
  -- and not by the tests (e.g., 'withTracePredicate').
  (TracerState maxLogLevel) <- withTracerUnsafe tracerConfig $ \tracer ref -> do
    mapM_ (traceWith tracer) traces
    readIORef ref
  pure maxLogLevel

{-------------------------------------------------------------------------------
  Property-based
-------------------------------------------------------------------------------}

instance Arbitrary Level where
  arbitrary = elements [minBound .. maxBound]
instance CoArbitrary Level
instance Function Level

newtype ConstCustomLogLevel = ConstCustomLogLevel {
    unConstCustomLogLevel :: (CustomLogLevel Level TestTrace)
  }
  -- deriving (Semigroup, Monoid) via (CustomLogLevel Level TestTrace)

instance Show ConstCustomLogLevel where
  show (ConstCustomLogLevel (CustomLogLevel f)) =
    "ConstCustomLogLevel: " <> show (f (TestDebug "") Debug)

instance Arbitrary ConstCustomLogLevel where
  arbitrary = do
    lvl <- arbitrary :: Gen Level
    pure $ ConstCustomLogLevel $ CustomLogLevel $ \_ -> const lvl

instance Arbitrary TestTrace where
  arbitrary = do
    c <- elements [TestDebug, TestInfo, TestNotice, TestWarning, TestError]
    s <- arbitrary :: Gen String
    pure $ c s

apply :: IsTrace l a => CustomLogLevel l a -> a -> l
apply (CustomLogLevel f) tr = f tr (getDefaultLogLevel tr)

applys :: IsTrace l a => [CustomLogLevel l a] -> a -> l
applys = apply . mconcat

prop_constLevelLastWins ::
  [ConstCustomLogLevel] -> ConstCustomLogLevel -> TestTrace -> Property
prop_constLevelLastWins xs x tr =
  applys (map unConstCustomLogLevel $ xs ++ [x]) tr
  === apply (unConstCustomLogLevel x) tr

prop_adaptLevelAssociative ::
  Fun Level Level -> Fun Level Level -> Fun Level Level -> TestTrace -> Property
prop_adaptLevelAssociative (Fn f1) (Fn f2) (Fn f3) tr =
  apply (c1 <> (c2 <> c3)) tr === apply ((c1 <> c2) <> c3) tr
  where toCustomLogLevel f = CustomLogLevel $ \_ lvl -> f lvl
        c1 = toCustomLogLevel f1
        c2 = toCustomLogLevel f2
        c3 = toCustomLogLevel f3

prop_adaptLevelUnit ::
  Fun Level Level -> TestTrace -> Property
prop_adaptLevelUnit (Fn f) tr =
  apply (mconcat $ pure c) tr === apply c tr
  where c = CustomLogLevel $ \_ lvl -> f lvl
