-- | Configuring individual test cases (examples)
--
-- Intended for unqualified import.
module Test.HsBindgen.Golden.TestCase (
    -- * Definition
    TestCase(..)
    -- * Construction
  , defaultTest
    -- ** Successful tests
  , testTrace
  , testTraceSimple
  , testTraceCustom
  , testDiagnostic
    -- ** Failing tests (that is, with hs-bindgen errors and no output)
  , failingTest
  , failingTestSimple
  , failingTestCustom
    -- * Execution
  , testParse
  , testTranslate
    -- ** Low-level
  , getTestConfig
  , getTestExtSpec
  , withTestTracer
  ) where

import Test.Tasty (TestName)

import Clang.HighLevel.Types qualified as Clang
import Clang.Paths
import HsBindgen.BindingSpec (ExternalBindingSpec, PrescriptiveBindingSpec)
import HsBindgen.Clang (ClangMsg(..))
import HsBindgen.Config (Config(..))
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Lib (TraceMsg(..))
import HsBindgen.Pipeline qualified as Pipeline
import HsBindgen.Util.Tracer

import Test.Common.HsBindgen.TracePredicate
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data TestCase = TestCase {
      -- | Name of the test (in the tasty test tree)
      testName :: TestName

      -- | The input header
    , testInput :: CHeaderIncludePath

      -- | Location of the input header, relative to the package root
    , testDir :: FilePath

      -- | Predicate for evaluating the trace messages
    , testTracePredicate :: TracePredicate TraceMsg

      -- | Does this test have any output?
      --
      -- Set this to 'False' for failing tests where we just want to check the
      -- trace messages and nothing else
    , testHasOutput :: Bool

      -- | Tests that require a specific @libclang@ version
      --
      -- If the predicate does not match, the test is skipped entirely.
    , testClangVersion :: Maybe ((Int, Int, Int) -> Bool)

      -- | Modify the default test configuration
    , testOnConfig :: Config -> Config

      -- | Modify the external binding specification
    , testOnExtSpec :: ExternalBindingSpec -> ExternalBindingSpec

      -- | Should we expect rust-bindgen to throw an error on this test?
      --
      -- This is true for all failing test cases, but it is also true for a
      -- handful of successful test cases which use features that aren't
      -- supported by rust-bindgen.
    , testRustBindgenFails :: Bool
    }

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

defaultTest ::
     String --  ^ Filename without the @.h@ extension
  -> TestCase
defaultTest filename = TestCase{
      testName             = filename
    , testInput            = CHeaderQuoteIncludePath $ filename ++ ".h"
    , testDir              = "examples/golden"
    , testTracePredicate   = defaultTracePredicate
    , testHasOutput        = True
    , testClangVersion     = Nothing
    , testOnConfig         = id
    , testOnExtSpec        = id
    , testRustBindgenFails = False
    }

testTrace :: String -> TracePredicate TraceMsg -> TestCase
testTrace filename trace =
    (defaultTest filename){testTracePredicate = trace}

testTraceSimple ::
     String
  -> (TraceMsg -> Maybe (TraceExpectation ()))
  -> TestCase
testTraceSimple filename trace =
    testTrace filename $ singleTracePredicate trace

testTraceCustom ::
     (Ord a, WrongCountMsg a)
  => String
  -> [a]
  -> (TraceMsg -> Maybe (TraceExpectation a))
  -> TestCase
testTraceCustom filename expected trace =
    testTrace filename $ customTracePredicate' expected trace

testDiagnostic ::
     String
  -> (Clang.Diagnostic -> Bool)
  -> TestCase
testDiagnostic filename p =
    testTraceSimple filename $ \case
      TraceClang (ClangDiagnostic x) | p x -> Just $ Expected ()
      _otherwise                           -> Nothing

{-------------------------------------------------------------------------------
  Construction: failing tests (tests with no output)
-------------------------------------------------------------------------------}

failingTest :: String -> TracePredicate TraceMsg -> TestCase
failingTest filename trace =
    (testTrace filename trace){
        testHasOutput = False
      , testDir       = "examples/failing"
      }

failingTestSimple ::
     String
  -> (TraceMsg -> Maybe (TraceExpectation ()))
  -> TestCase
failingTestSimple filename trace =
    failingTest filename $ singleTracePredicate trace

failingTestCustom ::
     (Ord a, WrongCountMsg a)
  => String
  -> [a]
  -> (TraceMsg -> Maybe (TraceExpectation a))
  -> TestCase
failingTestCustom filename expected trace =
    failingTest filename $ customTracePredicate' expected trace

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

getTestConfig :: IO TestResources -> TestCase -> IO Config
getTestConfig testResources TestCase{testOnConfig, testDir} =
    testOnConfig <$> getTestDefaultConfig testResources [testDir]

getTestExtSpec :: IO TestResources -> TestCase -> IO ExternalBindingSpec
getTestExtSpec testResources TestCase{testOnExtSpec} =
    testOnExtSpec <$> getTestDefaultExtSpec testResources

-- | Get prescriptive binding specification for the test
--
-- TODO: We're not testing with prescriptive binding specifications yet.
getTestPSpec :: IO TestResources -> TestCase -> IO PrescriptiveBindingSpec
getTestPSpec _ _ = return Pipeline.emptyBindingSpec

withTestTracer ::
     TestCase
  -> (Tracer IO TraceMsg -> IO b)
  -> IO b
withTestTracer TestCase{testTracePredicate} =
    withTracePredicate testTracePredicate

testParse :: IO TestResources -> TestCase -> IO C.TranslationUnit
testParse testResources test@TestCase{..} = do
    config  <- getTestConfig  testResources test
    extSpec <- getTestExtSpec testResources test
    pSpec   <- getTestPSpec   testResources test

    withTestTracer test $ \tracer ->
      Pipeline.parseCHeaders
        tracer
        config
        extSpec
        pSpec
        [testInput]

testTranslate :: IO TestResources -> TestCase -> IO [Hs.Decl]
testTranslate testResources test@TestCase{..} = do
    config  <- getTestConfig  testResources test
    extSpec <- getTestExtSpec testResources test
    pSpec   <- getTestPSpec   testResources test

    withTestTracer test $ \tracer ->
      Pipeline.translateCHeaders
        "testmodule"
        tracer
        config
        extSpec
        pSpec
        [testInput]
