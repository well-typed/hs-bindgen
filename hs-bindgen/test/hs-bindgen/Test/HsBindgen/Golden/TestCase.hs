-- | Configuring individual test cases (examples)
--
-- Intended for unqualified import.
module Test.HsBindgen.Golden.TestCase (
    -- * Definition
    TestCase(..)
  , TestRustBindgen(..)
  , testInputInclude
    -- * Construction
  , defaultTest
  , defaultFailingTest
    -- ** Successful tests
  , testTrace
  , testTraceSimple
  , testTraceCustom
  , testDiagnostic
    -- ** Failing tests (that is, with hs-bindgen errors and no output)
  , failingTestTrace
  , failingTestSimple
  , failingTestCustom
    -- * Execution
  , runTestParse
  , runTestTranslate
  , runTestRustBindgen
    -- ** Low-level
  , getTestConfig
  , getTestExtSpec
  , withTestTracer
  ) where

import System.FilePath ((</>))
import Test.Tasty (TestName)

import Clang.HighLevel.Types qualified as Clang
import Clang.Paths
import HsBindgen.BindingSpec (ExternalBindingSpec, PrescriptiveBindingSpec)
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Clang (ClangMsg (..))
import HsBindgen.Config (Config (..))
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Lib (TraceMsg (..))
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
    , testRustBindgen :: TestRustBindgen
    }

data TestRustBindgen =
    -- | Run @rust-bindgen@, and compare against golden test
    RustBindgenRun

    -- | Run @rust-bindgen@, and check that it fails
  | RustBindgenFail

    -- | Do not run @rust-bindgen@
  | RustBindgenIgnore

{-------------------------------------------------------------------------------
  Derived
-------------------------------------------------------------------------------}

testInputInclude :: TestCase -> CHeaderIncludePath
testInputInclude TestCase{testName} =
    CHeaderQuoteIncludePath $ testName ++ ".h"

testInputPath :: TestCase -> FilePath
testInputPath TestCase{testDir, testName} =
    testDir </> testName ++ ".h"

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

defaultTest ::
     String --  ^ Filename without the @.h@ extension
  -> TestCase
defaultTest filename = TestCase{
      testName             = filename
    , testDir              = "examples/golden"
    , testTracePredicate   = defaultTracePredicate
    , testHasOutput        = True
    , testClangVersion     = Nothing
    , testOnConfig         = id
    , testOnExtSpec        = id
    , testRustBindgen      = RustBindgenRun
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
     (Ord b, WrongCountMsg TraceMsg b)
  => String
  -> [b]
  -> (TraceMsg -> Maybe (TraceExpectation b))
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

defaultFailingTest :: String -> TestCase
defaultFailingTest filename = (defaultTest filename){
      testHasOutput = False
    , testDir       = "examples/failing"
    }

failingTestTrace :: String -> TracePredicate TraceMsg -> TestCase
failingTestTrace filename trace =
    (defaultFailingTest filename){testTracePredicate = trace}

failingTestSimple ::
     String
  -> (TraceMsg -> Maybe (TraceExpectation ()))
  -> TestCase
failingTestSimple filename trace =
    failingTestTrace filename $ singleTracePredicate trace

failingTestCustom ::
     (Ord a, WrongCountMsg TraceMsg a)
  => String
  -> [a]
  -> (TraceMsg -> Maybe (TraceExpectation a))
  -> TestCase
failingTestCustom filename expected trace =
    failingTestTrace filename $ customTracePredicate' expected trace

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
getTestPSpec _ _ = return BindingSpec.emptyBindingSpec

withTestTracer ::
     TestCase
  -> (Tracer IO TraceMsg -> IO b)
  -> IO b
withTestTracer TestCase{testTracePredicate} =
    withTracePredicate testTracePredicate

runTestParse :: IO TestResources -> TestCase -> IO C.TranslationUnit
runTestParse testResources test = do
    config  <- getTestConfig  testResources test
    extSpec <- getTestExtSpec testResources test
    pSpec   <- getTestPSpec   testResources test

    withTestTracer test $ \tracer ->
      Pipeline.parseCHeaders
        tracer
        config
        extSpec
        pSpec
        [testInputInclude test]

runTestTranslate :: IO TestResources -> TestCase -> IO [Hs.Decl]
runTestTranslate testResources test = do
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
        [testInputInclude test]

runTestRustBindgen :: IO TestResources -> TestCase -> IO RustBindgenResult
runTestRustBindgen testResources test = do
    config <- getTestConfig testResources test
    callRustBindgen
      testResources
      (configClangArgs config)
      (testInputPath test)

