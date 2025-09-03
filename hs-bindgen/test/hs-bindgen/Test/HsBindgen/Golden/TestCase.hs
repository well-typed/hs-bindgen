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
  , runTestHsBindgen
  , runTestHsBindgen'
  , runTestRustBindgen
  ) where

import Control.Exception (Exception (..), SomeException (..), handle)
import System.FilePath ((</>))
import Test.Tasty (TestName)

import Clang.HighLevel.Types qualified as Clang
import HsBindgen
import HsBindgen.BindingSpec
import HsBindgen.Clang.BuiltinIncDir
import HsBindgen.Config
import HsBindgen.Frontend
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

import Test.Common.HsBindgen.Trace (reportTrace)
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
    , testOnBootConfig :: BootConfig -> BootConfig

      -- | Modify the default test configuration
    , testOnFrontendConfig :: FrontendConfig -> FrontendConfig

      -- | Configure if the @stdlib@ binding specification should be used
    , testStdlibSpec :: EnableStdlibBindingSpec

      -- | Modify the default binding specification configuration
    , testExtBindingSpecs :: [FilePath]

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

testInputInclude :: TestCase -> UncheckedHashIncludeArg
testInputInclude TestCase{testName} = testName ++ ".h"

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
      testName                = filename
    , testDir                 = "examples/golden"
    , testTracePredicate      = defaultTracePredicate
    , testHasOutput           = True
    , testClangVersion        = Nothing
    , testOnBootConfig        = id
    , testOnFrontendConfig    = id
    , testStdlibSpec          = EnableStdlibBindingSpec
    , testExtBindingSpecs     = []
    , testRustBindgen         = RustBindgenRun
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
      TraceFrontend (FrontendClang (ClangDiagnostic x))
        | p x -> Just $ Expected ()
      _otherwise -> Nothing

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

getTestBootConfig :: IO TestResources -> TestCase -> IO BootConfig
getTestBootConfig testResources TestCase{..} = do
    root <- getTestPackageRoot testResources
    clangArgs <- getTestDefaultClangArgs testResources [testDir]
    return $ testOnBootConfig BootConfig {
        bootBuiltinIncDirConfig = BuiltinIncDirDisable
      , bootClangArgs = clangArgs
      , bootBindingSpecConfig = BindingSpecConfig {
            bindingSpecStdlibSpec = testStdlibSpec
          , bindingSpecExtBindingSpecs = map (root </>) testExtBindingSpecs
          , bindingSpecPrescriptiveBindingSpec = Nothing
          }
      }

getTestFrontendConfig :: TestCase -> FrontendConfig
getTestFrontendConfig TestCase{..} = testOnFrontendConfig def

getTestBackendConfig :: TestCase -> BackendConfig
getTestBackendConfig TestCase{..} = getTestDefaultBackendConfig testName

withTestTraceConfig ::
     TestCase
  -> (TracerConfig IO Level TraceMsg -> IO b)
  -> IO b
withTestTraceConfig TestCase{testTracePredicate} =
    withTraceConfigPredicate testTracePredicate

-- | Run 'hsBindgen'.
--
-- On 'TraceException's, print error traces.
runTestHsBindgen :: IO TestResources -> TestCase -> Artefacts as -> IO (NP I as)
runTestHsBindgen testResources test artefacts =
    handle exceptionHandler $ runTestHsBindgen' testResources test artefacts
  where
    exceptionHandler :: SomeException -> IO a
    exceptionHandler e@(SomeException e')
      | Just (TraceException @TraceMsg es) <- fromException e =
          mapM_  printTrace es >> throwIO e'
      | otherwise = throwIO e'
    printTrace = print . reportTrace

-- | Like 'runTestHsBindgen', but do not print error traces.
runTestHsBindgen' :: IO TestResources -> TestCase -> Artefacts as -> IO (NP I as)
runTestHsBindgen' testResources test artefacts = do
    bootConfig <- getTestBootConfig testResources test
    let frontendConfig = getTestFrontendConfig test
        backendConfig  = getTestBackendConfig test
        bindgenConfig  = BindgenConfig bootConfig frontendConfig backendConfig
    withTestTraceConfig test $ \traceConfig ->
      hsBindgen
        traceConfig
        bindgenConfig
        [testInputInclude test]
        artefacts

runTestRustBindgen :: IO TestResources -> TestCase -> IO RustBindgenResult
runTestRustBindgen testResources test = do
    bootConfig <- getTestBootConfig testResources test
    callRustBindgen
      testResources
      (bootClangArgs bootConfig)
      (testInputPath test)
