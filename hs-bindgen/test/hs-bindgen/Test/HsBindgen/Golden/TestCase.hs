-- | Configuring individual test cases (examples)
--
-- Intended for unqualified import.
module Test.HsBindgen.Golden.TestCase (
    -- * Definition
    TestCase(..)
  , testInputInclude
    -- * Construction
  , defaultTest
  , defaultFailingTest
    -- ** Successful tests
  , testVariant
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
  ) where

import Control.Exception (Exception (..), SomeException (..), handle)
import System.FilePath
import Test.Common.HsBindgen.Trace (reportTrace)
import Test.Common.HsBindgen.TracePredicate
import Test.HsBindgen.Resources
import Test.Tasty (TestName)

import Clang.HighLevel.Types qualified as Clang

import HsBindgen
import HsBindgen.Backend.Hs.Haddock.Config
import HsBindgen.BindingSpec
import HsBindgen.Config.ClangArgs
import HsBindgen.Config.Internal
import HsBindgen.Frontend
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data TestCase = TestCase {
      -- | Name of the test (in the tasty test tree) and the input header
      testName :: TestName

      -- | Name of the header file, e.g., "foo.h"
    , testInputHeaderFileName :: String

      -- | Directory that the input header is in, relative to the package root
    , testInputDir :: FilePath

      -- | Directory where output files should be stored, relative to the
      -- package root
    , testOutputDir :: FilePath

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

      -- | Modify the default external binding specification configuration
    , testExtBindingSpecs :: [FilePath]

      -- | Modify the default prescriptive binding specification configuration
    , testPrescriptiveBindingSpec :: Maybe FilePath

      -- | Whether or not the tests show full paths when rendering Haddock
      -- comments.
      --
      -- For tests this value should be 'Short' by default in order to avoid
      -- #966.
    , testPathStyle :: PathStyle
    }

{-------------------------------------------------------------------------------
  Derived
-------------------------------------------------------------------------------}

testInputInclude :: TestCase -> UncheckedHashIncludeArg
testInputInclude TestCase{testInputHeaderFileName} = testInputHeaderFileName

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

defaultTest ::
     String --  ^ Filepath to the header file without the @.h@ extension
  -> TestCase
defaultTest fp = TestCase{
      testName                    = fp
    , testInputHeaderFileName     = fp <.> "h"
    , testInputDir                = "examples" </> "golden"
    , testOutputDir               = "fixtures" </> fp
    , testTracePredicate          = defaultTracePredicate
    , testHasOutput               = True
    , testClangVersion            = Nothing
    , testOnBootConfig            = id
    , testOnFrontendConfig        = id
    , testStdlibSpec              = EnableStdlibBindingSpec
    , testExtBindingSpecs         = []
    , testPrescriptiveBindingSpec = Nothing
    , testPathStyle               = Short
    }

testVariant ::
     String --  ^ Filename without the @.h@ extension
  -> String --  ^ Variant suffix, appended to the output directory
  -> TestCase
testVariant filename suffix = defTest{
        testName      = testName defTest      ++ "." ++ suffix
      , testOutputDir = testOutputDir defTest ++ "." ++ suffix
      }
  where
    defTest = defaultTest filename

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
    , testInputDir  = "examples/golden"
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
    clangArgsConfig <- getTestDefaultClangArgsConfig testResources [testInputDir]
    return $ testOnBootConfig BootConfig {
        bootClangArgsConfig = clangArgsConfig {
            builtinIncDir = BuiltinIncDirDisable
          }
      , bootBindingSpecConfig = BindingSpecConfig {
            stdlibSpec              = testStdlibSpec
          , compatibility           = BindingSpecStrict
          , extBindingSpecs         = map (root </>) testExtBindingSpecs
          , prescriptiveBindingSpec = (root </>) <$> testPrescriptiveBindingSpec
          }
      }

getTestFrontendConfig :: TestCase -> FrontendConfig
getTestFrontendConfig TestCase{..} = testOnFrontendConfig def

getTestBackendConfig :: TestCase -> BackendConfig
getTestBackendConfig TestCase{..} = getTestDefaultBackendConfig testName testPathStyle

withTestTraceConfig ::
     (String -> IO ())
  -> TestCase
  -> (TracerConfig IO Level TraceMsg -> IO b)
  -> IO b
withTestTraceConfig report TestCase{testTracePredicate} =
    withTraceConfigPredicate report testTracePredicate

-- | Run 'hsBindgen'.
--
-- On 'TraceException's, print error traces.
runTestHsBindgen ::
  (String -> IO ()) -> IO TestResources -> TestCase -> Artefacts as -> IO (NP I as)
runTestHsBindgen report testResources test artefacts =
    handle exceptionHandler $
      runTestHsBindgen' report testResources test artefacts
  where
    exceptionHandler :: SomeException -> IO a
    exceptionHandler e@(SomeException e')
      | Just (TraceException @TraceMsg es) <- fromException e =
          mapM_  printTrace es >> throwIO e'
      | otherwise = throwIO e'
    printTrace = print . reportTrace

-- | Like 'runTestHsBindgen', but do not print error traces.
runTestHsBindgen' ::
  (String -> IO ()) -> IO TestResources -> TestCase -> Artefacts as -> IO (NP I as)
runTestHsBindgen' report testResources test artefacts = do
    bootConfig <- getTestBootConfig testResources test
    let frontendConfig = getTestFrontendConfig test
        backendConfig  = getTestBackendConfig test
        bindgenConfig  = BindgenConfig bootConfig frontendConfig backendConfig
    withTestTraceConfig report test $ \traceConfig ->
      hsBindgen
        traceConfig
        bindgenConfig
        "Example"
        [testInputInclude test]
        artefacts
