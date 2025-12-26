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
  , testTraceMulti
  , testDiagnostic
    -- ** Failing tests (that is, with hs-bindgen errors and no output)
  , failingTestTrace
  , failingTestSimple
  , failingTestMulti
    -- * Execution
  , runTestHsBindgenSuccess
  , runTestHsBindgenFailure
  ) where

import System.FilePath
import Test.Tasty (TestName)
import Test.Tasty.HUnit (assertFailure)

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

import Test.Common.HsBindgen.Trace.Predicate
import Test.HsBindgen.Resources

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

      -- | Modify the default boot test configuration
    , testOnBootConfig :: BootConfig -> BootConfig

      -- | Modify the default frontend test configuration
    , testOnFrontendConfig :: FrontendConfig -> FrontendConfig

      -- | Modify the default backend test configuration
    , testOnBackendConfig :: BackendConfig -> BackendConfig

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
    , testOnBackendConfig         = id
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

testTraceMulti ::
     (Ord b, RenderLabel b)
  => String
  -> [b]
  -> (TraceMsg -> Maybe (TraceExpectation b))
  -> TestCase
testTraceMulti filename expected trace =
    testTrace filename $ multiTracePredicate expected trace

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

failingTestMulti ::
     (Ord a, RenderLabel a)
  => String
  -> [a]
  -> (TraceMsg -> Maybe (TraceExpectation a))
  -> TestCase
failingTestMulti filename expected trace =
    failingTestTrace filename $ multiTracePredicate expected trace

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
      , bootBaseModuleName = "Example"
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
getTestBackendConfig TestCase{..} =
  testOnBackendConfig $ getTestDefaultBackendConfig testName testPathStyle

withTestTraceConfig ::
     (String -> IO ())
  -> TestCase
  -> (TracerConfig Level TraceMsg -> IO a)
  -> IO a
withTestTraceConfig report TestCase{testTracePredicate} =
    withTraceConfigPredicate report testTracePredicate

-- | Run 'hsBindgen'.
runTestHsBindgen ::
    (String -> IO ())
  -> IO TestResources
  -> TestCase
  -> Artefact a
  -> IO (Either BindgenError a)
runTestHsBindgen report testResources test artefacts = do
    bootConfig <- getTestBootConfig testResources test
    let frontendConfig = getTestFrontendConfig test
        backendConfig  = getTestBackendConfig test
        bindgenConfig  = BindgenConfig bootConfig frontendConfig backendConfig
    withTestTraceConfig report test $ \traceConfigUnsafe ->
      hsBindgenE
        traceConfigUnsafe
        quietTracerConfig
        bindgenConfig
        [testInputInclude test]
        artefacts

runTestHsBindgenSuccess ::
  (String -> IO ()) -> IO TestResources -> TestCase -> Artefact b -> IO b
runTestHsBindgenSuccess report resources test artefacts = do
    eRes <- runTestHsBindgen report resources test artefacts
    case eRes of
      Left er -> assertFailure (msgWith er)
      Right r -> pure r
  where
    msgWith :: BindgenError -> String
    msgWith e = "Expected 'hs-bindgen' to succeed, but received an error: " <> show e

runTestHsBindgenFailure ::
      Show b
  => (String -> IO ())
  -> IO TestResources
  -> TestCase
  -> Artefact b
  -> IO BindgenError
runTestHsBindgenFailure report resources test artefacts = do
    eRes <- runTestHsBindgen report resources test artefacts
    case eRes of
      Left er -> pure er
      Right r -> assertFailure (msgWith r)
  where
    msgWith :: Show b => b -> String
    msgWith r = mconcat [
        "Expected 'hs-bindgen' to fail, "
      , "but it succeeded with the following list of declarations:\n"
      , show r
      ]
