-- | Configuring individual test cases (examples)
--
-- Intended for unqualified import.
module Test.HsBindgen.Golden.TestCase (
    -- * Definition
    Outcome(..)
  , TestCase(..)
  , TestCaseSpec(..)
  , testInputInclude
    -- * Construction
  , defaultTest
  , defaultFailingTest
  , defaultFailingTestLibclang
    -- ** Successful tests
  , testVariant
  , testTrace
  , testTraceSimple
  , testTraceMulti
  , testDiagnostic
    -- ** Failing tests
  , failingTestTrace
  , failingTestSimple
  , failingTestMulti
  , failingTestLibclangTrace
  , failingTestLibclangSimple
  , failingTestLibclangMulti
    -- * Execution
  , runTestHsBindgen
  , runTestHsBindgenSuccess
    -- * From spec
  , fromSpec
  ) where

import System.FilePath
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

import Test.Common.HsBindgen.TestCase.Spec (Outcome (..), TestCaseSpec (..))
import Test.Common.HsBindgen.TestCase.Spec qualified as Spec
import Test.Common.HsBindgen.Trace.Predicate
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Full test case for golden tests
--
-- This type embeds 'TestCaseSpec' and adds golden-test-specific fields.
--
data TestCase = TestCase {
      -- | The shared test case specification
      spec :: TestCaseSpec

      -- | Directory where output files should be stored, relative to the
      -- package root
    , outputDir :: FilePath

      -- | Predicate for evaluating the trace messages
    , tracePredicate :: TracePredicate TraceMsg

      -- | Modify the default backend test configuration
    , onBackend :: BackendConfig -> BackendConfig

      -- | Whether or not the tests show full paths when rendering Haddock
      -- comments.
      --
      -- For tests this value should be 'Short' by default in order to avoid
      -- #966.
    , pathStyle :: PathStyle
    }
  deriving stock (Generic)

{-------------------------------------------------------------------------------
  Derived
-------------------------------------------------------------------------------}

testInputInclude :: TestCase -> UncheckedHashIncludeArg
testInputInclude test = test.spec.inputHeader

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Create a default test case from a file path (without .h extension)
defaultTest ::
     String --  ^ Filepath to the header file without the @.h@ extension
  -> TestCase
defaultTest fp = TestCase{
      spec           = Spec.defaultSpec fp
    , outputDir      = "fixtures" </> fp
    , tracePredicate = defaultTracePredicate
    , onBackend      = id
    , pathStyle      = Short
    }

-- | Create a test variant with a suffix appended to name and output directory
testVariant ::
     String --  ^ Filename without the @.h@ extension
  -> String --  ^ Variant suffix, appended to the output directory
  -> TestCase
testVariant filename suffix =
    defaultTest filename
      & #spec % #name %~ (++ ("." ++ suffix))
      & #outputDir    %~ (++ ("." ++ suffix))

testTrace :: String -> TracePredicate TraceMsg -> TestCase
testTrace filename trace = defaultTest filename & #tracePredicate .~ trace

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
  Construction: failing tests
-------------------------------------------------------------------------------}

defaultFailingTest :: String -> TestCase
defaultFailingTest filename =
    defaultTest filename
      & #spec % #outcome  .~ FailureBindgen
      & #spec % #inputDir .~ "examples/golden"

failingTestTrace :: String -> TracePredicate TraceMsg -> TestCase
failingTestTrace filename trace =
    defaultFailingTest filename
      & #tracePredicate .~ trace

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

defaultFailingTestLibclang :: String -> TestCase
defaultFailingTestLibclang filename =
    defaultTest filename
      & #spec % #outcome  .~ FailureLibclang
      & #spec % #inputDir .~ "examples/golden"

failingTestLibclangTrace :: String -> TracePredicate TraceMsg -> TestCase
failingTestLibclangTrace filename trace =
    defaultFailingTestLibclang filename
      & #tracePredicate .~ trace

failingTestLibclangSimple ::
     String
  -> (TraceMsg -> Maybe (TraceExpectation ()))
  -> TestCase
failingTestLibclangSimple filename trace =
    failingTestLibclangTrace filename $ singleTracePredicate trace

failingTestLibclangMulti ::
     (Ord a, RenderLabel a)
  => String
  -> [a]
  -> (TraceMsg -> Maybe (TraceExpectation a))
  -> TestCase
failingTestLibclangMulti filename expected trace =
    failingTestLibclangTrace filename $ multiTracePredicate expected trace

{-------------------------------------------------------------------------------
  Construction from spec
-------------------------------------------------------------------------------}

-- | Create a TestCase from a TestCaseSpec with default golden test settings
fromSpec :: TestCaseSpec -> TestCase
fromSpec s = TestCase {
      spec           = s
    , outputDir      = "fixtures" </> s.name
    , tracePredicate = defaultTracePredicate
    , onBackend      = id
    , pathStyle      = Short
    }

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

getTestBootConfig :: IO TestResources -> TestCase -> IO BootConfig
getTestBootConfig resources test = do
    root <- getTestPackageRoot resources
    clangArgsConfig <- getTestDefaultClangArgsConfig resources [test.spec.inputDir]
    return $ test.spec.onBoot BootConfig {
        clangArgs = clangArgsConfig {
            builtinIncDir = BuiltinIncDirDisable
          }
      , baseModule = "Example"
      , bindingSpec = BindingSpecConfig {
            stdlibSpec              = test.spec.specStdlib
          , compatibility           = BindingSpecStrict
          , extBindingSpecs         = map (root </>) test.spec.specExternal
          , prescriptiveBindingSpec = (root </>) <$> test.spec.specPrescriptive
          }
      }

getTestFrontendConfig :: TestCase -> FrontendConfig
getTestFrontendConfig test = test.spec.onFrontend def

getTestBackendConfig :: TestCase -> BackendConfig
getTestBackendConfig test =
    test.onBackend $ getTestDefaultBackendConfig test.spec.name test.pathStyle

withTestTraceConfig ::
     (String -> IO ())
  -> TestCase
  -> (TracerConfig Level TraceMsg -> IO a)
  -> IO a
withTestTraceConfig report test =
    withTraceConfigPredicate report test.tracePredicate

-- | Run 'hsBindgen'.
runTestHsBindgen ::
    (String -> IO ())
  -> IO TestResources
  -> TestCase
  -> Artefact a
  -> IO (Either BindgenError a)
runTestHsBindgen report resources test artefacts = do
    bootConfig <- getTestBootConfig resources test
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
