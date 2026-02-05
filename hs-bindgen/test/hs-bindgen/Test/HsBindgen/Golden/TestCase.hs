-- | Configuring individual test cases (examples)
--
-- Intended for unqualified import.
module Test.HsBindgen.Golden.TestCase (
    -- * Definition
    Outcome(..)
  , TestCase(..)
  , testInputInclude
    -- * Construction
  , defaultTest
  , defaultFailingTest
  , defaultFailingTestLibclang
  , getTestBackendConfig
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

data Outcome =
      -- | We expect the test to succeed (with or without output).
      Success
      -- | We expect the test to fail with 'hsBindgenE' returning a 'BindgenError'.
    | FailureBindgen
      -- | We expect the test to fail right after invoking `libclang`.
    | FailureLibclang

data TestCase = TestCase {
      -- | Name of the test (in the tasty test tree) and the input header
      name :: TestName

      -- | Name of the header file, e.g., "foo.h"
    , inputHeader :: String

      -- | Directory that the input header is in, relative to the package root
    , inputDir :: FilePath

      -- | Directory where output files should be stored, relative to the
      -- package root
    , outputDir :: FilePath

      -- | Predicate for evaluating the trace messages
    , tracePredicate :: TracePredicate TraceMsg

      -- | Does this test have output, or does it fail?
    , outcome :: Outcome

      -- | Tests that require a specific @libclang@ version
      --
      -- If the predicate does not match, the test is skipped entirely.
    , clangVersion :: Maybe ((Int, Int, Int) -> Bool)

      -- | Modify the default boot test configuration
    , onBoot :: BootConfig -> BootConfig

      -- | Modify the default frontend test configuration
    , onFrontend :: FrontendConfig -> FrontendConfig

      -- | Modify the default backend test configuration
    , onBackend :: BackendConfig -> BackendConfig

      -- | Configure if the @stdlib@ binding specification should be used
    , specStdlib :: EnableStdlibBindingSpec

      -- | Modify the default external binding specification configuration
    , specExternal :: [FilePath]

      -- | Modify the default prescriptive binding specification configuration
    , specPrescriptive :: Maybe FilePath

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
testInputInclude test = test.inputHeader

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

defaultTest ::
     String --  ^ Filepath to the header file without the @.h@ extension
  -> TestCase
defaultTest fp = TestCase{
      name             = fp
    , inputHeader      = fp <.> "h"
    , inputDir         = "examples" </> "golden"
    , outputDir        = "fixtures" </> fp
    , tracePredicate   = defaultTracePredicate
    , outcome          = Success
    , clangVersion     = Nothing
    , onBoot           = id
    , onFrontend       = id
    , onBackend        = id
    , specStdlib       = EnableStdlibBindingSpec
    , specExternal     = []
    , specPrescriptive = Nothing
    , pathStyle        = Short
    }

testVariant ::
     String --  ^ Filename without the @.h@ extension
  -> String --  ^ Variant suffix, appended to the output directory
  -> TestCase
testVariant filename suffix =
    defaultTest filename
      & #name      %~ (++ ("." ++ suffix))
      & #outputDir %~ (++ ("." ++ suffix))

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
      & #outcome  .~ FailureBindgen
      & #inputDir .~ "examples/golden"

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
      & #outcome  .~ FailureLibclang
      & #inputDir .~ "examples/golden"

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
  Execution
-------------------------------------------------------------------------------}

getTestBootConfig :: IO TestResources -> TestCase -> IO BootConfig
getTestBootConfig resources test = do
    root <- getTestPackageRoot resources
    clangArgsConfig <- getTestDefaultClangArgsConfig resources [test.inputDir]
    return $ test.onBoot BootConfig {
        clangArgs = clangArgsConfig {
            builtinIncDir = BuiltinIncDirDisable
          }
      , baseModule = "Example"
      , bindingSpec = BindingSpecConfig {
            stdlibSpec              = test.specStdlib
          , compatibility           = BindingSpecStrict
          , extBindingSpecs         = map (root </>) test.specExternal
          , prescriptiveBindingSpec = (root </>) <$> test.specPrescriptive
          }
      }

getTestFrontendConfig :: TestCase -> FrontendConfig
getTestFrontendConfig test = test.onFrontend def

getTestBackendConfig :: TestCase -> BackendConfig
getTestBackendConfig test =
    test.onBackend $ getTestDefaultBackendConfig test.name test.pathStyle

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
