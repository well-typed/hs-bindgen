-- | Configuring individual test cases (examples)
--
-- Intended for unqualified import.
module Test.HsBindgen.Golden.Infra.TestCase (
    -- * Definition
    Outcome(..)
  , TestCase(..)
  , SomeMacroLang(..)
  , macroLangCExpr
  , macroLangEmpty
  , macroLangRaw
  , testInputInclude
    -- * Construction
  , defaultTest
  , defaultFailingTest
  , defaultFailingTestLibclang
    -- ** Successful tests
  , testVariant
  , mkTestVariant
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
  , getTestBootConfig
  , getTestFrontendConfig
  , getTestBackendConfig
  , runTestHsBindgen
  , runTestHsBindgenSuccess
  ) where

import System.FilePath
import Test.Tasty (TestName)
import Test.Tasty.HUnit (assertFailure)

import Clang.CStandard (ClangCStandard)
import Clang.HighLevel.Types qualified as Clang

import HsBindgen
import HsBindgen.Backend.Hs.Haddock.Config
import HsBindgen.BindingSpec
import HsBindgen.Config.Internal
import HsBindgen.Frontend
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro qualified as Macro
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

-- | The macro language a test runs with.
--
-- The concrete tag @l@ is existentially hidden so that different tests (and
-- variants of the same test) can run with different macro languages. The macro
-- language is built from the boot-detected 'ClangCStandard'.
data SomeMacroLang =
    forall l. Macro.HasTypes l =>
      SomeMacroLang (ClangCStandard -> Macro.Lang l)

-- | The default macro language, backed by @c-expr-dsl@.
macroLangCExpr :: SomeMacroLang
macroLangCExpr = SomeMacroLang Macro.cExpr

-- | The 'Macro.Empty' macro language: recognizes no macros.
macroLangEmpty :: SomeMacroLang
macroLangEmpty = SomeMacroLang (const Macro.empty)

-- | The 'Macro.Raw' macro language: translates macros to their tokens.
macroLangRaw :: SomeMacroLang
macroLangRaw = SomeMacroLang (const Macro.raw)

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
    , tracePredicate :: TracePredicate Level TraceMsg

      -- | Does this test have output, or does it fail?
    , outcome :: Outcome

      -- | Tests that require a specific @libclang@ version
      --
      -- If the predicate does not match, the test is skipped entirely.
    , clangVersion :: Maybe ((Int, Int, Int) -> Bool)

      -- | Configure the C standard for this test
      --
      -- In general, a feature should be tested with the /minimum/ standard that
      -- supports the feature.  Additional tests with other standards may also
      -- be provided, when appropriate.
    , cStandard :: CStandard

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

      -- | In imports, put "qualified" before or after the module name
    , qualifiedStyle :: QualifiedStyle

      -- | The macro language to run this test with
    , macroLang :: SomeMacroLang
    }
  deriving stock (Generic)

{-------------------------------------------------------------------------------
  Derived
-------------------------------------------------------------------------------}

testInputInclude :: TestCase -> C.UncheckedHashIncludeArg
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
    , inputDir         = "test-artefacts" </> "headers" </> "golden"
    , outputDir        = "test-artefacts" </> "fixtures" </> fp
    , tracePredicate   = defaultTracePredicate
    , outcome          = Success
    , clangVersion     = Nothing
    , cStandard        = c89
    , onBoot           = id
    , onFrontend       = id
    , onBackend        = id
    , specStdlib       = EnableStdlibBindingSpec
    , specExternal     = []
    , specPrescriptive = Nothing
    , pathStyle        = Short
    , qualifiedStyle   = def
    , macroLang        = macroLangCExpr
    }

testVariant ::
     String        -- ^ Filename without the @.h@ extension
  -> Maybe Natural -- ^ Variant index
  -> String        -- ^ Variant suffix, appended to the output directory
  -> TestCase
testVariant filename index suffix =
    mkTestVariant index suffix $
      defaultTest filename

mkTestVariant ::
     Maybe Natural -- ^ Variant index
  -> String        -- ^ Variant suffix
  -> TestCase
  -> TestCase
mkTestVariant index suffix test = test
    & #name           %~ (++ indexInfix ++ "." ++ suffix)
    & #outputDir      %~ (++ indexInfix ++ "." ++ suffix)
  where
    indexInfix = maybe "" (\n -> "." ++ show n) index

testTrace :: String -> TracePredicate Level TraceMsg -> TestCase
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
      & #inputDir .~ "test-artefacts/headers/golden"

failingTestTrace :: String -> TracePredicate Level TraceMsg -> TestCase
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
      & #inputDir .~ "test-artefacts/headers/golden"

failingTestLibclangTrace :: String -> TracePredicate Level TraceMsg -> TestCase
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

getTestBootConfig :: TestCase -> TestResources -> BootConfig
getTestBootConfig test testResources = test.onBoot BootConfig {
      clangArgs =
        getTestClangArgsConfig test.cStandard [test.inputDir] testResources
    , baseModule = "Example"
    , bindingSpec = BindingSpecConfig {
          stdlibSpec              = test.specStdlib
        , compatibility           = BindingSpecStrict
        , extBindingSpecs         =
            map (testResources.packageRoot </>) test.specExternal
        , prescriptiveBindingSpec =
            (testResources.packageRoot </>) <$> test.specPrescriptive
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
withTestTraceConfig report test action =
    withTraceConfigPredicate report test.tracePredicate $ \traceConfig ->
      let traceConfig' = traceConfig
      in  action traceConfig'

-- | Run 'hsBindgen'.
--
-- The artefact is polymorphic in the macro-language tag @l@, so that the same
-- artefact can be run with whichever macro language 'test' selects.
runTestHsBindgen ::
     forall a.
     (String -> IO ())
  -> IO TestResources
  -> TestCase
  -> (forall l. Macro.HasTypes l => Artefact l a)
  -> IO (Either BindgenError a)
runTestHsBindgen report getTestResources test artefacts = do
    bootConfig <- getTestBootConfig test <$> getTestResources
    let frontendConfig = getTestFrontendConfig test
        backendConfig  = getTestBackendConfig test
        bindgenConfig  = BindgenConfig bootConfig frontendConfig backendConfig
    withTestTraceConfig report test $ \traceConfigUnsafe ->
      case test.macroLang of
        SomeMacroLang mkMacroLang ->
          hsBindgenEMacroLang
            (pure . mkMacroLang)
            traceConfigUnsafe
            quietTracerConfig
            bindgenConfig
            [testInputInclude test]
            artefacts

runTestHsBindgenSuccess ::
     forall b.
     (String -> IO ())
  -> IO TestResources
  -> TestCase
  -> (forall l. Macro.HasTypes l => Artefact l b)
  -> IO b
runTestHsBindgenSuccess report getTestResources test artefacts = do
    eRes <- runTestHsBindgen report getTestResources test artefacts
    case eRes of
      Left er -> assertFailure (msgWith er)
      Right r -> pure r
  where
    msgWith :: BindgenError -> String
    msgWith e = "Expected 'hs-bindgen' to succeed, but received an error: " <> show e
