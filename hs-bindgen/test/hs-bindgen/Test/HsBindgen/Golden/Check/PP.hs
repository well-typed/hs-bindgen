-- | Golden test: pretty-printed Haskell code
module Test.HsBindgen.Golden.Check.PP (check) where

import Control.Monad (when)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources
import Test.Tasty

import HsBindgen (getBindingsMultiple)
import HsBindgen hiding (getBindingsMultiple)
import HsBindgen.Backend.SHs.AST (ByCategory (..))
import HsBindgen.Config.Prelims
import HsBindgen.Errors (panicIO)
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test =
    withExampleDir $
    testGroup "pp" [
        goldenAnsiDiff (show bc) (fixture bc) $ \report -> do
          -- A golden tests should typically produce only a single file, so we
          -- run @hs-bindgen@ separately for each binding category. It's
          -- slightly unfortunate to invoke @hs-bindgen@ multiple times even if
          -- it can render all modules at the same time, but it's cheap to do so
          -- in practice.
          let artefacts = (,) <$> FinalModuleBaseName <*> getBindingsMultiple
          (baseName,(ByCategory output))
            <- runTestHsBindgenSuccess report testResources test artefacts

          -- A sanity check to make sure that that the modules we're rendering
          -- have the expected @Example@ base name.
          when (baseName /= "Example") $
            panicIO "The module base name should be Example!"

          -- Render the Haskell module
          let ppOutput = fromMaybe (renderEmptyModule bc) (output Map.!? bc)
          return $ ActualValue ppOutput

      | (bc :: BindingCategory) <- [minBound .. maxBound]
      ]
  where
    -- === Filepaths
    --
    -- A golden test needs to know at which filepath to store its output
    -- /before/ it actually runs the test, as it's typically static information.
    -- As such, we can't rely on the invocation of @hs-bindgen@ to generate the
    -- filepath names for us, so we redefine the logic for constructing
    -- filepaths here.
    --
    -- NOTE: workarounds using 'withResource' to run @hs-bindgen@ before the
    -- golden test has actually started might be possible, but they would likely
    -- also be rather clunky.

    -- | Everything except the top-level @Example.hs@ module is in the @Example@
    -- directory.
    withExampleDir :: TestTree -> TestTree
    withExampleDir k =
        withResource
          (createDirectoryIfMissing False (testOutputDir test </> "Example"))
          (\_ -> pure ())
          (\_ -> k)

    -- | The names of sub-modules are based solely on the binding category
    fixture :: BindingCategory -> FilePath
    fixture bc = testOutputDir test </> Hs.moduleNamePath moduleName
      where
        moduleName :: Hs.ModuleName
        moduleName = fromBaseModuleName "Example" (Just bc)

    -- === Module names
    --
    -- @hs-bindgen@ does not produce output for modules that are
    -- empty, but again since we are running a golden test and a golden test
    -- expects /some/ output, we synthesise an empty module (that should
    -- compile!) with the correct module name.

    -- | Render an empty module
    renderEmptyModule :: BindingCategory -> String
    renderEmptyModule bc = concat [
          "module "
        , Hs.moduleNameToString moduleName
        , " () where\n"
        ]
      where
        moduleName :: Hs.ModuleName
        moduleName = fromBaseModuleName "Example" (Just bc)
