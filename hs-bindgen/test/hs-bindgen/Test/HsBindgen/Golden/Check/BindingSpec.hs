-- | Golden test: generated binding specifications
module Test.HsBindgen.Golden.Check.BindingSpec (check) where

import Data.ByteString.UTF8 qualified as UTF8
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree)

import HsBindgen
import HsBindgen.BindingSpec qualified as BindingSpec

import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test =
    goldenAnsiDiff "bindingspec" fixture $ \report -> do
      let artefacts = getBindingSpec BindingSpec.FormatYAML
      output <- runTestHsBindgenSuccess report testResources test artefacts
      pure $ ActualValue $ UTF8.toString output
  where
    fixture :: FilePath
    fixture = test.outputDir </> "bindingspec" <.> "yaml"
