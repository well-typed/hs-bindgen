-- | Golden test: generated binding specifications
module Test.HsBindgen.Golden.Check.BindingSpec (check) where

import Data.ByteString.UTF8 qualified as UTF8
import System.FilePath ((</>))
import Test.Tasty (TestTree)

import HsBindgen.BindingSpec.Gen qualified as BindingSpec
import HsBindgen.Language.Haskell qualified as Hs

import HsBindgen
import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test =
    goldenAnsiDiff "bindingspec" fixture $ \_report -> do
      (I decls :* Nil) <- runTestRunArtefacts testResources test (Hs :* Nil)

      let output :: String
          output = UTF8.toString $
              BindingSpec.genBindingSpecYaml [testInputInclude test]
                (Hs.HsModuleName "Example")
                decls

      return $ ActualValue output
  where
    fixture :: FilePath
    fixture = "fixtures" </> (testName test ++ ".bindingspec.yaml")
