-- | Golden test: generated binding specifications
module Test.HsBindgen.Golden.Check.BindingSpec (check) where

import Data.ByteString.UTF8 qualified as UTF8
import System.FilePath ((</>))
import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources
import Test.Tasty (TestTree)

import HsBindgen
import HsBindgen.BindingSpec.Gen qualified as BindingSpec
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test =
    goldenAnsiDiff "bindingspec" fixture $ \report -> do
      (I getMainHeaders :* I omitTypes :* I hsDecls :* Nil) <-
        runTestHsBindgen report testResources test $
          GetMainHeaders :* OmitTypes :* HsDecls :* Nil

      let output :: String
          output = UTF8.toString $
              BindingSpec.genBindingSpecYaml
                (Hs.ModuleName "Example")
                getMainHeaders
                omitTypes
                -- TODO https://github.com/well-typed/hs-bindgen/issues/1089:
                -- Test all binding categories.
                (concat hsDecls)

      return $ ActualValue output
  where
    fixture :: FilePath
    fixture = "fixtures" </> (testName test ++ ".bindingspec.yaml")
