-- | Golden test: pretty-printed Haskell code
module Test.HsBindgen.Golden.Check.PP (check) where

import System.FilePath ((</>))
import Test.Tasty

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
    goldenAnsiDiff "pp" fixture $ \_report -> do
      let getBindingsA = getBindings :* Nil
      (I output :* Nil) <- runTestRunArtefacts testResources test getBindingsA
      return $ ActualValue output
  where
    fixture :: FilePath
    fixture = "fixtures" </> (testName test ++ ".pp.hs")
