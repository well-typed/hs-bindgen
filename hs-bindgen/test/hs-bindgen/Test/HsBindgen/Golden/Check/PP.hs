-- | Golden test: pretty-printed Haskell code
module Test.HsBindgen.Golden.Check.PP (check) where

import System.FilePath ((</>))
import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources
import Test.Tasty

import HsBindgen.Backend.SHs.AST (Safety (Safe))

import HsBindgen

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test =
    goldenAnsiDiff "pp" fixture $ \report -> do
      let artefacts = getBindings Safe :* Nil
      (I output :* Nil) <- runTestHsBindgen report testResources test artefacts
      return $ ActualValue output
  where
    fixture :: FilePath
    fixture = "fixtures" </> (testName test ++ ".pp.hs")
