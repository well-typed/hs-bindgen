-- | Golden test: language extensions required by generated code
module Test.HsBindgen.Golden.Check.Exts (check) where

import Data.List qualified as List
import System.FilePath ((</>))
import Test.Tasty

import HsBindgen.Imports

import HsBindgen
import HsBindgen.Backend.Extensions
import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test =
    goldenAnsiDiff "exts" fixture $ \_report -> do
      (I sHsDecls :* Nil) <- runTestRunArtefacts testResources test (SHs :* Nil)
      let output :: String
          output = unlines $ map show $ List.sort $ toList $
              foldMap requiredExtensions sHsDecls

      return $ ActualValue output
  where
    fixture :: FilePath
    fixture = "fixtures" </> (testName test ++ ".exts.txt")

