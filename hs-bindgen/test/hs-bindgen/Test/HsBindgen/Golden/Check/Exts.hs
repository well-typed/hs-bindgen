-- | Golden test: language extensions required by generated code
module Test.HsBindgen.Golden.Check.Exts (check) where

import Data.List qualified as List
import Language.Haskell.TH qualified as TH
import System.FilePath ((</>))
import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources
import Test.Tasty

import HsBindgen.Imports
import HsBindgen.TH.Internal

import HsBindgen

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test =
    goldenAnsiDiff "exts" fixture $ \_report -> do
      let artefacts = FinalDecls :* Nil
      (I decls :* Nil) <- runTestHsBindgen testResources test artefacts
      let requiredExts :: Set TH.Extension
          requiredExts = getExtensions decls

          output :: String
          output = unlines $ map show $ List.sort $ toList $ requiredExts

      return $ ActualValue output
  where
    fixture :: FilePath
    fixture = "fixtures" </> (testName test ++ ".exts.txt")

