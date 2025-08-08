-- | Golden test: language extensions required by generated code
module Test.HsBindgen.Golden.Check.Exts (check) where

import Data.List qualified as List
import System.FilePath ((</>))
import Test.Tasty

import HsBindgen.Imports
import HsBindgen.Pipeline.Lib qualified as Pipeline

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
      decls <- runTestTranslate testResources test

      let output :: String
          output = unlines $ map show $ List.sort $ toList $
              Pipeline.genExtensions
            $ Pipeline.genSHsDecls decls

      return $ ActualValue output
  where
    fixture :: FilePath
    fixture = "fixtures" </> (testName test ++ ".exts.txt")

