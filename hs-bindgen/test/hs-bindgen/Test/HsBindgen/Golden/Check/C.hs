-- | Golden test: C AST
module Test.HsBindgen.Golden.Check.C (check) where

import System.FilePath ((</>))
import Test.Tasty (TestTree)

import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Orphans.TreeDiff ()
import Test.HsBindgen.Resources
import Test.HsBindgen.Golden.TestCase

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test =
    goldenEDiff "treediff" fixture $ \_report -> do
      ActualValue <$> runTestParse testResources test
  where
    fixture :: FilePath
    fixture = "fixtures" </> (testName test ++ ".tree-diff.txt")
