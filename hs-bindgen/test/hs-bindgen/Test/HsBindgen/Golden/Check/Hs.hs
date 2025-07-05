-- | Golden test: generated Haskell AST
module Test.HsBindgen.Golden.Check.Hs (check) where

import Test.Tasty (TestTree)

import HsBindgen.Lib

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
    goldenEDiff "hs" fixture $ \_report -> do
      ActualValue <$>  testTranslate testResources test
  where
    fixture :: FilePath
    fixture = "fixtures" </> (testName test ++ ".hs")
