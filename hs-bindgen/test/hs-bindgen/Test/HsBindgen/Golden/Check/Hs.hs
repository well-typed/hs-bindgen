-- | Golden test: generated Haskell AST
module Test.HsBindgen.Golden.Check.Hs (check) where

import System.FilePath ((</>))
import Test.Tasty (TestTree)

import HsBindgen

import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Orphans.TreeDiff ()
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test =
    goldenEDiff "hs" fixture $ \_report -> do
      (I hsDecls :* Nil) <- runTestHsBindgen testResources test (HsDecls :* Nil)
      pure $ ActualValue hsDecls
  where
    fixture :: FilePath
    fixture = "fixtures" </> (testName test ++ ".hs")
