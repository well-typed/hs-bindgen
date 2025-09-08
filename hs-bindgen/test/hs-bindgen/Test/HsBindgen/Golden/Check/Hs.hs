-- | Golden test: generated Haskell AST
module Test.HsBindgen.Golden.Check.Hs (check) where

import System.FilePath ((</>))
import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Orphans.TreeDiff ()
import Test.HsBindgen.Resources
import Test.Tasty (TestTree)

import HsBindgen

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test =
    goldenEDiff "hs" fixture $ \_report -> do
      (I hsDecls :* Nil) <- runTestHsBindgen testResources test (HsDecls :* Nil)
      -- TODO https://github.com/well-typed/hs-bindgen/issues/1089: Test all
      -- binding categories.
      pure $ ActualValue $ concat hsDecls
  where
    fixture :: FilePath
    fixture = "fixtures" </> (testName test ++ ".hs")
