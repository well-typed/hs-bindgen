-- | Golden test: C AST
module Test.HsBindgen.Golden.Check.C (check) where

import System.FilePath ((</>))
import Test.Tasty (TestTree)

import HsBindgen
import HsBindgen.Frontend.AST.External qualified as C
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
    goldenEDiff "treediff" fixture $ \_report -> do
      (I deps :* I decls :* Nil) <- runTestRunArtefacts testResources test
        (Dependencies :* ReifiedC :* Nil)
      pure $ ActualValue $ C.TranslationUnit decls deps
  where
    fixture :: FilePath
    fixture = "fixtures" </> (testName test ++ ".tree-diff.txt")
