{-# LANGUAGE CPP #-}

#if defined(MIN_VERSION_GLASGOW_HASKELL)
#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
#define exportPattern data
#else
#define exportPattern pattern
#endif
#else
#define exportPattern pattern
#endif

-- | Construct tasty test tree
module Test.HsBindgen.Golden.Infra.TestCaseTree (
    TestCaseTree (..)
  , exportPattern TestCases
  , exportPattern TestCaseLeafs
  , testTreeFor
  , flatten
  ) where

import System.Directory (createDirectoryIfMissing)
import Test.Tasty (TestTree, testGroup, withResource)

import Clang.Version (ClangVersion (ClangVersion, ClangVersionUnknown),
                      runtimeClangVersion)

import Test.HsBindgen.Golden.Infra.Check.BindingSpec qualified as BindingSpec
import Test.HsBindgen.Golden.Infra.Check.FailureBindgen qualified as FailureBindgen
import Test.HsBindgen.Golden.Infra.Check.FailureLibclang qualified as FailureLibclang
import Test.HsBindgen.Golden.Infra.Check.PP qualified as PP
import Test.HsBindgen.Golden.Infra.Check.TH qualified as TH
import Test.HsBindgen.Golden.Infra.TestCase (Outcome (FailureBindgen, FailureLibclang, Success),
                                             TestCase (clangVersion, name, outcome, outputDir))
import Test.HsBindgen.Resources (TestResources)

data TestCaseTree =
    TestCaseSection String [TestCaseTree]
  | TestCaseLeaf TestCase

pattern TestCases :: String -> [TestCase] -> TestCaseTree
pattern TestCases label cases = TestCaseSection label (TestCaseLeafs cases)

pattern TestCaseLeafs :: [TestCase] -> [TestCaseTree]
pattern TestCaseLeafs cases <- (fmap (\(TestCaseLeaf test) -> test) -> cases)
  where TestCaseLeafs cases = fmap TestCaseLeaf cases

testTreeFor :: IO TestResources -> TestCaseTree -> TestTree
testTreeFor getTestResources = goTree
  where
    goTree :: TestCaseTree -> TestTree
    goTree (TestCaseSection label sections) =
        testGroup label $ map goTree sections
    goTree (TestCaseLeaf test) = goCase test

    goCase :: TestCase -> TestTree
    goCase test
      | Just versionPred <- test.clangVersion
      , case runtimeClangVersion of
          ClangVersion version  -> not (versionPred version)
          ClangVersionUnknown _ -> True
      = testGroup test.name []

      | otherwise
      = case test.outcome of
          Success ->
            withTestOutputDir test.outputDir $ testGroup test.name [
                TH.check          getTestResources test
              , PP.check          getTestResources test
              , BindingSpec.check getTestResources test
              ]
          FailureBindgen ->
            FailureBindgen.check getTestResources test
          FailureLibclang ->
            FailureLibclang.check getTestResources test

    withTestOutputDir :: FilePath -> TestTree -> TestTree
    withTestOutputDir outputDir k =
        withResource
          (createDirectoryIfMissing True outputDir)
          (\_ -> pure ())
          (\_ -> k)

flatten :: TestCaseTree -> [TestCase]
flatten = \case
    TestCaseSection _ subTrees -> concatMap flatten subTrees
    TestCaseLeaf leaf          -> [leaf]
