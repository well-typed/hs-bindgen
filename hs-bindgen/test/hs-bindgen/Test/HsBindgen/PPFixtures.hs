{-# LANGUAGE OverloadedRecordDot #-}

-- | Test suite to verify that PP-generated fixture files compile successfully
--
-- This test suite compiles all PP fixture @.hs@ files in a single batch via
-- direct @ghc@ invocation, verifying that the pretty-printed output produces
-- valid Haskell. With @--haddock@, it also generates browsable Haddock
-- documentation.
--
module Test.HsBindgen.PPFixtures (tests) where

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import System.FilePath (takeDirectory, (</>))
import Test.Tasty
import Test.Tasty.Runners (testPatternMatches)

import Test.Common.Util.Tasty.Golden (RunMode (..))
import Test.HsBindgen.Fixtures.Haddock (generateHaddockIndex)
import Test.HsBindgen.Fixtures.TestCases (FixtureStatus (..), haddockOutputDir,
                                          isCompile, mkFixtureTest)
import Test.HsBindgen.Golden (allTestCases)
import Test.HsBindgen.Golden.Infra.TestCase (TestCase (..))
import Test.HsBindgen.PPFixtures.Compile (setupPPBatchCompile)
import Test.HsBindgen.PPFixtures.TestCases (determinePPStatus)
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | @pathPrefix@ is the path of 'testGroup' names from the test tree root
-- to this point (e.g. @[\"test-hs-bindgen\"]@). 'testPatternMatches' needs
-- full paths to replicate tasty's @-p@ filtering.
tests :: Seq String -> IO TestResources -> TestTree
tests pathPrefix getTestResources =
    askOption $ \haddockMode ->
    askOption $ \case
      Fast -> testGroup "pp-fixtures" []
      Full -> askOption $ \testPattern ->
        withResource (setup testPattern haddockMode) (const $ return ()) $
          \getResults ->
            testGroup "pp-fixtures" $
              map (mkFixtureTest "fixture-" "PP compilation"
                    determinePPStatus getResults)
                  allTestCases
  where
    setup testPattern haddockMode = do
        testResources <- getTestResources
        let repoRoot   = takeDirectory testResources.packageRoot
            buildDir   = repoRoot </> "dist-newstyle" </> "fixtures"
            haddockDir = haddockOutputDir haddockMode buildDir
            -- Single pass: partition test cases into compiled and skipped
            (compiled, skippedCount) = foldr classify ([], 0 :: Int) allTestCases
            classify tc (cs, n)
              | not (isCompile status)  = (cs, n + 1)
              | not (matchesPattern tc) = (cs, n)
              | otherwise               = (entry tc : cs, n)
              where status = determinePPStatus tc
            matchesPattern tc = testPatternMatches testPattern
              (pathPrefix <> Seq.fromList ["pp-fixtures", tc.name])
            entry tc =
              ( tc.name
              , testResources.packageRoot </> tc.outputDir
              , determinePPStatus tc /= FixtureCompileNoOptcWerror
              )

        results <- setupPPBatchCompile repoRoot haddockDir compiled

        -- Generate index page for haddock output
        let compiledNames = map (\(n, _, _) -> n) compiled
        case haddockDir of
          Just dir -> generateHaddockIndex "fixture-" dir compiledNames skippedCount
          Nothing  -> return ()

        return results
