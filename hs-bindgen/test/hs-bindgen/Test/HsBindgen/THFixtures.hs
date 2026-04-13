{-# LANGUAGE OverloadedRecordDot #-}

-- | Test suite to verify that TH-generated bindings compile successfully
--
-- This test suite generates TH modules for each golden test fixture and
-- compiles them all via direct @ghc@ invocation, verifying that the TH output
-- produces valid Haskell. With @--haddock@, it also generates browsable
-- Haddock documentation.
--
module Test.HsBindgen.THFixtures (tests) where

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import System.FilePath (takeDirectory, (</>))
import Test.Tasty
import Test.Tasty.Runners (testPatternMatches)

import Test.Common.Util.Tasty.Golden (RunMode (..))
import Test.HsBindgen.Fixtures.Haddock (generateHaddockIndex)
import Test.HsBindgen.Fixtures.TestCases (haddockOutputDir, isCompile,
                                          mkFixtureTest)
import Test.HsBindgen.Golden (allTestCases)
import Test.HsBindgen.Golden.Infra.TestCase (TestCase (..))
import Test.HsBindgen.Resources
import Test.HsBindgen.THFixtures.Compile (setupBatchCompile)
import Test.HsBindgen.THFixtures.Generate (generateModule)
import Test.HsBindgen.THFixtures.TestCases (determineTHStatus)

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
      Fast -> testGroup "th-fixtures" []
      Full -> askOption $ \testPattern ->
        withResource (setup testPattern haddockMode) (const $ return ()) $
          \getResults ->
            testGroup "th-fixtures" $
              map (mkFixtureTest "th-fixture-" "TH compilation"
                    determineTHStatus getResults)
                  allTestCases
  where
    -- Only compile fixtures that match the @-p@ pattern. Without this,
    -- 'withResource' compiles all fixtures regardless of @-p@.
    setup testPattern haddockMode = do
        testResources <- getTestResources
        let repoRoot   = takeDirectory testResources.packageRoot
            buildDir   = repoRoot </> "dist-newstyle" </> "fixtures"
            haddockDir = haddockOutputDir haddockMode buildDir
            (cases, skippedCount) = foldr classify ([], 0 :: Int) allTestCases
            classify tc (cs, n)
              | not (isCompile (determineTHStatus tc)) = (cs, n + 1)
              | not (testPatternMatches testPattern
                  (pathPrefix <> Seq.fromList ["th-fixtures", tc.name])) = (cs, n)
              | otherwise = ((tc.name, generateModule testResources tc) : cs, n)

        results <- setupBatchCompile repoRoot haddockDir cases

        -- Generate index page for haddock output
        let compiledNames = map fst cases
        case haddockDir of
          Just dir -> generateHaddockIndex "th-fixture-" dir compiledNames skippedCount
          Nothing  -> return ()

        return results
