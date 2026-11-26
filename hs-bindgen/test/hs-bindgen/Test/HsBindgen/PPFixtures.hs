{-# LANGUAGE OverloadedRecordDot #-}

-- | Test suite to verify that PP-generated fixture files compile successfully
--
-- This test suite compiles all PP fixture @.hs@ files in a single batch via
-- direct @ghc@ invocation, verifying that the pretty-printed output produces
-- valid Haskell. With @--haddock@, it also generates browsable Haddock
-- documentation.
--
-- The fixtures are the golden directories of the golden test cases and of the
-- library tests.
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
import Test.HsBindgen.Golden (allLibraryTests, allTestCases)
import Test.HsBindgen.Golden.Infra.LibraryTest (LibraryOutcome (..),
                                                LibraryTest (..))
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
              ++ map (mkFixtureTest "fixture-" "PP compilation"
                       (const FixtureCompile) getResults . (.test))
                     libraryGoldens
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
              , ["-optc-Werror" | determinePPStatus tc /= FixtureCompileNoOptcWerror]
              )
            libraryEntries = [
                ( libraryTest.test.name
                , testResources.packageRoot </> libraryTest.test.outputDir
                , libraryGhcArgs libraryTest testResources.packageRoot
                )
              | libraryTest <- libraryGoldens
              , matchesPattern libraryTest.test
              ]
            jobs = compiled ++ libraryEntries

        results <- setupPPBatchCompile repoRoot haddockDir jobs

        -- Generate index page for haddock output
        let compiledNames = map (\(n, _, _) -> n) jobs
        case haddockDir of
          Just dir -> generateHaddockIndex "fixture-" dir compiledNames skippedCount
          Nothing  -> return ()

        return results

{-------------------------------------------------------------------------------
  Library tests
-------------------------------------------------------------------------------}

-- | The library tests that write a golden directory
libraryGoldens :: [LibraryTest]
libraryGoldens = [
      libraryTest
    | libraryTest <- allLibraryTests
    , WritesGolden <- [libraryTest.outcome]
    ]

-- | The extra GHC arguments to compile the golden directory of a library test
--
-- The C stubs include the root header by its name, so the include directory of
-- the run goes on the C include path. The golden directories of the libraries
-- it depends on go on the module search path.
libraryGhcArgs :: LibraryTest -> FilePath -> [String]
libraryGhcArgs libraryTest packageRoot = concat [
      ["-optc-Werror"]
    , ["-I" ++ packageRoot </> libraryTest.test.inputDir]
    , [ "-i" ++ packageRoot </> dependency.test.outputDir
      | dependency <- libraryTest.dependencies
      ]
    ]
