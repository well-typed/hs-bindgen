-- | Test suite to verify that TH-generated bindings compile successfully
--
-- This test suite generates TH modules for each golden test fixture and
-- compiles them all in a single batch via @cabal build@, verifying that
-- the TH output produces valid Haskell.
--
module Test.HsBindgen.THFixtures (tests) where

import Data.Map.Strict qualified as Map
import System.FilePath (takeDirectory)
import Test.Tasty
import Test.Tasty.Providers (IsTest (..), singleTest, testFailed)
import Test.Tasty.Providers.ConsoleFormat (noResultDetails)
import Test.Tasty.Runners (Outcome (..), Result (..))

import Test.HsBindgen.Golden (allTestCases)
import Test.HsBindgen.Golden.TestCase (TestCase (..))
import Test.HsBindgen.Resources
import Test.HsBindgen.THFixtures.Compile (setupBatchCompile, sanitizeLibName)
import Test.HsBindgen.THFixtures.Generate (generateModule)
import Test.HsBindgen.THFixtures.TestCases (THStatus (..), determineTHStatus)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests testResources =
    withResource setup (const $ return ()) $ \getResults ->
        testGroup "th-fixtures" $
            map (mkFixtureTest getResults) allTestCases
  where
    setup = do
        testResources' <- testResources
        let repoRoot = takeDirectory testResources'.packageRoot
            cases =
              [ (tc.name, generateModule testResources' tc)
              | tc <- allTestCases
              , determineTHStatus tc == THCompile
              ]
        setupBatchCompile repoRoot cases

mkFixtureTest :: IO (Map.Map String (Either String ())) -> TestCase -> TestTree
mkFixtureTest getResults tc = case determineTHStatus tc of
    THSkip reason ->
        singleTest tc.name $
          SimpleTest $ return $ skippedResult reason
    THCompile ->
        singleTest tc.name $ SimpleTest $ do
            results <- getResults
            let libName = "th-fixture-" ++ sanitizeLibName tc.name
            case Map.lookup libName results of
                Just (Right ()) -> return $ passedResult "TH compilation succeeded"
                Just (Left err) -> return $ testFailed $
                    "TH compilation failed:\n" ++ err
                Nothing -> return $ testFailed
                    "Library not found in batch build results"

newtype SimpleTest = SimpleTest (IO Result)

instance IsTest SimpleTest where
    run _ (SimpleTest action) _ = action
    testOptions = return []

passedResult :: String -> Result
passedResult msg = Result Success msg "OK" 0 noResultDetails

skippedResult :: String -> Result
skippedResult reason = Result Success ("Skipped: " ++ reason) ("Skipped: " ++ reason) 0 noResultDetails
