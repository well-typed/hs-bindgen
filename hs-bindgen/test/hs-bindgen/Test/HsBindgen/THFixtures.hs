{-# LANGUAGE OverloadedRecordDot #-}

-- | Test suite to verify that TH-generated bindings compile successfully
--
-- This test suite generates TH modules for each golden test fixture and
-- compiles them with GHC, verifying that the TH output produces valid Haskell.
--
module Test.HsBindgen.THFixtures (tests) where

import Test.Tasty
import Test.Tasty.Providers (IsTest (..), singleTest, testFailed)
import Test.Tasty.Providers.ConsoleFormat (noResultDetails)
import Test.Tasty.Runners (Outcome (..), Result (..))

import Test.HsBindgen.Golden (allTestCases)
import Test.HsBindgen.Golden.TestCase (TestCase (..))
import Test.HsBindgen.Resources
import Test.HsBindgen.THFixtures.Compile (compileThModule)
import Test.HsBindgen.THFixtures.Generate (generateModule)
import Test.HsBindgen.THFixtures.TestCases (THStatus (..), determineTHStatus)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests testResources =
    testGroup "th-fixtures" $
        map (mkFixtureTest testResources) allTestCases

mkFixtureTest :: IO TestResources -> TestCase -> TestTree
mkFixtureTest testResources tc = case determineTHStatus tc of
    THSkip reason ->
        singleTest tc.name $
          SimpleTest $ return $ skippedResult reason
    THCompile ->
        singleTest tc.name $ SimpleTest $ do
            pkgRoot <- getTestPackageRoot testResources
            let moduleContent = generateModule pkgRoot tc
            result <- compileThModule pkgRoot tc.name moduleContent
            case result of
                Right () -> return $ passedResult "TH compilation succeeded"
                Left err -> return $ testFailed $ "TH compilation failed:\n" ++ err

newtype SimpleTest = SimpleTest (IO Result)

instance IsTest SimpleTest where
    run _ (SimpleTest action) _ = action
    testOptions = return []

passedResult :: String -> Result
passedResult msg = Result Success msg "OK" 0 noResultDetails

skippedResult :: String -> Result
skippedResult reason = Result Success ("Skipped: " ++ reason) ("Skipped: " ++ reason) 0 noResultDetails
