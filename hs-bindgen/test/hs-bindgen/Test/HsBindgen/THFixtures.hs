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

import Test.HsBindgen.Resources
import Test.HsBindgen.THFixtures.Compile (compileThModule)
import Test.HsBindgen.THFixtures.Generate (generateModule)
import Test.HsBindgen.THFixtures.TestCases (THStatus (..), TestCaseInfo (..),
                                            allTestCases, specName)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests testResources =
    testGroup "th-fixtures" $
        map (mkFixtureTest testResources) allTestCases

mkFixtureTest :: IO TestResources -> TestCaseInfo -> TestTree
mkFixtureTest testResources tcInfo = case tcInfo.status of
    THSkip reason ->
        singleTest (specName tcInfo.spec) $
          SimpleTest $ return $ skippedResult reason
    THCompile ->
        singleTest (specName tcInfo.spec) $ SimpleTest $ do
            pkgRoot <- getTestPackageRoot testResources
            let moduleContent = generateModule pkgRoot tcInfo.spec
            result <- compileThModule pkgRoot (specName tcInfo.spec) moduleContent
            case result of
                Right () -> return $ passedResult "TH compilation succeeded"
                Left err -> return $ testFailed $ "TH compilation failed:\n" ++ err

newtype SimpleTest = SimpleTest (IO Result)

instance IsTest SimpleTest where
    run _ (SimpleTest action) _ = action
    testOptions = return []

passedResult :: String -> Result
passedResult msg = Result Success msg "Pass" 0 noResultDetails

skippedResult :: String -> Result
skippedResult reason = Result Success ("Skipped: " ++ reason) ("Skipped: " ++ reason) 0 noResultDetails
