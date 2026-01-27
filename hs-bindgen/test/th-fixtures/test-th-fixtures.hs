{-# LANGUAGE OverloadedRecordDot #-}

-- | Test suite to verify that TH-generated bindings compile successfully
--
-- This test suite generates TH modules for each golden test fixture and
-- compiles them with GHC, verifying that the TH output produces valid Haskell.
--
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.Common.Util.Cabal (findPackageDirectory)
import Test.THFixtures.Compile (compileThModule)
import Test.THFixtures.Generate (generateModule)
import Test.THFixtures.TestCases (THStatus (..), TestCaseInfo (..),
                                  allTestCases, specName)

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    pkgRoot <- findPackageDirectory "hs-bindgen"
    defaultMain $ testGroup "test-th-fixtures" $
        map (mkFixtureTest pkgRoot) allTestCases

mkFixtureTest :: FilePath -> TestCaseInfo -> TestTree
mkFixtureTest pkgRoot tcInfo = case tcInfo.status of
    THSkip _reason ->
        testCase (specName tcInfo.spec) $
            return ()  -- Skipped tests pass immediately
    THCompile ->
        testCase (specName tcInfo.spec) $ do
            let moduleContent = generateModule pkgRoot tcInfo.spec
            result <- compileThModule pkgRoot (specName tcInfo.spec) moduleContent
            case result of
                Right () -> return ()
                Left err -> assertFailure $ "TH compilation failed:\n" ++ err
