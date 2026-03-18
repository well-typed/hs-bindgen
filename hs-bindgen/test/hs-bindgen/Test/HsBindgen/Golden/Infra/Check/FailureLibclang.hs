-- | Golden test: Failure (non-zero exit code)
--
-- Test for non-zero exit code
module Test.HsBindgen.Golden.Infra.Check.FailureLibclang (check) where

import Control.Exception (Exception (..), SomeException, handle, throw)
import Test.Tasty (TestTree, askOption)
import Test.Tasty.HUnit

import HsBindgen
import HsBindgen.Clang

import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Golden.Infra.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check getTestResources test =
    askOption $ \(Debug debug) -> do
      let report :: String -> IO ()
          report = case debug of
            False -> const $ pure ()
            True  -> putStrLn
      testCase test.name $
        handle expectLibclangFailure $ do
          eRes <- runTestHsBindgen report getTestResources test FinalDecls
          assertFailure $ mconcat [
              "expected 'hs-bindgen' to fail early, "
            , "but it finished with the following result:\n"
            , show eRes
            ]

  where
    expectLibclangFailure :: SomeException -> IO ()
    expectLibclangFailure e = case fromException e of
      Just (_ :: LibclangException)  -> pure ()
      _                              -> throw e
