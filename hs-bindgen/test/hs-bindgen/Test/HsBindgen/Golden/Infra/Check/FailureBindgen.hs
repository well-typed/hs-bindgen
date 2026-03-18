-- | Golden test: failing test cases
--
-- For failing test cases, we verify the trace messages.
module Test.HsBindgen.Golden.Infra.Check.FailureBindgen (check) where

import Test.Tasty (TestTree, askOption)
import Test.Tasty.HUnit

import HsBindgen

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
      testCase test.name $ do
        eRes <- runTestHsBindgen report getTestResources test FinalDecls
        case eRes of
          Left  _ -> pure ()
          Right r -> assertFailure (msgWith r)
  where
    msgWith :: Show b => b -> String
    msgWith r = mconcat [
        "Expected 'hs-bindgen' to fail, "
      , "but it succeeded with the following list of declarations:\n"
      , show r
      ]
