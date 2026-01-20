-- | Golden test: failing test cases
--
-- For failing test cases, we verify the trace messages.
module Test.HsBindgen.Golden.Check.FailureBindgen (check) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit

import HsBindgen

import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test = testCase test.name $ do
    eRes <- runTestHsBindgen noReport testResources test FinalDecls
    case eRes of
      Left  _ -> pure ()
      Right r -> assertFailure (msgWith r)
  where
    noReport :: a -> IO ()
    noReport = const $ pure ()

    msgWith :: Show b => b -> String
    msgWith r = mconcat [
        "Expected 'hs-bindgen' to fail, "
      , "but it succeeded with the following list of declarations:\n"
      , show r
      ]
