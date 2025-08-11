-- | Golden test: failing test cases
--
-- For failing test cases, we verify the trace messages.
module Test.HsBindgen.Golden.Check.FailingTrace (check) where

import Control.Monad
import Test.Tasty (TestTree)
import Test.Tasty.HUnit

import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test = testCase (testName test) $ do
    -- We ignore any declarations that might have been successful
    void $ runTestBackend testResources test
