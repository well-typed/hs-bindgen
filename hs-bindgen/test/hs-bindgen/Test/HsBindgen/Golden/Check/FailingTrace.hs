{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE NoNamedFieldPuns  #-}
{-# LANGUAGE NoRecordWildCards #-}

-- | Golden test: failing test cases
--
-- For failing test cases, we verify the trace messages.
module Test.HsBindgen.Golden.Check.FailingTrace (check) where

import Control.Monad (void)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit

import HsBindgen

import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test = testCase (testName test) $
    -- We ignore any declarations that might have been successful
    void $ runTestHsBindgenFailure noReport testResources test FinalDecls
  where
    noReport :: a -> IO ()
    noReport = const $ pure ()
