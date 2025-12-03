-- | Golden test: failing test cases
--
-- For failing test cases, we verify the trace messages.
module Test.HsBindgen.Golden.Check.FailingTrace (check) where

import Control.Exception (SomeException, handle)
import Control.Monad (void)
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources
import Test.Tasty (TestTree)
import Test.Tasty.HUnit

import HsBindgen

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test = testCase (testName test) $ do
    -- We ignore any declarations that might have been successful
    let artefact = FinalDecls
    -- Use 'runTestHsBindgen'' to avoid that expected error traces clutter the
    -- terminal.
    handle exceptionHandler $ void $
      runTestHsBindgen noReport testResources test artefact
  where
    exceptionHandler :: SomeException -> IO ()
    exceptionHandler _ = pure ()

    noReport :: a -> IO ()
    noReport = const $ pure ()
