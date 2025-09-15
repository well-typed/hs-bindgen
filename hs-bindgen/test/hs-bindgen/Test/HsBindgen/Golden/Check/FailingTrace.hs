-- | Golden test: failing test cases
--
-- For failing test cases, we verify the trace messages.
module Test.HsBindgen.Golden.Check.FailingTrace (check) where

import Control.Exception (Exception (..), SomeException (..), handle, throwIO)
import Control.Monad (void)
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources
import Test.Tasty (TestTree)
import Test.Tasty.HUnit

import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

import HsBindgen

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test = testCase (testName test) $ do
    -- We ignore any declarations that might have been successful
    let artefacts = FinalDecls :* Nil
    -- Use 'runTestHsBindgen'' to avoid that expected error traces clutter the
    -- terminal.
    handle exceptionHandler $ void $ runTestHsBindgen' testResources test artefacts
  where
    exceptionHandler :: SomeException -> IO ()
    exceptionHandler e@(SomeException e')
      | Just (TraceException @TraceMsg _) <- fromException e =
          pure ()
      | otherwise = do
          putStrLn $ "Other exception: " ++ displayException e'
          throwIO e'
