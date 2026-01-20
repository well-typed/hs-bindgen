-- | Golden test: Failure (non-zero exit code)
--
-- Test for non-zero exit code
module Test.HsBindgen.Golden.Check.FailureLibclang (check) where

import Control.Exception (handle, throw)
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit

import HsBindgen

import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test = testCase test.name $
    handle expectExitFailure $ do
      eRes <- runTestHsBindgen noReport testResources test FinalDecls
      assertFailure $ mconcat [
          "expected hs-bindgen to fail early, "
        , "but it finished with the following result:\n"
        , show eRes
        ]

  where
    noReport :: a -> IO ()
    noReport = const $ pure ()

    expectExitFailure :: ExitCode -> IO ()
    expectExitFailure = \case
      -- We specifically test for exit code 2 here; it means that the
      -- `hs-bindgen` invocation of `libclang` has failed.
      ExitFailure 2  -> pure ()
      otherException -> throw otherException
