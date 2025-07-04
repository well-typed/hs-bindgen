-- | Golden test: rust-bindgen
module Test.HsBindgen.Golden.Check.Rust (check) where

import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test
  | testRustBindgenFails test
  = checkPanic testResources test

  | otherwise
  = goldenAnsiDiff "rust" fixture $ \report -> do
      result <- callRustBindgen testResources input
      case result of
        RustBindgenSuccess stdout ->
          return $ ActualValue stdout
        RustBindgenFailed exitCode stderr -> do
          report $ "Exit code: " <> show exitCode
          report $ "stderr: "    <> stderr
          return $ ActualFailed "rust-bindgen failed"
        RustBindgenNotCalled ->
          return $ ActualSkipped "rust-bindgen not available"
  where
    input, fixture :: FilePath
    input   = "examples" </> "golden" </> (testName test ++ ".h")
    fixture = "fixtures" </> (testName test ++ ".rs")

checkPanic :: IO TestResources -> TestCase -> TestTree
checkPanic testResources test =
    testCase "rust-panic" $ do
      result <- callRustBindgen testResources input
      case result of
        RustBindgenSuccess{} -> assertFailure "expected rust-bindgen to fail"
        RustBindgenFailed{}  -> return ()
        RustBindgenNotCalled -> return ()
  where
    input :: FilePath
    input = "examples" </> "golden" </> (testName test ++ ".h")
