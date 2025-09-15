-- | Golden test: rust-bindgen
module Test.HsBindgen.Golden.Check.Rust (check) where

import System.FilePath ((</>))
import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources
import Test.Tasty
import Test.Tasty.HUnit

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test =
    case testRustBindgen test of
      RustBindgenIgnore -> testGroup "rust" []
      RustBindgenFail   -> checkPanic testResources test
      RustBindgenRun    ->
        goldenAnsiDiff "rust" fixture $ \report -> do
          result <- runTestRustBindgen testResources test
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
    fixture :: FilePath
    fixture = "fixtures" </> (testName test ++ ".rs")

checkPanic :: IO TestResources -> TestCase -> TestTree
checkPanic testResources test =
    testCase "rust-panic" $ do
      result <- runTestRustBindgen testResources test
      case result of
        RustBindgenSuccess{} -> assertFailure "expected rust-bindgen to fail"
        RustBindgenFailed{}  -> return ()
        RustBindgenNotCalled -> return ()
