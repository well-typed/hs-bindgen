module Test.HsBindgen.Integration.OverwritePolicy (tests) where

import System.Directory (createDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.HsBindgen.Resources
import Test.Tasty
import Test.Tasty.HUnit

tests :: IO TestResources -> TestTree
tests testResources = testGroup "Integration.OverwritePolicy" [
    testDirOverwritePolicy  testResources
  , testFileOverwritePolicy testResources
  , testOverwritePolicies   testResources
  ]

testDirOverwritePolicy :: IO TestResources -> TestTree
testDirOverwritePolicy testResources = testCase "do not create output directory by default" $ do
  withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
    root <- getTestPackageRoot testResources
    let headerPath = root </> "examples/golden/functions/simple_func.h"
    (exitCode, _, _) <- readProcessWithExitCode "hs-bindgen-cli"
                                               [ "preprocess"
                                               , "--hs-output-dir"
                                               , tmpDir
                                               , headerPath
                                               ]
                                               ""
    exitCode @?= ExitFailure 1

testFileOverwritePolicy :: IO TestResources -> TestTree
testFileOverwritePolicy testResources = testCase "do not overwrite existing files by default" $ do
  withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
    root <- getTestPackageRoot testResources
    let headerPath = root </> "examples/golden/functions/simple_func.h"
    -- Create a file that would be overwritten.
    createDirectory $ tmpDir </> "Generated"
    writeFile (tmpDir </> "Generated/Safe.hs") "Placeholder file"
    (exitCode, _, _) <- readProcessWithExitCode "hs-bindgen-cli"
                                               [ "preprocess"
                                               , "--create-output-dirs"
                                               , "--hs-output-dir"
                                               , tmpDir
                                               , headerPath
                                               ]
                                               ""
    exitCode @?= ExitFailure 1

testOverwritePolicies :: IO TestResources -> TestTree
testOverwritePolicies testResources = testCase "create directories and overwrite files if told by user" $ do
  withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
    root <- getTestPackageRoot testResources
    let headerPath         = root </> "examples/golden/functions/simple_func.h"
        placeholderPath    = tmpDir </> "Generated/Safe.hs"
        placeholderContent =  "Placeholder file"
    -- Create a file that will be overwritten.
    createDirectory $ tmpDir </> "Generated"
    writeFile placeholderPath placeholderContent
    (exitCode, _, _) <- readProcessWithExitCode "hs-bindgen-cli"
                                               [ "preprocess"
                                               , "--create-output-dirs"
                                               , "--overwrite-files"
                                               , "--hs-output-dir"
                                               , tmpDir
                                               , headerPath
                                               ]
                                               ""
    exitCode @?= ExitSuccess
    -- Check that the placeholder file content has changed.
    content <- readFile placeholderPath
    assertBool "placeholder content has changed" (content /= placeholderContent)
