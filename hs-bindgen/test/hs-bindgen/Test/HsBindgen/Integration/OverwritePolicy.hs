module Test.HsBindgen.Integration.OverwritePolicy (tests) where

import System.Directory (createDirectory, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

import Test.HsBindgen.Resources

tests :: IO TestResources -> TestTree
tests getTestResources = testGroup "Integration.OverwritePolicy" [
    testDirPolicy                 getTestResources
  , testFilePolicy                getTestResources
  , testOverwritePolicies         getTestResources
  , testBindingSpecCreateDirs     getTestResources
  , testBindingSpecNoDirByDefault getTestResources
  ]

testDirPolicy :: IO TestResources -> TestTree
testDirPolicy getTestResources = testCase "do not create output directory by default" $ do
  withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
    root <- (.packageRoot) <$> getTestResources
    let headerPath = root </> "examples/golden/functions/simple_func.h"
        outDir = tmpDir </> "src"
    (exitCode, _, _) <- readProcessWithExitCode "hs-bindgen-cli"
                                               [ "preprocess"
                                               , "--hs-output-dir"
                                               , outDir
                                               , headerPath
                                               ]
                                               ""
    -- We specifically test for exit code 3 here; it means that `hs-bindgen` ran
    -- to completion, but an error has ocurred.
    exitCode @?= ExitFailure 3

testFilePolicy :: IO TestResources -> TestTree
testFilePolicy getTestResources = testCase "do not overwrite existing files by default" $ do
  withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
    root <- (.packageRoot) <$> getTestResources
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
    -- We specifically test for exit code 3 here; it means that `hs-bindgen` ran
    -- to completion, but an error has ocurred.
    exitCode @?= ExitFailure 3

testOverwritePolicies :: IO TestResources -> TestTree
testOverwritePolicies getTestResources = testCase "create directories and overwrite files if told by user" $ do
  withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
    root <- (.packageRoot) <$> getTestResources
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

testBindingSpecCreateDirs :: IO TestResources -> TestTree
testBindingSpecCreateDirs getTestResources = testCase "--create-output-dirs creates dirs for --gen-binding-spec" $ do
  withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
    root <- (.packageRoot) <$> getTestResources
    let headerPath      = root </> "examples/golden/functions/simple_func.h"
        bindingSpecPath = tmpDir </> "specs" </> "binding-spec.yaml"
    (exitCode, _, _) <- readProcessWithExitCode "hs-bindgen-cli"
                                               [ "preprocess"
                                               , "--create-output-dirs"
                                               , "--hs-output-dir"
                                               , tmpDir
                                               , "--gen-binding-spec"
                                               , bindingSpecPath
                                               , headerPath
                                               ]
                                               ""
    exitCode @?= ExitSuccess
    exists <- doesFileExist bindingSpecPath
    assertBool "binding spec file was created" exists

testBindingSpecNoDirByDefault :: IO TestResources -> TestTree
testBindingSpecNoDirByDefault getTestResources = testCase "do not create --gen-binding-spec directory by default" $ do
  withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
    root <- (.packageRoot) <$> getTestResources
    let headerPath      = root </> "examples/golden/functions/simple_func.h"
        bindingSpecPath = tmpDir </> "specs" </> "binding-spec.yaml"
    (exitCode, _, _) <- readProcessWithExitCode "hs-bindgen-cli"
                                               [ "preprocess"
                                               , "--hs-output-dir"
                                               , tmpDir
                                               , "--gen-binding-spec"
                                               , bindingSpecPath
                                               , headerPath
                                               ]
                                               ""
    exitCode @?= ExitFailure 3
