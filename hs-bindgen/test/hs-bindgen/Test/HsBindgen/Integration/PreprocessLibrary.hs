module Test.HsBindgen.Integration.PreprocessLibrary (tests) where

import Data.List (sort)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests getTestResources = testGroup "Integration.PreprocessLibrary" [
      testDryRun      getTestResources
    , testListModules getTestResources
    ]

testDryRun :: IO TestResources -> TestTree
testDryRun getTestResources =
    testCase "--dry-run exits successfully" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- (.packageRoot) <$> getTestResources
        let headerDir = root </> "test-artefacts" </> "headers"
        (exitCode, _stdout, _stderr) <- readProcessWithExitCode "hs-bindgen-cli"
          [ "preprocess-library"
          , "-I" , headerDir
          , "--module", "MyLib"
          , "--hs-output-dir", tmpDir
          , "--dry-run"
          , headerDir </> "mylib.h"
          ]
          ""
        exitCode @?= ExitSuccess

testListModules :: IO TestResources -> TestTree
testListModules getTestResources =
    testCase "--list-modules produces expected modules" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- (.packageRoot) <$> getTestResources
        let headerDir = root </> "test-artefacts" </> "headers"
        (exitCode, stdout, _stderr) <- readProcessWithExitCode "hs-bindgen-cli"
          [ "preprocess-library"
          , "-I" , headerDir
          , "--module", "MyLib"
          , "--hs-output-dir", tmpDir
          , "--list-modules"
          , "--dry-run"
          , headerDir </> "mylib.h"
          ]
          ""
        exitCode @?= ExitSuccess
        let isModuleLine l = not (null l) && notElem ' ' l
            modules = sort $ filter isModuleLine $ lines stdout
        sort ["MyLib.Mylib", "MyLib.Mylib.Ops", "MyLib.Mylib.Types"]
          @?= modules
