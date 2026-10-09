-- | @hs-bindgen-cli preprocess --dry-run@
module Test.HsBindgen.Integration.DryRun (tests) where

import Control.Monad (forM_)
import Data.List (isInfixOf)
import System.Directory (createDirectory, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

import Test.HsBindgen.Resources

tests :: IO TestResources -> TestTree
tests getTestResources = testGroup "Integration.DryRun" [
      testReport      getTestResources
    , testFilePolicy  getTestResources
    , testDirPolicy   getTestResources
    ]

-- | A dry run reports every file and writes none
testReport :: IO TestResources -> TestTree
testReport getTestResources = testCase "report the files, write nothing" $
    withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
      (exitCode, out, err) <- dryRun getTestResources tmpDir ["--create-output-dirs"]
      exitCode @?= ExitSuccess
      forM_ ["Safe.hs", "Unsafe.hs", "FunPtr.hs"] $ \file -> do
        let path = tmpDir </> "Generated" </> file
        assertBool ("reports " ++ path) $ show path `isInfixOf` (out ++ err)
      written <- listDirectory tmpDir
      written @?= []

-- | A file that a real run would refuse to overwrite fails a dry run too
testFilePolicy :: IO TestResources -> TestTree
testFilePolicy getTestResources = testCase "do not overwrite existing files" $
    withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
      createDirectory $ tmpDir </> "Generated"
      writeFile (tmpDir </> "Generated" </> "Safe.hs") placeholder
      (exitCode, _, _) <- dryRun getTestResources tmpDir ["--create-output-dirs"]
      exitCode @?= ExitFailure 4
      content <- readFile (tmpDir </> "Generated" </> "Safe.hs")
      content @?= placeholder
  where
    placeholder :: String
    placeholder = "Placeholder file"

-- | A missing output directory fails a dry run as it fails a real run
testDirPolicy :: IO TestResources -> TestTree
testDirPolicy getTestResources = testCase "do not create output directory" $
    withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
      (exitCode, _, _) <- dryRun getTestResources (tmpDir </> "src") []
      exitCode @?= ExitFailure 4

-- | Run @preprocess --dry-run@ on a header with one function
dryRun ::
     IO TestResources
  -> FilePath  -- ^ Output directory
  -> [String]  -- ^ More options
  -> IO (ExitCode, String, String)
dryRun getTestResources outDir options = do
    root <- (.packageRoot) <$> getTestResources
    let headerPath =
          root </> "test-artefacts/headers/golden/functions/simple_func.h"
    readProcessWithExitCode
      "hs-bindgen-cli"
      (concat [
          ["preprocess", "--dry-run", "--hs-output-dir", outDir]
        , options
        , [headerPath]
        ])
      ""
