module Test.HsBindgen.Integration.PreprocessLibrary (tests) where

import Data.List (isInfixOf)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Info (os)
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
      testBasicRun getTestResources
    , testExceptLibrary getTestResources
    , testDryRun getTestResources
    , testSelectByHeaderPath getTestResources
    , if caseInsensitiveFS
      then testCase "collision detection (skipped: case-insensitive FS)" $
             pure ()
      else testCollisionDetected getTestResources
    ]

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | The collision detection algorithm itself is platform-independent (tested
-- by the unit tests). These integration tests are skipped on case-insensitive
-- filesystems because the test headers (alpha.h and Alpha.h) cannot coexist
-- there.
caseInsensitiveFS :: Bool
caseInsensitiveFS = os `elem` ["mingw32", "darwin"]

headerDir :: TestResources -> FilePath
headerDir root = root.packageRoot </> "test-artefacts" </> "headers"

runLibraryMode ::
     TestResources
  -> FilePath
  -> [String]
  -> IO (ExitCode, String, String)
runLibraryMode root tmpDir extraArgs = do
    let hDir = headerDir root
    readProcessWithExitCode "hs-bindgen-cli"
      ([ "preprocess"
       , "-I", hDir
       , "--library", hDir
       , "--module", "MyLib"
       , "--hs-output-dir", tmpDir
       , "--unique-id", "test-pl"
       , "--create-output-dirs"
       , "--overwrite-files"
       , hDir </> "mylib.h"
       ] ++ extraArgs)
      ""

assertFilesExist :: String -> [FilePath] -> IO ()
assertFilesExist label paths =
    mapM_ (\f -> do
      exists <- doesFileExist f
      assertBool (label ++ ": expected " ++ f) exists) paths

assertFilesAbsent :: String -> [FilePath] -> IO ()
assertFilesAbsent label paths =
    mapM_ (\f -> do
      exists <- doesFileExist f
      assertBool (label ++ ": unexpected " ++ f) (not exists)) paths

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

testBasicRun :: IO TestResources -> TestTree
testBasicRun getTestResources =
    testCase "generates one module per sub-header" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        (exitCode, _stdout, stderr) <- runLibraryMode root tmpDir []
        exitCode @?= ExitSuccess
        assertFilesExist stderr
          [ tmpDir </> "MyLib" </> "Mylib" </> "Types.hs"
          , tmpDir </> "MyLib" </> "Mylib" </> "Internal.hs"
          , tmpDir </> "MyLib" </> "Mylib" </> "Ops" </> "Safe.hs"
          ]

testExceptLibrary :: IO TestResources -> TestTree
testExceptLibrary getTestResources =
    testCase "--except-library excludes headers from module generation" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        (exitCode, _stdout, stderr) <- runLibraryMode root tmpDir
          ["--except-library", "internal"]
        exitCode @?= ExitSuccess
        assertFilesExist stderr
          [ tmpDir </> "MyLib" </> "Mylib" </> "Types.hs"
          ]
        assertFilesAbsent stderr
          [ tmpDir </> "MyLib" </> "Mylib" </> "Internal.hs"
          ]

testDryRun :: IO TestResources -> TestTree
testDryRun getTestResources =
    testCase "--dry-run prints plan without generating files" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        (exitCode, stdout, _stderr) <- runLibraryMode root tmpDir ["--dry-run"]
        exitCode @?= ExitSuccess
        assertBool ("expected 'modules to generate' in stdout, got: " ++ stdout)
          ("modules to generate" `isInfixOf` stdout)
        assertFilesAbsent "dry-run should not generate files"
          [ tmpDir </> "MyLib" </> "Mylib" </> "Types.hs"
          ]

testSelectByHeaderPath :: IO TestResources -> TestTree
testSelectByHeaderPath getTestResources =
    testCase "--select-by-header-path restricts declarations in library mode" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        (exitCode, _stdout, stderr) <- runLibraryMode root tmpDir
          ["--select-by-header-path", "types\\.h"]
        exitCode @?= ExitSuccess
        assertFilesExist stderr
          [ tmpDir </> "MyLib" </> "Mylib" </> "Types.hs"
          ]
        assertFilesAbsent stderr
          [ tmpDir </> "MyLib" </> "Mylib" </> "Ops" </> "Safe.hs"
          , tmpDir </> "MyLib" </> "Mylib" </> "Internal.hs"
          ]

testCollisionDetected :: IO TestResources -> TestTree
testCollisionDetected getTestResources =
    testCase "detects case-folding collision and exits with error" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        let collideDir = headerDir root </> "collide"
        (exitCode, stdout, _stderr) <- readProcessWithExitCode "hs-bindgen-cli"
          [ "preprocess"
          , "-I", collideDir
          , "--library", collideDir
          , "--module", "C"
          , "--hs-output-dir", tmpDir
          , "--unique-id", "test-collision"
          , "--create-output-dirs"
          , "--overwrite-files"
          , collideDir </> "root.h"
          ]
          ""
        assertBool ("expected exit code 4, got: " ++ show exitCode)
          (exitCode == ExitFailure 4)
        assertBool ("expected collision message, got: " ++ stdout)
          ("collision" `isInfixOf` stdout)
