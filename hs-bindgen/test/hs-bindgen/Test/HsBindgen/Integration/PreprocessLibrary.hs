module Test.HsBindgen.Integration.PreprocessLibrary (tests) where

import Control.Exception (evaluate)
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
    , testGenBindingSpecDir getTestResources
    , testHeaderSelectionRejected getTestResources
    , testIncludeCycle getTestResources
    , testNoDeclarations getTestResources
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

-- | Library mode over one directory of headers, with base module @M@
runLibraryModeIn ::
     FilePath  -- ^ Header directory, also the @--library@ directory
  -> FilePath  -- ^ Root header, relative to that directory
  -> FilePath  -- ^ Output directory
  -> [String]
  -> IO (ExitCode, String, String)
runLibraryModeIn dir header tmpDir extraArgs =
    readProcessWithExitCode "hs-bindgen-cli"
      ([ "preprocess"
       , "-I", dir
       , "--library", dir
       , "--module", "M"
       , "--hs-output-dir", tmpDir
       , "--unique-id", "test-pl"
       , "--create-output-dirs"
       , "--overwrite-files"
       , dir </> header
       ] ++ extraArgs)
      ""

assertFilesExist :: String -> [FilePath] -> IO ()
assertFilesExist label paths =
    mapM_ (\f -> do
      exists <- doesFileExist f
      assertBool (label ++ ": expected " ++ f) exists) paths

-- | Read a whole file, so no handle is left open when the directory is removed
readFileStrict :: FilePath -> IO String
readFileStrict path = do
    contents <- readFile path
    _ <- evaluate (length contents)
    pure contents

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

testGenBindingSpecDir :: IO TestResources -> TestTree
testGenBindingSpecDir getTestResources =
    testCase "--gen-binding-spec-dir keeps one binding spec per module" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        let specDir = tmpDir </> "specs"
        (exitCode, _stdout, stderr) <- runLibraryMode root tmpDir
          ["--gen-binding-spec-dir", specDir]
        exitCode @?= ExitSuccess
        assertFilesExist stderr
          [ specDir </> "MyLib" </> "Mylib.yaml"
          , specDir </> "MyLib" </> "Mylib" </> "Types.yaml"
          , specDir </> "MyLib" </> "Mylib" </> "Ops.yaml"
          , specDir </> "MyLib" </> "Mylib" </> "Internal.yaml"
          ]

testHeaderSelectionRejected :: IO TestResources -> TestTree
testHeaderSelectionRejected getTestResources =
    testCase "header selection predicates are rejected in library mode" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        (exitCode, stdout, _stderr) <- runLibraryMode root tmpDir
          ["--select-from-main-headers"]
        exitCode @?= ExitFailure 2
        assertBool ("expected an error, got: " ++ stdout) $
          "header selection predicates" `isInfixOf` stdout

testIncludeCycle :: IO TestResources -> TestTree
testIncludeCycle getTestResources =
    testCase "headers that include each other share one module" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        (exitCode, _stdout, stderr) <- runLibraryModeIn
          (headerDir root </> "golden" </> "program-analysis")
          "circular_includes.h" tmpDir []
        exitCode @?= ExitSuccess
        let unitModule =
              tmpDir </> "M" </> "Circular_includes_Circular_includes_inner.hs"
        assertFilesExist stderr [unitModule]
        assertFilesAbsent stderr
          [ tmpDir </> "M" </> "Circular_includes.hs"
          , tmpDir </> "M" </> "Circular_includes_inner.hs"
          ]
        contents <- readFileStrict unitModule
        assertBool "expected declarations from circular_includes.h" $
          "OUTER_BEFORE_CIRCULAR_INCLUDE" `isInfixOf` contents
        assertBool "expected declarations from circular_includes_inner.h" $
          "INNER_BEFORE_CIRCULAR_INCLUDE" `isInfixOf` contents

testNoDeclarations :: IO TestResources -> TestTree
testNoDeclarations getTestResources =
    testCase "headers that declare nothing get no module" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        (exitCode, stdout, _stderr) <- runLibraryModeIn
          (headerDir root </> "golden" </> "macros" </> "parse")
          "macro_typedef_scope_multiple.h" tmpDir ["--list-base-module-names"]
        exitCode @?= ExitSuccess
        lines stdout @?=
          [ "M.Macro_typedef_scope_multiple_inner1"
          , "M.Macro_typedef_scope_multiple_inner2"
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
