module Test.HsBindgen.Integration.PreprocessLibrary (tests) where

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
      testGroup "basic" [
          testBasicRun getTestResources
        , testSingleFileMode getTestResources
        ]
    , testGroup "library root" [
          testLibraryRootRestrictsScope getTestResources
        , testExceptLibraryRoot getTestResources
        , testDryRun getTestResources
        , testListModules getTestResources
        ]
    , testGroup "selection predicates" [
          testSelectByHeaderPath getTestResources
        , testSelectExceptByHeaderPath getTestResources
        , testCombinedSelectAndExclude getTestResources
        , testSelectMatchingNothing getTestResources
        ]
    , testGroup "collision detection" $
        if caseInsensitiveFS
        then []
        else [
          testCaseFoldingCollisionDetected getTestResources
        , testCaseFoldingCollisionResolvedByExclude getTestResources
        , testCollisionStillDetectedInSingleFile getTestResources
        ]
    ]

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

caseInsensitiveFS :: Bool
caseInsensitiveFS = os `elem` ["mingw32", "darwin"]

headerDir :: TestResources -> FilePath
headerDir root = root.packageRoot </> "test-artefacts" </> "headers"

-- | Run preprocess-library with --library-root pointing at the test headers
-- directory (same as the -I dir).
runPreprocessLibrary ::
     TestResources
  -> FilePath
  -> [String]
  -> IO (ExitCode, String, String)
runPreprocessLibrary root tmpDir extraArgs = do
    let hDir = headerDir root
    readProcessWithExitCode "hs-bindgen-cli"
      ([ "preprocess-library"
       , "-I", hDir
       , "--library-root", hDir
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
  Basic
-------------------------------------------------------------------------------}

testBasicRun :: IO TestResources -> TestTree
testBasicRun getTestResources =
    testCase "file-per-module generates category files" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        (exitCode, _stdout, stderr) <- runPreprocessLibrary root tmpDir []
        exitCode @?= ExitSuccess
        assertFilesExist stderr
          [ tmpDir </> "MyLib" </> "Mylib" </> "Types.hs"
          , tmpDir </> "MyLib" </> "Mylib" </> "Internal.hs"
          , tmpDir </> "MyLib" </> "Mylib" </> "Ops" </> "Safe.hs"
          , tmpDir </> "MyLib" </> "Mylib" </> "Safe.hs"
          ]

testSingleFileMode :: IO TestResources -> TestTree
testSingleFileMode getTestResources =
    testCase "single-file mode generates one file per header" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        (exitCode, _stdout, stderr) <- runPreprocessLibrary root tmpDir
          ["--single-file", "--safe", ""]
        exitCode @?= ExitSuccess
        assertFilesExist stderr
          [ tmpDir </> "MyLib" </> "Mylib" </> "Types.hs"
          , tmpDir </> "MyLib" </> "Mylib" </> "Ops.hs"
          , tmpDir </> "MyLib" </> "Mylib.hs"
          ]

{-------------------------------------------------------------------------------
  Library root

  --library-root and --except-library-root control module-generation scope:
  which headers get their own Haskell module. These are independent of
  selection predicates, which control which declarations get bindings.
-------------------------------------------------------------------------------}

testLibraryRootRestrictsScope :: IO TestResources -> TestTree
testLibraryRootRestrictsScope getTestResources =
    testCase "--library-root restricts which headers get modules" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        let hDir = headerDir root
        (exitCode, _stdout, stderr) <- readProcessWithExitCode "hs-bindgen-cli"
          [ "preprocess-library"
          , "-I", hDir
          , "--library-root", hDir </> "mylib"
          , "--module", "MyLib"
          , "--hs-output-dir", tmpDir
          , "--unique-id", "test-lr"
          , "--create-output-dirs"
          , "--overwrite-files"
          , hDir </> "mylib.h"
          ]
          ""
        exitCode @?= ExitSuccess
        assertFilesExist stderr
          [ tmpDir </> "MyLib" </> "Types.hs"
          , tmpDir </> "MyLib" </> "Ops" </> "Safe.hs"
          , tmpDir </> "MyLib" </> "Internal.hs"
          ]
        assertFilesAbsent stderr
          [ tmpDir </> "MyLib" </> "Mylib" </> "Types.hs"
          , tmpDir </> "MyLib" </> "Mylib.hs"
          ]

testExceptLibraryRoot :: IO TestResources -> TestTree
testExceptLibraryRoot getTestResources =
    testCase "--except-library-root excludes headers from module generation" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        (exitCode, _stdout, stderr) <- runPreprocessLibrary root tmpDir
          ["--except-library-root", "internal"]
        exitCode @?= ExitSuccess
        assertFilesExist stderr
          [ tmpDir </> "MyLib" </> "Mylib" </> "Types.hs"
          , tmpDir </> "MyLib" </> "Mylib" </> "Ops" </> "Safe.hs"
          , tmpDir </> "MyLib" </> "Mylib" </> "Safe.hs"
          ]
        assertFilesAbsent stderr
          [ tmpDir </> "MyLib" </> "Mylib" </> "Internal.hs"
          ]

testDryRun :: IO TestResources -> TestTree
testDryRun getTestResources =
    testCase "--dry-run prints plan without generating files" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        (exitCode, stdout, _stderr) <- runPreprocessLibrary root tmpDir
          ["--dry-run"]
        exitCode @?= ExitSuccess
        assertBool ("expected 'modules to generate' in stdout, got: " ++ stdout)
          ("modules to generate" `isInfixOf'` stdout)
        assertFilesAbsent "dry-run should not generate files"
          [ tmpDir </> "MyLib" </> "Mylib" </> "Types.hs"
          ]

testListModules :: IO TestResources -> TestTree
testListModules getTestResources =
    testCase "--list-modules prints module names" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        (exitCode, stdout, _stderr) <- runPreprocessLibrary root tmpDir
          ["--list-modules"]
        exitCode @?= ExitSuccess
        assertBool ("expected module name in stdout, got: " ++ stdout)
          ("MyLib.Mylib" `isInfixOf'` stdout)
        assertFilesAbsent "list-modules should not generate files"
          [ tmpDir </> "MyLib" </> "Mylib" </> "Types.hs"
          ]

{-------------------------------------------------------------------------------
  Selection predicates

  Selection predicates control which declarations get bindings within each
  module. They are independent of --library-root / --except-library-root,
  which control which headers get modules.

  When a selection predicate matches nothing for a given header, no .hs
  files are written for that module (writeByCategory skips empty
  categories), so the assertFilesAbsent checks remain valid.
-------------------------------------------------------------------------------}

testSelectByHeaderPath :: IO TestResources -> TestTree
testSelectByHeaderPath getTestResources =
    testCase "--select-by-header-path restricts which declarations get bindings" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        (exitCode, _stdout, stderr) <- runPreprocessLibrary root tmpDir
          ["--select-by-header-path", "types\\.h"]
        exitCode @?= ExitSuccess
        assertFilesExist stderr
          [ tmpDir </> "MyLib" </> "Mylib" </> "Types.hs"
          ]
        assertFilesAbsent stderr
          [ tmpDir </> "MyLib" </> "Mylib" </> "Ops" </> "Safe.hs"
          , tmpDir </> "MyLib" </> "Mylib" </> "Internal.hs"
          ]

testSelectExceptByHeaderPath :: IO TestResources -> TestTree
testSelectExceptByHeaderPath getTestResources =
    testCase "--select-except-by-header-path excludes declarations" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        (exitCode, _stdout, stderr) <- runPreprocessLibrary root tmpDir
          ["--select-except-by-header-path", "internal"]
        exitCode @?= ExitSuccess
        assertFilesExist stderr
          [ tmpDir </> "MyLib" </> "Mylib" </> "Types.hs"
          , tmpDir </> "MyLib" </> "Mylib" </> "Ops" </> "Safe.hs"
          , tmpDir </> "MyLib" </> "Mylib" </> "Safe.hs"
          ]
        assertFilesAbsent stderr
          [ tmpDir </> "MyLib" </> "Mylib" </> "Internal.hs"
          ]

testCombinedSelectAndExclude :: IO TestResources -> TestTree
testCombinedSelectAndExclude getTestResources =
    testCase "--select-by + --select-except narrow declarations" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        (exitCode, _stdout, stderr) <- runPreprocessLibrary root tmpDir
          [ "--select-by-header-path", "mylib/"
          , "--select-except-by-header-path", "internal"
          ]
        exitCode @?= ExitSuccess
        assertFilesExist stderr
          [ tmpDir </> "MyLib" </> "Mylib" </> "Types.hs"
          , tmpDir </> "MyLib" </> "Mylib" </> "Ops" </> "Safe.hs"
          ]
        assertFilesAbsent stderr
          [ tmpDir </> "MyLib" </> "Mylib" </> "Internal.hs"
          , tmpDir </> "MyLib" </> "Mylib" </> "Safe.hs"
          ]

testSelectMatchingNothing :: IO TestResources -> TestTree
testSelectMatchingNothing getTestResources =
    testCase "--select-by-header-path matching nothing produces no files" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        (exitCode, _stdout, _stderr) <- runPreprocessLibrary root tmpDir
          ["--select-by-header-path", "nonexistent"]
        exitCode @?= ExitSuccess
        assertFilesAbsent "no match"
          [ tmpDir </> "MyLib" </> "Mylib" </> "Types.hs"
          , tmpDir </> "MyLib" </> "Mylib" </> "Internal.hs"
          ]

{-------------------------------------------------------------------------------
  Collision detection
-------------------------------------------------------------------------------}

testCaseFoldingCollisionDetected :: IO TestResources -> TestTree
testCaseFoldingCollisionDetected getTestResources =
    testCase "detects case-folding collision and exits with error" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        let hDir = headerDir root
            collideDir = hDir </> "collide"
        (exitCode, stdout, _stderr) <- readProcessWithExitCode "hs-bindgen-cli"
          [ "preprocess-library"
          , "-I", collideDir
          , "--library-root", collideDir
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
          ("collision" `isInfixOf'` stdout)

testCaseFoldingCollisionResolvedByExclude :: IO TestResources -> TestTree
testCaseFoldingCollisionResolvedByExclude getTestResources =
    testCase "collision resolved by --except-library-root" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        let hDir = headerDir root
            collideDir = hDir </> "collide"
        (exitCode, _stdout, _stderr) <- readProcessWithExitCode "hs-bindgen-cli"
          [ "preprocess-library"
          , "-I", collideDir
          , "--library-root", collideDir
          , "--module", "C"
          , "--hs-output-dir", tmpDir
          , "--unique-id", "test-collision-fix"
          , "--create-output-dirs"
          , "--overwrite-files"
          , "--except-library-root", "Alpha\\.h"
          , collideDir </> "root.h"
          ]
          ""
        exitCode @?= ExitSuccess

testCollisionStillDetectedInSingleFile :: IO TestResources -> TestTree
testCollisionStillDetectedInSingleFile getTestResources =
    testCase "direct collision detected even in single-file mode" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        let hDir = headerDir root
            collideDir = hDir </> "collide"
        (exitCode, stdout, _stderr) <- readProcessWithExitCode "hs-bindgen-cli"
          [ "preprocess-library"
          , "-I", collideDir
          , "--library-root", collideDir
          , "--module", "C"
          , "--hs-output-dir", tmpDir
          , "--unique-id", "test-sf-collision"
          , "--create-output-dirs"
          , "--overwrite-files"
          , "--single-file", "--safe", ""
          , collideDir </> "root.h"
          ]
          ""
        assertBool ("expected exit code 4, got: " ++ show exitCode)
          (exitCode == ExitFailure 4)
        assertBool ("expected collision message, got: " ++ stdout)
          ("collision" `isInfixOf'` stdout)

isInfixOf' :: String -> String -> Bool
isInfixOf' needle haystack = any (isPrefixOf' needle) (tails' haystack)

isPrefixOf' :: String -> String -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys) = x == y && isPrefixOf' xs ys

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' xs@(_ : xs') = xs : tails' xs'
