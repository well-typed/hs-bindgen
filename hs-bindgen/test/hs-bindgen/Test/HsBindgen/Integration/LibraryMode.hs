module Test.HsBindgen.Integration.LibraryMode (tests) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.List (isInfixOf)
import System.Directory (doesFileExist)
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
tests getTestResources = testGroup "Integration.LibraryMode" [
      testRun getTestResources
    , testCrossModuleReference getTestResources
    , testUsageErrors getTestResources
    ]

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

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
  Generating
-------------------------------------------------------------------------------}

testRun :: IO TestResources -> TestTree
testRun getTestResources =
    testCase "generates one module per sub-header" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        (exitCode, _stdout, stderr) <- runLibraryMode root tmpDir []
        assertEqual stderr ExitSuccess exitCode
        assertFilesExist stderr
          [ tmpDir </> "MyLib" </> "Mylib" </> "Types.hs"
          , tmpDir </> "MyLib" </> "Mylib" </> "Internal.hs"
          , tmpDir </> "MyLib" </> "Mylib" </> "Ops" </> "Safe.hs"
          ]

-- | b.h includes a.h, but a.h uses the struct that b.h defines
testCrossModuleReference :: IO TestResources -> TestTree
testCrossModuleReference getTestResources =
    testCase "a type lands in the module of the header that defines it" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        (exitCode, _stdout, stderr) <- runLibraryModeIn
          (headerDir root </> "forward_decl") "b.h" tmpDir []
        exitCode @?= ExitSuccess
        let safeModule = tmpDir </> "M" </> "A" </> "Safe.hs"
        assertFilesExist stderr [tmpDir </> "M" </> "B.hs", safeModule]
        -- a.h only declares the struct forward, so it has no types module
        assertFilesAbsent stderr [tmpDir </> "M" </> "A.hs"]
        contents <- readFileStrict safeModule
        assertBool "expected shape_draw to use the struct from M.B" $
          "M.B.Shape" `isInfixOf` contents

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

testUsageErrors :: IO TestResources -> TestTree
testUsageErrors getTestResources =
    testCase "flags that do not go together are usage errors" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        let hDir = headerDir root
        forM_ (cases tmpDir) $ \(args, message) -> do
          (exitCode, stdout, _stderr) <- readProcessWithExitCode "hs-bindgen-cli"
            ([ "preprocess"
             , "-I", hDir
             , "--module", "MyLib"
             , "--hs-output-dir", tmpDir
             ] ++ args ++ [hDir </> "mylib.h"])
            ""
          assertEqual (unwords args) (ExitFailure 2) exitCode
          assertBool ("expected " ++ show message ++ ", got: " ++ stdout) $
            message `isInfixOf` stdout
  where
    cases :: FilePath -> [([String], String)]
    cases tmpDir = [
        -- No header is under a directory that does not exist, so a mistyped
        -- directory would give a run that generates nothing and succeeds
        ( ["--library", tmpDir </> "no-such-directory"]
        , "--library is not a directory"
        )
      ]
