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
    , testModule
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

-- | A dry run prints the module with the headers that it is generated for
--
-- With @--library@ there is a line for each module; those are tested in
-- "Test.HsBindgen.Integration.Library".
testModule :: TestTree
testModule = testGroup "the module, as it is reported" [
      testCase "the module with its header" $
        inDirs $ \headers outDir -> do
          (exitCode, out, _) <- preprocess headers outDir ["--dry-run"] ["a.h"]
          exitCode @?= ExitSuccess
          lines out @?= ["M: a.h"]
          written <- listDirectory outDir
          written @?= []
    , testCase "the headers that are given are one module" $
        inDirs $ \headers outDir -> do
          (exitCode, out, _) <-
            preprocess headers outDir ["--dry-run"] ["a.h", "b.h"]
          exitCode @?= ExitSuccess
          lines out @?= ["M: a.h, b.h"]
    , testCase "a header that is included and not given" $
        inDirs $ \headers outDir -> do
          -- b.h includes a.h, and only the declarations of the given header
          -- are selected
          (exitCode, out, _) <- preprocess headers outDir ["--dry-run"] ["b.h"]
          exitCode @?= ExitSuccess
          lines out @?= ["M: b.h"]
    , testCase "a selection that takes in the header that is included" $
        inDirs $ \headers outDir -> do
          -- The headers are in the order in which they are included
          (exitCode, out, _) <- preprocess headers outDir
            ["--dry-run", "--select-from-all-headers"] ["b.h"]
          exitCode @?= ExitSuccess
          lines out @?= ["M: a.h, b.h"]
    , testCase "a declaration that gets no binding does not count" $
        inDirs $ \headers outDir -> do
          -- The struct of uses.h has a field of the struct of a.h, which is
          -- not selected, so it gets no binding
          (exitCode, out, _) <-
            preprocess headers outDir ["--dry-run"] ["b.h", "uses.h"]
          exitCode @?= ExitSuccess
          lines out @?= ["M: b.h"]
    , testCase "a module that is not written is not printed" $
        inDirs $ \headers outDir -> do
          -- guard.h has a macro and nothing that gets a binding
          (exitCode, out, _) <-
            preprocess headers outDir ["--dry-run"] ["guard.h"]
          exitCode @?= ExitSuccess
          out @?= ""
    , testCase "nothing is printed without --dry-run" $
        inDirs $ \headers outDir -> do
          (exitCode, out, _) <- preprocess headers outDir [] ["a.h"]
          exitCode @?= ExitSuccess
          out @?= ""
          written <- listDirectory outDir
          written @?= ["M.hs"]
    ]
  where
    -- A directory with headers and an output directory
    inDirs :: (FilePath -> FilePath -> IO a) -> IO a
    inDirs k =
        withSystemTempDirectory "hs-bindgen-test" $ \headers ->
        withSystemTempDirectory "hs-bindgen-test" $ \outDir -> do
          writeFile (headers </> "a.h") $ unlines [
              "#pragma once"
            , "struct a { int x; };"
            ]
          writeFile (headers </> "b.h") $ unlines [
              "#pragma once"
            , "#include \"a.h\""
            , "struct b { int y; };"
            ]
          writeFile (headers </> "uses.h") $ unlines [
              "#pragma once"
            , "#include \"a.h\""
            , "struct uses { struct a first; };"
            ]
          writeFile (headers </> "guard.h") $ unlines [
              "#ifndef GUARD_H"
            , "#define GUARD_H"
            , "#endif"
            ]
          k headers outDir

    -- Run @preprocess@ with base module @M@ on headers of the directory
    preprocess ::
         FilePath  -- ^ Directory on the include path
      -> FilePath  -- ^ Output directory
      -> [String]  -- ^ More options
      -> [String]  -- ^ The headers to read
      -> IO (ExitCode, String, String)
    preprocess headers outDir options given =
        readProcessWithExitCode
          "hs-bindgen-cli"
          (concat [
              ["preprocess", "-I", headers, "--module", "M"]
            , ["--hs-output-dir", outDir]
            , options
            , given
            ])
          ""

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
