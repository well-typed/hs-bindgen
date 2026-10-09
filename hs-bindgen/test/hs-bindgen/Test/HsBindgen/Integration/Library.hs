-- | @hs-bindgen-cli preprocess --library@
--
-- The modules themselves have golden tests in "Test.HsBindgen.Golden.Library".
-- The tests here are about the command: what it writes, what it refuses, and
-- how it exits.
module Test.HsBindgen.Integration.Library (tests) where

import Control.Monad (forM)
import Data.List (isInfixOf, sort)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

import Test.HsBindgen.Resources

tests :: IO TestResources -> TestTree
tests getTestResources = testGroup "Integration.Library" [
      testCase "one module for each header" $
        inOutputDir $ \outDir -> do
          (exitCode, _, _) <- preprocess getTestResources outDir forwardDecl []
          exitCode @?= ExitSuccess
          written <- listFiles outDir
          written @?= [
              "M" </> "A" </> "FunPtr.hs"
            , "M" </> "A" </> "Safe.hs"
            , "M" </> "A" </> "Unsafe.hs"
            , "M" </> "B.hs"
            ]
          safe <- readFile (outDir </> "M" </> "A" </> "Safe.hs")
          assertBool "M.A.Safe imports M.B" $
            "import qualified M.B" `isInfixOf` safe
    , testCase "one file for each header" $
        inOutputDir $ \outDir -> do
          (exitCode, _, _) <- preprocess getTestResources outDir forwardDecl
            ["--single-file", "--unsafe", ""]
          exitCode @?= ExitSuccess
          written <- listFiles outDir
          written @?= ["M" </> "A.hs", "M" </> "B.hs"]
    , testCase "a dry run writes nothing" $
        inOutputDir $ \outDir -> do
          (exitCode, _, _) <- preprocess getTestResources outDir forwardDecl
            ["--dry-run"]
          exitCode @?= ExitSuccess
          written <- listFiles outDir
          written @?= []
    , testGroup "usage errors" [
          testCase flag $
            inOutputDir $ \outDir -> do
              (exitCode, _, _) <-
                preprocess getTestResources outDir forwardDecl options
              exitCode @?= ExitFailure 2
              written <- listFiles outDir
              written @?= []
        | (flag, options) <- [
              ("--select-by-decl-name", ["--select-by-decl-name", "shape"])
            , ("--select-all", ["--select-all"])
            , ("--gen-binding-spec", ["--gen-binding-spec", "spec.yaml"])
            ]
        ]
    , testCase "a library directory that does not exist is a usage error" $
        inOutputDir $ \outDir -> do
          (exitCode, _, _) <- preprocess getTestResources outDir
            (\headers -> [headers </> "no_such_directory"]) []
          exitCode @?= ExitFailure 2
          written <- listFiles outDir
          written @?= []
    , testCase "no header in the library is an error" $
        inOutputDir $ \outDir -> do
          -- The directory exists, and the header that is read is not in it
          (exitCode, _, _) <- preprocess getTestResources outDir
            (\headers -> [headers </> "mylib"]) []
          exitCode @?= ExitFailure 4
          written <- listFiles outDir
          written @?= []
    , testCase "headers that use each other are an error" $
        inOutputDir $ \outDir ->
        withSystemTempDirectory "hs-bindgen-test" $ \headers -> do
          -- Each of the two structs has a pointer to the other
          writeFile (headers </> "a.h") $ unlines [
              "struct b;"
            , "struct a { struct b *b; };"
            ]
          writeFile (headers </> "b.h") $ unlines [
              "#include \"a.h\""
            , "struct b { struct a *a; };"
            ]
          (exitCode, out, err) <- run headers [headers] outDir [] "b.h"
          exitCode @?= ExitFailure 4
          assertBool "the error names the two headers" $
            all (`isInfixOf` (out ++ err)) ["a.h", "b.h"]
          written <- listFiles outDir
          written @?= []
    ]
  where
    inOutputDir :: (FilePath -> IO a) -> IO a
    inOutputDir = withSystemTempDirectory "hs-bindgen-test"

    -- The library directories, given the directory with the headers
    forwardDecl :: FilePath -> [FilePath]
    forwardDecl headers = [headers </> "forward_decl"]

-- | Run @preprocess --library@ on @forward_decl\/b.h@, with base module @M@
preprocess ::
     IO TestResources
  -> FilePath                  -- ^ Output directory
  -> (FilePath -> [FilePath])  -- ^ Library directories
  -> [String]                  -- ^ More options
  -> IO (ExitCode, String, String)
preprocess getTestResources outDir libraryDirs options = do
    root <- (.packageRoot) <$> getTestResources
    let headers = root </> "test-artefacts" </> "headers"
    run headers (libraryDirs headers) outDir options "forward_decl/b.h"

-- | Run @preprocess --library@ with base module @M@
run ::
     FilePath    -- ^ Directory on the include path
  -> [FilePath]  -- ^ Library directories
  -> FilePath    -- ^ Output directory
  -> [String]    -- ^ More options
  -> String      -- ^ The header to read
  -> IO (ExitCode, String, String)
run headers libraryDirs outDir options header =
    readProcessWithExitCode
      "hs-bindgen-cli"
      (concat [
          ["preprocess", "-I", headers, "--module", "M"]
        , concat [ ["--library", dir] | dir <- libraryDirs ]
        , ["--hs-output-dir", outDir]
        , options
        , [header]
        ])
      ""

-- | The files below a directory, with their paths below it, sorted
listFiles :: FilePath -> IO [FilePath]
listFiles dir = sort <$> go ""
  where
    go :: FilePath -> IO [FilePath]
    go below = do
        entries <- listDirectory (dir </> below)
        fmap concat $ forM entries $ \entry -> do
          let path = below </> entry
          isDir <- doesDirectoryExist (dir </> path)
          if isDir then go path else pure [path]
