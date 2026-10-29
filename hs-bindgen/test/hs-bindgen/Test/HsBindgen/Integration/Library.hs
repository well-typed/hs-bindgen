-- | @hs-bindgen-cli preprocess --library@: exit codes and the dry run
module Test.HsBindgen.Integration.Library (tests) where

import Data.List (isInfixOf)
import System.Directory (createDirectory, doesDirectoryExist)
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
tests getTestResources = testGroup "Integration.Library" [
      testGroup "usage errors exit with 2" [
          usageError getTestResources "a library directory that does not exist"
            "no such directory" $ \_ out -> [
                "--library", out </> "no-such-dir", "root.h"
              ]
        , usageError getTestResources "--single-file"
            "--single-file is not available" $ \lib _ -> [
                "--library", lib, "--single-file", "--safe", "Safe", "root.h"
              ]
        , usageError getTestResources "--gen-binding-spec"
            "--gen-binding-spec is not available" $ \lib out -> [
                "--library", lib, "--gen-binding-spec", out </> "spec.yaml"
              , "root.h"
              ]
        , usageError getTestResources "--prescriptive-binding-spec"
            "--prescriptive-binding-spec is not available" $ \lib out -> [
                "--library", lib, "--prescriptive-binding-spec", out </> "spec.yaml"
              , "root.h"
              ]
        ]
    , testGroup "errors exit with 4 and write nothing" [
          libraryError getTestResources "two headers with one module name"
            "name_clash" "module names are equal"
        , libraryError getTestResources "two modules that want one file"
            "file_clash" "files whose paths are equal"
        , testCase "no header read is in the library" $
            withOutputDir $ \out -> do
              let empty = out </> "empty"
              createDirectory empty
              (exitCode, stdout, _stderr) <-
                runPreprocess getTestResources "points" $
                  ["--library", empty] ++ outputArgs out ++ ["root.h"]
              exitCode @?= ExitFailure 4
              assertMentions "None of the headers read" stdout
              written <- doesDirectoryExist (out </> "hs")
              written @?= False
        ]
    , testCase "a dry run writes nothing and prints one line per module" $
        withOutputDir $ \out -> do
          lib <- libraryDir getTestResources "points"
          (exitCode, stdout, _stderr) <-
            runPreprocess getTestResources "points" $
              ["--library", lib, "--module", "Lib", "--dry-run"]
                ++ outputArgs out ++ ["root.h"]
          exitCode @?= ExitSuccess
          lines stdout @?= [
              "Lib.Geo.Point: point.h"
            , "Lib.Geo.Shape: geo/shape.h"
            , "Lib.Ui.Point: ui/point.h"
            ]
          written <- doesDirectoryExist (out </> "hs")
          written @?= False
    ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | A run that @preprocess@ refuses before it reads a header
usageError ::
     IO TestResources
  -> TestName
  -> String                                -- ^ Part of the message
  -> (FilePath -> FilePath -> [String])    -- ^ Arguments, given the library and an output directory
  -> TestTree
usageError getTestResources name message args = testCase name $
    withOutputDir $ \out -> do
      lib <- libraryDir getTestResources "points"
      (exitCode, _stdout, stderr) <-
        runPreprocess getTestResources "points" $ args lib out ++ outputArgs out
      exitCode @?= ExitFailure 2
      assertMentions message stderr

-- | A library that @preprocess@ refuses after planning it
libraryError ::
     IO TestResources
  -> TestName
  -> FilePath -- ^ Library, under @test-artefacts\/headers\/library@
  -> String   -- ^ Part of the message
  -> TestTree
libraryError getTestResources name library message = testCase name $
    withOutputDir $ \out -> do
      lib <- libraryDir getTestResources library
      (exitCode, stdout, _stderr) <-
        runPreprocess getTestResources library $
          ["--library", lib] ++ outputArgs out ++ ["root.h"]
      exitCode @?= ExitFailure 4
      assertMentions message stdout
      written <- doesDirectoryExist (out </> "hs")
      written @?= False

-- | Run @hs-bindgen-cli preprocess@, with the library as include directory
runPreprocess ::
     IO TestResources
  -> FilePath -- ^ Library, under @test-artefacts\/headers\/library@
  -> [String]
  -> IO (ExitCode, String, String)
runPreprocess getTestResources library args = do
    lib <- libraryDir getTestResources library
    readProcessWithExitCode "hs-bindgen-cli" ("preprocess" : "-I" : lib : args) ""

libraryDir :: IO TestResources -> FilePath -> IO FilePath
libraryDir getTestResources library = do
    root <- (.packageRoot) <$> getTestResources
    pure $ root </> "test-artefacts" </> "headers" </> "library" </> library

-- | Write into a directory that does not exist yet
outputArgs :: FilePath -> [String]
outputArgs out = ["--hs-output-dir", out </> "hs", "--create-output-dirs"]

withOutputDir :: (FilePath -> IO a) -> IO a
withOutputDir = withSystemTempDirectory "hs-bindgen-library"

assertMentions :: String -> String -> Assertion
assertMentions expected actual =
    assertBool ("Expected the output to mention " ++ show expected ++ ":\n" ++ actual) $
      expected `isInfixOf` actual
