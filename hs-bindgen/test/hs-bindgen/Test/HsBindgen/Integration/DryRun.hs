-- | @preprocess --dry-run@, in single-header mode and in library mode
--
-- A dry run writes nothing: no file and no directory, also with
-- @--create-output-dirs@. It prints what the real run would generate, and it
-- checks the output policies the way the real run does: a missing output
-- directory without @--create-output-dirs@, or an existing file without
-- @--overwrite-files@, fails with the same exit code and message. Errors that
-- only writing reveals, such as a read-only directory, are out of reach of a
-- dry run, so these tests do not cover them.
module Test.HsBindgen.Integration.DryRun (tests) where

import Control.Monad (forM)
import Data.List qualified as List
import System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                         listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests getTestResources = testGroup "Integration.DryRun" [
      testGroup "single-header" [
          testCase "lists the module and its headers, writes nothing" $
            listsWithoutWriting singleHeader getTestResources
        , testCase "an existing file fails as in the real run" $
            existingFile singleHeader getTestResources
        , testCase "a missing output directory fails as in the real run" $
            missingDirectory singleHeader getTestResources
        ]
    , testGroup "library" [
          testCase "lists the files of the real run, writes nothing" $
            listsWithoutWriting library getTestResources
        , testCase "an existing file fails as in the real run" $
            existingFile library getTestResources
        , testCase "a missing output directory fails as in the real run" $
            missingDirectory library getTestResources
        , testCase "a plan error fails as in the real run" $
            planError getTestResources
        ]
    ]

{-------------------------------------------------------------------------------
  Runs
-------------------------------------------------------------------------------}

-- | How a test runs @preprocess@ in one mode
data Mode = Mode {
      -- | The arguments, given the package root, the Haskell output directory
      -- and the binding specification output
      args    :: FilePath -> FilePath -> FilePath -> [String]
      -- | The binding specification output, given the temporary directory:
      -- the file of @--gen-binding-spec@ or the directory of
      -- @--gen-binding-spec-dir@
    , specOut :: FilePath -> FilePath
      -- | An output file to create beforehand, given the binding
      -- specification output
    , specFile :: FilePath -> FilePath
      -- | The stdout of a successful dry run, given the Haskell output
      -- directory and the binding specification output
    , listing  :: FilePath -> FilePath -> [String]
      -- | The files the real run writes, given the same directories
    , written  :: FilePath -> FilePath -> [FilePath]
    }

-- | Single-header mode over two headers: one module for both
singleHeader :: Mode
singleHeader = Mode{
      args = \root hsDir specPath -> [
          "--module", "SingleHeader"
        , "-I", root </> headersDir
        , "--hs-output-dir", hsDir
        , "--gen-binding-spec", specPath
        , "boot_files/a.h"
        , "boot_files/b.h"
        ]
    , specOut  = (</> "specs" </> "spec.yaml")
    , specFile = id
    , listing  = \_hsDir _specPath ->
        ["SingleHeader: boot_files/a.h, boot_files/b.h"]
    , written  = \hsDir specPath ->
           map (hsDir </>) [
               "SingleHeader.hs"
             , "SingleHeader" </> "FunPtr.hs"
             , "SingleHeader" </> "Safe.hs"
             , "SingleHeader" </> "Unsafe.hs"
             ]
        ++ [specPath]
    }

-- | Library mode over the headers of the @cycles/boot_files@ golden test: one
-- line per file, binding specifications included
library :: Mode
library = Mode{
      args = \root hsDir specDir -> [
          "--module", "BootFiles"
        , "--library", root </> headersDir </> "boot_files"
        , "-I", root </> headersDir
        , "--hs-output-dir", hsDir
        , "--gen-binding-spec-dir", specDir
        , "boot_files.h"
        ]
    , specOut  = (</> "specs")
    , specFile = (</> "BootFiles.A.bindingspec.yaml")
    , listing  = libraryFiles
    , written  = libraryFiles
    }
  where
    -- In the order of the listing, which sorts the paths
    libraryFiles :: FilePath -> FilePath -> [FilePath]
    libraryFiles hsDir specDir =
           map ((hsDir </> "BootFiles") </>) [
               "A.hs"
             , "A.hs-boot"
             , "A" </> "FunPtr.hs"
             , "A" </> "Safe.hs"
             , "A" </> "Unsafe.hs"
             , "B.hs"
             , "B.hs-boot"
             , "B" </> "FunPtr.hs"
             , "B" </> "Safe.hs"
             , "B" </> "Unsafe.hs"
             ]
        ++ map (specDir </>) [
               "BootFiles.A.bindingspec.yaml"
             , "BootFiles.B.bindingspec.yaml"
             ]

headersDir :: FilePath
headersDir = "test-artefacts" </> "headers" </> "library" </> "cycles"

-- | Run @hs-bindgen-cli preprocess@, and return its exit code and stdout
--
-- Errors go to stdout too, so stdout carries the message of a failed run.
preprocess :: [String] -> IO (ExitCode, String)
preprocess args = do
    (exitCode, out, _err) <-
      readProcessWithExitCode "hs-bindgen-cli" ("preprocess" : args) ""
    pure (exitCode, out)

{-------------------------------------------------------------------------------
  Checks
-------------------------------------------------------------------------------}

-- | A dry run prints the listing and writes nothing, even with
-- @--create-output-dirs@. In library mode, the listing names exactly the files
-- that the real run then writes.
listsWithoutWriting :: Mode -> IO TestResources -> Assertion
listsWithoutWriting mode getTestResources =
    withSystemTempDirectory "hs-bindgen-dry-run" $ \tmpDir -> do
      root <- (.packageRoot) <$> getTestResources
      let hsDir   = tmpDir </> "hs"
          specOut = mode.specOut tmpDir
          args    = "--create-output-dirs" : mode.args root hsDir specOut

      dryRun <- preprocess ("--dry-run" : args)
      dryRun @?= (ExitSuccess, unlines (mode.listing hsDir specOut))
      assertWroteNothing tmpDir []

      realRun <- fst <$> preprocess args
      realRun @?= ExitSuccess
      files <- filesUnder tmpDir
      assertEqual "the files the real run wrote"
        (List.sort (mode.written hsDir specOut))
        (map (tmpDir </>) files)

-- | An existing output file without @--overwrite-files@ fails the dry run with
-- the exit code and message of the real run. With the flag, the dry run
-- succeeds and leaves the file as it was.
existingFile :: Mode -> IO TestResources -> Assertion
existingFile mode getTestResources =
    withSystemTempDirectory "hs-bindgen-dry-run" $ \tmpDir -> do
      root <- (.packageRoot) <$> getTestResources
      let hsDir    = tmpDir </> "hs"
          specOut  = mode.specOut tmpDir
          existing = mode.specFile specOut
          args     = "--create-output-dirs" : mode.args root hsDir specOut
      createDirectoryIfMissing True (takeDirectory existing)
      writeFile existing "placeholder"
      before <- entriesUnder tmpDir

      dryRun@(_, dryOut) <- preprocess ("--dry-run" : args)
      fst dryRun @?= ExitFailure 4
      take 1 (lines dryOut) @?= ["Output file already exists: " ++ existing]
      realRun <- preprocess args
      realRun @?= dryRun
      assertWroteNothing tmpDir before

      overwrite <- preprocess ("--dry-run" : "--overwrite-files" : args)
      overwrite @?= (ExitSuccess, unlines (mode.listing hsDir specOut))
      assertWroteNothing tmpDir before
      contents <- readFile existing
      contents @?= "placeholder"

-- | A missing output directory without @--create-output-dirs@ fails the dry
-- run with the exit code and message of the real run. With the flag, the dry
-- run succeeds and creates nothing.
missingDirectory :: Mode -> IO TestResources -> Assertion
missingDirectory mode getTestResources =
    withSystemTempDirectory "hs-bindgen-dry-run" $ \tmpDir -> do
      root <- (.packageRoot) <$> getTestResources
      let hsDir   = tmpDir </> "hs"
          specOut = mode.specOut tmpDir
          args    = mode.args root hsDir specOut

      dryRun@(_, dryOut) <- preprocess ("--dry-run" : args)
      fst dryRun @?= ExitFailure 4
      take 1 (lines dryOut) @?= ["Output directory does not exist: " ++ hsDir]
      realRun <- preprocess args
      realRun @?= dryRun
      assertWroteNothing tmpDir []

      create <- preprocess ("--dry-run" : "--create-output-dirs" : args)
      create @?= (ExitSuccess, unlines (mode.listing hsDir specOut))
      assertWroteNothing tmpDir []

-- | A library plan error fails the dry run with the exit code and message of
-- the real run, and neither run writes anything
planError :: IO TestResources -> Assertion
planError getTestResources =
    withSystemTempDirectory "hs-bindgen-dry-run" $ \tmpDir -> do
      root <- (.packageRoot) <$> getTestResources
      let errorsDir = root </> "test-artefacts" </> "headers" </> "library" </> "errors"
          args = [
              "--module", "ModuleNameCollision"
            , "--library", errorsDir </> "module_name_collision"
            , "-I", errorsDir
            , "--hs-output-dir", tmpDir </> "hs"
            , "--gen-binding-spec-dir", tmpDir </> "specs"
            , "--create-output-dirs"
            , "module_name_collision.h"
            ]

      dryRun@(_, dryOut) <- preprocess ("--dry-run" : args)
      fst dryRun @?= ExitFailure 4
      assertBool ("reports the collision: " ++ dryOut) $
        "Module name collision:" `List.isPrefixOf` dryOut
      realRun <- preprocess args
      realRun @?= dryRun
      assertWroteNothing tmpDir []

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | The files and directories under a directory, relative to it, sorted
entriesUnder :: FilePath -> IO [FilePath]
entriesUnder root = List.sort . map fst <$> walk root

-- | The files under a directory, relative to it, sorted
filesUnder :: FilePath -> IO [FilePath]
filesUnder root = List.sort . map fst . filter (not . snd) <$> walk root

-- | Each entry under a directory, relative to it, and whether it is a
-- directory
walk :: FilePath -> IO [(FilePath, Bool)]
walk root = go ""
  where
    go :: FilePath -> IO [(FilePath, Bool)]
    go relative = do
        entries <- listDirectory (root </> relative)
        fmap concat $ forM entries $ \entry -> do
          let path = if null relative then entry else relative </> entry
          isDir <- doesDirectoryExist (root </> path)
          if isDir then ((path, True) :) <$> go path else pure [(path, False)]

-- | Nothing but the given entries lies under the directory
assertWroteNothing :: FilePath -> [FilePath] -> Assertion
assertWroteNothing dir before = do
    entries <- entriesUnder dir
    assertEqual "files and directories after the run" before entries
