-- | Golden test: the files a library-mode run writes
--
-- Which files a library run writes is only known after the run, so a test
-- compares the whole golden directory at once: the set of files and their
-- contents. Accepting the output rewrites the directory, and deletes the files
-- that the run no longer writes.
module Test.HsBindgen.Golden.Infra.Check.Library (
    check
    -- * Reused by equivalence check
  , Files
  , readFiles
  , compareFiles
  ) where

import Control.Exception (catch)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as UTF8
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                         listDirectory, removeDirectory, removeFile)
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (DependencyType (..), TestTree, after, askOption)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

import HsBindgen
import HsBindgen.Imports
import HsBindgen.Library (OutputFile (..), PlanError)

import Test.Common.Util.AnsiDiff (ansidiff)
import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Golden.Infra.LibraryTest
import Test.HsBindgen.Golden.Infra.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> LibraryTest -> TestTree
check getTestResources libraryTest' =
    afterDependencies $
      case libraryTest'.outcome of
        WritesGolden ->
          checkGolden getTestResources libraryTest'
        RejectedByPlan isExpected ->
          checkRejected getTestResources libraryTest' isExpected
  where
    -- A library can read the binding specifications in the golden directory
    -- of another library test. It runs after that test, so that it reads the
    -- new specifications when both accept new output.
    afterDependencies :: TestTree -> TestTree
    afterDependencies tree = foldr (after AllFinish . goldenTestPattern) tree
      libraryTest'.dependencies

    -- The golden test of a library test, and not its fixture compilation,
    -- which has the same name in another test group
    goldenTestPattern :: LibraryTest -> String
    goldenTestPattern dependency = concat [
          "$NF == ", show dependency.test.name
        , " && $(NF - 1) == \"library\""
        ]

-- | The run writes exactly the files of the golden directory
--
-- The runner itself checks that it generates the files of the library plan. It
-- reports a difference as a bug, which the trace predicate rejects.
checkGolden :: IO TestResources -> LibraryTest -> TestTree
checkGolden getTestResources libraryTest' =
    goldenTestSteps
      libraryTest'.test.name
      (readFiles =<< goldenDir)
      getActual
      (\golden actual -> pure (compareFiles golden actual))
      (\actual -> goldenDir >>= \dir -> writeFiles dir actual)
      (goldenDir >>= \dir -> writeFiles dir Map.empty)
  where
    goldenDir :: IO FilePath
    goldenDir = do
        testResources <- getTestResources
        pure $ testResources.packageRoot </> libraryTest'.test.outputDir

    getActual :: (String -> IO ()) -> IO (ActualValue Files)
    getActual report =
        withSystemTempDirectory "hs-bindgen-library" $ \outputDir -> do
          let bindingSpecDir = (outputDir </>) <$> libraryTest'.bindingSpecDir
          result <-
            runTestHsBindgenLibrary
              report
              getTestResources
              libraryTest'.test
              libraryTest'.libraryDirs
              outputDir
              bindingSpecDir
          case result of
            Left err ->
              pure $ ActualFailed $
                "Expected 'hs-bindgen' to succeed, but received an error: "
                  ++ show err
            Right _planned ->
              ActualValue <$> readFiles outputDir

-- | The plan rejects the run with the expected error, and nothing is written,
-- not even an output directory
checkRejected ::
     IO TestResources
  -> LibraryTest
  -> (PlanError -> Bool)
  -> TestTree
checkRejected getTestResources libraryTest' isExpected =
    askOption $ \(Debug debug) ->
    testCase libraryTest'.test.name $
      withSystemTempDirectory "hs-bindgen-library" $ \tmpDir -> do
        let outputDir      = tmpDir </> "out"
            bindingSpecDir = (outputDir </>) <$> libraryTest'.bindingSpecDir
            report         = if debug then putStrLn else const (pure ())
        result <-
          runTestHsBindgenLibrary
            report
            getTestResources
            libraryTest'.test
            libraryTest'.libraryDirs
            outputDir
            bindingSpecDir
        case result of
          Left (BindgenPlanError err)
            | isExpected err -> pure ()
          Left err ->
            assertFailure $ "Expected another plan error, but received: "
              ++ show err
          Right planned ->
            assertFailure $ "Expected a plan error, but the run planned: "
              ++ show (map (.path) planned)
        wroteSomething <- doesDirectoryExist outputDir
        assertBool "The rejected run created its output directory" $
          not wroteSomething

{-------------------------------------------------------------------------------
  Golden directories
-------------------------------------------------------------------------------}

-- | The contents of each file in a directory, by path relative to it
type Files = Map FilePath String

-- | Read every file under a directory, with LF line endings
--
-- Throws a does-not-exist error if the directory does not exist, so that a
-- missing golden directory counts as a missing golden value.
readFiles :: FilePath -> IO Files
readFiles dir = do
    paths <- listFilesUnder dir
    fmap Map.fromList $ forM paths $ \path -> do
      -- Read strictly: accepting the output may overwrite the file next.
      contents <- BS.readFile (dir </> path)
      pure (path, normaliseLineEndings (UTF8.toString contents))

-- | Turn CRLF line endings into LF
--
-- On Windows, @hs-bindgen@ writes the generated modules in text mode, so their
-- lines end with CRLF. The golden files end their lines with LF.
normaliseLineEndings :: String -> String
normaliseLineEndings ('\r' : '\n' : rest) = '\n' : normaliseLineEndings rest
normaliseLineEndings (c : rest)           = c : normaliseLineEndings rest
normaliseLineEndings []                   = []

-- | Make a directory hold exactly the given files
--
-- Deletes the files that are not given, and the directories left empty.
writeFiles :: FilePath -> Files -> IO ()
writeFiles dir files = do
    old <- listFilesUnder dir `catchDoesNotExist` []
    forM_ (filter (`Map.notMember` files) old) $ \path ->
      removeFile (dir </> path)
    removeEmptySubdirectories dir `catchDoesNotExist` ()
    forM_ (Map.toList files) $ \(path, contents) -> do
      createDirectoryIfMissing True (takeDirectory (dir </> path))
      BS.writeFile (dir </> path) (UTF8.fromString contents)
  where
    catchDoesNotExist :: IO a -> a -> IO a
    catchDoesNotExist action fallback =
        action `catch` \e ->
          if isDoesNotExistError e then pure fallback else throwIO e

-- | Report the files added, removed and changed, or 'Nothing' if none is
--
-- A changed file shows its diff, and an added file shows its contents. A
-- removed file is still in the golden directory, so the report names it only.
compareFiles :: Files -> Files -> Maybe String
compareFiles golden actual
  | golden == actual = Nothing
  | otherwise        = Just $ List.intercalate "\n" $ concat [
        [ "No longer written: " ++ path
        | path <- Map.keys (golden `Map.difference` actual)
        ]
      , [ "Newly written: " ++ path ++ "\n" ++ ansidiff "" new
        | (path, new) <- Map.toList (actual `Map.difference` golden)
        ]
      , [ "Changed: " ++ path ++ "\n" ++ ansidiff old new
        | (path, (old, new)) <-
            Map.toList (Map.intersectionWith (,) golden actual)
        , old /= new
        ]
      ]

-- | The files under a directory, relative to it, in a stable order
listFilesUnder :: FilePath -> IO [FilePath]
listFilesUnder root = List.sort . map (makeRelative root) <$> go root
  where
    go :: FilePath -> IO [FilePath]
    go dir = do
        entries <- listDirectory dir
        fmap concat $ forM entries $ \entry -> do
          let path = dir </> entry
          isDir <- doesDirectoryExist path
          if isDir then go path else pure [path]

-- | Remove the subdirectories that hold no file, bottom up
removeEmptySubdirectories :: FilePath -> IO ()
removeEmptySubdirectories root = do
    entries <- listDirectory root
    forM_ entries $ \entry -> do
      let path = root </> entry
      isDir <- doesDirectoryExist path
      when isDir $ do
        removeEmptySubdirectories path
        remaining <- listDirectory path
        when (null remaining) $ removeDirectory path
