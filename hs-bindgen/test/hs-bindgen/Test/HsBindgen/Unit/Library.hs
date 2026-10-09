-- | Library headers and the names of their modules
module Test.HsBindgen.Unit.Library (tests) where

import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import System.Directory (canonicalizePath, createDirectoryIfMissing,
                         getCurrentDirectory)
import System.FilePath (dropDrive, joinPath, splitDirectories, takeDrive, (</>))
import System.FilePath.Posix qualified as Posix
import System.FilePath.Windows qualified as Windows
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import Clang.Paths (RealPath (..))

import HsBindgen.Config.Prelims (BaseModuleName)
import HsBindgen.Library (Library (..), LibraryPath (..), libraryPaths,
                          moduleName, pathBelow)

{-------------------------------------------------------------------------------
  List of tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.HsBindgen.Unit.Library" [
      testGroup "pathBelow" [
          testGroup "posix" [
              testCase "a file in the directory" $
                posix ["/usr/include/lib"] "/usr/include/lib/a.h"
                  @?= Just (LibraryPath ("a.h" :| []))
            , testCase "a file further down" $
                posix ["/usr/include/lib"] "/usr/include/lib/sub/a.h"
                  @?= Just (LibraryPath ("sub" :| ["a.h"]))
            , testCase "a file elsewhere" $
                posix ["/usr/include/lib"] "/usr/include/other/a.h"
                  @?= Nothing
            , testCase "a sibling directory with the same prefix" $
                posix ["/usr/include/lib"] "/usr/include/lib2/a.h"
                  @?= Nothing
            , testCase "the directory itself" $
                posix ["/usr/include/lib"] "/usr/include/lib"
                  @?= Nothing
            , testCase "a directory written with a trailing separator" $
                posix ["/usr/include/lib/"] "/usr/include/lib/a.h"
                  @?= Just (LibraryPath ("a.h" :| []))
            , testCase "names that differ in case are different" $
                posix ["/usr/include/Lib"] "/usr/include/lib/a.h"
                  @?= Nothing
            , testCase "of nested directories the outermost counts" $
                posix
                  ["/usr/include/lib/sub", "/usr/include/lib"]
                  "/usr/include/lib/sub/a.h"
                  @?= Just (LibraryPath ("sub" :| ["a.h"]))
            , testCase "of several directories the one that has the file" $
                posix ["/opt/one", "/opt/two"] "/opt/two/a.h"
                  @?= Just (LibraryPath ("a.h" :| []))
            ]
        , testGroup "windows" [
              testCase "a file further down" $
                windows ["C:\\include\\lib"] "C:\\include\\lib\\sub\\a.h"
                  @?= Just (LibraryPath ("sub" :| ["a.h"]))
            , testCase "names that differ in case are the same" $
                windows ["c:\\Include\\LIB"] "C:\\include\\lib\\a.h"
                  @?= Just (LibraryPath ("a.h" :| []))
            , testCase "either separator" $
                windows ["C:/include/lib"] "C:\\include\\lib\\a.h"
                  @?= Just (LibraryPath ("a.h" :| []))
            , testCase "another drive" $
                windows ["D:\\include\\lib"] "C:\\include\\lib\\a.h"
                  @?= Nothing
            , testCase "a sibling directory with the same prefix" $
                windows ["C:\\include\\lib"] "C:\\include\\lib2\\a.h"
                  @?= Nothing
            ]
        ]
    , testGroup "libraryPaths" [
          testCase "however the directory is written" $
            inTree $ \root -> do
              let header = realPath (root </> "lib" </> "sub" </> "a.h")
                  found  = Map.fromList
                    [(header, LibraryPath ("sub" :| ["a.h"]))]
              upToRoot <- relativeToCurrentDirectory root
              forM_ [
                  root </> "lib"
                , root </> "lib" ++ "/"
                , root </> "lib" </> "sub" </> ".."
                , root </> "other" </> ".." </> "lib"
                , upToRoot </> "lib"
                ] $ \dir ->
                libraryPaths (library dir) [header] >>= (@?= found)
        , testCase "a header outside the directory is not a library header" $
            inTree $ \root -> do
              let header = realPath (root </> "other" </> "b.h")
              libraryPaths (library (root </> "lib")) [header]
                >>= (@?= Map.empty)
        , testCase "a directory that does not exist has no headers" $
            inTree $ \root -> do
              let header = realPath (root </> "lib" </> "sub" </> "a.h")
              libraryPaths (library (root </> "missing")) [header]
                >>= (@?= Map.empty)
        ]
    , testGroup "moduleName" [
          testCase "a header" $
            ["types.h"] `isNamed` Just "Lib.Types"
        , testCase "a header in a directory" $
            ["mylib", "types.h"] `isNamed` Just "Lib.Mylib.Types"
        , testCase "only the last extension goes" $
            ["foo.inc.h"] `isNamed` Just "Lib.Foo_inc"
        , testCase "a header without extension" $
            ["vector"] `isNamed` Just "Lib.Vector"
        , testCase "a character that cannot be in a module name" $
            ["foo-bar.h"] `isNamed` Just "Lib.Foo_bar"
        , testCase "a name that starts with a digit" $
            ["3d.h"] `isNamed` Just "Lib.C3D"
        , testCase "a name that starts with an underscore" $
            ["_private.h"] `isNamed` Just "Lib.C_Private"
        , testCase "a directory with a dot" $
            ["v1.2", "api.h"] `isNamed` Just "Lib.V1_2.Api"
        , testCase "a header with nothing before the extension" $
            [".h"] `isNamed` Nothing
        ]
    ]
  where
    posix, windows :: [FilePath] -> FilePath -> Maybe LibraryPath
    posix   = pathBelow Posix.splitDirectories   Posix.equalFilePath
    windows = pathBelow Windows.splitDirectories Windows.equalFilePath

    library :: FilePath -> Library
    library dir = Library{dirs = dir :| []}

    realPath :: FilePath -> RealPath
    realPath = RealPath . Text.pack

-- | The name of the module for the header with these components
isNamed :: [String] -> Maybe BaseModuleName -> Assertion
isNamed components expected = case components of
    []     -> fail "a library path has a component"
    c : cs -> moduleName "Lib" (LibraryPath (c :| cs)) @?= expected

-- | Run an action in a directory with @lib\/sub\/a.h@ and @other\/b.h@
inTree :: (FilePath -> IO a) -> IO a
inTree k = withSystemTempDirectory "hs-bindgen-test" $ \root -> do
    createDirectoryIfMissing True (root </> "lib" </> "sub")
    createDirectoryIfMissing True (root </> "other")
    writeFile (root </> "lib" </> "sub" </> "a.h") ""
    writeFile (root </> "other" </> "b.h") ""
    k root

-- | A path to the directory that starts from the current directory
--
-- It goes up to the root of the file system and down from there.  If the
-- directory is on another drive, there is no such path, and the result is the
-- directory as it was given.
relativeToCurrentDirectory :: FilePath -> IO FilePath
relativeToCurrentDirectory dir = do
    current <- canonicalizePath =<< getCurrentDirectory
    target  <- canonicalizePath dir
    pure $
      if takeDrive current == takeDrive target
        then joinPath (map (const "..") (splitDirectories (dropDrive current)))
               </> dropDrive target
        else dir
