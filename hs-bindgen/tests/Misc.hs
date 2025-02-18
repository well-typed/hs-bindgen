-- | Miscellaneous test utilities
module Misc (
    goldenVsStringDiff_,
    findPackageDirectory,
    clangArgs,
) where

import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as UTF8
import System.Directory (doesFileExist, setCurrentDirectory, getCurrentDirectory)
import System.FilePath ((</>), (-<.>))
import Test.Tasty (TestTree, TestName)

import AnsiDiff (ansidiff)
import TastyGolden
import HsBindgen.Clang.Paths
import HsBindgen.Lib

-------------------------------------------------------------------------------
-- tasty-golden wrapper
-------------------------------------------------------------------------------

-- | Like goldenVsString but using our own diff function.
goldenVsStringDiff_ :: TestName -> FilePath -> ((String -> IO ()) -> IO String) -> TestTree
goldenVsStringDiff_ name fp action = goldenTestSteps name correct action cmp update
  where
    correct :: IO String
    correct = do
        contents <- BS.readFile fp
        return $ UTF8.toString contents

    update :: String -> IO ()
    update s = BS.writeFile fp (UTF8.fromString s)

    cmp :: String -> String -> IO (Maybe String)
    cmp xss yss
        | xss == yss = return Nothing
        | otherwise  = return $ Just $ ansidiff xss yss

-------------------------------------------------------------------------------
-- package directory
-------------------------------------------------------------------------------

-- | In multi-package projects @cabal run test-suite@ will run the test-suite
-- from your current working directory (e.g. project root), which is often
-- not the package directory.
--
-- However, many tests are written so they assume that are run from
-- *package* directory.
findPackageDirectory :: String -> IO FilePath
findPackageDirectory pkgname = do
    here <- doesFileExist (pkgname -<.> ".cabal")
    if here
    then getCurrentDirectory
    else do
        there <- doesFileExist (pkgname </> pkgname -<.> ".cabal")
        if there
        then do
            setCurrentDirectory pkgname
            getCurrentDirectory
        -- do not try too hard, if not in the package directory, nor project root: abort
        else fail $ "Cannot find package directory for " ++ pkgname

-------------------------------------------------------------------------------
-- default ClangArgs used in tests
-------------------------------------------------------------------------------

clangArgs :: FilePath -> ClangArgs
clangArgs packageRoot = defaultClangArgs{
      clangTarget = Just (Target_Linux_X86_64, TargetEnvOverride "gnu")
    , clangCStandard = Just C23
    , clangSystemIncludePathDirs = [
          CIncludePathDir (packageRoot </> "musl-include/x86_64")
        ]
    , clangIncludePathDirs = [
          CIncludePathDir (packageRoot </> "examples")
        ]
    }
