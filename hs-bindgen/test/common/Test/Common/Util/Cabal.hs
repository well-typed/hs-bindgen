-- | Utilities for working with @cabal@
--
-- Intended for unqualified import.
module Test.Common.Util.Cabal (
    -- * Directories
    findPackageDirectory
  ) where

import System.Directory
import System.FilePath

{-------------------------------------------------------------------------------
  Directories
-------------------------------------------------------------------------------}

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

