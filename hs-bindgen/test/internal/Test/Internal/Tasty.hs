module Test.Internal.Tasty (
    -- * Resources
    withTmpDir
  ) where

import Control.Exception (catch)
import System.Directory qualified as Dir
import System.IO.Temp qualified as Temp
import Test.Tasty (TestTree, withResource)

{-------------------------------------------------------------------------------
  Resources
-------------------------------------------------------------------------------}

-- | Create a temporary directory to use during a test tree and remove it
-- afterwards
withTmpDir :: (IO FilePath -> TestTree) -> TestTree
withTmpDir =
    withResource
      ( flip Temp.createTempDirectory "hs-bindgen-test-"
          =<< Temp.getCanonicalTemporaryDirectory
      )
      (ignoringIOErrors . Dir.removeDirectoryRecursive)
  where
    ignoringIOErrors :: IO () -> IO ()
    ignoringIOErrors ioe = ioe `catch` (\e -> const (return ()) (e :: IOError))
