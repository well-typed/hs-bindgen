module HsBindgen.Clang.Paths (
    -- * C include search path directories
    CIncludePathDir(..)
  , CIncludeAbsPathDir
  , getCIncludeAbsPathDir
  , resolveCIncludeAbsPathDirs
    -- * C header paths
  , CHeaderRelPath
  , mkCHeaderRelPath
  , getCHeaderRelPath
  , CHeaderAbsPath
  , mkCHeaderAbsPath
  , getCHeaderAbsPath
  , resolveHeader
  ) where

import Control.Monad (forM, unless)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory qualified as Dir
import System.FilePath qualified as FilePath

{-------------------------------------------------------------------------------
  C include search path directories
-------------------------------------------------------------------------------}

-- | C include search path directory
--
-- This type represents a path specified by a user that may be absolute or
-- relative to the current working directory.
newtype CIncludePathDir = CIncludePathDir { getCIncludePathDir :: FilePath }
  deriving newtype (Eq, Ord, Show)

-- | C include search path directory (absolute)
newtype CIncludeAbsPathDir = CIncludeAbsPathDir {
      getCIncludeAbsPathDir :: FilePath
    }
  deriving newtype (Eq, Ord, Show)

-- | Resolve any number of 'CIncludePathDir'
--
-- This function fails if any path is not found or is not a directory.
resolveCIncludeAbsPathDirs :: [CIncludePathDir] -> IO [CIncludeAbsPathDir]
resolveCIncludeAbsPathDirs paths = do
    cwd <- Dir.getCurrentDirectory
    forM (getCIncludePathDir <$> paths) $ \path -> do
      let absPath
            | FilePath.isAbsolute path = path
            | otherwise                = cwd FilePath.</> path
      exists <- Dir.doesDirectoryExist absPath
      unless exists $
        fail ("include path not found or not a directory: " ++ path)
      return $ CIncludeAbsPathDir absPath

{-------------------------------------------------------------------------------
  C header paths
-------------------------------------------------------------------------------}

-- | C header path (relative)
--
-- A value must be specified as used in the C source code (relative to an
-- include directory).
--
-- Example: @time.h@
newtype CHeaderRelPath = CHeaderRelPath Text
  deriving newtype (Eq, Ord, Show)

-- | Construct a 'CHeaderRelPath'
--
-- This funtion fails if the path is not relative.  It does /not/ check
-- existence, readability, or if it points to a file or not.
mkCHeaderRelPath :: MonadFail m => Text -> m CHeaderRelPath
mkCHeaderRelPath t
    | FilePath.isRelative path = return $ CHeaderRelPath t
    | otherwise = fail $ "C header path not relative: " ++ path
  where
    path :: FilePath
    path = Text.unpack t

-- | Get the 'FilePath' representation of a 'CHeaderRelPath'
getCHeaderRelPath :: CHeaderRelPath -> FilePath
getCHeaderRelPath (CHeaderRelPath t) = Text.unpack t

-- | C header path (absolute)
--
-- Example: @/usr/include/time.h@
newtype CHeaderAbsPath = CHeaderAbsPath Text
  deriving newtype (Eq, Ord, Show)

-- | Construct a 'CHeaderAbsPath'
--
-- This function fails if the path is not absolute.  It does /not/ check
-- existence, readability, or if it points to a file or not.
mkCHeaderAbsPath :: MonadFail m => Text -> m CHeaderAbsPath
mkCHeaderAbsPath t
    | FilePath.isAbsolute path = return $ CHeaderAbsPath t
    | otherwise = fail $ "C header path is not absolute: " ++ path
  where
    path :: FilePath
    path = Text.unpack t

-- | Get the 'FilePath' representation of a 'CHeaderAbsPath'
getCHeaderAbsPath :: CHeaderAbsPath -> FilePath
getCHeaderAbsPath (CHeaderAbsPath t) = Text.unpack t

-- | Resolve a single 'CHeaderRelPath'
--
-- This function fails if the header is not found in the include search path.
resolveHeader ::
     [CIncludePathDir]
  -> CHeaderRelPath
  -> IO CHeaderAbsPath
resolveHeader includeDirs relPath = do
    let includeDirs' = getCIncludePathDir <$> includeDirs
        relPath'     = getCHeaderRelPath relPath
    mAbsPath <- Dir.findFile includeDirs' relPath'
    case mAbsPath of
      Just absPath -> return $ CHeaderAbsPath (Text.pack absPath)
      Nothing -> fail $ "header not found in include search path: " ++ relPath'
