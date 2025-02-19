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
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory qualified as Dir
import System.FilePath qualified as FilePath
import System.FilePath.Posix qualified as Posix

{-------------------------------------------------------------------------------
  C include search path directories
-------------------------------------------------------------------------------}

-- | C include search path directory
--
-- This type represents a path specified by a user that may be absolute or
-- relative to the current working directory.
newtype CIncludePathDir = CIncludePathDir { getCIncludePathDir :: FilePath }
  deriving newtype (Eq, Ord, Show)

instance IsString CIncludePathDir where
  fromString = CIncludePathDir

-- | C include search path directory (absolute)
newtype CIncludeAbsPathDir = CIncludeAbsPathDir {
      getCIncludeAbsPathDir :: FilePath
    }
  deriving newtype (Eq, Ord, Show)

-- | Resolve any number of 'CIncludePathDir'
--
-- This function returns an error if any path is not found or is not a
-- directory.
resolveCIncludeAbsPathDirs ::
     MonadIO m
  => [CIncludePathDir]
  -> m (Either String [CIncludeAbsPathDir])
resolveCIncludeAbsPathDirs paths = runExceptT $ do
    cwd <- liftIO Dir.getCurrentDirectory
    forM (getCIncludePathDir <$> paths) $ \path -> do
      let absPath
            | FilePath.isAbsolute path = path
            | otherwise                = cwd FilePath.</> path
      exists <- liftIO $ Dir.doesDirectoryExist absPath
      unless exists $
        throwError ("include path not found or not a directory: " ++ path)
      return $ CIncludeAbsPathDir absPath

{-------------------------------------------------------------------------------
  C header paths
-------------------------------------------------------------------------------}

-- | C header path (relative)
--
-- A value must be specified as used in the C source code (relative to an
-- include directory).  Forward slashes (@/@) must be used, even on Windows.
--
-- Example: @time.h@
newtype CHeaderRelPath = CHeaderRelPath { getCHeaderRelPath :: FilePath }
  deriving newtype (Eq, Ord, Show)

-- | Construct a 'CHeaderRelPath'
--
-- This funtion returns an error if the path is not relative or if it contains
-- a backslash.  It does /not/ check existence, readability, or if it points to
-- a file or not.
mkCHeaderRelPath :: FilePath -> Either String CHeaderRelPath
mkCHeaderRelPath path
    | '\\' `elem` path = Left $ "C header path contains a backslash: " ++ path
    | FilePath.isRelative path = Right $ CHeaderRelPath path
    | otherwise = Left $ "C header path not relative: " ++ path

-- | C header path (absolute)
--
-- Example: @/usr/include/time.h@
newtype CHeaderAbsPath = CHeaderAbsPath Text
  deriving newtype (Eq, Ord, Show)

-- | Construct a 'CHeaderAbsPath'
--
-- This function fails if the path is not absolute.  It does /not/ check
-- existence, readability, or if it points to a file or not.
mkCHeaderAbsPath :: Text -> Either String CHeaderAbsPath
mkCHeaderAbsPath t
    | FilePath.isAbsolute path = Right $ CHeaderAbsPath t
    | otherwise = Left $ "C header path is not absolute: " ++ path
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
     MonadIO m
  => [CIncludeAbsPathDir]
  -> CHeaderRelPath
  -> m (Either String CHeaderAbsPath)
resolveHeader includeDirs relPath = do
    let includeDirs' = getCIncludeAbsPathDir <$> includeDirs
        relPath'     = getCHeaderRelPath relPath
    mAbsPath <- liftIO $ Dir.findFile includeDirs' (nativeRelPath relPath')
    return $ case mAbsPath of
      Just absPath -> Right $ CHeaderAbsPath (Text.pack absPath)
      Nothing -> Left $ "header not found in include search path: " ++ relPath'
  where
    nativeRelPath :: FilePath -> FilePath
    nativeRelPath = FilePath.joinPath . Posix.splitDirectories
