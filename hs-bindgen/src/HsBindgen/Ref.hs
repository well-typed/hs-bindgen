module HsBindgen.Ref (
    -- * C References
    CNameSpelling(..)
  , CIncludePathDir
  , mkCIncludePathDir
  , getCIncludePathDir
  , CHeaderRelPath
  , mkCHeaderRelPath
  , getCHeaderRelPath
  , CHeaderAbsPath
  , mkCHeaderAbsPath
  , getCHeaderAbsPath
  , resolveHeader
  , resolveHeaders
    -- * Haskell References
  , HsPackageName(..)
  , HsModuleName(..)
  , HsIdentifier(..)
  ) where

import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import System.Directory qualified as Dir
import System.FilePath qualified as FilePath

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  C References
-------------------------------------------------------------------------------}

-- | Spelling of a C name
--
-- A value must specify @struct@ or @union@ when required.
--
-- Examples: @int8_t@, @struct tm@
--
-- This type is different from 'HsBindgen.C.AST.CName' in that it specifies the
-- spelling, not just a C identifier.
newtype CNameSpelling = CNameSpelling { getCNameSpelling :: Text }
  deriving newtype (Aeson.FromJSON, Eq, Ord, Show)

-- | C include search path directory
newtype CIncludePathDir = CIncludePathDir { getCIncludePathDir :: FilePath }
  deriving newtype (Eq, Ord, Show)

-- | Construct a 'CIncludePathDir'
--
-- This function fails if the path is not absolute.  It does /not/ check
-- existence, readability, or if it points to a directory or not.
mkCIncludePathDir :: MonadFail m => FilePath -> m CIncludePathDir
mkCIncludePathDir path
    | FilePath.isAbsolute path = return $ CIncludePathDir path
    | otherwise = fail $ "relative include path directory: " ++ path

-- | C header path (relative)
--
-- A value must be specified as used in the C source code (relative to an
-- include directory).
--
-- Example: @time.h@
newtype CHeaderRelPath = CHeaderRelPath Text
  deriving newtype (Eq, Ord, Show)

instance Aeson.FromJSON CHeaderRelPath where
  parseJSON = Aeson.withText "CHeaderRelPath" mkCHeaderRelPath

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

-- | Resolve a single C header
--
-- This function fails if the header is not found in the include search path.
resolveHeader ::
     [CIncludePathDir]
  -> CHeaderRelPath
  -> IO CHeaderAbsPath
resolveHeader includeDirs relPath =
    let includeDirs' = getCIncludePathDir <$> includeDirs
    in  resolveHeader' includeDirs' relPath

-- | Resolve multiple C headers
--
-- This function fails if any header is not found in the include search path.
resolveHeaders ::
     [CIncludePathDir]
  -> Set CHeaderRelPath
  -> IO (Map CHeaderRelPath CHeaderAbsPath)
resolveHeaders includeDirs relPathSet =
    let includeDirs' = getCIncludePathDir <$> includeDirs
    in  fmap Map.fromList . forM (Set.toList relPathSet) $ \relPath ->
          (relPath,) <$> resolveHeader' includeDirs' relPath

resolveHeader' :: [FilePath] -> CHeaderRelPath -> IO CHeaderAbsPath
resolveHeader' includeDirs relPath = do
    let relPath' = getCHeaderRelPath relPath
    mAbsPath <- Dir.findFile includeDirs relPath'
    case mAbsPath of
      Just absPath -> return $ CHeaderAbsPath (Text.pack absPath)
      Nothing -> fail $ "header not found in include search path: " ++ relPath'

{-------------------------------------------------------------------------------
  Haskell References
-------------------------------------------------------------------------------}

-- | Haskell package name
--
-- Example: @hs-bindgen-runtime@
newtype HsPackageName = HsPackageName { getHsPackageName :: Text }
  deriving newtype (Aeson.FromJSON, Eq, Ord, Show)

-- | Haskell module name
--
-- Example: @HsBindgen.Runtime.LibC@
newtype HsModuleName = HsModuleName { getHsModuleName :: Text }
  deriving newtype (Aeson.FromJSON, Eq, Ord, Show)

-- | Haskell identifier
--
-- Example: @CTm@
--
-- This type is different from 'HsBindgen.Hs.AST.HsName' in that it does not
-- include a 'HsBindgen.Hs.AST.Namespace'.
newtype HsIdentifier = HsIdentifier { getHsIdentifier :: Text }
  deriving newtype (Aeson.FromJSON, Eq, Ord, Show)
