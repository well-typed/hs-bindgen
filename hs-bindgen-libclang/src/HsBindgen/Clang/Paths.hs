module HsBindgen.Clang.Paths (
    -- * Source paths
    SourcePath(..)
  , getSourcePath
  , nullSourcePath

    -- * C header paths
  , CHeaderIncludePath(..)
  , getCHeaderIncludePath
  , ParseCHeaderIncludePathException(..)
  , parseCHeaderIncludePath
  , renderCHeaderIncludePath

    -- * C include path directories
  , CIncludePathDir(..)
  ) where

import Control.Exception (Exception(displayException))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.List qualified as List
import Data.String (IsString(fromString))
import System.FilePath qualified as FilePath

{-------------------------------------------------------------------------------
  Source paths
-------------------------------------------------------------------------------}

-- | Filesystem path of a source file, typically a C header
--
-- The 'Text' type is used because Clang uses UTF-8 internally for everything,
-- including paths.
--
-- The format of the path is platform-dependent.  For example, different
-- directory separators are used on different platforms.
newtype SourcePath = SourcePath Text
  deriving newtype (Eq, Ord, Show)

-- | Get the 'FilePath' representation of a 'SourcePath'
getSourcePath :: SourcePath -> FilePath
getSourcePath (SourcePath path) = Text.unpack path

-- | Determine if a 'SourcePath' is empty
nullSourcePath :: SourcePath -> Bool
nullSourcePath (SourcePath path) = Text.null path

{-------------------------------------------------------------------------------
  C header paths
-------------------------------------------------------------------------------}

-- | C header path, as specified in an include directive
--
-- This type represents an unresolved C header path.  It is relative to a
-- directory in the C include search path.
--
-- Forward slashes (@/@) must be used, even on Windows.
data CHeaderIncludePath =
    -- | C header path corresponding to @#include <PATH>@ syntax
    CHeaderSystemIncludePath FilePath
  | -- | C header path corresponding to @#include "PATH"@ syntax
    CHeaderQuoteIncludePath  FilePath
  deriving (Eq, Ord, Show)

-- | Get the 'FilePath' representation of a 'CHeaderIncludePath'
getCHeaderIncludePath :: CHeaderIncludePath -> FilePath
getCHeaderIncludePath = \case
  CHeaderSystemIncludePath path -> path
  CHeaderQuoteIncludePath  path -> path

-- | Failed to parse a 'CHeaderIncludePath'
data ParseCHeaderIncludePathException =
    -- | Path contains a backslash
    ParseCHeaderIncludePathBackslash String
  | -- | Path is not relative
    ParseCHeaderIncludePathNotRelative String
  deriving (Show)

instance Exception ParseCHeaderIncludePathException where
  displayException = \case
    ParseCHeaderIncludePathBackslash path ->
      "C header include path contains a backslash: " ++ path
    ParseCHeaderIncludePathNotRelative path ->
      "C header include path not relative: " ++ path

-- | Parse a 'CHeaderIncludePath'
--
-- Prefix @system:@ is used to construct a 'CHeaderSystemIncludePath'.  No
-- prefix is used to construct a 'CHeaderQuoteIncludePath'.
--
-- This function returns an error if the path is not relative or if it contains
-- a backslash.
parseCHeaderIncludePath ::
     String
  -> Either ParseCHeaderIncludePathException CHeaderIncludePath
parseCHeaderIncludePath path = case List.stripPrefix "system:" path of
    Just path' -> CHeaderSystemIncludePath <$> aux path'
    Nothing    -> CHeaderQuoteIncludePath  <$> aux path
  where
    aux :: FilePath -> Either ParseCHeaderIncludePathException FilePath
    aux path'
      | '\\' `elem` path' = Left $ ParseCHeaderIncludePathBackslash path
      | FilePath.isRelative path' = Right path'
      | otherwise = Left $ ParseCHeaderIncludePathNotRelative path

-- | Render a 'CHeaderIncludePath'
--
-- A 'CHeaderSystemIncludePath' is rendered with a @system:@ prefix.  A
-- 'CHeaderQuoteIncludePath' is rendered without a prefix.
renderCHeaderIncludePath :: CHeaderIncludePath -> String
renderCHeaderIncludePath = \case
    CHeaderSystemIncludePath path -> "system:" ++ path
    CHeaderQuoteIncludePath  path -> path

{-------------------------------------------------------------------------------
  C include path directories
-------------------------------------------------------------------------------}

-- | C include path directory
--
-- A C include path is an ordered list of directories that is used to resolve a
-- 'CHeaderIncludePath'.  The name cames from environment variables
-- @C_INCLUDE_PATH@ and @CPATH@.  It is unforuntaly confusing terminology that a
-- /search/ path is a list of /filesystem/ paths.
--
-- The wrapped directory path may be absolute or relative to the current working
-- directory.  When it is relative, resolved header paths in @libclang@ source
-- locations are also relative.
--
-- For example, resolving 'CHeaderIncludePath' @stdint.h@ with a C include path
-- that contains 'CIncludePathDir' @/usr/include@ may resolve to 'SourcePath'
-- @/usr/include/stdint.h@.
newtype CIncludePathDir = CIncludePathDir { getCIncludePathDir :: FilePath }
  deriving newtype (Eq, Ord, Show)

instance IsString CIncludePathDir where
  fromString = CIncludePathDir
