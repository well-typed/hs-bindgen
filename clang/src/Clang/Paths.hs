module Clang.Paths (
    -- * Source paths
    SourcePath(..)
  , getSourcePath
  , nullSourcePath

    -- * C include path directories
  , CIncludeDir(..)
  ) where

import Data.String
import Data.Text (Text)
import Data.Text qualified as Text

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
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Eq, IsString, Ord, Show)

-- | Get the 'FilePath' representation of a 'SourcePath'
getSourcePath :: SourcePath -> FilePath
getSourcePath (SourcePath path) = Text.unpack path

-- | Determine if a 'SourcePath' is empty
nullSourcePath :: SourcePath -> Bool
nullSourcePath (SourcePath path) = Text.null path

{-------------------------------------------------------------------------------
  C include path directories
-------------------------------------------------------------------------------}

-- | C include directory
--
-- The C system and quote include search paths are ordered lists of directories
-- that are used to resolve header file paths. The name comes from environment
-- variables @C_INCLUDE_PATH@ and @CPATH@. It is unforuntaly confusing
-- terminology that a /search/ path is a list of /filesystem/ paths.
--
-- The wrapped directory 'FilePath' may be absolute or relative to the current
-- working directory. When it is relative, resolved header file paths in
-- @libclang@ source locations are also relative.
--
-- For example, resolving "<stdint.h>" with a C system include search path that
-- contains 'CIncludeDir' @/usr/include@ may resolve to 'SourcePath'
-- @/usr/include/stdint.h@.
newtype CIncludeDir = CIncludeDir { getCIncludeDir :: FilePath }
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Eq, IsString, Ord, Show)
