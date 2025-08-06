module Clang.Paths (
    -- * Source paths
    SourcePath(..)
  , getSourcePath
  , nullSourcePath

    -- * C include directories
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
  C include directories
-------------------------------------------------------------------------------}

-- | C include directory
--
-- A /C include directory/ is a directory that contains C header files, and a
-- /C include search path/ is a list of C include directories that is used to
-- resolve headers.
--
-- The wrapped 'FilePath' may be absolute or relative to the current working
-- directory.  When an include directive is resolved using a relative
-- 'CIncludeDir', the resulting 'SourcePath' is also relative.
--
-- Examples:
--
-- * When using a C include search path that contains 'CIncludeDir'
--   @/usr/include@, @#include <stdint.h>@ may resolve to 'SourcePath'
--   @/usr/include/stdint.h@.
--
-- * When using a C include search path that contains 'CIncludeDir' @include@ (a
--   directory in the current working directory), @#include <foo.h>@ may resolve
--   to 'SourcePath' @include/foo.h@ (also relative to the current working
--   directory).
newtype CIncludeDir = CIncludeDir { getCIncludeDir :: FilePath }
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Eq, IsString, Ord, Show)
