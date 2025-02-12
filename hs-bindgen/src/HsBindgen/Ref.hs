module HsBindgen.Ref (
    -- * C References
    CNameSpelling(..)
  , CHeaderRelPath(..)
  , CHeaderAbsPath(..)
  , CIncludePathDir(..)
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

-- | C header path (relative)
--
-- A value must be specified as used in the C source code (relative to an
-- include directory).
--
-- Example: @time.h@
newtype CHeaderRelPath = CHeaderRelPath { getCHeaderRelPath :: Text }
  deriving newtype (Aeson.FromJSON, Eq, Ord, Show)

-- | C header path (absolute)
--
-- Example: @/usr/include/time.h@
newtype CHeaderAbsPath = CHeaderAbsPath { getCHeaderAbsPath :: Text }
  deriving newtype (Eq, Ord, Show)

-- | C include search path directory
newtype CIncludePathDir = CIncludePathDir { getCIncludePathDir :: FilePath }
  deriving newtype (Eq, Ord, Show)

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
    let relPath' = Text.unpack $ getCHeaderRelPath relPath
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
