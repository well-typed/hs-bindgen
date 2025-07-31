-- | Root header (header that includes all headers to be processed)
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.RootHeader (RootHeader)
-- > import HsBindgen.Frontend.RootHeader qualified as RootHeader
module HsBindgen.Frontend.RootHeader (
    RootHeader -- opaque
  , fromMainFiles
    -- * Filenames or paths passed to @#include@
  , HashIncludeArg(..)
  , getHashIncludeArg
  , ParseHashIncludeArgException(..)
  , parseHashIncludeArg
  , validateHashIncludeArg
  , renderHashIncludeArg

    -- * Generate header
  , name
  , content
    -- * Query
  , at
  , lookup
  ) where

import Control.Exception (Exception (displayException))
import Data.List qualified as List
import Data.Maybe (listToMaybe)
import Prelude hiding (lookup)
import System.FilePath qualified as FilePath

import Clang.HighLevel.Types
import Clang.Paths
import HsBindgen.Errors
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Abstract representation of the root header
--
-- This is /precisely/ the set of main files as specified by the user.
newtype RootHeader = RootHeader [HashIncludeArg]

fromMainFiles :: [HashIncludeArg] -> RootHeader
fromMainFiles = RootHeader

{-------------------------------------------------------------------------------
  Filenames or paths passed to @#include@
-------------------------------------------------------------------------------}

-- | C header filename or path, as specified in an @#include@ directive
--
-- This type represents an unresolved C header filename or path.  It is relative
-- to a directory in the C include search path.
--
-- Forward slashes (@/@) must be used, even on Windows.
data HashIncludeArg =
    -- | C header filename or path corresponding to @#include <PATH>@ syntax
    System FilePath
  | -- | C header filename or path corresponding to @#include "PATH"@ syntax
    Quote  FilePath
  deriving (Eq, Ord, Show)

-- | Get the 'FilePath' representation of a 'HashIncludeArg'
getHashIncludeArg :: HashIncludeArg -> FilePath
getHashIncludeArg = \case
  System path -> path
  Quote  path -> path

-- TODO https://github.com/well-typed/hs-bindgen/issues/958: Use a trace with
-- Warning default log level.
-- | Failed to parse a 'HashIncludeArg'
data ParseHashIncludeArgException =
    -- | Path contains a backslash
    ParseHashIncludeArgBackslash String
  | -- | Path is not relative
    ParseHashIncludeArgNotRelative String
  deriving (Show)

instance Exception ParseHashIncludeArgException where
  displayException = \case
    ParseHashIncludeArgBackslash path ->
      "C header include path contains a backslash: " ++ path
    ParseHashIncludeArgNotRelative path ->
      "C header include path not relative: " ++ path

-- TODO https://github.com/well-typed/hs-bindgen/issues/958: Remove parser.
-- | Parse a 'HashIncludeArg'
--
-- Prefix @system:@ is used to construct a 'System'.  No prefix is used to
-- construct a 'Quote'.
--
-- This function returns an error if the path is not relative or if it contains
-- a backslash.
parseHashIncludeArg ::
     String
  -> Either ParseHashIncludeArgException HashIncludeArg
parseHashIncludeArg path = case List.stripPrefix "system:" path of
    Nothing    -> validateHashIncludeArg $ Quote  path
    Just path' -> validateHashIncludeArg $ System path'

validateHashIncludeArg ::
     HashIncludeArg
  -> Either ParseHashIncludeArgException HashIncludeArg
validateHashIncludeArg = \case
  (Quote path)  -> Quote  <$> aux path
  (System path) -> System <$> aux path
  where
    aux :: FilePath -> Either ParseHashIncludeArgException FilePath
    aux path
      | '\\' `elem` path         = Left $ ParseHashIncludeArgBackslash path
      | FilePath.isRelative path = Right path
      | otherwise                = Left $ ParseHashIncludeArgNotRelative path

-- TODO https://github.com/well-typed/hs-bindgen/issues/958: Remove ('system:'
-- prefix not used anymore).
-- | Render a 'HashIncludeArg'
--
-- A 'System' is rendered with a @system:@ prefix.  A
-- 'Quote' is rendered without a prefix.
renderHashIncludeArg :: HashIncludeArg -> String
renderHashIncludeArg = \case
    System path -> "system:" ++ path
    Quote  path -> path



{-------------------------------------------------------------------------------
  Generate header
-------------------------------------------------------------------------------}

name :: SourcePath
name = SourcePath "hs-bindgen-root.h"

content :: RootHeader -> String
content (RootHeader headers) =
    unlines $ map toLine headers
  where
    toLine :: HashIncludeArg -> String
    toLine = \case
      System path -> "#include <"  ++ path ++ ">"
      Quote  path -> "#include \"" ++ path ++ "\""

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Get the include at the specified location in the root header
--
-- Precondition: the 'SingleLoc' must point to the root header.
at :: RootHeader -> SingleLoc -> HashIncludeArg
at (RootHeader headers) loc =
    case headers !? (singleLocLine loc - 1) of
      Just path -> path
      Nothing   -> panicPure "Unknown root header location"

-- | Get the include at the specified location, /if/ it is from the root header
--
-- This depends on the 'SourcePath' in the 'SingleLoc'.
lookup :: RootHeader -> SingleLoc -> Maybe HashIncludeArg
lookup rootHeader loc = do
    guard $ singleLocPath loc == name
    return $ rootHeader `at` loc

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- | List index (subscript) operator, starting from 0
--
-- Returns 'Nothing' if the index is out of bounds
(!?) :: [a] -> Int -> Maybe a
xs !? n
    | n < 0     = Nothing
    | otherwise = listToMaybe $ drop n xs
infixl 9 !?
{-# INLINABLE (!?) #-}
