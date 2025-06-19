-- | Root header (header that includes all headers to be processed)
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.RootHeader (RootHeader)
-- > import HsBindgen.Frontend.RootHeader qualified as RootHeader
module HsBindgen.Frontend.RootHeader (
    RootHeader -- opaque
  , fromMainFiles
    -- * Generate header
  , name
  , content
    -- * Query
  , at
  , lookup
  ) where

import Data.Maybe (listToMaybe)
import Prelude hiding (lookup)

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
newtype RootHeader = RootHeader [CHeaderIncludePath]

fromMainFiles :: [CHeaderIncludePath] -> RootHeader
fromMainFiles = RootHeader

{-------------------------------------------------------------------------------
  Generate header
-------------------------------------------------------------------------------}

name :: SourcePath
name = SourcePath "hs-bindgen-root.h"

content :: RootHeader -> String
content (RootHeader headers) =
    unlines $ map toLine headers
  where
    toLine :: CHeaderIncludePath -> String
    toLine = \case
      CHeaderSystemIncludePath path -> "#include <" ++ path ++ ">"
      CHeaderQuoteIncludePath  path -> "#include \"" ++ path ++ "\""

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Get the include at the specified location in the root header
--
-- Precondition: the 'SingleLoc' must point to the root header.
at :: RootHeader -> SingleLoc -> CHeaderIncludePath
at (RootHeader headers) loc =
    case headers !? (singleLocLine loc - 1) of
      Just path -> path
      Nothing   -> panicPure "Unknown root header location"

-- | Get the include at the specified location, /if/ it is from the root header
--
-- This depends on the 'SourcePath' in the 'SingleLoc'.
lookup :: RootHeader -> SingleLoc -> Maybe CHeaderIncludePath
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
