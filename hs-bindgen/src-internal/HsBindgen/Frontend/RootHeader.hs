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
  , HashIncludeArg
  , getHashIncludeArg
  , hashIncludeArgEither
  , hashIncludeArgWithTrace
  , hashIncludeArgUnsafe
    -- ** Trace message
  , HashIncludeArgMsg(..)
    -- * Generate header
  , name
  , content
    -- * Query
  , at
  , lookup
  ) where

import Data.Maybe (listToMaybe)
import Prelude hiding (lookup)
import System.FilePath qualified as FilePath

import Clang.HighLevel.Types
import Clang.Paths
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint qualified as PP

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Abstract representation of the root header.
--
-- This is /precisely/ the set of main files as specified by the user.
newtype RootHeader = RootHeader [HashIncludeArg]

fromMainFiles :: [HashIncludeArg] -> RootHeader
fromMainFiles = RootHeader

{-------------------------------------------------------------------------------
  C header files passed to @#include@
-------------------------------------------------------------------------------}

-- | C header file, as specified in an @#include@ directive (opaque).
--
-- This type represents an unresolved C header filename or path.  It is relative
-- to a directory in the C include search path.
--
-- Forward slashes (@/@) must be used, even on Windows.
--
-- We only support C header files corresponding to @#include <PATH>@ syntax, and
-- do not use "quote" includes corresponding to @#include "PATH"@.
newtype HashIncludeArg = System { unHashIncludeArg :: FilePath }
  deriving (Eq, Ord, Show)

-- | Get the 'FilePath' representation of a 'HashIncludeArg'
getHashIncludeArg :: HashIncludeArg -> FilePath
getHashIncludeArg = unHashIncludeArg

-- | Parse an argument to @#include@.
--
-- Return an error if the C header file is not relative or if it contains a
-- backslash.
hashIncludeArgEither :: FilePath -> Either HashIncludeArgMsg HashIncludeArg
hashIncludeArgEither arg
    | '\\' `elem` arg         = Left $ HashIncludeArgBackslash arg
    | FilePath.isRelative arg = Right $ System arg
    | otherwise               = Left $ HashIncludeArgNotRelative arg

-- | Construct a 'HashIncludeArg'.
--
-- Emit a trace if the C header file is not relative or if it contains a
-- backslash.
hashIncludeArgWithTrace
  :: Monad m
  => Tracer m HashIncludeArgMsg
  -> FilePath
  -> m HashIncludeArg
hashIncludeArgWithTrace tracer arg = case hashIncludeArgEither arg of
  Left msg -> traceWith tracer msg >> pure (System arg)
  Right x  -> pure x

-- | Internal; used to construct the standard library binding specifications and
-- in tests.
hashIncludeArgUnsafe :: FilePath -> HashIncludeArg
hashIncludeArgUnsafe = System

{-------------------------------------------------------------------------------
  Trace message
-------------------------------------------------------------------------------}

-- | @#include@ argument trace messages.
data HashIncludeArgMsg =
    HashIncludeArgBackslash   FilePath
  | HashIncludeArgNotRelative FilePath
  deriving (Show, Eq, Ord)

instance PrettyForTrace HashIncludeArgMsg where
  prettyForTrace = \case
    HashIncludeArgBackslash arg
      -> PP.string $ "C header include file contains a backslash: " ++ arg
    HashIncludeArgNotRelative arg
      -> PP.string $ "C header include file not relative: " ++ arg

instance HasDefaultLogLevel HashIncludeArgMsg where
  getDefaultLogLevel = const Warning

instance HasSource HashIncludeArgMsg where
  getSource = const HsBindgen

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
    toLine arg = "#include <"  ++ (getHashIncludeArg arg) ++ ">"

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
