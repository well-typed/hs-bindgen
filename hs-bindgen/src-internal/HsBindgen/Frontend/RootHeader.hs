-- | Root header (header that includes all headers to be processed)
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.RootHeader (RootHeader)
-- > import HsBindgen.Frontend.RootHeader qualified as RootHeader
module HsBindgen.Frontend.RootHeader (
    -- * RootHeader
    RootHeader -- opaque
  , fromMainFiles
    -- ** Generation
  , name
  , content
    -- ** Query
  , isInRootHeader
  , at
  , lookup

    -- * HashIncludeArg
  , HashIncludeArg(..)
  , hashIncludeArg
  , hashIncludeArgWithTrace
    -- ** Trace message
  , HashIncludeArgMsg(..)
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
  RootHeader
-------------------------------------------------------------------------------}

-- | Abstract representation of the root header
--
-- This is /precisely/ the set of main files as specified by the user.
newtype RootHeader = RootHeader [HashIncludeArg]

-- | Construct a 'RootHeader'
fromMainFiles :: [HashIncludeArg] -> RootHeader
fromMainFiles = RootHeader

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

-- | Root header @UnsavedFile@ name
name :: SourcePath
name = SourcePath "hs-bindgen-root.h"

-- | Root header content
--
-- The content contains one include per line, in order, with no extra lines.
-- Functions 'at' and 'lookup' rely on this.
content :: RootHeader -> String
content (RootHeader headers) =
    unlines $ map toLine headers
  where
    toLine :: HashIncludeArg -> String
    toLine arg = "#include <"  ++ getHashIncludeArg arg ++ ">"

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Check if the specified location is in the root header
isInRootHeader :: MultiLoc -> Bool
isInRootHeader = (== name) . singleLocPath . multiLocExpansion

-- | Get the 'HashIncludeArg' for the include at the specified location in the
-- root header
--
-- Precondition: the 'SingleLoc' must be for the root header.
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
  HashIncludeArg
-------------------------------------------------------------------------------}

-- | @#include@ argument (opaque)
--
-- We only support system includes, using @#include <PATH>@ syntax.  This type
-- represents the @PATH@ to a header.
--
-- A @#include@ argument is generally relative to a directory in the C include
-- search path.  We issue a 'Notice' if an absolute path is used, because it is
-- almost always a mistake.
--
-- A @#include@ argument is C syntax.  Forward slashes (@/@) are used to
-- separate directories, even on Windows.  Backslashes are interpreted as
-- characters in a directory or filename, not directory separators.  We issue a
-- 'Notice' if a backslash is used, because it is almost always a mistake.
newtype HashIncludeArg = HashIncludeArg { getHashIncludeArg :: FilePath }
  deriving stock (Show)
  deriving newtype (Eq, IsString, Ord)

-- | Construct a 'HashIncludeArg', returning trace messages
hashIncludeArg :: FilePath -> ([HashIncludeArgMsg], HashIncludeArg)
hashIncludeArg fp = (hashIncludeArgMsgs fp, HashIncludeArg fp)

-- | Construct a 'HashIncludeArg', emitting trace messages
hashIncludeArgWithTrace ::
     Monad m
  => Tracer m HashIncludeArgMsg
  -> FilePath
  -> m HashIncludeArg
hashIncludeArgWithTrace tracer fp = do
    let (msgs, arg) = hashIncludeArg fp
    mapM_ (traceWith tracer) msgs
    return arg

{-------------------------------------------------------------------------------
  Trace message
-------------------------------------------------------------------------------}

-- | @#include@ argument trace message
data HashIncludeArgMsg =
    HashIncludeArgBackslash   FilePath
  | HashIncludeArgNotRelative FilePath
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace HashIncludeArgMsg where
  prettyForTrace = \case
    HashIncludeArgBackslash arg ->
      PP.string $ "#include argument contains a backslash: " ++ arg
    HashIncludeArgNotRelative arg ->
      PP.string $ "#include argument not relative: " ++ arg

instance HasDefaultLogLevel HashIncludeArgMsg where
  getDefaultLogLevel = const Notice

instance HasSource HashIncludeArgMsg where
  getSource = const HsBindgen

hashIncludeArgMsgs :: FilePath -> [HashIncludeArgMsg]
hashIncludeArgMsgs fp = catMaybes [
      if '\\' `elem` fp
        then Just (HashIncludeArgBackslash fp)
        else Nothing
    , if FilePath.isRelative fp
        then Nothing
        else Just (HashIncludeArgNotRelative fp)
    ]

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
