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
  ) where

import Prelude hiding (lookup)

import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.IR.C qualified as C

{-------------------------------------------------------------------------------
  RootHeader
-------------------------------------------------------------------------------}

-- | Abstract representation of the root header
--
-- This is /precisely/ the set of main files as specified by the user.
newtype RootHeader = RootHeader [C.HashIncludeArg]

-- | Construct a t'RootHeader'
fromMainFiles :: [C.HashIncludeArg] -> RootHeader
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
-- Functions @at@ and 'Prelude.lookup' rely on this.
content :: RootHeader -> String
content (RootHeader headers) =
    unlines $ map toLine headers
  where
    toLine :: C.HashIncludeArg -> String
    toLine arg = "#include <"  ++ arg.path ++ ">"

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Check if the specified location is in the root header
isInRootHeader :: MultiLoc -> Bool
isInRootHeader = (== name) . singleLocPath . multiLocExpansion
