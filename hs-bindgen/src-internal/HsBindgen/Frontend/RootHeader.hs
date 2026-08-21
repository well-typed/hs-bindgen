-- | Root header (header that includes all headers to be processed)
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.RootHeader (RootHeader)
-- > import HsBindgen.Frontend.RootHeader qualified as RootHeader
module HsBindgen.Frontend.RootHeader (
    -- * RootHeader
    RootHeader -- opaque
  , fromRootDirectives
    -- ** Generation
  , name
  , content
    -- ** Query
  , isRootHeaderPath
  , isInRootHeader
    -- ** Trace message
  , RootHeaderMsg(..)
  ) where

import Prelude hiding (lookup)

import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.IR.C qualified as C
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  RootHeader
-------------------------------------------------------------------------------}

-- | Abstract representation of the root header
--
-- This is /precisely/ the list of root directives as specified by the user.
newtype RootHeader = RootHeader [C.RootDirective C.HashIncludeArg]

-- | Construct a t'RootHeader', returning trace messages
fromRootDirectives ::
     [C.RootDirective C.HashIncludeArg]
  -> ([RootHeaderMsg], RootHeader)
fromRootDirectives directives = (msgs, RootHeader directives)
  where
    -- Without a header there is nothing to generate bindings for. This is the
    -- single check: every way of using @hs-bindgen@ builds the root header here.
    msgs :: [RootHeaderMsg]
    msgs = [RootHeaderNoHashInclude | null (C.hashIncludeArgsOf directives)]

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

-- | Root header @UnsavedFile@ name
name :: SourcePath
name = SourcePath "hs-bindgen-root.h"

-- | Root header content
--
-- The content contains one directive per line, in order, with no extra lines.
-- The very same rendering is prepended to the generated CAPI wrapper source and
-- to the generated C test source, so that all C stages agree.
content :: RootHeader -> String
content (RootHeader directives) = C.renderRootDirectives directives

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Check if the specified path is the root header
isRootHeaderPath :: SourcePath -> Bool
isRootHeaderPath = (== name)

-- | Check if the specified location is in the root header
isInRootHeader :: MultiLoc -> Bool
isInRootHeader = isRootHeaderPath . singleLocPath . multiLocExpansion

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

-- | Root header trace message
data RootHeaderMsg =
    RootHeaderNoHashInclude
  deriving stock (Show)

instance PrettyForTrace RootHeaderMsg where
  prettyForTrace = \case
    RootHeaderNoHashInclude ->
      PP.string "no #include root directive: nothing to generate bindings for"

instance IsTrace Level RootHeaderMsg where
  -- A warning, not an error: hs-bindgen runs to completion, and no declarations
  -- is the correct result for a root header with no @#include@.
  getDefaultLogLevel = const Warning
  getSource          = const HsBindgen
  getTraceId         = const "root-header"
