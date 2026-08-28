-- | File identity and portable header names
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.C" should be used.
--
-- Within @HsBindgen.IR@, all modules aside from "HsBindgen.IR.C" should import
-- this module qualified for consistency.
--
-- > import HsBindgen.IR.C.HeaderName qualified as C
--
-- These are the types only. Deriving a t'HeaderName' needs @clang@ and lives in
-- "HsBindgen.Frontend.Analysis.HeaderName".
module HsBindgen.IR.C.HeaderName (
    -- * File identity
    FileId(..)
    -- * Header names
  , HeaderName(..)
  , headerNameArg
  , renderHeaderName
  , parseHeaderName
  , headerNameKey
  ) where

import HsBindgen.Imports
import HsBindgen.IR.C.HashIncludeArg qualified as C

{-------------------------------------------------------------------------------
  File identity
-------------------------------------------------------------------------------}

-- | Identity of a C source file
--
-- This is the file's real path: absolute, with symlinks and @..@ segments
-- resolved. Two 'FileId's are equal exactly when they denote the same file, no
-- matter which @#include@ argument reached it or in which order.
--
-- A 'FileId' is machine specific and never shown to the user. Use a
-- t'HeaderName' for anything the user sees or that we persist.
newtype FileId = FileId { path :: FilePath }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

{-------------------------------------------------------------------------------
  Header names
-------------------------------------------------------------------------------}

-- | The name of a header, as an @#include@ argument that reaches it
--
-- Specified by a round trip: including a t'HeaderName' resolves to the file it
-- names. That holds for a fixed C include search path, which is an assumption
-- every artefact @hs-bindgen@ emits already makes, since generated wrappers
-- @#include@ their headers by bracket name.
--
-- Unlike the path @clang@ reports, a t'HeaderName' does not depend on the
-- directive that happened to reach the file, nor on the order in which the
-- translation unit visited it.
--
-- The constructor is not a rendering detail. Writing a bracket name with quotes
-- changes which file it finds: a quote include first looks next to the
-- including file, and only then falls through to the search path, where a
-- shadowing copy may be waiting.
data HeaderName =
    -- | @#include \<arg\>@, with @arg@ relative to a search path directory
    ByBracket C.HashIncludeArg

    -- | @#include "arg"@, with @arg@ relative to the project root
    --
    -- Used for a header that no bracket name can reach, which happens when
    -- another search path directory holds the same relative path and comes
    -- first. Naming from the project root keeps the two apart, since that path
    -- /is/ the file.
  | ByQuote C.HashIncludeArg
  deriving stock (Show, Eq, Ord, Generic)

-- | The @#include@ argument of a t'HeaderName', dropping how it is delimited
headerNameArg :: HeaderName -> C.HashIncludeArg
headerNameArg = \case
    ByBracket arg -> arg
    ByQuote   arg -> arg

-- | Render a t'HeaderName' as the directive that reaches the file
renderHeaderName :: HeaderName -> String
renderHeaderName = \case
    ByBracket arg -> "#include <" ++ arg.path ++ ">"
    ByQuote   arg -> "#include \"" ++ arg.path ++ "\""

-- | The form to persist a t'HeaderName' in, which 'parseHeaderName' reads back
--
-- A bracket name is written bare, which is what every binding specification
-- written so far already contains, and a quote name keeps its quotes, which is
-- what tells the two apart.
--
-- Bare only works while the name cannot be mistaken for a delimited one, so a
-- bracket name that is already delimited gets brackets of its own. @\<@ and
-- @\"@ are legal in a filename, and these strings are persisted, so the pair
-- has to round trip for every name rather than for the likely ones.
headerNameKey :: HeaderName -> C.HashIncludeArg
headerNameKey = \case
    ByBracket arg
      | isDelimited arg.path -> C.HashIncludeArg ("<" ++ arg.path ++ ">")
      | otherwise            -> arg
    ByQuote arg              -> C.HashIncludeArg ("\"" ++ arg.path ++ "\"")

-- | Read a t'HeaderName' written the way it appears in C
--
-- @\<foo.h\>@ is bracket and @\"foo.h\"@ is quote, as in a directive. A bare
-- @foo.h@ is bracket too, which is what keeps binding specifications written
-- before quote names existed valid.
--
-- Inverse of 'headerNameKey'.
parseHeaderName :: FilePath -> HeaderName
parseHeaderName str
    | Just arg <- unwrap '<' '>' = ByBracket (C.HashIncludeArg arg)
    | Just arg <- unwrap '"' '"' = ByQuote   (C.HashIncludeArg arg)
    | otherwise                  = ByBracket (C.HashIncludeArg str)
  where
    unwrap :: Char -> Char -> Maybe String
    unwrap open close = case str of
      c : rest@(_:_) | c == open, last rest == close -> Just (init rest)
      _otherwise                                     -> Nothing

-- | Is this argument already written as a directive would write it?
isDelimited :: FilePath -> Bool
isDelimited str = case str of
    c : rest@(_:_) -> (c, last rest) `elem` [('<', '>'), ('"', '"')]
    _otherwise     -> False
