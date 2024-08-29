-- | Utilities for working with source locations
--
-- The functions in this module intentionally have the same names as the
-- corresponding functions in "HsBindgen.Clang.Core", and therefore the same
-- names as in @libclang@ itself.
--
-- Intended for qualified import.
--
-- > import HsBindgen.Clang.Aux.SourceLoc (SourceLoc(..), SourceRange(..))
-- > import HsBindgen.Clang.Aux.SourceLoc qualified as SourceLoc
module HsBindgen.Clang.Aux.SourceLoc (
    SourceLoc(..)
  , SourceRange(..)
    -- * Construction
  , clang_Cursor_getSpellingNameRange
  , clang_getCursorExtent
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Foreign

import HsBindgen.Clang.Core qualified as Core
import HsBindgen.Clang.Core hiding (
    clang_Cursor_getSpellingNameRange
  , clang_getCursorExtent
  )

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data SourceLoc = SourceLoc {
      sourceLocFile   :: !Strict.ByteString
    , sourceLocLine   :: !Int
    , sourceLocColumn :: !Int
    }
  deriving stock (Show, Eq, Ord)

data SourceRange = SourceRange {
      sourceRangeStart :: !SourceLoc
    , sourceRangeEnd   :: !SourceLoc
    }
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

clang_Cursor_getSpellingNameRange :: ForeignPtr CXCursor -> IO SourceRange
clang_Cursor_getSpellingNameRange cursor =
    toSourceRange =<< Core.clang_Cursor_getSpellingNameRange cursor 0 0

clang_getCursorExtent :: ForeignPtr CXCursor -> IO SourceRange
clang_getCursorExtent cursor =
    toSourceRange =<< Core.clang_getCursorExtent cursor

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

toSourceRange :: ForeignPtr CXSourceRange -> IO SourceRange
toSourceRange rng = do
    begin <- clang_getRangeStart rng
    end   <- clang_getRangeEnd   rng
    SourceRange
      <$> toSourceLoc begin
      <*> toSourceLoc end

toSourceLoc :: ForeignPtr CXSourceLocation -> IO SourceLoc
toSourceLoc loc = do
    (file, line, col, _bufOffset) <- clang_getSpellingLocation loc
    SourceLoc
      <$> clang_getFileName file
      <*> pure (fromIntegral line)
      <*> pure (fromIntegral col)
