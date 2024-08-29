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

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 qualified as BS.UTF8
import Data.List (intercalate)

import HsBindgen.Clang.Core qualified as Core
import HsBindgen.Clang.Core hiding (
    clang_Cursor_getSpellingNameRange
  , clang_getCursorExtent
  )

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data SourceLoc = SourceLoc {
      sourceLocFile   :: !ByteString
    , sourceLocLine   :: !Int
    , sourceLocColumn :: !Int
    }
  deriving stock (Eq, Ord)

data SourceRange = SourceRange {
      sourceRangeStart :: !SourceLoc
    , sourceRangeEnd   :: !SourceLoc
    }
  deriving stock (Eq, Ord)

{-------------------------------------------------------------------------------
  Show instances

  These are defined so that we could, if we wanted to, also provide an inverse
  'IsString' instance; this is the reason for the @show . pretty...@
-------------------------------------------------------------------------------}

instance Show SourceLoc where
  show = show . prettySourceLoc True

instance Show SourceRange where
  show = show . prettySourceRange

prettySourceLoc ::
     Bool -- ^ Should we show the file?
  -> SourceLoc -> String
prettySourceLoc showFile (SourceLoc file line col) =
    intercalate ":" . concat $ [
        -- Encoding and filepaths is a mess..
        [ BS.UTF8.toString file | showFile ]
      , [ show line, show col ]
      ]

prettySourceRange :: SourceRange -> String
prettySourceRange (SourceRange start end) = concat [
      prettySourceLoc True start
    , "-"
    , prettySourceLoc (sourceLocFile start /= sourceLocFile end) end
    ]

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

clang_Cursor_getSpellingNameRange :: CXCursor -> IO SourceRange
clang_Cursor_getSpellingNameRange cursor =
    toSourceRange =<< Core.clang_Cursor_getSpellingNameRange cursor 0 0

clang_getCursorExtent :: CXCursor -> IO SourceRange
clang_getCursorExtent cursor =
    toSourceRange =<< Core.clang_getCursorExtent cursor

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

toSourceRange :: CXSourceRange -> IO SourceRange
toSourceRange rng = do
    begin <- clang_getRangeStart rng
    end   <- clang_getRangeEnd   rng
    SourceRange
      <$> toSourceLoc begin
      <*> toSourceLoc end

toSourceLoc :: CXSourceLocation -> IO SourceLoc
toSourceLoc loc = do
    (file, line, col, _bufOffset) <- clang_getSpellingLocation loc
    SourceLoc
      <$> clang_getFileName file
      <*> pure (fromIntegral line)
      <*> pure (fromIntegral col)
