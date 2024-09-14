-- | Utilities for working with source locations
--
-- The functions in this module intentionally have the same names as the
-- corresponding functions in "HsBindgen.Clang.Core", and therefore the same
-- names as in @libclang@ itself.
--
-- Intended for qualified import.
--
-- > import HsBindgen.Clang.Util.SourceLoc (SourceLoc(..), SourceRange(..))
-- > import HsBindgen.Clang.Util.SourceLoc qualified as SourceLoc
module HsBindgen.Clang.Util.SourceLoc (
    SourceLoc(..)
  , SourceRange(..)
    -- * Construction
  , clang_Cursor_getSpellingNameRange
  , clang_getCursorLocation
  , clang_getCursorExtent
  , clang_getDiagnosticLocation
  , clang_getDiagnosticRange
  , clang_getDiagnosticFixIt
    -- * Low-level
  , toSourceLoc
  , toSourceRange
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 qualified as BS.UTF8
import Data.List (intercalate)
import Foreign.C

import HsBindgen.Clang.Core qualified as Core
import HsBindgen.Clang.Core hiding (
    clang_Cursor_getSpellingNameRange
  , clang_getCursorLocation
  , clang_getCursorExtent
  , clang_getDiagnosticLocation
  , clang_getDiagnosticRange
  , clang_getDiagnosticFixIt
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

clang_getCursorLocation :: CXCursor -> IO SourceLoc
clang_getCursorLocation cursor =
    toSourceLoc =<< Core.clang_getCursorLocation cursor

clang_getCursorExtent :: CXCursor -> IO SourceRange
clang_getCursorExtent cursor =
    toSourceRange =<< Core.clang_getCursorExtent cursor

clang_getDiagnosticLocation :: CXDiagnostic -> IO SourceLoc
clang_getDiagnosticLocation diag =
    toSourceLoc =<< Core.clang_getDiagnosticLocation diag

clang_getDiagnosticRange :: CXDiagnostic -> CUInt -> IO SourceRange
clang_getDiagnosticRange diag i =
    toSourceRange =<< Core.clang_getDiagnosticRange diag i

clang_getDiagnosticFixIt ::
     CXDiagnostic
  -> CUInt
  -> IO (SourceRange, ByteString)
clang_getDiagnosticFixIt diag i = do
    (range, bs) <- Core.clang_getDiagnosticFixIt diag i
    (,bs) <$> toSourceRange range

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
