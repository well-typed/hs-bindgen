-- | Utilities for working with source locations
--
-- Intended for qualified import.
--
-- > import HsBindgen.Clang.Util.SourceLoc.Type
-- > import HsBindgen.Clang.Util.SourceLoc qualified as SourceLoc
module HsBindgen.Clang.Util.SourceLoc (
    -- * Conversion
    toMulti
  , toRange
  , fromSingle
  , fromRange
    -- * Get single location
  , clang_getExpansionLocation
  , clang_getPresumedLocation
  , clang_getSpellingLocation
  , clang_getFileLocation
    -- * Convenience wrappers
    -- * for @CXSourceLocation@
  , clang_getDiagnosticLocation
  , clang_getCursorLocation
  , clang_getTokenLocation
    -- ** for @CXSourceRange@
  , clang_getDiagnosticRange
  , clang_getDiagnosticFixIt
  , clang_Cursor_getSpellingNameRange
  , clang_getCursorExtent
  , clang_getTokenExtent
  ) where

import Control.Monad
import Data.Text (Text)
import Foreign.C

import HsBindgen.Clang.Core qualified as Core
import HsBindgen.Clang.Util.SourceLoc.Type

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

toMulti :: Core.CXSourceLocation -> IO MultiLoc
toMulti location = do
    expansion <- clang_getExpansionLocation location

    let unlessIsExpanion :: SingleLoc -> Maybe SingleLoc
        unlessIsExpanion loc = do
            guard $ loc /= expansion
            return loc

    MultiLoc expansion
      <$> (unlessIsExpanion <$> clang_getPresumedLocation location)
      <*> (unlessIsExpanion <$> clang_getSpellingLocation location)
      <*> (unlessIsExpanion <$> clang_getFileLocation     location)

toRange :: Core.CXSourceRange -> IO (Range MultiLoc)
toRange = toRangeWith toMulti

fromSingle :: Core.CXTranslationUnit -> SingleLoc -> IO Core.CXSourceLocation
fromSingle unit SingleLoc{singleLocPath, singleLocLine, singleLocColumn} = do
     file <- Core.clang_getFile unit (getSourcePath singleLocPath)
     Core.clang_getLocation
       unit
       file
       (fromIntegral singleLocLine)
       (fromIntegral singleLocColumn)

fromRange :: Core.CXTranslationUnit -> Range SingleLoc -> IO Core.CXSourceRange
fromRange unit Range{rangeStart, rangeEnd} = do
    rangeStart' <- fromSingle unit rangeStart
    rangeEnd'   <- fromSingle unit rangeEnd
    Core.clang_getRange rangeStart' rangeEnd'

{-------------------------------------------------------------------------------
  Get single location
-------------------------------------------------------------------------------}

clang_getExpansionLocation :: Core.CXSourceLocation -> IO SingleLoc
clang_getExpansionLocation location =
    toSingle' =<< Core.clang_getExpansionLocation location

clang_getPresumedLocation :: Core.CXSourceLocation -> IO SingleLoc
clang_getPresumedLocation location =
    toSingle <$> Core.clang_getPresumedLocation location

clang_getSpellingLocation :: Core.CXSourceLocation -> IO SingleLoc
clang_getSpellingLocation location =
    toSingle' =<< Core.clang_getSpellingLocation location

clang_getFileLocation :: Core.CXSourceLocation -> IO SingleLoc
clang_getFileLocation location =
    toSingle' =<< Core.clang_getFileLocation location

{-------------------------------------------------------------------------------
  Convenience wrappers for @CXSourceLocation@
-------------------------------------------------------------------------------}

-- | Retrieve the source location of the given diagnostic.
clang_getDiagnosticLocation :: Core.CXDiagnostic -> IO MultiLoc
clang_getDiagnosticLocation diagnostic =
    toMulti =<< Core.clang_getDiagnosticLocation diagnostic

-- | Retrieve the physical location of the source constructor referenced by the
-- given cursor.
clang_getCursorLocation :: Core.CXCursor -> IO MultiLoc
clang_getCursorLocation cursor =
    toMulti =<< Core.clang_getCursorLocation cursor

-- | Retrieve the source location of the given token.
clang_getTokenLocation :: Core.CXTranslationUnit -> Core.CXToken -> IO MultiLoc
clang_getTokenLocation unit token =
    toMulti =<< Core.clang_getTokenLocation unit token

{-------------------------------------------------------------------------------
  Convenience wrappers for @CXSourceRange@
-------------------------------------------------------------------------------}

-- | Retrieve a source range associated with the diagnostic.
clang_getDiagnosticRange :: Core.CXDiagnostic -> CUInt -> IO (Range MultiLoc)
clang_getDiagnosticRange diagnostic range =
    toRange =<< Core.clang_getDiagnosticRange diagnostic range

-- | Retrieve the replacement information for a given fix-it.
clang_getDiagnosticFixIt ::
     Core.CXDiagnostic
  -> CUInt
  -> IO (Range MultiLoc, Text)
clang_getDiagnosticFixIt diagnostic fixit = do
    (range, replacement) <- Core.clang_getDiagnosticFixIt diagnostic fixit
    (, replacement) <$> toRange range

-- | Retrieve a range for a piece that forms the cursors spelling name.
--
-- TODO: This currently returns 'Range' 'SingleLoc' as I am assuming that the
-- relevant part of the returned range is the /spelling/ location. That
-- assumption may be false.
clang_Cursor_getSpellingNameRange ::
     Core.CXCursor
  -> CUInt
  -> CUInt
  -> IO (Range SingleLoc)
clang_Cursor_getSpellingNameRange cursor pieceIndex options = do
    range <- Core.clang_Cursor_getSpellingNameRange cursor pieceIndex options
    toRangeWith clang_getSpellingLocation range

-- | Retrieve the physical extent of the source construct referenced by the
-- given cursor.
clang_getCursorExtent :: Core.CXCursor -> IO (Range MultiLoc)
clang_getCursorExtent cursor =
    toRange =<< Core.clang_getCursorExtent cursor

-- | Retrieve a source range that covers the given token.
clang_getTokenExtent ::
     Core.CXTranslationUnit
  -> Core.CXToken
  -> IO (Range MultiLoc)
clang_getTokenExtent unit token =
    toRange =<< Core.clang_getTokenExtent unit token

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

toSingle :: (Text, CUInt, CUInt) -> SingleLoc
toSingle (singleLocPath, singleLocLine, singleLocColumn) = SingleLoc{
      singleLocPath   = SourcePath singleLocPath
    , singleLocLine   = fromIntegral singleLocLine
    , singleLocColumn = fromIntegral singleLocColumn
    }

toSingle' :: (Core.CXFile, CUInt, CUInt, CUInt) -> IO SingleLoc
toSingle' (file, singleLocLine, singleLocColumn, _offset) = do
    singleLocPath <- Core.clang_getFileName file
    return $ toSingle (singleLocPath, singleLocLine, singleLocColumn)

toRangeWith ::
    (Core.CXSourceLocation -> IO a)
  -> Core.CXSourceRange -> IO (Range a)
toRangeWith f range =
    Range
      <$> (f =<< Core.clang_getRangeStart range)
      <*> (f =<< Core.clang_getRangeEnd   range)

