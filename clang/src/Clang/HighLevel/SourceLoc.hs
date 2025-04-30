-- | Utilities for working with source locations
module Clang.HighLevel.SourceLoc (
    -- * Definition
    SingleLoc(..)
  , MultiLoc(..)
  , Range(..)
    -- * Comparisons
  , compareSingleLoc
  , rangeContainsLoc
    -- * Conversion
  , toMulti
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
import Data.List (intercalate)
import Data.Text (Text)
import Foreign.C
import GHC.Generics (Generic)
import GHC.Stack

import Clang.LowLevel.Core qualified as Core
import Clang.Paths

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | A /single/ location in a file
--
-- See 'MultiLoc' for additional discussion.
data SingleLoc = SingleLoc {
      singleLocPath   :: !SourcePath
    , singleLocLine   :: !Int
    , singleLocColumn :: !Int
    }
  deriving stock (Eq, Ord, Generic)

-- | Multiple related source locations
--
-- 'Core.CXSourceLocation' in @libclang@ corresponds to @SourceLocation@ in
-- @clang@, which can actually correspond to /multiple/ source locations in a
-- file; for example, in a header file such as
--
-- > #define M1 int
-- >
-- > struct ExampleStruct {
-- >   M1 m1;
-- >   ^
-- > };
--
-- then the source location at the caret (@^@) has an \"expansion location\",
-- which is the position at the caret, and a \"spelling location\", which
-- corresponds to the location of the @int@ token in the macro definition.
--
-- References:
--
-- * <https://clang.llvm.org/doxygen/classclang_1_1SourceLocation.html>
-- * <https://clang.llvm.org/doxygen/classclang_1_1SourceManager.html>
--   (@getExpansionLoc@, @getSpellingLoc@, @getDecomposedSpellingLoc@)
data MultiLoc = MultiLoc {
      -- | Expansion location
      --
      -- If the location refers into a macro expansion, this corresponds to the
      -- location of the macro expansion.
      --
      -- See <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#gadee4bea0fa34550663e869f48550eb1f>
      multiLocExpansion :: !SingleLoc

      -- | Presumed location
      --
      -- The given source location as specified in a @#line@ directive.
      --
      -- See <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#ga03508d9c944feeb3877515a1b08d36f9>
    , multiLocPresumed :: !(Maybe SingleLoc)

      -- | Spelling location
      --
      -- If the location refers into a macro instantiation, this corresponds to
      -- the /original/ location of the spelling in the source file.
      --
      -- /WARNING/: This field is only populated correctly from @llvm >= 19.1.0@;
      -- prior to that this is equal to 'multiLocFile'.
      -- See <https://github.com/llvm/llvm-project/pull/72400>.
      --
      -- See <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#ga01f1a342f7807ea742aedd2c61c46fa0>
    , multiLocSpelling :: !(Maybe SingleLoc)

      -- | File location
      --
      -- If the location refers into a macro expansion, this corresponds to the
      -- location of the macro expansion.
      -- If the location points at a macro argument, this corresponds to the
      -- location of the use of the argument.
      --
      -- See <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#gae0ee9ff0ea04f2446832fc12a7fd2ac8>
    , multiLocFile :: !(Maybe SingleLoc)
    }
  deriving stock (Eq, Ord, Generic)

-- | Range
--
-- 'Core.CXSourceRange' corresponds to @SourceRange@ in @clang@
-- <https://clang.llvm.org/doxygen/classclang_1_1SourceLocation.html>,
-- and therefore to @Range MultiLoc@; see 'MultiLoc' for additional discussion.
data Range a = Range {
      rangeStart :: !a
    , rangeEnd   :: !a
    }
  deriving stock (Eq, Ord, Generic)
  deriving stock (Functor, Foldable, Traversable)

{-------------------------------------------------------------------------------
  Comparisons
-------------------------------------------------------------------------------}

-- | Compare locations
--
-- Returns 'Nothing' if the locations aren't in the same file.
compareSingleLoc :: SingleLoc -> SingleLoc -> Maybe Ordering
compareSingleLoc a b = do
    guard $ singleLocPath a == singleLocPath b
    return $
      compare
        (singleLocLine a, singleLocColumn a)
        (singleLocLine b, singleLocColumn b)

-- | Check if a location falls within the given range
--
-- Treats the range as half-open, with an inclusive lower bound and exclusive
-- upper bound (following 'CXSourceRange').
--
-- Returns 'Nothing' if the three locations are not all in the same file.
rangeContainsLoc :: Range SingleLoc -> SingleLoc -> Maybe Bool
rangeContainsLoc Range{rangeStart, rangeEnd} loc = do
    afterStart <- (/= LT) <$> compareSingleLoc loc rangeStart
    beforeEnd  <- (== LT) <$> compareSingleLoc loc rangeEnd
    return $ afterStart && beforeEnd

{-------------------------------------------------------------------------------
  Show instances

  Technically speaking the validity of these instances depends on 'IsString'
  instances which we do not (yet?) define.
-------------------------------------------------------------------------------}

instance Show SingleLoc         where show = show . prettySingleLoc True
instance Show MultiLoc          where show = show . prettyMultiLoc  True
instance Show (Range SingleLoc) where show = show . prettyRangeSingleLoc
instance Show (Range MultiLoc)  where show = show . prettyRangeMultiLoc

deriving stock instance {-# OVERLAPPABLE #-} Show a => Show (Range a)

{-------------------------------------------------------------------------------
  Pretty-printing

  These instances mimick the behaviour of @SourceLocation::print@ and
  @SourceRange::print@ in @clang@.
-------------------------------------------------------------------------------}

type ShowFile = Bool

prettySingleLoc :: ShowFile -> SingleLoc -> String
prettySingleLoc showFile loc =
    intercalate ":" . concat $ [
        [ getSourcePath singleLocPath | showFile ]
      , [ show singleLocLine
        , show singleLocColumn
        ]
      ]
  where
    SingleLoc{singleLocPath, singleLocLine, singleLocColumn} = loc

prettyMultiLoc :: ShowFile -> MultiLoc -> String
prettyMultiLoc showFile multiLoc =
    intercalate " " . concat $ [
        [ prettySingleLoc showFile multiLocExpansion ]
      , [ "<Presumed=" ++ aux loc ++ ">" | Just loc <- [multiLocPresumed] ]
      , [ "<Spelling=" ++ aux loc ++ ">" | Just loc <- [multiLocSpelling] ]
      , [ "<File="     ++ aux loc ++ ">" | Just loc <- [multiLocFile]     ]
      ]
  where
    MultiLoc{
        multiLocExpansion
      , multiLocPresumed
      , multiLocSpelling
      , multiLocFile} = multiLoc

    aux :: SingleLoc -> [Char]
    aux loc =
        prettySingleLoc
          (singleLocPath loc /= singleLocPath multiLocExpansion)
          loc

prettyRangeSingleLoc :: Range SingleLoc -> String
prettyRangeSingleLoc = prettySourceRangeWith
      singleLocPath
      prettySingleLoc

prettyRangeMultiLoc :: Range MultiLoc -> String
prettyRangeMultiLoc =
    prettySourceRangeWith
      (singleLocPath . multiLocExpansion)
      prettyMultiLoc

prettySourceRangeWith ::
     (a -> SourcePath)
  -> (ShowFile -> a -> String)
  -> Range a -> String
prettySourceRangeWith path pretty Range{rangeStart, rangeEnd} = concat [
      "<"
    , pretty True rangeStart
    , "-"
    , pretty (path rangeStart /= path rangeEnd) rangeEnd
    , ">"
    ]

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

fromSingle :: HasCallStack => Core.CXTranslationUnit -> SingleLoc -> IO Core.CXSourceLocation
fromSingle unit SingleLoc{singleLocPath, singleLocLine, singleLocColumn} = do
     let SourcePath path = singleLocPath
     file <- Core.clang_getFile unit path
     Core.clang_getLocation
       unit
       file
       (fromIntegral singleLocLine)
       (fromIntegral singleLocColumn)

fromRange :: HasCallStack => Core.CXTranslationUnit -> Range SingleLoc -> IO Core.CXSourceRange
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
      singleLocPath   = SourcePath   singleLocPath
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

