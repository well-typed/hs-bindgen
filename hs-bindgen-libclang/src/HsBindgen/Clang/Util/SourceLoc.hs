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
    SourcePath(..)
  , SourceLoc(..)
  , SourceRange(..)
    -- * Construction
  , clang_Cursor_getSpellingNameRange
  , clang_getCursorExtent
  , clang_getCursorLocation
  , clang_getDiagnosticFixIt
  , clang_getDiagnosticLocation
  , clang_getDiagnosticRange
  , clang_getTokenExtent
  , clang_getTokenLocation
    -- * Low-level
  , toSourcePath
  , toSourceLoc
  , toSourceRange
  ) where

import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as Text
import Foreign.C
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal(..))

import HsBindgen.Clang.Core qualified as Core
import HsBindgen.Clang.Core hiding (
    clang_Cursor_getSpellingNameRange
  , clang_getCursorExtent
  , clang_getCursorLocation
  , clang_getDiagnosticFixIt
  , clang_getDiagnosticLocation
  , clang_getDiagnosticRange
  , clang_getTokenExtent
  , clang_getTokenLocation
  )

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Paths as reported by @libclang@
--
-- Clang uses UTF-8 internally for everything, including paths, which is why
-- this is 'Text', not 'OsPath'. There might still be differences between
-- platforms of course (such as directory separators).
newtype SourcePath = SourcePath {
      getSourcePath :: Text
    }
  deriving newtype (Eq, Ord)

data SourceLoc = SourceLoc {
      sourceLocFile   :: !SourcePath
    , sourceLocLine   :: !Int
    , sourceLocColumn :: !Int
    }
  deriving stock (Eq, Ord, Generic)

data SourceRange = SourceRange {
      sourceRangeStart :: !SourceLoc
    , sourceRangeEnd   :: !SourceLoc
    }
  deriving stock (Eq, Ord, Generic)

{-------------------------------------------------------------------------------
  Show instances

  These are defined so that we could, if we wanted to, also provide an inverse
  'IsString' instance; this is the reason for the @show . pretty...@
-------------------------------------------------------------------------------}

instance Show SourcePath where
  show = show . getSourcePath

instance Show SourceLoc where
  show = show . prettySourceLoc True

instance Show SourceRange where
  show = show . prettySourceRange

instance PrettyVal SourcePath where
  prettyVal = prettyVal . show

instance PrettyVal SourceLoc where
  prettyVal = prettyVal . show

instance PrettyVal SourceRange where
  prettyVal = prettyVal . show

prettySourceLoc ::
     Bool -- ^ Should we show the file?
  -> SourceLoc -> String
prettySourceLoc showFile (SourceLoc file line col) =
    intercalate ":" . concat $ [
        [ Text.unpack (getSourcePath file) | showFile ]
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
  -> IO (SourceRange, Text)
clang_getDiagnosticFixIt diag i = do
    (range, bs) <- Core.clang_getDiagnosticFixIt diag i
    (,bs) <$> toSourceRange range

clang_getTokenLocation :: CXTranslationUnit -> CXToken -> IO SourceLoc
clang_getTokenLocation unit token =
    toSourceLoc =<< Core.clang_getTokenLocation unit token

clang_getTokenExtent :: CXTranslationUnit -> CXToken -> IO SourceRange
clang_getTokenExtent unit token =
    toSourceRange =<< Core.clang_getTokenExtent unit token

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

toSourcePath :: Text -> SourcePath
toSourcePath = SourcePath

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
      <$> (toSourcePath <$> clang_getFileName file)
      <*> (pure $ fromIntegral line)
      <*> (pure $ fromIntegral col)
