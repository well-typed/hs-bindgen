module HsBindgen.Clang.HighLevel.Diagnostics (
    Diagnostic(..)
  , FixIt(..)
  , clang_getDiagnostics
  , diagnosticIsError
  ) where

import Control.Exception
import Data.Text (Text)
import Data.Text qualified as Text
import Foreign.C

import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Clang.HighLevel.SourceLoc (MultiLoc, Range)
import HsBindgen.Clang.HighLevel.SourceLoc qualified as SourceLoc
import HsBindgen.Runtime.Patterns

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Diagnostic = Diagnostic {
      -- | Formatted by @libclang@ in a manner that is suitable for display
      diagnosticFormatted :: Text

      -- | Severity
    , diagnosticSeverity :: SimpleEnum CXDiagnosticSeverity

      -- | Source location (where Clang would print the caret @^@)
    , diagnosticLocation  :: MultiLoc

      -- | Text of the diagnostic
    , diagnosticSpelling  :: Text

      -- | The command line option that enabled this diagnostic
    , diagnosticOption :: Maybe Text

      -- | The @libclang@ option to disable this option
    , diagnosticDisabledBy :: Maybe Text

      -- | Diagnostic category
    , diagnosticCategory :: Int

      -- | Rendered category
    , diagnosticCategoryText :: Text

      -- | Source range associated with the diagnostic
      --
      -- A diagnostic's source ranges highlight important elements in the source
      -- code. On the command line, Clang displays source ranges by underlining
      -- them with @~@ characters.
    , diagnosticRanges :: [Range MultiLoc]

      -- | Fix-it hints
    , diagnosticFixIts :: [FixIt]

      -- | Child diagnostics
    , diagnosticChildren  :: [Diagnostic]
    }
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Suggestion to fix the code
--
-- Fix-its are described in terms of a source range whose contents should be
-- replaced by a string. This approach generalizes over three kinds of
-- operations: removal of source code (the range covers the code to be removed
-- and the replacement string is empty), replacement of source code (the range
-- covers the code to be replaced and the replacement string provides the new
-- code), and insertion (both the start and end of the range point at the
-- insertion location, and the replacement string provides the text to insert).
data FixIt = FixIt {
      -- | Replacement range
      --
      -- The replacement range is the source range whose contents will be
      -- replaced with the returned replacement string. Note that source ranges
      -- are half-open ranges [a, b), so the source code should be replaced from
      -- a and up to (but not including) b.
      fixItRange :: Range MultiLoc

      -- | Text that should replace the source code
    , fixItReplacement :: Text
    }
  deriving stock (Show)

diagnosticIsError :: Diagnostic -> Bool
diagnosticIsError diag =
    case fromSimpleEnum (diagnosticSeverity diag) of
      Right CXDiagnostic_Error -> True
      Right CXDiagnostic_Fatal -> True
      Left _unknownSeverity    -> True
      Right _otherwise         -> False

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

clang_getDiagnostics ::
     Maybe FilePath -- ^ Directory to make paths relative to
  -> CXTranslationUnit
  -> Maybe (BitfieldEnum CXDiagnosticDisplayOptions)
     -- ^ Display options for constructing 'diagnosticFormatted'
     --
     -- If 'Nothing', uses 'clang_defaultDiagnosticDisplayOptions'.
  -> IO [Diagnostic]
clang_getDiagnostics relPath unit mDisplayOptions = do
    displayOptions <- case mDisplayOptions of
                        Just displayOptions -> return displayOptions
                        Nothing -> clang_defaultDiagnosticDisplayOptions
    getAll unit clang_getNumDiagnostics $ getDiagnostic relPath displayOptions

{-------------------------------------------------------------------------------
  Get all diagnostics
-------------------------------------------------------------------------------}

getDiagnostic ::
     Maybe FilePath -- ^ Directory to make paths relative to
  -> BitfieldEnum CXDiagnosticDisplayOptions
  -> CXTranslationUnit
  -> CUInt -> IO Diagnostic
getDiagnostic relPath displayOptions unit i =
    bracket (clang_getDiagnostic unit i) clang_disposeDiagnostic $
      reify relPath displayOptions

getDiagnosticInSet ::
     Maybe FilePath -- ^ Directory to make paths relative to
  -> BitfieldEnum CXDiagnosticDisplayOptions
  -> CXDiagnosticSet -> CUInt -> IO Diagnostic
getDiagnosticInSet relPath displayOptions set i =
    bracket (clang_getDiagnosticInSet set i) clang_disposeDiagnostic $
      reify relPath displayOptions

reify ::
     Maybe FilePath -- ^ Directory to make paths relative to
  -> BitfieldEnum CXDiagnosticDisplayOptions
  -> CXDiagnostic -> IO Diagnostic
reify relPath displayOptions diag = do
    diagnosticFormatted    <- clang_formatDiagnostic diag displayOptions
    diagnosticSeverity     <- clang_getDiagnosticSeverity diag
    diagnosticLocation     <- SourceLoc.clang_getDiagnosticLocation relPath diag
    diagnosticSpelling     <- clang_getDiagnosticSpelling diag
    (mOption, mDisabledBy) <- clang_getDiagnosticOption diag
    diagnosticCategory     <- fromIntegral <$> clang_getDiagnosticCategory diag
    diagnosticCategoryText <- clang_getDiagnosticCategoryText diag
    diagnosticRanges       <- getAll diag clang_getDiagnosticNumRanges $
                                SourceLoc.clang_getDiagnosticRange relPath
    diagnosticFixIts       <- getAll diag clang_getDiagnosticNumFixIts $
                                getDiagnosticFixIt relPath
    diagnosticChildren     <- getChildDiagnostics relPath displayOptions diag
    return $ Diagnostic {
          diagnosticFormatted
        , diagnosticSeverity
        , diagnosticLocation
        , diagnosticSpelling
        , diagnosticOption     = nonEmpty mOption
        , diagnosticDisabledBy = nonEmpty mDisabledBy
        , diagnosticCategory
        , diagnosticCategoryText
        , diagnosticRanges
        , diagnosticFixIts
        , diagnosticChildren
        }
  where
    nonEmpty :: Text -> Maybe Text
    nonEmpty bs
      | Text.null bs = Nothing
      | otherwise    = Just bs

getChildDiagnostics ::
     Maybe FilePath -- ^ Directory to make paths relative to
  -> BitfieldEnum CXDiagnosticDisplayOptions
  -> CXDiagnostic -> IO [Diagnostic]
getChildDiagnostics relPath displayOptions diag = do
    set <- clang_getChildDiagnostics diag
    getAll set clang_getNumDiagnosticsInSet $
      getDiagnosticInSet relPath displayOptions

getDiagnosticFixIt ::
     Maybe FilePath -- ^ Directory to make paths relative to
  -> CXDiagnostic
  -> CUInt
  -> IO FixIt
getDiagnosticFixIt relPath diag i =
    uncurry FixIt <$> SourceLoc.clang_getDiagnosticFixIt relPath diag i

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getAll :: a -> (a -> IO CUInt) -> (a -> CUInt -> IO b) -> IO [b]
getAll x getCount getElem = do
    count <- getCount x
    if count == 0
      then return []
      else mapM (getElem x) [0 .. pred count]
