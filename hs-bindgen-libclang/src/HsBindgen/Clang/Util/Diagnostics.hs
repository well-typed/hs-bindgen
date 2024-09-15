module HsBindgen.Clang.Util.Diagnostics (
    Diagnostic(..)
  , FixIt(..)
  , getDiagnostics
  , isError
  ) where

import Control.Exception
import Data.ByteString (ByteString)
import Foreign.C

import HsBindgen.Clang.Core
import HsBindgen.Clang.Util.SourceLoc (SourceLoc, SourceRange)
import HsBindgen.Clang.Util.SourceLoc qualified as SourceLoc
import HsBindgen.Patterns
import Data.ByteString qualified as BS

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Diagnostic = Diagnostic {
      -- | Formatted by @libclang@ in a manner that is suitable for display
      diagnosticFormatted :: ByteString

      -- | Severity
    , diagnosticSeverity :: SimpleEnum CXDiagnosticSeverity

      -- | Source location (where Clang would print the caret @^@)
    , diagnosticLocation  :: SourceLoc

      -- | Text of the diagnostic
    , diagnosticSpelling  :: ByteString

      -- | The command line option that enabled this diagnostic
    , diagnosticOption :: Maybe ByteString

      -- | The @libclang@ option to disable this option
    , diagnosticDisabledBy :: Maybe ByteString

      -- | Diagnostic category
    , diagnosticCategory :: Int

      -- | Rendered category
    , diagnosticCategoryText :: ByteString

      -- | Source range associated with the diagnostic
      --
      -- A diagnostic's source ranges highlight important elements in the source
      -- code. On the command line, Clang displays source ranges by underlining
      -- them with @~@ characters.
    , diagnosticRanges :: [SourceRange]

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
      fixItRange :: SourceRange

      -- | Text that should replace the source code
    , fixItReplacement :: ByteString
    }
  deriving stock (Show)

isError :: Diagnostic -> Bool
isError diag =
    case fromSimpleEnum (diagnosticSeverity diag) of
      Right CXDiagnostic_Error -> True
      Right CXDiagnostic_Fatal -> True
      Left _unknownSeverity    -> True
      Right _otherwise         -> False

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

getDiagnostics ::
     CXTranslationUnit
  -> Maybe (BitfieldEnum CXDiagnosticDisplayOptions)
     -- ^ Display options for constructing 'diagnosticFormatted'
     --
     -- If 'Nothing', uses 'clang_defaultDiagnosticDisplayOptions'.
  -> IO [Diagnostic]
getDiagnostics unit mDisplayOptions = do
    displayOptions <- case mDisplayOptions of
                        Just displayOptions -> return displayOptions
                        Nothing -> clang_defaultDiagnosticDisplayOptions
    getAll unit clang_getNumDiagnostics $ getDiagnostic displayOptions

{-------------------------------------------------------------------------------
  Get all diagnostics
-------------------------------------------------------------------------------}

getDiagnostic ::
     BitfieldEnum CXDiagnosticDisplayOptions
  -> CXTranslationUnit
  -> CUInt -> IO Diagnostic
getDiagnostic displayOptions unit i =
    bracket (clang_getDiagnostic unit i) clang_disposeDiagnostic $
      reify displayOptions

getDiagnosticInSet ::
     BitfieldEnum CXDiagnosticDisplayOptions
  -> CXDiagnosticSet -> CUInt -> IO Diagnostic
getDiagnosticInSet displayOptions set i =
    bracket (clang_getDiagnosticInSet set i) clang_disposeDiagnostic $
      reify displayOptions

reify ::
     BitfieldEnum CXDiagnosticDisplayOptions
  -> CXDiagnostic -> IO Diagnostic
reify displayOptions diag = do
    diagnosticFormatted    <- clang_formatDiagnostic diag displayOptions
    diagnosticSeverity     <- clang_getDiagnosticSeverity diag
    diagnosticLocation     <- SourceLoc.clang_getDiagnosticLocation diag
    diagnosticSpelling     <- clang_getDiagnosticSpelling diag
    (mOption, mDisabledBy) <- clang_getDiagnosticOption diag
    diagnosticCategory     <- fromIntegral <$> clang_getDiagnosticCategory diag
    diagnosticCategoryText <- clang_getDiagnosticCategoryText diag
    diagnosticRanges       <- getAll diag clang_getDiagnosticNumRanges $
                                SourceLoc.clang_getDiagnosticRange
    diagnosticFixIts       <- getAll diag clang_getDiagnosticNumFixIts $
                                getDiagnosticFixIt
    diagnosticChildren     <- getChildDiagnostics displayOptions diag
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
    nonEmpty :: ByteString -> Maybe ByteString
    nonEmpty bs
      | BS.null bs = Nothing
      | otherwise  = Just bs

getChildDiagnostics ::
     BitfieldEnum CXDiagnosticDisplayOptions
  -> CXDiagnostic -> IO [Diagnostic]
getChildDiagnostics displayOptions diag = do
    set <- clang_getChildDiagnostics diag
    getAll set clang_getNumDiagnosticsInSet $ getDiagnosticInSet displayOptions

getDiagnosticFixIt :: CXDiagnostic -> CUInt -> IO FixIt
getDiagnosticFixIt diag i =
    uncurry FixIt <$> SourceLoc.clang_getDiagnosticFixIt diag i

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getAll :: a -> (a -> IO CUInt) -> (a -> CUInt -> IO b) -> IO [b]
getAll x getCount getElem = do
    count <- getCount x
    if count == 0
      then return []
      else mapM (getElem x) [0 .. pred count]
