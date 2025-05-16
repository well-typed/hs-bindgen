module Clang.HighLevel.Diagnostics (
    Diagnostic(..)
  , FixIt(..)
  , clang_getDiagnostics
  , diagnosticIsError
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text qualified as Text
import Foreign.C

import Clang.Enum.Bitfield
import Clang.Enum.Simple
import Clang.HighLevel.SourceLoc (MultiLoc, Range)
import Clang.HighLevel.SourceLoc qualified as SourceLoc
import Clang.LowLevel.Core

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

-- TODO: Probably separate into Info/Warning/Error (issue #175).
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
     MonadIO m
  => CXTranslationUnit
  -> Maybe (BitfieldEnum CXDiagnosticDisplayOptions)
     -- ^ Display options for constructing 'diagnosticFormatted'
     --
     -- If 'Nothing', uses 'clang_defaultDiagnosticDisplayOptions'.
  -> m [Diagnostic]
clang_getDiagnostics unit mDisplayOptions = do
    displayOptions <- case mDisplayOptions of
                        Just displayOptions -> return displayOptions
                        Nothing -> clang_defaultDiagnosticDisplayOptions
    getAll unit clang_getNumDiagnostics $ getDiagnostic displayOptions

{-------------------------------------------------------------------------------
  Get all diagnostics
-------------------------------------------------------------------------------}

getDiagnostic ::
     MonadIO m
  => BitfieldEnum CXDiagnosticDisplayOptions
  -> CXTranslationUnit
  -> CUInt
  -> m Diagnostic
getDiagnostic displayOptions unit i = liftIO $
    bracket (clang_getDiagnostic unit i) clang_disposeDiagnostic $
      reify displayOptions

getDiagnosticInSet ::
     MonadIO m
  => BitfieldEnum CXDiagnosticDisplayOptions
  -> CXDiagnosticSet
  -> CUInt
  -> m Diagnostic
getDiagnosticInSet displayOptions set i = liftIO $
    bracket (clang_getDiagnosticInSet set i) clang_disposeDiagnostic $
      reify displayOptions

reify ::
     MonadIO m
  => BitfieldEnum CXDiagnosticDisplayOptions
  -> CXDiagnostic
  -> m Diagnostic
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
    nonEmpty :: Text -> Maybe Text
    nonEmpty bs
      | Text.null bs = Nothing
      | otherwise    = Just bs

getChildDiagnostics ::
     MonadIO m
  => BitfieldEnum CXDiagnosticDisplayOptions
  -> CXDiagnostic
  -> m [Diagnostic]
getChildDiagnostics displayOptions diag = do
    set <- clang_getChildDiagnostics diag
    getAll set clang_getNumDiagnosticsInSet $
      getDiagnosticInSet displayOptions

getDiagnosticFixIt ::
     MonadIO m
  => CXDiagnostic
  -> CUInt
  -> m FixIt
getDiagnosticFixIt diag i =
    uncurry FixIt <$> SourceLoc.clang_getDiagnosticFixIt diag i

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getAll :: Monad m => a -> (a -> m CUInt) -> (a -> CUInt -> m b) -> m [b]
getAll x getCount getElem = do
    count <- getCount x
    if count == 0
      then return []
      else mapM (getElem x) [0 .. pred count]
