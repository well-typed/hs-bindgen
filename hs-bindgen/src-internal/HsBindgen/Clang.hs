-- | Top-level call into @clang@, with @hs-bindgen@ specific features
module HsBindgen.Clang (
    -- * Top-level call into @clang@
    ClangSetup(..)
  , ClangInput(..)
  , defaultClangSetup
  , withClang
  , withClang'
    -- * Trace messages
  , ClangMsg(..)
  , infoHelpMessage
  ) where

import Data.Text qualified as Text
import Text.SimplePrettyPrint ((><))
import Text.SimplePrettyPrint qualified as PP

import Clang.Args
import Clang.Enum.Bitfield
import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Top-level call into clang
-------------------------------------------------------------------------------}

data ClangSetup = ClangSetup{
      args        :: ClangArgs
    , diagnostics :: DisplayDiagnostics
    , input       :: ClangInput
    , flags       :: BitfieldEnum CXTranslationUnit_Flags
    }
  deriving stock (Show, Eq)

instance PrettyForTrace ClangSetup where
  prettyForTrace = PP.show

data ClangInput =
    ClangInputFile SourcePath
  | ClangInputMemory FilePath String
  deriving stock (Show, Eq)

defaultClangSetup :: ClangArgs -> ClangInput -> ClangSetup
defaultClangSetup args input = ClangSetup{
      args        = args
    , diagnostics = DontDisplayDiagnostics
    , input       = input
    , flags       = bitfieldEnum [CXTranslationUnit_DetailedPreprocessingRecord]
    }

-- | Call clang to parse with the specified 'ClangSetup'
--
-- All diagnostics are traced.  'Nothing' is returned if any of them are errors.
-- The specified continuation is called only when there are no error
-- diagnostics.
withClang :: forall a.
     Tracer ClangMsg
  -> ClangSetup
  -> (CXTranslationUnit -> IO (Maybe a))
  -> IO (Maybe a)
withClang tracer setup k = withClang' tracer setup $ \unit -> do
    anyIsError <- traceDiagnostics unit
    if anyIsError
      then return Nothing
      else k unit
  where
    traceDiagnostics :: CXTranslationUnit -> IO Bool
    traceDiagnostics unit =
        go False =<< HighLevel.clang_getDiagnostics unit Nothing
      where
        go :: Bool -> [Diagnostic] -> IO Bool
        go !anyIsError []     = return anyIsError
        go !anyIsError (d:ds) = do
            traceWith (contramap ClangDiagnostic tracer) d
            go (anyIsError || diagnosticIsError d) ds

-- | Call clang to parse with the specified 'ClangSetup'
--
-- Diagnostics are not traced, and the specified continuation is called even if
-- there are error diagnostics.
--
-- This function is needed for @resolveHeaders@, where we need the paths for the
-- resolved headers even if some headers are not found.
withClang' :: forall a.
     Tracer ClangMsg
  -> ClangSetup
  -> (CXTranslationUnit -> IO (Maybe a))
  -> IO (Maybe a)
withClang' tracer setup k = do
    traceWith tracer $ ClangSetupMsg setup
    HighLevel.withIndex setup.diagnostics $ \index -> do
      let withUnit :: SourcePath -> [CXUnsavedFile] -> IO (Maybe a)
          withUnit path unsaved =
             HighLevel.withTranslationUnit2
               index
               (Just path)
               setup.args
               unsaved
               setup.flags
               onErrorCode
               k
      case setup.input of
        ClangInputFile path ->
          withUnit path []
        ClangInputMemory path contents -> do
          HighLevel.withUnsavedFile path contents $ \file  ->
            withUnit (SourcePath $ Text.pack path) [file]
  where
    onErrorCode :: SimpleEnum CXErrorCode -> IO (Maybe a)
    onErrorCode err = do
        traceWith tracer $ ClangErrorCode err
        return Nothing

{-------------------------------------------------------------------------------
  Log messages
-------------------------------------------------------------------------------}

-- | Errors and warnings resulting from interaction with clang
data ClangMsg =
    ClangErrorCode (SimpleEnum CXErrorCode)
  | ClangDiagnostic Diagnostic
  | ClangSetupMsg ClangSetup
  | ClangInvokedWithoutOptions
  deriving stock (Show)

instance PrettyForTrace ClangMsg where
  prettyForTrace = \case
      ClangErrorCode  x -> "clang error " >< PP.show x
      ClangDiagnostic diag
        | RootHeader.isInRootHeader diag.diagnosticLocation -> PP.text $
            case getFileNotFound diag.diagnosticSpelling of
              Just header -> "unable to resolve #include <" <> header <> ">"
              Nothing     -> case getFileNotFoundQ diag.diagnosticSpelling of
                Just header ->
                  "unable to resolve #include <" <> header
                    <> "> (must specify header relative to directory in C include search path)"
                Nothing     ->
                  Text.stripStart $ Text.dropWhile (/= ' ') diag.diagnosticFormatted
        | otherwise -> PP.text diag.diagnosticFormatted
      ClangSetupMsg   x -> prettyForTrace x
      ClangInvokedWithoutOptions ->
        PP.cat $ map PP.text infoHelpMessage
    where
      getFileNotFound :: Text -> Maybe Text
      getFileNotFound =
        fmap (Text.dropWhile (== '\'')) . Text.stripSuffix "' file not found"

      getFileNotFoundQ :: Text -> Maybe Text
      getFileNotFoundQ =
          fmap (Text.dropWhile (== '\'') . Text.dropWhile (/= '\''))
        . Text.stripSuffix "' file not found with <angled> include; use \"quotes\" instead"

infoHelpMessage :: [Text]
infoHelpMessage =
  [ "This command provides a way to get output from libclang."
  , " For example, use --clang-option=-v to see version and include"
  , " search path information, taking into account any other Clang"
  , " options and environment variables."
  ]


instance IsTrace Level ClangMsg where
  getDefaultLogLevel = \case
      ClangErrorCode  _          -> Error
      ClangDiagnostic x          -> if diagnosticIsError x then Error else Warning
      ClangSetupMsg   _          -> Debug
      ClangInvokedWithoutOptions -> Notice
  getSource = \case
      ClangErrorCode  _          -> Libclang
      ClangDiagnostic _          -> Libclang
      ClangSetupMsg   _          -> HsBindgen
      ClangInvokedWithoutOptions -> Libclang
  getTraceId = const "clang"
