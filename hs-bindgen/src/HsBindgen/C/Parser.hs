-- | Process the @clang@ C AST.
--
-- Intended for qualified import.
--
-- > import HsBindgen.C.Parser qualified as C
module HsBindgen.C.Parser (
    -- * Main entry point into libclang
    TranslationUnitException(..)
  , withTranslationUnit
    -- * Processing the 'CXTranslationUnit'
  , getTranslationUnitTargetTriple
  , foldTranslationUnitWith
  ) where

import Control.Exception
import Data.List (partition)
import System.IO

import HsBindgen.Clang.Args
import HsBindgen.Clang.HighLevel qualified as HighLevel
import HsBindgen.Clang.HighLevel.Types
import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Imports
import HsBindgen.Runtime.Enum.Bitfield
import HsBindgen.Runtime.Enum.Simple
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Main entry point into libclang
-------------------------------------------------------------------------------}

-- | Failed to parse the C source
--
-- This is thrown by 'withTranslationUnit'.
data TranslationUnitException =
    -- | Errors in the C file
    --
    -- TODO: <https://github.com/well-typed/hs-bindgen/issues/174> We should
    -- have a pretty renderer for diagnostics. For now we rely on
    -- 'diagnosticFormatted'.
    TranslationUnitCErrors [Text]

    -- | We cannot open the file (does not exist, permissions, ...)
    --
    -- @libclang@ reports 'CXError_Failure' in this case; we try to produce a
    -- more useful exception.
 | TranslationUnitCannotOpen FilePath

    -- | We failed to process file for some other reason
  | TranslationUnitUnknownError (SimpleEnum CXErrorCode)
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Parse C file
--
-- Maybe throw 'TranslationUnitException'.
withTranslationUnit ::
     Maybe FilePath        -- ^ Directory to make paths relative to
  -> Tracer IO Diagnostic  -- ^ Tracer for warnings
  -> ClangArgs
  -> FilePath
  -> (CXTranslationUnit -> IO r)
  -> IO r
withTranslationUnit relPath tracer args fp k = do
    -- checkFileExists fp

    index  <- clang_createIndex DontDisplayDiagnostics
    mUnit  <- clang_parseTranslationUnit2 index fp args flags

    case mUnit of
      Right unit -> do
        diags  <- HighLevel.clang_getDiagnostics relPath unit Nothing

        let errors, warnings :: [Diagnostic]
            (errors, warnings) = partition diagnosticIsError diags

        let _unused = warnings

        case errors of
          [] -> do
            -- TODO: <https://github.com/well-typed/hs-bindgen/issues/175>
            -- We should print warnings only optionally.
            forM_ warnings $ traceWith tracer Warning
            k unit
          errs ->
            throwIO $ TranslationUnitCErrors $ map diagnosticFormatted errs
      Left err -> do
        if err == simpleEnum CXError_Failure then do
          -- Attempt to find the cause of the failure. Our diagnosis here might
          -- be wrong (for example, it's theoretically possible that we report
          -- that the file does not exist because it was deleted /after/ the
          -- call to @libclang@, and the failure was really a different one),
          -- but for now we'll just accept that limitation in order to get
          -- more helpful error messages in the majority of cases.
          mCanOpen <- try $ withFile fp ReadMode $ \_h -> return ()
          case mCanOpen of
            Right () ->
              throwIO $ TranslationUnitUnknownError err
            Left (_ :: IOException) ->
              throwIO $ TranslationUnitCannotOpen fp
        else
          throwIO $ TranslationUnitUnknownError err
  where
    flags :: BitfieldEnum CXTranslationUnit_Flags
    flags = bitfieldEnum [
          CXTranslationUnit_SkipFunctionBodies
        , CXTranslationUnit_DetailedPreprocessingRecord
        , CXTranslationUnit_IncludeAttributedTypes
        , CXTranslationUnit_VisitImplicitAttributes
        ]


{-------------------------------------------------------------------------------
  Processing the 'CXTranslationUnit'
-------------------------------------------------------------------------------}

getTranslationUnitTargetTriple :: CXTranslationUnit -> IO Text
getTranslationUnitTargetTriple unit =
    bracket
        (clang_getTranslationUnitTargetInfo unit)
        clang_TargetInfo_dispose
        clang_TargetInfo_getTriple

foldTranslationUnitWith :: MonadUnliftIO m =>
     CXTranslationUnit
  -> (m [a] -> IO b)
  -> Fold m a
  -> IO b
foldTranslationUnitWith unit runFold fold = do
    cursor <- clang_getTranslationUnitCursor unit
    runFold $ HighLevel.clang_visitChildren cursor fold
