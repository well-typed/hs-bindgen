-- | Process the @clang@ C AST.
--
-- Intended for qualified import.
--
-- > import HsBindgen.C.Parser qualified as C
module HsBindgen.C.Parser (
    -- * Main entry point into libclang
    CErrors(..)
  , withTranslationUnit
    -- * Processing the 'CXTranslationUnit'
  , getTranslationUnitTargetTriple
  , foldTranslationUnitWith
  ) where

import Control.Exception
import Control.Monad
import Data.List (partition)
import Data.Text (Text)

import HsBindgen.Clang.Args
import HsBindgen.Clang.Core
import HsBindgen.Clang.Util.Diagnostics (Diagnostic(..))
import HsBindgen.Clang.Util.Diagnostics qualified as Diagnostics
import HsBindgen.Clang.Util.Fold
import HsBindgen.Patterns
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Main entry point into libclang
-------------------------------------------------------------------------------}

-- | Errors in the C source
--
-- This is thrown by 'withTranslationUnit'.
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/174>
-- We should have a pretty renderer for diagnostics. For now we rely on
-- 'diagnosticFormatted'.
data CErrors = CErrors [Text]
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Parse C file
--
-- Throws 'CErrors' if @libclang@ reported any errors in the C file.
withTranslationUnit ::
     Tracer IO Diagnostic  -- ^ Tracer for warnings
  -> ClangArgs
  -> FilePath
  -> (CXTranslationUnit -> IO r)
  -> IO r
withTranslationUnit tracer args fp k = do
    index  <- clang_createIndex DontDisplayDiagnostics
    unit   <- clang_parseTranslationUnit index fp args flags
    diags  <- Diagnostics.getDiagnostics unit Nothing

    let errors, warnings :: [Diagnostic]
        (errors, warnings) = partition Diagnostics.isError diags

    let _unused = warnings

    case errors of
      [] -> do
        -- TODO: <https://github.com/well-typed/hs-bindgen/issues/175>
        -- We should print warnings only optionally.
        forM_ warnings $ traceWith tracer Warning
        k unit
      errs ->
        throwIO $ CErrors $ map diagnosticFormatted errs
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

foldTranslationUnitWith ::
     CXTranslationUnit
  -> (FoldM m [a] -> IO b)
  -> Fold m a
  -> IO b
foldTranslationUnitWith unit runFold fold = do
    cursor <- clang_getTranslationUnitCursor unit
    runFold $ clang_fold cursor fold
