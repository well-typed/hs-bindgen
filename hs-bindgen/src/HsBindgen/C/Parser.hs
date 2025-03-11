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
  , foldTranslationUnitWith
    -- * Parsing
  , parseCHeader
    -- * Debugging/development
  , getTargetTriple
  ) where

import Control.Exception
import Data.List (partition)
import Data.Text qualified as Text
import System.IO

import Data.DynGraph qualified as DynGraph
import HsBindgen.Clang.Args
import HsBindgen.C.AST qualified as C
import HsBindgen.C.Fold qualified as C
import HsBindgen.C.Fold.DeclState qualified as C
import HsBindgen.C.Predicate (Predicate)
import HsBindgen.Clang.HighLevel qualified as HighLevel
import HsBindgen.Clang.HighLevel.Types
import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Clang.Paths
import HsBindgen.Errors
import HsBindgen.ExtBindings
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
     Tracer IO Diagnostic  -- ^ Tracer for warnings
  -> ClangArgs
  -> SourcePath
  -> (CXTranslationUnit -> IO r)
  -> IO r
withTranslationUnit tracer args src k =
    HighLevel.withIndex DontDisplayDiagnostics $ \index ->
      HighLevel.withTranslationUnit2 index src args [] flags $ \case
        Right unit -> do
          diags  <- HighLevel.clang_getDiagnostics unit Nothing
          let errors, warnings :: [Diagnostic]
              (errors, warnings) = partition diagnosticIsError diags
          unless (null errors) . throwIO $
            TranslationUnitCErrors (map diagnosticFormatted errors)
          -- TODO: <https://github.com/well-typed/hs-bindgen/issues/175>
          -- We should print warnings only optionally.
          forM_ warnings $ traceWith tracer Warning
          k unit
        Left err
          | err == simpleEnum CXError_Failure -> do
              -- Attempt to find the cause of the failure. Our diagnosis here
              -- might be wrong (for example, it's theoretically possible that
              -- we report that the file does not exist because it was deleted
              -- /after/ the call to @libclang@, and the failure was really a
              -- different one), but for now we'll just accept that limitation
              -- in order to get more helpful error messages in the majority of
              -- cases.
              mCanOpen <- try $ withFile fp ReadMode $ \_h -> return ()
              case mCanOpen of
                Right () ->
                  throwIO $ TranslationUnitUnknownError err
                Left (_ :: IOException) ->
                  throwIO $ TranslationUnitCannotOpen fp
          | otherwise -> throwIO $ TranslationUnitUnknownError err
  where
    flags :: BitfieldEnum CXTranslationUnit_Flags
    flags = bitfieldEnum [
          CXTranslationUnit_SkipFunctionBodies
        , CXTranslationUnit_DetailedPreprocessingRecord
        , CXTranslationUnit_IncludeAttributedTypes
        , CXTranslationUnit_VisitImplicitAttributes
        ]

    fp :: FilePath
    fp = getSourcePath src

{-------------------------------------------------------------------------------
  Processing the 'CXTranslationUnit'
-------------------------------------------------------------------------------}

foldTranslationUnitWith :: MonadUnliftIO m =>
     CXTranslationUnit
  -> (m [a] -> IO b)
  -> Fold m a
  -> IO b
foldTranslationUnitWith unit runFold fold = do
    cursor <- clang_getTranslationUnitCursor unit
    runFold $ HighLevel.clang_visitChildren cursor fold

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

parseCHeader ::
     Tracer IO C.Skipped
  -> ExtBindings                 -- ^ External bindings
  -> Predicate                   -- ^ Selection predicate
  -> CXTranslationUnit
  -> IO ([SourcePath], C.Header) -- ^ List of included headers and parsed header
parseCHeader skipTracer extBindings p unit = do
    (decls, finalDeclState) <-
      foldTranslationUnitWith
        unit
        (C.runFoldState C.initDeclState)
        (C.foldDecls skipTracer p extBindings unit)
    let decls' =
          [ d | C.TypeDecl _ d <- toList (C.typeDeclarations finalDeclState) ]
        depPaths = DynGraph.vertices $ C.cIncludePathGraph finalDeclState
    return (depPaths, C.Header (decls ++ decls'))

{-------------------------------------------------------------------------------
  Debugging/development
-------------------------------------------------------------------------------}

getTargetTriple :: ClangArgs -> IO Text
getTargetTriple args =
    HighLevel.withIndex DontDisplayDiagnostics $ \index ->
      HighLevel.withUnsavedFile hName hContent $ \unsavedFile ->
        HighLevel.withTranslationUnit2 index hPath args [unsavedFile] opts $
          \case
            Left err -> panicPure $
              "Clang parse translation unit error while getting target triple: "
                ++ show err
            Right unit ->
              bracket
                (clang_getTranslationUnitTargetInfo unit)
                clang_TargetInfo_dispose
                clang_TargetInfo_getTriple
  where
    hName :: FilePath
    hName = "hs-bindgen-triple.h"

    hPath :: SourcePath
    hPath = SourcePath $ Text.pack hName

    hContent :: String
    hContent = ""

    opts :: BitfieldEnum CXTranslationUnit_Flags
    opts = bitfieldEnum []
