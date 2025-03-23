-- | Process the @clang@ C AST.
--
-- Intended for qualified import.
--
-- > import HsBindgen.C.Parser qualified as C
module HsBindgen.C.Parser (
    -- * Parsing
    ParseCHeadersException(..)
  , parseCHeaders
    -- * Debugging/development
  , getTargetTriple
  ) where

import Control.Exception
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text

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
import HsBindgen.Util.Compat ((!?))
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

-- | Failed to parse the C source
--
-- This is thrown by 'parseCHeaders'.
data ParseCHeadersException =
    -- | Input header file not found
    ParseCHeadersInputFileNotFound CHeaderIncludePath

    -- | Errors in the C file
    --
    -- TODO: <https://github.com/well-typed/hs-bindgen/issues/174> We should
    -- have a pretty renderer for diagnostics. For now we rely on
    -- 'diagnosticFormatted'.
  | ParseCHeadersCErrors [Text]

    -- | We failed to process file for some other reason
  | ParseCHeadersUnknownError (SimpleEnum CXErrorCode)
  deriving stock (Show)

instance Exception ParseCHeadersException where
  toException = hsBindgenExceptionToException
  fromException = hsBindgenExceptionFromException
  displayException = \case
    ParseCHeadersInputFileNotFound path ->
      "header not found: " ++ getCHeaderIncludePath path
    ParseCHeadersCErrors errs -> unlines $ map Text.unpack errs
    ParseCHeadersUnknownError errCode ->
      "unknown error parsing C headers: " ++ show errCode

parseCHeaders ::
     Tracer IO Diagnostic  -- ^ Tracer for warnings
  -> Tracer IO C.Skipped
  -> ClangArgs
  -> Predicate
  -> ExtBindings
  -> [CHeaderIncludePath]
  -> IO ([SourcePath], C.Header) -- ^ List of included headers and parsed header
parseCHeaders diagTracer skipTracer args p extBindings headerIncludePaths =
    HighLevel.withIndex DontDisplayDiagnostics $ \index ->
      HighLevel.withUnsavedFile hFilePath hContent $ \file ->
        HighLevel.withTranslationUnit2 index C.rootHeaderName args [file] opts $
          \case
            Left err -> throwIO $ ParseCHeadersUnknownError err
            Right unit -> do
              (errors, warnings) <- List.partition diagnosticIsError
                <$> HighLevel.clang_getDiagnostics unit Nothing
              unless (null errors) $ throwIO (getError errors)
              -- TODO: <https://github.com/well-typed/hs-bindgen/issues/175>
              -- We should print warnings only optionally.
              forM_ warnings $ traceWith diagTracer Warning
              rootCursor <- clang_getTranslationUnitCursor unit
              (decls, finalDeclState) <-
                C.runFoldState C.initDeclState $
                  HighLevel.clang_visitChildren rootCursor $
                    C.foldDecls skipTracer p extBindings headerIncludePaths unit
              let decls' =
                    [ d
                    | C.TypeDecl _ d <-
                        toList (C.typeDeclarations finalDeclState)
                    ]
                  depPaths = List.delete C.rootHeaderName $
                    DynGraph.vertices $ C.cIncludePathGraph finalDeclState
              return (depPaths, C.Header (decls ++ decls'))
  where
    hFilePath :: FilePath
    hFilePath = getSourcePath C.rootHeaderName

    hContent :: String
    hContent = C.rootHeaderContent headerIncludePaths

    opts :: BitfieldEnum CXTranslationUnit_Flags
    opts = bitfieldEnum [
          CXTranslationUnit_SkipFunctionBodies
        , CXTranslationUnit_DetailedPreprocessingRecord
        , CXTranslationUnit_IncludeAttributedTypes
        , CXTranslationUnit_VisitImplicitAttributes
        ]

    getError :: [Diagnostic] -> ParseCHeadersException
    getError diags =
      case (Maybe.listToMaybe (mapMaybe getInputFileNotFoundError diags)) of
        Just e  -> e
        Nothing -> ParseCHeadersCErrors $ map diagnosticFormatted diags

    getInputFileNotFoundError :: Diagnostic -> Maybe ParseCHeadersException
    getInputFileNotFoundError Diagnostic{..} = do
      let sloc = multiLocExpansion diagnosticLocation
      guard $ singleLocPath sloc == C.rootHeaderName
      guard $ " file not found" `Text.isSuffixOf` diagnosticSpelling
      headerIncludePath <- headerIncludePaths !? (singleLocLine sloc - 1)
      return $ ParseCHeadersInputFileNotFound headerIncludePath

{-------------------------------------------------------------------------------
  Debugging/development
-------------------------------------------------------------------------------}

getTargetTriple :: ClangArgs -> IO Text
getTargetTriple args =
    HighLevel.withIndex DontDisplayDiagnostics $ \index ->
      HighLevel.withUnsavedFile hName hContent $ \file ->
        HighLevel.withTranslationUnit2 index hPath args [file] opts $
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
