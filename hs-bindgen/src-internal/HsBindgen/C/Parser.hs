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

import Clang.Args
import Clang.Enum.Bitfield
import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import HsBindgen.BindingSpec (ResolvedBindingSpec)
import HsBindgen.C.Predicate (Predicate)
import HsBindgen.Clang.Args (ExtraClangArgsMsg, withExtraClangArgs)
import HsBindgen.Errors
import HsBindgen.Frontend (processTranslationUnit)
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.Pass.Slice (ProgramSlicing)
import HsBindgen.Frontend.RootHeader (RootHeader)
import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Imports
import HsBindgen.TraceMsg
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
     HasCallStack =>
     Tracer IO TraceMsg
  -> ClangArgs
  -> Predicate
  -> ProgramSlicing
  -> ResolvedBindingSpec
  -> [CHeaderIncludePath]
  -> IO C.TranslationUnit
parseCHeaders tracer args predicate programSlicing extSpec mainFiles =
  withExtraClangArgs (contramap TraceExtraClangArgs tracer) args $ \args' ->
    HighLevel.withIndex DontDisplayDiagnostics $ \index ->
      HighLevel.withUnsavedFile hFilePath hContent $ \file -> do
        let onFailure :: SimpleEnum CXErrorCode -> IO a
            onFailure err = throwIO $ ParseCHeadersUnknownError err
        HighLevel.withTranslationUnit2 index (Just RootHeader.name) args' [file] opts onFailure $ \unit -> do
          (errors, warnings) <- List.partition diagnosticIsError
            <$> HighLevel.clang_getDiagnostics unit Nothing
          unless (null errors) $ throwIO (getError errors)
          forM_ warnings $ traceWith (contramap TraceDiagnostic tracer)
          processTranslationUnit
            (contramap TraceFrontend tracer)
            extSpec
            rootHeader
            predicate
            programSlicing
            unit
  where
    rootHeader :: RootHeader
    rootHeader = RootHeader.fromMainFiles mainFiles

    hFilePath :: FilePath
    hFilePath = getSourcePath RootHeader.name

    hContent :: String
    hContent = RootHeader.content rootHeader

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
      headerIncludePath <- RootHeader.lookup rootHeader sloc
      guard $ " file not found" `Text.isSuffixOf` diagnosticSpelling
      return $ ParseCHeadersInputFileNotFound headerIncludePath

{-------------------------------------------------------------------------------
  Debugging/development
-------------------------------------------------------------------------------}

getTargetTriple :: Tracer IO ExtraClangArgsMsg -> ClangArgs -> IO Text
getTargetTriple tracer args =
  withExtraClangArgs tracer args $ \args' ->
    HighLevel.withIndex DontDisplayDiagnostics $ \index ->
      HighLevel.withUnsavedFile hName hContent $ \file -> do
        let onFailure :: SimpleEnum CXErrorCode -> IO Text
            onFailure err = panicPure $
                   "Clang parse translation unit error while getting target triple: "
                ++ show err
        HighLevel.withTranslationUnit2 index (Just hPath) args' [file] opts onFailure $ \unit ->
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

