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
import Control.Tracer (Tracer)
import Data.List qualified as List
import Data.List.Compat ((!?))
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import GHC.Stack (callStack)

import Clang.Args
import Clang.Enum.Bitfield
import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import Data.DynGraph (DynGraph)
import Data.DynGraph qualified as DynGraph
import HsBindgen.C.Predicate (Predicate)
import HsBindgen.C.Tc.Macro qualified as Macro
import HsBindgen.Clang.Args (ExtraClangArgsLog, withExtraClangArgs)
import HsBindgen.Errors
import HsBindgen.ExtBindings
import HsBindgen.Frontend (processTranslationUnit)
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.Pass.Parse.Monad (ParseEnv (ParseEnv))
import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Imports
import HsBindgen.Util.Trace (Trace (TraceDiagnostic, TraceExtraClangArgs, TraceParse))
import HsBindgen.Util.Tracer (TraceWithCallStack, traceWithCallStack, useTrace)

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
     Tracer IO (TraceWithCallStack Trace)
  -> ClangArgs
  -> Predicate
  -> ExtBindings
  -> [CHeaderIncludePath]
  -> IO ([SourcePath], C.TranslationUnit) -- ^ List of included headers and parsed header
parseCHeaders tracer args predicate extBindings headerIncludePaths =
  withExtraClangArgs (useTrace TraceExtraClangArgs tracer) args $ \args' ->
    HighLevel.withIndex DontDisplayDiagnostics $ \index ->
      HighLevel.withUnsavedFile hFilePath hContent $ \file ->
        HighLevel.withTranslationUnit2 index RootHeader.name args' [file] opts $
          \case
            Left err -> throwIO $ ParseCHeadersUnknownError err
            Right unit -> do
              (errors, warnings) <- List.partition diagnosticIsError
                <$> HighLevel.clang_getDiagnostics unit Nothing
              unless (null errors) $ throwIO (getError errors)
              forM_ warnings $ traceWithCallStack
                                 (useTrace TraceDiagnostic tracer)
                                 callStack

              let mainSourcePaths = Set.fromList $ RootHeader.name :
                    map (SourcePath . Text.pack . getCHeaderIncludePath)
                      headerIncludePaths
                  parseTracer = useTrace TraceParse tracer
                  parseEnvironment :: ParseEnv
                  parseEnvironment = ParseEnv unit predicate mainSourcePaths parseTracer
              unit' <- processTranslationUnit parseEnvironment
              print unit'

              error "UHOH"

{-
              (decls, finalDeclState) <-
                C.runFoldState C.initDeclState $
                  HighLevel.clang_visitChildren rootCursor $
                    C.foldDecls (useTrace TraceSkipped tracer) p extBindings headerIncludePaths unit
              let decls' =
                    [ d
                    | C.TypeDecl _ d <-
                        toList (C.typeDeclarations finalDeclState)
                    ]
                  depPaths = List.delete C.RootHeader.name $
                    DynGraph.topSort (C.cIncludePathGraph finalDeclState)
              return (depPaths, C.Header (sortDecls depPaths (decls ++ decls')))
-}
  where
    hFilePath :: FilePath
    hFilePath = getSourcePath RootHeader.name

    hContent :: String
    hContent = RootHeader.content headerIncludePaths

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
      guard $ singleLocPath sloc == RootHeader.name
      guard $ " file not found" `Text.isSuffixOf` diagnosticSpelling
      headerIncludePath <- headerIncludePaths !? (singleLocLine sloc - 1)
      return $ ParseCHeadersInputFileNotFound headerIncludePath

{-------------------------------------------------------------------------------
  Debugging/development
-------------------------------------------------------------------------------}

getTargetTriple ::
  Tracer IO (TraceWithCallStack ExtraClangArgsLog) -> ClangArgs -> IO Text
getTargetTriple tracer args =
  withExtraClangArgs tracer args $ \args' ->
    HighLevel.withIndex DontDisplayDiagnostics $ \index ->
      HighLevel.withUnsavedFile hName hContent $ \file ->
        HighLevel.withTranslationUnit2 index hPath args' [file] opts $
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
