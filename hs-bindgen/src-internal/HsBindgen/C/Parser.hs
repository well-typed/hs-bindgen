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
import Data.List.Compat ((!?))
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text

import Clang.Args
import Clang.Enum.Bitfield
import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import Data.DynGraph qualified as DynGraph
import HsBindgen.C.AST qualified as C
import HsBindgen.C.Fold qualified as C
import HsBindgen.C.Fold.DeclState qualified as C
import HsBindgen.C.Predicate (Predicate)
import HsBindgen.Errors
import HsBindgen.ExtBindings
import HsBindgen.Imports
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
                    DynGraph.topSort $ C.cIncludePathGraph finalDeclState
              return (depPaths, C.Header (sortDecls depPaths (decls ++ decls')))
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

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- | Sort declarations by source location
--
-- 1. Source path, in specified order
-- 2. Line number
-- 3. Column number
sortDecls :: [SourcePath] -> [C.Decl] -> [C.Decl]
sortDecls sourcePaths = List.sortOn (aux . getSingleLoc)
  where
    getSingleLoc :: C.Decl -> SingleLoc
    getSingleLoc = \case
      C.DeclStruct s       -> C.structSourceLoc s
      C.DeclOpaqueStruct o -> C.opaqueStructSourceLoc o
      C.DeclUnion u        -> C.unionSourceLoc u
      C.DeclTypedef t      -> C.typedefSourceLoc t
      C.DeclEnum e         -> C.enumSourceLoc e
      C.DeclOpaqueEnum o   -> C.opaqueEnumSourceLoc o
      C.DeclMacro m        -> case m of
        C.MacroReparseError{} -> C.macroReparseErrorSourceLoc m
        C.MacroTcError{}      -> C.macroTcErrorSourceLoc m
        C.MacroDecl{}         -> C.macroDeclSourceLoc m
      C.DeclFunction f     -> C.functionSourceLoc f

    aux :: SingleLoc -> (Int, Int, Int)
    aux sloc =
        ( Map.findWithDefault maxBound (singleLocPath sloc) sourceMap
        , singleLocLine sloc
        , singleLocColumn sloc
        )

    sourceMap :: Map SourcePath Int
    sourceMap = Map.fromList $ zip sourcePaths [0..]
