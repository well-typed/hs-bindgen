{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Foreign.C.Types (CUInt)

import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative qualified as OA

import HsBindgen.Clang.Args
import HsBindgen.Clang.Enum.Bitfield
import HsBindgen.Clang.Enum.Simple
import HsBindgen.Clang.HighLevel qualified as HighLevel
import HsBindgen.Clang.HighLevel.Types
import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Clang.LowLevel.Doxygen
import HsBindgen.Clang.Paths
import HsBindgen.Resolve (resolveHeader)

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Options = Options {
      optBuiltin           :: Bool
    , optComments          :: Bool
    , optExtents           :: Bool
    , optFile              :: CHeaderIncludePath
    , optKind              :: Bool
    , optQuoteIncludePath  :: [CIncludePathDir]
    , optSameFile          :: Bool
    , optSystemIncludePath :: [CIncludePathDir]
    , optType              :: Bool
    }

{-------------------------------------------------------------------------------
  Implementation
-------------------------------------------------------------------------------}

clangAstDump :: Options -> IO ()
clangAstDump opts@Options{..} = do
    putStrLn $ "## `" ++ renderCHeaderIncludePath optFile ++ "`"
    putStrLn ""

    src <- resolveHeader cArgs optFile
    HighLevel.withIndex DontDisplayDiagnostics $ \index ->
      HighLevel.withTranslationUnit index src cArgs [] cOpts $ \unit -> do
        rootCursor <- clang_getTranslationUnitCursor unit
        void . HighLevel.clang_visitChildren rootCursor $ \cursor -> do
          loc <- clang_getPresumedLocation =<< clang_getCursorLocation cursor
          case loc of
            (file, _, _)
              | optSameFile && SourcePath file /= src -> pure $ Continue Nothing
              | not optBuiltin && isBuiltIn file      -> pure $ Continue Nothing
              | otherwise                             -> foldDecls opts cursor
  where
    cArgs :: ClangArgs
    cArgs = defaultClangArgs {
        clangSystemIncludePathDirs = optSystemIncludePath
      , clangQuoteIncludePathDirs  = optQuoteIncludePath
      }

    cOpts :: BitfieldEnum CXTranslationUnit_Flags
    cOpts = bitfieldEnum [
        CXTranslationUnit_SkipFunctionBodies
      , CXTranslationUnit_DetailedPreprocessingRecord
      , CXTranslationUnit_IncludeAttributedTypes
      , CXTranslationUnit_VisitImplicitAttributes
      ]

    isBuiltIn :: Text -> Bool
    isBuiltIn = (`elem` [T.pack "<built-in>", T.pack "<command line>"])

foldDecls :: Options -> CXCursor -> IO (Next IO ())
foldDecls opts@Options{..} cursor = do
    traceU_ 0 =<< clang_getCursorDisplayName cursor

    dumpParents
    when optExtents dumpExtent

    cursorKind <- clang_getCursorKind cursor
    traceU 1 "cursor kind" cursorKind
    isDecl <- clang_isDeclaration cursorKind
    when optKind $
      traceWhen 2 "declaration" id isDecl

    cursorType <- clang_getCursorType cursor

    isRecurse <- case fromSimpleEnum cursorKind of
      Right CXCursor_StructDecl -> do
        dumpType cursorType isDecl
        pure True
      Right CXCursor_EnumDecl -> do
        dumpType cursorType isDecl
        traceU 1 "integer type" =<< clang_getEnumDeclIntegerType cursor
        pure True
      Right CXCursor_FieldDecl -> do
        dumpType cursorType isDecl
        traceU 1 "field offset" =<< clang_Cursor_getOffsetOfField cursor
        isBitField <- clang_Cursor_isBitField cursor
        when isBitField $
          traceU 1 "bit width" =<< clang_getFieldDeclBitWidth cursor
        pure False -- leaf
      Right CXCursor_EnumConstantDecl -> do
        dumpType cursorType isDecl
        traceU 1 "integer value" =<< clang_getEnumConstantDeclValue cursor
        pure False -- leaf
      Right CXCursor_FunctionDecl -> do
        dumpType cursorType isDecl
        numArgs <- clang_getNumArgTypes cursorType
        traceU 1 "args" numArgs
        forM_ [0 .. numArgs - 1] $ \i -> do
          argType <- clang_getArgType cursorType (fromIntegral i)
          traceO_ 2 i =<< clang_getTypeSpelling argType
        resultType <- clang_getResultType cursorType
        traceU 1 "result" =<< clang_getTypeSpelling resultType
        pure False
      Right CXCursor_TypedefDecl -> do
        dumpType cursorType isDecl
        traceU 1 "typedef name" =<< clang_getTypedefName cursorType
        pure True -- results in repeated information unless typedef is decl
      Right CXCursor_MacroDefinition ->
        -- TODO not defined yet
        pure True
      Right CXCursor_InclusionDirective ->
        pure False -- does not matter
      Right CXCursor_UnionDecl -> do
        dumpType cursorType isDecl
        pure True
      Right{} -> False <$ traceL 1 "CURSOR_KIND_NOT_IMPLEMENTED"
      Left n  -> False <$ traceU 1 "CURSOR_KIND_ENUM_OUT_OF_RANGE" n

    when (isDecl && optComments) $ do
      commentText <- clang_Cursor_getRawCommentText cursor
      unless (T.null commentText) $ do
        traceU 1 "comment" commentText
        traceU 2 "brief" =<< clang_Cursor_getBriefCommentText cursor
        dumpComment 2 Nothing =<< clang_Cursor_getParsedComment cursor

    pure $ if isRecurse
      then Recurse (foldDecls opts) (const Nothing)
      else Continue Nothing
  where
    dumpParents :: IO ()
    dumpParents = do
      semanticParent <- clang_getCursorSemanticParent cursor
      lexicalParent  <- clang_getCursorLexicalParent  cursor
      parentsEq      <- clang_equalCursors semanticParent lexicalParent
      if parentsEq
        then
          traceU 1 "parent"
            =<< clang_getCursorDisplayName semanticParent
        else do
          traceU 1 "semantic parent"
            =<< clang_getCursorDisplayName semanticParent
          traceU 1 "lexical parent"
            =<< clang_getCursorDisplayName lexicalParent

    dumpExtent :: IO ()
    dumpExtent = do
      extent <- clang_getCursorExtent cursor
      (file, startLine, startCol) <-
        clang_getPresumedLocation =<< clang_getRangeStart extent
      (_, endLine, endCol) <-
        clang_getPresumedLocation =<< clang_getRangeEnd extent
      traceU 1 "extent" (file, (startLine, startCol), (endLine, endCol))

    dumpType :: CXType -> Bool -> IO ()
    dumpType cursorType isDecl = do
      traceU 1 "cursor type" =<< clang_getTypeSpelling cursorType
      when optType $ do
        let typeKind = cxtKind cursorType
        traceU 2 "kind" $ fromSimpleEnum typeKind
        traceU 3 "spelling" =<< clang_getTypeKindSpelling typeKind
        traceU 2 "canonical"
          =<< clang_getTypeSpelling =<< clang_getCanonicalType cursorType
        when isDecl $ do
          decl <- HighLevel.classifyDeclaration cursor
          case decl of
            DeclarationRegular -> do
              traceU 2 "declaration type" ("DeclarationRegular" :: String)
              traceU 2 "sizeof"
                =<< handleCallFailed (clang_Type_getSizeOf cursorType)
              traceU 2 "alignment"
                =<< handleCallFailed (clang_Type_getAlignOf cursorType)
            DeclarationForward{} ->
              traceU 2 "declaration type" ("DeclarationForward" :: String)
            DeclarationOpaque ->
              traceU 2 "declaration type" ("DeclarationOpaque" :: String)

dumpComment :: Int -> Maybe CUInt -> CXComment -> IO ()
dumpComment level mIdx comment = do
    commentKind <- clang_Comment_getKind comment
    maybe (traceU level) (traceO level) mIdx "kind" $ fromSimpleEnum commentKind
    traceWhen level1 "whitespace" id =<< clang_Comment_isWhitespace comment

    case fromSimpleEnum commentKind of
      Right CXComment_Null -> pure ()

      Right CXComment_Text ->
        traceU level1 "text" =<< clang_TextComment_getText comment

      Right CXComment_InlineCommand -> do
        traceU level1 "name"
          =<< clang_InlineCommandComment_getCommandName comment
        traceU level1 "render kind" . fromSimpleEnum
          =<< clang_InlineCommandComment_getRenderKind comment
        numArgs <- clang_InlineCommandComment_getNumArgs comment
        when (numArgs > 0) $ do
          traceU level1 "args" numArgs
          forM_ [0 .. numArgs - 1] $ \i ->
            traceO_ level2 i =<< clang_InlineCommandComment_getArgText comment i

      Right CXComment_HTMLStartTag -> do
        traceU level1 "name" =<< clang_HTMLTagComment_getTagName comment
        traceWhen level1 "self-closing" id
          =<< clang_HTMLStartTagComment_isSelfClosing comment
        numAttrs <- clang_HTMLStartTag_getNumAttrs comment
        when (numAttrs > 0) $ do
          traceU level1 "attributes" numAttrs
          forM_ [0 .. numAttrs - 1] $ \i -> do
            traceO_ level2 i =<< clang_HTMLStartTag_getAttrName comment i
            traceU level3 "value" =<< clang_HTMLStartTag_getAttrValue comment i

      Right CXComment_HTMLEndTag -> do
        traceU level1 "name" =<< clang_HTMLTagComment_getTagName comment

      Right CXComment_Paragraph -> pure ()

      Right CXComment_BlockCommand -> do
        traceU level1 "name" =<<
          clang_BlockCommandComment_getCommandName comment
        numArgs <- clang_BlockCommandComment_getNumArgs comment
        when (numArgs > 0) $ do
          traceU level1 "args" numArgs
          forM_ [0 .. numArgs - 1] $ \i ->
            traceO_ level2 i =<< clang_BlockCommandComment_getArgText comment i
        -- NOTE paragraph is also provided by children
        -- para <- clang_BlockCommandComment_getParagraph comment
        -- traceL level1 "paragraph"
        -- dumpComment level2 Nothing para

      Right CXComment_ParamCommand -> do
        traceU level1 "name" =<< clang_ParamCommandComment_getParamName comment
        traceU level1 "index"
          =<< clang_ParamCommandComment_getParamIndex comment
        traceUnless level2 "invalid" id
          =<< clang_ParamCommandComment_isParamIndexValid comment
        traceU level1 "direction" . fromSimpleEnum
          =<< clang_ParamCommandComment_getDirection comment
        traceU level2 "explicit"
          =<< clang_ParamCommandComment_isDirectionExplicit comment

      Right CXComment_TParamCommand -> do
        traceU level1 "name" =<< clang_TParamCommandComment_getParamName comment
        traceUnless level1 "position invalid" id
          =<< clang_TParamCommandComment_isParamPositionValid comment
        depth <- clang_TParamCommandComment_getDepth comment
        traceU level1 "depth" depth
        forM_ [0 .. depth] $ \i ->
          traceO level2 i "index"
            =<< clang_TParamCommandComment_getIndex comment i

      Right CXComment_VerbatimBlockCommand -> pure ()

      Right CXComment_VerbatimBlockLine ->
        traceU level1 "text" =<< clang_VerbatimBlockLineComment_getText comment

      Right CXComment_VerbatimLine ->
        traceU level1 "text" =<< clang_VerbatimLineComment_getText comment

      Right CXComment_FullComment -> do
        traceUnless level "HTML" T.null =<< clang_FullComment_getAsHTML comment
        traceUnless level "XML"  T.null =<< clang_FullComment_getAsXML  comment

      Left n -> traceU level1 "COMMENT_KIND_ENUM_OUT_OF_RANGE" n

    numChildren <- clang_Comment_getNumChildren comment
    when (numChildren > 0) $ do
      traceU level1 "children" numChildren
      forM_ [0 .. numChildren - 1] $ \i ->
        dumpComment level2 (Just i) =<< clang_Comment_getChild comment i
  where
    level1, level2, level3 :: Int
    level1 = level + 1
    level2 = level + 2
    level3 = level + 3

{-------------------------------------------------------------------------------
  Error handling
-------------------------------------------------------------------------------}

-- | Handle 'CallFailed' exceptions, returning the hint on error
handleCallFailed :: forall a m.
     MonadIO m
  => IO a
  -> m (Either String a)
handleCallFailed action = liftIO $ first getHint <$> try action
  where
    getHint :: CallFailed -> String
    getHint (CallFailed hint _backtrace) = hint

{-------------------------------------------------------------------------------
  Trace Functions
-------------------------------------------------------------------------------}

-- | Trace Markdown list item (internal)
--
-- This function is not meant to be used directly.  Use one of the below
-- functions instead.
trace' :: (Show a, MonadIO m)
  => Int           -- ^ indentation level, from 0
  -> Maybe Int     -- ^ index of ordered list
  -> Maybe String  -- ^ label
  -> Maybe a       -- ^ value
  -> m ()
trace' level mIndex mLabel mValue = liftIO . putStrLn $ concat
    [ replicate (level * 4) ' '
    , maybe "* " ((++ ". ") . show) mIndex
    , case (mLabel, mValue) of
        (Just label, Just value) -> label ++ ": " ++ show value
        (Just label, Nothing)    -> label
        (Nothing,    Just value) -> show value
        (Nothing,    Nothing)    -> ""
    ]

-- | Trace label (unordered)
traceL :: MonadIO m
  => Int     -- ^ indentation level, from 0
  -> String  -- ^ label
  -> m ()
traceL level label = trace' @() level Nothing (Just label) Nothing

-- | Trace value (unordered, no label)
traceU_ :: (Show a, MonadIO m)
  => Int  -- ^ indentation level, from 0
  -> a    -- ^ value
  -> m ()
traceU_ level value = trace' level Nothing Nothing (Just value)

-- | Trace value (unordered, with label)
traceU :: (Show a, MonadIO m)
  => Int     -- ^ indentation level, from 0
  -> String  -- ^ label
  -> a       -- ^ value
  -> m ()
traceU level label value = trace' level Nothing (Just label) (Just value)

-- | Trace value (ordered, no label)
traceO_ :: (Integral i, Show a, MonadIO m)
  => Int  -- ^ indentation level, from 0
  -> i    -- ^ index of ordered list
  -> a    -- ^ value
  -> m ()
traceO_ level index value =
    trace' level (Just $ fromIntegral index) Nothing (Just value)

-- | Trace value (ordered, with label)
traceO :: (Integral i, Show a, MonadIO m)
  => Int     -- ^ indentation level, from 0
  -> i       -- ^ index of ordered list
  -> String  -- ^ label
  -> a       -- ^ value
  -> m ()
traceO level index label value =
    trace' level (Just $ fromIntegral index) (Just label) (Just value)

-- | Trace value when predicate holds (unordered, with label)
traceWhen :: (Show a, MonadIO m)
  => Int          -- ^ indentation level, from 0
  -> String       -- ^ label
  -> (a -> Bool)  -- ^ predicate
  -> a            -- ^ value
  -> m ()
traceWhen level label p value = when (p value) $ traceU level label value

-- | Trace value when predicate does not hold (unordered, with label)
traceUnless :: (Show a, MonadIO m)
  => Int          -- ^ indentation level, from 0
  -> String       -- ^ label
  -> (a -> Bool)  -- ^ predicate
  -> a            -- ^ value
  -> m ()
traceUnless level label p value = unless (p value) $ traceU level label value

{-------------------------------------------------------------------------------
  CLI
-------------------------------------------------------------------------------}

main :: IO ()
main = clangAstDump . uncurry applyAll =<< OA.execParser pinfo
  where
    pinfo :: OA.ParserInfo (Bool, Options)
    pinfo = OA.info (OA.helper <*> parseOptions) $ mconcat
      [ OA.fullDesc
      , OA.progDesc "Clang AST dump"
      , OA.failureCode 2
      ]

    parseOptions :: OA.Parser (Bool, Options)
    parseOptions = do
      -- flags enabled by all flag
      optComments <- mkFlag "comments"  "show comments"
      optExtents  <- mkFlag "extents"   "show extents"
      optKind     <- mkFlag "kind"      "show kind details"
      optType     <- mkFlag "type"      "show type details"
      -- all flag
      optAll      <- mkFlag "all"       "enable all above flags"
      -- other options/arguments
      optBuiltin  <- mkFlag "builtin"   "show builtin macros"
      optSameFile <- mkFlag "same-file" "only show from specified file"
      optSystemIncludePath <- systemIncludePathOption
      optQuoteIncludePath  <- quoteIncludePathOption
      optFile              <- fileArgument
      pure (optAll, Options{..})

    systemIncludePathOption :: OA.Parser [CIncludePathDir]
    systemIncludePathOption = OA.many . OA.strOption $ mconcat
      [ OA.long "system-include-path"
      , OA.metavar "DIR"
      , OA.help "System include search path directory"
      ]

    quoteIncludePathOption :: OA.Parser [CIncludePathDir]
    quoteIncludePathOption = OA.many . OA.strOption $ mconcat
      [ OA.short 'I'
      , OA.long "include-path"
      , OA.metavar "DIR"
      , OA.help "Quote include search path directory"
      ]

    fileArgument :: OA.Parser CHeaderIncludePath
    fileArgument =
      OA.argument
        (OA.eitherReader $ first displayException . parseCHeaderIncludePath)
        $ mconcat [
              OA.metavar "FILE"
            , OA.help "C (header) file to parse"
            ]

    mkFlag :: String -> String -> OA.Parser Bool
    mkFlag flag doc = OA.switch $ OA.long flag <> OA.help doc

    applyAll :: Bool -> Options -> Options
    applyAll False opts = opts
    applyAll True  opts = opts {
        optComments = True
      , optExtents  = True
      , optKind     = True
      , optType     = True
      }
