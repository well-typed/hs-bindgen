{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Functor.Identity
import Foreign.C.Types (CUInt)

import Data.Text qualified as T
import Options.Applicative qualified as OA

import HsBindgen.Clang.Args
import HsBindgen.Clang.HighLevel qualified as HighLevel
import HsBindgen.Clang.HighLevel.Types
import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Clang.LowLevel.Doxygen
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Options = Options {
      optComments :: Bool
    , optExtents  :: Bool
    , optFile     :: FilePath
    , optKind     :: Bool
    , optSameFile :: Bool
    , optType     :: Bool
    }

{-------------------------------------------------------------------------------
  Implementation
-------------------------------------------------------------------------------}

clangAstDump :: Options -> IO ()
clangAstDump opts@Options{..} = do
    putStrLn $ "## `" ++ optFile ++ "`"
    putStrLn ""
    index <- clang_createIndex DontDisplayDiagnostics
    let clangOpts = bitfieldEnum [CXTranslationUnit_None]
    unit <- clang_parseTranslationUnit index optFile defaultClangArgs clangOpts
    rootCursor <- clang_getTranslationUnitCursor unit
    void . runFoldIdentity . HighLevel.clang_visitChildren rootCursor $
      \cursor -> do
        (cursorFile, _, _) <- liftIO $
          clang_getPresumedLocation =<< clang_getCursorLocation cursor
        if optSameFile && cursorFile /= T.pack optFile
          then pure $ Continue Nothing
          else foldDecls opts cursor

foldDecls :: Options -> CXCursor -> FoldM Identity (Next Identity ())
foldDecls opts@Options{..} cursor = do
    traceU_ 0 =<< liftIO (clang_getCursorDisplayName cursor)

    semanticParent <- liftIO $ clang_getCursorSemanticParent cursor
    lexicalParent  <- liftIO $ clang_getCursorLexicalParent  cursor
    parentsEq      <- liftIO $ clang_equalCursors semanticParent lexicalParent
    if parentsEq
      then
        traceWhen 1 "parent" (/= T.pack optFile)
          =<< liftIO (clang_getCursorDisplayName semanticParent)
      else do
        traceU 1 "semantic parent"
          =<< liftIO (clang_getCursorDisplayName semanticParent)
        traceU 1 "lexical parent"
          =<< liftIO (clang_getCursorDisplayName lexicalParent)

    when optExtents $ do
      extent <- liftIO $ clang_getCursorExtent cursor
      (file, startLine, startCol) <- liftIO $
        clang_getPresumedLocation =<< clang_getRangeStart extent
      (_, endLine, endCol) <- liftIO $
        clang_getPresumedLocation =<< clang_getRangeEnd extent
      traceU 1 "extent" (file, (startLine, startCol), (endLine, endCol))

    cursorKind <- liftIO $ clang_getCursorKind cursor
    traceU 1 "cursor kind" cursorKind
    isDecl <- liftIO $ clang_isDeclaration cursorKind
    when optKind $
      traceWhen 2 "declaration" id isDecl

    cursorType <- liftIO $ clang_getCursorType cursor
    let typeKind = cxtKind cursorType
    traceU 1 "cursor type" =<< liftIO (clang_getTypeSpelling cursorType)
    when optType $ do
      traceU 2 "kind" $ fromSimpleEnum typeKind
      traceU 3 "spelling" =<< liftIO (clang_getTypeKindSpelling typeKind)
      traceU 2 "canonical"
        =<< liftIO (clang_getTypeSpelling =<< clang_getCanonicalType cursorType)

    when (isDecl && optComments) $ do
      commentText <- liftIO $ clang_Cursor_getRawCommentText cursor
      unless (T.null commentText) $ do
        traceU 1 "comment" commentText
        traceU 2 "brief" =<< liftIO (clang_Cursor_getBriefCommentText cursor)
        liftIO $ dumpComment 2 Nothing =<< clang_Cursor_getParsedComment cursor

    let doRecurse  = pure $ Recurse (foldDecls opts) (const Nothing)
        --doContinue = pure $ Continue Nothing
    case fromSimpleEnum cursorKind of
      Right CXCursor_StructDecl      -> doRecurse
      Right CXCursor_EnumDecl        -> doRecurse
      Right CXCursor_TypedefDecl     -> doRecurse
      Right CXCursor_MacroDefinition -> doRecurse
      Right{} -> Continue Nothing <$ traceL 1 "CURSOR_KIND_NOT_IMPLEMENTED"
      Left n  -> Continue Nothing <$ traceU 1 "CURSOR_KIND_ENUM_OUT_OF_RANGE" n

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
      optSameFile <- mkFlag "same-file" "only show from specified file"
      optFile     <- fileArgument
      pure (optAll, Options{..})

    fileArgument :: OA.Parser FilePath
    fileArgument = OA.strArgument $ mconcat
      [ OA.metavar "FILE"
      , OA.help "C (header) file to parse"
      ]

    mkFlag :: String -> String -> OA.Parser Bool
    mkFlag flag doc = OA.switch $ OA.long flag <> OA.help doc

    applyAll :: Bool -> Options -> Options
    applyAll False opts = opts
    applyAll True Options{..} = Options {
        optComments = True
      , optExtents  = True
      , optFile     = optFile
      , optKind     = True
      , optSameFile = optSameFile
      , optType     = True
      }
