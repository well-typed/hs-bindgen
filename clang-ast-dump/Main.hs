{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad
import Foreign.C.Types (CUInt)

import Data.Text qualified as T
import Options.Applicative qualified as OA

import HsBindgen.Clang.Args
import HsBindgen.Clang.Core
import HsBindgen.Clang.Doxygen
import HsBindgen.Clang.Util.Classification
import HsBindgen.Clang.Util.Fold
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Options = Options {
      optComments :: Bool
    , optExtents  :: Bool
    , optFile     :: FilePath
    , optKind     :: Bool
    , optParent   :: Bool
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
    void . clang_fold rootCursor $ \parent cursor -> do
      (cursorFile, _, _) <-
        clang_getPresumedLocation =<< clang_getCursorLocation cursor
      if optSameFile && cursorFile /= T.pack optFile
        then pure $ Continue Nothing
        else foldDecls opts parent cursor

foldDecls :: Options -> CXCursor -> CXCursor -> IO (Next ())
foldDecls opts@Options{..} parent cursor = do
    traceU_ 0 =<< clang_getCursorDisplayName cursor

    when optParent $
      traceU 1 "parent" =<< clang_getCursorDisplayName parent

    when optExtents $ do
      extent <- clang_getCursorExtent cursor
      (file, startLine, startCol) <-
        clang_getPresumedLocation =<< clang_getRangeStart extent
      (_, endLine, endCol) <-
        clang_getPresumedLocation =<< clang_getRangeEnd extent
      traceU 1 "extent" (file, (startLine, startCol), (endLine, endCol))

    cursorKind <- clang_getCursorKind cursor
    traceU 1 "cursor kind" cursorKind
    isDecl <- clang_isDeclaration cursorKind
    when optKind $
      traceWhen 2 "declaration" isDecl

    cursorType <- clang_getCursorType cursor
    let typeKind = cxtKind cursorType
    traceU 1 "cursor type" =<< clang_getTypeSpelling cursorType
    when optType $ do
      traceU 2 "kind" $ fromSimpleEnum typeKind
      traceU 3 "spelling" =<< clang_getTypeKindSpelling typeKind
      traceU 2 "canonical"
        =<< clang_getTypeSpelling
        =<< clang_getCanonicalType cursorType

    when (isPointerType typeKind) $
      traceU 1 "pointer"
        =<< clang_getTypeSpelling
        =<< clang_getPointeeType cursorType

    when (isDecl && optComments) $ do
      commentText <- clang_Cursor_getRawCommentText cursor
      unless (T.null commentText) $ do
        traceU 1 "comment" commentText
        traceU 2 "brief" =<< clang_Cursor_getBriefCommentText cursor
        dumpComment 2 Nothing =<< clang_Cursor_getParsedComment cursor

    let doRecurse  = pure $ Recurse (foldDecls opts) (pure . const Nothing)
        doContinue = Continue Nothing <$ traceL 1 "CONTINUE"
    case fromSimpleEnum cursorKind of
      Right CXCursor_StructDecl      -> doRecurse
      Right CXCursor_EnumDecl        -> doRecurse
      Right CXCursor_TypedefDecl     -> doRecurse
      Right CXCursor_MacroDefinition -> doRecurse
      _otherwise                     -> doContinue

dumpComment :: Int -> Maybe CUInt -> CXComment -> IO ()
dumpComment level mIdx comment = do
    commentKind <- clang_Comment_getKind comment
    maybe (traceU level) (traceO level) mIdx "kind" $ fromSimpleEnum commentKind
    traceWhen level1 "whitespace" =<< clang_Comment_isWhitespace comment

    when (fromSimpleEnum commentKind == Right CXComment_Text) $
      traceU level1 "text" =<< clang_TextComment_getText comment

    when (fromSimpleEnum commentKind == Right CXComment_InlineCommand) $ do
      traceU level1 "name" =<< clang_InlineCommandComment_getCommandName comment
      traceU level1 "render kind" . fromSimpleEnum
        =<< clang_InlineCommandComment_getRenderKind comment
      argsCount <- clang_InlineCommandComment_getNumArgs comment
      when (argsCount > 0) $ do
        traceU level1 "args" argsCount
        forM_ [0 .. argsCount - 1] $ \i ->
          traceO_ level2 i =<< clang_InlineCommandComment_getArgText comment i

    -- TODO other kinds

    childCount <- clang_Comment_getNumChildren comment
    when (childCount > 0) $ do
      traceU level1 "children" childCount
      forM_ [0 .. childCount - 1] $ \i ->
        dumpComment level2 (Just i) =<< clang_Comment_getChild comment i
  where
    level1, level2 :: Int
    level1 = level + 1
    level2 = level + 2

{-------------------------------------------------------------------------------
  Helper Functions
-------------------------------------------------------------------------------}

trace' :: Show a => Int -> Maybe Int -> Maybe String -> Maybe a -> IO ()
trace' level mIndex mLabel mValue = putStrLn $ concat
    [ replicate (level * 4) ' '
    , maybe "* " ((++ ". ") . show) mIndex
    , case (mLabel, mValue) of
        (Just label, Just value) -> label ++ ": " ++ show value
        (Just label, Nothing)    -> label
        (Nothing,    Just value) -> show value
        (Nothing,    Nothing)    -> ""
    ]

traceL :: Int -> String -> IO ()
traceL level label = trace' @() level Nothing (Just label) Nothing

traceU_ :: Show a => Int -> a -> IO ()
traceU_ level value = trace' level Nothing Nothing (Just value)

traceU :: Show a => Int -> String -> a -> IO ()
traceU level label value = trace' level Nothing (Just label) (Just value)

traceO_ :: (Integral i, Show a) => Int -> i -> a -> IO ()
traceO_ level index value =
    trace' level (Just $ fromIntegral index) Nothing (Just value)

traceO :: (Integral i, Show a) => Int -> i -> String -> a -> IO ()
traceO level index label value =
    trace' level (Just $ fromIntegral index) (Just label) (Just value)

traceWhen :: Int -> String -> Bool -> IO ()
traceWhen level label b = when b $ traceU level label b

{-------------------------------------------------------------------------------
  CLI
-------------------------------------------------------------------------------}

parseArgs :: IO Options
parseArgs = OA.execParser pinfo
  where
    pinfo :: OA.ParserInfo Options
    pinfo = OA.info (OA.helper <*> parseOptions) $ mconcat
      [ OA.fullDesc
      , OA.progDesc "Clang AST dump"
      , OA.failureCode 2
      ]

    parseOptions :: OA.Parser Options
    parseOptions = do
      optFile     <- fileArgument
      optComments <- mkFlag "comments"  "show comments"
      optExtents  <- mkFlag "extents"   "show extents"
      optKind     <- mkFlag "kind"      "show kind details"
      optParent   <- mkFlag "parent"    "show parent"
      optSameFile <- mkFlag "same-file" "only show from specified file"
      optType     <- mkFlag "type"      "show type details"
      pure Options{..}

    fileArgument :: OA.Parser FilePath
    fileArgument = OA.strArgument $ mconcat
      [ OA.metavar "FILE"
      , OA.help "C (header) file to parse"
      ]

    mkFlag :: String -> String -> OA.Parser Bool
    mkFlag flag doc = OA.switch $ OA.long flag <> OA.help doc

main :: IO ()
main = clangAstDump =<< parseArgs
