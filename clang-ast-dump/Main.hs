{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad
import Foreign.C.Types (CUInt)

import Options.Applicative qualified as OA

import HsBindgen.Clang.Args
import HsBindgen.Clang.Core
import HsBindgen.Clang.Doxygen
import HsBindgen.Clang.Util.Classification
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Options = Options {
      optComments :: Bool
    , optExtents  :: Bool
    }

{-------------------------------------------------------------------------------
  Implementation
-------------------------------------------------------------------------------}

clangAstDump :: Options -> FilePath -> IO ()
clangAstDump opts path = do
    putStrLn $ "## `" ++ path ++ "`"
    putStrLn ""
    index <- clang_createIndex DontDisplayDiagnostics
    unit <- clang_parseTranslationUnit
              index
              path
              defaultClangArgs
              (bitfieldEnum [CXTranslationUnit_None])
    cursor <- clang_getTranslationUnitCursor unit
    terminatedPrematurely <- clang_visitChildren cursor $ visit opts
    when terminatedPrematurely $ do
      putStrLn ""
      putStrLn "BREAK"

visit :: Options -> CXCursor -> CXCursor -> IO (SimpleEnum CXChildVisitResult)
visit Options{..} cursor _cursorParent = do
    traceU_ 0 =<< clang_getCursorDisplayName cursor

    cursorType <- clang_getCursorType cursor
    let typeKind = cxtKind cursorType
    traceU 1 "cursor type kind" $ fromSimpleEnum typeKind
    traceU 2 "spelling" =<< clang_getTypeKindSpelling typeKind

    when (isPointerType typeKind) $
      traceU 1 "pointer"
        =<< clang_getTypeSpelling
        =<< clang_getPointeeType cursorType

    when (isRecordType typeKind) $
      traceU 1 "record"
        =<< clang_getTypeSpelling cursorType

    isDecl <- clang_isDeclaration =<< clang_getCursorKind cursor
    traceWhen 1 "declaration" isDecl

    when optExtents $ do
      cursorExtent <- clang_getCursorExtent cursor
      traceU 1 "extent start" . (\(_, line, col, _) -> (line, col))
        =<< clang_getExpansionLocation
        =<< clang_getRangeStart cursorExtent
      traceU 1 "extent end" . (\(_, line, col, _) -> (line, col))
        =<< clang_getExpansionLocation
        =<< clang_getRangeEnd cursorExtent

    when (isDecl && optComments) $ do
      traceU 1 "comment" =<< clang_Cursor_getRawCommentText cursor
      traceU 2 "brief" =<< clang_Cursor_getBriefCommentText cursor
      dumpComment 2 Nothing =<< clang_Cursor_getParsedComment cursor

    pure $ simpleEnum CXChildVisit_Recurse

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

--traceL :: Int -> String -> IO ()
--traceL level label = trace' @() level Nothing (Just label) Nothing

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

data CliOptions = CliOptions {
      cliOptions :: Options
    , cliFile    :: FilePath
    }

parseArgs :: IO CliOptions
parseArgs = OA.execParser pinfo
  where
    pinfo :: OA.ParserInfo CliOptions
    pinfo = OA.info (OA.helper <*> parseCliOptions) $ mconcat
      [ OA.fullDesc
      , OA.progDesc "Clang AST dump"
      , OA.failureCode 2
      ]

    parseCliOptions :: OA.Parser CliOptions
    parseCliOptions = do
      cliOptions <- parseOptions
      cliFile    <- fileArgument
      pure CliOptions{..}

    parseOptions :: OA.Parser Options
    parseOptions = do
      optComments <- commentsOption
      optExtents  <- extentsOption
      pure Options{..}

    commentsOption :: OA.Parser Bool
    commentsOption = OA.switch $ mconcat
      [ OA.long "comments"
      , OA.help "show comments"
      ]

    extentsOption :: OA.Parser Bool
    extentsOption = OA.switch $ mconcat
      [ OA.long "extents"
      , OA.help "show extents"
      ]

    fileArgument :: OA.Parser FilePath
    fileArgument = OA.strArgument $ mconcat
      [ OA.metavar "FILE"
      , OA.help "C (header) file to parse"
      ]

main :: IO ()
main = do
    CliOptions{..} <- parseArgs
    clangAstDump cliOptions cliFile
