-- * Haskell translation of the @libclang@ tutorial
--
-- See <https://clang.llvm.org/docs/LibClang.html>
module Main (main) where

import Control.Monad
import System.Environment

import HsBindgen.C.Clang
import HsBindgen.C.Clang.Util
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Reproduce the clang tutorial in Haskell
-------------------------------------------------------------------------------}

tutorial :: FilePath -> IO ()
tutorial fp = do
    --
    -- Obtain a cursor at the root of the translation unit
    --

    index  <- clang_createIndex DontDisplayDiagnostics
    unit   <- clang_parseTranslationUnit
                index
                fp
                []
                (bitfieldEnum [CXTranslationUnit_None])
    cursor <- clang_getTranslationUnitCursor unit

    --
    -- Visiting elements of an AST
    --

    _terminatedPrematurely <- clang_visitChildren
      cursor -- Root cursor
      (\current_cursor _parent -> do

        current_display_name <- clang_getCursorDisplayName current_cursor
        putStrLn $ "Visiting element " ++ show current_display_name

        --
        -- Extracting information from a Cursor
        --

        -- Extracting the Cursor kind

        cursor_type        <- clang_getCursorType current_cursor
        type_kind_spelling <- clang_getTypeKindSpelling (cxtKind cursor_type)
        putStrLn $ concat [
            "  "
          , "Type Kind: " ++ show type_kind_spelling
          , " (" ++ show (fromSimpleEnum $ cxtKind cursor_type) ++ ")"
          ]

        when (isPointerType $ cxtKind cursor_type) $ do
          pointed_to_type          <- clang_getPointeeType(cursor_type)
          pointed_to_type_spelling <- clang_getTypeSpelling pointed_to_type
          putStrLn $ "  pointing to type: " ++ show pointed_to_type_spelling

        when (isRecordType $ cxtKind cursor_type) $ do
          type_spelling <- clang_getTypeSpelling cursor_type
          putStrLn $ "  namely " ++ show type_spelling

        --
        -- Retrieving source locations
        --

        cursor_spelling <- clang_getCursorSpelling current_cursor
        cursor_range    <- clang_getCursorExtent current_cursor
        range_start     <- clang_getRangeStart cursor_range
        range_end       <- clang_getRangeEnd   cursor_range
        (_, start_line, start_column, _) <-
          clang_getExpansionLocation range_start
        (_, end_line, end_column, _) <-
          clang_getExpansionLocation range_end
        putStrLn $ concat [
            "  Cursor " ++ show cursor_spelling
          , " spanning lines "
          , show start_line ++ ":" ++ show start_column
          , " to "
          , show end_line ++ ":" ++ show end_column
          ]

        return $ simpleEnum CXChildVisit_Recurse
      )

    return ()

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    [fp] <- getArgs
    tutorial fp