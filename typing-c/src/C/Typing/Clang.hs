{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module C.Typing.Clang where

-- base
import Data.Foldable
  ( toList )
import Data.List
  ( intercalate )
import Data.Maybe
  ( listToMaybe )
import System.IO
  ( hPutStr )
import System.IO.Unsafe
  ( unsafePerformIO )

-- vec
import Data.Vec.Lazy
  ( Vec(..) )
import Data.Vec.Lazy qualified as Vec

-- hs-bindgen-libclang
import HsBindgen.Clang.HighLevel qualified as Clang
import  HsBindgen.Clang.HighLevel.Types qualified as Clang
import HsBindgen.Clang.LowLevel.Core qualified as Clang
  hiding ( clang_visitChildren, clang_getCursorSpelling )
import HsBindgen.Clang.Args qualified as Clang

-- hs-bindgen-patterns
import HsBindgen.Patterns qualified as Clang

-- text
import Data.Text
  ( Text )
import Data.Text qualified as Text
  ( unpack )

-- temporary
import System.IO.Temp qualified as Tmp

-- typing-c
import C.Type

--------------------------------------------------------------------------------

queryClangForResultType :: forall n. Vec n Type -> ( Vec n String -> String ) -> Maybe String
queryClangForResultType tys op = fmap Text.unpack $ listToMaybe $ unsafePerformIO $ callClang extractType sourceProgram
  where
    n :: Int
    n = length tys

    args, typedArgs :: Vec n String
    args = Vec.imap (\ i _ -> "x_" ++ show i) tys
    typedArgs = Vec.imap ( \i ty -> showTypeAsCType ty ( "x_" ++ show i ) ) tys

    sourceProgram :: String
    sourceProgram = unlines $ concat $
      [ [ "#include <stdio.h>"
        , ""
        ]
      , [ "typedef struct { int _unused; } " ++ s ++ ";"
        | i <- [ 1 .. n ]
        , let s = "ty_" ++ show i
        ]
      , [ ""
        , "void ignore(...) { };"
        , ""
        , "int main (" ++ intercalate ", " (toList typedArgs) ++ ") {"
        , "  ignore(" ++ op args ++ ");"
        , "  return 0;"
        , "}"
        ]
      ]

    extractType :: Clang.CXCursor -> IO ( Clang.Next IO Text )
    extractType cursor = do
      cursorKind <- Clang.fromSimpleEnum <$> Clang.clang_getCursorKind cursor
      case cursorKind of
        Right Clang.CXCursor_CallExpr -> do
          funNm <- Clang.getUserProvided <$> Clang.clang_getCursorSpelling cursor
          case funNm of
            Just u
              | u == "ignore"
              -> do
                arg <- Clang.clang_Cursor_getArgument cursor 0
                cxTy <- Clang.clang_getCursorType arg
                ty <- Clang.clang_getTypeSpelling cxTy
                return $ Clang.Break ( Just ty )
            _ -> return $ Clang.Recurse extractType listToMaybe
        _ -> return $ Clang.Recurse extractType listToMaybe


callClang :: Clang.Fold IO a -> String -> IO [a]
callClang f srcContents =
  -- TODO: we wouldn't need to create a temporary file if we supported
  -- unsaved files in 'clang_parseTranslationUnit'.
  Tmp.withSystemTempFile "src.c" $ \ fp h -> do
    hPutStr h srcContents
    index  <- Clang.clang_createIndex Clang.DisplayDiagnostics
    unit   <- Clang.clang_parseTranslationUnit index fp Clang.defaultClangArgs (Clang.bitfieldEnum [])
    rootCursor <- Clang.clang_getTranslationUnitCursor unit
    Clang.clang_visitChildren rootCursor f
