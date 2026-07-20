-- | Parse @fixture.h@ and walk its AST (cursor kind, spelling, source line:col)
-- through the ClangFFI.Wrappers combinator layer.
--
-- The twin of app/WalkLow.hs; the two must print byte-identical output.
module Main (main) where

import Control.Monad (forM_)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment (getArgs)

import Clang.Internal.ByValue (OnHaskellHeap)
import Clang.LowLevel.Core.Structs (CXCursor_)

import ClangFFI.Wrappers

main :: IO ()
main = do
    args <- getArgs
    let path = case args of
          (p : _) -> p
          []      -> "fixture.h"
    idx <- createIndex 0 0
    tu <- parseTU idx path ["-x", "c"]
    root <- tuCursor tu
    walk 0 root
    disposeTranslationUnit tu
    disposeIndex idx

walk :: Int -> OnHaskellHeap CXCursor_ -> IO ()
walk depth cur = do
    kids <- childrenOf cur
    forM_ kids $ \child -> do
      kind <- kindSpelling =<< cursorKind child
      name <- cursorSpelling child
      (line, col) <- spellingLineCol =<< cursorLocation child
      T.putStrLn $
        T.concat
          [ T.replicate depth "  "
          , kind
          , " \""
          , name
          , "\" "
          , T.pack (show line)
          , ":"
          , T.pack (show col)
          ]
      walk (depth + 1) child
