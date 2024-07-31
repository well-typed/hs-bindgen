module Main where

import Foreign

import HsBindgenCExample

main :: IO ()
main = do
    alloca $ \ptr -> do
      poke ptr $ HaskellStruct 1234 5678
      cShowStruct ptr
