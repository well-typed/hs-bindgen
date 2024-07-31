module Main where

import HsBindgenExample

main :: IO ()
main = do
    cHelloWorld
    cShowStruct $ HaskellStruct 1234 5678
