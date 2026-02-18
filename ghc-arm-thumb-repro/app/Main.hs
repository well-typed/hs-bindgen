module Main where

import Foreign.C
import Lib (myStrlen)

main :: IO ()
main = do
    len <- withCString "hello" myStrlen
    putStrLn ("strlen(\"hello\") = " ++ show len)
