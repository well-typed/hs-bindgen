module Main (main) where

import SomeOtherModule
import SomeStruct
import Foreign

main :: IO ()
main = do
    foo 1234
    with (SomeStruct 12 34) bar
