module Main where

import Foreign

import Handwritten ()
import Shape

{-------------------------------------------------------------------------------
  Test it out
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    r <- new_rect 1 2 3 4
    print =<< peek r

    c <- new_circle 5 6 7
    print =<< peek c
