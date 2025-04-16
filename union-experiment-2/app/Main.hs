module Main where

import Foreign

import Handwritten ()
import Shape

{-------------------------------------------------------------------------------
  Test it out
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    --
    -- Through 'Shape'
    --

    r <- peek =<< new_rect 1 2 3 4
    print r
    with r $ print_shape

    c <- peek =<< new_circle 5 6 7
    print c
    with c $ print_shape

    --
    -- Using 'Shape_details' directly
    --

--    with (shape_details r) $ _


