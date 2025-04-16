module Main where

import Control.Monad
import Foreign

import Shape
import UnionInfrastructure

import Handwritten ()

{-------------------------------------------------------------------------------
  Test it out
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    --
    -- Through 'Shape'
    --

    putStrLn "\n# peek and poke Shape"

    r <- peek =<< new_rect 1 2 3 4
    print r
    with r $ print_shape

    c <- peek =<< new_circle 5 6 7
    print c
    with c $ print_shape

    --
    -- Using 'Shape_details' directly
    --

    putStrLn "\n# poke Shape_details"

    withWritten (shape_details r) $
      print_shape_details (un_Shape_tag $ shape_tag r)
    withWritten (shape_details c) $
      print_shape_details (un_Shape_tag $ shape_tag c)

    putStrLn "\n# peek Shape_details"

    replicateM_ 5 $ print =<< getRandomShape

getRandomShape :: IO Shape
getRandomShape =
    alloca       $ \ptrTag ->
    staticAlloca $ \ptrDetails -> do
      random_shape_details ptrTag ptrDetails
      tag     <- peek ptrTag
      details <- readRawWithCtxt tag ptrDetails
      return $ Shape (Shape_tag tag) details



