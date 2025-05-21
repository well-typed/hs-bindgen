module Main where

import Foreign

import Point

foreign import capi unsafe "demo.h flip_point_wrapper"
  flip_point_wrapper :: Ptr Point -> Ptr Point -> IO ()

flip_point :: Point -> IO Point
flip_point arg =
    with arg $ \arg_ptr ->
    alloca   $ \res_ptr -> do
      flip_point_wrapper arg_ptr res_ptr
      peek res_ptr

main :: IO ()
main = print =<< flip_point Point{x = 1, y = 2}
