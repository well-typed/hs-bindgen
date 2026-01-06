module Main where

import SimpleBindings

myPoint :: Point
myPoint = Point 12 60

myPoint2 :: Point
myPoint2 = Point 60 12

main :: IO ()
main = add_points myPoint myPoint2 >>= print
