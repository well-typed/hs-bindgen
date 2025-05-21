module Point where

import Foreign
import Foreign.C

#include "demo.h"

data Point = Point {
      x :: CInt
    , y :: CInt
    }
  deriving stock (Show)

instance Storable Point where
  sizeOf    _ = (#size      struct point)
  alignment _ = (#alignment struct point)

  peek p = do
      x <- (#peek struct point, x) p
      y <- (#peek struct point, y) p
      return Point{x, y}

  poke p Point{x, y} = do
      (#poke struct point, x) p x
      (#poke struct point, y) p y
