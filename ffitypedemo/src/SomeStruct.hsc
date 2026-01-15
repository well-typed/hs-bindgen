module SomeStruct where

import Foreign
import C

#include "exporttest.h"

data SomeStruct = SomeStruct {
      i :: C
    , j :: C
    }

instance Storable SomeStruct where
  sizeOf    _ = #size      struct SomeStruct
  alignment _ = #alignment struct SomeStruct

  peek p = do
      i <- (#peek struct SomeStruct, i) p
      j <- (#peek struct SomeStruct, j) p
      return SomeStruct{i, j}

  poke p SomeStruct{i, j} = do
      (#poke struct SomeStruct, i) p i
      (#poke struct SomeStruct, j) p j

foreign import capi "exporttest.h bar"
  bar :: Ptr SomeStruct -> IO ()
