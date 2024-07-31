-- | Bindings written with the help of c2hs
module HsBindgenExample where

import Foreign

#include "hs-bindgen-c-example.h"

{-------------------------------------------------------------------------------
  Haskell definitions (no FFI)
-------------------------------------------------------------------------------}

data HaskellStruct = HaskellStruct {
      haskellStructA :: Int
    , haskellStructB :: Int
    }

{-------------------------------------------------------------------------------
  Patterns
-------------------------------------------------------------------------------}

withAlloca :: Storable a => a -> (Ptr () -> IO r) -> IO r
withAlloca s k =
    alloca $ \ptr -> do
      poke ptr s
      k (castPtr ptr)

{-------------------------------------------------------------------------------
  FFI
-------------------------------------------------------------------------------}

{# fun hs_bindgen_c_example_helloworld as cHelloWorld
     {} -> `()'
#}

{# fun hs_bindgen_c_example_showStruct as cShowStruct
     {withAlloca* `HaskellStruct'} -> `()'
#}

instance Storable HaskellStruct where
  sizeOf    _ = {#sizeof  ExampleStruct#}
  alignment _ = {#alignof ExampleStruct#}

  peek s = do
      haskellStructA <- fromIntegral <$> {#get ExampleStruct.a#} s
      haskellStructB <- fromIntegral <$> {#get ExampleStruct.b#} s
      return HaskellStruct{haskellStructA, haskellStructB}

  poke s HaskellStruct{haskellStructA, haskellStructB} = do
      {#set ExampleStruct.a#} s $ fromIntegral haskellStructA
      {#set ExampleStruct.b#} s $ fromIntegral haskellStructB
