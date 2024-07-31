{-# LANGUAGE CApiFFI #-}

-- | Bindings generated using @hsc2hs@
module HsBindgenCExample where

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
  FFI
-------------------------------------------------------------------------------}

foreign import capi "hs-bindgen-c-example.h hs_bindgen_c_example_showStruct"
  cShowStruct :: Ptr HaskellStruct -> IO ()

instance Storable HaskellStruct where
  sizeOf    _ = #size      struct ExampleStruct
  alignment _ = #alignment struct ExampleStruct

  peek s = do
      haskellStructA <- (#peek struct ExampleStruct, a) s
      haskellStructB <- (#peek struct ExampleStruct, b) s
      return HaskellStruct{haskellStructA, haskellStructB}

  poke s HaskellStruct{haskellStructA, haskellStructB} = do
      (#poke struct ExampleStruct, a) s haskellStructA
      (#poke struct ExampleStruct, b) s haskellStructB
