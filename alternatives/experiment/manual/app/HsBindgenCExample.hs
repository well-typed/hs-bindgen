{-# LANGUAGE CApiFFI #-}

-- | Handwritten bindings, using CAPI calling convention
--
-- <https://downloads.haskell.org/ghc/9.2.8/docs/html/users_guide/exts/ffi.html#the-capi-calling-convention>
module HsBindgenCExample where

import Foreign

{-------------------------------------------------------------------------------
  Haskell definitions (no FFI)
-------------------------------------------------------------------------------}

data HaskellStruct = HaskellStruct {
      haskellStructA :: Int
    , haskellStructB :: Int
    }

{-------------------------------------------------------------------------------
  Valid declarations
-------------------------------------------------------------------------------}

foreign import capi "hs-bindgen-c-example.h hs_bindgen_c_example_helloworld"
  cHelloWorld :: IO ()

foreign import capi "hs-bindgen-c-example.h hs_bindgen_c_example_showInt"
  cShowInt :: Int -> IO ()

foreign import capi "hs-bindgen-c-example.h hs_bindgen_c_example_showStruct"
  cShowStruct :: Ptr HaskellStruct -> IO ()

instance Storable HaskellStruct where
  sizeOf    _ = 8
  alignment _ = 4

  peek s = do
      haskellStructA <- peekByteOff s 0
      haskellStructB <- peekByteOff s 4
      return HaskellStruct{haskellStructA, haskellStructB}

  poke s HaskellStruct{haskellStructA, haskellStructB} = do
      pokeByteOff s 0 haskellStructA
      pokeByteOff s 4 haskellStructB

{-------------------------------------------------------------------------------
  Invalid declarations

  The CAPI works by generating a small C wrapper that looks something like this:

  > #include "hs-bindgen-c-example.h"
  > void ghc<uniq-id>showInt(void)
  > {
  >     hs_bindgen_c_example_showInt();
  > }

  Any errors we get therefore come from the C compiler. To actually see the
  generated wrapper, run

  > cabal run try-manual --ghc-option=-keep-tmp-files
-------------------------------------------------------------------------------}

-- No warnings or errors from the C compiler at all
foreign import capi "hs-bindgen-c-example.h hs_bindgen_c_example_helloworld"
  invalid_cHelloWorld_extraParam :: Int -> IO ()

-- C compiler /error/ ("too few arguments")
--foreign import capi "hs-bindgen-c-example.h hs_bindgen_c_example_showInt"
--  invalid_cShowInt_missingParam :: IO ()

-- C compiler /warning/ ("makes integer from pointer without a cast")
foreign import capi "hs-bindgen-c-example.h hs_bindgen_c_example_showInt"
  invalid_cShowInt_wrongParam :: Ptr Int -> IO ()