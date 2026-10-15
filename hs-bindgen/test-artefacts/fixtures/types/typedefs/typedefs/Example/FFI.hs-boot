{-# LANGUAGE NoImplicitPrelude #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO)
import {-# SOURCE #-} Example

hs_bindgen_a6c7dd49f5b9d470 ::
     Int2int
  -> IO (BG.FunPtr Int2int)

hs_bindgen_65378a8a3cf640ad ::
     BG.FunPtr Int2int
  -> Int2int

hs_bindgen_b171c028cdc0781d ::
     FunctionPointer_Function_Aux
  -> IO (BG.FunPtr FunctionPointer_Function_Aux)

hs_bindgen_4c3da8240a31e036 ::
     BG.FunPtr FunctionPointer_Function_Aux
  -> FunctionPointer_Function_Aux

hs_bindgen_766ae751d60365e9 ::
     NonFunctionPointer_Function
  -> IO (BG.FunPtr NonFunctionPointer_Function)

hs_bindgen_36c7108d046bcbc3 ::
     BG.FunPtr NonFunctionPointer_Function
  -> NonFunctionPointer_Function

hs_bindgen_00d16e666202ed6c ::
     F1_Aux
  -> IO (BG.FunPtr F1_Aux)

hs_bindgen_ddeb5206e8192425 ::
     BG.FunPtr F1_Aux
  -> F1_Aux

hs_bindgen_fa5806570b682579 ::
     G1
  -> IO (BG.FunPtr G1)

hs_bindgen_8405c8e75aa78be5 ::
     BG.FunPtr G1
  -> G1

hs_bindgen_ffae0d1234ed018f ::
     H1
  -> IO (BG.FunPtr H1)

hs_bindgen_1a33688324e1924f ::
     BG.FunPtr H1
  -> H1
