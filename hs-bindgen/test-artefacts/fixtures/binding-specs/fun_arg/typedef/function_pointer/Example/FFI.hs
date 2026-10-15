{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Example
import Prelude (IO, fmap)

-- __unique:__ @toA_Aux@
foreign import ccall safe "wrapper" hs_bindgen_1cabb32c661d9a0e_base ::
     (BG.CInt -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.CInt -> IO BG.CInt))

-- __unique:__ @toA_Aux@
hs_bindgen_1cabb32c661d9a0e ::
     A_Aux
  -> IO (BG.FunPtr A_Aux)
hs_bindgen_1cabb32c661d9a0e =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_1cabb32c661d9a0e_base (\x1 ->
                                                            fmap BG.toFFIType (BG.getField @"unwrapA_Aux" fun0 (BG.fromFFIType x1))))

-- __unique:__ @fromA_Aux@
foreign import ccall safe "dynamic" hs_bindgen_cdb12400c6863f15_base ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> BG.CInt -> IO BG.CInt

-- __unique:__ @fromA_Aux@
hs_bindgen_cdb12400c6863f15 ::
     BG.FunPtr A_Aux
  -> A_Aux
hs_bindgen_cdb12400c6863f15 =
  \funPtr0 ->
    A_Aux (\x1 ->
             fmap BG.fromFFIType (hs_bindgen_cdb12400c6863f15_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)))
