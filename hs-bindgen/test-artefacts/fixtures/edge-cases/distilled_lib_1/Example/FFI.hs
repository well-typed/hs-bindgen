{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Example.FFI where

import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Support as BG
import Example
import Prelude (IO, fmap)

-- __unique:__ @toCallback_t_Aux@
foreign import ccall safe "wrapper" hs_bindgen_b6b6922e35047658_base ::
     (BG.Ptr BG.Void -> HsBindgen.Runtime.LibC.Word32 -> IO HsBindgen.Runtime.LibC.Word32)
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> HsBindgen.Runtime.LibC.Word32 -> IO HsBindgen.Runtime.LibC.Word32))

-- __unique:__ @toCallback_t_Aux@
hs_bindgen_b6b6922e35047658 ::
     Callback_t_Aux
  -> IO (BG.FunPtr Callback_t_Aux)
hs_bindgen_b6b6922e35047658 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_b6b6922e35047658_base (\x1 ->
                                                            \x2 ->
                                                              fmap BG.toFFIType (BG.getField @"unwrapCallback_t_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2))))

-- __unique:__ @fromCallback_t_Aux@
foreign import ccall safe "dynamic" hs_bindgen_d6debb4b8d5bb869_base ::
     BG.FunPtr (BG.Ptr BG.Void -> HsBindgen.Runtime.LibC.Word32 -> IO HsBindgen.Runtime.LibC.Word32)
  -> BG.Ptr BG.Void -> HsBindgen.Runtime.LibC.Word32 -> IO HsBindgen.Runtime.LibC.Word32

-- __unique:__ @fromCallback_t_Aux@
hs_bindgen_d6debb4b8d5bb869 ::
     BG.FunPtr Callback_t_Aux
  -> Callback_t_Aux
hs_bindgen_d6debb4b8d5bb869 =
  \funPtr0 ->
    Callback_t_Aux (\x1 ->
                      \x2 ->
                        fmap BG.fromFFIType (hs_bindgen_d6debb4b8d5bb869_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)))
