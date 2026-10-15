{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Example
import Prelude (IO, fmap)

-- __unique:__ @toRunDriver_Aux@
foreign import ccall safe "wrapper" hs_bindgen_d86ecf261d7044c6_base ::
     (BG.Ptr BG.Void -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> IO BG.CInt))

-- __unique:__ @toRunDriver_Aux@
hs_bindgen_d86ecf261d7044c6 ::
     RunDriver_Aux
  -> IO (BG.FunPtr RunDriver_Aux)
hs_bindgen_d86ecf261d7044c6 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_d86ecf261d7044c6_base (\x1 ->
                                                            fmap BG.toFFIType (BG.getField @"unwrapRunDriver_Aux" fun0 (BG.fromFFIType x1))))

-- __unique:__ @fromRunDriver_Aux@
foreign import ccall safe "dynamic" hs_bindgen_6520ae39b50ffb4e_base ::
     BG.FunPtr (BG.Ptr BG.Void -> IO BG.CInt)
  -> BG.Ptr BG.Void -> IO BG.CInt

-- __unique:__ @fromRunDriver_Aux@
hs_bindgen_6520ae39b50ffb4e ::
     BG.FunPtr RunDriver_Aux
  -> RunDriver_Aux
hs_bindgen_6520ae39b50ffb4e =
  \funPtr0 ->
    RunDriver_Aux (\x1 ->
                     fmap BG.fromFFIType (hs_bindgen_6520ae39b50ffb4e_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)))
