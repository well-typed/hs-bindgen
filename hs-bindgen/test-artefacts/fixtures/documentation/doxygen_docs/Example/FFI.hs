{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Example
import Prelude (IO, fmap)

-- __unique:__ @toEvent_callback_t_Aux@
foreign import ccall safe "wrapper" hs_bindgen_111918b0aee2a7fb_base ::
     (BG.CInt -> BG.Ptr BG.Void -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.CInt -> BG.Ptr BG.Void -> IO BG.CInt))

-- __unique:__ @toEvent_callback_t_Aux@
hs_bindgen_111918b0aee2a7fb ::
     Event_callback_t_Aux
  -> IO (BG.FunPtr Event_callback_t_Aux)
hs_bindgen_111918b0aee2a7fb =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_111918b0aee2a7fb_base (\x1 ->
                                                            \x2 ->
                                                              fmap BG.toFFIType (BG.getField @"unwrapEvent_callback_t_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2))))

-- __unique:__ @fromEvent_callback_t_Aux@
foreign import ccall safe "dynamic" hs_bindgen_9e9d478c2d75628c_base ::
     BG.FunPtr (BG.CInt -> BG.Ptr BG.Void -> IO BG.CInt)
  -> BG.CInt -> BG.Ptr BG.Void -> IO BG.CInt

-- __unique:__ @fromEvent_callback_t_Aux@
hs_bindgen_9e9d478c2d75628c ::
     BG.FunPtr Event_callback_t_Aux
  -> Event_callback_t_Aux
hs_bindgen_9e9d478c2d75628c =
  \funPtr0 ->
    Event_callback_t_Aux (\x1 ->
                            \x2 ->
                              fmap BG.fromFFIType (hs_bindgen_9e9d478c2d75628c_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)))

-- __unique:__ @toProcessor_fn_t_Aux@
foreign import ccall safe "wrapper" hs_bindgen_d4e16471c82d5df0_base ::
     (BG.CInt -> BG.Ptr BG.Void -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.CInt -> BG.Ptr BG.Void -> IO BG.CInt))

-- __unique:__ @toProcessor_fn_t_Aux@
hs_bindgen_d4e16471c82d5df0 ::
     Processor_fn_t_Aux
  -> IO (BG.FunPtr Processor_fn_t_Aux)
hs_bindgen_d4e16471c82d5df0 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_d4e16471c82d5df0_base (\x1 ->
                                                            \x2 ->
                                                              fmap BG.toFFIType (BG.getField @"unwrapProcessor_fn_t_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2))))

-- __unique:__ @fromProcessor_fn_t_Aux@
foreign import ccall safe "dynamic" hs_bindgen_0d4b3d0461629423_base ::
     BG.FunPtr (BG.CInt -> BG.Ptr BG.Void -> IO BG.CInt)
  -> BG.CInt -> BG.Ptr BG.Void -> IO BG.CInt

-- __unique:__ @fromProcessor_fn_t_Aux@
hs_bindgen_0d4b3d0461629423 ::
     BG.FunPtr Processor_fn_t_Aux
  -> Processor_fn_t_Aux
hs_bindgen_0d4b3d0461629423 =
  \funPtr0 ->
    Processor_fn_t_Aux (\x1 ->
                          \x2 ->
                            fmap BG.fromFFIType (hs_bindgen_0d4b3d0461629423_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)))
