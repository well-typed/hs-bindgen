{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module TypedefAndRawCallbacks.Typedef_callback.Callback where

import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO, fmap)
import TypedefAndRawCallbacks.Typedef_callback

-- __unique:__ @toTypedef_callback_Aux@
foreign import ccall safe "wrapper" hs_bindgen_9e597cf3bc0f375d_base ::
     (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ()))

-- __unique:__ @toTypedef_callback_Aux@
hs_bindgen_9e597cf3bc0f375d ::
     Typedef_callback_Aux
  -> IO (BG.FunPtr Typedef_callback_Aux)
hs_bindgen_9e597cf3bc0f375d =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_9e597cf3bc0f375d_base (\x1 ->
                                                            \x2 ->
                                                              BG.getField @"unwrapTypedef_callback_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2)))

-- __unique:__ @fromTypedef_callback_Aux@
foreign import ccall safe "dynamic" hs_bindgen_f78eba5a3cd8d3f1_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @fromTypedef_callback_Aux@
hs_bindgen_f78eba5a3cd8d3f1 ::
     BG.FunPtr Typedef_callback_Aux
  -> Typedef_callback_Aux
hs_bindgen_f78eba5a3cd8d3f1 =
  \funPtr0 ->
    Typedef_callback_Aux (\x1 ->
                            \x2 ->
                              hs_bindgen_f78eba5a3cd8d3f1_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2))

instance BG.ToFunPtr Typedef_callback_Aux where

  toFunPtr = hs_bindgen_9e597cf3bc0f375d

instance BG.FromFunPtr Typedef_callback_Aux where

  fromFunPtr = hs_bindgen_f78eba5a3cd8d3f1
