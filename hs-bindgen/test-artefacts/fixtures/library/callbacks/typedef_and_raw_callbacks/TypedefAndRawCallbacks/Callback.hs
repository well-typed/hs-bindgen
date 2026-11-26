{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TypedefAndRawCallbacks.Callback where

import qualified HsBindgen.Runtime.Support as BG
import qualified TypedefAndRawCallbacks.Raw_callbacks
import Prelude (IO, fmap)

-- __unique:__ @instance ToFunPtr (BG.Ptr Loop -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_c0bf398a6ee3b9a1_base ::
     (BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr Loop -> IO ())@
hs_bindgen_c0bf398a6ee3b9a1 ::
     (BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop -> IO ())
  -> IO (BG.FunPtr (BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop -> IO ()))
hs_bindgen_c0bf398a6ee3b9a1 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_c0bf398a6ee3b9a1_base (\x1 ->
                                                            fun0 (BG.fromFFIType x1)))

-- __unique:__ @instance FromFunPtr (BG.Ptr Loop -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_69d1f17b045990e2_base ::
     BG.FunPtr (BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr Loop -> IO ())@
hs_bindgen_69d1f17b045990e2 ::
     BG.FunPtr (BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop -> IO ())
  -> BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop -> IO ()
hs_bindgen_69d1f17b045990e2 =
  \funPtr0 ->
    \x1 ->
      hs_bindgen_69d1f17b045990e2_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)

instance BG.ToFunPtr (BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop -> IO ()) where

  toFunPtr = hs_bindgen_c0bf398a6ee3b9a1

instance BG.FromFunPtr (BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop -> IO ()) where

  fromFunPtr = hs_bindgen_69d1f17b045990e2

-- __unique:__ @instance ToFunPtr (BG.Ptr Loop -> BG.CInt -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_bf1760a1573d34bd_base ::
     (BG.Ptr BG.Void -> BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.CInt -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr Loop -> BG.CInt -> IO ())@
hs_bindgen_bf1760a1573d34bd ::
     (BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop -> BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop -> BG.CInt -> IO ()))
hs_bindgen_bf1760a1573d34bd =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_bf1760a1573d34bd_base (\x1 ->
                                                            \x2 ->
                                                              fun0 (BG.fromFFIType x1) (BG.fromFFIType x2)))

-- __unique:__ @instance FromFunPtr (BG.Ptr Loop -> BG.CInt -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_bdf60e09344311dc_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.CInt -> IO ())
  -> BG.Ptr BG.Void -> BG.CInt -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr Loop -> BG.CInt -> IO ())@
hs_bindgen_bdf60e09344311dc ::
     BG.FunPtr (BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop -> BG.CInt -> IO ())
  -> BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop -> BG.CInt -> IO ()
hs_bindgen_bdf60e09344311dc =
  \funPtr0 ->
    \x1 ->
      \x2 ->
        hs_bindgen_bdf60e09344311dc_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)

instance BG.ToFunPtr (BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop -> BG.CInt -> IO ()) where

  toFunPtr = hs_bindgen_bf1760a1573d34bd

instance BG.FromFunPtr (BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop -> BG.CInt -> IO ()) where

  fromFunPtr = hs_bindgen_bdf60e09344311dc
