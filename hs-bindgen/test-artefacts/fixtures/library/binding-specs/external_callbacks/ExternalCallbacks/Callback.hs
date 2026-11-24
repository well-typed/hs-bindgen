{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ExternalCallbacks.Callback where

import qualified ExternalCallbacks.Handlers
import qualified GeneratedSpecs.Same_header
import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO, fmap)

-- __unique:__ @instance ToFunPtr (BG.Ptr Handlers -> BG.Ptr GeneratedSpecs.Same_header.Same_header -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_829920f98b689f17_base ::
     (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr Handlers -> BG.Ptr GeneratedSpecs.Same_header.Same_header -> IO ())@
hs_bindgen_829920f98b689f17 ::
     (BG.Ptr ExternalCallbacks.Handlers.Handlers -> BG.Ptr GeneratedSpecs.Same_header.Same_header -> IO ())
  -> IO (BG.FunPtr (BG.Ptr ExternalCallbacks.Handlers.Handlers -> BG.Ptr GeneratedSpecs.Same_header.Same_header -> IO ()))
hs_bindgen_829920f98b689f17 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_829920f98b689f17_base (\x1 ->
                                                            \x2 ->
                                                              fun0 (BG.fromFFIType x1) (BG.fromFFIType x2)))

-- __unique:__ @instance FromFunPtr (BG.Ptr Handlers -> BG.Ptr GeneratedSpecs.Same_header.Same_header -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_ebbfc6fbf07dbf12_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr Handlers -> BG.Ptr GeneratedSpecs.Same_header.Same_header -> IO ())@
hs_bindgen_ebbfc6fbf07dbf12 ::
     BG.FunPtr (BG.Ptr ExternalCallbacks.Handlers.Handlers -> BG.Ptr GeneratedSpecs.Same_header.Same_header -> IO ())
  -> BG.Ptr ExternalCallbacks.Handlers.Handlers -> BG.Ptr GeneratedSpecs.Same_header.Same_header -> IO ()
hs_bindgen_ebbfc6fbf07dbf12 =
  \funPtr0 ->
    \x1 ->
      \x2 ->
        hs_bindgen_ebbfc6fbf07dbf12_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)

instance BG.ToFunPtr (BG.Ptr ExternalCallbacks.Handlers.Handlers -> BG.Ptr GeneratedSpecs.Same_header.Same_header -> IO ()) where

  toFunPtr = hs_bindgen_829920f98b689f17

instance BG.FromFunPtr (BG.Ptr ExternalCallbacks.Handlers.Handlers -> BG.Ptr GeneratedSpecs.Same_header.Same_header -> IO ()) where

  fromFunPtr = hs_bindgen_ebbfc6fbf07dbf12
