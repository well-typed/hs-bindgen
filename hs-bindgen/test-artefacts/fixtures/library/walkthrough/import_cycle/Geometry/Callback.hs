{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Geometry.Callback where

import qualified Geometry.Point
import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO, fmap)

-- __unique:__ @instance ToFunPtr (BG.Ptr Point -> BG.Ptr BG.Void -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_a8aa1b3c7c120357_base ::
     (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr Point -> BG.Ptr BG.Void -> IO ())@
hs_bindgen_a8aa1b3c7c120357 ::
     (BG.Ptr Geometry.Point.Point -> BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr Geometry.Point.Point -> BG.Ptr BG.Void -> IO ()))
hs_bindgen_a8aa1b3c7c120357 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_a8aa1b3c7c120357_base (\x1 ->
                                                            \x2 ->
                                                              fun0 (BG.fromFFIType x1) (BG.fromFFIType x2)))

-- __unique:__ @instance FromFunPtr (BG.Ptr Point -> BG.Ptr BG.Void -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_9cb6fa09564deb73_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr Point -> BG.Ptr BG.Void -> IO ())@
hs_bindgen_9cb6fa09564deb73 ::
     BG.FunPtr (BG.Ptr Geometry.Point.Point -> BG.Ptr BG.Void -> IO ())
  -> BG.Ptr Geometry.Point.Point -> BG.Ptr BG.Void -> IO ()
hs_bindgen_9cb6fa09564deb73 =
  \funPtr0 ->
    \x1 ->
      \x2 ->
        hs_bindgen_9cb6fa09564deb73_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)

instance BG.ToFunPtr (BG.Ptr Geometry.Point.Point -> BG.Ptr BG.Void -> IO ()) where

  toFunPtr = hs_bindgen_a8aa1b3c7c120357

instance BG.FromFunPtr (BG.Ptr Geometry.Point.Point -> BG.Ptr BG.Void -> IO ()) where

  fromFunPtr = hs_bindgen_9cb6fa09564deb73
