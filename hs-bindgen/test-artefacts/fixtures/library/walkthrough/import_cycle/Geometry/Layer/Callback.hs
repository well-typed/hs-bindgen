{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Geometry.Layer.Callback where

import qualified HsBindgen.Runtime.Support as BG
import Geometry.Layer
import Prelude (IO, fmap)

-- __unique:__ @toLayer_paint_Aux@
foreign import ccall safe "wrapper" hs_bindgen_eb567b1411a08567_base ::
     (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ()))

-- __unique:__ @toLayer_paint_Aux@
hs_bindgen_eb567b1411a08567 ::
     Layer_paint_Aux
  -> IO (BG.FunPtr Layer_paint_Aux)
hs_bindgen_eb567b1411a08567 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_eb567b1411a08567_base (\x1 ->
                                                            \x2 ->
                                                              BG.getField @"unwrapLayer_paint_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2)))

-- __unique:__ @fromLayer_paint_Aux@
foreign import ccall safe "dynamic" hs_bindgen_b7034f4693363b6d_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @fromLayer_paint_Aux@
hs_bindgen_b7034f4693363b6d ::
     BG.FunPtr Layer_paint_Aux
  -> Layer_paint_Aux
hs_bindgen_b7034f4693363b6d =
  \funPtr0 ->
    Layer_paint_Aux (\x1 ->
                       \x2 ->
                         hs_bindgen_b7034f4693363b6d_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2))

instance BG.ToFunPtr Layer_paint_Aux where

  toFunPtr = hs_bindgen_eb567b1411a08567

instance BG.FromFunPtr Layer_paint_Aux where

  fromFunPtr = hs_bindgen_b7034f4693363b6d
