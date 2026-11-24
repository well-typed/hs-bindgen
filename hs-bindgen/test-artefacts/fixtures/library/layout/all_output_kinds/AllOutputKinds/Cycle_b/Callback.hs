{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module AllOutputKinds.Cycle_b.Callback where

import qualified HsBindgen.Runtime.Support as BG
import AllOutputKinds.Cycle_b
import Prelude (IO, fmap)

-- __unique:__ @toCycle_b_callback_Aux@
foreign import ccall safe "wrapper" hs_bindgen_de4eb1352549bb6f_base ::
     (BG.Ptr BG.Void -> BG.CFloat -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.CFloat -> IO ()))

-- __unique:__ @toCycle_b_callback_Aux@
hs_bindgen_de4eb1352549bb6f ::
     Cycle_b_callback_Aux
  -> IO (BG.FunPtr Cycle_b_callback_Aux)
hs_bindgen_de4eb1352549bb6f =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_de4eb1352549bb6f_base (\x1 ->
                                                            \x2 ->
                                                              BG.getField @"unwrapCycle_b_callback_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2)))

-- __unique:__ @fromCycle_b_callback_Aux@
foreign import ccall safe "dynamic" hs_bindgen_94e56c76f3a3c6d1_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.CFloat -> IO ())
  -> BG.Ptr BG.Void -> BG.CFloat -> IO ()

-- __unique:__ @fromCycle_b_callback_Aux@
hs_bindgen_94e56c76f3a3c6d1 ::
     BG.FunPtr Cycle_b_callback_Aux
  -> Cycle_b_callback_Aux
hs_bindgen_94e56c76f3a3c6d1 =
  \funPtr0 ->
    Cycle_b_callback_Aux (\x1 ->
                            \x2 ->
                              hs_bindgen_94e56c76f3a3c6d1_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2))

instance BG.ToFunPtr Cycle_b_callback_Aux where

  toFunPtr = hs_bindgen_de4eb1352549bb6f

instance BG.FromFunPtr Cycle_b_callback_Aux where

  fromFunPtr = hs_bindgen_94e56c76f3a3c6d1
