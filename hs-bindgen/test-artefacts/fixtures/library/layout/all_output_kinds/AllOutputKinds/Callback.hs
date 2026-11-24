{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AllOutputKinds.Callback where

import qualified AllOutputKinds.Cycle_b
import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO, fmap)

-- __unique:__ @instance ToFunPtr (BG.Ptr Cycle_b -> BG.Ptr Cycle_b -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_c76a7772ad3f237b_base ::
     (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr Cycle_b -> BG.Ptr Cycle_b -> IO ())@
hs_bindgen_c76a7772ad3f237b ::
     (BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> IO ())
  -> IO (BG.FunPtr (BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> IO ()))
hs_bindgen_c76a7772ad3f237b =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_c76a7772ad3f237b_base (\x1 ->
                                                            \x2 ->
                                                              fun0 (BG.fromFFIType x1) (BG.fromFFIType x2)))

-- __unique:__ @instance FromFunPtr (BG.Ptr Cycle_b -> BG.Ptr Cycle_b -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_7b911fd224a3b37a_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr Cycle_b -> BG.Ptr Cycle_b -> IO ())@
hs_bindgen_7b911fd224a3b37a ::
     BG.FunPtr (BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> IO ())
  -> BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> IO ()
hs_bindgen_7b911fd224a3b37a =
  \funPtr0 ->
    \x1 ->
      \x2 ->
        hs_bindgen_7b911fd224a3b37a_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)

instance BG.ToFunPtr (BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> IO ()) where

  toFunPtr = hs_bindgen_c76a7772ad3f237b

instance BG.FromFunPtr (BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> IO ()) where

  fromFunPtr = hs_bindgen_7b911fd224a3b37a
