{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Example
import Prelude (IO, fmap)

-- __unique:__ @toMyFunctionPointer_Aux@
foreign import ccall safe "wrapper" hs_bindgen_47dfd04698dd2e6f_base ::
     (BG.CInt -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.CInt -> IO BG.CInt))

-- __unique:__ @toMyFunctionPointer_Aux@
hs_bindgen_47dfd04698dd2e6f ::
     MyFunctionPointer_Aux
  -> IO (BG.FunPtr MyFunctionPointer_Aux)
hs_bindgen_47dfd04698dd2e6f =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_47dfd04698dd2e6f_base (\x1 ->
                                                            fmap BG.toFFIType (BG.getField @"unwrapMyFunctionPointer_Aux" fun0 (BG.fromFFIType x1))))

-- __unique:__ @fromMyFunctionPointer_Aux@
foreign import ccall safe "dynamic" hs_bindgen_5738272f94a589e2_base ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> BG.CInt -> IO BG.CInt

-- __unique:__ @fromMyFunctionPointer_Aux@
hs_bindgen_5738272f94a589e2 ::
     BG.FunPtr MyFunctionPointer_Aux
  -> MyFunctionPointer_Aux
hs_bindgen_5738272f94a589e2 =
  \funPtr0 ->
    MyFunctionPointer_Aux (\x1 ->
                             fmap BG.fromFFIType (hs_bindgen_5738272f94a589e2_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)))
