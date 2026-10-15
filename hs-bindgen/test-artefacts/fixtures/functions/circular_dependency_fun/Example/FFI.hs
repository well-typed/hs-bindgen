{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Example
import Prelude (IO, fmap)

-- __unique:__ @toFun_ptr_Aux@
foreign import ccall safe "wrapper" hs_bindgen_5964bbadb359ee4a_base ::
     (BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> IO ()))

-- __unique:__ @toFun_ptr_Aux@
hs_bindgen_5964bbadb359ee4a ::
     Fun_ptr_Aux
  -> IO (BG.FunPtr Fun_ptr_Aux)
hs_bindgen_5964bbadb359ee4a =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_5964bbadb359ee4a_base (\x1 ->
                                                            BG.getField @"unwrapFun_ptr_Aux" fun0 (BG.fromFFIType x1)))

-- __unique:__ @fromFun_ptr_Aux@
foreign import ccall safe "dynamic" hs_bindgen_f8391e85af67fcb6_base ::
     BG.FunPtr (BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @fromFun_ptr_Aux@
hs_bindgen_f8391e85af67fcb6 ::
     BG.FunPtr Fun_ptr_Aux
  -> Fun_ptr_Aux
hs_bindgen_f8391e85af67fcb6 =
  \funPtr0 ->
    Fun_ptr_Aux (\x1 ->
                   hs_bindgen_f8391e85af67fcb6_base (BG.castFunPtr funPtr0) (BG.toFFIType x1))
