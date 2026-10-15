{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Example
import Prelude (IO, fmap)

-- __unique:__ @toT_Aux@
foreign import ccall safe "wrapper" hs_bindgen_5927fedc3abfa99c_base ::
     (BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> IO ()))

-- __unique:__ @toT_Aux@
hs_bindgen_5927fedc3abfa99c ::
     T_Aux
  -> IO (BG.FunPtr T_Aux)
hs_bindgen_5927fedc3abfa99c =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_5927fedc3abfa99c_base (\x1 ->
                                                            BG.getField @"unwrapT_Aux" fun0 (BG.fromFFIType x1)))

-- __unique:__ @fromT_Aux@
foreign import ccall safe "dynamic" hs_bindgen_281617c90fa9307a_base ::
     BG.FunPtr (BG.CInt -> IO ())
  -> BG.CInt -> IO ()

-- __unique:__ @fromT_Aux@
hs_bindgen_281617c90fa9307a ::
     BG.FunPtr T_Aux
  -> T_Aux
hs_bindgen_281617c90fa9307a =
  \funPtr0 ->
    T_Aux (\x1 ->
             hs_bindgen_281617c90fa9307a_base (BG.castFunPtr funPtr0) (BG.toFFIType x1))
