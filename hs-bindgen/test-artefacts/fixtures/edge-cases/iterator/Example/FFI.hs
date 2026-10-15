{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Example
import Prelude (IO, fmap)

-- __unique:__ @toToggle_Aux@
foreign import ccall safe "wrapper" hs_bindgen_eca2bca8e63194be_base ::
     IO BG.CBool
  -> IO (BG.FunPtr (IO BG.CBool))

-- __unique:__ @toToggle_Aux@
hs_bindgen_eca2bca8e63194be ::
     Toggle_Aux
  -> IO (BG.FunPtr Toggle_Aux)
hs_bindgen_eca2bca8e63194be =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_eca2bca8e63194be_base (fmap BG.toFFIType (BG.getField @"unwrapToggle_Aux" fun0)))

-- __unique:__ @fromToggle_Aux@
foreign import ccall safe "dynamic" hs_bindgen_703fc4bdc168721d_base ::
     BG.FunPtr (IO BG.CBool)
  -> IO BG.CBool

-- __unique:__ @fromToggle_Aux@
hs_bindgen_703fc4bdc168721d ::
     BG.FunPtr Toggle_Aux
  -> Toggle_Aux
hs_bindgen_703fc4bdc168721d =
  \funPtr0 ->
    Toggle_Aux (fmap BG.fromFFIType (hs_bindgen_703fc4bdc168721d_base (BG.castFunPtr funPtr0)))

-- __unique:__ @toCounter_Aux@
foreign import ccall safe "wrapper" hs_bindgen_2202848aad97fe0a_base ::
     IO BG.CInt
  -> IO (BG.FunPtr (IO BG.CInt))

-- __unique:__ @toCounter_Aux@
hs_bindgen_2202848aad97fe0a ::
     Counter_Aux
  -> IO (BG.FunPtr Counter_Aux)
hs_bindgen_2202848aad97fe0a =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_2202848aad97fe0a_base (fmap BG.toFFIType (BG.getField @"unwrapCounter_Aux" fun0)))

-- __unique:__ @fromCounter_Aux@
foreign import ccall safe "dynamic" hs_bindgen_73304cb84e9a2f8f_base ::
     BG.FunPtr (IO BG.CInt)
  -> IO BG.CInt

-- __unique:__ @fromCounter_Aux@
hs_bindgen_73304cb84e9a2f8f ::
     BG.FunPtr Counter_Aux
  -> Counter_Aux
hs_bindgen_73304cb84e9a2f8f =
  \funPtr0 ->
    Counter_Aux (fmap BG.fromFFIType (hs_bindgen_73304cb84e9a2f8f_base (BG.castFunPtr funPtr0)))

-- __unique:__ @toVarCounter_Aux@
foreign import ccall safe "wrapper" hs_bindgen_42a7337570f8b0d0_base ::
     (BG.CInt -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.CInt -> IO BG.CInt))

-- __unique:__ @toVarCounter_Aux@
hs_bindgen_42a7337570f8b0d0 ::
     VarCounter_Aux
  -> IO (BG.FunPtr VarCounter_Aux)
hs_bindgen_42a7337570f8b0d0 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_42a7337570f8b0d0_base (\x1 ->
                                                            fmap BG.toFFIType (BG.getField @"unwrapVarCounter_Aux" fun0 (BG.fromFFIType x1))))

-- __unique:__ @fromVarCounter_Aux@
foreign import ccall safe "dynamic" hs_bindgen_43d902480175fccf_base ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> BG.CInt -> IO BG.CInt

-- __unique:__ @fromVarCounter_Aux@
hs_bindgen_43d902480175fccf ::
     BG.FunPtr VarCounter_Aux
  -> VarCounter_Aux
hs_bindgen_43d902480175fccf =
  \funPtr0 ->
    VarCounter_Aux (\x1 ->
                      fmap BG.fromFFIType (hs_bindgen_43d902480175fccf_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)))
