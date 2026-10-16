{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Example.HsBootInternal where

import qualified HsBindgen.Runtime.Support as BG
import Example
import Prelude (IO, fmap)

-- __unique:__ @toOn_event_Aux@
foreign import ccall safe "wrapper" hs_bindgen_70c2110bb36ed51a_base ::
     (BG.CInt -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.CInt -> IO BG.CInt))

-- __unique:__ @toOn_event_Aux@
hs_bindgen_70c2110bb36ed51a ::
     On_event_Aux
  -> IO (BG.FunPtr On_event_Aux)
hs_bindgen_70c2110bb36ed51a =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_70c2110bb36ed51a_base (\x1 ->
                                                            fmap BG.toFFIType (BG.getField @"unwrapOn_event_Aux" fun0 (BG.fromFFIType x1))))

-- __unique:__ @fromOn_event_Aux@
foreign import ccall safe "dynamic" hs_bindgen_a7705d093ccaa37a_base ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> BG.CInt -> IO BG.CInt

-- __unique:__ @fromOn_event_Aux@
hs_bindgen_a7705d093ccaa37a ::
     BG.FunPtr On_event_Aux
  -> On_event_Aux
hs_bindgen_a7705d093ccaa37a =
  \funPtr0 ->
    On_event_Aux (\x1 ->
                    fmap BG.fromFFIType (hs_bindgen_a7705d093ccaa37a_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)))
