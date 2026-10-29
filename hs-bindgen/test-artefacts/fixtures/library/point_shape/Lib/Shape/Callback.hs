{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Lib.Shape.Callback where

import qualified HsBindgen.Runtime.Support
import qualified HsBindgen.Runtime.Support as BG
import Lib.Point ()
import Lib.Shape
import Prelude (IO, fmap)

-- __unique:__ @toOn_draw_Aux@
foreign import ccall safe "wrapper" hs_bindgen_e0535817c449f9e4_base ::
     (BG.Ptr BG.Void -> IO HsBindgen.Runtime.Support.CInt)
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> IO HsBindgen.Runtime.Support.CInt))

-- __unique:__ @toOn_draw_Aux@
hs_bindgen_e0535817c449f9e4 ::
     On_draw_Aux
  -> IO (BG.FunPtr On_draw_Aux)
hs_bindgen_e0535817c449f9e4 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_e0535817c449f9e4_base (\x1 ->
                                                            fmap BG.toFFIType (BG.getField @"unwrapOn_draw_Aux" fun0 (BG.fromFFIType x1))))

-- __unique:__ @fromOn_draw_Aux@
foreign import ccall safe "dynamic" hs_bindgen_7a6697890840e122_base ::
     BG.FunPtr (BG.Ptr BG.Void -> IO HsBindgen.Runtime.Support.CInt)
  -> BG.Ptr BG.Void -> IO HsBindgen.Runtime.Support.CInt

-- __unique:__ @fromOn_draw_Aux@
hs_bindgen_7a6697890840e122 ::
     BG.FunPtr On_draw_Aux
  -> On_draw_Aux
hs_bindgen_7a6697890840e122 =
  \funPtr0 ->
    On_draw_Aux (\x1 ->
                   fmap BG.fromFFIType (hs_bindgen_7a6697890840e122_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)))
