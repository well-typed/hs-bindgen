{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Geometry.Layer.Safe
    ( Geometry.Layer.Safe.layer_paint_all
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Geometry.Layer
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <import_cycle.h>"
  , "void hs_bindgen_ce27507f98bc0ae5 ("
  , "  struct layer *arg1,"
  , "  layer_paint arg2,"
  , "  void *arg3"
  , ")"
  , "{"
  , "  (layer_paint_all)(arg1, arg2, arg3);"
  , "}"
  ]))

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Layer_Safe_layer_paint_all@
foreign import ccall safe "hs_bindgen_ce27507f98bc0ae5" hs_bindgen_ce27507f98bc0ae5_base ::
     BG.Ptr BG.Void
  -> BG.FunPtr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Layer_Safe_layer_paint_all@
hs_bindgen_ce27507f98bc0ae5 ::
     BG.Ptr Layer
  -> Layer_paint
  -> BG.Ptr BG.Void
  -> IO ()
hs_bindgen_ce27507f98bc0ae5 =
  \x0 ->
    \x1 ->
      \x2 ->
        hs_bindgen_ce27507f98bc0ae5_base (BG.toFFIType x0) (BG.toFFIType x1) (BG.toFFIType x2)

{-| __C declaration:__ @layer_paint_all@

    __defined at:__ @geometry\/layer.h 24:6@

    __exported by:__ @import_cycle.h@
-}
layer_paint_all ::
     BG.Ptr Layer
     -- ^ __C declaration:__ @layer@
  -> Layer_paint
     -- ^ __C declaration:__ @paint@
  -> BG.Ptr BG.Void
     -- ^ __C declaration:__ @user@
  -> IO ()
layer_paint_all = hs_bindgen_ce27507f98bc0ae5
