{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Geometry.Layer.FunPtr
    ( Geometry.Layer.FunPtr.layer_paint_all
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Geometry.Layer
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <import_cycle.h>"
  , "/* test_librarywalkthroughimport_cyc_Geometry.Layer_get_layer_paint_all */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c38219d1cd4f0b07 (void)) ("
  , "  struct layer *arg1,"
  , "  layer_paint arg2,"
  , "  void *arg3"
  , ")"
  , "{"
  , "  return &layer_paint_all;"
  , "}"
  ]))

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Layer_get_layer_paint_all@
foreign import ccall unsafe "hs_bindgen_c38219d1cd4f0b07" hs_bindgen_c38219d1cd4f0b07_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Layer_get_layer_paint_all@
hs_bindgen_c38219d1cd4f0b07 :: IO (BG.FunPtr (BG.Ptr Layer -> Layer_paint -> BG.Ptr BG.Void -> IO ()))
hs_bindgen_c38219d1cd4f0b07 =
  fmap BG.fromFFIType hs_bindgen_c38219d1cd4f0b07_base

{-# NOINLINE layer_paint_all #-}
{-| __C declaration:__ @layer_paint_all@

    __defined at:__ @geometry\/layer.h 24:6@

    __exported by:__ @import_cycle.h@
-}
layer_paint_all :: BG.FunPtr (BG.Ptr Layer -> Layer_paint -> BG.Ptr BG.Void -> IO ())
layer_paint_all =
  BG.unsafePerformIO hs_bindgen_c38219d1cd4f0b07
