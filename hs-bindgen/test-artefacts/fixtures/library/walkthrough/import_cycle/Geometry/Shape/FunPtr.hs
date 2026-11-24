{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Geometry.Shape.FunPtr
    ( Geometry.Shape.FunPtr.shape_new
    , Geometry.Shape.FunPtr.shape_count_matching
    , Geometry.Shape.FunPtr.shape_for_each_point
    )
  where

import qualified Geometry.Point
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Geometry.Shape
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <import_cycle.h>"
  , "/* test_librarywalkthroughimport_cyc_Geometry.Shape_get_shape_new */"
  , "__attribute__ ((const))"
  , "struct shape *(*hs_bindgen_1726be9796186074 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &shape_new;"
  , "}"
  , "/* test_librarywalkthroughimport_cyc_Geometry.Shape_get_shape_count_matching */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_66dd1e7b9eab6730 (void)) ("
  , "  struct shape **arg1,"
  , "  signed int arg2,"
  , "  shape_filter arg3,"
  , "  void *arg4"
  , ")"
  , "{"
  , "  return &shape_count_matching;"
  , "}"
  , "/* test_librarywalkthroughimport_cyc_Geometry.Shape_get_shape_for_each_point */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3f5dc35ae6f75fb3 (void)) ("
  , "  struct shape *arg1,"
  , "  void (*arg2) ("
  , "  struct point *arg1,"
  , "  void *arg2"
  , "),"
  , "  void *arg3"
  , ")"
  , "{"
  , "  return &shape_for_each_point;"
  , "}"
  ]))

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Shape_get_shape_new@
foreign import ccall unsafe "hs_bindgen_1726be9796186074" hs_bindgen_1726be9796186074_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Shape_get_shape_new@
hs_bindgen_1726be9796186074 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr Shape)))
hs_bindgen_1726be9796186074 =
  fmap BG.fromFFIType hs_bindgen_1726be9796186074_base

{-# NOINLINE shape_new #-}
{-| __C declaration:__ @shape_new@

    __defined at:__ @..\/shape.h 16:15@

    __exported by:__ @import_cycle.h@
-}
shape_new :: BG.FunPtr (BG.CInt -> IO (BG.Ptr Shape))
shape_new =
  BG.unsafePerformIO hs_bindgen_1726be9796186074

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Shape_get_shape_count_matching@
foreign import ccall unsafe "hs_bindgen_66dd1e7b9eab6730" hs_bindgen_66dd1e7b9eab6730_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Shape_get_shape_count_matching@
hs_bindgen_66dd1e7b9eab6730 :: IO (BG.FunPtr (BG.Ptr (BG.Ptr Shape) -> BG.CInt -> Shape_filter -> BG.Ptr BG.Void -> IO BG.CInt))
hs_bindgen_66dd1e7b9eab6730 =
  fmap BG.fromFFIType hs_bindgen_66dd1e7b9eab6730_base

{-# NOINLINE shape_count_matching #-}
{-| __C declaration:__ @shape_count_matching@

    __defined at:__ @..\/shape.h 17:5@

    __exported by:__ @import_cycle.h@
-}
shape_count_matching :: BG.FunPtr (BG.Ptr (BG.Ptr Shape) -> BG.CInt -> Shape_filter -> BG.Ptr BG.Void -> IO BG.CInt)
shape_count_matching =
  BG.unsafePerformIO hs_bindgen_66dd1e7b9eab6730

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Shape_get_shape_for_each_point@
foreign import ccall unsafe "hs_bindgen_3f5dc35ae6f75fb3" hs_bindgen_3f5dc35ae6f75fb3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Shape_get_shape_for_each_point@
hs_bindgen_3f5dc35ae6f75fb3 :: IO (BG.FunPtr (BG.Ptr Shape -> BG.FunPtr (BG.Ptr Geometry.Point.Point -> BG.Ptr BG.Void -> IO ()) -> BG.Ptr BG.Void -> IO ()))
hs_bindgen_3f5dc35ae6f75fb3 =
  fmap BG.fromFFIType hs_bindgen_3f5dc35ae6f75fb3_base

{-# NOINLINE shape_for_each_point #-}
{-| __C declaration:__ @shape_for_each_point@

    __defined at:__ @..\/shape.h 21:6@

    __exported by:__ @import_cycle.h@
-}
shape_for_each_point :: BG.FunPtr (BG.Ptr Shape -> BG.FunPtr (BG.Ptr Geometry.Point.Point -> BG.Ptr BG.Void -> IO ()) -> BG.Ptr BG.Void -> IO ())
shape_for_each_point =
  BG.unsafePerformIO hs_bindgen_3f5dc35ae6f75fb3
