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
  [ "#include <uses_without_including.h>"
  , "/* test_librarywalkthroughuses_witho_Geometry.Shape_get_shape_new */"
  , "__attribute__ ((const))"
  , "struct shape *(*hs_bindgen_90a44a64f1838f04 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &shape_new;"
  , "}"
  , "/* test_librarywalkthroughuses_witho_Geometry.Shape_get_shape_count_matching */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_3e337bf3720784cb (void)) ("
  , "  struct shape **arg1,"
  , "  signed int arg2,"
  , "  shape_filter arg3,"
  , "  void *arg4"
  , ")"
  , "{"
  , "  return &shape_count_matching;"
  , "}"
  , "/* test_librarywalkthroughuses_witho_Geometry.Shape_get_shape_for_each_point */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ceb7c9c10c7fed1d (void)) ("
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

-- __unique:__ @test_librarywalkthroughuses_witho_Geometry.Shape_get_shape_new@
foreign import ccall unsafe "hs_bindgen_90a44a64f1838f04" hs_bindgen_90a44a64f1838f04_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarywalkthroughuses_witho_Geometry.Shape_get_shape_new@
hs_bindgen_90a44a64f1838f04 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr Shape)))
hs_bindgen_90a44a64f1838f04 =
  fmap BG.fromFFIType hs_bindgen_90a44a64f1838f04_base

{-# NOINLINE shape_new #-}
{-| __C declaration:__ @shape_new@

    __defined at:__ @..\/shape.h 16:15@

    __exported by:__ @uses_without_including.h@
-}
shape_new :: BG.FunPtr (BG.CInt -> IO (BG.Ptr Shape))
shape_new =
  BG.unsafePerformIO hs_bindgen_90a44a64f1838f04

-- __unique:__ @test_librarywalkthroughuses_witho_Geometry.Shape_get_shape_count_matching@
foreign import ccall unsafe "hs_bindgen_3e337bf3720784cb" hs_bindgen_3e337bf3720784cb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarywalkthroughuses_witho_Geometry.Shape_get_shape_count_matching@
hs_bindgen_3e337bf3720784cb :: IO (BG.FunPtr (BG.Ptr (BG.Ptr Shape) -> BG.CInt -> Shape_filter -> BG.Ptr BG.Void -> IO BG.CInt))
hs_bindgen_3e337bf3720784cb =
  fmap BG.fromFFIType hs_bindgen_3e337bf3720784cb_base

{-# NOINLINE shape_count_matching #-}
{-| __C declaration:__ @shape_count_matching@

    __defined at:__ @..\/shape.h 17:5@

    __exported by:__ @uses_without_including.h@
-}
shape_count_matching :: BG.FunPtr (BG.Ptr (BG.Ptr Shape) -> BG.CInt -> Shape_filter -> BG.Ptr BG.Void -> IO BG.CInt)
shape_count_matching =
  BG.unsafePerformIO hs_bindgen_3e337bf3720784cb

-- __unique:__ @test_librarywalkthroughuses_witho_Geometry.Shape_get_shape_for_each_point@
foreign import ccall unsafe "hs_bindgen_ceb7c9c10c7fed1d" hs_bindgen_ceb7c9c10c7fed1d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarywalkthroughuses_witho_Geometry.Shape_get_shape_for_each_point@
hs_bindgen_ceb7c9c10c7fed1d :: IO (BG.FunPtr (BG.Ptr Shape -> BG.FunPtr (BG.Ptr Geometry.Point.Point -> BG.Ptr BG.Void -> IO ()) -> BG.Ptr BG.Void -> IO ()))
hs_bindgen_ceb7c9c10c7fed1d =
  fmap BG.fromFFIType hs_bindgen_ceb7c9c10c7fed1d_base

{-# NOINLINE shape_for_each_point #-}
{-| __C declaration:__ @shape_for_each_point@

    __defined at:__ @..\/shape.h 21:6@

    __exported by:__ @uses_without_including.h@
-}
shape_for_each_point :: BG.FunPtr (BG.Ptr Shape -> BG.FunPtr (BG.Ptr Geometry.Point.Point -> BG.Ptr BG.Void -> IO ()) -> BG.Ptr BG.Void -> IO ())
shape_for_each_point =
  BG.unsafePerformIO hs_bindgen_ceb7c9c10c7fed1d
