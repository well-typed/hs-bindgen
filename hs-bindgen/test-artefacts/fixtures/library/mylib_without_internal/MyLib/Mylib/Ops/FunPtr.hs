{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module MyLib.Mylib.Ops.FunPtr
    ( MyLib.Mylib.Ops.FunPtr.point_distance
    , MyLib.Mylib.Ops.FunPtr.rect_area
    , MyLib.Mylib.Ops.FunPtr.rect_contains
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified MyLib.Mylib.Types
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <mylib.h>"
  , "/* test_mylib_without_internal_MyLib.Mylib.Ops_get_point_distance */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_894277442746dd52 (void)) ("
  , "  point_t arg1,"
  , "  point_t arg2"
  , ")"
  , "{"
  , "  return &point_distance;"
  , "}"
  , "/* test_mylib_without_internal_MyLib.Mylib.Ops_get_rect_area */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_b80265d244a2698b (void)) ("
  , "  rect_t arg1"
  , ")"
  , "{"
  , "  return &rect_area;"
  , "}"
  , "/* test_mylib_without_internal_MyLib.Mylib.Ops_get_rect_contains */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_2ec06bc6184d47d5 (void)) ("
  , "  rect_t arg1,"
  , "  point_t arg2"
  , ")"
  , "{"
  , "  return &rect_contains;"
  , "}"
  ]))

-- __unique:__ @test_mylib_without_internal_MyLib.Mylib.Ops_get_point_distance@
foreign import ccall unsafe "hs_bindgen_894277442746dd52" hs_bindgen_894277442746dd52_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_mylib_without_internal_MyLib.Mylib.Ops_get_point_distance@
hs_bindgen_894277442746dd52 :: IO (BG.FunPtr (MyLib.Mylib.Types.Point_t -> MyLib.Mylib.Types.Point_t -> IO BG.CDouble))
hs_bindgen_894277442746dd52 =
  fmap BG.fromFFIType hs_bindgen_894277442746dd52_base

{-# NOINLINE point_distance #-}
{-| __C declaration:__ @point_distance@

    __defined at:__ @mylib\/ops.h 5:8@

    __exported by:__ @mylib.h@
-}
point_distance :: BG.FunPtr (MyLib.Mylib.Types.Point_t -> MyLib.Mylib.Types.Point_t -> IO BG.CDouble)
point_distance =
  BG.unsafePerformIO hs_bindgen_894277442746dd52

-- __unique:__ @test_mylib_without_internal_MyLib.Mylib.Ops_get_rect_area@
foreign import ccall unsafe "hs_bindgen_b80265d244a2698b" hs_bindgen_b80265d244a2698b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_mylib_without_internal_MyLib.Mylib.Ops_get_rect_area@
hs_bindgen_b80265d244a2698b :: IO (BG.FunPtr (MyLib.Mylib.Types.Rect_t -> IO BG.CDouble))
hs_bindgen_b80265d244a2698b =
  fmap BG.fromFFIType hs_bindgen_b80265d244a2698b_base

{-# NOINLINE rect_area #-}
{-| __C declaration:__ @rect_area@

    __defined at:__ @mylib\/ops.h 6:8@

    __exported by:__ @mylib.h@
-}
rect_area :: BG.FunPtr (MyLib.Mylib.Types.Rect_t -> IO BG.CDouble)
rect_area =
  BG.unsafePerformIO hs_bindgen_b80265d244a2698b

-- __unique:__ @test_mylib_without_internal_MyLib.Mylib.Ops_get_rect_contains@
foreign import ccall unsafe "hs_bindgen_2ec06bc6184d47d5" hs_bindgen_2ec06bc6184d47d5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_mylib_without_internal_MyLib.Mylib.Ops_get_rect_contains@
hs_bindgen_2ec06bc6184d47d5 :: IO (BG.FunPtr (MyLib.Mylib.Types.Rect_t -> MyLib.Mylib.Types.Point_t -> IO BG.CInt))
hs_bindgen_2ec06bc6184d47d5 =
  fmap BG.fromFFIType hs_bindgen_2ec06bc6184d47d5_base

{-# NOINLINE rect_contains #-}
{-| __C declaration:__ @rect_contains@

    __defined at:__ @mylib\/ops.h 7:5@

    __exported by:__ @mylib.h@
-}
rect_contains :: BG.FunPtr (MyLib.Mylib.Types.Rect_t -> MyLib.Mylib.Types.Point_t -> IO BG.CInt)
rect_contains =
  BG.unsafePerformIO hs_bindgen_2ec06bc6184d47d5
