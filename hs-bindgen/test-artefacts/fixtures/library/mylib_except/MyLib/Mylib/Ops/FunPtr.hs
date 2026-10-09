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
  , "/* test_librarymylib_except_MyLib.Mylib.Ops_get_point_distance */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_75a9333ff7111e73 (void)) ("
  , "  point_t arg1,"
  , "  point_t arg2"
  , ")"
  , "{"
  , "  return &point_distance;"
  , "}"
  , "/* test_librarymylib_except_MyLib.Mylib.Ops_get_rect_area */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_cd6a76dfe3c09d9c (void)) ("
  , "  rect_t arg1"
  , ")"
  , "{"
  , "  return &rect_area;"
  , "}"
  , "/* test_librarymylib_except_MyLib.Mylib.Ops_get_rect_contains */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_c499a455fb04b693 (void)) ("
  , "  rect_t arg1,"
  , "  point_t arg2"
  , ")"
  , "{"
  , "  return &rect_contains;"
  , "}"
  ]))

-- __unique:__ @test_librarymylib_except_MyLib.Mylib.Ops_get_point_distance@
foreign import ccall unsafe "hs_bindgen_75a9333ff7111e73" hs_bindgen_75a9333ff7111e73_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarymylib_except_MyLib.Mylib.Ops_get_point_distance@
hs_bindgen_75a9333ff7111e73 :: IO (BG.FunPtr (MyLib.Mylib.Types.Point_t -> MyLib.Mylib.Types.Point_t -> IO BG.CDouble))
hs_bindgen_75a9333ff7111e73 =
  fmap BG.fromFFIType hs_bindgen_75a9333ff7111e73_base

{-# NOINLINE point_distance #-}
{-| __C declaration:__ @point_distance@

    __defined at:__ @mylib\/ops.h 4:8@

    __exported by:__ @mylib.h@
-}
point_distance :: BG.FunPtr (MyLib.Mylib.Types.Point_t -> MyLib.Mylib.Types.Point_t -> IO BG.CDouble)
point_distance =
  BG.unsafePerformIO hs_bindgen_75a9333ff7111e73

-- __unique:__ @test_librarymylib_except_MyLib.Mylib.Ops_get_rect_area@
foreign import ccall unsafe "hs_bindgen_cd6a76dfe3c09d9c" hs_bindgen_cd6a76dfe3c09d9c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarymylib_except_MyLib.Mylib.Ops_get_rect_area@
hs_bindgen_cd6a76dfe3c09d9c :: IO (BG.FunPtr (MyLib.Mylib.Types.Rect_t -> IO BG.CDouble))
hs_bindgen_cd6a76dfe3c09d9c =
  fmap BG.fromFFIType hs_bindgen_cd6a76dfe3c09d9c_base

{-# NOINLINE rect_area #-}
{-| __C declaration:__ @rect_area@

    __defined at:__ @mylib\/ops.h 5:8@

    __exported by:__ @mylib.h@
-}
rect_area :: BG.FunPtr (MyLib.Mylib.Types.Rect_t -> IO BG.CDouble)
rect_area =
  BG.unsafePerformIO hs_bindgen_cd6a76dfe3c09d9c

-- __unique:__ @test_librarymylib_except_MyLib.Mylib.Ops_get_rect_contains@
foreign import ccall unsafe "hs_bindgen_c499a455fb04b693" hs_bindgen_c499a455fb04b693_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarymylib_except_MyLib.Mylib.Ops_get_rect_contains@
hs_bindgen_c499a455fb04b693 :: IO (BG.FunPtr (MyLib.Mylib.Types.Rect_t -> MyLib.Mylib.Types.Point_t -> IO BG.CInt))
hs_bindgen_c499a455fb04b693 =
  fmap BG.fromFFIType hs_bindgen_c499a455fb04b693_base

{-# NOINLINE rect_contains #-}
{-| __C declaration:__ @rect_contains@

    __defined at:__ @mylib\/ops.h 6:5@

    __exported by:__ @mylib.h@
-}
rect_contains :: BG.FunPtr (MyLib.Mylib.Types.Rect_t -> MyLib.Mylib.Types.Point_t -> IO BG.CInt)
rect_contains =
  BG.unsafePerformIO hs_bindgen_c499a455fb04b693
