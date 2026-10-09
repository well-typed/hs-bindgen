{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module MyLib.Mylib.Ops.FunPtr
    ( MyLib.Mylib.Ops.FunPtr.point_distance
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified MyLib.Mylib.Types
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <mylib.h>"
  , "/* test_librarymylib_except_decl_MyLib.Mylib.Ops_get_point_distance */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_aa31bd6bd6dfbafe (void)) ("
  , "  point_t arg1,"
  , "  point_t arg2"
  , ")"
  , "{"
  , "  return &point_distance;"
  , "}"
  ]))

-- __unique:__ @test_librarymylib_except_decl_MyLib.Mylib.Ops_get_point_distance@
foreign import ccall unsafe "hs_bindgen_aa31bd6bd6dfbafe" hs_bindgen_aa31bd6bd6dfbafe_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarymylib_except_decl_MyLib.Mylib.Ops_get_point_distance@
hs_bindgen_aa31bd6bd6dfbafe :: IO (BG.FunPtr (MyLib.Mylib.Types.Point_t -> MyLib.Mylib.Types.Point_t -> IO BG.CDouble))
hs_bindgen_aa31bd6bd6dfbafe =
  fmap BG.fromFFIType hs_bindgen_aa31bd6bd6dfbafe_base

{-# NOINLINE point_distance #-}
{-| __C declaration:__ @point_distance@

    __defined at:__ @mylib\/ops.h 4:8@

    __exported by:__ @mylib.h@
-}
point_distance :: BG.FunPtr (MyLib.Mylib.Types.Point_t -> MyLib.Mylib.Types.Point_t -> IO BG.CDouble)
point_distance =
  BG.unsafePerformIO hs_bindgen_aa31bd6bd6dfbafe
