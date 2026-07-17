{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.erf
    , Example.Unsafe.bad_fma
    , Example.Unsafe.no_args
    , Example.Unsafe.no_args_no_void
    , Example.Unsafe.fun
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/simple_func.h>"
  , "double hs_bindgen_da5d889180d72efd ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return (erf)(arg1);"
  , "}"
  , "double hs_bindgen_d02f37accebc0cb3 ("
  , "  double arg1,"
  , "  double arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return (bad_fma)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_9ea56ae4fab9a418 (void)"
  , "{"
  , "  (no_args)();"
  , "}"
  , "void hs_bindgen_a3d1783059ec7820 (void)"
  , "{"
  , "  (no_args_no_void)();"
  , "}"
  , "signed int hs_bindgen_91392ef466aa34e7 ("
  , "  char arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (fun)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_functionssimple_func_Example_Unsafe_erf@
foreign import ccall unsafe "hs_bindgen_da5d889180d72efd" hs_bindgen_da5d889180d72efd_base ::
     Double
  -> Double

-- __unique:__ @test_functionssimple_func_Example_Unsafe_erf@
hs_bindgen_da5d889180d72efd ::
     BG.CDouble
  -> BG.CDouble
hs_bindgen_da5d889180d72efd =
  BG.fromFFIType hs_bindgen_da5d889180d72efd_base

{-|

    Marked @__attribute((const))__@

    __C declaration:__ @erf@

    __defined at:__ @functions\/simple_func.h 1:8@

    __exported by:__ @functions\/simple_func.h@
-}
erf ::
     BG.CDouble
     -- ^ __C declaration:__ @arg@
  -> BG.CDouble
erf = hs_bindgen_da5d889180d72efd

-- __unique:__ @test_functionssimple_func_Example_Unsafe_bad_fma@
foreign import ccall unsafe "hs_bindgen_d02f37accebc0cb3" hs_bindgen_d02f37accebc0cb3_base ::
     Double
  -> Double
  -> Double
  -> IO Double

-- __unique:__ @test_functionssimple_func_Example_Unsafe_bad_fma@
hs_bindgen_d02f37accebc0cb3 ::
     BG.CDouble
  -> BG.CDouble
  -> BG.CDouble
  -> IO BG.CDouble
hs_bindgen_d02f37accebc0cb3 =
  BG.fromFFIType hs_bindgen_d02f37accebc0cb3_base

{-| __C declaration:__ @bad_fma@

    __defined at:__ @functions\/simple_func.h 3:22@

    __exported by:__ @functions\/simple_func.h@
-}
bad_fma ::
     BG.CDouble
     -- ^ __C declaration:__ @x@
  -> BG.CDouble
     -- ^ __C declaration:__ @y@
  -> BG.CDouble
     -- ^ __C declaration:__ @z@
  -> IO BG.CDouble
bad_fma = hs_bindgen_d02f37accebc0cb3

-- __unique:__ @test_functionssimple_func_Example_Unsafe_no_args@
foreign import ccall unsafe "hs_bindgen_9ea56ae4fab9a418" hs_bindgen_9ea56ae4fab9a418_base ::
     IO ()

-- __unique:__ @test_functionssimple_func_Example_Unsafe_no_args@
hs_bindgen_9ea56ae4fab9a418 :: IO ()
hs_bindgen_9ea56ae4fab9a418 =
  BG.fromFFIType hs_bindgen_9ea56ae4fab9a418_base

{-| __C declaration:__ @no_args@

    __defined at:__ @functions\/simple_func.h 7:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args :: IO ()
no_args = hs_bindgen_9ea56ae4fab9a418

-- __unique:__ @test_functionssimple_func_Example_Unsafe_no_args_no_void@
foreign import ccall unsafe "hs_bindgen_a3d1783059ec7820" hs_bindgen_a3d1783059ec7820_base ::
     IO ()

-- __unique:__ @test_functionssimple_func_Example_Unsafe_no_args_no_void@
hs_bindgen_a3d1783059ec7820 :: IO ()
hs_bindgen_a3d1783059ec7820 =
  BG.fromFFIType hs_bindgen_a3d1783059ec7820_base

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @functions\/simple_func.h 9:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args_no_void :: IO ()
no_args_no_void = hs_bindgen_a3d1783059ec7820

-- __unique:__ @test_functionssimple_func_Example_Unsafe_fun@
foreign import ccall unsafe "hs_bindgen_91392ef466aa34e7" hs_bindgen_91392ef466aa34e7_base ::
     BG.Int8
  -> Double
  -> IO BG.Int32

-- __unique:__ @test_functionssimple_func_Example_Unsafe_fun@
hs_bindgen_91392ef466aa34e7 ::
     BG.CChar
  -> BG.CDouble
  -> IO BG.CInt
hs_bindgen_91392ef466aa34e7 =
  BG.fromFFIType hs_bindgen_91392ef466aa34e7_base

{-| __C declaration:__ @fun@

    __defined at:__ @functions\/simple_func.h 11:5@

    __exported by:__ @functions\/simple_func.h@
-}
fun ::
     BG.CChar
     -- ^ __C declaration:__ @x@
  -> BG.CDouble
     -- ^ __C declaration:__ @y@
  -> IO BG.CInt
fun = hs_bindgen_91392ef466aa34e7
