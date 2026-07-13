{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.erf_random_user_specified_suffix
    , Example.FunPtr.bad_fma_random_user_specified_suffix
    , Example.FunPtr.no_args_random_user_specified_suffix
    , Example.FunPtr.no_args_no_void_random_user_specified_suffix
    , Example.FunPtr.fun_random_user_specified_suffix
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/simple_func.h>"
  , "/* test_functionssimple_func_1_rename_Example_get_erf */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_7d72952bb141f2c8 (void)) ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return &erf;"
  , "}"
  , "/* test_functionssimple_func_1_rename_Example_get_bad_fma */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_d8b56ce4d948e996 (void)) ("
  , "  double arg1,"
  , "  double arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return &bad_fma;"
  , "}"
  , "/* test_functionssimple_func_1_rename_Example_get_no_args */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b07b517c1cb304fc (void)) (void)"
  , "{"
  , "  return &no_args;"
  , "}"
  , "/* test_functionssimple_func_1_rename_Example_get_no_args_no_void */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cc43ff8560009309 (void)) (void)"
  , "{"
  , "  return &no_args_no_void;"
  , "}"
  , "/* test_functionssimple_func_1_rename_Example_get_fun */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_14a306e8e8f0ba6c (void)) ("
  , "  char arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &fun;"
  , "}"
  ]))

-- __unique:__ @test_functionssimple_func_1_rename_Example_get_erf@
foreign import ccall unsafe "hs_bindgen_7d72952bb141f2c8" hs_bindgen_7d72952bb141f2c8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionssimple_func_1_rename_Example_get_erf@
hs_bindgen_7d72952bb141f2c8 :: IO (BG.FunPtr (BG.CDouble -> IO BG.CDouble))
hs_bindgen_7d72952bb141f2c8 =
  BG.fromFFIType hs_bindgen_7d72952bb141f2c8_base

{-# NOINLINE erf_random_user_specified_suffix #-}
{-| __C declaration:__ @erf@

    __defined at:__ @functions\/simple_func.h 1:8@

    __exported by:__ @functions\/simple_func.h@
-}
erf_random_user_specified_suffix :: BG.FunPtr (BG.CDouble -> IO BG.CDouble)
erf_random_user_specified_suffix =
  BG.unsafePerformIO hs_bindgen_7d72952bb141f2c8

-- __unique:__ @test_functionssimple_func_1_rename_Example_get_bad_fma@
foreign import ccall unsafe "hs_bindgen_d8b56ce4d948e996" hs_bindgen_d8b56ce4d948e996_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionssimple_func_1_rename_Example_get_bad_fma@
hs_bindgen_d8b56ce4d948e996 :: IO (BG.FunPtr (BG.CDouble -> BG.CDouble -> BG.CDouble -> IO BG.CDouble))
hs_bindgen_d8b56ce4d948e996 =
  BG.fromFFIType hs_bindgen_d8b56ce4d948e996_base

{-# NOINLINE bad_fma_random_user_specified_suffix #-}
{-| __C declaration:__ @bad_fma@

    __defined at:__ @functions\/simple_func.h 3:22@

    __exported by:__ @functions\/simple_func.h@
-}
bad_fma_random_user_specified_suffix :: BG.FunPtr (BG.CDouble -> BG.CDouble -> BG.CDouble -> IO BG.CDouble)
bad_fma_random_user_specified_suffix =
  BG.unsafePerformIO hs_bindgen_d8b56ce4d948e996

-- __unique:__ @test_functionssimple_func_1_rename_Example_get_no_args@
foreign import ccall unsafe "hs_bindgen_b07b517c1cb304fc" hs_bindgen_b07b517c1cb304fc_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionssimple_func_1_rename_Example_get_no_args@
hs_bindgen_b07b517c1cb304fc :: IO (BG.FunPtr (IO ()))
hs_bindgen_b07b517c1cb304fc =
  BG.fromFFIType hs_bindgen_b07b517c1cb304fc_base

{-# NOINLINE no_args_random_user_specified_suffix #-}
{-| __C declaration:__ @no_args@

    __defined at:__ @functions\/simple_func.h 7:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args_random_user_specified_suffix :: BG.FunPtr (IO ())
no_args_random_user_specified_suffix =
  BG.unsafePerformIO hs_bindgen_b07b517c1cb304fc

-- __unique:__ @test_functionssimple_func_1_rename_Example_get_no_args_no_void@
foreign import ccall unsafe "hs_bindgen_cc43ff8560009309" hs_bindgen_cc43ff8560009309_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionssimple_func_1_rename_Example_get_no_args_no_void@
hs_bindgen_cc43ff8560009309 :: IO (BG.FunPtr (IO ()))
hs_bindgen_cc43ff8560009309 =
  BG.fromFFIType hs_bindgen_cc43ff8560009309_base

{-# NOINLINE no_args_no_void_random_user_specified_suffix #-}
{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @functions\/simple_func.h 9:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args_no_void_random_user_specified_suffix :: BG.FunPtr (IO ())
no_args_no_void_random_user_specified_suffix =
  BG.unsafePerformIO hs_bindgen_cc43ff8560009309

-- __unique:__ @test_functionssimple_func_1_rename_Example_get_fun@
foreign import ccall unsafe "hs_bindgen_14a306e8e8f0ba6c" hs_bindgen_14a306e8e8f0ba6c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionssimple_func_1_rename_Example_get_fun@
hs_bindgen_14a306e8e8f0ba6c :: IO (BG.FunPtr (BG.CChar -> BG.CDouble -> IO BG.CInt))
hs_bindgen_14a306e8e8f0ba6c =
  BG.fromFFIType hs_bindgen_14a306e8e8f0ba6c_base

{-# NOINLINE fun_random_user_specified_suffix #-}
{-| __C declaration:__ @fun@

    __defined at:__ @functions\/simple_func.h 11:5@

    __exported by:__ @functions\/simple_func.h@
-}
fun_random_user_specified_suffix :: BG.FunPtr (BG.CChar -> BG.CDouble -> IO BG.CInt)
fun_random_user_specified_suffix =
  BG.unsafePerformIO hs_bindgen_14a306e8e8f0ba6c
