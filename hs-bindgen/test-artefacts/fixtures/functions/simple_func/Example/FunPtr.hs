{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.erf
    , Example.FunPtr.bad_fma
    , Example.FunPtr.no_args
    , Example.FunPtr.no_args_no_void
    , Example.FunPtr.fun
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/simple_func.h>"
  , "/* test_functionssimple_func_Example_get_erf */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_97654202e0d79fbb (void)) ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return &erf;"
  , "}"
  , "/* test_functionssimple_func_Example_get_bad_fma */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_4c8418e152e9ddbc (void)) ("
  , "  double arg1,"
  , "  double arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return &bad_fma;"
  , "}"
  , "/* test_functionssimple_func_Example_get_no_args */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a271856b1fcc9477 (void)) (void)"
  , "{"
  , "  return &no_args;"
  , "}"
  , "/* test_functionssimple_func_Example_get_no_args_no_void */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_73f2a4bbf4a49702 (void)) (void)"
  , "{"
  , "  return &no_args_no_void;"
  , "}"
  , "/* test_functionssimple_func_Example_get_fun */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_5416cbf886368c01 (void)) ("
  , "  char arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &fun;"
  , "}"
  ]))

-- __unique:__ @test_functionssimple_func_Example_get_erf@
foreign import ccall unsafe "hs_bindgen_97654202e0d79fbb" hs_bindgen_97654202e0d79fbb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionssimple_func_Example_get_erf@
hs_bindgen_97654202e0d79fbb :: IO (BG.FunPtr (BG.CDouble -> IO BG.CDouble))
hs_bindgen_97654202e0d79fbb =
  BG.fromFFIType hs_bindgen_97654202e0d79fbb_base

{-# NOINLINE erf #-}
{-| __C declaration:__ @erf@

    __defined at:__ @functions\/simple_func.h 1:8@

    __exported by:__ @functions\/simple_func.h@
-}
erf :: BG.FunPtr (BG.CDouble -> IO BG.CDouble)
erf = BG.unsafePerformIO hs_bindgen_97654202e0d79fbb

-- __unique:__ @test_functionssimple_func_Example_get_bad_fma@
foreign import ccall unsafe "hs_bindgen_4c8418e152e9ddbc" hs_bindgen_4c8418e152e9ddbc_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionssimple_func_Example_get_bad_fma@
hs_bindgen_4c8418e152e9ddbc :: IO (BG.FunPtr (BG.CDouble -> BG.CDouble -> BG.CDouble -> IO BG.CDouble))
hs_bindgen_4c8418e152e9ddbc =
  BG.fromFFIType hs_bindgen_4c8418e152e9ddbc_base

{-# NOINLINE bad_fma #-}
{-| __C declaration:__ @bad_fma@

    __defined at:__ @functions\/simple_func.h 3:22@

    __exported by:__ @functions\/simple_func.h@
-}
bad_fma :: BG.FunPtr (BG.CDouble -> BG.CDouble -> BG.CDouble -> IO BG.CDouble)
bad_fma =
  BG.unsafePerformIO hs_bindgen_4c8418e152e9ddbc

-- __unique:__ @test_functionssimple_func_Example_get_no_args@
foreign import ccall unsafe "hs_bindgen_a271856b1fcc9477" hs_bindgen_a271856b1fcc9477_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionssimple_func_Example_get_no_args@
hs_bindgen_a271856b1fcc9477 :: IO (BG.FunPtr (IO ()))
hs_bindgen_a271856b1fcc9477 =
  BG.fromFFIType hs_bindgen_a271856b1fcc9477_base

{-# NOINLINE no_args #-}
{-| __C declaration:__ @no_args@

    __defined at:__ @functions\/simple_func.h 7:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args :: BG.FunPtr (IO ())
no_args =
  BG.unsafePerformIO hs_bindgen_a271856b1fcc9477

-- __unique:__ @test_functionssimple_func_Example_get_no_args_no_void@
foreign import ccall unsafe "hs_bindgen_73f2a4bbf4a49702" hs_bindgen_73f2a4bbf4a49702_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionssimple_func_Example_get_no_args_no_void@
hs_bindgen_73f2a4bbf4a49702 :: IO (BG.FunPtr (IO ()))
hs_bindgen_73f2a4bbf4a49702 =
  BG.fromFFIType hs_bindgen_73f2a4bbf4a49702_base

{-# NOINLINE no_args_no_void #-}
{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @functions\/simple_func.h 9:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args_no_void :: BG.FunPtr (IO ())
no_args_no_void =
  BG.unsafePerformIO hs_bindgen_73f2a4bbf4a49702

-- __unique:__ @test_functionssimple_func_Example_get_fun@
foreign import ccall unsafe "hs_bindgen_5416cbf886368c01" hs_bindgen_5416cbf886368c01_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionssimple_func_Example_get_fun@
hs_bindgen_5416cbf886368c01 :: IO (BG.FunPtr (BG.CChar -> BG.CDouble -> IO BG.CInt))
hs_bindgen_5416cbf886368c01 =
  BG.fromFFIType hs_bindgen_5416cbf886368c01_base

{-# NOINLINE fun #-}
{-| __C declaration:__ @fun@

    __defined at:__ @functions\/simple_func.h 11:5@

    __exported by:__ @functions\/simple_func.h@
-}
fun :: BG.FunPtr (BG.CChar -> BG.CDouble -> IO BG.CInt)
fun = BG.unsafePerformIO hs_bindgen_5416cbf886368c01
