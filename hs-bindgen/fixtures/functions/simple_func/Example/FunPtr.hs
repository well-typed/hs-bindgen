{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionssimple_func_Example_get_erf@
hs_bindgen_97654202e0d79fbb :: IO (RIP.FunPtr (RIP.CDouble -> IO RIP.CDouble))
hs_bindgen_97654202e0d79fbb =
  RIP.fromFFIType hs_bindgen_97654202e0d79fbb_base

{-# NOINLINE erf #-}
{-| __C declaration:__ @erf@

    __defined at:__ @functions\/simple_func.h 1:8@

    __exported by:__ @functions\/simple_func.h@
-}
erf :: RIP.FunPtr (RIP.CDouble -> IO RIP.CDouble)
erf = RIP.unsafePerformIO hs_bindgen_97654202e0d79fbb

-- __unique:__ @test_functionssimple_func_Example_get_bad_fma@
foreign import ccall unsafe "hs_bindgen_4c8418e152e9ddbc" hs_bindgen_4c8418e152e9ddbc_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionssimple_func_Example_get_bad_fma@
hs_bindgen_4c8418e152e9ddbc :: IO (RIP.FunPtr (RIP.CDouble -> RIP.CDouble -> RIP.CDouble -> IO RIP.CDouble))
hs_bindgen_4c8418e152e9ddbc =
  RIP.fromFFIType hs_bindgen_4c8418e152e9ddbc_base

{-# NOINLINE bad_fma #-}
{-| __C declaration:__ @bad_fma@

    __defined at:__ @functions\/simple_func.h 3:22@

    __exported by:__ @functions\/simple_func.h@
-}
bad_fma :: RIP.FunPtr (RIP.CDouble -> RIP.CDouble -> RIP.CDouble -> IO RIP.CDouble)
bad_fma =
  RIP.unsafePerformIO hs_bindgen_4c8418e152e9ddbc

-- __unique:__ @test_functionssimple_func_Example_get_no_args@
foreign import ccall unsafe "hs_bindgen_a271856b1fcc9477" hs_bindgen_a271856b1fcc9477_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionssimple_func_Example_get_no_args@
hs_bindgen_a271856b1fcc9477 :: IO (RIP.FunPtr (IO ()))
hs_bindgen_a271856b1fcc9477 =
  RIP.fromFFIType hs_bindgen_a271856b1fcc9477_base

{-# NOINLINE no_args #-}
{-| __C declaration:__ @no_args@

    __defined at:__ @functions\/simple_func.h 7:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args :: RIP.FunPtr (IO ())
no_args =
  RIP.unsafePerformIO hs_bindgen_a271856b1fcc9477

-- __unique:__ @test_functionssimple_func_Example_get_no_args_no_void@
foreign import ccall unsafe "hs_bindgen_73f2a4bbf4a49702" hs_bindgen_73f2a4bbf4a49702_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionssimple_func_Example_get_no_args_no_void@
hs_bindgen_73f2a4bbf4a49702 :: IO (RIP.FunPtr (IO ()))
hs_bindgen_73f2a4bbf4a49702 =
  RIP.fromFFIType hs_bindgen_73f2a4bbf4a49702_base

{-# NOINLINE no_args_no_void #-}
{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @functions\/simple_func.h 9:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args_no_void :: RIP.FunPtr (IO ())
no_args_no_void =
  RIP.unsafePerformIO hs_bindgen_73f2a4bbf4a49702

-- __unique:__ @test_functionssimple_func_Example_get_fun@
foreign import ccall unsafe "hs_bindgen_5416cbf886368c01" hs_bindgen_5416cbf886368c01_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionssimple_func_Example_get_fun@
hs_bindgen_5416cbf886368c01 :: IO (RIP.FunPtr (RIP.CChar -> RIP.CDouble -> IO RIP.CInt))
hs_bindgen_5416cbf886368c01 =
  RIP.fromFFIType hs_bindgen_5416cbf886368c01_base

{-# NOINLINE fun #-}
{-| __C declaration:__ @fun@

    __defined at:__ @functions\/simple_func.h 11:5@

    __exported by:__ @functions\/simple_func.h@
-}
fun :: RIP.FunPtr (RIP.CChar -> RIP.CDouble -> IO RIP.CInt)
fun = RIP.unsafePerformIO hs_bindgen_5416cbf886368c01
