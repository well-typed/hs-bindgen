{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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

-- | __unique:__ @test_functionssimple_func_1_rename_Example_get_erf@
foreign import ccall unsafe "hs_bindgen_7d72952bb141f2c8" hs_bindgen_7d72952bb141f2c8 ::
     IO (Ptr.FunPtr (FC.CDouble -> IO FC.CDouble))

{-# NOINLINE erf_suffix #-}

{-| __C declaration:__ @erf@

    __defined at:__ @functions\/simple_func.h:1:8@

    __exported by:__ @functions\/simple_func.h@
-}
erf_suffix :: Ptr.FunPtr (FC.CDouble -> IO FC.CDouble)
erf_suffix =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7d72952bb141f2c8

-- | __unique:__ @test_functionssimple_func_1_rename_Example_get_bad_fma@
foreign import ccall unsafe "hs_bindgen_d8b56ce4d948e996" hs_bindgen_d8b56ce4d948e996 ::
     IO (Ptr.FunPtr (FC.CDouble -> FC.CDouble -> FC.CDouble -> IO FC.CDouble))

{-# NOINLINE bad_fma_suffix #-}

{-| __C declaration:__ @bad_fma@

    __defined at:__ @functions\/simple_func.h:3:22@

    __exported by:__ @functions\/simple_func.h@
-}
bad_fma_suffix :: Ptr.FunPtr (FC.CDouble -> FC.CDouble -> FC.CDouble -> IO FC.CDouble)
bad_fma_suffix =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d8b56ce4d948e996

-- | __unique:__ @test_functionssimple_func_1_rename_Example_get_no_args@
foreign import ccall unsafe "hs_bindgen_b07b517c1cb304fc" hs_bindgen_b07b517c1cb304fc ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE no_args_suffix #-}

{-| __C declaration:__ @no_args@

    __defined at:__ @functions\/simple_func.h:7:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args_suffix :: Ptr.FunPtr (IO ())
no_args_suffix =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b07b517c1cb304fc

-- | __unique:__ @test_functionssimple_func_1_rename_Example_get_no_args_no_void@
foreign import ccall unsafe "hs_bindgen_cc43ff8560009309" hs_bindgen_cc43ff8560009309 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE no_args_no_void_suffix #-}

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @functions\/simple_func.h:9:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args_no_void_suffix :: Ptr.FunPtr (IO ())
no_args_no_void_suffix =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cc43ff8560009309

-- | __unique:__ @test_functionssimple_func_1_rename_Example_get_fun@
foreign import ccall unsafe "hs_bindgen_14a306e8e8f0ba6c" hs_bindgen_14a306e8e8f0ba6c ::
     IO (Ptr.FunPtr (FC.CChar -> FC.CDouble -> IO FC.CInt))

{-# NOINLINE fun_suffix #-}

{-| __C declaration:__ @fun@

    __defined at:__ @functions\/simple_func.h:11:5@

    __exported by:__ @functions\/simple_func.h@
-}
fun_suffix :: Ptr.FunPtr (FC.CChar -> FC.CDouble -> IO FC.CInt)
fun_suffix =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_14a306e8e8f0ba6c
