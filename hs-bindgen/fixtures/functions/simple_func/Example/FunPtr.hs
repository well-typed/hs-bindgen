{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/simple_func.h>"
  , "/* test_functionssimple_func_Example_get_erf_ptr */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_1308338f62c45845 (void)) ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return &erf;"
  , "}"
  , "/* test_functionssimple_func_Example_get_bad_fma_ptr */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_457f7d0956688086 (void)) ("
  , "  double arg1,"
  , "  double arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return &bad_fma;"
  , "}"
  , "/* test_functionssimple_func_Example_get_no_args_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_db204712b6d929ba (void)) (void)"
  , "{"
  , "  return &no_args;"
  , "}"
  , "/* test_functionssimple_func_Example_get_no_args_no_void_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d8fd245fa84413ae (void)) (void)"
  , "{"
  , "  return &no_args_no_void;"
  , "}"
  , "/* test_functionssimple_func_Example_get_fun_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_8541b259788f68ad (void)) ("
  , "  char arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &fun;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_1308338f62c45845" hs_bindgen_1308338f62c45845_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionssimple_func_Example_get_erf_ptr@
hs_bindgen_1308338f62c45845 ::
     IO (Ptr.FunPtr (FC.CDouble -> IO FC.CDouble))
hs_bindgen_1308338f62c45845 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_1308338f62c45845_base

{-# NOINLINE erf_ptr #-}

{-| __C declaration:__ @erf@

    __defined at:__ @functions\/simple_func.h:1:8@

    __exported by:__ @functions\/simple_func.h@
-}
erf_ptr :: Ptr.FunPtr (FC.CDouble -> IO FC.CDouble)
erf_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1308338f62c45845

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_457f7d0956688086" hs_bindgen_457f7d0956688086_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionssimple_func_Example_get_bad_fma_ptr@
hs_bindgen_457f7d0956688086 ::
     IO (Ptr.FunPtr (FC.CDouble -> FC.CDouble -> FC.CDouble -> IO FC.CDouble))
hs_bindgen_457f7d0956688086 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_457f7d0956688086_base

{-# NOINLINE bad_fma_ptr #-}

{-| __C declaration:__ @bad_fma@

    __defined at:__ @functions\/simple_func.h:3:22@

    __exported by:__ @functions\/simple_func.h@
-}
bad_fma_ptr :: Ptr.FunPtr (FC.CDouble -> FC.CDouble -> FC.CDouble -> IO FC.CDouble)
bad_fma_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_457f7d0956688086

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_db204712b6d929ba" hs_bindgen_db204712b6d929ba_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionssimple_func_Example_get_no_args_ptr@
hs_bindgen_db204712b6d929ba ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_db204712b6d929ba =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_db204712b6d929ba_base

{-# NOINLINE no_args_ptr #-}

{-| __C declaration:__ @no_args@

    __defined at:__ @functions\/simple_func.h:7:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args_ptr :: Ptr.FunPtr (IO ())
no_args_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_db204712b6d929ba

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_d8fd245fa84413ae" hs_bindgen_d8fd245fa84413ae_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionssimple_func_Example_get_no_args_no_void_ptr@
hs_bindgen_d8fd245fa84413ae ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_d8fd245fa84413ae =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_d8fd245fa84413ae_base

{-# NOINLINE no_args_no_void_ptr #-}

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @functions\/simple_func.h:9:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args_no_void_ptr :: Ptr.FunPtr (IO ())
no_args_no_void_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d8fd245fa84413ae

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_8541b259788f68ad" hs_bindgen_8541b259788f68ad_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionssimple_func_Example_get_fun_ptr@
hs_bindgen_8541b259788f68ad ::
     IO (Ptr.FunPtr (FC.CChar -> FC.CDouble -> IO FC.CInt))
hs_bindgen_8541b259788f68ad =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_8541b259788f68ad_base

{-# NOINLINE fun_ptr #-}

{-| __C declaration:__ @fun@

    __defined at:__ @functions\/simple_func.h:11:5@

    __exported by:__ @functions\/simple_func.h@
-}
fun_ptr :: Ptr.FunPtr (FC.CChar -> FC.CDouble -> IO FC.CInt)
fun_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8541b259788f68ad
