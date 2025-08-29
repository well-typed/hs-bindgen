{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <simple_func.h>\ndouble hs_bindgen_test_simple_func_3919a2f9a4498aaa (double arg1) { return erf(arg1); }\n/* get_erf_ptr */ __attribute__ ((const)) double (*hs_bindgen_test_simple_func_e3d5d1926d499ff8 (void)) (double arg1) { return &erf; } \ndouble hs_bindgen_test_simple_func_6be780963284c499 (double arg1, double arg2, double arg3) { return bad_fma(arg1, arg2, arg3); }\n/* get_bad_fma_ptr */ __attribute__ ((const)) double (*hs_bindgen_test_simple_func_6e78b576543cf13e (void)) (double arg1, double arg2, double arg3) { return &bad_fma; } \nvoid hs_bindgen_test_simple_func_63e35f316cc0a04e (void) { no_args(); }\n/* get_no_args_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_simple_func_a41af67e28348e9e (void)) (void) { return &no_args; } \nvoid hs_bindgen_test_simple_func_9d7e58d4e189732b (void) { no_args_no_void(); }\n/* get_no_args_no_void_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_simple_func_1f43e6c47e963043 (void)) (void) { return &no_args_no_void; } \nsigned int hs_bindgen_test_simple_func_a2c97786cd1ecc82 (char arg1, double arg2) { return fun(arg1, arg2); }\n/* get_fun_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_simple_func_1400673a07a5e708 (void)) (char arg1, double arg2) { return &fun; } \n")

{-| __from C:__ @erf@ -}
foreign import ccall safe "hs_bindgen_test_simple_func_3919a2f9a4498aaa" erf
  :: FC.CDouble
     {- ^ __from C:__ @arg@ -}
  -> IO FC.CDouble

foreign import ccall unsafe "hs_bindgen_test_simple_func_e3d5d1926d499ff8" hs_bindgen_test_simple_func_e3d5d1926d499ff8
  :: IO (F.FunPtr (FC.CDouble -> IO FC.CDouble))

{-# NOINLINE erf_ptr #-}

erf_ptr :: F.FunPtr (FC.CDouble -> IO FC.CDouble)
erf_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_simple_func_e3d5d1926d499ff8

{-| __from C:__ @bad_fma@ -}
foreign import ccall safe "hs_bindgen_test_simple_func_6be780963284c499" bad_fma
  :: FC.CDouble
     {- ^ __from C:__ @x@ -}
  -> FC.CDouble
     {- ^ __from C:__ @y@ -}
  -> FC.CDouble
     {- ^ __from C:__ @z@ -}
  -> IO FC.CDouble

foreign import ccall unsafe "hs_bindgen_test_simple_func_6e78b576543cf13e" hs_bindgen_test_simple_func_6e78b576543cf13e
  :: IO (F.FunPtr (FC.CDouble -> FC.CDouble -> FC.CDouble -> IO FC.CDouble))

{-# NOINLINE bad_fma_ptr #-}

bad_fma_ptr :: F.FunPtr (FC.CDouble -> FC.CDouble -> FC.CDouble -> IO FC.CDouble)
bad_fma_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_simple_func_6e78b576543cf13e

{-| __from C:__ @no_args@ -}
foreign import ccall safe "hs_bindgen_test_simple_func_63e35f316cc0a04e" no_args
  :: IO ()

foreign import ccall unsafe "hs_bindgen_test_simple_func_a41af67e28348e9e" hs_bindgen_test_simple_func_a41af67e28348e9e
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE no_args_ptr #-}

no_args_ptr :: F.FunPtr (IO ())
no_args_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_simple_func_a41af67e28348e9e

{-| __from C:__ @no_args_no_void@ -}
foreign import ccall safe "hs_bindgen_test_simple_func_9d7e58d4e189732b" no_args_no_void
  :: IO ()

foreign import ccall unsafe "hs_bindgen_test_simple_func_1f43e6c47e963043" hs_bindgen_test_simple_func_1f43e6c47e963043
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE no_args_no_void_ptr #-}

no_args_no_void_ptr :: F.FunPtr (IO ())
no_args_no_void_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_simple_func_1f43e6c47e963043

{-| __from C:__ @fun@ -}
foreign import ccall safe "hs_bindgen_test_simple_func_a2c97786cd1ecc82" fun
  :: FC.CChar
     {- ^ __from C:__ @x@ -}
  -> FC.CDouble
     {- ^ __from C:__ @y@ -}
  -> IO FC.CInt

foreign import ccall unsafe "hs_bindgen_test_simple_func_1400673a07a5e708" hs_bindgen_test_simple_func_1400673a07a5e708
  :: IO (F.FunPtr (FC.CChar -> FC.CDouble -> IO FC.CInt))

{-# NOINLINE fun_ptr #-}

fun_ptr :: F.FunPtr (FC.CChar -> FC.CDouble -> IO FC.CInt)
fun_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_simple_func_1400673a07a5e708
