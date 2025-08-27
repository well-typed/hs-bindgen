{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <simple_func.h>\ndouble hs_bindgen_test_simple_func_3919a2f9a4498aaa (double arg1) { return erf(arg1); }\ndouble hs_bindgen_test_simple_func_6be780963284c499 (double arg1, double arg2, double arg3) { return bad_fma(arg1, arg2, arg3); }\nvoid hs_bindgen_test_simple_func_63e35f316cc0a04e (void) { no_args(); }\nvoid hs_bindgen_test_simple_func_9d7e58d4e189732b (void) { no_args_no_void(); }\nsigned int hs_bindgen_test_simple_func_a2c97786cd1ecc82 (char arg1, double arg2) { return fun(arg1, arg2); }\n")

{-| __from C:__ @erf@ -}
foreign import ccall safe "hs_bindgen_test_simple_func_3919a2f9a4498aaa" erf
  :: FC.CDouble
     {- ^ __from C:__ @arg@ -}
  -> IO FC.CDouble

{-| __from C:__ @bad_fma@ -}
foreign import ccall safe "hs_bindgen_test_simple_func_6be780963284c499" bad_fma
  :: FC.CDouble
     {- ^ __from C:__ @x@ -}
  -> FC.CDouble
     {- ^ __from C:__ @y@ -}
  -> FC.CDouble
     {- ^ __from C:__ @z@ -}
  -> IO FC.CDouble

{-| __from C:__ @no_args@ -}
foreign import ccall safe "hs_bindgen_test_simple_func_63e35f316cc0a04e" no_args
  :: IO ()

{-| __from C:__ @no_args_no_void@ -}
foreign import ccall safe "hs_bindgen_test_simple_func_9d7e58d4e189732b" no_args_no_void
  :: IO ()

{-| __from C:__ @fun@ -}
foreign import ccall safe "hs_bindgen_test_simple_func_a2c97786cd1ecc82" fun
  :: FC.CChar
     {- ^ __from C:__ @x@ -}
  -> FC.CDouble
     {- ^ __from C:__ @y@ -}
  -> IO FC.CInt
