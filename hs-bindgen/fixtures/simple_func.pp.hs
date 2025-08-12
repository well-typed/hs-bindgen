{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <simple_func.h>\ndouble hs_bindgen_test_simple_func_3919a2f9a4498aaa (double arg1) { return erf(arg1); }\ndouble hs_bindgen_test_simple_func_6be780963284c499 (double arg1, double arg2, double arg3) { return bad_fma(arg1, arg2, arg3); }\nvoid hs_bindgen_test_simple_func_63e35f316cc0a04e (void) { no_args(); }\nvoid hs_bindgen_test_simple_func_9d7e58d4e189732b (void) { no_args_no_void(); }\nsigned int hs_bindgen_test_simple_func_a2c97786cd1ecc82 (char arg1, double arg2) { return fun(arg1, arg2); }\n")

foreign import ccall safe "hs_bindgen_test_simple_func_3919a2f9a4498aaa" erf :: FC.CDouble -> IO FC.CDouble

foreign import ccall safe "hs_bindgen_test_simple_func_6be780963284c499" bad_fma :: FC.CDouble -> FC.CDouble -> FC.CDouble -> IO FC.CDouble

foreign import ccall safe "hs_bindgen_test_simple_func_63e35f316cc0a04e" no_args :: IO ()

foreign import ccall safe "hs_bindgen_test_simple_func_9d7e58d4e189732b" no_args_no_void :: IO ()

foreign import ccall safe "hs_bindgen_test_simple_func_a2c97786cd1ecc82" fun :: FC.CChar -> FC.CDouble -> IO FC.CInt
