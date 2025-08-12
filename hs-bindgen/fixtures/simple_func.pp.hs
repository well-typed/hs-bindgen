{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <simple_func.h>\ndouble test_internal_erf (double arg1) { return erf(arg1); }\ndouble test_internal_bad_fma (double arg1, double arg2, double arg3) { return bad_fma(arg1, arg2, arg3); }\nvoid test_internal_no_args (void) { no_args(); }\nvoid test_internal_no_args_no_void (void) { no_args_no_void(); }\nsigned int test_internal_fun (char arg1, double arg2) { return fun(arg1, arg2); }\n")

foreign import ccall safe "test_internal_erf" erf :: FC.CDouble -> IO FC.CDouble

foreign import ccall safe "test_internal_bad_fma" bad_fma :: FC.CDouble -> FC.CDouble -> FC.CDouble -> IO FC.CDouble

foreign import ccall safe "test_internal_no_args" no_args :: IO ()

foreign import ccall safe "test_internal_no_args_no_void" no_args_no_void :: IO ()

foreign import ccall safe "test_internal_fun" fun :: FC.CChar -> FC.CDouble -> IO FC.CInt
