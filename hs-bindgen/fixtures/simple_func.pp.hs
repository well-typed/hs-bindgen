{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign.C as FC
import Prelude (IO)

-- double erf (double arg1)

foreign import capi safe "simple_func.h erf" erf :: FC.CDouble -> IO FC.CDouble

-- double bad_fma (double arg1, double arg2, double arg3)

foreign import capi safe "simple_func.h bad_fma" bad_fma :: FC.CDouble -> FC.CDouble -> FC.CDouble -> IO FC.CDouble

-- void no_args (void)

foreign import capi safe "simple_func.h no_args" no_args :: IO ()

-- void no_args_no_void (void)

foreign import capi safe "simple_func.h no_args_no_void" no_args_no_void :: IO ()

-- signed int fun (char arg1, double arg2)

foreign import capi safe "simple_func.h fun" fun :: FC.CChar -> FC.CDouble -> IO FC.CInt
