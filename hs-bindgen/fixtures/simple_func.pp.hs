{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign.C as FC
import Prelude (IO)

-- #include "simple_func.h"
-- double testmodule_erf (double arg1);
-- double testmodule_bad_fma (double arg1, double arg2, double arg3);
-- void testmodule_no_args (void);
-- void testmodule_no_args_no_void (void);
-- signed int testmodule_fun (char arg1, double arg2);

foreign import capi safe "simple_func.h erf" erf :: FC.CDouble -> IO FC.CDouble

foreign import capi safe "simple_func.h bad_fma" bad_fma :: FC.CDouble -> FC.CDouble -> FC.CDouble -> IO FC.CDouble

foreign import capi safe "simple_func.h no_args" no_args :: IO ()

foreign import capi safe "simple_func.h no_args_no_void" no_args_no_void :: IO ()

foreign import capi safe "simple_func.h fun" fun :: FC.CChar -> FC.CDouble -> IO FC.CInt
