{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign.C as FC
import Prelude (IO)

foreign import capi safe "simple_func.h erf" erf :: FC.CDouble -> IO FC.CDouble

foreign import capi safe "simple_func.h bad_fma" bad_fma :: FC.CDouble -> FC.CDouble -> FC.CDouble -> IO FC.CDouble

foreign import capi safe "simple_func.h no_args" no_args :: IO ()
