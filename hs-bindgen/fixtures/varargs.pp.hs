{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-foreign-imports #-}

module Example where

import qualified Foreign as F
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <varargs.h>\nvoid hs_bindgen_test_varargs_0fd77c5efa209398 (void) { h(); }\n/* get_h_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_varargs_0a93e926c5626347 (void)) (void) { return &h; } \n")

foreign import ccall safe "hs_bindgen_test_varargs_0fd77c5efa209398" h
  :: IO ()

foreign import ccall safe "hs_bindgen_test_varargs_0a93e926c5626347" h_ptr
  :: F.FunPtr (IO ())
