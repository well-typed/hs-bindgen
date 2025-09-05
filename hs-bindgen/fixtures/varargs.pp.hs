{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as F
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <varargs.h>\nvoid hs_bindgen_test_varargs_0fd77c5efa209398 (void) { h(); }\n/* get_h_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_varargs_0a93e926c5626347 (void)) (void) { return &h; } \n")

{-| __from C:__ @h@ -}
foreign import ccall safe "hs_bindgen_test_varargs_0fd77c5efa209398" h
  :: IO ()

foreign import ccall unsafe "hs_bindgen_test_varargs_0a93e926c5626347" hs_bindgen_test_varargs_0a93e926c5626347
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE h_ptr #-}

h_ptr :: F.FunPtr (IO ())
h_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_varargs_0a93e926c5626347
