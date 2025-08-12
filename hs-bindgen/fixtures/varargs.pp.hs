{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <varargs.h>\nvoid hs_bindgen_test_varargs_0fd77c5efa209398 (void) { h(); }\n")

foreign import ccall safe "hs_bindgen_test_varargs_0fd77c5efa209398" h :: IO ()
