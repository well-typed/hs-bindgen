{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <varargs.h>\nvoid testmodule_h (void) { h(); }\n")

foreign import ccall safe "testmodule_h" h :: IO ()
