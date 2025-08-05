{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <varargs.h>\nvoid testmodule_g (void) { g(); }\n")

foreign import ccall safe "testmodule_g" g :: IO ()
