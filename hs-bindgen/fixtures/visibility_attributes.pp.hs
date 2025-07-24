{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include \"visibility_attributes.h\"\nvoid testmodule_f0 (void) { f0(); }\nvoid testmodule_f1 (void) { f1(); }\n__attribute__ ((const)) signed int *get_i0_ptr (void) { return &i0; } \n__attribute__ ((const)) signed int *get_i1_ptr (void) { return &i1; } \n")

foreign import ccall safe "testmodule_f0" f0 :: IO ()

foreign import ccall safe "testmodule_f1" f1 :: IO ()

foreign import ccall safe "get_i0_ptr" i0 :: F.Ptr FC.CInt

foreign import ccall safe "get_i1_ptr" i1 :: F.Ptr FC.CInt
