{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI

$(CAPI.addCSource "#include <tentative_definitions.h>\n__attribute__ ((const)) signed int *get_i1_ptr (void) { return &i1; } \n__attribute__ ((const)) signed int *get_i2_ptr (void) { return &i2; } \n__attribute__ ((const)) signed int *get_i3_ptr (void) { return &i3; } \n")

foreign import ccall safe "get_i1_ptr" i1_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i2_ptr" i2_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i3_ptr" i3_ptr :: F.Ptr FC.CInt
