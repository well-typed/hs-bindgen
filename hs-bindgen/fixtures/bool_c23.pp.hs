{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI

$(CAPI.addCSource "#include \"bool_c23.h\"\n__attribute__ ((const)) _Bool *get_b_ptr (void) { return &b; } \n")

foreign import ccall safe "get_b_ptr" b :: F.Ptr FC.CBool
