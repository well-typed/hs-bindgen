{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include \"visibility_attributes.h\"\nvoid testmodule_f0 (void) { f0(); }\nvoid testmodule_f1 (void) { f1(); }\nvoid testmodule_f5 (void) { f5(); }\nvoid testmodule_f6 (void) { f6(); }\n__attribute__ ((const)) signed int *get_i0_ptr (void) { return &i0; } \n__attribute__ ((const)) signed int *get_i1_ptr (void) { return &i1; } \n__attribute__ ((const)) signed int *get_i2_ptr (void) { return &i2; } \n__attribute__ ((const)) signed int *get_i3_ptr (void) { return &i3; } \n__attribute__ ((const)) signed int *get_i4_ptr (void) { return &i4; } \n__attribute__ ((const)) signed int *get_i5_ptr (void) { return &i5; } \n__attribute__ ((const)) signed int *get_i6_ptr (void) { return &i6; } \n__attribute__ ((const)) signed int *get_i7_ptr (void) { return &i7; } \n__attribute__ ((const)) signed int *get_i8_ptr (void) { return &i8; } \n__attribute__ ((const)) signed int *get_i9_ptr (void) { return &i9; } \n")

foreign import ccall safe "testmodule_f0" f0 :: IO ()

foreign import ccall safe "testmodule_f1" f1 :: IO ()

foreign import ccall safe "testmodule_f5" f5 :: IO ()

foreign import ccall safe "testmodule_f6" f6 :: IO ()

foreign import ccall safe "get_i0_ptr" i0 :: F.Ptr FC.CInt

foreign import ccall safe "get_i1_ptr" i1 :: F.Ptr FC.CInt

foreign import ccall safe "get_i2_ptr" i2 :: F.Ptr FC.CInt

foreign import ccall safe "get_i3_ptr" i3 :: F.Ptr FC.CInt

foreign import ccall safe "get_i4_ptr" i4 :: F.Ptr FC.CInt

foreign import ccall safe "get_i5_ptr" i5 :: F.Ptr FC.CInt

foreign import ccall safe "get_i6_ptr" i6 :: F.Ptr FC.CInt

foreign import ccall safe "get_i7_ptr" i7 :: F.Ptr FC.CInt

foreign import ccall safe "get_i8_ptr" i8 :: F.Ptr FC.CInt

foreign import ccall safe "get_i9_ptr" i9 :: F.Ptr FC.CInt
