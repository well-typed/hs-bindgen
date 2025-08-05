{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include \"visibility_attributes.h\"\nvoid testmodule_f0 (void) { f0(); }\nvoid testmodule_f1 (void) { f1(); }\nvoid testmodule_f5 (void) { f5(); }\nvoid testmodule_f6 (void) { f6(); }\nvoid testmodule_f10 (void) { f10(); }\nvoid testmodule_f11 (void) { f11(); }\nvoid testmodule_f15 (void) { f15(); }\nvoid testmodule_f16 (void) { f16(); }\nvoid testmodule_f20 (void) { f20(); }\nvoid testmodule_f21 (void) { f21(); }\nvoid testmodule_f22 (void) { f22(); }\nvoid testmodule_f23 (void) { f23(); }\nvoid testmodule_f24 (void) { f24(); }\nvoid testmodule_f25 (void) { f25(); }\nvoid testmodule_f26 (void) { f26(); }\nvoid testmodule_f27 (void) { f27(); }\nvoid testmodule_f28 (void) { f28(); }\nvoid testmodule_f29 (void) { f29(); }\n__attribute__ ((const)) signed int *get_i0_ptr (void) { return &i0; } \n__attribute__ ((const)) signed int *get_i1_ptr (void) { return &i1; } \n__attribute__ ((const)) signed int *get_i5_ptr (void) { return &i5; } \n__attribute__ ((const)) signed int *get_i6_ptr (void) { return &i6; } \n__attribute__ ((const)) signed int *get_i10_ptr (void) { return &i10; } \n__attribute__ ((const)) signed int *get_i11_ptr (void) { return &i11; } \n__attribute__ ((const)) signed int *get_i15_ptr (void) { return &i15; } \n__attribute__ ((const)) signed int *get_i16_ptr (void) { return &i16; } \n")

foreign import ccall safe "testmodule_f0" f0 :: IO ()

foreign import ccall safe "testmodule_f1" f1 :: IO ()

foreign import ccall safe "testmodule_f5" f5 :: IO ()

foreign import ccall safe "testmodule_f6" f6 :: IO ()

foreign import ccall safe "testmodule_f10" f10 :: IO ()

foreign import ccall safe "testmodule_f11" f11 :: IO ()

foreign import ccall safe "testmodule_f15" f15 :: IO ()

foreign import ccall safe "testmodule_f16" f16 :: IO ()

foreign import ccall safe "testmodule_f20" f20 :: IO ()

foreign import ccall safe "testmodule_f21" f21 :: IO ()

foreign import ccall safe "testmodule_f22" f22 :: IO ()

foreign import ccall safe "testmodule_f23" f23 :: IO ()

foreign import ccall safe "testmodule_f24" f24 :: IO ()

foreign import ccall safe "testmodule_f25" f25 :: IO ()

foreign import ccall safe "testmodule_f26" f26 :: IO ()

foreign import ccall safe "testmodule_f27" f27 :: IO ()

foreign import ccall safe "testmodule_f28" f28 :: IO ()

foreign import ccall safe "testmodule_f29" f29 :: IO ()

foreign import ccall safe "get_i0_ptr" i0 :: F.Ptr FC.CInt

foreign import ccall safe "get_i1_ptr" i1 :: F.Ptr FC.CInt

foreign import ccall safe "get_i5_ptr" i5 :: F.Ptr FC.CInt

foreign import ccall safe "get_i6_ptr" i6 :: F.Ptr FC.CInt

foreign import ccall safe "get_i10_ptr" i10 :: F.Ptr FC.CInt

foreign import ccall safe "get_i11_ptr" i11 :: F.Ptr FC.CInt

foreign import ccall safe "get_i15_ptr" i15 :: F.Ptr FC.CInt

foreign import ccall safe "get_i16_ptr" i16 :: F.Ptr FC.CInt
