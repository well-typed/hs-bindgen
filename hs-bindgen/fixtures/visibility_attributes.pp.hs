{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <visibility_attributes.h>\nvoid test_internal_f0 (void) { f0(); }\nvoid test_internal_f1 (void) { f1(); }\nvoid test_internal_f2 (void) { f2(); }\nvoid test_internal_f3 (void) { f3(); }\nvoid test_internal_f4 (void) { f4(); }\nvoid test_internal_f5 (void) { f5(); }\nvoid test_internal_f6 (void) { f6(); }\nvoid test_internal_f7 (void) { f7(); }\nvoid test_internal_f8 (void) { f8(); }\nvoid test_internal_f9 (void) { f9(); }\nvoid test_internal_f10 (void) { f10(); }\nvoid test_internal_f11 (void) { f11(); }\nvoid test_internal_f12 (void) { f12(); }\nvoid test_internal_f13 (void) { f13(); }\nvoid test_internal_f14 (void) { f14(); }\nvoid test_internal_f15 (void) { f15(); }\nvoid test_internal_f16 (void) { f16(); }\nvoid test_internal_f17 (void) { f17(); }\nvoid test_internal_f18 (void) { f18(); }\nvoid test_internal_f19 (void) { f19(); }\nvoid test_internal_f20 (void) { f20(); }\nvoid test_internal_f21 (void) { f21(); }\nvoid test_internal_f22 (void) { f22(); }\nvoid test_internal_f23 (void) { f23(); }\nvoid test_internal_f24 (void) { f24(); }\nvoid test_internal_f25 (void) { f25(); }\nvoid test_internal_f26 (void) { f26(); }\nvoid test_internal_f27 (void) { f27(); }\nvoid test_internal_f28 (void) { f28(); }\nvoid test_internal_f29 (void) { f29(); }\n__attribute__ ((const)) signed int *get_i0_ptr (void) { return &i0; } \n__attribute__ ((const)) signed int *get_i1_ptr (void) { return &i1; } \n__attribute__ ((const)) signed int *get_i2_ptr (void) { return &i2; } \n__attribute__ ((const)) signed int *get_i3_ptr (void) { return &i3; } \n__attribute__ ((const)) signed int *get_i4_ptr (void) { return &i4; } \n__attribute__ ((const)) signed int *get_i5_ptr (void) { return &i5; } \n__attribute__ ((const)) signed int *get_i6_ptr (void) { return &i6; } \n__attribute__ ((const)) signed int *get_i7_ptr (void) { return &i7; } \n__attribute__ ((const)) signed int *get_i8_ptr (void) { return &i8; } \n__attribute__ ((const)) signed int *get_i9_ptr (void) { return &i9; } \n__attribute__ ((const)) signed int *get_i10_ptr (void) { return &i10; } \n__attribute__ ((const)) signed int *get_i11_ptr (void) { return &i11; } \n__attribute__ ((const)) signed int *get_i12_ptr (void) { return &i12; } \n__attribute__ ((const)) signed int *get_i13_ptr (void) { return &i13; } \n__attribute__ ((const)) signed int *get_i14_ptr (void) { return &i14; } \n__attribute__ ((const)) signed int *get_i15_ptr (void) { return &i15; } \n__attribute__ ((const)) signed int *get_i16_ptr (void) { return &i16; } \n__attribute__ ((const)) signed int *get_i17_ptr (void) { return &i17; } \n__attribute__ ((const)) signed int *get_i18_ptr (void) { return &i18; } \n__attribute__ ((const)) signed int *get_i19_ptr (void) { return &i19; } \n")

foreign import ccall safe "test_internal_f0" f0 :: IO ()

foreign import ccall safe "test_internal_f1" f1 :: IO ()

foreign import ccall safe "test_internal_f2" f2 :: IO ()

foreign import ccall safe "test_internal_f3" f3 :: IO ()

foreign import ccall safe "test_internal_f4" f4 :: IO ()

foreign import ccall safe "test_internal_f5" f5 :: IO ()

foreign import ccall safe "test_internal_f6" f6 :: IO ()

foreign import ccall safe "test_internal_f7" f7 :: IO ()

foreign import ccall safe "test_internal_f8" f8 :: IO ()

foreign import ccall safe "test_internal_f9" f9 :: IO ()

foreign import ccall safe "test_internal_f10" f10 :: IO ()

foreign import ccall safe "test_internal_f11" f11 :: IO ()

foreign import ccall safe "test_internal_f12" f12 :: IO ()

foreign import ccall safe "test_internal_f13" f13 :: IO ()

foreign import ccall safe "test_internal_f14" f14 :: IO ()

foreign import ccall safe "test_internal_f15" f15 :: IO ()

foreign import ccall safe "test_internal_f16" f16 :: IO ()

foreign import ccall safe "test_internal_f17" f17 :: IO ()

foreign import ccall safe "test_internal_f18" f18 :: IO ()

foreign import ccall safe "test_internal_f19" f19 :: IO ()

foreign import ccall safe "test_internal_f20" f20 :: IO ()

foreign import ccall safe "test_internal_f21" f21 :: IO ()

foreign import ccall safe "test_internal_f22" f22 :: IO ()

foreign import ccall safe "test_internal_f23" f23 :: IO ()

foreign import ccall safe "test_internal_f24" f24 :: IO ()

foreign import ccall safe "test_internal_f25" f25 :: IO ()

foreign import ccall safe "test_internal_f26" f26 :: IO ()

foreign import ccall safe "test_internal_f27" f27 :: IO ()

foreign import ccall safe "test_internal_f28" f28 :: IO ()

foreign import ccall safe "test_internal_f29" f29 :: IO ()

foreign import ccall safe "get_i0_ptr" i0_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i1_ptr" i1_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i2_ptr" i2_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i3_ptr" i3_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i4_ptr" i4_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i5_ptr" i5_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i6_ptr" i6_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i7_ptr" i7_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i8_ptr" i8_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i9_ptr" i9_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i10_ptr" i10_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i11_ptr" i11_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i12_ptr" i12_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i13_ptr" i13_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i14_ptr" i14_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i15_ptr" i15_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i16_ptr" i16_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i17_ptr" i17_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i18_ptr" i18_ptr :: F.Ptr FC.CInt

foreign import ccall safe "get_i19_ptr" i19_ptr :: F.Ptr FC.CInt
