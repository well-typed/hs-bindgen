{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <visibility_attributes.h>\nvoid hs_bindgen_test_visibility_attributes_beeef9b61fad5466 (void) { f0(); }\nvoid hs_bindgen_test_visibility_attributes_8b60d38de80093fa (void) { f1(); }\nvoid hs_bindgen_test_visibility_attributes_4a86b0420a250963 (void) { f2(); }\nvoid hs_bindgen_test_visibility_attributes_1b95ce9d55223970 (void) { f3(); }\nvoid hs_bindgen_test_visibility_attributes_cdd7cda26e655f5e (void) { f4(); }\nvoid hs_bindgen_test_visibility_attributes_3bc585c51fec9721 (void) { f5(); }\nvoid hs_bindgen_test_visibility_attributes_ba28bdf96df05f32 (void) { f6(); }\nvoid hs_bindgen_test_visibility_attributes_2fc1219d73636d66 (void) { f7(); }\nvoid hs_bindgen_test_visibility_attributes_7a7ce833f71ec006 (void) { f8(); }\nvoid hs_bindgen_test_visibility_attributes_3a6334fe1abf229c (void) { f9(); }\nvoid hs_bindgen_test_visibility_attributes_72e7a2a1e9f25000 (void) { f10(); }\nvoid hs_bindgen_test_visibility_attributes_052232f3a6ecd42e (void) { f11(); }\nvoid hs_bindgen_test_visibility_attributes_792700b287b37bc9 (void) { f12(); }\nvoid hs_bindgen_test_visibility_attributes_241ea65011175c11 (void) { f13(); }\nvoid hs_bindgen_test_visibility_attributes_2c775e867f8ea914 (void) { f14(); }\nvoid hs_bindgen_test_visibility_attributes_c9cc679279218ae9 (void) { f15(); }\nvoid hs_bindgen_test_visibility_attributes_ea5c7e17063c74da (void) { f16(); }\nvoid hs_bindgen_test_visibility_attributes_e72feade6b631a5f (void) { f17(); }\nvoid hs_bindgen_test_visibility_attributes_171950f0c44f6f22 (void) { f18(); }\nvoid hs_bindgen_test_visibility_attributes_5414de7b815a8658 (void) { f19(); }\nvoid hs_bindgen_test_visibility_attributes_74ab404c9f6d501e (void) { f20(); }\nvoid hs_bindgen_test_visibility_attributes_32c47e20171aaeee (void) { f21(); }\nvoid hs_bindgen_test_visibility_attributes_ce04bbbd1ae9ec66 (void) { f22(); }\nvoid hs_bindgen_test_visibility_attributes_005b979e638f474b (void) { f23(); }\nvoid hs_bindgen_test_visibility_attributes_e0225d72ee13bade (void) { f24(); }\nvoid hs_bindgen_test_visibility_attributes_c5c883717d0048e8 (void) { f25(); }\nvoid hs_bindgen_test_visibility_attributes_d54b2fb5a87420f7 (void) { f26(); }\nvoid hs_bindgen_test_visibility_attributes_d40140ee49300df1 (void) { f27(); }\nvoid hs_bindgen_test_visibility_attributes_358fecb39951c1ed (void) { f28(); }\nvoid hs_bindgen_test_visibility_attributes_8255bfc1a96b601c (void) { f29(); }\n__attribute__ ((const)) signed int *get_i0_ptr (void) { return &i0; } \n__attribute__ ((const)) signed int *get_i1_ptr (void) { return &i1; } \n__attribute__ ((const)) signed int *get_i2_ptr (void) { return &i2; } \n__attribute__ ((const)) signed int *get_i3_ptr (void) { return &i3; } \n__attribute__ ((const)) signed int *get_i4_ptr (void) { return &i4; } \n__attribute__ ((const)) signed int *get_i5_ptr (void) { return &i5; } \n__attribute__ ((const)) signed int *get_i6_ptr (void) { return &i6; } \n__attribute__ ((const)) signed int *get_i7_ptr (void) { return &i7; } \n__attribute__ ((const)) signed int *get_i8_ptr (void) { return &i8; } \n__attribute__ ((const)) signed int *get_i9_ptr (void) { return &i9; } \n__attribute__ ((const)) signed int *get_i10_ptr (void) { return &i10; } \n__attribute__ ((const)) signed int *get_i11_ptr (void) { return &i11; } \n__attribute__ ((const)) signed int *get_i12_ptr (void) { return &i12; } \n__attribute__ ((const)) signed int *get_i13_ptr (void) { return &i13; } \n__attribute__ ((const)) signed int *get_i14_ptr (void) { return &i14; } \n__attribute__ ((const)) signed int *get_i15_ptr (void) { return &i15; } \n__attribute__ ((const)) signed int *get_i16_ptr (void) { return &i16; } \n__attribute__ ((const)) signed int *get_i17_ptr (void) { return &i17; } \n__attribute__ ((const)) signed int *get_i18_ptr (void) { return &i18; } \n__attribute__ ((const)) signed int *get_i19_ptr (void) { return &i19; } \n")

foreign import ccall safe "hs_bindgen_test_visibility_attributes_beeef9b61fad5466" f0 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_8b60d38de80093fa" f1 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_4a86b0420a250963" f2 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_1b95ce9d55223970" f3 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_cdd7cda26e655f5e" f4 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_3bc585c51fec9721" f5 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_ba28bdf96df05f32" f6 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_2fc1219d73636d66" f7 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_7a7ce833f71ec006" f8 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_3a6334fe1abf229c" f9 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_72e7a2a1e9f25000" f10 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_052232f3a6ecd42e" f11 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_792700b287b37bc9" f12 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_241ea65011175c11" f13 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_2c775e867f8ea914" f14 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_c9cc679279218ae9" f15 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_ea5c7e17063c74da" f16 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_e72feade6b631a5f" f17 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_171950f0c44f6f22" f18 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_5414de7b815a8658" f19 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_74ab404c9f6d501e" f20 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_32c47e20171aaeee" f21 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_ce04bbbd1ae9ec66" f22 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_005b979e638f474b" f23 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_e0225d72ee13bade" f24 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_c5c883717d0048e8" f25 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_d54b2fb5a87420f7" f26 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_d40140ee49300df1" f27 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_358fecb39951c1ed" f28 :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_8255bfc1a96b601c" f29 :: IO ()

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
