{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <visibility_attributes.h>\nvoid hs_bindgen_test_visibility_attributes_beeef9b61fad5466 (void) { f0(); }\nvoid hs_bindgen_test_visibility_attributes_8b60d38de80093fa (void) { f1(); }\nvoid hs_bindgen_test_visibility_attributes_4a86b0420a250963 (void) { f2(); }\nvoid hs_bindgen_test_visibility_attributes_1b95ce9d55223970 (void) { f3(); }\nvoid hs_bindgen_test_visibility_attributes_cdd7cda26e655f5e (void) { f4(); }\nvoid hs_bindgen_test_visibility_attributes_3bc585c51fec9721 (void) { f5(); }\nvoid hs_bindgen_test_visibility_attributes_ba28bdf96df05f32 (void) { f6(); }\nvoid hs_bindgen_test_visibility_attributes_2fc1219d73636d66 (void) { f7(); }\nvoid hs_bindgen_test_visibility_attributes_7a7ce833f71ec006 (void) { f8(); }\nvoid hs_bindgen_test_visibility_attributes_3a6334fe1abf229c (void) { f9(); }\nvoid hs_bindgen_test_visibility_attributes_72e7a2a1e9f25000 (void) { f10(); }\nvoid hs_bindgen_test_visibility_attributes_052232f3a6ecd42e (void) { f11(); }\nvoid hs_bindgen_test_visibility_attributes_792700b287b37bc9 (void) { f12(); }\nvoid hs_bindgen_test_visibility_attributes_241ea65011175c11 (void) { f13(); }\nvoid hs_bindgen_test_visibility_attributes_2c775e867f8ea914 (void) { f14(); }\nvoid hs_bindgen_test_visibility_attributes_c9cc679279218ae9 (void) { f15(); }\nvoid hs_bindgen_test_visibility_attributes_ea5c7e17063c74da (void) { f16(); }\nvoid hs_bindgen_test_visibility_attributes_e72feade6b631a5f (void) { f17(); }\nvoid hs_bindgen_test_visibility_attributes_171950f0c44f6f22 (void) { f18(); }\nvoid hs_bindgen_test_visibility_attributes_5414de7b815a8658 (void) { f19(); }\nvoid hs_bindgen_test_visibility_attributes_74ab404c9f6d501e (void) { f20(); }\nvoid hs_bindgen_test_visibility_attributes_32c47e20171aaeee (void) { f21(); }\nvoid hs_bindgen_test_visibility_attributes_ce04bbbd1ae9ec66 (void) { f22(); }\nvoid hs_bindgen_test_visibility_attributes_005b979e638f474b (void) { f23(); }\nvoid hs_bindgen_test_visibility_attributes_e0225d72ee13bade (void) { f24(); }\nvoid hs_bindgen_test_visibility_attributes_c5c883717d0048e8 (void) { f25(); }\nvoid hs_bindgen_test_visibility_attributes_d54b2fb5a87420f7 (void) { f26(); }\nvoid hs_bindgen_test_visibility_attributes_d40140ee49300df1 (void) { f27(); }\nvoid hs_bindgen_test_visibility_attributes_358fecb39951c1ed (void) { f28(); }\nvoid hs_bindgen_test_visibility_attributes_8255bfc1a96b601c (void) { f29(); }\n/* get_i0_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_724fd59489c94c9f (void) { return &i0; } \n/* get_i1_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_736e69defba46ab4 (void) { return &i1; } \n/* get_i2_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_210c547ae5abcc02 (void) { return &i2; } \n/* get_i3_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_d6bb66d7f7107274 (void) { return &i3; } \n/* get_i4_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_8c4485c2eb6e1434 (void) { return &i4; } \n/* get_i5_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_6ff6b816265f91d3 (void) { return &i5; } \n/* get_i6_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_3bd2208d8e850002 (void) { return &i6; } \n/* get_i7_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_a3aa6eb624f2c014 (void) { return &i7; } \n/* get_i8_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_696700c5194eb184 (void) { return &i8; } \n/* get_i9_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_27bb5845debfdd10 (void) { return &i9; } \n/* get_i10_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_254dda0b2c3c245d (void) { return &i10; } \n/* get_i11_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_5ca63f16dc0b48e7 (void) { return &i11; } \n/* get_i12_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_75789ceaef5e5feb (void) { return &i12; } \n/* get_i13_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_6e3778cc97c78a2e (void) { return &i13; } \n/* get_i14_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_9ec03118dd66d7c5 (void) { return &i14; } \n/* get_i15_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_4b5a349cc99cdac9 (void) { return &i15; } \n/* get_i16_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_56cec68bd1e37a44 (void) { return &i16; } \n/* get_i17_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_e60a43107858a2bc (void) { return &i17; } \n/* get_i18_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_86247c32f4f34e6f (void) { return &i18; } \n/* get_i19_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_3d3a0ab3e093d4b1 (void) { return &i19; } \n")

foreign import ccall safe "hs_bindgen_test_visibility_attributes_beeef9b61fad5466" f0
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_8b60d38de80093fa" f1
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_4a86b0420a250963" f2
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_1b95ce9d55223970" f3
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_cdd7cda26e655f5e" f4
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_3bc585c51fec9721" f5
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_ba28bdf96df05f32" f6
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_2fc1219d73636d66" f7
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_7a7ce833f71ec006" f8
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_3a6334fe1abf229c" f9
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_72e7a2a1e9f25000" f10
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_052232f3a6ecd42e" f11
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_792700b287b37bc9" f12
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_241ea65011175c11" f13
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_2c775e867f8ea914" f14
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_c9cc679279218ae9" f15
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_ea5c7e17063c74da" f16
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_e72feade6b631a5f" f17
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_171950f0c44f6f22" f18
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_5414de7b815a8658" f19
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_74ab404c9f6d501e" f20
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_32c47e20171aaeee" f21
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_ce04bbbd1ae9ec66" f22
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_005b979e638f474b" f23
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_e0225d72ee13bade" f24
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_c5c883717d0048e8" f25
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_d54b2fb5a87420f7" f26
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_d40140ee49300df1" f27
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_358fecb39951c1ed" f28
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_8255bfc1a96b601c" f29
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_724fd59489c94c9f" i0_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_visibility_attributes_736e69defba46ab4" i1_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_visibility_attributes_210c547ae5abcc02" i2_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_visibility_attributes_d6bb66d7f7107274" i3_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_visibility_attributes_8c4485c2eb6e1434" i4_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_visibility_attributes_6ff6b816265f91d3" i5_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_visibility_attributes_3bd2208d8e850002" i6_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_visibility_attributes_a3aa6eb624f2c014" i7_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_visibility_attributes_696700c5194eb184" i8_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_visibility_attributes_27bb5845debfdd10" i9_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_visibility_attributes_254dda0b2c3c245d" i10_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_visibility_attributes_5ca63f16dc0b48e7" i11_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_visibility_attributes_75789ceaef5e5feb" i12_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_visibility_attributes_6e3778cc97c78a2e" i13_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_visibility_attributes_9ec03118dd66d7c5" i14_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_visibility_attributes_4b5a349cc99cdac9" i15_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_visibility_attributes_56cec68bd1e37a44" i16_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_visibility_attributes_e60a43107858a2bc" i17_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_visibility_attributes_86247c32f4f34e6f" i18_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_visibility_attributes_3d3a0ab3e093d4b1" i19_ptr
  :: F.Ptr FC.CInt
