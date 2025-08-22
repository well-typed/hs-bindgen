{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-foreign-imports #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <visibility_attributes.h>\nvoid hs_bindgen_test_visibility_attributes_beeef9b61fad5466 (void) { f0(); }\n/* get_f0_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_062fb354632b879a (void)) (void) { return &f0; } \nvoid hs_bindgen_test_visibility_attributes_8b60d38de80093fa (void) { f1(); }\n/* get_f1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_a1b79fe9af8e18b8 (void)) (void) { return &f1; } \nvoid hs_bindgen_test_visibility_attributes_4a86b0420a250963 (void) { f2(); }\n/* get_f2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_74cfd16f2b7e27ba (void)) (void) { return &f2; } \nvoid hs_bindgen_test_visibility_attributes_1b95ce9d55223970 (void) { f3(); }\n/* get_f3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_08809dca6bfda237 (void)) (void) { return &f3; } \nvoid hs_bindgen_test_visibility_attributes_cdd7cda26e655f5e (void) { f4(); }\n/* get_f4_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_bd64bfbfcdfa6624 (void)) (void) { return &f4; } \nvoid hs_bindgen_test_visibility_attributes_3bc585c51fec9721 (void) { f5(); }\n/* get_f5_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_0bfaa7955f34f9bf (void)) (void) { return &f5; } \nvoid hs_bindgen_test_visibility_attributes_ba28bdf96df05f32 (void) { f6(); }\n/* get_f6_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_4733471689f1bb70 (void)) (void) { return &f6; } \nvoid hs_bindgen_test_visibility_attributes_2fc1219d73636d66 (void) { f7(); }\n/* get_f7_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_9c959c1281029571 (void)) (void) { return &f7; } \nvoid hs_bindgen_test_visibility_attributes_7a7ce833f71ec006 (void) { f8(); }\n/* get_f8_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_e12a5a5d01dd5a47 (void)) (void) { return &f8; } \nvoid hs_bindgen_test_visibility_attributes_3a6334fe1abf229c (void) { f9(); }\n/* get_f9_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_3d87310c41d7398e (void)) (void) { return &f9; } \nvoid hs_bindgen_test_visibility_attributes_72e7a2a1e9f25000 (void) { f10(); }\n/* get_f10_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_1143dfcfb90def8a (void)) (void) { return &f10; } \nvoid hs_bindgen_test_visibility_attributes_052232f3a6ecd42e (void) { f11(); }\n/* get_f11_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_5d68570d79a6b4fb (void)) (void) { return &f11; } \nvoid hs_bindgen_test_visibility_attributes_792700b287b37bc9 (void) { f12(); }\n/* get_f12_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_46c1e11ee341b116 (void)) (void) { return &f12; } \nvoid hs_bindgen_test_visibility_attributes_241ea65011175c11 (void) { f13(); }\n/* get_f13_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_bb797b66bcb7e8e2 (void)) (void) { return &f13; } \nvoid hs_bindgen_test_visibility_attributes_2c775e867f8ea914 (void) { f14(); }\n/* get_f14_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_8969f1de409a19ee (void)) (void) { return &f14; } \nvoid hs_bindgen_test_visibility_attributes_c9cc679279218ae9 (void) { f15(); }\n/* get_f15_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_454ccc61fb5125af (void)) (void) { return &f15; } \nvoid hs_bindgen_test_visibility_attributes_ea5c7e17063c74da (void) { f16(); }\n/* get_f16_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_023fcc82fef21765 (void)) (void) { return &f16; } \nvoid hs_bindgen_test_visibility_attributes_e72feade6b631a5f (void) { f17(); }\n/* get_f17_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_7036d26aaf2d397f (void)) (void) { return &f17; } \nvoid hs_bindgen_test_visibility_attributes_171950f0c44f6f22 (void) { f18(); }\n/* get_f18_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_e6705f5e2f6359d8 (void)) (void) { return &f18; } \nvoid hs_bindgen_test_visibility_attributes_5414de7b815a8658 (void) { f19(); }\n/* get_f19_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_5b804a3dc8077495 (void)) (void) { return &f19; } \nvoid hs_bindgen_test_visibility_attributes_74ab404c9f6d501e (void) { f20(); }\n/* get_f20_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_763bb37c71b6d9a4 (void)) (void) { return &f20; } \nvoid hs_bindgen_test_visibility_attributes_32c47e20171aaeee (void) { f21(); }\n/* get_f21_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_8661e763dca1c8a0 (void)) (void) { return &f21; } \nvoid hs_bindgen_test_visibility_attributes_ce04bbbd1ae9ec66 (void) { f22(); }\n/* get_f22_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_b39debd724f07945 (void)) (void) { return &f22; } \nvoid hs_bindgen_test_visibility_attributes_005b979e638f474b (void) { f23(); }\n/* get_f23_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_5d6336e1a6bfd80a (void)) (void) { return &f23; } \nvoid hs_bindgen_test_visibility_attributes_e0225d72ee13bade (void) { f24(); }\n/* get_f24_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_cde5aeae243421cd (void)) (void) { return &f24; } \nvoid hs_bindgen_test_visibility_attributes_c5c883717d0048e8 (void) { f25(); }\n/* get_f25_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_c07253e5bc2ad1a2 (void)) (void) { return &f25; } \nvoid hs_bindgen_test_visibility_attributes_d54b2fb5a87420f7 (void) { f26(); }\n/* get_f26_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_e86ffdca3bb71235 (void)) (void) { return &f26; } \nvoid hs_bindgen_test_visibility_attributes_d40140ee49300df1 (void) { f27(); }\n/* get_f27_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_7668e339533a6fb4 (void)) (void) { return &f27; } \nvoid hs_bindgen_test_visibility_attributes_358fecb39951c1ed (void) { f28(); }\n/* get_f28_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_bf3df5c08da1121b (void)) (void) { return &f28; } \nvoid hs_bindgen_test_visibility_attributes_8255bfc1a96b601c (void) { f29(); }\n/* get_f29_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_fb811ac22d16d2c6 (void)) (void) { return &f29; } \n/* get_i0_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_724fd59489c94c9f (void) { return &i0; } \n/* get_i1_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_736e69defba46ab4 (void) { return &i1; } \n/* get_i2_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_210c547ae5abcc02 (void) { return &i2; } \n/* get_i3_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_d6bb66d7f7107274 (void) { return &i3; } \n/* get_i4_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_8c4485c2eb6e1434 (void) { return &i4; } \n/* get_i5_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_6ff6b816265f91d3 (void) { return &i5; } \n/* get_i6_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_3bd2208d8e850002 (void) { return &i6; } \n/* get_i7_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_a3aa6eb624f2c014 (void) { return &i7; } \n/* get_i8_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_696700c5194eb184 (void) { return &i8; } \n/* get_i9_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_27bb5845debfdd10 (void) { return &i9; } \n/* get_i10_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_254dda0b2c3c245d (void) { return &i10; } \n/* get_i11_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_5ca63f16dc0b48e7 (void) { return &i11; } \n/* get_i12_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_75789ceaef5e5feb (void) { return &i12; } \n/* get_i13_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_6e3778cc97c78a2e (void) { return &i13; } \n/* get_i14_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_9ec03118dd66d7c5 (void) { return &i14; } \n/* get_i15_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_4b5a349cc99cdac9 (void) { return &i15; } \n/* get_i16_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_56cec68bd1e37a44 (void) { return &i16; } \n/* get_i17_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_e60a43107858a2bc (void) { return &i17; } \n/* get_i18_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_86247c32f4f34e6f (void) { return &i18; } \n/* get_i19_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_3d3a0ab3e093d4b1 (void) { return &i19; } \n")

foreign import ccall safe "hs_bindgen_test_visibility_attributes_beeef9b61fad5466" f0
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_062fb354632b879a" f0_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_8b60d38de80093fa" f1
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_a1b79fe9af8e18b8" f1_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_4a86b0420a250963" f2
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_74cfd16f2b7e27ba" f2_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_1b95ce9d55223970" f3
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_08809dca6bfda237" f3_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_cdd7cda26e655f5e" f4
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_bd64bfbfcdfa6624" f4_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_3bc585c51fec9721" f5
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_0bfaa7955f34f9bf" f5_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_ba28bdf96df05f32" f6
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_4733471689f1bb70" f6_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_2fc1219d73636d66" f7
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_9c959c1281029571" f7_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_7a7ce833f71ec006" f8
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_e12a5a5d01dd5a47" f8_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_3a6334fe1abf229c" f9
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_3d87310c41d7398e" f9_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_72e7a2a1e9f25000" f10
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_1143dfcfb90def8a" f10_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_052232f3a6ecd42e" f11
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_5d68570d79a6b4fb" f11_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_792700b287b37bc9" f12
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_46c1e11ee341b116" f12_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_241ea65011175c11" f13
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_bb797b66bcb7e8e2" f13_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_2c775e867f8ea914" f14
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_8969f1de409a19ee" f14_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_c9cc679279218ae9" f15
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_454ccc61fb5125af" f15_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_ea5c7e17063c74da" f16
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_023fcc82fef21765" f16_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_e72feade6b631a5f" f17
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_7036d26aaf2d397f" f17_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_171950f0c44f6f22" f18
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_e6705f5e2f6359d8" f18_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_5414de7b815a8658" f19
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_5b804a3dc8077495" f19_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_74ab404c9f6d501e" f20
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_763bb37c71b6d9a4" f20_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_32c47e20171aaeee" f21
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_8661e763dca1c8a0" f21_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_ce04bbbd1ae9ec66" f22
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_b39debd724f07945" f22_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_005b979e638f474b" f23
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_5d6336e1a6bfd80a" f23_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_e0225d72ee13bade" f24
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_cde5aeae243421cd" f24_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_c5c883717d0048e8" f25
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_c07253e5bc2ad1a2" f25_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_d54b2fb5a87420f7" f26
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_e86ffdca3bb71235" f26_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_d40140ee49300df1" f27
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_7668e339533a6fb4" f27_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_358fecb39951c1ed" f28
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_bf3df5c08da1121b" f28_ptr
  :: F.FunPtr (IO ())

foreign import ccall safe "hs_bindgen_test_visibility_attributes_8255bfc1a96b601c" f29
  :: IO ()

foreign import ccall safe "hs_bindgen_test_visibility_attributes_fb811ac22d16d2c6" f29_ptr
  :: F.FunPtr (IO ())

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
