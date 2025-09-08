{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <visibility_attributes.h>\nvoid hs_bindgen_test_visibility_attributes_beeef9b61fad5466 (void) { f0(); }\n/* get_f0_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_062fb354632b879a (void)) (void) { return &f0; } \nvoid hs_bindgen_test_visibility_attributes_8b60d38de80093fa (void) { f1(); }\n/* get_f1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_a1b79fe9af8e18b8 (void)) (void) { return &f1; } \nvoid hs_bindgen_test_visibility_attributes_4a86b0420a250963 (void) { f2(); }\n/* get_f2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_74cfd16f2b7e27ba (void)) (void) { return &f2; } \nvoid hs_bindgen_test_visibility_attributes_1b95ce9d55223970 (void) { f3(); }\n/* get_f3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_08809dca6bfda237 (void)) (void) { return &f3; } \nvoid hs_bindgen_test_visibility_attributes_cdd7cda26e655f5e (void) { f4(); }\n/* get_f4_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_bd64bfbfcdfa6624 (void)) (void) { return &f4; } \nvoid hs_bindgen_test_visibility_attributes_3bc585c51fec9721 (void) { f5(); }\n/* get_f5_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_0bfaa7955f34f9bf (void)) (void) { return &f5; } \nvoid hs_bindgen_test_visibility_attributes_ba28bdf96df05f32 (void) { f6(); }\n/* get_f6_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_4733471689f1bb70 (void)) (void) { return &f6; } \nvoid hs_bindgen_test_visibility_attributes_2fc1219d73636d66 (void) { f7(); }\n/* get_f7_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_9c959c1281029571 (void)) (void) { return &f7; } \nvoid hs_bindgen_test_visibility_attributes_7a7ce833f71ec006 (void) { f8(); }\n/* get_f8_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_e12a5a5d01dd5a47 (void)) (void) { return &f8; } \nvoid hs_bindgen_test_visibility_attributes_3a6334fe1abf229c (void) { f9(); }\n/* get_f9_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_3d87310c41d7398e (void)) (void) { return &f9; } \nvoid hs_bindgen_test_visibility_attributes_72e7a2a1e9f25000 (void) { f10(); }\n/* get_f10_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_1143dfcfb90def8a (void)) (void) { return &f10; } \nvoid hs_bindgen_test_visibility_attributes_052232f3a6ecd42e (void) { f11(); }\n/* get_f11_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_5d68570d79a6b4fb (void)) (void) { return &f11; } \nvoid hs_bindgen_test_visibility_attributes_792700b287b37bc9 (void) { f12(); }\n/* get_f12_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_46c1e11ee341b116 (void)) (void) { return &f12; } \nvoid hs_bindgen_test_visibility_attributes_241ea65011175c11 (void) { f13(); }\n/* get_f13_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_bb797b66bcb7e8e2 (void)) (void) { return &f13; } \nvoid hs_bindgen_test_visibility_attributes_2c775e867f8ea914 (void) { f14(); }\n/* get_f14_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_8969f1de409a19ee (void)) (void) { return &f14; } \nvoid hs_bindgen_test_visibility_attributes_c9cc679279218ae9 (void) { f15(); }\n/* get_f15_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_454ccc61fb5125af (void)) (void) { return &f15; } \nvoid hs_bindgen_test_visibility_attributes_ea5c7e17063c74da (void) { f16(); }\n/* get_f16_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_023fcc82fef21765 (void)) (void) { return &f16; } \nvoid hs_bindgen_test_visibility_attributes_e72feade6b631a5f (void) { f17(); }\n/* get_f17_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_7036d26aaf2d397f (void)) (void) { return &f17; } \nvoid hs_bindgen_test_visibility_attributes_171950f0c44f6f22 (void) { f18(); }\n/* get_f18_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_e6705f5e2f6359d8 (void)) (void) { return &f18; } \nvoid hs_bindgen_test_visibility_attributes_5414de7b815a8658 (void) { f19(); }\n/* get_f19_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_5b804a3dc8077495 (void)) (void) { return &f19; } \nvoid hs_bindgen_test_visibility_attributes_74ab404c9f6d501e (void) { f20(); }\n/* get_f20_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_763bb37c71b6d9a4 (void)) (void) { return &f20; } \nvoid hs_bindgen_test_visibility_attributes_32c47e20171aaeee (void) { f21(); }\n/* get_f21_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_8661e763dca1c8a0 (void)) (void) { return &f21; } \nvoid hs_bindgen_test_visibility_attributes_ce04bbbd1ae9ec66 (void) { f22(); }\n/* get_f22_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_b39debd724f07945 (void)) (void) { return &f22; } \nvoid hs_bindgen_test_visibility_attributes_005b979e638f474b (void) { f23(); }\n/* get_f23_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_5d6336e1a6bfd80a (void)) (void) { return &f23; } \nvoid hs_bindgen_test_visibility_attributes_e0225d72ee13bade (void) { f24(); }\n/* get_f24_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_cde5aeae243421cd (void)) (void) { return &f24; } \nvoid hs_bindgen_test_visibility_attributes_c5c883717d0048e8 (void) { f25(); }\n/* get_f25_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_c07253e5bc2ad1a2 (void)) (void) { return &f25; } \nvoid hs_bindgen_test_visibility_attributes_d54b2fb5a87420f7 (void) { f26(); }\n/* get_f26_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_e86ffdca3bb71235 (void)) (void) { return &f26; } \nvoid hs_bindgen_test_visibility_attributes_d40140ee49300df1 (void) { f27(); }\n/* get_f27_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_7668e339533a6fb4 (void)) (void) { return &f27; } \nvoid hs_bindgen_test_visibility_attributes_358fecb39951c1ed (void) { f28(); }\n/* get_f28_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_bf3df5c08da1121b (void)) (void) { return &f28; } \nvoid hs_bindgen_test_visibility_attributes_8255bfc1a96b601c (void) { f29(); }\n/* get_f29_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_fb811ac22d16d2c6 (void)) (void) { return &f29; } \n/* get_i0_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_724fd59489c94c9f (void) { return &i0; } \n/* get_i1_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_736e69defba46ab4 (void) { return &i1; } \n/* get_i2_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_210c547ae5abcc02 (void) { return &i2; } \n/* get_i3_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_d6bb66d7f7107274 (void) { return &i3; } \n/* get_i4_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_8c4485c2eb6e1434 (void) { return &i4; } \n/* get_i5_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_6ff6b816265f91d3 (void) { return &i5; } \n/* get_i6_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_3bd2208d8e850002 (void) { return &i6; } \n/* get_i7_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_a3aa6eb624f2c014 (void) { return &i7; } \n/* get_i8_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_696700c5194eb184 (void) { return &i8; } \n/* get_i9_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_27bb5845debfdd10 (void) { return &i9; } \n/* get_i10_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_254dda0b2c3c245d (void) { return &i10; } \n/* get_i11_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_5ca63f16dc0b48e7 (void) { return &i11; } \n/* get_i12_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_75789ceaef5e5feb (void) { return &i12; } \n/* get_i13_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_6e3778cc97c78a2e (void) { return &i13; } \n/* get_i14_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_9ec03118dd66d7c5 (void) { return &i14; } \n/* get_i15_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_4b5a349cc99cdac9 (void) { return &i15; } \n/* get_i16_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_56cec68bd1e37a44 (void) { return &i16; } \n/* get_i17_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_e60a43107858a2bc (void) { return &i17; } \n/* get_i18_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_86247c32f4f34e6f (void) { return &i18; } \n/* get_i19_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_3d3a0ab3e093d4b1 (void) { return &i19; } \n")

{-| __C declaration:__ @f0@

    __defined at:__ @visibility_attributes.h:17:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_beeef9b61fad5466" f0
  :: IO ()

{-| __C declaration:__ @f0@

    __defined at:__ @visibility_attributes.h:17:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_062fb354632b879a" hs_bindgen_test_visibility_attributes_062fb354632b879a
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f0_ptr #-}

f0_ptr :: Ptr.FunPtr (IO ())
f0_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_062fb354632b879a

{-| __C declaration:__ @f1@

    __defined at:__ @visibility_attributes.h:18:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_8b60d38de80093fa" f1
  :: IO ()

{-| __C declaration:__ @f1@

    __defined at:__ @visibility_attributes.h:18:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_a1b79fe9af8e18b8" hs_bindgen_test_visibility_attributes_a1b79fe9af8e18b8
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f1_ptr #-}

f1_ptr :: Ptr.FunPtr (IO ())
f1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_a1b79fe9af8e18b8

{-| __C declaration:__ @f2@

    __defined at:__ @visibility_attributes.h:19:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_4a86b0420a250963" f2
  :: IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @visibility_attributes.h:19:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_74cfd16f2b7e27ba" hs_bindgen_test_visibility_attributes_74cfd16f2b7e27ba
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f2_ptr #-}

f2_ptr :: Ptr.FunPtr (IO ())
f2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_74cfd16f2b7e27ba

{-| __C declaration:__ @f3@

    __defined at:__ @visibility_attributes.h:20:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_1b95ce9d55223970" f3
  :: IO ()

{-| __C declaration:__ @f3@

    __defined at:__ @visibility_attributes.h:20:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_08809dca6bfda237" hs_bindgen_test_visibility_attributes_08809dca6bfda237
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f3_ptr #-}

f3_ptr :: Ptr.FunPtr (IO ())
f3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_08809dca6bfda237

{-| __C declaration:__ @f4@

    __defined at:__ @visibility_attributes.h:21:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_cdd7cda26e655f5e" f4
  :: IO ()

{-| __C declaration:__ @f4@

    __defined at:__ @visibility_attributes.h:21:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_bd64bfbfcdfa6624" hs_bindgen_test_visibility_attributes_bd64bfbfcdfa6624
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f4_ptr #-}

f4_ptr :: Ptr.FunPtr (IO ())
f4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_bd64bfbfcdfa6624

{-| __C declaration:__ @f5@

    __defined at:__ @visibility_attributes.h:24:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_3bc585c51fec9721" f5
  :: IO ()

{-| __C declaration:__ @f5@

    __defined at:__ @visibility_attributes.h:24:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_0bfaa7955f34f9bf" hs_bindgen_test_visibility_attributes_0bfaa7955f34f9bf
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f5_ptr #-}

f5_ptr :: Ptr.FunPtr (IO ())
f5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_0bfaa7955f34f9bf

{-| __C declaration:__ @f6@

    __defined at:__ @visibility_attributes.h:25:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_ba28bdf96df05f32" f6
  :: IO ()

{-| __C declaration:__ @f6@

    __defined at:__ @visibility_attributes.h:25:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_4733471689f1bb70" hs_bindgen_test_visibility_attributes_4733471689f1bb70
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f6_ptr #-}

f6_ptr :: Ptr.FunPtr (IO ())
f6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_4733471689f1bb70

{-| __C declaration:__ @f7@

    __defined at:__ @visibility_attributes.h:26:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_2fc1219d73636d66" f7
  :: IO ()

{-| __C declaration:__ @f7@

    __defined at:__ @visibility_attributes.h:26:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_9c959c1281029571" hs_bindgen_test_visibility_attributes_9c959c1281029571
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f7_ptr #-}

f7_ptr :: Ptr.FunPtr (IO ())
f7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_9c959c1281029571

{-| __C declaration:__ @f8@

    __defined at:__ @visibility_attributes.h:27:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_7a7ce833f71ec006" f8
  :: IO ()

{-| __C declaration:__ @f8@

    __defined at:__ @visibility_attributes.h:27:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_e12a5a5d01dd5a47" hs_bindgen_test_visibility_attributes_e12a5a5d01dd5a47
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f8_ptr #-}

f8_ptr :: Ptr.FunPtr (IO ())
f8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_e12a5a5d01dd5a47

{-| __C declaration:__ @f9@

    __defined at:__ @visibility_attributes.h:28:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_3a6334fe1abf229c" f9
  :: IO ()

{-| __C declaration:__ @f9@

    __defined at:__ @visibility_attributes.h:28:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_3d87310c41d7398e" hs_bindgen_test_visibility_attributes_3d87310c41d7398e
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f9_ptr #-}

f9_ptr :: Ptr.FunPtr (IO ())
f9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_3d87310c41d7398e

{-| __C declaration:__ @f10@

    __defined at:__ @visibility_attributes.h:31:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_72e7a2a1e9f25000" f10
  :: IO ()

{-| __C declaration:__ @f10@

    __defined at:__ @visibility_attributes.h:31:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_1143dfcfb90def8a" hs_bindgen_test_visibility_attributes_1143dfcfb90def8a
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f10_ptr #-}

f10_ptr :: Ptr.FunPtr (IO ())
f10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_1143dfcfb90def8a

{-| __C declaration:__ @f11@

    __defined at:__ @visibility_attributes.h:32:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_052232f3a6ecd42e" f11
  :: IO ()

{-| __C declaration:__ @f11@

    __defined at:__ @visibility_attributes.h:32:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_5d68570d79a6b4fb" hs_bindgen_test_visibility_attributes_5d68570d79a6b4fb
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f11_ptr #-}

f11_ptr :: Ptr.FunPtr (IO ())
f11_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_5d68570d79a6b4fb

{-| __C declaration:__ @f12@

    __defined at:__ @visibility_attributes.h:33:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_792700b287b37bc9" f12
  :: IO ()

{-| __C declaration:__ @f12@

    __defined at:__ @visibility_attributes.h:33:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_46c1e11ee341b116" hs_bindgen_test_visibility_attributes_46c1e11ee341b116
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f12_ptr #-}

f12_ptr :: Ptr.FunPtr (IO ())
f12_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_46c1e11ee341b116

{-| __C declaration:__ @f13@

    __defined at:__ @visibility_attributes.h:34:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_241ea65011175c11" f13
  :: IO ()

{-| __C declaration:__ @f13@

    __defined at:__ @visibility_attributes.h:34:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_bb797b66bcb7e8e2" hs_bindgen_test_visibility_attributes_bb797b66bcb7e8e2
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f13_ptr #-}

f13_ptr :: Ptr.FunPtr (IO ())
f13_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_bb797b66bcb7e8e2

{-| __C declaration:__ @f14@

    __defined at:__ @visibility_attributes.h:35:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_2c775e867f8ea914" f14
  :: IO ()

{-| __C declaration:__ @f14@

    __defined at:__ @visibility_attributes.h:35:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_8969f1de409a19ee" hs_bindgen_test_visibility_attributes_8969f1de409a19ee
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f14_ptr #-}

f14_ptr :: Ptr.FunPtr (IO ())
f14_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_8969f1de409a19ee

{-| __C declaration:__ @f15@

    __defined at:__ @visibility_attributes.h:38:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_c9cc679279218ae9" f15
  :: IO ()

{-| __C declaration:__ @f15@

    __defined at:__ @visibility_attributes.h:38:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_454ccc61fb5125af" hs_bindgen_test_visibility_attributes_454ccc61fb5125af
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f15_ptr #-}

f15_ptr :: Ptr.FunPtr (IO ())
f15_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_454ccc61fb5125af

{-| __C declaration:__ @f16@

    __defined at:__ @visibility_attributes.h:39:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_ea5c7e17063c74da" f16
  :: IO ()

{-| __C declaration:__ @f16@

    __defined at:__ @visibility_attributes.h:39:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_023fcc82fef21765" hs_bindgen_test_visibility_attributes_023fcc82fef21765
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f16_ptr #-}

f16_ptr :: Ptr.FunPtr (IO ())
f16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_023fcc82fef21765

{-| __C declaration:__ @f17@

    __defined at:__ @visibility_attributes.h:40:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_e72feade6b631a5f" f17
  :: IO ()

{-| __C declaration:__ @f17@

    __defined at:__ @visibility_attributes.h:40:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_7036d26aaf2d397f" hs_bindgen_test_visibility_attributes_7036d26aaf2d397f
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f17_ptr #-}

f17_ptr :: Ptr.FunPtr (IO ())
f17_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_7036d26aaf2d397f

{-| __C declaration:__ @f18@

    __defined at:__ @visibility_attributes.h:41:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_171950f0c44f6f22" f18
  :: IO ()

{-| __C declaration:__ @f18@

    __defined at:__ @visibility_attributes.h:41:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_e6705f5e2f6359d8" hs_bindgen_test_visibility_attributes_e6705f5e2f6359d8
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f18_ptr #-}

f18_ptr :: Ptr.FunPtr (IO ())
f18_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_e6705f5e2f6359d8

{-| __C declaration:__ @f19@

    __defined at:__ @visibility_attributes.h:42:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_5414de7b815a8658" f19
  :: IO ()

{-| __C declaration:__ @f19@

    __defined at:__ @visibility_attributes.h:42:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_5b804a3dc8077495" hs_bindgen_test_visibility_attributes_5b804a3dc8077495
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f19_ptr #-}

f19_ptr :: Ptr.FunPtr (IO ())
f19_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_5b804a3dc8077495

{-| __C declaration:__ @f20@

    __defined at:__ @visibility_attributes.h:45:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_74ab404c9f6d501e" f20
  :: IO ()

{-| __C declaration:__ @f20@

    __defined at:__ @visibility_attributes.h:45:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_763bb37c71b6d9a4" hs_bindgen_test_visibility_attributes_763bb37c71b6d9a4
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f20_ptr #-}

f20_ptr :: Ptr.FunPtr (IO ())
f20_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_763bb37c71b6d9a4

{-| __C declaration:__ @f21@

    __defined at:__ @visibility_attributes.h:46:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_32c47e20171aaeee" f21
  :: IO ()

{-| __C declaration:__ @f21@

    __defined at:__ @visibility_attributes.h:46:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_8661e763dca1c8a0" hs_bindgen_test_visibility_attributes_8661e763dca1c8a0
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f21_ptr #-}

f21_ptr :: Ptr.FunPtr (IO ())
f21_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_8661e763dca1c8a0

{-| __C declaration:__ @f22@

    __defined at:__ @visibility_attributes.h:47:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_ce04bbbd1ae9ec66" f22
  :: IO ()

{-| __C declaration:__ @f22@

    __defined at:__ @visibility_attributes.h:47:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_b39debd724f07945" hs_bindgen_test_visibility_attributes_b39debd724f07945
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f22_ptr #-}

f22_ptr :: Ptr.FunPtr (IO ())
f22_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_b39debd724f07945

{-| __C declaration:__ @f23@

    __defined at:__ @visibility_attributes.h:48:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_005b979e638f474b" f23
  :: IO ()

{-| __C declaration:__ @f23@

    __defined at:__ @visibility_attributes.h:48:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_5d6336e1a6bfd80a" hs_bindgen_test_visibility_attributes_5d6336e1a6bfd80a
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f23_ptr #-}

f23_ptr :: Ptr.FunPtr (IO ())
f23_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_5d6336e1a6bfd80a

{-| __C declaration:__ @f24@

    __defined at:__ @visibility_attributes.h:49:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_e0225d72ee13bade" f24
  :: IO ()

{-| __C declaration:__ @f24@

    __defined at:__ @visibility_attributes.h:49:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_cde5aeae243421cd" hs_bindgen_test_visibility_attributes_cde5aeae243421cd
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f24_ptr #-}

f24_ptr :: Ptr.FunPtr (IO ())
f24_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_cde5aeae243421cd

{-| __C declaration:__ @f25@

    __defined at:__ @visibility_attributes.h:52:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_c5c883717d0048e8" f25
  :: IO ()

{-| __C declaration:__ @f25@

    __defined at:__ @visibility_attributes.h:52:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_c07253e5bc2ad1a2" hs_bindgen_test_visibility_attributes_c07253e5bc2ad1a2
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f25_ptr #-}

f25_ptr :: Ptr.FunPtr (IO ())
f25_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_c07253e5bc2ad1a2

{-| __C declaration:__ @f26@

    __defined at:__ @visibility_attributes.h:53:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_d54b2fb5a87420f7" f26
  :: IO ()

{-| __C declaration:__ @f26@

    __defined at:__ @visibility_attributes.h:53:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_e86ffdca3bb71235" hs_bindgen_test_visibility_attributes_e86ffdca3bb71235
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f26_ptr #-}

f26_ptr :: Ptr.FunPtr (IO ())
f26_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_e86ffdca3bb71235

{-| __C declaration:__ @f27@

    __defined at:__ @visibility_attributes.h:54:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_d40140ee49300df1" f27
  :: IO ()

{-| __C declaration:__ @f27@

    __defined at:__ @visibility_attributes.h:54:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_7668e339533a6fb4" hs_bindgen_test_visibility_attributes_7668e339533a6fb4
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f27_ptr #-}

f27_ptr :: Ptr.FunPtr (IO ())
f27_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_7668e339533a6fb4

{-| __C declaration:__ @f28@

    __defined at:__ @visibility_attributes.h:55:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_358fecb39951c1ed" f28
  :: IO ()

{-| __C declaration:__ @f28@

    __defined at:__ @visibility_attributes.h:55:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_bf3df5c08da1121b" hs_bindgen_test_visibility_attributes_bf3df5c08da1121b
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f28_ptr #-}

f28_ptr :: Ptr.FunPtr (IO ())
f28_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_bf3df5c08da1121b

{-| __C declaration:__ @f29@

    __defined at:__ @visibility_attributes.h:56:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_8255bfc1a96b601c" f29
  :: IO ()

{-| __C declaration:__ @f29@

    __defined at:__ @visibility_attributes.h:56:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_fb811ac22d16d2c6" hs_bindgen_test_visibility_attributes_fb811ac22d16d2c6
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f29_ptr #-}

f29_ptr :: Ptr.FunPtr (IO ())
f29_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_fb811ac22d16d2c6

{-| __C declaration:__ @i0@

    __defined at:__ @visibility_attributes.h:61:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_724fd59489c94c9f" hs_bindgen_test_visibility_attributes_724fd59489c94c9f
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i0_ptr #-}

i0_ptr :: Ptr.Ptr FC.CInt
i0_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_724fd59489c94c9f

{-| __C declaration:__ @i1@

    __defined at:__ @visibility_attributes.h:62:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_736e69defba46ab4" hs_bindgen_test_visibility_attributes_736e69defba46ab4
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i1_ptr #-}

i1_ptr :: Ptr.Ptr FC.CInt
i1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_736e69defba46ab4

{-| __C declaration:__ @i2@

    __defined at:__ @visibility_attributes.h:63:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_210c547ae5abcc02" hs_bindgen_test_visibility_attributes_210c547ae5abcc02
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i2_ptr #-}

i2_ptr :: Ptr.Ptr FC.CInt
i2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_210c547ae5abcc02

{-| __C declaration:__ @i3@

    __defined at:__ @visibility_attributes.h:64:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_d6bb66d7f7107274" hs_bindgen_test_visibility_attributes_d6bb66d7f7107274
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i3_ptr #-}

i3_ptr :: Ptr.Ptr FC.CInt
i3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_d6bb66d7f7107274

{-| __C declaration:__ @i4@

    __defined at:__ @visibility_attributes.h:65:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_8c4485c2eb6e1434" hs_bindgen_test_visibility_attributes_8c4485c2eb6e1434
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i4_ptr #-}

i4_ptr :: Ptr.Ptr FC.CInt
i4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_8c4485c2eb6e1434

{-| __C declaration:__ @i5@

    __defined at:__ @visibility_attributes.h:68:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_6ff6b816265f91d3" hs_bindgen_test_visibility_attributes_6ff6b816265f91d3
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i5_ptr #-}

i5_ptr :: Ptr.Ptr FC.CInt
i5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_6ff6b816265f91d3

{-| __C declaration:__ @i6@

    __defined at:__ @visibility_attributes.h:69:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_3bd2208d8e850002" hs_bindgen_test_visibility_attributes_3bd2208d8e850002
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i6_ptr #-}

i6_ptr :: Ptr.Ptr FC.CInt
i6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_3bd2208d8e850002

{-| __C declaration:__ @i7@

    __defined at:__ @visibility_attributes.h:70:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_a3aa6eb624f2c014" hs_bindgen_test_visibility_attributes_a3aa6eb624f2c014
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i7_ptr #-}

i7_ptr :: Ptr.Ptr FC.CInt
i7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_a3aa6eb624f2c014

{-| __C declaration:__ @i8@

    __defined at:__ @visibility_attributes.h:71:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_696700c5194eb184" hs_bindgen_test_visibility_attributes_696700c5194eb184
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i8_ptr #-}

i8_ptr :: Ptr.Ptr FC.CInt
i8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_696700c5194eb184

{-| __C declaration:__ @i9@

    __defined at:__ @visibility_attributes.h:72:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_27bb5845debfdd10" hs_bindgen_test_visibility_attributes_27bb5845debfdd10
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i9_ptr #-}

i9_ptr :: Ptr.Ptr FC.CInt
i9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_27bb5845debfdd10

{-| __C declaration:__ @i10@

    __defined at:__ @visibility_attributes.h:75:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_254dda0b2c3c245d" hs_bindgen_test_visibility_attributes_254dda0b2c3c245d
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i10_ptr #-}

i10_ptr :: Ptr.Ptr FC.CInt
i10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_254dda0b2c3c245d

{-| __C declaration:__ @i11@

    __defined at:__ @visibility_attributes.h:76:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_5ca63f16dc0b48e7" hs_bindgen_test_visibility_attributes_5ca63f16dc0b48e7
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i11_ptr #-}

i11_ptr :: Ptr.Ptr FC.CInt
i11_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_5ca63f16dc0b48e7

{-| __C declaration:__ @i12@

    __defined at:__ @visibility_attributes.h:77:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_75789ceaef5e5feb" hs_bindgen_test_visibility_attributes_75789ceaef5e5feb
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i12_ptr #-}

i12_ptr :: Ptr.Ptr FC.CInt
i12_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_75789ceaef5e5feb

{-| __C declaration:__ @i13@

    __defined at:__ @visibility_attributes.h:78:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_6e3778cc97c78a2e" hs_bindgen_test_visibility_attributes_6e3778cc97c78a2e
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i13_ptr #-}

i13_ptr :: Ptr.Ptr FC.CInt
i13_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_6e3778cc97c78a2e

{-| __C declaration:__ @i14@

    __defined at:__ @visibility_attributes.h:79:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_9ec03118dd66d7c5" hs_bindgen_test_visibility_attributes_9ec03118dd66d7c5
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i14_ptr #-}

i14_ptr :: Ptr.Ptr FC.CInt
i14_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_9ec03118dd66d7c5

{-| __C declaration:__ @i15@

    __defined at:__ @visibility_attributes.h:82:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_4b5a349cc99cdac9" hs_bindgen_test_visibility_attributes_4b5a349cc99cdac9
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i15_ptr #-}

i15_ptr :: Ptr.Ptr FC.CInt
i15_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_4b5a349cc99cdac9

{-| __C declaration:__ @i16@

    __defined at:__ @visibility_attributes.h:83:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_56cec68bd1e37a44" hs_bindgen_test_visibility_attributes_56cec68bd1e37a44
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i16_ptr #-}

i16_ptr :: Ptr.Ptr FC.CInt
i16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_56cec68bd1e37a44

{-| __C declaration:__ @i17@

    __defined at:__ @visibility_attributes.h:84:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_e60a43107858a2bc" hs_bindgen_test_visibility_attributes_e60a43107858a2bc
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i17_ptr #-}

i17_ptr :: Ptr.Ptr FC.CInt
i17_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_e60a43107858a2bc

{-| __C declaration:__ @i18@

    __defined at:__ @visibility_attributes.h:85:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_86247c32f4f34e6f" hs_bindgen_test_visibility_attributes_86247c32f4f34e6f
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i18_ptr #-}

i18_ptr :: Ptr.Ptr FC.CInt
i18_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_86247c32f4f34e6f

{-| __C declaration:__ @i19@

    __defined at:__ @visibility_attributes.h:86:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_3d3a0ab3e093d4b1" hs_bindgen_test_visibility_attributes_3d3a0ab3e093d4b1
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i19_ptr #-}

i19_ptr :: Ptr.Ptr FC.CInt
i19_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_3d3a0ab3e093d4b1
