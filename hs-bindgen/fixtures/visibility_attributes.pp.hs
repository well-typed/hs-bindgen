{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <visibility_attributes.h>\nvoid hs_bindgen_test_visibility_attributes_beeef9b61fad5466 (void) { f0(); }\nvoid hs_bindgen_test_visibility_attributes_8b60d38de80093fa (void) { f1(); }\nvoid hs_bindgen_test_visibility_attributes_4a86b0420a250963 (void) { f2(); }\nvoid hs_bindgen_test_visibility_attributes_1b95ce9d55223970 (void) { f3(); }\nvoid hs_bindgen_test_visibility_attributes_cdd7cda26e655f5e (void) { f4(); }\nvoid hs_bindgen_test_visibility_attributes_3bc585c51fec9721 (void) { f5(); }\nvoid hs_bindgen_test_visibility_attributes_ba28bdf96df05f32 (void) { f6(); }\nvoid hs_bindgen_test_visibility_attributes_2fc1219d73636d66 (void) { f7(); }\nvoid hs_bindgen_test_visibility_attributes_7a7ce833f71ec006 (void) { f8(); }\nvoid hs_bindgen_test_visibility_attributes_3a6334fe1abf229c (void) { f9(); }\nvoid hs_bindgen_test_visibility_attributes_72e7a2a1e9f25000 (void) { f10(); }\nvoid hs_bindgen_test_visibility_attributes_052232f3a6ecd42e (void) { f11(); }\nvoid hs_bindgen_test_visibility_attributes_792700b287b37bc9 (void) { f12(); }\nvoid hs_bindgen_test_visibility_attributes_241ea65011175c11 (void) { f13(); }\nvoid hs_bindgen_test_visibility_attributes_2c775e867f8ea914 (void) { f14(); }\nvoid hs_bindgen_test_visibility_attributes_c9cc679279218ae9 (void) { f15(); }\nvoid hs_bindgen_test_visibility_attributes_ea5c7e17063c74da (void) { f16(); }\nvoid hs_bindgen_test_visibility_attributes_e72feade6b631a5f (void) { f17(); }\nvoid hs_bindgen_test_visibility_attributes_171950f0c44f6f22 (void) { f18(); }\nvoid hs_bindgen_test_visibility_attributes_5414de7b815a8658 (void) { f19(); }\nvoid hs_bindgen_test_visibility_attributes_74ab404c9f6d501e (void) { f20(); }\nvoid hs_bindgen_test_visibility_attributes_32c47e20171aaeee (void) { f21(); }\nvoid hs_bindgen_test_visibility_attributes_ce04bbbd1ae9ec66 (void) { f22(); }\nvoid hs_bindgen_test_visibility_attributes_005b979e638f474b (void) { f23(); }\nvoid hs_bindgen_test_visibility_attributes_e0225d72ee13bade (void) { f24(); }\nvoid hs_bindgen_test_visibility_attributes_c5c883717d0048e8 (void) { f25(); }\nvoid hs_bindgen_test_visibility_attributes_d54b2fb5a87420f7 (void) { f26(); }\nvoid hs_bindgen_test_visibility_attributes_d40140ee49300df1 (void) { f27(); }\nvoid hs_bindgen_test_visibility_attributes_358fecb39951c1ed (void) { f28(); }\nvoid hs_bindgen_test_visibility_attributes_8255bfc1a96b601c (void) { f29(); }\n/* get_i0_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_724fd59489c94c9f (void) { return &i0; } \n/* get_i1_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_736e69defba46ab4 (void) { return &i1; } \n/* get_i2_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_210c547ae5abcc02 (void) { return &i2; } \n/* get_i3_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_d6bb66d7f7107274 (void) { return &i3; } \n/* get_i4_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_8c4485c2eb6e1434 (void) { return &i4; } \n/* get_i5_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_6ff6b816265f91d3 (void) { return &i5; } \n/* get_i6_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_3bd2208d8e850002 (void) { return &i6; } \n/* get_i7_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_a3aa6eb624f2c014 (void) { return &i7; } \n/* get_i8_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_696700c5194eb184 (void) { return &i8; } \n/* get_i9_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_27bb5845debfdd10 (void) { return &i9; } \n/* get_i10_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_254dda0b2c3c245d (void) { return &i10; } \n/* get_i11_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_5ca63f16dc0b48e7 (void) { return &i11; } \n/* get_i12_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_75789ceaef5e5feb (void) { return &i12; } \n/* get_i13_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_6e3778cc97c78a2e (void) { return &i13; } \n/* get_i14_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_9ec03118dd66d7c5 (void) { return &i14; } \n/* get_i15_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_4b5a349cc99cdac9 (void) { return &i15; } \n/* get_i16_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_56cec68bd1e37a44 (void) { return &i16; } \n/* get_i17_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_e60a43107858a2bc (void) { return &i17; } \n/* get_i18_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_86247c32f4f34e6f (void) { return &i18; } \n/* get_i19_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_3d3a0ab3e093d4b1 (void) { return &i19; } \n")

{-| __/Automatically generated from C/__

    __C declaration:__ @f0@

    __defined at:__ @visibility_attributes.h:17:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_beeef9b61fad5466" f0
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f1@

    __defined at:__ @visibility_attributes.h:18:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_8b60d38de80093fa" f1
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f2@

    __defined at:__ @visibility_attributes.h:19:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_4a86b0420a250963" f2
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f3@

    __defined at:__ @visibility_attributes.h:20:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_1b95ce9d55223970" f3
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f4@

    __defined at:__ @visibility_attributes.h:21:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_cdd7cda26e655f5e" f4
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f5@

    __defined at:__ @visibility_attributes.h:24:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_3bc585c51fec9721" f5
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f6@

    __defined at:__ @visibility_attributes.h:25:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_ba28bdf96df05f32" f6
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f7@

    __defined at:__ @visibility_attributes.h:26:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_2fc1219d73636d66" f7
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f8@

    __defined at:__ @visibility_attributes.h:27:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_7a7ce833f71ec006" f8
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f9@

    __defined at:__ @visibility_attributes.h:28:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_3a6334fe1abf229c" f9
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f10@

    __defined at:__ @visibility_attributes.h:31:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_72e7a2a1e9f25000" f10
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f11@

    __defined at:__ @visibility_attributes.h:32:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_052232f3a6ecd42e" f11
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f12@

    __defined at:__ @visibility_attributes.h:33:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_792700b287b37bc9" f12
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f13@

    __defined at:__ @visibility_attributes.h:34:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_241ea65011175c11" f13
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f14@

    __defined at:__ @visibility_attributes.h:35:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_2c775e867f8ea914" f14
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f15@

    __defined at:__ @visibility_attributes.h:38:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_c9cc679279218ae9" f15
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f16@

    __defined at:__ @visibility_attributes.h:39:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_ea5c7e17063c74da" f16
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f17@

    __defined at:__ @visibility_attributes.h:40:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_e72feade6b631a5f" f17
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f18@

    __defined at:__ @visibility_attributes.h:41:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_171950f0c44f6f22" f18
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f19@

    __defined at:__ @visibility_attributes.h:42:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_5414de7b815a8658" f19
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f20@

    __defined at:__ @visibility_attributes.h:45:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_74ab404c9f6d501e" f20
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f21@

    __defined at:__ @visibility_attributes.h:46:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_32c47e20171aaeee" f21
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f22@

    __defined at:__ @visibility_attributes.h:47:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_ce04bbbd1ae9ec66" f22
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f23@

    __defined at:__ @visibility_attributes.h:48:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_005b979e638f474b" f23
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f24@

    __defined at:__ @visibility_attributes.h:49:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_e0225d72ee13bade" f24
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f25@

    __defined at:__ @visibility_attributes.h:52:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_c5c883717d0048e8" f25
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f26@

    __defined at:__ @visibility_attributes.h:53:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_d54b2fb5a87420f7" f26
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f27@

    __defined at:__ @visibility_attributes.h:54:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_d40140ee49300df1" f27
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f28@

    __defined at:__ @visibility_attributes.h:55:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_358fecb39951c1ed" f28
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @f29@

    __defined at:__ @visibility_attributes.h:56:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_8255bfc1a96b601c" f29
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @i0@

    __defined at:__ @visibility_attributes.h:61:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_724fd59489c94c9f" hs_bindgen_test_visibility_attributes_724fd59489c94c9f
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i0_ptr #-}

i0_ptr :: F.Ptr FC.CInt
i0_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_724fd59489c94c9f

{-| __/Automatically generated from C/__

    __C declaration:__ @i1@

    __defined at:__ @visibility_attributes.h:62:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_736e69defba46ab4" hs_bindgen_test_visibility_attributes_736e69defba46ab4
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i1_ptr #-}

i1_ptr :: F.Ptr FC.CInt
i1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_736e69defba46ab4

{-| __/Automatically generated from C/__

    __C declaration:__ @i2@

    __defined at:__ @visibility_attributes.h:63:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_210c547ae5abcc02" hs_bindgen_test_visibility_attributes_210c547ae5abcc02
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i2_ptr #-}

i2_ptr :: F.Ptr FC.CInt
i2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_210c547ae5abcc02

{-| __/Automatically generated from C/__

    __C declaration:__ @i3@

    __defined at:__ @visibility_attributes.h:64:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_d6bb66d7f7107274" hs_bindgen_test_visibility_attributes_d6bb66d7f7107274
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i3_ptr #-}

i3_ptr :: F.Ptr FC.CInt
i3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_d6bb66d7f7107274

{-| __/Automatically generated from C/__

    __C declaration:__ @i4@

    __defined at:__ @visibility_attributes.h:65:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_8c4485c2eb6e1434" hs_bindgen_test_visibility_attributes_8c4485c2eb6e1434
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i4_ptr #-}

i4_ptr :: F.Ptr FC.CInt
i4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_8c4485c2eb6e1434

{-| __/Automatically generated from C/__

    __C declaration:__ @i5@

    __defined at:__ @visibility_attributes.h:68:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_6ff6b816265f91d3" hs_bindgen_test_visibility_attributes_6ff6b816265f91d3
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i5_ptr #-}

i5_ptr :: F.Ptr FC.CInt
i5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_6ff6b816265f91d3

{-| __/Automatically generated from C/__

    __C declaration:__ @i6@

    __defined at:__ @visibility_attributes.h:69:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_3bd2208d8e850002" hs_bindgen_test_visibility_attributes_3bd2208d8e850002
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i6_ptr #-}

i6_ptr :: F.Ptr FC.CInt
i6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_3bd2208d8e850002

{-| __/Automatically generated from C/__

    __C declaration:__ @i7@

    __defined at:__ @visibility_attributes.h:70:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_a3aa6eb624f2c014" hs_bindgen_test_visibility_attributes_a3aa6eb624f2c014
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i7_ptr #-}

i7_ptr :: F.Ptr FC.CInt
i7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_a3aa6eb624f2c014

{-| __/Automatically generated from C/__

    __C declaration:__ @i8@

    __defined at:__ @visibility_attributes.h:71:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_696700c5194eb184" hs_bindgen_test_visibility_attributes_696700c5194eb184
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i8_ptr #-}

i8_ptr :: F.Ptr FC.CInt
i8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_696700c5194eb184

{-| __/Automatically generated from C/__

    __C declaration:__ @i9@

    __defined at:__ @visibility_attributes.h:72:48@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_27bb5845debfdd10" hs_bindgen_test_visibility_attributes_27bb5845debfdd10
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i9_ptr #-}

i9_ptr :: F.Ptr FC.CInt
i9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_27bb5845debfdd10

{-| __/Automatically generated from C/__

    __C declaration:__ @i10@

    __defined at:__ @visibility_attributes.h:75:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_254dda0b2c3c245d" hs_bindgen_test_visibility_attributes_254dda0b2c3c245d
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i10_ptr #-}

i10_ptr :: F.Ptr FC.CInt
i10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_254dda0b2c3c245d

{-| __/Automatically generated from C/__

    __C declaration:__ @i11@

    __defined at:__ @visibility_attributes.h:76:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_5ca63f16dc0b48e7" hs_bindgen_test_visibility_attributes_5ca63f16dc0b48e7
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i11_ptr #-}

i11_ptr :: F.Ptr FC.CInt
i11_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_5ca63f16dc0b48e7

{-| __/Automatically generated from C/__

    __C declaration:__ @i12@

    __defined at:__ @visibility_attributes.h:77:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_75789ceaef5e5feb" hs_bindgen_test_visibility_attributes_75789ceaef5e5feb
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i12_ptr #-}

i12_ptr :: F.Ptr FC.CInt
i12_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_75789ceaef5e5feb

{-| __/Automatically generated from C/__

    __C declaration:__ @i13@

    __defined at:__ @visibility_attributes.h:78:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_6e3778cc97c78a2e" hs_bindgen_test_visibility_attributes_6e3778cc97c78a2e
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i13_ptr #-}

i13_ptr :: F.Ptr FC.CInt
i13_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_6e3778cc97c78a2e

{-| __/Automatically generated from C/__

    __C declaration:__ @i14@

    __defined at:__ @visibility_attributes.h:79:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_9ec03118dd66d7c5" hs_bindgen_test_visibility_attributes_9ec03118dd66d7c5
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i14_ptr #-}

i14_ptr :: F.Ptr FC.CInt
i14_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_9ec03118dd66d7c5

{-| __/Automatically generated from C/__

    __C declaration:__ @i15@

    __defined at:__ @visibility_attributes.h:82:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_4b5a349cc99cdac9" hs_bindgen_test_visibility_attributes_4b5a349cc99cdac9
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i15_ptr #-}

i15_ptr :: F.Ptr FC.CInt
i15_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_4b5a349cc99cdac9

{-| __/Automatically generated from C/__

    __C declaration:__ @i16@

    __defined at:__ @visibility_attributes.h:83:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_56cec68bd1e37a44" hs_bindgen_test_visibility_attributes_56cec68bd1e37a44
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i16_ptr #-}

i16_ptr :: F.Ptr FC.CInt
i16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_56cec68bd1e37a44

{-| __/Automatically generated from C/__

    __C declaration:__ @i17@

    __defined at:__ @visibility_attributes.h:84:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_e60a43107858a2bc" hs_bindgen_test_visibility_attributes_e60a43107858a2bc
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i17_ptr #-}

i17_ptr :: F.Ptr FC.CInt
i17_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_e60a43107858a2bc

{-| __/Automatically generated from C/__

    __C declaration:__ @i18@

    __defined at:__ @visibility_attributes.h:85:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_86247c32f4f34e6f" hs_bindgen_test_visibility_attributes_86247c32f4f34e6f
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i18_ptr #-}

i18_ptr :: F.Ptr FC.CInt
i18_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_86247c32f4f34e6f

{-| __/Automatically generated from C/__

    __C declaration:__ @i19@

    __defined at:__ @visibility_attributes.h:86:55@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_3d3a0ab3e093d4b1" hs_bindgen_test_visibility_attributes_3d3a0ab3e093d4b1
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i19_ptr #-}

i19_ptr :: F.Ptr FC.CInt
i19_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_3d3a0ab3e093d4b1
