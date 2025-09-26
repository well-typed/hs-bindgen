{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource "#include <visibility_attributes.h>\nvoid hs_bindgen_test_visibility_attributes_e8fda12159f2be9f (void) { f0(); }\nvoid hs_bindgen_test_visibility_attributes_a2f84d2570ef3892 (void) { f1(); }\nvoid hs_bindgen_test_visibility_attributes_1d043de05a457e90 (void) { f2(); }\nvoid hs_bindgen_test_visibility_attributes_e23eff1955ebb459 (void) { f3(); }\nvoid hs_bindgen_test_visibility_attributes_ce219a1a5351d14e (void) { f4(); }\nvoid hs_bindgen_test_visibility_attributes_caebbc1a0babf9c3 (void) { f5(); }\nvoid hs_bindgen_test_visibility_attributes_0e94ab16fe1245e4 (void) { f6(); }\nvoid hs_bindgen_test_visibility_attributes_595393c65507c7b2 (void) { f7(); }\nvoid hs_bindgen_test_visibility_attributes_2e6297cc5a3e79e0 (void) { f8(); }\nvoid hs_bindgen_test_visibility_attributes_7d6b88eb048c2261 (void) { f9(); }\nvoid hs_bindgen_test_visibility_attributes_4ef53db381225865 (void) { f10(); }\nvoid hs_bindgen_test_visibility_attributes_c626c8c382be7e47 (void) { f11(); }\nvoid hs_bindgen_test_visibility_attributes_09bc3cf816a85839 (void) { f12(); }\nvoid hs_bindgen_test_visibility_attributes_eadf3eb9d39365cf (void) { f13(); }\nvoid hs_bindgen_test_visibility_attributes_24313656b5162754 (void) { f14(); }\nvoid hs_bindgen_test_visibility_attributes_db09067813df28c1 (void) { f15(); }\nvoid hs_bindgen_test_visibility_attributes_4ce3176c4406cf10 (void) { f16(); }\nvoid hs_bindgen_test_visibility_attributes_2354b7b245be3629 (void) { f17(); }\nvoid hs_bindgen_test_visibility_attributes_a24f6e8ea1a74456 (void) { f18(); }\nvoid hs_bindgen_test_visibility_attributes_e14cdae313c9647d (void) { f19(); }\nvoid hs_bindgen_test_visibility_attributes_d973493c824fdf05 (void) { f20(); }\nvoid hs_bindgen_test_visibility_attributes_cc1b3614f810260c (void) { f21(); }\nvoid hs_bindgen_test_visibility_attributes_cb764aa14ed3e34c (void) { f22(); }\nvoid hs_bindgen_test_visibility_attributes_dc225cc74f4331bf (void) { f23(); }\nvoid hs_bindgen_test_visibility_attributes_88449e6c03902cdf (void) { f24(); }\nvoid hs_bindgen_test_visibility_attributes_7370ca6009a58826 (void) { f25(); }\nvoid hs_bindgen_test_visibility_attributes_a67323b70e59146d (void) { f26(); }\nvoid hs_bindgen_test_visibility_attributes_968a7a3827f17839 (void) { f27(); }\nvoid hs_bindgen_test_visibility_attributes_fbb18ffa92c2c5be (void) { f28(); }\nvoid hs_bindgen_test_visibility_attributes_60afc98eb89b8a2d (void) { f29(); }\n/* get_f0_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_a03f2cbeac50b3d3 (void)) (void) { return &f0; } \n/* get_f1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_5469bdc0395f86c1 (void)) (void) { return &f1; } \n/* get_f2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_490ca7e8c8282a69 (void)) (void) { return &f2; } \n/* get_f3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_38506a9ac5626bf2 (void)) (void) { return &f3; } \n/* get_f4_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_10e5fac8fefa811b (void)) (void) { return &f4; } \n/* get_f5_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_3f137e2ee71fd73b (void)) (void) { return &f5; } \n/* get_f6_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_b69720e01b3b7ccd (void)) (void) { return &f6; } \n/* get_f7_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_97be5f53b506f3b5 (void)) (void) { return &f7; } \n/* get_f8_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_ae7ef3d579d77d0b (void)) (void) { return &f8; } \n/* get_f9_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_ef6d611329a20b40 (void)) (void) { return &f9; } \n/* get_f10_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_45797238134784ac (void)) (void) { return &f10; } \n/* get_f11_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_f09c80553786e039 (void)) (void) { return &f11; } \n/* get_f12_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_2e9999eac1cab3da (void)) (void) { return &f12; } \n/* get_f13_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_32e5be8a3f3ac037 (void)) (void) { return &f13; } \n/* get_f14_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_0b00a23924c6dc70 (void)) (void) { return &f14; } \n/* get_f15_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_0d2891732562e5ef (void)) (void) { return &f15; } \n/* get_f16_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_f25227febbe8db15 (void)) (void) { return &f16; } \n/* get_f17_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_b90b1824c2839fd2 (void)) (void) { return &f17; } \n/* get_f18_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_24ba8b98fe453a5c (void)) (void) { return &f18; } \n/* get_f19_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_b857c57c0cf79909 (void)) (void) { return &f19; } \n/* get_f20_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_d695cc521dd39753 (void)) (void) { return &f20; } \n/* get_f21_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_7311dbbdd00abedc (void)) (void) { return &f21; } \n/* get_f22_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_4246b9344ad4db0d (void)) (void) { return &f22; } \n/* get_f23_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_dcef056ccb5953f9 (void)) (void) { return &f23; } \n/* get_f24_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_9ab6df359be6d370 (void)) (void) { return &f24; } \n/* get_f25_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_80cad6f0afd3f1fc (void)) (void) { return &f25; } \n/* get_f26_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_b1580cdccf30f552 (void)) (void) { return &f26; } \n/* get_f27_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_2e4891a5e2afe0df (void)) (void) { return &f27; } \n/* get_f28_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_e2949a9b7b7cbfc0 (void)) (void) { return &f28; } \n/* get_f29_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_visibility_attributes_1224b39f0e8e72cd (void)) (void) { return &f29; } \n/* get_i0_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_216496b15d8f3143 (void) { return &i0; } \n/* get_i1_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_8a4a155fb4b3e983 (void) { return &i1; } \n/* get_i2_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_8a341976b53c3159 (void) { return &i2; } \n/* get_i3_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_8a18e8a325536dc5 (void) { return &i3; } \n/* get_i4_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_8a083f6803595ed2 (void) { return &i4; } \n/* get_i5_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_b9d322a4c171d6fa (void) { return &i5; } \n/* get_i6_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_2c4836056a76ae78 (void) { return &i6; } \n/* get_i7_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_b9d40a2f9eb7062e (void) { return &i7; } \n/* get_i8_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_3fd4c67173dc11ce (void) { return &i8; } \n/* get_i9_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_b9ff2784975e7295 (void) { return &i9; } \n/* get_i10_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_17a29f1f8c101878 (void) { return &i10; } \n/* get_i11_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_4e591df5ac4216c9 (void) { return &i11; } \n/* get_i12_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_7a8621a15e9246c4 (void) { return &i12; } \n/* get_i13_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_96cb39c8a898775e (void) { return &i13; } \n/* get_i14_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_9f817ee25723fbcd (void) { return &i14; } \n/* get_i15_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_0f8755cf60822f2e (void) { return &i15; } \n/* get_i16_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_3023a5c63e753d58 (void) { return &i16; } \n/* get_i17_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_b5610fa653ca61e6 (void) { return &i17; } \n/* get_i18_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_17edf053c36e012c (void) { return &i18; } \n/* get_i19_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_visibility_attributes_df1c0751f896e6b8 (void) { return &i19; } \n")

{-| __C declaration:__ @f0@

    __defined at:__ @visibility_attributes.h:17:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_e8fda12159f2be9f" f0
  :: IO ()

{-| __C declaration:__ @f1@

    __defined at:__ @visibility_attributes.h:18:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_a2f84d2570ef3892" f1
  :: IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @visibility_attributes.h:19:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_1d043de05a457e90" f2
  :: IO ()

{-| __C declaration:__ @f3@

    __defined at:__ @visibility_attributes.h:20:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_e23eff1955ebb459" f3
  :: IO ()

{-| __C declaration:__ @f4@

    __defined at:__ @visibility_attributes.h:21:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_ce219a1a5351d14e" f4
  :: IO ()

{-| __C declaration:__ @f5@

    __defined at:__ @visibility_attributes.h:24:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_caebbc1a0babf9c3" f5
  :: IO ()

{-| __C declaration:__ @f6@

    __defined at:__ @visibility_attributes.h:25:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_0e94ab16fe1245e4" f6
  :: IO ()

{-| __C declaration:__ @f7@

    __defined at:__ @visibility_attributes.h:26:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_595393c65507c7b2" f7
  :: IO ()

{-| __C declaration:__ @f8@

    __defined at:__ @visibility_attributes.h:27:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_2e6297cc5a3e79e0" f8
  :: IO ()

{-| __C declaration:__ @f9@

    __defined at:__ @visibility_attributes.h:28:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_7d6b88eb048c2261" f9
  :: IO ()

{-| __C declaration:__ @f10@

    __defined at:__ @visibility_attributes.h:31:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_4ef53db381225865" f10
  :: IO ()

{-| __C declaration:__ @f11@

    __defined at:__ @visibility_attributes.h:32:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_c626c8c382be7e47" f11
  :: IO ()

{-| __C declaration:__ @f12@

    __defined at:__ @visibility_attributes.h:33:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_09bc3cf816a85839" f12
  :: IO ()

{-| __C declaration:__ @f13@

    __defined at:__ @visibility_attributes.h:34:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_eadf3eb9d39365cf" f13
  :: IO ()

{-| __C declaration:__ @f14@

    __defined at:__ @visibility_attributes.h:35:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_24313656b5162754" f14
  :: IO ()

{-| __C declaration:__ @f15@

    __defined at:__ @visibility_attributes.h:38:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_db09067813df28c1" f15
  :: IO ()

{-| __C declaration:__ @f16@

    __defined at:__ @visibility_attributes.h:39:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_4ce3176c4406cf10" f16
  :: IO ()

{-| __C declaration:__ @f17@

    __defined at:__ @visibility_attributes.h:40:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_2354b7b245be3629" f17
  :: IO ()

{-| __C declaration:__ @f18@

    __defined at:__ @visibility_attributes.h:41:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_a24f6e8ea1a74456" f18
  :: IO ()

{-| __C declaration:__ @f19@

    __defined at:__ @visibility_attributes.h:42:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_e14cdae313c9647d" f19
  :: IO ()

{-| __C declaration:__ @f20@

    __defined at:__ @visibility_attributes.h:45:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_d973493c824fdf05" f20
  :: IO ()

{-| __C declaration:__ @f21@

    __defined at:__ @visibility_attributes.h:46:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_cc1b3614f810260c" f21
  :: IO ()

{-| __C declaration:__ @f22@

    __defined at:__ @visibility_attributes.h:47:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_cb764aa14ed3e34c" f22
  :: IO ()

{-| __C declaration:__ @f23@

    __defined at:__ @visibility_attributes.h:48:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_dc225cc74f4331bf" f23
  :: IO ()

{-| __C declaration:__ @f24@

    __defined at:__ @visibility_attributes.h:49:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_88449e6c03902cdf" f24
  :: IO ()

{-| __C declaration:__ @f25@

    __defined at:__ @visibility_attributes.h:52:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_7370ca6009a58826" f25
  :: IO ()

{-| __C declaration:__ @f26@

    __defined at:__ @visibility_attributes.h:53:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_a67323b70e59146d" f26
  :: IO ()

{-| __C declaration:__ @f27@

    __defined at:__ @visibility_attributes.h:54:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_968a7a3827f17839" f27
  :: IO ()

{-| __C declaration:__ @f28@

    __defined at:__ @visibility_attributes.h:55:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_fbb18ffa92c2c5be" f28
  :: IO ()

{-| __C declaration:__ @f29@

    __defined at:__ @visibility_attributes.h:56:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_60afc98eb89b8a2d" f29
  :: IO ()

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_a03f2cbeac50b3d3" hs_bindgen_test_visibility_attributes_a03f2cbeac50b3d3
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f0_ptr #-}

{-| __C declaration:__ @f0@

    __defined at:__ @visibility_attributes.h:17:56@

    __exported by:__ @visibility_attributes.h@
-}
f0_ptr :: Ptr.FunPtr (IO ())
f0_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_a03f2cbeac50b3d3

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_5469bdc0395f86c1" hs_bindgen_test_visibility_attributes_5469bdc0395f86c1
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f1_ptr #-}

{-| __C declaration:__ @f1@

    __defined at:__ @visibility_attributes.h:18:56@

    __exported by:__ @visibility_attributes.h@
-}
f1_ptr :: Ptr.FunPtr (IO ())
f1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_5469bdc0395f86c1

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_490ca7e8c8282a69" hs_bindgen_test_visibility_attributes_490ca7e8c8282a69
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f2_ptr #-}

{-| __C declaration:__ @f2@

    __defined at:__ @visibility_attributes.h:19:56@

    __exported by:__ @visibility_attributes.h@
-}
f2_ptr :: Ptr.FunPtr (IO ())
f2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_490ca7e8c8282a69

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_38506a9ac5626bf2" hs_bindgen_test_visibility_attributes_38506a9ac5626bf2
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f3_ptr #-}

{-| __C declaration:__ @f3@

    __defined at:__ @visibility_attributes.h:20:56@

    __exported by:__ @visibility_attributes.h@
-}
f3_ptr :: Ptr.FunPtr (IO ())
f3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_38506a9ac5626bf2

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_10e5fac8fefa811b" hs_bindgen_test_visibility_attributes_10e5fac8fefa811b
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f4_ptr #-}

{-| __C declaration:__ @f4@

    __defined at:__ @visibility_attributes.h:21:56@

    __exported by:__ @visibility_attributes.h@
-}
f4_ptr :: Ptr.FunPtr (IO ())
f4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_10e5fac8fefa811b

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_3f137e2ee71fd73b" hs_bindgen_test_visibility_attributes_3f137e2ee71fd73b
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f5_ptr #-}

{-| __C declaration:__ @f5@

    __defined at:__ @visibility_attributes.h:24:56@

    __exported by:__ @visibility_attributes.h@
-}
f5_ptr :: Ptr.FunPtr (IO ())
f5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_3f137e2ee71fd73b

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_b69720e01b3b7ccd" hs_bindgen_test_visibility_attributes_b69720e01b3b7ccd
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f6_ptr #-}

{-| __C declaration:__ @f6@

    __defined at:__ @visibility_attributes.h:25:56@

    __exported by:__ @visibility_attributes.h@
-}
f6_ptr :: Ptr.FunPtr (IO ())
f6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_b69720e01b3b7ccd

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_97be5f53b506f3b5" hs_bindgen_test_visibility_attributes_97be5f53b506f3b5
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f7_ptr #-}

{-| __C declaration:__ @f7@

    __defined at:__ @visibility_attributes.h:26:56@

    __exported by:__ @visibility_attributes.h@
-}
f7_ptr :: Ptr.FunPtr (IO ())
f7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_97be5f53b506f3b5

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_ae7ef3d579d77d0b" hs_bindgen_test_visibility_attributes_ae7ef3d579d77d0b
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f8_ptr #-}

{-| __C declaration:__ @f8@

    __defined at:__ @visibility_attributes.h:27:56@

    __exported by:__ @visibility_attributes.h@
-}
f8_ptr :: Ptr.FunPtr (IO ())
f8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_ae7ef3d579d77d0b

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_ef6d611329a20b40" hs_bindgen_test_visibility_attributes_ef6d611329a20b40
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f9_ptr #-}

{-| __C declaration:__ @f9@

    __defined at:__ @visibility_attributes.h:28:56@

    __exported by:__ @visibility_attributes.h@
-}
f9_ptr :: Ptr.FunPtr (IO ())
f9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_ef6d611329a20b40

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_45797238134784ac" hs_bindgen_test_visibility_attributes_45797238134784ac
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f10_ptr #-}

{-| __C declaration:__ @f10@

    __defined at:__ @visibility_attributes.h:31:56@

    __exported by:__ @visibility_attributes.h@
-}
f10_ptr :: Ptr.FunPtr (IO ())
f10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_45797238134784ac

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_f09c80553786e039" hs_bindgen_test_visibility_attributes_f09c80553786e039
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f11_ptr #-}

{-| __C declaration:__ @f11@

    __defined at:__ @visibility_attributes.h:32:56@

    __exported by:__ @visibility_attributes.h@
-}
f11_ptr :: Ptr.FunPtr (IO ())
f11_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_f09c80553786e039

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_2e9999eac1cab3da" hs_bindgen_test_visibility_attributes_2e9999eac1cab3da
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f12_ptr #-}

{-| __C declaration:__ @f12@

    __defined at:__ @visibility_attributes.h:33:56@

    __exported by:__ @visibility_attributes.h@
-}
f12_ptr :: Ptr.FunPtr (IO ())
f12_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_2e9999eac1cab3da

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_32e5be8a3f3ac037" hs_bindgen_test_visibility_attributes_32e5be8a3f3ac037
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f13_ptr #-}

{-| __C declaration:__ @f13@

    __defined at:__ @visibility_attributes.h:34:56@

    __exported by:__ @visibility_attributes.h@
-}
f13_ptr :: Ptr.FunPtr (IO ())
f13_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_32e5be8a3f3ac037

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_0b00a23924c6dc70" hs_bindgen_test_visibility_attributes_0b00a23924c6dc70
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f14_ptr #-}

{-| __C declaration:__ @f14@

    __defined at:__ @visibility_attributes.h:35:56@

    __exported by:__ @visibility_attributes.h@
-}
f14_ptr :: Ptr.FunPtr (IO ())
f14_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_0b00a23924c6dc70

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_0d2891732562e5ef" hs_bindgen_test_visibility_attributes_0d2891732562e5ef
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f15_ptr #-}

{-| __C declaration:__ @f15@

    __defined at:__ @visibility_attributes.h:38:56@

    __exported by:__ @visibility_attributes.h@
-}
f15_ptr :: Ptr.FunPtr (IO ())
f15_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_0d2891732562e5ef

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_f25227febbe8db15" hs_bindgen_test_visibility_attributes_f25227febbe8db15
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f16_ptr #-}

{-| __C declaration:__ @f16@

    __defined at:__ @visibility_attributes.h:39:56@

    __exported by:__ @visibility_attributes.h@
-}
f16_ptr :: Ptr.FunPtr (IO ())
f16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_f25227febbe8db15

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_b90b1824c2839fd2" hs_bindgen_test_visibility_attributes_b90b1824c2839fd2
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f17_ptr #-}

{-| __C declaration:__ @f17@

    __defined at:__ @visibility_attributes.h:40:56@

    __exported by:__ @visibility_attributes.h@
-}
f17_ptr :: Ptr.FunPtr (IO ())
f17_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_b90b1824c2839fd2

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_24ba8b98fe453a5c" hs_bindgen_test_visibility_attributes_24ba8b98fe453a5c
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f18_ptr #-}

{-| __C declaration:__ @f18@

    __defined at:__ @visibility_attributes.h:41:56@

    __exported by:__ @visibility_attributes.h@
-}
f18_ptr :: Ptr.FunPtr (IO ())
f18_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_24ba8b98fe453a5c

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_b857c57c0cf79909" hs_bindgen_test_visibility_attributes_b857c57c0cf79909
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f19_ptr #-}

{-| __C declaration:__ @f19@

    __defined at:__ @visibility_attributes.h:42:56@

    __exported by:__ @visibility_attributes.h@
-}
f19_ptr :: Ptr.FunPtr (IO ())
f19_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_b857c57c0cf79909

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_d695cc521dd39753" hs_bindgen_test_visibility_attributes_d695cc521dd39753
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f20_ptr #-}

{-| __C declaration:__ @f20@

    __defined at:__ @visibility_attributes.h:45:56@

    __exported by:__ @visibility_attributes.h@
-}
f20_ptr :: Ptr.FunPtr (IO ())
f20_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_d695cc521dd39753

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_7311dbbdd00abedc" hs_bindgen_test_visibility_attributes_7311dbbdd00abedc
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f21_ptr #-}

{-| __C declaration:__ @f21@

    __defined at:__ @visibility_attributes.h:46:56@

    __exported by:__ @visibility_attributes.h@
-}
f21_ptr :: Ptr.FunPtr (IO ())
f21_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_7311dbbdd00abedc

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_4246b9344ad4db0d" hs_bindgen_test_visibility_attributes_4246b9344ad4db0d
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f22_ptr #-}

{-| __C declaration:__ @f22@

    __defined at:__ @visibility_attributes.h:47:56@

    __exported by:__ @visibility_attributes.h@
-}
f22_ptr :: Ptr.FunPtr (IO ())
f22_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_4246b9344ad4db0d

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_dcef056ccb5953f9" hs_bindgen_test_visibility_attributes_dcef056ccb5953f9
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f23_ptr #-}

{-| __C declaration:__ @f23@

    __defined at:__ @visibility_attributes.h:48:56@

    __exported by:__ @visibility_attributes.h@
-}
f23_ptr :: Ptr.FunPtr (IO ())
f23_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_dcef056ccb5953f9

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_9ab6df359be6d370" hs_bindgen_test_visibility_attributes_9ab6df359be6d370
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f24_ptr #-}

{-| __C declaration:__ @f24@

    __defined at:__ @visibility_attributes.h:49:56@

    __exported by:__ @visibility_attributes.h@
-}
f24_ptr :: Ptr.FunPtr (IO ())
f24_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_9ab6df359be6d370

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_80cad6f0afd3f1fc" hs_bindgen_test_visibility_attributes_80cad6f0afd3f1fc
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f25_ptr #-}

{-| __C declaration:__ @f25@

    __defined at:__ @visibility_attributes.h:52:56@

    __exported by:__ @visibility_attributes.h@
-}
f25_ptr :: Ptr.FunPtr (IO ())
f25_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_80cad6f0afd3f1fc

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_b1580cdccf30f552" hs_bindgen_test_visibility_attributes_b1580cdccf30f552
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f26_ptr #-}

{-| __C declaration:__ @f26@

    __defined at:__ @visibility_attributes.h:53:56@

    __exported by:__ @visibility_attributes.h@
-}
f26_ptr :: Ptr.FunPtr (IO ())
f26_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_b1580cdccf30f552

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_2e4891a5e2afe0df" hs_bindgen_test_visibility_attributes_2e4891a5e2afe0df
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f27_ptr #-}

{-| __C declaration:__ @f27@

    __defined at:__ @visibility_attributes.h:54:56@

    __exported by:__ @visibility_attributes.h@
-}
f27_ptr :: Ptr.FunPtr (IO ())
f27_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_2e4891a5e2afe0df

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_e2949a9b7b7cbfc0" hs_bindgen_test_visibility_attributes_e2949a9b7b7cbfc0
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f28_ptr #-}

{-| __C declaration:__ @f28@

    __defined at:__ @visibility_attributes.h:55:56@

    __exported by:__ @visibility_attributes.h@
-}
f28_ptr :: Ptr.FunPtr (IO ())
f28_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_e2949a9b7b7cbfc0

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_1224b39f0e8e72cd" hs_bindgen_test_visibility_attributes_1224b39f0e8e72cd
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f29_ptr #-}

{-| __C declaration:__ @f29@

    __defined at:__ @visibility_attributes.h:56:56@

    __exported by:__ @visibility_attributes.h@
-}
f29_ptr :: Ptr.FunPtr (IO ())
f29_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_1224b39f0e8e72cd

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_216496b15d8f3143" hs_bindgen_test_visibility_attributes_216496b15d8f3143
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i0_ptr #-}

{-| __C declaration:__ @i0@

    __defined at:__ @visibility_attributes.h:61:48@

    __exported by:__ @visibility_attributes.h@
-}
i0_ptr :: Ptr.Ptr FC.CInt
i0_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_216496b15d8f3143

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_8a4a155fb4b3e983" hs_bindgen_test_visibility_attributes_8a4a155fb4b3e983
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i1_ptr #-}

{-| __C declaration:__ @i1@

    __defined at:__ @visibility_attributes.h:62:48@

    __exported by:__ @visibility_attributes.h@
-}
i1_ptr :: Ptr.Ptr FC.CInt
i1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_8a4a155fb4b3e983

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_8a341976b53c3159" hs_bindgen_test_visibility_attributes_8a341976b53c3159
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i2_ptr #-}

{-| __C declaration:__ @i2@

    __defined at:__ @visibility_attributes.h:63:48@

    __exported by:__ @visibility_attributes.h@
-}
i2_ptr :: Ptr.Ptr FC.CInt
i2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_8a341976b53c3159

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_8a18e8a325536dc5" hs_bindgen_test_visibility_attributes_8a18e8a325536dc5
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i3_ptr #-}

{-| __C declaration:__ @i3@

    __defined at:__ @visibility_attributes.h:64:48@

    __exported by:__ @visibility_attributes.h@
-}
i3_ptr :: Ptr.Ptr FC.CInt
i3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_8a18e8a325536dc5

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_8a083f6803595ed2" hs_bindgen_test_visibility_attributes_8a083f6803595ed2
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i4_ptr #-}

{-| __C declaration:__ @i4@

    __defined at:__ @visibility_attributes.h:65:48@

    __exported by:__ @visibility_attributes.h@
-}
i4_ptr :: Ptr.Ptr FC.CInt
i4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_8a083f6803595ed2

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_b9d322a4c171d6fa" hs_bindgen_test_visibility_attributes_b9d322a4c171d6fa
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i5_ptr #-}

{-| __C declaration:__ @i5@

    __defined at:__ @visibility_attributes.h:68:48@

    __exported by:__ @visibility_attributes.h@
-}
i5_ptr :: Ptr.Ptr FC.CInt
i5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_b9d322a4c171d6fa

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_2c4836056a76ae78" hs_bindgen_test_visibility_attributes_2c4836056a76ae78
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i6_ptr #-}

{-| __C declaration:__ @i6@

    __defined at:__ @visibility_attributes.h:69:48@

    __exported by:__ @visibility_attributes.h@
-}
i6_ptr :: Ptr.Ptr FC.CInt
i6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_2c4836056a76ae78

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_b9d40a2f9eb7062e" hs_bindgen_test_visibility_attributes_b9d40a2f9eb7062e
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i7_ptr #-}

{-| __C declaration:__ @i7@

    __defined at:__ @visibility_attributes.h:70:48@

    __exported by:__ @visibility_attributes.h@
-}
i7_ptr :: Ptr.Ptr FC.CInt
i7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_b9d40a2f9eb7062e

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_3fd4c67173dc11ce" hs_bindgen_test_visibility_attributes_3fd4c67173dc11ce
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i8_ptr #-}

{-| __C declaration:__ @i8@

    __defined at:__ @visibility_attributes.h:71:48@

    __exported by:__ @visibility_attributes.h@
-}
i8_ptr :: Ptr.Ptr FC.CInt
i8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_3fd4c67173dc11ce

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_b9ff2784975e7295" hs_bindgen_test_visibility_attributes_b9ff2784975e7295
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i9_ptr #-}

{-| __C declaration:__ @i9@

    __defined at:__ @visibility_attributes.h:72:48@

    __exported by:__ @visibility_attributes.h@
-}
i9_ptr :: Ptr.Ptr FC.CInt
i9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_b9ff2784975e7295

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_17a29f1f8c101878" hs_bindgen_test_visibility_attributes_17a29f1f8c101878
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i10_ptr #-}

{-| __C declaration:__ @i10@

    __defined at:__ @visibility_attributes.h:75:55@

    __exported by:__ @visibility_attributes.h@
-}
i10_ptr :: Ptr.Ptr FC.CInt
i10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_17a29f1f8c101878

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_4e591df5ac4216c9" hs_bindgen_test_visibility_attributes_4e591df5ac4216c9
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i11_ptr #-}

{-| __C declaration:__ @i11@

    __defined at:__ @visibility_attributes.h:76:55@

    __exported by:__ @visibility_attributes.h@
-}
i11_ptr :: Ptr.Ptr FC.CInt
i11_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_4e591df5ac4216c9

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_7a8621a15e9246c4" hs_bindgen_test_visibility_attributes_7a8621a15e9246c4
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i12_ptr #-}

{-| __C declaration:__ @i12@

    __defined at:__ @visibility_attributes.h:77:55@

    __exported by:__ @visibility_attributes.h@
-}
i12_ptr :: Ptr.Ptr FC.CInt
i12_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_7a8621a15e9246c4

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_96cb39c8a898775e" hs_bindgen_test_visibility_attributes_96cb39c8a898775e
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i13_ptr #-}

{-| __C declaration:__ @i13@

    __defined at:__ @visibility_attributes.h:78:55@

    __exported by:__ @visibility_attributes.h@
-}
i13_ptr :: Ptr.Ptr FC.CInt
i13_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_96cb39c8a898775e

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_9f817ee25723fbcd" hs_bindgen_test_visibility_attributes_9f817ee25723fbcd
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i14_ptr #-}

{-| __C declaration:__ @i14@

    __defined at:__ @visibility_attributes.h:79:55@

    __exported by:__ @visibility_attributes.h@
-}
i14_ptr :: Ptr.Ptr FC.CInt
i14_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_9f817ee25723fbcd

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_0f8755cf60822f2e" hs_bindgen_test_visibility_attributes_0f8755cf60822f2e
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i15_ptr #-}

{-| __C declaration:__ @i15@

    __defined at:__ @visibility_attributes.h:82:55@

    __exported by:__ @visibility_attributes.h@
-}
i15_ptr :: Ptr.Ptr FC.CInt
i15_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_0f8755cf60822f2e

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_3023a5c63e753d58" hs_bindgen_test_visibility_attributes_3023a5c63e753d58
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i16_ptr #-}

{-| __C declaration:__ @i16@

    __defined at:__ @visibility_attributes.h:83:55@

    __exported by:__ @visibility_attributes.h@
-}
i16_ptr :: Ptr.Ptr FC.CInt
i16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_3023a5c63e753d58

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_b5610fa653ca61e6" hs_bindgen_test_visibility_attributes_b5610fa653ca61e6
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i17_ptr #-}

{-| __C declaration:__ @i17@

    __defined at:__ @visibility_attributes.h:84:55@

    __exported by:__ @visibility_attributes.h@
-}
i17_ptr :: Ptr.Ptr FC.CInt
i17_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_b5610fa653ca61e6

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_17edf053c36e012c" hs_bindgen_test_visibility_attributes_17edf053c36e012c
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i18_ptr #-}

{-| __C declaration:__ @i18@

    __defined at:__ @visibility_attributes.h:85:55@

    __exported by:__ @visibility_attributes.h@
-}
i18_ptr :: Ptr.Ptr FC.CInt
i18_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_17edf053c36e012c

foreign import ccall unsafe "hs_bindgen_test_visibility_attributes_df1c0751f896e6b8" hs_bindgen_test_visibility_attributes_df1c0751f896e6b8
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i19_ptr #-}

{-| __C declaration:__ @i19@

    __defined at:__ @visibility_attributes.h:86:55@

    __exported by:__ @visibility_attributes.h@
-}
i19_ptr :: Ptr.Ptr FC.CInt
i19_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_visibility_attributes_df1c0751f896e6b8
