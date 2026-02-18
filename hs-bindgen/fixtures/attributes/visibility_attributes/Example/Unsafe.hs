{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <attributes/visibility_attributes.h>"
  , "void hs_bindgen_df56d82c9186c794 (void)"
  , "{"
  , "  f0();"
  , "}"
  , "void hs_bindgen_2e25b1147da3cdee (void)"
  , "{"
  , "  f1();"
  , "}"
  , "void hs_bindgen_c57341dd645988f6 (void)"
  , "{"
  , "  f2();"
  , "}"
  , "void hs_bindgen_5858f33f5b12f541 (void)"
  , "{"
  , "  f3();"
  , "}"
  , "void hs_bindgen_d57b75423078a644 (void)"
  , "{"
  , "  f4();"
  , "}"
  , "void hs_bindgen_d7e35ffb8ef15009 (void)"
  , "{"
  , "  f5();"
  , "}"
  , "void hs_bindgen_de345dceb6694e2e (void)"
  , "{"
  , "  f6();"
  , "}"
  , "void hs_bindgen_df2fd4ea47789bb8 (void)"
  , "{"
  , "  f7();"
  , "}"
  , "void hs_bindgen_a49e6066d8dd0628 (void)"
  , "{"
  , "  f8();"
  , "}"
  , "void hs_bindgen_e94ceb5e10d07a59 (void)"
  , "{"
  , "  f9();"
  , "}"
  , "void hs_bindgen_d329fc979eb3d29e (void)"
  , "{"
  , "  f10();"
  , "}"
  , "void hs_bindgen_9f7176bfdf1871cf (void)"
  , "{"
  , "  f11();"
  , "}"
  , "void hs_bindgen_d06a71df91d678f1 (void)"
  , "{"
  , "  f12();"
  , "}"
  , "void hs_bindgen_32916e0b4775516d (void)"
  , "{"
  , "  f13();"
  , "}"
  , "void hs_bindgen_56f26ce5de7906a8 (void)"
  , "{"
  , "  f14();"
  , "}"
  , "void hs_bindgen_fd2972764f48a143 (void)"
  , "{"
  , "  f15();"
  , "}"
  , "void hs_bindgen_f63a042e539ff8b6 (void)"
  , "{"
  , "  f16();"
  , "}"
  , "void hs_bindgen_1ae5adf1961e06f6 (void)"
  , "{"
  , "  f17();"
  , "}"
  , "void hs_bindgen_cf7978ab53dc8140 (void)"
  , "{"
  , "  f18();"
  , "}"
  , "void hs_bindgen_f81ec562e6b7dc0f (void)"
  , "{"
  , "  f19();"
  , "}"
  , "void hs_bindgen_95b4b9ea66f61551 (void)"
  , "{"
  , "  f20();"
  , "}"
  , "void hs_bindgen_c64dd47543cb7378 (void)"
  , "{"
  , "  f21();"
  , "}"
  , "void hs_bindgen_ce27e3f019c9d235 (void)"
  , "{"
  , "  f22();"
  , "}"
  , "void hs_bindgen_ae8f854ac4a07381 (void)"
  , "{"
  , "  f23();"
  , "}"
  , "void hs_bindgen_d4585a283ed9fc36 (void)"
  , "{"
  , "  f24();"
  , "}"
  , "void hs_bindgen_f600df85706a5312 (void)"
  , "{"
  , "  f25();"
  , "}"
  , "void hs_bindgen_92731c80b3281e1c (void)"
  , "{"
  , "  f26();"
  , "}"
  , "void hs_bindgen_930247d3730559af (void)"
  , "{"
  , "  f27();"
  , "}"
  , "void hs_bindgen_2d1ce37b69d8d467 (void)"
  , "{"
  , "  f28();"
  , "}"
  , "void hs_bindgen_3f89a86cfcdf475d (void)"
  , "{"
  , "  f29();"
  , "}"
  ]))

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f0@
foreign import ccall unsafe "hs_bindgen_df56d82c9186c794" hs_bindgen_df56d82c9186c794_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f0@
hs_bindgen_df56d82c9186c794 :: IO ()
hs_bindgen_df56d82c9186c794 =
  RIP.fromFFIType hs_bindgen_df56d82c9186c794_base

{-| __C declaration:__ @f0@

    __defined at:__ @attributes\/visibility_attributes.h 17:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f0 :: IO ()
f0 = hs_bindgen_df56d82c9186c794

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f1@
foreign import ccall unsafe "hs_bindgen_2e25b1147da3cdee" hs_bindgen_2e25b1147da3cdee_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f1@
hs_bindgen_2e25b1147da3cdee :: IO ()
hs_bindgen_2e25b1147da3cdee =
  RIP.fromFFIType hs_bindgen_2e25b1147da3cdee_base

{-| __C declaration:__ @f1@

    __defined at:__ @attributes\/visibility_attributes.h 18:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f1 :: IO ()
f1 = hs_bindgen_2e25b1147da3cdee

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f2@
foreign import ccall unsafe "hs_bindgen_c57341dd645988f6" hs_bindgen_c57341dd645988f6_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f2@
hs_bindgen_c57341dd645988f6 :: IO ()
hs_bindgen_c57341dd645988f6 =
  RIP.fromFFIType hs_bindgen_c57341dd645988f6_base

{-| __C declaration:__ @f2@

    __defined at:__ @attributes\/visibility_attributes.h 19:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f2 :: IO ()
f2 = hs_bindgen_c57341dd645988f6

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f3@
foreign import ccall unsafe "hs_bindgen_5858f33f5b12f541" hs_bindgen_5858f33f5b12f541_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f3@
hs_bindgen_5858f33f5b12f541 :: IO ()
hs_bindgen_5858f33f5b12f541 =
  RIP.fromFFIType hs_bindgen_5858f33f5b12f541_base

{-| __C declaration:__ @f3@

    __defined at:__ @attributes\/visibility_attributes.h 20:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f3 :: IO ()
f3 = hs_bindgen_5858f33f5b12f541

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f4@
foreign import ccall unsafe "hs_bindgen_d57b75423078a644" hs_bindgen_d57b75423078a644_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f4@
hs_bindgen_d57b75423078a644 :: IO ()
hs_bindgen_d57b75423078a644 =
  RIP.fromFFIType hs_bindgen_d57b75423078a644_base

{-| __C declaration:__ @f4@

    __defined at:__ @attributes\/visibility_attributes.h 21:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f4 :: IO ()
f4 = hs_bindgen_d57b75423078a644

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f5@
foreign import ccall unsafe "hs_bindgen_d7e35ffb8ef15009" hs_bindgen_d7e35ffb8ef15009_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f5@
hs_bindgen_d7e35ffb8ef15009 :: IO ()
hs_bindgen_d7e35ffb8ef15009 =
  RIP.fromFFIType hs_bindgen_d7e35ffb8ef15009_base

{-| __C declaration:__ @f5@

    __defined at:__ @attributes\/visibility_attributes.h 24:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f5 :: IO ()
f5 = hs_bindgen_d7e35ffb8ef15009

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f6@
foreign import ccall unsafe "hs_bindgen_de345dceb6694e2e" hs_bindgen_de345dceb6694e2e_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f6@
hs_bindgen_de345dceb6694e2e :: IO ()
hs_bindgen_de345dceb6694e2e =
  RIP.fromFFIType hs_bindgen_de345dceb6694e2e_base

{-| __C declaration:__ @f6@

    __defined at:__ @attributes\/visibility_attributes.h 25:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f6 :: IO ()
f6 = hs_bindgen_de345dceb6694e2e

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f7@
foreign import ccall unsafe "hs_bindgen_df2fd4ea47789bb8" hs_bindgen_df2fd4ea47789bb8_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f7@
hs_bindgen_df2fd4ea47789bb8 :: IO ()
hs_bindgen_df2fd4ea47789bb8 =
  RIP.fromFFIType hs_bindgen_df2fd4ea47789bb8_base

{-| __C declaration:__ @f7@

    __defined at:__ @attributes\/visibility_attributes.h 26:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f7 :: IO ()
f7 = hs_bindgen_df2fd4ea47789bb8

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f8@
foreign import ccall unsafe "hs_bindgen_a49e6066d8dd0628" hs_bindgen_a49e6066d8dd0628_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f8@
hs_bindgen_a49e6066d8dd0628 :: IO ()
hs_bindgen_a49e6066d8dd0628 =
  RIP.fromFFIType hs_bindgen_a49e6066d8dd0628_base

{-| __C declaration:__ @f8@

    __defined at:__ @attributes\/visibility_attributes.h 27:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f8 :: IO ()
f8 = hs_bindgen_a49e6066d8dd0628

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f9@
foreign import ccall unsafe "hs_bindgen_e94ceb5e10d07a59" hs_bindgen_e94ceb5e10d07a59_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f9@
hs_bindgen_e94ceb5e10d07a59 :: IO ()
hs_bindgen_e94ceb5e10d07a59 =
  RIP.fromFFIType hs_bindgen_e94ceb5e10d07a59_base

{-| __C declaration:__ @f9@

    __defined at:__ @attributes\/visibility_attributes.h 28:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f9 :: IO ()
f9 = hs_bindgen_e94ceb5e10d07a59

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f10@
foreign import ccall unsafe "hs_bindgen_d329fc979eb3d29e" hs_bindgen_d329fc979eb3d29e_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f10@
hs_bindgen_d329fc979eb3d29e :: IO ()
hs_bindgen_d329fc979eb3d29e =
  RIP.fromFFIType hs_bindgen_d329fc979eb3d29e_base

{-| __C declaration:__ @f10@

    __defined at:__ @attributes\/visibility_attributes.h 31:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f10 :: IO ()
f10 = hs_bindgen_d329fc979eb3d29e

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f11@
foreign import ccall unsafe "hs_bindgen_9f7176bfdf1871cf" hs_bindgen_9f7176bfdf1871cf_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f11@
hs_bindgen_9f7176bfdf1871cf :: IO ()
hs_bindgen_9f7176bfdf1871cf =
  RIP.fromFFIType hs_bindgen_9f7176bfdf1871cf_base

{-| __C declaration:__ @f11@

    __defined at:__ @attributes\/visibility_attributes.h 32:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f11 :: IO ()
f11 = hs_bindgen_9f7176bfdf1871cf

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f12@
foreign import ccall unsafe "hs_bindgen_d06a71df91d678f1" hs_bindgen_d06a71df91d678f1_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f12@
hs_bindgen_d06a71df91d678f1 :: IO ()
hs_bindgen_d06a71df91d678f1 =
  RIP.fromFFIType hs_bindgen_d06a71df91d678f1_base

{-| __C declaration:__ @f12@

    __defined at:__ @attributes\/visibility_attributes.h 33:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f12 :: IO ()
f12 = hs_bindgen_d06a71df91d678f1

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f13@
foreign import ccall unsafe "hs_bindgen_32916e0b4775516d" hs_bindgen_32916e0b4775516d_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f13@
hs_bindgen_32916e0b4775516d :: IO ()
hs_bindgen_32916e0b4775516d =
  RIP.fromFFIType hs_bindgen_32916e0b4775516d_base

{-| __C declaration:__ @f13@

    __defined at:__ @attributes\/visibility_attributes.h 34:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f13 :: IO ()
f13 = hs_bindgen_32916e0b4775516d

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f14@
foreign import ccall unsafe "hs_bindgen_56f26ce5de7906a8" hs_bindgen_56f26ce5de7906a8_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f14@
hs_bindgen_56f26ce5de7906a8 :: IO ()
hs_bindgen_56f26ce5de7906a8 =
  RIP.fromFFIType hs_bindgen_56f26ce5de7906a8_base

{-| __C declaration:__ @f14@

    __defined at:__ @attributes\/visibility_attributes.h 35:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f14 :: IO ()
f14 = hs_bindgen_56f26ce5de7906a8

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f15@
foreign import ccall unsafe "hs_bindgen_fd2972764f48a143" hs_bindgen_fd2972764f48a143_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f15@
hs_bindgen_fd2972764f48a143 :: IO ()
hs_bindgen_fd2972764f48a143 =
  RIP.fromFFIType hs_bindgen_fd2972764f48a143_base

{-| __C declaration:__ @f15@

    __defined at:__ @attributes\/visibility_attributes.h 38:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f15 :: IO ()
f15 = hs_bindgen_fd2972764f48a143

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f16@
foreign import ccall unsafe "hs_bindgen_f63a042e539ff8b6" hs_bindgen_f63a042e539ff8b6_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f16@
hs_bindgen_f63a042e539ff8b6 :: IO ()
hs_bindgen_f63a042e539ff8b6 =
  RIP.fromFFIType hs_bindgen_f63a042e539ff8b6_base

{-| __C declaration:__ @f16@

    __defined at:__ @attributes\/visibility_attributes.h 39:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f16 :: IO ()
f16 = hs_bindgen_f63a042e539ff8b6

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f17@
foreign import ccall unsafe "hs_bindgen_1ae5adf1961e06f6" hs_bindgen_1ae5adf1961e06f6_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f17@
hs_bindgen_1ae5adf1961e06f6 :: IO ()
hs_bindgen_1ae5adf1961e06f6 =
  RIP.fromFFIType hs_bindgen_1ae5adf1961e06f6_base

{-| __C declaration:__ @f17@

    __defined at:__ @attributes\/visibility_attributes.h 40:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f17 :: IO ()
f17 = hs_bindgen_1ae5adf1961e06f6

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f18@
foreign import ccall unsafe "hs_bindgen_cf7978ab53dc8140" hs_bindgen_cf7978ab53dc8140_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f18@
hs_bindgen_cf7978ab53dc8140 :: IO ()
hs_bindgen_cf7978ab53dc8140 =
  RIP.fromFFIType hs_bindgen_cf7978ab53dc8140_base

{-| __C declaration:__ @f18@

    __defined at:__ @attributes\/visibility_attributes.h 41:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f18 :: IO ()
f18 = hs_bindgen_cf7978ab53dc8140

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f19@
foreign import ccall unsafe "hs_bindgen_f81ec562e6b7dc0f" hs_bindgen_f81ec562e6b7dc0f_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f19@
hs_bindgen_f81ec562e6b7dc0f :: IO ()
hs_bindgen_f81ec562e6b7dc0f =
  RIP.fromFFIType hs_bindgen_f81ec562e6b7dc0f_base

{-| __C declaration:__ @f19@

    __defined at:__ @attributes\/visibility_attributes.h 42:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f19 :: IO ()
f19 = hs_bindgen_f81ec562e6b7dc0f

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f20@
foreign import ccall unsafe "hs_bindgen_95b4b9ea66f61551" hs_bindgen_95b4b9ea66f61551_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f20@
hs_bindgen_95b4b9ea66f61551 :: IO ()
hs_bindgen_95b4b9ea66f61551 =
  RIP.fromFFIType hs_bindgen_95b4b9ea66f61551_base

{-| __C declaration:__ @f20@

    __defined at:__ @attributes\/visibility_attributes.h 45:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f20 :: IO ()
f20 = hs_bindgen_95b4b9ea66f61551

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f21@
foreign import ccall unsafe "hs_bindgen_c64dd47543cb7378" hs_bindgen_c64dd47543cb7378_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f21@
hs_bindgen_c64dd47543cb7378 :: IO ()
hs_bindgen_c64dd47543cb7378 =
  RIP.fromFFIType hs_bindgen_c64dd47543cb7378_base

{-| __C declaration:__ @f21@

    __defined at:__ @attributes\/visibility_attributes.h 46:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f21 :: IO ()
f21 = hs_bindgen_c64dd47543cb7378

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f22@
foreign import ccall unsafe "hs_bindgen_ce27e3f019c9d235" hs_bindgen_ce27e3f019c9d235_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f22@
hs_bindgen_ce27e3f019c9d235 :: IO ()
hs_bindgen_ce27e3f019c9d235 =
  RIP.fromFFIType hs_bindgen_ce27e3f019c9d235_base

{-| __C declaration:__ @f22@

    __defined at:__ @attributes\/visibility_attributes.h 47:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f22 :: IO ()
f22 = hs_bindgen_ce27e3f019c9d235

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f23@
foreign import ccall unsafe "hs_bindgen_ae8f854ac4a07381" hs_bindgen_ae8f854ac4a07381_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f23@
hs_bindgen_ae8f854ac4a07381 :: IO ()
hs_bindgen_ae8f854ac4a07381 =
  RIP.fromFFIType hs_bindgen_ae8f854ac4a07381_base

{-| __C declaration:__ @f23@

    __defined at:__ @attributes\/visibility_attributes.h 48:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f23 :: IO ()
f23 = hs_bindgen_ae8f854ac4a07381

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f24@
foreign import ccall unsafe "hs_bindgen_d4585a283ed9fc36" hs_bindgen_d4585a283ed9fc36_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f24@
hs_bindgen_d4585a283ed9fc36 :: IO ()
hs_bindgen_d4585a283ed9fc36 =
  RIP.fromFFIType hs_bindgen_d4585a283ed9fc36_base

{-| __C declaration:__ @f24@

    __defined at:__ @attributes\/visibility_attributes.h 49:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f24 :: IO ()
f24 = hs_bindgen_d4585a283ed9fc36

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f25@
foreign import ccall unsafe "hs_bindgen_f600df85706a5312" hs_bindgen_f600df85706a5312_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f25@
hs_bindgen_f600df85706a5312 :: IO ()
hs_bindgen_f600df85706a5312 =
  RIP.fromFFIType hs_bindgen_f600df85706a5312_base

{-| __C declaration:__ @f25@

    __defined at:__ @attributes\/visibility_attributes.h 52:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f25 :: IO ()
f25 = hs_bindgen_f600df85706a5312

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f26@
foreign import ccall unsafe "hs_bindgen_92731c80b3281e1c" hs_bindgen_92731c80b3281e1c_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f26@
hs_bindgen_92731c80b3281e1c :: IO ()
hs_bindgen_92731c80b3281e1c =
  RIP.fromFFIType hs_bindgen_92731c80b3281e1c_base

{-| __C declaration:__ @f26@

    __defined at:__ @attributes\/visibility_attributes.h 53:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f26 :: IO ()
f26 = hs_bindgen_92731c80b3281e1c

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f27@
foreign import ccall unsafe "hs_bindgen_930247d3730559af" hs_bindgen_930247d3730559af_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f27@
hs_bindgen_930247d3730559af :: IO ()
hs_bindgen_930247d3730559af =
  RIP.fromFFIType hs_bindgen_930247d3730559af_base

{-| __C declaration:__ @f27@

    __defined at:__ @attributes\/visibility_attributes.h 54:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f27 :: IO ()
f27 = hs_bindgen_930247d3730559af

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f28@
foreign import ccall unsafe "hs_bindgen_2d1ce37b69d8d467" hs_bindgen_2d1ce37b69d8d467_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f28@
hs_bindgen_2d1ce37b69d8d467 :: IO ()
hs_bindgen_2d1ce37b69d8d467 =
  RIP.fromFFIType hs_bindgen_2d1ce37b69d8d467_base

{-| __C declaration:__ @f28@

    __defined at:__ @attributes\/visibility_attributes.h 55:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f28 :: IO ()
f28 = hs_bindgen_2d1ce37b69d8d467

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f29@
foreign import ccall unsafe "hs_bindgen_3f89a86cfcdf475d" hs_bindgen_3f89a86cfcdf475d_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f29@
hs_bindgen_3f89a86cfcdf475d :: IO ()
hs_bindgen_3f89a86cfcdf475d =
  RIP.fromFFIType hs_bindgen_3f89a86cfcdf475d_base

{-| __C declaration:__ @f29@

    __defined at:__ @attributes\/visibility_attributes.h 56:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f29 :: IO ()
f29 = hs_bindgen_3f89a86cfcdf475d
