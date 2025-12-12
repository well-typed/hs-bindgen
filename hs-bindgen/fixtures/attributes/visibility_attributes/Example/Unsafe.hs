{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_df56d82c9186c794" f0_base ::
     IO ()

{-| __C declaration:__ @f0@

    __defined at:__ @attributes\/visibility_attributes.h:17:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f0@
-}
f0 ::
     IO ()
f0 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f0_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_2e25b1147da3cdee" f1_base ::
     IO ()

{-| __C declaration:__ @f1@

    __defined at:__ @attributes\/visibility_attributes.h:18:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f1@
-}
f1 ::
     IO ()
f1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f1_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_c57341dd645988f6" f2_base ::
     IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @attributes\/visibility_attributes.h:19:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f2@
-}
f2 ::
     IO ()
f2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f2_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_5858f33f5b12f541" f3_base ::
     IO ()

{-| __C declaration:__ @f3@

    __defined at:__ @attributes\/visibility_attributes.h:20:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f3@
-}
f3 ::
     IO ()
f3 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f3_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_d57b75423078a644" f4_base ::
     IO ()

{-| __C declaration:__ @f4@

    __defined at:__ @attributes\/visibility_attributes.h:21:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f4@
-}
f4 ::
     IO ()
f4 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f4_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_d7e35ffb8ef15009" f5_base ::
     IO ()

{-| __C declaration:__ @f5@

    __defined at:__ @attributes\/visibility_attributes.h:24:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f5@
-}
f5 ::
     IO ()
f5 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f5_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_de345dceb6694e2e" f6_base ::
     IO ()

{-| __C declaration:__ @f6@

    __defined at:__ @attributes\/visibility_attributes.h:25:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f6@
-}
f6 ::
     IO ()
f6 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f6_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_df2fd4ea47789bb8" f7_base ::
     IO ()

{-| __C declaration:__ @f7@

    __defined at:__ @attributes\/visibility_attributes.h:26:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f7@
-}
f7 ::
     IO ()
f7 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f7_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a49e6066d8dd0628" f8_base ::
     IO ()

{-| __C declaration:__ @f8@

    __defined at:__ @attributes\/visibility_attributes.h:27:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f8@
-}
f8 ::
     IO ()
f8 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f8_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_e94ceb5e10d07a59" f9_base ::
     IO ()

{-| __C declaration:__ @f9@

    __defined at:__ @attributes\/visibility_attributes.h:28:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f9@
-}
f9 ::
     IO ()
f9 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f9_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_d329fc979eb3d29e" f10_base ::
     IO ()

{-| __C declaration:__ @f10@

    __defined at:__ @attributes\/visibility_attributes.h:31:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f10@
-}
f10 ::
     IO ()
f10 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f10_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_9f7176bfdf1871cf" f11_base ::
     IO ()

{-| __C declaration:__ @f11@

    __defined at:__ @attributes\/visibility_attributes.h:32:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f11@
-}
f11 ::
     IO ()
f11 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f11_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_d06a71df91d678f1" f12_base ::
     IO ()

{-| __C declaration:__ @f12@

    __defined at:__ @attributes\/visibility_attributes.h:33:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f12@
-}
f12 ::
     IO ()
f12 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f12_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_32916e0b4775516d" f13_base ::
     IO ()

{-| __C declaration:__ @f13@

    __defined at:__ @attributes\/visibility_attributes.h:34:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f13@
-}
f13 ::
     IO ()
f13 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f13_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_56f26ce5de7906a8" f14_base ::
     IO ()

{-| __C declaration:__ @f14@

    __defined at:__ @attributes\/visibility_attributes.h:35:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f14@
-}
f14 ::
     IO ()
f14 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f14_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_fd2972764f48a143" f15_base ::
     IO ()

{-| __C declaration:__ @f15@

    __defined at:__ @attributes\/visibility_attributes.h:38:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f15@
-}
f15 ::
     IO ()
f15 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f15_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f63a042e539ff8b6" f16_base ::
     IO ()

{-| __C declaration:__ @f16@

    __defined at:__ @attributes\/visibility_attributes.h:39:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f16@
-}
f16 ::
     IO ()
f16 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f16_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_1ae5adf1961e06f6" f17_base ::
     IO ()

{-| __C declaration:__ @f17@

    __defined at:__ @attributes\/visibility_attributes.h:40:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f17@
-}
f17 ::
     IO ()
f17 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f17_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_cf7978ab53dc8140" f18_base ::
     IO ()

{-| __C declaration:__ @f18@

    __defined at:__ @attributes\/visibility_attributes.h:41:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f18@
-}
f18 ::
     IO ()
f18 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f18_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f81ec562e6b7dc0f" f19_base ::
     IO ()

{-| __C declaration:__ @f19@

    __defined at:__ @attributes\/visibility_attributes.h:42:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f19@
-}
f19 ::
     IO ()
f19 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f19_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_95b4b9ea66f61551" f20_base ::
     IO ()

{-| __C declaration:__ @f20@

    __defined at:__ @attributes\/visibility_attributes.h:45:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f20@
-}
f20 ::
     IO ()
f20 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f20_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_c64dd47543cb7378" f21_base ::
     IO ()

{-| __C declaration:__ @f21@

    __defined at:__ @attributes\/visibility_attributes.h:46:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f21@
-}
f21 ::
     IO ()
f21 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f21_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_ce27e3f019c9d235" f22_base ::
     IO ()

{-| __C declaration:__ @f22@

    __defined at:__ @attributes\/visibility_attributes.h:47:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f22@
-}
f22 ::
     IO ()
f22 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f22_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_ae8f854ac4a07381" f23_base ::
     IO ()

{-| __C declaration:__ @f23@

    __defined at:__ @attributes\/visibility_attributes.h:48:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f23@
-}
f23 ::
     IO ()
f23 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f23_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_d4585a283ed9fc36" f24_base ::
     IO ()

{-| __C declaration:__ @f24@

    __defined at:__ @attributes\/visibility_attributes.h:49:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f24@
-}
f24 ::
     IO ()
f24 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f24_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f600df85706a5312" f25_base ::
     IO ()

{-| __C declaration:__ @f25@

    __defined at:__ @attributes\/visibility_attributes.h:52:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f25@
-}
f25 ::
     IO ()
f25 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f25_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_92731c80b3281e1c" f26_base ::
     IO ()

{-| __C declaration:__ @f26@

    __defined at:__ @attributes\/visibility_attributes.h:53:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f26@
-}
f26 ::
     IO ()
f26 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f26_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_930247d3730559af" f27_base ::
     IO ()

{-| __C declaration:__ @f27@

    __defined at:__ @attributes\/visibility_attributes.h:54:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f27@
-}
f27 ::
     IO ()
f27 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f27_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_2d1ce37b69d8d467" f28_base ::
     IO ()

{-| __C declaration:__ @f28@

    __defined at:__ @attributes\/visibility_attributes.h:55:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f28@
-}
f28 ::
     IO ()
f28 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f28_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_3f89a86cfcdf475d" f29_base ::
     IO ()

{-| __C declaration:__ @f29@

    __defined at:__ @attributes\/visibility_attributes.h:56:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f29@
-}
f29 ::
     IO ()
f29 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f29_base
