{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

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

{-| __C declaration:__ @f0@

    __defined at:__ @attributes\/visibility_attributes.h:17:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f0@
-}
foreign import ccall unsafe "hs_bindgen_df56d82c9186c794" f0 ::
     IO ()

{-| __C declaration:__ @f1@

    __defined at:__ @attributes\/visibility_attributes.h:18:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f1@
-}
foreign import ccall unsafe "hs_bindgen_2e25b1147da3cdee" f1 ::
     IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @attributes\/visibility_attributes.h:19:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f2@
-}
foreign import ccall unsafe "hs_bindgen_c57341dd645988f6" f2 ::
     IO ()

{-| __C declaration:__ @f3@

    __defined at:__ @attributes\/visibility_attributes.h:20:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f3@
-}
foreign import ccall unsafe "hs_bindgen_5858f33f5b12f541" f3 ::
     IO ()

{-| __C declaration:__ @f4@

    __defined at:__ @attributes\/visibility_attributes.h:21:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f4@
-}
foreign import ccall unsafe "hs_bindgen_d57b75423078a644" f4 ::
     IO ()

{-| __C declaration:__ @f5@

    __defined at:__ @attributes\/visibility_attributes.h:24:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f5@
-}
foreign import ccall unsafe "hs_bindgen_d7e35ffb8ef15009" f5 ::
     IO ()

{-| __C declaration:__ @f6@

    __defined at:__ @attributes\/visibility_attributes.h:25:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f6@
-}
foreign import ccall unsafe "hs_bindgen_de345dceb6694e2e" f6 ::
     IO ()

{-| __C declaration:__ @f7@

    __defined at:__ @attributes\/visibility_attributes.h:26:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f7@
-}
foreign import ccall unsafe "hs_bindgen_df2fd4ea47789bb8" f7 ::
     IO ()

{-| __C declaration:__ @f8@

    __defined at:__ @attributes\/visibility_attributes.h:27:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f8@
-}
foreign import ccall unsafe "hs_bindgen_a49e6066d8dd0628" f8 ::
     IO ()

{-| __C declaration:__ @f9@

    __defined at:__ @attributes\/visibility_attributes.h:28:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f9@
-}
foreign import ccall unsafe "hs_bindgen_e94ceb5e10d07a59" f9 ::
     IO ()

{-| __C declaration:__ @f10@

    __defined at:__ @attributes\/visibility_attributes.h:31:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f10@
-}
foreign import ccall unsafe "hs_bindgen_d329fc979eb3d29e" f10 ::
     IO ()

{-| __C declaration:__ @f11@

    __defined at:__ @attributes\/visibility_attributes.h:32:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f11@
-}
foreign import ccall unsafe "hs_bindgen_9f7176bfdf1871cf" f11 ::
     IO ()

{-| __C declaration:__ @f12@

    __defined at:__ @attributes\/visibility_attributes.h:33:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f12@
-}
foreign import ccall unsafe "hs_bindgen_d06a71df91d678f1" f12 ::
     IO ()

{-| __C declaration:__ @f13@

    __defined at:__ @attributes\/visibility_attributes.h:34:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f13@
-}
foreign import ccall unsafe "hs_bindgen_32916e0b4775516d" f13 ::
     IO ()

{-| __C declaration:__ @f14@

    __defined at:__ @attributes\/visibility_attributes.h:35:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f14@
-}
foreign import ccall unsafe "hs_bindgen_56f26ce5de7906a8" f14 ::
     IO ()

{-| __C declaration:__ @f15@

    __defined at:__ @attributes\/visibility_attributes.h:38:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f15@
-}
foreign import ccall unsafe "hs_bindgen_fd2972764f48a143" f15 ::
     IO ()

{-| __C declaration:__ @f16@

    __defined at:__ @attributes\/visibility_attributes.h:39:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f16@
-}
foreign import ccall unsafe "hs_bindgen_f63a042e539ff8b6" f16 ::
     IO ()

{-| __C declaration:__ @f17@

    __defined at:__ @attributes\/visibility_attributes.h:40:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f17@
-}
foreign import ccall unsafe "hs_bindgen_1ae5adf1961e06f6" f17 ::
     IO ()

{-| __C declaration:__ @f18@

    __defined at:__ @attributes\/visibility_attributes.h:41:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f18@
-}
foreign import ccall unsafe "hs_bindgen_cf7978ab53dc8140" f18 ::
     IO ()

{-| __C declaration:__ @f19@

    __defined at:__ @attributes\/visibility_attributes.h:42:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f19@
-}
foreign import ccall unsafe "hs_bindgen_f81ec562e6b7dc0f" f19 ::
     IO ()

{-| __C declaration:__ @f20@

    __defined at:__ @attributes\/visibility_attributes.h:45:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f20@
-}
foreign import ccall unsafe "hs_bindgen_95b4b9ea66f61551" f20 ::
     IO ()

{-| __C declaration:__ @f21@

    __defined at:__ @attributes\/visibility_attributes.h:46:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f21@
-}
foreign import ccall unsafe "hs_bindgen_c64dd47543cb7378" f21 ::
     IO ()

{-| __C declaration:__ @f22@

    __defined at:__ @attributes\/visibility_attributes.h:47:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f22@
-}
foreign import ccall unsafe "hs_bindgen_ce27e3f019c9d235" f22 ::
     IO ()

{-| __C declaration:__ @f23@

    __defined at:__ @attributes\/visibility_attributes.h:48:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f23@
-}
foreign import ccall unsafe "hs_bindgen_ae8f854ac4a07381" f23 ::
     IO ()

{-| __C declaration:__ @f24@

    __defined at:__ @attributes\/visibility_attributes.h:49:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f24@
-}
foreign import ccall unsafe "hs_bindgen_d4585a283ed9fc36" f24 ::
     IO ()

{-| __C declaration:__ @f25@

    __defined at:__ @attributes\/visibility_attributes.h:52:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f25@
-}
foreign import ccall unsafe "hs_bindgen_f600df85706a5312" f25 ::
     IO ()

{-| __C declaration:__ @f26@

    __defined at:__ @attributes\/visibility_attributes.h:53:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f26@
-}
foreign import ccall unsafe "hs_bindgen_92731c80b3281e1c" f26 ::
     IO ()

{-| __C declaration:__ @f27@

    __defined at:__ @attributes\/visibility_attributes.h:54:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f27@
-}
foreign import ccall unsafe "hs_bindgen_930247d3730559af" f27 ::
     IO ()

{-| __C declaration:__ @f28@

    __defined at:__ @attributes\/visibility_attributes.h:55:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f28@
-}
foreign import ccall unsafe "hs_bindgen_2d1ce37b69d8d467" f28 ::
     IO ()

{-| __C declaration:__ @f29@

    __defined at:__ @attributes\/visibility_attributes.h:56:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Unsafe_f29@
-}
foreign import ccall unsafe "hs_bindgen_3f89a86cfcdf475d" f29 ::
     IO ()
