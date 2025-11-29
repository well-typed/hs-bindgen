{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <attributes/visibility_attributes.h>"
  , "void hs_bindgen_test_attributesvisibility_attribut_364a9ac67a1b8d06 (void)"
  , "{"
  , "  f0();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_b2cefd7907644d46 (void)"
  , "{"
  , "  f1();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_a58b72650fa977ee (void)"
  , "{"
  , "  f2();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_c1345b7446cf5382 (void)"
  , "{"
  , "  f3();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_75c5664e6786046a (void)"
  , "{"
  , "  f4();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_9f844896f3f3c88d (void)"
  , "{"
  , "  f5();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_e0a5a4fc19b62599 (void)"
  , "{"
  , "  f6();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_70c5869247d872a8 (void)"
  , "{"
  , "  f7();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_b8e7d5bf68f1589c (void)"
  , "{"
  , "  f8();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_66f32c3802e45f5f (void)"
  , "{"
  , "  f9();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_aef805cd02ad9a2d (void)"
  , "{"
  , "  f10();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_7125958a0c9937d0 (void)"
  , "{"
  , "  f11();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_db77085c34364d28 (void)"
  , "{"
  , "  f12();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_1d5ca648793dd55d (void)"
  , "{"
  , "  f13();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_d7419a7a0f54a318 (void)"
  , "{"
  , "  f14();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_c13c7caa7b7975bd (void)"
  , "{"
  , "  f15();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_afddd756445c1886 (void)"
  , "{"
  , "  f16();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_0251339797426aeb (void)"
  , "{"
  , "  f17();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_53bad33fe3385f43 (void)"
  , "{"
  , "  f18();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_a590a0ef7d59299a (void)"
  , "{"
  , "  f19();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_d59767bb5809d130 (void)"
  , "{"
  , "  f20();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_d760cb7b056d69f0 (void)"
  , "{"
  , "  f21();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_a6adada742441949 (void)"
  , "{"
  , "  f22();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_e047ca4bd129798b (void)"
  , "{"
  , "  f23();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_f9c731dd203c804b (void)"
  , "{"
  , "  f24();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_670f0d85ee1a598f (void)"
  , "{"
  , "  f25();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_bb1d2ccfd304cf16 (void)"
  , "{"
  , "  f26();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_18d98000751a226a (void)"
  , "{"
  , "  f27();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_1a287b5eef6bd2d5 (void)"
  , "{"
  , "  f28();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_66d5d89f59cefc22 (void)"
  , "{"
  , "  f29();"
  , "}"
  ]))

{-| __C declaration:__ @f0@

    __defined at:__ @attributes\/visibility_attributes.h:17:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f0@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_364a9ac67a1b8d06" f0 ::
     IO ()

{-| __C declaration:__ @f1@

    __defined at:__ @attributes\/visibility_attributes.h:18:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f1@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_b2cefd7907644d46" f1 ::
     IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @attributes\/visibility_attributes.h:19:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f2@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_a58b72650fa977ee" f2 ::
     IO ()

{-| __C declaration:__ @f3@

    __defined at:__ @attributes\/visibility_attributes.h:20:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f3@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_c1345b7446cf5382" f3 ::
     IO ()

{-| __C declaration:__ @f4@

    __defined at:__ @attributes\/visibility_attributes.h:21:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f4@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_75c5664e6786046a" f4 ::
     IO ()

{-| __C declaration:__ @f5@

    __defined at:__ @attributes\/visibility_attributes.h:24:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f5@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_9f844896f3f3c88d" f5 ::
     IO ()

{-| __C declaration:__ @f6@

    __defined at:__ @attributes\/visibility_attributes.h:25:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f6@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_e0a5a4fc19b62599" f6 ::
     IO ()

{-| __C declaration:__ @f7@

    __defined at:__ @attributes\/visibility_attributes.h:26:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f7@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_70c5869247d872a8" f7 ::
     IO ()

{-| __C declaration:__ @f8@

    __defined at:__ @attributes\/visibility_attributes.h:27:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f8@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_b8e7d5bf68f1589c" f8 ::
     IO ()

{-| __C declaration:__ @f9@

    __defined at:__ @attributes\/visibility_attributes.h:28:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f9@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_66f32c3802e45f5f" f9 ::
     IO ()

{-| __C declaration:__ @f10@

    __defined at:__ @attributes\/visibility_attributes.h:31:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f10@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_aef805cd02ad9a2d" f10 ::
     IO ()

{-| __C declaration:__ @f11@

    __defined at:__ @attributes\/visibility_attributes.h:32:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f11@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_7125958a0c9937d0" f11 ::
     IO ()

{-| __C declaration:__ @f12@

    __defined at:__ @attributes\/visibility_attributes.h:33:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f12@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_db77085c34364d28" f12 ::
     IO ()

{-| __C declaration:__ @f13@

    __defined at:__ @attributes\/visibility_attributes.h:34:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f13@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_1d5ca648793dd55d" f13 ::
     IO ()

{-| __C declaration:__ @f14@

    __defined at:__ @attributes\/visibility_attributes.h:35:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f14@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_d7419a7a0f54a318" f14 ::
     IO ()

{-| __C declaration:__ @f15@

    __defined at:__ @attributes\/visibility_attributes.h:38:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f15@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_c13c7caa7b7975bd" f15 ::
     IO ()

{-| __C declaration:__ @f16@

    __defined at:__ @attributes\/visibility_attributes.h:39:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f16@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_afddd756445c1886" f16 ::
     IO ()

{-| __C declaration:__ @f17@

    __defined at:__ @attributes\/visibility_attributes.h:40:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f17@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_0251339797426aeb" f17 ::
     IO ()

{-| __C declaration:__ @f18@

    __defined at:__ @attributes\/visibility_attributes.h:41:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f18@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_53bad33fe3385f43" f18 ::
     IO ()

{-| __C declaration:__ @f19@

    __defined at:__ @attributes\/visibility_attributes.h:42:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f19@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_a590a0ef7d59299a" f19 ::
     IO ()

{-| __C declaration:__ @f20@

    __defined at:__ @attributes\/visibility_attributes.h:45:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f20@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_d59767bb5809d130" f20 ::
     IO ()

{-| __C declaration:__ @f21@

    __defined at:__ @attributes\/visibility_attributes.h:46:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f21@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_d760cb7b056d69f0" f21 ::
     IO ()

{-| __C declaration:__ @f22@

    __defined at:__ @attributes\/visibility_attributes.h:47:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f22@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_a6adada742441949" f22 ::
     IO ()

{-| __C declaration:__ @f23@

    __defined at:__ @attributes\/visibility_attributes.h:48:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f23@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_e047ca4bd129798b" f23 ::
     IO ()

{-| __C declaration:__ @f24@

    __defined at:__ @attributes\/visibility_attributes.h:49:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f24@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_f9c731dd203c804b" f24 ::
     IO ()

{-| __C declaration:__ @f25@

    __defined at:__ @attributes\/visibility_attributes.h:52:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f25@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_670f0d85ee1a598f" f25 ::
     IO ()

{-| __C declaration:__ @f26@

    __defined at:__ @attributes\/visibility_attributes.h:53:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f26@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_bb1d2ccfd304cf16" f26 ::
     IO ()

{-| __C declaration:__ @f27@

    __defined at:__ @attributes\/visibility_attributes.h:54:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f27@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_18d98000751a226a" f27 ::
     IO ()

{-| __C declaration:__ @f28@

    __defined at:__ @attributes\/visibility_attributes.h:55:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f28@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_1a287b5eef6bd2d5" f28 ::
     IO ()

{-| __C declaration:__ @f29@

    __defined at:__ @attributes\/visibility_attributes.h:56:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Safe_f29@
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_66d5d89f59cefc22" f29 ::
     IO ()
