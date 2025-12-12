{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <attributes/visibility_attributes.h>"
  , "void hs_bindgen_e64a83c5f7f51679 (void)"
  , "{"
  , "  f0();"
  , "}"
  , "void hs_bindgen_8881b0f4ce94e440 (void)"
  , "{"
  , "  f1();"
  , "}"
  , "void hs_bindgen_4ede7f4c96b4d1b5 (void)"
  , "{"
  , "  f2();"
  , "}"
  , "void hs_bindgen_018f1e15132ff973 (void)"
  , "{"
  , "  f3();"
  , "}"
  , "void hs_bindgen_9d93cf33b1a833d1 (void)"
  , "{"
  , "  f4();"
  , "}"
  , "void hs_bindgen_959298c4e5cb061a (void)"
  , "{"
  , "  f5();"
  , "}"
  , "void hs_bindgen_218297218a4953d6 (void)"
  , "{"
  , "  f6();"
  , "}"
  , "void hs_bindgen_df320fe3d4683ff9 (void)"
  , "{"
  , "  f7();"
  , "}"
  , "void hs_bindgen_cd613f46d2e06e18 (void)"
  , "{"
  , "  f8();"
  , "}"
  , "void hs_bindgen_67774b3f22bd7286 (void)"
  , "{"
  , "  f9();"
  , "}"
  , "void hs_bindgen_dbbd2e0417380ce9 (void)"
  , "{"
  , "  f10();"
  , "}"
  , "void hs_bindgen_9d8fadad3f85e1fc (void)"
  , "{"
  , "  f11();"
  , "}"
  , "void hs_bindgen_558a7ef50878f4b2 (void)"
  , "{"
  , "  f12();"
  , "}"
  , "void hs_bindgen_30add751959aac79 (void)"
  , "{"
  , "  f13();"
  , "}"
  , "void hs_bindgen_06b989df2d3622ad (void)"
  , "{"
  , "  f14();"
  , "}"
  , "void hs_bindgen_11627343b78e6e76 (void)"
  , "{"
  , "  f15();"
  , "}"
  , "void hs_bindgen_5d3aa9a924674163 (void)"
  , "{"
  , "  f16();"
  , "}"
  , "void hs_bindgen_65abad67aeb6b955 (void)"
  , "{"
  , "  f17();"
  , "}"
  , "void hs_bindgen_0350778b298751b2 (void)"
  , "{"
  , "  f18();"
  , "}"
  , "void hs_bindgen_aacba18c2fb3dae1 (void)"
  , "{"
  , "  f19();"
  , "}"
  , "void hs_bindgen_023f4d5d2a56d2e9 (void)"
  , "{"
  , "  f20();"
  , "}"
  , "void hs_bindgen_3a70db9f9e280b85 (void)"
  , "{"
  , "  f21();"
  , "}"
  , "void hs_bindgen_a4fc9586b7510ea6 (void)"
  , "{"
  , "  f22();"
  , "}"
  , "void hs_bindgen_c25df69a5f23a9b9 (void)"
  , "{"
  , "  f23();"
  , "}"
  , "void hs_bindgen_909c5201154b4617 (void)"
  , "{"
  , "  f24();"
  , "}"
  , "void hs_bindgen_dfbd362260cd0fba (void)"
  , "{"
  , "  f25();"
  , "}"
  , "void hs_bindgen_d9c4d8bc1b7d8a59 (void)"
  , "{"
  , "  f26();"
  , "}"
  , "void hs_bindgen_71a726a6d6e62f14 (void)"
  , "{"
  , "  f27();"
  , "}"
  , "void hs_bindgen_70f35808778a1423 (void)"
  , "{"
  , "  f28();"
  , "}"
  , "void hs_bindgen_7ba7293cedb52447 (void)"
  , "{"
  , "  f29();"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_e64a83c5f7f51679" f0_base ::
     IO ()

{-| __C declaration:__ @f0@

    __defined at:__ @attributes\/visibility_attributes.h:17:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f0@
-}
f0 ::
     IO ()
f0 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f0_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_8881b0f4ce94e440" f1_base ::
     IO ()

{-| __C declaration:__ @f1@

    __defined at:__ @attributes\/visibility_attributes.h:18:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f1@
-}
f1 ::
     IO ()
f1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f1_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_4ede7f4c96b4d1b5" f2_base ::
     IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @attributes\/visibility_attributes.h:19:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f2@
-}
f2 ::
     IO ()
f2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f2_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_018f1e15132ff973" f3_base ::
     IO ()

{-| __C declaration:__ @f3@

    __defined at:__ @attributes\/visibility_attributes.h:20:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f3@
-}
f3 ::
     IO ()
f3 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f3_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_9d93cf33b1a833d1" f4_base ::
     IO ()

{-| __C declaration:__ @f4@

    __defined at:__ @attributes\/visibility_attributes.h:21:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f4@
-}
f4 ::
     IO ()
f4 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f4_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_959298c4e5cb061a" f5_base ::
     IO ()

{-| __C declaration:__ @f5@

    __defined at:__ @attributes\/visibility_attributes.h:24:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f5@
-}
f5 ::
     IO ()
f5 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f5_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_218297218a4953d6" f6_base ::
     IO ()

{-| __C declaration:__ @f6@

    __defined at:__ @attributes\/visibility_attributes.h:25:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f6@
-}
f6 ::
     IO ()
f6 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f6_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_df320fe3d4683ff9" f7_base ::
     IO ()

{-| __C declaration:__ @f7@

    __defined at:__ @attributes\/visibility_attributes.h:26:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f7@
-}
f7 ::
     IO ()
f7 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f7_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_cd613f46d2e06e18" f8_base ::
     IO ()

{-| __C declaration:__ @f8@

    __defined at:__ @attributes\/visibility_attributes.h:27:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f8@
-}
f8 ::
     IO ()
f8 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f8_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_67774b3f22bd7286" f9_base ::
     IO ()

{-| __C declaration:__ @f9@

    __defined at:__ @attributes\/visibility_attributes.h:28:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f9@
-}
f9 ::
     IO ()
f9 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f9_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_dbbd2e0417380ce9" f10_base ::
     IO ()

{-| __C declaration:__ @f10@

    __defined at:__ @attributes\/visibility_attributes.h:31:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f10@
-}
f10 ::
     IO ()
f10 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f10_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_9d8fadad3f85e1fc" f11_base ::
     IO ()

{-| __C declaration:__ @f11@

    __defined at:__ @attributes\/visibility_attributes.h:32:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f11@
-}
f11 ::
     IO ()
f11 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f11_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_558a7ef50878f4b2" f12_base ::
     IO ()

{-| __C declaration:__ @f12@

    __defined at:__ @attributes\/visibility_attributes.h:33:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f12@
-}
f12 ::
     IO ()
f12 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f12_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_30add751959aac79" f13_base ::
     IO ()

{-| __C declaration:__ @f13@

    __defined at:__ @attributes\/visibility_attributes.h:34:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f13@
-}
f13 ::
     IO ()
f13 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f13_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_06b989df2d3622ad" f14_base ::
     IO ()

{-| __C declaration:__ @f14@

    __defined at:__ @attributes\/visibility_attributes.h:35:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f14@
-}
f14 ::
     IO ()
f14 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f14_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_11627343b78e6e76" f15_base ::
     IO ()

{-| __C declaration:__ @f15@

    __defined at:__ @attributes\/visibility_attributes.h:38:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f15@
-}
f15 ::
     IO ()
f15 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f15_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_5d3aa9a924674163" f16_base ::
     IO ()

{-| __C declaration:__ @f16@

    __defined at:__ @attributes\/visibility_attributes.h:39:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f16@
-}
f16 ::
     IO ()
f16 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f16_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_65abad67aeb6b955" f17_base ::
     IO ()

{-| __C declaration:__ @f17@

    __defined at:__ @attributes\/visibility_attributes.h:40:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f17@
-}
f17 ::
     IO ()
f17 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f17_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_0350778b298751b2" f18_base ::
     IO ()

{-| __C declaration:__ @f18@

    __defined at:__ @attributes\/visibility_attributes.h:41:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f18@
-}
f18 ::
     IO ()
f18 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f18_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_aacba18c2fb3dae1" f19_base ::
     IO ()

{-| __C declaration:__ @f19@

    __defined at:__ @attributes\/visibility_attributes.h:42:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f19@
-}
f19 ::
     IO ()
f19 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f19_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_023f4d5d2a56d2e9" f20_base ::
     IO ()

{-| __C declaration:__ @f20@

    __defined at:__ @attributes\/visibility_attributes.h:45:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f20@
-}
f20 ::
     IO ()
f20 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f20_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_3a70db9f9e280b85" f21_base ::
     IO ()

{-| __C declaration:__ @f21@

    __defined at:__ @attributes\/visibility_attributes.h:46:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f21@
-}
f21 ::
     IO ()
f21 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f21_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_a4fc9586b7510ea6" f22_base ::
     IO ()

{-| __C declaration:__ @f22@

    __defined at:__ @attributes\/visibility_attributes.h:47:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f22@
-}
f22 ::
     IO ()
f22 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f22_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_c25df69a5f23a9b9" f23_base ::
     IO ()

{-| __C declaration:__ @f23@

    __defined at:__ @attributes\/visibility_attributes.h:48:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f23@
-}
f23 ::
     IO ()
f23 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f23_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_909c5201154b4617" f24_base ::
     IO ()

{-| __C declaration:__ @f24@

    __defined at:__ @attributes\/visibility_attributes.h:49:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f24@
-}
f24 ::
     IO ()
f24 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f24_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_dfbd362260cd0fba" f25_base ::
     IO ()

{-| __C declaration:__ @f25@

    __defined at:__ @attributes\/visibility_attributes.h:52:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f25@
-}
f25 ::
     IO ()
f25 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f25_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_d9c4d8bc1b7d8a59" f26_base ::
     IO ()

{-| __C declaration:__ @f26@

    __defined at:__ @attributes\/visibility_attributes.h:53:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f26@
-}
f26 ::
     IO ()
f26 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f26_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_71a726a6d6e62f14" f27_base ::
     IO ()

{-| __C declaration:__ @f27@

    __defined at:__ @attributes\/visibility_attributes.h:54:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f27@
-}
f27 ::
     IO ()
f27 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f27_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_70f35808778a1423" f28_base ::
     IO ()

{-| __C declaration:__ @f28@

    __defined at:__ @attributes\/visibility_attributes.h:55:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f28@
-}
f28 ::
     IO ()
f28 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f28_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_7ba7293cedb52447" f29_base ::
     IO ()

{-| __C declaration:__ @f29@

    __defined at:__ @attributes\/visibility_attributes.h:56:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f29@
-}
f29 ::
     IO ()
f29 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f29_base
