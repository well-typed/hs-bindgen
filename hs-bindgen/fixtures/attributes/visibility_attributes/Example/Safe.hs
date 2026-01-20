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

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f0@
foreign import ccall safe "hs_bindgen_e64a83c5f7f51679" hs_bindgen_e64a83c5f7f51679_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f0@
hs_bindgen_e64a83c5f7f51679 :: IO ()
hs_bindgen_e64a83c5f7f51679 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_e64a83c5f7f51679_base

{-| __C declaration:__ @f0@

    __defined at:__ @attributes\/visibility_attributes.h 17:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f0 :: IO ()
f0 = hs_bindgen_e64a83c5f7f51679

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f1@
foreign import ccall safe "hs_bindgen_8881b0f4ce94e440" hs_bindgen_8881b0f4ce94e440_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f1@
hs_bindgen_8881b0f4ce94e440 :: IO ()
hs_bindgen_8881b0f4ce94e440 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_8881b0f4ce94e440_base

{-| __C declaration:__ @f1@

    __defined at:__ @attributes\/visibility_attributes.h 18:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f1 :: IO ()
f1 = hs_bindgen_8881b0f4ce94e440

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f2@
foreign import ccall safe "hs_bindgen_4ede7f4c96b4d1b5" hs_bindgen_4ede7f4c96b4d1b5_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f2@
hs_bindgen_4ede7f4c96b4d1b5 :: IO ()
hs_bindgen_4ede7f4c96b4d1b5 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4ede7f4c96b4d1b5_base

{-| __C declaration:__ @f2@

    __defined at:__ @attributes\/visibility_attributes.h 19:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f2 :: IO ()
f2 = hs_bindgen_4ede7f4c96b4d1b5

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f3@
foreign import ccall safe "hs_bindgen_018f1e15132ff973" hs_bindgen_018f1e15132ff973_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f3@
hs_bindgen_018f1e15132ff973 :: IO ()
hs_bindgen_018f1e15132ff973 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_018f1e15132ff973_base

{-| __C declaration:__ @f3@

    __defined at:__ @attributes\/visibility_attributes.h 20:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f3 :: IO ()
f3 = hs_bindgen_018f1e15132ff973

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f4@
foreign import ccall safe "hs_bindgen_9d93cf33b1a833d1" hs_bindgen_9d93cf33b1a833d1_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f4@
hs_bindgen_9d93cf33b1a833d1 :: IO ()
hs_bindgen_9d93cf33b1a833d1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_9d93cf33b1a833d1_base

{-| __C declaration:__ @f4@

    __defined at:__ @attributes\/visibility_attributes.h 21:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f4 :: IO ()
f4 = hs_bindgen_9d93cf33b1a833d1

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f5@
foreign import ccall safe "hs_bindgen_959298c4e5cb061a" hs_bindgen_959298c4e5cb061a_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f5@
hs_bindgen_959298c4e5cb061a :: IO ()
hs_bindgen_959298c4e5cb061a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_959298c4e5cb061a_base

{-| __C declaration:__ @f5@

    __defined at:__ @attributes\/visibility_attributes.h 24:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f5 :: IO ()
f5 = hs_bindgen_959298c4e5cb061a

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f6@
foreign import ccall safe "hs_bindgen_218297218a4953d6" hs_bindgen_218297218a4953d6_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f6@
hs_bindgen_218297218a4953d6 :: IO ()
hs_bindgen_218297218a4953d6 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_218297218a4953d6_base

{-| __C declaration:__ @f6@

    __defined at:__ @attributes\/visibility_attributes.h 25:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f6 :: IO ()
f6 = hs_bindgen_218297218a4953d6

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f7@
foreign import ccall safe "hs_bindgen_df320fe3d4683ff9" hs_bindgen_df320fe3d4683ff9_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f7@
hs_bindgen_df320fe3d4683ff9 :: IO ()
hs_bindgen_df320fe3d4683ff9 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_df320fe3d4683ff9_base

{-| __C declaration:__ @f7@

    __defined at:__ @attributes\/visibility_attributes.h 26:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f7 :: IO ()
f7 = hs_bindgen_df320fe3d4683ff9

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f8@
foreign import ccall safe "hs_bindgen_cd613f46d2e06e18" hs_bindgen_cd613f46d2e06e18_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f8@
hs_bindgen_cd613f46d2e06e18 :: IO ()
hs_bindgen_cd613f46d2e06e18 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_cd613f46d2e06e18_base

{-| __C declaration:__ @f8@

    __defined at:__ @attributes\/visibility_attributes.h 27:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f8 :: IO ()
f8 = hs_bindgen_cd613f46d2e06e18

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f9@
foreign import ccall safe "hs_bindgen_67774b3f22bd7286" hs_bindgen_67774b3f22bd7286_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f9@
hs_bindgen_67774b3f22bd7286 :: IO ()
hs_bindgen_67774b3f22bd7286 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_67774b3f22bd7286_base

{-| __C declaration:__ @f9@

    __defined at:__ @attributes\/visibility_attributes.h 28:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f9 :: IO ()
f9 = hs_bindgen_67774b3f22bd7286

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f10@
foreign import ccall safe "hs_bindgen_dbbd2e0417380ce9" hs_bindgen_dbbd2e0417380ce9_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f10@
hs_bindgen_dbbd2e0417380ce9 :: IO ()
hs_bindgen_dbbd2e0417380ce9 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_dbbd2e0417380ce9_base

{-| __C declaration:__ @f10@

    __defined at:__ @attributes\/visibility_attributes.h 31:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f10 :: IO ()
f10 = hs_bindgen_dbbd2e0417380ce9

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f11@
foreign import ccall safe "hs_bindgen_9d8fadad3f85e1fc" hs_bindgen_9d8fadad3f85e1fc_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f11@
hs_bindgen_9d8fadad3f85e1fc :: IO ()
hs_bindgen_9d8fadad3f85e1fc =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_9d8fadad3f85e1fc_base

{-| __C declaration:__ @f11@

    __defined at:__ @attributes\/visibility_attributes.h 32:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f11 :: IO ()
f11 = hs_bindgen_9d8fadad3f85e1fc

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f12@
foreign import ccall safe "hs_bindgen_558a7ef50878f4b2" hs_bindgen_558a7ef50878f4b2_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f12@
hs_bindgen_558a7ef50878f4b2 :: IO ()
hs_bindgen_558a7ef50878f4b2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_558a7ef50878f4b2_base

{-| __C declaration:__ @f12@

    __defined at:__ @attributes\/visibility_attributes.h 33:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f12 :: IO ()
f12 = hs_bindgen_558a7ef50878f4b2

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f13@
foreign import ccall safe "hs_bindgen_30add751959aac79" hs_bindgen_30add751959aac79_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f13@
hs_bindgen_30add751959aac79 :: IO ()
hs_bindgen_30add751959aac79 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_30add751959aac79_base

{-| __C declaration:__ @f13@

    __defined at:__ @attributes\/visibility_attributes.h 34:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f13 :: IO ()
f13 = hs_bindgen_30add751959aac79

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f14@
foreign import ccall safe "hs_bindgen_06b989df2d3622ad" hs_bindgen_06b989df2d3622ad_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f14@
hs_bindgen_06b989df2d3622ad :: IO ()
hs_bindgen_06b989df2d3622ad =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_06b989df2d3622ad_base

{-| __C declaration:__ @f14@

    __defined at:__ @attributes\/visibility_attributes.h 35:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f14 :: IO ()
f14 = hs_bindgen_06b989df2d3622ad

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f15@
foreign import ccall safe "hs_bindgen_11627343b78e6e76" hs_bindgen_11627343b78e6e76_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f15@
hs_bindgen_11627343b78e6e76 :: IO ()
hs_bindgen_11627343b78e6e76 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_11627343b78e6e76_base

{-| __C declaration:__ @f15@

    __defined at:__ @attributes\/visibility_attributes.h 38:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f15 :: IO ()
f15 = hs_bindgen_11627343b78e6e76

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f16@
foreign import ccall safe "hs_bindgen_5d3aa9a924674163" hs_bindgen_5d3aa9a924674163_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f16@
hs_bindgen_5d3aa9a924674163 :: IO ()
hs_bindgen_5d3aa9a924674163 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_5d3aa9a924674163_base

{-| __C declaration:__ @f16@

    __defined at:__ @attributes\/visibility_attributes.h 39:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f16 :: IO ()
f16 = hs_bindgen_5d3aa9a924674163

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f17@
foreign import ccall safe "hs_bindgen_65abad67aeb6b955" hs_bindgen_65abad67aeb6b955_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f17@
hs_bindgen_65abad67aeb6b955 :: IO ()
hs_bindgen_65abad67aeb6b955 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_65abad67aeb6b955_base

{-| __C declaration:__ @f17@

    __defined at:__ @attributes\/visibility_attributes.h 40:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f17 :: IO ()
f17 = hs_bindgen_65abad67aeb6b955

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f18@
foreign import ccall safe "hs_bindgen_0350778b298751b2" hs_bindgen_0350778b298751b2_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f18@
hs_bindgen_0350778b298751b2 :: IO ()
hs_bindgen_0350778b298751b2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_0350778b298751b2_base

{-| __C declaration:__ @f18@

    __defined at:__ @attributes\/visibility_attributes.h 41:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f18 :: IO ()
f18 = hs_bindgen_0350778b298751b2

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f19@
foreign import ccall safe "hs_bindgen_aacba18c2fb3dae1" hs_bindgen_aacba18c2fb3dae1_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f19@
hs_bindgen_aacba18c2fb3dae1 :: IO ()
hs_bindgen_aacba18c2fb3dae1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_aacba18c2fb3dae1_base

{-| __C declaration:__ @f19@

    __defined at:__ @attributes\/visibility_attributes.h 42:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f19 :: IO ()
f19 = hs_bindgen_aacba18c2fb3dae1

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f20@
foreign import ccall safe "hs_bindgen_023f4d5d2a56d2e9" hs_bindgen_023f4d5d2a56d2e9_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f20@
hs_bindgen_023f4d5d2a56d2e9 :: IO ()
hs_bindgen_023f4d5d2a56d2e9 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_023f4d5d2a56d2e9_base

{-| __C declaration:__ @f20@

    __defined at:__ @attributes\/visibility_attributes.h 45:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f20 :: IO ()
f20 = hs_bindgen_023f4d5d2a56d2e9

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f21@
foreign import ccall safe "hs_bindgen_3a70db9f9e280b85" hs_bindgen_3a70db9f9e280b85_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f21@
hs_bindgen_3a70db9f9e280b85 :: IO ()
hs_bindgen_3a70db9f9e280b85 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_3a70db9f9e280b85_base

{-| __C declaration:__ @f21@

    __defined at:__ @attributes\/visibility_attributes.h 46:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f21 :: IO ()
f21 = hs_bindgen_3a70db9f9e280b85

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f22@
foreign import ccall safe "hs_bindgen_a4fc9586b7510ea6" hs_bindgen_a4fc9586b7510ea6_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f22@
hs_bindgen_a4fc9586b7510ea6 :: IO ()
hs_bindgen_a4fc9586b7510ea6 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a4fc9586b7510ea6_base

{-| __C declaration:__ @f22@

    __defined at:__ @attributes\/visibility_attributes.h 47:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f22 :: IO ()
f22 = hs_bindgen_a4fc9586b7510ea6

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f23@
foreign import ccall safe "hs_bindgen_c25df69a5f23a9b9" hs_bindgen_c25df69a5f23a9b9_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f23@
hs_bindgen_c25df69a5f23a9b9 :: IO ()
hs_bindgen_c25df69a5f23a9b9 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_c25df69a5f23a9b9_base

{-| __C declaration:__ @f23@

    __defined at:__ @attributes\/visibility_attributes.h 48:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f23 :: IO ()
f23 = hs_bindgen_c25df69a5f23a9b9

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f24@
foreign import ccall safe "hs_bindgen_909c5201154b4617" hs_bindgen_909c5201154b4617_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f24@
hs_bindgen_909c5201154b4617 :: IO ()
hs_bindgen_909c5201154b4617 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_909c5201154b4617_base

{-| __C declaration:__ @f24@

    __defined at:__ @attributes\/visibility_attributes.h 49:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f24 :: IO ()
f24 = hs_bindgen_909c5201154b4617

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f25@
foreign import ccall safe "hs_bindgen_dfbd362260cd0fba" hs_bindgen_dfbd362260cd0fba_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f25@
hs_bindgen_dfbd362260cd0fba :: IO ()
hs_bindgen_dfbd362260cd0fba =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_dfbd362260cd0fba_base

{-| __C declaration:__ @f25@

    __defined at:__ @attributes\/visibility_attributes.h 52:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f25 :: IO ()
f25 = hs_bindgen_dfbd362260cd0fba

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f26@
foreign import ccall safe "hs_bindgen_d9c4d8bc1b7d8a59" hs_bindgen_d9c4d8bc1b7d8a59_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f26@
hs_bindgen_d9c4d8bc1b7d8a59 :: IO ()
hs_bindgen_d9c4d8bc1b7d8a59 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_d9c4d8bc1b7d8a59_base

{-| __C declaration:__ @f26@

    __defined at:__ @attributes\/visibility_attributes.h 53:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f26 :: IO ()
f26 = hs_bindgen_d9c4d8bc1b7d8a59

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f27@
foreign import ccall safe "hs_bindgen_71a726a6d6e62f14" hs_bindgen_71a726a6d6e62f14_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f27@
hs_bindgen_71a726a6d6e62f14 :: IO ()
hs_bindgen_71a726a6d6e62f14 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_71a726a6d6e62f14_base

{-| __C declaration:__ @f27@

    __defined at:__ @attributes\/visibility_attributes.h 54:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f27 :: IO ()
f27 = hs_bindgen_71a726a6d6e62f14

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f28@
foreign import ccall safe "hs_bindgen_70f35808778a1423" hs_bindgen_70f35808778a1423_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f28@
hs_bindgen_70f35808778a1423 :: IO ()
hs_bindgen_70f35808778a1423 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_70f35808778a1423_base

{-| __C declaration:__ @f28@

    __defined at:__ @attributes\/visibility_attributes.h 55:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f28 :: IO ()
f28 = hs_bindgen_70f35808778a1423

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f29@
foreign import ccall safe "hs_bindgen_7ba7293cedb52447" hs_bindgen_7ba7293cedb52447_base ::
     IO ()

-- __unique:__ @test_attributesvisibility_attribut_Example_Safe_f29@
hs_bindgen_7ba7293cedb52447 :: IO ()
hs_bindgen_7ba7293cedb52447 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_7ba7293cedb52447_base

{-| __C declaration:__ @f29@

    __defined at:__ @attributes\/visibility_attributes.h 56:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f29 :: IO ()
f29 = hs_bindgen_7ba7293cedb52447
