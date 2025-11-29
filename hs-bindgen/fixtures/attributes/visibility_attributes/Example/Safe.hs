{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

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

{-| __C declaration:__ @f0@

    __defined at:__ @attributes\/visibility_attributes.h:17:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f0@
-}
foreign import ccall safe "hs_bindgen_e64a83c5f7f51679" f0 ::
     IO ()

{-| __C declaration:__ @f1@

    __defined at:__ @attributes\/visibility_attributes.h:18:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f1@
-}
foreign import ccall safe "hs_bindgen_8881b0f4ce94e440" f1 ::
     IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @attributes\/visibility_attributes.h:19:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f2@
-}
foreign import ccall safe "hs_bindgen_4ede7f4c96b4d1b5" f2 ::
     IO ()

{-| __C declaration:__ @f3@

    __defined at:__ @attributes\/visibility_attributes.h:20:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f3@
-}
foreign import ccall safe "hs_bindgen_018f1e15132ff973" f3 ::
     IO ()

{-| __C declaration:__ @f4@

    __defined at:__ @attributes\/visibility_attributes.h:21:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f4@
-}
foreign import ccall safe "hs_bindgen_9d93cf33b1a833d1" f4 ::
     IO ()

{-| __C declaration:__ @f5@

    __defined at:__ @attributes\/visibility_attributes.h:24:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f5@
-}
foreign import ccall safe "hs_bindgen_959298c4e5cb061a" f5 ::
     IO ()

{-| __C declaration:__ @f6@

    __defined at:__ @attributes\/visibility_attributes.h:25:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f6@
-}
foreign import ccall safe "hs_bindgen_218297218a4953d6" f6 ::
     IO ()

{-| __C declaration:__ @f7@

    __defined at:__ @attributes\/visibility_attributes.h:26:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f7@
-}
foreign import ccall safe "hs_bindgen_df320fe3d4683ff9" f7 ::
     IO ()

{-| __C declaration:__ @f8@

    __defined at:__ @attributes\/visibility_attributes.h:27:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f8@
-}
foreign import ccall safe "hs_bindgen_cd613f46d2e06e18" f8 ::
     IO ()

{-| __C declaration:__ @f9@

    __defined at:__ @attributes\/visibility_attributes.h:28:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f9@
-}
foreign import ccall safe "hs_bindgen_67774b3f22bd7286" f9 ::
     IO ()

{-| __C declaration:__ @f10@

    __defined at:__ @attributes\/visibility_attributes.h:31:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f10@
-}
foreign import ccall safe "hs_bindgen_dbbd2e0417380ce9" f10 ::
     IO ()

{-| __C declaration:__ @f11@

    __defined at:__ @attributes\/visibility_attributes.h:32:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f11@
-}
foreign import ccall safe "hs_bindgen_9d8fadad3f85e1fc" f11 ::
     IO ()

{-| __C declaration:__ @f12@

    __defined at:__ @attributes\/visibility_attributes.h:33:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f12@
-}
foreign import ccall safe "hs_bindgen_558a7ef50878f4b2" f12 ::
     IO ()

{-| __C declaration:__ @f13@

    __defined at:__ @attributes\/visibility_attributes.h:34:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f13@
-}
foreign import ccall safe "hs_bindgen_30add751959aac79" f13 ::
     IO ()

{-| __C declaration:__ @f14@

    __defined at:__ @attributes\/visibility_attributes.h:35:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f14@
-}
foreign import ccall safe "hs_bindgen_06b989df2d3622ad" f14 ::
     IO ()

{-| __C declaration:__ @f15@

    __defined at:__ @attributes\/visibility_attributes.h:38:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f15@
-}
foreign import ccall safe "hs_bindgen_11627343b78e6e76" f15 ::
     IO ()

{-| __C declaration:__ @f16@

    __defined at:__ @attributes\/visibility_attributes.h:39:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f16@
-}
foreign import ccall safe "hs_bindgen_5d3aa9a924674163" f16 ::
     IO ()

{-| __C declaration:__ @f17@

    __defined at:__ @attributes\/visibility_attributes.h:40:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f17@
-}
foreign import ccall safe "hs_bindgen_65abad67aeb6b955" f17 ::
     IO ()

{-| __C declaration:__ @f18@

    __defined at:__ @attributes\/visibility_attributes.h:41:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f18@
-}
foreign import ccall safe "hs_bindgen_0350778b298751b2" f18 ::
     IO ()

{-| __C declaration:__ @f19@

    __defined at:__ @attributes\/visibility_attributes.h:42:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f19@
-}
foreign import ccall safe "hs_bindgen_aacba18c2fb3dae1" f19 ::
     IO ()

{-| __C declaration:__ @f20@

    __defined at:__ @attributes\/visibility_attributes.h:45:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f20@
-}
foreign import ccall safe "hs_bindgen_023f4d5d2a56d2e9" f20 ::
     IO ()

{-| __C declaration:__ @f21@

    __defined at:__ @attributes\/visibility_attributes.h:46:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f21@
-}
foreign import ccall safe "hs_bindgen_3a70db9f9e280b85" f21 ::
     IO ()

{-| __C declaration:__ @f22@

    __defined at:__ @attributes\/visibility_attributes.h:47:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f22@
-}
foreign import ccall safe "hs_bindgen_a4fc9586b7510ea6" f22 ::
     IO ()

{-| __C declaration:__ @f23@

    __defined at:__ @attributes\/visibility_attributes.h:48:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f23@
-}
foreign import ccall safe "hs_bindgen_c25df69a5f23a9b9" f23 ::
     IO ()

{-| __C declaration:__ @f24@

    __defined at:__ @attributes\/visibility_attributes.h:49:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f24@
-}
foreign import ccall safe "hs_bindgen_909c5201154b4617" f24 ::
     IO ()

{-| __C declaration:__ @f25@

    __defined at:__ @attributes\/visibility_attributes.h:52:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f25@
-}
foreign import ccall safe "hs_bindgen_dfbd362260cd0fba" f25 ::
     IO ()

{-| __C declaration:__ @f26@

    __defined at:__ @attributes\/visibility_attributes.h:53:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f26@
-}
foreign import ccall safe "hs_bindgen_d9c4d8bc1b7d8a59" f26 ::
     IO ()

{-| __C declaration:__ @f27@

    __defined at:__ @attributes\/visibility_attributes.h:54:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f27@
-}
foreign import ccall safe "hs_bindgen_71a726a6d6e62f14" f27 ::
     IO ()

{-| __C declaration:__ @f28@

    __defined at:__ @attributes\/visibility_attributes.h:55:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f28@
-}
foreign import ccall safe "hs_bindgen_70f35808778a1423" f28 ::
     IO ()

{-| __C declaration:__ @f29@

    __defined at:__ @attributes\/visibility_attributes.h:56:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @test_attributesvisibility_attribut_Example_Safe_f29@
-}
foreign import ccall safe "hs_bindgen_7ba7293cedb52447" f29 ::
     IO ()
