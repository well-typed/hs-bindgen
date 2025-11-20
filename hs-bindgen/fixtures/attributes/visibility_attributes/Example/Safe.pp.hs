{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <attributes/visibility_attributes.h>"
  , "void hs_bindgen_test_attributesvisibility_attribut_e8fda12159f2be9f (void)"
  , "{"
  , "  f0();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_a2f84d2570ef3892 (void)"
  , "{"
  , "  f1();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_1d043de05a457e90 (void)"
  , "{"
  , "  f2();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_e23eff1955ebb459 (void)"
  , "{"
  , "  f3();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_ce219a1a5351d14e (void)"
  , "{"
  , "  f4();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_caebbc1a0babf9c3 (void)"
  , "{"
  , "  f5();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_0e94ab16fe1245e4 (void)"
  , "{"
  , "  f6();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_595393c65507c7b2 (void)"
  , "{"
  , "  f7();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_2e6297cc5a3e79e0 (void)"
  , "{"
  , "  f8();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_7d6b88eb048c2261 (void)"
  , "{"
  , "  f9();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_4ef53db381225865 (void)"
  , "{"
  , "  f10();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_c626c8c382be7e47 (void)"
  , "{"
  , "  f11();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_09bc3cf816a85839 (void)"
  , "{"
  , "  f12();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_eadf3eb9d39365cf (void)"
  , "{"
  , "  f13();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_24313656b5162754 (void)"
  , "{"
  , "  f14();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_db09067813df28c1 (void)"
  , "{"
  , "  f15();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_4ce3176c4406cf10 (void)"
  , "{"
  , "  f16();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_2354b7b245be3629 (void)"
  , "{"
  , "  f17();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_a24f6e8ea1a74456 (void)"
  , "{"
  , "  f18();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_e14cdae313c9647d (void)"
  , "{"
  , "  f19();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_d973493c824fdf05 (void)"
  , "{"
  , "  f20();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_cc1b3614f810260c (void)"
  , "{"
  , "  f21();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_cb764aa14ed3e34c (void)"
  , "{"
  , "  f22();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_dc225cc74f4331bf (void)"
  , "{"
  , "  f23();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_88449e6c03902cdf (void)"
  , "{"
  , "  f24();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_7370ca6009a58826 (void)"
  , "{"
  , "  f25();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_a67323b70e59146d (void)"
  , "{"
  , "  f26();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_968a7a3827f17839 (void)"
  , "{"
  , "  f27();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_fbb18ffa92c2c5be (void)"
  , "{"
  , "  f28();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_60afc98eb89b8a2d (void)"
  , "{"
  , "  f29();"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_e8fda12159f2be9f" f0_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f0@

    __defined at:__ @attributes\/visibility_attributes.h:17:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f0 ::
     IO ()
f0 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f0_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_a2f84d2570ef3892" f1_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f1@

    __defined at:__ @attributes\/visibility_attributes.h:18:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f1 ::
     IO ()
f1 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f1_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_1d043de05a457e90" f2_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f2@

    __defined at:__ @attributes\/visibility_attributes.h:19:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f2 ::
     IO ()
f2 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f2_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_e23eff1955ebb459" f3_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f3@

    __defined at:__ @attributes\/visibility_attributes.h:20:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f3 ::
     IO ()
f3 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f3_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_ce219a1a5351d14e" f4_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f4@

    __defined at:__ @attributes\/visibility_attributes.h:21:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f4 ::
     IO ()
f4 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f4_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_caebbc1a0babf9c3" f5_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f5@

    __defined at:__ @attributes\/visibility_attributes.h:24:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f5 ::
     IO ()
f5 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f5_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_0e94ab16fe1245e4" f6_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f6@

    __defined at:__ @attributes\/visibility_attributes.h:25:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f6 ::
     IO ()
f6 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f6_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_595393c65507c7b2" f7_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f7@

    __defined at:__ @attributes\/visibility_attributes.h:26:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f7 ::
     IO ()
f7 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f7_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_2e6297cc5a3e79e0" f8_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f8@

    __defined at:__ @attributes\/visibility_attributes.h:27:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f8 ::
     IO ()
f8 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f8_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_7d6b88eb048c2261" f9_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f9@

    __defined at:__ @attributes\/visibility_attributes.h:28:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f9 ::
     IO ()
f9 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f9_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_4ef53db381225865" f10_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f10@

    __defined at:__ @attributes\/visibility_attributes.h:31:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f10 ::
     IO ()
f10 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f10_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_c626c8c382be7e47" f11_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f11@

    __defined at:__ @attributes\/visibility_attributes.h:32:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f11 ::
     IO ()
f11 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f11_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_09bc3cf816a85839" f12_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f12@

    __defined at:__ @attributes\/visibility_attributes.h:33:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f12 ::
     IO ()
f12 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f12_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_eadf3eb9d39365cf" f13_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f13@

    __defined at:__ @attributes\/visibility_attributes.h:34:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f13 ::
     IO ()
f13 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f13_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_24313656b5162754" f14_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f14@

    __defined at:__ @attributes\/visibility_attributes.h:35:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f14 ::
     IO ()
f14 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f14_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_db09067813df28c1" f15_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f15@

    __defined at:__ @attributes\/visibility_attributes.h:38:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f15 ::
     IO ()
f15 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f15_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_4ce3176c4406cf10" f16_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f16@

    __defined at:__ @attributes\/visibility_attributes.h:39:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f16 ::
     IO ()
f16 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f16_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_2354b7b245be3629" f17_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f17@

    __defined at:__ @attributes\/visibility_attributes.h:40:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f17 ::
     IO ()
f17 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f17_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_a24f6e8ea1a74456" f18_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f18@

    __defined at:__ @attributes\/visibility_attributes.h:41:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f18 ::
     IO ()
f18 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f18_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_e14cdae313c9647d" f19_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f19@

    __defined at:__ @attributes\/visibility_attributes.h:42:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f19 ::
     IO ()
f19 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f19_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_d973493c824fdf05" f20_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f20@

    __defined at:__ @attributes\/visibility_attributes.h:45:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f20 ::
     IO ()
f20 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f20_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_cc1b3614f810260c" f21_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f21@

    __defined at:__ @attributes\/visibility_attributes.h:46:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f21 ::
     IO ()
f21 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f21_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_cb764aa14ed3e34c" f22_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f22@

    __defined at:__ @attributes\/visibility_attributes.h:47:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f22 ::
     IO ()
f22 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f22_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_dc225cc74f4331bf" f23_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f23@

    __defined at:__ @attributes\/visibility_attributes.h:48:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f23 ::
     IO ()
f23 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f23_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_88449e6c03902cdf" f24_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f24@

    __defined at:__ @attributes\/visibility_attributes.h:49:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f24 ::
     IO ()
f24 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f24_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_7370ca6009a58826" f25_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f25@

    __defined at:__ @attributes\/visibility_attributes.h:52:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f25 ::
     IO ()
f25 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f25_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_a67323b70e59146d" f26_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f26@

    __defined at:__ @attributes\/visibility_attributes.h:53:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f26 ::
     IO ()
f26 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f26_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_968a7a3827f17839" f27_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f27@

    __defined at:__ @attributes\/visibility_attributes.h:54:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f27 ::
     IO ()
f27 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f27_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_fbb18ffa92c2c5be" f28_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f28@

    __defined at:__ @attributes\/visibility_attributes.h:55:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f28 ::
     IO ()
f28 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f28_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_attributesvisibility_attribut_60afc98eb89b8a2d" f29_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @f29@

    __defined at:__ @attributes\/visibility_attributes.h:56:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f29 ::
     IO ()
f29 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType f29_base
