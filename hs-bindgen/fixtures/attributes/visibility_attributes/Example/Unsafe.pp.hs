{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <attributes/visibility_attributes.h>"
  , "void hs_bindgen_test_attributesvisibility_attribut_3ff941535f1a906c (void)"
  , "{"
  , "  f0();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_c1788128a5b1c813 (void)"
  , "{"
  , "  f1();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_14361e995fb5684a (void)"
  , "{"
  , "  f2();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_2bef032cbe15ffd0 (void)"
  , "{"
  , "  f3();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_cd0cf1428bcc9b38 (void)"
  , "{"
  , "  f4();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_8c6188a2eaf5d0d4 (void)"
  , "{"
  , "  f5();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_b8eff0c55713150e (void)"
  , "{"
  , "  f6();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_71135129c9373ee7 (void)"
  , "{"
  , "  f7();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_febb2843049709cd (void)"
  , "{"
  , "  f8();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_f6882e1e65092791 (void)"
  , "{"
  , "  f9();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_7d21aeb39d51b64f (void)"
  , "{"
  , "  f10();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_fe9b18a1d2845606 (void)"
  , "{"
  , "  f11();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_8b5aacef3fb80581 (void)"
  , "{"
  , "  f12();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_9162af7e5d2b25bd (void)"
  , "{"
  , "  f13();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_92d022f6d12704a3 (void)"
  , "{"
  , "  f14();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_1ade4a16b9edc93f (void)"
  , "{"
  , "  f15();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_702e08cfa9d8f107 (void)"
  , "{"
  , "  f16();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_6839cf744c467402 (void)"
  , "{"
  , "  f17();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_bcbefba8cee060e9 (void)"
  , "{"
  , "  f18();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_6991c5868d8397b8 (void)"
  , "{"
  , "  f19();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_168f1d96d48b3571 (void)"
  , "{"
  , "  f20();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_05b34816425cdccc (void)"
  , "{"
  , "  f21();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_1531783017d14d65 (void)"
  , "{"
  , "  f22();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_e086f69c4390fd7e (void)"
  , "{"
  , "  f23();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_eaa6acf5b4299e7c (void)"
  , "{"
  , "  f24();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_a82bd6ddcf01332d (void)"
  , "{"
  , "  f25();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_1ee51cc55408f9a7 (void)"
  , "{"
  , "  f26();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_f6035b3578b5d5cd (void)"
  , "{"
  , "  f27();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_a827e3b8d932270f (void)"
  , "{"
  , "  f28();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_316dcf70a67165b5 (void)"
  , "{"
  , "  f29();"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_3ff941535f1a906c" f0_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_c1788128a5b1c813" f1_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_14361e995fb5684a" f2_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_2bef032cbe15ffd0" f3_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_cd0cf1428bcc9b38" f4_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_8c6188a2eaf5d0d4" f5_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_b8eff0c55713150e" f6_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_71135129c9373ee7" f7_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_febb2843049709cd" f8_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_f6882e1e65092791" f9_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_7d21aeb39d51b64f" f10_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_fe9b18a1d2845606" f11_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_8b5aacef3fb80581" f12_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_9162af7e5d2b25bd" f13_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_92d022f6d12704a3" f14_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_1ade4a16b9edc93f" f15_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_702e08cfa9d8f107" f16_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_6839cf744c467402" f17_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_bcbefba8cee060e9" f18_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_6991c5868d8397b8" f19_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_168f1d96d48b3571" f20_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_05b34816425cdccc" f21_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_1531783017d14d65" f22_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_e086f69c4390fd7e" f23_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_eaa6acf5b4299e7c" f24_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_a82bd6ddcf01332d" f25_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_1ee51cc55408f9a7" f26_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_f6035b3578b5d5cd" f27_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_a827e3b8d932270f" f28_base ::
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
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_316dcf70a67165b5" f29_base ::
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
