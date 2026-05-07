{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.f0
    , Example.Safe.f1
    , Example.Safe.f2
    , Example.Safe.f3
    , Example.Safe.f4
    , Example.Safe.f5
    , Example.Safe.f6
    , Example.Safe.f7
    , Example.Safe.f8
    , Example.Safe.f9
    , Example.Safe.f10
    , Example.Safe.f11
    , Example.Safe.f12
    , Example.Safe.f13
    , Example.Safe.f14
    , Example.Safe.f15
    , Example.Safe.f16
    , Example.Safe.f17
    , Example.Safe.f18
    , Example.Safe.f19
    , Example.Safe.f20
    , Example.Safe.f21
    , Example.Safe.f22
    , Example.Safe.f23
    , Example.Safe.f24
    , Example.Safe.f25
    , Example.Safe.f26
    , Example.Safe.f27
    , Example.Safe.f28
    , Example.Safe.f29
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <attributes/visibility/functions.h>"
  , "void hs_bindgen_3f27c62fb190454a (void)"
  , "{"
  , "  (f0)();"
  , "}"
  , "void hs_bindgen_1baf9aba48bb8585 (void)"
  , "{"
  , "  (f1)();"
  , "}"
  , "void hs_bindgen_9de994ef69391f75 (void)"
  , "{"
  , "  (f2)();"
  , "}"
  , "void hs_bindgen_e7b6b3fe1e458b05 (void)"
  , "{"
  , "  (f3)();"
  , "}"
  , "void hs_bindgen_55f6714f782d28a7 (void)"
  , "{"
  , "  (f4)();"
  , "}"
  , "void hs_bindgen_08ec707e7bc17377 (void)"
  , "{"
  , "  (f5)();"
  , "}"
  , "void hs_bindgen_610d965a82ee5d22 (void)"
  , "{"
  , "  (f6)();"
  , "}"
  , "void hs_bindgen_5f54d7b893c2f695 (void)"
  , "{"
  , "  (f7)();"
  , "}"
  , "void hs_bindgen_219c458bfad16b17 (void)"
  , "{"
  , "  (f8)();"
  , "}"
  , "void hs_bindgen_5d3fb29d7697a006 (void)"
  , "{"
  , "  (f9)();"
  , "}"
  , "void hs_bindgen_5064aed792451d33 (void)"
  , "{"
  , "  (f10)();"
  , "}"
  , "void hs_bindgen_25e0cdea1045ed28 (void)"
  , "{"
  , "  (f11)();"
  , "}"
  , "void hs_bindgen_729b9541b191c0a9 (void)"
  , "{"
  , "  (f12)();"
  , "}"
  , "void hs_bindgen_570476629e39b1ad (void)"
  , "{"
  , "  (f13)();"
  , "}"
  , "void hs_bindgen_094d4428896cec1a (void)"
  , "{"
  , "  (f14)();"
  , "}"
  , "void hs_bindgen_a15c67349afb7d1b (void)"
  , "{"
  , "  (f15)();"
  , "}"
  , "void hs_bindgen_024506459e135eeb (void)"
  , "{"
  , "  (f16)();"
  , "}"
  , "void hs_bindgen_8297e7356e9a3699 (void)"
  , "{"
  , "  (f17)();"
  , "}"
  , "void hs_bindgen_8c9d69b89b46c409 (void)"
  , "{"
  , "  (f18)();"
  , "}"
  , "void hs_bindgen_411e20c811bb4f75 (void)"
  , "{"
  , "  (f19)();"
  , "}"
  , "void hs_bindgen_57573c31920ee01d (void)"
  , "{"
  , "  (f20)();"
  , "}"
  , "void hs_bindgen_0f3e02be6a27086a (void)"
  , "{"
  , "  (f21)();"
  , "}"
  , "void hs_bindgen_03810317ee329129 (void)"
  , "{"
  , "  (f22)();"
  , "}"
  , "void hs_bindgen_e2fd13d795062844 (void)"
  , "{"
  , "  (f23)();"
  , "}"
  , "void hs_bindgen_87272e2adfb6015b (void)"
  , "{"
  , "  (f24)();"
  , "}"
  , "void hs_bindgen_b573824e77968985 (void)"
  , "{"
  , "  (f25)();"
  , "}"
  , "void hs_bindgen_9b5ba03996903917 (void)"
  , "{"
  , "  (f26)();"
  , "}"
  , "void hs_bindgen_4d6e2dac36287b1e (void)"
  , "{"
  , "  (f27)();"
  , "}"
  , "void hs_bindgen_d8e66fca82e1889d (void)"
  , "{"
  , "  (f28)();"
  , "}"
  , "void hs_bindgen_600c38ba2dfb8c71 (void)"
  , "{"
  , "  (f29)();"
  , "}"
  ]))

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f0@
foreign import ccall safe "hs_bindgen_3f27c62fb190454a" hs_bindgen_3f27c62fb190454a_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f0@
hs_bindgen_3f27c62fb190454a :: IO ()
hs_bindgen_3f27c62fb190454a =
  RIP.fromFFIType hs_bindgen_3f27c62fb190454a_base

{-| __C declaration:__ @f0@

    __defined at:__ @attributes\/visibility\/functions.h 14:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f0 :: IO ()
f0 = hs_bindgen_3f27c62fb190454a

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f1@
foreign import ccall safe "hs_bindgen_1baf9aba48bb8585" hs_bindgen_1baf9aba48bb8585_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f1@
hs_bindgen_1baf9aba48bb8585 :: IO ()
hs_bindgen_1baf9aba48bb8585 =
  RIP.fromFFIType hs_bindgen_1baf9aba48bb8585_base

{-| __C declaration:__ @f1@

    __defined at:__ @attributes\/visibility\/functions.h 15:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f1 :: IO ()
f1 = hs_bindgen_1baf9aba48bb8585

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f2@
foreign import ccall safe "hs_bindgen_9de994ef69391f75" hs_bindgen_9de994ef69391f75_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f2@
hs_bindgen_9de994ef69391f75 :: IO ()
hs_bindgen_9de994ef69391f75 =
  RIP.fromFFIType hs_bindgen_9de994ef69391f75_base

{-| __C declaration:__ @f2@

    __defined at:__ @attributes\/visibility\/functions.h 16:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f2 :: IO ()
f2 = hs_bindgen_9de994ef69391f75

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f3@
foreign import ccall safe "hs_bindgen_e7b6b3fe1e458b05" hs_bindgen_e7b6b3fe1e458b05_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f3@
hs_bindgen_e7b6b3fe1e458b05 :: IO ()
hs_bindgen_e7b6b3fe1e458b05 =
  RIP.fromFFIType hs_bindgen_e7b6b3fe1e458b05_base

{-| __C declaration:__ @f3@

    __defined at:__ @attributes\/visibility\/functions.h 17:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f3 :: IO ()
f3 = hs_bindgen_e7b6b3fe1e458b05

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f4@
foreign import ccall safe "hs_bindgen_55f6714f782d28a7" hs_bindgen_55f6714f782d28a7_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f4@
hs_bindgen_55f6714f782d28a7 :: IO ()
hs_bindgen_55f6714f782d28a7 =
  RIP.fromFFIType hs_bindgen_55f6714f782d28a7_base

{-| __C declaration:__ @f4@

    __defined at:__ @attributes\/visibility\/functions.h 18:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f4 :: IO ()
f4 = hs_bindgen_55f6714f782d28a7

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f5@
foreign import ccall safe "hs_bindgen_08ec707e7bc17377" hs_bindgen_08ec707e7bc17377_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f5@
hs_bindgen_08ec707e7bc17377 :: IO ()
hs_bindgen_08ec707e7bc17377 =
  RIP.fromFFIType hs_bindgen_08ec707e7bc17377_base

{-| __C declaration:__ @f5@

    __defined at:__ @attributes\/visibility\/functions.h 21:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f5 :: IO ()
f5 = hs_bindgen_08ec707e7bc17377

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f6@
foreign import ccall safe "hs_bindgen_610d965a82ee5d22" hs_bindgen_610d965a82ee5d22_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f6@
hs_bindgen_610d965a82ee5d22 :: IO ()
hs_bindgen_610d965a82ee5d22 =
  RIP.fromFFIType hs_bindgen_610d965a82ee5d22_base

{-| __C declaration:__ @f6@

    __defined at:__ @attributes\/visibility\/functions.h 22:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f6 :: IO ()
f6 = hs_bindgen_610d965a82ee5d22

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f7@
foreign import ccall safe "hs_bindgen_5f54d7b893c2f695" hs_bindgen_5f54d7b893c2f695_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f7@
hs_bindgen_5f54d7b893c2f695 :: IO ()
hs_bindgen_5f54d7b893c2f695 =
  RIP.fromFFIType hs_bindgen_5f54d7b893c2f695_base

{-| __C declaration:__ @f7@

    __defined at:__ @attributes\/visibility\/functions.h 23:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f7 :: IO ()
f7 = hs_bindgen_5f54d7b893c2f695

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f8@
foreign import ccall safe "hs_bindgen_219c458bfad16b17" hs_bindgen_219c458bfad16b17_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f8@
hs_bindgen_219c458bfad16b17 :: IO ()
hs_bindgen_219c458bfad16b17 =
  RIP.fromFFIType hs_bindgen_219c458bfad16b17_base

{-| __C declaration:__ @f8@

    __defined at:__ @attributes\/visibility\/functions.h 24:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f8 :: IO ()
f8 = hs_bindgen_219c458bfad16b17

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f9@
foreign import ccall safe "hs_bindgen_5d3fb29d7697a006" hs_bindgen_5d3fb29d7697a006_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f9@
hs_bindgen_5d3fb29d7697a006 :: IO ()
hs_bindgen_5d3fb29d7697a006 =
  RIP.fromFFIType hs_bindgen_5d3fb29d7697a006_base

{-| __C declaration:__ @f9@

    __defined at:__ @attributes\/visibility\/functions.h 25:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f9 :: IO ()
f9 = hs_bindgen_5d3fb29d7697a006

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f10@
foreign import ccall safe "hs_bindgen_5064aed792451d33" hs_bindgen_5064aed792451d33_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f10@
hs_bindgen_5064aed792451d33 :: IO ()
hs_bindgen_5064aed792451d33 =
  RIP.fromFFIType hs_bindgen_5064aed792451d33_base

{-| __C declaration:__ @f10@

    __defined at:__ @attributes\/visibility\/functions.h 28:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f10 :: IO ()
f10 = hs_bindgen_5064aed792451d33

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f11@
foreign import ccall safe "hs_bindgen_25e0cdea1045ed28" hs_bindgen_25e0cdea1045ed28_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f11@
hs_bindgen_25e0cdea1045ed28 :: IO ()
hs_bindgen_25e0cdea1045ed28 =
  RIP.fromFFIType hs_bindgen_25e0cdea1045ed28_base

{-| __C declaration:__ @f11@

    __defined at:__ @attributes\/visibility\/functions.h 29:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f11 :: IO ()
f11 = hs_bindgen_25e0cdea1045ed28

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f12@
foreign import ccall safe "hs_bindgen_729b9541b191c0a9" hs_bindgen_729b9541b191c0a9_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f12@
hs_bindgen_729b9541b191c0a9 :: IO ()
hs_bindgen_729b9541b191c0a9 =
  RIP.fromFFIType hs_bindgen_729b9541b191c0a9_base

{-| __C declaration:__ @f12@

    __defined at:__ @attributes\/visibility\/functions.h 30:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f12 :: IO ()
f12 = hs_bindgen_729b9541b191c0a9

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f13@
foreign import ccall safe "hs_bindgen_570476629e39b1ad" hs_bindgen_570476629e39b1ad_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f13@
hs_bindgen_570476629e39b1ad :: IO ()
hs_bindgen_570476629e39b1ad =
  RIP.fromFFIType hs_bindgen_570476629e39b1ad_base

{-| __C declaration:__ @f13@

    __defined at:__ @attributes\/visibility\/functions.h 31:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f13 :: IO ()
f13 = hs_bindgen_570476629e39b1ad

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f14@
foreign import ccall safe "hs_bindgen_094d4428896cec1a" hs_bindgen_094d4428896cec1a_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f14@
hs_bindgen_094d4428896cec1a :: IO ()
hs_bindgen_094d4428896cec1a =
  RIP.fromFFIType hs_bindgen_094d4428896cec1a_base

{-| __C declaration:__ @f14@

    __defined at:__ @attributes\/visibility\/functions.h 32:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f14 :: IO ()
f14 = hs_bindgen_094d4428896cec1a

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f15@
foreign import ccall safe "hs_bindgen_a15c67349afb7d1b" hs_bindgen_a15c67349afb7d1b_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f15@
hs_bindgen_a15c67349afb7d1b :: IO ()
hs_bindgen_a15c67349afb7d1b =
  RIP.fromFFIType hs_bindgen_a15c67349afb7d1b_base

{-| __C declaration:__ @f15@

    __defined at:__ @attributes\/visibility\/functions.h 35:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f15 :: IO ()
f15 = hs_bindgen_a15c67349afb7d1b

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f16@
foreign import ccall safe "hs_bindgen_024506459e135eeb" hs_bindgen_024506459e135eeb_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f16@
hs_bindgen_024506459e135eeb :: IO ()
hs_bindgen_024506459e135eeb =
  RIP.fromFFIType hs_bindgen_024506459e135eeb_base

{-| __C declaration:__ @f16@

    __defined at:__ @attributes\/visibility\/functions.h 36:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f16 :: IO ()
f16 = hs_bindgen_024506459e135eeb

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f17@
foreign import ccall safe "hs_bindgen_8297e7356e9a3699" hs_bindgen_8297e7356e9a3699_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f17@
hs_bindgen_8297e7356e9a3699 :: IO ()
hs_bindgen_8297e7356e9a3699 =
  RIP.fromFFIType hs_bindgen_8297e7356e9a3699_base

{-| __C declaration:__ @f17@

    __defined at:__ @attributes\/visibility\/functions.h 37:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f17 :: IO ()
f17 = hs_bindgen_8297e7356e9a3699

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f18@
foreign import ccall safe "hs_bindgen_8c9d69b89b46c409" hs_bindgen_8c9d69b89b46c409_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f18@
hs_bindgen_8c9d69b89b46c409 :: IO ()
hs_bindgen_8c9d69b89b46c409 =
  RIP.fromFFIType hs_bindgen_8c9d69b89b46c409_base

{-| __C declaration:__ @f18@

    __defined at:__ @attributes\/visibility\/functions.h 38:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f18 :: IO ()
f18 = hs_bindgen_8c9d69b89b46c409

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f19@
foreign import ccall safe "hs_bindgen_411e20c811bb4f75" hs_bindgen_411e20c811bb4f75_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f19@
hs_bindgen_411e20c811bb4f75 :: IO ()
hs_bindgen_411e20c811bb4f75 =
  RIP.fromFFIType hs_bindgen_411e20c811bb4f75_base

{-| __C declaration:__ @f19@

    __defined at:__ @attributes\/visibility\/functions.h 39:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f19 :: IO ()
f19 = hs_bindgen_411e20c811bb4f75

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f20@
foreign import ccall safe "hs_bindgen_57573c31920ee01d" hs_bindgen_57573c31920ee01d_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f20@
hs_bindgen_57573c31920ee01d :: IO ()
hs_bindgen_57573c31920ee01d =
  RIP.fromFFIType hs_bindgen_57573c31920ee01d_base

{-| __C declaration:__ @f20@

    __defined at:__ @attributes\/visibility\/functions.h 42:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f20 :: IO ()
f20 = hs_bindgen_57573c31920ee01d

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f21@
foreign import ccall safe "hs_bindgen_0f3e02be6a27086a" hs_bindgen_0f3e02be6a27086a_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f21@
hs_bindgen_0f3e02be6a27086a :: IO ()
hs_bindgen_0f3e02be6a27086a =
  RIP.fromFFIType hs_bindgen_0f3e02be6a27086a_base

{-| __C declaration:__ @f21@

    __defined at:__ @attributes\/visibility\/functions.h 43:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f21 :: IO ()
f21 = hs_bindgen_0f3e02be6a27086a

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f22@
foreign import ccall safe "hs_bindgen_03810317ee329129" hs_bindgen_03810317ee329129_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f22@
hs_bindgen_03810317ee329129 :: IO ()
hs_bindgen_03810317ee329129 =
  RIP.fromFFIType hs_bindgen_03810317ee329129_base

{-| __C declaration:__ @f22@

    __defined at:__ @attributes\/visibility\/functions.h 44:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f22 :: IO ()
f22 = hs_bindgen_03810317ee329129

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f23@
foreign import ccall safe "hs_bindgen_e2fd13d795062844" hs_bindgen_e2fd13d795062844_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f23@
hs_bindgen_e2fd13d795062844 :: IO ()
hs_bindgen_e2fd13d795062844 =
  RIP.fromFFIType hs_bindgen_e2fd13d795062844_base

{-| __C declaration:__ @f23@

    __defined at:__ @attributes\/visibility\/functions.h 45:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f23 :: IO ()
f23 = hs_bindgen_e2fd13d795062844

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f24@
foreign import ccall safe "hs_bindgen_87272e2adfb6015b" hs_bindgen_87272e2adfb6015b_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f24@
hs_bindgen_87272e2adfb6015b :: IO ()
hs_bindgen_87272e2adfb6015b =
  RIP.fromFFIType hs_bindgen_87272e2adfb6015b_base

{-| __C declaration:__ @f24@

    __defined at:__ @attributes\/visibility\/functions.h 46:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f24 :: IO ()
f24 = hs_bindgen_87272e2adfb6015b

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f25@
foreign import ccall safe "hs_bindgen_b573824e77968985" hs_bindgen_b573824e77968985_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f25@
hs_bindgen_b573824e77968985 :: IO ()
hs_bindgen_b573824e77968985 =
  RIP.fromFFIType hs_bindgen_b573824e77968985_base

{-| __C declaration:__ @f25@

    __defined at:__ @attributes\/visibility\/functions.h 49:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f25 :: IO ()
f25 = hs_bindgen_b573824e77968985

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f26@
foreign import ccall safe "hs_bindgen_9b5ba03996903917" hs_bindgen_9b5ba03996903917_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f26@
hs_bindgen_9b5ba03996903917 :: IO ()
hs_bindgen_9b5ba03996903917 =
  RIP.fromFFIType hs_bindgen_9b5ba03996903917_base

{-| __C declaration:__ @f26@

    __defined at:__ @attributes\/visibility\/functions.h 50:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f26 :: IO ()
f26 = hs_bindgen_9b5ba03996903917

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f27@
foreign import ccall safe "hs_bindgen_4d6e2dac36287b1e" hs_bindgen_4d6e2dac36287b1e_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f27@
hs_bindgen_4d6e2dac36287b1e :: IO ()
hs_bindgen_4d6e2dac36287b1e =
  RIP.fromFFIType hs_bindgen_4d6e2dac36287b1e_base

{-| __C declaration:__ @f27@

    __defined at:__ @attributes\/visibility\/functions.h 51:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f27 :: IO ()
f27 = hs_bindgen_4d6e2dac36287b1e

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f28@
foreign import ccall safe "hs_bindgen_d8e66fca82e1889d" hs_bindgen_d8e66fca82e1889d_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f28@
hs_bindgen_d8e66fca82e1889d :: IO ()
hs_bindgen_d8e66fca82e1889d =
  RIP.fromFFIType hs_bindgen_d8e66fca82e1889d_base

{-| __C declaration:__ @f28@

    __defined at:__ @attributes\/visibility\/functions.h 52:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f28 :: IO ()
f28 = hs_bindgen_d8e66fca82e1889d

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f29@
foreign import ccall safe "hs_bindgen_600c38ba2dfb8c71" hs_bindgen_600c38ba2dfb8c71_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Safe_f29@
hs_bindgen_600c38ba2dfb8c71 :: IO ()
hs_bindgen_600c38ba2dfb8c71 =
  RIP.fromFFIType hs_bindgen_600c38ba2dfb8c71_base

{-| __C declaration:__ @f29@

    __defined at:__ @attributes\/visibility\/functions.h 53:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f29 :: IO ()
f29 = hs_bindgen_600c38ba2dfb8c71
