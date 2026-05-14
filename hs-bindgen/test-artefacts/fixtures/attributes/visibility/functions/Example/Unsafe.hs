{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.f0
    , Example.Unsafe.f1
    , Example.Unsafe.f2
    , Example.Unsafe.f3
    , Example.Unsafe.f4
    , Example.Unsafe.f5
    , Example.Unsafe.f6
    , Example.Unsafe.f7
    , Example.Unsafe.f8
    , Example.Unsafe.f9
    , Example.Unsafe.f10
    , Example.Unsafe.f11
    , Example.Unsafe.f12
    , Example.Unsafe.f13
    , Example.Unsafe.f14
    , Example.Unsafe.f15
    , Example.Unsafe.f16
    , Example.Unsafe.f17
    , Example.Unsafe.f18
    , Example.Unsafe.f19
    , Example.Unsafe.f20
    , Example.Unsafe.f21
    , Example.Unsafe.f22
    , Example.Unsafe.f23
    , Example.Unsafe.f24
    , Example.Unsafe.f25
    , Example.Unsafe.f26
    , Example.Unsafe.f27
    , Example.Unsafe.f28
    , Example.Unsafe.f29
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <attributes/visibility/functions.h>"
  , "void hs_bindgen_effdd993df42b356 (void)"
  , "{"
  , "  (f0)();"
  , "}"
  , "void hs_bindgen_8197d65e56e329f3 (void)"
  , "{"
  , "  (f1)();"
  , "}"
  , "void hs_bindgen_d39f970af8a8a222 (void)"
  , "{"
  , "  (f2)();"
  , "}"
  , "void hs_bindgen_be64763ec1549ecf (void)"
  , "{"
  , "  (f3)();"
  , "}"
  , "void hs_bindgen_79aec631efd41b2a (void)"
  , "{"
  , "  (f4)();"
  , "}"
  , "void hs_bindgen_def05384051a5dd9 (void)"
  , "{"
  , "  (f5)();"
  , "}"
  , "void hs_bindgen_92d1f9974393a261 (void)"
  , "{"
  , "  (f6)();"
  , "}"
  , "void hs_bindgen_90f3068e094c2abd (void)"
  , "{"
  , "  (f7)();"
  , "}"
  , "void hs_bindgen_6d1cfd91b3d13e55 (void)"
  , "{"
  , "  (f8)();"
  , "}"
  , "void hs_bindgen_d35fb9045bcdab71 (void)"
  , "{"
  , "  (f9)();"
  , "}"
  , "void hs_bindgen_40c723b61f2bee28 (void)"
  , "{"
  , "  (f10)();"
  , "}"
  , "void hs_bindgen_53a546d2dc84168b (void)"
  , "{"
  , "  (f11)();"
  , "}"
  , "void hs_bindgen_47e1da4038b5a9b4 (void)"
  , "{"
  , "  (f12)();"
  , "}"
  , "void hs_bindgen_3b90be410cc76ded (void)"
  , "{"
  , "  (f13)();"
  , "}"
  , "void hs_bindgen_d7d157b17d6f4d9e (void)"
  , "{"
  , "  (f14)();"
  , "}"
  , "void hs_bindgen_32a0e71bc346c633 (void)"
  , "{"
  , "  (f15)();"
  , "}"
  , "void hs_bindgen_084799171b269718 (void)"
  , "{"
  , "  (f16)();"
  , "}"
  , "void hs_bindgen_9399b438ad7d1540 (void)"
  , "{"
  , "  (f17)();"
  , "}"
  , "void hs_bindgen_b0cfd100fa7549bb (void)"
  , "{"
  , "  (f18)();"
  , "}"
  , "void hs_bindgen_00c6e9c82555231a (void)"
  , "{"
  , "  (f19)();"
  , "}"
  , "void hs_bindgen_6630b37e4a8fd258 (void)"
  , "{"
  , "  (f20)();"
  , "}"
  , "void hs_bindgen_1e5e762b3fc32381 (void)"
  , "{"
  , "  (f21)();"
  , "}"
  , "void hs_bindgen_7a55ac9a1d761c51 (void)"
  , "{"
  , "  (f22)();"
  , "}"
  , "void hs_bindgen_6941e6411b943f0b (void)"
  , "{"
  , "  (f23)();"
  , "}"
  , "void hs_bindgen_654278bb511c1626 (void)"
  , "{"
  , "  (f24)();"
  , "}"
  , "void hs_bindgen_5222f10cec613983 (void)"
  , "{"
  , "  (f25)();"
  , "}"
  , "void hs_bindgen_aa77927916c2e213 (void)"
  , "{"
  , "  (f26)();"
  , "}"
  , "void hs_bindgen_102db79a88a6b692 (void)"
  , "{"
  , "  (f27)();"
  , "}"
  , "void hs_bindgen_d475a551dd32819d (void)"
  , "{"
  , "  (f28)();"
  , "}"
  , "void hs_bindgen_afb63d27b0dea11b (void)"
  , "{"
  , "  (f29)();"
  , "}"
  ]))

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f0@
foreign import ccall unsafe "hs_bindgen_effdd993df42b356" hs_bindgen_effdd993df42b356_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f0@
hs_bindgen_effdd993df42b356 :: IO ()
hs_bindgen_effdd993df42b356 =
  RIP.fromFFIType hs_bindgen_effdd993df42b356_base

{-| __C declaration:__ @f0@

    __defined at:__ @attributes\/visibility\/functions.h 14:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f0 :: IO ()
f0 = hs_bindgen_effdd993df42b356

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f1@
foreign import ccall unsafe "hs_bindgen_8197d65e56e329f3" hs_bindgen_8197d65e56e329f3_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f1@
hs_bindgen_8197d65e56e329f3 :: IO ()
hs_bindgen_8197d65e56e329f3 =
  RIP.fromFFIType hs_bindgen_8197d65e56e329f3_base

{-| __C declaration:__ @f1@

    __defined at:__ @attributes\/visibility\/functions.h 15:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f1 :: IO ()
f1 = hs_bindgen_8197d65e56e329f3

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f2@
foreign import ccall unsafe "hs_bindgen_d39f970af8a8a222" hs_bindgen_d39f970af8a8a222_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f2@
hs_bindgen_d39f970af8a8a222 :: IO ()
hs_bindgen_d39f970af8a8a222 =
  RIP.fromFFIType hs_bindgen_d39f970af8a8a222_base

{-| __C declaration:__ @f2@

    __defined at:__ @attributes\/visibility\/functions.h 16:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f2 :: IO ()
f2 = hs_bindgen_d39f970af8a8a222

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f3@
foreign import ccall unsafe "hs_bindgen_be64763ec1549ecf" hs_bindgen_be64763ec1549ecf_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f3@
hs_bindgen_be64763ec1549ecf :: IO ()
hs_bindgen_be64763ec1549ecf =
  RIP.fromFFIType hs_bindgen_be64763ec1549ecf_base

{-| __C declaration:__ @f3@

    __defined at:__ @attributes\/visibility\/functions.h 17:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f3 :: IO ()
f3 = hs_bindgen_be64763ec1549ecf

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f4@
foreign import ccall unsafe "hs_bindgen_79aec631efd41b2a" hs_bindgen_79aec631efd41b2a_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f4@
hs_bindgen_79aec631efd41b2a :: IO ()
hs_bindgen_79aec631efd41b2a =
  RIP.fromFFIType hs_bindgen_79aec631efd41b2a_base

{-| __C declaration:__ @f4@

    __defined at:__ @attributes\/visibility\/functions.h 18:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f4 :: IO ()
f4 = hs_bindgen_79aec631efd41b2a

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f5@
foreign import ccall unsafe "hs_bindgen_def05384051a5dd9" hs_bindgen_def05384051a5dd9_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f5@
hs_bindgen_def05384051a5dd9 :: IO ()
hs_bindgen_def05384051a5dd9 =
  RIP.fromFFIType hs_bindgen_def05384051a5dd9_base

{-| __C declaration:__ @f5@

    __defined at:__ @attributes\/visibility\/functions.h 21:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f5 :: IO ()
f5 = hs_bindgen_def05384051a5dd9

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f6@
foreign import ccall unsafe "hs_bindgen_92d1f9974393a261" hs_bindgen_92d1f9974393a261_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f6@
hs_bindgen_92d1f9974393a261 :: IO ()
hs_bindgen_92d1f9974393a261 =
  RIP.fromFFIType hs_bindgen_92d1f9974393a261_base

{-| __C declaration:__ @f6@

    __defined at:__ @attributes\/visibility\/functions.h 22:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f6 :: IO ()
f6 = hs_bindgen_92d1f9974393a261

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f7@
foreign import ccall unsafe "hs_bindgen_90f3068e094c2abd" hs_bindgen_90f3068e094c2abd_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f7@
hs_bindgen_90f3068e094c2abd :: IO ()
hs_bindgen_90f3068e094c2abd =
  RIP.fromFFIType hs_bindgen_90f3068e094c2abd_base

{-| __C declaration:__ @f7@

    __defined at:__ @attributes\/visibility\/functions.h 23:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f7 :: IO ()
f7 = hs_bindgen_90f3068e094c2abd

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f8@
foreign import ccall unsafe "hs_bindgen_6d1cfd91b3d13e55" hs_bindgen_6d1cfd91b3d13e55_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f8@
hs_bindgen_6d1cfd91b3d13e55 :: IO ()
hs_bindgen_6d1cfd91b3d13e55 =
  RIP.fromFFIType hs_bindgen_6d1cfd91b3d13e55_base

{-| __C declaration:__ @f8@

    __defined at:__ @attributes\/visibility\/functions.h 24:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f8 :: IO ()
f8 = hs_bindgen_6d1cfd91b3d13e55

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f9@
foreign import ccall unsafe "hs_bindgen_d35fb9045bcdab71" hs_bindgen_d35fb9045bcdab71_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f9@
hs_bindgen_d35fb9045bcdab71 :: IO ()
hs_bindgen_d35fb9045bcdab71 =
  RIP.fromFFIType hs_bindgen_d35fb9045bcdab71_base

{-| __C declaration:__ @f9@

    __defined at:__ @attributes\/visibility\/functions.h 25:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f9 :: IO ()
f9 = hs_bindgen_d35fb9045bcdab71

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f10@
foreign import ccall unsafe "hs_bindgen_40c723b61f2bee28" hs_bindgen_40c723b61f2bee28_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f10@
hs_bindgen_40c723b61f2bee28 :: IO ()
hs_bindgen_40c723b61f2bee28 =
  RIP.fromFFIType hs_bindgen_40c723b61f2bee28_base

{-| __C declaration:__ @f10@

    __defined at:__ @attributes\/visibility\/functions.h 28:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f10 :: IO ()
f10 = hs_bindgen_40c723b61f2bee28

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f11@
foreign import ccall unsafe "hs_bindgen_53a546d2dc84168b" hs_bindgen_53a546d2dc84168b_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f11@
hs_bindgen_53a546d2dc84168b :: IO ()
hs_bindgen_53a546d2dc84168b =
  RIP.fromFFIType hs_bindgen_53a546d2dc84168b_base

{-| __C declaration:__ @f11@

    __defined at:__ @attributes\/visibility\/functions.h 29:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f11 :: IO ()
f11 = hs_bindgen_53a546d2dc84168b

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f12@
foreign import ccall unsafe "hs_bindgen_47e1da4038b5a9b4" hs_bindgen_47e1da4038b5a9b4_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f12@
hs_bindgen_47e1da4038b5a9b4 :: IO ()
hs_bindgen_47e1da4038b5a9b4 =
  RIP.fromFFIType hs_bindgen_47e1da4038b5a9b4_base

{-| __C declaration:__ @f12@

    __defined at:__ @attributes\/visibility\/functions.h 30:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f12 :: IO ()
f12 = hs_bindgen_47e1da4038b5a9b4

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f13@
foreign import ccall unsafe "hs_bindgen_3b90be410cc76ded" hs_bindgen_3b90be410cc76ded_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f13@
hs_bindgen_3b90be410cc76ded :: IO ()
hs_bindgen_3b90be410cc76ded =
  RIP.fromFFIType hs_bindgen_3b90be410cc76ded_base

{-| __C declaration:__ @f13@

    __defined at:__ @attributes\/visibility\/functions.h 31:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f13 :: IO ()
f13 = hs_bindgen_3b90be410cc76ded

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f14@
foreign import ccall unsafe "hs_bindgen_d7d157b17d6f4d9e" hs_bindgen_d7d157b17d6f4d9e_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f14@
hs_bindgen_d7d157b17d6f4d9e :: IO ()
hs_bindgen_d7d157b17d6f4d9e =
  RIP.fromFFIType hs_bindgen_d7d157b17d6f4d9e_base

{-| __C declaration:__ @f14@

    __defined at:__ @attributes\/visibility\/functions.h 32:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f14 :: IO ()
f14 = hs_bindgen_d7d157b17d6f4d9e

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f15@
foreign import ccall unsafe "hs_bindgen_32a0e71bc346c633" hs_bindgen_32a0e71bc346c633_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f15@
hs_bindgen_32a0e71bc346c633 :: IO ()
hs_bindgen_32a0e71bc346c633 =
  RIP.fromFFIType hs_bindgen_32a0e71bc346c633_base

{-| __C declaration:__ @f15@

    __defined at:__ @attributes\/visibility\/functions.h 35:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f15 :: IO ()
f15 = hs_bindgen_32a0e71bc346c633

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f16@
foreign import ccall unsafe "hs_bindgen_084799171b269718" hs_bindgen_084799171b269718_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f16@
hs_bindgen_084799171b269718 :: IO ()
hs_bindgen_084799171b269718 =
  RIP.fromFFIType hs_bindgen_084799171b269718_base

{-| __C declaration:__ @f16@

    __defined at:__ @attributes\/visibility\/functions.h 36:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f16 :: IO ()
f16 = hs_bindgen_084799171b269718

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f17@
foreign import ccall unsafe "hs_bindgen_9399b438ad7d1540" hs_bindgen_9399b438ad7d1540_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f17@
hs_bindgen_9399b438ad7d1540 :: IO ()
hs_bindgen_9399b438ad7d1540 =
  RIP.fromFFIType hs_bindgen_9399b438ad7d1540_base

{-| __C declaration:__ @f17@

    __defined at:__ @attributes\/visibility\/functions.h 37:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f17 :: IO ()
f17 = hs_bindgen_9399b438ad7d1540

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f18@
foreign import ccall unsafe "hs_bindgen_b0cfd100fa7549bb" hs_bindgen_b0cfd100fa7549bb_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f18@
hs_bindgen_b0cfd100fa7549bb :: IO ()
hs_bindgen_b0cfd100fa7549bb =
  RIP.fromFFIType hs_bindgen_b0cfd100fa7549bb_base

{-| __C declaration:__ @f18@

    __defined at:__ @attributes\/visibility\/functions.h 38:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f18 :: IO ()
f18 = hs_bindgen_b0cfd100fa7549bb

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f19@
foreign import ccall unsafe "hs_bindgen_00c6e9c82555231a" hs_bindgen_00c6e9c82555231a_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f19@
hs_bindgen_00c6e9c82555231a :: IO ()
hs_bindgen_00c6e9c82555231a =
  RIP.fromFFIType hs_bindgen_00c6e9c82555231a_base

{-| __C declaration:__ @f19@

    __defined at:__ @attributes\/visibility\/functions.h 39:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f19 :: IO ()
f19 = hs_bindgen_00c6e9c82555231a

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f20@
foreign import ccall unsafe "hs_bindgen_6630b37e4a8fd258" hs_bindgen_6630b37e4a8fd258_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f20@
hs_bindgen_6630b37e4a8fd258 :: IO ()
hs_bindgen_6630b37e4a8fd258 =
  RIP.fromFFIType hs_bindgen_6630b37e4a8fd258_base

{-| __C declaration:__ @f20@

    __defined at:__ @attributes\/visibility\/functions.h 42:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f20 :: IO ()
f20 = hs_bindgen_6630b37e4a8fd258

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f21@
foreign import ccall unsafe "hs_bindgen_1e5e762b3fc32381" hs_bindgen_1e5e762b3fc32381_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f21@
hs_bindgen_1e5e762b3fc32381 :: IO ()
hs_bindgen_1e5e762b3fc32381 =
  RIP.fromFFIType hs_bindgen_1e5e762b3fc32381_base

{-| __C declaration:__ @f21@

    __defined at:__ @attributes\/visibility\/functions.h 43:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f21 :: IO ()
f21 = hs_bindgen_1e5e762b3fc32381

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f22@
foreign import ccall unsafe "hs_bindgen_7a55ac9a1d761c51" hs_bindgen_7a55ac9a1d761c51_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f22@
hs_bindgen_7a55ac9a1d761c51 :: IO ()
hs_bindgen_7a55ac9a1d761c51 =
  RIP.fromFFIType hs_bindgen_7a55ac9a1d761c51_base

{-| __C declaration:__ @f22@

    __defined at:__ @attributes\/visibility\/functions.h 44:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f22 :: IO ()
f22 = hs_bindgen_7a55ac9a1d761c51

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f23@
foreign import ccall unsafe "hs_bindgen_6941e6411b943f0b" hs_bindgen_6941e6411b943f0b_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f23@
hs_bindgen_6941e6411b943f0b :: IO ()
hs_bindgen_6941e6411b943f0b =
  RIP.fromFFIType hs_bindgen_6941e6411b943f0b_base

{-| __C declaration:__ @f23@

    __defined at:__ @attributes\/visibility\/functions.h 45:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f23 :: IO ()
f23 = hs_bindgen_6941e6411b943f0b

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f24@
foreign import ccall unsafe "hs_bindgen_654278bb511c1626" hs_bindgen_654278bb511c1626_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f24@
hs_bindgen_654278bb511c1626 :: IO ()
hs_bindgen_654278bb511c1626 =
  RIP.fromFFIType hs_bindgen_654278bb511c1626_base

{-| __C declaration:__ @f24@

    __defined at:__ @attributes\/visibility\/functions.h 46:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f24 :: IO ()
f24 = hs_bindgen_654278bb511c1626

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f25@
foreign import ccall unsafe "hs_bindgen_5222f10cec613983" hs_bindgen_5222f10cec613983_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f25@
hs_bindgen_5222f10cec613983 :: IO ()
hs_bindgen_5222f10cec613983 =
  RIP.fromFFIType hs_bindgen_5222f10cec613983_base

{-| __C declaration:__ @f25@

    __defined at:__ @attributes\/visibility\/functions.h 49:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f25 :: IO ()
f25 = hs_bindgen_5222f10cec613983

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f26@
foreign import ccall unsafe "hs_bindgen_aa77927916c2e213" hs_bindgen_aa77927916c2e213_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f26@
hs_bindgen_aa77927916c2e213 :: IO ()
hs_bindgen_aa77927916c2e213 =
  RIP.fromFFIType hs_bindgen_aa77927916c2e213_base

{-| __C declaration:__ @f26@

    __defined at:__ @attributes\/visibility\/functions.h 50:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f26 :: IO ()
f26 = hs_bindgen_aa77927916c2e213

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f27@
foreign import ccall unsafe "hs_bindgen_102db79a88a6b692" hs_bindgen_102db79a88a6b692_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f27@
hs_bindgen_102db79a88a6b692 :: IO ()
hs_bindgen_102db79a88a6b692 =
  RIP.fromFFIType hs_bindgen_102db79a88a6b692_base

{-| __C declaration:__ @f27@

    __defined at:__ @attributes\/visibility\/functions.h 51:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f27 :: IO ()
f27 = hs_bindgen_102db79a88a6b692

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f28@
foreign import ccall unsafe "hs_bindgen_d475a551dd32819d" hs_bindgen_d475a551dd32819d_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f28@
hs_bindgen_d475a551dd32819d :: IO ()
hs_bindgen_d475a551dd32819d =
  RIP.fromFFIType hs_bindgen_d475a551dd32819d_base

{-| __C declaration:__ @f28@

    __defined at:__ @attributes\/visibility\/functions.h 52:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f28 :: IO ()
f28 = hs_bindgen_d475a551dd32819d

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f29@
foreign import ccall unsafe "hs_bindgen_afb63d27b0dea11b" hs_bindgen_afb63d27b0dea11b_base ::
     IO ()

-- __unique:__ @test_attributesvisibilityfunction_Example_Unsafe_f29@
hs_bindgen_afb63d27b0dea11b :: IO ()
hs_bindgen_afb63d27b0dea11b =
  RIP.fromFFIType hs_bindgen_afb63d27b0dea11b_base

{-| __C declaration:__ @f29@

    __defined at:__ @attributes\/visibility\/functions.h 53:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f29 :: IO ()
f29 = hs_bindgen_afb63d27b0dea11b
