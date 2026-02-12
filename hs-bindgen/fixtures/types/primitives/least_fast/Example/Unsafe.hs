{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified GHC.Int
import qualified GHC.Word
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/primitives/least_fast.h>"
  , "int_least8_t hs_bindgen_57bd94cbb0575f53 (void)"
  , "{"
  , "  return int_least8_t_fun();"
  , "}"
  , "int_least16_t hs_bindgen_f02c70143a88af90 (void)"
  , "{"
  , "  return int_least16_t_fun();"
  , "}"
  , "int_least32_t hs_bindgen_f5bbad7b8ac35f06 (void)"
  , "{"
  , "  return int_least32_t_fun();"
  , "}"
  , "int_least64_t hs_bindgen_b2036b95960e2751 (void)"
  , "{"
  , "  return int_least64_t_fun();"
  , "}"
  , "uint_least8_t hs_bindgen_d893fc1d41730120 (void)"
  , "{"
  , "  return uint_least8_t_fun();"
  , "}"
  , "uint_least16_t hs_bindgen_eaf2d3e09a41513e (void)"
  , "{"
  , "  return uint_least16_t_fun();"
  , "}"
  , "uint_least32_t hs_bindgen_dfdb9c77b9f2e16d (void)"
  , "{"
  , "  return uint_least32_t_fun();"
  , "}"
  , "uint_least64_t hs_bindgen_35c7ddf6def5281e (void)"
  , "{"
  , "  return uint_least64_t_fun();"
  , "}"
  , "int_fast8_t hs_bindgen_5144ac530f23abd8 (void)"
  , "{"
  , "  return int_fast8_t_fun();"
  , "}"
  , "int_fast16_t hs_bindgen_a102db8605e0b669 (void)"
  , "{"
  , "  return int_fast16_t_fun();"
  , "}"
  , "int_fast32_t hs_bindgen_97faff4568d5bc6a (void)"
  , "{"
  , "  return int_fast32_t_fun();"
  , "}"
  , "int_fast64_t hs_bindgen_ce1a64e61a9f9432 (void)"
  , "{"
  , "  return int_fast64_t_fun();"
  , "}"
  , "uint_fast8_t hs_bindgen_653d74ee9fd982e2 (void)"
  , "{"
  , "  return uint_fast8_t_fun();"
  , "}"
  , "uint_fast16_t hs_bindgen_31bf7eba8a098bae (void)"
  , "{"
  , "  return uint_fast16_t_fun();"
  , "}"
  , "uint_fast32_t hs_bindgen_fb324d72364e5685 (void)"
  , "{"
  , "  return uint_fast32_t_fun();"
  , "}"
  , "uint_fast64_t hs_bindgen_a2995f3603c68c65 (void)"
  , "{"
  , "  return uint_fast64_t_fun();"
  , "}"
  ]))

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_int_least8_t_fun@
foreign import ccall unsafe "hs_bindgen_57bd94cbb0575f53" hs_bindgen_57bd94cbb0575f53_base ::
     IO GHC.Int.Int8

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_int_least8_t_fun@
hs_bindgen_57bd94cbb0575f53 :: IO Int_least8_t
hs_bindgen_57bd94cbb0575f53 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_57bd94cbb0575f53_base

{-| __C declaration:__ @int_least8_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 7:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_least8_t_fun :: IO Int_least8_t
int_least8_t_fun = hs_bindgen_57bd94cbb0575f53

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_int_least16_t_fun@
foreign import ccall unsafe "hs_bindgen_f02c70143a88af90" hs_bindgen_f02c70143a88af90_base ::
     IO GHC.Int.Int16

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_int_least16_t_fun@
hs_bindgen_f02c70143a88af90 :: IO Int_least16_t
hs_bindgen_f02c70143a88af90 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_f02c70143a88af90_base

{-| __C declaration:__ @int_least16_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 8:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_least16_t_fun :: IO Int_least16_t
int_least16_t_fun = hs_bindgen_f02c70143a88af90

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_int_least32_t_fun@
foreign import ccall unsafe "hs_bindgen_f5bbad7b8ac35f06" hs_bindgen_f5bbad7b8ac35f06_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_int_least32_t_fun@
hs_bindgen_f5bbad7b8ac35f06 :: IO Int_least32_t
hs_bindgen_f5bbad7b8ac35f06 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_f5bbad7b8ac35f06_base

{-| __C declaration:__ @int_least32_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 9:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_least32_t_fun :: IO Int_least32_t
int_least32_t_fun = hs_bindgen_f5bbad7b8ac35f06

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_int_least64_t_fun@
foreign import ccall unsafe "hs_bindgen_b2036b95960e2751" hs_bindgen_b2036b95960e2751_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_int_least64_t_fun@
hs_bindgen_b2036b95960e2751 :: IO Int_least64_t
hs_bindgen_b2036b95960e2751 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_b2036b95960e2751_base

{-| __C declaration:__ @int_least64_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 10:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_least64_t_fun :: IO Int_least64_t
int_least64_t_fun = hs_bindgen_b2036b95960e2751

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_uint_least8_t_fun@
foreign import ccall unsafe "hs_bindgen_d893fc1d41730120" hs_bindgen_d893fc1d41730120_base ::
     IO GHC.Word.Word8

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_uint_least8_t_fun@
hs_bindgen_d893fc1d41730120 :: IO Uint_least8_t
hs_bindgen_d893fc1d41730120 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_d893fc1d41730120_base

{-| __C declaration:__ @uint_least8_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 12:16@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_least8_t_fun :: IO Uint_least8_t
uint_least8_t_fun = hs_bindgen_d893fc1d41730120

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_uint_least16_t_fun@
foreign import ccall unsafe "hs_bindgen_eaf2d3e09a41513e" hs_bindgen_eaf2d3e09a41513e_base ::
     IO GHC.Word.Word16

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_uint_least16_t_fun@
hs_bindgen_eaf2d3e09a41513e :: IO Uint_least16_t
hs_bindgen_eaf2d3e09a41513e =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_eaf2d3e09a41513e_base

{-| __C declaration:__ @uint_least16_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 13:16@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_least16_t_fun :: IO Uint_least16_t
uint_least16_t_fun = hs_bindgen_eaf2d3e09a41513e

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_uint_least32_t_fun@
foreign import ccall unsafe "hs_bindgen_dfdb9c77b9f2e16d" hs_bindgen_dfdb9c77b9f2e16d_base ::
     IO GHC.Word.Word32

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_uint_least32_t_fun@
hs_bindgen_dfdb9c77b9f2e16d :: IO Uint_least32_t
hs_bindgen_dfdb9c77b9f2e16d =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_dfdb9c77b9f2e16d_base

{-| __C declaration:__ @uint_least32_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 14:16@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_least32_t_fun :: IO Uint_least32_t
uint_least32_t_fun = hs_bindgen_dfdb9c77b9f2e16d

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_uint_least64_t_fun@
foreign import ccall unsafe "hs_bindgen_35c7ddf6def5281e" hs_bindgen_35c7ddf6def5281e_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_uint_least64_t_fun@
hs_bindgen_35c7ddf6def5281e :: IO Uint_least64_t
hs_bindgen_35c7ddf6def5281e =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_35c7ddf6def5281e_base

{-| __C declaration:__ @uint_least64_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 15:16@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_least64_t_fun :: IO Uint_least64_t
uint_least64_t_fun = hs_bindgen_35c7ddf6def5281e

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_int_fast8_t_fun@
foreign import ccall unsafe "hs_bindgen_5144ac530f23abd8" hs_bindgen_5144ac530f23abd8_base ::
     IO GHC.Int.Int8

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_int_fast8_t_fun@
hs_bindgen_5144ac530f23abd8 :: IO Int_fast8_t
hs_bindgen_5144ac530f23abd8 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_5144ac530f23abd8_base

{-| __C declaration:__ @int_fast8_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 17:14@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_fast8_t_fun :: IO Int_fast8_t
int_fast8_t_fun = hs_bindgen_5144ac530f23abd8

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_int_fast16_t_fun@
foreign import ccall unsafe "hs_bindgen_a102db8605e0b669" hs_bindgen_a102db8605e0b669_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_int_fast16_t_fun@
hs_bindgen_a102db8605e0b669 :: IO Int_fast16_t
hs_bindgen_a102db8605e0b669 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_a102db8605e0b669_base

{-| __C declaration:__ @int_fast16_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 18:14@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_fast16_t_fun :: IO Int_fast16_t
int_fast16_t_fun = hs_bindgen_a102db8605e0b669

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_int_fast32_t_fun@
foreign import ccall unsafe "hs_bindgen_97faff4568d5bc6a" hs_bindgen_97faff4568d5bc6a_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_int_fast32_t_fun@
hs_bindgen_97faff4568d5bc6a :: IO Int_fast32_t
hs_bindgen_97faff4568d5bc6a =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_97faff4568d5bc6a_base

{-| __C declaration:__ @int_fast32_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 19:14@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_fast32_t_fun :: IO Int_fast32_t
int_fast32_t_fun = hs_bindgen_97faff4568d5bc6a

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_int_fast64_t_fun@
foreign import ccall unsafe "hs_bindgen_ce1a64e61a9f9432" hs_bindgen_ce1a64e61a9f9432_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_int_fast64_t_fun@
hs_bindgen_ce1a64e61a9f9432 :: IO Int_fast64_t
hs_bindgen_ce1a64e61a9f9432 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_ce1a64e61a9f9432_base

{-| __C declaration:__ @int_fast64_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 20:14@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_fast64_t_fun :: IO Int_fast64_t
int_fast64_t_fun = hs_bindgen_ce1a64e61a9f9432

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_uint_fast8_t_fun@
foreign import ccall unsafe "hs_bindgen_653d74ee9fd982e2" hs_bindgen_653d74ee9fd982e2_base ::
     IO GHC.Word.Word8

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_uint_fast8_t_fun@
hs_bindgen_653d74ee9fd982e2 :: IO Uint_fast8_t
hs_bindgen_653d74ee9fd982e2 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_653d74ee9fd982e2_base

{-| __C declaration:__ @uint_fast8_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 22:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_fast8_t_fun :: IO Uint_fast8_t
uint_fast8_t_fun = hs_bindgen_653d74ee9fd982e2

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_uint_fast16_t_fun@
foreign import ccall unsafe "hs_bindgen_31bf7eba8a098bae" hs_bindgen_31bf7eba8a098bae_base ::
     IO GHC.Word.Word32

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_uint_fast16_t_fun@
hs_bindgen_31bf7eba8a098bae :: IO Uint_fast16_t
hs_bindgen_31bf7eba8a098bae =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_31bf7eba8a098bae_base

{-| __C declaration:__ @uint_fast16_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 23:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_fast16_t_fun :: IO Uint_fast16_t
uint_fast16_t_fun = hs_bindgen_31bf7eba8a098bae

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_uint_fast32_t_fun@
foreign import ccall unsafe "hs_bindgen_fb324d72364e5685" hs_bindgen_fb324d72364e5685_base ::
     IO GHC.Word.Word32

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_uint_fast32_t_fun@
hs_bindgen_fb324d72364e5685 :: IO Uint_fast32_t
hs_bindgen_fb324d72364e5685 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_fb324d72364e5685_base

{-| __C declaration:__ @uint_fast32_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 24:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_fast32_t_fun :: IO Uint_fast32_t
uint_fast32_t_fun = hs_bindgen_fb324d72364e5685

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_uint_fast64_t_fun@
foreign import ccall unsafe "hs_bindgen_a2995f3603c68c65" hs_bindgen_a2995f3603c68c65_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_typesprimitivesleast_fast_Example_Unsafe_uint_fast64_t_fun@
hs_bindgen_a2995f3603c68c65 :: IO Uint_fast64_t
hs_bindgen_a2995f3603c68c65 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_a2995f3603c68c65_base

{-| __C declaration:__ @uint_fast64_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 25:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_fast64_t_fun :: IO Uint_fast64_t
uint_fast64_t_fun = hs_bindgen_a2995f3603c68c65
