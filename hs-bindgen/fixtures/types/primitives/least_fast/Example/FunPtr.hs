{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/primitives/least_fast.h>"
  , "/* test_typesprimitivesleast_fast_Example_get_int_least8_t_fun */"
  , "__attribute__ ((const))"
  , "int_least8_t (*hs_bindgen_0391876bad2745ce (void)) (void)"
  , "{"
  , "  return &int_least8_t_fun;"
  , "}"
  , "/* test_typesprimitivesleast_fast_Example_get_int_least16_t_fun */"
  , "__attribute__ ((const))"
  , "int_least16_t (*hs_bindgen_0af5d61729271291 (void)) (void)"
  , "{"
  , "  return &int_least16_t_fun;"
  , "}"
  , "/* test_typesprimitivesleast_fast_Example_get_int_least32_t_fun */"
  , "__attribute__ ((const))"
  , "int_least32_t (*hs_bindgen_335ec637d8d81f56 (void)) (void)"
  , "{"
  , "  return &int_least32_t_fun;"
  , "}"
  , "/* test_typesprimitivesleast_fast_Example_get_int_least64_t_fun */"
  , "__attribute__ ((const))"
  , "int_least64_t (*hs_bindgen_eedbb79d8eb0ca67 (void)) (void)"
  , "{"
  , "  return &int_least64_t_fun;"
  , "}"
  , "/* test_typesprimitivesleast_fast_Example_get_uint_least8_t_fun */"
  , "__attribute__ ((const))"
  , "uint_least8_t (*hs_bindgen_cae759b263e9dcdd (void)) (void)"
  , "{"
  , "  return &uint_least8_t_fun;"
  , "}"
  , "/* test_typesprimitivesleast_fast_Example_get_uint_least16_t_fun */"
  , "__attribute__ ((const))"
  , "uint_least16_t (*hs_bindgen_66a8cb9486f831fe (void)) (void)"
  , "{"
  , "  return &uint_least16_t_fun;"
  , "}"
  , "/* test_typesprimitivesleast_fast_Example_get_uint_least32_t_fun */"
  , "__attribute__ ((const))"
  , "uint_least32_t (*hs_bindgen_bfc0feef86d3e7d0 (void)) (void)"
  , "{"
  , "  return &uint_least32_t_fun;"
  , "}"
  , "/* test_typesprimitivesleast_fast_Example_get_uint_least64_t_fun */"
  , "__attribute__ ((const))"
  , "uint_least64_t (*hs_bindgen_1886bff0b6836087 (void)) (void)"
  , "{"
  , "  return &uint_least64_t_fun;"
  , "}"
  , "/* test_typesprimitivesleast_fast_Example_get_int_fast8_t_fun */"
  , "__attribute__ ((const))"
  , "int_fast8_t (*hs_bindgen_08f27d79a8ef226d (void)) (void)"
  , "{"
  , "  return &int_fast8_t_fun;"
  , "}"
  , "/* test_typesprimitivesleast_fast_Example_get_int_fast16_t_fun */"
  , "__attribute__ ((const))"
  , "int_fast16_t (*hs_bindgen_1bff6fbb3c56abd5 (void)) (void)"
  , "{"
  , "  return &int_fast16_t_fun;"
  , "}"
  , "/* test_typesprimitivesleast_fast_Example_get_int_fast32_t_fun */"
  , "__attribute__ ((const))"
  , "int_fast32_t (*hs_bindgen_8a1d629866650da2 (void)) (void)"
  , "{"
  , "  return &int_fast32_t_fun;"
  , "}"
  , "/* test_typesprimitivesleast_fast_Example_get_int_fast64_t_fun */"
  , "__attribute__ ((const))"
  , "int_fast64_t (*hs_bindgen_c112b0dedac2cffa (void)) (void)"
  , "{"
  , "  return &int_fast64_t_fun;"
  , "}"
  , "/* test_typesprimitivesleast_fast_Example_get_uint_fast8_t_fun */"
  , "__attribute__ ((const))"
  , "uint_fast8_t (*hs_bindgen_c20e96bd69bdb439 (void)) (void)"
  , "{"
  , "  return &uint_fast8_t_fun;"
  , "}"
  , "/* test_typesprimitivesleast_fast_Example_get_uint_fast16_t_fun */"
  , "__attribute__ ((const))"
  , "uint_fast16_t (*hs_bindgen_c4f607ba753f0616 (void)) (void)"
  , "{"
  , "  return &uint_fast16_t_fun;"
  , "}"
  , "/* test_typesprimitivesleast_fast_Example_get_uint_fast32_t_fun */"
  , "__attribute__ ((const))"
  , "uint_fast32_t (*hs_bindgen_c22f26851616c098 (void)) (void)"
  , "{"
  , "  return &uint_fast32_t_fun;"
  , "}"
  , "/* test_typesprimitivesleast_fast_Example_get_uint_fast64_t_fun */"
  , "__attribute__ ((const))"
  , "uint_fast64_t (*hs_bindgen_3ee59d750d0d2d2a (void)) (void)"
  , "{"
  , "  return &uint_fast64_t_fun;"
  , "}"
  ]))

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_int_least8_t_fun@
foreign import ccall unsafe "hs_bindgen_0391876bad2745ce" hs_bindgen_0391876bad2745ce_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_int_least8_t_fun@
hs_bindgen_0391876bad2745ce :: IO (Ptr.FunPtr (IO Int_least8_t))
hs_bindgen_0391876bad2745ce =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_0391876bad2745ce_base

{-# NOINLINE int_least8_t_fun #-}
{-| __C declaration:__ @int_least8_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 7:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_least8_t_fun :: Ptr.FunPtr (IO Int_least8_t)
int_least8_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0391876bad2745ce

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_int_least16_t_fun@
foreign import ccall unsafe "hs_bindgen_0af5d61729271291" hs_bindgen_0af5d61729271291_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_int_least16_t_fun@
hs_bindgen_0af5d61729271291 :: IO (Ptr.FunPtr (IO Int_least16_t))
hs_bindgen_0af5d61729271291 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_0af5d61729271291_base

{-# NOINLINE int_least16_t_fun #-}
{-| __C declaration:__ @int_least16_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 8:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_least16_t_fun :: Ptr.FunPtr (IO Int_least16_t)
int_least16_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0af5d61729271291

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_int_least32_t_fun@
foreign import ccall unsafe "hs_bindgen_335ec637d8d81f56" hs_bindgen_335ec637d8d81f56_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_int_least32_t_fun@
hs_bindgen_335ec637d8d81f56 :: IO (Ptr.FunPtr (IO Int_least32_t))
hs_bindgen_335ec637d8d81f56 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_335ec637d8d81f56_base

{-# NOINLINE int_least32_t_fun #-}
{-| __C declaration:__ @int_least32_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 9:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_least32_t_fun :: Ptr.FunPtr (IO Int_least32_t)
int_least32_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_335ec637d8d81f56

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_int_least64_t_fun@
foreign import ccall unsafe "hs_bindgen_eedbb79d8eb0ca67" hs_bindgen_eedbb79d8eb0ca67_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_int_least64_t_fun@
hs_bindgen_eedbb79d8eb0ca67 :: IO (Ptr.FunPtr (IO Int_least64_t))
hs_bindgen_eedbb79d8eb0ca67 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_eedbb79d8eb0ca67_base

{-# NOINLINE int_least64_t_fun #-}
{-| __C declaration:__ @int_least64_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 10:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_least64_t_fun :: Ptr.FunPtr (IO Int_least64_t)
int_least64_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_eedbb79d8eb0ca67

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_uint_least8_t_fun@
foreign import ccall unsafe "hs_bindgen_cae759b263e9dcdd" hs_bindgen_cae759b263e9dcdd_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_uint_least8_t_fun@
hs_bindgen_cae759b263e9dcdd :: IO (Ptr.FunPtr (IO Uint_least8_t))
hs_bindgen_cae759b263e9dcdd =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_cae759b263e9dcdd_base

{-# NOINLINE uint_least8_t_fun #-}
{-| __C declaration:__ @uint_least8_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 12:16@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_least8_t_fun :: Ptr.FunPtr (IO Uint_least8_t)
uint_least8_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cae759b263e9dcdd

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_uint_least16_t_fun@
foreign import ccall unsafe "hs_bindgen_66a8cb9486f831fe" hs_bindgen_66a8cb9486f831fe_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_uint_least16_t_fun@
hs_bindgen_66a8cb9486f831fe :: IO (Ptr.FunPtr (IO Uint_least16_t))
hs_bindgen_66a8cb9486f831fe =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_66a8cb9486f831fe_base

{-# NOINLINE uint_least16_t_fun #-}
{-| __C declaration:__ @uint_least16_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 13:16@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_least16_t_fun :: Ptr.FunPtr (IO Uint_least16_t)
uint_least16_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_66a8cb9486f831fe

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_uint_least32_t_fun@
foreign import ccall unsafe "hs_bindgen_bfc0feef86d3e7d0" hs_bindgen_bfc0feef86d3e7d0_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_uint_least32_t_fun@
hs_bindgen_bfc0feef86d3e7d0 :: IO (Ptr.FunPtr (IO Uint_least32_t))
hs_bindgen_bfc0feef86d3e7d0 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_bfc0feef86d3e7d0_base

{-# NOINLINE uint_least32_t_fun #-}
{-| __C declaration:__ @uint_least32_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 14:16@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_least32_t_fun :: Ptr.FunPtr (IO Uint_least32_t)
uint_least32_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bfc0feef86d3e7d0

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_uint_least64_t_fun@
foreign import ccall unsafe "hs_bindgen_1886bff0b6836087" hs_bindgen_1886bff0b6836087_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_uint_least64_t_fun@
hs_bindgen_1886bff0b6836087 :: IO (Ptr.FunPtr (IO Uint_least64_t))
hs_bindgen_1886bff0b6836087 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_1886bff0b6836087_base

{-# NOINLINE uint_least64_t_fun #-}
{-| __C declaration:__ @uint_least64_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 15:16@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_least64_t_fun :: Ptr.FunPtr (IO Uint_least64_t)
uint_least64_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1886bff0b6836087

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_int_fast8_t_fun@
foreign import ccall unsafe "hs_bindgen_08f27d79a8ef226d" hs_bindgen_08f27d79a8ef226d_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_int_fast8_t_fun@
hs_bindgen_08f27d79a8ef226d :: IO (Ptr.FunPtr (IO Int_fast8_t))
hs_bindgen_08f27d79a8ef226d =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_08f27d79a8ef226d_base

{-# NOINLINE int_fast8_t_fun #-}
{-| __C declaration:__ @int_fast8_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 17:14@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_fast8_t_fun :: Ptr.FunPtr (IO Int_fast8_t)
int_fast8_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_08f27d79a8ef226d

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_int_fast16_t_fun@
foreign import ccall unsafe "hs_bindgen_1bff6fbb3c56abd5" hs_bindgen_1bff6fbb3c56abd5_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_int_fast16_t_fun@
hs_bindgen_1bff6fbb3c56abd5 :: IO (Ptr.FunPtr (IO Int_fast16_t))
hs_bindgen_1bff6fbb3c56abd5 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_1bff6fbb3c56abd5_base

{-# NOINLINE int_fast16_t_fun #-}
{-| __C declaration:__ @int_fast16_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 18:14@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_fast16_t_fun :: Ptr.FunPtr (IO Int_fast16_t)
int_fast16_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1bff6fbb3c56abd5

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_int_fast32_t_fun@
foreign import ccall unsafe "hs_bindgen_8a1d629866650da2" hs_bindgen_8a1d629866650da2_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_int_fast32_t_fun@
hs_bindgen_8a1d629866650da2 :: IO (Ptr.FunPtr (IO Int_fast32_t))
hs_bindgen_8a1d629866650da2 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_8a1d629866650da2_base

{-# NOINLINE int_fast32_t_fun #-}
{-| __C declaration:__ @int_fast32_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 19:14@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_fast32_t_fun :: Ptr.FunPtr (IO Int_fast32_t)
int_fast32_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8a1d629866650da2

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_int_fast64_t_fun@
foreign import ccall unsafe "hs_bindgen_c112b0dedac2cffa" hs_bindgen_c112b0dedac2cffa_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_int_fast64_t_fun@
hs_bindgen_c112b0dedac2cffa :: IO (Ptr.FunPtr (IO Int_fast64_t))
hs_bindgen_c112b0dedac2cffa =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_c112b0dedac2cffa_base

{-# NOINLINE int_fast64_t_fun #-}
{-| __C declaration:__ @int_fast64_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 20:14@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_fast64_t_fun :: Ptr.FunPtr (IO Int_fast64_t)
int_fast64_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c112b0dedac2cffa

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_uint_fast8_t_fun@
foreign import ccall unsafe "hs_bindgen_c20e96bd69bdb439" hs_bindgen_c20e96bd69bdb439_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_uint_fast8_t_fun@
hs_bindgen_c20e96bd69bdb439 :: IO (Ptr.FunPtr (IO Uint_fast8_t))
hs_bindgen_c20e96bd69bdb439 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_c20e96bd69bdb439_base

{-# NOINLINE uint_fast8_t_fun #-}
{-| __C declaration:__ @uint_fast8_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 22:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_fast8_t_fun :: Ptr.FunPtr (IO Uint_fast8_t)
uint_fast8_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c20e96bd69bdb439

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_uint_fast16_t_fun@
foreign import ccall unsafe "hs_bindgen_c4f607ba753f0616" hs_bindgen_c4f607ba753f0616_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_uint_fast16_t_fun@
hs_bindgen_c4f607ba753f0616 :: IO (Ptr.FunPtr (IO Uint_fast16_t))
hs_bindgen_c4f607ba753f0616 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_c4f607ba753f0616_base

{-# NOINLINE uint_fast16_t_fun #-}
{-| __C declaration:__ @uint_fast16_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 23:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_fast16_t_fun :: Ptr.FunPtr (IO Uint_fast16_t)
uint_fast16_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c4f607ba753f0616

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_uint_fast32_t_fun@
foreign import ccall unsafe "hs_bindgen_c22f26851616c098" hs_bindgen_c22f26851616c098_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_uint_fast32_t_fun@
hs_bindgen_c22f26851616c098 :: IO (Ptr.FunPtr (IO Uint_fast32_t))
hs_bindgen_c22f26851616c098 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_c22f26851616c098_base

{-# NOINLINE uint_fast32_t_fun #-}
{-| __C declaration:__ @uint_fast32_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 24:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_fast32_t_fun :: Ptr.FunPtr (IO Uint_fast32_t)
uint_fast32_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c22f26851616c098

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_uint_fast64_t_fun@
foreign import ccall unsafe "hs_bindgen_3ee59d750d0d2d2a" hs_bindgen_3ee59d750d0d2d2a_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typesprimitivesleast_fast_Example_get_uint_fast64_t_fun@
hs_bindgen_3ee59d750d0d2d2a :: IO (Ptr.FunPtr (IO Uint_fast64_t))
hs_bindgen_3ee59d750d0d2d2a =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_3ee59d750d0d2d2a_base

{-# NOINLINE uint_fast64_t_fun #-}
{-| __C declaration:__ @uint_fast64_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 25:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_fast64_t_fun :: Ptr.FunPtr (IO Uint_fast64_t)
uint_fast64_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3ee59d750d0d2d2a
