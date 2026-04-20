{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.f1
    , Example.FunPtr.f2
    , Example.FunPtr.f3
    , Example.FunPtr.f4
    , Example.FunPtr.f5
    , Example.FunPtr.f6
    , Example.FunPtr.f7
    , Example.FunPtr.f8
    , Example.FunPtr.f9
    , Example.FunPtr.f10
    , Example.FunPtr.f11
    , Example.FunPtr.f12
    , Example.FunPtr.f13
    , Example.FunPtr.f14
    , Example.FunPtr.f15
    , Example.FunPtr.f16
    , Example.FunPtr.f17
    , Example.FunPtr.f18
    , Example.FunPtr.f19
    , Example.FunPtr.f20
    , Example.FunPtr.f21
    , Example.FunPtr.f22
    , Example.FunPtr.f23
    , Example.FunPtr.f24
    , Example.FunPtr.f25
    , Example.FunPtr.f26
    , Example.FunPtr.f27
    , Example.FunPtr.f28
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/reparse_arithmetic_types.h>"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f1 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_ebb99cc5ca909dbf (void)) ("
  , "  char arg1"
  , ")"
  , "{"
  , "  return &f1;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f2 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_3351ff3f4d8a5abd (void)) ("
  , "  signed char arg1"
  , ")"
  , "{"
  , "  return &f2;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f3 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_67c839668253cfe9 (void)) ("
  , "  unsigned char arg1"
  , ")"
  , "{"
  , "  return &f3;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f4 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_c828b90e5d5f18a1 (void)) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &f4;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f5 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_fd907368e0e31d4b (void)) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &f5;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f6 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_b4b66b392a99377b (void)) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &f6;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f7 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_ec3cd03026a1633f (void)) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &f7;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f8 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_e7b1c3a1c4f5b84a (void)) ("
  , "  unsigned short arg1"
  , ")"
  , "{"
  , "  return &f8;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f9 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_7accbeb833f46e12 (void)) ("
  , "  unsigned short arg1"
  , ")"
  , "{"
  , "  return &f9;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f10 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_aac0dc02a765d1b8 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &f10;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f11 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_2517979024c47c45 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &f11;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f12 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_3933d1f631ec58b0 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &f12;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f13 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_b68c62cf00f60189 (void)) ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return &f13;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f14 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_9b183f23a12ebff6 (void)) ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return &f14;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f15 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_c51083814d8ae2da (void)) ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return &f15;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f16 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_3db87e4d245da0b9 (void)) ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return &f16;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f17 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_c67c791eaecdf3e4 (void)) ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return &f17;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f18 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_b6aebb69d914fafc (void)) ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return &f18;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f19 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_b0fc0b5f9d3655f0 (void)) ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return &f19;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f20 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_52e415e600e55d2d (void)) ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return &f20;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f21 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_2190f01b986beb9b (void)) ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return &f21;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f22 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_8889f9f47fc90b7c (void)) ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return &f22;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f23 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_028951472795c23d (void)) ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return &f23;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f24 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_81579afc5577422e (void)) ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return &f24;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f25 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_81230c41df4ab36f (void)) ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return &f25;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f26 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_750219148022d1bd (void)) ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return &f26;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f27 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_76572bbe936a0500 (void)) ("
  , "  float arg1"
  , ")"
  , "{"
  , "  return &f27;"
  , "}"
  , "/* test_macrosreparse_arithmetic_type_Example_get_f28 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_3fd53b4ec725518a (void)) ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return &f28;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f1@
foreign import ccall unsafe "hs_bindgen_ebb99cc5ca909dbf" hs_bindgen_ebb99cc5ca909dbf_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f1@
hs_bindgen_ebb99cc5ca909dbf :: IO (RIP.FunPtr (RIP.CChar -> IO A))
hs_bindgen_ebb99cc5ca909dbf =
  RIP.fromFFIType hs_bindgen_ebb99cc5ca909dbf_base

{-# NOINLINE f1 #-}
{-| __C declaration:__ @f1@

    __defined at:__ @macros\/reparse_arithmetic_types.h 21:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f1 :: RIP.FunPtr (RIP.CChar -> IO A)
f1 = RIP.unsafePerformIO hs_bindgen_ebb99cc5ca909dbf

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f2@
foreign import ccall unsafe "hs_bindgen_3351ff3f4d8a5abd" hs_bindgen_3351ff3f4d8a5abd_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f2@
hs_bindgen_3351ff3f4d8a5abd :: IO (RIP.FunPtr (RIP.CSChar -> IO A))
hs_bindgen_3351ff3f4d8a5abd =
  RIP.fromFFIType hs_bindgen_3351ff3f4d8a5abd_base

{-# NOINLINE f2 #-}
{-| __C declaration:__ @f2@

    __defined at:__ @macros\/reparse_arithmetic_types.h 22:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f2 :: RIP.FunPtr (RIP.CSChar -> IO A)
f2 = RIP.unsafePerformIO hs_bindgen_3351ff3f4d8a5abd

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f3@
foreign import ccall unsafe "hs_bindgen_67c839668253cfe9" hs_bindgen_67c839668253cfe9_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f3@
hs_bindgen_67c839668253cfe9 :: IO (RIP.FunPtr (RIP.CUChar -> IO A))
hs_bindgen_67c839668253cfe9 =
  RIP.fromFFIType hs_bindgen_67c839668253cfe9_base

{-# NOINLINE f3 #-}
{-| __C declaration:__ @f3@

    __defined at:__ @macros\/reparse_arithmetic_types.h 23:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f3 :: RIP.FunPtr (RIP.CUChar -> IO A)
f3 = RIP.unsafePerformIO hs_bindgen_67c839668253cfe9

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f4@
foreign import ccall unsafe "hs_bindgen_c828b90e5d5f18a1" hs_bindgen_c828b90e5d5f18a1_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f4@
hs_bindgen_c828b90e5d5f18a1 :: IO (RIP.FunPtr (RIP.CShort -> IO A))
hs_bindgen_c828b90e5d5f18a1 =
  RIP.fromFFIType hs_bindgen_c828b90e5d5f18a1_base

{-# NOINLINE f4 #-}
{-| __C declaration:__ @f4@

    __defined at:__ @macros\/reparse_arithmetic_types.h 28:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f4 :: RIP.FunPtr (RIP.CShort -> IO A)
f4 = RIP.unsafePerformIO hs_bindgen_c828b90e5d5f18a1

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f5@
foreign import ccall unsafe "hs_bindgen_fd907368e0e31d4b" hs_bindgen_fd907368e0e31d4b_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f5@
hs_bindgen_fd907368e0e31d4b :: IO (RIP.FunPtr (RIP.CShort -> IO A))
hs_bindgen_fd907368e0e31d4b =
  RIP.fromFFIType hs_bindgen_fd907368e0e31d4b_base

{-# NOINLINE f5 #-}
{-| __C declaration:__ @f5@

    __defined at:__ @macros\/reparse_arithmetic_types.h 29:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f5 :: RIP.FunPtr (RIP.CShort -> IO A)
f5 = RIP.unsafePerformIO hs_bindgen_fd907368e0e31d4b

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f6@
foreign import ccall unsafe "hs_bindgen_b4b66b392a99377b" hs_bindgen_b4b66b392a99377b_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f6@
hs_bindgen_b4b66b392a99377b :: IO (RIP.FunPtr (RIP.CShort -> IO A))
hs_bindgen_b4b66b392a99377b =
  RIP.fromFFIType hs_bindgen_b4b66b392a99377b_base

{-# NOINLINE f6 #-}
{-| __C declaration:__ @f6@

    __defined at:__ @macros\/reparse_arithmetic_types.h 30:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f6 :: RIP.FunPtr (RIP.CShort -> IO A)
f6 = RIP.unsafePerformIO hs_bindgen_b4b66b392a99377b

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f7@
foreign import ccall unsafe "hs_bindgen_ec3cd03026a1633f" hs_bindgen_ec3cd03026a1633f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f7@
hs_bindgen_ec3cd03026a1633f :: IO (RIP.FunPtr (RIP.CShort -> IO A))
hs_bindgen_ec3cd03026a1633f =
  RIP.fromFFIType hs_bindgen_ec3cd03026a1633f_base

{-# NOINLINE f7 #-}
{-| __C declaration:__ @f7@

    __defined at:__ @macros\/reparse_arithmetic_types.h 31:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f7 :: RIP.FunPtr (RIP.CShort -> IO A)
f7 = RIP.unsafePerformIO hs_bindgen_ec3cd03026a1633f

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f8@
foreign import ccall unsafe "hs_bindgen_e7b1c3a1c4f5b84a" hs_bindgen_e7b1c3a1c4f5b84a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f8@
hs_bindgen_e7b1c3a1c4f5b84a :: IO (RIP.FunPtr (RIP.CUShort -> IO A))
hs_bindgen_e7b1c3a1c4f5b84a =
  RIP.fromFFIType hs_bindgen_e7b1c3a1c4f5b84a_base

{-# NOINLINE f8 #-}
{-| __C declaration:__ @f8@

    __defined at:__ @macros\/reparse_arithmetic_types.h 32:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f8 :: RIP.FunPtr (RIP.CUShort -> IO A)
f8 = RIP.unsafePerformIO hs_bindgen_e7b1c3a1c4f5b84a

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f9@
foreign import ccall unsafe "hs_bindgen_7accbeb833f46e12" hs_bindgen_7accbeb833f46e12_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f9@
hs_bindgen_7accbeb833f46e12 :: IO (RIP.FunPtr (RIP.CUShort -> IO A))
hs_bindgen_7accbeb833f46e12 =
  RIP.fromFFIType hs_bindgen_7accbeb833f46e12_base

{-# NOINLINE f9 #-}
{-| __C declaration:__ @f9@

    __defined at:__ @macros\/reparse_arithmetic_types.h 33:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f9 :: RIP.FunPtr (RIP.CUShort -> IO A)
f9 = RIP.unsafePerformIO hs_bindgen_7accbeb833f46e12

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f10@
foreign import ccall unsafe "hs_bindgen_aac0dc02a765d1b8" hs_bindgen_aac0dc02a765d1b8_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f10@
hs_bindgen_aac0dc02a765d1b8 :: IO (RIP.FunPtr (RIP.CInt -> IO A))
hs_bindgen_aac0dc02a765d1b8 =
  RIP.fromFFIType hs_bindgen_aac0dc02a765d1b8_base

{-# NOINLINE f10 #-}
{-| __C declaration:__ @f10@

    __defined at:__ @macros\/reparse_arithmetic_types.h 35:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f10 :: RIP.FunPtr (RIP.CInt -> IO A)
f10 = RIP.unsafePerformIO hs_bindgen_aac0dc02a765d1b8

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f11@
foreign import ccall unsafe "hs_bindgen_2517979024c47c45" hs_bindgen_2517979024c47c45_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f11@
hs_bindgen_2517979024c47c45 :: IO (RIP.FunPtr (RIP.CInt -> IO RIP.CInt))
hs_bindgen_2517979024c47c45 =
  RIP.fromFFIType hs_bindgen_2517979024c47c45_base

{-# NOINLINE f11 #-}
{-| __C declaration:__ @f11@

    __defined at:__ @macros\/reparse_arithmetic_types.h 36:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f11 :: RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
f11 = RIP.unsafePerformIO hs_bindgen_2517979024c47c45

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f12@
foreign import ccall unsafe "hs_bindgen_3933d1f631ec58b0" hs_bindgen_3933d1f631ec58b0_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f12@
hs_bindgen_3933d1f631ec58b0 :: IO (RIP.FunPtr (RIP.CInt -> IO A))
hs_bindgen_3933d1f631ec58b0 =
  RIP.fromFFIType hs_bindgen_3933d1f631ec58b0_base

{-# NOINLINE f12 #-}
{-| __C declaration:__ @f12@

    __defined at:__ @macros\/reparse_arithmetic_types.h 37:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f12 :: RIP.FunPtr (RIP.CInt -> IO A)
f12 = RIP.unsafePerformIO hs_bindgen_3933d1f631ec58b0

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f13@
foreign import ccall unsafe "hs_bindgen_b68c62cf00f60189" hs_bindgen_b68c62cf00f60189_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f13@
hs_bindgen_b68c62cf00f60189 :: IO (RIP.FunPtr (RIP.CUInt -> IO RIP.CInt))
hs_bindgen_b68c62cf00f60189 =
  RIP.fromFFIType hs_bindgen_b68c62cf00f60189_base

{-# NOINLINE f13 #-}
{-| __C declaration:__ @f13@

    __defined at:__ @macros\/reparse_arithmetic_types.h 38:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f13 :: RIP.FunPtr (RIP.CUInt -> IO RIP.CInt)
f13 = RIP.unsafePerformIO hs_bindgen_b68c62cf00f60189

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f14@
foreign import ccall unsafe "hs_bindgen_9b183f23a12ebff6" hs_bindgen_9b183f23a12ebff6_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f14@
hs_bindgen_9b183f23a12ebff6 :: IO (RIP.FunPtr (RIP.CUInt -> IO A))
hs_bindgen_9b183f23a12ebff6 =
  RIP.fromFFIType hs_bindgen_9b183f23a12ebff6_base

{-# NOINLINE f14 #-}
{-| __C declaration:__ @f14@

    __defined at:__ @macros\/reparse_arithmetic_types.h 39:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f14 :: RIP.FunPtr (RIP.CUInt -> IO A)
f14 = RIP.unsafePerformIO hs_bindgen_9b183f23a12ebff6

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f15@
foreign import ccall unsafe "hs_bindgen_c51083814d8ae2da" hs_bindgen_c51083814d8ae2da_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f15@
hs_bindgen_c51083814d8ae2da :: IO (RIP.FunPtr (RIP.CLong -> IO A))
hs_bindgen_c51083814d8ae2da =
  RIP.fromFFIType hs_bindgen_c51083814d8ae2da_base

{-# NOINLINE f15 #-}
{-| __C declaration:__ @f15@

    __defined at:__ @macros\/reparse_arithmetic_types.h 41:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f15 :: RIP.FunPtr (RIP.CLong -> IO A)
f15 = RIP.unsafePerformIO hs_bindgen_c51083814d8ae2da

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f16@
foreign import ccall unsafe "hs_bindgen_3db87e4d245da0b9" hs_bindgen_3db87e4d245da0b9_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f16@
hs_bindgen_3db87e4d245da0b9 :: IO (RIP.FunPtr (RIP.CLong -> IO A))
hs_bindgen_3db87e4d245da0b9 =
  RIP.fromFFIType hs_bindgen_3db87e4d245da0b9_base

{-# NOINLINE f16 #-}
{-| __C declaration:__ @f16@

    __defined at:__ @macros\/reparse_arithmetic_types.h 42:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f16 :: RIP.FunPtr (RIP.CLong -> IO A)
f16 = RIP.unsafePerformIO hs_bindgen_3db87e4d245da0b9

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f17@
foreign import ccall unsafe "hs_bindgen_c67c791eaecdf3e4" hs_bindgen_c67c791eaecdf3e4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f17@
hs_bindgen_c67c791eaecdf3e4 :: IO (RIP.FunPtr (RIP.CLong -> IO A))
hs_bindgen_c67c791eaecdf3e4 =
  RIP.fromFFIType hs_bindgen_c67c791eaecdf3e4_base

{-# NOINLINE f17 #-}
{-| __C declaration:__ @f17@

    __defined at:__ @macros\/reparse_arithmetic_types.h 43:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f17 :: RIP.FunPtr (RIP.CLong -> IO A)
f17 = RIP.unsafePerformIO hs_bindgen_c67c791eaecdf3e4

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f18@
foreign import ccall unsafe "hs_bindgen_b6aebb69d914fafc" hs_bindgen_b6aebb69d914fafc_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f18@
hs_bindgen_b6aebb69d914fafc :: IO (RIP.FunPtr (RIP.CLong -> IO A))
hs_bindgen_b6aebb69d914fafc =
  RIP.fromFFIType hs_bindgen_b6aebb69d914fafc_base

{-# NOINLINE f18 #-}
{-| __C declaration:__ @f18@

    __defined at:__ @macros\/reparse_arithmetic_types.h 44:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f18 :: RIP.FunPtr (RIP.CLong -> IO A)
f18 = RIP.unsafePerformIO hs_bindgen_b6aebb69d914fafc

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f19@
foreign import ccall unsafe "hs_bindgen_b0fc0b5f9d3655f0" hs_bindgen_b0fc0b5f9d3655f0_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f19@
hs_bindgen_b0fc0b5f9d3655f0 :: IO (RIP.FunPtr (RIP.CULong -> IO A))
hs_bindgen_b0fc0b5f9d3655f0 =
  RIP.fromFFIType hs_bindgen_b0fc0b5f9d3655f0_base

{-# NOINLINE f19 #-}
{-| __C declaration:__ @f19@

    __defined at:__ @macros\/reparse_arithmetic_types.h 45:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f19 :: RIP.FunPtr (RIP.CULong -> IO A)
f19 = RIP.unsafePerformIO hs_bindgen_b0fc0b5f9d3655f0

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f20@
foreign import ccall unsafe "hs_bindgen_52e415e600e55d2d" hs_bindgen_52e415e600e55d2d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f20@
hs_bindgen_52e415e600e55d2d :: IO (RIP.FunPtr (RIP.CULong -> IO A))
hs_bindgen_52e415e600e55d2d =
  RIP.fromFFIType hs_bindgen_52e415e600e55d2d_base

{-# NOINLINE f20 #-}
{-| __C declaration:__ @f20@

    __defined at:__ @macros\/reparse_arithmetic_types.h 46:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f20 :: RIP.FunPtr (RIP.CULong -> IO A)
f20 = RIP.unsafePerformIO hs_bindgen_52e415e600e55d2d

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f21@
foreign import ccall unsafe "hs_bindgen_2190f01b986beb9b" hs_bindgen_2190f01b986beb9b_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f21@
hs_bindgen_2190f01b986beb9b :: IO (RIP.FunPtr (RIP.CLong -> IO A))
hs_bindgen_2190f01b986beb9b =
  RIP.fromFFIType hs_bindgen_2190f01b986beb9b_base

{-# NOINLINE f21 #-}
{-| __C declaration:__ @f21@

    __defined at:__ @macros\/reparse_arithmetic_types.h 48:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f21 :: RIP.FunPtr (RIP.CLong -> IO A)
f21 = RIP.unsafePerformIO hs_bindgen_2190f01b986beb9b

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f22@
foreign import ccall unsafe "hs_bindgen_8889f9f47fc90b7c" hs_bindgen_8889f9f47fc90b7c_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f22@
hs_bindgen_8889f9f47fc90b7c :: IO (RIP.FunPtr (RIP.CLong -> IO A))
hs_bindgen_8889f9f47fc90b7c =
  RIP.fromFFIType hs_bindgen_8889f9f47fc90b7c_base

{-# NOINLINE f22 #-}
{-| __C declaration:__ @f22@

    __defined at:__ @macros\/reparse_arithmetic_types.h 49:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f22 :: RIP.FunPtr (RIP.CLong -> IO A)
f22 = RIP.unsafePerformIO hs_bindgen_8889f9f47fc90b7c

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f23@
foreign import ccall unsafe "hs_bindgen_028951472795c23d" hs_bindgen_028951472795c23d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f23@
hs_bindgen_028951472795c23d :: IO (RIP.FunPtr (RIP.CLong -> IO A))
hs_bindgen_028951472795c23d =
  RIP.fromFFIType hs_bindgen_028951472795c23d_base

{-# NOINLINE f23 #-}
{-| __C declaration:__ @f23@

    __defined at:__ @macros\/reparse_arithmetic_types.h 50:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f23 :: RIP.FunPtr (RIP.CLong -> IO A)
f23 = RIP.unsafePerformIO hs_bindgen_028951472795c23d

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f24@
foreign import ccall unsafe "hs_bindgen_81579afc5577422e" hs_bindgen_81579afc5577422e_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f24@
hs_bindgen_81579afc5577422e :: IO (RIP.FunPtr (RIP.CLong -> IO A))
hs_bindgen_81579afc5577422e =
  RIP.fromFFIType hs_bindgen_81579afc5577422e_base

{-# NOINLINE f24 #-}
{-| __C declaration:__ @f24@

    __defined at:__ @macros\/reparse_arithmetic_types.h 51:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f24 :: RIP.FunPtr (RIP.CLong -> IO A)
f24 = RIP.unsafePerformIO hs_bindgen_81579afc5577422e

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f25@
foreign import ccall unsafe "hs_bindgen_81230c41df4ab36f" hs_bindgen_81230c41df4ab36f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f25@
hs_bindgen_81230c41df4ab36f :: IO (RIP.FunPtr (RIP.CULong -> IO A))
hs_bindgen_81230c41df4ab36f =
  RIP.fromFFIType hs_bindgen_81230c41df4ab36f_base

{-# NOINLINE f25 #-}
{-| __C declaration:__ @f25@

    __defined at:__ @macros\/reparse_arithmetic_types.h 52:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f25 :: RIP.FunPtr (RIP.CULong -> IO A)
f25 = RIP.unsafePerformIO hs_bindgen_81230c41df4ab36f

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f26@
foreign import ccall unsafe "hs_bindgen_750219148022d1bd" hs_bindgen_750219148022d1bd_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f26@
hs_bindgen_750219148022d1bd :: IO (RIP.FunPtr (RIP.CULong -> IO A))
hs_bindgen_750219148022d1bd =
  RIP.fromFFIType hs_bindgen_750219148022d1bd_base

{-# NOINLINE f26 #-}
{-| __C declaration:__ @f26@

    __defined at:__ @macros\/reparse_arithmetic_types.h 53:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f26 :: RIP.FunPtr (RIP.CULong -> IO A)
f26 = RIP.unsafePerformIO hs_bindgen_750219148022d1bd

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f27@
foreign import ccall unsafe "hs_bindgen_76572bbe936a0500" hs_bindgen_76572bbe936a0500_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f27@
hs_bindgen_76572bbe936a0500 :: IO (RIP.FunPtr (RIP.CFloat -> IO A))
hs_bindgen_76572bbe936a0500 =
  RIP.fromFFIType hs_bindgen_76572bbe936a0500_base

{-# NOINLINE f27 #-}
{-| __C declaration:__ @f27@

    __defined at:__ @macros\/reparse_arithmetic_types.h 58:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f27 :: RIP.FunPtr (RIP.CFloat -> IO A)
f27 = RIP.unsafePerformIO hs_bindgen_76572bbe936a0500

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f28@
foreign import ccall unsafe "hs_bindgen_3fd53b4ec725518a" hs_bindgen_3fd53b4ec725518a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_get_f28@
hs_bindgen_3fd53b4ec725518a :: IO (RIP.FunPtr (RIP.CDouble -> IO A))
hs_bindgen_3fd53b4ec725518a =
  RIP.fromFFIType hs_bindgen_3fd53b4ec725518a_base

{-# NOINLINE f28 #-}
{-| __C declaration:__ @f28@

    __defined at:__ @macros\/reparse_arithmetic_types.h 59:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f28 :: RIP.FunPtr (RIP.CDouble -> IO A)
f28 = RIP.unsafePerformIO hs_bindgen_3fd53b4ec725518a
