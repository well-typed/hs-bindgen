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
  [ "#include <macros/reparse/arithmetic_types.h>"
  , "/* test_macrosreparsearithmetic_type_Example_get_f1 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_1208d85624465c51 (void)) ("
  , "  char arg1"
  , ")"
  , "{"
  , "  return &f1;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f2 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_90bf738ed6e126cf (void)) ("
  , "  signed char arg1"
  , ")"
  , "{"
  , "  return &f2;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f3 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_541fb107a8c068d0 (void)) ("
  , "  unsigned char arg1"
  , ")"
  , "{"
  , "  return &f3;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f4 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_695a21f5c624ca64 (void)) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &f4;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f5 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_43491bdffe43bbe1 (void)) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &f5;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f6 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_ffafdd943146661a (void)) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &f6;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f7 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_7c6250122a3c59a3 (void)) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &f7;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f8 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_60cbc6e072d14643 (void)) ("
  , "  unsigned short arg1"
  , ")"
  , "{"
  , "  return &f8;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f9 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_fa356fa03c563829 (void)) ("
  , "  unsigned short arg1"
  , ")"
  , "{"
  , "  return &f9;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f10 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_6971f14ec2033453 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &f10;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f11 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_18e46ae0dfd9dbe0 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &f11;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f12 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_0c1c36784fe71810 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &f12;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f13 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_8d9bec0d1944b558 (void)) ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return &f13;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f14 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_796f92f5831916a6 (void)) ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return &f14;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f15 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_2cb6292b4cc8f420 (void)) ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return &f15;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f16 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_b7dd3e78d24cd30c (void)) ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return &f16;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f17 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_894ff9aa74f040d9 (void)) ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return &f17;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f18 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_57359ef6c14eb820 (void)) ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return &f18;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f19 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_2173cee7470d34a4 (void)) ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return &f19;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f20 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_85dc99217461b608 (void)) ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return &f20;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f21 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_350486e538e7ca90 (void)) ("
  , "  signed long long arg1"
  , ")"
  , "{"
  , "  return &f21;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f22 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_ad2c8dc646b29be0 (void)) ("
  , "  signed long long arg1"
  , ")"
  , "{"
  , "  return &f22;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f23 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_01837bab50ebbe4c (void)) ("
  , "  signed long long arg1"
  , ")"
  , "{"
  , "  return &f23;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f24 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_051d17dfd1125c86 (void)) ("
  , "  signed long long arg1"
  , ")"
  , "{"
  , "  return &f24;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f25 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_ba5dd3a36d6f2756 (void)) ("
  , "  unsigned long long arg1"
  , ")"
  , "{"
  , "  return &f25;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f26 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_22f3b97563fd757f (void)) ("
  , "  unsigned long long arg1"
  , ")"
  , "{"
  , "  return &f26;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f27 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_fc81895dfae9b1e5 (void)) ("
  , "  float arg1"
  , ")"
  , "{"
  , "  return &f27;"
  , "}"
  , "/* test_macrosreparsearithmetic_type_Example_get_f28 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_f95062f509265011 (void)) ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return &f28;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f1@
foreign import ccall unsafe "hs_bindgen_1208d85624465c51" hs_bindgen_1208d85624465c51_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f1@
hs_bindgen_1208d85624465c51 :: IO (RIP.FunPtr (RIP.CChar -> IO A))
hs_bindgen_1208d85624465c51 =
  RIP.fromFFIType hs_bindgen_1208d85624465c51_base

{-# NOINLINE f1 #-}
{-| __C declaration:__ @f1@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 21:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f1 :: RIP.FunPtr (RIP.CChar -> IO A)
f1 = RIP.unsafePerformIO hs_bindgen_1208d85624465c51

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f2@
foreign import ccall unsafe "hs_bindgen_90bf738ed6e126cf" hs_bindgen_90bf738ed6e126cf_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f2@
hs_bindgen_90bf738ed6e126cf :: IO (RIP.FunPtr (RIP.CSChar -> IO A))
hs_bindgen_90bf738ed6e126cf =
  RIP.fromFFIType hs_bindgen_90bf738ed6e126cf_base

{-# NOINLINE f2 #-}
{-| __C declaration:__ @f2@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 22:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f2 :: RIP.FunPtr (RIP.CSChar -> IO A)
f2 = RIP.unsafePerformIO hs_bindgen_90bf738ed6e126cf

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f3@
foreign import ccall unsafe "hs_bindgen_541fb107a8c068d0" hs_bindgen_541fb107a8c068d0_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f3@
hs_bindgen_541fb107a8c068d0 :: IO (RIP.FunPtr (RIP.CUChar -> IO A))
hs_bindgen_541fb107a8c068d0 =
  RIP.fromFFIType hs_bindgen_541fb107a8c068d0_base

{-# NOINLINE f3 #-}
{-| __C declaration:__ @f3@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 23:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f3 :: RIP.FunPtr (RIP.CUChar -> IO A)
f3 = RIP.unsafePerformIO hs_bindgen_541fb107a8c068d0

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f4@
foreign import ccall unsafe "hs_bindgen_695a21f5c624ca64" hs_bindgen_695a21f5c624ca64_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f4@
hs_bindgen_695a21f5c624ca64 :: IO (RIP.FunPtr (RIP.CShort -> IO A))
hs_bindgen_695a21f5c624ca64 =
  RIP.fromFFIType hs_bindgen_695a21f5c624ca64_base

{-# NOINLINE f4 #-}
{-| __C declaration:__ @f4@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 28:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f4 :: RIP.FunPtr (RIP.CShort -> IO A)
f4 = RIP.unsafePerformIO hs_bindgen_695a21f5c624ca64

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f5@
foreign import ccall unsafe "hs_bindgen_43491bdffe43bbe1" hs_bindgen_43491bdffe43bbe1_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f5@
hs_bindgen_43491bdffe43bbe1 :: IO (RIP.FunPtr (RIP.CShort -> IO A))
hs_bindgen_43491bdffe43bbe1 =
  RIP.fromFFIType hs_bindgen_43491bdffe43bbe1_base

{-# NOINLINE f5 #-}
{-| __C declaration:__ @f5@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 29:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f5 :: RIP.FunPtr (RIP.CShort -> IO A)
f5 = RIP.unsafePerformIO hs_bindgen_43491bdffe43bbe1

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f6@
foreign import ccall unsafe "hs_bindgen_ffafdd943146661a" hs_bindgen_ffafdd943146661a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f6@
hs_bindgen_ffafdd943146661a :: IO (RIP.FunPtr (RIP.CShort -> IO A))
hs_bindgen_ffafdd943146661a =
  RIP.fromFFIType hs_bindgen_ffafdd943146661a_base

{-# NOINLINE f6 #-}
{-| __C declaration:__ @f6@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 30:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f6 :: RIP.FunPtr (RIP.CShort -> IO A)
f6 = RIP.unsafePerformIO hs_bindgen_ffafdd943146661a

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f7@
foreign import ccall unsafe "hs_bindgen_7c6250122a3c59a3" hs_bindgen_7c6250122a3c59a3_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f7@
hs_bindgen_7c6250122a3c59a3 :: IO (RIP.FunPtr (RIP.CShort -> IO A))
hs_bindgen_7c6250122a3c59a3 =
  RIP.fromFFIType hs_bindgen_7c6250122a3c59a3_base

{-# NOINLINE f7 #-}
{-| __C declaration:__ @f7@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 31:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f7 :: RIP.FunPtr (RIP.CShort -> IO A)
f7 = RIP.unsafePerformIO hs_bindgen_7c6250122a3c59a3

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f8@
foreign import ccall unsafe "hs_bindgen_60cbc6e072d14643" hs_bindgen_60cbc6e072d14643_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f8@
hs_bindgen_60cbc6e072d14643 :: IO (RIP.FunPtr (RIP.CUShort -> IO A))
hs_bindgen_60cbc6e072d14643 =
  RIP.fromFFIType hs_bindgen_60cbc6e072d14643_base

{-# NOINLINE f8 #-}
{-| __C declaration:__ @f8@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 32:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f8 :: RIP.FunPtr (RIP.CUShort -> IO A)
f8 = RIP.unsafePerformIO hs_bindgen_60cbc6e072d14643

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f9@
foreign import ccall unsafe "hs_bindgen_fa356fa03c563829" hs_bindgen_fa356fa03c563829_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f9@
hs_bindgen_fa356fa03c563829 :: IO (RIP.FunPtr (RIP.CUShort -> IO A))
hs_bindgen_fa356fa03c563829 =
  RIP.fromFFIType hs_bindgen_fa356fa03c563829_base

{-# NOINLINE f9 #-}
{-| __C declaration:__ @f9@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 33:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f9 :: RIP.FunPtr (RIP.CUShort -> IO A)
f9 = RIP.unsafePerformIO hs_bindgen_fa356fa03c563829

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f10@
foreign import ccall unsafe "hs_bindgen_6971f14ec2033453" hs_bindgen_6971f14ec2033453_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f10@
hs_bindgen_6971f14ec2033453 :: IO (RIP.FunPtr (RIP.CInt -> IO A))
hs_bindgen_6971f14ec2033453 =
  RIP.fromFFIType hs_bindgen_6971f14ec2033453_base

{-# NOINLINE f10 #-}
{-| __C declaration:__ @f10@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 35:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f10 :: RIP.FunPtr (RIP.CInt -> IO A)
f10 = RIP.unsafePerformIO hs_bindgen_6971f14ec2033453

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f11@
foreign import ccall unsafe "hs_bindgen_18e46ae0dfd9dbe0" hs_bindgen_18e46ae0dfd9dbe0_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f11@
hs_bindgen_18e46ae0dfd9dbe0 :: IO (RIP.FunPtr (RIP.CInt -> IO A))
hs_bindgen_18e46ae0dfd9dbe0 =
  RIP.fromFFIType hs_bindgen_18e46ae0dfd9dbe0_base

{-# NOINLINE f11 #-}
{-| __C declaration:__ @f11@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 36:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f11 :: RIP.FunPtr (RIP.CInt -> IO A)
f11 = RIP.unsafePerformIO hs_bindgen_18e46ae0dfd9dbe0

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f12@
foreign import ccall unsafe "hs_bindgen_0c1c36784fe71810" hs_bindgen_0c1c36784fe71810_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f12@
hs_bindgen_0c1c36784fe71810 :: IO (RIP.FunPtr (RIP.CInt -> IO A))
hs_bindgen_0c1c36784fe71810 =
  RIP.fromFFIType hs_bindgen_0c1c36784fe71810_base

{-# NOINLINE f12 #-}
{-| __C declaration:__ @f12@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 37:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f12 :: RIP.FunPtr (RIP.CInt -> IO A)
f12 = RIP.unsafePerformIO hs_bindgen_0c1c36784fe71810

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f13@
foreign import ccall unsafe "hs_bindgen_8d9bec0d1944b558" hs_bindgen_8d9bec0d1944b558_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f13@
hs_bindgen_8d9bec0d1944b558 :: IO (RIP.FunPtr (RIP.CUInt -> IO A))
hs_bindgen_8d9bec0d1944b558 =
  RIP.fromFFIType hs_bindgen_8d9bec0d1944b558_base

{-# NOINLINE f13 #-}
{-| __C declaration:__ @f13@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 38:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f13 :: RIP.FunPtr (RIP.CUInt -> IO A)
f13 = RIP.unsafePerformIO hs_bindgen_8d9bec0d1944b558

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f14@
foreign import ccall unsafe "hs_bindgen_796f92f5831916a6" hs_bindgen_796f92f5831916a6_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f14@
hs_bindgen_796f92f5831916a6 :: IO (RIP.FunPtr (RIP.CUInt -> IO A))
hs_bindgen_796f92f5831916a6 =
  RIP.fromFFIType hs_bindgen_796f92f5831916a6_base

{-# NOINLINE f14 #-}
{-| __C declaration:__ @f14@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 39:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f14 :: RIP.FunPtr (RIP.CUInt -> IO A)
f14 = RIP.unsafePerformIO hs_bindgen_796f92f5831916a6

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f15@
foreign import ccall unsafe "hs_bindgen_2cb6292b4cc8f420" hs_bindgen_2cb6292b4cc8f420_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f15@
hs_bindgen_2cb6292b4cc8f420 :: IO (RIP.FunPtr (RIP.CLong -> IO A))
hs_bindgen_2cb6292b4cc8f420 =
  RIP.fromFFIType hs_bindgen_2cb6292b4cc8f420_base

{-# NOINLINE f15 #-}
{-| __C declaration:__ @f15@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 41:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f15 :: RIP.FunPtr (RIP.CLong -> IO A)
f15 = RIP.unsafePerformIO hs_bindgen_2cb6292b4cc8f420

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f16@
foreign import ccall unsafe "hs_bindgen_b7dd3e78d24cd30c" hs_bindgen_b7dd3e78d24cd30c_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f16@
hs_bindgen_b7dd3e78d24cd30c :: IO (RIP.FunPtr (RIP.CLong -> IO A))
hs_bindgen_b7dd3e78d24cd30c =
  RIP.fromFFIType hs_bindgen_b7dd3e78d24cd30c_base

{-# NOINLINE f16 #-}
{-| __C declaration:__ @f16@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 42:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f16 :: RIP.FunPtr (RIP.CLong -> IO A)
f16 = RIP.unsafePerformIO hs_bindgen_b7dd3e78d24cd30c

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f17@
foreign import ccall unsafe "hs_bindgen_894ff9aa74f040d9" hs_bindgen_894ff9aa74f040d9_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f17@
hs_bindgen_894ff9aa74f040d9 :: IO (RIP.FunPtr (RIP.CLong -> IO A))
hs_bindgen_894ff9aa74f040d9 =
  RIP.fromFFIType hs_bindgen_894ff9aa74f040d9_base

{-# NOINLINE f17 #-}
{-| __C declaration:__ @f17@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 43:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f17 :: RIP.FunPtr (RIP.CLong -> IO A)
f17 = RIP.unsafePerformIO hs_bindgen_894ff9aa74f040d9

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f18@
foreign import ccall unsafe "hs_bindgen_57359ef6c14eb820" hs_bindgen_57359ef6c14eb820_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f18@
hs_bindgen_57359ef6c14eb820 :: IO (RIP.FunPtr (RIP.CLong -> IO A))
hs_bindgen_57359ef6c14eb820 =
  RIP.fromFFIType hs_bindgen_57359ef6c14eb820_base

{-# NOINLINE f18 #-}
{-| __C declaration:__ @f18@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 44:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f18 :: RIP.FunPtr (RIP.CLong -> IO A)
f18 = RIP.unsafePerformIO hs_bindgen_57359ef6c14eb820

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f19@
foreign import ccall unsafe "hs_bindgen_2173cee7470d34a4" hs_bindgen_2173cee7470d34a4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f19@
hs_bindgen_2173cee7470d34a4 :: IO (RIP.FunPtr (RIP.CULong -> IO A))
hs_bindgen_2173cee7470d34a4 =
  RIP.fromFFIType hs_bindgen_2173cee7470d34a4_base

{-# NOINLINE f19 #-}
{-| __C declaration:__ @f19@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 45:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f19 :: RIP.FunPtr (RIP.CULong -> IO A)
f19 = RIP.unsafePerformIO hs_bindgen_2173cee7470d34a4

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f20@
foreign import ccall unsafe "hs_bindgen_85dc99217461b608" hs_bindgen_85dc99217461b608_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f20@
hs_bindgen_85dc99217461b608 :: IO (RIP.FunPtr (RIP.CULong -> IO A))
hs_bindgen_85dc99217461b608 =
  RIP.fromFFIType hs_bindgen_85dc99217461b608_base

{-# NOINLINE f20 #-}
{-| __C declaration:__ @f20@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 46:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f20 :: RIP.FunPtr (RIP.CULong -> IO A)
f20 = RIP.unsafePerformIO hs_bindgen_85dc99217461b608

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f21@
foreign import ccall unsafe "hs_bindgen_350486e538e7ca90" hs_bindgen_350486e538e7ca90_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f21@
hs_bindgen_350486e538e7ca90 :: IO (RIP.FunPtr (RIP.CLLong -> IO A))
hs_bindgen_350486e538e7ca90 =
  RIP.fromFFIType hs_bindgen_350486e538e7ca90_base

{-# NOINLINE f21 #-}
{-| __C declaration:__ @f21@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 48:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f21 :: RIP.FunPtr (RIP.CLLong -> IO A)
f21 = RIP.unsafePerformIO hs_bindgen_350486e538e7ca90

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f22@
foreign import ccall unsafe "hs_bindgen_ad2c8dc646b29be0" hs_bindgen_ad2c8dc646b29be0_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f22@
hs_bindgen_ad2c8dc646b29be0 :: IO (RIP.FunPtr (RIP.CLLong -> IO A))
hs_bindgen_ad2c8dc646b29be0 =
  RIP.fromFFIType hs_bindgen_ad2c8dc646b29be0_base

{-# NOINLINE f22 #-}
{-| __C declaration:__ @f22@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 49:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f22 :: RIP.FunPtr (RIP.CLLong -> IO A)
f22 = RIP.unsafePerformIO hs_bindgen_ad2c8dc646b29be0

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f23@
foreign import ccall unsafe "hs_bindgen_01837bab50ebbe4c" hs_bindgen_01837bab50ebbe4c_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f23@
hs_bindgen_01837bab50ebbe4c :: IO (RIP.FunPtr (RIP.CLLong -> IO A))
hs_bindgen_01837bab50ebbe4c =
  RIP.fromFFIType hs_bindgen_01837bab50ebbe4c_base

{-# NOINLINE f23 #-}
{-| __C declaration:__ @f23@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 50:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f23 :: RIP.FunPtr (RIP.CLLong -> IO A)
f23 = RIP.unsafePerformIO hs_bindgen_01837bab50ebbe4c

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f24@
foreign import ccall unsafe "hs_bindgen_051d17dfd1125c86" hs_bindgen_051d17dfd1125c86_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f24@
hs_bindgen_051d17dfd1125c86 :: IO (RIP.FunPtr (RIP.CLLong -> IO A))
hs_bindgen_051d17dfd1125c86 =
  RIP.fromFFIType hs_bindgen_051d17dfd1125c86_base

{-# NOINLINE f24 #-}
{-| __C declaration:__ @f24@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 51:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f24 :: RIP.FunPtr (RIP.CLLong -> IO A)
f24 = RIP.unsafePerformIO hs_bindgen_051d17dfd1125c86

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f25@
foreign import ccall unsafe "hs_bindgen_ba5dd3a36d6f2756" hs_bindgen_ba5dd3a36d6f2756_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f25@
hs_bindgen_ba5dd3a36d6f2756 :: IO (RIP.FunPtr (RIP.CULLong -> IO A))
hs_bindgen_ba5dd3a36d6f2756 =
  RIP.fromFFIType hs_bindgen_ba5dd3a36d6f2756_base

{-# NOINLINE f25 #-}
{-| __C declaration:__ @f25@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 52:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f25 :: RIP.FunPtr (RIP.CULLong -> IO A)
f25 = RIP.unsafePerformIO hs_bindgen_ba5dd3a36d6f2756

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f26@
foreign import ccall unsafe "hs_bindgen_22f3b97563fd757f" hs_bindgen_22f3b97563fd757f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f26@
hs_bindgen_22f3b97563fd757f :: IO (RIP.FunPtr (RIP.CULLong -> IO A))
hs_bindgen_22f3b97563fd757f =
  RIP.fromFFIType hs_bindgen_22f3b97563fd757f_base

{-# NOINLINE f26 #-}
{-| __C declaration:__ @f26@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 53:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f26 :: RIP.FunPtr (RIP.CULLong -> IO A)
f26 = RIP.unsafePerformIO hs_bindgen_22f3b97563fd757f

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f27@
foreign import ccall unsafe "hs_bindgen_fc81895dfae9b1e5" hs_bindgen_fc81895dfae9b1e5_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f27@
hs_bindgen_fc81895dfae9b1e5 :: IO (RIP.FunPtr (RIP.CFloat -> IO A))
hs_bindgen_fc81895dfae9b1e5 =
  RIP.fromFFIType hs_bindgen_fc81895dfae9b1e5_base

{-# NOINLINE f27 #-}
{-| __C declaration:__ @f27@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 58:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f27 :: RIP.FunPtr (RIP.CFloat -> IO A)
f27 = RIP.unsafePerformIO hs_bindgen_fc81895dfae9b1e5

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f28@
foreign import ccall unsafe "hs_bindgen_f95062f509265011" hs_bindgen_f95062f509265011_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsearithmetic_type_Example_get_f28@
hs_bindgen_f95062f509265011 :: IO (RIP.FunPtr (RIP.CDouble -> IO A))
hs_bindgen_f95062f509265011 =
  RIP.fromFFIType hs_bindgen_f95062f509265011_base

{-# NOINLINE f28 #-}
{-| __C declaration:__ @f28@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 59:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f28 :: RIP.FunPtr (RIP.CDouble -> IO A)
f28 = RIP.unsafePerformIO hs_bindgen_f95062f509265011
