{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.f1
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
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/reparse/arithmetic_types.h>"
  , "A hs_bindgen_7e7e01d691b1fb2a ("
  , "  char arg1"
  , ")"
  , "{"
  , "  return (f1)(arg1);"
  , "}"
  , "A hs_bindgen_91658d4948bcfe06 ("
  , "  signed char arg1"
  , ")"
  , "{"
  , "  return (f2)(arg1);"
  , "}"
  , "A hs_bindgen_40fb57deec0c99d6 ("
  , "  unsigned char arg1"
  , ")"
  , "{"
  , "  return (f3)(arg1);"
  , "}"
  , "A hs_bindgen_d454dc277e0cb86b ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (f4)(arg1);"
  , "}"
  , "A hs_bindgen_446e834618998df2 ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (f5)(arg1);"
  , "}"
  , "A hs_bindgen_267cc7905b42c66c ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (f6)(arg1);"
  , "}"
  , "A hs_bindgen_e9eef0af846987d5 ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (f7)(arg1);"
  , "}"
  , "A hs_bindgen_926fbc7f9888d375 ("
  , "  unsigned short arg1"
  , ")"
  , "{"
  , "  return (f8)(arg1);"
  , "}"
  , "A hs_bindgen_ec8aef57bd8d53bc ("
  , "  unsigned short arg1"
  , ")"
  , "{"
  , "  return (f9)(arg1);"
  , "}"
  , "A hs_bindgen_d0045e58c2a6f1c4 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (f10)(arg1);"
  , "}"
  , "A hs_bindgen_928bc5a3801d9fba ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (f11)(arg1);"
  , "}"
  , "A hs_bindgen_0efc218270b00ee6 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (f12)(arg1);"
  , "}"
  , "A hs_bindgen_403a8a02a230ce24 ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return (f13)(arg1);"
  , "}"
  , "A hs_bindgen_9371cd597ae3022d ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return (f14)(arg1);"
  , "}"
  , "A hs_bindgen_f07c6429182c10c0 ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f15)(arg1);"
  , "}"
  , "A hs_bindgen_6af229e466926ec5 ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f16)(arg1);"
  , "}"
  , "A hs_bindgen_0bc846cbecfd41ba ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f17)(arg1);"
  , "}"
  , "A hs_bindgen_0985403200f5a9de ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f18)(arg1);"
  , "}"
  , "A hs_bindgen_91ef2c8f3da40e3c ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return (f19)(arg1);"
  , "}"
  , "A hs_bindgen_39b524909ca528b5 ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return (f20)(arg1);"
  , "}"
  , "A hs_bindgen_efa4b421b2928f1e ("
  , "  signed long long arg1"
  , ")"
  , "{"
  , "  return (f21)(arg1);"
  , "}"
  , "A hs_bindgen_881c4734d1b436b5 ("
  , "  signed long long arg1"
  , ")"
  , "{"
  , "  return (f22)(arg1);"
  , "}"
  , "A hs_bindgen_0ed2a5cf6068b35a ("
  , "  signed long long arg1"
  , ")"
  , "{"
  , "  return (f23)(arg1);"
  , "}"
  , "A hs_bindgen_157ff76d8f7857e2 ("
  , "  signed long long arg1"
  , ")"
  , "{"
  , "  return (f24)(arg1);"
  , "}"
  , "A hs_bindgen_6046e1b60a2bdbda ("
  , "  unsigned long long arg1"
  , ")"
  , "{"
  , "  return (f25)(arg1);"
  , "}"
  , "A hs_bindgen_3f072797069fc217 ("
  , "  unsigned long long arg1"
  , ")"
  , "{"
  , "  return (f26)(arg1);"
  , "}"
  , "A hs_bindgen_4da0472b80930fbf ("
  , "  float arg1"
  , ")"
  , "{"
  , "  return (f27)(arg1);"
  , "}"
  , "A hs_bindgen_98b4d9dc27aecea0 ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return (f28)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f1@
foreign import ccall safe "hs_bindgen_7e7e01d691b1fb2a" hs_bindgen_7e7e01d691b1fb2a_base ::
     RIP.Int8
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f1@
hs_bindgen_7e7e01d691b1fb2a ::
     RIP.CChar
  -> IO A
hs_bindgen_7e7e01d691b1fb2a =
  RIP.fromFFIType hs_bindgen_7e7e01d691b1fb2a_base

{-| __C declaration:__ @f1@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 21:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f1 ::
     RIP.CChar
     -- ^ __C declaration:__ @x@
  -> IO A
f1 = hs_bindgen_7e7e01d691b1fb2a

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f2@
foreign import ccall safe "hs_bindgen_91658d4948bcfe06" hs_bindgen_91658d4948bcfe06_base ::
     RIP.Int8
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f2@
hs_bindgen_91658d4948bcfe06 ::
     RIP.CSChar
  -> IO A
hs_bindgen_91658d4948bcfe06 =
  RIP.fromFFIType hs_bindgen_91658d4948bcfe06_base

{-| __C declaration:__ @f2@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 22:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f2 ::
     RIP.CSChar
     -- ^ __C declaration:__ @x@
  -> IO A
f2 = hs_bindgen_91658d4948bcfe06

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f3@
foreign import ccall safe "hs_bindgen_40fb57deec0c99d6" hs_bindgen_40fb57deec0c99d6_base ::
     RIP.Word8
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f3@
hs_bindgen_40fb57deec0c99d6 ::
     RIP.CUChar
  -> IO A
hs_bindgen_40fb57deec0c99d6 =
  RIP.fromFFIType hs_bindgen_40fb57deec0c99d6_base

{-| __C declaration:__ @f3@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 23:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f3 ::
     RIP.CUChar
     -- ^ __C declaration:__ @x@
  -> IO A
f3 = hs_bindgen_40fb57deec0c99d6

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f4@
foreign import ccall safe "hs_bindgen_d454dc277e0cb86b" hs_bindgen_d454dc277e0cb86b_base ::
     RIP.Int16
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f4@
hs_bindgen_d454dc277e0cb86b ::
     RIP.CShort
  -> IO A
hs_bindgen_d454dc277e0cb86b =
  RIP.fromFFIType hs_bindgen_d454dc277e0cb86b_base

{-| __C declaration:__ @f4@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 28:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f4 ::
     RIP.CShort
     -- ^ __C declaration:__ @x@
  -> IO A
f4 = hs_bindgen_d454dc277e0cb86b

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f5@
foreign import ccall safe "hs_bindgen_446e834618998df2" hs_bindgen_446e834618998df2_base ::
     RIP.Int16
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f5@
hs_bindgen_446e834618998df2 ::
     RIP.CShort
  -> IO A
hs_bindgen_446e834618998df2 =
  RIP.fromFFIType hs_bindgen_446e834618998df2_base

{-| __C declaration:__ @f5@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 29:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f5 ::
     RIP.CShort
     -- ^ __C declaration:__ @x@
  -> IO A
f5 = hs_bindgen_446e834618998df2

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f6@
foreign import ccall safe "hs_bindgen_267cc7905b42c66c" hs_bindgen_267cc7905b42c66c_base ::
     RIP.Int16
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f6@
hs_bindgen_267cc7905b42c66c ::
     RIP.CShort
  -> IO A
hs_bindgen_267cc7905b42c66c =
  RIP.fromFFIType hs_bindgen_267cc7905b42c66c_base

{-| __C declaration:__ @f6@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 30:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f6 ::
     RIP.CShort
     -- ^ __C declaration:__ @x@
  -> IO A
f6 = hs_bindgen_267cc7905b42c66c

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f7@
foreign import ccall safe "hs_bindgen_e9eef0af846987d5" hs_bindgen_e9eef0af846987d5_base ::
     RIP.Int16
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f7@
hs_bindgen_e9eef0af846987d5 ::
     RIP.CShort
  -> IO A
hs_bindgen_e9eef0af846987d5 =
  RIP.fromFFIType hs_bindgen_e9eef0af846987d5_base

{-| __C declaration:__ @f7@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 31:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f7 ::
     RIP.CShort
     -- ^ __C declaration:__ @x@
  -> IO A
f7 = hs_bindgen_e9eef0af846987d5

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f8@
foreign import ccall safe "hs_bindgen_926fbc7f9888d375" hs_bindgen_926fbc7f9888d375_base ::
     RIP.Word16
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f8@
hs_bindgen_926fbc7f9888d375 ::
     RIP.CUShort
  -> IO A
hs_bindgen_926fbc7f9888d375 =
  RIP.fromFFIType hs_bindgen_926fbc7f9888d375_base

{-| __C declaration:__ @f8@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 32:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f8 ::
     RIP.CUShort
     -- ^ __C declaration:__ @x@
  -> IO A
f8 = hs_bindgen_926fbc7f9888d375

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f9@
foreign import ccall safe "hs_bindgen_ec8aef57bd8d53bc" hs_bindgen_ec8aef57bd8d53bc_base ::
     RIP.Word16
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f9@
hs_bindgen_ec8aef57bd8d53bc ::
     RIP.CUShort
  -> IO A
hs_bindgen_ec8aef57bd8d53bc =
  RIP.fromFFIType hs_bindgen_ec8aef57bd8d53bc_base

{-| __C declaration:__ @f9@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 33:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f9 ::
     RIP.CUShort
     -- ^ __C declaration:__ @x@
  -> IO A
f9 = hs_bindgen_ec8aef57bd8d53bc

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f10@
foreign import ccall safe "hs_bindgen_d0045e58c2a6f1c4" hs_bindgen_d0045e58c2a6f1c4_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f10@
hs_bindgen_d0045e58c2a6f1c4 ::
     RIP.CInt
  -> IO A
hs_bindgen_d0045e58c2a6f1c4 =
  RIP.fromFFIType hs_bindgen_d0045e58c2a6f1c4_base

{-| __C declaration:__ @f10@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 35:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f10 ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO A
f10 = hs_bindgen_d0045e58c2a6f1c4

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f11@
foreign import ccall safe "hs_bindgen_928bc5a3801d9fba" hs_bindgen_928bc5a3801d9fba_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f11@
hs_bindgen_928bc5a3801d9fba ::
     RIP.CInt
  -> IO A
hs_bindgen_928bc5a3801d9fba =
  RIP.fromFFIType hs_bindgen_928bc5a3801d9fba_base

{-| __C declaration:__ @f11@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 36:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f11 ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO A
f11 = hs_bindgen_928bc5a3801d9fba

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f12@
foreign import ccall safe "hs_bindgen_0efc218270b00ee6" hs_bindgen_0efc218270b00ee6_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f12@
hs_bindgen_0efc218270b00ee6 ::
     RIP.CInt
  -> IO A
hs_bindgen_0efc218270b00ee6 =
  RIP.fromFFIType hs_bindgen_0efc218270b00ee6_base

{-| __C declaration:__ @f12@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 37:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f12 ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO A
f12 = hs_bindgen_0efc218270b00ee6

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f13@
foreign import ccall safe "hs_bindgen_403a8a02a230ce24" hs_bindgen_403a8a02a230ce24_base ::
     RIP.Word32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f13@
hs_bindgen_403a8a02a230ce24 ::
     RIP.CUInt
  -> IO A
hs_bindgen_403a8a02a230ce24 =
  RIP.fromFFIType hs_bindgen_403a8a02a230ce24_base

{-| __C declaration:__ @f13@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 38:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f13 ::
     RIP.CUInt
     -- ^ __C declaration:__ @x@
  -> IO A
f13 = hs_bindgen_403a8a02a230ce24

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f14@
foreign import ccall safe "hs_bindgen_9371cd597ae3022d" hs_bindgen_9371cd597ae3022d_base ::
     RIP.Word32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f14@
hs_bindgen_9371cd597ae3022d ::
     RIP.CUInt
  -> IO A
hs_bindgen_9371cd597ae3022d =
  RIP.fromFFIType hs_bindgen_9371cd597ae3022d_base

{-| __C declaration:__ @f14@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 39:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f14 ::
     RIP.CUInt
     -- ^ __C declaration:__ @x@
  -> IO A
f14 = hs_bindgen_9371cd597ae3022d

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f15@
foreign import ccall safe "hs_bindgen_f07c6429182c10c0" hs_bindgen_f07c6429182c10c0_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f15@
hs_bindgen_f07c6429182c10c0 ::
     RIP.CLong
  -> IO A
hs_bindgen_f07c6429182c10c0 =
  RIP.fromFFIType hs_bindgen_f07c6429182c10c0_base

{-| __C declaration:__ @f15@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 41:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f15 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f15 = hs_bindgen_f07c6429182c10c0

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f16@
foreign import ccall safe "hs_bindgen_6af229e466926ec5" hs_bindgen_6af229e466926ec5_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f16@
hs_bindgen_6af229e466926ec5 ::
     RIP.CLong
  -> IO A
hs_bindgen_6af229e466926ec5 =
  RIP.fromFFIType hs_bindgen_6af229e466926ec5_base

{-| __C declaration:__ @f16@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 42:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f16 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f16 = hs_bindgen_6af229e466926ec5

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f17@
foreign import ccall safe "hs_bindgen_0bc846cbecfd41ba" hs_bindgen_0bc846cbecfd41ba_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f17@
hs_bindgen_0bc846cbecfd41ba ::
     RIP.CLong
  -> IO A
hs_bindgen_0bc846cbecfd41ba =
  RIP.fromFFIType hs_bindgen_0bc846cbecfd41ba_base

{-| __C declaration:__ @f17@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 43:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f17 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f17 = hs_bindgen_0bc846cbecfd41ba

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f18@
foreign import ccall safe "hs_bindgen_0985403200f5a9de" hs_bindgen_0985403200f5a9de_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f18@
hs_bindgen_0985403200f5a9de ::
     RIP.CLong
  -> IO A
hs_bindgen_0985403200f5a9de =
  RIP.fromFFIType hs_bindgen_0985403200f5a9de_base

{-| __C declaration:__ @f18@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 44:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f18 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f18 = hs_bindgen_0985403200f5a9de

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f19@
foreign import ccall safe "hs_bindgen_91ef2c8f3da40e3c" hs_bindgen_91ef2c8f3da40e3c_base ::
     RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f19@
hs_bindgen_91ef2c8f3da40e3c ::
     RIP.CULong
  -> IO A
hs_bindgen_91ef2c8f3da40e3c =
  RIP.fromFFIType hs_bindgen_91ef2c8f3da40e3c_base

{-| __C declaration:__ @f19@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 45:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f19 ::
     RIP.CULong
     -- ^ __C declaration:__ @x@
  -> IO A
f19 = hs_bindgen_91ef2c8f3da40e3c

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f20@
foreign import ccall safe "hs_bindgen_39b524909ca528b5" hs_bindgen_39b524909ca528b5_base ::
     RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f20@
hs_bindgen_39b524909ca528b5 ::
     RIP.CULong
  -> IO A
hs_bindgen_39b524909ca528b5 =
  RIP.fromFFIType hs_bindgen_39b524909ca528b5_base

{-| __C declaration:__ @f20@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 46:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f20 ::
     RIP.CULong
     -- ^ __C declaration:__ @x@
  -> IO A
f20 = hs_bindgen_39b524909ca528b5

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f21@
foreign import ccall safe "hs_bindgen_efa4b421b2928f1e" hs_bindgen_efa4b421b2928f1e_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f21@
hs_bindgen_efa4b421b2928f1e ::
     RIP.CLLong
  -> IO A
hs_bindgen_efa4b421b2928f1e =
  RIP.fromFFIType hs_bindgen_efa4b421b2928f1e_base

{-| __C declaration:__ @f21@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 48:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f21 ::
     RIP.CLLong
     -- ^ __C declaration:__ @x@
  -> IO A
f21 = hs_bindgen_efa4b421b2928f1e

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f22@
foreign import ccall safe "hs_bindgen_881c4734d1b436b5" hs_bindgen_881c4734d1b436b5_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f22@
hs_bindgen_881c4734d1b436b5 ::
     RIP.CLLong
  -> IO A
hs_bindgen_881c4734d1b436b5 =
  RIP.fromFFIType hs_bindgen_881c4734d1b436b5_base

{-| __C declaration:__ @f22@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 49:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f22 ::
     RIP.CLLong
     -- ^ __C declaration:__ @x@
  -> IO A
f22 = hs_bindgen_881c4734d1b436b5

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f23@
foreign import ccall safe "hs_bindgen_0ed2a5cf6068b35a" hs_bindgen_0ed2a5cf6068b35a_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f23@
hs_bindgen_0ed2a5cf6068b35a ::
     RIP.CLLong
  -> IO A
hs_bindgen_0ed2a5cf6068b35a =
  RIP.fromFFIType hs_bindgen_0ed2a5cf6068b35a_base

{-| __C declaration:__ @f23@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 50:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f23 ::
     RIP.CLLong
     -- ^ __C declaration:__ @x@
  -> IO A
f23 = hs_bindgen_0ed2a5cf6068b35a

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f24@
foreign import ccall safe "hs_bindgen_157ff76d8f7857e2" hs_bindgen_157ff76d8f7857e2_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f24@
hs_bindgen_157ff76d8f7857e2 ::
     RIP.CLLong
  -> IO A
hs_bindgen_157ff76d8f7857e2 =
  RIP.fromFFIType hs_bindgen_157ff76d8f7857e2_base

{-| __C declaration:__ @f24@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 51:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f24 ::
     RIP.CLLong
     -- ^ __C declaration:__ @x@
  -> IO A
f24 = hs_bindgen_157ff76d8f7857e2

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f25@
foreign import ccall safe "hs_bindgen_6046e1b60a2bdbda" hs_bindgen_6046e1b60a2bdbda_base ::
     RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f25@
hs_bindgen_6046e1b60a2bdbda ::
     RIP.CULLong
  -> IO A
hs_bindgen_6046e1b60a2bdbda =
  RIP.fromFFIType hs_bindgen_6046e1b60a2bdbda_base

{-| __C declaration:__ @f25@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 52:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f25 ::
     RIP.CULLong
     -- ^ __C declaration:__ @x@
  -> IO A
f25 = hs_bindgen_6046e1b60a2bdbda

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f26@
foreign import ccall safe "hs_bindgen_3f072797069fc217" hs_bindgen_3f072797069fc217_base ::
     RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f26@
hs_bindgen_3f072797069fc217 ::
     RIP.CULLong
  -> IO A
hs_bindgen_3f072797069fc217 =
  RIP.fromFFIType hs_bindgen_3f072797069fc217_base

{-| __C declaration:__ @f26@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 53:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f26 ::
     RIP.CULLong
     -- ^ __C declaration:__ @x@
  -> IO A
f26 = hs_bindgen_3f072797069fc217

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f27@
foreign import ccall safe "hs_bindgen_4da0472b80930fbf" hs_bindgen_4da0472b80930fbf_base ::
     Float
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f27@
hs_bindgen_4da0472b80930fbf ::
     RIP.CFloat
  -> IO A
hs_bindgen_4da0472b80930fbf =
  RIP.fromFFIType hs_bindgen_4da0472b80930fbf_base

{-| __C declaration:__ @f27@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 58:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f27 ::
     RIP.CFloat
     -- ^ __C declaration:__ @x@
  -> IO A
f27 = hs_bindgen_4da0472b80930fbf

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f28@
foreign import ccall safe "hs_bindgen_98b4d9dc27aecea0" hs_bindgen_98b4d9dc27aecea0_base ::
     Double
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Safe_f28@
hs_bindgen_98b4d9dc27aecea0 ::
     RIP.CDouble
  -> IO A
hs_bindgen_98b4d9dc27aecea0 =
  RIP.fromFFIType hs_bindgen_98b4d9dc27aecea0_base

{-| __C declaration:__ @f28@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 59:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f28 ::
     RIP.CDouble
     -- ^ __C declaration:__ @x@
  -> IO A
f28 = hs_bindgen_98b4d9dc27aecea0
