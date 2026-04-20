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
  [ "#include <macros/reparse_arithmetic_types.h>"
  , "A hs_bindgen_bb1c033ecf7e801a ("
  , "  char arg1"
  , ")"
  , "{"
  , "  return (f1)(arg1);"
  , "}"
  , "A hs_bindgen_04ac3e6dd480d7ca ("
  , "  signed char arg1"
  , ")"
  , "{"
  , "  return (f2)(arg1);"
  , "}"
  , "A hs_bindgen_a32ce76876bccf1f ("
  , "  unsigned char arg1"
  , ")"
  , "{"
  , "  return (f3)(arg1);"
  , "}"
  , "A hs_bindgen_ddbdd3d6c060be86 ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (f4)(arg1);"
  , "}"
  , "A hs_bindgen_174ecb48bf4b226b ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (f5)(arg1);"
  , "}"
  , "A hs_bindgen_ef2d717da4930246 ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (f6)(arg1);"
  , "}"
  , "A hs_bindgen_2ce0b625c0b2b5fa ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (f7)(arg1);"
  , "}"
  , "A hs_bindgen_95f594fd4196ff46 ("
  , "  unsigned short arg1"
  , ")"
  , "{"
  , "  return (f8)(arg1);"
  , "}"
  , "A hs_bindgen_751cf60b47f9ce5c ("
  , "  unsigned short arg1"
  , ")"
  , "{"
  , "  return (f9)(arg1);"
  , "}"
  , "A hs_bindgen_498b1def6a2e15e2 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (f10)(arg1);"
  , "}"
  , "signed int hs_bindgen_e50d150910f93eb9 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (f11)(arg1);"
  , "}"
  , "A hs_bindgen_f83b05b1dc756ae7 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (f12)(arg1);"
  , "}"
  , "signed int hs_bindgen_d4447da824d823cd ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return (f13)(arg1);"
  , "}"
  , "A hs_bindgen_864f9afbd3d58e03 ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return (f14)(arg1);"
  , "}"
  , "A hs_bindgen_aed8721b6600d538 ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f15)(arg1);"
  , "}"
  , "A hs_bindgen_6eea0d932c341e57 ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f16)(arg1);"
  , "}"
  , "A hs_bindgen_1f866dd647156868 ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f17)(arg1);"
  , "}"
  , "A hs_bindgen_b8ed3db53b7c66ef ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f18)(arg1);"
  , "}"
  , "A hs_bindgen_1f0dd18c87711f07 ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return (f19)(arg1);"
  , "}"
  , "A hs_bindgen_f4c9a10bd525c7b9 ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return (f20)(arg1);"
  , "}"
  , "A hs_bindgen_ea52f54ff3581034 ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f21)(arg1);"
  , "}"
  , "A hs_bindgen_9b3ae7f60620689e ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f22)(arg1);"
  , "}"
  , "A hs_bindgen_c3bf58dd95aae652 ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f23)(arg1);"
  , "}"
  , "A hs_bindgen_3a0c938781f29a95 ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f24)(arg1);"
  , "}"
  , "A hs_bindgen_c9394fe9466e9254 ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return (f25)(arg1);"
  , "}"
  , "A hs_bindgen_c4f865bdc9e7fb5d ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return (f26)(arg1);"
  , "}"
  , "A hs_bindgen_065625ed2b866193 ("
  , "  float arg1"
  , ")"
  , "{"
  , "  return (f27)(arg1);"
  , "}"
  , "A hs_bindgen_2c6578d90eb6bd5c ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return (f28)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f1@
foreign import ccall safe "hs_bindgen_bb1c033ecf7e801a" hs_bindgen_bb1c033ecf7e801a_base ::
     RIP.Int8
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f1@
hs_bindgen_bb1c033ecf7e801a ::
     RIP.CChar
  -> IO A
hs_bindgen_bb1c033ecf7e801a =
  RIP.fromFFIType hs_bindgen_bb1c033ecf7e801a_base

{-| __C declaration:__ @f1@

    __defined at:__ @macros\/reparse_arithmetic_types.h 21:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f1 ::
     RIP.CChar
     -- ^ __C declaration:__ @x@
  -> IO A
f1 = hs_bindgen_bb1c033ecf7e801a

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f2@
foreign import ccall safe "hs_bindgen_04ac3e6dd480d7ca" hs_bindgen_04ac3e6dd480d7ca_base ::
     RIP.Int8
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f2@
hs_bindgen_04ac3e6dd480d7ca ::
     RIP.CSChar
  -> IO A
hs_bindgen_04ac3e6dd480d7ca =
  RIP.fromFFIType hs_bindgen_04ac3e6dd480d7ca_base

{-| __C declaration:__ @f2@

    __defined at:__ @macros\/reparse_arithmetic_types.h 22:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f2 ::
     RIP.CSChar
     -- ^ __C declaration:__ @x@
  -> IO A
f2 = hs_bindgen_04ac3e6dd480d7ca

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f3@
foreign import ccall safe "hs_bindgen_a32ce76876bccf1f" hs_bindgen_a32ce76876bccf1f_base ::
     RIP.Word8
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f3@
hs_bindgen_a32ce76876bccf1f ::
     RIP.CUChar
  -> IO A
hs_bindgen_a32ce76876bccf1f =
  RIP.fromFFIType hs_bindgen_a32ce76876bccf1f_base

{-| __C declaration:__ @f3@

    __defined at:__ @macros\/reparse_arithmetic_types.h 23:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f3 ::
     RIP.CUChar
     -- ^ __C declaration:__ @x@
  -> IO A
f3 = hs_bindgen_a32ce76876bccf1f

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f4@
foreign import ccall safe "hs_bindgen_ddbdd3d6c060be86" hs_bindgen_ddbdd3d6c060be86_base ::
     RIP.Int16
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f4@
hs_bindgen_ddbdd3d6c060be86 ::
     RIP.CShort
  -> IO A
hs_bindgen_ddbdd3d6c060be86 =
  RIP.fromFFIType hs_bindgen_ddbdd3d6c060be86_base

{-| __C declaration:__ @f4@

    __defined at:__ @macros\/reparse_arithmetic_types.h 28:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f4 ::
     RIP.CShort
     -- ^ __C declaration:__ @x@
  -> IO A
f4 = hs_bindgen_ddbdd3d6c060be86

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f5@
foreign import ccall safe "hs_bindgen_174ecb48bf4b226b" hs_bindgen_174ecb48bf4b226b_base ::
     RIP.Int16
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f5@
hs_bindgen_174ecb48bf4b226b ::
     RIP.CShort
  -> IO A
hs_bindgen_174ecb48bf4b226b =
  RIP.fromFFIType hs_bindgen_174ecb48bf4b226b_base

{-| __C declaration:__ @f5@

    __defined at:__ @macros\/reparse_arithmetic_types.h 29:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f5 ::
     RIP.CShort
     -- ^ __C declaration:__ @x@
  -> IO A
f5 = hs_bindgen_174ecb48bf4b226b

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f6@
foreign import ccall safe "hs_bindgen_ef2d717da4930246" hs_bindgen_ef2d717da4930246_base ::
     RIP.Int16
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f6@
hs_bindgen_ef2d717da4930246 ::
     RIP.CShort
  -> IO A
hs_bindgen_ef2d717da4930246 =
  RIP.fromFFIType hs_bindgen_ef2d717da4930246_base

{-| __C declaration:__ @f6@

    __defined at:__ @macros\/reparse_arithmetic_types.h 30:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f6 ::
     RIP.CShort
     -- ^ __C declaration:__ @x@
  -> IO A
f6 = hs_bindgen_ef2d717da4930246

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f7@
foreign import ccall safe "hs_bindgen_2ce0b625c0b2b5fa" hs_bindgen_2ce0b625c0b2b5fa_base ::
     RIP.Int16
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f7@
hs_bindgen_2ce0b625c0b2b5fa ::
     RIP.CShort
  -> IO A
hs_bindgen_2ce0b625c0b2b5fa =
  RIP.fromFFIType hs_bindgen_2ce0b625c0b2b5fa_base

{-| __C declaration:__ @f7@

    __defined at:__ @macros\/reparse_arithmetic_types.h 31:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f7 ::
     RIP.CShort
     -- ^ __C declaration:__ @x@
  -> IO A
f7 = hs_bindgen_2ce0b625c0b2b5fa

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f8@
foreign import ccall safe "hs_bindgen_95f594fd4196ff46" hs_bindgen_95f594fd4196ff46_base ::
     RIP.Word16
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f8@
hs_bindgen_95f594fd4196ff46 ::
     RIP.CUShort
  -> IO A
hs_bindgen_95f594fd4196ff46 =
  RIP.fromFFIType hs_bindgen_95f594fd4196ff46_base

{-| __C declaration:__ @f8@

    __defined at:__ @macros\/reparse_arithmetic_types.h 32:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f8 ::
     RIP.CUShort
     -- ^ __C declaration:__ @x@
  -> IO A
f8 = hs_bindgen_95f594fd4196ff46

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f9@
foreign import ccall safe "hs_bindgen_751cf60b47f9ce5c" hs_bindgen_751cf60b47f9ce5c_base ::
     RIP.Word16
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f9@
hs_bindgen_751cf60b47f9ce5c ::
     RIP.CUShort
  -> IO A
hs_bindgen_751cf60b47f9ce5c =
  RIP.fromFFIType hs_bindgen_751cf60b47f9ce5c_base

{-| __C declaration:__ @f9@

    __defined at:__ @macros\/reparse_arithmetic_types.h 33:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f9 ::
     RIP.CUShort
     -- ^ __C declaration:__ @x@
  -> IO A
f9 = hs_bindgen_751cf60b47f9ce5c

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f10@
foreign import ccall safe "hs_bindgen_498b1def6a2e15e2" hs_bindgen_498b1def6a2e15e2_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f10@
hs_bindgen_498b1def6a2e15e2 ::
     RIP.CInt
  -> IO A
hs_bindgen_498b1def6a2e15e2 =
  RIP.fromFFIType hs_bindgen_498b1def6a2e15e2_base

{-| __C declaration:__ @f10@

    __defined at:__ @macros\/reparse_arithmetic_types.h 35:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f10 ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO A
f10 = hs_bindgen_498b1def6a2e15e2

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f11@
foreign import ccall safe "hs_bindgen_e50d150910f93eb9" hs_bindgen_e50d150910f93eb9_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f11@
hs_bindgen_e50d150910f93eb9 ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_e50d150910f93eb9 =
  RIP.fromFFIType hs_bindgen_e50d150910f93eb9_base

{-| __C declaration:__ @f11@

    __defined at:__ @macros\/reparse_arithmetic_types.h 36:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f11 ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO RIP.CInt
f11 = hs_bindgen_e50d150910f93eb9

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f12@
foreign import ccall safe "hs_bindgen_f83b05b1dc756ae7" hs_bindgen_f83b05b1dc756ae7_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f12@
hs_bindgen_f83b05b1dc756ae7 ::
     RIP.CInt
  -> IO A
hs_bindgen_f83b05b1dc756ae7 =
  RIP.fromFFIType hs_bindgen_f83b05b1dc756ae7_base

{-| __C declaration:__ @f12@

    __defined at:__ @macros\/reparse_arithmetic_types.h 37:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f12 ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO A
f12 = hs_bindgen_f83b05b1dc756ae7

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f13@
foreign import ccall safe "hs_bindgen_d4447da824d823cd" hs_bindgen_d4447da824d823cd_base ::
     RIP.Word32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f13@
hs_bindgen_d4447da824d823cd ::
     RIP.CUInt
  -> IO RIP.CInt
hs_bindgen_d4447da824d823cd =
  RIP.fromFFIType hs_bindgen_d4447da824d823cd_base

{-| __C declaration:__ @f13@

    __defined at:__ @macros\/reparse_arithmetic_types.h 38:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f13 ::
     RIP.CUInt
     -- ^ __C declaration:__ @x@
  -> IO RIP.CInt
f13 = hs_bindgen_d4447da824d823cd

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f14@
foreign import ccall safe "hs_bindgen_864f9afbd3d58e03" hs_bindgen_864f9afbd3d58e03_base ::
     RIP.Word32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f14@
hs_bindgen_864f9afbd3d58e03 ::
     RIP.CUInt
  -> IO A
hs_bindgen_864f9afbd3d58e03 =
  RIP.fromFFIType hs_bindgen_864f9afbd3d58e03_base

{-| __C declaration:__ @f14@

    __defined at:__ @macros\/reparse_arithmetic_types.h 39:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f14 ::
     RIP.CUInt
     -- ^ __C declaration:__ @x@
  -> IO A
f14 = hs_bindgen_864f9afbd3d58e03

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f15@
foreign import ccall safe "hs_bindgen_aed8721b6600d538" hs_bindgen_aed8721b6600d538_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f15@
hs_bindgen_aed8721b6600d538 ::
     RIP.CLong
  -> IO A
hs_bindgen_aed8721b6600d538 =
  RIP.fromFFIType hs_bindgen_aed8721b6600d538_base

{-| __C declaration:__ @f15@

    __defined at:__ @macros\/reparse_arithmetic_types.h 41:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f15 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f15 = hs_bindgen_aed8721b6600d538

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f16@
foreign import ccall safe "hs_bindgen_6eea0d932c341e57" hs_bindgen_6eea0d932c341e57_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f16@
hs_bindgen_6eea0d932c341e57 ::
     RIP.CLong
  -> IO A
hs_bindgen_6eea0d932c341e57 =
  RIP.fromFFIType hs_bindgen_6eea0d932c341e57_base

{-| __C declaration:__ @f16@

    __defined at:__ @macros\/reparse_arithmetic_types.h 42:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f16 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f16 = hs_bindgen_6eea0d932c341e57

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f17@
foreign import ccall safe "hs_bindgen_1f866dd647156868" hs_bindgen_1f866dd647156868_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f17@
hs_bindgen_1f866dd647156868 ::
     RIP.CLong
  -> IO A
hs_bindgen_1f866dd647156868 =
  RIP.fromFFIType hs_bindgen_1f866dd647156868_base

{-| __C declaration:__ @f17@

    __defined at:__ @macros\/reparse_arithmetic_types.h 43:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f17 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f17 = hs_bindgen_1f866dd647156868

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f18@
foreign import ccall safe "hs_bindgen_b8ed3db53b7c66ef" hs_bindgen_b8ed3db53b7c66ef_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f18@
hs_bindgen_b8ed3db53b7c66ef ::
     RIP.CLong
  -> IO A
hs_bindgen_b8ed3db53b7c66ef =
  RIP.fromFFIType hs_bindgen_b8ed3db53b7c66ef_base

{-| __C declaration:__ @f18@

    __defined at:__ @macros\/reparse_arithmetic_types.h 44:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f18 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f18 = hs_bindgen_b8ed3db53b7c66ef

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f19@
foreign import ccall safe "hs_bindgen_1f0dd18c87711f07" hs_bindgen_1f0dd18c87711f07_base ::
     RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f19@
hs_bindgen_1f0dd18c87711f07 ::
     RIP.CULong
  -> IO A
hs_bindgen_1f0dd18c87711f07 =
  RIP.fromFFIType hs_bindgen_1f0dd18c87711f07_base

{-| __C declaration:__ @f19@

    __defined at:__ @macros\/reparse_arithmetic_types.h 45:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f19 ::
     RIP.CULong
     -- ^ __C declaration:__ @x@
  -> IO A
f19 = hs_bindgen_1f0dd18c87711f07

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f20@
foreign import ccall safe "hs_bindgen_f4c9a10bd525c7b9" hs_bindgen_f4c9a10bd525c7b9_base ::
     RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f20@
hs_bindgen_f4c9a10bd525c7b9 ::
     RIP.CULong
  -> IO A
hs_bindgen_f4c9a10bd525c7b9 =
  RIP.fromFFIType hs_bindgen_f4c9a10bd525c7b9_base

{-| __C declaration:__ @f20@

    __defined at:__ @macros\/reparse_arithmetic_types.h 46:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f20 ::
     RIP.CULong
     -- ^ __C declaration:__ @x@
  -> IO A
f20 = hs_bindgen_f4c9a10bd525c7b9

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f21@
foreign import ccall safe "hs_bindgen_ea52f54ff3581034" hs_bindgen_ea52f54ff3581034_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f21@
hs_bindgen_ea52f54ff3581034 ::
     RIP.CLong
  -> IO A
hs_bindgen_ea52f54ff3581034 =
  RIP.fromFFIType hs_bindgen_ea52f54ff3581034_base

{-| __C declaration:__ @f21@

    __defined at:__ @macros\/reparse_arithmetic_types.h 48:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f21 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f21 = hs_bindgen_ea52f54ff3581034

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f22@
foreign import ccall safe "hs_bindgen_9b3ae7f60620689e" hs_bindgen_9b3ae7f60620689e_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f22@
hs_bindgen_9b3ae7f60620689e ::
     RIP.CLong
  -> IO A
hs_bindgen_9b3ae7f60620689e =
  RIP.fromFFIType hs_bindgen_9b3ae7f60620689e_base

{-| __C declaration:__ @f22@

    __defined at:__ @macros\/reparse_arithmetic_types.h 49:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f22 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f22 = hs_bindgen_9b3ae7f60620689e

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f23@
foreign import ccall safe "hs_bindgen_c3bf58dd95aae652" hs_bindgen_c3bf58dd95aae652_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f23@
hs_bindgen_c3bf58dd95aae652 ::
     RIP.CLong
  -> IO A
hs_bindgen_c3bf58dd95aae652 =
  RIP.fromFFIType hs_bindgen_c3bf58dd95aae652_base

{-| __C declaration:__ @f23@

    __defined at:__ @macros\/reparse_arithmetic_types.h 50:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f23 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f23 = hs_bindgen_c3bf58dd95aae652

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f24@
foreign import ccall safe "hs_bindgen_3a0c938781f29a95" hs_bindgen_3a0c938781f29a95_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f24@
hs_bindgen_3a0c938781f29a95 ::
     RIP.CLong
  -> IO A
hs_bindgen_3a0c938781f29a95 =
  RIP.fromFFIType hs_bindgen_3a0c938781f29a95_base

{-| __C declaration:__ @f24@

    __defined at:__ @macros\/reparse_arithmetic_types.h 51:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f24 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f24 = hs_bindgen_3a0c938781f29a95

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f25@
foreign import ccall safe "hs_bindgen_c9394fe9466e9254" hs_bindgen_c9394fe9466e9254_base ::
     RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f25@
hs_bindgen_c9394fe9466e9254 ::
     RIP.CULong
  -> IO A
hs_bindgen_c9394fe9466e9254 =
  RIP.fromFFIType hs_bindgen_c9394fe9466e9254_base

{-| __C declaration:__ @f25@

    __defined at:__ @macros\/reparse_arithmetic_types.h 52:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f25 ::
     RIP.CULong
     -- ^ __C declaration:__ @x@
  -> IO A
f25 = hs_bindgen_c9394fe9466e9254

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f26@
foreign import ccall safe "hs_bindgen_c4f865bdc9e7fb5d" hs_bindgen_c4f865bdc9e7fb5d_base ::
     RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f26@
hs_bindgen_c4f865bdc9e7fb5d ::
     RIP.CULong
  -> IO A
hs_bindgen_c4f865bdc9e7fb5d =
  RIP.fromFFIType hs_bindgen_c4f865bdc9e7fb5d_base

{-| __C declaration:__ @f26@

    __defined at:__ @macros\/reparse_arithmetic_types.h 53:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f26 ::
     RIP.CULong
     -- ^ __C declaration:__ @x@
  -> IO A
f26 = hs_bindgen_c4f865bdc9e7fb5d

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f27@
foreign import ccall safe "hs_bindgen_065625ed2b866193" hs_bindgen_065625ed2b866193_base ::
     Float
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f27@
hs_bindgen_065625ed2b866193 ::
     RIP.CFloat
  -> IO A
hs_bindgen_065625ed2b866193 =
  RIP.fromFFIType hs_bindgen_065625ed2b866193_base

{-| __C declaration:__ @f27@

    __defined at:__ @macros\/reparse_arithmetic_types.h 58:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f27 ::
     RIP.CFloat
     -- ^ __C declaration:__ @x@
  -> IO A
f27 = hs_bindgen_065625ed2b866193

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f28@
foreign import ccall safe "hs_bindgen_2c6578d90eb6bd5c" hs_bindgen_2c6578d90eb6bd5c_base ::
     Double
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Safe_f28@
hs_bindgen_2c6578d90eb6bd5c ::
     RIP.CDouble
  -> IO A
hs_bindgen_2c6578d90eb6bd5c =
  RIP.fromFFIType hs_bindgen_2c6578d90eb6bd5c_base

{-| __C declaration:__ @f28@

    __defined at:__ @macros\/reparse_arithmetic_types.h 59:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f28 ::
     RIP.CDouble
     -- ^ __C declaration:__ @x@
  -> IO A
f28 = hs_bindgen_2c6578d90eb6bd5c
