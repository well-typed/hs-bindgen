{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.f1
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
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/reparse_arithmetic_types.h>"
  , "A hs_bindgen_93e621670601d0cd ("
  , "  char arg1"
  , ")"
  , "{"
  , "  return (f1)(arg1);"
  , "}"
  , "A hs_bindgen_8d292a46f5a8dd8d ("
  , "  signed char arg1"
  , ")"
  , "{"
  , "  return (f2)(arg1);"
  , "}"
  , "A hs_bindgen_74261cdb0fbe635f ("
  , "  unsigned char arg1"
  , ")"
  , "{"
  , "  return (f3)(arg1);"
  , "}"
  , "A hs_bindgen_c95f9efe0dd50c93 ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (f4)(arg1);"
  , "}"
  , "A hs_bindgen_d10b31bf8d27f6fc ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (f5)(arg1);"
  , "}"
  , "A hs_bindgen_3fa9709f337eab12 ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (f6)(arg1);"
  , "}"
  , "A hs_bindgen_1b4d3b85e2effa7e ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (f7)(arg1);"
  , "}"
  , "A hs_bindgen_9815c4f12a17191b ("
  , "  unsigned short arg1"
  , ")"
  , "{"
  , "  return (f8)(arg1);"
  , "}"
  , "A hs_bindgen_8870a1617c594e81 ("
  , "  unsigned short arg1"
  , ")"
  , "{"
  , "  return (f9)(arg1);"
  , "}"
  , "A hs_bindgen_d8cf76545d90f6f5 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (f10)(arg1);"
  , "}"
  , "signed int hs_bindgen_21c3896990e488d7 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (f11)(arg1);"
  , "}"
  , "A hs_bindgen_bb7965299e2ae640 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (f12)(arg1);"
  , "}"
  , "signed int hs_bindgen_18851b26abc94040 ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return (f13)(arg1);"
  , "}"
  , "A hs_bindgen_19cda67a187de450 ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return (f14)(arg1);"
  , "}"
  , "A hs_bindgen_3b4e19e9d32bd171 ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f15)(arg1);"
  , "}"
  , "A hs_bindgen_982bfd8318c0ffec ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f16)(arg1);"
  , "}"
  , "A hs_bindgen_f67f503878415036 ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f17)(arg1);"
  , "}"
  , "A hs_bindgen_30405c9ae068d28b ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f18)(arg1);"
  , "}"
  , "A hs_bindgen_ee05369d2355e5f1 ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return (f19)(arg1);"
  , "}"
  , "A hs_bindgen_afa5f3bae03f34e0 ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return (f20)(arg1);"
  , "}"
  , "A hs_bindgen_488efc7f974c971b ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f21)(arg1);"
  , "}"
  , "A hs_bindgen_d6563ce8f86f1541 ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f22)(arg1);"
  , "}"
  , "A hs_bindgen_0ecb5a60c4365a73 ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f23)(arg1);"
  , "}"
  , "A hs_bindgen_0d5e24fc786feaec ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f24)(arg1);"
  , "}"
  , "A hs_bindgen_d6b4f74553c4e27b ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return (f25)(arg1);"
  , "}"
  , "A hs_bindgen_da234f84dc67b630 ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return (f26)(arg1);"
  , "}"
  , "A hs_bindgen_26b01e993c12e2f6 ("
  , "  float arg1"
  , ")"
  , "{"
  , "  return (f27)(arg1);"
  , "}"
  , "A hs_bindgen_18913631127a2f42 ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return (f28)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f1@
foreign import ccall unsafe "hs_bindgen_93e621670601d0cd" hs_bindgen_93e621670601d0cd_base ::
     RIP.Int8
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f1@
hs_bindgen_93e621670601d0cd ::
     RIP.CChar
  -> IO A
hs_bindgen_93e621670601d0cd =
  RIP.fromFFIType hs_bindgen_93e621670601d0cd_base

{-| __C declaration:__ @f1@

    __defined at:__ @macros\/reparse_arithmetic_types.h 21:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f1 ::
     RIP.CChar
     -- ^ __C declaration:__ @x@
  -> IO A
f1 = hs_bindgen_93e621670601d0cd

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f2@
foreign import ccall unsafe "hs_bindgen_8d292a46f5a8dd8d" hs_bindgen_8d292a46f5a8dd8d_base ::
     RIP.Int8
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f2@
hs_bindgen_8d292a46f5a8dd8d ::
     RIP.CSChar
  -> IO A
hs_bindgen_8d292a46f5a8dd8d =
  RIP.fromFFIType hs_bindgen_8d292a46f5a8dd8d_base

{-| __C declaration:__ @f2@

    __defined at:__ @macros\/reparse_arithmetic_types.h 22:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f2 ::
     RIP.CSChar
     -- ^ __C declaration:__ @x@
  -> IO A
f2 = hs_bindgen_8d292a46f5a8dd8d

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f3@
foreign import ccall unsafe "hs_bindgen_74261cdb0fbe635f" hs_bindgen_74261cdb0fbe635f_base ::
     RIP.Word8
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f3@
hs_bindgen_74261cdb0fbe635f ::
     RIP.CUChar
  -> IO A
hs_bindgen_74261cdb0fbe635f =
  RIP.fromFFIType hs_bindgen_74261cdb0fbe635f_base

{-| __C declaration:__ @f3@

    __defined at:__ @macros\/reparse_arithmetic_types.h 23:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f3 ::
     RIP.CUChar
     -- ^ __C declaration:__ @x@
  -> IO A
f3 = hs_bindgen_74261cdb0fbe635f

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f4@
foreign import ccall unsafe "hs_bindgen_c95f9efe0dd50c93" hs_bindgen_c95f9efe0dd50c93_base ::
     RIP.Int16
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f4@
hs_bindgen_c95f9efe0dd50c93 ::
     RIP.CShort
  -> IO A
hs_bindgen_c95f9efe0dd50c93 =
  RIP.fromFFIType hs_bindgen_c95f9efe0dd50c93_base

{-| __C declaration:__ @f4@

    __defined at:__ @macros\/reparse_arithmetic_types.h 28:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f4 ::
     RIP.CShort
     -- ^ __C declaration:__ @x@
  -> IO A
f4 = hs_bindgen_c95f9efe0dd50c93

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f5@
foreign import ccall unsafe "hs_bindgen_d10b31bf8d27f6fc" hs_bindgen_d10b31bf8d27f6fc_base ::
     RIP.Int16
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f5@
hs_bindgen_d10b31bf8d27f6fc ::
     RIP.CShort
  -> IO A
hs_bindgen_d10b31bf8d27f6fc =
  RIP.fromFFIType hs_bindgen_d10b31bf8d27f6fc_base

{-| __C declaration:__ @f5@

    __defined at:__ @macros\/reparse_arithmetic_types.h 29:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f5 ::
     RIP.CShort
     -- ^ __C declaration:__ @x@
  -> IO A
f5 = hs_bindgen_d10b31bf8d27f6fc

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f6@
foreign import ccall unsafe "hs_bindgen_3fa9709f337eab12" hs_bindgen_3fa9709f337eab12_base ::
     RIP.Int16
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f6@
hs_bindgen_3fa9709f337eab12 ::
     RIP.CShort
  -> IO A
hs_bindgen_3fa9709f337eab12 =
  RIP.fromFFIType hs_bindgen_3fa9709f337eab12_base

{-| __C declaration:__ @f6@

    __defined at:__ @macros\/reparse_arithmetic_types.h 30:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f6 ::
     RIP.CShort
     -- ^ __C declaration:__ @x@
  -> IO A
f6 = hs_bindgen_3fa9709f337eab12

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f7@
foreign import ccall unsafe "hs_bindgen_1b4d3b85e2effa7e" hs_bindgen_1b4d3b85e2effa7e_base ::
     RIP.Int16
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f7@
hs_bindgen_1b4d3b85e2effa7e ::
     RIP.CShort
  -> IO A
hs_bindgen_1b4d3b85e2effa7e =
  RIP.fromFFIType hs_bindgen_1b4d3b85e2effa7e_base

{-| __C declaration:__ @f7@

    __defined at:__ @macros\/reparse_arithmetic_types.h 31:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f7 ::
     RIP.CShort
     -- ^ __C declaration:__ @x@
  -> IO A
f7 = hs_bindgen_1b4d3b85e2effa7e

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f8@
foreign import ccall unsafe "hs_bindgen_9815c4f12a17191b" hs_bindgen_9815c4f12a17191b_base ::
     RIP.Word16
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f8@
hs_bindgen_9815c4f12a17191b ::
     RIP.CUShort
  -> IO A
hs_bindgen_9815c4f12a17191b =
  RIP.fromFFIType hs_bindgen_9815c4f12a17191b_base

{-| __C declaration:__ @f8@

    __defined at:__ @macros\/reparse_arithmetic_types.h 32:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f8 ::
     RIP.CUShort
     -- ^ __C declaration:__ @x@
  -> IO A
f8 = hs_bindgen_9815c4f12a17191b

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f9@
foreign import ccall unsafe "hs_bindgen_8870a1617c594e81" hs_bindgen_8870a1617c594e81_base ::
     RIP.Word16
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f9@
hs_bindgen_8870a1617c594e81 ::
     RIP.CUShort
  -> IO A
hs_bindgen_8870a1617c594e81 =
  RIP.fromFFIType hs_bindgen_8870a1617c594e81_base

{-| __C declaration:__ @f9@

    __defined at:__ @macros\/reparse_arithmetic_types.h 33:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f9 ::
     RIP.CUShort
     -- ^ __C declaration:__ @x@
  -> IO A
f9 = hs_bindgen_8870a1617c594e81

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f10@
foreign import ccall unsafe "hs_bindgen_d8cf76545d90f6f5" hs_bindgen_d8cf76545d90f6f5_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f10@
hs_bindgen_d8cf76545d90f6f5 ::
     RIP.CInt
  -> IO A
hs_bindgen_d8cf76545d90f6f5 =
  RIP.fromFFIType hs_bindgen_d8cf76545d90f6f5_base

{-| __C declaration:__ @f10@

    __defined at:__ @macros\/reparse_arithmetic_types.h 35:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f10 ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO A
f10 = hs_bindgen_d8cf76545d90f6f5

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f11@
foreign import ccall unsafe "hs_bindgen_21c3896990e488d7" hs_bindgen_21c3896990e488d7_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f11@
hs_bindgen_21c3896990e488d7 ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_21c3896990e488d7 =
  RIP.fromFFIType hs_bindgen_21c3896990e488d7_base

{-| __C declaration:__ @f11@

    __defined at:__ @macros\/reparse_arithmetic_types.h 36:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f11 ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO RIP.CInt
f11 = hs_bindgen_21c3896990e488d7

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f12@
foreign import ccall unsafe "hs_bindgen_bb7965299e2ae640" hs_bindgen_bb7965299e2ae640_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f12@
hs_bindgen_bb7965299e2ae640 ::
     RIP.CInt
  -> IO A
hs_bindgen_bb7965299e2ae640 =
  RIP.fromFFIType hs_bindgen_bb7965299e2ae640_base

{-| __C declaration:__ @f12@

    __defined at:__ @macros\/reparse_arithmetic_types.h 37:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f12 ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO A
f12 = hs_bindgen_bb7965299e2ae640

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f13@
foreign import ccall unsafe "hs_bindgen_18851b26abc94040" hs_bindgen_18851b26abc94040_base ::
     RIP.Word32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f13@
hs_bindgen_18851b26abc94040 ::
     RIP.CUInt
  -> IO RIP.CInt
hs_bindgen_18851b26abc94040 =
  RIP.fromFFIType hs_bindgen_18851b26abc94040_base

{-| __C declaration:__ @f13@

    __defined at:__ @macros\/reparse_arithmetic_types.h 38:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f13 ::
     RIP.CUInt
     -- ^ __C declaration:__ @x@
  -> IO RIP.CInt
f13 = hs_bindgen_18851b26abc94040

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f14@
foreign import ccall unsafe "hs_bindgen_19cda67a187de450" hs_bindgen_19cda67a187de450_base ::
     RIP.Word32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f14@
hs_bindgen_19cda67a187de450 ::
     RIP.CUInt
  -> IO A
hs_bindgen_19cda67a187de450 =
  RIP.fromFFIType hs_bindgen_19cda67a187de450_base

{-| __C declaration:__ @f14@

    __defined at:__ @macros\/reparse_arithmetic_types.h 39:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f14 ::
     RIP.CUInt
     -- ^ __C declaration:__ @x@
  -> IO A
f14 = hs_bindgen_19cda67a187de450

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f15@
foreign import ccall unsafe "hs_bindgen_3b4e19e9d32bd171" hs_bindgen_3b4e19e9d32bd171_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f15@
hs_bindgen_3b4e19e9d32bd171 ::
     RIP.CLong
  -> IO A
hs_bindgen_3b4e19e9d32bd171 =
  RIP.fromFFIType hs_bindgen_3b4e19e9d32bd171_base

{-| __C declaration:__ @f15@

    __defined at:__ @macros\/reparse_arithmetic_types.h 41:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f15 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f15 = hs_bindgen_3b4e19e9d32bd171

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f16@
foreign import ccall unsafe "hs_bindgen_982bfd8318c0ffec" hs_bindgen_982bfd8318c0ffec_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f16@
hs_bindgen_982bfd8318c0ffec ::
     RIP.CLong
  -> IO A
hs_bindgen_982bfd8318c0ffec =
  RIP.fromFFIType hs_bindgen_982bfd8318c0ffec_base

{-| __C declaration:__ @f16@

    __defined at:__ @macros\/reparse_arithmetic_types.h 42:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f16 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f16 = hs_bindgen_982bfd8318c0ffec

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f17@
foreign import ccall unsafe "hs_bindgen_f67f503878415036" hs_bindgen_f67f503878415036_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f17@
hs_bindgen_f67f503878415036 ::
     RIP.CLong
  -> IO A
hs_bindgen_f67f503878415036 =
  RIP.fromFFIType hs_bindgen_f67f503878415036_base

{-| __C declaration:__ @f17@

    __defined at:__ @macros\/reparse_arithmetic_types.h 43:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f17 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f17 = hs_bindgen_f67f503878415036

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f18@
foreign import ccall unsafe "hs_bindgen_30405c9ae068d28b" hs_bindgen_30405c9ae068d28b_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f18@
hs_bindgen_30405c9ae068d28b ::
     RIP.CLong
  -> IO A
hs_bindgen_30405c9ae068d28b =
  RIP.fromFFIType hs_bindgen_30405c9ae068d28b_base

{-| __C declaration:__ @f18@

    __defined at:__ @macros\/reparse_arithmetic_types.h 44:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f18 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f18 = hs_bindgen_30405c9ae068d28b

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f19@
foreign import ccall unsafe "hs_bindgen_ee05369d2355e5f1" hs_bindgen_ee05369d2355e5f1_base ::
     RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f19@
hs_bindgen_ee05369d2355e5f1 ::
     RIP.CULong
  -> IO A
hs_bindgen_ee05369d2355e5f1 =
  RIP.fromFFIType hs_bindgen_ee05369d2355e5f1_base

{-| __C declaration:__ @f19@

    __defined at:__ @macros\/reparse_arithmetic_types.h 45:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f19 ::
     RIP.CULong
     -- ^ __C declaration:__ @x@
  -> IO A
f19 = hs_bindgen_ee05369d2355e5f1

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f20@
foreign import ccall unsafe "hs_bindgen_afa5f3bae03f34e0" hs_bindgen_afa5f3bae03f34e0_base ::
     RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f20@
hs_bindgen_afa5f3bae03f34e0 ::
     RIP.CULong
  -> IO A
hs_bindgen_afa5f3bae03f34e0 =
  RIP.fromFFIType hs_bindgen_afa5f3bae03f34e0_base

{-| __C declaration:__ @f20@

    __defined at:__ @macros\/reparse_arithmetic_types.h 46:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f20 ::
     RIP.CULong
     -- ^ __C declaration:__ @x@
  -> IO A
f20 = hs_bindgen_afa5f3bae03f34e0

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f21@
foreign import ccall unsafe "hs_bindgen_488efc7f974c971b" hs_bindgen_488efc7f974c971b_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f21@
hs_bindgen_488efc7f974c971b ::
     RIP.CLong
  -> IO A
hs_bindgen_488efc7f974c971b =
  RIP.fromFFIType hs_bindgen_488efc7f974c971b_base

{-| __C declaration:__ @f21@

    __defined at:__ @macros\/reparse_arithmetic_types.h 48:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f21 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f21 = hs_bindgen_488efc7f974c971b

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f22@
foreign import ccall unsafe "hs_bindgen_d6563ce8f86f1541" hs_bindgen_d6563ce8f86f1541_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f22@
hs_bindgen_d6563ce8f86f1541 ::
     RIP.CLong
  -> IO A
hs_bindgen_d6563ce8f86f1541 =
  RIP.fromFFIType hs_bindgen_d6563ce8f86f1541_base

{-| __C declaration:__ @f22@

    __defined at:__ @macros\/reparse_arithmetic_types.h 49:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f22 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f22 = hs_bindgen_d6563ce8f86f1541

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f23@
foreign import ccall unsafe "hs_bindgen_0ecb5a60c4365a73" hs_bindgen_0ecb5a60c4365a73_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f23@
hs_bindgen_0ecb5a60c4365a73 ::
     RIP.CLong
  -> IO A
hs_bindgen_0ecb5a60c4365a73 =
  RIP.fromFFIType hs_bindgen_0ecb5a60c4365a73_base

{-| __C declaration:__ @f23@

    __defined at:__ @macros\/reparse_arithmetic_types.h 50:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f23 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f23 = hs_bindgen_0ecb5a60c4365a73

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f24@
foreign import ccall unsafe "hs_bindgen_0d5e24fc786feaec" hs_bindgen_0d5e24fc786feaec_base ::
     RIP.Int64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f24@
hs_bindgen_0d5e24fc786feaec ::
     RIP.CLong
  -> IO A
hs_bindgen_0d5e24fc786feaec =
  RIP.fromFFIType hs_bindgen_0d5e24fc786feaec_base

{-| __C declaration:__ @f24@

    __defined at:__ @macros\/reparse_arithmetic_types.h 51:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f24 ::
     RIP.CLong
     -- ^ __C declaration:__ @x@
  -> IO A
f24 = hs_bindgen_0d5e24fc786feaec

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f25@
foreign import ccall unsafe "hs_bindgen_d6b4f74553c4e27b" hs_bindgen_d6b4f74553c4e27b_base ::
     RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f25@
hs_bindgen_d6b4f74553c4e27b ::
     RIP.CULong
  -> IO A
hs_bindgen_d6b4f74553c4e27b =
  RIP.fromFFIType hs_bindgen_d6b4f74553c4e27b_base

{-| __C declaration:__ @f25@

    __defined at:__ @macros\/reparse_arithmetic_types.h 52:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f25 ::
     RIP.CULong
     -- ^ __C declaration:__ @x@
  -> IO A
f25 = hs_bindgen_d6b4f74553c4e27b

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f26@
foreign import ccall unsafe "hs_bindgen_da234f84dc67b630" hs_bindgen_da234f84dc67b630_base ::
     RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f26@
hs_bindgen_da234f84dc67b630 ::
     RIP.CULong
  -> IO A
hs_bindgen_da234f84dc67b630 =
  RIP.fromFFIType hs_bindgen_da234f84dc67b630_base

{-| __C declaration:__ @f26@

    __defined at:__ @macros\/reparse_arithmetic_types.h 53:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f26 ::
     RIP.CULong
     -- ^ __C declaration:__ @x@
  -> IO A
f26 = hs_bindgen_da234f84dc67b630

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f27@
foreign import ccall unsafe "hs_bindgen_26b01e993c12e2f6" hs_bindgen_26b01e993c12e2f6_base ::
     Float
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f27@
hs_bindgen_26b01e993c12e2f6 ::
     RIP.CFloat
  -> IO A
hs_bindgen_26b01e993c12e2f6 =
  RIP.fromFFIType hs_bindgen_26b01e993c12e2f6_base

{-| __C declaration:__ @f27@

    __defined at:__ @macros\/reparse_arithmetic_types.h 58:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f27 ::
     RIP.CFloat
     -- ^ __C declaration:__ @x@
  -> IO A
f27 = hs_bindgen_26b01e993c12e2f6

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f28@
foreign import ccall unsafe "hs_bindgen_18913631127a2f42" hs_bindgen_18913631127a2f42_base ::
     Double
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_arithmetic_type_Example_Unsafe_f28@
hs_bindgen_18913631127a2f42 ::
     RIP.CDouble
  -> IO A
hs_bindgen_18913631127a2f42 =
  RIP.fromFFIType hs_bindgen_18913631127a2f42_base

{-| __C declaration:__ @f28@

    __defined at:__ @macros\/reparse_arithmetic_types.h 59:3@

    __exported by:__ @macros\/reparse_arithmetic_types.h@
-}
f28 ::
     RIP.CDouble
     -- ^ __C declaration:__ @x@
  -> IO A
f28 = hs_bindgen_18913631127a2f42
