{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.set_foo_8
    , Example.Safe.eq_foo_8
    , Example.Safe.set_foo_16
    , Example.Safe.eq_foo_16
    , Example.Safe.set_foo_32
    , Example.Safe.eq_foo_32
    , Example.Safe.set_foo_64
    , Example.Safe.eq_foo_64
    , Example.Safe.set_bar_8_8
    , Example.Safe.eq_bar_8_8
    , Example.Safe.set_bar_8_16
    , Example.Safe.eq_bar_8_16
    , Example.Safe.set_bar_8_32
    , Example.Safe.eq_bar_8_32
    , Example.Safe.set_bar_8_64
    , Example.Safe.eq_bar_8_64
    , Example.Safe.set_bar_16_16
    , Example.Safe.eq_bar_16_16
    , Example.Safe.set_bar_16_32
    , Example.Safe.eq_bar_16_32
    , Example.Safe.set_bar_16_64
    , Example.Safe.eq_bar_16_64
    , Example.Safe.set_bar_32_32
    , Example.Safe.eq_bar_32_32
    , Example.Safe.set_bar_32_64
    , Example.Safe.eq_bar_32_64
    , Example.Safe.set_bar_64_64
    , Example.Safe.eq_bar_64_64
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/structs/bitfields.h>"
  , "void hs_bindgen_d709d3e43745d9d1 ("
  , "  struct foo_8 *arg1,"
  , "  signed char arg2,"
  , "  signed char arg3,"
  , "  signed char arg4,"
  , "  signed char arg5,"
  , "  signed char arg6,"
  , "  signed char arg7"
  , ")"
  , "{"
  , "  (set_foo_8)(arg1, arg2, arg3, arg4, arg5, arg6, arg7);"
  , "}"
  , "_Bool hs_bindgen_333df3a6717d2344 ("
  , "  struct foo_8 *arg1,"
  , "  signed char arg2,"
  , "  signed char arg3,"
  , "  signed char arg4,"
  , "  signed char arg5,"
  , "  signed char arg6,"
  , "  signed char arg7"
  , ")"
  , "{"
  , "  return (eq_foo_8)(arg1, arg2, arg3, arg4, arg5, arg6, arg7);"
  , "}"
  , "void hs_bindgen_a32d14ecd4c88208 ("
  , "  struct foo_16 *arg1,"
  , "  signed char arg2,"
  , "  signed int arg3,"
  , "  signed int arg4,"
  , "  signed int arg5,"
  , "  signed int arg6,"
  , "  signed int arg7"
  , ")"
  , "{"
  , "  (set_foo_16)(arg1, arg2, arg3, arg4, arg5, arg6, arg7);"
  , "}"
  , "_Bool hs_bindgen_393f8ab8499de0df ("
  , "  struct foo_16 *arg1,"
  , "  signed char arg2,"
  , "  signed int arg3,"
  , "  signed int arg4,"
  , "  signed int arg5,"
  , "  signed int arg6,"
  , "  signed int arg7"
  , ")"
  , "{"
  , "  return (eq_foo_16)(arg1, arg2, arg3, arg4, arg5, arg6, arg7);"
  , "}"
  , "void hs_bindgen_768898a3ea8b0e71 ("
  , "  struct foo_32 *arg1,"
  , "  signed char arg2,"
  , "  signed int arg3,"
  , "  signed int arg4,"
  , "  signed int arg5,"
  , "  signed long arg6,"
  , "  signed int arg7,"
  , "  signed long arg8"
  , ")"
  , "{"
  , "  (set_foo_32)(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);"
  , "}"
  , "_Bool hs_bindgen_c6b8a2d7947cba29 ("
  , "  struct foo_32 *arg1,"
  , "  signed char arg2,"
  , "  signed int arg3,"
  , "  signed int arg4,"
  , "  signed int arg5,"
  , "  signed long arg6,"
  , "  signed int arg7,"
  , "  signed long arg8"
  , ")"
  , "{"
  , "  return (eq_foo_32)(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);"
  , "}"
  , "void hs_bindgen_ccda87b4c75b7ba8 ("
  , "  struct foo_64 *arg1,"
  , "  signed long arg2,"
  , "  signed long long arg3,"
  , "  signed long long arg4,"
  , "  signed long long arg5"
  , ")"
  , "{"
  , "  (set_foo_64)(arg1, arg2, arg3, arg4, arg5);"
  , "}"
  , "_Bool hs_bindgen_c5e31dc7361d2c10 ("
  , "  struct foo_64 *arg1,"
  , "  signed long arg2,"
  , "  signed long long arg3,"
  , "  signed long long arg4,"
  , "  signed long long arg5"
  , ")"
  , "{"
  , "  return (eq_foo_64)(arg1, arg2, arg3, arg4, arg5);"
  , "}"
  , "void hs_bindgen_943f154c90bf60d8 ("
  , "  struct bar_8_8 *arg1,"
  , "  signed char arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  (set_bar_8_8)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_8f2f180df0461ff8 ("
  , "  struct bar_8_8 *arg1,"
  , "  signed char arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return (eq_bar_8_8)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_952bcdd2fd03a086 ("
  , "  struct bar_8_16 *arg1,"
  , "  signed int arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  (set_bar_8_16)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_79e9ce863f424e9c ("
  , "  struct bar_8_16 *arg1,"
  , "  signed int arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return (eq_bar_8_16)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_c7e2e66c8931a7e2 ("
  , "  struct bar_8_32 *arg1,"
  , "  signed long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  (set_bar_8_32)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_035bd9a2a7abb9a7 ("
  , "  struct bar_8_32 *arg1,"
  , "  signed long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return (eq_bar_8_32)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_af2a92736ba39bab ("
  , "  struct bar_8_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  (set_bar_8_64)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_809190212faa1188 ("
  , "  struct bar_8_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return (eq_bar_8_64)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_3162f51ed8ec59f1 ("
  , "  struct bar_16_16 *arg1,"
  , "  signed int arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  (set_bar_16_16)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_de2f1050d9dc0e8c ("
  , "  struct bar_16_16 *arg1,"
  , "  signed int arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return (eq_bar_16_16)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_aa1139534c1f4753 ("
  , "  struct bar_16_32 *arg1,"
  , "  signed long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  (set_bar_16_32)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_d219fbfa3b87c798 ("
  , "  struct bar_16_32 *arg1,"
  , "  signed long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return (eq_bar_16_32)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_4cc149f0a4f291df ("
  , "  struct bar_16_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  (set_bar_16_64)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_061b326047171bd9 ("
  , "  struct bar_16_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return (eq_bar_16_64)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_a69c80f1cee32ea0 ("
  , "  struct bar_32_32 *arg1,"
  , "  signed long arg2,"
  , "  signed long arg3"
  , ")"
  , "{"
  , "  (set_bar_32_32)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_9350827bfd7d54ad ("
  , "  struct bar_32_32 *arg1,"
  , "  signed long arg2,"
  , "  signed long arg3"
  , ")"
  , "{"
  , "  return (eq_bar_32_32)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_bf91cf0bd9c30b10 ("
  , "  struct bar_32_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed long arg3"
  , ")"
  , "{"
  , "  (set_bar_32_64)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_ba686f3e80ed74f3 ("
  , "  struct bar_32_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed long arg3"
  , ")"
  , "{"
  , "  return (eq_bar_32_64)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_7088877daad5c4d7 ("
  , "  struct bar_64_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed long long arg3"
  , ")"
  , "{"
  , "  (set_bar_64_64)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_933ce24b55469f6c ("
  , "  struct bar_64_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed long long arg3"
  , ")"
  , "{"
  , "  return (eq_bar_64_64)(arg1, arg2, arg3);"
  , "}"
  ]))

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_foo_8@
foreign import ccall safe "hs_bindgen_d709d3e43745d9d1" hs_bindgen_d709d3e43745d9d1_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> RIP.Int8
  -> RIP.Int8
  -> RIP.Int8
  -> RIP.Int8
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_foo_8@
hs_bindgen_d709d3e43745d9d1 ::
     RIP.Ptr Foo_8
  -> RIP.CSChar
  -> RIP.CSChar
  -> RIP.CSChar
  -> RIP.CSChar
  -> RIP.CSChar
  -> RIP.CSChar
  -> IO ()
hs_bindgen_d709d3e43745d9d1 =
  RIP.fromFFIType hs_bindgen_d709d3e43745d9d1_base

{-| __C declaration:__ @set_foo_8@

    __defined at:__ @types\/structs\/bitfields.h 34:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_foo_8 ::
     RIP.Ptr Foo_8
     -- ^ __C declaration:__ @x@
  -> RIP.CSChar
     -- ^ __C declaration:__ @a@
  -> RIP.CSChar
     -- ^ __C declaration:__ @b@
  -> RIP.CSChar
     -- ^ __C declaration:__ @c@
  -> RIP.CSChar
     -- ^ __C declaration:__ @d@
  -> RIP.CSChar
     -- ^ __C declaration:__ @e@
  -> RIP.CSChar
     -- ^ __C declaration:__ @f@
  -> IO ()
set_foo_8 = hs_bindgen_d709d3e43745d9d1

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_foo_8@
foreign import ccall safe "hs_bindgen_333df3a6717d2344" hs_bindgen_333df3a6717d2344_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> RIP.Int8
  -> RIP.Int8
  -> RIP.Int8
  -> RIP.Int8
  -> RIP.Int8
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_foo_8@
hs_bindgen_333df3a6717d2344 ::
     RIP.Ptr Foo_8
  -> RIP.CSChar
  -> RIP.CSChar
  -> RIP.CSChar
  -> RIP.CSChar
  -> RIP.CSChar
  -> RIP.CSChar
  -> IO RIP.CBool
hs_bindgen_333df3a6717d2344 =
  RIP.fromFFIType hs_bindgen_333df3a6717d2344_base

{-| __C declaration:__ @eq_foo_8@

    __defined at:__ @types\/structs\/bitfields.h 50:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_foo_8 ::
     RIP.Ptr Foo_8
     -- ^ __C declaration:__ @x@
  -> RIP.CSChar
     -- ^ __C declaration:__ @a@
  -> RIP.CSChar
     -- ^ __C declaration:__ @b@
  -> RIP.CSChar
     -- ^ __C declaration:__ @c@
  -> RIP.CSChar
     -- ^ __C declaration:__ @d@
  -> RIP.CSChar
     -- ^ __C declaration:__ @e@
  -> RIP.CSChar
     -- ^ __C declaration:__ @f@
  -> IO RIP.CBool
eq_foo_8 = hs_bindgen_333df3a6717d2344

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_foo_16@
foreign import ccall safe "hs_bindgen_a32d14ecd4c88208" hs_bindgen_a32d14ecd4c88208_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_foo_16@
hs_bindgen_a32d14ecd4c88208 ::
     RIP.Ptr Foo_16
  -> RIP.CSChar
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CInt
  -> IO ()
hs_bindgen_a32d14ecd4c88208 =
  RIP.fromFFIType hs_bindgen_a32d14ecd4c88208_base

{-| __C declaration:__ @set_foo_16@

    __defined at:__ @types\/structs\/bitfields.h 79:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_foo_16 ::
     RIP.Ptr Foo_16
     -- ^ __C declaration:__ @x@
  -> RIP.CSChar
     -- ^ __C declaration:__ @a@
  -> RIP.CInt
     -- ^ __C declaration:__ @b@
  -> RIP.CInt
     -- ^ __C declaration:__ @c@
  -> RIP.CInt
     -- ^ __C declaration:__ @d@
  -> RIP.CInt
     -- ^ __C declaration:__ @e@
  -> RIP.CInt
     -- ^ __C declaration:__ @f@
  -> IO ()
set_foo_16 = hs_bindgen_a32d14ecd4c88208

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_foo_16@
foreign import ccall safe "hs_bindgen_393f8ab8499de0df" hs_bindgen_393f8ab8499de0df_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_foo_16@
hs_bindgen_393f8ab8499de0df ::
     RIP.Ptr Foo_16
  -> RIP.CSChar
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CInt
  -> IO RIP.CBool
hs_bindgen_393f8ab8499de0df =
  RIP.fromFFIType hs_bindgen_393f8ab8499de0df_base

{-| __C declaration:__ @eq_foo_16@

    __defined at:__ @types\/structs\/bitfields.h 95:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_foo_16 ::
     RIP.Ptr Foo_16
     -- ^ __C declaration:__ @x@
  -> RIP.CSChar
     -- ^ __C declaration:__ @a@
  -> RIP.CInt
     -- ^ __C declaration:__ @b@
  -> RIP.CInt
     -- ^ __C declaration:__ @c@
  -> RIP.CInt
     -- ^ __C declaration:__ @d@
  -> RIP.CInt
     -- ^ __C declaration:__ @e@
  -> RIP.CInt
     -- ^ __C declaration:__ @f@
  -> IO RIP.CBool
eq_foo_16 = hs_bindgen_393f8ab8499de0df

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_foo_32@
foreign import ccall safe "hs_bindgen_768898a3ea8b0e71" hs_bindgen_768898a3ea8b0e71_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int64
  -> RIP.Int32
  -> RIP.Int64
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_foo_32@
hs_bindgen_768898a3ea8b0e71 ::
     RIP.Ptr Foo_32
  -> RIP.CSChar
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CLong
  -> RIP.CInt
  -> RIP.CLong
  -> IO ()
hs_bindgen_768898a3ea8b0e71 =
  RIP.fromFFIType hs_bindgen_768898a3ea8b0e71_base

{-| __C declaration:__ @set_foo_32@

    __defined at:__ @types\/structs\/bitfields.h 125:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_foo_32 ::
     RIP.Ptr Foo_32
     -- ^ __C declaration:__ @x@
  -> RIP.CSChar
     -- ^ __C declaration:__ @a@
  -> RIP.CInt
     -- ^ __C declaration:__ @b@
  -> RIP.CInt
     -- ^ __C declaration:__ @c@
  -> RIP.CInt
     -- ^ __C declaration:__ @d@
  -> RIP.CLong
     -- ^ __C declaration:__ @e@
  -> RIP.CInt
     -- ^ __C declaration:__ @f@
  -> RIP.CLong
     -- ^ __C declaration:__ @g@
  -> IO ()
set_foo_32 = hs_bindgen_768898a3ea8b0e71

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_foo_32@
foreign import ccall safe "hs_bindgen_c6b8a2d7947cba29" hs_bindgen_c6b8a2d7947cba29_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int64
  -> RIP.Int32
  -> RIP.Int64
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_foo_32@
hs_bindgen_c6b8a2d7947cba29 ::
     RIP.Ptr Foo_32
  -> RIP.CSChar
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CLong
  -> RIP.CInt
  -> RIP.CLong
  -> IO RIP.CBool
hs_bindgen_c6b8a2d7947cba29 =
  RIP.fromFFIType hs_bindgen_c6b8a2d7947cba29_base

{-| __C declaration:__ @eq_foo_32@

    __defined at:__ @types\/structs\/bitfields.h 143:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_foo_32 ::
     RIP.Ptr Foo_32
     -- ^ __C declaration:__ @x@
  -> RIP.CSChar
     -- ^ __C declaration:__ @a@
  -> RIP.CInt
     -- ^ __C declaration:__ @b@
  -> RIP.CInt
     -- ^ __C declaration:__ @c@
  -> RIP.CInt
     -- ^ __C declaration:__ @d@
  -> RIP.CLong
     -- ^ __C declaration:__ @e@
  -> RIP.CInt
     -- ^ __C declaration:__ @f@
  -> RIP.CLong
     -- ^ __C declaration:__ @g@
  -> IO RIP.CBool
eq_foo_32 = hs_bindgen_c6b8a2d7947cba29

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_foo_64@
foreign import ccall safe "hs_bindgen_ccda87b4c75b7ba8" hs_bindgen_ccda87b4c75b7ba8_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int64
  -> RIP.Int64
  -> RIP.Int64
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_foo_64@
hs_bindgen_ccda87b4c75b7ba8 ::
     RIP.Ptr Foo_64
  -> RIP.CLong
  -> RIP.CLLong
  -> RIP.CLLong
  -> RIP.CLLong
  -> IO ()
hs_bindgen_ccda87b4c75b7ba8 =
  RIP.fromFFIType hs_bindgen_ccda87b4c75b7ba8_base

{-| __C declaration:__ @set_foo_64@

    __defined at:__ @types\/structs\/bitfields.h 172:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_foo_64 ::
     RIP.Ptr Foo_64
     -- ^ __C declaration:__ @x@
  -> RIP.CLong
     -- ^ __C declaration:__ @a@
  -> RIP.CLLong
     -- ^ __C declaration:__ @b@
  -> RIP.CLLong
     -- ^ __C declaration:__ @c@
  -> RIP.CLLong
     -- ^ __C declaration:__ @d@
  -> IO ()
set_foo_64 = hs_bindgen_ccda87b4c75b7ba8

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_foo_64@
foreign import ccall safe "hs_bindgen_c5e31dc7361d2c10" hs_bindgen_c5e31dc7361d2c10_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int64
  -> RIP.Int64
  -> RIP.Int64
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_foo_64@
hs_bindgen_c5e31dc7361d2c10 ::
     RIP.Ptr Foo_64
  -> RIP.CLong
  -> RIP.CLLong
  -> RIP.CLLong
  -> RIP.CLLong
  -> IO RIP.CBool
hs_bindgen_c5e31dc7361d2c10 =
  RIP.fromFFIType hs_bindgen_c5e31dc7361d2c10_base

{-| __C declaration:__ @eq_foo_64@

    __defined at:__ @types\/structs\/bitfields.h 184:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_foo_64 ::
     RIP.Ptr Foo_64
     -- ^ __C declaration:__ @x@
  -> RIP.CLong
     -- ^ __C declaration:__ @a@
  -> RIP.CLLong
     -- ^ __C declaration:__ @b@
  -> RIP.CLLong
     -- ^ __C declaration:__ @c@
  -> RIP.CLLong
     -- ^ __C declaration:__ @d@
  -> IO RIP.CBool
eq_foo_64 = hs_bindgen_c5e31dc7361d2c10

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_8_8@
foreign import ccall safe "hs_bindgen_943f154c90bf60d8" hs_bindgen_943f154c90bf60d8_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_8_8@
hs_bindgen_943f154c90bf60d8 ::
     RIP.Ptr Bar_8_8
  -> RIP.CSChar
  -> RIP.CInt
  -> IO ()
hs_bindgen_943f154c90bf60d8 =
  RIP.fromFFIType hs_bindgen_943f154c90bf60d8_base

{-| __C declaration:__ @set_bar_8_8@

    __defined at:__ @types\/structs\/bitfields.h 205:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_8_8 ::
     RIP.Ptr Bar_8_8
     -- ^ __C declaration:__ @x@
  -> RIP.CSChar
     -- ^ __C declaration:__ @a@
  -> RIP.CInt
     -- ^ __C declaration:__ @b@
  -> IO ()
set_bar_8_8 = hs_bindgen_943f154c90bf60d8

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_8_8@
foreign import ccall safe "hs_bindgen_8f2f180df0461ff8" hs_bindgen_8f2f180df0461ff8_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_8_8@
hs_bindgen_8f2f180df0461ff8 ::
     RIP.Ptr Bar_8_8
  -> RIP.CSChar
  -> RIP.CInt
  -> IO RIP.CBool
hs_bindgen_8f2f180df0461ff8 =
  RIP.fromFFIType hs_bindgen_8f2f180df0461ff8_base

{-| __C declaration:__ @eq_bar_8_8@

    __defined at:__ @types\/structs\/bitfields.h 213:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_8_8 ::
     RIP.Ptr Bar_8_8
     -- ^ __C declaration:__ @x@
  -> RIP.CSChar
     -- ^ __C declaration:__ @a@
  -> RIP.CInt
     -- ^ __C declaration:__ @b@
  -> IO RIP.CBool
eq_bar_8_8 = hs_bindgen_8f2f180df0461ff8

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_8_16@
foreign import ccall safe "hs_bindgen_952bcdd2fd03a086" hs_bindgen_952bcdd2fd03a086_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int32
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_8_16@
hs_bindgen_952bcdd2fd03a086 ::
     RIP.Ptr Bar_8_16
  -> RIP.CInt
  -> RIP.CInt
  -> IO ()
hs_bindgen_952bcdd2fd03a086 =
  RIP.fromFFIType hs_bindgen_952bcdd2fd03a086_base

{-| __C declaration:__ @set_bar_8_16@

    __defined at:__ @types\/structs\/bitfields.h 230:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_8_16 ::
     RIP.Ptr Bar_8_16
     -- ^ __C declaration:__ @x@
  -> RIP.CInt
     -- ^ __C declaration:__ @a@
  -> RIP.CInt
     -- ^ __C declaration:__ @b@
  -> IO ()
set_bar_8_16 = hs_bindgen_952bcdd2fd03a086

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_8_16@
foreign import ccall safe "hs_bindgen_79e9ce863f424e9c" hs_bindgen_79e9ce863f424e9c_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int32
  -> RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_8_16@
hs_bindgen_79e9ce863f424e9c ::
     RIP.Ptr Bar_8_16
  -> RIP.CInt
  -> RIP.CInt
  -> IO RIP.CBool
hs_bindgen_79e9ce863f424e9c =
  RIP.fromFFIType hs_bindgen_79e9ce863f424e9c_base

{-| __C declaration:__ @eq_bar_8_16@

    __defined at:__ @types\/structs\/bitfields.h 238:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_8_16 ::
     RIP.Ptr Bar_8_16
     -- ^ __C declaration:__ @x@
  -> RIP.CInt
     -- ^ __C declaration:__ @a@
  -> RIP.CInt
     -- ^ __C declaration:__ @b@
  -> IO RIP.CBool
eq_bar_8_16 = hs_bindgen_79e9ce863f424e9c

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_8_32@
foreign import ccall safe "hs_bindgen_c7e2e66c8931a7e2" hs_bindgen_c7e2e66c8931a7e2_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_8_32@
hs_bindgen_c7e2e66c8931a7e2 ::
     RIP.Ptr Bar_8_32
  -> RIP.CLong
  -> RIP.CInt
  -> IO ()
hs_bindgen_c7e2e66c8931a7e2 =
  RIP.fromFFIType hs_bindgen_c7e2e66c8931a7e2_base

{-| __C declaration:__ @set_bar_8_32@

    __defined at:__ @types\/structs\/bitfields.h 255:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_8_32 ::
     RIP.Ptr Bar_8_32
     -- ^ __C declaration:__ @x@
  -> RIP.CLong
     -- ^ __C declaration:__ @a@
  -> RIP.CInt
     -- ^ __C declaration:__ @b@
  -> IO ()
set_bar_8_32 = hs_bindgen_c7e2e66c8931a7e2

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_8_32@
foreign import ccall safe "hs_bindgen_035bd9a2a7abb9a7" hs_bindgen_035bd9a2a7abb9a7_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_8_32@
hs_bindgen_035bd9a2a7abb9a7 ::
     RIP.Ptr Bar_8_32
  -> RIP.CLong
  -> RIP.CInt
  -> IO RIP.CBool
hs_bindgen_035bd9a2a7abb9a7 =
  RIP.fromFFIType hs_bindgen_035bd9a2a7abb9a7_base

{-| __C declaration:__ @eq_bar_8_32@

    __defined at:__ @types\/structs\/bitfields.h 263:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_8_32 ::
     RIP.Ptr Bar_8_32
     -- ^ __C declaration:__ @x@
  -> RIP.CLong
     -- ^ __C declaration:__ @a@
  -> RIP.CInt
     -- ^ __C declaration:__ @b@
  -> IO RIP.CBool
eq_bar_8_32 = hs_bindgen_035bd9a2a7abb9a7

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_8_64@
foreign import ccall safe "hs_bindgen_af2a92736ba39bab" hs_bindgen_af2a92736ba39bab_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_8_64@
hs_bindgen_af2a92736ba39bab ::
     RIP.Ptr Bar_8_64
  -> RIP.CLLong
  -> RIP.CInt
  -> IO ()
hs_bindgen_af2a92736ba39bab =
  RIP.fromFFIType hs_bindgen_af2a92736ba39bab_base

{-| __C declaration:__ @set_bar_8_64@

    __defined at:__ @types\/structs\/bitfields.h 280:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_8_64 ::
     RIP.Ptr Bar_8_64
     -- ^ __C declaration:__ @x@
  -> RIP.CLLong
     -- ^ __C declaration:__ @a@
  -> RIP.CInt
     -- ^ __C declaration:__ @b@
  -> IO ()
set_bar_8_64 = hs_bindgen_af2a92736ba39bab

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_8_64@
foreign import ccall safe "hs_bindgen_809190212faa1188" hs_bindgen_809190212faa1188_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_8_64@
hs_bindgen_809190212faa1188 ::
     RIP.Ptr Bar_8_64
  -> RIP.CLLong
  -> RIP.CInt
  -> IO RIP.CBool
hs_bindgen_809190212faa1188 =
  RIP.fromFFIType hs_bindgen_809190212faa1188_base

{-| __C declaration:__ @eq_bar_8_64@

    __defined at:__ @types\/structs\/bitfields.h 288:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_8_64 ::
     RIP.Ptr Bar_8_64
     -- ^ __C declaration:__ @x@
  -> RIP.CLLong
     -- ^ __C declaration:__ @a@
  -> RIP.CInt
     -- ^ __C declaration:__ @b@
  -> IO RIP.CBool
eq_bar_8_64 = hs_bindgen_809190212faa1188

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_16_16@
foreign import ccall safe "hs_bindgen_3162f51ed8ec59f1" hs_bindgen_3162f51ed8ec59f1_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int32
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_16_16@
hs_bindgen_3162f51ed8ec59f1 ::
     RIP.Ptr Bar_16_16
  -> RIP.CInt
  -> RIP.CInt
  -> IO ()
hs_bindgen_3162f51ed8ec59f1 =
  RIP.fromFFIType hs_bindgen_3162f51ed8ec59f1_base

{-| __C declaration:__ @set_bar_16_16@

    __defined at:__ @types\/structs\/bitfields.h 305:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_16_16 ::
     RIP.Ptr Bar_16_16
     -- ^ __C declaration:__ @x@
  -> RIP.CInt
     -- ^ __C declaration:__ @a@
  -> RIP.CInt
     -- ^ __C declaration:__ @b@
  -> IO ()
set_bar_16_16 = hs_bindgen_3162f51ed8ec59f1

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_16_16@
foreign import ccall safe "hs_bindgen_de2f1050d9dc0e8c" hs_bindgen_de2f1050d9dc0e8c_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int32
  -> RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_16_16@
hs_bindgen_de2f1050d9dc0e8c ::
     RIP.Ptr Bar_16_16
  -> RIP.CInt
  -> RIP.CInt
  -> IO RIP.CBool
hs_bindgen_de2f1050d9dc0e8c =
  RIP.fromFFIType hs_bindgen_de2f1050d9dc0e8c_base

{-| __C declaration:__ @eq_bar_16_16@

    __defined at:__ @types\/structs\/bitfields.h 313:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_16_16 ::
     RIP.Ptr Bar_16_16
     -- ^ __C declaration:__ @x@
  -> RIP.CInt
     -- ^ __C declaration:__ @a@
  -> RIP.CInt
     -- ^ __C declaration:__ @b@
  -> IO RIP.CBool
eq_bar_16_16 = hs_bindgen_de2f1050d9dc0e8c

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_16_32@
foreign import ccall safe "hs_bindgen_aa1139534c1f4753" hs_bindgen_aa1139534c1f4753_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_16_32@
hs_bindgen_aa1139534c1f4753 ::
     RIP.Ptr Bar_16_32
  -> RIP.CLong
  -> RIP.CInt
  -> IO ()
hs_bindgen_aa1139534c1f4753 =
  RIP.fromFFIType hs_bindgen_aa1139534c1f4753_base

{-| __C declaration:__ @set_bar_16_32@

    __defined at:__ @types\/structs\/bitfields.h 330:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_16_32 ::
     RIP.Ptr Bar_16_32
     -- ^ __C declaration:__ @x@
  -> RIP.CLong
     -- ^ __C declaration:__ @a@
  -> RIP.CInt
     -- ^ __C declaration:__ @b@
  -> IO ()
set_bar_16_32 = hs_bindgen_aa1139534c1f4753

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_16_32@
foreign import ccall safe "hs_bindgen_d219fbfa3b87c798" hs_bindgen_d219fbfa3b87c798_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_16_32@
hs_bindgen_d219fbfa3b87c798 ::
     RIP.Ptr Bar_16_32
  -> RIP.CLong
  -> RIP.CInt
  -> IO RIP.CBool
hs_bindgen_d219fbfa3b87c798 =
  RIP.fromFFIType hs_bindgen_d219fbfa3b87c798_base

{-| __C declaration:__ @eq_bar_16_32@

    __defined at:__ @types\/structs\/bitfields.h 338:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_16_32 ::
     RIP.Ptr Bar_16_32
     -- ^ __C declaration:__ @x@
  -> RIP.CLong
     -- ^ __C declaration:__ @a@
  -> RIP.CInt
     -- ^ __C declaration:__ @b@
  -> IO RIP.CBool
eq_bar_16_32 = hs_bindgen_d219fbfa3b87c798

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_16_64@
foreign import ccall safe "hs_bindgen_4cc149f0a4f291df" hs_bindgen_4cc149f0a4f291df_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_16_64@
hs_bindgen_4cc149f0a4f291df ::
     RIP.Ptr Bar_16_64
  -> RIP.CLLong
  -> RIP.CInt
  -> IO ()
hs_bindgen_4cc149f0a4f291df =
  RIP.fromFFIType hs_bindgen_4cc149f0a4f291df_base

{-| __C declaration:__ @set_bar_16_64@

    __defined at:__ @types\/structs\/bitfields.h 355:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_16_64 ::
     RIP.Ptr Bar_16_64
     -- ^ __C declaration:__ @x@
  -> RIP.CLLong
     -- ^ __C declaration:__ @a@
  -> RIP.CInt
     -- ^ __C declaration:__ @b@
  -> IO ()
set_bar_16_64 = hs_bindgen_4cc149f0a4f291df

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_16_64@
foreign import ccall safe "hs_bindgen_061b326047171bd9" hs_bindgen_061b326047171bd9_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_16_64@
hs_bindgen_061b326047171bd9 ::
     RIP.Ptr Bar_16_64
  -> RIP.CLLong
  -> RIP.CInt
  -> IO RIP.CBool
hs_bindgen_061b326047171bd9 =
  RIP.fromFFIType hs_bindgen_061b326047171bd9_base

{-| __C declaration:__ @eq_bar_16_64@

    __defined at:__ @types\/structs\/bitfields.h 363:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_16_64 ::
     RIP.Ptr Bar_16_64
     -- ^ __C declaration:__ @x@
  -> RIP.CLLong
     -- ^ __C declaration:__ @a@
  -> RIP.CInt
     -- ^ __C declaration:__ @b@
  -> IO RIP.CBool
eq_bar_16_64 = hs_bindgen_061b326047171bd9

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_32_32@
foreign import ccall safe "hs_bindgen_a69c80f1cee32ea0" hs_bindgen_a69c80f1cee32ea0_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int64
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_32_32@
hs_bindgen_a69c80f1cee32ea0 ::
     RIP.Ptr Bar_32_32
  -> RIP.CLong
  -> RIP.CLong
  -> IO ()
hs_bindgen_a69c80f1cee32ea0 =
  RIP.fromFFIType hs_bindgen_a69c80f1cee32ea0_base

{-| __C declaration:__ @set_bar_32_32@

    __defined at:__ @types\/structs\/bitfields.h 380:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_32_32 ::
     RIP.Ptr Bar_32_32
     -- ^ __C declaration:__ @x@
  -> RIP.CLong
     -- ^ __C declaration:__ @a@
  -> RIP.CLong
     -- ^ __C declaration:__ @b@
  -> IO ()
set_bar_32_32 = hs_bindgen_a69c80f1cee32ea0

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_32_32@
foreign import ccall safe "hs_bindgen_9350827bfd7d54ad" hs_bindgen_9350827bfd7d54ad_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int64
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_32_32@
hs_bindgen_9350827bfd7d54ad ::
     RIP.Ptr Bar_32_32
  -> RIP.CLong
  -> RIP.CLong
  -> IO RIP.CBool
hs_bindgen_9350827bfd7d54ad =
  RIP.fromFFIType hs_bindgen_9350827bfd7d54ad_base

{-| __C declaration:__ @eq_bar_32_32@

    __defined at:__ @types\/structs\/bitfields.h 388:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_32_32 ::
     RIP.Ptr Bar_32_32
     -- ^ __C declaration:__ @x@
  -> RIP.CLong
     -- ^ __C declaration:__ @a@
  -> RIP.CLong
     -- ^ __C declaration:__ @b@
  -> IO RIP.CBool
eq_bar_32_32 = hs_bindgen_9350827bfd7d54ad

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_32_64@
foreign import ccall safe "hs_bindgen_bf91cf0bd9c30b10" hs_bindgen_bf91cf0bd9c30b10_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int64
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_32_64@
hs_bindgen_bf91cf0bd9c30b10 ::
     RIP.Ptr Bar_32_64
  -> RIP.CLLong
  -> RIP.CLong
  -> IO ()
hs_bindgen_bf91cf0bd9c30b10 =
  RIP.fromFFIType hs_bindgen_bf91cf0bd9c30b10_base

{-| __C declaration:__ @set_bar_32_64@

    __defined at:__ @types\/structs\/bitfields.h 405:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_32_64 ::
     RIP.Ptr Bar_32_64
     -- ^ __C declaration:__ @x@
  -> RIP.CLLong
     -- ^ __C declaration:__ @a@
  -> RIP.CLong
     -- ^ __C declaration:__ @b@
  -> IO ()
set_bar_32_64 = hs_bindgen_bf91cf0bd9c30b10

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_32_64@
foreign import ccall safe "hs_bindgen_ba686f3e80ed74f3" hs_bindgen_ba686f3e80ed74f3_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int64
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_32_64@
hs_bindgen_ba686f3e80ed74f3 ::
     RIP.Ptr Bar_32_64
  -> RIP.CLLong
  -> RIP.CLong
  -> IO RIP.CBool
hs_bindgen_ba686f3e80ed74f3 =
  RIP.fromFFIType hs_bindgen_ba686f3e80ed74f3_base

{-| __C declaration:__ @eq_bar_32_64@

    __defined at:__ @types\/structs\/bitfields.h 413:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_32_64 ::
     RIP.Ptr Bar_32_64
     -- ^ __C declaration:__ @x@
  -> RIP.CLLong
     -- ^ __C declaration:__ @a@
  -> RIP.CLong
     -- ^ __C declaration:__ @b@
  -> IO RIP.CBool
eq_bar_32_64 = hs_bindgen_ba686f3e80ed74f3

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_64_64@
foreign import ccall safe "hs_bindgen_7088877daad5c4d7" hs_bindgen_7088877daad5c4d7_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int64
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Safe_set_bar_64_64@
hs_bindgen_7088877daad5c4d7 ::
     RIP.Ptr Bar_64_64
  -> RIP.CLLong
  -> RIP.CLLong
  -> IO ()
hs_bindgen_7088877daad5c4d7 =
  RIP.fromFFIType hs_bindgen_7088877daad5c4d7_base

{-| __C declaration:__ @set_bar_64_64@

    __defined at:__ @types\/structs\/bitfields.h 430:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_64_64 ::
     RIP.Ptr Bar_64_64
     -- ^ __C declaration:__ @x@
  -> RIP.CLLong
     -- ^ __C declaration:__ @a@
  -> RIP.CLLong
     -- ^ __C declaration:__ @b@
  -> IO ()
set_bar_64_64 = hs_bindgen_7088877daad5c4d7

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_64_64@
foreign import ccall safe "hs_bindgen_933ce24b55469f6c" hs_bindgen_933ce24b55469f6c_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int64
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Safe_eq_bar_64_64@
hs_bindgen_933ce24b55469f6c ::
     RIP.Ptr Bar_64_64
  -> RIP.CLLong
  -> RIP.CLLong
  -> IO RIP.CBool
hs_bindgen_933ce24b55469f6c =
  RIP.fromFFIType hs_bindgen_933ce24b55469f6c_base

{-| __C declaration:__ @eq_bar_64_64@

    __defined at:__ @types\/structs\/bitfields.h 438:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_64_64 ::
     RIP.Ptr Bar_64_64
     -- ^ __C declaration:__ @x@
  -> RIP.CLLong
     -- ^ __C declaration:__ @a@
  -> RIP.CLLong
     -- ^ __C declaration:__ @b@
  -> IO RIP.CBool
eq_bar_64_64 = hs_bindgen_933ce24b55469f6c
