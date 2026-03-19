{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.set_foo_8
    , Example.Unsafe.eq_foo_8
    , Example.Unsafe.set_foo_16
    , Example.Unsafe.eq_foo_16
    , Example.Unsafe.set_foo_32
    , Example.Unsafe.eq_foo_32
    , Example.Unsafe.set_foo_64
    , Example.Unsafe.eq_foo_64
    , Example.Unsafe.set_bar_8_8
    , Example.Unsafe.eq_bar_8_8
    , Example.Unsafe.set_bar_8_16
    , Example.Unsafe.eq_bar_8_16
    , Example.Unsafe.set_bar_8_32
    , Example.Unsafe.eq_bar_8_32
    , Example.Unsafe.set_bar_8_64
    , Example.Unsafe.eq_bar_8_64
    , Example.Unsafe.set_bar_16_16
    , Example.Unsafe.eq_bar_16_16
    , Example.Unsafe.set_bar_16_32
    , Example.Unsafe.eq_bar_16_32
    , Example.Unsafe.set_bar_16_64
    , Example.Unsafe.eq_bar_16_64
    , Example.Unsafe.set_bar_32_32
    , Example.Unsafe.eq_bar_32_32
    , Example.Unsafe.set_bar_32_64
    , Example.Unsafe.eq_bar_32_64
    , Example.Unsafe.set_bar_64_64
    , Example.Unsafe.eq_bar_64_64
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/structs/bitfields.h>"
  , "void hs_bindgen_a8ac7a1b0514167d ("
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
  , "_Bool hs_bindgen_532217451d82e4f2 ("
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
  , "void hs_bindgen_cf7e49b6c0d45892 ("
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
  , "_Bool hs_bindgen_c9ce653a93e32939 ("
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
  , "void hs_bindgen_ac4a3be0b97ff87c ("
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
  , "_Bool hs_bindgen_516f69d150e332cd ("
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
  , "void hs_bindgen_6949b993d0bbcef8 ("
  , "  struct foo_64 *arg1,"
  , "  signed long arg2,"
  , "  signed long long arg3,"
  , "  signed long long arg4,"
  , "  signed long long arg5"
  , ")"
  , "{"
  , "  (set_foo_64)(arg1, arg2, arg3, arg4, arg5);"
  , "}"
  , "_Bool hs_bindgen_caeeb54830aeac7c ("
  , "  struct foo_64 *arg1,"
  , "  signed long arg2,"
  , "  signed long long arg3,"
  , "  signed long long arg4,"
  , "  signed long long arg5"
  , ")"
  , "{"
  , "  return (eq_foo_64)(arg1, arg2, arg3, arg4, arg5);"
  , "}"
  , "void hs_bindgen_d70f29d570a66d4d ("
  , "  struct bar_8_8 *arg1,"
  , "  signed char arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  (set_bar_8_8)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_49666188cc53a844 ("
  , "  struct bar_8_8 *arg1,"
  , "  signed char arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return (eq_bar_8_8)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_4402e495ed80145b ("
  , "  struct bar_8_16 *arg1,"
  , "  signed int arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  (set_bar_8_16)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_21018e5939f04a18 ("
  , "  struct bar_8_16 *arg1,"
  , "  signed int arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return (eq_bar_8_16)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_b6b609ae99686c1a ("
  , "  struct bar_8_32 *arg1,"
  , "  signed long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  (set_bar_8_32)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_57a297fb054d1b61 ("
  , "  struct bar_8_32 *arg1,"
  , "  signed long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return (eq_bar_8_32)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_5624ee6706340c28 ("
  , "  struct bar_8_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  (set_bar_8_64)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_57a006547646b355 ("
  , "  struct bar_8_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return (eq_bar_8_64)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_908406015b2ae986 ("
  , "  struct bar_16_16 *arg1,"
  , "  signed int arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  (set_bar_16_16)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_83f25fcbc4919d87 ("
  , "  struct bar_16_16 *arg1,"
  , "  signed int arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return (eq_bar_16_16)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_2fc932c4505d95bc ("
  , "  struct bar_16_32 *arg1,"
  , "  signed long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  (set_bar_16_32)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_d07b7c7434009536 ("
  , "  struct bar_16_32 *arg1,"
  , "  signed long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return (eq_bar_16_32)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_aa9694a81fe70333 ("
  , "  struct bar_16_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  (set_bar_16_64)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_775b431e7994bbb4 ("
  , "  struct bar_16_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return (eq_bar_16_64)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_c554619ce78113ba ("
  , "  struct bar_32_32 *arg1,"
  , "  signed long arg2,"
  , "  signed long arg3"
  , ")"
  , "{"
  , "  (set_bar_32_32)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_1821b4c2024c6db0 ("
  , "  struct bar_32_32 *arg1,"
  , "  signed long arg2,"
  , "  signed long arg3"
  , ")"
  , "{"
  , "  return (eq_bar_32_32)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_adf958d470681c09 ("
  , "  struct bar_32_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed long arg3"
  , ")"
  , "{"
  , "  (set_bar_32_64)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_6f9d2a4c9efe9bd1 ("
  , "  struct bar_32_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed long arg3"
  , ")"
  , "{"
  , "  return (eq_bar_32_64)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_d33dcf0f97e09802 ("
  , "  struct bar_64_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed long long arg3"
  , ")"
  , "{"
  , "  (set_bar_64_64)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_49948f3caf9be86d ("
  , "  struct bar_64_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed long long arg3"
  , ")"
  , "{"
  , "  return (eq_bar_64_64)(arg1, arg2, arg3);"
  , "}"
  ]))

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_foo_8@
foreign import ccall unsafe "hs_bindgen_a8ac7a1b0514167d" hs_bindgen_a8ac7a1b0514167d_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> RIP.Int8
  -> RIP.Int8
  -> RIP.Int8
  -> RIP.Int8
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_foo_8@
hs_bindgen_a8ac7a1b0514167d ::
     RIP.Ptr Foo_8
  -> RIP.CSChar
  -> RIP.CSChar
  -> RIP.CSChar
  -> RIP.CSChar
  -> RIP.CSChar
  -> RIP.CSChar
  -> IO ()
hs_bindgen_a8ac7a1b0514167d =
  RIP.fromFFIType hs_bindgen_a8ac7a1b0514167d_base

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
set_foo_8 = hs_bindgen_a8ac7a1b0514167d

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_foo_8@
foreign import ccall unsafe "hs_bindgen_532217451d82e4f2" hs_bindgen_532217451d82e4f2_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> RIP.Int8
  -> RIP.Int8
  -> RIP.Int8
  -> RIP.Int8
  -> RIP.Int8
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_foo_8@
hs_bindgen_532217451d82e4f2 ::
     RIP.Ptr Foo_8
  -> RIP.CSChar
  -> RIP.CSChar
  -> RIP.CSChar
  -> RIP.CSChar
  -> RIP.CSChar
  -> RIP.CSChar
  -> IO RIP.CBool
hs_bindgen_532217451d82e4f2 =
  RIP.fromFFIType hs_bindgen_532217451d82e4f2_base

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
eq_foo_8 = hs_bindgen_532217451d82e4f2

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_foo_16@
foreign import ccall unsafe "hs_bindgen_cf7e49b6c0d45892" hs_bindgen_cf7e49b6c0d45892_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_foo_16@
hs_bindgen_cf7e49b6c0d45892 ::
     RIP.Ptr Foo_16
  -> RIP.CSChar
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CInt
  -> IO ()
hs_bindgen_cf7e49b6c0d45892 =
  RIP.fromFFIType hs_bindgen_cf7e49b6c0d45892_base

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
set_foo_16 = hs_bindgen_cf7e49b6c0d45892

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_foo_16@
foreign import ccall unsafe "hs_bindgen_c9ce653a93e32939" hs_bindgen_c9ce653a93e32939_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_foo_16@
hs_bindgen_c9ce653a93e32939 ::
     RIP.Ptr Foo_16
  -> RIP.CSChar
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CInt
  -> IO RIP.CBool
hs_bindgen_c9ce653a93e32939 =
  RIP.fromFFIType hs_bindgen_c9ce653a93e32939_base

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
eq_foo_16 = hs_bindgen_c9ce653a93e32939

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_foo_32@
foreign import ccall unsafe "hs_bindgen_ac4a3be0b97ff87c" hs_bindgen_ac4a3be0b97ff87c_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int64
  -> RIP.Int32
  -> RIP.Int64
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_foo_32@
hs_bindgen_ac4a3be0b97ff87c ::
     RIP.Ptr Foo_32
  -> RIP.CSChar
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CLong
  -> RIP.CInt
  -> RIP.CLong
  -> IO ()
hs_bindgen_ac4a3be0b97ff87c =
  RIP.fromFFIType hs_bindgen_ac4a3be0b97ff87c_base

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
set_foo_32 = hs_bindgen_ac4a3be0b97ff87c

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_foo_32@
foreign import ccall unsafe "hs_bindgen_516f69d150e332cd" hs_bindgen_516f69d150e332cd_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int32
  -> RIP.Int64
  -> RIP.Int32
  -> RIP.Int64
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_foo_32@
hs_bindgen_516f69d150e332cd ::
     RIP.Ptr Foo_32
  -> RIP.CSChar
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CInt
  -> RIP.CLong
  -> RIP.CInt
  -> RIP.CLong
  -> IO RIP.CBool
hs_bindgen_516f69d150e332cd =
  RIP.fromFFIType hs_bindgen_516f69d150e332cd_base

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
eq_foo_32 = hs_bindgen_516f69d150e332cd

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_foo_64@
foreign import ccall unsafe "hs_bindgen_6949b993d0bbcef8" hs_bindgen_6949b993d0bbcef8_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int64
  -> RIP.Int64
  -> RIP.Int64
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_foo_64@
hs_bindgen_6949b993d0bbcef8 ::
     RIP.Ptr Foo_64
  -> RIP.CLong
  -> RIP.CLLong
  -> RIP.CLLong
  -> RIP.CLLong
  -> IO ()
hs_bindgen_6949b993d0bbcef8 =
  RIP.fromFFIType hs_bindgen_6949b993d0bbcef8_base

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
set_foo_64 = hs_bindgen_6949b993d0bbcef8

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_foo_64@
foreign import ccall unsafe "hs_bindgen_caeeb54830aeac7c" hs_bindgen_caeeb54830aeac7c_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int64
  -> RIP.Int64
  -> RIP.Int64
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_foo_64@
hs_bindgen_caeeb54830aeac7c ::
     RIP.Ptr Foo_64
  -> RIP.CLong
  -> RIP.CLLong
  -> RIP.CLLong
  -> RIP.CLLong
  -> IO RIP.CBool
hs_bindgen_caeeb54830aeac7c =
  RIP.fromFFIType hs_bindgen_caeeb54830aeac7c_base

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
eq_foo_64 = hs_bindgen_caeeb54830aeac7c

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_8_8@
foreign import ccall unsafe "hs_bindgen_d70f29d570a66d4d" hs_bindgen_d70f29d570a66d4d_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_8_8@
hs_bindgen_d70f29d570a66d4d ::
     RIP.Ptr Bar_8_8
  -> RIP.CSChar
  -> RIP.CInt
  -> IO ()
hs_bindgen_d70f29d570a66d4d =
  RIP.fromFFIType hs_bindgen_d70f29d570a66d4d_base

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
set_bar_8_8 = hs_bindgen_d70f29d570a66d4d

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_8_8@
foreign import ccall unsafe "hs_bindgen_49666188cc53a844" hs_bindgen_49666188cc53a844_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_8_8@
hs_bindgen_49666188cc53a844 ::
     RIP.Ptr Bar_8_8
  -> RIP.CSChar
  -> RIP.CInt
  -> IO RIP.CBool
hs_bindgen_49666188cc53a844 =
  RIP.fromFFIType hs_bindgen_49666188cc53a844_base

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
eq_bar_8_8 = hs_bindgen_49666188cc53a844

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_8_16@
foreign import ccall unsafe "hs_bindgen_4402e495ed80145b" hs_bindgen_4402e495ed80145b_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int32
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_8_16@
hs_bindgen_4402e495ed80145b ::
     RIP.Ptr Bar_8_16
  -> RIP.CInt
  -> RIP.CInt
  -> IO ()
hs_bindgen_4402e495ed80145b =
  RIP.fromFFIType hs_bindgen_4402e495ed80145b_base

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
set_bar_8_16 = hs_bindgen_4402e495ed80145b

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_8_16@
foreign import ccall unsafe "hs_bindgen_21018e5939f04a18" hs_bindgen_21018e5939f04a18_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int32
  -> RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_8_16@
hs_bindgen_21018e5939f04a18 ::
     RIP.Ptr Bar_8_16
  -> RIP.CInt
  -> RIP.CInt
  -> IO RIP.CBool
hs_bindgen_21018e5939f04a18 =
  RIP.fromFFIType hs_bindgen_21018e5939f04a18_base

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
eq_bar_8_16 = hs_bindgen_21018e5939f04a18

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_8_32@
foreign import ccall unsafe "hs_bindgen_b6b609ae99686c1a" hs_bindgen_b6b609ae99686c1a_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_8_32@
hs_bindgen_b6b609ae99686c1a ::
     RIP.Ptr Bar_8_32
  -> RIP.CLong
  -> RIP.CInt
  -> IO ()
hs_bindgen_b6b609ae99686c1a =
  RIP.fromFFIType hs_bindgen_b6b609ae99686c1a_base

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
set_bar_8_32 = hs_bindgen_b6b609ae99686c1a

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_8_32@
foreign import ccall unsafe "hs_bindgen_57a297fb054d1b61" hs_bindgen_57a297fb054d1b61_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_8_32@
hs_bindgen_57a297fb054d1b61 ::
     RIP.Ptr Bar_8_32
  -> RIP.CLong
  -> RIP.CInt
  -> IO RIP.CBool
hs_bindgen_57a297fb054d1b61 =
  RIP.fromFFIType hs_bindgen_57a297fb054d1b61_base

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
eq_bar_8_32 = hs_bindgen_57a297fb054d1b61

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_8_64@
foreign import ccall unsafe "hs_bindgen_5624ee6706340c28" hs_bindgen_5624ee6706340c28_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_8_64@
hs_bindgen_5624ee6706340c28 ::
     RIP.Ptr Bar_8_64
  -> RIP.CLLong
  -> RIP.CInt
  -> IO ()
hs_bindgen_5624ee6706340c28 =
  RIP.fromFFIType hs_bindgen_5624ee6706340c28_base

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
set_bar_8_64 = hs_bindgen_5624ee6706340c28

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_8_64@
foreign import ccall unsafe "hs_bindgen_57a006547646b355" hs_bindgen_57a006547646b355_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_8_64@
hs_bindgen_57a006547646b355 ::
     RIP.Ptr Bar_8_64
  -> RIP.CLLong
  -> RIP.CInt
  -> IO RIP.CBool
hs_bindgen_57a006547646b355 =
  RIP.fromFFIType hs_bindgen_57a006547646b355_base

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
eq_bar_8_64 = hs_bindgen_57a006547646b355

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_16_16@
foreign import ccall unsafe "hs_bindgen_908406015b2ae986" hs_bindgen_908406015b2ae986_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int32
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_16_16@
hs_bindgen_908406015b2ae986 ::
     RIP.Ptr Bar_16_16
  -> RIP.CInt
  -> RIP.CInt
  -> IO ()
hs_bindgen_908406015b2ae986 =
  RIP.fromFFIType hs_bindgen_908406015b2ae986_base

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
set_bar_16_16 = hs_bindgen_908406015b2ae986

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_16_16@
foreign import ccall unsafe "hs_bindgen_83f25fcbc4919d87" hs_bindgen_83f25fcbc4919d87_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int32
  -> RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_16_16@
hs_bindgen_83f25fcbc4919d87 ::
     RIP.Ptr Bar_16_16
  -> RIP.CInt
  -> RIP.CInt
  -> IO RIP.CBool
hs_bindgen_83f25fcbc4919d87 =
  RIP.fromFFIType hs_bindgen_83f25fcbc4919d87_base

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
eq_bar_16_16 = hs_bindgen_83f25fcbc4919d87

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_16_32@
foreign import ccall unsafe "hs_bindgen_2fc932c4505d95bc" hs_bindgen_2fc932c4505d95bc_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_16_32@
hs_bindgen_2fc932c4505d95bc ::
     RIP.Ptr Bar_16_32
  -> RIP.CLong
  -> RIP.CInt
  -> IO ()
hs_bindgen_2fc932c4505d95bc =
  RIP.fromFFIType hs_bindgen_2fc932c4505d95bc_base

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
set_bar_16_32 = hs_bindgen_2fc932c4505d95bc

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_16_32@
foreign import ccall unsafe "hs_bindgen_d07b7c7434009536" hs_bindgen_d07b7c7434009536_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_16_32@
hs_bindgen_d07b7c7434009536 ::
     RIP.Ptr Bar_16_32
  -> RIP.CLong
  -> RIP.CInt
  -> IO RIP.CBool
hs_bindgen_d07b7c7434009536 =
  RIP.fromFFIType hs_bindgen_d07b7c7434009536_base

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
eq_bar_16_32 = hs_bindgen_d07b7c7434009536

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_16_64@
foreign import ccall unsafe "hs_bindgen_aa9694a81fe70333" hs_bindgen_aa9694a81fe70333_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_16_64@
hs_bindgen_aa9694a81fe70333 ::
     RIP.Ptr Bar_16_64
  -> RIP.CLLong
  -> RIP.CInt
  -> IO ()
hs_bindgen_aa9694a81fe70333 =
  RIP.fromFFIType hs_bindgen_aa9694a81fe70333_base

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
set_bar_16_64 = hs_bindgen_aa9694a81fe70333

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_16_64@
foreign import ccall unsafe "hs_bindgen_775b431e7994bbb4" hs_bindgen_775b431e7994bbb4_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_16_64@
hs_bindgen_775b431e7994bbb4 ::
     RIP.Ptr Bar_16_64
  -> RIP.CLLong
  -> RIP.CInt
  -> IO RIP.CBool
hs_bindgen_775b431e7994bbb4 =
  RIP.fromFFIType hs_bindgen_775b431e7994bbb4_base

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
eq_bar_16_64 = hs_bindgen_775b431e7994bbb4

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_32_32@
foreign import ccall unsafe "hs_bindgen_c554619ce78113ba" hs_bindgen_c554619ce78113ba_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int64
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_32_32@
hs_bindgen_c554619ce78113ba ::
     RIP.Ptr Bar_32_32
  -> RIP.CLong
  -> RIP.CLong
  -> IO ()
hs_bindgen_c554619ce78113ba =
  RIP.fromFFIType hs_bindgen_c554619ce78113ba_base

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
set_bar_32_32 = hs_bindgen_c554619ce78113ba

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_32_32@
foreign import ccall unsafe "hs_bindgen_1821b4c2024c6db0" hs_bindgen_1821b4c2024c6db0_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int64
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_32_32@
hs_bindgen_1821b4c2024c6db0 ::
     RIP.Ptr Bar_32_32
  -> RIP.CLong
  -> RIP.CLong
  -> IO RIP.CBool
hs_bindgen_1821b4c2024c6db0 =
  RIP.fromFFIType hs_bindgen_1821b4c2024c6db0_base

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
eq_bar_32_32 = hs_bindgen_1821b4c2024c6db0

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_32_64@
foreign import ccall unsafe "hs_bindgen_adf958d470681c09" hs_bindgen_adf958d470681c09_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int64
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_32_64@
hs_bindgen_adf958d470681c09 ::
     RIP.Ptr Bar_32_64
  -> RIP.CLLong
  -> RIP.CLong
  -> IO ()
hs_bindgen_adf958d470681c09 =
  RIP.fromFFIType hs_bindgen_adf958d470681c09_base

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
set_bar_32_64 = hs_bindgen_adf958d470681c09

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_32_64@
foreign import ccall unsafe "hs_bindgen_6f9d2a4c9efe9bd1" hs_bindgen_6f9d2a4c9efe9bd1_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int64
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_32_64@
hs_bindgen_6f9d2a4c9efe9bd1 ::
     RIP.Ptr Bar_32_64
  -> RIP.CLLong
  -> RIP.CLong
  -> IO RIP.CBool
hs_bindgen_6f9d2a4c9efe9bd1 =
  RIP.fromFFIType hs_bindgen_6f9d2a4c9efe9bd1_base

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
eq_bar_32_64 = hs_bindgen_6f9d2a4c9efe9bd1

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_64_64@
foreign import ccall unsafe "hs_bindgen_d33dcf0f97e09802" hs_bindgen_d33dcf0f97e09802_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int64
  -> IO ()

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_set_bar_64_64@
hs_bindgen_d33dcf0f97e09802 ::
     RIP.Ptr Bar_64_64
  -> RIP.CLLong
  -> RIP.CLLong
  -> IO ()
hs_bindgen_d33dcf0f97e09802 =
  RIP.fromFFIType hs_bindgen_d33dcf0f97e09802_base

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
set_bar_64_64 = hs_bindgen_d33dcf0f97e09802

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_64_64@
foreign import ccall unsafe "hs_bindgen_49948f3caf9be86d" hs_bindgen_49948f3caf9be86d_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int64
  -> RIP.Int64
  -> IO RIP.Word8

-- __unique:__ @test_typesstructsbitfields_Example_Unsafe_eq_bar_64_64@
hs_bindgen_49948f3caf9be86d ::
     RIP.Ptr Bar_64_64
  -> RIP.CLLong
  -> RIP.CLLong
  -> IO RIP.CBool
hs_bindgen_49948f3caf9be86d =
  RIP.fromFFIType hs_bindgen_49948f3caf9be86d_base

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
eq_bar_64_64 = hs_bindgen_49948f3caf9be86d
