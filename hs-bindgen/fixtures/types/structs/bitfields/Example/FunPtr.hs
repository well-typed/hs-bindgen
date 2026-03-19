{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.set_foo_8
    , Example.FunPtr.eq_foo_8
    , Example.FunPtr.set_foo_16
    , Example.FunPtr.eq_foo_16
    , Example.FunPtr.set_foo_32
    , Example.FunPtr.eq_foo_32
    , Example.FunPtr.set_foo_64
    , Example.FunPtr.eq_foo_64
    , Example.FunPtr.set_bar_8_8
    , Example.FunPtr.eq_bar_8_8
    , Example.FunPtr.set_bar_8_16
    , Example.FunPtr.eq_bar_8_16
    , Example.FunPtr.set_bar_8_32
    , Example.FunPtr.eq_bar_8_32
    , Example.FunPtr.set_bar_8_64
    , Example.FunPtr.eq_bar_8_64
    , Example.FunPtr.set_bar_16_16
    , Example.FunPtr.eq_bar_16_16
    , Example.FunPtr.set_bar_16_32
    , Example.FunPtr.eq_bar_16_32
    , Example.FunPtr.set_bar_16_64
    , Example.FunPtr.eq_bar_16_64
    , Example.FunPtr.set_bar_32_32
    , Example.FunPtr.eq_bar_32_32
    , Example.FunPtr.set_bar_32_64
    , Example.FunPtr.eq_bar_32_64
    , Example.FunPtr.set_bar_64_64
    , Example.FunPtr.eq_bar_64_64
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/structs/bitfields.h>"
  , "/* test_typesstructsbitfields_Example_get_set_foo_8 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fe08388e1e6a6060 (void)) ("
  , "  struct foo_8 *arg1,"
  , "  signed char arg2,"
  , "  signed char arg3,"
  , "  signed char arg4,"
  , "  signed char arg5,"
  , "  signed char arg6,"
  , "  signed char arg7"
  , ")"
  , "{"
  , "  return &set_foo_8;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_eq_foo_8 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_04b0e4ecb5c875a0 (void)) ("
  , "  struct foo_8 *arg1,"
  , "  signed char arg2,"
  , "  signed char arg3,"
  , "  signed char arg4,"
  , "  signed char arg5,"
  , "  signed char arg6,"
  , "  signed char arg7"
  , ")"
  , "{"
  , "  return &eq_foo_8;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_set_foo_16 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1e3f334f1aeeab11 (void)) ("
  , "  struct foo_16 *arg1,"
  , "  signed char arg2,"
  , "  signed int arg3,"
  , "  signed int arg4,"
  , "  signed int arg5,"
  , "  signed int arg6,"
  , "  signed int arg7"
  , ")"
  , "{"
  , "  return &set_foo_16;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_eq_foo_16 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_d8b22963aebe92ee (void)) ("
  , "  struct foo_16 *arg1,"
  , "  signed char arg2,"
  , "  signed int arg3,"
  , "  signed int arg4,"
  , "  signed int arg5,"
  , "  signed int arg6,"
  , "  signed int arg7"
  , ")"
  , "{"
  , "  return &eq_foo_16;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_set_foo_32 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c5328b8746148226 (void)) ("
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
  , "  return &set_foo_32;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_eq_foo_32 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_bad5efd3edec907f (void)) ("
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
  , "  return &eq_foo_32;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_set_foo_64 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c5e0cb2fae30f205 (void)) ("
  , "  struct foo_64 *arg1,"
  , "  signed long arg2,"
  , "  signed long long arg3,"
  , "  signed long long arg4,"
  , "  signed long long arg5"
  , ")"
  , "{"
  , "  return &set_foo_64;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_eq_foo_64 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_bf4e4696a76108d2 (void)) ("
  , "  struct foo_64 *arg1,"
  , "  signed long arg2,"
  , "  signed long long arg3,"
  , "  signed long long arg4,"
  , "  signed long long arg5"
  , ")"
  , "{"
  , "  return &eq_foo_64;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_set_bar_8_8 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4a8efa809aae097a (void)) ("
  , "  struct bar_8_8 *arg1,"
  , "  signed char arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &set_bar_8_8;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_eq_bar_8_8 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_313c51982bd09adf (void)) ("
  , "  struct bar_8_8 *arg1,"
  , "  signed char arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &eq_bar_8_8;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_set_bar_8_16 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d1b8e074d464e182 (void)) ("
  , "  struct bar_8_16 *arg1,"
  , "  signed int arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &set_bar_8_16;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_eq_bar_8_16 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_21cee5868ce4a4fb (void)) ("
  , "  struct bar_8_16 *arg1,"
  , "  signed int arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &eq_bar_8_16;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_set_bar_8_32 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_02ba995a762444e4 (void)) ("
  , "  struct bar_8_32 *arg1,"
  , "  signed long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &set_bar_8_32;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_eq_bar_8_32 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_1cd770b7232b8faa (void)) ("
  , "  struct bar_8_32 *arg1,"
  , "  signed long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &eq_bar_8_32;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_set_bar_8_64 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a6f6a7482aead8e6 (void)) ("
  , "  struct bar_8_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &set_bar_8_64;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_eq_bar_8_64 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_f7c3866b01ace476 (void)) ("
  , "  struct bar_8_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &eq_bar_8_64;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_set_bar_16_16 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fa72ee2afb190952 (void)) ("
  , "  struct bar_16_16 *arg1,"
  , "  signed int arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &set_bar_16_16;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_eq_bar_16_16 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_e4686d88a051cf3d (void)) ("
  , "  struct bar_16_16 *arg1,"
  , "  signed int arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &eq_bar_16_16;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_set_bar_16_32 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cf87e9ae69537049 (void)) ("
  , "  struct bar_16_32 *arg1,"
  , "  signed long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &set_bar_16_32;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_eq_bar_16_32 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_73c7f2a41933abf6 (void)) ("
  , "  struct bar_16_32 *arg1,"
  , "  signed long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &eq_bar_16_32;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_set_bar_16_64 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8900d8bcf4c33175 (void)) ("
  , "  struct bar_16_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &set_bar_16_64;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_eq_bar_16_64 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_a03f3fa06237c9cf (void)) ("
  , "  struct bar_16_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &eq_bar_16_64;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_set_bar_32_32 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a7d8c6f4e3b7edab (void)) ("
  , "  struct bar_32_32 *arg1,"
  , "  signed long arg2,"
  , "  signed long arg3"
  , ")"
  , "{"
  , "  return &set_bar_32_32;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_eq_bar_32_32 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_519f008ef22c79b0 (void)) ("
  , "  struct bar_32_32 *arg1,"
  , "  signed long arg2,"
  , "  signed long arg3"
  , ")"
  , "{"
  , "  return &eq_bar_32_32;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_set_bar_32_64 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c8d0a404bdb3c4fe (void)) ("
  , "  struct bar_32_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed long arg3"
  , ")"
  , "{"
  , "  return &set_bar_32_64;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_eq_bar_32_64 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_5547e5356fb5659e (void)) ("
  , "  struct bar_32_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed long arg3"
  , ")"
  , "{"
  , "  return &eq_bar_32_64;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_set_bar_64_64 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6519f4fe17fd7395 (void)) ("
  , "  struct bar_64_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed long long arg3"
  , ")"
  , "{"
  , "  return &set_bar_64_64;"
  , "}"
  , "/* test_typesstructsbitfields_Example_get_eq_bar_64_64 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_9b1def459fcd8bd9 (void)) ("
  , "  struct bar_64_64 *arg1,"
  , "  signed long long arg2,"
  , "  signed long long arg3"
  , ")"
  , "{"
  , "  return &eq_bar_64_64;"
  , "}"
  ]))

-- __unique:__ @test_typesstructsbitfields_Example_get_set_foo_8@
foreign import ccall unsafe "hs_bindgen_fe08388e1e6a6060" hs_bindgen_fe08388e1e6a6060_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_set_foo_8@
hs_bindgen_fe08388e1e6a6060 :: IO (RIP.FunPtr ((RIP.Ptr Foo_8) -> RIP.CSChar -> RIP.CSChar -> RIP.CSChar -> RIP.CSChar -> RIP.CSChar -> RIP.CSChar -> IO ()))
hs_bindgen_fe08388e1e6a6060 =
  RIP.fromFFIType hs_bindgen_fe08388e1e6a6060_base

{-# NOINLINE set_foo_8 #-}
{-| __C declaration:__ @set_foo_8@

    __defined at:__ @types\/structs\/bitfields.h 34:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_foo_8 :: RIP.FunPtr ((RIP.Ptr Foo_8) -> RIP.CSChar -> RIP.CSChar -> RIP.CSChar -> RIP.CSChar -> RIP.CSChar -> RIP.CSChar -> IO ())
set_foo_8 =
  RIP.unsafePerformIO hs_bindgen_fe08388e1e6a6060

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_foo_8@
foreign import ccall unsafe "hs_bindgen_04b0e4ecb5c875a0" hs_bindgen_04b0e4ecb5c875a0_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_foo_8@
hs_bindgen_04b0e4ecb5c875a0 :: IO (RIP.FunPtr ((RIP.Ptr Foo_8) -> RIP.CSChar -> RIP.CSChar -> RIP.CSChar -> RIP.CSChar -> RIP.CSChar -> RIP.CSChar -> IO RIP.CBool))
hs_bindgen_04b0e4ecb5c875a0 =
  RIP.fromFFIType hs_bindgen_04b0e4ecb5c875a0_base

{-# NOINLINE eq_foo_8 #-}
{-| __C declaration:__ @eq_foo_8@

    __defined at:__ @types\/structs\/bitfields.h 50:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_foo_8 :: RIP.FunPtr ((RIP.Ptr Foo_8) -> RIP.CSChar -> RIP.CSChar -> RIP.CSChar -> RIP.CSChar -> RIP.CSChar -> RIP.CSChar -> IO RIP.CBool)
eq_foo_8 =
  RIP.unsafePerformIO hs_bindgen_04b0e4ecb5c875a0

-- __unique:__ @test_typesstructsbitfields_Example_get_set_foo_16@
foreign import ccall unsafe "hs_bindgen_1e3f334f1aeeab11" hs_bindgen_1e3f334f1aeeab11_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_set_foo_16@
hs_bindgen_1e3f334f1aeeab11 :: IO (RIP.FunPtr ((RIP.Ptr Foo_16) -> RIP.CSChar -> RIP.CInt -> RIP.CInt -> RIP.CInt -> RIP.CInt -> RIP.CInt -> IO ()))
hs_bindgen_1e3f334f1aeeab11 =
  RIP.fromFFIType hs_bindgen_1e3f334f1aeeab11_base

{-# NOINLINE set_foo_16 #-}
{-| __C declaration:__ @set_foo_16@

    __defined at:__ @types\/structs\/bitfields.h 79:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_foo_16 :: RIP.FunPtr ((RIP.Ptr Foo_16) -> RIP.CSChar -> RIP.CInt -> RIP.CInt -> RIP.CInt -> RIP.CInt -> RIP.CInt -> IO ())
set_foo_16 =
  RIP.unsafePerformIO hs_bindgen_1e3f334f1aeeab11

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_foo_16@
foreign import ccall unsafe "hs_bindgen_d8b22963aebe92ee" hs_bindgen_d8b22963aebe92ee_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_foo_16@
hs_bindgen_d8b22963aebe92ee :: IO (RIP.FunPtr ((RIP.Ptr Foo_16) -> RIP.CSChar -> RIP.CInt -> RIP.CInt -> RIP.CInt -> RIP.CInt -> RIP.CInt -> IO RIP.CBool))
hs_bindgen_d8b22963aebe92ee =
  RIP.fromFFIType hs_bindgen_d8b22963aebe92ee_base

{-# NOINLINE eq_foo_16 #-}
{-| __C declaration:__ @eq_foo_16@

    __defined at:__ @types\/structs\/bitfields.h 95:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_foo_16 :: RIP.FunPtr ((RIP.Ptr Foo_16) -> RIP.CSChar -> RIP.CInt -> RIP.CInt -> RIP.CInt -> RIP.CInt -> RIP.CInt -> IO RIP.CBool)
eq_foo_16 =
  RIP.unsafePerformIO hs_bindgen_d8b22963aebe92ee

-- __unique:__ @test_typesstructsbitfields_Example_get_set_foo_32@
foreign import ccall unsafe "hs_bindgen_c5328b8746148226" hs_bindgen_c5328b8746148226_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_set_foo_32@
hs_bindgen_c5328b8746148226 :: IO (RIP.FunPtr ((RIP.Ptr Foo_32) -> RIP.CSChar -> RIP.CInt -> RIP.CInt -> RIP.CInt -> RIP.CLong -> RIP.CInt -> RIP.CLong -> IO ()))
hs_bindgen_c5328b8746148226 =
  RIP.fromFFIType hs_bindgen_c5328b8746148226_base

{-# NOINLINE set_foo_32 #-}
{-| __C declaration:__ @set_foo_32@

    __defined at:__ @types\/structs\/bitfields.h 125:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_foo_32 :: RIP.FunPtr ((RIP.Ptr Foo_32) -> RIP.CSChar -> RIP.CInt -> RIP.CInt -> RIP.CInt -> RIP.CLong -> RIP.CInt -> RIP.CLong -> IO ())
set_foo_32 =
  RIP.unsafePerformIO hs_bindgen_c5328b8746148226

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_foo_32@
foreign import ccall unsafe "hs_bindgen_bad5efd3edec907f" hs_bindgen_bad5efd3edec907f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_foo_32@
hs_bindgen_bad5efd3edec907f :: IO (RIP.FunPtr ((RIP.Ptr Foo_32) -> RIP.CSChar -> RIP.CInt -> RIP.CInt -> RIP.CInt -> RIP.CLong -> RIP.CInt -> RIP.CLong -> IO RIP.CBool))
hs_bindgen_bad5efd3edec907f =
  RIP.fromFFIType hs_bindgen_bad5efd3edec907f_base

{-# NOINLINE eq_foo_32 #-}
{-| __C declaration:__ @eq_foo_32@

    __defined at:__ @types\/structs\/bitfields.h 143:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_foo_32 :: RIP.FunPtr ((RIP.Ptr Foo_32) -> RIP.CSChar -> RIP.CInt -> RIP.CInt -> RIP.CInt -> RIP.CLong -> RIP.CInt -> RIP.CLong -> IO RIP.CBool)
eq_foo_32 =
  RIP.unsafePerformIO hs_bindgen_bad5efd3edec907f

-- __unique:__ @test_typesstructsbitfields_Example_get_set_foo_64@
foreign import ccall unsafe "hs_bindgen_c5e0cb2fae30f205" hs_bindgen_c5e0cb2fae30f205_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_set_foo_64@
hs_bindgen_c5e0cb2fae30f205 :: IO (RIP.FunPtr ((RIP.Ptr Foo_64) -> RIP.CLong -> RIP.CLLong -> RIP.CLLong -> RIP.CLLong -> IO ()))
hs_bindgen_c5e0cb2fae30f205 =
  RIP.fromFFIType hs_bindgen_c5e0cb2fae30f205_base

{-# NOINLINE set_foo_64 #-}
{-| __C declaration:__ @set_foo_64@

    __defined at:__ @types\/structs\/bitfields.h 172:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_foo_64 :: RIP.FunPtr ((RIP.Ptr Foo_64) -> RIP.CLong -> RIP.CLLong -> RIP.CLLong -> RIP.CLLong -> IO ())
set_foo_64 =
  RIP.unsafePerformIO hs_bindgen_c5e0cb2fae30f205

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_foo_64@
foreign import ccall unsafe "hs_bindgen_bf4e4696a76108d2" hs_bindgen_bf4e4696a76108d2_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_foo_64@
hs_bindgen_bf4e4696a76108d2 :: IO (RIP.FunPtr ((RIP.Ptr Foo_64) -> RIP.CLong -> RIP.CLLong -> RIP.CLLong -> RIP.CLLong -> IO RIP.CBool))
hs_bindgen_bf4e4696a76108d2 =
  RIP.fromFFIType hs_bindgen_bf4e4696a76108d2_base

{-# NOINLINE eq_foo_64 #-}
{-| __C declaration:__ @eq_foo_64@

    __defined at:__ @types\/structs\/bitfields.h 184:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_foo_64 :: RIP.FunPtr ((RIP.Ptr Foo_64) -> RIP.CLong -> RIP.CLLong -> RIP.CLLong -> RIP.CLLong -> IO RIP.CBool)
eq_foo_64 =
  RIP.unsafePerformIO hs_bindgen_bf4e4696a76108d2

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_8_8@
foreign import ccall unsafe "hs_bindgen_4a8efa809aae097a" hs_bindgen_4a8efa809aae097a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_8_8@
hs_bindgen_4a8efa809aae097a :: IO (RIP.FunPtr ((RIP.Ptr Bar_8_8) -> RIP.CSChar -> RIP.CInt -> IO ()))
hs_bindgen_4a8efa809aae097a =
  RIP.fromFFIType hs_bindgen_4a8efa809aae097a_base

{-# NOINLINE set_bar_8_8 #-}
{-| __C declaration:__ @set_bar_8_8@

    __defined at:__ @types\/structs\/bitfields.h 205:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_8_8 :: RIP.FunPtr ((RIP.Ptr Bar_8_8) -> RIP.CSChar -> RIP.CInt -> IO ())
set_bar_8_8 =
  RIP.unsafePerformIO hs_bindgen_4a8efa809aae097a

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_8_8@
foreign import ccall unsafe "hs_bindgen_313c51982bd09adf" hs_bindgen_313c51982bd09adf_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_8_8@
hs_bindgen_313c51982bd09adf :: IO (RIP.FunPtr ((RIP.Ptr Bar_8_8) -> RIP.CSChar -> RIP.CInt -> IO RIP.CBool))
hs_bindgen_313c51982bd09adf =
  RIP.fromFFIType hs_bindgen_313c51982bd09adf_base

{-# NOINLINE eq_bar_8_8 #-}
{-| __C declaration:__ @eq_bar_8_8@

    __defined at:__ @types\/structs\/bitfields.h 213:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_8_8 :: RIP.FunPtr ((RIP.Ptr Bar_8_8) -> RIP.CSChar -> RIP.CInt -> IO RIP.CBool)
eq_bar_8_8 =
  RIP.unsafePerformIO hs_bindgen_313c51982bd09adf

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_8_16@
foreign import ccall unsafe "hs_bindgen_d1b8e074d464e182" hs_bindgen_d1b8e074d464e182_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_8_16@
hs_bindgen_d1b8e074d464e182 :: IO (RIP.FunPtr ((RIP.Ptr Bar_8_16) -> RIP.CInt -> RIP.CInt -> IO ()))
hs_bindgen_d1b8e074d464e182 =
  RIP.fromFFIType hs_bindgen_d1b8e074d464e182_base

{-# NOINLINE set_bar_8_16 #-}
{-| __C declaration:__ @set_bar_8_16@

    __defined at:__ @types\/structs\/bitfields.h 230:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_8_16 :: RIP.FunPtr ((RIP.Ptr Bar_8_16) -> RIP.CInt -> RIP.CInt -> IO ())
set_bar_8_16 =
  RIP.unsafePerformIO hs_bindgen_d1b8e074d464e182

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_8_16@
foreign import ccall unsafe "hs_bindgen_21cee5868ce4a4fb" hs_bindgen_21cee5868ce4a4fb_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_8_16@
hs_bindgen_21cee5868ce4a4fb :: IO (RIP.FunPtr ((RIP.Ptr Bar_8_16) -> RIP.CInt -> RIP.CInt -> IO RIP.CBool))
hs_bindgen_21cee5868ce4a4fb =
  RIP.fromFFIType hs_bindgen_21cee5868ce4a4fb_base

{-# NOINLINE eq_bar_8_16 #-}
{-| __C declaration:__ @eq_bar_8_16@

    __defined at:__ @types\/structs\/bitfields.h 238:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_8_16 :: RIP.FunPtr ((RIP.Ptr Bar_8_16) -> RIP.CInt -> RIP.CInt -> IO RIP.CBool)
eq_bar_8_16 =
  RIP.unsafePerformIO hs_bindgen_21cee5868ce4a4fb

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_8_32@
foreign import ccall unsafe "hs_bindgen_02ba995a762444e4" hs_bindgen_02ba995a762444e4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_8_32@
hs_bindgen_02ba995a762444e4 :: IO (RIP.FunPtr ((RIP.Ptr Bar_8_32) -> RIP.CLong -> RIP.CInt -> IO ()))
hs_bindgen_02ba995a762444e4 =
  RIP.fromFFIType hs_bindgen_02ba995a762444e4_base

{-# NOINLINE set_bar_8_32 #-}
{-| __C declaration:__ @set_bar_8_32@

    __defined at:__ @types\/structs\/bitfields.h 255:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_8_32 :: RIP.FunPtr ((RIP.Ptr Bar_8_32) -> RIP.CLong -> RIP.CInt -> IO ())
set_bar_8_32 =
  RIP.unsafePerformIO hs_bindgen_02ba995a762444e4

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_8_32@
foreign import ccall unsafe "hs_bindgen_1cd770b7232b8faa" hs_bindgen_1cd770b7232b8faa_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_8_32@
hs_bindgen_1cd770b7232b8faa :: IO (RIP.FunPtr ((RIP.Ptr Bar_8_32) -> RIP.CLong -> RIP.CInt -> IO RIP.CBool))
hs_bindgen_1cd770b7232b8faa =
  RIP.fromFFIType hs_bindgen_1cd770b7232b8faa_base

{-# NOINLINE eq_bar_8_32 #-}
{-| __C declaration:__ @eq_bar_8_32@

    __defined at:__ @types\/structs\/bitfields.h 263:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_8_32 :: RIP.FunPtr ((RIP.Ptr Bar_8_32) -> RIP.CLong -> RIP.CInt -> IO RIP.CBool)
eq_bar_8_32 =
  RIP.unsafePerformIO hs_bindgen_1cd770b7232b8faa

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_8_64@
foreign import ccall unsafe "hs_bindgen_a6f6a7482aead8e6" hs_bindgen_a6f6a7482aead8e6_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_8_64@
hs_bindgen_a6f6a7482aead8e6 :: IO (RIP.FunPtr ((RIP.Ptr Bar_8_64) -> RIP.CLLong -> RIP.CInt -> IO ()))
hs_bindgen_a6f6a7482aead8e6 =
  RIP.fromFFIType hs_bindgen_a6f6a7482aead8e6_base

{-# NOINLINE set_bar_8_64 #-}
{-| __C declaration:__ @set_bar_8_64@

    __defined at:__ @types\/structs\/bitfields.h 280:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_8_64 :: RIP.FunPtr ((RIP.Ptr Bar_8_64) -> RIP.CLLong -> RIP.CInt -> IO ())
set_bar_8_64 =
  RIP.unsafePerformIO hs_bindgen_a6f6a7482aead8e6

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_8_64@
foreign import ccall unsafe "hs_bindgen_f7c3866b01ace476" hs_bindgen_f7c3866b01ace476_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_8_64@
hs_bindgen_f7c3866b01ace476 :: IO (RIP.FunPtr ((RIP.Ptr Bar_8_64) -> RIP.CLLong -> RIP.CInt -> IO RIP.CBool))
hs_bindgen_f7c3866b01ace476 =
  RIP.fromFFIType hs_bindgen_f7c3866b01ace476_base

{-# NOINLINE eq_bar_8_64 #-}
{-| __C declaration:__ @eq_bar_8_64@

    __defined at:__ @types\/structs\/bitfields.h 288:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_8_64 :: RIP.FunPtr ((RIP.Ptr Bar_8_64) -> RIP.CLLong -> RIP.CInt -> IO RIP.CBool)
eq_bar_8_64 =
  RIP.unsafePerformIO hs_bindgen_f7c3866b01ace476

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_16_16@
foreign import ccall unsafe "hs_bindgen_fa72ee2afb190952" hs_bindgen_fa72ee2afb190952_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_16_16@
hs_bindgen_fa72ee2afb190952 :: IO (RIP.FunPtr ((RIP.Ptr Bar_16_16) -> RIP.CInt -> RIP.CInt -> IO ()))
hs_bindgen_fa72ee2afb190952 =
  RIP.fromFFIType hs_bindgen_fa72ee2afb190952_base

{-# NOINLINE set_bar_16_16 #-}
{-| __C declaration:__ @set_bar_16_16@

    __defined at:__ @types\/structs\/bitfields.h 305:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_16_16 :: RIP.FunPtr ((RIP.Ptr Bar_16_16) -> RIP.CInt -> RIP.CInt -> IO ())
set_bar_16_16 =
  RIP.unsafePerformIO hs_bindgen_fa72ee2afb190952

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_16_16@
foreign import ccall unsafe "hs_bindgen_e4686d88a051cf3d" hs_bindgen_e4686d88a051cf3d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_16_16@
hs_bindgen_e4686d88a051cf3d :: IO (RIP.FunPtr ((RIP.Ptr Bar_16_16) -> RIP.CInt -> RIP.CInt -> IO RIP.CBool))
hs_bindgen_e4686d88a051cf3d =
  RIP.fromFFIType hs_bindgen_e4686d88a051cf3d_base

{-# NOINLINE eq_bar_16_16 #-}
{-| __C declaration:__ @eq_bar_16_16@

    __defined at:__ @types\/structs\/bitfields.h 313:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_16_16 :: RIP.FunPtr ((RIP.Ptr Bar_16_16) -> RIP.CInt -> RIP.CInt -> IO RIP.CBool)
eq_bar_16_16 =
  RIP.unsafePerformIO hs_bindgen_e4686d88a051cf3d

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_16_32@
foreign import ccall unsafe "hs_bindgen_cf87e9ae69537049" hs_bindgen_cf87e9ae69537049_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_16_32@
hs_bindgen_cf87e9ae69537049 :: IO (RIP.FunPtr ((RIP.Ptr Bar_16_32) -> RIP.CLong -> RIP.CInt -> IO ()))
hs_bindgen_cf87e9ae69537049 =
  RIP.fromFFIType hs_bindgen_cf87e9ae69537049_base

{-# NOINLINE set_bar_16_32 #-}
{-| __C declaration:__ @set_bar_16_32@

    __defined at:__ @types\/structs\/bitfields.h 330:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_16_32 :: RIP.FunPtr ((RIP.Ptr Bar_16_32) -> RIP.CLong -> RIP.CInt -> IO ())
set_bar_16_32 =
  RIP.unsafePerformIO hs_bindgen_cf87e9ae69537049

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_16_32@
foreign import ccall unsafe "hs_bindgen_73c7f2a41933abf6" hs_bindgen_73c7f2a41933abf6_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_16_32@
hs_bindgen_73c7f2a41933abf6 :: IO (RIP.FunPtr ((RIP.Ptr Bar_16_32) -> RIP.CLong -> RIP.CInt -> IO RIP.CBool))
hs_bindgen_73c7f2a41933abf6 =
  RIP.fromFFIType hs_bindgen_73c7f2a41933abf6_base

{-# NOINLINE eq_bar_16_32 #-}
{-| __C declaration:__ @eq_bar_16_32@

    __defined at:__ @types\/structs\/bitfields.h 338:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_16_32 :: RIP.FunPtr ((RIP.Ptr Bar_16_32) -> RIP.CLong -> RIP.CInt -> IO RIP.CBool)
eq_bar_16_32 =
  RIP.unsafePerformIO hs_bindgen_73c7f2a41933abf6

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_16_64@
foreign import ccall unsafe "hs_bindgen_8900d8bcf4c33175" hs_bindgen_8900d8bcf4c33175_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_16_64@
hs_bindgen_8900d8bcf4c33175 :: IO (RIP.FunPtr ((RIP.Ptr Bar_16_64) -> RIP.CLLong -> RIP.CInt -> IO ()))
hs_bindgen_8900d8bcf4c33175 =
  RIP.fromFFIType hs_bindgen_8900d8bcf4c33175_base

{-# NOINLINE set_bar_16_64 #-}
{-| __C declaration:__ @set_bar_16_64@

    __defined at:__ @types\/structs\/bitfields.h 355:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_16_64 :: RIP.FunPtr ((RIP.Ptr Bar_16_64) -> RIP.CLLong -> RIP.CInt -> IO ())
set_bar_16_64 =
  RIP.unsafePerformIO hs_bindgen_8900d8bcf4c33175

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_16_64@
foreign import ccall unsafe "hs_bindgen_a03f3fa06237c9cf" hs_bindgen_a03f3fa06237c9cf_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_16_64@
hs_bindgen_a03f3fa06237c9cf :: IO (RIP.FunPtr ((RIP.Ptr Bar_16_64) -> RIP.CLLong -> RIP.CInt -> IO RIP.CBool))
hs_bindgen_a03f3fa06237c9cf =
  RIP.fromFFIType hs_bindgen_a03f3fa06237c9cf_base

{-# NOINLINE eq_bar_16_64 #-}
{-| __C declaration:__ @eq_bar_16_64@

    __defined at:__ @types\/structs\/bitfields.h 363:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_16_64 :: RIP.FunPtr ((RIP.Ptr Bar_16_64) -> RIP.CLLong -> RIP.CInt -> IO RIP.CBool)
eq_bar_16_64 =
  RIP.unsafePerformIO hs_bindgen_a03f3fa06237c9cf

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_32_32@
foreign import ccall unsafe "hs_bindgen_a7d8c6f4e3b7edab" hs_bindgen_a7d8c6f4e3b7edab_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_32_32@
hs_bindgen_a7d8c6f4e3b7edab :: IO (RIP.FunPtr ((RIP.Ptr Bar_32_32) -> RIP.CLong -> RIP.CLong -> IO ()))
hs_bindgen_a7d8c6f4e3b7edab =
  RIP.fromFFIType hs_bindgen_a7d8c6f4e3b7edab_base

{-# NOINLINE set_bar_32_32 #-}
{-| __C declaration:__ @set_bar_32_32@

    __defined at:__ @types\/structs\/bitfields.h 380:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_32_32 :: RIP.FunPtr ((RIP.Ptr Bar_32_32) -> RIP.CLong -> RIP.CLong -> IO ())
set_bar_32_32 =
  RIP.unsafePerformIO hs_bindgen_a7d8c6f4e3b7edab

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_32_32@
foreign import ccall unsafe "hs_bindgen_519f008ef22c79b0" hs_bindgen_519f008ef22c79b0_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_32_32@
hs_bindgen_519f008ef22c79b0 :: IO (RIP.FunPtr ((RIP.Ptr Bar_32_32) -> RIP.CLong -> RIP.CLong -> IO RIP.CBool))
hs_bindgen_519f008ef22c79b0 =
  RIP.fromFFIType hs_bindgen_519f008ef22c79b0_base

{-# NOINLINE eq_bar_32_32 #-}
{-| __C declaration:__ @eq_bar_32_32@

    __defined at:__ @types\/structs\/bitfields.h 388:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_32_32 :: RIP.FunPtr ((RIP.Ptr Bar_32_32) -> RIP.CLong -> RIP.CLong -> IO RIP.CBool)
eq_bar_32_32 =
  RIP.unsafePerformIO hs_bindgen_519f008ef22c79b0

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_32_64@
foreign import ccall unsafe "hs_bindgen_c8d0a404bdb3c4fe" hs_bindgen_c8d0a404bdb3c4fe_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_32_64@
hs_bindgen_c8d0a404bdb3c4fe :: IO (RIP.FunPtr ((RIP.Ptr Bar_32_64) -> RIP.CLLong -> RIP.CLong -> IO ()))
hs_bindgen_c8d0a404bdb3c4fe =
  RIP.fromFFIType hs_bindgen_c8d0a404bdb3c4fe_base

{-# NOINLINE set_bar_32_64 #-}
{-| __C declaration:__ @set_bar_32_64@

    __defined at:__ @types\/structs\/bitfields.h 405:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_32_64 :: RIP.FunPtr ((RIP.Ptr Bar_32_64) -> RIP.CLLong -> RIP.CLong -> IO ())
set_bar_32_64 =
  RIP.unsafePerformIO hs_bindgen_c8d0a404bdb3c4fe

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_32_64@
foreign import ccall unsafe "hs_bindgen_5547e5356fb5659e" hs_bindgen_5547e5356fb5659e_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_32_64@
hs_bindgen_5547e5356fb5659e :: IO (RIP.FunPtr ((RIP.Ptr Bar_32_64) -> RIP.CLLong -> RIP.CLong -> IO RIP.CBool))
hs_bindgen_5547e5356fb5659e =
  RIP.fromFFIType hs_bindgen_5547e5356fb5659e_base

{-# NOINLINE eq_bar_32_64 #-}
{-| __C declaration:__ @eq_bar_32_64@

    __defined at:__ @types\/structs\/bitfields.h 413:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_32_64 :: RIP.FunPtr ((RIP.Ptr Bar_32_64) -> RIP.CLLong -> RIP.CLong -> IO RIP.CBool)
eq_bar_32_64 =
  RIP.unsafePerformIO hs_bindgen_5547e5356fb5659e

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_64_64@
foreign import ccall unsafe "hs_bindgen_6519f4fe17fd7395" hs_bindgen_6519f4fe17fd7395_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_set_bar_64_64@
hs_bindgen_6519f4fe17fd7395 :: IO (RIP.FunPtr ((RIP.Ptr Bar_64_64) -> RIP.CLLong -> RIP.CLLong -> IO ()))
hs_bindgen_6519f4fe17fd7395 =
  RIP.fromFFIType hs_bindgen_6519f4fe17fd7395_base

{-# NOINLINE set_bar_64_64 #-}
{-| __C declaration:__ @set_bar_64_64@

    __defined at:__ @types\/structs\/bitfields.h 430:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
set_bar_64_64 :: RIP.FunPtr ((RIP.Ptr Bar_64_64) -> RIP.CLLong -> RIP.CLLong -> IO ())
set_bar_64_64 =
  RIP.unsafePerformIO hs_bindgen_6519f4fe17fd7395

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_64_64@
foreign import ccall unsafe "hs_bindgen_9b1def459fcd8bd9" hs_bindgen_9b1def459fcd8bd9_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsbitfields_Example_get_eq_bar_64_64@
hs_bindgen_9b1def459fcd8bd9 :: IO (RIP.FunPtr ((RIP.Ptr Bar_64_64) -> RIP.CLLong -> RIP.CLLong -> IO RIP.CBool))
hs_bindgen_9b1def459fcd8bd9 =
  RIP.fromFFIType hs_bindgen_9b1def459fcd8bd9_base

{-# NOINLINE eq_bar_64_64 #-}
{-| __C declaration:__ @eq_bar_64_64@

    __defined at:__ @types\/structs\/bitfields.h 438:20@

    __exported by:__ @types\/structs\/bitfields.h@
-}
eq_bar_64_64 :: RIP.FunPtr ((RIP.Ptr Bar_64_64) -> RIP.CLLong -> RIP.CLLong -> IO RIP.CBool)
eq_bar_64_64 =
  RIP.unsafePerformIO hs_bindgen_9b1def459fcd8bd9
