{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.LibC
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/standard_library_external_binding_specs.h>"
  , "/* test_bindingspecsstandard_library_Example_get_bool_fun */"
  , "__attribute__ ((const))"
  , "bool (*hs_bindgen_75adf67a8f322942 (void)) (void)"
  , "{"
  , "  return &bool_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_int8_t_fun */"
  , "__attribute__ ((const))"
  , "int8_t (*hs_bindgen_54ffea42129c5242 (void)) (void)"
  , "{"
  , "  return &int8_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_int16_t_fun */"
  , "__attribute__ ((const))"
  , "int16_t (*hs_bindgen_cd3bcd979ae9c2ad (void)) (void)"
  , "{"
  , "  return &int16_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_int32_t_fun */"
  , "__attribute__ ((const))"
  , "int32_t (*hs_bindgen_9e03518d6a75792c (void)) (void)"
  , "{"
  , "  return &int32_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_int64_t_fun */"
  , "__attribute__ ((const))"
  , "int64_t (*hs_bindgen_ee35e85337e626fe (void)) (void)"
  , "{"
  , "  return &int64_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_uint8_t_fun */"
  , "__attribute__ ((const))"
  , "uint8_t (*hs_bindgen_2858475d9a83d229 (void)) (void)"
  , "{"
  , "  return &uint8_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_uint16_t_fun */"
  , "__attribute__ ((const))"
  , "uint16_t (*hs_bindgen_e4e6365bac70370e (void)) (void)"
  , "{"
  , "  return &uint16_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_uint32_t_fun */"
  , "__attribute__ ((const))"
  , "uint32_t (*hs_bindgen_31b670863658629c (void)) (void)"
  , "{"
  , "  return &uint32_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_uint64_t_fun */"
  , "__attribute__ ((const))"
  , "uint64_t (*hs_bindgen_7bff4397ac5b0a84 (void)) (void)"
  , "{"
  , "  return &uint64_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_int_least8_t_fun */"
  , "__attribute__ ((const))"
  , "int_least8_t (*hs_bindgen_204ba2aa5f40c5fa (void)) (void)"
  , "{"
  , "  return &int_least8_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_int_least16_t_fun */"
  , "__attribute__ ((const))"
  , "int_least16_t (*hs_bindgen_9754ff4ef5c8d79c (void)) (void)"
  , "{"
  , "  return &int_least16_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_int_least32_t_fun */"
  , "__attribute__ ((const))"
  , "int_least32_t (*hs_bindgen_ea21f76cf0f4788b (void)) (void)"
  , "{"
  , "  return &int_least32_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_int_least64_t_fun */"
  , "__attribute__ ((const))"
  , "int_least64_t (*hs_bindgen_81e7393ac6db4b83 (void)) (void)"
  , "{"
  , "  return &int_least64_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_uint_least8_t_fun */"
  , "__attribute__ ((const))"
  , "uint_least8_t (*hs_bindgen_f3b54e247fddd580 (void)) (void)"
  , "{"
  , "  return &uint_least8_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_uint_least16_t_fun */"
  , "__attribute__ ((const))"
  , "uint_least16_t (*hs_bindgen_e432f4a44b5c00fb (void)) (void)"
  , "{"
  , "  return &uint_least16_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_uint_least32_t_fun */"
  , "__attribute__ ((const))"
  , "uint_least32_t (*hs_bindgen_4731341b16926be1 (void)) (void)"
  , "{"
  , "  return &uint_least32_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_uint_least64_t_fun */"
  , "__attribute__ ((const))"
  , "uint_least64_t (*hs_bindgen_a796d3cd88467c40 (void)) (void)"
  , "{"
  , "  return &uint_least64_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_int_fast8_t_fun */"
  , "__attribute__ ((const))"
  , "int_fast8_t (*hs_bindgen_b9d35460329d6b4d (void)) (void)"
  , "{"
  , "  return &int_fast8_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_int_fast32_t_fun */"
  , "__attribute__ ((const))"
  , "int_fast32_t (*hs_bindgen_ac3c0ef78b4b880b (void)) (void)"
  , "{"
  , "  return &int_fast32_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_int_fast64_t_fun */"
  , "__attribute__ ((const))"
  , "int_fast64_t (*hs_bindgen_051b82f2be6fb5cf (void)) (void)"
  , "{"
  , "  return &int_fast64_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_uint_fast8_t_fun */"
  , "__attribute__ ((const))"
  , "uint_fast8_t (*hs_bindgen_8deac59faf09717a (void)) (void)"
  , "{"
  , "  return &uint_fast8_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_uint_fast32_t_fun */"
  , "__attribute__ ((const))"
  , "uint_fast32_t (*hs_bindgen_d2ec431c9ba1e735 (void)) (void)"
  , "{"
  , "  return &uint_fast32_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_uint_fast64_t_fun */"
  , "__attribute__ ((const))"
  , "uint_fast64_t (*hs_bindgen_45a6a0d1f2cdf473 (void)) (void)"
  , "{"
  , "  return &uint_fast64_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_intmax_t_fun */"
  , "__attribute__ ((const))"
  , "intmax_t (*hs_bindgen_356803d6f1ee9998 (void)) (void)"
  , "{"
  , "  return &intmax_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_uintmax_t_fun */"
  , "__attribute__ ((const))"
  , "uintmax_t (*hs_bindgen_2536b40423863c3d (void)) (void)"
  , "{"
  , "  return &uintmax_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_intptr_t_fun */"
  , "__attribute__ ((const))"
  , "intptr_t (*hs_bindgen_7588b9aeb45c8721 (void)) (void)"
  , "{"
  , "  return &intptr_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_uintptr_t_fun */"
  , "__attribute__ ((const))"
  , "uintptr_t (*hs_bindgen_7edd04feb0afc86a (void)) (void)"
  , "{"
  , "  return &uintptr_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_size_t_fun */"
  , "__attribute__ ((const))"
  , "size_t (*hs_bindgen_dbcbc3d798d90475 (void)) (void)"
  , "{"
  , "  return &size_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_ptrdiff_t_fun */"
  , "__attribute__ ((const))"
  , "ptrdiff_t (*hs_bindgen_caab6bc707229fac (void)) (void)"
  , "{"
  , "  return &ptrdiff_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_wchar_t_fun */"
  , "__attribute__ ((const))"
  , "wchar_t (*hs_bindgen_dccb9712296f4b74 (void)) (void)"
  , "{"
  , "  return &wchar_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_wint_t_fun */"
  , "__attribute__ ((const))"
  , "wint_t (*hs_bindgen_d8cb689ef8e61b17 (void)) (void)"
  , "{"
  , "  return &wint_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_wctrans_t_fun */"
  , "__attribute__ ((const))"
  , "wctrans_t (*hs_bindgen_28185bca762ef301 (void)) (void)"
  , "{"
  , "  return &wctrans_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_wctype_t_fun */"
  , "__attribute__ ((const))"
  , "wctype_t (*hs_bindgen_5fd9a8b4db6b244b (void)) (void)"
  , "{"
  , "  return &wctype_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_char16_t_fun */"
  , "__attribute__ ((const))"
  , "char16_t (*hs_bindgen_4cb6b9c15fb1faa4 (void)) (void)"
  , "{"
  , "  return &char16_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_char32_t_fun */"
  , "__attribute__ ((const))"
  , "char32_t (*hs_bindgen_2027a27340ed8e9a (void)) (void)"
  , "{"
  , "  return &char32_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_time_t_fun */"
  , "__attribute__ ((const))"
  , "time_t (*hs_bindgen_c7ee4c8b957eea48 (void)) (void)"
  , "{"
  , "  return &time_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_clock_t_fun */"
  , "__attribute__ ((const))"
  , "clock_t (*hs_bindgen_e0560b6cdb0474fc (void)) (void)"
  , "{"
  , "  return &clock_t_fun;"
  , "}"
  , "/* test_bindingspecsstandard_library_Example_get_sig_atomic_t_fun */"
  , "__attribute__ ((const))"
  , "sig_atomic_t (*hs_bindgen_2d7ac7038960087a (void)) (void)"
  , "{"
  , "  return &sig_atomic_t_fun;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsstandard_library_Example_get_bool_fun@
foreign import ccall unsafe "hs_bindgen_75adf67a8f322942" hs_bindgen_75adf67a8f322942_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_bool_fun@
hs_bindgen_75adf67a8f322942 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CBool))
hs_bindgen_75adf67a8f322942 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_75adf67a8f322942_base

{-# NOINLINE bool_fun #-}
{-| __C declaration:__ @bool_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 7:6@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
bool_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CBool)
bool_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_75adf67a8f322942

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int8_t_fun@
foreign import ccall unsafe "hs_bindgen_54ffea42129c5242" hs_bindgen_54ffea42129c5242_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int8_t_fun@
hs_bindgen_54ffea42129c5242 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int8))
hs_bindgen_54ffea42129c5242 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_54ffea42129c5242_base

{-# NOINLINE int8_t_fun #-}
{-| __C declaration:__ @int8_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 11:8@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int8_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int8)
int8_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_54ffea42129c5242

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int16_t_fun@
foreign import ccall unsafe "hs_bindgen_cd3bcd979ae9c2ad" hs_bindgen_cd3bcd979ae9c2ad_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int16_t_fun@
hs_bindgen_cd3bcd979ae9c2ad :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int16))
hs_bindgen_cd3bcd979ae9c2ad =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_cd3bcd979ae9c2ad_base

{-# NOINLINE int16_t_fun #-}
{-| __C declaration:__ @int16_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 12:9@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int16_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int16)
int16_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cd3bcd979ae9c2ad

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int32_t_fun@
foreign import ccall unsafe "hs_bindgen_9e03518d6a75792c" hs_bindgen_9e03518d6a75792c_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int32_t_fun@
hs_bindgen_9e03518d6a75792c :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int32))
hs_bindgen_9e03518d6a75792c =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_9e03518d6a75792c_base

{-# NOINLINE int32_t_fun #-}
{-| __C declaration:__ @int32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 13:9@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int32_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int32)
int32_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9e03518d6a75792c

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int64_t_fun@
foreign import ccall unsafe "hs_bindgen_ee35e85337e626fe" hs_bindgen_ee35e85337e626fe_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int64_t_fun@
hs_bindgen_ee35e85337e626fe :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int64))
hs_bindgen_ee35e85337e626fe =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_ee35e85337e626fe_base

{-# NOINLINE int64_t_fun #-}
{-| __C declaration:__ @int64_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 14:9@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int64_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int64)
int64_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ee35e85337e626fe

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint8_t_fun@
foreign import ccall unsafe "hs_bindgen_2858475d9a83d229" hs_bindgen_2858475d9a83d229_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint8_t_fun@
hs_bindgen_2858475d9a83d229 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word8))
hs_bindgen_2858475d9a83d229 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_2858475d9a83d229_base

{-# NOINLINE uint8_t_fun #-}
{-| __C declaration:__ @uint8_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 15:9@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint8_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word8)
uint8_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2858475d9a83d229

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint16_t_fun@
foreign import ccall unsafe "hs_bindgen_e4e6365bac70370e" hs_bindgen_e4e6365bac70370e_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint16_t_fun@
hs_bindgen_e4e6365bac70370e :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word16))
hs_bindgen_e4e6365bac70370e =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_e4e6365bac70370e_base

{-# NOINLINE uint16_t_fun #-}
{-| __C declaration:__ @uint16_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 16:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint16_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word16)
uint16_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e4e6365bac70370e

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint32_t_fun@
foreign import ccall unsafe "hs_bindgen_31b670863658629c" hs_bindgen_31b670863658629c_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint32_t_fun@
hs_bindgen_31b670863658629c :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word32))
hs_bindgen_31b670863658629c =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_31b670863658629c_base

{-# NOINLINE uint32_t_fun #-}
{-| __C declaration:__ @uint32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 17:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint32_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word32)
uint32_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_31b670863658629c

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint64_t_fun@
foreign import ccall unsafe "hs_bindgen_7bff4397ac5b0a84" hs_bindgen_7bff4397ac5b0a84_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint64_t_fun@
hs_bindgen_7bff4397ac5b0a84 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word64))
hs_bindgen_7bff4397ac5b0a84 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_7bff4397ac5b0a84_base

{-# NOINLINE uint64_t_fun #-}
{-| __C declaration:__ @uint64_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 18:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint64_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word64)
uint64_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7bff4397ac5b0a84

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int_least8_t_fun@
foreign import ccall unsafe "hs_bindgen_204ba2aa5f40c5fa" hs_bindgen_204ba2aa5f40c5fa_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int_least8_t_fun@
hs_bindgen_204ba2aa5f40c5fa :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int8))
hs_bindgen_204ba2aa5f40c5fa =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_204ba2aa5f40c5fa_base

{-# NOINLINE int_least8_t_fun #-}
{-| __C declaration:__ @int_least8_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 19:14@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_least8_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int8)
int_least8_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_204ba2aa5f40c5fa

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int_least16_t_fun@
foreign import ccall unsafe "hs_bindgen_9754ff4ef5c8d79c" hs_bindgen_9754ff4ef5c8d79c_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int_least16_t_fun@
hs_bindgen_9754ff4ef5c8d79c :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int16))
hs_bindgen_9754ff4ef5c8d79c =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_9754ff4ef5c8d79c_base

{-# NOINLINE int_least16_t_fun #-}
{-| __C declaration:__ @int_least16_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 20:15@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_least16_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int16)
int_least16_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9754ff4ef5c8d79c

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int_least32_t_fun@
foreign import ccall unsafe "hs_bindgen_ea21f76cf0f4788b" hs_bindgen_ea21f76cf0f4788b_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int_least32_t_fun@
hs_bindgen_ea21f76cf0f4788b :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int32))
hs_bindgen_ea21f76cf0f4788b =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_ea21f76cf0f4788b_base

{-# NOINLINE int_least32_t_fun #-}
{-| __C declaration:__ @int_least32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 21:15@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_least32_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int32)
int_least32_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ea21f76cf0f4788b

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int_least64_t_fun@
foreign import ccall unsafe "hs_bindgen_81e7393ac6db4b83" hs_bindgen_81e7393ac6db4b83_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int_least64_t_fun@
hs_bindgen_81e7393ac6db4b83 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int64))
hs_bindgen_81e7393ac6db4b83 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_81e7393ac6db4b83_base

{-# NOINLINE int_least64_t_fun #-}
{-| __C declaration:__ @int_least64_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 22:15@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_least64_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int64)
int_least64_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_81e7393ac6db4b83

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint_least8_t_fun@
foreign import ccall unsafe "hs_bindgen_f3b54e247fddd580" hs_bindgen_f3b54e247fddd580_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint_least8_t_fun@
hs_bindgen_f3b54e247fddd580 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word8))
hs_bindgen_f3b54e247fddd580 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_f3b54e247fddd580_base

{-# NOINLINE uint_least8_t_fun #-}
{-| __C declaration:__ @uint_least8_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 23:15@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_least8_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word8)
uint_least8_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f3b54e247fddd580

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint_least16_t_fun@
foreign import ccall unsafe "hs_bindgen_e432f4a44b5c00fb" hs_bindgen_e432f4a44b5c00fb_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint_least16_t_fun@
hs_bindgen_e432f4a44b5c00fb :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word16))
hs_bindgen_e432f4a44b5c00fb =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_e432f4a44b5c00fb_base

{-# NOINLINE uint_least16_t_fun #-}
{-| __C declaration:__ @uint_least16_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 24:16@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_least16_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word16)
uint_least16_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e432f4a44b5c00fb

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint_least32_t_fun@
foreign import ccall unsafe "hs_bindgen_4731341b16926be1" hs_bindgen_4731341b16926be1_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint_least32_t_fun@
hs_bindgen_4731341b16926be1 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word32))
hs_bindgen_4731341b16926be1 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_4731341b16926be1_base

{-# NOINLINE uint_least32_t_fun #-}
{-| __C declaration:__ @uint_least32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 25:16@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_least32_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word32)
uint_least32_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4731341b16926be1

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint_least64_t_fun@
foreign import ccall unsafe "hs_bindgen_a796d3cd88467c40" hs_bindgen_a796d3cd88467c40_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint_least64_t_fun@
hs_bindgen_a796d3cd88467c40 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word64))
hs_bindgen_a796d3cd88467c40 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_a796d3cd88467c40_base

{-# NOINLINE uint_least64_t_fun #-}
{-| __C declaration:__ @uint_least64_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 26:16@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_least64_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word64)
uint_least64_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a796d3cd88467c40

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int_fast8_t_fun@
foreign import ccall unsafe "hs_bindgen_b9d35460329d6b4d" hs_bindgen_b9d35460329d6b4d_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int_fast8_t_fun@
hs_bindgen_b9d35460329d6b4d :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int8))
hs_bindgen_b9d35460329d6b4d =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_b9d35460329d6b4d_base

{-# NOINLINE int_fast8_t_fun #-}
{-| __C declaration:__ @int_fast8_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 27:13@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_fast8_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int8)
int_fast8_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b9d35460329d6b4d

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int_fast32_t_fun@
foreign import ccall unsafe "hs_bindgen_ac3c0ef78b4b880b" hs_bindgen_ac3c0ef78b4b880b_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int_fast32_t_fun@
hs_bindgen_ac3c0ef78b4b880b :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int32))
hs_bindgen_ac3c0ef78b4b880b =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_ac3c0ef78b4b880b_base

{-# NOINLINE int_fast32_t_fun #-}
{-| __C declaration:__ @int_fast32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 30:14@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_fast32_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int32)
int_fast32_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ac3c0ef78b4b880b

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int_fast64_t_fun@
foreign import ccall unsafe "hs_bindgen_051b82f2be6fb5cf" hs_bindgen_051b82f2be6fb5cf_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_int_fast64_t_fun@
hs_bindgen_051b82f2be6fb5cf :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int64))
hs_bindgen_051b82f2be6fb5cf =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_051b82f2be6fb5cf_base

{-# NOINLINE int_fast64_t_fun #-}
{-| __C declaration:__ @int_fast64_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 31:14@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_fast64_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int64)
int_fast64_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_051b82f2be6fb5cf

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint_fast8_t_fun@
foreign import ccall unsafe "hs_bindgen_8deac59faf09717a" hs_bindgen_8deac59faf09717a_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint_fast8_t_fun@
hs_bindgen_8deac59faf09717a :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word8))
hs_bindgen_8deac59faf09717a =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_8deac59faf09717a_base

{-# NOINLINE uint_fast8_t_fun #-}
{-| __C declaration:__ @uint_fast8_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 32:14@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_fast8_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word8)
uint_fast8_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8deac59faf09717a

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint_fast32_t_fun@
foreign import ccall unsafe "hs_bindgen_d2ec431c9ba1e735" hs_bindgen_d2ec431c9ba1e735_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint_fast32_t_fun@
hs_bindgen_d2ec431c9ba1e735 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word32))
hs_bindgen_d2ec431c9ba1e735 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_d2ec431c9ba1e735_base

{-# NOINLINE uint_fast32_t_fun #-}
{-| __C declaration:__ @uint_fast32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 35:15@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_fast32_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word32)
uint_fast32_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d2ec431c9ba1e735

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint_fast64_t_fun@
foreign import ccall unsafe "hs_bindgen_45a6a0d1f2cdf473" hs_bindgen_45a6a0d1f2cdf473_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uint_fast64_t_fun@
hs_bindgen_45a6a0d1f2cdf473 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word64))
hs_bindgen_45a6a0d1f2cdf473 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_45a6a0d1f2cdf473_base

{-# NOINLINE uint_fast64_t_fun #-}
{-| __C declaration:__ @uint_fast64_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 36:15@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_fast64_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word64)
uint_fast64_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_45a6a0d1f2cdf473

-- __unique:__ @test_bindingspecsstandard_library_Example_get_intmax_t_fun@
foreign import ccall unsafe "hs_bindgen_356803d6f1ee9998" hs_bindgen_356803d6f1ee9998_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_intmax_t_fun@
hs_bindgen_356803d6f1ee9998 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CIntMax))
hs_bindgen_356803d6f1ee9998 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_356803d6f1ee9998_base

{-# NOINLINE intmax_t_fun #-}
{-| __C declaration:__ @intmax_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 37:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
intmax_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CIntMax)
intmax_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_356803d6f1ee9998

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uintmax_t_fun@
foreign import ccall unsafe "hs_bindgen_2536b40423863c3d" hs_bindgen_2536b40423863c3d_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uintmax_t_fun@
hs_bindgen_2536b40423863c3d :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CUIntMax))
hs_bindgen_2536b40423863c3d =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_2536b40423863c3d_base

{-# NOINLINE uintmax_t_fun #-}
{-| __C declaration:__ @uintmax_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 38:11@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uintmax_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CUIntMax)
uintmax_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2536b40423863c3d

-- __unique:__ @test_bindingspecsstandard_library_Example_get_intptr_t_fun@
foreign import ccall unsafe "hs_bindgen_7588b9aeb45c8721" hs_bindgen_7588b9aeb45c8721_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_intptr_t_fun@
hs_bindgen_7588b9aeb45c8721 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CIntPtr))
hs_bindgen_7588b9aeb45c8721 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_7588b9aeb45c8721_base

{-# NOINLINE intptr_t_fun #-}
{-| __C declaration:__ @intptr_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 39:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
intptr_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CIntPtr)
intptr_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7588b9aeb45c8721

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uintptr_t_fun@
foreign import ccall unsafe "hs_bindgen_7edd04feb0afc86a" hs_bindgen_7edd04feb0afc86a_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_uintptr_t_fun@
hs_bindgen_7edd04feb0afc86a :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CUIntPtr))
hs_bindgen_7edd04feb0afc86a =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_7edd04feb0afc86a_base

{-# NOINLINE uintptr_t_fun #-}
{-| __C declaration:__ @uintptr_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 40:11@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uintptr_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CUIntPtr)
uintptr_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7edd04feb0afc86a

-- __unique:__ @test_bindingspecsstandard_library_Example_get_size_t_fun@
foreign import ccall unsafe "hs_bindgen_dbcbc3d798d90475" hs_bindgen_dbcbc3d798d90475_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_size_t_fun@
hs_bindgen_dbcbc3d798d90475 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CSize))
hs_bindgen_dbcbc3d798d90475 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_dbcbc3d798d90475_base

{-# NOINLINE size_t_fun #-}
{-| __C declaration:__ @size_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 51:8@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
size_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CSize)
size_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dbcbc3d798d90475

-- __unique:__ @test_bindingspecsstandard_library_Example_get_ptrdiff_t_fun@
foreign import ccall unsafe "hs_bindgen_caab6bc707229fac" hs_bindgen_caab6bc707229fac_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_ptrdiff_t_fun@
hs_bindgen_caab6bc707229fac :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CPtrdiff))
hs_bindgen_caab6bc707229fac =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_caab6bc707229fac_base

{-# NOINLINE ptrdiff_t_fun #-}
{-| __C declaration:__ @ptrdiff_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 52:11@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
ptrdiff_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CPtrdiff)
ptrdiff_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_caab6bc707229fac

-- __unique:__ @test_bindingspecsstandard_library_Example_get_wchar_t_fun@
foreign import ccall unsafe "hs_bindgen_dccb9712296f4b74" hs_bindgen_dccb9712296f4b74_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_wchar_t_fun@
hs_bindgen_dccb9712296f4b74 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CWchar))
hs_bindgen_dccb9712296f4b74 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_dccb9712296f4b74_base

{-# NOINLINE wchar_t_fun #-}
{-| __C declaration:__ @wchar_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 63:9@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
wchar_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CWchar)
wchar_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dccb9712296f4b74

-- __unique:__ @test_bindingspecsstandard_library_Example_get_wint_t_fun@
foreign import ccall unsafe "hs_bindgen_d8cb689ef8e61b17" hs_bindgen_d8cb689ef8e61b17_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_wint_t_fun@
hs_bindgen_d8cb689ef8e61b17 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CWintT))
hs_bindgen_d8cb689ef8e61b17 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_d8cb689ef8e61b17_base

{-# NOINLINE wint_t_fun #-}
{-| __C declaration:__ @wint_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 64:8@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
wint_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CWintT)
wint_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d8cb689ef8e61b17

-- __unique:__ @test_bindingspecsstandard_library_Example_get_wctrans_t_fun@
foreign import ccall unsafe "hs_bindgen_28185bca762ef301" hs_bindgen_28185bca762ef301_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_wctrans_t_fun@
hs_bindgen_28185bca762ef301 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CWctransT))
hs_bindgen_28185bca762ef301 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_28185bca762ef301_base

{-# NOINLINE wctrans_t_fun #-}
{-| __C declaration:__ @wctrans_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 67:11@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
wctrans_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CWctransT)
wctrans_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_28185bca762ef301

-- __unique:__ @test_bindingspecsstandard_library_Example_get_wctype_t_fun@
foreign import ccall unsafe "hs_bindgen_5fd9a8b4db6b244b" hs_bindgen_5fd9a8b4db6b244b_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_wctype_t_fun@
hs_bindgen_5fd9a8b4db6b244b :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CWctypeT))
hs_bindgen_5fd9a8b4db6b244b =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_5fd9a8b4db6b244b_base

{-# NOINLINE wctype_t_fun #-}
{-| __C declaration:__ @wctype_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 68:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
wctype_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CWctypeT)
wctype_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5fd9a8b4db6b244b

-- __unique:__ @test_bindingspecsstandard_library_Example_get_char16_t_fun@
foreign import ccall unsafe "hs_bindgen_4cb6b9c15fb1faa4" hs_bindgen_4cb6b9c15fb1faa4_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_char16_t_fun@
hs_bindgen_4cb6b9c15fb1faa4 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CChar16T))
hs_bindgen_4cb6b9c15fb1faa4 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_4cb6b9c15fb1faa4_base

{-# NOINLINE char16_t_fun #-}
{-| __C declaration:__ @char16_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 69:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
char16_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CChar16T)
char16_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4cb6b9c15fb1faa4

-- __unique:__ @test_bindingspecsstandard_library_Example_get_char32_t_fun@
foreign import ccall unsafe "hs_bindgen_2027a27340ed8e9a" hs_bindgen_2027a27340ed8e9a_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_char32_t_fun@
hs_bindgen_2027a27340ed8e9a :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CChar32T))
hs_bindgen_2027a27340ed8e9a =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_2027a27340ed8e9a_base

{-# NOINLINE char32_t_fun #-}
{-| __C declaration:__ @char32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 70:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
char32_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CChar32T)
char32_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2027a27340ed8e9a

-- __unique:__ @test_bindingspecsstandard_library_Example_get_time_t_fun@
foreign import ccall unsafe "hs_bindgen_c7ee4c8b957eea48" hs_bindgen_c7ee4c8b957eea48_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_time_t_fun@
hs_bindgen_c7ee4c8b957eea48 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CTime))
hs_bindgen_c7ee4c8b957eea48 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_c7ee4c8b957eea48_base

{-# NOINLINE time_t_fun #-}
{-| __C declaration:__ @time_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 74:8@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
time_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CTime)
time_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c7ee4c8b957eea48

-- __unique:__ @test_bindingspecsstandard_library_Example_get_clock_t_fun@
foreign import ccall unsafe "hs_bindgen_e0560b6cdb0474fc" hs_bindgen_e0560b6cdb0474fc_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_clock_t_fun@
hs_bindgen_e0560b6cdb0474fc :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CClock))
hs_bindgen_e0560b6cdb0474fc =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_e0560b6cdb0474fc_base

{-# NOINLINE clock_t_fun #-}
{-| __C declaration:__ @clock_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 75:9@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
clock_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CClock)
clock_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e0560b6cdb0474fc

-- __unique:__ @test_bindingspecsstandard_library_Example_get_sig_atomic_t_fun@
foreign import ccall unsafe "hs_bindgen_2d7ac7038960087a" hs_bindgen_2d7ac7038960087a_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_get_sig_atomic_t_fun@
hs_bindgen_2d7ac7038960087a :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CSigAtomic))
hs_bindgen_2d7ac7038960087a =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_2d7ac7038960087a_base

{-# NOINLINE sig_atomic_t_fun #-}
{-| __C declaration:__ @sig_atomic_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 88:14@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
sig_atomic_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CSigAtomic)
sig_atomic_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2d7ac7038960087a
