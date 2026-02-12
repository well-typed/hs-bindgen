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
  [ "#include <binding-specs/stdlib/return_values.h>"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_bool_fun */"
  , "__attribute__ ((const))"
  , "bool (*hs_bindgen_4d50a5da1d788aef (void)) (void)"
  , "{"
  , "  return &bool_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_int8_t_fun */"
  , "__attribute__ ((const))"
  , "int8_t (*hs_bindgen_afc12424e14cd900 (void)) (void)"
  , "{"
  , "  return &int8_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_int16_t_fun */"
  , "__attribute__ ((const))"
  , "int16_t (*hs_bindgen_bb1fdb972582362b (void)) (void)"
  , "{"
  , "  return &int16_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_int32_t_fun */"
  , "__attribute__ ((const))"
  , "int32_t (*hs_bindgen_3ddcee1ca6e45ed1 (void)) (void)"
  , "{"
  , "  return &int32_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_int64_t_fun */"
  , "__attribute__ ((const))"
  , "int64_t (*hs_bindgen_87f02bc29c3faac0 (void)) (void)"
  , "{"
  , "  return &int64_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_uint8_t_fun */"
  , "__attribute__ ((const))"
  , "uint8_t (*hs_bindgen_e41f6841cb24d084 (void)) (void)"
  , "{"
  , "  return &uint8_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_uint16_t_fun */"
  , "__attribute__ ((const))"
  , "uint16_t (*hs_bindgen_a99c4081312f9a64 (void)) (void)"
  , "{"
  , "  return &uint16_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_uint32_t_fun */"
  , "__attribute__ ((const))"
  , "uint32_t (*hs_bindgen_d7c52be7143a7a68 (void)) (void)"
  , "{"
  , "  return &uint32_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_uint64_t_fun */"
  , "__attribute__ ((const))"
  , "uint64_t (*hs_bindgen_ce7fc806ab2fc8e0 (void)) (void)"
  , "{"
  , "  return &uint64_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_intmax_t_fun */"
  , "__attribute__ ((const))"
  , "intmax_t (*hs_bindgen_5b1986d8b6b49f7e (void)) (void)"
  , "{"
  , "  return &intmax_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_uintmax_t_fun */"
  , "__attribute__ ((const))"
  , "uintmax_t (*hs_bindgen_022ea568c1d290f4 (void)) (void)"
  , "{"
  , "  return &uintmax_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_intptr_t_fun */"
  , "__attribute__ ((const))"
  , "intptr_t (*hs_bindgen_35eda59e93c8db9a (void)) (void)"
  , "{"
  , "  return &intptr_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_uintptr_t_fun */"
  , "__attribute__ ((const))"
  , "uintptr_t (*hs_bindgen_a45e879780c141ec (void)) (void)"
  , "{"
  , "  return &uintptr_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_fenv_t_fun */"
  , "__attribute__ ((const))"
  , "fenv_t *(*hs_bindgen_266815b1305c1241 (void)) (void)"
  , "{"
  , "  return &fenv_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_fexcept_t_fun */"
  , "__attribute__ ((const))"
  , "fexcept_t *(*hs_bindgen_d0eee798617140b8 (void)) (void)"
  , "{"
  , "  return &fexcept_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_size_t_fun */"
  , "__attribute__ ((const))"
  , "size_t (*hs_bindgen_0120eaefb471d45e (void)) (void)"
  , "{"
  , "  return &size_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_ptrdiff_t_fun */"
  , "__attribute__ ((const))"
  , "ptrdiff_t (*hs_bindgen_aa0444e3b2fda3de (void)) (void)"
  , "{"
  , "  return &ptrdiff_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_wchar_t_fun */"
  , "__attribute__ ((const))"
  , "wchar_t (*hs_bindgen_c3e6697f6a14bc13 (void)) (void)"
  , "{"
  , "  return &wchar_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_wint_t_fun */"
  , "__attribute__ ((const))"
  , "wint_t (*hs_bindgen_f3077dc66d53977f (void)) (void)"
  , "{"
  , "  return &wint_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_mbstate_t_fun */"
  , "__attribute__ ((const))"
  , "mbstate_t *(*hs_bindgen_b863e606ff088408 (void)) (void)"
  , "{"
  , "  return &mbstate_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_wctrans_t_fun */"
  , "__attribute__ ((const))"
  , "wctrans_t (*hs_bindgen_a9599637dbc12608 (void)) (void)"
  , "{"
  , "  return &wctrans_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_wctype_t_fun */"
  , "__attribute__ ((const))"
  , "wctype_t (*hs_bindgen_207a54450593099a (void)) (void)"
  , "{"
  , "  return &wctype_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_char16_t_fun */"
  , "__attribute__ ((const))"
  , "char16_t (*hs_bindgen_28a6e39f56489ba2 (void)) (void)"
  , "{"
  , "  return &char16_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_char32_t_fun */"
  , "__attribute__ ((const))"
  , "char32_t (*hs_bindgen_f918c5a2d6c16885 (void)) (void)"
  , "{"
  , "  return &char32_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_time_t_fun */"
  , "__attribute__ ((const))"
  , "time_t (*hs_bindgen_f29cfe7708be784f (void)) (void)"
  , "{"
  , "  return &time_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_clock_t_fun */"
  , "__attribute__ ((const))"
  , "clock_t (*hs_bindgen_509d092c2afe1ddc (void)) (void)"
  , "{"
  , "  return &clock_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_FILE_fun */"
  , "__attribute__ ((const))"
  , "FILE *(*hs_bindgen_8f5880570a38b5dd (void)) (void)"
  , "{"
  , "  return &FILE_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_fpos_t_fun */"
  , "__attribute__ ((const))"
  , "fpos_t *(*hs_bindgen_8b563cc85422e790 (void)) (void)"
  , "{"
  , "  return &fpos_t_fun;"
  , "}"
  , "/* test_bindingspecsstdlibreturn_va_Example_get_sig_atomic_t_fun */"
  , "__attribute__ ((const))"
  , "sig_atomic_t (*hs_bindgen_9fcd36cfaa3ea373 (void)) (void)"
  , "{"
  , "  return &sig_atomic_t_fun;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_bool_fun@
foreign import ccall unsafe "hs_bindgen_4d50a5da1d788aef" hs_bindgen_4d50a5da1d788aef_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_bool_fun@
hs_bindgen_4d50a5da1d788aef :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CBool))
hs_bindgen_4d50a5da1d788aef =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_4d50a5da1d788aef_base

{-# NOINLINE bool_fun #-}
{-| __C declaration:__ @bool_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 5:6@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
bool_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CBool)
bool_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4d50a5da1d788aef

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_int8_t_fun@
foreign import ccall unsafe "hs_bindgen_afc12424e14cd900" hs_bindgen_afc12424e14cd900_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_int8_t_fun@
hs_bindgen_afc12424e14cd900 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int8))
hs_bindgen_afc12424e14cd900 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_afc12424e14cd900_base

{-# NOINLINE int8_t_fun #-}
{-| __C declaration:__ @int8_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 9:8@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
int8_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int8)
int8_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_afc12424e14cd900

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_int16_t_fun@
foreign import ccall unsafe "hs_bindgen_bb1fdb972582362b" hs_bindgen_bb1fdb972582362b_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_int16_t_fun@
hs_bindgen_bb1fdb972582362b :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int16))
hs_bindgen_bb1fdb972582362b =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_bb1fdb972582362b_base

{-# NOINLINE int16_t_fun #-}
{-| __C declaration:__ @int16_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 10:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
int16_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int16)
int16_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bb1fdb972582362b

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_int32_t_fun@
foreign import ccall unsafe "hs_bindgen_3ddcee1ca6e45ed1" hs_bindgen_3ddcee1ca6e45ed1_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_int32_t_fun@
hs_bindgen_3ddcee1ca6e45ed1 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int32))
hs_bindgen_3ddcee1ca6e45ed1 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_3ddcee1ca6e45ed1_base

{-# NOINLINE int32_t_fun #-}
{-| __C declaration:__ @int32_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 11:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
int32_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int32)
int32_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3ddcee1ca6e45ed1

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_int64_t_fun@
foreign import ccall unsafe "hs_bindgen_87f02bc29c3faac0" hs_bindgen_87f02bc29c3faac0_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_int64_t_fun@
hs_bindgen_87f02bc29c3faac0 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int64))
hs_bindgen_87f02bc29c3faac0 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_87f02bc29c3faac0_base

{-# NOINLINE int64_t_fun #-}
{-| __C declaration:__ @int64_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 12:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
int64_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Int64)
int64_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_87f02bc29c3faac0

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_uint8_t_fun@
foreign import ccall unsafe "hs_bindgen_e41f6841cb24d084" hs_bindgen_e41f6841cb24d084_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_uint8_t_fun@
hs_bindgen_e41f6841cb24d084 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word8))
hs_bindgen_e41f6841cb24d084 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_e41f6841cb24d084_base

{-# NOINLINE uint8_t_fun #-}
{-| __C declaration:__ @uint8_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 13:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
uint8_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word8)
uint8_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e41f6841cb24d084

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_uint16_t_fun@
foreign import ccall unsafe "hs_bindgen_a99c4081312f9a64" hs_bindgen_a99c4081312f9a64_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_uint16_t_fun@
hs_bindgen_a99c4081312f9a64 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word16))
hs_bindgen_a99c4081312f9a64 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_a99c4081312f9a64_base

{-# NOINLINE uint16_t_fun #-}
{-| __C declaration:__ @uint16_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 14:10@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
uint16_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word16)
uint16_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a99c4081312f9a64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_uint32_t_fun@
foreign import ccall unsafe "hs_bindgen_d7c52be7143a7a68" hs_bindgen_d7c52be7143a7a68_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_uint32_t_fun@
hs_bindgen_d7c52be7143a7a68 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word32))
hs_bindgen_d7c52be7143a7a68 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_d7c52be7143a7a68_base

{-# NOINLINE uint32_t_fun #-}
{-| __C declaration:__ @uint32_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 15:10@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
uint32_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word32)
uint32_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d7c52be7143a7a68

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_uint64_t_fun@
foreign import ccall unsafe "hs_bindgen_ce7fc806ab2fc8e0" hs_bindgen_ce7fc806ab2fc8e0_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_uint64_t_fun@
hs_bindgen_ce7fc806ab2fc8e0 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word64))
hs_bindgen_ce7fc806ab2fc8e0 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_ce7fc806ab2fc8e0_base

{-# NOINLINE uint64_t_fun #-}
{-| __C declaration:__ @uint64_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 16:10@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
uint64_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.Word64)
uint64_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ce7fc806ab2fc8e0

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_intmax_t_fun@
foreign import ccall unsafe "hs_bindgen_5b1986d8b6b49f7e" hs_bindgen_5b1986d8b6b49f7e_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_intmax_t_fun@
hs_bindgen_5b1986d8b6b49f7e :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CIntMax))
hs_bindgen_5b1986d8b6b49f7e =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_5b1986d8b6b49f7e_base

{-# NOINLINE intmax_t_fun #-}
{-| __C declaration:__ @intmax_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 17:10@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
intmax_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CIntMax)
intmax_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5b1986d8b6b49f7e

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_uintmax_t_fun@
foreign import ccall unsafe "hs_bindgen_022ea568c1d290f4" hs_bindgen_022ea568c1d290f4_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_uintmax_t_fun@
hs_bindgen_022ea568c1d290f4 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CUIntMax))
hs_bindgen_022ea568c1d290f4 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_022ea568c1d290f4_base

{-# NOINLINE uintmax_t_fun #-}
{-| __C declaration:__ @uintmax_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 18:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
uintmax_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CUIntMax)
uintmax_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_022ea568c1d290f4

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_intptr_t_fun@
foreign import ccall unsafe "hs_bindgen_35eda59e93c8db9a" hs_bindgen_35eda59e93c8db9a_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_intptr_t_fun@
hs_bindgen_35eda59e93c8db9a :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CIntPtr))
hs_bindgen_35eda59e93c8db9a =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_35eda59e93c8db9a_base

{-# NOINLINE intptr_t_fun #-}
{-| __C declaration:__ @intptr_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 19:10@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
intptr_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CIntPtr)
intptr_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_35eda59e93c8db9a

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_uintptr_t_fun@
foreign import ccall unsafe "hs_bindgen_a45e879780c141ec" hs_bindgen_a45e879780c141ec_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_uintptr_t_fun@
hs_bindgen_a45e879780c141ec :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CUIntPtr))
hs_bindgen_a45e879780c141ec =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_a45e879780c141ec_base

{-# NOINLINE uintptr_t_fun #-}
{-| __C declaration:__ @uintptr_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 20:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
uintptr_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CUIntPtr)
uintptr_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a45e879780c141ec

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_fenv_t_fun@
foreign import ccall unsafe "hs_bindgen_266815b1305c1241" hs_bindgen_266815b1305c1241_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_fenv_t_fun@
hs_bindgen_266815b1305c1241 :: IO (Ptr.FunPtr (IO (Ptr.Ptr HsBindgen.Runtime.LibC.CFenvT)))
hs_bindgen_266815b1305c1241 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_266815b1305c1241_base

{-# NOINLINE fenv_t_fun #-}
{-| __C declaration:__ @fenv_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 24:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
fenv_t_fun :: Ptr.FunPtr (IO (Ptr.Ptr HsBindgen.Runtime.LibC.CFenvT))
fenv_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_266815b1305c1241

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_fexcept_t_fun@
foreign import ccall unsafe "hs_bindgen_d0eee798617140b8" hs_bindgen_d0eee798617140b8_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_fexcept_t_fun@
hs_bindgen_d0eee798617140b8 :: IO (Ptr.FunPtr (IO (Ptr.Ptr HsBindgen.Runtime.LibC.CFexceptT)))
hs_bindgen_d0eee798617140b8 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_d0eee798617140b8_base

{-# NOINLINE fexcept_t_fun #-}
{-| __C declaration:__ @fexcept_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 25:12@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
fexcept_t_fun :: Ptr.FunPtr (IO (Ptr.Ptr HsBindgen.Runtime.LibC.CFexceptT))
fexcept_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d0eee798617140b8

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_size_t_fun@
foreign import ccall unsafe "hs_bindgen_0120eaefb471d45e" hs_bindgen_0120eaefb471d45e_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_size_t_fun@
hs_bindgen_0120eaefb471d45e :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CSize))
hs_bindgen_0120eaefb471d45e =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_0120eaefb471d45e_base

{-# NOINLINE size_t_fun #-}
{-| __C declaration:__ @size_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 29:8@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
size_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CSize)
size_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0120eaefb471d45e

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_ptrdiff_t_fun@
foreign import ccall unsafe "hs_bindgen_aa0444e3b2fda3de" hs_bindgen_aa0444e3b2fda3de_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_ptrdiff_t_fun@
hs_bindgen_aa0444e3b2fda3de :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CPtrdiff))
hs_bindgen_aa0444e3b2fda3de =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_aa0444e3b2fda3de_base

{-# NOINLINE ptrdiff_t_fun #-}
{-| __C declaration:__ @ptrdiff_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 30:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
ptrdiff_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CPtrdiff)
ptrdiff_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_aa0444e3b2fda3de

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_wchar_t_fun@
foreign import ccall unsafe "hs_bindgen_c3e6697f6a14bc13" hs_bindgen_c3e6697f6a14bc13_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_wchar_t_fun@
hs_bindgen_c3e6697f6a14bc13 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CWchar))
hs_bindgen_c3e6697f6a14bc13 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_c3e6697f6a14bc13_base

{-# NOINLINE wchar_t_fun #-}
{-| __C declaration:__ @wchar_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 41:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
wchar_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CWchar)
wchar_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c3e6697f6a14bc13

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_wint_t_fun@
foreign import ccall unsafe "hs_bindgen_f3077dc66d53977f" hs_bindgen_f3077dc66d53977f_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_wint_t_fun@
hs_bindgen_f3077dc66d53977f :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CWintT))
hs_bindgen_f3077dc66d53977f =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_f3077dc66d53977f_base

{-# NOINLINE wint_t_fun #-}
{-| __C declaration:__ @wint_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 42:8@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
wint_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CWintT)
wint_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f3077dc66d53977f

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_mbstate_t_fun@
foreign import ccall unsafe "hs_bindgen_b863e606ff088408" hs_bindgen_b863e606ff088408_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_mbstate_t_fun@
hs_bindgen_b863e606ff088408 :: IO (Ptr.FunPtr (IO (Ptr.Ptr HsBindgen.Runtime.LibC.CMbstateT)))
hs_bindgen_b863e606ff088408 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_b863e606ff088408_base

{-# NOINLINE mbstate_t_fun #-}
{-| __C declaration:__ @mbstate_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 43:12@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
mbstate_t_fun :: Ptr.FunPtr (IO (Ptr.Ptr HsBindgen.Runtime.LibC.CMbstateT))
mbstate_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b863e606ff088408

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_wctrans_t_fun@
foreign import ccall unsafe "hs_bindgen_a9599637dbc12608" hs_bindgen_a9599637dbc12608_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_wctrans_t_fun@
hs_bindgen_a9599637dbc12608 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CWctransT))
hs_bindgen_a9599637dbc12608 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_a9599637dbc12608_base

{-# NOINLINE wctrans_t_fun #-}
{-| __C declaration:__ @wctrans_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 44:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
wctrans_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CWctransT)
wctrans_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a9599637dbc12608

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_wctype_t_fun@
foreign import ccall unsafe "hs_bindgen_207a54450593099a" hs_bindgen_207a54450593099a_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_wctype_t_fun@
hs_bindgen_207a54450593099a :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CWctypeT))
hs_bindgen_207a54450593099a =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_207a54450593099a_base

{-# NOINLINE wctype_t_fun #-}
{-| __C declaration:__ @wctype_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 45:10@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
wctype_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CWctypeT)
wctype_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_207a54450593099a

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_char16_t_fun@
foreign import ccall unsafe "hs_bindgen_28a6e39f56489ba2" hs_bindgen_28a6e39f56489ba2_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_char16_t_fun@
hs_bindgen_28a6e39f56489ba2 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CChar16T))
hs_bindgen_28a6e39f56489ba2 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_28a6e39f56489ba2_base

{-# NOINLINE char16_t_fun #-}
{-| __C declaration:__ @char16_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 46:10@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
char16_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CChar16T)
char16_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_28a6e39f56489ba2

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_char32_t_fun@
foreign import ccall unsafe "hs_bindgen_f918c5a2d6c16885" hs_bindgen_f918c5a2d6c16885_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_char32_t_fun@
hs_bindgen_f918c5a2d6c16885 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CChar32T))
hs_bindgen_f918c5a2d6c16885 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_f918c5a2d6c16885_base

{-# NOINLINE char32_t_fun #-}
{-| __C declaration:__ @char32_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 47:10@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
char32_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CChar32T)
char32_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f918c5a2d6c16885

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_time_t_fun@
foreign import ccall unsafe "hs_bindgen_f29cfe7708be784f" hs_bindgen_f29cfe7708be784f_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_time_t_fun@
hs_bindgen_f29cfe7708be784f :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CTime))
hs_bindgen_f29cfe7708be784f =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_f29cfe7708be784f_base

{-# NOINLINE time_t_fun #-}
{-| __C declaration:__ @time_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 51:8@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
time_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CTime)
time_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f29cfe7708be784f

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_clock_t_fun@
foreign import ccall unsafe "hs_bindgen_509d092c2afe1ddc" hs_bindgen_509d092c2afe1ddc_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_clock_t_fun@
hs_bindgen_509d092c2afe1ddc :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CClock))
hs_bindgen_509d092c2afe1ddc =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_509d092c2afe1ddc_base

{-# NOINLINE clock_t_fun #-}
{-| __C declaration:__ @clock_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 52:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
clock_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CClock)
clock_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_509d092c2afe1ddc

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_FILE_fun@
foreign import ccall unsafe "hs_bindgen_8f5880570a38b5dd" hs_bindgen_8f5880570a38b5dd_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_FILE_fun@
hs_bindgen_8f5880570a38b5dd :: IO (Ptr.FunPtr (IO (Ptr.Ptr HsBindgen.Runtime.LibC.CFile)))
hs_bindgen_8f5880570a38b5dd =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_8f5880570a38b5dd_base

{-# NOINLINE fILE_fun #-}
{-| __C declaration:__ @FILE_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 58:7@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
fILE_fun :: Ptr.FunPtr (IO (Ptr.Ptr HsBindgen.Runtime.LibC.CFile))
fILE_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8f5880570a38b5dd

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_fpos_t_fun@
foreign import ccall unsafe "hs_bindgen_8b563cc85422e790" hs_bindgen_8b563cc85422e790_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_fpos_t_fun@
hs_bindgen_8b563cc85422e790 :: IO (Ptr.FunPtr (IO (Ptr.Ptr HsBindgen.Runtime.LibC.CFpos)))
hs_bindgen_8b563cc85422e790 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_8b563cc85422e790_base

{-# NOINLINE fpos_t_fun #-}
{-| __C declaration:__ @fpos_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 59:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
fpos_t_fun :: Ptr.FunPtr (IO (Ptr.Ptr HsBindgen.Runtime.LibC.CFpos))
fpos_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8b563cc85422e790

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_sig_atomic_t_fun@
foreign import ccall unsafe "hs_bindgen_9fcd36cfaa3ea373" hs_bindgen_9fcd36cfaa3ea373_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_get_sig_atomic_t_fun@
hs_bindgen_9fcd36cfaa3ea373 :: IO (Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CSigAtomic))
hs_bindgen_9fcd36cfaa3ea373 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_9fcd36cfaa3ea373_base

{-# NOINLINE sig_atomic_t_fun #-}
{-| __C declaration:__ @sig_atomic_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 63:14@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
sig_atomic_t_fun :: Ptr.FunPtr (IO HsBindgen.Runtime.LibC.CSigAtomic)
sig_atomic_t_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9fcd36cfaa3ea373
