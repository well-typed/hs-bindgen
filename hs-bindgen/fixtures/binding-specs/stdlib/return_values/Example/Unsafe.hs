{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified GHC.Word
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.LibC
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/stdlib/return_values.h>"
  , "bool hs_bindgen_6b68851ce9c1f36f (void)"
  , "{"
  , "  return bool_fun();"
  , "}"
  , "int8_t hs_bindgen_a408da1bfa030a51 (void)"
  , "{"
  , "  return int8_t_fun();"
  , "}"
  , "int16_t hs_bindgen_6785058f9844b9b5 (void)"
  , "{"
  , "  return int16_t_fun();"
  , "}"
  , "int32_t hs_bindgen_191a7b2b445fb350 (void)"
  , "{"
  , "  return int32_t_fun();"
  , "}"
  , "int64_t hs_bindgen_0a523ac71d31b35c (void)"
  , "{"
  , "  return int64_t_fun();"
  , "}"
  , "uint8_t hs_bindgen_f4245406bfdade75 (void)"
  , "{"
  , "  return uint8_t_fun();"
  , "}"
  , "uint16_t hs_bindgen_f7a0d726d7268b30 (void)"
  , "{"
  , "  return uint16_t_fun();"
  , "}"
  , "uint32_t hs_bindgen_432f65d5f6e3c062 (void)"
  , "{"
  , "  return uint32_t_fun();"
  , "}"
  , "uint64_t hs_bindgen_af91a28f07360676 (void)"
  , "{"
  , "  return uint64_t_fun();"
  , "}"
  , "intmax_t hs_bindgen_34ebd7f9b1c9877f (void)"
  , "{"
  , "  return intmax_t_fun();"
  , "}"
  , "uintmax_t hs_bindgen_f147b22f36663ba2 (void)"
  , "{"
  , "  return uintmax_t_fun();"
  , "}"
  , "intptr_t hs_bindgen_42bfd915930be089 (void)"
  , "{"
  , "  return intptr_t_fun();"
  , "}"
  , "uintptr_t hs_bindgen_3d4e032e4f99ccdf (void)"
  , "{"
  , "  return uintptr_t_fun();"
  , "}"
  , "fenv_t *hs_bindgen_d12ec6bea98f91aa (void)"
  , "{"
  , "  return fenv_t_fun();"
  , "}"
  , "fexcept_t *hs_bindgen_bfb6abdc4397c064 (void)"
  , "{"
  , "  return fexcept_t_fun();"
  , "}"
  , "size_t hs_bindgen_3030e3c8a5de2e7e (void)"
  , "{"
  , "  return size_t_fun();"
  , "}"
  , "ptrdiff_t hs_bindgen_7033a7ddfdfe770f (void)"
  , "{"
  , "  return ptrdiff_t_fun();"
  , "}"
  , "wchar_t hs_bindgen_ebae7f4d5db1ecd0 (void)"
  , "{"
  , "  return wchar_t_fun();"
  , "}"
  , "wint_t hs_bindgen_623e42289a1c3dc3 (void)"
  , "{"
  , "  return wint_t_fun();"
  , "}"
  , "mbstate_t *hs_bindgen_bc84314eca8f2a7c (void)"
  , "{"
  , "  return mbstate_t_fun();"
  , "}"
  , "wctrans_t hs_bindgen_5524c9adc3841732 (void)"
  , "{"
  , "  return wctrans_t_fun();"
  , "}"
  , "wctype_t hs_bindgen_72885cbcc8213893 (void)"
  , "{"
  , "  return wctype_t_fun();"
  , "}"
  , "char16_t hs_bindgen_4284e8eb72d43965 (void)"
  , "{"
  , "  return char16_t_fun();"
  , "}"
  , "char32_t hs_bindgen_74e1167907c5de0e (void)"
  , "{"
  , "  return char32_t_fun();"
  , "}"
  , "time_t hs_bindgen_a0c949d46b12627a (void)"
  , "{"
  , "  return time_t_fun();"
  , "}"
  , "clock_t hs_bindgen_ac7afd479db138db (void)"
  , "{"
  , "  return clock_t_fun();"
  , "}"
  , "FILE *hs_bindgen_51b6e01d56713f95 (void)"
  , "{"
  , "  return FILE_fun();"
  , "}"
  , "fpos_t *hs_bindgen_e9ed5e9a298e9a7e (void)"
  , "{"
  , "  return fpos_t_fun();"
  , "}"
  , "sig_atomic_t hs_bindgen_1aecb4fb9faabcf5 (void)"
  , "{"
  , "  return sig_atomic_t_fun();"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_bool_fun@
foreign import ccall unsafe "hs_bindgen_6b68851ce9c1f36f" hs_bindgen_6b68851ce9c1f36f_base ::
     IO GHC.Word.Word8

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_bool_fun@
hs_bindgen_6b68851ce9c1f36f :: IO HsBindgen.Runtime.LibC.CBool
hs_bindgen_6b68851ce9c1f36f =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_6b68851ce9c1f36f_base

{-| __C declaration:__ @bool_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 5:6@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
bool_fun :: IO HsBindgen.Runtime.LibC.CBool
bool_fun = hs_bindgen_6b68851ce9c1f36f

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_int8_t_fun@
foreign import ccall unsafe "hs_bindgen_a408da1bfa030a51" hs_bindgen_a408da1bfa030a51_base ::
     IO GHC.Int.Int8

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_int8_t_fun@
hs_bindgen_a408da1bfa030a51 :: IO HsBindgen.Runtime.LibC.Int8
hs_bindgen_a408da1bfa030a51 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_a408da1bfa030a51_base

{-| __C declaration:__ @int8_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 9:8@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
int8_t_fun :: IO HsBindgen.Runtime.LibC.Int8
int8_t_fun = hs_bindgen_a408da1bfa030a51

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_int16_t_fun@
foreign import ccall unsafe "hs_bindgen_6785058f9844b9b5" hs_bindgen_6785058f9844b9b5_base ::
     IO GHC.Int.Int16

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_int16_t_fun@
hs_bindgen_6785058f9844b9b5 :: IO HsBindgen.Runtime.LibC.Int16
hs_bindgen_6785058f9844b9b5 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_6785058f9844b9b5_base

{-| __C declaration:__ @int16_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 10:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
int16_t_fun :: IO HsBindgen.Runtime.LibC.Int16
int16_t_fun = hs_bindgen_6785058f9844b9b5

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_int32_t_fun@
foreign import ccall unsafe "hs_bindgen_191a7b2b445fb350" hs_bindgen_191a7b2b445fb350_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_int32_t_fun@
hs_bindgen_191a7b2b445fb350 :: IO HsBindgen.Runtime.LibC.Int32
hs_bindgen_191a7b2b445fb350 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_191a7b2b445fb350_base

{-| __C declaration:__ @int32_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 11:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
int32_t_fun :: IO HsBindgen.Runtime.LibC.Int32
int32_t_fun = hs_bindgen_191a7b2b445fb350

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_int64_t_fun@
foreign import ccall unsafe "hs_bindgen_0a523ac71d31b35c" hs_bindgen_0a523ac71d31b35c_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_int64_t_fun@
hs_bindgen_0a523ac71d31b35c :: IO HsBindgen.Runtime.LibC.Int64
hs_bindgen_0a523ac71d31b35c =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_0a523ac71d31b35c_base

{-| __C declaration:__ @int64_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 12:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
int64_t_fun :: IO HsBindgen.Runtime.LibC.Int64
int64_t_fun = hs_bindgen_0a523ac71d31b35c

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_uint8_t_fun@
foreign import ccall unsafe "hs_bindgen_f4245406bfdade75" hs_bindgen_f4245406bfdade75_base ::
     IO GHC.Word.Word8

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_uint8_t_fun@
hs_bindgen_f4245406bfdade75 :: IO HsBindgen.Runtime.LibC.Word8
hs_bindgen_f4245406bfdade75 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_f4245406bfdade75_base

{-| __C declaration:__ @uint8_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 13:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
uint8_t_fun :: IO HsBindgen.Runtime.LibC.Word8
uint8_t_fun = hs_bindgen_f4245406bfdade75

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_uint16_t_fun@
foreign import ccall unsafe "hs_bindgen_f7a0d726d7268b30" hs_bindgen_f7a0d726d7268b30_base ::
     IO GHC.Word.Word16

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_uint16_t_fun@
hs_bindgen_f7a0d726d7268b30 :: IO HsBindgen.Runtime.LibC.Word16
hs_bindgen_f7a0d726d7268b30 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_f7a0d726d7268b30_base

{-| __C declaration:__ @uint16_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 14:10@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
uint16_t_fun :: IO HsBindgen.Runtime.LibC.Word16
uint16_t_fun = hs_bindgen_f7a0d726d7268b30

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_uint32_t_fun@
foreign import ccall unsafe "hs_bindgen_432f65d5f6e3c062" hs_bindgen_432f65d5f6e3c062_base ::
     IO GHC.Word.Word32

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_uint32_t_fun@
hs_bindgen_432f65d5f6e3c062 :: IO HsBindgen.Runtime.LibC.Word32
hs_bindgen_432f65d5f6e3c062 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_432f65d5f6e3c062_base

{-| __C declaration:__ @uint32_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 15:10@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
uint32_t_fun :: IO HsBindgen.Runtime.LibC.Word32
uint32_t_fun = hs_bindgen_432f65d5f6e3c062

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_uint64_t_fun@
foreign import ccall unsafe "hs_bindgen_af91a28f07360676" hs_bindgen_af91a28f07360676_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_uint64_t_fun@
hs_bindgen_af91a28f07360676 :: IO HsBindgen.Runtime.LibC.Word64
hs_bindgen_af91a28f07360676 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_af91a28f07360676_base

{-| __C declaration:__ @uint64_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 16:10@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
uint64_t_fun :: IO HsBindgen.Runtime.LibC.Word64
uint64_t_fun = hs_bindgen_af91a28f07360676

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_intmax_t_fun@
foreign import ccall unsafe "hs_bindgen_34ebd7f9b1c9877f" hs_bindgen_34ebd7f9b1c9877f_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_intmax_t_fun@
hs_bindgen_34ebd7f9b1c9877f :: IO HsBindgen.Runtime.LibC.CIntMax
hs_bindgen_34ebd7f9b1c9877f =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_34ebd7f9b1c9877f_base

{-| __C declaration:__ @intmax_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 17:10@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
intmax_t_fun :: IO HsBindgen.Runtime.LibC.CIntMax
intmax_t_fun = hs_bindgen_34ebd7f9b1c9877f

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_uintmax_t_fun@
foreign import ccall unsafe "hs_bindgen_f147b22f36663ba2" hs_bindgen_f147b22f36663ba2_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_uintmax_t_fun@
hs_bindgen_f147b22f36663ba2 :: IO HsBindgen.Runtime.LibC.CUIntMax
hs_bindgen_f147b22f36663ba2 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_f147b22f36663ba2_base

{-| __C declaration:__ @uintmax_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 18:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
uintmax_t_fun :: IO HsBindgen.Runtime.LibC.CUIntMax
uintmax_t_fun = hs_bindgen_f147b22f36663ba2

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_intptr_t_fun@
foreign import ccall unsafe "hs_bindgen_42bfd915930be089" hs_bindgen_42bfd915930be089_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_intptr_t_fun@
hs_bindgen_42bfd915930be089 :: IO HsBindgen.Runtime.LibC.CIntPtr
hs_bindgen_42bfd915930be089 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_42bfd915930be089_base

{-| __C declaration:__ @intptr_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 19:10@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
intptr_t_fun :: IO HsBindgen.Runtime.LibC.CIntPtr
intptr_t_fun = hs_bindgen_42bfd915930be089

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_uintptr_t_fun@
foreign import ccall unsafe "hs_bindgen_3d4e032e4f99ccdf" hs_bindgen_3d4e032e4f99ccdf_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_uintptr_t_fun@
hs_bindgen_3d4e032e4f99ccdf :: IO HsBindgen.Runtime.LibC.CUIntPtr
hs_bindgen_3d4e032e4f99ccdf =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_3d4e032e4f99ccdf_base

{-| __C declaration:__ @uintptr_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 20:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
uintptr_t_fun :: IO HsBindgen.Runtime.LibC.CUIntPtr
uintptr_t_fun = hs_bindgen_3d4e032e4f99ccdf

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_fenv_t_fun@
foreign import ccall unsafe "hs_bindgen_d12ec6bea98f91aa" hs_bindgen_d12ec6bea98f91aa_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_fenv_t_fun@
hs_bindgen_d12ec6bea98f91aa :: IO (Ptr.Ptr HsBindgen.Runtime.LibC.CFenvT)
hs_bindgen_d12ec6bea98f91aa =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_d12ec6bea98f91aa_base

{-| __C declaration:__ @fenv_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 24:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
fenv_t_fun :: IO (Ptr.Ptr HsBindgen.Runtime.LibC.CFenvT)
fenv_t_fun = hs_bindgen_d12ec6bea98f91aa

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_fexcept_t_fun@
foreign import ccall unsafe "hs_bindgen_bfb6abdc4397c064" hs_bindgen_bfb6abdc4397c064_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_fexcept_t_fun@
hs_bindgen_bfb6abdc4397c064 :: IO (Ptr.Ptr HsBindgen.Runtime.LibC.CFexceptT)
hs_bindgen_bfb6abdc4397c064 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_bfb6abdc4397c064_base

{-| __C declaration:__ @fexcept_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 25:12@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
fexcept_t_fun :: IO (Ptr.Ptr HsBindgen.Runtime.LibC.CFexceptT)
fexcept_t_fun = hs_bindgen_bfb6abdc4397c064

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_size_t_fun@
foreign import ccall unsafe "hs_bindgen_3030e3c8a5de2e7e" hs_bindgen_3030e3c8a5de2e7e_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_size_t_fun@
hs_bindgen_3030e3c8a5de2e7e :: IO HsBindgen.Runtime.LibC.CSize
hs_bindgen_3030e3c8a5de2e7e =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_3030e3c8a5de2e7e_base

{-| __C declaration:__ @size_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 29:8@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
size_t_fun :: IO HsBindgen.Runtime.LibC.CSize
size_t_fun = hs_bindgen_3030e3c8a5de2e7e

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_ptrdiff_t_fun@
foreign import ccall unsafe "hs_bindgen_7033a7ddfdfe770f" hs_bindgen_7033a7ddfdfe770f_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_ptrdiff_t_fun@
hs_bindgen_7033a7ddfdfe770f :: IO HsBindgen.Runtime.LibC.CPtrdiff
hs_bindgen_7033a7ddfdfe770f =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_7033a7ddfdfe770f_base

{-| __C declaration:__ @ptrdiff_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 30:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
ptrdiff_t_fun :: IO HsBindgen.Runtime.LibC.CPtrdiff
ptrdiff_t_fun = hs_bindgen_7033a7ddfdfe770f

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_wchar_t_fun@
foreign import ccall unsafe "hs_bindgen_ebae7f4d5db1ecd0" hs_bindgen_ebae7f4d5db1ecd0_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_wchar_t_fun@
hs_bindgen_ebae7f4d5db1ecd0 :: IO HsBindgen.Runtime.LibC.CWchar
hs_bindgen_ebae7f4d5db1ecd0 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_ebae7f4d5db1ecd0_base

{-| __C declaration:__ @wchar_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 41:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
wchar_t_fun :: IO HsBindgen.Runtime.LibC.CWchar
wchar_t_fun = hs_bindgen_ebae7f4d5db1ecd0

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_wint_t_fun@
foreign import ccall unsafe "hs_bindgen_623e42289a1c3dc3" hs_bindgen_623e42289a1c3dc3_base ::
     IO GHC.Word.Word32

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_wint_t_fun@
hs_bindgen_623e42289a1c3dc3 :: IO HsBindgen.Runtime.LibC.CWintT
hs_bindgen_623e42289a1c3dc3 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_623e42289a1c3dc3_base

{-| __C declaration:__ @wint_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 42:8@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
wint_t_fun :: IO HsBindgen.Runtime.LibC.CWintT
wint_t_fun = hs_bindgen_623e42289a1c3dc3

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_mbstate_t_fun@
foreign import ccall unsafe "hs_bindgen_bc84314eca8f2a7c" hs_bindgen_bc84314eca8f2a7c_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_mbstate_t_fun@
hs_bindgen_bc84314eca8f2a7c :: IO (Ptr.Ptr HsBindgen.Runtime.LibC.CMbstateT)
hs_bindgen_bc84314eca8f2a7c =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_bc84314eca8f2a7c_base

{-| __C declaration:__ @mbstate_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 43:12@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
mbstate_t_fun :: IO (Ptr.Ptr HsBindgen.Runtime.LibC.CMbstateT)
mbstate_t_fun = hs_bindgen_bc84314eca8f2a7c

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_wctrans_t_fun@
foreign import ccall unsafe "hs_bindgen_5524c9adc3841732" hs_bindgen_5524c9adc3841732_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_wctrans_t_fun@
hs_bindgen_5524c9adc3841732 :: IO HsBindgen.Runtime.LibC.CWctransT
hs_bindgen_5524c9adc3841732 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_5524c9adc3841732_base

{-| __C declaration:__ @wctrans_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 44:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
wctrans_t_fun :: IO HsBindgen.Runtime.LibC.CWctransT
wctrans_t_fun = hs_bindgen_5524c9adc3841732

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_wctype_t_fun@
foreign import ccall unsafe "hs_bindgen_72885cbcc8213893" hs_bindgen_72885cbcc8213893_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_wctype_t_fun@
hs_bindgen_72885cbcc8213893 :: IO HsBindgen.Runtime.LibC.CWctypeT
hs_bindgen_72885cbcc8213893 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_72885cbcc8213893_base

{-| __C declaration:__ @wctype_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 45:10@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
wctype_t_fun :: IO HsBindgen.Runtime.LibC.CWctypeT
wctype_t_fun = hs_bindgen_72885cbcc8213893

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_char16_t_fun@
foreign import ccall unsafe "hs_bindgen_4284e8eb72d43965" hs_bindgen_4284e8eb72d43965_base ::
     IO GHC.Word.Word16

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_char16_t_fun@
hs_bindgen_4284e8eb72d43965 :: IO HsBindgen.Runtime.LibC.CChar16T
hs_bindgen_4284e8eb72d43965 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_4284e8eb72d43965_base

{-| __C declaration:__ @char16_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 46:10@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
char16_t_fun :: IO HsBindgen.Runtime.LibC.CChar16T
char16_t_fun = hs_bindgen_4284e8eb72d43965

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_char32_t_fun@
foreign import ccall unsafe "hs_bindgen_74e1167907c5de0e" hs_bindgen_74e1167907c5de0e_base ::
     IO GHC.Word.Word32

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_char32_t_fun@
hs_bindgen_74e1167907c5de0e :: IO HsBindgen.Runtime.LibC.CChar32T
hs_bindgen_74e1167907c5de0e =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_74e1167907c5de0e_base

{-| __C declaration:__ @char32_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 47:10@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
char32_t_fun :: IO HsBindgen.Runtime.LibC.CChar32T
char32_t_fun = hs_bindgen_74e1167907c5de0e

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_time_t_fun@
foreign import ccall unsafe "hs_bindgen_a0c949d46b12627a" hs_bindgen_a0c949d46b12627a_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_time_t_fun@
hs_bindgen_a0c949d46b12627a :: IO HsBindgen.Runtime.LibC.CTime
hs_bindgen_a0c949d46b12627a =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_a0c949d46b12627a_base

{-| __C declaration:__ @time_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 51:8@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
time_t_fun :: IO HsBindgen.Runtime.LibC.CTime
time_t_fun = hs_bindgen_a0c949d46b12627a

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_clock_t_fun@
foreign import ccall unsafe "hs_bindgen_ac7afd479db138db" hs_bindgen_ac7afd479db138db_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_clock_t_fun@
hs_bindgen_ac7afd479db138db :: IO HsBindgen.Runtime.LibC.CClock
hs_bindgen_ac7afd479db138db =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_ac7afd479db138db_base

{-| __C declaration:__ @clock_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 52:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
clock_t_fun :: IO HsBindgen.Runtime.LibC.CClock
clock_t_fun = hs_bindgen_ac7afd479db138db

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_FILE_fun@
foreign import ccall unsafe "hs_bindgen_51b6e01d56713f95" hs_bindgen_51b6e01d56713f95_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_FILE_fun@
hs_bindgen_51b6e01d56713f95 :: IO (Ptr.Ptr HsBindgen.Runtime.LibC.CFile)
hs_bindgen_51b6e01d56713f95 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_51b6e01d56713f95_base

{-| __C declaration:__ @FILE_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 58:7@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
fILE_fun :: IO (Ptr.Ptr HsBindgen.Runtime.LibC.CFile)
fILE_fun = hs_bindgen_51b6e01d56713f95

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_fpos_t_fun@
foreign import ccall unsafe "hs_bindgen_e9ed5e9a298e9a7e" hs_bindgen_e9ed5e9a298e9a7e_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_fpos_t_fun@
hs_bindgen_e9ed5e9a298e9a7e :: IO (Ptr.Ptr HsBindgen.Runtime.LibC.CFpos)
hs_bindgen_e9ed5e9a298e9a7e =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_e9ed5e9a298e9a7e_base

{-| __C declaration:__ @fpos_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 59:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
fpos_t_fun :: IO (Ptr.Ptr HsBindgen.Runtime.LibC.CFpos)
fpos_t_fun = hs_bindgen_e9ed5e9a298e9a7e

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_sig_atomic_t_fun@
foreign import ccall unsafe "hs_bindgen_1aecb4fb9faabcf5" hs_bindgen_1aecb4fb9faabcf5_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Unsafe_sig_atomic_t_fun@
hs_bindgen_1aecb4fb9faabcf5 :: IO HsBindgen.Runtime.LibC.CSigAtomic
hs_bindgen_1aecb4fb9faabcf5 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_1aecb4fb9faabcf5_base

{-| __C declaration:__ @sig_atomic_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 63:14@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
sig_atomic_t_fun :: IO HsBindgen.Runtime.LibC.CSigAtomic
sig_atomic_t_fun = hs_bindgen_1aecb4fb9faabcf5
