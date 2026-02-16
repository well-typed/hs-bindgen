{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.LibC

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/stdlib/return_values.h>"
  , "bool hs_bindgen_4cd199ba56c14547 (void)"
  , "{"
  , "  return bool_fun();"
  , "}"
  , "int8_t hs_bindgen_73c8e1f452482878 (void)"
  , "{"
  , "  return int8_t_fun();"
  , "}"
  , "int16_t hs_bindgen_da3259b2446eb9bf (void)"
  , "{"
  , "  return int16_t_fun();"
  , "}"
  , "int32_t hs_bindgen_7dd577f0c34f0ff7 (void)"
  , "{"
  , "  return int32_t_fun();"
  , "}"
  , "int64_t hs_bindgen_7a2709fa911a350e (void)"
  , "{"
  , "  return int64_t_fun();"
  , "}"
  , "uint8_t hs_bindgen_1b2e687eaa715924 (void)"
  , "{"
  , "  return uint8_t_fun();"
  , "}"
  , "uint16_t hs_bindgen_6da73658633b176e (void)"
  , "{"
  , "  return uint16_t_fun();"
  , "}"
  , "uint32_t hs_bindgen_c6cd0ee414c746de (void)"
  , "{"
  , "  return uint32_t_fun();"
  , "}"
  , "uint64_t hs_bindgen_cbb9017c67ce7652 (void)"
  , "{"
  , "  return uint64_t_fun();"
  , "}"
  , "intmax_t hs_bindgen_844bf55e16de43a0 (void)"
  , "{"
  , "  return intmax_t_fun();"
  , "}"
  , "uintmax_t hs_bindgen_56900c9a548480d5 (void)"
  , "{"
  , "  return uintmax_t_fun();"
  , "}"
  , "intptr_t hs_bindgen_d656827fe2704de3 (void)"
  , "{"
  , "  return intptr_t_fun();"
  , "}"
  , "uintptr_t hs_bindgen_401c738c79c5e4ef (void)"
  , "{"
  , "  return uintptr_t_fun();"
  , "}"
  , "fenv_t *hs_bindgen_eb417aeaae001cd4 (void)"
  , "{"
  , "  return fenv_t_fun();"
  , "}"
  , "fexcept_t *hs_bindgen_42f9ee7a2fa5ed3b (void)"
  , "{"
  , "  return fexcept_t_fun();"
  , "}"
  , "size_t hs_bindgen_66f735fc270b795a (void)"
  , "{"
  , "  return size_t_fun();"
  , "}"
  , "ptrdiff_t hs_bindgen_2f5574764965bb49 (void)"
  , "{"
  , "  return ptrdiff_t_fun();"
  , "}"
  , "wchar_t hs_bindgen_57ff329a6c1ab247 (void)"
  , "{"
  , "  return wchar_t_fun();"
  , "}"
  , "wint_t hs_bindgen_fef1399636729a64 (void)"
  , "{"
  , "  return wint_t_fun();"
  , "}"
  , "mbstate_t *hs_bindgen_ec1c0b9428114ec6 (void)"
  , "{"
  , "  return mbstate_t_fun();"
  , "}"
  , "wctrans_t hs_bindgen_2d9b204c006f0667 (void)"
  , "{"
  , "  return wctrans_t_fun();"
  , "}"
  , "wctype_t hs_bindgen_977cddf620e22ba0 (void)"
  , "{"
  , "  return wctype_t_fun();"
  , "}"
  , "char16_t hs_bindgen_4780a15ae4e392fd (void)"
  , "{"
  , "  return char16_t_fun();"
  , "}"
  , "char32_t hs_bindgen_d258f4d9903b4e39 (void)"
  , "{"
  , "  return char32_t_fun();"
  , "}"
  , "time_t hs_bindgen_82c178bd7c4d08b0 (void)"
  , "{"
  , "  return time_t_fun();"
  , "}"
  , "clock_t hs_bindgen_b472ecf9ffec8045 (void)"
  , "{"
  , "  return clock_t_fun();"
  , "}"
  , "FILE *hs_bindgen_0caa7d72a7701f1f (void)"
  , "{"
  , "  return FILE_fun();"
  , "}"
  , "fpos_t *hs_bindgen_47b37de28902f506 (void)"
  , "{"
  , "  return fpos_t_fun();"
  , "}"
  , "sig_atomic_t hs_bindgen_222eab426c700a03 (void)"
  , "{"
  , "  return sig_atomic_t_fun();"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_bool_fun@
foreign import ccall safe "hs_bindgen_4cd199ba56c14547" hs_bindgen_4cd199ba56c14547_base ::
     IO RIP.Word8

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_bool_fun@
hs_bindgen_4cd199ba56c14547 :: IO HsBindgen.Runtime.LibC.CBool
hs_bindgen_4cd199ba56c14547 =
  RIP.fromFFIType hs_bindgen_4cd199ba56c14547_base

{-| __C declaration:__ @bool_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 20:6@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
bool_fun :: IO HsBindgen.Runtime.LibC.CBool
bool_fun = hs_bindgen_4cd199ba56c14547

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_int8_t_fun@
foreign import ccall safe "hs_bindgen_73c8e1f452482878" hs_bindgen_73c8e1f452482878_base ::
     IO RIP.Int8

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_int8_t_fun@
hs_bindgen_73c8e1f452482878 :: IO HsBindgen.Runtime.LibC.Int8
hs_bindgen_73c8e1f452482878 =
  RIP.fromFFIType hs_bindgen_73c8e1f452482878_base

{-| __C declaration:__ @int8_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 23:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
int8_t_fun :: IO HsBindgen.Runtime.LibC.Int8
int8_t_fun = hs_bindgen_73c8e1f452482878

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_int16_t_fun@
foreign import ccall safe "hs_bindgen_da3259b2446eb9bf" hs_bindgen_da3259b2446eb9bf_base ::
     IO RIP.Int16

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_int16_t_fun@
hs_bindgen_da3259b2446eb9bf :: IO HsBindgen.Runtime.LibC.Int16
hs_bindgen_da3259b2446eb9bf =
  RIP.fromFFIType hs_bindgen_da3259b2446eb9bf_base

{-| __C declaration:__ @int16_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 24:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
int16_t_fun :: IO HsBindgen.Runtime.LibC.Int16
int16_t_fun = hs_bindgen_da3259b2446eb9bf

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_int32_t_fun@
foreign import ccall safe "hs_bindgen_7dd577f0c34f0ff7" hs_bindgen_7dd577f0c34f0ff7_base ::
     IO RIP.Int32

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_int32_t_fun@
hs_bindgen_7dd577f0c34f0ff7 :: IO HsBindgen.Runtime.LibC.Int32
hs_bindgen_7dd577f0c34f0ff7 =
  RIP.fromFFIType hs_bindgen_7dd577f0c34f0ff7_base

{-| __C declaration:__ @int32_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 25:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
int32_t_fun :: IO HsBindgen.Runtime.LibC.Int32
int32_t_fun = hs_bindgen_7dd577f0c34f0ff7

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_int64_t_fun@
foreign import ccall safe "hs_bindgen_7a2709fa911a350e" hs_bindgen_7a2709fa911a350e_base ::
     IO RIP.Int64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_int64_t_fun@
hs_bindgen_7a2709fa911a350e :: IO HsBindgen.Runtime.LibC.Int64
hs_bindgen_7a2709fa911a350e =
  RIP.fromFFIType hs_bindgen_7a2709fa911a350e_base

{-| __C declaration:__ @int64_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 26:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
int64_t_fun :: IO HsBindgen.Runtime.LibC.Int64
int64_t_fun = hs_bindgen_7a2709fa911a350e

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_uint8_t_fun@
foreign import ccall safe "hs_bindgen_1b2e687eaa715924" hs_bindgen_1b2e687eaa715924_base ::
     IO RIP.Word8

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_uint8_t_fun@
hs_bindgen_1b2e687eaa715924 :: IO HsBindgen.Runtime.LibC.Word8
hs_bindgen_1b2e687eaa715924 =
  RIP.fromFFIType hs_bindgen_1b2e687eaa715924_base

{-| __C declaration:__ @uint8_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 27:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
uint8_t_fun :: IO HsBindgen.Runtime.LibC.Word8
uint8_t_fun = hs_bindgen_1b2e687eaa715924

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_uint16_t_fun@
foreign import ccall safe "hs_bindgen_6da73658633b176e" hs_bindgen_6da73658633b176e_base ::
     IO RIP.Word16

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_uint16_t_fun@
hs_bindgen_6da73658633b176e :: IO HsBindgen.Runtime.LibC.Word16
hs_bindgen_6da73658633b176e =
  RIP.fromFFIType hs_bindgen_6da73658633b176e_base

{-| __C declaration:__ @uint16_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 28:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
uint16_t_fun :: IO HsBindgen.Runtime.LibC.Word16
uint16_t_fun = hs_bindgen_6da73658633b176e

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_uint32_t_fun@
foreign import ccall safe "hs_bindgen_c6cd0ee414c746de" hs_bindgen_c6cd0ee414c746de_base ::
     IO RIP.Word32

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_uint32_t_fun@
hs_bindgen_c6cd0ee414c746de :: IO HsBindgen.Runtime.LibC.Word32
hs_bindgen_c6cd0ee414c746de =
  RIP.fromFFIType hs_bindgen_c6cd0ee414c746de_base

{-| __C declaration:__ @uint32_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 29:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
uint32_t_fun :: IO HsBindgen.Runtime.LibC.Word32
uint32_t_fun = hs_bindgen_c6cd0ee414c746de

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_uint64_t_fun@
foreign import ccall safe "hs_bindgen_cbb9017c67ce7652" hs_bindgen_cbb9017c67ce7652_base ::
     IO RIP.Word64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_uint64_t_fun@
hs_bindgen_cbb9017c67ce7652 :: IO HsBindgen.Runtime.LibC.Word64
hs_bindgen_cbb9017c67ce7652 =
  RIP.fromFFIType hs_bindgen_cbb9017c67ce7652_base

{-| __C declaration:__ @uint64_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 30:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
uint64_t_fun :: IO HsBindgen.Runtime.LibC.Word64
uint64_t_fun = hs_bindgen_cbb9017c67ce7652

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_intmax_t_fun@
foreign import ccall safe "hs_bindgen_844bf55e16de43a0" hs_bindgen_844bf55e16de43a0_base ::
     IO RIP.Int64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_intmax_t_fun@
hs_bindgen_844bf55e16de43a0 :: IO HsBindgen.Runtime.LibC.CIntMax
hs_bindgen_844bf55e16de43a0 =
  RIP.fromFFIType hs_bindgen_844bf55e16de43a0_base

{-| __C declaration:__ @intmax_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 31:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
intmax_t_fun :: IO HsBindgen.Runtime.LibC.CIntMax
intmax_t_fun = hs_bindgen_844bf55e16de43a0

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_uintmax_t_fun@
foreign import ccall safe "hs_bindgen_56900c9a548480d5" hs_bindgen_56900c9a548480d5_base ::
     IO RIP.Word64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_uintmax_t_fun@
hs_bindgen_56900c9a548480d5 :: IO HsBindgen.Runtime.LibC.CUIntMax
hs_bindgen_56900c9a548480d5 =
  RIP.fromFFIType hs_bindgen_56900c9a548480d5_base

{-| __C declaration:__ @uintmax_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 32:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
uintmax_t_fun :: IO HsBindgen.Runtime.LibC.CUIntMax
uintmax_t_fun = hs_bindgen_56900c9a548480d5

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_intptr_t_fun@
foreign import ccall safe "hs_bindgen_d656827fe2704de3" hs_bindgen_d656827fe2704de3_base ::
     IO RIP.Int64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_intptr_t_fun@
hs_bindgen_d656827fe2704de3 :: IO HsBindgen.Runtime.LibC.CIntPtr
hs_bindgen_d656827fe2704de3 =
  RIP.fromFFIType hs_bindgen_d656827fe2704de3_base

{-| __C declaration:__ @intptr_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 33:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
intptr_t_fun :: IO HsBindgen.Runtime.LibC.CIntPtr
intptr_t_fun = hs_bindgen_d656827fe2704de3

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_uintptr_t_fun@
foreign import ccall safe "hs_bindgen_401c738c79c5e4ef" hs_bindgen_401c738c79c5e4ef_base ::
     IO RIP.Word64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_uintptr_t_fun@
hs_bindgen_401c738c79c5e4ef :: IO HsBindgen.Runtime.LibC.CUIntPtr
hs_bindgen_401c738c79c5e4ef =
  RIP.fromFFIType hs_bindgen_401c738c79c5e4ef_base

{-| __C declaration:__ @uintptr_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 34:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
uintptr_t_fun :: IO HsBindgen.Runtime.LibC.CUIntPtr
uintptr_t_fun = hs_bindgen_401c738c79c5e4ef

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_fenv_t_fun@
foreign import ccall safe "hs_bindgen_eb417aeaae001cd4" hs_bindgen_eb417aeaae001cd4_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_fenv_t_fun@
hs_bindgen_eb417aeaae001cd4 :: IO (RIP.Ptr HsBindgen.Runtime.LibC.CFenvT)
hs_bindgen_eb417aeaae001cd4 =
  RIP.fromFFIType hs_bindgen_eb417aeaae001cd4_base

{-| __C declaration:__ @fenv_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 37:12@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
fenv_t_fun :: IO (RIP.Ptr HsBindgen.Runtime.LibC.CFenvT)
fenv_t_fun = hs_bindgen_eb417aeaae001cd4

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_fexcept_t_fun@
foreign import ccall safe "hs_bindgen_42f9ee7a2fa5ed3b" hs_bindgen_42f9ee7a2fa5ed3b_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_fexcept_t_fun@
hs_bindgen_42f9ee7a2fa5ed3b :: IO (RIP.Ptr HsBindgen.Runtime.LibC.CFexceptT)
hs_bindgen_42f9ee7a2fa5ed3b =
  RIP.fromFFIType hs_bindgen_42f9ee7a2fa5ed3b_base

{-| __C declaration:__ @fexcept_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 38:12@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
fexcept_t_fun :: IO (RIP.Ptr HsBindgen.Runtime.LibC.CFexceptT)
fexcept_t_fun = hs_bindgen_42f9ee7a2fa5ed3b

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_size_t_fun@
foreign import ccall safe "hs_bindgen_66f735fc270b795a" hs_bindgen_66f735fc270b795a_base ::
     IO RIP.Word64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_size_t_fun@
hs_bindgen_66f735fc270b795a :: IO HsBindgen.Runtime.LibC.CSize
hs_bindgen_66f735fc270b795a =
  RIP.fromFFIType hs_bindgen_66f735fc270b795a_base

{-| __C declaration:__ @size_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 41:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
size_t_fun :: IO HsBindgen.Runtime.LibC.CSize
size_t_fun = hs_bindgen_66f735fc270b795a

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_ptrdiff_t_fun@
foreign import ccall safe "hs_bindgen_2f5574764965bb49" hs_bindgen_2f5574764965bb49_base ::
     IO RIP.Int64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_ptrdiff_t_fun@
hs_bindgen_2f5574764965bb49 :: IO HsBindgen.Runtime.LibC.CPtrdiff
hs_bindgen_2f5574764965bb49 =
  RIP.fromFFIType hs_bindgen_2f5574764965bb49_base

{-| __C declaration:__ @ptrdiff_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 42:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
ptrdiff_t_fun :: IO HsBindgen.Runtime.LibC.CPtrdiff
ptrdiff_t_fun = hs_bindgen_2f5574764965bb49

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_wchar_t_fun@
foreign import ccall safe "hs_bindgen_57ff329a6c1ab247" hs_bindgen_57ff329a6c1ab247_base ::
     IO RIP.Int32

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_wchar_t_fun@
hs_bindgen_57ff329a6c1ab247 :: IO HsBindgen.Runtime.LibC.CWchar
hs_bindgen_57ff329a6c1ab247 =
  RIP.fromFFIType hs_bindgen_57ff329a6c1ab247_base

{-| __C declaration:__ @wchar_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 49:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
wchar_t_fun :: IO HsBindgen.Runtime.LibC.CWchar
wchar_t_fun = hs_bindgen_57ff329a6c1ab247

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_wint_t_fun@
foreign import ccall safe "hs_bindgen_fef1399636729a64" hs_bindgen_fef1399636729a64_base ::
     IO RIP.Word32

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_wint_t_fun@
hs_bindgen_fef1399636729a64 :: IO HsBindgen.Runtime.LibC.CWintT
hs_bindgen_fef1399636729a64 =
  RIP.fromFFIType hs_bindgen_fef1399636729a64_base

{-| __C declaration:__ @wint_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 50:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
wint_t_fun :: IO HsBindgen.Runtime.LibC.CWintT
wint_t_fun = hs_bindgen_fef1399636729a64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_mbstate_t_fun@
foreign import ccall safe "hs_bindgen_ec1c0b9428114ec6" hs_bindgen_ec1c0b9428114ec6_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_mbstate_t_fun@
hs_bindgen_ec1c0b9428114ec6 :: IO (RIP.Ptr HsBindgen.Runtime.LibC.CMbstateT)
hs_bindgen_ec1c0b9428114ec6 =
  RIP.fromFFIType hs_bindgen_ec1c0b9428114ec6_base

{-| __C declaration:__ @mbstate_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 51:12@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
mbstate_t_fun :: IO (RIP.Ptr HsBindgen.Runtime.LibC.CMbstateT)
mbstate_t_fun = hs_bindgen_ec1c0b9428114ec6

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_wctrans_t_fun@
foreign import ccall safe "hs_bindgen_2d9b204c006f0667" hs_bindgen_2d9b204c006f0667_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_wctrans_t_fun@
hs_bindgen_2d9b204c006f0667 :: IO HsBindgen.Runtime.LibC.CWctransT
hs_bindgen_2d9b204c006f0667 =
  RIP.fromFFIType hs_bindgen_2d9b204c006f0667_base

{-| __C declaration:__ @wctrans_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 52:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
wctrans_t_fun :: IO HsBindgen.Runtime.LibC.CWctransT
wctrans_t_fun = hs_bindgen_2d9b204c006f0667

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_wctype_t_fun@
foreign import ccall safe "hs_bindgen_977cddf620e22ba0" hs_bindgen_977cddf620e22ba0_base ::
     IO RIP.Word64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_wctype_t_fun@
hs_bindgen_977cddf620e22ba0 :: IO HsBindgen.Runtime.LibC.CWctypeT
hs_bindgen_977cddf620e22ba0 =
  RIP.fromFFIType hs_bindgen_977cddf620e22ba0_base

{-| __C declaration:__ @wctype_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 53:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
wctype_t_fun :: IO HsBindgen.Runtime.LibC.CWctypeT
wctype_t_fun = hs_bindgen_977cddf620e22ba0

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_char16_t_fun@
foreign import ccall safe "hs_bindgen_4780a15ae4e392fd" hs_bindgen_4780a15ae4e392fd_base ::
     IO RIP.Word16

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_char16_t_fun@
hs_bindgen_4780a15ae4e392fd :: IO HsBindgen.Runtime.LibC.CChar16T
hs_bindgen_4780a15ae4e392fd =
  RIP.fromFFIType hs_bindgen_4780a15ae4e392fd_base

{-| __C declaration:__ @char16_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 54:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
char16_t_fun :: IO HsBindgen.Runtime.LibC.CChar16T
char16_t_fun = hs_bindgen_4780a15ae4e392fd

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_char32_t_fun@
foreign import ccall safe "hs_bindgen_d258f4d9903b4e39" hs_bindgen_d258f4d9903b4e39_base ::
     IO RIP.Word32

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_char32_t_fun@
hs_bindgen_d258f4d9903b4e39 :: IO HsBindgen.Runtime.LibC.CChar32T
hs_bindgen_d258f4d9903b4e39 =
  RIP.fromFFIType hs_bindgen_d258f4d9903b4e39_base

{-| __C declaration:__ @char32_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 55:11@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
char32_t_fun :: IO HsBindgen.Runtime.LibC.CChar32T
char32_t_fun = hs_bindgen_d258f4d9903b4e39

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_time_t_fun@
foreign import ccall safe "hs_bindgen_82c178bd7c4d08b0" hs_bindgen_82c178bd7c4d08b0_base ::
     IO RIP.Int64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_time_t_fun@
hs_bindgen_82c178bd7c4d08b0 :: IO HsBindgen.Runtime.LibC.CTime
hs_bindgen_82c178bd7c4d08b0 =
  RIP.fromFFIType hs_bindgen_82c178bd7c4d08b0_base

{-| __C declaration:__ @time_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 58:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
time_t_fun :: IO HsBindgen.Runtime.LibC.CTime
time_t_fun = hs_bindgen_82c178bd7c4d08b0

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_clock_t_fun@
foreign import ccall safe "hs_bindgen_b472ecf9ffec8045" hs_bindgen_b472ecf9ffec8045_base ::
     IO RIP.Int64

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_clock_t_fun@
hs_bindgen_b472ecf9ffec8045 :: IO HsBindgen.Runtime.LibC.CClock
hs_bindgen_b472ecf9ffec8045 =
  RIP.fromFFIType hs_bindgen_b472ecf9ffec8045_base

{-| __C declaration:__ @clock_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 59:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
clock_t_fun :: IO HsBindgen.Runtime.LibC.CClock
clock_t_fun = hs_bindgen_b472ecf9ffec8045

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_FILE_fun@
foreign import ccall safe "hs_bindgen_0caa7d72a7701f1f" hs_bindgen_0caa7d72a7701f1f_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_FILE_fun@
hs_bindgen_0caa7d72a7701f1f :: IO (RIP.Ptr HsBindgen.Runtime.LibC.CFile)
hs_bindgen_0caa7d72a7701f1f =
  RIP.fromFFIType hs_bindgen_0caa7d72a7701f1f_base

{-| __C declaration:__ @FILE_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 64:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
fILE_fun :: IO (RIP.Ptr HsBindgen.Runtime.LibC.CFile)
fILE_fun = hs_bindgen_0caa7d72a7701f1f

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_fpos_t_fun@
foreign import ccall safe "hs_bindgen_47b37de28902f506" hs_bindgen_47b37de28902f506_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_fpos_t_fun@
hs_bindgen_47b37de28902f506 :: IO (RIP.Ptr HsBindgen.Runtime.LibC.CFpos)
hs_bindgen_47b37de28902f506 =
  RIP.fromFFIType hs_bindgen_47b37de28902f506_base

{-| __C declaration:__ @fpos_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 65:9@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
fpos_t_fun :: IO (RIP.Ptr HsBindgen.Runtime.LibC.CFpos)
fpos_t_fun = hs_bindgen_47b37de28902f506

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_sig_atomic_t_fun@
foreign import ccall safe "hs_bindgen_222eab426c700a03" hs_bindgen_222eab426c700a03_base ::
     IO RIP.Int32

-- __unique:__ @test_bindingspecsstdlibreturn_va_Example_Safe_sig_atomic_t_fun@
hs_bindgen_222eab426c700a03 :: IO HsBindgen.Runtime.LibC.CSigAtomic
hs_bindgen_222eab426c700a03 =
  RIP.fromFFIType hs_bindgen_222eab426c700a03_base

{-| __C declaration:__ @sig_atomic_t_fun@

    __defined at:__ @binding-specs\/stdlib\/return_values.h 68:14@

    __exported by:__ @binding-specs\/stdlib\/return_values.h@
-}
sig_atomic_t_fun :: IO HsBindgen.Runtime.LibC.CSigAtomic
sig_atomic_t_fun = hs_bindgen_222eab426c700a03
