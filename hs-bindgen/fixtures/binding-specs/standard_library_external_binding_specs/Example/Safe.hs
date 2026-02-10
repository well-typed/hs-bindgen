{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified GHC.Word
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.LibC
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/standard_library_external_binding_specs.h>"
  , "bool hs_bindgen_262e8aa1eac7a016 (void)"
  , "{"
  , "  return bool_fun();"
  , "}"
  , "int8_t hs_bindgen_ff70de79bf2f9862 (void)"
  , "{"
  , "  return int8_t_fun();"
  , "}"
  , "int16_t hs_bindgen_c149cbd27e2cf209 (void)"
  , "{"
  , "  return int16_t_fun();"
  , "}"
  , "int32_t hs_bindgen_d97ef8ef05c29e07 (void)"
  , "{"
  , "  return int32_t_fun();"
  , "}"
  , "int64_t hs_bindgen_84cc0c433f5e3bb6 (void)"
  , "{"
  , "  return int64_t_fun();"
  , "}"
  , "uint8_t hs_bindgen_252cf5f0030dea9a (void)"
  , "{"
  , "  return uint8_t_fun();"
  , "}"
  , "uint16_t hs_bindgen_da9bcc99e8fe0e5e (void)"
  , "{"
  , "  return uint16_t_fun();"
  , "}"
  , "uint32_t hs_bindgen_de65dc64ff5986ee (void)"
  , "{"
  , "  return uint32_t_fun();"
  , "}"
  , "uint64_t hs_bindgen_be0138155abb9a04 (void)"
  , "{"
  , "  return uint64_t_fun();"
  , "}"
  , "int_least8_t hs_bindgen_0043d5d006ffa214 (void)"
  , "{"
  , "  return int_least8_t_fun();"
  , "}"
  , "int_least16_t hs_bindgen_9b8156340f11d00d (void)"
  , "{"
  , "  return int_least16_t_fun();"
  , "}"
  , "int_least32_t hs_bindgen_2914c83d7e6624e9 (void)"
  , "{"
  , "  return int_least32_t_fun();"
  , "}"
  , "int_least64_t hs_bindgen_2631286a56dd050e (void)"
  , "{"
  , "  return int_least64_t_fun();"
  , "}"
  , "uint_least8_t hs_bindgen_ebde9700abb902fd (void)"
  , "{"
  , "  return uint_least8_t_fun();"
  , "}"
  , "uint_least16_t hs_bindgen_f873a66d826c7216 (void)"
  , "{"
  , "  return uint_least16_t_fun();"
  , "}"
  , "uint_least32_t hs_bindgen_decaeedca94fc75c (void)"
  , "{"
  , "  return uint_least32_t_fun();"
  , "}"
  , "uint_least64_t hs_bindgen_28b0c8d29f0851ee (void)"
  , "{"
  , "  return uint_least64_t_fun();"
  , "}"
  , "int_fast8_t hs_bindgen_b7aa33aaedca81d7 (void)"
  , "{"
  , "  return int_fast8_t_fun();"
  , "}"
  , "int_fast32_t hs_bindgen_25656ffcc152d756 (void)"
  , "{"
  , "  return int_fast32_t_fun();"
  , "}"
  , "int_fast64_t hs_bindgen_e8a617e9a9e9cd15 (void)"
  , "{"
  , "  return int_fast64_t_fun();"
  , "}"
  , "uint_fast8_t hs_bindgen_e96abcd52af2967f (void)"
  , "{"
  , "  return uint_fast8_t_fun();"
  , "}"
  , "uint_fast32_t hs_bindgen_cddc66cf3fe8c8f4 (void)"
  , "{"
  , "  return uint_fast32_t_fun();"
  , "}"
  , "uint_fast64_t hs_bindgen_d6a7e470ffc73660 (void)"
  , "{"
  , "  return uint_fast64_t_fun();"
  , "}"
  , "intmax_t hs_bindgen_4a2e0be8c0f07e83 (void)"
  , "{"
  , "  return intmax_t_fun();"
  , "}"
  , "uintmax_t hs_bindgen_dda0cf03aba26ff0 (void)"
  , "{"
  , "  return uintmax_t_fun();"
  , "}"
  , "intptr_t hs_bindgen_ca712b255f45c40e (void)"
  , "{"
  , "  return intptr_t_fun();"
  , "}"
  , "uintptr_t hs_bindgen_624ddd7ec7a31d85 (void)"
  , "{"
  , "  return uintptr_t_fun();"
  , "}"
  , "size_t hs_bindgen_40eea19a42956f94 (void)"
  , "{"
  , "  return size_t_fun();"
  , "}"
  , "ptrdiff_t hs_bindgen_916ccf08da9021d6 (void)"
  , "{"
  , "  return ptrdiff_t_fun();"
  , "}"
  , "wchar_t hs_bindgen_43335779500999a0 (void)"
  , "{"
  , "  return wchar_t_fun();"
  , "}"
  , "wint_t hs_bindgen_852219a0121b41d7 (void)"
  , "{"
  , "  return wint_t_fun();"
  , "}"
  , "wctrans_t hs_bindgen_c53570b502d22ff4 (void)"
  , "{"
  , "  return wctrans_t_fun();"
  , "}"
  , "wctype_t hs_bindgen_6617bb0c70b633c0 (void)"
  , "{"
  , "  return wctype_t_fun();"
  , "}"
  , "char16_t hs_bindgen_39b4c66be46c6190 (void)"
  , "{"
  , "  return char16_t_fun();"
  , "}"
  , "char32_t hs_bindgen_6cbd9aa92f356fbc (void)"
  , "{"
  , "  return char32_t_fun();"
  , "}"
  , "time_t hs_bindgen_b9005ae8ae14b093 (void)"
  , "{"
  , "  return time_t_fun();"
  , "}"
  , "clock_t hs_bindgen_e5521449112e47f8 (void)"
  , "{"
  , "  return clock_t_fun();"
  , "}"
  , "sig_atomic_t hs_bindgen_3023ada5e9267a14 (void)"
  , "{"
  , "  return sig_atomic_t_fun();"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_bool_fun@
foreign import ccall safe "hs_bindgen_262e8aa1eac7a016" hs_bindgen_262e8aa1eac7a016_base ::
     IO GHC.Word.Word8

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_bool_fun@
hs_bindgen_262e8aa1eac7a016 :: IO HsBindgen.Runtime.LibC.CBool
hs_bindgen_262e8aa1eac7a016 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_262e8aa1eac7a016_base

{-| __C declaration:__ @bool_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 7:6@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
bool_fun :: IO HsBindgen.Runtime.LibC.CBool
bool_fun = hs_bindgen_262e8aa1eac7a016

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int8_t_fun@
foreign import ccall safe "hs_bindgen_ff70de79bf2f9862" hs_bindgen_ff70de79bf2f9862_base ::
     IO GHC.Int.Int8

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int8_t_fun@
hs_bindgen_ff70de79bf2f9862 :: IO HsBindgen.Runtime.LibC.Int8
hs_bindgen_ff70de79bf2f9862 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_ff70de79bf2f9862_base

{-| __C declaration:__ @int8_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 11:8@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int8_t_fun :: IO HsBindgen.Runtime.LibC.Int8
int8_t_fun = hs_bindgen_ff70de79bf2f9862

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int16_t_fun@
foreign import ccall safe "hs_bindgen_c149cbd27e2cf209" hs_bindgen_c149cbd27e2cf209_base ::
     IO GHC.Int.Int16

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int16_t_fun@
hs_bindgen_c149cbd27e2cf209 :: IO HsBindgen.Runtime.LibC.Int16
hs_bindgen_c149cbd27e2cf209 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_c149cbd27e2cf209_base

{-| __C declaration:__ @int16_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 12:9@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int16_t_fun :: IO HsBindgen.Runtime.LibC.Int16
int16_t_fun = hs_bindgen_c149cbd27e2cf209

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int32_t_fun@
foreign import ccall safe "hs_bindgen_d97ef8ef05c29e07" hs_bindgen_d97ef8ef05c29e07_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int32_t_fun@
hs_bindgen_d97ef8ef05c29e07 :: IO HsBindgen.Runtime.LibC.Int32
hs_bindgen_d97ef8ef05c29e07 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_d97ef8ef05c29e07_base

{-| __C declaration:__ @int32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 13:9@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int32_t_fun :: IO HsBindgen.Runtime.LibC.Int32
int32_t_fun = hs_bindgen_d97ef8ef05c29e07

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int64_t_fun@
foreign import ccall safe "hs_bindgen_84cc0c433f5e3bb6" hs_bindgen_84cc0c433f5e3bb6_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int64_t_fun@
hs_bindgen_84cc0c433f5e3bb6 :: IO HsBindgen.Runtime.LibC.Int64
hs_bindgen_84cc0c433f5e3bb6 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_84cc0c433f5e3bb6_base

{-| __C declaration:__ @int64_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 14:9@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int64_t_fun :: IO HsBindgen.Runtime.LibC.Int64
int64_t_fun = hs_bindgen_84cc0c433f5e3bb6

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint8_t_fun@
foreign import ccall safe "hs_bindgen_252cf5f0030dea9a" hs_bindgen_252cf5f0030dea9a_base ::
     IO GHC.Word.Word8

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint8_t_fun@
hs_bindgen_252cf5f0030dea9a :: IO HsBindgen.Runtime.LibC.Word8
hs_bindgen_252cf5f0030dea9a =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_252cf5f0030dea9a_base

{-| __C declaration:__ @uint8_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 15:9@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint8_t_fun :: IO HsBindgen.Runtime.LibC.Word8
uint8_t_fun = hs_bindgen_252cf5f0030dea9a

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint16_t_fun@
foreign import ccall safe "hs_bindgen_da9bcc99e8fe0e5e" hs_bindgen_da9bcc99e8fe0e5e_base ::
     IO GHC.Word.Word16

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint16_t_fun@
hs_bindgen_da9bcc99e8fe0e5e :: IO HsBindgen.Runtime.LibC.Word16
hs_bindgen_da9bcc99e8fe0e5e =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_da9bcc99e8fe0e5e_base

{-| __C declaration:__ @uint16_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 16:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint16_t_fun :: IO HsBindgen.Runtime.LibC.Word16
uint16_t_fun = hs_bindgen_da9bcc99e8fe0e5e

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint32_t_fun@
foreign import ccall safe "hs_bindgen_de65dc64ff5986ee" hs_bindgen_de65dc64ff5986ee_base ::
     IO GHC.Word.Word32

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint32_t_fun@
hs_bindgen_de65dc64ff5986ee :: IO HsBindgen.Runtime.LibC.Word32
hs_bindgen_de65dc64ff5986ee =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_de65dc64ff5986ee_base

{-| __C declaration:__ @uint32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 17:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint32_t_fun :: IO HsBindgen.Runtime.LibC.Word32
uint32_t_fun = hs_bindgen_de65dc64ff5986ee

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint64_t_fun@
foreign import ccall safe "hs_bindgen_be0138155abb9a04" hs_bindgen_be0138155abb9a04_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint64_t_fun@
hs_bindgen_be0138155abb9a04 :: IO HsBindgen.Runtime.LibC.Word64
hs_bindgen_be0138155abb9a04 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_be0138155abb9a04_base

{-| __C declaration:__ @uint64_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 18:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint64_t_fun :: IO HsBindgen.Runtime.LibC.Word64
uint64_t_fun = hs_bindgen_be0138155abb9a04

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int_least8_t_fun@
foreign import ccall safe "hs_bindgen_0043d5d006ffa214" hs_bindgen_0043d5d006ffa214_base ::
     IO GHC.Int.Int8

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int_least8_t_fun@
hs_bindgen_0043d5d006ffa214 :: IO HsBindgen.Runtime.LibC.Int8
hs_bindgen_0043d5d006ffa214 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_0043d5d006ffa214_base

{-| __C declaration:__ @int_least8_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 19:14@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_least8_t_fun :: IO HsBindgen.Runtime.LibC.Int8
int_least8_t_fun = hs_bindgen_0043d5d006ffa214

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int_least16_t_fun@
foreign import ccall safe "hs_bindgen_9b8156340f11d00d" hs_bindgen_9b8156340f11d00d_base ::
     IO GHC.Int.Int16

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int_least16_t_fun@
hs_bindgen_9b8156340f11d00d :: IO HsBindgen.Runtime.LibC.Int16
hs_bindgen_9b8156340f11d00d =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_9b8156340f11d00d_base

{-| __C declaration:__ @int_least16_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 20:15@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_least16_t_fun :: IO HsBindgen.Runtime.LibC.Int16
int_least16_t_fun = hs_bindgen_9b8156340f11d00d

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int_least32_t_fun@
foreign import ccall safe "hs_bindgen_2914c83d7e6624e9" hs_bindgen_2914c83d7e6624e9_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int_least32_t_fun@
hs_bindgen_2914c83d7e6624e9 :: IO HsBindgen.Runtime.LibC.Int32
hs_bindgen_2914c83d7e6624e9 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_2914c83d7e6624e9_base

{-| __C declaration:__ @int_least32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 21:15@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_least32_t_fun :: IO HsBindgen.Runtime.LibC.Int32
int_least32_t_fun = hs_bindgen_2914c83d7e6624e9

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int_least64_t_fun@
foreign import ccall safe "hs_bindgen_2631286a56dd050e" hs_bindgen_2631286a56dd050e_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int_least64_t_fun@
hs_bindgen_2631286a56dd050e :: IO HsBindgen.Runtime.LibC.Int64
hs_bindgen_2631286a56dd050e =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_2631286a56dd050e_base

{-| __C declaration:__ @int_least64_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 22:15@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_least64_t_fun :: IO HsBindgen.Runtime.LibC.Int64
int_least64_t_fun = hs_bindgen_2631286a56dd050e

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint_least8_t_fun@
foreign import ccall safe "hs_bindgen_ebde9700abb902fd" hs_bindgen_ebde9700abb902fd_base ::
     IO GHC.Word.Word8

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint_least8_t_fun@
hs_bindgen_ebde9700abb902fd :: IO HsBindgen.Runtime.LibC.Word8
hs_bindgen_ebde9700abb902fd =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_ebde9700abb902fd_base

{-| __C declaration:__ @uint_least8_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 23:15@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_least8_t_fun :: IO HsBindgen.Runtime.LibC.Word8
uint_least8_t_fun = hs_bindgen_ebde9700abb902fd

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint_least16_t_fun@
foreign import ccall safe "hs_bindgen_f873a66d826c7216" hs_bindgen_f873a66d826c7216_base ::
     IO GHC.Word.Word16

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint_least16_t_fun@
hs_bindgen_f873a66d826c7216 :: IO HsBindgen.Runtime.LibC.Word16
hs_bindgen_f873a66d826c7216 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_f873a66d826c7216_base

{-| __C declaration:__ @uint_least16_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 24:16@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_least16_t_fun :: IO HsBindgen.Runtime.LibC.Word16
uint_least16_t_fun = hs_bindgen_f873a66d826c7216

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint_least32_t_fun@
foreign import ccall safe "hs_bindgen_decaeedca94fc75c" hs_bindgen_decaeedca94fc75c_base ::
     IO GHC.Word.Word32

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint_least32_t_fun@
hs_bindgen_decaeedca94fc75c :: IO HsBindgen.Runtime.LibC.Word32
hs_bindgen_decaeedca94fc75c =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_decaeedca94fc75c_base

{-| __C declaration:__ @uint_least32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 25:16@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_least32_t_fun :: IO HsBindgen.Runtime.LibC.Word32
uint_least32_t_fun = hs_bindgen_decaeedca94fc75c

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint_least64_t_fun@
foreign import ccall safe "hs_bindgen_28b0c8d29f0851ee" hs_bindgen_28b0c8d29f0851ee_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint_least64_t_fun@
hs_bindgen_28b0c8d29f0851ee :: IO HsBindgen.Runtime.LibC.Word64
hs_bindgen_28b0c8d29f0851ee =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_28b0c8d29f0851ee_base

{-| __C declaration:__ @uint_least64_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 26:16@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_least64_t_fun :: IO HsBindgen.Runtime.LibC.Word64
uint_least64_t_fun = hs_bindgen_28b0c8d29f0851ee

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int_fast8_t_fun@
foreign import ccall safe "hs_bindgen_b7aa33aaedca81d7" hs_bindgen_b7aa33aaedca81d7_base ::
     IO GHC.Int.Int8

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int_fast8_t_fun@
hs_bindgen_b7aa33aaedca81d7 :: IO HsBindgen.Runtime.LibC.Int8
hs_bindgen_b7aa33aaedca81d7 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_b7aa33aaedca81d7_base

{-| __C declaration:__ @int_fast8_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 27:13@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_fast8_t_fun :: IO HsBindgen.Runtime.LibC.Int8
int_fast8_t_fun = hs_bindgen_b7aa33aaedca81d7

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int_fast32_t_fun@
foreign import ccall safe "hs_bindgen_25656ffcc152d756" hs_bindgen_25656ffcc152d756_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int_fast32_t_fun@
hs_bindgen_25656ffcc152d756 :: IO HsBindgen.Runtime.LibC.Int32
hs_bindgen_25656ffcc152d756 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_25656ffcc152d756_base

{-| __C declaration:__ @int_fast32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 30:14@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_fast32_t_fun :: IO HsBindgen.Runtime.LibC.Int32
int_fast32_t_fun = hs_bindgen_25656ffcc152d756

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int_fast64_t_fun@
foreign import ccall safe "hs_bindgen_e8a617e9a9e9cd15" hs_bindgen_e8a617e9a9e9cd15_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_int_fast64_t_fun@
hs_bindgen_e8a617e9a9e9cd15 :: IO HsBindgen.Runtime.LibC.Int64
hs_bindgen_e8a617e9a9e9cd15 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_e8a617e9a9e9cd15_base

{-| __C declaration:__ @int_fast64_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 31:14@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_fast64_t_fun :: IO HsBindgen.Runtime.LibC.Int64
int_fast64_t_fun = hs_bindgen_e8a617e9a9e9cd15

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint_fast8_t_fun@
foreign import ccall safe "hs_bindgen_e96abcd52af2967f" hs_bindgen_e96abcd52af2967f_base ::
     IO GHC.Word.Word8

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint_fast8_t_fun@
hs_bindgen_e96abcd52af2967f :: IO HsBindgen.Runtime.LibC.Word8
hs_bindgen_e96abcd52af2967f =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_e96abcd52af2967f_base

{-| __C declaration:__ @uint_fast8_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 32:14@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_fast8_t_fun :: IO HsBindgen.Runtime.LibC.Word8
uint_fast8_t_fun = hs_bindgen_e96abcd52af2967f

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint_fast32_t_fun@
foreign import ccall safe "hs_bindgen_cddc66cf3fe8c8f4" hs_bindgen_cddc66cf3fe8c8f4_base ::
     IO GHC.Word.Word32

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint_fast32_t_fun@
hs_bindgen_cddc66cf3fe8c8f4 :: IO HsBindgen.Runtime.LibC.Word32
hs_bindgen_cddc66cf3fe8c8f4 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_cddc66cf3fe8c8f4_base

{-| __C declaration:__ @uint_fast32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 35:15@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_fast32_t_fun :: IO HsBindgen.Runtime.LibC.Word32
uint_fast32_t_fun = hs_bindgen_cddc66cf3fe8c8f4

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint_fast64_t_fun@
foreign import ccall safe "hs_bindgen_d6a7e470ffc73660" hs_bindgen_d6a7e470ffc73660_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uint_fast64_t_fun@
hs_bindgen_d6a7e470ffc73660 :: IO HsBindgen.Runtime.LibC.Word64
hs_bindgen_d6a7e470ffc73660 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_d6a7e470ffc73660_base

{-| __C declaration:__ @uint_fast64_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 36:15@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_fast64_t_fun :: IO HsBindgen.Runtime.LibC.Word64
uint_fast64_t_fun = hs_bindgen_d6a7e470ffc73660

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_intmax_t_fun@
foreign import ccall safe "hs_bindgen_4a2e0be8c0f07e83" hs_bindgen_4a2e0be8c0f07e83_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_intmax_t_fun@
hs_bindgen_4a2e0be8c0f07e83 :: IO HsBindgen.Runtime.LibC.CIntMax
hs_bindgen_4a2e0be8c0f07e83 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_4a2e0be8c0f07e83_base

{-| __C declaration:__ @intmax_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 37:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
intmax_t_fun :: IO HsBindgen.Runtime.LibC.CIntMax
intmax_t_fun = hs_bindgen_4a2e0be8c0f07e83

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uintmax_t_fun@
foreign import ccall safe "hs_bindgen_dda0cf03aba26ff0" hs_bindgen_dda0cf03aba26ff0_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uintmax_t_fun@
hs_bindgen_dda0cf03aba26ff0 :: IO HsBindgen.Runtime.LibC.CUIntMax
hs_bindgen_dda0cf03aba26ff0 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_dda0cf03aba26ff0_base

{-| __C declaration:__ @uintmax_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 38:11@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uintmax_t_fun :: IO HsBindgen.Runtime.LibC.CUIntMax
uintmax_t_fun = hs_bindgen_dda0cf03aba26ff0

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_intptr_t_fun@
foreign import ccall safe "hs_bindgen_ca712b255f45c40e" hs_bindgen_ca712b255f45c40e_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_intptr_t_fun@
hs_bindgen_ca712b255f45c40e :: IO HsBindgen.Runtime.LibC.CIntPtr
hs_bindgen_ca712b255f45c40e =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_ca712b255f45c40e_base

{-| __C declaration:__ @intptr_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 39:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
intptr_t_fun :: IO HsBindgen.Runtime.LibC.CIntPtr
intptr_t_fun = hs_bindgen_ca712b255f45c40e

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uintptr_t_fun@
foreign import ccall safe "hs_bindgen_624ddd7ec7a31d85" hs_bindgen_624ddd7ec7a31d85_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_uintptr_t_fun@
hs_bindgen_624ddd7ec7a31d85 :: IO HsBindgen.Runtime.LibC.CUIntPtr
hs_bindgen_624ddd7ec7a31d85 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_624ddd7ec7a31d85_base

{-| __C declaration:__ @uintptr_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 40:11@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uintptr_t_fun :: IO HsBindgen.Runtime.LibC.CUIntPtr
uintptr_t_fun = hs_bindgen_624ddd7ec7a31d85

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_size_t_fun@
foreign import ccall safe "hs_bindgen_40eea19a42956f94" hs_bindgen_40eea19a42956f94_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_size_t_fun@
hs_bindgen_40eea19a42956f94 :: IO HsBindgen.Runtime.LibC.CSize
hs_bindgen_40eea19a42956f94 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_40eea19a42956f94_base

{-| __C declaration:__ @size_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 51:8@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
size_t_fun :: IO HsBindgen.Runtime.LibC.CSize
size_t_fun = hs_bindgen_40eea19a42956f94

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_ptrdiff_t_fun@
foreign import ccall safe "hs_bindgen_916ccf08da9021d6" hs_bindgen_916ccf08da9021d6_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_ptrdiff_t_fun@
hs_bindgen_916ccf08da9021d6 :: IO HsBindgen.Runtime.LibC.CPtrdiff
hs_bindgen_916ccf08da9021d6 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_916ccf08da9021d6_base

{-| __C declaration:__ @ptrdiff_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 52:11@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
ptrdiff_t_fun :: IO HsBindgen.Runtime.LibC.CPtrdiff
ptrdiff_t_fun = hs_bindgen_916ccf08da9021d6

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_wchar_t_fun@
foreign import ccall safe "hs_bindgen_43335779500999a0" hs_bindgen_43335779500999a0_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_wchar_t_fun@
hs_bindgen_43335779500999a0 :: IO HsBindgen.Runtime.LibC.CWchar
hs_bindgen_43335779500999a0 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_43335779500999a0_base

{-| __C declaration:__ @wchar_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 63:9@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
wchar_t_fun :: IO HsBindgen.Runtime.LibC.CWchar
wchar_t_fun = hs_bindgen_43335779500999a0

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_wint_t_fun@
foreign import ccall safe "hs_bindgen_852219a0121b41d7" hs_bindgen_852219a0121b41d7_base ::
     IO GHC.Word.Word32

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_wint_t_fun@
hs_bindgen_852219a0121b41d7 :: IO HsBindgen.Runtime.LibC.CWintT
hs_bindgen_852219a0121b41d7 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_852219a0121b41d7_base

{-| __C declaration:__ @wint_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 64:8@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
wint_t_fun :: IO HsBindgen.Runtime.LibC.CWintT
wint_t_fun = hs_bindgen_852219a0121b41d7

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_wctrans_t_fun@
foreign import ccall safe "hs_bindgen_c53570b502d22ff4" hs_bindgen_c53570b502d22ff4_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_wctrans_t_fun@
hs_bindgen_c53570b502d22ff4 :: IO HsBindgen.Runtime.LibC.CWctransT
hs_bindgen_c53570b502d22ff4 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_c53570b502d22ff4_base

{-| __C declaration:__ @wctrans_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 67:11@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
wctrans_t_fun :: IO HsBindgen.Runtime.LibC.CWctransT
wctrans_t_fun = hs_bindgen_c53570b502d22ff4

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_wctype_t_fun@
foreign import ccall safe "hs_bindgen_6617bb0c70b633c0" hs_bindgen_6617bb0c70b633c0_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_wctype_t_fun@
hs_bindgen_6617bb0c70b633c0 :: IO HsBindgen.Runtime.LibC.CWctypeT
hs_bindgen_6617bb0c70b633c0 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_6617bb0c70b633c0_base

{-| __C declaration:__ @wctype_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 68:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
wctype_t_fun :: IO HsBindgen.Runtime.LibC.CWctypeT
wctype_t_fun = hs_bindgen_6617bb0c70b633c0

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_char16_t_fun@
foreign import ccall safe "hs_bindgen_39b4c66be46c6190" hs_bindgen_39b4c66be46c6190_base ::
     IO GHC.Word.Word16

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_char16_t_fun@
hs_bindgen_39b4c66be46c6190 :: IO HsBindgen.Runtime.LibC.CChar16T
hs_bindgen_39b4c66be46c6190 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_39b4c66be46c6190_base

{-| __C declaration:__ @char16_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 69:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
char16_t_fun :: IO HsBindgen.Runtime.LibC.CChar16T
char16_t_fun = hs_bindgen_39b4c66be46c6190

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_char32_t_fun@
foreign import ccall safe "hs_bindgen_6cbd9aa92f356fbc" hs_bindgen_6cbd9aa92f356fbc_base ::
     IO GHC.Word.Word32

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_char32_t_fun@
hs_bindgen_6cbd9aa92f356fbc :: IO HsBindgen.Runtime.LibC.CChar32T
hs_bindgen_6cbd9aa92f356fbc =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_6cbd9aa92f356fbc_base

{-| __C declaration:__ @char32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 70:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
char32_t_fun :: IO HsBindgen.Runtime.LibC.CChar32T
char32_t_fun = hs_bindgen_6cbd9aa92f356fbc

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_time_t_fun@
foreign import ccall safe "hs_bindgen_b9005ae8ae14b093" hs_bindgen_b9005ae8ae14b093_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_time_t_fun@
hs_bindgen_b9005ae8ae14b093 :: IO HsBindgen.Runtime.LibC.CTime
hs_bindgen_b9005ae8ae14b093 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_b9005ae8ae14b093_base

{-| __C declaration:__ @time_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 74:8@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
time_t_fun :: IO HsBindgen.Runtime.LibC.CTime
time_t_fun = hs_bindgen_b9005ae8ae14b093

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_clock_t_fun@
foreign import ccall safe "hs_bindgen_e5521449112e47f8" hs_bindgen_e5521449112e47f8_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_clock_t_fun@
hs_bindgen_e5521449112e47f8 :: IO HsBindgen.Runtime.LibC.CClock
hs_bindgen_e5521449112e47f8 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_e5521449112e47f8_base

{-| __C declaration:__ @clock_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 75:9@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
clock_t_fun :: IO HsBindgen.Runtime.LibC.CClock
clock_t_fun = hs_bindgen_e5521449112e47f8

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_sig_atomic_t_fun@
foreign import ccall safe "hs_bindgen_3023ada5e9267a14" hs_bindgen_3023ada5e9267a14_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_bindingspecsstandard_library_Example_Safe_sig_atomic_t_fun@
hs_bindgen_3023ada5e9267a14 :: IO HsBindgen.Runtime.LibC.CSigAtomic
hs_bindgen_3023ada5e9267a14 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_3023ada5e9267a14_base

{-| __C declaration:__ @sig_atomic_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 88:14@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
sig_atomic_t_fun :: IO HsBindgen.Runtime.LibC.CSigAtomic
sig_atomic_t_fun = hs_bindgen_3023ada5e9267a14
