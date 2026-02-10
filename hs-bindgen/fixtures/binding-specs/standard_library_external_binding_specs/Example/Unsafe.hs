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
  [ "#include <binding-specs/standard_library_external_binding_specs.h>"
  , "bool hs_bindgen_fddc3c29f5fef9cb (void)"
  , "{"
  , "  return bool_fun();"
  , "}"
  , "int8_t hs_bindgen_ae5edfeb524aaa2b (void)"
  , "{"
  , "  return int8_t_fun();"
  , "}"
  , "int16_t hs_bindgen_9a81f182cddccf3b (void)"
  , "{"
  , "  return int16_t_fun();"
  , "}"
  , "int32_t hs_bindgen_f36eee2b99414771 (void)"
  , "{"
  , "  return int32_t_fun();"
  , "}"
  , "int64_t hs_bindgen_48be99faf540934c (void)"
  , "{"
  , "  return int64_t_fun();"
  , "}"
  , "uint8_t hs_bindgen_ae088da4a5427da9 (void)"
  , "{"
  , "  return uint8_t_fun();"
  , "}"
  , "uint16_t hs_bindgen_a409b5e4c8f08bab (void)"
  , "{"
  , "  return uint16_t_fun();"
  , "}"
  , "uint32_t hs_bindgen_d3046bb79999c5ee (void)"
  , "{"
  , "  return uint32_t_fun();"
  , "}"
  , "uint64_t hs_bindgen_f0baafcba3ef341f (void)"
  , "{"
  , "  return uint64_t_fun();"
  , "}"
  , "int_least8_t hs_bindgen_42d43898e2bbce55 (void)"
  , "{"
  , "  return int_least8_t_fun();"
  , "}"
  , "int_least16_t hs_bindgen_6350d76366b7c38a (void)"
  , "{"
  , "  return int_least16_t_fun();"
  , "}"
  , "int_least32_t hs_bindgen_282f4da244daeeb3 (void)"
  , "{"
  , "  return int_least32_t_fun();"
  , "}"
  , "int_least64_t hs_bindgen_a992b9370875bc4f (void)"
  , "{"
  , "  return int_least64_t_fun();"
  , "}"
  , "uint_least8_t hs_bindgen_9143c626ba1553fc (void)"
  , "{"
  , "  return uint_least8_t_fun();"
  , "}"
  , "uint_least16_t hs_bindgen_b48d2270e32f25ec (void)"
  , "{"
  , "  return uint_least16_t_fun();"
  , "}"
  , "uint_least32_t hs_bindgen_e49bc74d9fb4983d (void)"
  , "{"
  , "  return uint_least32_t_fun();"
  , "}"
  , "uint_least64_t hs_bindgen_8cd2ab25e650607b (void)"
  , "{"
  , "  return uint_least64_t_fun();"
  , "}"
  , "int_fast8_t hs_bindgen_0a79089430ad992e (void)"
  , "{"
  , "  return int_fast8_t_fun();"
  , "}"
  , "int_fast32_t hs_bindgen_73fb6d800ee5680a (void)"
  , "{"
  , "  return int_fast32_t_fun();"
  , "}"
  , "int_fast64_t hs_bindgen_13ca37318a4b7ff5 (void)"
  , "{"
  , "  return int_fast64_t_fun();"
  , "}"
  , "uint_fast8_t hs_bindgen_f090a3b4bbf12f04 (void)"
  , "{"
  , "  return uint_fast8_t_fun();"
  , "}"
  , "uint_fast32_t hs_bindgen_59209267124fbc45 (void)"
  , "{"
  , "  return uint_fast32_t_fun();"
  , "}"
  , "uint_fast64_t hs_bindgen_a78f57cdabeb1ca1 (void)"
  , "{"
  , "  return uint_fast64_t_fun();"
  , "}"
  , "intmax_t hs_bindgen_ac33d7503cd16cf9 (void)"
  , "{"
  , "  return intmax_t_fun();"
  , "}"
  , "uintmax_t hs_bindgen_c729cac1dced98eb (void)"
  , "{"
  , "  return uintmax_t_fun();"
  , "}"
  , "intptr_t hs_bindgen_c0b84d83fb5eb309 (void)"
  , "{"
  , "  return intptr_t_fun();"
  , "}"
  , "uintptr_t hs_bindgen_2c88bc97436e95f5 (void)"
  , "{"
  , "  return uintptr_t_fun();"
  , "}"
  , "size_t hs_bindgen_8c7a9840d42953ec (void)"
  , "{"
  , "  return size_t_fun();"
  , "}"
  , "ptrdiff_t hs_bindgen_bfc4a1bcb4b291a5 (void)"
  , "{"
  , "  return ptrdiff_t_fun();"
  , "}"
  , "wchar_t hs_bindgen_fc2d05ee4c0cfb45 (void)"
  , "{"
  , "  return wchar_t_fun();"
  , "}"
  , "wint_t hs_bindgen_5d91844f4b4fd8b3 (void)"
  , "{"
  , "  return wint_t_fun();"
  , "}"
  , "wctrans_t hs_bindgen_3592bacf3bbee0f8 (void)"
  , "{"
  , "  return wctrans_t_fun();"
  , "}"
  , "wctype_t hs_bindgen_30121c3526c3ac78 (void)"
  , "{"
  , "  return wctype_t_fun();"
  , "}"
  , "char16_t hs_bindgen_4fd0150e665ead24 (void)"
  , "{"
  , "  return char16_t_fun();"
  , "}"
  , "char32_t hs_bindgen_2c0f7df3bd2fe4df (void)"
  , "{"
  , "  return char32_t_fun();"
  , "}"
  , "time_t hs_bindgen_ab883c49289a0016 (void)"
  , "{"
  , "  return time_t_fun();"
  , "}"
  , "clock_t hs_bindgen_eb18d400828e034b (void)"
  , "{"
  , "  return clock_t_fun();"
  , "}"
  , "sig_atomic_t hs_bindgen_5b52ae35b955c28f (void)"
  , "{"
  , "  return sig_atomic_t_fun();"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_bool_fun@
foreign import ccall unsafe "hs_bindgen_fddc3c29f5fef9cb" hs_bindgen_fddc3c29f5fef9cb_base ::
     IO GHC.Word.Word8

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_bool_fun@
hs_bindgen_fddc3c29f5fef9cb :: IO HsBindgen.Runtime.LibC.CBool
hs_bindgen_fddc3c29f5fef9cb =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_fddc3c29f5fef9cb_base

{-| __C declaration:__ @bool_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 7:6@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
bool_fun :: IO HsBindgen.Runtime.LibC.CBool
bool_fun = hs_bindgen_fddc3c29f5fef9cb

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int8_t_fun@
foreign import ccall unsafe "hs_bindgen_ae5edfeb524aaa2b" hs_bindgen_ae5edfeb524aaa2b_base ::
     IO GHC.Int.Int8

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int8_t_fun@
hs_bindgen_ae5edfeb524aaa2b :: IO HsBindgen.Runtime.LibC.Int8
hs_bindgen_ae5edfeb524aaa2b =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_ae5edfeb524aaa2b_base

{-| __C declaration:__ @int8_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 11:8@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int8_t_fun :: IO HsBindgen.Runtime.LibC.Int8
int8_t_fun = hs_bindgen_ae5edfeb524aaa2b

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int16_t_fun@
foreign import ccall unsafe "hs_bindgen_9a81f182cddccf3b" hs_bindgen_9a81f182cddccf3b_base ::
     IO GHC.Int.Int16

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int16_t_fun@
hs_bindgen_9a81f182cddccf3b :: IO HsBindgen.Runtime.LibC.Int16
hs_bindgen_9a81f182cddccf3b =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_9a81f182cddccf3b_base

{-| __C declaration:__ @int16_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 12:9@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int16_t_fun :: IO HsBindgen.Runtime.LibC.Int16
int16_t_fun = hs_bindgen_9a81f182cddccf3b

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int32_t_fun@
foreign import ccall unsafe "hs_bindgen_f36eee2b99414771" hs_bindgen_f36eee2b99414771_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int32_t_fun@
hs_bindgen_f36eee2b99414771 :: IO HsBindgen.Runtime.LibC.Int32
hs_bindgen_f36eee2b99414771 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_f36eee2b99414771_base

{-| __C declaration:__ @int32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 13:9@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int32_t_fun :: IO HsBindgen.Runtime.LibC.Int32
int32_t_fun = hs_bindgen_f36eee2b99414771

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int64_t_fun@
foreign import ccall unsafe "hs_bindgen_48be99faf540934c" hs_bindgen_48be99faf540934c_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int64_t_fun@
hs_bindgen_48be99faf540934c :: IO HsBindgen.Runtime.LibC.Int64
hs_bindgen_48be99faf540934c =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_48be99faf540934c_base

{-| __C declaration:__ @int64_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 14:9@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int64_t_fun :: IO HsBindgen.Runtime.LibC.Int64
int64_t_fun = hs_bindgen_48be99faf540934c

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint8_t_fun@
foreign import ccall unsafe "hs_bindgen_ae088da4a5427da9" hs_bindgen_ae088da4a5427da9_base ::
     IO GHC.Word.Word8

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint8_t_fun@
hs_bindgen_ae088da4a5427da9 :: IO HsBindgen.Runtime.LibC.Word8
hs_bindgen_ae088da4a5427da9 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_ae088da4a5427da9_base

{-| __C declaration:__ @uint8_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 15:9@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint8_t_fun :: IO HsBindgen.Runtime.LibC.Word8
uint8_t_fun = hs_bindgen_ae088da4a5427da9

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint16_t_fun@
foreign import ccall unsafe "hs_bindgen_a409b5e4c8f08bab" hs_bindgen_a409b5e4c8f08bab_base ::
     IO GHC.Word.Word16

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint16_t_fun@
hs_bindgen_a409b5e4c8f08bab :: IO HsBindgen.Runtime.LibC.Word16
hs_bindgen_a409b5e4c8f08bab =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_a409b5e4c8f08bab_base

{-| __C declaration:__ @uint16_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 16:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint16_t_fun :: IO HsBindgen.Runtime.LibC.Word16
uint16_t_fun = hs_bindgen_a409b5e4c8f08bab

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint32_t_fun@
foreign import ccall unsafe "hs_bindgen_d3046bb79999c5ee" hs_bindgen_d3046bb79999c5ee_base ::
     IO GHC.Word.Word32

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint32_t_fun@
hs_bindgen_d3046bb79999c5ee :: IO HsBindgen.Runtime.LibC.Word32
hs_bindgen_d3046bb79999c5ee =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_d3046bb79999c5ee_base

{-| __C declaration:__ @uint32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 17:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint32_t_fun :: IO HsBindgen.Runtime.LibC.Word32
uint32_t_fun = hs_bindgen_d3046bb79999c5ee

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint64_t_fun@
foreign import ccall unsafe "hs_bindgen_f0baafcba3ef341f" hs_bindgen_f0baafcba3ef341f_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint64_t_fun@
hs_bindgen_f0baafcba3ef341f :: IO HsBindgen.Runtime.LibC.Word64
hs_bindgen_f0baafcba3ef341f =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_f0baafcba3ef341f_base

{-| __C declaration:__ @uint64_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 18:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint64_t_fun :: IO HsBindgen.Runtime.LibC.Word64
uint64_t_fun = hs_bindgen_f0baafcba3ef341f

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int_least8_t_fun@
foreign import ccall unsafe "hs_bindgen_42d43898e2bbce55" hs_bindgen_42d43898e2bbce55_base ::
     IO GHC.Int.Int8

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int_least8_t_fun@
hs_bindgen_42d43898e2bbce55 :: IO HsBindgen.Runtime.LibC.Int8
hs_bindgen_42d43898e2bbce55 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_42d43898e2bbce55_base

{-| __C declaration:__ @int_least8_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 19:14@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_least8_t_fun :: IO HsBindgen.Runtime.LibC.Int8
int_least8_t_fun = hs_bindgen_42d43898e2bbce55

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int_least16_t_fun@
foreign import ccall unsafe "hs_bindgen_6350d76366b7c38a" hs_bindgen_6350d76366b7c38a_base ::
     IO GHC.Int.Int16

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int_least16_t_fun@
hs_bindgen_6350d76366b7c38a :: IO HsBindgen.Runtime.LibC.Int16
hs_bindgen_6350d76366b7c38a =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_6350d76366b7c38a_base

{-| __C declaration:__ @int_least16_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 20:15@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_least16_t_fun :: IO HsBindgen.Runtime.LibC.Int16
int_least16_t_fun = hs_bindgen_6350d76366b7c38a

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int_least32_t_fun@
foreign import ccall unsafe "hs_bindgen_282f4da244daeeb3" hs_bindgen_282f4da244daeeb3_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int_least32_t_fun@
hs_bindgen_282f4da244daeeb3 :: IO HsBindgen.Runtime.LibC.Int32
hs_bindgen_282f4da244daeeb3 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_282f4da244daeeb3_base

{-| __C declaration:__ @int_least32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 21:15@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_least32_t_fun :: IO HsBindgen.Runtime.LibC.Int32
int_least32_t_fun = hs_bindgen_282f4da244daeeb3

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int_least64_t_fun@
foreign import ccall unsafe "hs_bindgen_a992b9370875bc4f" hs_bindgen_a992b9370875bc4f_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int_least64_t_fun@
hs_bindgen_a992b9370875bc4f :: IO HsBindgen.Runtime.LibC.Int64
hs_bindgen_a992b9370875bc4f =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_a992b9370875bc4f_base

{-| __C declaration:__ @int_least64_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 22:15@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_least64_t_fun :: IO HsBindgen.Runtime.LibC.Int64
int_least64_t_fun = hs_bindgen_a992b9370875bc4f

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint_least8_t_fun@
foreign import ccall unsafe "hs_bindgen_9143c626ba1553fc" hs_bindgen_9143c626ba1553fc_base ::
     IO GHC.Word.Word8

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint_least8_t_fun@
hs_bindgen_9143c626ba1553fc :: IO HsBindgen.Runtime.LibC.Word8
hs_bindgen_9143c626ba1553fc =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_9143c626ba1553fc_base

{-| __C declaration:__ @uint_least8_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 23:15@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_least8_t_fun :: IO HsBindgen.Runtime.LibC.Word8
uint_least8_t_fun = hs_bindgen_9143c626ba1553fc

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint_least16_t_fun@
foreign import ccall unsafe "hs_bindgen_b48d2270e32f25ec" hs_bindgen_b48d2270e32f25ec_base ::
     IO GHC.Word.Word16

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint_least16_t_fun@
hs_bindgen_b48d2270e32f25ec :: IO HsBindgen.Runtime.LibC.Word16
hs_bindgen_b48d2270e32f25ec =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_b48d2270e32f25ec_base

{-| __C declaration:__ @uint_least16_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 24:16@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_least16_t_fun :: IO HsBindgen.Runtime.LibC.Word16
uint_least16_t_fun = hs_bindgen_b48d2270e32f25ec

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint_least32_t_fun@
foreign import ccall unsafe "hs_bindgen_e49bc74d9fb4983d" hs_bindgen_e49bc74d9fb4983d_base ::
     IO GHC.Word.Word32

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint_least32_t_fun@
hs_bindgen_e49bc74d9fb4983d :: IO HsBindgen.Runtime.LibC.Word32
hs_bindgen_e49bc74d9fb4983d =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_e49bc74d9fb4983d_base

{-| __C declaration:__ @uint_least32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 25:16@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_least32_t_fun :: IO HsBindgen.Runtime.LibC.Word32
uint_least32_t_fun = hs_bindgen_e49bc74d9fb4983d

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint_least64_t_fun@
foreign import ccall unsafe "hs_bindgen_8cd2ab25e650607b" hs_bindgen_8cd2ab25e650607b_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint_least64_t_fun@
hs_bindgen_8cd2ab25e650607b :: IO HsBindgen.Runtime.LibC.Word64
hs_bindgen_8cd2ab25e650607b =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_8cd2ab25e650607b_base

{-| __C declaration:__ @uint_least64_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 26:16@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_least64_t_fun :: IO HsBindgen.Runtime.LibC.Word64
uint_least64_t_fun = hs_bindgen_8cd2ab25e650607b

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int_fast8_t_fun@
foreign import ccall unsafe "hs_bindgen_0a79089430ad992e" hs_bindgen_0a79089430ad992e_base ::
     IO GHC.Int.Int8

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int_fast8_t_fun@
hs_bindgen_0a79089430ad992e :: IO HsBindgen.Runtime.LibC.Int8
hs_bindgen_0a79089430ad992e =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_0a79089430ad992e_base

{-| __C declaration:__ @int_fast8_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 27:13@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_fast8_t_fun :: IO HsBindgen.Runtime.LibC.Int8
int_fast8_t_fun = hs_bindgen_0a79089430ad992e

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int_fast32_t_fun@
foreign import ccall unsafe "hs_bindgen_73fb6d800ee5680a" hs_bindgen_73fb6d800ee5680a_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int_fast32_t_fun@
hs_bindgen_73fb6d800ee5680a :: IO HsBindgen.Runtime.LibC.Int32
hs_bindgen_73fb6d800ee5680a =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_73fb6d800ee5680a_base

{-| __C declaration:__ @int_fast32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 30:14@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_fast32_t_fun :: IO HsBindgen.Runtime.LibC.Int32
int_fast32_t_fun = hs_bindgen_73fb6d800ee5680a

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int_fast64_t_fun@
foreign import ccall unsafe "hs_bindgen_13ca37318a4b7ff5" hs_bindgen_13ca37318a4b7ff5_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_int_fast64_t_fun@
hs_bindgen_13ca37318a4b7ff5 :: IO HsBindgen.Runtime.LibC.Int64
hs_bindgen_13ca37318a4b7ff5 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_13ca37318a4b7ff5_base

{-| __C declaration:__ @int_fast64_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 31:14@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
int_fast64_t_fun :: IO HsBindgen.Runtime.LibC.Int64
int_fast64_t_fun = hs_bindgen_13ca37318a4b7ff5

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint_fast8_t_fun@
foreign import ccall unsafe "hs_bindgen_f090a3b4bbf12f04" hs_bindgen_f090a3b4bbf12f04_base ::
     IO GHC.Word.Word8

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint_fast8_t_fun@
hs_bindgen_f090a3b4bbf12f04 :: IO HsBindgen.Runtime.LibC.Word8
hs_bindgen_f090a3b4bbf12f04 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_f090a3b4bbf12f04_base

{-| __C declaration:__ @uint_fast8_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 32:14@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_fast8_t_fun :: IO HsBindgen.Runtime.LibC.Word8
uint_fast8_t_fun = hs_bindgen_f090a3b4bbf12f04

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint_fast32_t_fun@
foreign import ccall unsafe "hs_bindgen_59209267124fbc45" hs_bindgen_59209267124fbc45_base ::
     IO GHC.Word.Word32

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint_fast32_t_fun@
hs_bindgen_59209267124fbc45 :: IO HsBindgen.Runtime.LibC.Word32
hs_bindgen_59209267124fbc45 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_59209267124fbc45_base

{-| __C declaration:__ @uint_fast32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 35:15@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_fast32_t_fun :: IO HsBindgen.Runtime.LibC.Word32
uint_fast32_t_fun = hs_bindgen_59209267124fbc45

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint_fast64_t_fun@
foreign import ccall unsafe "hs_bindgen_a78f57cdabeb1ca1" hs_bindgen_a78f57cdabeb1ca1_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uint_fast64_t_fun@
hs_bindgen_a78f57cdabeb1ca1 :: IO HsBindgen.Runtime.LibC.Word64
hs_bindgen_a78f57cdabeb1ca1 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_a78f57cdabeb1ca1_base

{-| __C declaration:__ @uint_fast64_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 36:15@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uint_fast64_t_fun :: IO HsBindgen.Runtime.LibC.Word64
uint_fast64_t_fun = hs_bindgen_a78f57cdabeb1ca1

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_intmax_t_fun@
foreign import ccall unsafe "hs_bindgen_ac33d7503cd16cf9" hs_bindgen_ac33d7503cd16cf9_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_intmax_t_fun@
hs_bindgen_ac33d7503cd16cf9 :: IO HsBindgen.Runtime.LibC.CIntMax
hs_bindgen_ac33d7503cd16cf9 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_ac33d7503cd16cf9_base

{-| __C declaration:__ @intmax_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 37:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
intmax_t_fun :: IO HsBindgen.Runtime.LibC.CIntMax
intmax_t_fun = hs_bindgen_ac33d7503cd16cf9

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uintmax_t_fun@
foreign import ccall unsafe "hs_bindgen_c729cac1dced98eb" hs_bindgen_c729cac1dced98eb_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uintmax_t_fun@
hs_bindgen_c729cac1dced98eb :: IO HsBindgen.Runtime.LibC.CUIntMax
hs_bindgen_c729cac1dced98eb =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_c729cac1dced98eb_base

{-| __C declaration:__ @uintmax_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 38:11@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uintmax_t_fun :: IO HsBindgen.Runtime.LibC.CUIntMax
uintmax_t_fun = hs_bindgen_c729cac1dced98eb

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_intptr_t_fun@
foreign import ccall unsafe "hs_bindgen_c0b84d83fb5eb309" hs_bindgen_c0b84d83fb5eb309_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_intptr_t_fun@
hs_bindgen_c0b84d83fb5eb309 :: IO HsBindgen.Runtime.LibC.CIntPtr
hs_bindgen_c0b84d83fb5eb309 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_c0b84d83fb5eb309_base

{-| __C declaration:__ @intptr_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 39:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
intptr_t_fun :: IO HsBindgen.Runtime.LibC.CIntPtr
intptr_t_fun = hs_bindgen_c0b84d83fb5eb309

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uintptr_t_fun@
foreign import ccall unsafe "hs_bindgen_2c88bc97436e95f5" hs_bindgen_2c88bc97436e95f5_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_uintptr_t_fun@
hs_bindgen_2c88bc97436e95f5 :: IO HsBindgen.Runtime.LibC.CUIntPtr
hs_bindgen_2c88bc97436e95f5 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_2c88bc97436e95f5_base

{-| __C declaration:__ @uintptr_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 40:11@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
uintptr_t_fun :: IO HsBindgen.Runtime.LibC.CUIntPtr
uintptr_t_fun = hs_bindgen_2c88bc97436e95f5

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_size_t_fun@
foreign import ccall unsafe "hs_bindgen_8c7a9840d42953ec" hs_bindgen_8c7a9840d42953ec_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_size_t_fun@
hs_bindgen_8c7a9840d42953ec :: IO HsBindgen.Runtime.LibC.CSize
hs_bindgen_8c7a9840d42953ec =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_8c7a9840d42953ec_base

{-| __C declaration:__ @size_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 51:8@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
size_t_fun :: IO HsBindgen.Runtime.LibC.CSize
size_t_fun = hs_bindgen_8c7a9840d42953ec

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_ptrdiff_t_fun@
foreign import ccall unsafe "hs_bindgen_bfc4a1bcb4b291a5" hs_bindgen_bfc4a1bcb4b291a5_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_ptrdiff_t_fun@
hs_bindgen_bfc4a1bcb4b291a5 :: IO HsBindgen.Runtime.LibC.CPtrdiff
hs_bindgen_bfc4a1bcb4b291a5 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_bfc4a1bcb4b291a5_base

{-| __C declaration:__ @ptrdiff_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 52:11@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
ptrdiff_t_fun :: IO HsBindgen.Runtime.LibC.CPtrdiff
ptrdiff_t_fun = hs_bindgen_bfc4a1bcb4b291a5

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_wchar_t_fun@
foreign import ccall unsafe "hs_bindgen_fc2d05ee4c0cfb45" hs_bindgen_fc2d05ee4c0cfb45_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_wchar_t_fun@
hs_bindgen_fc2d05ee4c0cfb45 :: IO HsBindgen.Runtime.LibC.CWchar
hs_bindgen_fc2d05ee4c0cfb45 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_fc2d05ee4c0cfb45_base

{-| __C declaration:__ @wchar_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 63:9@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
wchar_t_fun :: IO HsBindgen.Runtime.LibC.CWchar
wchar_t_fun = hs_bindgen_fc2d05ee4c0cfb45

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_wint_t_fun@
foreign import ccall unsafe "hs_bindgen_5d91844f4b4fd8b3" hs_bindgen_5d91844f4b4fd8b3_base ::
     IO GHC.Word.Word32

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_wint_t_fun@
hs_bindgen_5d91844f4b4fd8b3 :: IO HsBindgen.Runtime.LibC.CWintT
hs_bindgen_5d91844f4b4fd8b3 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_5d91844f4b4fd8b3_base

{-| __C declaration:__ @wint_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 64:8@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
wint_t_fun :: IO HsBindgen.Runtime.LibC.CWintT
wint_t_fun = hs_bindgen_5d91844f4b4fd8b3

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_wctrans_t_fun@
foreign import ccall unsafe "hs_bindgen_3592bacf3bbee0f8" hs_bindgen_3592bacf3bbee0f8_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_wctrans_t_fun@
hs_bindgen_3592bacf3bbee0f8 :: IO HsBindgen.Runtime.LibC.CWctransT
hs_bindgen_3592bacf3bbee0f8 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_3592bacf3bbee0f8_base

{-| __C declaration:__ @wctrans_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 67:11@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
wctrans_t_fun :: IO HsBindgen.Runtime.LibC.CWctransT
wctrans_t_fun = hs_bindgen_3592bacf3bbee0f8

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_wctype_t_fun@
foreign import ccall unsafe "hs_bindgen_30121c3526c3ac78" hs_bindgen_30121c3526c3ac78_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_wctype_t_fun@
hs_bindgen_30121c3526c3ac78 :: IO HsBindgen.Runtime.LibC.CWctypeT
hs_bindgen_30121c3526c3ac78 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_30121c3526c3ac78_base

{-| __C declaration:__ @wctype_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 68:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
wctype_t_fun :: IO HsBindgen.Runtime.LibC.CWctypeT
wctype_t_fun = hs_bindgen_30121c3526c3ac78

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_char16_t_fun@
foreign import ccall unsafe "hs_bindgen_4fd0150e665ead24" hs_bindgen_4fd0150e665ead24_base ::
     IO GHC.Word.Word16

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_char16_t_fun@
hs_bindgen_4fd0150e665ead24 :: IO HsBindgen.Runtime.LibC.CChar16T
hs_bindgen_4fd0150e665ead24 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_4fd0150e665ead24_base

{-| __C declaration:__ @char16_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 69:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
char16_t_fun :: IO HsBindgen.Runtime.LibC.CChar16T
char16_t_fun = hs_bindgen_4fd0150e665ead24

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_char32_t_fun@
foreign import ccall unsafe "hs_bindgen_2c0f7df3bd2fe4df" hs_bindgen_2c0f7df3bd2fe4df_base ::
     IO GHC.Word.Word32

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_char32_t_fun@
hs_bindgen_2c0f7df3bd2fe4df :: IO HsBindgen.Runtime.LibC.CChar32T
hs_bindgen_2c0f7df3bd2fe4df =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_2c0f7df3bd2fe4df_base

{-| __C declaration:__ @char32_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 70:10@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
char32_t_fun :: IO HsBindgen.Runtime.LibC.CChar32T
char32_t_fun = hs_bindgen_2c0f7df3bd2fe4df

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_time_t_fun@
foreign import ccall unsafe "hs_bindgen_ab883c49289a0016" hs_bindgen_ab883c49289a0016_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_time_t_fun@
hs_bindgen_ab883c49289a0016 :: IO HsBindgen.Runtime.LibC.CTime
hs_bindgen_ab883c49289a0016 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_ab883c49289a0016_base

{-| __C declaration:__ @time_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 74:8@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
time_t_fun :: IO HsBindgen.Runtime.LibC.CTime
time_t_fun = hs_bindgen_ab883c49289a0016

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_clock_t_fun@
foreign import ccall unsafe "hs_bindgen_eb18d400828e034b" hs_bindgen_eb18d400828e034b_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_clock_t_fun@
hs_bindgen_eb18d400828e034b :: IO HsBindgen.Runtime.LibC.CClock
hs_bindgen_eb18d400828e034b =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_eb18d400828e034b_base

{-| __C declaration:__ @clock_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 75:9@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
clock_t_fun :: IO HsBindgen.Runtime.LibC.CClock
clock_t_fun = hs_bindgen_eb18d400828e034b

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_sig_atomic_t_fun@
foreign import ccall unsafe "hs_bindgen_5b52ae35b955c28f" hs_bindgen_5b52ae35b955c28f_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_bindingspecsstandard_library_Example_Unsafe_sig_atomic_t_fun@
hs_bindgen_5b52ae35b955c28f :: IO HsBindgen.Runtime.LibC.CSigAtomic
hs_bindgen_5b52ae35b955c28f =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_5b52ae35b955c28f_base

{-| __C declaration:__ @sig_atomic_t_fun@

    __defined at:__ @binding-specs\/standard_library_external_binding_specs.h 88:14@

    __exported by:__ @binding-specs\/standard_library_external_binding_specs.h@
-}
sig_atomic_t_fun :: IO HsBindgen.Runtime.LibC.CSigAtomic
sig_atomic_t_fun = hs_bindgen_5b52ae35b955c28f
