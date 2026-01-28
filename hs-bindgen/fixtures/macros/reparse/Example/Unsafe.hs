{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Data.Complex
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified GHC.Word
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (Double, Float, IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
  [ "#include <macros/reparse.h>"
  , "void hs_bindgen_c1716e300ba327c7 ("
  , "  A arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  args_char1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3ef14607a6187aaa ("
  , "  A arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  args_char2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_5b0a626f64912f9d ("
  , "  A arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  args_char3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_36e4501239085bc1 ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  args_short1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a187e0233daeb237 ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  args_short2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2460adeff61561ce ("
  , "  A arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  args_short3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4f13ab06db79b7f2 ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  args_int1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f1657d18f6f8a1ed ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  args_int2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9ac58b8eb806be42 ("
  , "  A arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  args_int3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f73c59fe22a9870e ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  args_long1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_84a824853fc83077 ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  args_long2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c4c1a08ddf9cd5bc ("
  , "  A arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  args_long3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_58a6b5f118525c6c ("
  , "  A arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  args_float(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ffc58625c3a51d8f ("
  , "  A arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  args_double(arg1, arg2);"
  , "}"
  , "void hs_bindgen_51fb2da1d100c9a7 ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  args_bool1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b2d19f91a7b9f7d3 ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  args_struct(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_bc74164a05d282c7 ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  args_union(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_69f08c1d9f5e590e ("
  , "  A arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  args_enum(arg1, arg2);"
  , "}"
  , "void hs_bindgen_785b005f35d4d7ec ("
  , "  A arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  args_pointer1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_edc45a1b9750dcd3 ("
  , "  A arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  args_pointer2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_102895862f35ca35 ("
  , "  A arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  args_pointer3(arg1, arg2);"
  , "}"
  , "A hs_bindgen_78f9ea765accb501 (void)"
  , "{"
  , "  return ret_A();"
  , "}"
  , "char hs_bindgen_e1e99ef9fc54a288 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char1(arg1);"
  , "}"
  , "signed char hs_bindgen_f6217639a7e142d3 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char2(arg1);"
  , "}"
  , "unsigned char hs_bindgen_759b6cec946323f4 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char3(arg1);"
  , "}"
  , "signed short hs_bindgen_bf062c8332405f82 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short1(arg1);"
  , "}"
  , "signed short hs_bindgen_3d9d5e4b8135169a ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short2(arg1);"
  , "}"
  , "unsigned short hs_bindgen_63b44610868e424f ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short3(arg1);"
  , "}"
  , "signed int hs_bindgen_1a8d68c887085fbf ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int1(arg1);"
  , "}"
  , "signed int hs_bindgen_f64653c7b4576075 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int2(arg1);"
  , "}"
  , "unsigned int hs_bindgen_d2030910b711f1d8 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int3(arg1);"
  , "}"
  , "signed long hs_bindgen_2d6a30810e6b27e3 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long1(arg1);"
  , "}"
  , "signed long hs_bindgen_02885fe1cf2771da ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long2(arg1);"
  , "}"
  , "unsigned long hs_bindgen_888c9704132541d5 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long3(arg1);"
  , "}"
  , "float hs_bindgen_2d2ce0d386f26293 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_float(arg1);"
  , "}"
  , "double hs_bindgen_de353a737de53428 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_double(arg1);"
  , "}"
  , "_Bool hs_bindgen_91e2ab77e68f0288 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_bool1(arg1);"
  , "}"
  , "void hs_bindgen_9f29c7eee02f6d53 ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_struct(arg1);"
  , "}"
  , "void hs_bindgen_6844bf5f5a5f6681 ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_union(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_f96c4bc30b6b17e8 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_enum(arg1);"
  , "}"
  , "signed int *hs_bindgen_bfb6069e1423e7a5 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer1(arg1);"
  , "}"
  , "signed int **hs_bindgen_ffae633548386d89 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer2(arg1);"
  , "}"
  , "void *hs_bindgen_550cb4a23c6ab2ff ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer3(arg1);"
  , "}"
  , "signed int hs_bindgen_f7a7a45a80ae39f7 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return body1(arg1);"
  , "}"
  , "A hs_bindgen_364e73b014d7d4df (void)"
  , "{"
  , "  return body2();"
  , "}"
  , "void hs_bindgen_88b4cd11afc4f6c1 ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  args_complex_float(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_0ddc53d8e91cb32a ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  args_complex_double(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_eb82eb840e288900 ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_complex_float(arg1);"
  , "}"
  , "void hs_bindgen_cbc25ea9cbdd2365 ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_complex_double(arg1);"
  , "}"
  , "void hs_bindgen_3258de4ffd2c08af ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  bespoke_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_74b2cd1defdd5609 ("
  , "  A arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  bespoke_args2(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_5405c1e037d1e115 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return bespoke_ret1(arg1);"
  , "}"
  , "size_t hs_bindgen_a6a3e5a828532360 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return bespoke_ret2(arg1);"
  , "}"
  , "void hs_bindgen_4956a52bf5073b9f ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  arr_args1(arg1);"
  , "}"
  , "void hs_bindgen_0fc8b091085a88e9 ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  arr_args2(arg1);"
  , "}"
  , "void hs_bindgen_ca6f1bc1a29b85f8 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  arr_args3(arg1);"
  , "}"
  , "void hs_bindgen_a168ae0de206febe ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  arr_args4(arg1);"
  , "}"
  , "void hs_bindgen_8e63f57f1f5d662e ("
  , "  A arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  funptr_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_927bd07f48d05d21 ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  funptr_args2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c82e078d3c54a6bc ("
  , "  A arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  funptr_args3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_211ad1ac5399caec ("
  , "  A arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  funptr_args4(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9057c59d70e815d7 ("
  , "  A arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  funptr_args5(arg1, arg2);"
  , "}"
  , "void hs_bindgen_153515e0ff74574f ("
  , "  A arg1"
  , ")"
  , "{"
  , "  comments1(arg1);"
  , "}"
  , "void hs_bindgen_8cc833db463cc95c ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  const_prim_before1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d767bbef00031d57 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  const_prim_before2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a36dfeb811993297 ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  const_prim_before3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d7fa2440be24e954 ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  const_prim_after1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c169229f24baf752 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  const_prim_after2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c0780f7624ed1d3e ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  const_prim_after3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_fda903bc1139b1d6 ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a5a70f3be654ea00 ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b813910f6a632ce2 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_dc22b02b2f53aa5b ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before4(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_503736261279760d ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before5(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_ed0a8c0e15f5d2ce ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before6(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4659c22d39cc1bb3 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before7(arg1, arg2);"
  , "}"
  , "void hs_bindgen_530245b77093b08c ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before8(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c31a804bd742193e ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_53756fa3a68ab067 ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4134ad71149d6139 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3de6157427334101 ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after4(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_fc4ef8c9107c1ae6 ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after5(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_5e20c60b725ae606 ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after6(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a0f20d4b9a07ff5b ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after7(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3a020035eb2fe7f8 ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after8(arg1, arg2);"
  , "}"
  , "void hs_bindgen_17623ba5065bf95d ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  const_pointers_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_02d08ccd5df88a98 ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  const_pointers_args2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_50c423f2237cb6b5 ("
  , "  A arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_99c29c45d78348e9 ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args4(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6a92dbfae24b1bcd ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args5(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_0c07f1e0256fd705 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret1(arg1);"
  , "}"
  , "signed int const *hs_bindgen_d12c8210ff3c3711 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret2(arg1);"
  , "}"
  , "signed int *const hs_bindgen_a58bc0be6f564801 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret3(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_622bb8150470138b ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret4(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_d49bd331ad2077e5 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret5(arg1);"
  , "}"
  , "void hs_bindgen_224608f780bff5bd ("
  , "  A const *arg1"
  , ")"
  , "{"
  , "  const_array_elem1(arg1);"
  , "}"
  , "void hs_bindgen_9aa74ad89f2c1fba ("
  , "  A const **arg1"
  , ")"
  , "{"
  , "  const_array_elem2(arg1);"
  , "}"
  , "void hs_bindgen_6a328300c5ef0c9e ("
  , "  A *const *arg1"
  , ")"
  , "{"
  , "  const_array_elem3(arg1);"
  , "}"
  , "A hs_bindgen_13a7d78e11555d58 (void)"
  , "{"
  , "  return noParams1();"
  , "}"
  , "A hs_bindgen_672f4691ee7a367c (void)"
  , "{"
  , "  return noParams2();"
  , "}"
  , "void hs_bindgen_591f84e2163a5d18 ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  noParams3(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_8cdf7774adb0f0b4 ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return funptr_ret1(arg1);"
  , "}"
  , "signed int (*hs_bindgen_a4e08267a9070ede ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return funptr_ret2(arg1);"
  , "}"
  , "void (*hs_bindgen_65fa30510d244cbf ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return funptr_ret3(arg1);"
  , "}"
  , "char (*hs_bindgen_da12eaec295883aa ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret4(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_281c53214b1cdcb4 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret5(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_16628c257aa64a76 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret6(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_79fb0c30f546a547 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret7(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_4668d2ff9d5bfc40 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret8(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_c044d7074789febc ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret9(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_628ced6eccc7783a ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret10(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_char1@
foreign import ccall unsafe "hs_bindgen_c1716e300ba327c7" hs_bindgen_c1716e300ba327c7_base ::
     GHC.Int.Int32
  -> GHC.Int.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_char1@
hs_bindgen_c1716e300ba327c7 ::
     A
  -> FC.CChar
  -> IO ()
hs_bindgen_c1716e300ba327c7 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_c1716e300ba327c7_base

{-| Function declarations

__C declaration:__ @args_char1@

__defined at:__ @macros\/reparse.h 17:6@

__exported by:__ @macros\/reparse.h@
-}
args_char1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_char1 = hs_bindgen_c1716e300ba327c7

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_char2@
foreign import ccall unsafe "hs_bindgen_3ef14607a6187aaa" hs_bindgen_3ef14607a6187aaa_base ::
     GHC.Int.Int32
  -> GHC.Int.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_char2@
hs_bindgen_3ef14607a6187aaa ::
     A
  -> FC.CSChar
  -> IO ()
hs_bindgen_3ef14607a6187aaa =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_3ef14607a6187aaa_base

{-| __C declaration:__ @args_char2@

    __defined at:__ @macros\/reparse.h 18:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CSChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_char2 = hs_bindgen_3ef14607a6187aaa

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_char3@
foreign import ccall unsafe "hs_bindgen_5b0a626f64912f9d" hs_bindgen_5b0a626f64912f9d_base ::
     GHC.Int.Int32
  -> GHC.Word.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_char3@
hs_bindgen_5b0a626f64912f9d ::
     A
  -> FC.CUChar
  -> IO ()
hs_bindgen_5b0a626f64912f9d =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_5b0a626f64912f9d_base

{-| __C declaration:__ @args_char3@

    __defined at:__ @macros\/reparse.h 19:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CUChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_char3 = hs_bindgen_5b0a626f64912f9d

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_short1@
foreign import ccall unsafe "hs_bindgen_36e4501239085bc1" hs_bindgen_36e4501239085bc1_base ::
     GHC.Int.Int32
  -> GHC.Int.Int16
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_short1@
hs_bindgen_36e4501239085bc1 ::
     A
  -> FC.CShort
  -> IO ()
hs_bindgen_36e4501239085bc1 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_36e4501239085bc1_base

{-| __C declaration:__ @args_short1@

    __defined at:__ @macros\/reparse.h 21:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CShort
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_short1 = hs_bindgen_36e4501239085bc1

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_short2@
foreign import ccall unsafe "hs_bindgen_a187e0233daeb237" hs_bindgen_a187e0233daeb237_base ::
     GHC.Int.Int32
  -> GHC.Int.Int16
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_short2@
hs_bindgen_a187e0233daeb237 ::
     A
  -> FC.CShort
  -> IO ()
hs_bindgen_a187e0233daeb237 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_a187e0233daeb237_base

{-| __C declaration:__ @args_short2@

    __defined at:__ @macros\/reparse.h 22:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CShort
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_short2 = hs_bindgen_a187e0233daeb237

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_short3@
foreign import ccall unsafe "hs_bindgen_2460adeff61561ce" hs_bindgen_2460adeff61561ce_base ::
     GHC.Int.Int32
  -> GHC.Word.Word16
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_short3@
hs_bindgen_2460adeff61561ce ::
     A
  -> FC.CUShort
  -> IO ()
hs_bindgen_2460adeff61561ce =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_2460adeff61561ce_base

{-| __C declaration:__ @args_short3@

    __defined at:__ @macros\/reparse.h 23:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CUShort
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_short3 = hs_bindgen_2460adeff61561ce

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_int1@
foreign import ccall unsafe "hs_bindgen_4f13ab06db79b7f2" hs_bindgen_4f13ab06db79b7f2_base ::
     GHC.Int.Int32
  -> GHC.Int.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_int1@
hs_bindgen_4f13ab06db79b7f2 ::
     A
  -> FC.CInt
  -> IO ()
hs_bindgen_4f13ab06db79b7f2 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_4f13ab06db79b7f2_base

{-| __C declaration:__ @args_int1@

    __defined at:__ @macros\/reparse.h 25:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_int1 = hs_bindgen_4f13ab06db79b7f2

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_int2@
foreign import ccall unsafe "hs_bindgen_f1657d18f6f8a1ed" hs_bindgen_f1657d18f6f8a1ed_base ::
     GHC.Int.Int32
  -> GHC.Int.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_int2@
hs_bindgen_f1657d18f6f8a1ed ::
     A
  -> FC.CInt
  -> IO ()
hs_bindgen_f1657d18f6f8a1ed =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_f1657d18f6f8a1ed_base

{-| __C declaration:__ @args_int2@

    __defined at:__ @macros\/reparse.h 26:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_int2 = hs_bindgen_f1657d18f6f8a1ed

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_int3@
foreign import ccall unsafe "hs_bindgen_9ac58b8eb806be42" hs_bindgen_9ac58b8eb806be42_base ::
     GHC.Int.Int32
  -> GHC.Word.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_int3@
hs_bindgen_9ac58b8eb806be42 ::
     A
  -> FC.CUInt
  -> IO ()
hs_bindgen_9ac58b8eb806be42 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_9ac58b8eb806be42_base

{-| __C declaration:__ @args_int3@

    __defined at:__ @macros\/reparse.h 27:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CUInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_int3 = hs_bindgen_9ac58b8eb806be42

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_long1@
foreign import ccall unsafe "hs_bindgen_f73c59fe22a9870e" hs_bindgen_f73c59fe22a9870e_base ::
     GHC.Int.Int32
  -> GHC.Int.Int64
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_long1@
hs_bindgen_f73c59fe22a9870e ::
     A
  -> FC.CLong
  -> IO ()
hs_bindgen_f73c59fe22a9870e =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_f73c59fe22a9870e_base

{-| __C declaration:__ @args_long1@

    __defined at:__ @macros\/reparse.h 29:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CLong
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_long1 = hs_bindgen_f73c59fe22a9870e

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_long2@
foreign import ccall unsafe "hs_bindgen_84a824853fc83077" hs_bindgen_84a824853fc83077_base ::
     GHC.Int.Int32
  -> GHC.Int.Int64
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_long2@
hs_bindgen_84a824853fc83077 ::
     A
  -> FC.CLong
  -> IO ()
hs_bindgen_84a824853fc83077 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_84a824853fc83077_base

{-| __C declaration:__ @args_long2@

    __defined at:__ @macros\/reparse.h 30:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CLong
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_long2 = hs_bindgen_84a824853fc83077

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_long3@
foreign import ccall unsafe "hs_bindgen_c4c1a08ddf9cd5bc" hs_bindgen_c4c1a08ddf9cd5bc_base ::
     GHC.Int.Int32
  -> GHC.Word.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_long3@
hs_bindgen_c4c1a08ddf9cd5bc ::
     A
  -> FC.CULong
  -> IO ()
hs_bindgen_c4c1a08ddf9cd5bc =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_c4c1a08ddf9cd5bc_base

{-| __C declaration:__ @args_long3@

    __defined at:__ @macros\/reparse.h 31:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CULong
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_long3 = hs_bindgen_c4c1a08ddf9cd5bc

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_float@
foreign import ccall unsafe "hs_bindgen_58a6b5f118525c6c" hs_bindgen_58a6b5f118525c6c_base ::
     GHC.Int.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_float@
hs_bindgen_58a6b5f118525c6c ::
     A
  -> FC.CFloat
  -> IO ()
hs_bindgen_58a6b5f118525c6c =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_58a6b5f118525c6c_base

{-| __C declaration:__ @args_float@

    __defined at:__ @macros\/reparse.h 33:6@

    __exported by:__ @macros\/reparse.h@
-}
args_float ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_float = hs_bindgen_58a6b5f118525c6c

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_double@
foreign import ccall unsafe "hs_bindgen_ffc58625c3a51d8f" hs_bindgen_ffc58625c3a51d8f_base ::
     GHC.Int.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_double@
hs_bindgen_ffc58625c3a51d8f ::
     A
  -> FC.CDouble
  -> IO ()
hs_bindgen_ffc58625c3a51d8f =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_ffc58625c3a51d8f_base

{-| __C declaration:__ @args_double@

    __defined at:__ @macros\/reparse.h 34:6@

    __exported by:__ @macros\/reparse.h@
-}
args_double ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_double = hs_bindgen_ffc58625c3a51d8f

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_bool1@
foreign import ccall unsafe "hs_bindgen_51fb2da1d100c9a7" hs_bindgen_51fb2da1d100c9a7_base ::
     GHC.Int.Int32
  -> GHC.Word.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_bool1@
hs_bindgen_51fb2da1d100c9a7 ::
     A
  -> FC.CBool
  -> IO ()
hs_bindgen_51fb2da1d100c9a7 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_51fb2da1d100c9a7_base

{-| __C declaration:__ @args_bool1@

    __defined at:__ @macros\/reparse.h 35:6@

    __exported by:__ @macros\/reparse.h@
-}
args_bool1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_bool1 = hs_bindgen_51fb2da1d100c9a7

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_struct@
foreign import ccall unsafe "hs_bindgen_b2d19f91a7b9f7d3" hs_bindgen_b2d19f91a7b9f7d3_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_struct@
hs_bindgen_b2d19f91a7b9f7d3 ::
     A
  -> Ptr.Ptr Some_struct
  -> IO ()
hs_bindgen_b2d19f91a7b9f7d3 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_b2d19f91a7b9f7d3_base

{-| __C declaration:__ @args_struct@

    __defined at:__ @macros\/reparse.h 37:6@

    __exported by:__ @macros\/reparse.h@
-}
args_struct ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_struct
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_struct =
  \arg10 ->
    \arg21 ->
      F.with arg21 (\arg22 ->
                      hs_bindgen_b2d19f91a7b9f7d3 arg10 arg22)

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_union@
foreign import ccall unsafe "hs_bindgen_bc74164a05d282c7" hs_bindgen_bc74164a05d282c7_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_union@
hs_bindgen_bc74164a05d282c7 ::
     A
  -> Ptr.Ptr Some_union
  -> IO ()
hs_bindgen_bc74164a05d282c7 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_bc74164a05d282c7_base

{-| __C declaration:__ @args_union@

    __defined at:__ @macros\/reparse.h 38:6@

    __exported by:__ @macros\/reparse.h@
-}
args_union ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_union
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_union =
  \arg10 ->
    \arg21 ->
      F.with arg21 (\arg22 ->
                      hs_bindgen_bc74164a05d282c7 arg10 arg22)

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_enum@
foreign import ccall unsafe "hs_bindgen_69f08c1d9f5e590e" hs_bindgen_69f08c1d9f5e590e_base ::
     GHC.Int.Int32
  -> GHC.Word.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_enum@
hs_bindgen_69f08c1d9f5e590e ::
     A
  -> Some_enum
  -> IO ()
hs_bindgen_69f08c1d9f5e590e =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_69f08c1d9f5e590e_base

{-| __C declaration:__ @args_enum@

    __defined at:__ @macros\/reparse.h 39:6@

    __exported by:__ @macros\/reparse.h@
-}
args_enum ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_enum
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_enum = hs_bindgen_69f08c1d9f5e590e

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_pointer1@
foreign import ccall unsafe "hs_bindgen_785b005f35d4d7ec" hs_bindgen_785b005f35d4d7ec_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_pointer1@
hs_bindgen_785b005f35d4d7ec ::
     A
  -> Ptr.Ptr FC.CInt
  -> IO ()
hs_bindgen_785b005f35d4d7ec =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_785b005f35d4d7ec_base

{-| __C declaration:__ @args_pointer1@

    __defined at:__ @macros\/reparse.h 41:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_pointer1 = hs_bindgen_785b005f35d4d7ec

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_pointer2@
foreign import ccall unsafe "hs_bindgen_edc45a1b9750dcd3" hs_bindgen_edc45a1b9750dcd3_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_pointer2@
hs_bindgen_edc45a1b9750dcd3 ::
     A
  -> Ptr.Ptr (Ptr.Ptr FC.CInt)
  -> IO ()
hs_bindgen_edc45a1b9750dcd3 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_edc45a1b9750dcd3_base

{-| __C declaration:__ @args_pointer2@

    __defined at:__ @macros\/reparse.h 42:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.Ptr (Ptr.Ptr FC.CInt)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_pointer2 = hs_bindgen_edc45a1b9750dcd3

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_pointer3@
foreign import ccall unsafe "hs_bindgen_102895862f35ca35" hs_bindgen_102895862f35ca35_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_pointer3@
hs_bindgen_102895862f35ca35 ::
     A
  -> Ptr.Ptr Void
  -> IO ()
hs_bindgen_102895862f35ca35 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_102895862f35ca35_base

{-| __C declaration:__ @args_pointer3@

    __defined at:__ @macros\/reparse.h 43:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.Ptr Void
     -- ^ __C declaration:__ @arg3@
  -> IO ()
args_pointer3 = hs_bindgen_102895862f35ca35

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_A@
foreign import ccall unsafe "hs_bindgen_78f9ea765accb501" hs_bindgen_78f9ea765accb501_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_A@
hs_bindgen_78f9ea765accb501 :: IO A
hs_bindgen_78f9ea765accb501 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_78f9ea765accb501_base

{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h 47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A :: IO A
ret_A = hs_bindgen_78f9ea765accb501

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_char1@
foreign import ccall unsafe "hs_bindgen_e1e99ef9fc54a288" hs_bindgen_e1e99ef9fc54a288_base ::
     GHC.Int.Int32
  -> IO GHC.Int.Int8

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_char1@
hs_bindgen_e1e99ef9fc54a288 ::
     A
  -> IO FC.CChar
hs_bindgen_e1e99ef9fc54a288 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_e1e99ef9fc54a288_base

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h 49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CChar
ret_char1 = hs_bindgen_e1e99ef9fc54a288

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_char2@
foreign import ccall unsafe "hs_bindgen_f6217639a7e142d3" hs_bindgen_f6217639a7e142d3_base ::
     GHC.Int.Int32
  -> IO GHC.Int.Int8

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_char2@
hs_bindgen_f6217639a7e142d3 ::
     A
  -> IO FC.CSChar
hs_bindgen_f6217639a7e142d3 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_f6217639a7e142d3_base

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h 50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CSChar
ret_char2 = hs_bindgen_f6217639a7e142d3

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_char3@
foreign import ccall unsafe "hs_bindgen_759b6cec946323f4" hs_bindgen_759b6cec946323f4_base ::
     GHC.Int.Int32
  -> IO GHC.Word.Word8

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_char3@
hs_bindgen_759b6cec946323f4 ::
     A
  -> IO FC.CUChar
hs_bindgen_759b6cec946323f4 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_759b6cec946323f4_base

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h 51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CUChar
ret_char3 = hs_bindgen_759b6cec946323f4

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_short1@
foreign import ccall unsafe "hs_bindgen_bf062c8332405f82" hs_bindgen_bf062c8332405f82_base ::
     GHC.Int.Int32
  -> IO GHC.Int.Int16

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_short1@
hs_bindgen_bf062c8332405f82 ::
     A
  -> IO FC.CShort
hs_bindgen_bf062c8332405f82 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_bf062c8332405f82_base

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h 53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CShort
ret_short1 = hs_bindgen_bf062c8332405f82

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_short2@
foreign import ccall unsafe "hs_bindgen_3d9d5e4b8135169a" hs_bindgen_3d9d5e4b8135169a_base ::
     GHC.Int.Int32
  -> IO GHC.Int.Int16

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_short2@
hs_bindgen_3d9d5e4b8135169a ::
     A
  -> IO FC.CShort
hs_bindgen_3d9d5e4b8135169a =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_3d9d5e4b8135169a_base

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h 54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CShort
ret_short2 = hs_bindgen_3d9d5e4b8135169a

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_short3@
foreign import ccall unsafe "hs_bindgen_63b44610868e424f" hs_bindgen_63b44610868e424f_base ::
     GHC.Int.Int32
  -> IO GHC.Word.Word16

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_short3@
hs_bindgen_63b44610868e424f ::
     A
  -> IO FC.CUShort
hs_bindgen_63b44610868e424f =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_63b44610868e424f_base

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h 55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CUShort
ret_short3 = hs_bindgen_63b44610868e424f

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_int1@
foreign import ccall unsafe "hs_bindgen_1a8d68c887085fbf" hs_bindgen_1a8d68c887085fbf_base ::
     GHC.Int.Int32
  -> IO GHC.Int.Int32

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_int1@
hs_bindgen_1a8d68c887085fbf ::
     A
  -> IO FC.CInt
hs_bindgen_1a8d68c887085fbf =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_1a8d68c887085fbf_base

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h 57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CInt
ret_int1 = hs_bindgen_1a8d68c887085fbf

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_int2@
foreign import ccall unsafe "hs_bindgen_f64653c7b4576075" hs_bindgen_f64653c7b4576075_base ::
     GHC.Int.Int32
  -> IO GHC.Int.Int32

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_int2@
hs_bindgen_f64653c7b4576075 ::
     A
  -> IO FC.CInt
hs_bindgen_f64653c7b4576075 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_f64653c7b4576075_base

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h 58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CInt
ret_int2 = hs_bindgen_f64653c7b4576075

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_int3@
foreign import ccall unsafe "hs_bindgen_d2030910b711f1d8" hs_bindgen_d2030910b711f1d8_base ::
     GHC.Int.Int32
  -> IO GHC.Word.Word32

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_int3@
hs_bindgen_d2030910b711f1d8 ::
     A
  -> IO FC.CUInt
hs_bindgen_d2030910b711f1d8 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d2030910b711f1d8_base

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h 59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CUInt
ret_int3 = hs_bindgen_d2030910b711f1d8

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_long1@
foreign import ccall unsafe "hs_bindgen_2d6a30810e6b27e3" hs_bindgen_2d6a30810e6b27e3_base ::
     GHC.Int.Int32
  -> IO GHC.Int.Int64

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_long1@
hs_bindgen_2d6a30810e6b27e3 ::
     A
  -> IO FC.CLong
hs_bindgen_2d6a30810e6b27e3 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_2d6a30810e6b27e3_base

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h 61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CLong
ret_long1 = hs_bindgen_2d6a30810e6b27e3

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_long2@
foreign import ccall unsafe "hs_bindgen_02885fe1cf2771da" hs_bindgen_02885fe1cf2771da_base ::
     GHC.Int.Int32
  -> IO GHC.Int.Int64

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_long2@
hs_bindgen_02885fe1cf2771da ::
     A
  -> IO FC.CLong
hs_bindgen_02885fe1cf2771da =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_02885fe1cf2771da_base

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h 62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CLong
ret_long2 = hs_bindgen_02885fe1cf2771da

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_long3@
foreign import ccall unsafe "hs_bindgen_888c9704132541d5" hs_bindgen_888c9704132541d5_base ::
     GHC.Int.Int32
  -> IO GHC.Word.Word64

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_long3@
hs_bindgen_888c9704132541d5 ::
     A
  -> IO FC.CULong
hs_bindgen_888c9704132541d5 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_888c9704132541d5_base

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h 63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CULong
ret_long3 = hs_bindgen_888c9704132541d5

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_float@
foreign import ccall unsafe "hs_bindgen_2d2ce0d386f26293" hs_bindgen_2d2ce0d386f26293_base ::
     GHC.Int.Int32
  -> IO Float

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_float@
hs_bindgen_2d2ce0d386f26293 ::
     A
  -> IO FC.CFloat
hs_bindgen_2d2ce0d386f26293 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_2d2ce0d386f26293_base

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h 65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CFloat
ret_float = hs_bindgen_2d2ce0d386f26293

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_double@
foreign import ccall unsafe "hs_bindgen_de353a737de53428" hs_bindgen_de353a737de53428_base ::
     GHC.Int.Int32
  -> IO Double

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_double@
hs_bindgen_de353a737de53428 ::
     A
  -> IO FC.CDouble
hs_bindgen_de353a737de53428 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_de353a737de53428_base

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h 66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CDouble
ret_double = hs_bindgen_de353a737de53428

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_bool1@
foreign import ccall unsafe "hs_bindgen_91e2ab77e68f0288" hs_bindgen_91e2ab77e68f0288_base ::
     GHC.Int.Int32
  -> IO GHC.Word.Word8

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_bool1@
hs_bindgen_91e2ab77e68f0288 ::
     A
  -> IO FC.CBool
hs_bindgen_91e2ab77e68f0288 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_91e2ab77e68f0288_base

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h 67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CBool
ret_bool1 = hs_bindgen_91e2ab77e68f0288

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_struct@
foreign import ccall unsafe "hs_bindgen_9f29c7eee02f6d53" hs_bindgen_9f29c7eee02f6d53_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_struct@
hs_bindgen_9f29c7eee02f6d53 ::
     A
  -> Ptr.Ptr Some_struct
  -> IO ()
hs_bindgen_9f29c7eee02f6d53 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_9f29c7eee02f6d53_base

{-| __C declaration:__ @ret_struct@

    __defined at:__ @macros\/reparse.h 69:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_struct ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO Some_struct
ret_struct =
  \arg10 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\res1 ->
                                            hs_bindgen_9f29c7eee02f6d53 arg10 res1)

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_union@
foreign import ccall unsafe "hs_bindgen_6844bf5f5a5f6681" hs_bindgen_6844bf5f5a5f6681_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_union@
hs_bindgen_6844bf5f5a5f6681 ::
     A
  -> Ptr.Ptr Some_union
  -> IO ()
hs_bindgen_6844bf5f5a5f6681 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_6844bf5f5a5f6681_base

{-| __C declaration:__ @ret_union@

    __defined at:__ @macros\/reparse.h 70:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_union ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO Some_union
ret_union =
  \arg10 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\res1 ->
                                            hs_bindgen_6844bf5f5a5f6681 arg10 res1)

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_enum@
foreign import ccall unsafe "hs_bindgen_f96c4bc30b6b17e8" hs_bindgen_f96c4bc30b6b17e8_base ::
     GHC.Int.Int32
  -> IO GHC.Word.Word32

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_enum@
hs_bindgen_f96c4bc30b6b17e8 ::
     A
  -> IO Some_enum
hs_bindgen_f96c4bc30b6b17e8 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_f96c4bc30b6b17e8_base

{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h 71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO Some_enum
ret_enum = hs_bindgen_f96c4bc30b6b17e8

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_pointer1@
foreign import ccall unsafe "hs_bindgen_bfb6069e1423e7a5" hs_bindgen_bfb6069e1423e7a5_base ::
     GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_pointer1@
hs_bindgen_bfb6069e1423e7a5 ::
     A
  -> IO (Ptr.Ptr FC.CInt)
hs_bindgen_bfb6069e1423e7a5 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_bfb6069e1423e7a5_base

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h 73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.Ptr FC.CInt)
ret_pointer1 = hs_bindgen_bfb6069e1423e7a5

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_pointer2@
foreign import ccall unsafe "hs_bindgen_ffae633548386d89" hs_bindgen_ffae633548386d89_base ::
     GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_pointer2@
hs_bindgen_ffae633548386d89 ::
     A
  -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt))
hs_bindgen_ffae633548386d89 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_ffae633548386d89_base

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h 74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt))
ret_pointer2 = hs_bindgen_ffae633548386d89

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_pointer3@
foreign import ccall unsafe "hs_bindgen_550cb4a23c6ab2ff" hs_bindgen_550cb4a23c6ab2ff_base ::
     GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_pointer3@
hs_bindgen_550cb4a23c6ab2ff ::
     A
  -> IO (Ptr.Ptr Void)
hs_bindgen_550cb4a23c6ab2ff =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_550cb4a23c6ab2ff_base

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h 75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.Ptr Void)
ret_pointer3 = hs_bindgen_550cb4a23c6ab2ff

-- __unique:__ @test_macrosreparse_Example_Unsafe_body1@
foreign import ccall unsafe "hs_bindgen_f7a7a45a80ae39f7" hs_bindgen_f7a7a45a80ae39f7_base ::
     GHC.Int.Int32
  -> IO GHC.Int.Int32

-- __unique:__ @test_macrosreparse_Example_Unsafe_body1@
hs_bindgen_f7a7a45a80ae39f7 ::
     A
  -> IO FC.CInt
hs_bindgen_f7a7a45a80ae39f7 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_f7a7a45a80ae39f7_base

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h 79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CInt
body1 = hs_bindgen_f7a7a45a80ae39f7

-- __unique:__ @test_macrosreparse_Example_Unsafe_body2@
foreign import ccall unsafe "hs_bindgen_364e73b014d7d4df" hs_bindgen_364e73b014d7d4df_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_macrosreparse_Example_Unsafe_body2@
hs_bindgen_364e73b014d7d4df :: IO A
hs_bindgen_364e73b014d7d4df =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_364e73b014d7d4df_base

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h 80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2 :: IO A
body2 = hs_bindgen_364e73b014d7d4df

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_complex_float@
foreign import ccall unsafe "hs_bindgen_88b4cd11afc4f6c1" hs_bindgen_88b4cd11afc4f6c1_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_complex_float@
hs_bindgen_88b4cd11afc4f6c1 ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> IO ()
hs_bindgen_88b4cd11afc4f6c1 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_88b4cd11afc4f6c1_base

{-| __C declaration:__ @args_complex_float@

    __defined at:__ @macros\/reparse.h 84:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_float ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Data.Complex.Complex FC.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_complex_float =
  \arg10 ->
    \arg21 ->
      F.with arg21 (\arg22 ->
                      hs_bindgen_88b4cd11afc4f6c1 arg10 arg22)

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_complex_double@
foreign import ccall unsafe "hs_bindgen_0ddc53d8e91cb32a" hs_bindgen_0ddc53d8e91cb32a_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_args_complex_double@
hs_bindgen_0ddc53d8e91cb32a ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> IO ()
hs_bindgen_0ddc53d8e91cb32a =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_0ddc53d8e91cb32a_base

{-| __C declaration:__ @args_complex_double@

    __defined at:__ @macros\/reparse.h 85:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_double ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Data.Complex.Complex FC.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_complex_double =
  \arg10 ->
    \arg21 ->
      F.with arg21 (\arg22 ->
                      hs_bindgen_0ddc53d8e91cb32a arg10 arg22)

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_complex_float@
foreign import ccall unsafe "hs_bindgen_eb82eb840e288900" hs_bindgen_eb82eb840e288900_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_complex_float@
hs_bindgen_eb82eb840e288900 ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> IO ()
hs_bindgen_eb82eb840e288900 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_eb82eb840e288900_base

{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @macros\/reparse.h 86:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_float ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Data.Complex.Complex FC.CFloat)
ret_complex_float =
  \arg10 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\res1 ->
                                            hs_bindgen_eb82eb840e288900 arg10 res1)

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_complex_double@
foreign import ccall unsafe "hs_bindgen_cbc25ea9cbdd2365" hs_bindgen_cbc25ea9cbdd2365_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_ret_complex_double@
hs_bindgen_cbc25ea9cbdd2365 ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> IO ()
hs_bindgen_cbc25ea9cbdd2365 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_cbc25ea9cbdd2365_base

{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @macros\/reparse.h 87:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_double ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Data.Complex.Complex FC.CDouble)
ret_complex_double =
  \arg10 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\res1 ->
                                            hs_bindgen_cbc25ea9cbdd2365 arg10 res1)

-- __unique:__ @test_macrosreparse_Example_Unsafe_bespoke_args1@
foreign import ccall unsafe "hs_bindgen_3258de4ffd2c08af" hs_bindgen_3258de4ffd2c08af_base ::
     GHC.Int.Int32
  -> GHC.Word.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_bespoke_args1@
hs_bindgen_3258de4ffd2c08af ::
     A
  -> FC.CBool
  -> IO ()
hs_bindgen_3258de4ffd2c08af =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_3258de4ffd2c08af_base

{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @macros\/reparse.h 94:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
bespoke_args1 = hs_bindgen_3258de4ffd2c08af

-- __unique:__ @test_macrosreparse_Example_Unsafe_bespoke_args2@
foreign import ccall unsafe "hs_bindgen_74b2cd1defdd5609" hs_bindgen_74b2cd1defdd5609_base ::
     GHC.Int.Int32
  -> GHC.Word.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_bespoke_args2@
hs_bindgen_74b2cd1defdd5609 ::
     A
  -> HsBindgen.Runtime.Prelude.CSize
  -> IO ()
hs_bindgen_74b2cd1defdd5609 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_74b2cd1defdd5609_base

{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @macros\/reparse.h 95:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.Prelude.CSize
     -- ^ __C declaration:__ @arg2@
  -> IO ()
bespoke_args2 = hs_bindgen_74b2cd1defdd5609

-- __unique:__ @test_macrosreparse_Example_Unsafe_bespoke_ret1@
foreign import ccall unsafe "hs_bindgen_5405c1e037d1e115" hs_bindgen_5405c1e037d1e115_base ::
     GHC.Int.Int32
  -> IO GHC.Word.Word8

-- __unique:__ @test_macrosreparse_Example_Unsafe_bespoke_ret1@
hs_bindgen_5405c1e037d1e115 ::
     A
  -> IO FC.CBool
hs_bindgen_5405c1e037d1e115 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_5405c1e037d1e115_base

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h 97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CBool
bespoke_ret1 = hs_bindgen_5405c1e037d1e115

-- __unique:__ @test_macrosreparse_Example_Unsafe_bespoke_ret2@
foreign import ccall unsafe "hs_bindgen_a6a3e5a828532360" hs_bindgen_a6a3e5a828532360_base ::
     GHC.Int.Int32
  -> IO GHC.Word.Word64

-- __unique:__ @test_macrosreparse_Example_Unsafe_bespoke_ret2@
hs_bindgen_a6a3e5a828532360 ::
     A
  -> IO HsBindgen.Runtime.Prelude.CSize
hs_bindgen_a6a3e5a828532360 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_a6a3e5a828532360_base

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h 98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO HsBindgen.Runtime.Prelude.CSize
bespoke_ret2 = hs_bindgen_a6a3e5a828532360

-- __unique:__ @test_macrosreparse_Example_Unsafe_arr_args1@
foreign import ccall unsafe "hs_bindgen_4956a52bf5073b9f" hs_bindgen_4956a52bf5073b9f_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_arr_args1@
hs_bindgen_4956a52bf5073b9f ::
     Ptr.Ptr A
  -> IO ()
hs_bindgen_4956a52bf5073b9f =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_4956a52bf5073b9f_base

{-| Arrays

__C declaration:__ @arr_args1@

__defined at:__ @macros\/reparse.h 104:6@

__exported by:__ @macros\/reparse.h@
-}
arr_args1 ::
     Ptr.Ptr A
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args1 = hs_bindgen_4956a52bf5073b9f

-- __unique:__ @test_macrosreparse_Example_Unsafe_arr_args2@
foreign import ccall unsafe "hs_bindgen_0fc8b091085a88e9" hs_bindgen_0fc8b091085a88e9_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_arr_args2@
hs_bindgen_0fc8b091085a88e9 ::
     Ptr.Ptr (Ptr.Ptr A)
  -> IO ()
hs_bindgen_0fc8b091085a88e9 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_0fc8b091085a88e9_base

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h 105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2 ::
     Ptr.Ptr (Ptr.Ptr A)
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args2 = hs_bindgen_0fc8b091085a88e9

-- __unique:__ @test_macrosreparse_Example_Unsafe_arr_args3@
foreign import ccall unsafe "hs_bindgen_ca6f1bc1a29b85f8" hs_bindgen_ca6f1bc1a29b85f8_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_arr_args3@
hs_bindgen_ca6f1bc1a29b85f8 ::
     Ptr.Ptr A
  -> IO ()
hs_bindgen_ca6f1bc1a29b85f8 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_ca6f1bc1a29b85f8_base

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h 106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3 ::
     Ptr.Ptr A
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args3 = hs_bindgen_ca6f1bc1a29b85f8

-- __unique:__ @test_macrosreparse_Example_Unsafe_arr_args4@
foreign import ccall unsafe "hs_bindgen_a168ae0de206febe" hs_bindgen_a168ae0de206febe_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_arr_args4@
hs_bindgen_a168ae0de206febe ::
     Ptr.Ptr (Ptr.Ptr A)
  -> IO ()
hs_bindgen_a168ae0de206febe =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_a168ae0de206febe_base

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h 107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4 ::
     Ptr.Ptr (Ptr.Ptr A)
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args4 = hs_bindgen_a168ae0de206febe

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_args1@
foreign import ccall unsafe "hs_bindgen_8e63f57f1f5d662e" hs_bindgen_8e63f57f1f5d662e_base ::
     GHC.Int.Int32
  -> Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_args1@
hs_bindgen_8e63f57f1f5d662e ::
     A
  -> Ptr.FunPtr (IO ())
  -> IO ()
hs_bindgen_8e63f57f1f5d662e =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_8e63f57f1f5d662e_base

{-| Function pointers

__C declaration:__ @funptr_args1@

__defined at:__ @macros\/reparse.h 126:6@

__exported by:__ @macros\/reparse.h@
-}
funptr_args1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.FunPtr (IO ())
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args1 = hs_bindgen_8e63f57f1f5d662e

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_args2@
foreign import ccall unsafe "hs_bindgen_927bd07f48d05d21" hs_bindgen_927bd07f48d05d21_base ::
     GHC.Int.Int32
  -> Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_args2@
hs_bindgen_927bd07f48d05d21 ::
     A
  -> Ptr.FunPtr (IO FC.CInt)
  -> IO ()
hs_bindgen_927bd07f48d05d21 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_927bd07f48d05d21_base

{-| __C declaration:__ @funptr_args2@

    __defined at:__ @macros\/reparse.h 127:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.FunPtr (IO FC.CInt)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args2 = hs_bindgen_927bd07f48d05d21

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_args3@
foreign import ccall unsafe "hs_bindgen_c82e078d3c54a6bc" hs_bindgen_c82e078d3c54a6bc_base ::
     GHC.Int.Int32
  -> Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_args3@
hs_bindgen_c82e078d3c54a6bc ::
     A
  -> Ptr.FunPtr (FC.CInt -> IO ())
  -> IO ()
hs_bindgen_c82e078d3c54a6bc =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_c82e078d3c54a6bc_base

{-| __C declaration:__ @funptr_args3@

    __defined at:__ @macros\/reparse.h 128:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.FunPtr (FC.CInt -> IO ())
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args3 = hs_bindgen_c82e078d3c54a6bc

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_args4@
foreign import ccall unsafe "hs_bindgen_211ad1ac5399caec" hs_bindgen_211ad1ac5399caec_base ::
     GHC.Int.Int32
  -> Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_args4@
hs_bindgen_211ad1ac5399caec ::
     A
  -> Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)
  -> IO ()
hs_bindgen_211ad1ac5399caec =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_211ad1ac5399caec_base

{-| __C declaration:__ @funptr_args4@

    __defined at:__ @macros\/reparse.h 129:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args4 = hs_bindgen_211ad1ac5399caec

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_args5@
foreign import ccall unsafe "hs_bindgen_9057c59d70e815d7" hs_bindgen_9057c59d70e815d7_base ::
     GHC.Int.Int32
  -> Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_args5@
hs_bindgen_9057c59d70e815d7 ::
     A
  -> Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))
  -> IO ()
hs_bindgen_9057c59d70e815d7 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_9057c59d70e815d7_base

{-| __C declaration:__ @funptr_args5@

    __defined at:__ @macros\/reparse.h 130:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args5 = hs_bindgen_9057c59d70e815d7

-- __unique:__ @test_macrosreparse_Example_Unsafe_comments1@
foreign import ccall unsafe "hs_bindgen_153515e0ff74574f" hs_bindgen_153515e0ff74574f_base ::
     GHC.Int.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_comments1@
hs_bindgen_153515e0ff74574f ::
     A
  -> IO ()
hs_bindgen_153515e0ff74574f =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_153515e0ff74574f_base

{-| Comments in awkward places

  (Prior to language-c we failed to parse there.)

__C declaration:__ @comments1@

__defined at:__ @macros\/reparse.h 144:25@

__exported by:__ @macros\/reparse.h@
-}
comments1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO ()
comments1 = hs_bindgen_153515e0ff74574f

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_prim_before1@
foreign import ccall unsafe "hs_bindgen_8cc833db463cc95c" hs_bindgen_8cc833db463cc95c_base ::
     GHC.Int.Int32
  -> GHC.Int.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_prim_before1@
hs_bindgen_8cc833db463cc95c ::
     A
  -> FC.CChar
  -> IO ()
hs_bindgen_8cc833db463cc95c =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_8cc833db463cc95c_base

{-| `const` qualifier

  NOTE: These were not parsed correctly prior to the switch to language-c.

__C declaration:__ @const_prim_before1@

__defined at:__ @macros\/reparse.h 179:6@

__exported by:__ @macros\/reparse.h@
-}
const_prim_before1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_before1 = hs_bindgen_8cc833db463cc95c

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_prim_before2@
foreign import ccall unsafe "hs_bindgen_d767bbef00031d57" hs_bindgen_d767bbef00031d57_base ::
     GHC.Int.Int32
  -> GHC.Int.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_prim_before2@
hs_bindgen_d767bbef00031d57 ::
     A
  -> FC.CSChar
  -> IO ()
hs_bindgen_d767bbef00031d57 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d767bbef00031d57_base

{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @macros\/reparse.h 180:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CSChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_before2 = hs_bindgen_d767bbef00031d57

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_prim_before3@
foreign import ccall unsafe "hs_bindgen_a36dfeb811993297" hs_bindgen_a36dfeb811993297_base ::
     GHC.Int.Int32
  -> GHC.Word.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_prim_before3@
hs_bindgen_a36dfeb811993297 ::
     A
  -> FC.CUChar
  -> IO ()
hs_bindgen_a36dfeb811993297 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_a36dfeb811993297_base

{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @macros\/reparse.h 181:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CUChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_before3 = hs_bindgen_a36dfeb811993297

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_prim_after1@
foreign import ccall unsafe "hs_bindgen_d7fa2440be24e954" hs_bindgen_d7fa2440be24e954_base ::
     GHC.Int.Int32
  -> GHC.Int.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_prim_after1@
hs_bindgen_d7fa2440be24e954 ::
     A
  -> FC.CChar
  -> IO ()
hs_bindgen_d7fa2440be24e954 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d7fa2440be24e954_base

{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @macros\/reparse.h 182:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_after1 = hs_bindgen_d7fa2440be24e954

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_prim_after2@
foreign import ccall unsafe "hs_bindgen_c169229f24baf752" hs_bindgen_c169229f24baf752_base ::
     GHC.Int.Int32
  -> GHC.Int.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_prim_after2@
hs_bindgen_c169229f24baf752 ::
     A
  -> FC.CSChar
  -> IO ()
hs_bindgen_c169229f24baf752 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_c169229f24baf752_base

{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @macros\/reparse.h 183:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CSChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_after2 = hs_bindgen_c169229f24baf752

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_prim_after3@
foreign import ccall unsafe "hs_bindgen_c0780f7624ed1d3e" hs_bindgen_c0780f7624ed1d3e_base ::
     GHC.Int.Int32
  -> GHC.Word.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_prim_after3@
hs_bindgen_c0780f7624ed1d3e ::
     A
  -> FC.CUChar
  -> IO ()
hs_bindgen_c0780f7624ed1d3e =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_c0780f7624ed1d3e_base

{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @macros\/reparse.h 184:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CUChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_after3 = hs_bindgen_c0780f7624ed1d3e

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_before1@
foreign import ccall unsafe "hs_bindgen_fda903bc1139b1d6" hs_bindgen_fda903bc1139b1d6_base ::
     GHC.Int.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_before1@
hs_bindgen_fda903bc1139b1d6 ::
     A
  -> FC.CFloat
  -> IO ()
hs_bindgen_fda903bc1139b1d6 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_fda903bc1139b1d6_base

{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @macros\/reparse.h 188:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before1 =
  hs_bindgen_fda903bc1139b1d6

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_before2@
foreign import ccall unsafe "hs_bindgen_a5a70f3be654ea00" hs_bindgen_a5a70f3be654ea00_base ::
     GHC.Int.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_before2@
hs_bindgen_a5a70f3be654ea00 ::
     A
  -> FC.CDouble
  -> IO ()
hs_bindgen_a5a70f3be654ea00 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_a5a70f3be654ea00_base

{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @macros\/reparse.h 189:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before2 =
  hs_bindgen_a5a70f3be654ea00

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_before3@
foreign import ccall unsafe "hs_bindgen_b813910f6a632ce2" hs_bindgen_b813910f6a632ce2_base ::
     GHC.Int.Int32
  -> GHC.Word.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_before3@
hs_bindgen_b813910f6a632ce2 ::
     A
  -> FC.CBool
  -> IO ()
hs_bindgen_b813910f6a632ce2 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_b813910f6a632ce2_base

{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @macros\/reparse.h 190:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before3 =
  hs_bindgen_b813910f6a632ce2

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_before4@
foreign import ccall unsafe "hs_bindgen_dc22b02b2f53aa5b" hs_bindgen_dc22b02b2f53aa5b_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_before4@
hs_bindgen_dc22b02b2f53aa5b ::
     A
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Some_struct
  -> IO ()
hs_bindgen_dc22b02b2f53aa5b =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_dc22b02b2f53aa5b_base

{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @macros\/reparse.h 191:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_struct
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before4 =
  \arg10 ->
    \arg21 ->
      F.with arg21 (\arg22 ->
                      hs_bindgen_dc22b02b2f53aa5b arg10 (HsBindgen.Runtime.ConstPtr.ConstPtr arg22))

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_before5@
foreign import ccall unsafe "hs_bindgen_503736261279760d" hs_bindgen_503736261279760d_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_before5@
hs_bindgen_503736261279760d ::
     A
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Some_union
  -> IO ()
hs_bindgen_503736261279760d =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_503736261279760d_base

{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @macros\/reparse.h 192:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_union
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before5 =
  \arg10 ->
    \arg21 ->
      F.with arg21 (\arg22 ->
                      hs_bindgen_503736261279760d arg10 (HsBindgen.Runtime.ConstPtr.ConstPtr arg22))

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_before6@
foreign import ccall unsafe "hs_bindgen_ed0a8c0e15f5d2ce" hs_bindgen_ed0a8c0e15f5d2ce_base ::
     GHC.Int.Int32
  -> GHC.Word.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_before6@
hs_bindgen_ed0a8c0e15f5d2ce ::
     A
  -> Some_enum
  -> IO ()
hs_bindgen_ed0a8c0e15f5d2ce =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_ed0a8c0e15f5d2ce_base

{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @macros\/reparse.h 193:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before6 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_enum
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before6 =
  hs_bindgen_ed0a8c0e15f5d2ce

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_before7@
foreign import ccall unsafe "hs_bindgen_4659c22d39cc1bb3" hs_bindgen_4659c22d39cc1bb3_base ::
     GHC.Int.Int32
  -> GHC.Word.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_before7@
hs_bindgen_4659c22d39cc1bb3 ::
     A
  -> FC.CBool
  -> IO ()
hs_bindgen_4659c22d39cc1bb3 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_4659c22d39cc1bb3_base

{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @macros\/reparse.h 194:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before7 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before7 =
  hs_bindgen_4659c22d39cc1bb3

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_before8@
foreign import ccall unsafe "hs_bindgen_530245b77093b08c" hs_bindgen_530245b77093b08c_base ::
     GHC.Int.Int32
  -> GHC.Word.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_before8@
hs_bindgen_530245b77093b08c ::
     A
  -> HsBindgen.Runtime.Prelude.CSize
  -> IO ()
hs_bindgen_530245b77093b08c =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_530245b77093b08c_base

{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @macros\/reparse.h 195:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before8 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.Prelude.CSize
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before8 =
  hs_bindgen_530245b77093b08c

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_after1@
foreign import ccall unsafe "hs_bindgen_c31a804bd742193e" hs_bindgen_c31a804bd742193e_base ::
     GHC.Int.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_after1@
hs_bindgen_c31a804bd742193e ::
     A
  -> FC.CFloat
  -> IO ()
hs_bindgen_c31a804bd742193e =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_c31a804bd742193e_base

{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @macros\/reparse.h 197:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after1 =
  hs_bindgen_c31a804bd742193e

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_after2@
foreign import ccall unsafe "hs_bindgen_53756fa3a68ab067" hs_bindgen_53756fa3a68ab067_base ::
     GHC.Int.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_after2@
hs_bindgen_53756fa3a68ab067 ::
     A
  -> FC.CDouble
  -> IO ()
hs_bindgen_53756fa3a68ab067 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_53756fa3a68ab067_base

{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @macros\/reparse.h 198:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after2 =
  hs_bindgen_53756fa3a68ab067

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_after3@
foreign import ccall unsafe "hs_bindgen_4134ad71149d6139" hs_bindgen_4134ad71149d6139_base ::
     GHC.Int.Int32
  -> GHC.Word.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_after3@
hs_bindgen_4134ad71149d6139 ::
     A
  -> FC.CBool
  -> IO ()
hs_bindgen_4134ad71149d6139 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_4134ad71149d6139_base

{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @macros\/reparse.h 199:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after3 =
  hs_bindgen_4134ad71149d6139

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_after4@
foreign import ccall unsafe "hs_bindgen_3de6157427334101" hs_bindgen_3de6157427334101_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_after4@
hs_bindgen_3de6157427334101 ::
     A
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Some_struct
  -> IO ()
hs_bindgen_3de6157427334101 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_3de6157427334101_base

{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @macros\/reparse.h 200:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_struct
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after4 =
  \arg10 ->
    \arg21 ->
      F.with arg21 (\arg22 ->
                      hs_bindgen_3de6157427334101 arg10 (HsBindgen.Runtime.ConstPtr.ConstPtr arg22))

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_after5@
foreign import ccall unsafe "hs_bindgen_fc4ef8c9107c1ae6" hs_bindgen_fc4ef8c9107c1ae6_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_after5@
hs_bindgen_fc4ef8c9107c1ae6 ::
     A
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Some_union
  -> IO ()
hs_bindgen_fc4ef8c9107c1ae6 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_fc4ef8c9107c1ae6_base

{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @macros\/reparse.h 201:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_union
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after5 =
  \arg10 ->
    \arg21 ->
      F.with arg21 (\arg22 ->
                      hs_bindgen_fc4ef8c9107c1ae6 arg10 (HsBindgen.Runtime.ConstPtr.ConstPtr arg22))

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_after6@
foreign import ccall unsafe "hs_bindgen_5e20c60b725ae606" hs_bindgen_5e20c60b725ae606_base ::
     GHC.Int.Int32
  -> GHC.Word.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_after6@
hs_bindgen_5e20c60b725ae606 ::
     A
  -> Some_enum
  -> IO ()
hs_bindgen_5e20c60b725ae606 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_5e20c60b725ae606_base

{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @macros\/reparse.h 202:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after6 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_enum
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after6 =
  hs_bindgen_5e20c60b725ae606

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_after7@
foreign import ccall unsafe "hs_bindgen_a0f20d4b9a07ff5b" hs_bindgen_a0f20d4b9a07ff5b_base ::
     GHC.Int.Int32
  -> GHC.Word.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_after7@
hs_bindgen_a0f20d4b9a07ff5b ::
     A
  -> FC.CBool
  -> IO ()
hs_bindgen_a0f20d4b9a07ff5b =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_a0f20d4b9a07ff5b_base

{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @macros\/reparse.h 203:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after7 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after7 =
  hs_bindgen_a0f20d4b9a07ff5b

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_after8@
foreign import ccall unsafe "hs_bindgen_3a020035eb2fe7f8" hs_bindgen_3a020035eb2fe7f8_base ::
     GHC.Int.Int32
  -> GHC.Word.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_withoutSign_after8@
hs_bindgen_3a020035eb2fe7f8 ::
     A
  -> HsBindgen.Runtime.Prelude.CSize
  -> IO ()
hs_bindgen_3a020035eb2fe7f8 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_3a020035eb2fe7f8_base

{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @macros\/reparse.h 204:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after8 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.Prelude.CSize
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after8 =
  hs_bindgen_3a020035eb2fe7f8

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_args1@
foreign import ccall unsafe "hs_bindgen_17623ba5065bf95d" hs_bindgen_17623ba5065bf95d_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_args1@
hs_bindgen_17623ba5065bf95d ::
     A
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
  -> IO ()
hs_bindgen_17623ba5065bf95d =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_17623ba5065bf95d_base

{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @macros\/reparse.h 208:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args1 = hs_bindgen_17623ba5065bf95d

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_args2@
foreign import ccall unsafe "hs_bindgen_02d08ccd5df88a98" hs_bindgen_02d08ccd5df88a98_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_args2@
hs_bindgen_02d08ccd5df88a98 ::
     A
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
  -> IO ()
hs_bindgen_02d08ccd5df88a98 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_02d08ccd5df88a98_base

{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @macros\/reparse.h 209:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args2 = hs_bindgen_02d08ccd5df88a98

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_args3@
foreign import ccall unsafe "hs_bindgen_50c423f2237cb6b5" hs_bindgen_50c423f2237cb6b5_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_args3@
hs_bindgen_50c423f2237cb6b5 ::
     A
  -> Ptr.Ptr FC.CInt
  -> IO ()
hs_bindgen_50c423f2237cb6b5 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_50c423f2237cb6b5_base

{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @macros\/reparse.h 210:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args3 = hs_bindgen_50c423f2237cb6b5

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_args4@
foreign import ccall unsafe "hs_bindgen_99c29c45d78348e9" hs_bindgen_99c29c45d78348e9_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_args4@
hs_bindgen_99c29c45d78348e9 ::
     A
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
  -> IO ()
hs_bindgen_99c29c45d78348e9 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_99c29c45d78348e9_base

{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @macros\/reparse.h 211:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args4 = hs_bindgen_99c29c45d78348e9

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_args5@
foreign import ccall unsafe "hs_bindgen_6a92dbfae24b1bcd" hs_bindgen_6a92dbfae24b1bcd_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_args5@
hs_bindgen_6a92dbfae24b1bcd ::
     A
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
  -> IO ()
hs_bindgen_6a92dbfae24b1bcd =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_6a92dbfae24b1bcd_base

{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @macros\/reparse.h 212:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args5 = hs_bindgen_6a92dbfae24b1bcd

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_ret1@
foreign import ccall unsafe "hs_bindgen_0c07f1e0256fd705" hs_bindgen_0c07f1e0256fd705_base ::
     GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_ret1@
hs_bindgen_0c07f1e0256fd705 ::
     A
  -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
hs_bindgen_0c07f1e0256fd705 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_0c07f1e0256fd705_base

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h 214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
const_pointers_ret1 = hs_bindgen_0c07f1e0256fd705

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_ret2@
foreign import ccall unsafe "hs_bindgen_d12c8210ff3c3711" hs_bindgen_d12c8210ff3c3711_base ::
     GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_ret2@
hs_bindgen_d12c8210ff3c3711 ::
     A
  -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
hs_bindgen_d12c8210ff3c3711 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d12c8210ff3c3711_base

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h 215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
const_pointers_ret2 = hs_bindgen_d12c8210ff3c3711

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_ret3@
foreign import ccall unsafe "hs_bindgen_a58bc0be6f564801" hs_bindgen_a58bc0be6f564801_base ::
     GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_ret3@
hs_bindgen_a58bc0be6f564801 ::
     A
  -> IO (Ptr.Ptr FC.CInt)
hs_bindgen_a58bc0be6f564801 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_a58bc0be6f564801_base

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h 216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.Ptr FC.CInt)
const_pointers_ret3 = hs_bindgen_a58bc0be6f564801

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_ret4@
foreign import ccall unsafe "hs_bindgen_622bb8150470138b" hs_bindgen_622bb8150470138b_base ::
     GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_ret4@
hs_bindgen_622bb8150470138b ::
     A
  -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
hs_bindgen_622bb8150470138b =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_622bb8150470138b_base

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h 217:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
const_pointers_ret4 = hs_bindgen_622bb8150470138b

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_ret5@
foreign import ccall unsafe "hs_bindgen_d49bd331ad2077e5" hs_bindgen_d49bd331ad2077e5_base ::
     GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_pointers_ret5@
hs_bindgen_d49bd331ad2077e5 ::
     A
  -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
hs_bindgen_d49bd331ad2077e5 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d49bd331ad2077e5_base

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h 218:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
const_pointers_ret5 = hs_bindgen_d49bd331ad2077e5

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_array_elem1@
foreign import ccall unsafe "hs_bindgen_224608f780bff5bd" hs_bindgen_224608f780bff5bd_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_array_elem1@
hs_bindgen_224608f780bff5bd ::
     HsBindgen.Runtime.ConstPtr.ConstPtr A
  -> IO ()
hs_bindgen_224608f780bff5bd =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_224608f780bff5bd_base

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h 246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 ::
     HsBindgen.Runtime.ConstPtr.ConstPtr A
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem1 = hs_bindgen_224608f780bff5bd

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_array_elem2@
foreign import ccall unsafe "hs_bindgen_9aa74ad89f2c1fba" hs_bindgen_9aa74ad89f2c1fba_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_array_elem2@
hs_bindgen_9aa74ad89f2c1fba ::
     Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr A)
  -> IO ()
hs_bindgen_9aa74ad89f2c1fba =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_9aa74ad89f2c1fba_base

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h 247:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2 ::
     Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr A)
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem2 = hs_bindgen_9aa74ad89f2c1fba

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_array_elem3@
foreign import ccall unsafe "hs_bindgen_6a328300c5ef0c9e" hs_bindgen_6a328300c5ef0c9e_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_const_array_elem3@
hs_bindgen_6a328300c5ef0c9e ::
     HsBindgen.Runtime.ConstPtr.ConstPtr (Ptr.Ptr A)
  -> IO ()
hs_bindgen_6a328300c5ef0c9e =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_6a328300c5ef0c9e_base

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h 248:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 ::
     HsBindgen.Runtime.ConstPtr.ConstPtr (Ptr.Ptr A)
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem3 = hs_bindgen_6a328300c5ef0c9e

-- __unique:__ @test_macrosreparse_Example_Unsafe_noParams1@
foreign import ccall unsafe "hs_bindgen_13a7d78e11555d58" hs_bindgen_13a7d78e11555d58_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_macrosreparse_Example_Unsafe_noParams1@
hs_bindgen_13a7d78e11555d58 :: IO A
hs_bindgen_13a7d78e11555d58 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_13a7d78e11555d58_base

{-| Other examples we reparsed /incorrectly/ before language-c

__C declaration:__ @noParams1@

__defined at:__ @macros\/reparse.h 256:3@

__exported by:__ @macros\/reparse.h@
-}
noParams1 :: IO A
noParams1 = hs_bindgen_13a7d78e11555d58

-- __unique:__ @test_macrosreparse_Example_Unsafe_noParams2@
foreign import ccall unsafe "hs_bindgen_672f4691ee7a367c" hs_bindgen_672f4691ee7a367c_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_macrosreparse_Example_Unsafe_noParams2@
hs_bindgen_672f4691ee7a367c :: IO A
hs_bindgen_672f4691ee7a367c =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_672f4691ee7a367c_base

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h 257:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2 :: IO A
noParams2 = hs_bindgen_672f4691ee7a367c

-- __unique:__ @test_macrosreparse_Example_Unsafe_noParams3@
foreign import ccall unsafe "hs_bindgen_591f84e2163a5d18" hs_bindgen_591f84e2163a5d18_base ::
     GHC.Int.Int32
  -> Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Unsafe_noParams3@
hs_bindgen_591f84e2163a5d18 ::
     A
  -> Ptr.FunPtr (IO FC.CInt)
  -> IO ()
hs_bindgen_591f84e2163a5d18 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_591f84e2163a5d18_base

{-| __C declaration:__ @noParams3@

    __defined at:__ @macros\/reparse.h 258:6@

    __exported by:__ @macros\/reparse.h@
-}
noParams3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.FunPtr (IO FC.CInt)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
noParams3 = hs_bindgen_591f84e2163a5d18

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret1@
foreign import ccall unsafe "hs_bindgen_8cdf7774adb0f0b4" hs_bindgen_8cdf7774adb0f0b4_base ::
     GHC.Int.Int32
  -> IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret1@
hs_bindgen_8cdf7774adb0f0b4 ::
     A
  -> IO (Ptr.FunPtr (IO ()))
hs_bindgen_8cdf7774adb0f0b4 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_8cdf7774adb0f0b4_base

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h 262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (IO ()))
funptr_ret1 = hs_bindgen_8cdf7774adb0f0b4

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret2@
foreign import ccall unsafe "hs_bindgen_a4e08267a9070ede" hs_bindgen_a4e08267a9070ede_base ::
     GHC.Int.Int32
  -> IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret2@
hs_bindgen_a4e08267a9070ede ::
     A
  -> IO (Ptr.FunPtr (IO FC.CInt))
hs_bindgen_a4e08267a9070ede =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_a4e08267a9070ede_base

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h 263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (IO FC.CInt))
funptr_ret2 = hs_bindgen_a4e08267a9070ede

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret3@
foreign import ccall unsafe "hs_bindgen_65fa30510d244cbf" hs_bindgen_65fa30510d244cbf_base ::
     GHC.Int.Int32
  -> IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret3@
hs_bindgen_65fa30510d244cbf ::
     A
  -> IO (Ptr.FunPtr (FC.CInt -> IO ()))
hs_bindgen_65fa30510d244cbf =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_65fa30510d244cbf_base

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h 264:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (FC.CInt -> IO ()))
funptr_ret3 = hs_bindgen_65fa30510d244cbf

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret4@
foreign import ccall unsafe "hs_bindgen_da12eaec295883aa" hs_bindgen_da12eaec295883aa_base ::
     GHC.Int.Int32
  -> IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret4@
hs_bindgen_da12eaec295883aa ::
     A
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar))
hs_bindgen_da12eaec295883aa =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_da12eaec295883aa_base

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h 265:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar))
funptr_ret4 = hs_bindgen_da12eaec295883aa

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret5@
foreign import ccall unsafe "hs_bindgen_281c53214b1cdcb4" hs_bindgen_281c53214b1cdcb4_base ::
     GHC.Int.Int32
  -> IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret5@
hs_bindgen_281c53214b1cdcb4 ::
     A
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))
hs_bindgen_281c53214b1cdcb4 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_281c53214b1cdcb4_base

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h 269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))
funptr_ret5 = hs_bindgen_281c53214b1cdcb4

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret6@
foreign import ccall unsafe "hs_bindgen_16628c257aa64a76" hs_bindgen_16628c257aa64a76_base ::
     GHC.Int.Int32
  -> IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret6@
hs_bindgen_16628c257aa64a76 ::
     A
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))
hs_bindgen_16628c257aa64a76 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_16628c257aa64a76_base

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h 270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))
funptr_ret6 = hs_bindgen_16628c257aa64a76

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret7@
foreign import ccall unsafe "hs_bindgen_79fb0c30f546a547" hs_bindgen_79fb0c30f546a547_base ::
     GHC.Int.Int32
  -> IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret7@
hs_bindgen_79fb0c30f546a547 ::
     A
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))
hs_bindgen_79fb0c30f546a547 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_79fb0c30f546a547_base

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h 271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))
funptr_ret7 = hs_bindgen_79fb0c30f546a547

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret8@
foreign import ccall unsafe "hs_bindgen_4668d2ff9d5bfc40" hs_bindgen_4668d2ff9d5bfc40_base ::
     GHC.Int.Int32
  -> IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret8@
hs_bindgen_4668d2ff9d5bfc40 ::
     A
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))
hs_bindgen_4668d2ff9d5bfc40 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_4668d2ff9d5bfc40_base

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h 272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))
funptr_ret8 = hs_bindgen_4668d2ff9d5bfc40

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret9@
foreign import ccall unsafe "hs_bindgen_c044d7074789febc" hs_bindgen_c044d7074789febc_base ::
     GHC.Int.Int32
  -> IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret9@
hs_bindgen_c044d7074789febc ::
     A
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))
hs_bindgen_c044d7074789febc =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_c044d7074789febc_base

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h 273:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))
funptr_ret9 = hs_bindgen_c044d7074789febc

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret10@
foreign import ccall unsafe "hs_bindgen_628ced6eccc7783a" hs_bindgen_628ced6eccc7783a_base ::
     GHC.Int.Int32
  -> IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosreparse_Example_Unsafe_funptr_ret10@
hs_bindgen_628ced6eccc7783a ::
     A
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))
hs_bindgen_628ced6eccc7783a =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_628ced6eccc7783a_base

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h 274:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))
funptr_ret10 = hs_bindgen_628ced6eccc7783a
