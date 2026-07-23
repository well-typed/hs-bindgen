{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.args_char1
    , Example.Safe.args_char2
    , Example.Safe.args_char3
    , Example.Safe.args_short1
    , Example.Safe.args_short2
    , Example.Safe.args_short3
    , Example.Safe.args_int1
    , Example.Safe.args_int2
    , Example.Safe.args_int3
    , Example.Safe.args_long1
    , Example.Safe.args_long2
    , Example.Safe.args_long3
    , Example.Safe.args_float
    , Example.Safe.args_double
    , Example.Safe.args_bool1
    , Example.Safe.args_struct
    , Example.Safe.args_union
    , Example.Safe.args_enum
    , Example.Safe.args_pointer1
    , Example.Safe.args_pointer2
    , Example.Safe.args_pointer3
    , Example.Safe.ret_A
    , Example.Safe.ret_char1
    , Example.Safe.ret_char2
    , Example.Safe.ret_char3
    , Example.Safe.ret_short1
    , Example.Safe.ret_short2
    , Example.Safe.ret_short3
    , Example.Safe.ret_int1
    , Example.Safe.ret_int2
    , Example.Safe.ret_int3
    , Example.Safe.ret_long1
    , Example.Safe.ret_long2
    , Example.Safe.ret_long3
    , Example.Safe.ret_float
    , Example.Safe.ret_double
    , Example.Safe.ret_bool1
    , Example.Safe.ret_struct
    , Example.Safe.ret_union
    , Example.Safe.ret_enum
    , Example.Safe.ret_pointer1
    , Example.Safe.ret_pointer2
    , Example.Safe.ret_pointer3
    , Example.Safe.body1
    , Example.Safe.body2
    , Example.Safe.args_complex_float
    , Example.Safe.args_complex_double
    , Example.Safe.ret_complex_float
    , Example.Safe.ret_complex_double
    , Example.Safe.bespoke_args1
    , Example.Safe.bespoke_args2
    , Example.Safe.bespoke_ret1
    , Example.Safe.bespoke_ret2
    , Example.Safe.arr_args1
    , Example.Safe.arr_args2
    , Example.Safe.arr_args3
    , Example.Safe.arr_args4
    , Example.Safe.funptr_args1
    , Example.Safe.funptr_args2
    , Example.Safe.funptr_args3
    , Example.Safe.funptr_args4
    , Example.Safe.funptr_args5
    , Example.Safe.comments1
    , Example.Safe.const_prim_before1
    , Example.Safe.const_prim_before2
    , Example.Safe.const_prim_before3
    , Example.Safe.const_prim_after1
    , Example.Safe.const_prim_after2
    , Example.Safe.const_prim_after3
    , Example.Safe.const_withoutSign_before1
    , Example.Safe.const_withoutSign_before2
    , Example.Safe.const_withoutSign_before3
    , Example.Safe.const_withoutSign_before4
    , Example.Safe.const_withoutSign_before5
    , Example.Safe.const_withoutSign_before6
    , Example.Safe.const_withoutSign_before7
    , Example.Safe.const_withoutSign_before8
    , Example.Safe.const_withoutSign_after1
    , Example.Safe.const_withoutSign_after2
    , Example.Safe.const_withoutSign_after3
    , Example.Safe.const_withoutSign_after4
    , Example.Safe.const_withoutSign_after5
    , Example.Safe.const_withoutSign_after6
    , Example.Safe.const_withoutSign_after7
    , Example.Safe.const_withoutSign_after8
    , Example.Safe.const_pointers_args1
    , Example.Safe.const_pointers_args2
    , Example.Safe.const_pointers_args3
    , Example.Safe.const_pointers_args4
    , Example.Safe.const_pointers_args5
    , Example.Safe.const_pointers_ret1
    , Example.Safe.const_pointers_ret2
    , Example.Safe.const_pointers_ret3
    , Example.Safe.const_pointers_ret4
    , Example.Safe.const_pointers_ret5
    , Example.Safe.const_array_elem1
    , Example.Safe.const_array_elem2
    , Example.Safe.const_array_elem3
    , Example.Safe.noParams1
    , Example.Safe.noParams2
    , Example.Safe.noParams3
    , Example.Safe.funptr_ret1
    , Example.Safe.funptr_ret2
    , Example.Safe.funptr_ret3
    , Example.Safe.funptr_ret4
    , Example.Safe.funptr_ret5
    , Example.Safe.funptr_ret6
    , Example.Safe.funptr_ret7
    , Example.Safe.funptr_ret8
    , Example.Safe.funptr_ret9
    , Example.Safe.funptr_ret10
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse.h>"
  , "void hs_bindgen_9950741591b35cb0 ("
  , "  signed int arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  (args_char1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3b213f36de64a78b ("
  , "  signed int arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  (args_char2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d72da53837273ed1 ("
  , "  signed int arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  (args_char3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8e00ad94407efdce ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e5814201b232d64c ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3bbb21d02f9e8caa ("
  , "  signed int arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  (args_short3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_fba8d44819fb4d50 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_42cfc5afda432a51 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b877c77021f548cb ("
  , "  signed int arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  (args_int3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_365ed0d5f00ece62 ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a1204b1083b6700a ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4af0668f1653ce01 ("
  , "  signed int arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  (args_long3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b8acf83f3c3a43b0 ("
  , "  signed int arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  (args_float)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2a27bb82268cc870 ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  (args_double)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3f1481ac913e0394 ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (args_bool1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_998092a62cba613e ("
  , "  signed int arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  (args_struct)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_b83fe7017c88f63e ("
  , "  signed int arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  (args_union)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_45ef9d5579ceaa74 ("
  , "  signed int arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  (args_enum)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_862d74c6849bbcd0 ("
  , "  signed int arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  (args_pointer1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0df09844c5645db9 ("
  , "  signed int arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  (args_pointer2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_221cb5a1c4ce6a3b ("
  , "  signed int arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  (args_pointer3)(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_dfd4e13321b706b1 (void)"
  , "{"
  , "  return (ret_A)();"
  , "}"
  , "char hs_bindgen_5289300b6ae3ff51 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char1)(arg1);"
  , "}"
  , "signed char hs_bindgen_b258f6c4e2c9be38 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char2)(arg1);"
  , "}"
  , "unsigned char hs_bindgen_734b5b749255713a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char3)(arg1);"
  , "}"
  , "signed short hs_bindgen_d17d4780d98d2561 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short1)(arg1);"
  , "}"
  , "signed short hs_bindgen_1f808a3cec3781e1 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short2)(arg1);"
  , "}"
  , "unsigned short hs_bindgen_5204d0272f75f1a2 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short3)(arg1);"
  , "}"
  , "signed int hs_bindgen_14351eeb2c92732a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int1)(arg1);"
  , "}"
  , "signed int hs_bindgen_f91baafc096613a0 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int2)(arg1);"
  , "}"
  , "unsigned int hs_bindgen_e78391249ef8ffbf ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int3)(arg1);"
  , "}"
  , "signed long hs_bindgen_4f8ae7ed648c72e4 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long1)(arg1);"
  , "}"
  , "signed long hs_bindgen_c0a47369a5a10695 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long2)(arg1);"
  , "}"
  , "unsigned long hs_bindgen_64aa36b6c820a3bf ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long3)(arg1);"
  , "}"
  , "float hs_bindgen_b53c45aaccbaaf3a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_float)(arg1);"
  , "}"
  , "double hs_bindgen_0ca050f5bbe69c94 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_double)(arg1);"
  , "}"
  , "_Bool hs_bindgen_530e0725cadf8f52 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_bool1)(arg1);"
  , "}"
  , "void hs_bindgen_8f19d5621a4f3e92 ("
  , "  signed int arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_struct)(arg1);"
  , "}"
  , "void hs_bindgen_e93cfabcefb5051b ("
  , "  signed int arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_union)(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_f672d8d1020b037f ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_enum)(arg1);"
  , "}"
  , "signed int *hs_bindgen_1b0ed1f4c7ddeb1e ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer1)(arg1);"
  , "}"
  , "signed int **hs_bindgen_46ff1f39be014f9f ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer2)(arg1);"
  , "}"
  , "void *hs_bindgen_c4737a52f27dfdac ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer3)(arg1);"
  , "}"
  , "signed int hs_bindgen_fdef71f6bc647af6 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (body1)(arg1);"
  , "}"
  , "signed int hs_bindgen_4ebf4e201327a21a (void)"
  , "{"
  , "  return (body2)();"
  , "}"
  , "void hs_bindgen_7229e893bab43698 ("
  , "  signed int arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_float)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_0cbb6cfd403656ec ("
  , "  signed int arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_double)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_7fa01b9c64606663 ("
  , "  signed int arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_float)(arg1);"
  , "}"
  , "void hs_bindgen_81ad2d3ffe70c3a9 ("
  , "  signed int arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_double)(arg1);"
  , "}"
  , "void hs_bindgen_1c3c1f6f6a7e2bc8 ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (bespoke_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a5e6d8b6d973331f ("
  , "  signed int arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  (bespoke_args2)(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_0f143bde59bade44 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret1)(arg1);"
  , "}"
  , "size_t hs_bindgen_3fe674df63c48d30 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret2)(arg1);"
  , "}"
  , "void hs_bindgen_a8eaeaa944a099d9 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (arr_args1)(arg1);"
  , "}"
  , "void hs_bindgen_f928df06f2f528e0 ("
  , "  signed int **arg1"
  , ")"
  , "{"
  , "  (arr_args2)(arg1);"
  , "}"
  , "void hs_bindgen_2fb2e9d0563a690b ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (arr_args3)(arg1);"
  , "}"
  , "void hs_bindgen_b9b529def42c1efd ("
  , "  signed int **arg1"
  , ")"
  , "{"
  , "  (arr_args4)(arg1);"
  , "}"
  , "void hs_bindgen_5ab0516185b48906 ("
  , "  signed int arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_682810b65a83f101 ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a06192a621a12016 ("
  , "  signed int arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_414fd8b92ff2611b ("
  , "  signed int arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_62b477fc0c092e32 ("
  , "  signed int arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args5)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_17fb56aae1527722 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (comments1)(arg1);"
  , "}"
  , "void hs_bindgen_e70333f7108e59a8 ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_14a503cc3e41e014 ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_504ad9cb98a4f946 ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c0c67bf5b144c658 ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_72fa51e2c7023b88 ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b09c2805c508fddf ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_85bf325f09491972 ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_45a61f720b0fe2ed ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_36c611aa17d28531 ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a614ed967b401309 ("
  , "  signed int arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_0e258c684851b84f ("
  , "  signed int arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_9f7d70e8a0106516 ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0b71a82a5fa4fc87 ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_7ebb88bce2abdf74 ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_7188d6a351cf7dd5 ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_bd2e5fd19a5f4d84 ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ab0f4d2e36402e2f ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d4fdffd51b526049 ("
  , "  signed int arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_662a5811d33311fd ("
  , "  signed int arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_6755aa8f356d9bb1 ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_38694a08d2a7599a ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_44cd79114d322610 ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_30969f0e26032752 ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_35235928bc01e390 ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9853f1c7b6472022 ("
  , "  signed int arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e3b1d92f43d9cda7 ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b4a0d9b232b8ee87 ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args5)(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_ce61a185fdbd1b5e ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret1)(arg1);"
  , "}"
  , "signed int const *hs_bindgen_001714607a85bf44 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret2)(arg1);"
  , "}"
  , "signed int *const hs_bindgen_0481dd02a7bda0cb ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret3)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_2b32282230882c4f ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret4)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_094656d98097f48e ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret5)(arg1);"
  , "}"
  , "void hs_bindgen_5f2909e0d90b7530 ("
  , "  signed int const *arg1"
  , ")"
  , "{"
  , "  (const_array_elem1)(arg1);"
  , "}"
  , "void hs_bindgen_a9a1c34158c32e13 ("
  , "  signed int const **arg1"
  , ")"
  , "{"
  , "  (const_array_elem2)(arg1);"
  , "}"
  , "void hs_bindgen_55765da8c98676da ("
  , "  signed int *const *arg1"
  , ")"
  , "{"
  , "  (const_array_elem3)(arg1);"
  , "}"
  , "signed int hs_bindgen_2692ad6e69dd9bf6 (void)"
  , "{"
  , "  return (noParams1)();"
  , "}"
  , "signed int hs_bindgen_b016e4354ee966d9 (void)"
  , "{"
  , "  return (noParams2)();"
  , "}"
  , "void hs_bindgen_9a5fca17446c0b64 ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (noParams3)(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_e0ae2ea8372de864 ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_105cd0216456de95 ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret2)(arg1);"
  , "}"
  , "void (*hs_bindgen_2d7d8cfa71504d88 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (funptr_ret3)(arg1);"
  , "}"
  , "char (*hs_bindgen_46f0ed4c8295ba70 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret4)(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_f8291bd691623354 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret5)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_2eb8ef0a6794c225 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret6)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_8cc60b45c83cb72b ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret7)(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_2ad075d6ca6beea5 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret8)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_fe5e863d48f781be ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret9)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_2a029ddc00ba6ce1 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret10)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_char1@
foreign import ccall safe "hs_bindgen_9950741591b35cb0" hs_bindgen_9950741591b35cb0_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_char1@
hs_bindgen_9950741591b35cb0 ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_9950741591b35cb0 =
  BG.fromFFIType hs_bindgen_9950741591b35cb0_base

{-| Function declarations

    __C declaration:__ @args_char1@

    __defined at:__ @macros\/reparse.h 17:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_char1 = hs_bindgen_9950741591b35cb0

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_char2@
foreign import ccall safe "hs_bindgen_3b213f36de64a78b" hs_bindgen_3b213f36de64a78b_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_char2@
hs_bindgen_3b213f36de64a78b ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_3b213f36de64a78b =
  BG.fromFFIType hs_bindgen_3b213f36de64a78b_base

{-| __C declaration:__ @args_char2@

    __defined at:__ @macros\/reparse.h 18:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CSChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_char2 = hs_bindgen_3b213f36de64a78b

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_char3@
foreign import ccall safe "hs_bindgen_d72da53837273ed1" hs_bindgen_d72da53837273ed1_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_char3@
hs_bindgen_d72da53837273ed1 ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_d72da53837273ed1 =
  BG.fromFFIType hs_bindgen_d72da53837273ed1_base

{-| __C declaration:__ @args_char3@

    __defined at:__ @macros\/reparse.h 19:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CUChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_char3 = hs_bindgen_d72da53837273ed1

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_short1@
foreign import ccall safe "hs_bindgen_8e00ad94407efdce" hs_bindgen_8e00ad94407efdce_base ::
     BG.Int32
  -> BG.Int16
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_short1@
hs_bindgen_8e00ad94407efdce ::
     BG.CInt
  -> BG.CShort
  -> IO ()
hs_bindgen_8e00ad94407efdce =
  BG.fromFFIType hs_bindgen_8e00ad94407efdce_base

{-| __C declaration:__ @args_short1@

    __defined at:__ @macros\/reparse.h 21:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CShort
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_short1 = hs_bindgen_8e00ad94407efdce

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_short2@
foreign import ccall safe "hs_bindgen_e5814201b232d64c" hs_bindgen_e5814201b232d64c_base ::
     BG.Int32
  -> BG.Int16
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_short2@
hs_bindgen_e5814201b232d64c ::
     BG.CInt
  -> BG.CShort
  -> IO ()
hs_bindgen_e5814201b232d64c =
  BG.fromFFIType hs_bindgen_e5814201b232d64c_base

{-| __C declaration:__ @args_short2@

    __defined at:__ @macros\/reparse.h 22:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CShort
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_short2 = hs_bindgen_e5814201b232d64c

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_short3@
foreign import ccall safe "hs_bindgen_3bbb21d02f9e8caa" hs_bindgen_3bbb21d02f9e8caa_base ::
     BG.Int32
  -> BG.Word16
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_short3@
hs_bindgen_3bbb21d02f9e8caa ::
     BG.CInt
  -> BG.CUShort
  -> IO ()
hs_bindgen_3bbb21d02f9e8caa =
  BG.fromFFIType hs_bindgen_3bbb21d02f9e8caa_base

{-| __C declaration:__ @args_short3@

    __defined at:__ @macros\/reparse.h 23:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CUShort
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_short3 = hs_bindgen_3bbb21d02f9e8caa

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_int1@
foreign import ccall safe "hs_bindgen_fba8d44819fb4d50" hs_bindgen_fba8d44819fb4d50_base ::
     BG.Int32
  -> BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_int1@
hs_bindgen_fba8d44819fb4d50 ::
     BG.CInt
  -> BG.CInt
  -> IO ()
hs_bindgen_fba8d44819fb4d50 =
  BG.fromFFIType hs_bindgen_fba8d44819fb4d50_base

{-| __C declaration:__ @args_int1@

    __defined at:__ @macros\/reparse.h 25:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_int1 = hs_bindgen_fba8d44819fb4d50

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_int2@
foreign import ccall safe "hs_bindgen_42cfc5afda432a51" hs_bindgen_42cfc5afda432a51_base ::
     BG.Int32
  -> BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_int2@
hs_bindgen_42cfc5afda432a51 ::
     BG.CInt
  -> BG.CInt
  -> IO ()
hs_bindgen_42cfc5afda432a51 =
  BG.fromFFIType hs_bindgen_42cfc5afda432a51_base

{-| __C declaration:__ @args_int2@

    __defined at:__ @macros\/reparse.h 26:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_int2 = hs_bindgen_42cfc5afda432a51

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_int3@
foreign import ccall safe "hs_bindgen_b877c77021f548cb" hs_bindgen_b877c77021f548cb_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_int3@
hs_bindgen_b877c77021f548cb ::
     BG.CInt
  -> BG.CUInt
  -> IO ()
hs_bindgen_b877c77021f548cb =
  BG.fromFFIType hs_bindgen_b877c77021f548cb_base

{-| __C declaration:__ @args_int3@

    __defined at:__ @macros\/reparse.h 27:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CUInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_int3 = hs_bindgen_b877c77021f548cb

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_long1@
foreign import ccall safe "hs_bindgen_365ed0d5f00ece62" hs_bindgen_365ed0d5f00ece62_base ::
     BG.Int32
  -> BG.Int64
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_long1@
hs_bindgen_365ed0d5f00ece62 ::
     BG.CInt
  -> BG.CLong
  -> IO ()
hs_bindgen_365ed0d5f00ece62 =
  BG.fromFFIType hs_bindgen_365ed0d5f00ece62_base

{-| __C declaration:__ @args_long1@

    __defined at:__ @macros\/reparse.h 29:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CLong
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_long1 = hs_bindgen_365ed0d5f00ece62

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_long2@
foreign import ccall safe "hs_bindgen_a1204b1083b6700a" hs_bindgen_a1204b1083b6700a_base ::
     BG.Int32
  -> BG.Int64
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_long2@
hs_bindgen_a1204b1083b6700a ::
     BG.CInt
  -> BG.CLong
  -> IO ()
hs_bindgen_a1204b1083b6700a =
  BG.fromFFIType hs_bindgen_a1204b1083b6700a_base

{-| __C declaration:__ @args_long2@

    __defined at:__ @macros\/reparse.h 30:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CLong
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_long2 = hs_bindgen_a1204b1083b6700a

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_long3@
foreign import ccall safe "hs_bindgen_4af0668f1653ce01" hs_bindgen_4af0668f1653ce01_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_long3@
hs_bindgen_4af0668f1653ce01 ::
     BG.CInt
  -> BG.CULong
  -> IO ()
hs_bindgen_4af0668f1653ce01 =
  BG.fromFFIType hs_bindgen_4af0668f1653ce01_base

{-| __C declaration:__ @args_long3@

    __defined at:__ @macros\/reparse.h 31:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CULong
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_long3 = hs_bindgen_4af0668f1653ce01

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_float@
foreign import ccall safe "hs_bindgen_b8acf83f3c3a43b0" hs_bindgen_b8acf83f3c3a43b0_base ::
     BG.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_float@
hs_bindgen_b8acf83f3c3a43b0 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_b8acf83f3c3a43b0 =
  BG.fromFFIType hs_bindgen_b8acf83f3c3a43b0_base

{-| __C declaration:__ @args_float@

    __defined at:__ @macros\/reparse.h 33:6@

    __exported by:__ @macros\/reparse.h@
-}
args_float ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_float = hs_bindgen_b8acf83f3c3a43b0

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_double@
foreign import ccall safe "hs_bindgen_2a27bb82268cc870" hs_bindgen_2a27bb82268cc870_base ::
     BG.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_double@
hs_bindgen_2a27bb82268cc870 ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_2a27bb82268cc870 =
  BG.fromFFIType hs_bindgen_2a27bb82268cc870_base

{-| __C declaration:__ @args_double@

    __defined at:__ @macros\/reparse.h 34:6@

    __exported by:__ @macros\/reparse.h@
-}
args_double ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_double = hs_bindgen_2a27bb82268cc870

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_bool1@
foreign import ccall safe "hs_bindgen_3f1481ac913e0394" hs_bindgen_3f1481ac913e0394_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_bool1@
hs_bindgen_3f1481ac913e0394 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_3f1481ac913e0394 =
  BG.fromFFIType hs_bindgen_3f1481ac913e0394_base

{-| __C declaration:__ @args_bool1@

    __defined at:__ @macros\/reparse.h 35:6@

    __exported by:__ @macros\/reparse.h@
-}
args_bool1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_bool1 = hs_bindgen_3f1481ac913e0394

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_struct@
foreign import ccall safe "hs_bindgen_998092a62cba613e" hs_bindgen_998092a62cba613e_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_struct@
hs_bindgen_998092a62cba613e ::
     BG.CInt
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_998092a62cba613e =
  BG.fromFFIType hs_bindgen_998092a62cba613e_base

{-| __C declaration:__ @args_struct@

    __defined at:__ @macros\/reparse.h 37:6@

    __exported by:__ @macros\/reparse.h@
-}
args_struct ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> Some_struct
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_struct =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_998092a62cba613e arg10 arg22)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_union@
foreign import ccall safe "hs_bindgen_b83fe7017c88f63e" hs_bindgen_b83fe7017c88f63e_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_union@
hs_bindgen_b83fe7017c88f63e ::
     BG.CInt
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_b83fe7017c88f63e =
  BG.fromFFIType hs_bindgen_b83fe7017c88f63e_base

{-| __C declaration:__ @args_union@

    __defined at:__ @macros\/reparse.h 38:6@

    __exported by:__ @macros\/reparse.h@
-}
args_union ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> Some_union
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_union =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_b83fe7017c88f63e arg10 arg22)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_enum@
foreign import ccall safe "hs_bindgen_45ef9d5579ceaa74" hs_bindgen_45ef9d5579ceaa74_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_enum@
hs_bindgen_45ef9d5579ceaa74 ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_45ef9d5579ceaa74 =
  BG.fromFFIType hs_bindgen_45ef9d5579ceaa74_base

{-| __C declaration:__ @args_enum@

    __defined at:__ @macros\/reparse.h 39:6@

    __exported by:__ @macros\/reparse.h@
-}
args_enum ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> Some_enum
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_enum = hs_bindgen_45ef9d5579ceaa74

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_pointer1@
foreign import ccall safe "hs_bindgen_862d74c6849bbcd0" hs_bindgen_862d74c6849bbcd0_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_pointer1@
hs_bindgen_862d74c6849bbcd0 ::
     BG.CInt
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_862d74c6849bbcd0 =
  BG.fromFFIType hs_bindgen_862d74c6849bbcd0_base

{-| __C declaration:__ @args_pointer1@

    __defined at:__ @macros\/reparse.h 41:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.Ptr BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_pointer1 = hs_bindgen_862d74c6849bbcd0

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_pointer2@
foreign import ccall safe "hs_bindgen_0df09844c5645db9" hs_bindgen_0df09844c5645db9_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_pointer2@
hs_bindgen_0df09844c5645db9 ::
     BG.CInt
  -> BG.Ptr (BG.Ptr BG.CInt)
  -> IO ()
hs_bindgen_0df09844c5645db9 =
  BG.fromFFIType hs_bindgen_0df09844c5645db9_base

{-| __C declaration:__ @args_pointer2@

    __defined at:__ @macros\/reparse.h 42:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.Ptr (BG.Ptr BG.CInt)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_pointer2 = hs_bindgen_0df09844c5645db9

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_pointer3@
foreign import ccall safe "hs_bindgen_221cb5a1c4ce6a3b" hs_bindgen_221cb5a1c4ce6a3b_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_pointer3@
hs_bindgen_221cb5a1c4ce6a3b ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()
hs_bindgen_221cb5a1c4ce6a3b =
  BG.fromFFIType hs_bindgen_221cb5a1c4ce6a3b_base

{-| __C declaration:__ @args_pointer3@

    __defined at:__ @macros\/reparse.h 43:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.Ptr BG.Void
     -- ^ __C declaration:__ @arg3@
  -> IO ()
args_pointer3 = hs_bindgen_221cb5a1c4ce6a3b

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_A@
foreign import ccall safe "hs_bindgen_dfd4e13321b706b1" hs_bindgen_dfd4e13321b706b1_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_A@
hs_bindgen_dfd4e13321b706b1 :: IO BG.CInt
hs_bindgen_dfd4e13321b706b1 =
  BG.fromFFIType hs_bindgen_dfd4e13321b706b1_base

{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h 47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A :: IO BG.CInt
ret_A = hs_bindgen_dfd4e13321b706b1

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_char1@
foreign import ccall safe "hs_bindgen_5289300b6ae3ff51" hs_bindgen_5289300b6ae3ff51_base ::
     BG.Int32
  -> IO BG.Int8

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_char1@
hs_bindgen_5289300b6ae3ff51 ::
     BG.CInt
  -> IO BG.CChar
hs_bindgen_5289300b6ae3ff51 =
  BG.fromFFIType hs_bindgen_5289300b6ae3ff51_base

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h 49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CChar
ret_char1 = hs_bindgen_5289300b6ae3ff51

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_char2@
foreign import ccall safe "hs_bindgen_b258f6c4e2c9be38" hs_bindgen_b258f6c4e2c9be38_base ::
     BG.Int32
  -> IO BG.Int8

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_char2@
hs_bindgen_b258f6c4e2c9be38 ::
     BG.CInt
  -> IO BG.CSChar
hs_bindgen_b258f6c4e2c9be38 =
  BG.fromFFIType hs_bindgen_b258f6c4e2c9be38_base

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h 50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CSChar
ret_char2 = hs_bindgen_b258f6c4e2c9be38

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_char3@
foreign import ccall safe "hs_bindgen_734b5b749255713a" hs_bindgen_734b5b749255713a_base ::
     BG.Int32
  -> IO BG.Word8

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_char3@
hs_bindgen_734b5b749255713a ::
     BG.CInt
  -> IO BG.CUChar
hs_bindgen_734b5b749255713a =
  BG.fromFFIType hs_bindgen_734b5b749255713a_base

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h 51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUChar
ret_char3 = hs_bindgen_734b5b749255713a

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_short1@
foreign import ccall safe "hs_bindgen_d17d4780d98d2561" hs_bindgen_d17d4780d98d2561_base ::
     BG.Int32
  -> IO BG.Int16

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_short1@
hs_bindgen_d17d4780d98d2561 ::
     BG.CInt
  -> IO BG.CShort
hs_bindgen_d17d4780d98d2561 =
  BG.fromFFIType hs_bindgen_d17d4780d98d2561_base

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h 53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short1 = hs_bindgen_d17d4780d98d2561

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_short2@
foreign import ccall safe "hs_bindgen_1f808a3cec3781e1" hs_bindgen_1f808a3cec3781e1_base ::
     BG.Int32
  -> IO BG.Int16

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_short2@
hs_bindgen_1f808a3cec3781e1 ::
     BG.CInt
  -> IO BG.CShort
hs_bindgen_1f808a3cec3781e1 =
  BG.fromFFIType hs_bindgen_1f808a3cec3781e1_base

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h 54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short2 = hs_bindgen_1f808a3cec3781e1

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_short3@
foreign import ccall safe "hs_bindgen_5204d0272f75f1a2" hs_bindgen_5204d0272f75f1a2_base ::
     BG.Int32
  -> IO BG.Word16

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_short3@
hs_bindgen_5204d0272f75f1a2 ::
     BG.CInt
  -> IO BG.CUShort
hs_bindgen_5204d0272f75f1a2 =
  BG.fromFFIType hs_bindgen_5204d0272f75f1a2_base

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h 55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUShort
ret_short3 = hs_bindgen_5204d0272f75f1a2

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_int1@
foreign import ccall safe "hs_bindgen_14351eeb2c92732a" hs_bindgen_14351eeb2c92732a_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_int1@
hs_bindgen_14351eeb2c92732a ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_14351eeb2c92732a =
  BG.fromFFIType hs_bindgen_14351eeb2c92732a_base

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h 57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int1 = hs_bindgen_14351eeb2c92732a

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_int2@
foreign import ccall safe "hs_bindgen_f91baafc096613a0" hs_bindgen_f91baafc096613a0_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_int2@
hs_bindgen_f91baafc096613a0 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_f91baafc096613a0 =
  BG.fromFFIType hs_bindgen_f91baafc096613a0_base

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h 58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int2 = hs_bindgen_f91baafc096613a0

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_int3@
foreign import ccall safe "hs_bindgen_e78391249ef8ffbf" hs_bindgen_e78391249ef8ffbf_base ::
     BG.Int32
  -> IO BG.Word32

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_int3@
hs_bindgen_e78391249ef8ffbf ::
     BG.CInt
  -> IO BG.CUInt
hs_bindgen_e78391249ef8ffbf =
  BG.fromFFIType hs_bindgen_e78391249ef8ffbf_base

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h 59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUInt
ret_int3 = hs_bindgen_e78391249ef8ffbf

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_long1@
foreign import ccall safe "hs_bindgen_4f8ae7ed648c72e4" hs_bindgen_4f8ae7ed648c72e4_base ::
     BG.Int32
  -> IO BG.Int64

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_long1@
hs_bindgen_4f8ae7ed648c72e4 ::
     BG.CInt
  -> IO BG.CLong
hs_bindgen_4f8ae7ed648c72e4 =
  BG.fromFFIType hs_bindgen_4f8ae7ed648c72e4_base

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h 61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long1 = hs_bindgen_4f8ae7ed648c72e4

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_long2@
foreign import ccall safe "hs_bindgen_c0a47369a5a10695" hs_bindgen_c0a47369a5a10695_base ::
     BG.Int32
  -> IO BG.Int64

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_long2@
hs_bindgen_c0a47369a5a10695 ::
     BG.CInt
  -> IO BG.CLong
hs_bindgen_c0a47369a5a10695 =
  BG.fromFFIType hs_bindgen_c0a47369a5a10695_base

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h 62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long2 = hs_bindgen_c0a47369a5a10695

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_long3@
foreign import ccall safe "hs_bindgen_64aa36b6c820a3bf" hs_bindgen_64aa36b6c820a3bf_base ::
     BG.Int32
  -> IO BG.Word64

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_long3@
hs_bindgen_64aa36b6c820a3bf ::
     BG.CInt
  -> IO BG.CULong
hs_bindgen_64aa36b6c820a3bf =
  BG.fromFFIType hs_bindgen_64aa36b6c820a3bf_base

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h 63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CULong
ret_long3 = hs_bindgen_64aa36b6c820a3bf

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_float@
foreign import ccall safe "hs_bindgen_b53c45aaccbaaf3a" hs_bindgen_b53c45aaccbaaf3a_base ::
     BG.Int32
  -> IO Float

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_float@
hs_bindgen_b53c45aaccbaaf3a ::
     BG.CInt
  -> IO BG.CFloat
hs_bindgen_b53c45aaccbaaf3a =
  BG.fromFFIType hs_bindgen_b53c45aaccbaaf3a_base

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h 65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CFloat
ret_float = hs_bindgen_b53c45aaccbaaf3a

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_double@
foreign import ccall safe "hs_bindgen_0ca050f5bbe69c94" hs_bindgen_0ca050f5bbe69c94_base ::
     BG.Int32
  -> IO Double

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_double@
hs_bindgen_0ca050f5bbe69c94 ::
     BG.CInt
  -> IO BG.CDouble
hs_bindgen_0ca050f5bbe69c94 =
  BG.fromFFIType hs_bindgen_0ca050f5bbe69c94_base

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h 66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CDouble
ret_double = hs_bindgen_0ca050f5bbe69c94

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_bool1@
foreign import ccall safe "hs_bindgen_530e0725cadf8f52" hs_bindgen_530e0725cadf8f52_base ::
     BG.Int32
  -> IO BG.Word8

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_bool1@
hs_bindgen_530e0725cadf8f52 ::
     BG.CInt
  -> IO BG.CBool
hs_bindgen_530e0725cadf8f52 =
  BG.fromFFIType hs_bindgen_530e0725cadf8f52_base

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h 67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
ret_bool1 = hs_bindgen_530e0725cadf8f52

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_struct@
foreign import ccall safe "hs_bindgen_8f19d5621a4f3e92" hs_bindgen_8f19d5621a4f3e92_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_struct@
hs_bindgen_8f19d5621a4f3e92 ::
     BG.CInt
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_8f19d5621a4f3e92 =
  BG.fromFFIType hs_bindgen_8f19d5621a4f3e92_base

{-| __C declaration:__ @ret_struct@

    __defined at:__ @macros\/reparse.h 69:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_struct ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO Some_struct
ret_struct =
  \arg10 ->
    BG.allocaAndPeek (\res1 ->
                        hs_bindgen_8f19d5621a4f3e92 arg10 res1)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_union@
foreign import ccall safe "hs_bindgen_e93cfabcefb5051b" hs_bindgen_e93cfabcefb5051b_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_union@
hs_bindgen_e93cfabcefb5051b ::
     BG.CInt
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_e93cfabcefb5051b =
  BG.fromFFIType hs_bindgen_e93cfabcefb5051b_base

{-| __C declaration:__ @ret_union@

    __defined at:__ @macros\/reparse.h 70:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_union ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO Some_union
ret_union =
  \arg10 ->
    BG.allocaAndPeek (\res1 ->
                        hs_bindgen_e93cfabcefb5051b arg10 res1)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_enum@
foreign import ccall safe "hs_bindgen_f672d8d1020b037f" hs_bindgen_f672d8d1020b037f_base ::
     BG.Int32
  -> IO BG.Word32

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_enum@
hs_bindgen_f672d8d1020b037f ::
     BG.CInt
  -> IO Some_enum
hs_bindgen_f672d8d1020b037f =
  BG.fromFFIType hs_bindgen_f672d8d1020b037f_base

{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h 71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO Some_enum
ret_enum = hs_bindgen_f672d8d1020b037f

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_pointer1@
foreign import ccall safe "hs_bindgen_1b0ed1f4c7ddeb1e" hs_bindgen_1b0ed1f4c7ddeb1e_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_pointer1@
hs_bindgen_1b0ed1f4c7ddeb1e ::
     BG.CInt
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_1b0ed1f4c7ddeb1e =
  BG.fromFFIType hs_bindgen_1b0ed1f4c7ddeb1e_base

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h 73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
ret_pointer1 = hs_bindgen_1b0ed1f4c7ddeb1e

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_pointer2@
foreign import ccall safe "hs_bindgen_46ff1f39be014f9f" hs_bindgen_46ff1f39be014f9f_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_pointer2@
hs_bindgen_46ff1f39be014f9f ::
     BG.CInt
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
hs_bindgen_46ff1f39be014f9f =
  BG.fromFFIType hs_bindgen_46ff1f39be014f9f_base

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h 74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
ret_pointer2 = hs_bindgen_46ff1f39be014f9f

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_pointer3@
foreign import ccall safe "hs_bindgen_c4737a52f27dfdac" hs_bindgen_c4737a52f27dfdac_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_pointer3@
hs_bindgen_c4737a52f27dfdac ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)
hs_bindgen_c4737a52f27dfdac =
  BG.fromFFIType hs_bindgen_c4737a52f27dfdac_base

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h 75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.Void)
ret_pointer3 = hs_bindgen_c4737a52f27dfdac

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_body1@
foreign import ccall safe "hs_bindgen_fdef71f6bc647af6" hs_bindgen_fdef71f6bc647af6_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_body1@
hs_bindgen_fdef71f6bc647af6 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_fdef71f6bc647af6 =
  BG.fromFFIType hs_bindgen_fdef71f6bc647af6_base

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h 79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
body1 = hs_bindgen_fdef71f6bc647af6

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_body2@
foreign import ccall safe "hs_bindgen_4ebf4e201327a21a" hs_bindgen_4ebf4e201327a21a_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_body2@
hs_bindgen_4ebf4e201327a21a :: IO BG.CInt
hs_bindgen_4ebf4e201327a21a =
  BG.fromFFIType hs_bindgen_4ebf4e201327a21a_base

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h 80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2 :: IO BG.CInt
body2 = hs_bindgen_4ebf4e201327a21a

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_complex_float@
foreign import ccall safe "hs_bindgen_7229e893bab43698" hs_bindgen_7229e893bab43698_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_complex_float@
hs_bindgen_7229e893bab43698 ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_7229e893bab43698 =
  BG.fromFFIType hs_bindgen_7229e893bab43698_base

{-| __C declaration:__ @args_complex_float@

    __defined at:__ @macros\/reparse.h 84:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_float ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.Complex BG.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_complex_float =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_7229e893bab43698 arg10 arg22)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_complex_double@
foreign import ccall safe "hs_bindgen_0cbb6cfd403656ec" hs_bindgen_0cbb6cfd403656ec_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_args_complex_double@
hs_bindgen_0cbb6cfd403656ec ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_0cbb6cfd403656ec =
  BG.fromFFIType hs_bindgen_0cbb6cfd403656ec_base

{-| __C declaration:__ @args_complex_double@

    __defined at:__ @macros\/reparse.h 85:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_double ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.Complex BG.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_complex_double =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_0cbb6cfd403656ec arg10 arg22)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_complex_float@
foreign import ccall safe "hs_bindgen_7fa01b9c64606663" hs_bindgen_7fa01b9c64606663_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_complex_float@
hs_bindgen_7fa01b9c64606663 ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_7fa01b9c64606663 =
  BG.fromFFIType hs_bindgen_7fa01b9c64606663_base

{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @macros\/reparse.h 86:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_float ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Complex BG.CFloat)
ret_complex_float =
  \arg10 ->
    BG.allocaAndPeek (\res1 ->
                        hs_bindgen_7fa01b9c64606663 arg10 res1)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_complex_double@
foreign import ccall safe "hs_bindgen_81ad2d3ffe70c3a9" hs_bindgen_81ad2d3ffe70c3a9_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_ret_complex_double@
hs_bindgen_81ad2d3ffe70c3a9 ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_81ad2d3ffe70c3a9 =
  BG.fromFFIType hs_bindgen_81ad2d3ffe70c3a9_base

{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @macros\/reparse.h 87:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_double ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Complex BG.CDouble)
ret_complex_double =
  \arg10 ->
    BG.allocaAndPeek (\res1 ->
                        hs_bindgen_81ad2d3ffe70c3a9 arg10 res1)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_bespoke_args1@
foreign import ccall safe "hs_bindgen_1c3c1f6f6a7e2bc8" hs_bindgen_1c3c1f6f6a7e2bc8_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_bespoke_args1@
hs_bindgen_1c3c1f6f6a7e2bc8 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_1c3c1f6f6a7e2bc8 =
  BG.fromFFIType hs_bindgen_1c3c1f6f6a7e2bc8_base

{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @macros\/reparse.h 94:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
bespoke_args1 = hs_bindgen_1c3c1f6f6a7e2bc8

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_bespoke_args2@
foreign import ccall safe "hs_bindgen_a5e6d8b6d973331f" hs_bindgen_a5e6d8b6d973331f_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_bespoke_args2@
hs_bindgen_a5e6d8b6d973331f ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_a5e6d8b6d973331f =
  BG.fromFFIType hs_bindgen_a5e6d8b6d973331f_base

{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @macros\/reparse.h 95:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.LibC.CSize
     -- ^ __C declaration:__ @arg2@
  -> IO ()
bespoke_args2 = hs_bindgen_a5e6d8b6d973331f

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_bespoke_ret1@
foreign import ccall safe "hs_bindgen_0f143bde59bade44" hs_bindgen_0f143bde59bade44_base ::
     BG.Int32
  -> IO BG.Word8

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_bespoke_ret1@
hs_bindgen_0f143bde59bade44 ::
     BG.CInt
  -> IO BG.CBool
hs_bindgen_0f143bde59bade44 =
  BG.fromFFIType hs_bindgen_0f143bde59bade44_base

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h 97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
bespoke_ret1 = hs_bindgen_0f143bde59bade44

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_bespoke_ret2@
foreign import ccall safe "hs_bindgen_3fe674df63c48d30" hs_bindgen_3fe674df63c48d30_base ::
     BG.Int32
  -> IO BG.Word64

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_bespoke_ret2@
hs_bindgen_3fe674df63c48d30 ::
     BG.CInt
  -> IO HsBindgen.Runtime.LibC.CSize
hs_bindgen_3fe674df63c48d30 =
  BG.fromFFIType hs_bindgen_3fe674df63c48d30_base

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h 98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO HsBindgen.Runtime.LibC.CSize
bespoke_ret2 = hs_bindgen_3fe674df63c48d30

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_arr_args1@
foreign import ccall safe "hs_bindgen_a8eaeaa944a099d9" hs_bindgen_a8eaeaa944a099d9_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_arr_args1@
hs_bindgen_a8eaeaa944a099d9 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt))
  -> IO ()
hs_bindgen_a8eaeaa944a099d9 =
  BG.fromFFIType hs_bindgen_a8eaeaa944a099d9_base

{-| Arrays

    __C declaration:__ @arr_args1@

    __defined at:__ @macros\/reparse.h 104:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args1 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args1 = hs_bindgen_a8eaeaa944a099d9

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_arr_args2@
foreign import ccall safe "hs_bindgen_f928df06f2f528e0" hs_bindgen_f928df06f2f528e0_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_arr_args2@
hs_bindgen_f928df06f2f528e0 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_f928df06f2f528e0 =
  BG.fromFFIType hs_bindgen_f928df06f2f528e0_base

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h 105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args2 = hs_bindgen_f928df06f2f528e0

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_arr_args3@
foreign import ccall safe "hs_bindgen_2fb2e9d0563a690b" hs_bindgen_2fb2e9d0563a690b_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_arr_args3@
hs_bindgen_2fb2e9d0563a690b ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt))
  -> IO ()
hs_bindgen_2fb2e9d0563a690b =
  BG.fromFFIType hs_bindgen_2fb2e9d0563a690b_base

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h 106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args3 = hs_bindgen_2fb2e9d0563a690b

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_arr_args4@
foreign import ccall safe "hs_bindgen_b9b529def42c1efd" hs_bindgen_b9b529def42c1efd_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_arr_args4@
hs_bindgen_b9b529def42c1efd ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_b9b529def42c1efd =
  BG.fromFFIType hs_bindgen_b9b529def42c1efd_base

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h 107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args4 = hs_bindgen_b9b529def42c1efd

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_args1@
foreign import ccall safe "hs_bindgen_5ab0516185b48906" hs_bindgen_5ab0516185b48906_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_args1@
hs_bindgen_5ab0516185b48906 ::
     BG.CInt
  -> BG.FunPtr (IO ())
  -> IO ()
hs_bindgen_5ab0516185b48906 =
  BG.fromFFIType hs_bindgen_5ab0516185b48906_base

{-| Function pointers

    __C declaration:__ @funptr_args1@

    __defined at:__ @macros\/reparse.h 126:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.FunPtr (IO ())
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args1 = hs_bindgen_5ab0516185b48906

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_args2@
foreign import ccall safe "hs_bindgen_682810b65a83f101" hs_bindgen_682810b65a83f101_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_args2@
hs_bindgen_682810b65a83f101 ::
     BG.CInt
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_682810b65a83f101 =
  BG.fromFFIType hs_bindgen_682810b65a83f101_base

{-| __C declaration:__ @funptr_args2@

    __defined at:__ @macros\/reparse.h 127:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.FunPtr (IO BG.CInt)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args2 = hs_bindgen_682810b65a83f101

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_args3@
foreign import ccall safe "hs_bindgen_a06192a621a12016" hs_bindgen_a06192a621a12016_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_args3@
hs_bindgen_a06192a621a12016 ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> IO ())
  -> IO ()
hs_bindgen_a06192a621a12016 =
  BG.fromFFIType hs_bindgen_a06192a621a12016_base

{-| __C declaration:__ @funptr_args3@

    __defined at:__ @macros\/reparse.h 128:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.FunPtr (BG.CInt -> IO ())
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args3 = hs_bindgen_a06192a621a12016

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_args4@
foreign import ccall safe "hs_bindgen_414fd8b92ff2611b" hs_bindgen_414fd8b92ff2611b_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_args4@
hs_bindgen_414fd8b92ff2611b ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar)
  -> IO ()
hs_bindgen_414fd8b92ff2611b =
  BG.fromFFIType hs_bindgen_414fd8b92ff2611b_base

{-| __C declaration:__ @funptr_args4@

    __defined at:__ @macros\/reparse.h 129:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args4 = hs_bindgen_414fd8b92ff2611b

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_args5@
foreign import ccall safe "hs_bindgen_62b477fc0c092e32" hs_bindgen_62b477fc0c092e32_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_args5@
hs_bindgen_62b477fc0c092e32 ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))
  -> IO ()
hs_bindgen_62b477fc0c092e32 =
  BG.fromFFIType hs_bindgen_62b477fc0c092e32_base

{-| __C declaration:__ @funptr_args5@

    __defined at:__ @macros\/reparse.h 130:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args5 = hs_bindgen_62b477fc0c092e32

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_comments1@
foreign import ccall safe "hs_bindgen_17fb56aae1527722" hs_bindgen_17fb56aae1527722_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_comments1@
hs_bindgen_17fb56aae1527722 ::
     BG.CInt
  -> IO ()
hs_bindgen_17fb56aae1527722 =
  BG.fromFFIType hs_bindgen_17fb56aae1527722_base

{-| Comments in awkward places

    (Prior to language-c we failed to parse there.)

    __C declaration:__ @comments1@

    __defined at:__ @macros\/reparse.h 144:25@

    __exported by:__ @macros\/reparse.h@
-}
comments1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO ()
comments1 = hs_bindgen_17fb56aae1527722

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_prim_before1@
foreign import ccall safe "hs_bindgen_e70333f7108e59a8" hs_bindgen_e70333f7108e59a8_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_prim_before1@
hs_bindgen_e70333f7108e59a8 ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_e70333f7108e59a8 =
  BG.fromFFIType hs_bindgen_e70333f7108e59a8_base

{-| @const@ qualifier

    NOTE: These were not parsed correctly prior to the switch to language-c.

    __C declaration:__ @const_prim_before1@

    __defined at:__ @macros\/reparse.h 177:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_before1 = hs_bindgen_e70333f7108e59a8

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_prim_before2@
foreign import ccall safe "hs_bindgen_14a503cc3e41e014" hs_bindgen_14a503cc3e41e014_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_prim_before2@
hs_bindgen_14a503cc3e41e014 ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_14a503cc3e41e014 =
  BG.fromFFIType hs_bindgen_14a503cc3e41e014_base

{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @macros\/reparse.h 178:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CSChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_before2 = hs_bindgen_14a503cc3e41e014

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_prim_before3@
foreign import ccall safe "hs_bindgen_504ad9cb98a4f946" hs_bindgen_504ad9cb98a4f946_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_prim_before3@
hs_bindgen_504ad9cb98a4f946 ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_504ad9cb98a4f946 =
  BG.fromFFIType hs_bindgen_504ad9cb98a4f946_base

{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @macros\/reparse.h 179:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CUChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_before3 = hs_bindgen_504ad9cb98a4f946

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_prim_after1@
foreign import ccall safe "hs_bindgen_c0c67bf5b144c658" hs_bindgen_c0c67bf5b144c658_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_prim_after1@
hs_bindgen_c0c67bf5b144c658 ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_c0c67bf5b144c658 =
  BG.fromFFIType hs_bindgen_c0c67bf5b144c658_base

{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @macros\/reparse.h 180:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_after1 = hs_bindgen_c0c67bf5b144c658

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_prim_after2@
foreign import ccall safe "hs_bindgen_72fa51e2c7023b88" hs_bindgen_72fa51e2c7023b88_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_prim_after2@
hs_bindgen_72fa51e2c7023b88 ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_72fa51e2c7023b88 =
  BG.fromFFIType hs_bindgen_72fa51e2c7023b88_base

{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @macros\/reparse.h 181:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CSChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_after2 = hs_bindgen_72fa51e2c7023b88

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_prim_after3@
foreign import ccall safe "hs_bindgen_b09c2805c508fddf" hs_bindgen_b09c2805c508fddf_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_prim_after3@
hs_bindgen_b09c2805c508fddf ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_b09c2805c508fddf =
  BG.fromFFIType hs_bindgen_b09c2805c508fddf_base

{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @macros\/reparse.h 182:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CUChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_after3 = hs_bindgen_b09c2805c508fddf

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_before1@
foreign import ccall safe "hs_bindgen_85bf325f09491972" hs_bindgen_85bf325f09491972_base ::
     BG.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_before1@
hs_bindgen_85bf325f09491972 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_85bf325f09491972 =
  BG.fromFFIType hs_bindgen_85bf325f09491972_base

{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @macros\/reparse.h 186:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before1 =
  hs_bindgen_85bf325f09491972

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_before2@
foreign import ccall safe "hs_bindgen_45a61f720b0fe2ed" hs_bindgen_45a61f720b0fe2ed_base ::
     BG.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_before2@
hs_bindgen_45a61f720b0fe2ed ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_45a61f720b0fe2ed =
  BG.fromFFIType hs_bindgen_45a61f720b0fe2ed_base

{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @macros\/reparse.h 187:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before2 =
  hs_bindgen_45a61f720b0fe2ed

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_before3@
foreign import ccall safe "hs_bindgen_36c611aa17d28531" hs_bindgen_36c611aa17d28531_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_before3@
hs_bindgen_36c611aa17d28531 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_36c611aa17d28531 =
  BG.fromFFIType hs_bindgen_36c611aa17d28531_base

{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @macros\/reparse.h 188:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before3 =
  hs_bindgen_36c611aa17d28531

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_before4@
foreign import ccall safe "hs_bindgen_a614ed967b401309" hs_bindgen_a614ed967b401309_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_before4@
hs_bindgen_a614ed967b401309 ::
     BG.CInt
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_a614ed967b401309 =
  BG.fromFFIType hs_bindgen_a614ed967b401309_base

{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @macros\/reparse.h 189:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> Some_struct
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before4 =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_a614ed967b401309 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_before5@
foreign import ccall safe "hs_bindgen_0e258c684851b84f" hs_bindgen_0e258c684851b84f_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_before5@
hs_bindgen_0e258c684851b84f ::
     BG.CInt
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_0e258c684851b84f =
  BG.fromFFIType hs_bindgen_0e258c684851b84f_base

{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @macros\/reparse.h 190:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> Some_union
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before5 =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_0e258c684851b84f arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_before6@
foreign import ccall safe "hs_bindgen_9f7d70e8a0106516" hs_bindgen_9f7d70e8a0106516_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_before6@
hs_bindgen_9f7d70e8a0106516 ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_9f7d70e8a0106516 =
  BG.fromFFIType hs_bindgen_9f7d70e8a0106516_base

{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @macros\/reparse.h 191:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before6 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> Some_enum
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before6 =
  hs_bindgen_9f7d70e8a0106516

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_before7@
foreign import ccall safe "hs_bindgen_0b71a82a5fa4fc87" hs_bindgen_0b71a82a5fa4fc87_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_before7@
hs_bindgen_0b71a82a5fa4fc87 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_0b71a82a5fa4fc87 =
  BG.fromFFIType hs_bindgen_0b71a82a5fa4fc87_base

{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @macros\/reparse.h 192:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before7 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before7 =
  hs_bindgen_0b71a82a5fa4fc87

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_before8@
foreign import ccall safe "hs_bindgen_7ebb88bce2abdf74" hs_bindgen_7ebb88bce2abdf74_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_before8@
hs_bindgen_7ebb88bce2abdf74 ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_7ebb88bce2abdf74 =
  BG.fromFFIType hs_bindgen_7ebb88bce2abdf74_base

{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @macros\/reparse.h 193:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before8 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.LibC.CSize
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before8 =
  hs_bindgen_7ebb88bce2abdf74

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_after1@
foreign import ccall safe "hs_bindgen_7188d6a351cf7dd5" hs_bindgen_7188d6a351cf7dd5_base ::
     BG.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_after1@
hs_bindgen_7188d6a351cf7dd5 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_7188d6a351cf7dd5 =
  BG.fromFFIType hs_bindgen_7188d6a351cf7dd5_base

{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @macros\/reparse.h 195:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after1 =
  hs_bindgen_7188d6a351cf7dd5

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_after2@
foreign import ccall safe "hs_bindgen_bd2e5fd19a5f4d84" hs_bindgen_bd2e5fd19a5f4d84_base ::
     BG.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_after2@
hs_bindgen_bd2e5fd19a5f4d84 ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_bd2e5fd19a5f4d84 =
  BG.fromFFIType hs_bindgen_bd2e5fd19a5f4d84_base

{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @macros\/reparse.h 196:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after2 =
  hs_bindgen_bd2e5fd19a5f4d84

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_after3@
foreign import ccall safe "hs_bindgen_ab0f4d2e36402e2f" hs_bindgen_ab0f4d2e36402e2f_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_after3@
hs_bindgen_ab0f4d2e36402e2f ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_ab0f4d2e36402e2f =
  BG.fromFFIType hs_bindgen_ab0f4d2e36402e2f_base

{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @macros\/reparse.h 197:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after3 =
  hs_bindgen_ab0f4d2e36402e2f

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_after4@
foreign import ccall safe "hs_bindgen_d4fdffd51b526049" hs_bindgen_d4fdffd51b526049_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_after4@
hs_bindgen_d4fdffd51b526049 ::
     BG.CInt
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_d4fdffd51b526049 =
  BG.fromFFIType hs_bindgen_d4fdffd51b526049_base

{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @macros\/reparse.h 198:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> Some_struct
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after4 =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_d4fdffd51b526049 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_after5@
foreign import ccall safe "hs_bindgen_662a5811d33311fd" hs_bindgen_662a5811d33311fd_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_after5@
hs_bindgen_662a5811d33311fd ::
     BG.CInt
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_662a5811d33311fd =
  BG.fromFFIType hs_bindgen_662a5811d33311fd_base

{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @macros\/reparse.h 199:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> Some_union
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after5 =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_662a5811d33311fd arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_after6@
foreign import ccall safe "hs_bindgen_6755aa8f356d9bb1" hs_bindgen_6755aa8f356d9bb1_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_after6@
hs_bindgen_6755aa8f356d9bb1 ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_6755aa8f356d9bb1 =
  BG.fromFFIType hs_bindgen_6755aa8f356d9bb1_base

{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @macros\/reparse.h 200:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after6 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> Some_enum
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after6 =
  hs_bindgen_6755aa8f356d9bb1

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_after7@
foreign import ccall safe "hs_bindgen_38694a08d2a7599a" hs_bindgen_38694a08d2a7599a_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_after7@
hs_bindgen_38694a08d2a7599a ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_38694a08d2a7599a =
  BG.fromFFIType hs_bindgen_38694a08d2a7599a_base

{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @macros\/reparse.h 201:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after7 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after7 =
  hs_bindgen_38694a08d2a7599a

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_after8@
foreign import ccall safe "hs_bindgen_44cd79114d322610" hs_bindgen_44cd79114d322610_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_withoutSign_after8@
hs_bindgen_44cd79114d322610 ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_44cd79114d322610 =
  BG.fromFFIType hs_bindgen_44cd79114d322610_base

{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @macros\/reparse.h 202:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after8 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.LibC.CSize
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after8 =
  hs_bindgen_44cd79114d322610

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_args1@
foreign import ccall safe "hs_bindgen_30969f0e26032752" hs_bindgen_30969f0e26032752_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_args1@
hs_bindgen_30969f0e26032752 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_30969f0e26032752 =
  BG.fromFFIType hs_bindgen_30969f0e26032752_base

{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @macros\/reparse.h 206:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> PtrConst.PtrConst BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args1 = hs_bindgen_30969f0e26032752

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_args2@
foreign import ccall safe "hs_bindgen_35235928bc01e390" hs_bindgen_35235928bc01e390_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_args2@
hs_bindgen_35235928bc01e390 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_35235928bc01e390 =
  BG.fromFFIType hs_bindgen_35235928bc01e390_base

{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @macros\/reparse.h 207:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> PtrConst.PtrConst BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args2 = hs_bindgen_35235928bc01e390

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_args3@
foreign import ccall safe "hs_bindgen_9853f1c7b6472022" hs_bindgen_9853f1c7b6472022_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_args3@
hs_bindgen_9853f1c7b6472022 ::
     BG.CInt
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_9853f1c7b6472022 =
  BG.fromFFIType hs_bindgen_9853f1c7b6472022_base

{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @macros\/reparse.h 208:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.Ptr BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args3 = hs_bindgen_9853f1c7b6472022

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_args4@
foreign import ccall safe "hs_bindgen_e3b1d92f43d9cda7" hs_bindgen_e3b1d92f43d9cda7_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_args4@
hs_bindgen_e3b1d92f43d9cda7 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_e3b1d92f43d9cda7 =
  BG.fromFFIType hs_bindgen_e3b1d92f43d9cda7_base

{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @macros\/reparse.h 209:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> PtrConst.PtrConst BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args4 = hs_bindgen_e3b1d92f43d9cda7

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_args5@
foreign import ccall safe "hs_bindgen_b4a0d9b232b8ee87" hs_bindgen_b4a0d9b232b8ee87_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_args5@
hs_bindgen_b4a0d9b232b8ee87 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_b4a0d9b232b8ee87 =
  BG.fromFFIType hs_bindgen_b4a0d9b232b8ee87_base

{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @macros\/reparse.h 210:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> PtrConst.PtrConst BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args5 = hs_bindgen_b4a0d9b232b8ee87

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_ret1@
foreign import ccall safe "hs_bindgen_ce61a185fdbd1b5e" hs_bindgen_ce61a185fdbd1b5e_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_ret1@
hs_bindgen_ce61a185fdbd1b5e ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_ce61a185fdbd1b5e =
  BG.fromFFIType hs_bindgen_ce61a185fdbd1b5e_base

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h 212:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret1 = hs_bindgen_ce61a185fdbd1b5e

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_ret2@
foreign import ccall safe "hs_bindgen_001714607a85bf44" hs_bindgen_001714607a85bf44_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_ret2@
hs_bindgen_001714607a85bf44 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_001714607a85bf44 =
  BG.fromFFIType hs_bindgen_001714607a85bf44_base

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h 213:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret2 = hs_bindgen_001714607a85bf44

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_ret3@
foreign import ccall safe "hs_bindgen_0481dd02a7bda0cb" hs_bindgen_0481dd02a7bda0cb_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_ret3@
hs_bindgen_0481dd02a7bda0cb ::
     BG.CInt
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_0481dd02a7bda0cb =
  BG.fromFFIType hs_bindgen_0481dd02a7bda0cb_base

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h 214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
const_pointers_ret3 = hs_bindgen_0481dd02a7bda0cb

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_ret4@
foreign import ccall safe "hs_bindgen_2b32282230882c4f" hs_bindgen_2b32282230882c4f_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_ret4@
hs_bindgen_2b32282230882c4f ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_2b32282230882c4f =
  BG.fromFFIType hs_bindgen_2b32282230882c4f_base

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h 215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret4 = hs_bindgen_2b32282230882c4f

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_ret5@
foreign import ccall safe "hs_bindgen_094656d98097f48e" hs_bindgen_094656d98097f48e_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_pointers_ret5@
hs_bindgen_094656d98097f48e ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_094656d98097f48e =
  BG.fromFFIType hs_bindgen_094656d98097f48e_base

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h 216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret5 = hs_bindgen_094656d98097f48e

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_array_elem1@
foreign import ccall safe "hs_bindgen_5f2909e0d90b7530" hs_bindgen_5f2909e0d90b7530_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_array_elem1@
hs_bindgen_5f2909e0d90b7530 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt))
  -> IO ()
hs_bindgen_5f2909e0d90b7530 =
  BG.fromFFIType hs_bindgen_5f2909e0d90b7530_base

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h 244:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem1 = hs_bindgen_5f2909e0d90b7530

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_array_elem2@
foreign import ccall safe "hs_bindgen_a9a1c34158c32e13" hs_bindgen_a9a1c34158c32e13_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_array_elem2@
hs_bindgen_a9a1c34158c32e13 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt)))
  -> IO ()
hs_bindgen_a9a1c34158c32e13 =
  BG.fromFFIType hs_bindgen_a9a1c34158c32e13_base

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h 245:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem2 = hs_bindgen_a9a1c34158c32e13

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_array_elem3@
foreign import ccall safe "hs_bindgen_55765da8c98676da" hs_bindgen_55765da8c98676da_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_const_array_elem3@
hs_bindgen_55765da8c98676da ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_55765da8c98676da =
  BG.fromFFIType hs_bindgen_55765da8c98676da_base

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h 246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem3 = hs_bindgen_55765da8c98676da

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_noParams1@
foreign import ccall safe "hs_bindgen_2692ad6e69dd9bf6" hs_bindgen_2692ad6e69dd9bf6_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_noParams1@
hs_bindgen_2692ad6e69dd9bf6 :: IO BG.CInt
hs_bindgen_2692ad6e69dd9bf6 =
  BG.fromFFIType hs_bindgen_2692ad6e69dd9bf6_base

{-| Other examples we reparsed /incorrectly/ before language-c

    __C declaration:__ @noParams1@

    __defined at:__ @macros\/reparse.h 254:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams1 :: IO BG.CInt
noParams1 = hs_bindgen_2692ad6e69dd9bf6

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_noParams2@
foreign import ccall safe "hs_bindgen_b016e4354ee966d9" hs_bindgen_b016e4354ee966d9_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_noParams2@
hs_bindgen_b016e4354ee966d9 :: IO BG.CInt
hs_bindgen_b016e4354ee966d9 =
  BG.fromFFIType hs_bindgen_b016e4354ee966d9_base

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h 255:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2 :: IO BG.CInt
noParams2 = hs_bindgen_b016e4354ee966d9

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_noParams3@
foreign import ccall safe "hs_bindgen_9a5fca17446c0b64" hs_bindgen_9a5fca17446c0b64_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_noParams3@
hs_bindgen_9a5fca17446c0b64 ::
     BG.CInt
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_9a5fca17446c0b64 =
  BG.fromFFIType hs_bindgen_9a5fca17446c0b64_base

{-| __C declaration:__ @noParams3@

    __defined at:__ @macros\/reparse.h 256:6@

    __exported by:__ @macros\/reparse.h@
-}
noParams3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.FunPtr (IO BG.CInt)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
noParams3 = hs_bindgen_9a5fca17446c0b64

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret1@
foreign import ccall safe "hs_bindgen_e0ae2ea8372de864" hs_bindgen_e0ae2ea8372de864_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret1@
hs_bindgen_e0ae2ea8372de864 ::
     BG.CInt
  -> IO (BG.FunPtr (IO ()))
hs_bindgen_e0ae2ea8372de864 =
  BG.fromFFIType hs_bindgen_e0ae2ea8372de864_base

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h 260:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO ()))
funptr_ret1 = hs_bindgen_e0ae2ea8372de864

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret2@
foreign import ccall safe "hs_bindgen_105cd0216456de95" hs_bindgen_105cd0216456de95_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret2@
hs_bindgen_105cd0216456de95 ::
     BG.CInt
  -> IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_105cd0216456de95 =
  BG.fromFFIType hs_bindgen_105cd0216456de95_base

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h 261:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO BG.CInt))
funptr_ret2 = hs_bindgen_105cd0216456de95

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret3@
foreign import ccall safe "hs_bindgen_2d7d8cfa71504d88" hs_bindgen_2d7d8cfa71504d88_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret3@
hs_bindgen_2d7d8cfa71504d88 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_2d7d8cfa71504d88 =
  BG.fromFFIType hs_bindgen_2d7d8cfa71504d88_base

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h 262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
funptr_ret3 = hs_bindgen_2d7d8cfa71504d88

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret4@
foreign import ccall safe "hs_bindgen_46f0ed4c8295ba70" hs_bindgen_46f0ed4c8295ba70_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret4@
hs_bindgen_46f0ed4c8295ba70 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
hs_bindgen_46f0ed4c8295ba70 =
  BG.fromFFIType hs_bindgen_46f0ed4c8295ba70_base

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h 263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
funptr_ret4 = hs_bindgen_46f0ed4c8295ba70

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret5@
foreign import ccall safe "hs_bindgen_f8291bd691623354" hs_bindgen_f8291bd691623354_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret5@
hs_bindgen_f8291bd691623354 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_f8291bd691623354 =
  BG.fromFFIType hs_bindgen_f8291bd691623354_base

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h 267:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret5 = hs_bindgen_f8291bd691623354

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret6@
foreign import ccall safe "hs_bindgen_2eb8ef0a6794c225" hs_bindgen_2eb8ef0a6794c225_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret6@
hs_bindgen_2eb8ef0a6794c225 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_2eb8ef0a6794c225 =
  BG.fromFFIType hs_bindgen_2eb8ef0a6794c225_base

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h 268:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret6 = hs_bindgen_2eb8ef0a6794c225

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret7@
foreign import ccall safe "hs_bindgen_8cc60b45c83cb72b" hs_bindgen_8cc60b45c83cb72b_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret7@
hs_bindgen_8cc60b45c83cb72b ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_8cc60b45c83cb72b =
  BG.fromFFIType hs_bindgen_8cc60b45c83cb72b_base

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h 269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret7 = hs_bindgen_8cc60b45c83cb72b

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret8@
foreign import ccall safe "hs_bindgen_2ad075d6ca6beea5" hs_bindgen_2ad075d6ca6beea5_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret8@
hs_bindgen_2ad075d6ca6beea5 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_2ad075d6ca6beea5 =
  BG.fromFFIType hs_bindgen_2ad075d6ca6beea5_base

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h 270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret8 = hs_bindgen_2ad075d6ca6beea5

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret9@
foreign import ccall safe "hs_bindgen_fe5e863d48f781be" hs_bindgen_fe5e863d48f781be_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret9@
hs_bindgen_fe5e863d48f781be ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_fe5e863d48f781be =
  BG.fromFFIType hs_bindgen_fe5e863d48f781be_base

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h 271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret9 = hs_bindgen_fe5e863d48f781be

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret10@
foreign import ccall safe "hs_bindgen_2a029ddc00ba6ce1" hs_bindgen_2a029ddc00ba6ce1_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Safe_funptr_ret10@
hs_bindgen_2a029ddc00ba6ce1 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_2a029ddc00ba6ce1 =
  BG.fromFFIType hs_bindgen_2a029ddc00ba6ce1_base

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h 272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret10 = hs_bindgen_2a029ddc00ba6ce1
