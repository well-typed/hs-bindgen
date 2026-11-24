{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.args_char1
    , Example.Unsafe.args_char2
    , Example.Unsafe.args_char3
    , Example.Unsafe.args_short1
    , Example.Unsafe.args_short2
    , Example.Unsafe.args_short3
    , Example.Unsafe.args_int1
    , Example.Unsafe.args_int2
    , Example.Unsafe.args_int3
    , Example.Unsafe.args_long1
    , Example.Unsafe.args_long2
    , Example.Unsafe.args_long3
    , Example.Unsafe.args_float
    , Example.Unsafe.args_double
    , Example.Unsafe.args_bool1
    , Example.Unsafe.args_struct
    , Example.Unsafe.args_union
    , Example.Unsafe.args_enum
    , Example.Unsafe.args_pointer1
    , Example.Unsafe.args_pointer2
    , Example.Unsafe.args_pointer3
    , Example.Unsafe.ret_A
    , Example.Unsafe.ret_char1
    , Example.Unsafe.ret_char2
    , Example.Unsafe.ret_char3
    , Example.Unsafe.ret_short1
    , Example.Unsafe.ret_short2
    , Example.Unsafe.ret_short3
    , Example.Unsafe.ret_int1
    , Example.Unsafe.ret_int2
    , Example.Unsafe.ret_int3
    , Example.Unsafe.ret_long1
    , Example.Unsafe.ret_long2
    , Example.Unsafe.ret_long3
    , Example.Unsafe.ret_float
    , Example.Unsafe.ret_double
    , Example.Unsafe.ret_bool1
    , Example.Unsafe.ret_struct
    , Example.Unsafe.ret_union
    , Example.Unsafe.ret_enum
    , Example.Unsafe.ret_pointer1
    , Example.Unsafe.ret_pointer2
    , Example.Unsafe.ret_pointer3
    , Example.Unsafe.body1
    , Example.Unsafe.body2
    , Example.Unsafe.args_complex_float
    , Example.Unsafe.args_complex_double
    , Example.Unsafe.ret_complex_float
    , Example.Unsafe.ret_complex_double
    , Example.Unsafe.bespoke_args1
    , Example.Unsafe.bespoke_args2
    , Example.Unsafe.bespoke_ret1
    , Example.Unsafe.bespoke_ret2
    , Example.Unsafe.arr_args1
    , Example.Unsafe.arr_args2
    , Example.Unsafe.arr_args3
    , Example.Unsafe.arr_args4
    , Example.Unsafe.funptr_args1
    , Example.Unsafe.funptr_args2
    , Example.Unsafe.funptr_args3
    , Example.Unsafe.funptr_args4
    , Example.Unsafe.funptr_args5
    , Example.Unsafe.comments1
    , Example.Unsafe.const_prim_before1
    , Example.Unsafe.const_prim_before2
    , Example.Unsafe.const_prim_before3
    , Example.Unsafe.const_prim_after1
    , Example.Unsafe.const_prim_after2
    , Example.Unsafe.const_prim_after3
    , Example.Unsafe.const_withoutSign_before1
    , Example.Unsafe.const_withoutSign_before2
    , Example.Unsafe.const_withoutSign_before3
    , Example.Unsafe.const_withoutSign_before4
    , Example.Unsafe.const_withoutSign_before5
    , Example.Unsafe.const_withoutSign_before6
    , Example.Unsafe.const_withoutSign_before7
    , Example.Unsafe.const_withoutSign_before8
    , Example.Unsafe.const_withoutSign_after1
    , Example.Unsafe.const_withoutSign_after2
    , Example.Unsafe.const_withoutSign_after3
    , Example.Unsafe.const_withoutSign_after4
    , Example.Unsafe.const_withoutSign_after5
    , Example.Unsafe.const_withoutSign_after6
    , Example.Unsafe.const_withoutSign_after7
    , Example.Unsafe.const_withoutSign_after8
    , Example.Unsafe.const_pointers_args1
    , Example.Unsafe.const_pointers_args2
    , Example.Unsafe.const_pointers_args3
    , Example.Unsafe.const_pointers_args4
    , Example.Unsafe.const_pointers_args5
    , Example.Unsafe.const_pointers_ret1
    , Example.Unsafe.const_pointers_ret2
    , Example.Unsafe.const_pointers_ret3
    , Example.Unsafe.const_pointers_ret4
    , Example.Unsafe.const_pointers_ret5
    , Example.Unsafe.const_array_elem1
    , Example.Unsafe.const_array_elem2
    , Example.Unsafe.const_array_elem3
    , Example.Unsafe.noParams1
    , Example.Unsafe.noParams2
    , Example.Unsafe.noParams3
    , Example.Unsafe.funptr_ret1
    , Example.Unsafe.funptr_ret2
    , Example.Unsafe.funptr_ret3
    , Example.Unsafe.funptr_ret4
    , Example.Unsafe.funptr_ret5
    , Example.Unsafe.funptr_ret6
    , Example.Unsafe.funptr_ret7
    , Example.Unsafe.funptr_ret8
    , Example.Unsafe.funptr_ret9
    , Example.Unsafe.funptr_ret10
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
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse.h>"
  , "void hs_bindgen_6448f530b1517a59 ("
  , "  signed int arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  (args_char1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_108c76b2848e6d54 ("
  , "  signed int arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  (args_char2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2d4b1a75f6f59c5b ("
  , "  signed int arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  (args_char3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1236095d70a3ff3f ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3a213664fd798269 ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_554c22c3e55c18a7 ("
  , "  signed int arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  (args_short3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ad07735ff05cc46d ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_88c8a1f75c198f8e ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8d24ed2d9d725889 ("
  , "  signed int arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  (args_int3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_890ee6e5e9121648 ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_cb941b244987a522 ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a3ff0e1eebb2424b ("
  , "  signed int arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  (args_long3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_26b09c7e326fb3c1 ("
  , "  signed int arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  (args_float)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_109b1d9747cc2d2d ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  (args_double)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4327da73d9d86855 ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (args_bool1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_576bc45bc22cfe5e ("
  , "  signed int arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  (args_struct)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_0b0a08c66efe840e ("
  , "  signed int arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  (args_union)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_7bfdbf1ac4ea054b ("
  , "  signed int arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  (args_enum)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_27641aaf6cda68e3 ("
  , "  signed int arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  (args_pointer1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c75f8f681296d74c ("
  , "  signed int arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  (args_pointer2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_93530094eac440b2 ("
  , "  signed int arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  (args_pointer3)(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_cf83f084b6a81231 (void)"
  , "{"
  , "  return (ret_A)();"
  , "}"
  , "char hs_bindgen_fd6cfa4b316e0739 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char1)(arg1);"
  , "}"
  , "signed char hs_bindgen_fa2e2b2fca8d9654 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char2)(arg1);"
  , "}"
  , "unsigned char hs_bindgen_f4044a361d9942cd ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char3)(arg1);"
  , "}"
  , "signed short hs_bindgen_b4ebc3099765799f ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short1)(arg1);"
  , "}"
  , "signed short hs_bindgen_9df1263ca3c92111 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short2)(arg1);"
  , "}"
  , "unsigned short hs_bindgen_82aa728bbe106837 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short3)(arg1);"
  , "}"
  , "signed int hs_bindgen_7148291c56110562 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int1)(arg1);"
  , "}"
  , "signed int hs_bindgen_afe8488ee554e99e ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int2)(arg1);"
  , "}"
  , "unsigned int hs_bindgen_a3eb469a758e7e13 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int3)(arg1);"
  , "}"
  , "signed long hs_bindgen_f3e05eb898b31259 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long1)(arg1);"
  , "}"
  , "signed long hs_bindgen_f8dfab8e889d35c0 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long2)(arg1);"
  , "}"
  , "unsigned long hs_bindgen_b1bdfa2dbc1e0fdc ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long3)(arg1);"
  , "}"
  , "float hs_bindgen_00a493294dc8f0c2 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_float)(arg1);"
  , "}"
  , "double hs_bindgen_e31acbbe54e1d6d9 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_double)(arg1);"
  , "}"
  , "_Bool hs_bindgen_95bdeeac93b33d9c ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_bool1)(arg1);"
  , "}"
  , "void hs_bindgen_7582053589398bdd ("
  , "  signed int arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_struct)(arg1);"
  , "}"
  , "void hs_bindgen_423545d7c3963f58 ("
  , "  signed int arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_union)(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_9125ddf662b72b0d ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_enum)(arg1);"
  , "}"
  , "signed int *hs_bindgen_8e4a995cdd57aff3 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer1)(arg1);"
  , "}"
  , "signed int **hs_bindgen_59029e559a592079 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer2)(arg1);"
  , "}"
  , "void *hs_bindgen_751214cb51b76314 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer3)(arg1);"
  , "}"
  , "signed int hs_bindgen_f3c82ba208d636ee ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (body1)(arg1);"
  , "}"
  , "signed int hs_bindgen_71d4c66741004a25 (void)"
  , "{"
  , "  return (body2)();"
  , "}"
  , "void hs_bindgen_85280677ec35bd0c ("
  , "  signed int arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_float)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_97fc7fbd50b7e919 ("
  , "  signed int arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_double)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_8715588b04b3e0f0 ("
  , "  signed int arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_float)(arg1);"
  , "}"
  , "void hs_bindgen_a8a260fdd9ff3093 ("
  , "  signed int arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_double)(arg1);"
  , "}"
  , "void hs_bindgen_cbdd22386d6150ae ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (bespoke_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1043208b768ac883 ("
  , "  signed int arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  (bespoke_args2)(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_6bb186cc70e5f91d ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret1)(arg1);"
  , "}"
  , "size_t hs_bindgen_5177d72ffbe458a9 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret2)(arg1);"
  , "}"
  , "void hs_bindgen_2fd7ff58c5e04cec ("
  , "  signed int arg1[]"
  , ")"
  , "{"
  , "  (arr_args1)(arg1);"
  , "}"
  , "void hs_bindgen_d9d8e30d6b3c3cee ("
  , "  signed int *arg1[]"
  , ")"
  , "{"
  , "  (arr_args2)(arg1);"
  , "}"
  , "void hs_bindgen_fb294bf43d77f72a ("
  , "  signed int arg1[5]"
  , ")"
  , "{"
  , "  (arr_args3)(arg1);"
  , "}"
  , "void hs_bindgen_ad8d32216b36261c ("
  , "  signed int *arg1[5]"
  , ")"
  , "{"
  , "  (arr_args4)(arg1);"
  , "}"
  , "void hs_bindgen_74a25131d12ca2b0 ("
  , "  signed int arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_50343787dda1817e ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_377b9d8ba2b5b34b ("
  , "  signed int arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9364c52d3faee0db ("
  , "  signed int arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_453fa57f7149caea ("
  , "  signed int arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args5)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2e1ba55fcc77e24e ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (comments1)(arg1);"
  , "}"
  , "void hs_bindgen_b041d62a9fdd9a38 ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ec2ee6a22d0cd792 ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9c1530cae4d6bcfd ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b80a39e376927ea4 ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3630309782c30a03 ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e43e1936d66096f4 ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_12a4e22aef83eb87 ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_dee7b440fd9165d2 ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a7f0535fbe9a29b2 ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b2fc471aa0642c61 ("
  , "  signed int arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_13abd80fdff110c6 ("
  , "  signed int arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_4777fcef57294b3d ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9e06fc613313c597 ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_73f5e08ab7bd794b ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b14f99125c09c927 ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_556866f9af5fde43 ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_5c48020adde0c968 ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_daa7275a71a9097b ("
  , "  signed int arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_5f9bf73a7cf0466c ("
  , "  signed int arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_05a68f095a849513 ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_eab87bfb2631dc6b ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3ed2b97d934ca93c ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_630176bbd5e66084 ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4dd601ef369e68cc ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_924f6c70a7144c60 ("
  , "  signed int arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1186886634b954ed ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2e1883ef3cecf662 ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args5)(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_50d86134f90cf6fd ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret1)(arg1);"
  , "}"
  , "signed int const *hs_bindgen_768447ff73613740 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret2)(arg1);"
  , "}"
  , "signed int *const hs_bindgen_4d20943b7ef372c8 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret3)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_c5b4ac7cc309ef4b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret4)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_f9413491848fbfe5 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret5)(arg1);"
  , "}"
  , "void hs_bindgen_35b43343e1505a9f ("
  , "  signed int const arg1[]"
  , ")"
  , "{"
  , "  (const_array_elem1)(arg1);"
  , "}"
  , "void hs_bindgen_33fb0e4285c5cfeb ("
  , "  signed int const *arg1[]"
  , ")"
  , "{"
  , "  (const_array_elem2)(arg1);"
  , "}"
  , "void hs_bindgen_4b391cb720429a17 ("
  , "  signed int *const arg1[]"
  , ")"
  , "{"
  , "  (const_array_elem3)(arg1);"
  , "}"
  , "signed int hs_bindgen_35ba30a3c17deabd (void)"
  , "{"
  , "  return (noParams1)();"
  , "}"
  , "signed int hs_bindgen_2ceca322647596fe (void)"
  , "{"
  , "  return (noParams2)();"
  , "}"
  , "void hs_bindgen_2cdc3cb3c34d668f ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (noParams3)(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_66bc29740adfcc7a ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_c46d39805a107470 ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret2)(arg1);"
  , "}"
  , "void (*hs_bindgen_3b9e3f14dfcdf935 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (funptr_ret3)(arg1);"
  , "}"
  , "char (*hs_bindgen_51cf7b4c9987a2d6 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret4)(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_13b9d73bf1eacbea ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret5)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_16c8d185cd1b6b6a ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret6)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_877736bfb7172200 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret7)(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_9a92afad45659474 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret8)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_c05706d70431baec ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret9)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_2ba78e0232c0eba0 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret10)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_char1@
foreign import ccall unsafe "hs_bindgen_6448f530b1517a59" hs_bindgen_6448f530b1517a59_base ::
     BG.CInt
  -> BG.CChar
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_char1@
hs_bindgen_6448f530b1517a59 ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_6448f530b1517a59 =
  \x0 ->
    \x1 ->
      hs_bindgen_6448f530b1517a59_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_char1 = hs_bindgen_6448f530b1517a59

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_char2@
foreign import ccall unsafe "hs_bindgen_108c76b2848e6d54" hs_bindgen_108c76b2848e6d54_base ::
     BG.CInt
  -> BG.CSChar
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_char2@
hs_bindgen_108c76b2848e6d54 ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_108c76b2848e6d54 =
  \x0 ->
    \x1 ->
      hs_bindgen_108c76b2848e6d54_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_char2 = hs_bindgen_108c76b2848e6d54

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_char3@
foreign import ccall unsafe "hs_bindgen_2d4b1a75f6f59c5b" hs_bindgen_2d4b1a75f6f59c5b_base ::
     BG.CInt
  -> BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_char3@
hs_bindgen_2d4b1a75f6f59c5b ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_2d4b1a75f6f59c5b =
  \x0 ->
    \x1 ->
      hs_bindgen_2d4b1a75f6f59c5b_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_char3 = hs_bindgen_2d4b1a75f6f59c5b

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_short1@
foreign import ccall unsafe "hs_bindgen_1236095d70a3ff3f" hs_bindgen_1236095d70a3ff3f_base ::
     BG.CInt
  -> BG.CShort
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_short1@
hs_bindgen_1236095d70a3ff3f ::
     BG.CInt
  -> BG.CShort
  -> IO ()
hs_bindgen_1236095d70a3ff3f =
  \x0 ->
    \x1 ->
      hs_bindgen_1236095d70a3ff3f_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_short1 = hs_bindgen_1236095d70a3ff3f

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_short2@
foreign import ccall unsafe "hs_bindgen_3a213664fd798269" hs_bindgen_3a213664fd798269_base ::
     BG.CInt
  -> BG.CShort
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_short2@
hs_bindgen_3a213664fd798269 ::
     BG.CInt
  -> BG.CShort
  -> IO ()
hs_bindgen_3a213664fd798269 =
  \x0 ->
    \x1 ->
      hs_bindgen_3a213664fd798269_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_short2 = hs_bindgen_3a213664fd798269

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_short3@
foreign import ccall unsafe "hs_bindgen_554c22c3e55c18a7" hs_bindgen_554c22c3e55c18a7_base ::
     BG.CInt
  -> BG.CUShort
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_short3@
hs_bindgen_554c22c3e55c18a7 ::
     BG.CInt
  -> BG.CUShort
  -> IO ()
hs_bindgen_554c22c3e55c18a7 =
  \x0 ->
    \x1 ->
      hs_bindgen_554c22c3e55c18a7_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_short3 = hs_bindgen_554c22c3e55c18a7

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_int1@
foreign import ccall unsafe "hs_bindgen_ad07735ff05cc46d" hs_bindgen_ad07735ff05cc46d_base ::
     BG.CInt
  -> BG.CInt
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_int1@
hs_bindgen_ad07735ff05cc46d ::
     BG.CInt
  -> BG.CInt
  -> IO ()
hs_bindgen_ad07735ff05cc46d =
  \x0 ->
    \x1 ->
      hs_bindgen_ad07735ff05cc46d_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_int1 = hs_bindgen_ad07735ff05cc46d

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_int2@
foreign import ccall unsafe "hs_bindgen_88c8a1f75c198f8e" hs_bindgen_88c8a1f75c198f8e_base ::
     BG.CInt
  -> BG.CInt
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_int2@
hs_bindgen_88c8a1f75c198f8e ::
     BG.CInt
  -> BG.CInt
  -> IO ()
hs_bindgen_88c8a1f75c198f8e =
  \x0 ->
    \x1 ->
      hs_bindgen_88c8a1f75c198f8e_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_int2 = hs_bindgen_88c8a1f75c198f8e

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_int3@
foreign import ccall unsafe "hs_bindgen_8d24ed2d9d725889" hs_bindgen_8d24ed2d9d725889_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_int3@
hs_bindgen_8d24ed2d9d725889 ::
     BG.CInt
  -> BG.CUInt
  -> IO ()
hs_bindgen_8d24ed2d9d725889 =
  \x0 ->
    \x1 ->
      hs_bindgen_8d24ed2d9d725889_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_int3 = hs_bindgen_8d24ed2d9d725889

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_long1@
foreign import ccall unsafe "hs_bindgen_890ee6e5e9121648" hs_bindgen_890ee6e5e9121648_base ::
     BG.CInt
  -> BG.CLong
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_long1@
hs_bindgen_890ee6e5e9121648 ::
     BG.CInt
  -> BG.CLong
  -> IO ()
hs_bindgen_890ee6e5e9121648 =
  \x0 ->
    \x1 ->
      hs_bindgen_890ee6e5e9121648_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_long1 = hs_bindgen_890ee6e5e9121648

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_long2@
foreign import ccall unsafe "hs_bindgen_cb941b244987a522" hs_bindgen_cb941b244987a522_base ::
     BG.CInt
  -> BG.CLong
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_long2@
hs_bindgen_cb941b244987a522 ::
     BG.CInt
  -> BG.CLong
  -> IO ()
hs_bindgen_cb941b244987a522 =
  \x0 ->
    \x1 ->
      hs_bindgen_cb941b244987a522_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_long2 = hs_bindgen_cb941b244987a522

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_long3@
foreign import ccall unsafe "hs_bindgen_a3ff0e1eebb2424b" hs_bindgen_a3ff0e1eebb2424b_base ::
     BG.CInt
  -> BG.CULong
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_long3@
hs_bindgen_a3ff0e1eebb2424b ::
     BG.CInt
  -> BG.CULong
  -> IO ()
hs_bindgen_a3ff0e1eebb2424b =
  \x0 ->
    \x1 ->
      hs_bindgen_a3ff0e1eebb2424b_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_long3 = hs_bindgen_a3ff0e1eebb2424b

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_float@
foreign import ccall unsafe "hs_bindgen_26b09c7e326fb3c1" hs_bindgen_26b09c7e326fb3c1_base ::
     BG.CInt
  -> BG.CFloat
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_float@
hs_bindgen_26b09c7e326fb3c1 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_26b09c7e326fb3c1 =
  \x0 ->
    \x1 ->
      hs_bindgen_26b09c7e326fb3c1_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_float = hs_bindgen_26b09c7e326fb3c1

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_double@
foreign import ccall unsafe "hs_bindgen_109b1d9747cc2d2d" hs_bindgen_109b1d9747cc2d2d_base ::
     BG.CInt
  -> BG.CDouble
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_double@
hs_bindgen_109b1d9747cc2d2d ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_109b1d9747cc2d2d =
  \x0 ->
    \x1 ->
      hs_bindgen_109b1d9747cc2d2d_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_double = hs_bindgen_109b1d9747cc2d2d

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_bool1@
foreign import ccall unsafe "hs_bindgen_4327da73d9d86855" hs_bindgen_4327da73d9d86855_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_bool1@
hs_bindgen_4327da73d9d86855 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_4327da73d9d86855 =
  \x0 ->
    \x1 ->
      hs_bindgen_4327da73d9d86855_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_bool1 = hs_bindgen_4327da73d9d86855

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_struct@
foreign import ccall unsafe "hs_bindgen_576bc45bc22cfe5e" hs_bindgen_576bc45bc22cfe5e_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_struct@
hs_bindgen_576bc45bc22cfe5e ::
     BG.CInt
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_576bc45bc22cfe5e =
  \x0 ->
    \x1 ->
      hs_bindgen_576bc45bc22cfe5e_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_576bc45bc22cfe5e arg10 arg22)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_union@
foreign import ccall unsafe "hs_bindgen_0b0a08c66efe840e" hs_bindgen_0b0a08c66efe840e_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_union@
hs_bindgen_0b0a08c66efe840e ::
     BG.CInt
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_0b0a08c66efe840e =
  \x0 ->
    \x1 ->
      hs_bindgen_0b0a08c66efe840e_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_0b0a08c66efe840e arg10 arg22)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_enum@
foreign import ccall unsafe "hs_bindgen_7bfdbf1ac4ea054b" hs_bindgen_7bfdbf1ac4ea054b_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_enum@
hs_bindgen_7bfdbf1ac4ea054b ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_7bfdbf1ac4ea054b =
  \x0 ->
    \x1 ->
      hs_bindgen_7bfdbf1ac4ea054b_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_enum = hs_bindgen_7bfdbf1ac4ea054b

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_pointer1@
foreign import ccall unsafe "hs_bindgen_27641aaf6cda68e3" hs_bindgen_27641aaf6cda68e3_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_pointer1@
hs_bindgen_27641aaf6cda68e3 ::
     BG.CInt
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_27641aaf6cda68e3 =
  \x0 ->
    \x1 ->
      hs_bindgen_27641aaf6cda68e3_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_pointer1 = hs_bindgen_27641aaf6cda68e3

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_pointer2@
foreign import ccall unsafe "hs_bindgen_c75f8f681296d74c" hs_bindgen_c75f8f681296d74c_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_pointer2@
hs_bindgen_c75f8f681296d74c ::
     BG.CInt
  -> BG.Ptr (BG.Ptr BG.CInt)
  -> IO ()
hs_bindgen_c75f8f681296d74c =
  \x0 ->
    \x1 ->
      hs_bindgen_c75f8f681296d74c_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_pointer2 = hs_bindgen_c75f8f681296d74c

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_pointer3@
foreign import ccall unsafe "hs_bindgen_93530094eac440b2" hs_bindgen_93530094eac440b2_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_pointer3@
hs_bindgen_93530094eac440b2 ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()
hs_bindgen_93530094eac440b2 =
  \x0 ->
    \x1 ->
      hs_bindgen_93530094eac440b2_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_pointer3 = hs_bindgen_93530094eac440b2

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_A@
foreign import ccall unsafe "hs_bindgen_cf83f084b6a81231" hs_bindgen_cf83f084b6a81231_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_A@
hs_bindgen_cf83f084b6a81231 :: IO BG.CInt
hs_bindgen_cf83f084b6a81231 =
  fmap BG.fromFFIType hs_bindgen_cf83f084b6a81231_base

{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h 47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A :: IO BG.CInt
ret_A = hs_bindgen_cf83f084b6a81231

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_char1@
foreign import ccall unsafe "hs_bindgen_fd6cfa4b316e0739" hs_bindgen_fd6cfa4b316e0739_base ::
     BG.CInt
  -> IO BG.CChar

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_char1@
hs_bindgen_fd6cfa4b316e0739 ::
     BG.CInt
  -> IO BG.CChar
hs_bindgen_fd6cfa4b316e0739 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_fd6cfa4b316e0739_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h 49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CChar
ret_char1 = hs_bindgen_fd6cfa4b316e0739

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_char2@
foreign import ccall unsafe "hs_bindgen_fa2e2b2fca8d9654" hs_bindgen_fa2e2b2fca8d9654_base ::
     BG.CInt
  -> IO BG.CSChar

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_char2@
hs_bindgen_fa2e2b2fca8d9654 ::
     BG.CInt
  -> IO BG.CSChar
hs_bindgen_fa2e2b2fca8d9654 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_fa2e2b2fca8d9654_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h 50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CSChar
ret_char2 = hs_bindgen_fa2e2b2fca8d9654

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_char3@
foreign import ccall unsafe "hs_bindgen_f4044a361d9942cd" hs_bindgen_f4044a361d9942cd_base ::
     BG.CInt
  -> IO BG.CUChar

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_char3@
hs_bindgen_f4044a361d9942cd ::
     BG.CInt
  -> IO BG.CUChar
hs_bindgen_f4044a361d9942cd =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_f4044a361d9942cd_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h 51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUChar
ret_char3 = hs_bindgen_f4044a361d9942cd

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_short1@
foreign import ccall unsafe "hs_bindgen_b4ebc3099765799f" hs_bindgen_b4ebc3099765799f_base ::
     BG.CInt
  -> IO BG.CShort

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_short1@
hs_bindgen_b4ebc3099765799f ::
     BG.CInt
  -> IO BG.CShort
hs_bindgen_b4ebc3099765799f =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_b4ebc3099765799f_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h 53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short1 = hs_bindgen_b4ebc3099765799f

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_short2@
foreign import ccall unsafe "hs_bindgen_9df1263ca3c92111" hs_bindgen_9df1263ca3c92111_base ::
     BG.CInt
  -> IO BG.CShort

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_short2@
hs_bindgen_9df1263ca3c92111 ::
     BG.CInt
  -> IO BG.CShort
hs_bindgen_9df1263ca3c92111 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_9df1263ca3c92111_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h 54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short2 = hs_bindgen_9df1263ca3c92111

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_short3@
foreign import ccall unsafe "hs_bindgen_82aa728bbe106837" hs_bindgen_82aa728bbe106837_base ::
     BG.CInt
  -> IO BG.CUShort

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_short3@
hs_bindgen_82aa728bbe106837 ::
     BG.CInt
  -> IO BG.CUShort
hs_bindgen_82aa728bbe106837 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_82aa728bbe106837_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h 55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUShort
ret_short3 = hs_bindgen_82aa728bbe106837

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_int1@
foreign import ccall unsafe "hs_bindgen_7148291c56110562" hs_bindgen_7148291c56110562_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_int1@
hs_bindgen_7148291c56110562 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_7148291c56110562 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_7148291c56110562_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h 57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int1 = hs_bindgen_7148291c56110562

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_int2@
foreign import ccall unsafe "hs_bindgen_afe8488ee554e99e" hs_bindgen_afe8488ee554e99e_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_int2@
hs_bindgen_afe8488ee554e99e ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_afe8488ee554e99e =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_afe8488ee554e99e_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h 58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int2 = hs_bindgen_afe8488ee554e99e

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_int3@
foreign import ccall unsafe "hs_bindgen_a3eb469a758e7e13" hs_bindgen_a3eb469a758e7e13_base ::
     BG.CInt
  -> IO BG.CUInt

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_int3@
hs_bindgen_a3eb469a758e7e13 ::
     BG.CInt
  -> IO BG.CUInt
hs_bindgen_a3eb469a758e7e13 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_a3eb469a758e7e13_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h 59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUInt
ret_int3 = hs_bindgen_a3eb469a758e7e13

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_long1@
foreign import ccall unsafe "hs_bindgen_f3e05eb898b31259" hs_bindgen_f3e05eb898b31259_base ::
     BG.CInt
  -> IO BG.CLong

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_long1@
hs_bindgen_f3e05eb898b31259 ::
     BG.CInt
  -> IO BG.CLong
hs_bindgen_f3e05eb898b31259 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_f3e05eb898b31259_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h 61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long1 = hs_bindgen_f3e05eb898b31259

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_long2@
foreign import ccall unsafe "hs_bindgen_f8dfab8e889d35c0" hs_bindgen_f8dfab8e889d35c0_base ::
     BG.CInt
  -> IO BG.CLong

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_long2@
hs_bindgen_f8dfab8e889d35c0 ::
     BG.CInt
  -> IO BG.CLong
hs_bindgen_f8dfab8e889d35c0 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_f8dfab8e889d35c0_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h 62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long2 = hs_bindgen_f8dfab8e889d35c0

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_long3@
foreign import ccall unsafe "hs_bindgen_b1bdfa2dbc1e0fdc" hs_bindgen_b1bdfa2dbc1e0fdc_base ::
     BG.CInt
  -> IO BG.CULong

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_long3@
hs_bindgen_b1bdfa2dbc1e0fdc ::
     BG.CInt
  -> IO BG.CULong
hs_bindgen_b1bdfa2dbc1e0fdc =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_b1bdfa2dbc1e0fdc_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h 63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CULong
ret_long3 = hs_bindgen_b1bdfa2dbc1e0fdc

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_float@
foreign import ccall unsafe "hs_bindgen_00a493294dc8f0c2" hs_bindgen_00a493294dc8f0c2_base ::
     BG.CInt
  -> IO BG.CFloat

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_float@
hs_bindgen_00a493294dc8f0c2 ::
     BG.CInt
  -> IO BG.CFloat
hs_bindgen_00a493294dc8f0c2 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_00a493294dc8f0c2_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h 65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CFloat
ret_float = hs_bindgen_00a493294dc8f0c2

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_double@
foreign import ccall unsafe "hs_bindgen_e31acbbe54e1d6d9" hs_bindgen_e31acbbe54e1d6d9_base ::
     BG.CInt
  -> IO BG.CDouble

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_double@
hs_bindgen_e31acbbe54e1d6d9 ::
     BG.CInt
  -> IO BG.CDouble
hs_bindgen_e31acbbe54e1d6d9 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_e31acbbe54e1d6d9_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h 66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CDouble
ret_double = hs_bindgen_e31acbbe54e1d6d9

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_bool1@
foreign import ccall unsafe "hs_bindgen_95bdeeac93b33d9c" hs_bindgen_95bdeeac93b33d9c_base ::
     BG.CInt
  -> IO BG.CBool

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_bool1@
hs_bindgen_95bdeeac93b33d9c ::
     BG.CInt
  -> IO BG.CBool
hs_bindgen_95bdeeac93b33d9c =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_95bdeeac93b33d9c_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h 67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
ret_bool1 = hs_bindgen_95bdeeac93b33d9c

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_struct@
foreign import ccall unsafe "hs_bindgen_7582053589398bdd" hs_bindgen_7582053589398bdd_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_struct@
hs_bindgen_7582053589398bdd ::
     BG.CInt
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_7582053589398bdd =
  \x0 ->
    \x1 ->
      hs_bindgen_7582053589398bdd_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_7582053589398bdd arg10 res1)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_union@
foreign import ccall unsafe "hs_bindgen_423545d7c3963f58" hs_bindgen_423545d7c3963f58_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_union@
hs_bindgen_423545d7c3963f58 ::
     BG.CInt
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_423545d7c3963f58 =
  \x0 ->
    \x1 ->
      hs_bindgen_423545d7c3963f58_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_423545d7c3963f58 arg10 res1)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_enum@
foreign import ccall unsafe "hs_bindgen_9125ddf662b72b0d" hs_bindgen_9125ddf662b72b0d_base ::
     BG.CInt
  -> IO BG.CUInt

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_enum@
hs_bindgen_9125ddf662b72b0d ::
     BG.CInt
  -> IO Some_enum
hs_bindgen_9125ddf662b72b0d =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_9125ddf662b72b0d_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h 71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO Some_enum
ret_enum = hs_bindgen_9125ddf662b72b0d

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_pointer1@
foreign import ccall unsafe "hs_bindgen_8e4a995cdd57aff3" hs_bindgen_8e4a995cdd57aff3_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_pointer1@
hs_bindgen_8e4a995cdd57aff3 ::
     BG.CInt
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_8e4a995cdd57aff3 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_8e4a995cdd57aff3_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h 73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
ret_pointer1 = hs_bindgen_8e4a995cdd57aff3

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_pointer2@
foreign import ccall unsafe "hs_bindgen_59029e559a592079" hs_bindgen_59029e559a592079_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_pointer2@
hs_bindgen_59029e559a592079 ::
     BG.CInt
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
hs_bindgen_59029e559a592079 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_59029e559a592079_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h 74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
ret_pointer2 = hs_bindgen_59029e559a592079

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_pointer3@
foreign import ccall unsafe "hs_bindgen_751214cb51b76314" hs_bindgen_751214cb51b76314_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_pointer3@
hs_bindgen_751214cb51b76314 ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)
hs_bindgen_751214cb51b76314 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_751214cb51b76314_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h 75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.Void)
ret_pointer3 = hs_bindgen_751214cb51b76314

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_body1@
foreign import ccall unsafe "hs_bindgen_f3c82ba208d636ee" hs_bindgen_f3c82ba208d636ee_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_body1@
hs_bindgen_f3c82ba208d636ee ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_f3c82ba208d636ee =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_f3c82ba208d636ee_base (BG.toFFIType x0))

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h 79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
body1 = hs_bindgen_f3c82ba208d636ee

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_body2@
foreign import ccall unsafe "hs_bindgen_71d4c66741004a25" hs_bindgen_71d4c66741004a25_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_body2@
hs_bindgen_71d4c66741004a25 :: IO BG.CInt
hs_bindgen_71d4c66741004a25 =
  fmap BG.fromFFIType hs_bindgen_71d4c66741004a25_base

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h 80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2 :: IO BG.CInt
body2 = hs_bindgen_71d4c66741004a25

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_complex_float@
foreign import ccall unsafe "hs_bindgen_85280677ec35bd0c" hs_bindgen_85280677ec35bd0c_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_complex_float@
hs_bindgen_85280677ec35bd0c ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_85280677ec35bd0c =
  \x0 ->
    \x1 ->
      hs_bindgen_85280677ec35bd0c_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_85280677ec35bd0c arg10 arg22)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_complex_double@
foreign import ccall unsafe "hs_bindgen_97fc7fbd50b7e919" hs_bindgen_97fc7fbd50b7e919_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_args_complex_double@
hs_bindgen_97fc7fbd50b7e919 ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_97fc7fbd50b7e919 =
  \x0 ->
    \x1 ->
      hs_bindgen_97fc7fbd50b7e919_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_97fc7fbd50b7e919 arg10 arg22)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_complex_float@
foreign import ccall unsafe "hs_bindgen_8715588b04b3e0f0" hs_bindgen_8715588b04b3e0f0_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_complex_float@
hs_bindgen_8715588b04b3e0f0 ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_8715588b04b3e0f0 =
  \x0 ->
    \x1 ->
      hs_bindgen_8715588b04b3e0f0_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_8715588b04b3e0f0 arg10 res1)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_complex_double@
foreign import ccall unsafe "hs_bindgen_a8a260fdd9ff3093" hs_bindgen_a8a260fdd9ff3093_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_ret_complex_double@
hs_bindgen_a8a260fdd9ff3093 ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_a8a260fdd9ff3093 =
  \x0 ->
    \x1 ->
      hs_bindgen_a8a260fdd9ff3093_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_a8a260fdd9ff3093 arg10 res1)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_bespoke_args1@
foreign import ccall unsafe "hs_bindgen_cbdd22386d6150ae" hs_bindgen_cbdd22386d6150ae_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_bespoke_args1@
hs_bindgen_cbdd22386d6150ae ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_cbdd22386d6150ae =
  \x0 ->
    \x1 ->
      hs_bindgen_cbdd22386d6150ae_base (BG.toFFIType x0) (BG.toFFIType x1)

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
bespoke_args1 = hs_bindgen_cbdd22386d6150ae

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_bespoke_args2@
foreign import ccall unsafe "hs_bindgen_1043208b768ac883" hs_bindgen_1043208b768ac883_base ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_bespoke_args2@
hs_bindgen_1043208b768ac883 ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_1043208b768ac883 =
  \x0 ->
    \x1 ->
      hs_bindgen_1043208b768ac883_base (BG.toFFIType x0) (BG.toFFIType x1)

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
bespoke_args2 = hs_bindgen_1043208b768ac883

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_bespoke_ret1@
foreign import ccall unsafe "hs_bindgen_6bb186cc70e5f91d" hs_bindgen_6bb186cc70e5f91d_base ::
     BG.CInt
  -> IO BG.CBool

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_bespoke_ret1@
hs_bindgen_6bb186cc70e5f91d ::
     BG.CInt
  -> IO BG.CBool
hs_bindgen_6bb186cc70e5f91d =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_6bb186cc70e5f91d_base (BG.toFFIType x0))

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h 97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
bespoke_ret1 = hs_bindgen_6bb186cc70e5f91d

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_bespoke_ret2@
foreign import ccall unsafe "hs_bindgen_5177d72ffbe458a9" hs_bindgen_5177d72ffbe458a9_base ::
     BG.CInt
  -> IO HsBindgen.Runtime.LibC.CSize

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_bespoke_ret2@
hs_bindgen_5177d72ffbe458a9 ::
     BG.CInt
  -> IO HsBindgen.Runtime.LibC.CSize
hs_bindgen_5177d72ffbe458a9 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_5177d72ffbe458a9_base (BG.toFFIType x0))

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h 98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO HsBindgen.Runtime.LibC.CSize
bespoke_ret2 = hs_bindgen_5177d72ffbe458a9

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_arr_args1@
foreign import ccall unsafe "hs_bindgen_2fd7ff58c5e04cec" hs_bindgen_2fd7ff58c5e04cec_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_arr_args1@
hs_bindgen_2fd7ff58c5e04cec ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt))
  -> IO ()
hs_bindgen_2fd7ff58c5e04cec =
  \x0 ->
    hs_bindgen_2fd7ff58c5e04cec_base (BG.toFFIType x0)

{-| Arrays

    __C declaration:__ @arr_args1@

    __defined at:__ @macros\/reparse.h 104:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args1 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args1 = hs_bindgen_2fd7ff58c5e04cec

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_arr_args2@
foreign import ccall unsafe "hs_bindgen_d9d8e30d6b3c3cee" hs_bindgen_d9d8e30d6b3c3cee_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_arr_args2@
hs_bindgen_d9d8e30d6b3c3cee ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_d9d8e30d6b3c3cee =
  \x0 ->
    hs_bindgen_d9d8e30d6b3c3cee_base (BG.toFFIType x0)

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h 105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args2 = hs_bindgen_d9d8e30d6b3c3cee

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_arr_args3@
foreign import ccall unsafe "hs_bindgen_fb294bf43d77f72a" hs_bindgen_fb294bf43d77f72a_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_arr_args3@
hs_bindgen_fb294bf43d77f72a ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt))
  -> IO ()
hs_bindgen_fb294bf43d77f72a =
  \x0 ->
    hs_bindgen_fb294bf43d77f72a_base (BG.toFFIType x0)

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h 106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args3 = hs_bindgen_fb294bf43d77f72a

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_arr_args4@
foreign import ccall unsafe "hs_bindgen_ad8d32216b36261c" hs_bindgen_ad8d32216b36261c_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_arr_args4@
hs_bindgen_ad8d32216b36261c ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_ad8d32216b36261c =
  \x0 ->
    hs_bindgen_ad8d32216b36261c_base (BG.toFFIType x0)

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h 107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args4 = hs_bindgen_ad8d32216b36261c

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_args1@
foreign import ccall unsafe "hs_bindgen_74a25131d12ca2b0" hs_bindgen_74a25131d12ca2b0_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_args1@
hs_bindgen_74a25131d12ca2b0 ::
     BG.CInt
  -> BG.FunPtr (IO ())
  -> IO ()
hs_bindgen_74a25131d12ca2b0 =
  \x0 ->
    \x1 ->
      hs_bindgen_74a25131d12ca2b0_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args1 = hs_bindgen_74a25131d12ca2b0

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_args2@
foreign import ccall unsafe "hs_bindgen_50343787dda1817e" hs_bindgen_50343787dda1817e_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_args2@
hs_bindgen_50343787dda1817e ::
     BG.CInt
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_50343787dda1817e =
  \x0 ->
    \x1 ->
      hs_bindgen_50343787dda1817e_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args2 = hs_bindgen_50343787dda1817e

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_args3@
foreign import ccall unsafe "hs_bindgen_377b9d8ba2b5b34b" hs_bindgen_377b9d8ba2b5b34b_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_args3@
hs_bindgen_377b9d8ba2b5b34b ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> IO ())
  -> IO ()
hs_bindgen_377b9d8ba2b5b34b =
  \x0 ->
    \x1 ->
      hs_bindgen_377b9d8ba2b5b34b_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args3 = hs_bindgen_377b9d8ba2b5b34b

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_args4@
foreign import ccall unsafe "hs_bindgen_9364c52d3faee0db" hs_bindgen_9364c52d3faee0db_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_args4@
hs_bindgen_9364c52d3faee0db ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar)
  -> IO ()
hs_bindgen_9364c52d3faee0db =
  \x0 ->
    \x1 ->
      hs_bindgen_9364c52d3faee0db_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args4 = hs_bindgen_9364c52d3faee0db

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_args5@
foreign import ccall unsafe "hs_bindgen_453fa57f7149caea" hs_bindgen_453fa57f7149caea_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_args5@
hs_bindgen_453fa57f7149caea ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))
  -> IO ()
hs_bindgen_453fa57f7149caea =
  \x0 ->
    \x1 ->
      hs_bindgen_453fa57f7149caea_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args5 = hs_bindgen_453fa57f7149caea

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_comments1@
foreign import ccall unsafe "hs_bindgen_2e1ba55fcc77e24e" hs_bindgen_2e1ba55fcc77e24e_base ::
     BG.CInt
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_comments1@
hs_bindgen_2e1ba55fcc77e24e ::
     BG.CInt
  -> IO ()
hs_bindgen_2e1ba55fcc77e24e =
  \x0 ->
    hs_bindgen_2e1ba55fcc77e24e_base (BG.toFFIType x0)

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
comments1 = hs_bindgen_2e1ba55fcc77e24e

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_prim_before1@
foreign import ccall unsafe "hs_bindgen_b041d62a9fdd9a38" hs_bindgen_b041d62a9fdd9a38_base ::
     BG.CInt
  -> BG.CChar
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_prim_before1@
hs_bindgen_b041d62a9fdd9a38 ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_b041d62a9fdd9a38 =
  \x0 ->
    \x1 ->
      hs_bindgen_b041d62a9fdd9a38_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_before1 = hs_bindgen_b041d62a9fdd9a38

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_prim_before2@
foreign import ccall unsafe "hs_bindgen_ec2ee6a22d0cd792" hs_bindgen_ec2ee6a22d0cd792_base ::
     BG.CInt
  -> BG.CSChar
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_prim_before2@
hs_bindgen_ec2ee6a22d0cd792 ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_ec2ee6a22d0cd792 =
  \x0 ->
    \x1 ->
      hs_bindgen_ec2ee6a22d0cd792_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_before2 = hs_bindgen_ec2ee6a22d0cd792

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_prim_before3@
foreign import ccall unsafe "hs_bindgen_9c1530cae4d6bcfd" hs_bindgen_9c1530cae4d6bcfd_base ::
     BG.CInt
  -> BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_prim_before3@
hs_bindgen_9c1530cae4d6bcfd ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_9c1530cae4d6bcfd =
  \x0 ->
    \x1 ->
      hs_bindgen_9c1530cae4d6bcfd_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_before3 = hs_bindgen_9c1530cae4d6bcfd

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_prim_after1@
foreign import ccall unsafe "hs_bindgen_b80a39e376927ea4" hs_bindgen_b80a39e376927ea4_base ::
     BG.CInt
  -> BG.CChar
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_prim_after1@
hs_bindgen_b80a39e376927ea4 ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_b80a39e376927ea4 =
  \x0 ->
    \x1 ->
      hs_bindgen_b80a39e376927ea4_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_after1 = hs_bindgen_b80a39e376927ea4

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_prim_after2@
foreign import ccall unsafe "hs_bindgen_3630309782c30a03" hs_bindgen_3630309782c30a03_base ::
     BG.CInt
  -> BG.CSChar
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_prim_after2@
hs_bindgen_3630309782c30a03 ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_3630309782c30a03 =
  \x0 ->
    \x1 ->
      hs_bindgen_3630309782c30a03_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_after2 = hs_bindgen_3630309782c30a03

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_prim_after3@
foreign import ccall unsafe "hs_bindgen_e43e1936d66096f4" hs_bindgen_e43e1936d66096f4_base ::
     BG.CInt
  -> BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_prim_after3@
hs_bindgen_e43e1936d66096f4 ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_e43e1936d66096f4 =
  \x0 ->
    \x1 ->
      hs_bindgen_e43e1936d66096f4_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_after3 = hs_bindgen_e43e1936d66096f4

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_before1@
foreign import ccall unsafe "hs_bindgen_12a4e22aef83eb87" hs_bindgen_12a4e22aef83eb87_base ::
     BG.CInt
  -> BG.CFloat
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_before1@
hs_bindgen_12a4e22aef83eb87 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_12a4e22aef83eb87 =
  \x0 ->
    \x1 ->
      hs_bindgen_12a4e22aef83eb87_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_12a4e22aef83eb87

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_before2@
foreign import ccall unsafe "hs_bindgen_dee7b440fd9165d2" hs_bindgen_dee7b440fd9165d2_base ::
     BG.CInt
  -> BG.CDouble
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_before2@
hs_bindgen_dee7b440fd9165d2 ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_dee7b440fd9165d2 =
  \x0 ->
    \x1 ->
      hs_bindgen_dee7b440fd9165d2_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_dee7b440fd9165d2

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_before3@
foreign import ccall unsafe "hs_bindgen_a7f0535fbe9a29b2" hs_bindgen_a7f0535fbe9a29b2_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_before3@
hs_bindgen_a7f0535fbe9a29b2 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_a7f0535fbe9a29b2 =
  \x0 ->
    \x1 ->
      hs_bindgen_a7f0535fbe9a29b2_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_a7f0535fbe9a29b2

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_before4@
foreign import ccall unsafe "hs_bindgen_b2fc471aa0642c61" hs_bindgen_b2fc471aa0642c61_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_before4@
hs_bindgen_b2fc471aa0642c61 ::
     BG.CInt
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_b2fc471aa0642c61 =
  \x0 ->
    \x1 ->
      hs_bindgen_b2fc471aa0642c61_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_b2fc471aa0642c61 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_before5@
foreign import ccall unsafe "hs_bindgen_13abd80fdff110c6" hs_bindgen_13abd80fdff110c6_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_before5@
hs_bindgen_13abd80fdff110c6 ::
     BG.CInt
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_13abd80fdff110c6 =
  \x0 ->
    \x1 ->
      hs_bindgen_13abd80fdff110c6_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_13abd80fdff110c6 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_before6@
foreign import ccall unsafe "hs_bindgen_4777fcef57294b3d" hs_bindgen_4777fcef57294b3d_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_before6@
hs_bindgen_4777fcef57294b3d ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_4777fcef57294b3d =
  \x0 ->
    \x1 ->
      hs_bindgen_4777fcef57294b3d_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_4777fcef57294b3d

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_before7@
foreign import ccall unsafe "hs_bindgen_9e06fc613313c597" hs_bindgen_9e06fc613313c597_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_before7@
hs_bindgen_9e06fc613313c597 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_9e06fc613313c597 =
  \x0 ->
    \x1 ->
      hs_bindgen_9e06fc613313c597_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_9e06fc613313c597

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_before8@
foreign import ccall unsafe "hs_bindgen_73f5e08ab7bd794b" hs_bindgen_73f5e08ab7bd794b_base ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_before8@
hs_bindgen_73f5e08ab7bd794b ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_73f5e08ab7bd794b =
  \x0 ->
    \x1 ->
      hs_bindgen_73f5e08ab7bd794b_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_73f5e08ab7bd794b

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_after1@
foreign import ccall unsafe "hs_bindgen_b14f99125c09c927" hs_bindgen_b14f99125c09c927_base ::
     BG.CInt
  -> BG.CFloat
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_after1@
hs_bindgen_b14f99125c09c927 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_b14f99125c09c927 =
  \x0 ->
    \x1 ->
      hs_bindgen_b14f99125c09c927_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_b14f99125c09c927

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_after2@
foreign import ccall unsafe "hs_bindgen_556866f9af5fde43" hs_bindgen_556866f9af5fde43_base ::
     BG.CInt
  -> BG.CDouble
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_after2@
hs_bindgen_556866f9af5fde43 ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_556866f9af5fde43 =
  \x0 ->
    \x1 ->
      hs_bindgen_556866f9af5fde43_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_556866f9af5fde43

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_after3@
foreign import ccall unsafe "hs_bindgen_5c48020adde0c968" hs_bindgen_5c48020adde0c968_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_after3@
hs_bindgen_5c48020adde0c968 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_5c48020adde0c968 =
  \x0 ->
    \x1 ->
      hs_bindgen_5c48020adde0c968_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_5c48020adde0c968

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_after4@
foreign import ccall unsafe "hs_bindgen_daa7275a71a9097b" hs_bindgen_daa7275a71a9097b_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_after4@
hs_bindgen_daa7275a71a9097b ::
     BG.CInt
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_daa7275a71a9097b =
  \x0 ->
    \x1 ->
      hs_bindgen_daa7275a71a9097b_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_daa7275a71a9097b arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_after5@
foreign import ccall unsafe "hs_bindgen_5f9bf73a7cf0466c" hs_bindgen_5f9bf73a7cf0466c_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_after5@
hs_bindgen_5f9bf73a7cf0466c ::
     BG.CInt
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_5f9bf73a7cf0466c =
  \x0 ->
    \x1 ->
      hs_bindgen_5f9bf73a7cf0466c_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_5f9bf73a7cf0466c arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_after6@
foreign import ccall unsafe "hs_bindgen_05a68f095a849513" hs_bindgen_05a68f095a849513_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_after6@
hs_bindgen_05a68f095a849513 ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_05a68f095a849513 =
  \x0 ->
    \x1 ->
      hs_bindgen_05a68f095a849513_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_05a68f095a849513

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_after7@
foreign import ccall unsafe "hs_bindgen_eab87bfb2631dc6b" hs_bindgen_eab87bfb2631dc6b_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_after7@
hs_bindgen_eab87bfb2631dc6b ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_eab87bfb2631dc6b =
  \x0 ->
    \x1 ->
      hs_bindgen_eab87bfb2631dc6b_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_eab87bfb2631dc6b

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_after8@
foreign import ccall unsafe "hs_bindgen_3ed2b97d934ca93c" hs_bindgen_3ed2b97d934ca93c_base ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_withoutSign_after8@
hs_bindgen_3ed2b97d934ca93c ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_3ed2b97d934ca93c =
  \x0 ->
    \x1 ->
      hs_bindgen_3ed2b97d934ca93c_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_3ed2b97d934ca93c

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_args1@
foreign import ccall unsafe "hs_bindgen_630176bbd5e66084" hs_bindgen_630176bbd5e66084_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_args1@
hs_bindgen_630176bbd5e66084 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_630176bbd5e66084 =
  \x0 ->
    \x1 ->
      hs_bindgen_630176bbd5e66084_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args1 = hs_bindgen_630176bbd5e66084

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_args2@
foreign import ccall unsafe "hs_bindgen_4dd601ef369e68cc" hs_bindgen_4dd601ef369e68cc_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_args2@
hs_bindgen_4dd601ef369e68cc ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_4dd601ef369e68cc =
  \x0 ->
    \x1 ->
      hs_bindgen_4dd601ef369e68cc_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args2 = hs_bindgen_4dd601ef369e68cc

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_args3@
foreign import ccall unsafe "hs_bindgen_924f6c70a7144c60" hs_bindgen_924f6c70a7144c60_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_args3@
hs_bindgen_924f6c70a7144c60 ::
     BG.CInt
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_924f6c70a7144c60 =
  \x0 ->
    \x1 ->
      hs_bindgen_924f6c70a7144c60_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args3 = hs_bindgen_924f6c70a7144c60

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_args4@
foreign import ccall unsafe "hs_bindgen_1186886634b954ed" hs_bindgen_1186886634b954ed_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_args4@
hs_bindgen_1186886634b954ed ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_1186886634b954ed =
  \x0 ->
    \x1 ->
      hs_bindgen_1186886634b954ed_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args4 = hs_bindgen_1186886634b954ed

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_args5@
foreign import ccall unsafe "hs_bindgen_2e1883ef3cecf662" hs_bindgen_2e1883ef3cecf662_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_args5@
hs_bindgen_2e1883ef3cecf662 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_2e1883ef3cecf662 =
  \x0 ->
    \x1 ->
      hs_bindgen_2e1883ef3cecf662_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args5 = hs_bindgen_2e1883ef3cecf662

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_ret1@
foreign import ccall unsafe "hs_bindgen_50d86134f90cf6fd" hs_bindgen_50d86134f90cf6fd_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_ret1@
hs_bindgen_50d86134f90cf6fd ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_50d86134f90cf6fd =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_50d86134f90cf6fd_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h 212:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret1 = hs_bindgen_50d86134f90cf6fd

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_ret2@
foreign import ccall unsafe "hs_bindgen_768447ff73613740" hs_bindgen_768447ff73613740_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_ret2@
hs_bindgen_768447ff73613740 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_768447ff73613740 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_768447ff73613740_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h 213:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret2 = hs_bindgen_768447ff73613740

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_ret3@
foreign import ccall unsafe "hs_bindgen_4d20943b7ef372c8" hs_bindgen_4d20943b7ef372c8_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_ret3@
hs_bindgen_4d20943b7ef372c8 ::
     BG.CInt
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_4d20943b7ef372c8 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_4d20943b7ef372c8_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h 214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
const_pointers_ret3 = hs_bindgen_4d20943b7ef372c8

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_ret4@
foreign import ccall unsafe "hs_bindgen_c5b4ac7cc309ef4b" hs_bindgen_c5b4ac7cc309ef4b_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_ret4@
hs_bindgen_c5b4ac7cc309ef4b ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_c5b4ac7cc309ef4b =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_c5b4ac7cc309ef4b_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h 215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret4 = hs_bindgen_c5b4ac7cc309ef4b

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_ret5@
foreign import ccall unsafe "hs_bindgen_f9413491848fbfe5" hs_bindgen_f9413491848fbfe5_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_pointers_ret5@
hs_bindgen_f9413491848fbfe5 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_f9413491848fbfe5 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_f9413491848fbfe5_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h 216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret5 = hs_bindgen_f9413491848fbfe5

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_array_elem1@
foreign import ccall unsafe "hs_bindgen_35b43343e1505a9f" hs_bindgen_35b43343e1505a9f_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_array_elem1@
hs_bindgen_35b43343e1505a9f ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt))
  -> IO ()
hs_bindgen_35b43343e1505a9f =
  \x0 ->
    hs_bindgen_35b43343e1505a9f_base (BG.toFFIType x0)

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h 244:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem1 = hs_bindgen_35b43343e1505a9f

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_array_elem2@
foreign import ccall unsafe "hs_bindgen_33fb0e4285c5cfeb" hs_bindgen_33fb0e4285c5cfeb_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_array_elem2@
hs_bindgen_33fb0e4285c5cfeb ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt)))
  -> IO ()
hs_bindgen_33fb0e4285c5cfeb =
  \x0 ->
    hs_bindgen_33fb0e4285c5cfeb_base (BG.toFFIType x0)

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h 245:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem2 = hs_bindgen_33fb0e4285c5cfeb

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_array_elem3@
foreign import ccall unsafe "hs_bindgen_4b391cb720429a17" hs_bindgen_4b391cb720429a17_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_const_array_elem3@
hs_bindgen_4b391cb720429a17 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_4b391cb720429a17 =
  \x0 ->
    hs_bindgen_4b391cb720429a17_base (BG.toFFIType x0)

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h 246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem3 = hs_bindgen_4b391cb720429a17

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_noParams1@
foreign import ccall unsafe "hs_bindgen_35ba30a3c17deabd" hs_bindgen_35ba30a3c17deabd_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_noParams1@
hs_bindgen_35ba30a3c17deabd :: IO BG.CInt
hs_bindgen_35ba30a3c17deabd =
  fmap BG.fromFFIType hs_bindgen_35ba30a3c17deabd_base

{-| Other examples we reparsed /incorrectly/ before language-c

    __C declaration:__ @noParams1@

    __defined at:__ @macros\/reparse.h 254:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams1 :: IO BG.CInt
noParams1 = hs_bindgen_35ba30a3c17deabd

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_noParams2@
foreign import ccall unsafe "hs_bindgen_2ceca322647596fe" hs_bindgen_2ceca322647596fe_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_noParams2@
hs_bindgen_2ceca322647596fe :: IO BG.CInt
hs_bindgen_2ceca322647596fe =
  fmap BG.fromFFIType hs_bindgen_2ceca322647596fe_base

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h 255:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2 :: IO BG.CInt
noParams2 = hs_bindgen_2ceca322647596fe

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_noParams3@
foreign import ccall unsafe "hs_bindgen_2cdc3cb3c34d668f" hs_bindgen_2cdc3cb3c34d668f_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_noParams3@
hs_bindgen_2cdc3cb3c34d668f ::
     BG.CInt
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_2cdc3cb3c34d668f =
  \x0 ->
    \x1 ->
      hs_bindgen_2cdc3cb3c34d668f_base (BG.toFFIType x0) (BG.toFFIType x1)

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
noParams3 = hs_bindgen_2cdc3cb3c34d668f

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret1@
foreign import ccall unsafe "hs_bindgen_66bc29740adfcc7a" hs_bindgen_66bc29740adfcc7a_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret1@
hs_bindgen_66bc29740adfcc7a ::
     BG.CInt
  -> IO (BG.FunPtr (IO ()))
hs_bindgen_66bc29740adfcc7a =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_66bc29740adfcc7a_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h 260:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO ()))
funptr_ret1 = hs_bindgen_66bc29740adfcc7a

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret2@
foreign import ccall unsafe "hs_bindgen_c46d39805a107470" hs_bindgen_c46d39805a107470_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret2@
hs_bindgen_c46d39805a107470 ::
     BG.CInt
  -> IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_c46d39805a107470 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_c46d39805a107470_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h 261:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO BG.CInt))
funptr_ret2 = hs_bindgen_c46d39805a107470

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret3@
foreign import ccall unsafe "hs_bindgen_3b9e3f14dfcdf935" hs_bindgen_3b9e3f14dfcdf935_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret3@
hs_bindgen_3b9e3f14dfcdf935 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_3b9e3f14dfcdf935 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_3b9e3f14dfcdf935_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h 262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
funptr_ret3 = hs_bindgen_3b9e3f14dfcdf935

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret4@
foreign import ccall unsafe "hs_bindgen_51cf7b4c9987a2d6" hs_bindgen_51cf7b4c9987a2d6_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret4@
hs_bindgen_51cf7b4c9987a2d6 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
hs_bindgen_51cf7b4c9987a2d6 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_51cf7b4c9987a2d6_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h 263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
funptr_ret4 = hs_bindgen_51cf7b4c9987a2d6

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret5@
foreign import ccall unsafe "hs_bindgen_13b9d73bf1eacbea" hs_bindgen_13b9d73bf1eacbea_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret5@
hs_bindgen_13b9d73bf1eacbea ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_13b9d73bf1eacbea =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_13b9d73bf1eacbea_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h 267:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret5 = hs_bindgen_13b9d73bf1eacbea

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret6@
foreign import ccall unsafe "hs_bindgen_16c8d185cd1b6b6a" hs_bindgen_16c8d185cd1b6b6a_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret6@
hs_bindgen_16c8d185cd1b6b6a ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_16c8d185cd1b6b6a =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_16c8d185cd1b6b6a_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h 268:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret6 = hs_bindgen_16c8d185cd1b6b6a

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret7@
foreign import ccall unsafe "hs_bindgen_877736bfb7172200" hs_bindgen_877736bfb7172200_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret7@
hs_bindgen_877736bfb7172200 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_877736bfb7172200 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_877736bfb7172200_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h 269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret7 = hs_bindgen_877736bfb7172200

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret8@
foreign import ccall unsafe "hs_bindgen_9a92afad45659474" hs_bindgen_9a92afad45659474_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret8@
hs_bindgen_9a92afad45659474 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_9a92afad45659474 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_9a92afad45659474_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h 270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret8 = hs_bindgen_9a92afad45659474

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret9@
foreign import ccall unsafe "hs_bindgen_c05706d70431baec" hs_bindgen_c05706d70431baec_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret9@
hs_bindgen_c05706d70431baec ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_c05706d70431baec =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_c05706d70431baec_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h 271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret9 = hs_bindgen_c05706d70431baec

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret10@
foreign import ccall unsafe "hs_bindgen_2ba78e0232c0eba0" hs_bindgen_2ba78e0232c0eba0_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Unsafe_funptr_ret10@
hs_bindgen_2ba78e0232c0eba0 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_2ba78e0232c0eba0 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_2ba78e0232c0eba0_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h 272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret10 = hs_bindgen_2ba78e0232c0eba0
