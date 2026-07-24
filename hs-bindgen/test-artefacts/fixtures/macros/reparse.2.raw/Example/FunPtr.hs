{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.args_char1
    , Example.FunPtr.args_char2
    , Example.FunPtr.args_char3
    , Example.FunPtr.args_short1
    , Example.FunPtr.args_short2
    , Example.FunPtr.args_short3
    , Example.FunPtr.args_int1
    , Example.FunPtr.args_int2
    , Example.FunPtr.args_int3
    , Example.FunPtr.args_long1
    , Example.FunPtr.args_long2
    , Example.FunPtr.args_long3
    , Example.FunPtr.args_float
    , Example.FunPtr.args_double
    , Example.FunPtr.args_bool1
    , Example.FunPtr.args_struct
    , Example.FunPtr.args_union
    , Example.FunPtr.args_enum
    , Example.FunPtr.args_pointer1
    , Example.FunPtr.args_pointer2
    , Example.FunPtr.args_pointer3
    , Example.FunPtr.ret_A
    , Example.FunPtr.ret_char1
    , Example.FunPtr.ret_char2
    , Example.FunPtr.ret_char3
    , Example.FunPtr.ret_short1
    , Example.FunPtr.ret_short2
    , Example.FunPtr.ret_short3
    , Example.FunPtr.ret_int1
    , Example.FunPtr.ret_int2
    , Example.FunPtr.ret_int3
    , Example.FunPtr.ret_long1
    , Example.FunPtr.ret_long2
    , Example.FunPtr.ret_long3
    , Example.FunPtr.ret_float
    , Example.FunPtr.ret_double
    , Example.FunPtr.ret_bool1
    , Example.FunPtr.ret_struct
    , Example.FunPtr.ret_union
    , Example.FunPtr.ret_enum
    , Example.FunPtr.ret_pointer1
    , Example.FunPtr.ret_pointer2
    , Example.FunPtr.ret_pointer3
    , Example.FunPtr.body1
    , Example.FunPtr.body2
    , Example.FunPtr.args_complex_float
    , Example.FunPtr.args_complex_double
    , Example.FunPtr.ret_complex_float
    , Example.FunPtr.ret_complex_double
    , Example.FunPtr.bespoke_args1
    , Example.FunPtr.bespoke_args2
    , Example.FunPtr.bespoke_ret1
    , Example.FunPtr.bespoke_ret2
    , Example.FunPtr.arr_args1
    , Example.FunPtr.arr_args2
    , Example.FunPtr.arr_args3
    , Example.FunPtr.arr_args4
    , Example.FunPtr.funptr_args1
    , Example.FunPtr.funptr_args2
    , Example.FunPtr.funptr_args3
    , Example.FunPtr.funptr_args4
    , Example.FunPtr.funptr_args5
    , Example.FunPtr.comments1
    , Example.FunPtr.const_prim_before1
    , Example.FunPtr.const_prim_before2
    , Example.FunPtr.const_prim_before3
    , Example.FunPtr.const_prim_after1
    , Example.FunPtr.const_prim_after2
    , Example.FunPtr.const_prim_after3
    , Example.FunPtr.const_withoutSign_before1
    , Example.FunPtr.const_withoutSign_before2
    , Example.FunPtr.const_withoutSign_before3
    , Example.FunPtr.const_withoutSign_before4
    , Example.FunPtr.const_withoutSign_before5
    , Example.FunPtr.const_withoutSign_before6
    , Example.FunPtr.const_withoutSign_before7
    , Example.FunPtr.const_withoutSign_before8
    , Example.FunPtr.const_withoutSign_after1
    , Example.FunPtr.const_withoutSign_after2
    , Example.FunPtr.const_withoutSign_after3
    , Example.FunPtr.const_withoutSign_after4
    , Example.FunPtr.const_withoutSign_after5
    , Example.FunPtr.const_withoutSign_after6
    , Example.FunPtr.const_withoutSign_after7
    , Example.FunPtr.const_withoutSign_after8
    , Example.FunPtr.const_pointers_args1
    , Example.FunPtr.const_pointers_args2
    , Example.FunPtr.const_pointers_args3
    , Example.FunPtr.const_pointers_args4
    , Example.FunPtr.const_pointers_args5
    , Example.FunPtr.const_pointers_ret1
    , Example.FunPtr.const_pointers_ret2
    , Example.FunPtr.const_pointers_ret3
    , Example.FunPtr.const_pointers_ret4
    , Example.FunPtr.const_pointers_ret5
    , Example.FunPtr.const_array_elem1
    , Example.FunPtr.const_array_elem2
    , Example.FunPtr.const_array_elem3
    , Example.FunPtr.noParams1
    , Example.FunPtr.noParams2
    , Example.FunPtr.noParams3
    , Example.FunPtr.funptr_ret1
    , Example.FunPtr.funptr_ret2
    , Example.FunPtr.funptr_ret3
    , Example.FunPtr.funptr_ret4
    , Example.FunPtr.funptr_ret5
    , Example.FunPtr.funptr_ret6
    , Example.FunPtr.funptr_ret7
    , Example.FunPtr.funptr_ret8
    , Example.FunPtr.funptr_ret9
    , Example.FunPtr.funptr_ret10
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
  , "/* test_macrosreparse_2_raw_Example_get_args_char1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_30055b4cfdb7a3e2 (void)) ("
  , "  signed int arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &args_char1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_char2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_44f283f141f81fc4 (void)) ("
  , "  signed int arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  return &args_char2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_char3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_27b5bf56bbc8f080 (void)) ("
  , "  signed int arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  return &args_char3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_short1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_985bed578f09aef8 (void)) ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_short2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4213a6df8d0c03f1 (void)) ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_short3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_82e313aaa763196f (void)) ("
  , "  signed int arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  return &args_short3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_int1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_605e1f38f823a881 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_int2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3b8afc742e92814e (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_int3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_bf10dc64323b0995 (void)) ("
  , "  signed int arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  return &args_int3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_long1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_dae65ec08c1d514e (void)) ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_long2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e83db9fe9e4243df (void)) ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_long3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_42d83281e20c0186 (void)) ("
  , "  signed int arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  return &args_long3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_float */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3b0ba5af6ce01aed (void)) ("
  , "  signed int arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  return &args_float;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_double */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d14a589fdc8b9407 (void)) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &args_double;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_bool1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a829f5810873021d (void)) ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &args_bool1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_struct */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_243cc2e9d0181102 (void)) ("
  , "  signed int arg1,"
  , "  struct some_struct arg2"
  , ")"
  , "{"
  , "  return &args_struct;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_union */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b3e0ceada39fdc01 (void)) ("
  , "  signed int arg1,"
  , "  union some_union arg2"
  , ")"
  , "{"
  , "  return &args_union;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_enum */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d11159ff99847117 (void)) ("
  , "  signed int arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  return &args_enum;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_pointer1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f3c28cbcc1bc9d17 (void)) ("
  , "  signed int arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return &args_pointer1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_pointer2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_190bfc6a0f4bff4f (void)) ("
  , "  signed int arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  return &args_pointer2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_pointer3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_26b1c20d174e45d5 (void)) ("
  , "  signed int arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  return &args_pointer3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_A */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_8760cbd218bc55d1 (void)) (void)"
  , "{"
  , "  return &ret_A;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_char1 */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_e910062cffeff2d2 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_char1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_char2 */"
  , "__attribute__ ((const))"
  , "signed char (*hs_bindgen_d9218f2ac4ee5a50 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_char2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_char3 */"
  , "__attribute__ ((const))"
  , "unsigned char (*hs_bindgen_7a2d6ef06c7567a1 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_char3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_short1 */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_c715ac00f4ca7f36 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_short1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_short2 */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_880f34b16d908c1f (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_short2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_short3 */"
  , "__attribute__ ((const))"
  , "unsigned short (*hs_bindgen_ea8c8e8164273c1a (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_short3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_int1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_97a22a914dd2414c (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_int1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_int2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_77a2b7921cb7566a (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_int2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_int3 */"
  , "__attribute__ ((const))"
  , "unsigned int (*hs_bindgen_0dc8eed9c02e16ff (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_int3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_long1 */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_8a42512fc931fa53 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_long1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_long2 */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_8293ce652f4875b7 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_long2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_long3 */"
  , "__attribute__ ((const))"
  , "unsigned long (*hs_bindgen_1ca2d3b54e331b1f (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_long3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_float */"
  , "__attribute__ ((const))"
  , "float (*hs_bindgen_5362002da5a787ba (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_float;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_double */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_0ae8c6485107bf6f (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_double;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_bool1 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_04d8f1fc8df996ba (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_bool1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_struct */"
  , "__attribute__ ((const))"
  , "struct some_struct (*hs_bindgen_b7cd9769c7dac7b8 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_struct;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_union */"
  , "__attribute__ ((const))"
  , "union some_union (*hs_bindgen_b4afde7383e5aff4 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_union;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_enum */"
  , "__attribute__ ((const))"
  , "enum some_enum (*hs_bindgen_239f0b420c073c18 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_enum;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_pointer1 */"
  , "__attribute__ ((const))"
  , "signed int *(*hs_bindgen_56eab0f8d0bdf79f (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_pointer1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_pointer2 */"
  , "__attribute__ ((const))"
  , "signed int **(*hs_bindgen_4c952a3d40721870 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_pointer2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_pointer3 */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_ccb0b4753d3e2490 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_pointer3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_body1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_442c91e88aec2368 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &body1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_body2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_cfbec40e47da01c3 (void)) (void)"
  , "{"
  , "  return &body2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_complex_float */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4319227a8aaa0b24 (void)) ("
  , "  signed int arg1,"
  , "  float _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_float;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_args_complex_double */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9620841c151e1094 (void)) ("
  , "  signed int arg1,"
  , "  double _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_double;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_complex_float */"
  , "__attribute__ ((const))"
  , "float _Complex (*hs_bindgen_aa2302691b1dc578 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_complex_float;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_ret_complex_double */"
  , "__attribute__ ((const))"
  , "double _Complex (*hs_bindgen_27f2ccb0e11ce3d4 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_complex_double;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_bespoke_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5f39596c5788b2cf (void)) ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &bespoke_args1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_bespoke_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6fc3008f0e66bd81 (void)) ("
  , "  signed int arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &bespoke_args2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_bespoke_ret1 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_02e119349dfc0dc6 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_bespoke_ret2 */"
  , "__attribute__ ((const))"
  , "size_t (*hs_bindgen_447b3d2bccfb7ae8 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_arr_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_869038d8098a2bf2 (void)) ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return &arr_args1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_arr_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d97856ae41779071 (void)) ("
  , "  signed int **arg1"
  , ")"
  , "{"
  , "  return &arr_args2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_arr_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e5fcdc276e072021 (void)) ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return &arr_args3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_arr_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5960a1a097c40b0b (void)) ("
  , "  signed int **arg1"
  , ")"
  , "{"
  , "  return &arr_args4;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_funptr_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6e1c21a20bf17727 (void)) ("
  , "  signed int arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_funptr_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_316b1d39ef1c99c8 (void)) ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_funptr_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3b587acc49334022 (void)) ("
  , "  signed int arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_funptr_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_880f8ecb89c20f54 (void)) ("
  , "  signed int arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args4;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_funptr_args5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_25220e78ceba78f8 (void)) ("
  , "  signed int arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args5;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_comments1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6cef08508aed5305 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &comments1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_prim_before1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_71a50f57cd1216e6 (void)) ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_prim_before2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5233d49c08278554 (void)) ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_prim_before3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b47263d563e229b2 (void)) ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_prim_after1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9b538efe2347a74e (void)) ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_prim_after2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e9bd30449260d07b (void)) ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_prim_after3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f1e92cd065630ccb (void)) ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_withoutSign_before1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_aea63f55c25fa26d (void)) ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_withoutSign_before2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1a34eb11a6ccfd86 (void)) ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_withoutSign_before3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d9af8e3a8991a2e4 (void)) ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_withoutSign_before4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_558f393b1eddf40d (void)) ("
  , "  signed int arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before4;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_withoutSign_before5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e62c93c0973b2781 (void)) ("
  , "  signed int arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before5;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_withoutSign_before6 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e56acb4f339a362d (void)) ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before6;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_withoutSign_before7 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c298f88f131c980b (void)) ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before7;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_withoutSign_before8 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_38d5cf2b5d94860c (void)) ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before8;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_withoutSign_after1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_362151a5ba0ce905 (void)) ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_withoutSign_after2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_071d6b5b46afca86 (void)) ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_withoutSign_after3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cb70cd180a753e3a (void)) ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_withoutSign_after4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_17e67974fc77dde6 (void)) ("
  , "  signed int arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after4;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_withoutSign_after5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_aea1ac49e3a717ae (void)) ("
  , "  signed int arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after5;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_withoutSign_after6 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_89ad81e6dc3a6ed4 (void)) ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after6;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_withoutSign_after7 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_29b119a94317aae6 (void)) ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after7;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_withoutSign_after8 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_be8db07cc695795d (void)) ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after8;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_pointers_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3f4562f74152c205 (void)) ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_pointers_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_87278ccb01e7fee0 (void)) ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_pointers_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1e25b241655c3ebb (void)) ("
  , "  signed int arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_pointers_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fa94aed37836859b (void)) ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args4;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_pointers_args5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_800407f1e67deddb (void)) ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args5;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_pointers_ret1 */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_91e3fb45ca972f9d (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_pointers_ret2 */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_ce269be70accca82 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_pointers_ret3 */"
  , "__attribute__ ((const))"
  , "signed int *const (*hs_bindgen_f0f2ae46270c9785 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_pointers_ret4 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_4ecbc6cf48fbe0c3 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret4;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_pointers_ret5 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_c0b16cd44f63f52f (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret5;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_array_elem1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_db9eb973118a6fbf (void)) ("
  , "  signed int const *arg1"
  , ")"
  , "{"
  , "  return &const_array_elem1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_array_elem2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9288cd4171172005 (void)) ("
  , "  signed int const **arg1"
  , ")"
  , "{"
  , "  return &const_array_elem2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const_array_elem3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c20403bc692b1bde (void)) ("
  , "  signed int *const *arg1"
  , ")"
  , "{"
  , "  return &const_array_elem3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_noParams1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_5ca24e7f6ac8cff0 (void)) (void)"
  , "{"
  , "  return &noParams1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_noParams2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_6f664f6fdcbced27 (void)) (void)"
  , "{"
  , "  return &noParams2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_noParams3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1067f730bd5b4c43 (void)) ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &noParams3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_funptr_ret1 */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_6b5258a8560bc11c (void)) ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_funptr_ret2 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_723b02be7cb23263 (void)) ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_funptr_ret3 */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_7c4d8fc844c6f97b (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &funptr_ret3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_funptr_ret4 */"
  , "__attribute__ ((const))"
  , "char (*(*hs_bindgen_25b578cce360cf12 (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret4;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_funptr_ret5 */"
  , "__attribute__ ((const))"
  , "signed int *(*(*hs_bindgen_5e58691a68a915de (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret5;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_funptr_ret6 */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_0c36a271f93a9dff (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret6;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_funptr_ret7 */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_67439554fe1e797d (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret7;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_funptr_ret8 */"
  , "__attribute__ ((const))"
  , "signed int *const (*(*hs_bindgen_870a61b2b66eecf3 (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret8;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_funptr_ret9 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_e7920c47095333a3 (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret9;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_funptr_ret10 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_aa1eab9b222fa4b8 (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret10;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_char1@
foreign import ccall unsafe "hs_bindgen_30055b4cfdb7a3e2" hs_bindgen_30055b4cfdb7a3e2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_char1@
hs_bindgen_30055b4cfdb7a3e2 :: IO (BG.FunPtr (BG.CInt -> BG.CChar -> IO ()))
hs_bindgen_30055b4cfdb7a3e2 =
  BG.fromFFIType hs_bindgen_30055b4cfdb7a3e2_base

{-# NOINLINE args_char1 #-}
{-| Function declarations

    __C declaration:__ @args_char1@

    __defined at:__ @macros\/reparse.h 17:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char1 :: BG.FunPtr (BG.CInt -> BG.CChar -> IO ())
args_char1 =
  BG.unsafePerformIO hs_bindgen_30055b4cfdb7a3e2

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_char2@
foreign import ccall unsafe "hs_bindgen_44f283f141f81fc4" hs_bindgen_44f283f141f81fc4_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_char2@
hs_bindgen_44f283f141f81fc4 :: IO (BG.FunPtr (BG.CInt -> BG.CSChar -> IO ()))
hs_bindgen_44f283f141f81fc4 =
  BG.fromFFIType hs_bindgen_44f283f141f81fc4_base

{-# NOINLINE args_char2 #-}
{-| __C declaration:__ @args_char2@

    __defined at:__ @macros\/reparse.h 18:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char2 :: BG.FunPtr (BG.CInt -> BG.CSChar -> IO ())
args_char2 =
  BG.unsafePerformIO hs_bindgen_44f283f141f81fc4

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_char3@
foreign import ccall unsafe "hs_bindgen_27b5bf56bbc8f080" hs_bindgen_27b5bf56bbc8f080_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_char3@
hs_bindgen_27b5bf56bbc8f080 :: IO (BG.FunPtr (BG.CInt -> BG.CUChar -> IO ()))
hs_bindgen_27b5bf56bbc8f080 =
  BG.fromFFIType hs_bindgen_27b5bf56bbc8f080_base

{-# NOINLINE args_char3 #-}
{-| __C declaration:__ @args_char3@

    __defined at:__ @macros\/reparse.h 19:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char3 :: BG.FunPtr (BG.CInt -> BG.CUChar -> IO ())
args_char3 =
  BG.unsafePerformIO hs_bindgen_27b5bf56bbc8f080

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_short1@
foreign import ccall unsafe "hs_bindgen_985bed578f09aef8" hs_bindgen_985bed578f09aef8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_short1@
hs_bindgen_985bed578f09aef8 :: IO (BG.FunPtr (BG.CInt -> BG.CShort -> IO ()))
hs_bindgen_985bed578f09aef8 =
  BG.fromFFIType hs_bindgen_985bed578f09aef8_base

{-# NOINLINE args_short1 #-}
{-| __C declaration:__ @args_short1@

    __defined at:__ @macros\/reparse.h 21:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short1 :: BG.FunPtr (BG.CInt -> BG.CShort -> IO ())
args_short1 =
  BG.unsafePerformIO hs_bindgen_985bed578f09aef8

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_short2@
foreign import ccall unsafe "hs_bindgen_4213a6df8d0c03f1" hs_bindgen_4213a6df8d0c03f1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_short2@
hs_bindgen_4213a6df8d0c03f1 :: IO (BG.FunPtr (BG.CInt -> BG.CShort -> IO ()))
hs_bindgen_4213a6df8d0c03f1 =
  BG.fromFFIType hs_bindgen_4213a6df8d0c03f1_base

{-# NOINLINE args_short2 #-}
{-| __C declaration:__ @args_short2@

    __defined at:__ @macros\/reparse.h 22:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short2 :: BG.FunPtr (BG.CInt -> BG.CShort -> IO ())
args_short2 =
  BG.unsafePerformIO hs_bindgen_4213a6df8d0c03f1

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_short3@
foreign import ccall unsafe "hs_bindgen_82e313aaa763196f" hs_bindgen_82e313aaa763196f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_short3@
hs_bindgen_82e313aaa763196f :: IO (BG.FunPtr (BG.CInt -> BG.CUShort -> IO ()))
hs_bindgen_82e313aaa763196f =
  BG.fromFFIType hs_bindgen_82e313aaa763196f_base

{-# NOINLINE args_short3 #-}
{-| __C declaration:__ @args_short3@

    __defined at:__ @macros\/reparse.h 23:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short3 :: BG.FunPtr (BG.CInt -> BG.CUShort -> IO ())
args_short3 =
  BG.unsafePerformIO hs_bindgen_82e313aaa763196f

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_int1@
foreign import ccall unsafe "hs_bindgen_605e1f38f823a881" hs_bindgen_605e1f38f823a881_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_int1@
hs_bindgen_605e1f38f823a881 :: IO (BG.FunPtr (BG.CInt -> BG.CInt -> IO ()))
hs_bindgen_605e1f38f823a881 =
  BG.fromFFIType hs_bindgen_605e1f38f823a881_base

{-# NOINLINE args_int1 #-}
{-| __C declaration:__ @args_int1@

    __defined at:__ @macros\/reparse.h 25:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int1 :: BG.FunPtr (BG.CInt -> BG.CInt -> IO ())
args_int1 =
  BG.unsafePerformIO hs_bindgen_605e1f38f823a881

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_int2@
foreign import ccall unsafe "hs_bindgen_3b8afc742e92814e" hs_bindgen_3b8afc742e92814e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_int2@
hs_bindgen_3b8afc742e92814e :: IO (BG.FunPtr (BG.CInt -> BG.CInt -> IO ()))
hs_bindgen_3b8afc742e92814e =
  BG.fromFFIType hs_bindgen_3b8afc742e92814e_base

{-# NOINLINE args_int2 #-}
{-| __C declaration:__ @args_int2@

    __defined at:__ @macros\/reparse.h 26:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int2 :: BG.FunPtr (BG.CInt -> BG.CInt -> IO ())
args_int2 =
  BG.unsafePerformIO hs_bindgen_3b8afc742e92814e

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_int3@
foreign import ccall unsafe "hs_bindgen_bf10dc64323b0995" hs_bindgen_bf10dc64323b0995_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_int3@
hs_bindgen_bf10dc64323b0995 :: IO (BG.FunPtr (BG.CInt -> BG.CUInt -> IO ()))
hs_bindgen_bf10dc64323b0995 =
  BG.fromFFIType hs_bindgen_bf10dc64323b0995_base

{-# NOINLINE args_int3 #-}
{-| __C declaration:__ @args_int3@

    __defined at:__ @macros\/reparse.h 27:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int3 :: BG.FunPtr (BG.CInt -> BG.CUInt -> IO ())
args_int3 =
  BG.unsafePerformIO hs_bindgen_bf10dc64323b0995

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_long1@
foreign import ccall unsafe "hs_bindgen_dae65ec08c1d514e" hs_bindgen_dae65ec08c1d514e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_long1@
hs_bindgen_dae65ec08c1d514e :: IO (BG.FunPtr (BG.CInt -> BG.CLong -> IO ()))
hs_bindgen_dae65ec08c1d514e =
  BG.fromFFIType hs_bindgen_dae65ec08c1d514e_base

{-# NOINLINE args_long1 #-}
{-| __C declaration:__ @args_long1@

    __defined at:__ @macros\/reparse.h 29:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long1 :: BG.FunPtr (BG.CInt -> BG.CLong -> IO ())
args_long1 =
  BG.unsafePerformIO hs_bindgen_dae65ec08c1d514e

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_long2@
foreign import ccall unsafe "hs_bindgen_e83db9fe9e4243df" hs_bindgen_e83db9fe9e4243df_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_long2@
hs_bindgen_e83db9fe9e4243df :: IO (BG.FunPtr (BG.CInt -> BG.CLong -> IO ()))
hs_bindgen_e83db9fe9e4243df =
  BG.fromFFIType hs_bindgen_e83db9fe9e4243df_base

{-# NOINLINE args_long2 #-}
{-| __C declaration:__ @args_long2@

    __defined at:__ @macros\/reparse.h 30:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long2 :: BG.FunPtr (BG.CInt -> BG.CLong -> IO ())
args_long2 =
  BG.unsafePerformIO hs_bindgen_e83db9fe9e4243df

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_long3@
foreign import ccall unsafe "hs_bindgen_42d83281e20c0186" hs_bindgen_42d83281e20c0186_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_long3@
hs_bindgen_42d83281e20c0186 :: IO (BG.FunPtr (BG.CInt -> BG.CULong -> IO ()))
hs_bindgen_42d83281e20c0186 =
  BG.fromFFIType hs_bindgen_42d83281e20c0186_base

{-# NOINLINE args_long3 #-}
{-| __C declaration:__ @args_long3@

    __defined at:__ @macros\/reparse.h 31:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long3 :: BG.FunPtr (BG.CInt -> BG.CULong -> IO ())
args_long3 =
  BG.unsafePerformIO hs_bindgen_42d83281e20c0186

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_float@
foreign import ccall unsafe "hs_bindgen_3b0ba5af6ce01aed" hs_bindgen_3b0ba5af6ce01aed_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_float@
hs_bindgen_3b0ba5af6ce01aed :: IO (BG.FunPtr (BG.CInt -> BG.CFloat -> IO ()))
hs_bindgen_3b0ba5af6ce01aed =
  BG.fromFFIType hs_bindgen_3b0ba5af6ce01aed_base

{-# NOINLINE args_float #-}
{-| __C declaration:__ @args_float@

    __defined at:__ @macros\/reparse.h 33:6@

    __exported by:__ @macros\/reparse.h@
-}
args_float :: BG.FunPtr (BG.CInt -> BG.CFloat -> IO ())
args_float =
  BG.unsafePerformIO hs_bindgen_3b0ba5af6ce01aed

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_double@
foreign import ccall unsafe "hs_bindgen_d14a589fdc8b9407" hs_bindgen_d14a589fdc8b9407_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_double@
hs_bindgen_d14a589fdc8b9407 :: IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO ()))
hs_bindgen_d14a589fdc8b9407 =
  BG.fromFFIType hs_bindgen_d14a589fdc8b9407_base

{-# NOINLINE args_double #-}
{-| __C declaration:__ @args_double@

    __defined at:__ @macros\/reparse.h 34:6@

    __exported by:__ @macros\/reparse.h@
-}
args_double :: BG.FunPtr (BG.CInt -> BG.CDouble -> IO ())
args_double =
  BG.unsafePerformIO hs_bindgen_d14a589fdc8b9407

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_bool1@
foreign import ccall unsafe "hs_bindgen_a829f5810873021d" hs_bindgen_a829f5810873021d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_bool1@
hs_bindgen_a829f5810873021d :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_a829f5810873021d =
  BG.fromFFIType hs_bindgen_a829f5810873021d_base

{-# NOINLINE args_bool1 #-}
{-| __C declaration:__ @args_bool1@

    __defined at:__ @macros\/reparse.h 35:6@

    __exported by:__ @macros\/reparse.h@
-}
args_bool1 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
args_bool1 =
  BG.unsafePerformIO hs_bindgen_a829f5810873021d

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_struct@
foreign import ccall unsafe "hs_bindgen_243cc2e9d0181102" hs_bindgen_243cc2e9d0181102_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_struct@
hs_bindgen_243cc2e9d0181102 :: IO (BG.FunPtr (BG.CInt -> Some_struct -> IO ()))
hs_bindgen_243cc2e9d0181102 =
  BG.fromFFIType hs_bindgen_243cc2e9d0181102_base

{-# NOINLINE args_struct #-}
{-| __C declaration:__ @args_struct@

    __defined at:__ @macros\/reparse.h 37:6@

    __exported by:__ @macros\/reparse.h@
-}
args_struct :: BG.FunPtr (BG.CInt -> Some_struct -> IO ())
args_struct =
  BG.unsafePerformIO hs_bindgen_243cc2e9d0181102

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_union@
foreign import ccall unsafe "hs_bindgen_b3e0ceada39fdc01" hs_bindgen_b3e0ceada39fdc01_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_union@
hs_bindgen_b3e0ceada39fdc01 :: IO (BG.FunPtr (BG.CInt -> Some_union -> IO ()))
hs_bindgen_b3e0ceada39fdc01 =
  BG.fromFFIType hs_bindgen_b3e0ceada39fdc01_base

{-# NOINLINE args_union #-}
{-| __C declaration:__ @args_union@

    __defined at:__ @macros\/reparse.h 38:6@

    __exported by:__ @macros\/reparse.h@
-}
args_union :: BG.FunPtr (BG.CInt -> Some_union -> IO ())
args_union =
  BG.unsafePerformIO hs_bindgen_b3e0ceada39fdc01

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_enum@
foreign import ccall unsafe "hs_bindgen_d11159ff99847117" hs_bindgen_d11159ff99847117_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_enum@
hs_bindgen_d11159ff99847117 :: IO (BG.FunPtr (BG.CInt -> Some_enum -> IO ()))
hs_bindgen_d11159ff99847117 =
  BG.fromFFIType hs_bindgen_d11159ff99847117_base

{-# NOINLINE args_enum #-}
{-| __C declaration:__ @args_enum@

    __defined at:__ @macros\/reparse.h 39:6@

    __exported by:__ @macros\/reparse.h@
-}
args_enum :: BG.FunPtr (BG.CInt -> Some_enum -> IO ())
args_enum =
  BG.unsafePerformIO hs_bindgen_d11159ff99847117

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_pointer1@
foreign import ccall unsafe "hs_bindgen_f3c28cbcc1bc9d17" hs_bindgen_f3c28cbcc1bc9d17_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_pointer1@
hs_bindgen_f3c28cbcc1bc9d17 :: IO (BG.FunPtr (BG.CInt -> BG.Ptr BG.CInt -> IO ()))
hs_bindgen_f3c28cbcc1bc9d17 =
  BG.fromFFIType hs_bindgen_f3c28cbcc1bc9d17_base

{-# NOINLINE args_pointer1 #-}
{-| __C declaration:__ @args_pointer1@

    __defined at:__ @macros\/reparse.h 41:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer1 :: BG.FunPtr (BG.CInt -> BG.Ptr BG.CInt -> IO ())
args_pointer1 =
  BG.unsafePerformIO hs_bindgen_f3c28cbcc1bc9d17

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_pointer2@
foreign import ccall unsafe "hs_bindgen_190bfc6a0f4bff4f" hs_bindgen_190bfc6a0f4bff4f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_pointer2@
hs_bindgen_190bfc6a0f4bff4f :: IO (BG.FunPtr (BG.CInt -> BG.Ptr (BG.Ptr BG.CInt) -> IO ()))
hs_bindgen_190bfc6a0f4bff4f =
  BG.fromFFIType hs_bindgen_190bfc6a0f4bff4f_base

{-# NOINLINE args_pointer2 #-}
{-| __C declaration:__ @args_pointer2@

    __defined at:__ @macros\/reparse.h 42:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer2 :: BG.FunPtr (BG.CInt -> BG.Ptr (BG.Ptr BG.CInt) -> IO ())
args_pointer2 =
  BG.unsafePerformIO hs_bindgen_190bfc6a0f4bff4f

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_pointer3@
foreign import ccall unsafe "hs_bindgen_26b1c20d174e45d5" hs_bindgen_26b1c20d174e45d5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_pointer3@
hs_bindgen_26b1c20d174e45d5 :: IO (BG.FunPtr (BG.CInt -> BG.Ptr BG.Void -> IO ()))
hs_bindgen_26b1c20d174e45d5 =
  BG.fromFFIType hs_bindgen_26b1c20d174e45d5_base

{-# NOINLINE args_pointer3 #-}
{-| __C declaration:__ @args_pointer3@

    __defined at:__ @macros\/reparse.h 43:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer3 :: BG.FunPtr (BG.CInt -> BG.Ptr BG.Void -> IO ())
args_pointer3 =
  BG.unsafePerformIO hs_bindgen_26b1c20d174e45d5

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_A@
foreign import ccall unsafe "hs_bindgen_8760cbd218bc55d1" hs_bindgen_8760cbd218bc55d1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_A@
hs_bindgen_8760cbd218bc55d1 :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_8760cbd218bc55d1 =
  BG.fromFFIType hs_bindgen_8760cbd218bc55d1_base

{-# NOINLINE ret_A #-}
{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h 47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A :: BG.FunPtr (IO BG.CInt)
ret_A =
  BG.unsafePerformIO hs_bindgen_8760cbd218bc55d1

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_char1@
foreign import ccall unsafe "hs_bindgen_e910062cffeff2d2" hs_bindgen_e910062cffeff2d2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_char1@
hs_bindgen_e910062cffeff2d2 :: IO (BG.FunPtr (BG.CInt -> IO BG.CChar))
hs_bindgen_e910062cffeff2d2 =
  BG.fromFFIType hs_bindgen_e910062cffeff2d2_base

{-# NOINLINE ret_char1 #-}
{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h 49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1 :: BG.FunPtr (BG.CInt -> IO BG.CChar)
ret_char1 =
  BG.unsafePerformIO hs_bindgen_e910062cffeff2d2

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_char2@
foreign import ccall unsafe "hs_bindgen_d9218f2ac4ee5a50" hs_bindgen_d9218f2ac4ee5a50_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_char2@
hs_bindgen_d9218f2ac4ee5a50 :: IO (BG.FunPtr (BG.CInt -> IO BG.CSChar))
hs_bindgen_d9218f2ac4ee5a50 =
  BG.fromFFIType hs_bindgen_d9218f2ac4ee5a50_base

{-# NOINLINE ret_char2 #-}
{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h 50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2 :: BG.FunPtr (BG.CInt -> IO BG.CSChar)
ret_char2 =
  BG.unsafePerformIO hs_bindgen_d9218f2ac4ee5a50

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_char3@
foreign import ccall unsafe "hs_bindgen_7a2d6ef06c7567a1" hs_bindgen_7a2d6ef06c7567a1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_char3@
hs_bindgen_7a2d6ef06c7567a1 :: IO (BG.FunPtr (BG.CInt -> IO BG.CUChar))
hs_bindgen_7a2d6ef06c7567a1 =
  BG.fromFFIType hs_bindgen_7a2d6ef06c7567a1_base

{-# NOINLINE ret_char3 #-}
{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h 51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3 :: BG.FunPtr (BG.CInt -> IO BG.CUChar)
ret_char3 =
  BG.unsafePerformIO hs_bindgen_7a2d6ef06c7567a1

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_short1@
foreign import ccall unsafe "hs_bindgen_c715ac00f4ca7f36" hs_bindgen_c715ac00f4ca7f36_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_short1@
hs_bindgen_c715ac00f4ca7f36 :: IO (BG.FunPtr (BG.CInt -> IO BG.CShort))
hs_bindgen_c715ac00f4ca7f36 =
  BG.fromFFIType hs_bindgen_c715ac00f4ca7f36_base

{-# NOINLINE ret_short1 #-}
{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h 53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1 :: BG.FunPtr (BG.CInt -> IO BG.CShort)
ret_short1 =
  BG.unsafePerformIO hs_bindgen_c715ac00f4ca7f36

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_short2@
foreign import ccall unsafe "hs_bindgen_880f34b16d908c1f" hs_bindgen_880f34b16d908c1f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_short2@
hs_bindgen_880f34b16d908c1f :: IO (BG.FunPtr (BG.CInt -> IO BG.CShort))
hs_bindgen_880f34b16d908c1f =
  BG.fromFFIType hs_bindgen_880f34b16d908c1f_base

{-# NOINLINE ret_short2 #-}
{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h 54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2 :: BG.FunPtr (BG.CInt -> IO BG.CShort)
ret_short2 =
  BG.unsafePerformIO hs_bindgen_880f34b16d908c1f

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_short3@
foreign import ccall unsafe "hs_bindgen_ea8c8e8164273c1a" hs_bindgen_ea8c8e8164273c1a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_short3@
hs_bindgen_ea8c8e8164273c1a :: IO (BG.FunPtr (BG.CInt -> IO BG.CUShort))
hs_bindgen_ea8c8e8164273c1a =
  BG.fromFFIType hs_bindgen_ea8c8e8164273c1a_base

{-# NOINLINE ret_short3 #-}
{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h 55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3 :: BG.FunPtr (BG.CInt -> IO BG.CUShort)
ret_short3 =
  BG.unsafePerformIO hs_bindgen_ea8c8e8164273c1a

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_int1@
foreign import ccall unsafe "hs_bindgen_97a22a914dd2414c" hs_bindgen_97a22a914dd2414c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_int1@
hs_bindgen_97a22a914dd2414c :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_97a22a914dd2414c =
  BG.fromFFIType hs_bindgen_97a22a914dd2414c_base

{-# NOINLINE ret_int1 #-}
{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h 57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1 :: BG.FunPtr (BG.CInt -> IO BG.CInt)
ret_int1 =
  BG.unsafePerformIO hs_bindgen_97a22a914dd2414c

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_int2@
foreign import ccall unsafe "hs_bindgen_77a2b7921cb7566a" hs_bindgen_77a2b7921cb7566a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_int2@
hs_bindgen_77a2b7921cb7566a :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_77a2b7921cb7566a =
  BG.fromFFIType hs_bindgen_77a2b7921cb7566a_base

{-# NOINLINE ret_int2 #-}
{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h 58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2 :: BG.FunPtr (BG.CInt -> IO BG.CInt)
ret_int2 =
  BG.unsafePerformIO hs_bindgen_77a2b7921cb7566a

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_int3@
foreign import ccall unsafe "hs_bindgen_0dc8eed9c02e16ff" hs_bindgen_0dc8eed9c02e16ff_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_int3@
hs_bindgen_0dc8eed9c02e16ff :: IO (BG.FunPtr (BG.CInt -> IO BG.CUInt))
hs_bindgen_0dc8eed9c02e16ff =
  BG.fromFFIType hs_bindgen_0dc8eed9c02e16ff_base

{-# NOINLINE ret_int3 #-}
{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h 59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3 :: BG.FunPtr (BG.CInt -> IO BG.CUInt)
ret_int3 =
  BG.unsafePerformIO hs_bindgen_0dc8eed9c02e16ff

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_long1@
foreign import ccall unsafe "hs_bindgen_8a42512fc931fa53" hs_bindgen_8a42512fc931fa53_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_long1@
hs_bindgen_8a42512fc931fa53 :: IO (BG.FunPtr (BG.CInt -> IO BG.CLong))
hs_bindgen_8a42512fc931fa53 =
  BG.fromFFIType hs_bindgen_8a42512fc931fa53_base

{-# NOINLINE ret_long1 #-}
{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h 61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1 :: BG.FunPtr (BG.CInt -> IO BG.CLong)
ret_long1 =
  BG.unsafePerformIO hs_bindgen_8a42512fc931fa53

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_long2@
foreign import ccall unsafe "hs_bindgen_8293ce652f4875b7" hs_bindgen_8293ce652f4875b7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_long2@
hs_bindgen_8293ce652f4875b7 :: IO (BG.FunPtr (BG.CInt -> IO BG.CLong))
hs_bindgen_8293ce652f4875b7 =
  BG.fromFFIType hs_bindgen_8293ce652f4875b7_base

{-# NOINLINE ret_long2 #-}
{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h 62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2 :: BG.FunPtr (BG.CInt -> IO BG.CLong)
ret_long2 =
  BG.unsafePerformIO hs_bindgen_8293ce652f4875b7

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_long3@
foreign import ccall unsafe "hs_bindgen_1ca2d3b54e331b1f" hs_bindgen_1ca2d3b54e331b1f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_long3@
hs_bindgen_1ca2d3b54e331b1f :: IO (BG.FunPtr (BG.CInt -> IO BG.CULong))
hs_bindgen_1ca2d3b54e331b1f =
  BG.fromFFIType hs_bindgen_1ca2d3b54e331b1f_base

{-# NOINLINE ret_long3 #-}
{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h 63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3 :: BG.FunPtr (BG.CInt -> IO BG.CULong)
ret_long3 =
  BG.unsafePerformIO hs_bindgen_1ca2d3b54e331b1f

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_float@
foreign import ccall unsafe "hs_bindgen_5362002da5a787ba" hs_bindgen_5362002da5a787ba_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_float@
hs_bindgen_5362002da5a787ba :: IO (BG.FunPtr (BG.CInt -> IO BG.CFloat))
hs_bindgen_5362002da5a787ba =
  BG.fromFFIType hs_bindgen_5362002da5a787ba_base

{-# NOINLINE ret_float #-}
{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h 65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float :: BG.FunPtr (BG.CInt -> IO BG.CFloat)
ret_float =
  BG.unsafePerformIO hs_bindgen_5362002da5a787ba

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_double@
foreign import ccall unsafe "hs_bindgen_0ae8c6485107bf6f" hs_bindgen_0ae8c6485107bf6f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_double@
hs_bindgen_0ae8c6485107bf6f :: IO (BG.FunPtr (BG.CInt -> IO BG.CDouble))
hs_bindgen_0ae8c6485107bf6f =
  BG.fromFFIType hs_bindgen_0ae8c6485107bf6f_base

{-# NOINLINE ret_double #-}
{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h 66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double :: BG.FunPtr (BG.CInt -> IO BG.CDouble)
ret_double =
  BG.unsafePerformIO hs_bindgen_0ae8c6485107bf6f

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_bool1@
foreign import ccall unsafe "hs_bindgen_04d8f1fc8df996ba" hs_bindgen_04d8f1fc8df996ba_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_bool1@
hs_bindgen_04d8f1fc8df996ba :: IO (BG.FunPtr (BG.CInt -> IO BG.CBool))
hs_bindgen_04d8f1fc8df996ba =
  BG.fromFFIType hs_bindgen_04d8f1fc8df996ba_base

{-# NOINLINE ret_bool1 #-}
{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h 67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1 :: BG.FunPtr (BG.CInt -> IO BG.CBool)
ret_bool1 =
  BG.unsafePerformIO hs_bindgen_04d8f1fc8df996ba

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_struct@
foreign import ccall unsafe "hs_bindgen_b7cd9769c7dac7b8" hs_bindgen_b7cd9769c7dac7b8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_struct@
hs_bindgen_b7cd9769c7dac7b8 :: IO (BG.FunPtr (BG.CInt -> IO Some_struct))
hs_bindgen_b7cd9769c7dac7b8 =
  BG.fromFFIType hs_bindgen_b7cd9769c7dac7b8_base

{-# NOINLINE ret_struct #-}
{-| __C declaration:__ @ret_struct@

    __defined at:__ @macros\/reparse.h 69:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_struct :: BG.FunPtr (BG.CInt -> IO Some_struct)
ret_struct =
  BG.unsafePerformIO hs_bindgen_b7cd9769c7dac7b8

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_union@
foreign import ccall unsafe "hs_bindgen_b4afde7383e5aff4" hs_bindgen_b4afde7383e5aff4_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_union@
hs_bindgen_b4afde7383e5aff4 :: IO (BG.FunPtr (BG.CInt -> IO Some_union))
hs_bindgen_b4afde7383e5aff4 =
  BG.fromFFIType hs_bindgen_b4afde7383e5aff4_base

{-# NOINLINE ret_union #-}
{-| __C declaration:__ @ret_union@

    __defined at:__ @macros\/reparse.h 70:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_union :: BG.FunPtr (BG.CInt -> IO Some_union)
ret_union =
  BG.unsafePerformIO hs_bindgen_b4afde7383e5aff4

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_enum@
foreign import ccall unsafe "hs_bindgen_239f0b420c073c18" hs_bindgen_239f0b420c073c18_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_enum@
hs_bindgen_239f0b420c073c18 :: IO (BG.FunPtr (BG.CInt -> IO Some_enum))
hs_bindgen_239f0b420c073c18 =
  BG.fromFFIType hs_bindgen_239f0b420c073c18_base

{-# NOINLINE ret_enum #-}
{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h 71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum :: BG.FunPtr (BG.CInt -> IO Some_enum)
ret_enum =
  BG.unsafePerformIO hs_bindgen_239f0b420c073c18

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_pointer1@
foreign import ccall unsafe "hs_bindgen_56eab0f8d0bdf79f" hs_bindgen_56eab0f8d0bdf79f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_pointer1@
hs_bindgen_56eab0f8d0bdf79f :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt)))
hs_bindgen_56eab0f8d0bdf79f =
  BG.fromFFIType hs_bindgen_56eab0f8d0bdf79f_base

{-# NOINLINE ret_pointer1 #-}
{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h 73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt))
ret_pointer1 =
  BG.unsafePerformIO hs_bindgen_56eab0f8d0bdf79f

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_pointer2@
foreign import ccall unsafe "hs_bindgen_4c952a3d40721870" hs_bindgen_4c952a3d40721870_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_pointer2@
hs_bindgen_4c952a3d40721870 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr BG.CInt))))
hs_bindgen_4c952a3d40721870 =
  BG.fromFFIType hs_bindgen_4c952a3d40721870_base

{-# NOINLINE ret_pointer2 #-}
{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h 74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr BG.CInt)))
ret_pointer2 =
  BG.unsafePerformIO hs_bindgen_4c952a3d40721870

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_pointer3@
foreign import ccall unsafe "hs_bindgen_ccb0b4753d3e2490" hs_bindgen_ccb0b4753d3e2490_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_pointer3@
hs_bindgen_ccb0b4753d3e2490 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.Void)))
hs_bindgen_ccb0b4753d3e2490 =
  BG.fromFFIType hs_bindgen_ccb0b4753d3e2490_base

{-# NOINLINE ret_pointer3 #-}
{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h 75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.Void))
ret_pointer3 =
  BG.unsafePerformIO hs_bindgen_ccb0b4753d3e2490

-- __unique:__ @test_macrosreparse_2_raw_Example_get_body1@
foreign import ccall unsafe "hs_bindgen_442c91e88aec2368" hs_bindgen_442c91e88aec2368_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_body1@
hs_bindgen_442c91e88aec2368 :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_442c91e88aec2368 =
  BG.fromFFIType hs_bindgen_442c91e88aec2368_base

{-# NOINLINE body1 #-}
{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h 79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1 :: BG.FunPtr (BG.CInt -> IO BG.CInt)
body1 =
  BG.unsafePerformIO hs_bindgen_442c91e88aec2368

-- __unique:__ @test_macrosreparse_2_raw_Example_get_body2@
foreign import ccall unsafe "hs_bindgen_cfbec40e47da01c3" hs_bindgen_cfbec40e47da01c3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_body2@
hs_bindgen_cfbec40e47da01c3 :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_cfbec40e47da01c3 =
  BG.fromFFIType hs_bindgen_cfbec40e47da01c3_base

{-# NOINLINE body2 #-}
{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h 80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2 :: BG.FunPtr (IO BG.CInt)
body2 =
  BG.unsafePerformIO hs_bindgen_cfbec40e47da01c3

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_complex_float@
foreign import ccall unsafe "hs_bindgen_4319227a8aaa0b24" hs_bindgen_4319227a8aaa0b24_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_complex_float@
hs_bindgen_4319227a8aaa0b24 :: IO (BG.FunPtr (BG.CInt -> BG.Complex BG.CFloat -> IO ()))
hs_bindgen_4319227a8aaa0b24 =
  BG.fromFFIType hs_bindgen_4319227a8aaa0b24_base

{-# NOINLINE args_complex_float #-}
{-| __C declaration:__ @args_complex_float@

    __defined at:__ @macros\/reparse.h 84:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_float :: BG.FunPtr (BG.CInt -> BG.Complex BG.CFloat -> IO ())
args_complex_float =
  BG.unsafePerformIO hs_bindgen_4319227a8aaa0b24

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_complex_double@
foreign import ccall unsafe "hs_bindgen_9620841c151e1094" hs_bindgen_9620841c151e1094_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_args_complex_double@
hs_bindgen_9620841c151e1094 :: IO (BG.FunPtr (BG.CInt -> BG.Complex BG.CDouble -> IO ()))
hs_bindgen_9620841c151e1094 =
  BG.fromFFIType hs_bindgen_9620841c151e1094_base

{-# NOINLINE args_complex_double #-}
{-| __C declaration:__ @args_complex_double@

    __defined at:__ @macros\/reparse.h 85:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_double :: BG.FunPtr (BG.CInt -> BG.Complex BG.CDouble -> IO ())
args_complex_double =
  BG.unsafePerformIO hs_bindgen_9620841c151e1094

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_complex_float@
foreign import ccall unsafe "hs_bindgen_aa2302691b1dc578" hs_bindgen_aa2302691b1dc578_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_complex_float@
hs_bindgen_aa2302691b1dc578 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Complex BG.CFloat)))
hs_bindgen_aa2302691b1dc578 =
  BG.fromFFIType hs_bindgen_aa2302691b1dc578_base

{-# NOINLINE ret_complex_float #-}
{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @macros\/reparse.h 86:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_float :: BG.FunPtr (BG.CInt -> IO (BG.Complex BG.CFloat))
ret_complex_float =
  BG.unsafePerformIO hs_bindgen_aa2302691b1dc578

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_complex_double@
foreign import ccall unsafe "hs_bindgen_27f2ccb0e11ce3d4" hs_bindgen_27f2ccb0e11ce3d4_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_ret_complex_double@
hs_bindgen_27f2ccb0e11ce3d4 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Complex BG.CDouble)))
hs_bindgen_27f2ccb0e11ce3d4 =
  BG.fromFFIType hs_bindgen_27f2ccb0e11ce3d4_base

{-# NOINLINE ret_complex_double #-}
{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @macros\/reparse.h 87:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_double :: BG.FunPtr (BG.CInt -> IO (BG.Complex BG.CDouble))
ret_complex_double =
  BG.unsafePerformIO hs_bindgen_27f2ccb0e11ce3d4

-- __unique:__ @test_macrosreparse_2_raw_Example_get_bespoke_args1@
foreign import ccall unsafe "hs_bindgen_5f39596c5788b2cf" hs_bindgen_5f39596c5788b2cf_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_bespoke_args1@
hs_bindgen_5f39596c5788b2cf :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_5f39596c5788b2cf =
  BG.fromFFIType hs_bindgen_5f39596c5788b2cf_base

{-# NOINLINE bespoke_args1 #-}
{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @macros\/reparse.h 94:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args1 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
bespoke_args1 =
  BG.unsafePerformIO hs_bindgen_5f39596c5788b2cf

-- __unique:__ @test_macrosreparse_2_raw_Example_get_bespoke_args2@
foreign import ccall unsafe "hs_bindgen_6fc3008f0e66bd81" hs_bindgen_6fc3008f0e66bd81_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_bespoke_args2@
hs_bindgen_6fc3008f0e66bd81 :: IO (BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_6fc3008f0e66bd81 =
  BG.fromFFIType hs_bindgen_6fc3008f0e66bd81_base

{-# NOINLINE bespoke_args2 #-}
{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @macros\/reparse.h 95:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args2 :: BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ())
bespoke_args2 =
  BG.unsafePerformIO hs_bindgen_6fc3008f0e66bd81

-- __unique:__ @test_macrosreparse_2_raw_Example_get_bespoke_ret1@
foreign import ccall unsafe "hs_bindgen_02e119349dfc0dc6" hs_bindgen_02e119349dfc0dc6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_bespoke_ret1@
hs_bindgen_02e119349dfc0dc6 :: IO (BG.FunPtr (BG.CInt -> IO BG.CBool))
hs_bindgen_02e119349dfc0dc6 =
  BG.fromFFIType hs_bindgen_02e119349dfc0dc6_base

{-# NOINLINE bespoke_ret1 #-}
{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h 97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1 :: BG.FunPtr (BG.CInt -> IO BG.CBool)
bespoke_ret1 =
  BG.unsafePerformIO hs_bindgen_02e119349dfc0dc6

-- __unique:__ @test_macrosreparse_2_raw_Example_get_bespoke_ret2@
foreign import ccall unsafe "hs_bindgen_447b3d2bccfb7ae8" hs_bindgen_447b3d2bccfb7ae8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_bespoke_ret2@
hs_bindgen_447b3d2bccfb7ae8 :: IO (BG.FunPtr (BG.CInt -> IO HsBindgen.Runtime.LibC.CSize))
hs_bindgen_447b3d2bccfb7ae8 =
  BG.fromFFIType hs_bindgen_447b3d2bccfb7ae8_base

{-# NOINLINE bespoke_ret2 #-}
{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h 98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2 :: BG.FunPtr (BG.CInt -> IO HsBindgen.Runtime.LibC.CSize)
bespoke_ret2 =
  BG.unsafePerformIO hs_bindgen_447b3d2bccfb7ae8

-- __unique:__ @test_macrosreparse_2_raw_Example_get_arr_args1@
foreign import ccall unsafe "hs_bindgen_869038d8098a2bf2" hs_bindgen_869038d8098a2bf2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_arr_args1@
hs_bindgen_869038d8098a2bf2 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO ()))
hs_bindgen_869038d8098a2bf2 =
  BG.fromFFIType hs_bindgen_869038d8098a2bf2_base

{-# NOINLINE arr_args1 #-}
{-| Arrays

    __C declaration:__ @arr_args1@

    __defined at:__ @macros\/reparse.h 104:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args1 :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO ())
arr_args1 =
  BG.unsafePerformIO hs_bindgen_869038d8098a2bf2

-- __unique:__ @test_macrosreparse_2_raw_Example_get_arr_args2@
foreign import ccall unsafe "hs_bindgen_d97856ae41779071" hs_bindgen_d97856ae41779071_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_arr_args2@
hs_bindgen_d97856ae41779071 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt))) -> IO ()))
hs_bindgen_d97856ae41779071 =
  BG.fromFFIType hs_bindgen_d97856ae41779071_base

{-# NOINLINE arr_args2 #-}
{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h 105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2 :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt))) -> IO ())
arr_args2 =
  BG.unsafePerformIO hs_bindgen_d97856ae41779071

-- __unique:__ @test_macrosreparse_2_raw_Example_get_arr_args3@
foreign import ccall unsafe "hs_bindgen_e5fcdc276e072021" hs_bindgen_e5fcdc276e072021_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_arr_args3@
hs_bindgen_e5fcdc276e072021 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt)) -> IO ()))
hs_bindgen_e5fcdc276e072021 =
  BG.fromFFIType hs_bindgen_e5fcdc276e072021_base

{-# NOINLINE arr_args3 #-}
{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h 106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3 :: BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt)) -> IO ())
arr_args3 =
  BG.unsafePerformIO hs_bindgen_e5fcdc276e072021

-- __unique:__ @test_macrosreparse_2_raw_Example_get_arr_args4@
foreign import ccall unsafe "hs_bindgen_5960a1a097c40b0b" hs_bindgen_5960a1a097c40b0b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_arr_args4@
hs_bindgen_5960a1a097c40b0b :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt))) -> IO ()))
hs_bindgen_5960a1a097c40b0b =
  BG.fromFFIType hs_bindgen_5960a1a097c40b0b_base

{-# NOINLINE arr_args4 #-}
{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h 107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4 :: BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt))) -> IO ())
arr_args4 =
  BG.unsafePerformIO hs_bindgen_5960a1a097c40b0b

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_args1@
foreign import ccall unsafe "hs_bindgen_6e1c21a20bf17727" hs_bindgen_6e1c21a20bf17727_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_args1@
hs_bindgen_6e1c21a20bf17727 :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (IO ()) -> IO ()))
hs_bindgen_6e1c21a20bf17727 =
  BG.fromFFIType hs_bindgen_6e1c21a20bf17727_base

{-# NOINLINE funptr_args1 #-}
{-| Function pointers

    __C declaration:__ @funptr_args1@

    __defined at:__ @macros\/reparse.h 126:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args1 :: BG.FunPtr (BG.CInt -> BG.FunPtr (IO ()) -> IO ())
funptr_args1 =
  BG.unsafePerformIO hs_bindgen_6e1c21a20bf17727

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_args2@
foreign import ccall unsafe "hs_bindgen_316b1d39ef1c99c8" hs_bindgen_316b1d39ef1c99c8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_args2@
hs_bindgen_316b1d39ef1c99c8 :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (IO BG.CInt) -> IO ()))
hs_bindgen_316b1d39ef1c99c8 =
  BG.fromFFIType hs_bindgen_316b1d39ef1c99c8_base

{-# NOINLINE funptr_args2 #-}
{-| __C declaration:__ @funptr_args2@

    __defined at:__ @macros\/reparse.h 127:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args2 :: BG.FunPtr (BG.CInt -> BG.FunPtr (IO BG.CInt) -> IO ())
funptr_args2 =
  BG.unsafePerformIO hs_bindgen_316b1d39ef1c99c8

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_args3@
foreign import ccall unsafe "hs_bindgen_3b587acc49334022" hs_bindgen_3b587acc49334022_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_args3@
hs_bindgen_3b587acc49334022 :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> IO ()) -> IO ()))
hs_bindgen_3b587acc49334022 =
  BG.fromFFIType hs_bindgen_3b587acc49334022_base

{-# NOINLINE funptr_args3 #-}
{-| __C declaration:__ @funptr_args3@

    __defined at:__ @macros\/reparse.h 128:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args3 :: BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> IO ()) -> IO ())
funptr_args3 =
  BG.unsafePerformIO hs_bindgen_3b587acc49334022

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_args4@
foreign import ccall unsafe "hs_bindgen_880f8ecb89c20f54" hs_bindgen_880f8ecb89c20f54_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_args4@
hs_bindgen_880f8ecb89c20f54 :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar) -> IO ()))
hs_bindgen_880f8ecb89c20f54 =
  BG.fromFFIType hs_bindgen_880f8ecb89c20f54_base

{-# NOINLINE funptr_args4 #-}
{-| __C declaration:__ @funptr_args4@

    __defined at:__ @macros\/reparse.h 129:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args4 :: BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar) -> IO ())
funptr_args4 =
  BG.unsafePerformIO hs_bindgen_880f8ecb89c20f54

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_args5@
foreign import ccall unsafe "hs_bindgen_25220e78ceba78f8" hs_bindgen_25220e78ceba78f8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_args5@
hs_bindgen_25220e78ceba78f8 :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)) -> IO ()))
hs_bindgen_25220e78ceba78f8 =
  BG.fromFFIType hs_bindgen_25220e78ceba78f8_base

{-# NOINLINE funptr_args5 #-}
{-| __C declaration:__ @funptr_args5@

    __defined at:__ @macros\/reparse.h 130:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args5 :: BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)) -> IO ())
funptr_args5 =
  BG.unsafePerformIO hs_bindgen_25220e78ceba78f8

-- __unique:__ @test_macrosreparse_2_raw_Example_get_comments1@
foreign import ccall unsafe "hs_bindgen_6cef08508aed5305" hs_bindgen_6cef08508aed5305_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_comments1@
hs_bindgen_6cef08508aed5305 :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_6cef08508aed5305 =
  BG.fromFFIType hs_bindgen_6cef08508aed5305_base

{-# NOINLINE comments1 #-}
{-| Comments in awkward places

    (Prior to language-c we failed to parse there.)

    __C declaration:__ @comments1@

    __defined at:__ @macros\/reparse.h 144:25@

    __exported by:__ @macros\/reparse.h@
-}
comments1 :: BG.FunPtr (BG.CInt -> IO ())
comments1 =
  BG.unsafePerformIO hs_bindgen_6cef08508aed5305

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_prim_before1@
foreign import ccall unsafe "hs_bindgen_71a50f57cd1216e6" hs_bindgen_71a50f57cd1216e6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_prim_before1@
hs_bindgen_71a50f57cd1216e6 :: IO (BG.FunPtr (BG.CInt -> BG.CChar -> IO ()))
hs_bindgen_71a50f57cd1216e6 =
  BG.fromFFIType hs_bindgen_71a50f57cd1216e6_base

{-# NOINLINE const_prim_before1 #-}
{-| @const@ qualifier

    NOTE: These were not parsed correctly prior to the switch to language-c.

    __C declaration:__ @const_prim_before1@

    __defined at:__ @macros\/reparse.h 177:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before1 :: BG.FunPtr (BG.CInt -> BG.CChar -> IO ())
const_prim_before1 =
  BG.unsafePerformIO hs_bindgen_71a50f57cd1216e6

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_prim_before2@
foreign import ccall unsafe "hs_bindgen_5233d49c08278554" hs_bindgen_5233d49c08278554_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_prim_before2@
hs_bindgen_5233d49c08278554 :: IO (BG.FunPtr (BG.CInt -> BG.CSChar -> IO ()))
hs_bindgen_5233d49c08278554 =
  BG.fromFFIType hs_bindgen_5233d49c08278554_base

{-# NOINLINE const_prim_before2 #-}
{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @macros\/reparse.h 178:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before2 :: BG.FunPtr (BG.CInt -> BG.CSChar -> IO ())
const_prim_before2 =
  BG.unsafePerformIO hs_bindgen_5233d49c08278554

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_prim_before3@
foreign import ccall unsafe "hs_bindgen_b47263d563e229b2" hs_bindgen_b47263d563e229b2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_prim_before3@
hs_bindgen_b47263d563e229b2 :: IO (BG.FunPtr (BG.CInt -> BG.CUChar -> IO ()))
hs_bindgen_b47263d563e229b2 =
  BG.fromFFIType hs_bindgen_b47263d563e229b2_base

{-# NOINLINE const_prim_before3 #-}
{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @macros\/reparse.h 179:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before3 :: BG.FunPtr (BG.CInt -> BG.CUChar -> IO ())
const_prim_before3 =
  BG.unsafePerformIO hs_bindgen_b47263d563e229b2

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_prim_after1@
foreign import ccall unsafe "hs_bindgen_9b538efe2347a74e" hs_bindgen_9b538efe2347a74e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_prim_after1@
hs_bindgen_9b538efe2347a74e :: IO (BG.FunPtr (BG.CInt -> BG.CChar -> IO ()))
hs_bindgen_9b538efe2347a74e =
  BG.fromFFIType hs_bindgen_9b538efe2347a74e_base

{-# NOINLINE const_prim_after1 #-}
{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @macros\/reparse.h 180:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after1 :: BG.FunPtr (BG.CInt -> BG.CChar -> IO ())
const_prim_after1 =
  BG.unsafePerformIO hs_bindgen_9b538efe2347a74e

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_prim_after2@
foreign import ccall unsafe "hs_bindgen_e9bd30449260d07b" hs_bindgen_e9bd30449260d07b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_prim_after2@
hs_bindgen_e9bd30449260d07b :: IO (BG.FunPtr (BG.CInt -> BG.CSChar -> IO ()))
hs_bindgen_e9bd30449260d07b =
  BG.fromFFIType hs_bindgen_e9bd30449260d07b_base

{-# NOINLINE const_prim_after2 #-}
{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @macros\/reparse.h 181:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after2 :: BG.FunPtr (BG.CInt -> BG.CSChar -> IO ())
const_prim_after2 =
  BG.unsafePerformIO hs_bindgen_e9bd30449260d07b

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_prim_after3@
foreign import ccall unsafe "hs_bindgen_f1e92cd065630ccb" hs_bindgen_f1e92cd065630ccb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_prim_after3@
hs_bindgen_f1e92cd065630ccb :: IO (BG.FunPtr (BG.CInt -> BG.CUChar -> IO ()))
hs_bindgen_f1e92cd065630ccb =
  BG.fromFFIType hs_bindgen_f1e92cd065630ccb_base

{-# NOINLINE const_prim_after3 #-}
{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @macros\/reparse.h 182:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after3 :: BG.FunPtr (BG.CInt -> BG.CUChar -> IO ())
const_prim_after3 =
  BG.unsafePerformIO hs_bindgen_f1e92cd065630ccb

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_before1@
foreign import ccall unsafe "hs_bindgen_aea63f55c25fa26d" hs_bindgen_aea63f55c25fa26d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_before1@
hs_bindgen_aea63f55c25fa26d :: IO (BG.FunPtr (BG.CInt -> BG.CFloat -> IO ()))
hs_bindgen_aea63f55c25fa26d =
  BG.fromFFIType hs_bindgen_aea63f55c25fa26d_base

{-# NOINLINE const_withoutSign_before1 #-}
{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @macros\/reparse.h 186:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before1 :: BG.FunPtr (BG.CInt -> BG.CFloat -> IO ())
const_withoutSign_before1 =
  BG.unsafePerformIO hs_bindgen_aea63f55c25fa26d

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_before2@
foreign import ccall unsafe "hs_bindgen_1a34eb11a6ccfd86" hs_bindgen_1a34eb11a6ccfd86_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_before2@
hs_bindgen_1a34eb11a6ccfd86 :: IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO ()))
hs_bindgen_1a34eb11a6ccfd86 =
  BG.fromFFIType hs_bindgen_1a34eb11a6ccfd86_base

{-# NOINLINE const_withoutSign_before2 #-}
{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @macros\/reparse.h 187:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before2 :: BG.FunPtr (BG.CInt -> BG.CDouble -> IO ())
const_withoutSign_before2 =
  BG.unsafePerformIO hs_bindgen_1a34eb11a6ccfd86

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_before3@
foreign import ccall unsafe "hs_bindgen_d9af8e3a8991a2e4" hs_bindgen_d9af8e3a8991a2e4_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_before3@
hs_bindgen_d9af8e3a8991a2e4 :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_d9af8e3a8991a2e4 =
  BG.fromFFIType hs_bindgen_d9af8e3a8991a2e4_base

{-# NOINLINE const_withoutSign_before3 #-}
{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @macros\/reparse.h 188:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before3 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
const_withoutSign_before3 =
  BG.unsafePerformIO hs_bindgen_d9af8e3a8991a2e4

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_before4@
foreign import ccall unsafe "hs_bindgen_558f393b1eddf40d" hs_bindgen_558f393b1eddf40d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_before4@
hs_bindgen_558f393b1eddf40d :: IO (BG.FunPtr (BG.CInt -> Some_struct -> IO ()))
hs_bindgen_558f393b1eddf40d =
  BG.fromFFIType hs_bindgen_558f393b1eddf40d_base

{-# NOINLINE const_withoutSign_before4 #-}
{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @macros\/reparse.h 189:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before4 :: BG.FunPtr (BG.CInt -> Some_struct -> IO ())
const_withoutSign_before4 =
  BG.unsafePerformIO hs_bindgen_558f393b1eddf40d

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_before5@
foreign import ccall unsafe "hs_bindgen_e62c93c0973b2781" hs_bindgen_e62c93c0973b2781_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_before5@
hs_bindgen_e62c93c0973b2781 :: IO (BG.FunPtr (BG.CInt -> Some_union -> IO ()))
hs_bindgen_e62c93c0973b2781 =
  BG.fromFFIType hs_bindgen_e62c93c0973b2781_base

{-# NOINLINE const_withoutSign_before5 #-}
{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @macros\/reparse.h 190:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before5 :: BG.FunPtr (BG.CInt -> Some_union -> IO ())
const_withoutSign_before5 =
  BG.unsafePerformIO hs_bindgen_e62c93c0973b2781

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_before6@
foreign import ccall unsafe "hs_bindgen_e56acb4f339a362d" hs_bindgen_e56acb4f339a362d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_before6@
hs_bindgen_e56acb4f339a362d :: IO (BG.FunPtr (BG.CInt -> Some_enum -> IO ()))
hs_bindgen_e56acb4f339a362d =
  BG.fromFFIType hs_bindgen_e56acb4f339a362d_base

{-# NOINLINE const_withoutSign_before6 #-}
{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @macros\/reparse.h 191:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before6 :: BG.FunPtr (BG.CInt -> Some_enum -> IO ())
const_withoutSign_before6 =
  BG.unsafePerformIO hs_bindgen_e56acb4f339a362d

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_before7@
foreign import ccall unsafe "hs_bindgen_c298f88f131c980b" hs_bindgen_c298f88f131c980b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_before7@
hs_bindgen_c298f88f131c980b :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_c298f88f131c980b =
  BG.fromFFIType hs_bindgen_c298f88f131c980b_base

{-# NOINLINE const_withoutSign_before7 #-}
{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @macros\/reparse.h 192:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before7 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
const_withoutSign_before7 =
  BG.unsafePerformIO hs_bindgen_c298f88f131c980b

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_before8@
foreign import ccall unsafe "hs_bindgen_38d5cf2b5d94860c" hs_bindgen_38d5cf2b5d94860c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_before8@
hs_bindgen_38d5cf2b5d94860c :: IO (BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_38d5cf2b5d94860c =
  BG.fromFFIType hs_bindgen_38d5cf2b5d94860c_base

{-# NOINLINE const_withoutSign_before8 #-}
{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @macros\/reparse.h 193:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before8 :: BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ())
const_withoutSign_before8 =
  BG.unsafePerformIO hs_bindgen_38d5cf2b5d94860c

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_after1@
foreign import ccall unsafe "hs_bindgen_362151a5ba0ce905" hs_bindgen_362151a5ba0ce905_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_after1@
hs_bindgen_362151a5ba0ce905 :: IO (BG.FunPtr (BG.CInt -> BG.CFloat -> IO ()))
hs_bindgen_362151a5ba0ce905 =
  BG.fromFFIType hs_bindgen_362151a5ba0ce905_base

{-# NOINLINE const_withoutSign_after1 #-}
{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @macros\/reparse.h 195:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after1 :: BG.FunPtr (BG.CInt -> BG.CFloat -> IO ())
const_withoutSign_after1 =
  BG.unsafePerformIO hs_bindgen_362151a5ba0ce905

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_after2@
foreign import ccall unsafe "hs_bindgen_071d6b5b46afca86" hs_bindgen_071d6b5b46afca86_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_after2@
hs_bindgen_071d6b5b46afca86 :: IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO ()))
hs_bindgen_071d6b5b46afca86 =
  BG.fromFFIType hs_bindgen_071d6b5b46afca86_base

{-# NOINLINE const_withoutSign_after2 #-}
{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @macros\/reparse.h 196:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after2 :: BG.FunPtr (BG.CInt -> BG.CDouble -> IO ())
const_withoutSign_after2 =
  BG.unsafePerformIO hs_bindgen_071d6b5b46afca86

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_after3@
foreign import ccall unsafe "hs_bindgen_cb70cd180a753e3a" hs_bindgen_cb70cd180a753e3a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_after3@
hs_bindgen_cb70cd180a753e3a :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_cb70cd180a753e3a =
  BG.fromFFIType hs_bindgen_cb70cd180a753e3a_base

{-# NOINLINE const_withoutSign_after3 #-}
{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @macros\/reparse.h 197:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after3 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
const_withoutSign_after3 =
  BG.unsafePerformIO hs_bindgen_cb70cd180a753e3a

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_after4@
foreign import ccall unsafe "hs_bindgen_17e67974fc77dde6" hs_bindgen_17e67974fc77dde6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_after4@
hs_bindgen_17e67974fc77dde6 :: IO (BG.FunPtr (BG.CInt -> Some_struct -> IO ()))
hs_bindgen_17e67974fc77dde6 =
  BG.fromFFIType hs_bindgen_17e67974fc77dde6_base

{-# NOINLINE const_withoutSign_after4 #-}
{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @macros\/reparse.h 198:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after4 :: BG.FunPtr (BG.CInt -> Some_struct -> IO ())
const_withoutSign_after4 =
  BG.unsafePerformIO hs_bindgen_17e67974fc77dde6

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_after5@
foreign import ccall unsafe "hs_bindgen_aea1ac49e3a717ae" hs_bindgen_aea1ac49e3a717ae_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_after5@
hs_bindgen_aea1ac49e3a717ae :: IO (BG.FunPtr (BG.CInt -> Some_union -> IO ()))
hs_bindgen_aea1ac49e3a717ae =
  BG.fromFFIType hs_bindgen_aea1ac49e3a717ae_base

{-# NOINLINE const_withoutSign_after5 #-}
{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @macros\/reparse.h 199:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after5 :: BG.FunPtr (BG.CInt -> Some_union -> IO ())
const_withoutSign_after5 =
  BG.unsafePerformIO hs_bindgen_aea1ac49e3a717ae

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_after6@
foreign import ccall unsafe "hs_bindgen_89ad81e6dc3a6ed4" hs_bindgen_89ad81e6dc3a6ed4_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_after6@
hs_bindgen_89ad81e6dc3a6ed4 :: IO (BG.FunPtr (BG.CInt -> Some_enum -> IO ()))
hs_bindgen_89ad81e6dc3a6ed4 =
  BG.fromFFIType hs_bindgen_89ad81e6dc3a6ed4_base

{-# NOINLINE const_withoutSign_after6 #-}
{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @macros\/reparse.h 200:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after6 :: BG.FunPtr (BG.CInt -> Some_enum -> IO ())
const_withoutSign_after6 =
  BG.unsafePerformIO hs_bindgen_89ad81e6dc3a6ed4

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_after7@
foreign import ccall unsafe "hs_bindgen_29b119a94317aae6" hs_bindgen_29b119a94317aae6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_after7@
hs_bindgen_29b119a94317aae6 :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_29b119a94317aae6 =
  BG.fromFFIType hs_bindgen_29b119a94317aae6_base

{-# NOINLINE const_withoutSign_after7 #-}
{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @macros\/reparse.h 201:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after7 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
const_withoutSign_after7 =
  BG.unsafePerformIO hs_bindgen_29b119a94317aae6

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_after8@
foreign import ccall unsafe "hs_bindgen_be8db07cc695795d" hs_bindgen_be8db07cc695795d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_withoutSign_after8@
hs_bindgen_be8db07cc695795d :: IO (BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_be8db07cc695795d =
  BG.fromFFIType hs_bindgen_be8db07cc695795d_base

{-# NOINLINE const_withoutSign_after8 #-}
{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @macros\/reparse.h 202:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after8 :: BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ())
const_withoutSign_after8 =
  BG.unsafePerformIO hs_bindgen_be8db07cc695795d

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_args1@
foreign import ccall unsafe "hs_bindgen_3f4562f74152c205" hs_bindgen_3f4562f74152c205_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_args1@
hs_bindgen_3f4562f74152c205 :: IO (BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_3f4562f74152c205 =
  BG.fromFFIType hs_bindgen_3f4562f74152c205_base

{-# NOINLINE const_pointers_args1 #-}
{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @macros\/reparse.h 206:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args1 :: BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args1 =
  BG.unsafePerformIO hs_bindgen_3f4562f74152c205

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_args2@
foreign import ccall unsafe "hs_bindgen_87278ccb01e7fee0" hs_bindgen_87278ccb01e7fee0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_args2@
hs_bindgen_87278ccb01e7fee0 :: IO (BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_87278ccb01e7fee0 =
  BG.fromFFIType hs_bindgen_87278ccb01e7fee0_base

{-# NOINLINE const_pointers_args2 #-}
{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @macros\/reparse.h 207:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args2 :: BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args2 =
  BG.unsafePerformIO hs_bindgen_87278ccb01e7fee0

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_args3@
foreign import ccall unsafe "hs_bindgen_1e25b241655c3ebb" hs_bindgen_1e25b241655c3ebb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_args3@
hs_bindgen_1e25b241655c3ebb :: IO (BG.FunPtr (BG.CInt -> BG.Ptr BG.CInt -> IO ()))
hs_bindgen_1e25b241655c3ebb =
  BG.fromFFIType hs_bindgen_1e25b241655c3ebb_base

{-# NOINLINE const_pointers_args3 #-}
{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @macros\/reparse.h 208:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args3 :: BG.FunPtr (BG.CInt -> BG.Ptr BG.CInt -> IO ())
const_pointers_args3 =
  BG.unsafePerformIO hs_bindgen_1e25b241655c3ebb

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_args4@
foreign import ccall unsafe "hs_bindgen_fa94aed37836859b" hs_bindgen_fa94aed37836859b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_args4@
hs_bindgen_fa94aed37836859b :: IO (BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_fa94aed37836859b =
  BG.fromFFIType hs_bindgen_fa94aed37836859b_base

{-# NOINLINE const_pointers_args4 #-}
{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @macros\/reparse.h 209:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args4 :: BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args4 =
  BG.unsafePerformIO hs_bindgen_fa94aed37836859b

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_args5@
foreign import ccall unsafe "hs_bindgen_800407f1e67deddb" hs_bindgen_800407f1e67deddb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_args5@
hs_bindgen_800407f1e67deddb :: IO (BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_800407f1e67deddb =
  BG.fromFFIType hs_bindgen_800407f1e67deddb_base

{-# NOINLINE const_pointers_args5 #-}
{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @macros\/reparse.h 210:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args5 :: BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args5 =
  BG.unsafePerformIO hs_bindgen_800407f1e67deddb

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_ret1@
foreign import ccall unsafe "hs_bindgen_91e3fb45ca972f9d" hs_bindgen_91e3fb45ca972f9d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_ret1@
hs_bindgen_91e3fb45ca972f9d :: IO (BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_91e3fb45ca972f9d =
  BG.fromFFIType hs_bindgen_91e3fb45ca972f9d_base

{-# NOINLINE const_pointers_ret1 #-}
{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h 212:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1 :: BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret1 =
  BG.unsafePerformIO hs_bindgen_91e3fb45ca972f9d

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_ret2@
foreign import ccall unsafe "hs_bindgen_ce269be70accca82" hs_bindgen_ce269be70accca82_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_ret2@
hs_bindgen_ce269be70accca82 :: IO (BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_ce269be70accca82 =
  BG.fromFFIType hs_bindgen_ce269be70accca82_base

{-# NOINLINE const_pointers_ret2 #-}
{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h 213:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2 :: BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret2 =
  BG.unsafePerformIO hs_bindgen_ce269be70accca82

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_ret3@
foreign import ccall unsafe "hs_bindgen_f0f2ae46270c9785" hs_bindgen_f0f2ae46270c9785_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_ret3@
hs_bindgen_f0f2ae46270c9785 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt)))
hs_bindgen_f0f2ae46270c9785 =
  BG.fromFFIType hs_bindgen_f0f2ae46270c9785_base

{-# NOINLINE const_pointers_ret3 #-}
{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h 214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt))
const_pointers_ret3 =
  BG.unsafePerformIO hs_bindgen_f0f2ae46270c9785

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_ret4@
foreign import ccall unsafe "hs_bindgen_4ecbc6cf48fbe0c3" hs_bindgen_4ecbc6cf48fbe0c3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_ret4@
hs_bindgen_4ecbc6cf48fbe0c3 :: IO (BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_4ecbc6cf48fbe0c3 =
  BG.fromFFIType hs_bindgen_4ecbc6cf48fbe0c3_base

{-# NOINLINE const_pointers_ret4 #-}
{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h 215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4 :: BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret4 =
  BG.unsafePerformIO hs_bindgen_4ecbc6cf48fbe0c3

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_ret5@
foreign import ccall unsafe "hs_bindgen_c0b16cd44f63f52f" hs_bindgen_c0b16cd44f63f52f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_pointers_ret5@
hs_bindgen_c0b16cd44f63f52f :: IO (BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_c0b16cd44f63f52f =
  BG.fromFFIType hs_bindgen_c0b16cd44f63f52f_base

{-# NOINLINE const_pointers_ret5 #-}
{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h 216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5 :: BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret5 =
  BG.unsafePerformIO hs_bindgen_c0b16cd44f63f52f

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_array_elem1@
foreign import ccall unsafe "hs_bindgen_db9eb973118a6fbf" hs_bindgen_db9eb973118a6fbf_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_array_elem1@
hs_bindgen_db9eb973118a6fbf :: IO (BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO ()))
hs_bindgen_db9eb973118a6fbf =
  BG.fromFFIType hs_bindgen_db9eb973118a6fbf_base

{-# NOINLINE const_array_elem1 #-}
{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h 244:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 :: BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO ())
const_array_elem1 =
  BG.unsafePerformIO hs_bindgen_db9eb973118a6fbf

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_array_elem2@
foreign import ccall unsafe "hs_bindgen_9288cd4171172005" hs_bindgen_9288cd4171172005_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_array_elem2@
hs_bindgen_9288cd4171172005 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt))) -> IO ()))
hs_bindgen_9288cd4171172005 =
  BG.fromFFIType hs_bindgen_9288cd4171172005_base

{-# NOINLINE const_array_elem2 #-}
{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h 245:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2 :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt))) -> IO ())
const_array_elem2 =
  BG.unsafePerformIO hs_bindgen_9288cd4171172005

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_array_elem3@
foreign import ccall unsafe "hs_bindgen_c20403bc692b1bde" hs_bindgen_c20403bc692b1bde_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const_array_elem3@
hs_bindgen_c20403bc692b1bde :: IO (BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt))) -> IO ()))
hs_bindgen_c20403bc692b1bde =
  BG.fromFFIType hs_bindgen_c20403bc692b1bde_base

{-# NOINLINE const_array_elem3 #-}
{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h 246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 :: BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt))) -> IO ())
const_array_elem3 =
  BG.unsafePerformIO hs_bindgen_c20403bc692b1bde

-- __unique:__ @test_macrosreparse_2_raw_Example_get_noParams1@
foreign import ccall unsafe "hs_bindgen_5ca24e7f6ac8cff0" hs_bindgen_5ca24e7f6ac8cff0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_noParams1@
hs_bindgen_5ca24e7f6ac8cff0 :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_5ca24e7f6ac8cff0 =
  BG.fromFFIType hs_bindgen_5ca24e7f6ac8cff0_base

{-# NOINLINE noParams1 #-}
{-| Other examples we reparsed /incorrectly/ before language-c

    __C declaration:__ @noParams1@

    __defined at:__ @macros\/reparse.h 254:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams1 :: BG.FunPtr (IO BG.CInt)
noParams1 =
  BG.unsafePerformIO hs_bindgen_5ca24e7f6ac8cff0

-- __unique:__ @test_macrosreparse_2_raw_Example_get_noParams2@
foreign import ccall unsafe "hs_bindgen_6f664f6fdcbced27" hs_bindgen_6f664f6fdcbced27_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_noParams2@
hs_bindgen_6f664f6fdcbced27 :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_6f664f6fdcbced27 =
  BG.fromFFIType hs_bindgen_6f664f6fdcbced27_base

{-# NOINLINE noParams2 #-}
{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h 255:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2 :: BG.FunPtr (IO BG.CInt)
noParams2 =
  BG.unsafePerformIO hs_bindgen_6f664f6fdcbced27

-- __unique:__ @test_macrosreparse_2_raw_Example_get_noParams3@
foreign import ccall unsafe "hs_bindgen_1067f730bd5b4c43" hs_bindgen_1067f730bd5b4c43_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_noParams3@
hs_bindgen_1067f730bd5b4c43 :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (IO BG.CInt) -> IO ()))
hs_bindgen_1067f730bd5b4c43 =
  BG.fromFFIType hs_bindgen_1067f730bd5b4c43_base

{-# NOINLINE noParams3 #-}
{-| __C declaration:__ @noParams3@

    __defined at:__ @macros\/reparse.h 256:6@

    __exported by:__ @macros\/reparse.h@
-}
noParams3 :: BG.FunPtr (BG.CInt -> BG.FunPtr (IO BG.CInt) -> IO ())
noParams3 =
  BG.unsafePerformIO hs_bindgen_1067f730bd5b4c43

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret1@
foreign import ccall unsafe "hs_bindgen_6b5258a8560bc11c" hs_bindgen_6b5258a8560bc11c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret1@
hs_bindgen_6b5258a8560bc11c :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (IO ()))))
hs_bindgen_6b5258a8560bc11c =
  BG.fromFFIType hs_bindgen_6b5258a8560bc11c_base

{-# NOINLINE funptr_ret1 #-}
{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h 260:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (IO ())))
funptr_ret1 =
  BG.unsafePerformIO hs_bindgen_6b5258a8560bc11c

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret2@
foreign import ccall unsafe "hs_bindgen_723b02be7cb23263" hs_bindgen_723b02be7cb23263_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret2@
hs_bindgen_723b02be7cb23263 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (IO BG.CInt))))
hs_bindgen_723b02be7cb23263 =
  BG.fromFFIType hs_bindgen_723b02be7cb23263_base

{-# NOINLINE funptr_ret2 #-}
{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h 261:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (IO BG.CInt)))
funptr_ret2 =
  BG.unsafePerformIO hs_bindgen_723b02be7cb23263

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret3@
foreign import ccall unsafe "hs_bindgen_7c4d8fc844c6f97b" hs_bindgen_7c4d8fc844c6f97b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret3@
hs_bindgen_7c4d8fc844c6f97b :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> IO ()))))
hs_bindgen_7c4d8fc844c6f97b =
  BG.fromFFIType hs_bindgen_7c4d8fc844c6f97b_base

{-# NOINLINE funptr_ret3 #-}
{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h 262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> IO ())))
funptr_ret3 =
  BG.unsafePerformIO hs_bindgen_7c4d8fc844c6f97b

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret4@
foreign import ccall unsafe "hs_bindgen_25b578cce360cf12" hs_bindgen_25b578cce360cf12_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret4@
hs_bindgen_25b578cce360cf12 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))))
hs_bindgen_25b578cce360cf12 =
  BG.fromFFIType hs_bindgen_25b578cce360cf12_base

{-# NOINLINE funptr_ret4 #-}
{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h 263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar)))
funptr_ret4 =
  BG.unsafePerformIO hs_bindgen_25b578cce360cf12

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret5@
foreign import ccall unsafe "hs_bindgen_5e58691a68a915de" hs_bindgen_5e58691a68a915de_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret5@
hs_bindgen_5e58691a68a915de :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))))
hs_bindgen_5e58691a68a915de =
  BG.fromFFIType hs_bindgen_5e58691a68a915de_base

{-# NOINLINE funptr_ret5 #-}
{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h 267:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))))
funptr_ret5 =
  BG.unsafePerformIO hs_bindgen_5e58691a68a915de

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret6@
foreign import ccall unsafe "hs_bindgen_0c36a271f93a9dff" hs_bindgen_0c36a271f93a9dff_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret6@
hs_bindgen_0c36a271f93a9dff :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_0c36a271f93a9dff =
  BG.fromFFIType hs_bindgen_0c36a271f93a9dff_base

{-# NOINLINE funptr_ret6 #-}
{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h 268:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret6 =
  BG.unsafePerformIO hs_bindgen_0c36a271f93a9dff

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret7@
foreign import ccall unsafe "hs_bindgen_67439554fe1e797d" hs_bindgen_67439554fe1e797d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret7@
hs_bindgen_67439554fe1e797d :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_67439554fe1e797d =
  BG.fromFFIType hs_bindgen_67439554fe1e797d_base

{-# NOINLINE funptr_ret7 #-}
{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h 269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret7 =
  BG.unsafePerformIO hs_bindgen_67439554fe1e797d

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret8@
foreign import ccall unsafe "hs_bindgen_870a61b2b66eecf3" hs_bindgen_870a61b2b66eecf3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret8@
hs_bindgen_870a61b2b66eecf3 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))))
hs_bindgen_870a61b2b66eecf3 =
  BG.fromFFIType hs_bindgen_870a61b2b66eecf3_base

{-# NOINLINE funptr_ret8 #-}
{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h 270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))))
funptr_ret8 =
  BG.unsafePerformIO hs_bindgen_870a61b2b66eecf3

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret9@
foreign import ccall unsafe "hs_bindgen_e7920c47095333a3" hs_bindgen_e7920c47095333a3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret9@
hs_bindgen_e7920c47095333a3 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_e7920c47095333a3 =
  BG.fromFFIType hs_bindgen_e7920c47095333a3_base

{-# NOINLINE funptr_ret9 #-}
{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h 271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret9 =
  BG.unsafePerformIO hs_bindgen_e7920c47095333a3

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret10@
foreign import ccall unsafe "hs_bindgen_aa1eab9b222fa4b8" hs_bindgen_aa1eab9b222fa4b8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_funptr_ret10@
hs_bindgen_aa1eab9b222fa4b8 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_aa1eab9b222fa4b8 =
  BG.fromFFIType hs_bindgen_aa1eab9b222fa4b8_base

{-# NOINLINE funptr_ret10 #-}
{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h 272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret10 =
  BG.unsafePerformIO hs_bindgen_aa1eab9b222fa4b8
