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
  , "/* test_macrosreparse_1_empty_Example_get_args_char1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b80f882fdcf9d390 (void)) ("
  , "  signed int arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &args_char1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_char2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b604969d2857133d (void)) ("
  , "  signed int arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  return &args_char2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_char3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9b95a55e6ceb1621 (void)) ("
  , "  signed int arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  return &args_char3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_short1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f72fa5fc8023eb3f (void)) ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_short2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5e4d62486e4f3759 (void)) ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_short3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cef5409d73a3ebb9 (void)) ("
  , "  signed int arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  return &args_short3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_int1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_67400948e9390e71 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_int2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f931f0229bddda59 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_int3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fa14da0adf102175 (void)) ("
  , "  signed int arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  return &args_int3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_long1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_af7f311d12eb20c0 (void)) ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_long2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_09143fbe9a30e2a2 (void)) ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_long3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9a73b75e254c0317 (void)) ("
  , "  signed int arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  return &args_long3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_float */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_98a70ab7e71e634d (void)) ("
  , "  signed int arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  return &args_float;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_double */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2bc72f8a3bdbad63 (void)) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &args_double;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_bool1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4f6046208f0c55ab (void)) ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &args_bool1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_struct */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_511a8f421aca49d7 (void)) ("
  , "  signed int arg1,"
  , "  struct some_struct arg2"
  , ")"
  , "{"
  , "  return &args_struct;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_union */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_edf2a54db0ba78fa (void)) ("
  , "  signed int arg1,"
  , "  union some_union arg2"
  , ")"
  , "{"
  , "  return &args_union;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_enum */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fc74034537b4ecf6 (void)) ("
  , "  signed int arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  return &args_enum;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_pointer1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_594ea5d2885bc64d (void)) ("
  , "  signed int arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return &args_pointer1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_pointer2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5179b8dedaed5e5e (void)) ("
  , "  signed int arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  return &args_pointer2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_pointer3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_72daaebaddd12e45 (void)) ("
  , "  signed int arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  return &args_pointer3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_A */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_cf6e0ba74be5a2da (void)) (void)"
  , "{"
  , "  return &ret_A;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_char1 */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_f223792546e3022e (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_char1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_char2 */"
  , "__attribute__ ((const))"
  , "signed char (*hs_bindgen_9bb497f3e0172b6d (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_char2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_char3 */"
  , "__attribute__ ((const))"
  , "unsigned char (*hs_bindgen_3be11c39ee59643e (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_char3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_short1 */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_a65b0b8d17719893 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_short1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_short2 */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_5190e7513c922ff0 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_short2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_short3 */"
  , "__attribute__ ((const))"
  , "unsigned short (*hs_bindgen_43b7df2a55a61277 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_short3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_int1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_0c98313fb5db2326 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_int1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_int2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_ec162368f9f436e3 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_int2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_int3 */"
  , "__attribute__ ((const))"
  , "unsigned int (*hs_bindgen_7affbbac42cd29e2 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_int3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_long1 */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_7622c56c9539e04c (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_long1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_long2 */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_815df1fcd6285cb1 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_long2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_long3 */"
  , "__attribute__ ((const))"
  , "unsigned long (*hs_bindgen_246bedca6a708d23 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_long3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_float */"
  , "__attribute__ ((const))"
  , "float (*hs_bindgen_d0a43141a677f147 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_float;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_double */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_e39c2a2347167cd5 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_double;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_bool1 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_2f72fbc044a4bbfd (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_bool1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_struct */"
  , "__attribute__ ((const))"
  , "struct some_struct (*hs_bindgen_3ed00edaff786c0e (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_struct;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_union */"
  , "__attribute__ ((const))"
  , "union some_union (*hs_bindgen_1b270595798725a6 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_union;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_enum */"
  , "__attribute__ ((const))"
  , "enum some_enum (*hs_bindgen_df1e959d57683966 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_enum;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_pointer1 */"
  , "__attribute__ ((const))"
  , "signed int *(*hs_bindgen_f66df065bdb19e72 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_pointer1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_pointer2 */"
  , "__attribute__ ((const))"
  , "signed int **(*hs_bindgen_12d9b147f5523ea9 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_pointer2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_pointer3 */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_4ac9f0fe826d0748 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_pointer3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_body1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_2974316436a63e20 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &body1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_body2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_2b14d27784f519af (void)) (void)"
  , "{"
  , "  return &body2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_complex_float */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_387d68ceb6d006f9 (void)) ("
  , "  signed int arg1,"
  , "  float _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_float;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_args_complex_double */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a0dd7e51d4c3782c (void)) ("
  , "  signed int arg1,"
  , "  double _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_double;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_complex_float */"
  , "__attribute__ ((const))"
  , "float _Complex (*hs_bindgen_121af3746ec0fe57 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_complex_float;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_ret_complex_double */"
  , "__attribute__ ((const))"
  , "double _Complex (*hs_bindgen_f5f4fa2b769a0807 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_complex_double;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_bespoke_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6c590995b92657c7 (void)) ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &bespoke_args1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_bespoke_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b871d8c50d13d10a (void)) ("
  , "  signed int arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &bespoke_args2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_bespoke_ret1 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_457eeaee9b36d630 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_bespoke_ret2 */"
  , "__attribute__ ((const))"
  , "size_t (*hs_bindgen_11a75beb43d5fc2e (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_arr_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_618624c6e6b83a22 (void)) ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return &arr_args1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_arr_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2ad0461638177d93 (void)) ("
  , "  signed int **arg1"
  , ")"
  , "{"
  , "  return &arr_args2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_arr_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c77b1b370d4c5f4b (void)) ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return &arr_args3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_arr_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_de199c863fb8ac12 (void)) ("
  , "  signed int **arg1"
  , ")"
  , "{"
  , "  return &arr_args4;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_funptr_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7fc280d0a83fec73 (void)) ("
  , "  signed int arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_funptr_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9beb81169b296475 (void)) ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_funptr_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d85158bc0b976b5a (void)) ("
  , "  signed int arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_funptr_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0b011779ec7227d9 (void)) ("
  , "  signed int arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args4;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_funptr_args5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d358fcc92cb10cf3 (void)) ("
  , "  signed int arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args5;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_comments1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0fd159d332801a82 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &comments1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_prim_before1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a40876f663bcef73 (void)) ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_prim_before2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_dc5eb25e2e741ab6 (void)) ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_prim_before3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9718326f589a56a0 (void)) ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_prim_after1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cde59c88cf7b3db3 (void)) ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_prim_after2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d5ab011afdbef099 (void)) ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_prim_after3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_30f31bede8bfc87c (void)) ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_withoutSign_before1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6cc702c24ac0eeb2 (void)) ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_withoutSign_before2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e77ba4a5962cc68c (void)) ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_withoutSign_before3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c4d9de5a3852ba73 (void)) ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_withoutSign_before4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_585a37cd5f43f5da (void)) ("
  , "  signed int arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before4;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_withoutSign_before5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_655c28dba28a9a53 (void)) ("
  , "  signed int arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before5;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_withoutSign_before6 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9e08bdc34bafeee1 (void)) ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before6;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_withoutSign_before7 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5736b6db8251bdd5 (void)) ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before7;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_withoutSign_before8 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c398560b654a8d9d (void)) ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before8;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_withoutSign_after1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_10bff078d94c8cd0 (void)) ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_withoutSign_after2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b75e6b8d1be2004b (void)) ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_withoutSign_after3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_60f26b5a2f8cf394 (void)) ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_withoutSign_after4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0503ec9e78e10edf (void)) ("
  , "  signed int arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after4;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_withoutSign_after5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1657dae083620adf (void)) ("
  , "  signed int arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after5;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_withoutSign_after6 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_44c293b62ff2015d (void)) ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after6;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_withoutSign_after7 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ed406d5edbd28ec1 (void)) ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after7;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_withoutSign_after8 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2cca685d1adca496 (void)) ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after8;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_pointers_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5da738696e5beb04 (void)) ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_pointers_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_783634f682149eb7 (void)) ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_pointers_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e8c26ca0eb024ce8 (void)) ("
  , "  signed int arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_pointers_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_21e206a694593eb1 (void)) ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args4;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_pointers_args5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cc3a8f368db00421 (void)) ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args5;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_pointers_ret1 */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_a9979680c0124010 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_pointers_ret2 */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_c865f82ab69a4a3e (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_pointers_ret3 */"
  , "__attribute__ ((const))"
  , "signed int *const (*hs_bindgen_61570ab40d22e404 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_pointers_ret4 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_7d02f676d3a68181 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret4;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_pointers_ret5 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_3a389377331b29b9 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret5;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_array_elem1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4810bd323f24b8d0 (void)) ("
  , "  signed int const *arg1"
  , ")"
  , "{"
  , "  return &const_array_elem1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_array_elem2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8bed93eafa056c5e (void)) ("
  , "  signed int const **arg1"
  , ")"
  , "{"
  , "  return &const_array_elem2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const_array_elem3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c569d0da33bf00e3 (void)) ("
  , "  signed int *const *arg1"
  , ")"
  , "{"
  , "  return &const_array_elem3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_noParams1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_91d4c9002424f82d (void)) (void)"
  , "{"
  , "  return &noParams1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_noParams2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a317809988dfeb84 (void)) (void)"
  , "{"
  , "  return &noParams2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_noParams3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c23ef724e4f315be (void)) ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &noParams3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_funptr_ret1 */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_660d89308b2b5a69 (void)) ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_funptr_ret2 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_bc7009eeb6b86418 (void)) ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_funptr_ret3 */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_565629b4e4c20a7e (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &funptr_ret3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_funptr_ret4 */"
  , "__attribute__ ((const))"
  , "char (*(*hs_bindgen_3563760f061700c9 (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret4;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_funptr_ret5 */"
  , "__attribute__ ((const))"
  , "signed int *(*(*hs_bindgen_65c114a912faa4d1 (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret5;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_funptr_ret6 */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_13dce2f12ceb5ef5 (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret6;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_funptr_ret7 */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_e69f4bc4bd10259c (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret7;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_funptr_ret8 */"
  , "__attribute__ ((const))"
  , "signed int *const (*(*hs_bindgen_f06ecca1a94b60e8 (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret8;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_funptr_ret9 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_65e9b457479db3fb (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret9;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_funptr_ret10 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_b5fb601528eabcdb (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret10;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_char1@
foreign import ccall unsafe "hs_bindgen_b80f882fdcf9d390" hs_bindgen_b80f882fdcf9d390_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_char1@
hs_bindgen_b80f882fdcf9d390 :: IO (BG.FunPtr (BG.CInt -> BG.CChar -> IO ()))
hs_bindgen_b80f882fdcf9d390 =
  BG.fromFFIType hs_bindgen_b80f882fdcf9d390_base

{-# NOINLINE args_char1 #-}
{-| Function declarations

    __C declaration:__ @args_char1@

    __defined at:__ @macros\/reparse.h 17:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char1 :: BG.FunPtr (BG.CInt -> BG.CChar -> IO ())
args_char1 =
  BG.unsafePerformIO hs_bindgen_b80f882fdcf9d390

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_char2@
foreign import ccall unsafe "hs_bindgen_b604969d2857133d" hs_bindgen_b604969d2857133d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_char2@
hs_bindgen_b604969d2857133d :: IO (BG.FunPtr (BG.CInt -> BG.CSChar -> IO ()))
hs_bindgen_b604969d2857133d =
  BG.fromFFIType hs_bindgen_b604969d2857133d_base

{-# NOINLINE args_char2 #-}
{-| __C declaration:__ @args_char2@

    __defined at:__ @macros\/reparse.h 18:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char2 :: BG.FunPtr (BG.CInt -> BG.CSChar -> IO ())
args_char2 =
  BG.unsafePerformIO hs_bindgen_b604969d2857133d

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_char3@
foreign import ccall unsafe "hs_bindgen_9b95a55e6ceb1621" hs_bindgen_9b95a55e6ceb1621_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_char3@
hs_bindgen_9b95a55e6ceb1621 :: IO (BG.FunPtr (BG.CInt -> BG.CUChar -> IO ()))
hs_bindgen_9b95a55e6ceb1621 =
  BG.fromFFIType hs_bindgen_9b95a55e6ceb1621_base

{-# NOINLINE args_char3 #-}
{-| __C declaration:__ @args_char3@

    __defined at:__ @macros\/reparse.h 19:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char3 :: BG.FunPtr (BG.CInt -> BG.CUChar -> IO ())
args_char3 =
  BG.unsafePerformIO hs_bindgen_9b95a55e6ceb1621

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_short1@
foreign import ccall unsafe "hs_bindgen_f72fa5fc8023eb3f" hs_bindgen_f72fa5fc8023eb3f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_short1@
hs_bindgen_f72fa5fc8023eb3f :: IO (BG.FunPtr (BG.CInt -> BG.CShort -> IO ()))
hs_bindgen_f72fa5fc8023eb3f =
  BG.fromFFIType hs_bindgen_f72fa5fc8023eb3f_base

{-# NOINLINE args_short1 #-}
{-| __C declaration:__ @args_short1@

    __defined at:__ @macros\/reparse.h 21:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short1 :: BG.FunPtr (BG.CInt -> BG.CShort -> IO ())
args_short1 =
  BG.unsafePerformIO hs_bindgen_f72fa5fc8023eb3f

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_short2@
foreign import ccall unsafe "hs_bindgen_5e4d62486e4f3759" hs_bindgen_5e4d62486e4f3759_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_short2@
hs_bindgen_5e4d62486e4f3759 :: IO (BG.FunPtr (BG.CInt -> BG.CShort -> IO ()))
hs_bindgen_5e4d62486e4f3759 =
  BG.fromFFIType hs_bindgen_5e4d62486e4f3759_base

{-# NOINLINE args_short2 #-}
{-| __C declaration:__ @args_short2@

    __defined at:__ @macros\/reparse.h 22:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short2 :: BG.FunPtr (BG.CInt -> BG.CShort -> IO ())
args_short2 =
  BG.unsafePerformIO hs_bindgen_5e4d62486e4f3759

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_short3@
foreign import ccall unsafe "hs_bindgen_cef5409d73a3ebb9" hs_bindgen_cef5409d73a3ebb9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_short3@
hs_bindgen_cef5409d73a3ebb9 :: IO (BG.FunPtr (BG.CInt -> BG.CUShort -> IO ()))
hs_bindgen_cef5409d73a3ebb9 =
  BG.fromFFIType hs_bindgen_cef5409d73a3ebb9_base

{-# NOINLINE args_short3 #-}
{-| __C declaration:__ @args_short3@

    __defined at:__ @macros\/reparse.h 23:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short3 :: BG.FunPtr (BG.CInt -> BG.CUShort -> IO ())
args_short3 =
  BG.unsafePerformIO hs_bindgen_cef5409d73a3ebb9

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_int1@
foreign import ccall unsafe "hs_bindgen_67400948e9390e71" hs_bindgen_67400948e9390e71_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_int1@
hs_bindgen_67400948e9390e71 :: IO (BG.FunPtr (BG.CInt -> BG.CInt -> IO ()))
hs_bindgen_67400948e9390e71 =
  BG.fromFFIType hs_bindgen_67400948e9390e71_base

{-# NOINLINE args_int1 #-}
{-| __C declaration:__ @args_int1@

    __defined at:__ @macros\/reparse.h 25:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int1 :: BG.FunPtr (BG.CInt -> BG.CInt -> IO ())
args_int1 =
  BG.unsafePerformIO hs_bindgen_67400948e9390e71

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_int2@
foreign import ccall unsafe "hs_bindgen_f931f0229bddda59" hs_bindgen_f931f0229bddda59_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_int2@
hs_bindgen_f931f0229bddda59 :: IO (BG.FunPtr (BG.CInt -> BG.CInt -> IO ()))
hs_bindgen_f931f0229bddda59 =
  BG.fromFFIType hs_bindgen_f931f0229bddda59_base

{-# NOINLINE args_int2 #-}
{-| __C declaration:__ @args_int2@

    __defined at:__ @macros\/reparse.h 26:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int2 :: BG.FunPtr (BG.CInt -> BG.CInt -> IO ())
args_int2 =
  BG.unsafePerformIO hs_bindgen_f931f0229bddda59

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_int3@
foreign import ccall unsafe "hs_bindgen_fa14da0adf102175" hs_bindgen_fa14da0adf102175_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_int3@
hs_bindgen_fa14da0adf102175 :: IO (BG.FunPtr (BG.CInt -> BG.CUInt -> IO ()))
hs_bindgen_fa14da0adf102175 =
  BG.fromFFIType hs_bindgen_fa14da0adf102175_base

{-# NOINLINE args_int3 #-}
{-| __C declaration:__ @args_int3@

    __defined at:__ @macros\/reparse.h 27:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int3 :: BG.FunPtr (BG.CInt -> BG.CUInt -> IO ())
args_int3 =
  BG.unsafePerformIO hs_bindgen_fa14da0adf102175

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_long1@
foreign import ccall unsafe "hs_bindgen_af7f311d12eb20c0" hs_bindgen_af7f311d12eb20c0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_long1@
hs_bindgen_af7f311d12eb20c0 :: IO (BG.FunPtr (BG.CInt -> BG.CLong -> IO ()))
hs_bindgen_af7f311d12eb20c0 =
  BG.fromFFIType hs_bindgen_af7f311d12eb20c0_base

{-# NOINLINE args_long1 #-}
{-| __C declaration:__ @args_long1@

    __defined at:__ @macros\/reparse.h 29:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long1 :: BG.FunPtr (BG.CInt -> BG.CLong -> IO ())
args_long1 =
  BG.unsafePerformIO hs_bindgen_af7f311d12eb20c0

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_long2@
foreign import ccall unsafe "hs_bindgen_09143fbe9a30e2a2" hs_bindgen_09143fbe9a30e2a2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_long2@
hs_bindgen_09143fbe9a30e2a2 :: IO (BG.FunPtr (BG.CInt -> BG.CLong -> IO ()))
hs_bindgen_09143fbe9a30e2a2 =
  BG.fromFFIType hs_bindgen_09143fbe9a30e2a2_base

{-# NOINLINE args_long2 #-}
{-| __C declaration:__ @args_long2@

    __defined at:__ @macros\/reparse.h 30:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long2 :: BG.FunPtr (BG.CInt -> BG.CLong -> IO ())
args_long2 =
  BG.unsafePerformIO hs_bindgen_09143fbe9a30e2a2

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_long3@
foreign import ccall unsafe "hs_bindgen_9a73b75e254c0317" hs_bindgen_9a73b75e254c0317_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_long3@
hs_bindgen_9a73b75e254c0317 :: IO (BG.FunPtr (BG.CInt -> BG.CULong -> IO ()))
hs_bindgen_9a73b75e254c0317 =
  BG.fromFFIType hs_bindgen_9a73b75e254c0317_base

{-# NOINLINE args_long3 #-}
{-| __C declaration:__ @args_long3@

    __defined at:__ @macros\/reparse.h 31:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long3 :: BG.FunPtr (BG.CInt -> BG.CULong -> IO ())
args_long3 =
  BG.unsafePerformIO hs_bindgen_9a73b75e254c0317

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_float@
foreign import ccall unsafe "hs_bindgen_98a70ab7e71e634d" hs_bindgen_98a70ab7e71e634d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_float@
hs_bindgen_98a70ab7e71e634d :: IO (BG.FunPtr (BG.CInt -> BG.CFloat -> IO ()))
hs_bindgen_98a70ab7e71e634d =
  BG.fromFFIType hs_bindgen_98a70ab7e71e634d_base

{-# NOINLINE args_float #-}
{-| __C declaration:__ @args_float@

    __defined at:__ @macros\/reparse.h 33:6@

    __exported by:__ @macros\/reparse.h@
-}
args_float :: BG.FunPtr (BG.CInt -> BG.CFloat -> IO ())
args_float =
  BG.unsafePerformIO hs_bindgen_98a70ab7e71e634d

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_double@
foreign import ccall unsafe "hs_bindgen_2bc72f8a3bdbad63" hs_bindgen_2bc72f8a3bdbad63_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_double@
hs_bindgen_2bc72f8a3bdbad63 :: IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO ()))
hs_bindgen_2bc72f8a3bdbad63 =
  BG.fromFFIType hs_bindgen_2bc72f8a3bdbad63_base

{-# NOINLINE args_double #-}
{-| __C declaration:__ @args_double@

    __defined at:__ @macros\/reparse.h 34:6@

    __exported by:__ @macros\/reparse.h@
-}
args_double :: BG.FunPtr (BG.CInt -> BG.CDouble -> IO ())
args_double =
  BG.unsafePerformIO hs_bindgen_2bc72f8a3bdbad63

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_bool1@
foreign import ccall unsafe "hs_bindgen_4f6046208f0c55ab" hs_bindgen_4f6046208f0c55ab_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_bool1@
hs_bindgen_4f6046208f0c55ab :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_4f6046208f0c55ab =
  BG.fromFFIType hs_bindgen_4f6046208f0c55ab_base

{-# NOINLINE args_bool1 #-}
{-| __C declaration:__ @args_bool1@

    __defined at:__ @macros\/reparse.h 35:6@

    __exported by:__ @macros\/reparse.h@
-}
args_bool1 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
args_bool1 =
  BG.unsafePerformIO hs_bindgen_4f6046208f0c55ab

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_struct@
foreign import ccall unsafe "hs_bindgen_511a8f421aca49d7" hs_bindgen_511a8f421aca49d7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_struct@
hs_bindgen_511a8f421aca49d7 :: IO (BG.FunPtr (BG.CInt -> Some_struct -> IO ()))
hs_bindgen_511a8f421aca49d7 =
  BG.fromFFIType hs_bindgen_511a8f421aca49d7_base

{-# NOINLINE args_struct #-}
{-| __C declaration:__ @args_struct@

    __defined at:__ @macros\/reparse.h 37:6@

    __exported by:__ @macros\/reparse.h@
-}
args_struct :: BG.FunPtr (BG.CInt -> Some_struct -> IO ())
args_struct =
  BG.unsafePerformIO hs_bindgen_511a8f421aca49d7

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_union@
foreign import ccall unsafe "hs_bindgen_edf2a54db0ba78fa" hs_bindgen_edf2a54db0ba78fa_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_union@
hs_bindgen_edf2a54db0ba78fa :: IO (BG.FunPtr (BG.CInt -> Some_union -> IO ()))
hs_bindgen_edf2a54db0ba78fa =
  BG.fromFFIType hs_bindgen_edf2a54db0ba78fa_base

{-# NOINLINE args_union #-}
{-| __C declaration:__ @args_union@

    __defined at:__ @macros\/reparse.h 38:6@

    __exported by:__ @macros\/reparse.h@
-}
args_union :: BG.FunPtr (BG.CInt -> Some_union -> IO ())
args_union =
  BG.unsafePerformIO hs_bindgen_edf2a54db0ba78fa

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_enum@
foreign import ccall unsafe "hs_bindgen_fc74034537b4ecf6" hs_bindgen_fc74034537b4ecf6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_enum@
hs_bindgen_fc74034537b4ecf6 :: IO (BG.FunPtr (BG.CInt -> Some_enum -> IO ()))
hs_bindgen_fc74034537b4ecf6 =
  BG.fromFFIType hs_bindgen_fc74034537b4ecf6_base

{-# NOINLINE args_enum #-}
{-| __C declaration:__ @args_enum@

    __defined at:__ @macros\/reparse.h 39:6@

    __exported by:__ @macros\/reparse.h@
-}
args_enum :: BG.FunPtr (BG.CInt -> Some_enum -> IO ())
args_enum =
  BG.unsafePerformIO hs_bindgen_fc74034537b4ecf6

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_pointer1@
foreign import ccall unsafe "hs_bindgen_594ea5d2885bc64d" hs_bindgen_594ea5d2885bc64d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_pointer1@
hs_bindgen_594ea5d2885bc64d :: IO (BG.FunPtr (BG.CInt -> BG.Ptr BG.CInt -> IO ()))
hs_bindgen_594ea5d2885bc64d =
  BG.fromFFIType hs_bindgen_594ea5d2885bc64d_base

{-# NOINLINE args_pointer1 #-}
{-| __C declaration:__ @args_pointer1@

    __defined at:__ @macros\/reparse.h 41:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer1 :: BG.FunPtr (BG.CInt -> BG.Ptr BG.CInt -> IO ())
args_pointer1 =
  BG.unsafePerformIO hs_bindgen_594ea5d2885bc64d

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_pointer2@
foreign import ccall unsafe "hs_bindgen_5179b8dedaed5e5e" hs_bindgen_5179b8dedaed5e5e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_pointer2@
hs_bindgen_5179b8dedaed5e5e :: IO (BG.FunPtr (BG.CInt -> BG.Ptr (BG.Ptr BG.CInt) -> IO ()))
hs_bindgen_5179b8dedaed5e5e =
  BG.fromFFIType hs_bindgen_5179b8dedaed5e5e_base

{-# NOINLINE args_pointer2 #-}
{-| __C declaration:__ @args_pointer2@

    __defined at:__ @macros\/reparse.h 42:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer2 :: BG.FunPtr (BG.CInt -> BG.Ptr (BG.Ptr BG.CInt) -> IO ())
args_pointer2 =
  BG.unsafePerformIO hs_bindgen_5179b8dedaed5e5e

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_pointer3@
foreign import ccall unsafe "hs_bindgen_72daaebaddd12e45" hs_bindgen_72daaebaddd12e45_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_pointer3@
hs_bindgen_72daaebaddd12e45 :: IO (BG.FunPtr (BG.CInt -> BG.Ptr BG.Void -> IO ()))
hs_bindgen_72daaebaddd12e45 =
  BG.fromFFIType hs_bindgen_72daaebaddd12e45_base

{-# NOINLINE args_pointer3 #-}
{-| __C declaration:__ @args_pointer3@

    __defined at:__ @macros\/reparse.h 43:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer3 :: BG.FunPtr (BG.CInt -> BG.Ptr BG.Void -> IO ())
args_pointer3 =
  BG.unsafePerformIO hs_bindgen_72daaebaddd12e45

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_A@
foreign import ccall unsafe "hs_bindgen_cf6e0ba74be5a2da" hs_bindgen_cf6e0ba74be5a2da_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_A@
hs_bindgen_cf6e0ba74be5a2da :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_cf6e0ba74be5a2da =
  BG.fromFFIType hs_bindgen_cf6e0ba74be5a2da_base

{-# NOINLINE ret_A #-}
{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h 47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A :: BG.FunPtr (IO BG.CInt)
ret_A =
  BG.unsafePerformIO hs_bindgen_cf6e0ba74be5a2da

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_char1@
foreign import ccall unsafe "hs_bindgen_f223792546e3022e" hs_bindgen_f223792546e3022e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_char1@
hs_bindgen_f223792546e3022e :: IO (BG.FunPtr (BG.CInt -> IO BG.CChar))
hs_bindgen_f223792546e3022e =
  BG.fromFFIType hs_bindgen_f223792546e3022e_base

{-# NOINLINE ret_char1 #-}
{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h 49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1 :: BG.FunPtr (BG.CInt -> IO BG.CChar)
ret_char1 =
  BG.unsafePerformIO hs_bindgen_f223792546e3022e

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_char2@
foreign import ccall unsafe "hs_bindgen_9bb497f3e0172b6d" hs_bindgen_9bb497f3e0172b6d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_char2@
hs_bindgen_9bb497f3e0172b6d :: IO (BG.FunPtr (BG.CInt -> IO BG.CSChar))
hs_bindgen_9bb497f3e0172b6d =
  BG.fromFFIType hs_bindgen_9bb497f3e0172b6d_base

{-# NOINLINE ret_char2 #-}
{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h 50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2 :: BG.FunPtr (BG.CInt -> IO BG.CSChar)
ret_char2 =
  BG.unsafePerformIO hs_bindgen_9bb497f3e0172b6d

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_char3@
foreign import ccall unsafe "hs_bindgen_3be11c39ee59643e" hs_bindgen_3be11c39ee59643e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_char3@
hs_bindgen_3be11c39ee59643e :: IO (BG.FunPtr (BG.CInt -> IO BG.CUChar))
hs_bindgen_3be11c39ee59643e =
  BG.fromFFIType hs_bindgen_3be11c39ee59643e_base

{-# NOINLINE ret_char3 #-}
{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h 51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3 :: BG.FunPtr (BG.CInt -> IO BG.CUChar)
ret_char3 =
  BG.unsafePerformIO hs_bindgen_3be11c39ee59643e

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_short1@
foreign import ccall unsafe "hs_bindgen_a65b0b8d17719893" hs_bindgen_a65b0b8d17719893_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_short1@
hs_bindgen_a65b0b8d17719893 :: IO (BG.FunPtr (BG.CInt -> IO BG.CShort))
hs_bindgen_a65b0b8d17719893 =
  BG.fromFFIType hs_bindgen_a65b0b8d17719893_base

{-# NOINLINE ret_short1 #-}
{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h 53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1 :: BG.FunPtr (BG.CInt -> IO BG.CShort)
ret_short1 =
  BG.unsafePerformIO hs_bindgen_a65b0b8d17719893

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_short2@
foreign import ccall unsafe "hs_bindgen_5190e7513c922ff0" hs_bindgen_5190e7513c922ff0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_short2@
hs_bindgen_5190e7513c922ff0 :: IO (BG.FunPtr (BG.CInt -> IO BG.CShort))
hs_bindgen_5190e7513c922ff0 =
  BG.fromFFIType hs_bindgen_5190e7513c922ff0_base

{-# NOINLINE ret_short2 #-}
{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h 54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2 :: BG.FunPtr (BG.CInt -> IO BG.CShort)
ret_short2 =
  BG.unsafePerformIO hs_bindgen_5190e7513c922ff0

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_short3@
foreign import ccall unsafe "hs_bindgen_43b7df2a55a61277" hs_bindgen_43b7df2a55a61277_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_short3@
hs_bindgen_43b7df2a55a61277 :: IO (BG.FunPtr (BG.CInt -> IO BG.CUShort))
hs_bindgen_43b7df2a55a61277 =
  BG.fromFFIType hs_bindgen_43b7df2a55a61277_base

{-# NOINLINE ret_short3 #-}
{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h 55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3 :: BG.FunPtr (BG.CInt -> IO BG.CUShort)
ret_short3 =
  BG.unsafePerformIO hs_bindgen_43b7df2a55a61277

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_int1@
foreign import ccall unsafe "hs_bindgen_0c98313fb5db2326" hs_bindgen_0c98313fb5db2326_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_int1@
hs_bindgen_0c98313fb5db2326 :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_0c98313fb5db2326 =
  BG.fromFFIType hs_bindgen_0c98313fb5db2326_base

{-# NOINLINE ret_int1 #-}
{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h 57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1 :: BG.FunPtr (BG.CInt -> IO BG.CInt)
ret_int1 =
  BG.unsafePerformIO hs_bindgen_0c98313fb5db2326

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_int2@
foreign import ccall unsafe "hs_bindgen_ec162368f9f436e3" hs_bindgen_ec162368f9f436e3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_int2@
hs_bindgen_ec162368f9f436e3 :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_ec162368f9f436e3 =
  BG.fromFFIType hs_bindgen_ec162368f9f436e3_base

{-# NOINLINE ret_int2 #-}
{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h 58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2 :: BG.FunPtr (BG.CInt -> IO BG.CInt)
ret_int2 =
  BG.unsafePerformIO hs_bindgen_ec162368f9f436e3

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_int3@
foreign import ccall unsafe "hs_bindgen_7affbbac42cd29e2" hs_bindgen_7affbbac42cd29e2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_int3@
hs_bindgen_7affbbac42cd29e2 :: IO (BG.FunPtr (BG.CInt -> IO BG.CUInt))
hs_bindgen_7affbbac42cd29e2 =
  BG.fromFFIType hs_bindgen_7affbbac42cd29e2_base

{-# NOINLINE ret_int3 #-}
{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h 59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3 :: BG.FunPtr (BG.CInt -> IO BG.CUInt)
ret_int3 =
  BG.unsafePerformIO hs_bindgen_7affbbac42cd29e2

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_long1@
foreign import ccall unsafe "hs_bindgen_7622c56c9539e04c" hs_bindgen_7622c56c9539e04c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_long1@
hs_bindgen_7622c56c9539e04c :: IO (BG.FunPtr (BG.CInt -> IO BG.CLong))
hs_bindgen_7622c56c9539e04c =
  BG.fromFFIType hs_bindgen_7622c56c9539e04c_base

{-# NOINLINE ret_long1 #-}
{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h 61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1 :: BG.FunPtr (BG.CInt -> IO BG.CLong)
ret_long1 =
  BG.unsafePerformIO hs_bindgen_7622c56c9539e04c

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_long2@
foreign import ccall unsafe "hs_bindgen_815df1fcd6285cb1" hs_bindgen_815df1fcd6285cb1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_long2@
hs_bindgen_815df1fcd6285cb1 :: IO (BG.FunPtr (BG.CInt -> IO BG.CLong))
hs_bindgen_815df1fcd6285cb1 =
  BG.fromFFIType hs_bindgen_815df1fcd6285cb1_base

{-# NOINLINE ret_long2 #-}
{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h 62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2 :: BG.FunPtr (BG.CInt -> IO BG.CLong)
ret_long2 =
  BG.unsafePerformIO hs_bindgen_815df1fcd6285cb1

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_long3@
foreign import ccall unsafe "hs_bindgen_246bedca6a708d23" hs_bindgen_246bedca6a708d23_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_long3@
hs_bindgen_246bedca6a708d23 :: IO (BG.FunPtr (BG.CInt -> IO BG.CULong))
hs_bindgen_246bedca6a708d23 =
  BG.fromFFIType hs_bindgen_246bedca6a708d23_base

{-# NOINLINE ret_long3 #-}
{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h 63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3 :: BG.FunPtr (BG.CInt -> IO BG.CULong)
ret_long3 =
  BG.unsafePerformIO hs_bindgen_246bedca6a708d23

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_float@
foreign import ccall unsafe "hs_bindgen_d0a43141a677f147" hs_bindgen_d0a43141a677f147_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_float@
hs_bindgen_d0a43141a677f147 :: IO (BG.FunPtr (BG.CInt -> IO BG.CFloat))
hs_bindgen_d0a43141a677f147 =
  BG.fromFFIType hs_bindgen_d0a43141a677f147_base

{-# NOINLINE ret_float #-}
{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h 65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float :: BG.FunPtr (BG.CInt -> IO BG.CFloat)
ret_float =
  BG.unsafePerformIO hs_bindgen_d0a43141a677f147

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_double@
foreign import ccall unsafe "hs_bindgen_e39c2a2347167cd5" hs_bindgen_e39c2a2347167cd5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_double@
hs_bindgen_e39c2a2347167cd5 :: IO (BG.FunPtr (BG.CInt -> IO BG.CDouble))
hs_bindgen_e39c2a2347167cd5 =
  BG.fromFFIType hs_bindgen_e39c2a2347167cd5_base

{-# NOINLINE ret_double #-}
{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h 66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double :: BG.FunPtr (BG.CInt -> IO BG.CDouble)
ret_double =
  BG.unsafePerformIO hs_bindgen_e39c2a2347167cd5

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_bool1@
foreign import ccall unsafe "hs_bindgen_2f72fbc044a4bbfd" hs_bindgen_2f72fbc044a4bbfd_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_bool1@
hs_bindgen_2f72fbc044a4bbfd :: IO (BG.FunPtr (BG.CInt -> IO BG.CBool))
hs_bindgen_2f72fbc044a4bbfd =
  BG.fromFFIType hs_bindgen_2f72fbc044a4bbfd_base

{-# NOINLINE ret_bool1 #-}
{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h 67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1 :: BG.FunPtr (BG.CInt -> IO BG.CBool)
ret_bool1 =
  BG.unsafePerformIO hs_bindgen_2f72fbc044a4bbfd

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_struct@
foreign import ccall unsafe "hs_bindgen_3ed00edaff786c0e" hs_bindgen_3ed00edaff786c0e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_struct@
hs_bindgen_3ed00edaff786c0e :: IO (BG.FunPtr (BG.CInt -> IO Some_struct))
hs_bindgen_3ed00edaff786c0e =
  BG.fromFFIType hs_bindgen_3ed00edaff786c0e_base

{-# NOINLINE ret_struct #-}
{-| __C declaration:__ @ret_struct@

    __defined at:__ @macros\/reparse.h 69:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_struct :: BG.FunPtr (BG.CInt -> IO Some_struct)
ret_struct =
  BG.unsafePerformIO hs_bindgen_3ed00edaff786c0e

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_union@
foreign import ccall unsafe "hs_bindgen_1b270595798725a6" hs_bindgen_1b270595798725a6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_union@
hs_bindgen_1b270595798725a6 :: IO (BG.FunPtr (BG.CInt -> IO Some_union))
hs_bindgen_1b270595798725a6 =
  BG.fromFFIType hs_bindgen_1b270595798725a6_base

{-# NOINLINE ret_union #-}
{-| __C declaration:__ @ret_union@

    __defined at:__ @macros\/reparse.h 70:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_union :: BG.FunPtr (BG.CInt -> IO Some_union)
ret_union =
  BG.unsafePerformIO hs_bindgen_1b270595798725a6

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_enum@
foreign import ccall unsafe "hs_bindgen_df1e959d57683966" hs_bindgen_df1e959d57683966_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_enum@
hs_bindgen_df1e959d57683966 :: IO (BG.FunPtr (BG.CInt -> IO Some_enum))
hs_bindgen_df1e959d57683966 =
  BG.fromFFIType hs_bindgen_df1e959d57683966_base

{-# NOINLINE ret_enum #-}
{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h 71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum :: BG.FunPtr (BG.CInt -> IO Some_enum)
ret_enum =
  BG.unsafePerformIO hs_bindgen_df1e959d57683966

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_pointer1@
foreign import ccall unsafe "hs_bindgen_f66df065bdb19e72" hs_bindgen_f66df065bdb19e72_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_pointer1@
hs_bindgen_f66df065bdb19e72 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt)))
hs_bindgen_f66df065bdb19e72 =
  BG.fromFFIType hs_bindgen_f66df065bdb19e72_base

{-# NOINLINE ret_pointer1 #-}
{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h 73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt))
ret_pointer1 =
  BG.unsafePerformIO hs_bindgen_f66df065bdb19e72

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_pointer2@
foreign import ccall unsafe "hs_bindgen_12d9b147f5523ea9" hs_bindgen_12d9b147f5523ea9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_pointer2@
hs_bindgen_12d9b147f5523ea9 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr BG.CInt))))
hs_bindgen_12d9b147f5523ea9 =
  BG.fromFFIType hs_bindgen_12d9b147f5523ea9_base

{-# NOINLINE ret_pointer2 #-}
{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h 74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr BG.CInt)))
ret_pointer2 =
  BG.unsafePerformIO hs_bindgen_12d9b147f5523ea9

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_pointer3@
foreign import ccall unsafe "hs_bindgen_4ac9f0fe826d0748" hs_bindgen_4ac9f0fe826d0748_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_pointer3@
hs_bindgen_4ac9f0fe826d0748 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.Void)))
hs_bindgen_4ac9f0fe826d0748 =
  BG.fromFFIType hs_bindgen_4ac9f0fe826d0748_base

{-# NOINLINE ret_pointer3 #-}
{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h 75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.Void))
ret_pointer3 =
  BG.unsafePerformIO hs_bindgen_4ac9f0fe826d0748

-- __unique:__ @test_macrosreparse_1_empty_Example_get_body1@
foreign import ccall unsafe "hs_bindgen_2974316436a63e20" hs_bindgen_2974316436a63e20_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_body1@
hs_bindgen_2974316436a63e20 :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_2974316436a63e20 =
  BG.fromFFIType hs_bindgen_2974316436a63e20_base

{-# NOINLINE body1 #-}
{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h 79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1 :: BG.FunPtr (BG.CInt -> IO BG.CInt)
body1 =
  BG.unsafePerformIO hs_bindgen_2974316436a63e20

-- __unique:__ @test_macrosreparse_1_empty_Example_get_body2@
foreign import ccall unsafe "hs_bindgen_2b14d27784f519af" hs_bindgen_2b14d27784f519af_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_body2@
hs_bindgen_2b14d27784f519af :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_2b14d27784f519af =
  BG.fromFFIType hs_bindgen_2b14d27784f519af_base

{-# NOINLINE body2 #-}
{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h 80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2 :: BG.FunPtr (IO BG.CInt)
body2 =
  BG.unsafePerformIO hs_bindgen_2b14d27784f519af

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_complex_float@
foreign import ccall unsafe "hs_bindgen_387d68ceb6d006f9" hs_bindgen_387d68ceb6d006f9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_complex_float@
hs_bindgen_387d68ceb6d006f9 :: IO (BG.FunPtr (BG.CInt -> BG.Complex BG.CFloat -> IO ()))
hs_bindgen_387d68ceb6d006f9 =
  BG.fromFFIType hs_bindgen_387d68ceb6d006f9_base

{-# NOINLINE args_complex_float #-}
{-| __C declaration:__ @args_complex_float@

    __defined at:__ @macros\/reparse.h 84:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_float :: BG.FunPtr (BG.CInt -> BG.Complex BG.CFloat -> IO ())
args_complex_float =
  BG.unsafePerformIO hs_bindgen_387d68ceb6d006f9

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_complex_double@
foreign import ccall unsafe "hs_bindgen_a0dd7e51d4c3782c" hs_bindgen_a0dd7e51d4c3782c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_args_complex_double@
hs_bindgen_a0dd7e51d4c3782c :: IO (BG.FunPtr (BG.CInt -> BG.Complex BG.CDouble -> IO ()))
hs_bindgen_a0dd7e51d4c3782c =
  BG.fromFFIType hs_bindgen_a0dd7e51d4c3782c_base

{-# NOINLINE args_complex_double #-}
{-| __C declaration:__ @args_complex_double@

    __defined at:__ @macros\/reparse.h 85:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_double :: BG.FunPtr (BG.CInt -> BG.Complex BG.CDouble -> IO ())
args_complex_double =
  BG.unsafePerformIO hs_bindgen_a0dd7e51d4c3782c

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_complex_float@
foreign import ccall unsafe "hs_bindgen_121af3746ec0fe57" hs_bindgen_121af3746ec0fe57_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_complex_float@
hs_bindgen_121af3746ec0fe57 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Complex BG.CFloat)))
hs_bindgen_121af3746ec0fe57 =
  BG.fromFFIType hs_bindgen_121af3746ec0fe57_base

{-# NOINLINE ret_complex_float #-}
{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @macros\/reparse.h 86:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_float :: BG.FunPtr (BG.CInt -> IO (BG.Complex BG.CFloat))
ret_complex_float =
  BG.unsafePerformIO hs_bindgen_121af3746ec0fe57

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_complex_double@
foreign import ccall unsafe "hs_bindgen_f5f4fa2b769a0807" hs_bindgen_f5f4fa2b769a0807_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_ret_complex_double@
hs_bindgen_f5f4fa2b769a0807 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Complex BG.CDouble)))
hs_bindgen_f5f4fa2b769a0807 =
  BG.fromFFIType hs_bindgen_f5f4fa2b769a0807_base

{-# NOINLINE ret_complex_double #-}
{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @macros\/reparse.h 87:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_double :: BG.FunPtr (BG.CInt -> IO (BG.Complex BG.CDouble))
ret_complex_double =
  BG.unsafePerformIO hs_bindgen_f5f4fa2b769a0807

-- __unique:__ @test_macrosreparse_1_empty_Example_get_bespoke_args1@
foreign import ccall unsafe "hs_bindgen_6c590995b92657c7" hs_bindgen_6c590995b92657c7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_bespoke_args1@
hs_bindgen_6c590995b92657c7 :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_6c590995b92657c7 =
  BG.fromFFIType hs_bindgen_6c590995b92657c7_base

{-# NOINLINE bespoke_args1 #-}
{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @macros\/reparse.h 94:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args1 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
bespoke_args1 =
  BG.unsafePerformIO hs_bindgen_6c590995b92657c7

-- __unique:__ @test_macrosreparse_1_empty_Example_get_bespoke_args2@
foreign import ccall unsafe "hs_bindgen_b871d8c50d13d10a" hs_bindgen_b871d8c50d13d10a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_bespoke_args2@
hs_bindgen_b871d8c50d13d10a :: IO (BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_b871d8c50d13d10a =
  BG.fromFFIType hs_bindgen_b871d8c50d13d10a_base

{-# NOINLINE bespoke_args2 #-}
{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @macros\/reparse.h 95:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args2 :: BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ())
bespoke_args2 =
  BG.unsafePerformIO hs_bindgen_b871d8c50d13d10a

-- __unique:__ @test_macrosreparse_1_empty_Example_get_bespoke_ret1@
foreign import ccall unsafe "hs_bindgen_457eeaee9b36d630" hs_bindgen_457eeaee9b36d630_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_bespoke_ret1@
hs_bindgen_457eeaee9b36d630 :: IO (BG.FunPtr (BG.CInt -> IO BG.CBool))
hs_bindgen_457eeaee9b36d630 =
  BG.fromFFIType hs_bindgen_457eeaee9b36d630_base

{-# NOINLINE bespoke_ret1 #-}
{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h 97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1 :: BG.FunPtr (BG.CInt -> IO BG.CBool)
bespoke_ret1 =
  BG.unsafePerformIO hs_bindgen_457eeaee9b36d630

-- __unique:__ @test_macrosreparse_1_empty_Example_get_bespoke_ret2@
foreign import ccall unsafe "hs_bindgen_11a75beb43d5fc2e" hs_bindgen_11a75beb43d5fc2e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_bespoke_ret2@
hs_bindgen_11a75beb43d5fc2e :: IO (BG.FunPtr (BG.CInt -> IO HsBindgen.Runtime.LibC.CSize))
hs_bindgen_11a75beb43d5fc2e =
  BG.fromFFIType hs_bindgen_11a75beb43d5fc2e_base

{-# NOINLINE bespoke_ret2 #-}
{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h 98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2 :: BG.FunPtr (BG.CInt -> IO HsBindgen.Runtime.LibC.CSize)
bespoke_ret2 =
  BG.unsafePerformIO hs_bindgen_11a75beb43d5fc2e

-- __unique:__ @test_macrosreparse_1_empty_Example_get_arr_args1@
foreign import ccall unsafe "hs_bindgen_618624c6e6b83a22" hs_bindgen_618624c6e6b83a22_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_arr_args1@
hs_bindgen_618624c6e6b83a22 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO ()))
hs_bindgen_618624c6e6b83a22 =
  BG.fromFFIType hs_bindgen_618624c6e6b83a22_base

{-# NOINLINE arr_args1 #-}
{-| Arrays

    __C declaration:__ @arr_args1@

    __defined at:__ @macros\/reparse.h 104:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args1 :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO ())
arr_args1 =
  BG.unsafePerformIO hs_bindgen_618624c6e6b83a22

-- __unique:__ @test_macrosreparse_1_empty_Example_get_arr_args2@
foreign import ccall unsafe "hs_bindgen_2ad0461638177d93" hs_bindgen_2ad0461638177d93_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_arr_args2@
hs_bindgen_2ad0461638177d93 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt))) -> IO ()))
hs_bindgen_2ad0461638177d93 =
  BG.fromFFIType hs_bindgen_2ad0461638177d93_base

{-# NOINLINE arr_args2 #-}
{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h 105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2 :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt))) -> IO ())
arr_args2 =
  BG.unsafePerformIO hs_bindgen_2ad0461638177d93

-- __unique:__ @test_macrosreparse_1_empty_Example_get_arr_args3@
foreign import ccall unsafe "hs_bindgen_c77b1b370d4c5f4b" hs_bindgen_c77b1b370d4c5f4b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_arr_args3@
hs_bindgen_c77b1b370d4c5f4b :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt)) -> IO ()))
hs_bindgen_c77b1b370d4c5f4b =
  BG.fromFFIType hs_bindgen_c77b1b370d4c5f4b_base

{-# NOINLINE arr_args3 #-}
{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h 106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3 :: BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt)) -> IO ())
arr_args3 =
  BG.unsafePerformIO hs_bindgen_c77b1b370d4c5f4b

-- __unique:__ @test_macrosreparse_1_empty_Example_get_arr_args4@
foreign import ccall unsafe "hs_bindgen_de199c863fb8ac12" hs_bindgen_de199c863fb8ac12_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_arr_args4@
hs_bindgen_de199c863fb8ac12 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt))) -> IO ()))
hs_bindgen_de199c863fb8ac12 =
  BG.fromFFIType hs_bindgen_de199c863fb8ac12_base

{-# NOINLINE arr_args4 #-}
{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h 107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4 :: BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt))) -> IO ())
arr_args4 =
  BG.unsafePerformIO hs_bindgen_de199c863fb8ac12

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_args1@
foreign import ccall unsafe "hs_bindgen_7fc280d0a83fec73" hs_bindgen_7fc280d0a83fec73_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_args1@
hs_bindgen_7fc280d0a83fec73 :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (IO ()) -> IO ()))
hs_bindgen_7fc280d0a83fec73 =
  BG.fromFFIType hs_bindgen_7fc280d0a83fec73_base

{-# NOINLINE funptr_args1 #-}
{-| Function pointers

    __C declaration:__ @funptr_args1@

    __defined at:__ @macros\/reparse.h 126:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args1 :: BG.FunPtr (BG.CInt -> BG.FunPtr (IO ()) -> IO ())
funptr_args1 =
  BG.unsafePerformIO hs_bindgen_7fc280d0a83fec73

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_args2@
foreign import ccall unsafe "hs_bindgen_9beb81169b296475" hs_bindgen_9beb81169b296475_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_args2@
hs_bindgen_9beb81169b296475 :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (IO BG.CInt) -> IO ()))
hs_bindgen_9beb81169b296475 =
  BG.fromFFIType hs_bindgen_9beb81169b296475_base

{-# NOINLINE funptr_args2 #-}
{-| __C declaration:__ @funptr_args2@

    __defined at:__ @macros\/reparse.h 127:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args2 :: BG.FunPtr (BG.CInt -> BG.FunPtr (IO BG.CInt) -> IO ())
funptr_args2 =
  BG.unsafePerformIO hs_bindgen_9beb81169b296475

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_args3@
foreign import ccall unsafe "hs_bindgen_d85158bc0b976b5a" hs_bindgen_d85158bc0b976b5a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_args3@
hs_bindgen_d85158bc0b976b5a :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> IO ()) -> IO ()))
hs_bindgen_d85158bc0b976b5a =
  BG.fromFFIType hs_bindgen_d85158bc0b976b5a_base

{-# NOINLINE funptr_args3 #-}
{-| __C declaration:__ @funptr_args3@

    __defined at:__ @macros\/reparse.h 128:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args3 :: BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> IO ()) -> IO ())
funptr_args3 =
  BG.unsafePerformIO hs_bindgen_d85158bc0b976b5a

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_args4@
foreign import ccall unsafe "hs_bindgen_0b011779ec7227d9" hs_bindgen_0b011779ec7227d9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_args4@
hs_bindgen_0b011779ec7227d9 :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar) -> IO ()))
hs_bindgen_0b011779ec7227d9 =
  BG.fromFFIType hs_bindgen_0b011779ec7227d9_base

{-# NOINLINE funptr_args4 #-}
{-| __C declaration:__ @funptr_args4@

    __defined at:__ @macros\/reparse.h 129:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args4 :: BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar) -> IO ())
funptr_args4 =
  BG.unsafePerformIO hs_bindgen_0b011779ec7227d9

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_args5@
foreign import ccall unsafe "hs_bindgen_d358fcc92cb10cf3" hs_bindgen_d358fcc92cb10cf3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_args5@
hs_bindgen_d358fcc92cb10cf3 :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)) -> IO ()))
hs_bindgen_d358fcc92cb10cf3 =
  BG.fromFFIType hs_bindgen_d358fcc92cb10cf3_base

{-# NOINLINE funptr_args5 #-}
{-| __C declaration:__ @funptr_args5@

    __defined at:__ @macros\/reparse.h 130:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args5 :: BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)) -> IO ())
funptr_args5 =
  BG.unsafePerformIO hs_bindgen_d358fcc92cb10cf3

-- __unique:__ @test_macrosreparse_1_empty_Example_get_comments1@
foreign import ccall unsafe "hs_bindgen_0fd159d332801a82" hs_bindgen_0fd159d332801a82_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_comments1@
hs_bindgen_0fd159d332801a82 :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_0fd159d332801a82 =
  BG.fromFFIType hs_bindgen_0fd159d332801a82_base

{-# NOINLINE comments1 #-}
{-| Comments in awkward places

    (Prior to language-c we failed to parse there.)

    __C declaration:__ @comments1@

    __defined at:__ @macros\/reparse.h 144:25@

    __exported by:__ @macros\/reparse.h@
-}
comments1 :: BG.FunPtr (BG.CInt -> IO ())
comments1 =
  BG.unsafePerformIO hs_bindgen_0fd159d332801a82

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_prim_before1@
foreign import ccall unsafe "hs_bindgen_a40876f663bcef73" hs_bindgen_a40876f663bcef73_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_prim_before1@
hs_bindgen_a40876f663bcef73 :: IO (BG.FunPtr (BG.CInt -> BG.CChar -> IO ()))
hs_bindgen_a40876f663bcef73 =
  BG.fromFFIType hs_bindgen_a40876f663bcef73_base

{-# NOINLINE const_prim_before1 #-}
{-| @const@ qualifier

    NOTE: These were not parsed correctly prior to the switch to language-c.

    __C declaration:__ @const_prim_before1@

    __defined at:__ @macros\/reparse.h 177:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before1 :: BG.FunPtr (BG.CInt -> BG.CChar -> IO ())
const_prim_before1 =
  BG.unsafePerformIO hs_bindgen_a40876f663bcef73

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_prim_before2@
foreign import ccall unsafe "hs_bindgen_dc5eb25e2e741ab6" hs_bindgen_dc5eb25e2e741ab6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_prim_before2@
hs_bindgen_dc5eb25e2e741ab6 :: IO (BG.FunPtr (BG.CInt -> BG.CSChar -> IO ()))
hs_bindgen_dc5eb25e2e741ab6 =
  BG.fromFFIType hs_bindgen_dc5eb25e2e741ab6_base

{-# NOINLINE const_prim_before2 #-}
{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @macros\/reparse.h 178:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before2 :: BG.FunPtr (BG.CInt -> BG.CSChar -> IO ())
const_prim_before2 =
  BG.unsafePerformIO hs_bindgen_dc5eb25e2e741ab6

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_prim_before3@
foreign import ccall unsafe "hs_bindgen_9718326f589a56a0" hs_bindgen_9718326f589a56a0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_prim_before3@
hs_bindgen_9718326f589a56a0 :: IO (BG.FunPtr (BG.CInt -> BG.CUChar -> IO ()))
hs_bindgen_9718326f589a56a0 =
  BG.fromFFIType hs_bindgen_9718326f589a56a0_base

{-# NOINLINE const_prim_before3 #-}
{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @macros\/reparse.h 179:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before3 :: BG.FunPtr (BG.CInt -> BG.CUChar -> IO ())
const_prim_before3 =
  BG.unsafePerformIO hs_bindgen_9718326f589a56a0

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_prim_after1@
foreign import ccall unsafe "hs_bindgen_cde59c88cf7b3db3" hs_bindgen_cde59c88cf7b3db3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_prim_after1@
hs_bindgen_cde59c88cf7b3db3 :: IO (BG.FunPtr (BG.CInt -> BG.CChar -> IO ()))
hs_bindgen_cde59c88cf7b3db3 =
  BG.fromFFIType hs_bindgen_cde59c88cf7b3db3_base

{-# NOINLINE const_prim_after1 #-}
{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @macros\/reparse.h 180:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after1 :: BG.FunPtr (BG.CInt -> BG.CChar -> IO ())
const_prim_after1 =
  BG.unsafePerformIO hs_bindgen_cde59c88cf7b3db3

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_prim_after2@
foreign import ccall unsafe "hs_bindgen_d5ab011afdbef099" hs_bindgen_d5ab011afdbef099_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_prim_after2@
hs_bindgen_d5ab011afdbef099 :: IO (BG.FunPtr (BG.CInt -> BG.CSChar -> IO ()))
hs_bindgen_d5ab011afdbef099 =
  BG.fromFFIType hs_bindgen_d5ab011afdbef099_base

{-# NOINLINE const_prim_after2 #-}
{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @macros\/reparse.h 181:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after2 :: BG.FunPtr (BG.CInt -> BG.CSChar -> IO ())
const_prim_after2 =
  BG.unsafePerformIO hs_bindgen_d5ab011afdbef099

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_prim_after3@
foreign import ccall unsafe "hs_bindgen_30f31bede8bfc87c" hs_bindgen_30f31bede8bfc87c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_prim_after3@
hs_bindgen_30f31bede8bfc87c :: IO (BG.FunPtr (BG.CInt -> BG.CUChar -> IO ()))
hs_bindgen_30f31bede8bfc87c =
  BG.fromFFIType hs_bindgen_30f31bede8bfc87c_base

{-# NOINLINE const_prim_after3 #-}
{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @macros\/reparse.h 182:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after3 :: BG.FunPtr (BG.CInt -> BG.CUChar -> IO ())
const_prim_after3 =
  BG.unsafePerformIO hs_bindgen_30f31bede8bfc87c

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_before1@
foreign import ccall unsafe "hs_bindgen_6cc702c24ac0eeb2" hs_bindgen_6cc702c24ac0eeb2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_before1@
hs_bindgen_6cc702c24ac0eeb2 :: IO (BG.FunPtr (BG.CInt -> BG.CFloat -> IO ()))
hs_bindgen_6cc702c24ac0eeb2 =
  BG.fromFFIType hs_bindgen_6cc702c24ac0eeb2_base

{-# NOINLINE const_withoutSign_before1 #-}
{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @macros\/reparse.h 186:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before1 :: BG.FunPtr (BG.CInt -> BG.CFloat -> IO ())
const_withoutSign_before1 =
  BG.unsafePerformIO hs_bindgen_6cc702c24ac0eeb2

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_before2@
foreign import ccall unsafe "hs_bindgen_e77ba4a5962cc68c" hs_bindgen_e77ba4a5962cc68c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_before2@
hs_bindgen_e77ba4a5962cc68c :: IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO ()))
hs_bindgen_e77ba4a5962cc68c =
  BG.fromFFIType hs_bindgen_e77ba4a5962cc68c_base

{-# NOINLINE const_withoutSign_before2 #-}
{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @macros\/reparse.h 187:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before2 :: BG.FunPtr (BG.CInt -> BG.CDouble -> IO ())
const_withoutSign_before2 =
  BG.unsafePerformIO hs_bindgen_e77ba4a5962cc68c

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_before3@
foreign import ccall unsafe "hs_bindgen_c4d9de5a3852ba73" hs_bindgen_c4d9de5a3852ba73_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_before3@
hs_bindgen_c4d9de5a3852ba73 :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_c4d9de5a3852ba73 =
  BG.fromFFIType hs_bindgen_c4d9de5a3852ba73_base

{-# NOINLINE const_withoutSign_before3 #-}
{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @macros\/reparse.h 188:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before3 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
const_withoutSign_before3 =
  BG.unsafePerformIO hs_bindgen_c4d9de5a3852ba73

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_before4@
foreign import ccall unsafe "hs_bindgen_585a37cd5f43f5da" hs_bindgen_585a37cd5f43f5da_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_before4@
hs_bindgen_585a37cd5f43f5da :: IO (BG.FunPtr (BG.CInt -> Some_struct -> IO ()))
hs_bindgen_585a37cd5f43f5da =
  BG.fromFFIType hs_bindgen_585a37cd5f43f5da_base

{-# NOINLINE const_withoutSign_before4 #-}
{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @macros\/reparse.h 189:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before4 :: BG.FunPtr (BG.CInt -> Some_struct -> IO ())
const_withoutSign_before4 =
  BG.unsafePerformIO hs_bindgen_585a37cd5f43f5da

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_before5@
foreign import ccall unsafe "hs_bindgen_655c28dba28a9a53" hs_bindgen_655c28dba28a9a53_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_before5@
hs_bindgen_655c28dba28a9a53 :: IO (BG.FunPtr (BG.CInt -> Some_union -> IO ()))
hs_bindgen_655c28dba28a9a53 =
  BG.fromFFIType hs_bindgen_655c28dba28a9a53_base

{-# NOINLINE const_withoutSign_before5 #-}
{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @macros\/reparse.h 190:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before5 :: BG.FunPtr (BG.CInt -> Some_union -> IO ())
const_withoutSign_before5 =
  BG.unsafePerformIO hs_bindgen_655c28dba28a9a53

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_before6@
foreign import ccall unsafe "hs_bindgen_9e08bdc34bafeee1" hs_bindgen_9e08bdc34bafeee1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_before6@
hs_bindgen_9e08bdc34bafeee1 :: IO (BG.FunPtr (BG.CInt -> Some_enum -> IO ()))
hs_bindgen_9e08bdc34bafeee1 =
  BG.fromFFIType hs_bindgen_9e08bdc34bafeee1_base

{-# NOINLINE const_withoutSign_before6 #-}
{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @macros\/reparse.h 191:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before6 :: BG.FunPtr (BG.CInt -> Some_enum -> IO ())
const_withoutSign_before6 =
  BG.unsafePerformIO hs_bindgen_9e08bdc34bafeee1

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_before7@
foreign import ccall unsafe "hs_bindgen_5736b6db8251bdd5" hs_bindgen_5736b6db8251bdd5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_before7@
hs_bindgen_5736b6db8251bdd5 :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_5736b6db8251bdd5 =
  BG.fromFFIType hs_bindgen_5736b6db8251bdd5_base

{-# NOINLINE const_withoutSign_before7 #-}
{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @macros\/reparse.h 192:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before7 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
const_withoutSign_before7 =
  BG.unsafePerformIO hs_bindgen_5736b6db8251bdd5

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_before8@
foreign import ccall unsafe "hs_bindgen_c398560b654a8d9d" hs_bindgen_c398560b654a8d9d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_before8@
hs_bindgen_c398560b654a8d9d :: IO (BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_c398560b654a8d9d =
  BG.fromFFIType hs_bindgen_c398560b654a8d9d_base

{-# NOINLINE const_withoutSign_before8 #-}
{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @macros\/reparse.h 193:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before8 :: BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ())
const_withoutSign_before8 =
  BG.unsafePerformIO hs_bindgen_c398560b654a8d9d

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_after1@
foreign import ccall unsafe "hs_bindgen_10bff078d94c8cd0" hs_bindgen_10bff078d94c8cd0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_after1@
hs_bindgen_10bff078d94c8cd0 :: IO (BG.FunPtr (BG.CInt -> BG.CFloat -> IO ()))
hs_bindgen_10bff078d94c8cd0 =
  BG.fromFFIType hs_bindgen_10bff078d94c8cd0_base

{-# NOINLINE const_withoutSign_after1 #-}
{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @macros\/reparse.h 195:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after1 :: BG.FunPtr (BG.CInt -> BG.CFloat -> IO ())
const_withoutSign_after1 =
  BG.unsafePerformIO hs_bindgen_10bff078d94c8cd0

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_after2@
foreign import ccall unsafe "hs_bindgen_b75e6b8d1be2004b" hs_bindgen_b75e6b8d1be2004b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_after2@
hs_bindgen_b75e6b8d1be2004b :: IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO ()))
hs_bindgen_b75e6b8d1be2004b =
  BG.fromFFIType hs_bindgen_b75e6b8d1be2004b_base

{-# NOINLINE const_withoutSign_after2 #-}
{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @macros\/reparse.h 196:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after2 :: BG.FunPtr (BG.CInt -> BG.CDouble -> IO ())
const_withoutSign_after2 =
  BG.unsafePerformIO hs_bindgen_b75e6b8d1be2004b

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_after3@
foreign import ccall unsafe "hs_bindgen_60f26b5a2f8cf394" hs_bindgen_60f26b5a2f8cf394_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_after3@
hs_bindgen_60f26b5a2f8cf394 :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_60f26b5a2f8cf394 =
  BG.fromFFIType hs_bindgen_60f26b5a2f8cf394_base

{-# NOINLINE const_withoutSign_after3 #-}
{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @macros\/reparse.h 197:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after3 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
const_withoutSign_after3 =
  BG.unsafePerformIO hs_bindgen_60f26b5a2f8cf394

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_after4@
foreign import ccall unsafe "hs_bindgen_0503ec9e78e10edf" hs_bindgen_0503ec9e78e10edf_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_after4@
hs_bindgen_0503ec9e78e10edf :: IO (BG.FunPtr (BG.CInt -> Some_struct -> IO ()))
hs_bindgen_0503ec9e78e10edf =
  BG.fromFFIType hs_bindgen_0503ec9e78e10edf_base

{-# NOINLINE const_withoutSign_after4 #-}
{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @macros\/reparse.h 198:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after4 :: BG.FunPtr (BG.CInt -> Some_struct -> IO ())
const_withoutSign_after4 =
  BG.unsafePerformIO hs_bindgen_0503ec9e78e10edf

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_after5@
foreign import ccall unsafe "hs_bindgen_1657dae083620adf" hs_bindgen_1657dae083620adf_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_after5@
hs_bindgen_1657dae083620adf :: IO (BG.FunPtr (BG.CInt -> Some_union -> IO ()))
hs_bindgen_1657dae083620adf =
  BG.fromFFIType hs_bindgen_1657dae083620adf_base

{-# NOINLINE const_withoutSign_after5 #-}
{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @macros\/reparse.h 199:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after5 :: BG.FunPtr (BG.CInt -> Some_union -> IO ())
const_withoutSign_after5 =
  BG.unsafePerformIO hs_bindgen_1657dae083620adf

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_after6@
foreign import ccall unsafe "hs_bindgen_44c293b62ff2015d" hs_bindgen_44c293b62ff2015d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_after6@
hs_bindgen_44c293b62ff2015d :: IO (BG.FunPtr (BG.CInt -> Some_enum -> IO ()))
hs_bindgen_44c293b62ff2015d =
  BG.fromFFIType hs_bindgen_44c293b62ff2015d_base

{-# NOINLINE const_withoutSign_after6 #-}
{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @macros\/reparse.h 200:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after6 :: BG.FunPtr (BG.CInt -> Some_enum -> IO ())
const_withoutSign_after6 =
  BG.unsafePerformIO hs_bindgen_44c293b62ff2015d

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_after7@
foreign import ccall unsafe "hs_bindgen_ed406d5edbd28ec1" hs_bindgen_ed406d5edbd28ec1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_after7@
hs_bindgen_ed406d5edbd28ec1 :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_ed406d5edbd28ec1 =
  BG.fromFFIType hs_bindgen_ed406d5edbd28ec1_base

{-# NOINLINE const_withoutSign_after7 #-}
{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @macros\/reparse.h 201:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after7 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
const_withoutSign_after7 =
  BG.unsafePerformIO hs_bindgen_ed406d5edbd28ec1

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_after8@
foreign import ccall unsafe "hs_bindgen_2cca685d1adca496" hs_bindgen_2cca685d1adca496_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_withoutSign_after8@
hs_bindgen_2cca685d1adca496 :: IO (BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_2cca685d1adca496 =
  BG.fromFFIType hs_bindgen_2cca685d1adca496_base

{-# NOINLINE const_withoutSign_after8 #-}
{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @macros\/reparse.h 202:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after8 :: BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ())
const_withoutSign_after8 =
  BG.unsafePerformIO hs_bindgen_2cca685d1adca496

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_args1@
foreign import ccall unsafe "hs_bindgen_5da738696e5beb04" hs_bindgen_5da738696e5beb04_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_args1@
hs_bindgen_5da738696e5beb04 :: IO (BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_5da738696e5beb04 =
  BG.fromFFIType hs_bindgen_5da738696e5beb04_base

{-# NOINLINE const_pointers_args1 #-}
{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @macros\/reparse.h 206:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args1 :: BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args1 =
  BG.unsafePerformIO hs_bindgen_5da738696e5beb04

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_args2@
foreign import ccall unsafe "hs_bindgen_783634f682149eb7" hs_bindgen_783634f682149eb7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_args2@
hs_bindgen_783634f682149eb7 :: IO (BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_783634f682149eb7 =
  BG.fromFFIType hs_bindgen_783634f682149eb7_base

{-# NOINLINE const_pointers_args2 #-}
{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @macros\/reparse.h 207:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args2 :: BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args2 =
  BG.unsafePerformIO hs_bindgen_783634f682149eb7

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_args3@
foreign import ccall unsafe "hs_bindgen_e8c26ca0eb024ce8" hs_bindgen_e8c26ca0eb024ce8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_args3@
hs_bindgen_e8c26ca0eb024ce8 :: IO (BG.FunPtr (BG.CInt -> BG.Ptr BG.CInt -> IO ()))
hs_bindgen_e8c26ca0eb024ce8 =
  BG.fromFFIType hs_bindgen_e8c26ca0eb024ce8_base

{-# NOINLINE const_pointers_args3 #-}
{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @macros\/reparse.h 208:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args3 :: BG.FunPtr (BG.CInt -> BG.Ptr BG.CInt -> IO ())
const_pointers_args3 =
  BG.unsafePerformIO hs_bindgen_e8c26ca0eb024ce8

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_args4@
foreign import ccall unsafe "hs_bindgen_21e206a694593eb1" hs_bindgen_21e206a694593eb1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_args4@
hs_bindgen_21e206a694593eb1 :: IO (BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_21e206a694593eb1 =
  BG.fromFFIType hs_bindgen_21e206a694593eb1_base

{-# NOINLINE const_pointers_args4 #-}
{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @macros\/reparse.h 209:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args4 :: BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args4 =
  BG.unsafePerformIO hs_bindgen_21e206a694593eb1

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_args5@
foreign import ccall unsafe "hs_bindgen_cc3a8f368db00421" hs_bindgen_cc3a8f368db00421_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_args5@
hs_bindgen_cc3a8f368db00421 :: IO (BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_cc3a8f368db00421 =
  BG.fromFFIType hs_bindgen_cc3a8f368db00421_base

{-# NOINLINE const_pointers_args5 #-}
{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @macros\/reparse.h 210:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args5 :: BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args5 =
  BG.unsafePerformIO hs_bindgen_cc3a8f368db00421

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_ret1@
foreign import ccall unsafe "hs_bindgen_a9979680c0124010" hs_bindgen_a9979680c0124010_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_ret1@
hs_bindgen_a9979680c0124010 :: IO (BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_a9979680c0124010 =
  BG.fromFFIType hs_bindgen_a9979680c0124010_base

{-# NOINLINE const_pointers_ret1 #-}
{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h 212:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1 :: BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret1 =
  BG.unsafePerformIO hs_bindgen_a9979680c0124010

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_ret2@
foreign import ccall unsafe "hs_bindgen_c865f82ab69a4a3e" hs_bindgen_c865f82ab69a4a3e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_ret2@
hs_bindgen_c865f82ab69a4a3e :: IO (BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_c865f82ab69a4a3e =
  BG.fromFFIType hs_bindgen_c865f82ab69a4a3e_base

{-# NOINLINE const_pointers_ret2 #-}
{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h 213:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2 :: BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret2 =
  BG.unsafePerformIO hs_bindgen_c865f82ab69a4a3e

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_ret3@
foreign import ccall unsafe "hs_bindgen_61570ab40d22e404" hs_bindgen_61570ab40d22e404_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_ret3@
hs_bindgen_61570ab40d22e404 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt)))
hs_bindgen_61570ab40d22e404 =
  BG.fromFFIType hs_bindgen_61570ab40d22e404_base

{-# NOINLINE const_pointers_ret3 #-}
{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h 214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt))
const_pointers_ret3 =
  BG.unsafePerformIO hs_bindgen_61570ab40d22e404

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_ret4@
foreign import ccall unsafe "hs_bindgen_7d02f676d3a68181" hs_bindgen_7d02f676d3a68181_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_ret4@
hs_bindgen_7d02f676d3a68181 :: IO (BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_7d02f676d3a68181 =
  BG.fromFFIType hs_bindgen_7d02f676d3a68181_base

{-# NOINLINE const_pointers_ret4 #-}
{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h 215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4 :: BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret4 =
  BG.unsafePerformIO hs_bindgen_7d02f676d3a68181

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_ret5@
foreign import ccall unsafe "hs_bindgen_3a389377331b29b9" hs_bindgen_3a389377331b29b9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_pointers_ret5@
hs_bindgen_3a389377331b29b9 :: IO (BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_3a389377331b29b9 =
  BG.fromFFIType hs_bindgen_3a389377331b29b9_base

{-# NOINLINE const_pointers_ret5 #-}
{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h 216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5 :: BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret5 =
  BG.unsafePerformIO hs_bindgen_3a389377331b29b9

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_array_elem1@
foreign import ccall unsafe "hs_bindgen_4810bd323f24b8d0" hs_bindgen_4810bd323f24b8d0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_array_elem1@
hs_bindgen_4810bd323f24b8d0 :: IO (BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO ()))
hs_bindgen_4810bd323f24b8d0 =
  BG.fromFFIType hs_bindgen_4810bd323f24b8d0_base

{-# NOINLINE const_array_elem1 #-}
{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h 244:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 :: BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO ())
const_array_elem1 =
  BG.unsafePerformIO hs_bindgen_4810bd323f24b8d0

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_array_elem2@
foreign import ccall unsafe "hs_bindgen_8bed93eafa056c5e" hs_bindgen_8bed93eafa056c5e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_array_elem2@
hs_bindgen_8bed93eafa056c5e :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt))) -> IO ()))
hs_bindgen_8bed93eafa056c5e =
  BG.fromFFIType hs_bindgen_8bed93eafa056c5e_base

{-# NOINLINE const_array_elem2 #-}
{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h 245:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2 :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt))) -> IO ())
const_array_elem2 =
  BG.unsafePerformIO hs_bindgen_8bed93eafa056c5e

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_array_elem3@
foreign import ccall unsafe "hs_bindgen_c569d0da33bf00e3" hs_bindgen_c569d0da33bf00e3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const_array_elem3@
hs_bindgen_c569d0da33bf00e3 :: IO (BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt))) -> IO ()))
hs_bindgen_c569d0da33bf00e3 =
  BG.fromFFIType hs_bindgen_c569d0da33bf00e3_base

{-# NOINLINE const_array_elem3 #-}
{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h 246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 :: BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt))) -> IO ())
const_array_elem3 =
  BG.unsafePerformIO hs_bindgen_c569d0da33bf00e3

-- __unique:__ @test_macrosreparse_1_empty_Example_get_noParams1@
foreign import ccall unsafe "hs_bindgen_91d4c9002424f82d" hs_bindgen_91d4c9002424f82d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_noParams1@
hs_bindgen_91d4c9002424f82d :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_91d4c9002424f82d =
  BG.fromFFIType hs_bindgen_91d4c9002424f82d_base

{-# NOINLINE noParams1 #-}
{-| Other examples we reparsed /incorrectly/ before language-c

    __C declaration:__ @noParams1@

    __defined at:__ @macros\/reparse.h 254:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams1 :: BG.FunPtr (IO BG.CInt)
noParams1 =
  BG.unsafePerformIO hs_bindgen_91d4c9002424f82d

-- __unique:__ @test_macrosreparse_1_empty_Example_get_noParams2@
foreign import ccall unsafe "hs_bindgen_a317809988dfeb84" hs_bindgen_a317809988dfeb84_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_noParams2@
hs_bindgen_a317809988dfeb84 :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_a317809988dfeb84 =
  BG.fromFFIType hs_bindgen_a317809988dfeb84_base

{-# NOINLINE noParams2 #-}
{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h 255:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2 :: BG.FunPtr (IO BG.CInt)
noParams2 =
  BG.unsafePerformIO hs_bindgen_a317809988dfeb84

-- __unique:__ @test_macrosreparse_1_empty_Example_get_noParams3@
foreign import ccall unsafe "hs_bindgen_c23ef724e4f315be" hs_bindgen_c23ef724e4f315be_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_noParams3@
hs_bindgen_c23ef724e4f315be :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (IO BG.CInt) -> IO ()))
hs_bindgen_c23ef724e4f315be =
  BG.fromFFIType hs_bindgen_c23ef724e4f315be_base

{-# NOINLINE noParams3 #-}
{-| __C declaration:__ @noParams3@

    __defined at:__ @macros\/reparse.h 256:6@

    __exported by:__ @macros\/reparse.h@
-}
noParams3 :: BG.FunPtr (BG.CInt -> BG.FunPtr (IO BG.CInt) -> IO ())
noParams3 =
  BG.unsafePerformIO hs_bindgen_c23ef724e4f315be

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret1@
foreign import ccall unsafe "hs_bindgen_660d89308b2b5a69" hs_bindgen_660d89308b2b5a69_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret1@
hs_bindgen_660d89308b2b5a69 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (IO ()))))
hs_bindgen_660d89308b2b5a69 =
  BG.fromFFIType hs_bindgen_660d89308b2b5a69_base

{-# NOINLINE funptr_ret1 #-}
{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h 260:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (IO ())))
funptr_ret1 =
  BG.unsafePerformIO hs_bindgen_660d89308b2b5a69

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret2@
foreign import ccall unsafe "hs_bindgen_bc7009eeb6b86418" hs_bindgen_bc7009eeb6b86418_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret2@
hs_bindgen_bc7009eeb6b86418 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (IO BG.CInt))))
hs_bindgen_bc7009eeb6b86418 =
  BG.fromFFIType hs_bindgen_bc7009eeb6b86418_base

{-# NOINLINE funptr_ret2 #-}
{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h 261:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (IO BG.CInt)))
funptr_ret2 =
  BG.unsafePerformIO hs_bindgen_bc7009eeb6b86418

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret3@
foreign import ccall unsafe "hs_bindgen_565629b4e4c20a7e" hs_bindgen_565629b4e4c20a7e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret3@
hs_bindgen_565629b4e4c20a7e :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> IO ()))))
hs_bindgen_565629b4e4c20a7e =
  BG.fromFFIType hs_bindgen_565629b4e4c20a7e_base

{-# NOINLINE funptr_ret3 #-}
{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h 262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> IO ())))
funptr_ret3 =
  BG.unsafePerformIO hs_bindgen_565629b4e4c20a7e

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret4@
foreign import ccall unsafe "hs_bindgen_3563760f061700c9" hs_bindgen_3563760f061700c9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret4@
hs_bindgen_3563760f061700c9 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))))
hs_bindgen_3563760f061700c9 =
  BG.fromFFIType hs_bindgen_3563760f061700c9_base

{-# NOINLINE funptr_ret4 #-}
{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h 263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar)))
funptr_ret4 =
  BG.unsafePerformIO hs_bindgen_3563760f061700c9

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret5@
foreign import ccall unsafe "hs_bindgen_65c114a912faa4d1" hs_bindgen_65c114a912faa4d1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret5@
hs_bindgen_65c114a912faa4d1 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))))
hs_bindgen_65c114a912faa4d1 =
  BG.fromFFIType hs_bindgen_65c114a912faa4d1_base

{-# NOINLINE funptr_ret5 #-}
{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h 267:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))))
funptr_ret5 =
  BG.unsafePerformIO hs_bindgen_65c114a912faa4d1

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret6@
foreign import ccall unsafe "hs_bindgen_13dce2f12ceb5ef5" hs_bindgen_13dce2f12ceb5ef5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret6@
hs_bindgen_13dce2f12ceb5ef5 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_13dce2f12ceb5ef5 =
  BG.fromFFIType hs_bindgen_13dce2f12ceb5ef5_base

{-# NOINLINE funptr_ret6 #-}
{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h 268:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret6 =
  BG.unsafePerformIO hs_bindgen_13dce2f12ceb5ef5

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret7@
foreign import ccall unsafe "hs_bindgen_e69f4bc4bd10259c" hs_bindgen_e69f4bc4bd10259c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret7@
hs_bindgen_e69f4bc4bd10259c :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_e69f4bc4bd10259c =
  BG.fromFFIType hs_bindgen_e69f4bc4bd10259c_base

{-# NOINLINE funptr_ret7 #-}
{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h 269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret7 =
  BG.unsafePerformIO hs_bindgen_e69f4bc4bd10259c

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret8@
foreign import ccall unsafe "hs_bindgen_f06ecca1a94b60e8" hs_bindgen_f06ecca1a94b60e8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret8@
hs_bindgen_f06ecca1a94b60e8 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))))
hs_bindgen_f06ecca1a94b60e8 =
  BG.fromFFIType hs_bindgen_f06ecca1a94b60e8_base

{-# NOINLINE funptr_ret8 #-}
{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h 270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))))
funptr_ret8 =
  BG.unsafePerformIO hs_bindgen_f06ecca1a94b60e8

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret9@
foreign import ccall unsafe "hs_bindgen_65e9b457479db3fb" hs_bindgen_65e9b457479db3fb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret9@
hs_bindgen_65e9b457479db3fb :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_65e9b457479db3fb =
  BG.fromFFIType hs_bindgen_65e9b457479db3fb_base

{-# NOINLINE funptr_ret9 #-}
{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h 271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret9 =
  BG.unsafePerformIO hs_bindgen_65e9b457479db3fb

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret10@
foreign import ccall unsafe "hs_bindgen_b5fb601528eabcdb" hs_bindgen_b5fb601528eabcdb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_funptr_ret10@
hs_bindgen_b5fb601528eabcdb :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_b5fb601528eabcdb =
  BG.fromFFIType hs_bindgen_b5fb601528eabcdb_base

{-# NOINLINE funptr_ret10 #-}
{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h 272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret10 =
  BG.unsafePerformIO hs_bindgen_b5fb601528eabcdb
