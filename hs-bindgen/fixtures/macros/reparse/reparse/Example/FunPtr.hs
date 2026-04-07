{-# LANGUAGE CApiFFI #-}
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
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/reparse/reparse.h>"
  , "/* test_macrosreparsereparse_Example_get_args_char1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_655b5bf82d42b590 (void)) ("
  , "  A arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &args_char1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_char2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_81d54f6d63f2ed19 (void)) ("
  , "  A arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  return &args_char2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_char3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d793f508bdf9f5c6 (void)) ("
  , "  A arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  return &args_char3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_short1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cb242ae15212e939 (void)) ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_short2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2d970081c0980735 (void)) ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_short3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_797b5129f0cfffa2 (void)) ("
  , "  A arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  return &args_short3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_int1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_619798a1c8d0bfca (void)) ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_int2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_97c85c8d02b6f627 (void)) ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_int3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_afcdbe4f006f8c2b (void)) ("
  , "  A arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  return &args_int3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_long1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_baef59c31efb820f (void)) ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_long2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6a2205ed67738398 (void)) ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_long3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_35ab6eb3e740ecd2 (void)) ("
  , "  A arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  return &args_long3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_float */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ab5cb315ba4a33b2 (void)) ("
  , "  A arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  return &args_float;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_double */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_85400a229437da50 (void)) ("
  , "  A arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &args_double;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_bool1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_597d45b3d4d9b5f7 (void)) ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &args_bool1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_struct */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_52baaf2320d99b57 (void)) ("
  , "  A arg1,"
  , "  struct some_struct arg2"
  , ")"
  , "{"
  , "  return &args_struct;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_union */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_27f55af07c32f7ae (void)) ("
  , "  A arg1,"
  , "  union some_union arg2"
  , ")"
  , "{"
  , "  return &args_union;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_enum */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5f6ca308bea69502 (void)) ("
  , "  A arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  return &args_enum;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_pointer1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_16f1614973762c62 (void)) ("
  , "  A arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return &args_pointer1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_pointer2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_eda587435f979d0d (void)) ("
  , "  A arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  return &args_pointer2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_pointer3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_093dd3cb21768646 (void)) ("
  , "  A arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  return &args_pointer3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_A */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_952128586d963823 (void)) (void)"
  , "{"
  , "  return &ret_A;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_char1 */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_39aa8e5a95ade23a (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_char2 */"
  , "__attribute__ ((const))"
  , "signed char (*hs_bindgen_801ce1d57b0a0089 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_char3 */"
  , "__attribute__ ((const))"
  , "unsigned char (*hs_bindgen_9e6610a4c215587a (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_short1 */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_50a99b6777a0550d (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_short2 */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_e44ee6f10f3bd576 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_short3 */"
  , "__attribute__ ((const))"
  , "unsigned short (*hs_bindgen_b71dbbef519f0443 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_int1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_baca367ca69b524d (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_int2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_4f0510a8e8bf5123 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_int3 */"
  , "__attribute__ ((const))"
  , "unsigned int (*hs_bindgen_2940dc2ace9172f2 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_long1 */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_55dcc6e90f57b81c (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_long2 */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_3db4e5c4056aa81a (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_long3 */"
  , "__attribute__ ((const))"
  , "unsigned long (*hs_bindgen_bd722f3d0914da09 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_float */"
  , "__attribute__ ((const))"
  , "float (*hs_bindgen_b4cd5f4a743ed383 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_float;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_double */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_bc63b78d43a6fd38 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_double;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_bool1 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_bbaeb752daeb3677 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_bool1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_struct */"
  , "__attribute__ ((const))"
  , "struct some_struct (*hs_bindgen_4aa4603bda20b20c (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_struct;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_union */"
  , "__attribute__ ((const))"
  , "union some_union (*hs_bindgen_ac23e9fd63b87ac2 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_union;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_enum */"
  , "__attribute__ ((const))"
  , "enum some_enum (*hs_bindgen_93750afbbf403335 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_enum;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_pointer1 */"
  , "__attribute__ ((const))"
  , "signed int *(*hs_bindgen_060233a9f71d30b9 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_pointer2 */"
  , "__attribute__ ((const))"
  , "signed int **(*hs_bindgen_5536d6de064d8458 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_pointer3 */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_6c60fd4174f2572b (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_body1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_e620e634dd6dbc7c (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &body1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_body2 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_64d7296583e02d63 (void)) (void)"
  , "{"
  , "  return &body2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_complex_float */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0744f8d931d3e7f4 (void)) ("
  , "  A arg1,"
  , "  float _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_float;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_args_complex_double */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_269479584677cede (void)) ("
  , "  A arg1,"
  , "  double _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_double;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_complex_float */"
  , "__attribute__ ((const))"
  , "float _Complex (*hs_bindgen_afae5decabe57665 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_complex_float;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_ret_complex_double */"
  , "__attribute__ ((const))"
  , "double _Complex (*hs_bindgen_68387eb859e4b698 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_complex_double;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_bespoke_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0bd7971264a6bf64 (void)) ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &bespoke_args1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_bespoke_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_706232c15d20ff49 (void)) ("
  , "  A arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &bespoke_args2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_bespoke_ret1 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_ff54c0d6fe585928 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_bespoke_ret2 */"
  , "__attribute__ ((const))"
  , "size_t (*hs_bindgen_c5dc3c52a26cd344 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_arr_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_359bf29bf59196e7 (void)) ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  return &arr_args1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_arr_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b6bce8b55a9de81e (void)) ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  return &arr_args2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_arr_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f2e44ad1f4fe2e94 (void)) ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  return &arr_args3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_arr_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_65ae5ca2c039185e (void)) ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  return &arr_args4;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_funptr_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_91ecc9c2f6ed6603 (void)) ("
  , "  A arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_funptr_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3cdcb41e6cc987b9 (void)) ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_funptr_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a6e4b4f8699e5f23 (void)) ("
  , "  A arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_funptr_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2dd30cc65d616aff (void)) ("
  , "  A arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args4;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_funptr_args5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f2124e2e7c75bb0a (void)) ("
  , "  A arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args5;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_comments1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_21eadb56a2e5e4a4 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &comments1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_prim_before1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cd4862b0e302d655 (void)) ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_prim_before2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_dc00f3fb59caef58 (void)) ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_prim_before3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_bf5a8ac1f5200d40 (void)) ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_prim_after1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_42b9029e89dc27ee (void)) ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_prim_after2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_46259eab2e8f6f43 (void)) ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_prim_after3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ae9ff7d244d2d09f (void)) ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_withoutSign_before1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cdc21c51762e7a1d (void)) ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_withoutSign_before2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3d2ac579d88a02e4 (void)) ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_withoutSign_before3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9763d5925860a2d5 (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_withoutSign_before4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a2f5cc9d4c6ca636 (void)) ("
  , "  A arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before4;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_withoutSign_before5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7d32427f93f35862 (void)) ("
  , "  A arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before5;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_withoutSign_before6 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e670df8520dfe7b8 (void)) ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before6;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_withoutSign_before7 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_daa0cd5e000d286b (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before7;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_withoutSign_before8 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cb0db7618a34b1da (void)) ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before8;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_withoutSign_after1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_27af6dd95fea1fa5 (void)) ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_withoutSign_after2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1cdb662a9cf0647a (void)) ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_withoutSign_after3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4094ded85265b2e8 (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_withoutSign_after4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_703ef6e2bc66a459 (void)) ("
  , "  A arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after4;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_withoutSign_after5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0afa081c272debb4 (void)) ("
  , "  A arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after5;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_withoutSign_after6 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1e6fa74fb181999d (void)) ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after6;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_withoutSign_after7 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4e9fbac9079b42df (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after7;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_withoutSign_after8 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b09b86ef1a4a302f (void)) ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after8;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_pointers_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8c4314aad3357d3b (void)) ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_pointers_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4f42493cc43d6dc0 (void)) ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_pointers_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_52332819a6e33524 (void)) ("
  , "  A arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_pointers_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4e35df98dff13f86 (void)) ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args4;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_pointers_args5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1caf0649ea276f7c (void)) ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args5;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_pointers_ret1 */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_1ccc4e5ba93fcf35 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_pointers_ret2 */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_9eb816dfd37773cf (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_pointers_ret3 */"
  , "__attribute__ ((const))"
  , "signed int *const (*hs_bindgen_14b8233170e4a4a1 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_pointers_ret4 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_e41a772a388000d9 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret4;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_pointers_ret5 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_6f67bfe897ac3801 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret5;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_array_elem1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f7dbf9e037616d58 (void)) ("
  , "  A const *arg1"
  , ")"
  , "{"
  , "  return &const_array_elem1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_array_elem2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9ffee618c9ba69df (void)) ("
  , "  A const **arg1"
  , ")"
  , "{"
  , "  return &const_array_elem2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_const_array_elem3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2d15e55c091ec37e (void)) ("
  , "  A *const *arg1"
  , ")"
  , "{"
  , "  return &const_array_elem3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_noParams1 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_31bf3a43a9ecac51 (void)) (void)"
  , "{"
  , "  return &noParams1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_noParams2 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_89ebc691d8a57938 (void)) (void)"
  , "{"
  , "  return &noParams2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_noParams3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5dc2b26d52df36c1 (void)) ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &noParams3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_funptr_ret1 */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_30400d1c4a66532a (void)) ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret1;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_funptr_ret2 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_6f4960a7e9d7d73a (void)) ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret2;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_funptr_ret3 */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_104f2e9a3d744d43 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &funptr_ret3;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_funptr_ret4 */"
  , "__attribute__ ((const))"
  , "char (*(*hs_bindgen_6feb948588d84bd8 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret4;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_funptr_ret5 */"
  , "__attribute__ ((const))"
  , "signed int *(*(*hs_bindgen_e32443b8a050161d (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret5;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_funptr_ret6 */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_7e14d72951bba6e3 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret6;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_funptr_ret7 */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_a05584c46e3d1f5c (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret7;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_funptr_ret8 */"
  , "__attribute__ ((const))"
  , "signed int *const (*(*hs_bindgen_c7f5a58273570329 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret8;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_funptr_ret9 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_f5bab8520c61852b (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret9;"
  , "}"
  , "/* test_macrosreparsereparse_Example_get_funptr_ret10 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_e43abe6c9a357e5c (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret10;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsereparse_Example_get_args_char1@
foreign import ccall unsafe "hs_bindgen_655b5bf82d42b590" hs_bindgen_655b5bf82d42b590_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_char1@
hs_bindgen_655b5bf82d42b590 :: IO (RIP.FunPtr (A -> RIP.CChar -> IO ()))
hs_bindgen_655b5bf82d42b590 =
  RIP.fromFFIType hs_bindgen_655b5bf82d42b590_base

{-# NOINLINE args_char1 #-}
{-| Function declarations

__C declaration:__ @args_char1@

__defined at:__ @macros\/reparse\/reparse.h 17:6@

__exported by:__ @macros\/reparse\/reparse.h@
-}
args_char1 :: RIP.FunPtr (A -> RIP.CChar -> IO ())
args_char1 =
  RIP.unsafePerformIO hs_bindgen_655b5bf82d42b590

-- __unique:__ @test_macrosreparsereparse_Example_get_args_char2@
foreign import ccall unsafe "hs_bindgen_81d54f6d63f2ed19" hs_bindgen_81d54f6d63f2ed19_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_char2@
hs_bindgen_81d54f6d63f2ed19 :: IO (RIP.FunPtr (A -> RIP.CSChar -> IO ()))
hs_bindgen_81d54f6d63f2ed19 =
  RIP.fromFFIType hs_bindgen_81d54f6d63f2ed19_base

{-# NOINLINE args_char2 #-}
{-| __C declaration:__ @args_char2@

    __defined at:__ @macros\/reparse\/reparse.h 18:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_char2 :: RIP.FunPtr (A -> RIP.CSChar -> IO ())
args_char2 =
  RIP.unsafePerformIO hs_bindgen_81d54f6d63f2ed19

-- __unique:__ @test_macrosreparsereparse_Example_get_args_char3@
foreign import ccall unsafe "hs_bindgen_d793f508bdf9f5c6" hs_bindgen_d793f508bdf9f5c6_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_char3@
hs_bindgen_d793f508bdf9f5c6 :: IO (RIP.FunPtr (A -> RIP.CUChar -> IO ()))
hs_bindgen_d793f508bdf9f5c6 =
  RIP.fromFFIType hs_bindgen_d793f508bdf9f5c6_base

{-# NOINLINE args_char3 #-}
{-| __C declaration:__ @args_char3@

    __defined at:__ @macros\/reparse\/reparse.h 19:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_char3 :: RIP.FunPtr (A -> RIP.CUChar -> IO ())
args_char3 =
  RIP.unsafePerformIO hs_bindgen_d793f508bdf9f5c6

-- __unique:__ @test_macrosreparsereparse_Example_get_args_short1@
foreign import ccall unsafe "hs_bindgen_cb242ae15212e939" hs_bindgen_cb242ae15212e939_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_short1@
hs_bindgen_cb242ae15212e939 :: IO (RIP.FunPtr (A -> RIP.CShort -> IO ()))
hs_bindgen_cb242ae15212e939 =
  RIP.fromFFIType hs_bindgen_cb242ae15212e939_base

{-# NOINLINE args_short1 #-}
{-| __C declaration:__ @args_short1@

    __defined at:__ @macros\/reparse\/reparse.h 21:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_short1 :: RIP.FunPtr (A -> RIP.CShort -> IO ())
args_short1 =
  RIP.unsafePerformIO hs_bindgen_cb242ae15212e939

-- __unique:__ @test_macrosreparsereparse_Example_get_args_short2@
foreign import ccall unsafe "hs_bindgen_2d970081c0980735" hs_bindgen_2d970081c0980735_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_short2@
hs_bindgen_2d970081c0980735 :: IO (RIP.FunPtr (A -> RIP.CShort -> IO ()))
hs_bindgen_2d970081c0980735 =
  RIP.fromFFIType hs_bindgen_2d970081c0980735_base

{-# NOINLINE args_short2 #-}
{-| __C declaration:__ @args_short2@

    __defined at:__ @macros\/reparse\/reparse.h 22:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_short2 :: RIP.FunPtr (A -> RIP.CShort -> IO ())
args_short2 =
  RIP.unsafePerformIO hs_bindgen_2d970081c0980735

-- __unique:__ @test_macrosreparsereparse_Example_get_args_short3@
foreign import ccall unsafe "hs_bindgen_797b5129f0cfffa2" hs_bindgen_797b5129f0cfffa2_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_short3@
hs_bindgen_797b5129f0cfffa2 :: IO (RIP.FunPtr (A -> RIP.CUShort -> IO ()))
hs_bindgen_797b5129f0cfffa2 =
  RIP.fromFFIType hs_bindgen_797b5129f0cfffa2_base

{-# NOINLINE args_short3 #-}
{-| __C declaration:__ @args_short3@

    __defined at:__ @macros\/reparse\/reparse.h 23:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_short3 :: RIP.FunPtr (A -> RIP.CUShort -> IO ())
args_short3 =
  RIP.unsafePerformIO hs_bindgen_797b5129f0cfffa2

-- __unique:__ @test_macrosreparsereparse_Example_get_args_int1@
foreign import ccall unsafe "hs_bindgen_619798a1c8d0bfca" hs_bindgen_619798a1c8d0bfca_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_int1@
hs_bindgen_619798a1c8d0bfca :: IO (RIP.FunPtr (A -> RIP.CInt -> IO ()))
hs_bindgen_619798a1c8d0bfca =
  RIP.fromFFIType hs_bindgen_619798a1c8d0bfca_base

{-# NOINLINE args_int1 #-}
{-| __C declaration:__ @args_int1@

    __defined at:__ @macros\/reparse\/reparse.h 25:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_int1 :: RIP.FunPtr (A -> RIP.CInt -> IO ())
args_int1 =
  RIP.unsafePerformIO hs_bindgen_619798a1c8d0bfca

-- __unique:__ @test_macrosreparsereparse_Example_get_args_int2@
foreign import ccall unsafe "hs_bindgen_97c85c8d02b6f627" hs_bindgen_97c85c8d02b6f627_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_int2@
hs_bindgen_97c85c8d02b6f627 :: IO (RIP.FunPtr (A -> RIP.CInt -> IO ()))
hs_bindgen_97c85c8d02b6f627 =
  RIP.fromFFIType hs_bindgen_97c85c8d02b6f627_base

{-# NOINLINE args_int2 #-}
{-| __C declaration:__ @args_int2@

    __defined at:__ @macros\/reparse\/reparse.h 26:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_int2 :: RIP.FunPtr (A -> RIP.CInt -> IO ())
args_int2 =
  RIP.unsafePerformIO hs_bindgen_97c85c8d02b6f627

-- __unique:__ @test_macrosreparsereparse_Example_get_args_int3@
foreign import ccall unsafe "hs_bindgen_afcdbe4f006f8c2b" hs_bindgen_afcdbe4f006f8c2b_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_int3@
hs_bindgen_afcdbe4f006f8c2b :: IO (RIP.FunPtr (A -> RIP.CUInt -> IO ()))
hs_bindgen_afcdbe4f006f8c2b =
  RIP.fromFFIType hs_bindgen_afcdbe4f006f8c2b_base

{-# NOINLINE args_int3 #-}
{-| __C declaration:__ @args_int3@

    __defined at:__ @macros\/reparse\/reparse.h 27:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_int3 :: RIP.FunPtr (A -> RIP.CUInt -> IO ())
args_int3 =
  RIP.unsafePerformIO hs_bindgen_afcdbe4f006f8c2b

-- __unique:__ @test_macrosreparsereparse_Example_get_args_long1@
foreign import ccall unsafe "hs_bindgen_baef59c31efb820f" hs_bindgen_baef59c31efb820f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_long1@
hs_bindgen_baef59c31efb820f :: IO (RIP.FunPtr (A -> RIP.CLong -> IO ()))
hs_bindgen_baef59c31efb820f =
  RIP.fromFFIType hs_bindgen_baef59c31efb820f_base

{-# NOINLINE args_long1 #-}
{-| __C declaration:__ @args_long1@

    __defined at:__ @macros\/reparse\/reparse.h 29:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_long1 :: RIP.FunPtr (A -> RIP.CLong -> IO ())
args_long1 =
  RIP.unsafePerformIO hs_bindgen_baef59c31efb820f

-- __unique:__ @test_macrosreparsereparse_Example_get_args_long2@
foreign import ccall unsafe "hs_bindgen_6a2205ed67738398" hs_bindgen_6a2205ed67738398_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_long2@
hs_bindgen_6a2205ed67738398 :: IO (RIP.FunPtr (A -> RIP.CLong -> IO ()))
hs_bindgen_6a2205ed67738398 =
  RIP.fromFFIType hs_bindgen_6a2205ed67738398_base

{-# NOINLINE args_long2 #-}
{-| __C declaration:__ @args_long2@

    __defined at:__ @macros\/reparse\/reparse.h 30:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_long2 :: RIP.FunPtr (A -> RIP.CLong -> IO ())
args_long2 =
  RIP.unsafePerformIO hs_bindgen_6a2205ed67738398

-- __unique:__ @test_macrosreparsereparse_Example_get_args_long3@
foreign import ccall unsafe "hs_bindgen_35ab6eb3e740ecd2" hs_bindgen_35ab6eb3e740ecd2_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_long3@
hs_bindgen_35ab6eb3e740ecd2 :: IO (RIP.FunPtr (A -> RIP.CULong -> IO ()))
hs_bindgen_35ab6eb3e740ecd2 =
  RIP.fromFFIType hs_bindgen_35ab6eb3e740ecd2_base

{-# NOINLINE args_long3 #-}
{-| __C declaration:__ @args_long3@

    __defined at:__ @macros\/reparse\/reparse.h 31:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_long3 :: RIP.FunPtr (A -> RIP.CULong -> IO ())
args_long3 =
  RIP.unsafePerformIO hs_bindgen_35ab6eb3e740ecd2

-- __unique:__ @test_macrosreparsereparse_Example_get_args_float@
foreign import ccall unsafe "hs_bindgen_ab5cb315ba4a33b2" hs_bindgen_ab5cb315ba4a33b2_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_float@
hs_bindgen_ab5cb315ba4a33b2 :: IO (RIP.FunPtr (A -> RIP.CFloat -> IO ()))
hs_bindgen_ab5cb315ba4a33b2 =
  RIP.fromFFIType hs_bindgen_ab5cb315ba4a33b2_base

{-# NOINLINE args_float #-}
{-| __C declaration:__ @args_float@

    __defined at:__ @macros\/reparse\/reparse.h 33:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_float :: RIP.FunPtr (A -> RIP.CFloat -> IO ())
args_float =
  RIP.unsafePerformIO hs_bindgen_ab5cb315ba4a33b2

-- __unique:__ @test_macrosreparsereparse_Example_get_args_double@
foreign import ccall unsafe "hs_bindgen_85400a229437da50" hs_bindgen_85400a229437da50_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_double@
hs_bindgen_85400a229437da50 :: IO (RIP.FunPtr (A -> RIP.CDouble -> IO ()))
hs_bindgen_85400a229437da50 =
  RIP.fromFFIType hs_bindgen_85400a229437da50_base

{-# NOINLINE args_double #-}
{-| __C declaration:__ @args_double@

    __defined at:__ @macros\/reparse\/reparse.h 34:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_double :: RIP.FunPtr (A -> RIP.CDouble -> IO ())
args_double =
  RIP.unsafePerformIO hs_bindgen_85400a229437da50

-- __unique:__ @test_macrosreparsereparse_Example_get_args_bool1@
foreign import ccall unsafe "hs_bindgen_597d45b3d4d9b5f7" hs_bindgen_597d45b3d4d9b5f7_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_bool1@
hs_bindgen_597d45b3d4d9b5f7 :: IO (RIP.FunPtr (A -> RIP.CBool -> IO ()))
hs_bindgen_597d45b3d4d9b5f7 =
  RIP.fromFFIType hs_bindgen_597d45b3d4d9b5f7_base

{-# NOINLINE args_bool1 #-}
{-| __C declaration:__ @args_bool1@

    __defined at:__ @macros\/reparse\/reparse.h 35:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_bool1 :: RIP.FunPtr (A -> RIP.CBool -> IO ())
args_bool1 =
  RIP.unsafePerformIO hs_bindgen_597d45b3d4d9b5f7

-- __unique:__ @test_macrosreparsereparse_Example_get_args_struct@
foreign import ccall unsafe "hs_bindgen_52baaf2320d99b57" hs_bindgen_52baaf2320d99b57_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_struct@
hs_bindgen_52baaf2320d99b57 :: IO (RIP.FunPtr (A -> Some_struct -> IO ()))
hs_bindgen_52baaf2320d99b57 =
  RIP.fromFFIType hs_bindgen_52baaf2320d99b57_base

{-# NOINLINE args_struct #-}
{-| __C declaration:__ @args_struct@

    __defined at:__ @macros\/reparse\/reparse.h 37:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_struct :: RIP.FunPtr (A -> Some_struct -> IO ())
args_struct =
  RIP.unsafePerformIO hs_bindgen_52baaf2320d99b57

-- __unique:__ @test_macrosreparsereparse_Example_get_args_union@
foreign import ccall unsafe "hs_bindgen_27f55af07c32f7ae" hs_bindgen_27f55af07c32f7ae_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_union@
hs_bindgen_27f55af07c32f7ae :: IO (RIP.FunPtr (A -> Some_union -> IO ()))
hs_bindgen_27f55af07c32f7ae =
  RIP.fromFFIType hs_bindgen_27f55af07c32f7ae_base

{-# NOINLINE args_union #-}
{-| __C declaration:__ @args_union@

    __defined at:__ @macros\/reparse\/reparse.h 38:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_union :: RIP.FunPtr (A -> Some_union -> IO ())
args_union =
  RIP.unsafePerformIO hs_bindgen_27f55af07c32f7ae

-- __unique:__ @test_macrosreparsereparse_Example_get_args_enum@
foreign import ccall unsafe "hs_bindgen_5f6ca308bea69502" hs_bindgen_5f6ca308bea69502_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_enum@
hs_bindgen_5f6ca308bea69502 :: IO (RIP.FunPtr (A -> Some_enum -> IO ()))
hs_bindgen_5f6ca308bea69502 =
  RIP.fromFFIType hs_bindgen_5f6ca308bea69502_base

{-# NOINLINE args_enum #-}
{-| __C declaration:__ @args_enum@

    __defined at:__ @macros\/reparse\/reparse.h 39:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_enum :: RIP.FunPtr (A -> Some_enum -> IO ())
args_enum =
  RIP.unsafePerformIO hs_bindgen_5f6ca308bea69502

-- __unique:__ @test_macrosreparsereparse_Example_get_args_pointer1@
foreign import ccall unsafe "hs_bindgen_16f1614973762c62" hs_bindgen_16f1614973762c62_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_pointer1@
hs_bindgen_16f1614973762c62 :: IO (RIP.FunPtr (A -> (RIP.Ptr RIP.CInt) -> IO ()))
hs_bindgen_16f1614973762c62 =
  RIP.fromFFIType hs_bindgen_16f1614973762c62_base

{-# NOINLINE args_pointer1 #-}
{-| __C declaration:__ @args_pointer1@

    __defined at:__ @macros\/reparse\/reparse.h 41:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_pointer1 :: RIP.FunPtr (A -> (RIP.Ptr RIP.CInt) -> IO ())
args_pointer1 =
  RIP.unsafePerformIO hs_bindgen_16f1614973762c62

-- __unique:__ @test_macrosreparsereparse_Example_get_args_pointer2@
foreign import ccall unsafe "hs_bindgen_eda587435f979d0d" hs_bindgen_eda587435f979d0d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_pointer2@
hs_bindgen_eda587435f979d0d :: IO (RIP.FunPtr (A -> (RIP.Ptr (RIP.Ptr RIP.CInt)) -> IO ()))
hs_bindgen_eda587435f979d0d =
  RIP.fromFFIType hs_bindgen_eda587435f979d0d_base

{-# NOINLINE args_pointer2 #-}
{-| __C declaration:__ @args_pointer2@

    __defined at:__ @macros\/reparse\/reparse.h 42:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_pointer2 :: RIP.FunPtr (A -> (RIP.Ptr (RIP.Ptr RIP.CInt)) -> IO ())
args_pointer2 =
  RIP.unsafePerformIO hs_bindgen_eda587435f979d0d

-- __unique:__ @test_macrosreparsereparse_Example_get_args_pointer3@
foreign import ccall unsafe "hs_bindgen_093dd3cb21768646" hs_bindgen_093dd3cb21768646_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_pointer3@
hs_bindgen_093dd3cb21768646 :: IO (RIP.FunPtr (A -> (RIP.Ptr RIP.Void) -> IO ()))
hs_bindgen_093dd3cb21768646 =
  RIP.fromFFIType hs_bindgen_093dd3cb21768646_base

{-# NOINLINE args_pointer3 #-}
{-| __C declaration:__ @args_pointer3@

    __defined at:__ @macros\/reparse\/reparse.h 43:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_pointer3 :: RIP.FunPtr (A -> (RIP.Ptr RIP.Void) -> IO ())
args_pointer3 =
  RIP.unsafePerformIO hs_bindgen_093dd3cb21768646

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_A@
foreign import ccall unsafe "hs_bindgen_952128586d963823" hs_bindgen_952128586d963823_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_A@
hs_bindgen_952128586d963823 :: IO (RIP.FunPtr (IO A))
hs_bindgen_952128586d963823 =
  RIP.fromFFIType hs_bindgen_952128586d963823_base

{-# NOINLINE ret_A #-}
{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse\/reparse.h 47:3@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_A :: RIP.FunPtr (IO A)
ret_A =
  RIP.unsafePerformIO hs_bindgen_952128586d963823

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_char1@
foreign import ccall unsafe "hs_bindgen_39aa8e5a95ade23a" hs_bindgen_39aa8e5a95ade23a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_char1@
hs_bindgen_39aa8e5a95ade23a :: IO (RIP.FunPtr (A -> IO RIP.CChar))
hs_bindgen_39aa8e5a95ade23a =
  RIP.fromFFIType hs_bindgen_39aa8e5a95ade23a_base

{-# NOINLINE ret_char1 #-}
{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse\/reparse.h 49:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_char1 :: RIP.FunPtr (A -> IO RIP.CChar)
ret_char1 =
  RIP.unsafePerformIO hs_bindgen_39aa8e5a95ade23a

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_char2@
foreign import ccall unsafe "hs_bindgen_801ce1d57b0a0089" hs_bindgen_801ce1d57b0a0089_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_char2@
hs_bindgen_801ce1d57b0a0089 :: IO (RIP.FunPtr (A -> IO RIP.CSChar))
hs_bindgen_801ce1d57b0a0089 =
  RIP.fromFFIType hs_bindgen_801ce1d57b0a0089_base

{-# NOINLINE ret_char2 #-}
{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse\/reparse.h 50:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_char2 :: RIP.FunPtr (A -> IO RIP.CSChar)
ret_char2 =
  RIP.unsafePerformIO hs_bindgen_801ce1d57b0a0089

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_char3@
foreign import ccall unsafe "hs_bindgen_9e6610a4c215587a" hs_bindgen_9e6610a4c215587a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_char3@
hs_bindgen_9e6610a4c215587a :: IO (RIP.FunPtr (A -> IO RIP.CUChar))
hs_bindgen_9e6610a4c215587a =
  RIP.fromFFIType hs_bindgen_9e6610a4c215587a_base

{-# NOINLINE ret_char3 #-}
{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse\/reparse.h 51:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_char3 :: RIP.FunPtr (A -> IO RIP.CUChar)
ret_char3 =
  RIP.unsafePerformIO hs_bindgen_9e6610a4c215587a

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_short1@
foreign import ccall unsafe "hs_bindgen_50a99b6777a0550d" hs_bindgen_50a99b6777a0550d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_short1@
hs_bindgen_50a99b6777a0550d :: IO (RIP.FunPtr (A -> IO RIP.CShort))
hs_bindgen_50a99b6777a0550d =
  RIP.fromFFIType hs_bindgen_50a99b6777a0550d_base

{-# NOINLINE ret_short1 #-}
{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse\/reparse.h 53:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_short1 :: RIP.FunPtr (A -> IO RIP.CShort)
ret_short1 =
  RIP.unsafePerformIO hs_bindgen_50a99b6777a0550d

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_short2@
foreign import ccall unsafe "hs_bindgen_e44ee6f10f3bd576" hs_bindgen_e44ee6f10f3bd576_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_short2@
hs_bindgen_e44ee6f10f3bd576 :: IO (RIP.FunPtr (A -> IO RIP.CShort))
hs_bindgen_e44ee6f10f3bd576 =
  RIP.fromFFIType hs_bindgen_e44ee6f10f3bd576_base

{-# NOINLINE ret_short2 #-}
{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse\/reparse.h 54:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_short2 :: RIP.FunPtr (A -> IO RIP.CShort)
ret_short2 =
  RIP.unsafePerformIO hs_bindgen_e44ee6f10f3bd576

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_short3@
foreign import ccall unsafe "hs_bindgen_b71dbbef519f0443" hs_bindgen_b71dbbef519f0443_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_short3@
hs_bindgen_b71dbbef519f0443 :: IO (RIP.FunPtr (A -> IO RIP.CUShort))
hs_bindgen_b71dbbef519f0443 =
  RIP.fromFFIType hs_bindgen_b71dbbef519f0443_base

{-# NOINLINE ret_short3 #-}
{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse\/reparse.h 55:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_short3 :: RIP.FunPtr (A -> IO RIP.CUShort)
ret_short3 =
  RIP.unsafePerformIO hs_bindgen_b71dbbef519f0443

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_int1@
foreign import ccall unsafe "hs_bindgen_baca367ca69b524d" hs_bindgen_baca367ca69b524d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_int1@
hs_bindgen_baca367ca69b524d :: IO (RIP.FunPtr (A -> IO RIP.CInt))
hs_bindgen_baca367ca69b524d =
  RIP.fromFFIType hs_bindgen_baca367ca69b524d_base

{-# NOINLINE ret_int1 #-}
{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse\/reparse.h 57:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_int1 :: RIP.FunPtr (A -> IO RIP.CInt)
ret_int1 =
  RIP.unsafePerformIO hs_bindgen_baca367ca69b524d

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_int2@
foreign import ccall unsafe "hs_bindgen_4f0510a8e8bf5123" hs_bindgen_4f0510a8e8bf5123_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_int2@
hs_bindgen_4f0510a8e8bf5123 :: IO (RIP.FunPtr (A -> IO RIP.CInt))
hs_bindgen_4f0510a8e8bf5123 =
  RIP.fromFFIType hs_bindgen_4f0510a8e8bf5123_base

{-# NOINLINE ret_int2 #-}
{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse\/reparse.h 58:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_int2 :: RIP.FunPtr (A -> IO RIP.CInt)
ret_int2 =
  RIP.unsafePerformIO hs_bindgen_4f0510a8e8bf5123

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_int3@
foreign import ccall unsafe "hs_bindgen_2940dc2ace9172f2" hs_bindgen_2940dc2ace9172f2_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_int3@
hs_bindgen_2940dc2ace9172f2 :: IO (RIP.FunPtr (A -> IO RIP.CUInt))
hs_bindgen_2940dc2ace9172f2 =
  RIP.fromFFIType hs_bindgen_2940dc2ace9172f2_base

{-# NOINLINE ret_int3 #-}
{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse\/reparse.h 59:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_int3 :: RIP.FunPtr (A -> IO RIP.CUInt)
ret_int3 =
  RIP.unsafePerformIO hs_bindgen_2940dc2ace9172f2

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_long1@
foreign import ccall unsafe "hs_bindgen_55dcc6e90f57b81c" hs_bindgen_55dcc6e90f57b81c_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_long1@
hs_bindgen_55dcc6e90f57b81c :: IO (RIP.FunPtr (A -> IO RIP.CLong))
hs_bindgen_55dcc6e90f57b81c =
  RIP.fromFFIType hs_bindgen_55dcc6e90f57b81c_base

{-# NOINLINE ret_long1 #-}
{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse\/reparse.h 61:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_long1 :: RIP.FunPtr (A -> IO RIP.CLong)
ret_long1 =
  RIP.unsafePerformIO hs_bindgen_55dcc6e90f57b81c

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_long2@
foreign import ccall unsafe "hs_bindgen_3db4e5c4056aa81a" hs_bindgen_3db4e5c4056aa81a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_long2@
hs_bindgen_3db4e5c4056aa81a :: IO (RIP.FunPtr (A -> IO RIP.CLong))
hs_bindgen_3db4e5c4056aa81a =
  RIP.fromFFIType hs_bindgen_3db4e5c4056aa81a_base

{-# NOINLINE ret_long2 #-}
{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse\/reparse.h 62:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_long2 :: RIP.FunPtr (A -> IO RIP.CLong)
ret_long2 =
  RIP.unsafePerformIO hs_bindgen_3db4e5c4056aa81a

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_long3@
foreign import ccall unsafe "hs_bindgen_bd722f3d0914da09" hs_bindgen_bd722f3d0914da09_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_long3@
hs_bindgen_bd722f3d0914da09 :: IO (RIP.FunPtr (A -> IO RIP.CULong))
hs_bindgen_bd722f3d0914da09 =
  RIP.fromFFIType hs_bindgen_bd722f3d0914da09_base

{-# NOINLINE ret_long3 #-}
{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse\/reparse.h 63:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_long3 :: RIP.FunPtr (A -> IO RIP.CULong)
ret_long3 =
  RIP.unsafePerformIO hs_bindgen_bd722f3d0914da09

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_float@
foreign import ccall unsafe "hs_bindgen_b4cd5f4a743ed383" hs_bindgen_b4cd5f4a743ed383_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_float@
hs_bindgen_b4cd5f4a743ed383 :: IO (RIP.FunPtr (A -> IO RIP.CFloat))
hs_bindgen_b4cd5f4a743ed383 =
  RIP.fromFFIType hs_bindgen_b4cd5f4a743ed383_base

{-# NOINLINE ret_float #-}
{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse\/reparse.h 65:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_float :: RIP.FunPtr (A -> IO RIP.CFloat)
ret_float =
  RIP.unsafePerformIO hs_bindgen_b4cd5f4a743ed383

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_double@
foreign import ccall unsafe "hs_bindgen_bc63b78d43a6fd38" hs_bindgen_bc63b78d43a6fd38_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_double@
hs_bindgen_bc63b78d43a6fd38 :: IO (RIP.FunPtr (A -> IO RIP.CDouble))
hs_bindgen_bc63b78d43a6fd38 =
  RIP.fromFFIType hs_bindgen_bc63b78d43a6fd38_base

{-# NOINLINE ret_double #-}
{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse\/reparse.h 66:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_double :: RIP.FunPtr (A -> IO RIP.CDouble)
ret_double =
  RIP.unsafePerformIO hs_bindgen_bc63b78d43a6fd38

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_bool1@
foreign import ccall unsafe "hs_bindgen_bbaeb752daeb3677" hs_bindgen_bbaeb752daeb3677_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_bool1@
hs_bindgen_bbaeb752daeb3677 :: IO (RIP.FunPtr (A -> IO RIP.CBool))
hs_bindgen_bbaeb752daeb3677 =
  RIP.fromFFIType hs_bindgen_bbaeb752daeb3677_base

{-# NOINLINE ret_bool1 #-}
{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse\/reparse.h 67:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_bool1 :: RIP.FunPtr (A -> IO RIP.CBool)
ret_bool1 =
  RIP.unsafePerformIO hs_bindgen_bbaeb752daeb3677

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_struct@
foreign import ccall unsafe "hs_bindgen_4aa4603bda20b20c" hs_bindgen_4aa4603bda20b20c_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_struct@
hs_bindgen_4aa4603bda20b20c :: IO (RIP.FunPtr (A -> IO Some_struct))
hs_bindgen_4aa4603bda20b20c =
  RIP.fromFFIType hs_bindgen_4aa4603bda20b20c_base

{-# NOINLINE ret_struct #-}
{-| __C declaration:__ @ret_struct@

    __defined at:__ @macros\/reparse\/reparse.h 69:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_struct :: RIP.FunPtr (A -> IO Some_struct)
ret_struct =
  RIP.unsafePerformIO hs_bindgen_4aa4603bda20b20c

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_union@
foreign import ccall unsafe "hs_bindgen_ac23e9fd63b87ac2" hs_bindgen_ac23e9fd63b87ac2_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_union@
hs_bindgen_ac23e9fd63b87ac2 :: IO (RIP.FunPtr (A -> IO Some_union))
hs_bindgen_ac23e9fd63b87ac2 =
  RIP.fromFFIType hs_bindgen_ac23e9fd63b87ac2_base

{-# NOINLINE ret_union #-}
{-| __C declaration:__ @ret_union@

    __defined at:__ @macros\/reparse\/reparse.h 70:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_union :: RIP.FunPtr (A -> IO Some_union)
ret_union =
  RIP.unsafePerformIO hs_bindgen_ac23e9fd63b87ac2

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_enum@
foreign import ccall unsafe "hs_bindgen_93750afbbf403335" hs_bindgen_93750afbbf403335_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_enum@
hs_bindgen_93750afbbf403335 :: IO (RIP.FunPtr (A -> IO Some_enum))
hs_bindgen_93750afbbf403335 =
  RIP.fromFFIType hs_bindgen_93750afbbf403335_base

{-# NOINLINE ret_enum #-}
{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse\/reparse.h 71:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_enum :: RIP.FunPtr (A -> IO Some_enum)
ret_enum =
  RIP.unsafePerformIO hs_bindgen_93750afbbf403335

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_pointer1@
foreign import ccall unsafe "hs_bindgen_060233a9f71d30b9" hs_bindgen_060233a9f71d30b9_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_pointer1@
hs_bindgen_060233a9f71d30b9 :: IO (RIP.FunPtr (A -> IO (RIP.Ptr RIP.CInt)))
hs_bindgen_060233a9f71d30b9 =
  RIP.fromFFIType hs_bindgen_060233a9f71d30b9_base

{-# NOINLINE ret_pointer1 #-}
{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse\/reparse.h 73:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_pointer1 :: RIP.FunPtr (A -> IO (RIP.Ptr RIP.CInt))
ret_pointer1 =
  RIP.unsafePerformIO hs_bindgen_060233a9f71d30b9

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_pointer2@
foreign import ccall unsafe "hs_bindgen_5536d6de064d8458" hs_bindgen_5536d6de064d8458_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_pointer2@
hs_bindgen_5536d6de064d8458 :: IO (RIP.FunPtr (A -> IO (RIP.Ptr (RIP.Ptr RIP.CInt))))
hs_bindgen_5536d6de064d8458 =
  RIP.fromFFIType hs_bindgen_5536d6de064d8458_base

{-# NOINLINE ret_pointer2 #-}
{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse\/reparse.h 74:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_pointer2 :: RIP.FunPtr (A -> IO (RIP.Ptr (RIP.Ptr RIP.CInt)))
ret_pointer2 =
  RIP.unsafePerformIO hs_bindgen_5536d6de064d8458

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_pointer3@
foreign import ccall unsafe "hs_bindgen_6c60fd4174f2572b" hs_bindgen_6c60fd4174f2572b_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_pointer3@
hs_bindgen_6c60fd4174f2572b :: IO (RIP.FunPtr (A -> IO (RIP.Ptr RIP.Void)))
hs_bindgen_6c60fd4174f2572b =
  RIP.fromFFIType hs_bindgen_6c60fd4174f2572b_base

{-# NOINLINE ret_pointer3 #-}
{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse\/reparse.h 75:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_pointer3 :: RIP.FunPtr (A -> IO (RIP.Ptr RIP.Void))
ret_pointer3 =
  RIP.unsafePerformIO hs_bindgen_6c60fd4174f2572b

-- __unique:__ @test_macrosreparsereparse_Example_get_body1@
foreign import ccall unsafe "hs_bindgen_e620e634dd6dbc7c" hs_bindgen_e620e634dd6dbc7c_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_body1@
hs_bindgen_e620e634dd6dbc7c :: IO (RIP.FunPtr (A -> IO RIP.CInt))
hs_bindgen_e620e634dd6dbc7c =
  RIP.fromFFIType hs_bindgen_e620e634dd6dbc7c_base

{-# NOINLINE body1 #-}
{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse\/reparse.h 79:5@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
body1 :: RIP.FunPtr (A -> IO RIP.CInt)
body1 =
  RIP.unsafePerformIO hs_bindgen_e620e634dd6dbc7c

-- __unique:__ @test_macrosreparsereparse_Example_get_body2@
foreign import ccall unsafe "hs_bindgen_64d7296583e02d63" hs_bindgen_64d7296583e02d63_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_body2@
hs_bindgen_64d7296583e02d63 :: IO (RIP.FunPtr (IO A))
hs_bindgen_64d7296583e02d63 =
  RIP.fromFFIType hs_bindgen_64d7296583e02d63_base

{-# NOINLINE body2 #-}
{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse\/reparse.h 80:3@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
body2 :: RIP.FunPtr (IO A)
body2 =
  RIP.unsafePerformIO hs_bindgen_64d7296583e02d63

-- __unique:__ @test_macrosreparsereparse_Example_get_args_complex_float@
foreign import ccall unsafe "hs_bindgen_0744f8d931d3e7f4" hs_bindgen_0744f8d931d3e7f4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_complex_float@
hs_bindgen_0744f8d931d3e7f4 :: IO (RIP.FunPtr (A -> (RIP.Complex RIP.CFloat) -> IO ()))
hs_bindgen_0744f8d931d3e7f4 =
  RIP.fromFFIType hs_bindgen_0744f8d931d3e7f4_base

{-# NOINLINE args_complex_float #-}
{-| __C declaration:__ @args_complex_float@

    __defined at:__ @macros\/reparse\/reparse.h 84:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_complex_float :: RIP.FunPtr (A -> (RIP.Complex RIP.CFloat) -> IO ())
args_complex_float =
  RIP.unsafePerformIO hs_bindgen_0744f8d931d3e7f4

-- __unique:__ @test_macrosreparsereparse_Example_get_args_complex_double@
foreign import ccall unsafe "hs_bindgen_269479584677cede" hs_bindgen_269479584677cede_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_args_complex_double@
hs_bindgen_269479584677cede :: IO (RIP.FunPtr (A -> (RIP.Complex RIP.CDouble) -> IO ()))
hs_bindgen_269479584677cede =
  RIP.fromFFIType hs_bindgen_269479584677cede_base

{-# NOINLINE args_complex_double #-}
{-| __C declaration:__ @args_complex_double@

    __defined at:__ @macros\/reparse\/reparse.h 85:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_complex_double :: RIP.FunPtr (A -> (RIP.Complex RIP.CDouble) -> IO ())
args_complex_double =
  RIP.unsafePerformIO hs_bindgen_269479584677cede

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_complex_float@
foreign import ccall unsafe "hs_bindgen_afae5decabe57665" hs_bindgen_afae5decabe57665_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_complex_float@
hs_bindgen_afae5decabe57665 :: IO (RIP.FunPtr (A -> IO (RIP.Complex RIP.CFloat)))
hs_bindgen_afae5decabe57665 =
  RIP.fromFFIType hs_bindgen_afae5decabe57665_base

{-# NOINLINE ret_complex_float #-}
{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @macros\/reparse\/reparse.h 86:17@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_complex_float :: RIP.FunPtr (A -> IO (RIP.Complex RIP.CFloat))
ret_complex_float =
  RIP.unsafePerformIO hs_bindgen_afae5decabe57665

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_complex_double@
foreign import ccall unsafe "hs_bindgen_68387eb859e4b698" hs_bindgen_68387eb859e4b698_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_ret_complex_double@
hs_bindgen_68387eb859e4b698 :: IO (RIP.FunPtr (A -> IO (RIP.Complex RIP.CDouble)))
hs_bindgen_68387eb859e4b698 =
  RIP.fromFFIType hs_bindgen_68387eb859e4b698_base

{-# NOINLINE ret_complex_double #-}
{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @macros\/reparse\/reparse.h 87:17@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_complex_double :: RIP.FunPtr (A -> IO (RIP.Complex RIP.CDouble))
ret_complex_double =
  RIP.unsafePerformIO hs_bindgen_68387eb859e4b698

-- __unique:__ @test_macrosreparsereparse_Example_get_bespoke_args1@
foreign import ccall unsafe "hs_bindgen_0bd7971264a6bf64" hs_bindgen_0bd7971264a6bf64_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_bespoke_args1@
hs_bindgen_0bd7971264a6bf64 :: IO (RIP.FunPtr (A -> RIP.CBool -> IO ()))
hs_bindgen_0bd7971264a6bf64 =
  RIP.fromFFIType hs_bindgen_0bd7971264a6bf64_base

{-# NOINLINE bespoke_args1 #-}
{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @macros\/reparse\/reparse.h 94:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
bespoke_args1 :: RIP.FunPtr (A -> RIP.CBool -> IO ())
bespoke_args1 =
  RIP.unsafePerformIO hs_bindgen_0bd7971264a6bf64

-- __unique:__ @test_macrosreparsereparse_Example_get_bespoke_args2@
foreign import ccall unsafe "hs_bindgen_706232c15d20ff49" hs_bindgen_706232c15d20ff49_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_bespoke_args2@
hs_bindgen_706232c15d20ff49 :: IO (RIP.FunPtr (A -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_706232c15d20ff49 =
  RIP.fromFFIType hs_bindgen_706232c15d20ff49_base

{-# NOINLINE bespoke_args2 #-}
{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @macros\/reparse\/reparse.h 95:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
bespoke_args2 :: RIP.FunPtr (A -> HsBindgen.Runtime.LibC.CSize -> IO ())
bespoke_args2 =
  RIP.unsafePerformIO hs_bindgen_706232c15d20ff49

-- __unique:__ @test_macrosreparsereparse_Example_get_bespoke_ret1@
foreign import ccall unsafe "hs_bindgen_ff54c0d6fe585928" hs_bindgen_ff54c0d6fe585928_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_bespoke_ret1@
hs_bindgen_ff54c0d6fe585928 :: IO (RIP.FunPtr (A -> IO RIP.CBool))
hs_bindgen_ff54c0d6fe585928 =
  RIP.fromFFIType hs_bindgen_ff54c0d6fe585928_base

{-# NOINLINE bespoke_ret1 #-}
{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse\/reparse.h 97:8@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
bespoke_ret1 :: RIP.FunPtr (A -> IO RIP.CBool)
bespoke_ret1 =
  RIP.unsafePerformIO hs_bindgen_ff54c0d6fe585928

-- __unique:__ @test_macrosreparsereparse_Example_get_bespoke_ret2@
foreign import ccall unsafe "hs_bindgen_c5dc3c52a26cd344" hs_bindgen_c5dc3c52a26cd344_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_bespoke_ret2@
hs_bindgen_c5dc3c52a26cd344 :: IO (RIP.FunPtr (A -> IO HsBindgen.Runtime.LibC.CSize))
hs_bindgen_c5dc3c52a26cd344 =
  RIP.fromFFIType hs_bindgen_c5dc3c52a26cd344_base

{-# NOINLINE bespoke_ret2 #-}
{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse\/reparse.h 98:8@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
bespoke_ret2 :: RIP.FunPtr (A -> IO HsBindgen.Runtime.LibC.CSize)
bespoke_ret2 =
  RIP.unsafePerformIO hs_bindgen_c5dc3c52a26cd344

-- __unique:__ @test_macrosreparsereparse_Example_get_arr_args1@
foreign import ccall unsafe "hs_bindgen_359bf29bf59196e7" hs_bindgen_359bf29bf59196e7_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_arr_args1@
hs_bindgen_359bf29bf59196e7 :: IO (RIP.FunPtr ((RIP.Ptr (IsA.Elem (IA.IncompleteArray A))) -> IO ()))
hs_bindgen_359bf29bf59196e7 =
  RIP.fromFFIType hs_bindgen_359bf29bf59196e7_base

{-# NOINLINE arr_args1 #-}
{-| Arrays

__C declaration:__ @arr_args1@

__defined at:__ @macros\/reparse\/reparse.h 104:6@

__exported by:__ @macros\/reparse\/reparse.h@
-}
arr_args1 :: RIP.FunPtr ((RIP.Ptr (IsA.Elem (IA.IncompleteArray A))) -> IO ())
arr_args1 =
  RIP.unsafePerformIO hs_bindgen_359bf29bf59196e7

-- __unique:__ @test_macrosreparsereparse_Example_get_arr_args2@
foreign import ccall unsafe "hs_bindgen_b6bce8b55a9de81e" hs_bindgen_b6bce8b55a9de81e_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_arr_args2@
hs_bindgen_b6bce8b55a9de81e :: IO (RIP.FunPtr ((RIP.Ptr (IsA.Elem (IA.IncompleteArray (RIP.Ptr A)))) -> IO ()))
hs_bindgen_b6bce8b55a9de81e =
  RIP.fromFFIType hs_bindgen_b6bce8b55a9de81e_base

{-# NOINLINE arr_args2 #-}
{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse\/reparse.h 105:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
arr_args2 :: RIP.FunPtr ((RIP.Ptr (IsA.Elem (IA.IncompleteArray (RIP.Ptr A)))) -> IO ())
arr_args2 =
  RIP.unsafePerformIO hs_bindgen_b6bce8b55a9de81e

-- __unique:__ @test_macrosreparsereparse_Example_get_arr_args3@
foreign import ccall unsafe "hs_bindgen_f2e44ad1f4fe2e94" hs_bindgen_f2e44ad1f4fe2e94_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_arr_args3@
hs_bindgen_f2e44ad1f4fe2e94 :: IO (RIP.FunPtr ((RIP.Ptr (IsA.Elem ((CA.ConstantArray 5) A))) -> IO ()))
hs_bindgen_f2e44ad1f4fe2e94 =
  RIP.fromFFIType hs_bindgen_f2e44ad1f4fe2e94_base

{-# NOINLINE arr_args3 #-}
{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse\/reparse.h 106:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
arr_args3 :: RIP.FunPtr ((RIP.Ptr (IsA.Elem ((CA.ConstantArray 5) A))) -> IO ())
arr_args3 =
  RIP.unsafePerformIO hs_bindgen_f2e44ad1f4fe2e94

-- __unique:__ @test_macrosreparsereparse_Example_get_arr_args4@
foreign import ccall unsafe "hs_bindgen_65ae5ca2c039185e" hs_bindgen_65ae5ca2c039185e_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_arr_args4@
hs_bindgen_65ae5ca2c039185e :: IO (RIP.FunPtr ((RIP.Ptr (IsA.Elem ((CA.ConstantArray 5) (RIP.Ptr A)))) -> IO ()))
hs_bindgen_65ae5ca2c039185e =
  RIP.fromFFIType hs_bindgen_65ae5ca2c039185e_base

{-# NOINLINE arr_args4 #-}
{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse\/reparse.h 107:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
arr_args4 :: RIP.FunPtr ((RIP.Ptr (IsA.Elem ((CA.ConstantArray 5) (RIP.Ptr A)))) -> IO ())
arr_args4 =
  RIP.unsafePerformIO hs_bindgen_65ae5ca2c039185e

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_args1@
foreign import ccall unsafe "hs_bindgen_91ecc9c2f6ed6603" hs_bindgen_91ecc9c2f6ed6603_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_args1@
hs_bindgen_91ecc9c2f6ed6603 :: IO (RIP.FunPtr (A -> (RIP.FunPtr (IO ())) -> IO ()))
hs_bindgen_91ecc9c2f6ed6603 =
  RIP.fromFFIType hs_bindgen_91ecc9c2f6ed6603_base

{-# NOINLINE funptr_args1 #-}
{-| Function pointers

__C declaration:__ @funptr_args1@

__defined at:__ @macros\/reparse\/reparse.h 126:6@

__exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_args1 :: RIP.FunPtr (A -> (RIP.FunPtr (IO ())) -> IO ())
funptr_args1 =
  RIP.unsafePerformIO hs_bindgen_91ecc9c2f6ed6603

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_args2@
foreign import ccall unsafe "hs_bindgen_3cdcb41e6cc987b9" hs_bindgen_3cdcb41e6cc987b9_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_args2@
hs_bindgen_3cdcb41e6cc987b9 :: IO (RIP.FunPtr (A -> (RIP.FunPtr (IO RIP.CInt)) -> IO ()))
hs_bindgen_3cdcb41e6cc987b9 =
  RIP.fromFFIType hs_bindgen_3cdcb41e6cc987b9_base

{-# NOINLINE funptr_args2 #-}
{-| __C declaration:__ @funptr_args2@

    __defined at:__ @macros\/reparse\/reparse.h 127:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_args2 :: RIP.FunPtr (A -> (RIP.FunPtr (IO RIP.CInt)) -> IO ())
funptr_args2 =
  RIP.unsafePerformIO hs_bindgen_3cdcb41e6cc987b9

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_args3@
foreign import ccall unsafe "hs_bindgen_a6e4b4f8699e5f23" hs_bindgen_a6e4b4f8699e5f23_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_args3@
hs_bindgen_a6e4b4f8699e5f23 :: IO (RIP.FunPtr (A -> (RIP.FunPtr (RIP.CInt -> IO ())) -> IO ()))
hs_bindgen_a6e4b4f8699e5f23 =
  RIP.fromFFIType hs_bindgen_a6e4b4f8699e5f23_base

{-# NOINLINE funptr_args3 #-}
{-| __C declaration:__ @funptr_args3@

    __defined at:__ @macros\/reparse\/reparse.h 128:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_args3 :: RIP.FunPtr (A -> (RIP.FunPtr (RIP.CInt -> IO ())) -> IO ())
funptr_args3 =
  RIP.unsafePerformIO hs_bindgen_a6e4b4f8699e5f23

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_args4@
foreign import ccall unsafe "hs_bindgen_2dd30cc65d616aff" hs_bindgen_2dd30cc65d616aff_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_args4@
hs_bindgen_2dd30cc65d616aff :: IO (RIP.FunPtr (A -> (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO RIP.CChar)) -> IO ()))
hs_bindgen_2dd30cc65d616aff =
  RIP.fromFFIType hs_bindgen_2dd30cc65d616aff_base

{-# NOINLINE funptr_args4 #-}
{-| __C declaration:__ @funptr_args4@

    __defined at:__ @macros\/reparse\/reparse.h 129:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_args4 :: RIP.FunPtr (A -> (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO RIP.CChar)) -> IO ())
funptr_args4 =
  RIP.unsafePerformIO hs_bindgen_2dd30cc65d616aff

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_args5@
foreign import ccall unsafe "hs_bindgen_f2124e2e7c75bb0a" hs_bindgen_f2124e2e7c75bb0a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_args5@
hs_bindgen_f2124e2e7c75bb0a :: IO (RIP.FunPtr (A -> (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt))) -> IO ()))
hs_bindgen_f2124e2e7c75bb0a =
  RIP.fromFFIType hs_bindgen_f2124e2e7c75bb0a_base

{-# NOINLINE funptr_args5 #-}
{-| __C declaration:__ @funptr_args5@

    __defined at:__ @macros\/reparse\/reparse.h 130:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_args5 :: RIP.FunPtr (A -> (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt))) -> IO ())
funptr_args5 =
  RIP.unsafePerformIO hs_bindgen_f2124e2e7c75bb0a

-- __unique:__ @test_macrosreparsereparse_Example_get_comments1@
foreign import ccall unsafe "hs_bindgen_21eadb56a2e5e4a4" hs_bindgen_21eadb56a2e5e4a4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_comments1@
hs_bindgen_21eadb56a2e5e4a4 :: IO (RIP.FunPtr (A -> IO ()))
hs_bindgen_21eadb56a2e5e4a4 =
  RIP.fromFFIType hs_bindgen_21eadb56a2e5e4a4_base

{-# NOINLINE comments1 #-}
{-| Comments in awkward places

  (Prior to language-c we failed to parse there.)

__C declaration:__ @comments1@

__defined at:__ @macros\/reparse\/reparse.h 144:25@

__exported by:__ @macros\/reparse\/reparse.h@
-}
comments1 :: RIP.FunPtr (A -> IO ())
comments1 =
  RIP.unsafePerformIO hs_bindgen_21eadb56a2e5e4a4

-- __unique:__ @test_macrosreparsereparse_Example_get_const_prim_before1@
foreign import ccall unsafe "hs_bindgen_cd4862b0e302d655" hs_bindgen_cd4862b0e302d655_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_prim_before1@
hs_bindgen_cd4862b0e302d655 :: IO (RIP.FunPtr (A -> RIP.CChar -> IO ()))
hs_bindgen_cd4862b0e302d655 =
  RIP.fromFFIType hs_bindgen_cd4862b0e302d655_base

{-# NOINLINE const_prim_before1 #-}
{-| `const` qualifier

  NOTE: These were not parsed correctly prior to the switch to language-c.

__C declaration:__ @const_prim_before1@

__defined at:__ @macros\/reparse\/reparse.h 179:6@

__exported by:__ @macros\/reparse\/reparse.h@
-}
const_prim_before1 :: RIP.FunPtr (A -> RIP.CChar -> IO ())
const_prim_before1 =
  RIP.unsafePerformIO hs_bindgen_cd4862b0e302d655

-- __unique:__ @test_macrosreparsereparse_Example_get_const_prim_before2@
foreign import ccall unsafe "hs_bindgen_dc00f3fb59caef58" hs_bindgen_dc00f3fb59caef58_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_prim_before2@
hs_bindgen_dc00f3fb59caef58 :: IO (RIP.FunPtr (A -> RIP.CSChar -> IO ()))
hs_bindgen_dc00f3fb59caef58 =
  RIP.fromFFIType hs_bindgen_dc00f3fb59caef58_base

{-# NOINLINE const_prim_before2 #-}
{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @macros\/reparse\/reparse.h 180:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_prim_before2 :: RIP.FunPtr (A -> RIP.CSChar -> IO ())
const_prim_before2 =
  RIP.unsafePerformIO hs_bindgen_dc00f3fb59caef58

-- __unique:__ @test_macrosreparsereparse_Example_get_const_prim_before3@
foreign import ccall unsafe "hs_bindgen_bf5a8ac1f5200d40" hs_bindgen_bf5a8ac1f5200d40_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_prim_before3@
hs_bindgen_bf5a8ac1f5200d40 :: IO (RIP.FunPtr (A -> RIP.CUChar -> IO ()))
hs_bindgen_bf5a8ac1f5200d40 =
  RIP.fromFFIType hs_bindgen_bf5a8ac1f5200d40_base

{-# NOINLINE const_prim_before3 #-}
{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @macros\/reparse\/reparse.h 181:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_prim_before3 :: RIP.FunPtr (A -> RIP.CUChar -> IO ())
const_prim_before3 =
  RIP.unsafePerformIO hs_bindgen_bf5a8ac1f5200d40

-- __unique:__ @test_macrosreparsereparse_Example_get_const_prim_after1@
foreign import ccall unsafe "hs_bindgen_42b9029e89dc27ee" hs_bindgen_42b9029e89dc27ee_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_prim_after1@
hs_bindgen_42b9029e89dc27ee :: IO (RIP.FunPtr (A -> RIP.CChar -> IO ()))
hs_bindgen_42b9029e89dc27ee =
  RIP.fromFFIType hs_bindgen_42b9029e89dc27ee_base

{-# NOINLINE const_prim_after1 #-}
{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @macros\/reparse\/reparse.h 182:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_prim_after1 :: RIP.FunPtr (A -> RIP.CChar -> IO ())
const_prim_after1 =
  RIP.unsafePerformIO hs_bindgen_42b9029e89dc27ee

-- __unique:__ @test_macrosreparsereparse_Example_get_const_prim_after2@
foreign import ccall unsafe "hs_bindgen_46259eab2e8f6f43" hs_bindgen_46259eab2e8f6f43_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_prim_after2@
hs_bindgen_46259eab2e8f6f43 :: IO (RIP.FunPtr (A -> RIP.CSChar -> IO ()))
hs_bindgen_46259eab2e8f6f43 =
  RIP.fromFFIType hs_bindgen_46259eab2e8f6f43_base

{-# NOINLINE const_prim_after2 #-}
{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @macros\/reparse\/reparse.h 183:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_prim_after2 :: RIP.FunPtr (A -> RIP.CSChar -> IO ())
const_prim_after2 =
  RIP.unsafePerformIO hs_bindgen_46259eab2e8f6f43

-- __unique:__ @test_macrosreparsereparse_Example_get_const_prim_after3@
foreign import ccall unsafe "hs_bindgen_ae9ff7d244d2d09f" hs_bindgen_ae9ff7d244d2d09f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_prim_after3@
hs_bindgen_ae9ff7d244d2d09f :: IO (RIP.FunPtr (A -> RIP.CUChar -> IO ()))
hs_bindgen_ae9ff7d244d2d09f =
  RIP.fromFFIType hs_bindgen_ae9ff7d244d2d09f_base

{-# NOINLINE const_prim_after3 #-}
{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @macros\/reparse\/reparse.h 184:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_prim_after3 :: RIP.FunPtr (A -> RIP.CUChar -> IO ())
const_prim_after3 =
  RIP.unsafePerformIO hs_bindgen_ae9ff7d244d2d09f

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_before1@
foreign import ccall unsafe "hs_bindgen_cdc21c51762e7a1d" hs_bindgen_cdc21c51762e7a1d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_before1@
hs_bindgen_cdc21c51762e7a1d :: IO (RIP.FunPtr (A -> RIP.CFloat -> IO ()))
hs_bindgen_cdc21c51762e7a1d =
  RIP.fromFFIType hs_bindgen_cdc21c51762e7a1d_base

{-# NOINLINE const_withoutSign_before1 #-}
{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @macros\/reparse\/reparse.h 188:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_before1 :: RIP.FunPtr (A -> RIP.CFloat -> IO ())
const_withoutSign_before1 =
  RIP.unsafePerformIO hs_bindgen_cdc21c51762e7a1d

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_before2@
foreign import ccall unsafe "hs_bindgen_3d2ac579d88a02e4" hs_bindgen_3d2ac579d88a02e4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_before2@
hs_bindgen_3d2ac579d88a02e4 :: IO (RIP.FunPtr (A -> RIP.CDouble -> IO ()))
hs_bindgen_3d2ac579d88a02e4 =
  RIP.fromFFIType hs_bindgen_3d2ac579d88a02e4_base

{-# NOINLINE const_withoutSign_before2 #-}
{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @macros\/reparse\/reparse.h 189:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_before2 :: RIP.FunPtr (A -> RIP.CDouble -> IO ())
const_withoutSign_before2 =
  RIP.unsafePerformIO hs_bindgen_3d2ac579d88a02e4

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_before3@
foreign import ccall unsafe "hs_bindgen_9763d5925860a2d5" hs_bindgen_9763d5925860a2d5_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_before3@
hs_bindgen_9763d5925860a2d5 :: IO (RIP.FunPtr (A -> RIP.CBool -> IO ()))
hs_bindgen_9763d5925860a2d5 =
  RIP.fromFFIType hs_bindgen_9763d5925860a2d5_base

{-# NOINLINE const_withoutSign_before3 #-}
{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @macros\/reparse\/reparse.h 190:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_before3 :: RIP.FunPtr (A -> RIP.CBool -> IO ())
const_withoutSign_before3 =
  RIP.unsafePerformIO hs_bindgen_9763d5925860a2d5

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_before4@
foreign import ccall unsafe "hs_bindgen_a2f5cc9d4c6ca636" hs_bindgen_a2f5cc9d4c6ca636_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_before4@
hs_bindgen_a2f5cc9d4c6ca636 :: IO (RIP.FunPtr (A -> Some_struct -> IO ()))
hs_bindgen_a2f5cc9d4c6ca636 =
  RIP.fromFFIType hs_bindgen_a2f5cc9d4c6ca636_base

{-# NOINLINE const_withoutSign_before4 #-}
{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @macros\/reparse\/reparse.h 191:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_before4 :: RIP.FunPtr (A -> Some_struct -> IO ())
const_withoutSign_before4 =
  RIP.unsafePerformIO hs_bindgen_a2f5cc9d4c6ca636

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_before5@
foreign import ccall unsafe "hs_bindgen_7d32427f93f35862" hs_bindgen_7d32427f93f35862_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_before5@
hs_bindgen_7d32427f93f35862 :: IO (RIP.FunPtr (A -> Some_union -> IO ()))
hs_bindgen_7d32427f93f35862 =
  RIP.fromFFIType hs_bindgen_7d32427f93f35862_base

{-# NOINLINE const_withoutSign_before5 #-}
{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @macros\/reparse\/reparse.h 192:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_before5 :: RIP.FunPtr (A -> Some_union -> IO ())
const_withoutSign_before5 =
  RIP.unsafePerformIO hs_bindgen_7d32427f93f35862

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_before6@
foreign import ccall unsafe "hs_bindgen_e670df8520dfe7b8" hs_bindgen_e670df8520dfe7b8_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_before6@
hs_bindgen_e670df8520dfe7b8 :: IO (RIP.FunPtr (A -> Some_enum -> IO ()))
hs_bindgen_e670df8520dfe7b8 =
  RIP.fromFFIType hs_bindgen_e670df8520dfe7b8_base

{-# NOINLINE const_withoutSign_before6 #-}
{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @macros\/reparse\/reparse.h 193:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_before6 :: RIP.FunPtr (A -> Some_enum -> IO ())
const_withoutSign_before6 =
  RIP.unsafePerformIO hs_bindgen_e670df8520dfe7b8

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_before7@
foreign import ccall unsafe "hs_bindgen_daa0cd5e000d286b" hs_bindgen_daa0cd5e000d286b_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_before7@
hs_bindgen_daa0cd5e000d286b :: IO (RIP.FunPtr (A -> RIP.CBool -> IO ()))
hs_bindgen_daa0cd5e000d286b =
  RIP.fromFFIType hs_bindgen_daa0cd5e000d286b_base

{-# NOINLINE const_withoutSign_before7 #-}
{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @macros\/reparse\/reparse.h 194:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_before7 :: RIP.FunPtr (A -> RIP.CBool -> IO ())
const_withoutSign_before7 =
  RIP.unsafePerformIO hs_bindgen_daa0cd5e000d286b

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_before8@
foreign import ccall unsafe "hs_bindgen_cb0db7618a34b1da" hs_bindgen_cb0db7618a34b1da_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_before8@
hs_bindgen_cb0db7618a34b1da :: IO (RIP.FunPtr (A -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_cb0db7618a34b1da =
  RIP.fromFFIType hs_bindgen_cb0db7618a34b1da_base

{-# NOINLINE const_withoutSign_before8 #-}
{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @macros\/reparse\/reparse.h 195:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_before8 :: RIP.FunPtr (A -> HsBindgen.Runtime.LibC.CSize -> IO ())
const_withoutSign_before8 =
  RIP.unsafePerformIO hs_bindgen_cb0db7618a34b1da

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_after1@
foreign import ccall unsafe "hs_bindgen_27af6dd95fea1fa5" hs_bindgen_27af6dd95fea1fa5_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_after1@
hs_bindgen_27af6dd95fea1fa5 :: IO (RIP.FunPtr (A -> RIP.CFloat -> IO ()))
hs_bindgen_27af6dd95fea1fa5 =
  RIP.fromFFIType hs_bindgen_27af6dd95fea1fa5_base

{-# NOINLINE const_withoutSign_after1 #-}
{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @macros\/reparse\/reparse.h 197:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_after1 :: RIP.FunPtr (A -> RIP.CFloat -> IO ())
const_withoutSign_after1 =
  RIP.unsafePerformIO hs_bindgen_27af6dd95fea1fa5

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_after2@
foreign import ccall unsafe "hs_bindgen_1cdb662a9cf0647a" hs_bindgen_1cdb662a9cf0647a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_after2@
hs_bindgen_1cdb662a9cf0647a :: IO (RIP.FunPtr (A -> RIP.CDouble -> IO ()))
hs_bindgen_1cdb662a9cf0647a =
  RIP.fromFFIType hs_bindgen_1cdb662a9cf0647a_base

{-# NOINLINE const_withoutSign_after2 #-}
{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @macros\/reparse\/reparse.h 198:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_after2 :: RIP.FunPtr (A -> RIP.CDouble -> IO ())
const_withoutSign_after2 =
  RIP.unsafePerformIO hs_bindgen_1cdb662a9cf0647a

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_after3@
foreign import ccall unsafe "hs_bindgen_4094ded85265b2e8" hs_bindgen_4094ded85265b2e8_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_after3@
hs_bindgen_4094ded85265b2e8 :: IO (RIP.FunPtr (A -> RIP.CBool -> IO ()))
hs_bindgen_4094ded85265b2e8 =
  RIP.fromFFIType hs_bindgen_4094ded85265b2e8_base

{-# NOINLINE const_withoutSign_after3 #-}
{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @macros\/reparse\/reparse.h 199:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_after3 :: RIP.FunPtr (A -> RIP.CBool -> IO ())
const_withoutSign_after3 =
  RIP.unsafePerformIO hs_bindgen_4094ded85265b2e8

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_after4@
foreign import ccall unsafe "hs_bindgen_703ef6e2bc66a459" hs_bindgen_703ef6e2bc66a459_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_after4@
hs_bindgen_703ef6e2bc66a459 :: IO (RIP.FunPtr (A -> Some_struct -> IO ()))
hs_bindgen_703ef6e2bc66a459 =
  RIP.fromFFIType hs_bindgen_703ef6e2bc66a459_base

{-# NOINLINE const_withoutSign_after4 #-}
{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @macros\/reparse\/reparse.h 200:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_after4 :: RIP.FunPtr (A -> Some_struct -> IO ())
const_withoutSign_after4 =
  RIP.unsafePerformIO hs_bindgen_703ef6e2bc66a459

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_after5@
foreign import ccall unsafe "hs_bindgen_0afa081c272debb4" hs_bindgen_0afa081c272debb4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_after5@
hs_bindgen_0afa081c272debb4 :: IO (RIP.FunPtr (A -> Some_union -> IO ()))
hs_bindgen_0afa081c272debb4 =
  RIP.fromFFIType hs_bindgen_0afa081c272debb4_base

{-# NOINLINE const_withoutSign_after5 #-}
{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @macros\/reparse\/reparse.h 201:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_after5 :: RIP.FunPtr (A -> Some_union -> IO ())
const_withoutSign_after5 =
  RIP.unsafePerformIO hs_bindgen_0afa081c272debb4

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_after6@
foreign import ccall unsafe "hs_bindgen_1e6fa74fb181999d" hs_bindgen_1e6fa74fb181999d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_after6@
hs_bindgen_1e6fa74fb181999d :: IO (RIP.FunPtr (A -> Some_enum -> IO ()))
hs_bindgen_1e6fa74fb181999d =
  RIP.fromFFIType hs_bindgen_1e6fa74fb181999d_base

{-# NOINLINE const_withoutSign_after6 #-}
{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @macros\/reparse\/reparse.h 202:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_after6 :: RIP.FunPtr (A -> Some_enum -> IO ())
const_withoutSign_after6 =
  RIP.unsafePerformIO hs_bindgen_1e6fa74fb181999d

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_after7@
foreign import ccall unsafe "hs_bindgen_4e9fbac9079b42df" hs_bindgen_4e9fbac9079b42df_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_after7@
hs_bindgen_4e9fbac9079b42df :: IO (RIP.FunPtr (A -> RIP.CBool -> IO ()))
hs_bindgen_4e9fbac9079b42df =
  RIP.fromFFIType hs_bindgen_4e9fbac9079b42df_base

{-# NOINLINE const_withoutSign_after7 #-}
{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @macros\/reparse\/reparse.h 203:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_after7 :: RIP.FunPtr (A -> RIP.CBool -> IO ())
const_withoutSign_after7 =
  RIP.unsafePerformIO hs_bindgen_4e9fbac9079b42df

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_after8@
foreign import ccall unsafe "hs_bindgen_b09b86ef1a4a302f" hs_bindgen_b09b86ef1a4a302f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_withoutSign_after8@
hs_bindgen_b09b86ef1a4a302f :: IO (RIP.FunPtr (A -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_b09b86ef1a4a302f =
  RIP.fromFFIType hs_bindgen_b09b86ef1a4a302f_base

{-# NOINLINE const_withoutSign_after8 #-}
{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @macros\/reparse\/reparse.h 204:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_after8 :: RIP.FunPtr (A -> HsBindgen.Runtime.LibC.CSize -> IO ())
const_withoutSign_after8 =
  RIP.unsafePerformIO hs_bindgen_b09b86ef1a4a302f

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_args1@
foreign import ccall unsafe "hs_bindgen_8c4314aad3357d3b" hs_bindgen_8c4314aad3357d3b_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_args1@
hs_bindgen_8c4314aad3357d3b :: IO (RIP.FunPtr (A -> (PtrConst.PtrConst RIP.CInt) -> IO ()))
hs_bindgen_8c4314aad3357d3b =
  RIP.fromFFIType hs_bindgen_8c4314aad3357d3b_base

{-# NOINLINE const_pointers_args1 #-}
{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @macros\/reparse\/reparse.h 208:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_args1 :: RIP.FunPtr (A -> (PtrConst.PtrConst RIP.CInt) -> IO ())
const_pointers_args1 =
  RIP.unsafePerformIO hs_bindgen_8c4314aad3357d3b

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_args2@
foreign import ccall unsafe "hs_bindgen_4f42493cc43d6dc0" hs_bindgen_4f42493cc43d6dc0_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_args2@
hs_bindgen_4f42493cc43d6dc0 :: IO (RIP.FunPtr (A -> (PtrConst.PtrConst RIP.CInt) -> IO ()))
hs_bindgen_4f42493cc43d6dc0 =
  RIP.fromFFIType hs_bindgen_4f42493cc43d6dc0_base

{-# NOINLINE const_pointers_args2 #-}
{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @macros\/reparse\/reparse.h 209:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_args2 :: RIP.FunPtr (A -> (PtrConst.PtrConst RIP.CInt) -> IO ())
const_pointers_args2 =
  RIP.unsafePerformIO hs_bindgen_4f42493cc43d6dc0

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_args3@
foreign import ccall unsafe "hs_bindgen_52332819a6e33524" hs_bindgen_52332819a6e33524_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_args3@
hs_bindgen_52332819a6e33524 :: IO (RIP.FunPtr (A -> (RIP.Ptr RIP.CInt) -> IO ()))
hs_bindgen_52332819a6e33524 =
  RIP.fromFFIType hs_bindgen_52332819a6e33524_base

{-# NOINLINE const_pointers_args3 #-}
{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @macros\/reparse\/reparse.h 210:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_args3 :: RIP.FunPtr (A -> (RIP.Ptr RIP.CInt) -> IO ())
const_pointers_args3 =
  RIP.unsafePerformIO hs_bindgen_52332819a6e33524

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_args4@
foreign import ccall unsafe "hs_bindgen_4e35df98dff13f86" hs_bindgen_4e35df98dff13f86_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_args4@
hs_bindgen_4e35df98dff13f86 :: IO (RIP.FunPtr (A -> (PtrConst.PtrConst RIP.CInt) -> IO ()))
hs_bindgen_4e35df98dff13f86 =
  RIP.fromFFIType hs_bindgen_4e35df98dff13f86_base

{-# NOINLINE const_pointers_args4 #-}
{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @macros\/reparse\/reparse.h 211:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_args4 :: RIP.FunPtr (A -> (PtrConst.PtrConst RIP.CInt) -> IO ())
const_pointers_args4 =
  RIP.unsafePerformIO hs_bindgen_4e35df98dff13f86

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_args5@
foreign import ccall unsafe "hs_bindgen_1caf0649ea276f7c" hs_bindgen_1caf0649ea276f7c_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_args5@
hs_bindgen_1caf0649ea276f7c :: IO (RIP.FunPtr (A -> (PtrConst.PtrConst RIP.CInt) -> IO ()))
hs_bindgen_1caf0649ea276f7c =
  RIP.fromFFIType hs_bindgen_1caf0649ea276f7c_base

{-# NOINLINE const_pointers_args5 #-}
{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @macros\/reparse\/reparse.h 212:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_args5 :: RIP.FunPtr (A -> (PtrConst.PtrConst RIP.CInt) -> IO ())
const_pointers_args5 =
  RIP.unsafePerformIO hs_bindgen_1caf0649ea276f7c

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_ret1@
foreign import ccall unsafe "hs_bindgen_1ccc4e5ba93fcf35" hs_bindgen_1ccc4e5ba93fcf35_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_ret1@
hs_bindgen_1ccc4e5ba93fcf35 :: IO (RIP.FunPtr (A -> IO (PtrConst.PtrConst RIP.CInt)))
hs_bindgen_1ccc4e5ba93fcf35 =
  RIP.fromFFIType hs_bindgen_1ccc4e5ba93fcf35_base

{-# NOINLINE const_pointers_ret1 #-}
{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse\/reparse.h 214:19@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_ret1 :: RIP.FunPtr (A -> IO (PtrConst.PtrConst RIP.CInt))
const_pointers_ret1 =
  RIP.unsafePerformIO hs_bindgen_1ccc4e5ba93fcf35

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_ret2@
foreign import ccall unsafe "hs_bindgen_9eb816dfd37773cf" hs_bindgen_9eb816dfd37773cf_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_ret2@
hs_bindgen_9eb816dfd37773cf :: IO (RIP.FunPtr (A -> IO (PtrConst.PtrConst RIP.CInt)))
hs_bindgen_9eb816dfd37773cf =
  RIP.fromFFIType hs_bindgen_9eb816dfd37773cf_base

{-# NOINLINE const_pointers_ret2 #-}
{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse\/reparse.h 215:19@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_ret2 :: RIP.FunPtr (A -> IO (PtrConst.PtrConst RIP.CInt))
const_pointers_ret2 =
  RIP.unsafePerformIO hs_bindgen_9eb816dfd37773cf

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_ret3@
foreign import ccall unsafe "hs_bindgen_14b8233170e4a4a1" hs_bindgen_14b8233170e4a4a1_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_ret3@
hs_bindgen_14b8233170e4a4a1 :: IO (RIP.FunPtr (A -> IO (RIP.Ptr RIP.CInt)))
hs_bindgen_14b8233170e4a4a1 =
  RIP.fromFFIType hs_bindgen_14b8233170e4a4a1_base

{-# NOINLINE const_pointers_ret3 #-}
{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse\/reparse.h 216:19@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_ret3 :: RIP.FunPtr (A -> IO (RIP.Ptr RIP.CInt))
const_pointers_ret3 =
  RIP.unsafePerformIO hs_bindgen_14b8233170e4a4a1

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_ret4@
foreign import ccall unsafe "hs_bindgen_e41a772a388000d9" hs_bindgen_e41a772a388000d9_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_ret4@
hs_bindgen_e41a772a388000d9 :: IO (RIP.FunPtr (A -> IO (PtrConst.PtrConst RIP.CInt)))
hs_bindgen_e41a772a388000d9 =
  RIP.fromFFIType hs_bindgen_e41a772a388000d9_base

{-# NOINLINE const_pointers_ret4 #-}
{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse\/reparse.h 217:19@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_ret4 :: RIP.FunPtr (A -> IO (PtrConst.PtrConst RIP.CInt))
const_pointers_ret4 =
  RIP.unsafePerformIO hs_bindgen_e41a772a388000d9

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_ret5@
foreign import ccall unsafe "hs_bindgen_6f67bfe897ac3801" hs_bindgen_6f67bfe897ac3801_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_pointers_ret5@
hs_bindgen_6f67bfe897ac3801 :: IO (RIP.FunPtr (A -> IO (PtrConst.PtrConst RIP.CInt)))
hs_bindgen_6f67bfe897ac3801 =
  RIP.fromFFIType hs_bindgen_6f67bfe897ac3801_base

{-# NOINLINE const_pointers_ret5 #-}
{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse\/reparse.h 218:19@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_ret5 :: RIP.FunPtr (A -> IO (PtrConst.PtrConst RIP.CInt))
const_pointers_ret5 =
  RIP.unsafePerformIO hs_bindgen_6f67bfe897ac3801

-- __unique:__ @test_macrosreparsereparse_Example_get_const_array_elem1@
foreign import ccall unsafe "hs_bindgen_f7dbf9e037616d58" hs_bindgen_f7dbf9e037616d58_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_array_elem1@
hs_bindgen_f7dbf9e037616d58 :: IO (RIP.FunPtr ((PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray A))) -> IO ()))
hs_bindgen_f7dbf9e037616d58 =
  RIP.fromFFIType hs_bindgen_f7dbf9e037616d58_base

{-# NOINLINE const_array_elem1 #-}
{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse\/reparse.h 246:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_array_elem1 :: RIP.FunPtr ((PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray A))) -> IO ())
const_array_elem1 =
  RIP.unsafePerformIO hs_bindgen_f7dbf9e037616d58

-- __unique:__ @test_macrosreparsereparse_Example_get_const_array_elem2@
foreign import ccall unsafe "hs_bindgen_9ffee618c9ba69df" hs_bindgen_9ffee618c9ba69df_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_array_elem2@
hs_bindgen_9ffee618c9ba69df :: IO (RIP.FunPtr ((RIP.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst A)))) -> IO ()))
hs_bindgen_9ffee618c9ba69df =
  RIP.fromFFIType hs_bindgen_9ffee618c9ba69df_base

{-# NOINLINE const_array_elem2 #-}
{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse\/reparse.h 247:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_array_elem2 :: RIP.FunPtr ((RIP.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst A)))) -> IO ())
const_array_elem2 =
  RIP.unsafePerformIO hs_bindgen_9ffee618c9ba69df

-- __unique:__ @test_macrosreparsereparse_Example_get_const_array_elem3@
foreign import ccall unsafe "hs_bindgen_2d15e55c091ec37e" hs_bindgen_2d15e55c091ec37e_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_const_array_elem3@
hs_bindgen_2d15e55c091ec37e :: IO (RIP.FunPtr ((PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (RIP.Ptr A)))) -> IO ()))
hs_bindgen_2d15e55c091ec37e =
  RIP.fromFFIType hs_bindgen_2d15e55c091ec37e_base

{-# NOINLINE const_array_elem3 #-}
{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse\/reparse.h 248:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_array_elem3 :: RIP.FunPtr ((PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (RIP.Ptr A)))) -> IO ())
const_array_elem3 =
  RIP.unsafePerformIO hs_bindgen_2d15e55c091ec37e

-- __unique:__ @test_macrosreparsereparse_Example_get_noParams1@
foreign import ccall unsafe "hs_bindgen_31bf3a43a9ecac51" hs_bindgen_31bf3a43a9ecac51_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_noParams1@
hs_bindgen_31bf3a43a9ecac51 :: IO (RIP.FunPtr (IO A))
hs_bindgen_31bf3a43a9ecac51 =
  RIP.fromFFIType hs_bindgen_31bf3a43a9ecac51_base

{-# NOINLINE noParams1 #-}
{-| Other examples we reparsed /incorrectly/ before language-c

__C declaration:__ @noParams1@

__defined at:__ @macros\/reparse\/reparse.h 256:3@

__exported by:__ @macros\/reparse\/reparse.h@
-}
noParams1 :: RIP.FunPtr (IO A)
noParams1 =
  RIP.unsafePerformIO hs_bindgen_31bf3a43a9ecac51

-- __unique:__ @test_macrosreparsereparse_Example_get_noParams2@
foreign import ccall unsafe "hs_bindgen_89ebc691d8a57938" hs_bindgen_89ebc691d8a57938_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_noParams2@
hs_bindgen_89ebc691d8a57938 :: IO (RIP.FunPtr (IO A))
hs_bindgen_89ebc691d8a57938 =
  RIP.fromFFIType hs_bindgen_89ebc691d8a57938_base

{-# NOINLINE noParams2 #-}
{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse\/reparse.h 257:3@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
noParams2 :: RIP.FunPtr (IO A)
noParams2 =
  RIP.unsafePerformIO hs_bindgen_89ebc691d8a57938

-- __unique:__ @test_macrosreparsereparse_Example_get_noParams3@
foreign import ccall unsafe "hs_bindgen_5dc2b26d52df36c1" hs_bindgen_5dc2b26d52df36c1_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_noParams3@
hs_bindgen_5dc2b26d52df36c1 :: IO (RIP.FunPtr (A -> (RIP.FunPtr (IO RIP.CInt)) -> IO ()))
hs_bindgen_5dc2b26d52df36c1 =
  RIP.fromFFIType hs_bindgen_5dc2b26d52df36c1_base

{-# NOINLINE noParams3 #-}
{-| __C declaration:__ @noParams3@

    __defined at:__ @macros\/reparse\/reparse.h 258:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
noParams3 :: RIP.FunPtr (A -> (RIP.FunPtr (IO RIP.CInt)) -> IO ())
noParams3 =
  RIP.unsafePerformIO hs_bindgen_5dc2b26d52df36c1

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret1@
foreign import ccall unsafe "hs_bindgen_30400d1c4a66532a" hs_bindgen_30400d1c4a66532a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret1@
hs_bindgen_30400d1c4a66532a :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (IO ()))))
hs_bindgen_30400d1c4a66532a =
  RIP.fromFFIType hs_bindgen_30400d1c4a66532a_base

{-# NOINLINE funptr_ret1 #-}
{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse\/reparse.h 262:8@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret1 :: RIP.FunPtr (A -> IO (RIP.FunPtr (IO ())))
funptr_ret1 =
  RIP.unsafePerformIO hs_bindgen_30400d1c4a66532a

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret2@
foreign import ccall unsafe "hs_bindgen_6f4960a7e9d7d73a" hs_bindgen_6f4960a7e9d7d73a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret2@
hs_bindgen_6f4960a7e9d7d73a :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (IO RIP.CInt))))
hs_bindgen_6f4960a7e9d7d73a =
  RIP.fromFFIType hs_bindgen_6f4960a7e9d7d73a_base

{-# NOINLINE funptr_ret2 #-}
{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse\/reparse.h 263:8@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret2 :: RIP.FunPtr (A -> IO (RIP.FunPtr (IO RIP.CInt)))
funptr_ret2 =
  RIP.unsafePerformIO hs_bindgen_6f4960a7e9d7d73a

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret3@
foreign import ccall unsafe "hs_bindgen_104f2e9a3d744d43" hs_bindgen_104f2e9a3d744d43_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret3@
hs_bindgen_104f2e9a3d744d43 :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> IO ()))))
hs_bindgen_104f2e9a3d744d43 =
  RIP.fromFFIType hs_bindgen_104f2e9a3d744d43_base

{-# NOINLINE funptr_ret3 #-}
{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse\/reparse.h 264:8@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret3 :: RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> IO ())))
funptr_ret3 =
  RIP.unsafePerformIO hs_bindgen_104f2e9a3d744d43

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret4@
foreign import ccall unsafe "hs_bindgen_6feb948588d84bd8" hs_bindgen_6feb948588d84bd8_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret4@
hs_bindgen_6feb948588d84bd8 :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO RIP.CChar))))
hs_bindgen_6feb948588d84bd8 =
  RIP.fromFFIType hs_bindgen_6feb948588d84bd8_base

{-# NOINLINE funptr_ret4 #-}
{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse\/reparse.h 265:8@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret4 :: RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO RIP.CChar)))
funptr_ret4 =
  RIP.unsafePerformIO hs_bindgen_6feb948588d84bd8

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret5@
foreign import ccall unsafe "hs_bindgen_e32443b8a050161d" hs_bindgen_e32443b8a050161d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret5@
hs_bindgen_e32443b8a050161d :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt)))))
hs_bindgen_e32443b8a050161d =
  RIP.fromFFIType hs_bindgen_e32443b8a050161d_base

{-# NOINLINE funptr_ret5 #-}
{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse\/reparse.h 269:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret5 :: RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt))))
funptr_ret5 =
  RIP.unsafePerformIO hs_bindgen_e32443b8a050161d

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret6@
foreign import ccall unsafe "hs_bindgen_7e14d72951bba6e3" hs_bindgen_7e14d72951bba6e3_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret6@
hs_bindgen_7e14d72951bba6e3 :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))))
hs_bindgen_7e14d72951bba6e3 =
  RIP.fromFFIType hs_bindgen_7e14d72951bba6e3_base

{-# NOINLINE funptr_ret6 #-}
{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse\/reparse.h 270:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret6 :: RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt))))
funptr_ret6 =
  RIP.unsafePerformIO hs_bindgen_7e14d72951bba6e3

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret7@
foreign import ccall unsafe "hs_bindgen_a05584c46e3d1f5c" hs_bindgen_a05584c46e3d1f5c_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret7@
hs_bindgen_a05584c46e3d1f5c :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))))
hs_bindgen_a05584c46e3d1f5c =
  RIP.fromFFIType hs_bindgen_a05584c46e3d1f5c_base

{-# NOINLINE funptr_ret7 #-}
{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse\/reparse.h 271:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret7 :: RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt))))
funptr_ret7 =
  RIP.unsafePerformIO hs_bindgen_a05584c46e3d1f5c

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret8@
foreign import ccall unsafe "hs_bindgen_c7f5a58273570329" hs_bindgen_c7f5a58273570329_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret8@
hs_bindgen_c7f5a58273570329 :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt)))))
hs_bindgen_c7f5a58273570329 =
  RIP.fromFFIType hs_bindgen_c7f5a58273570329_base

{-# NOINLINE funptr_ret8 #-}
{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse\/reparse.h 272:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret8 :: RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt))))
funptr_ret8 =
  RIP.unsafePerformIO hs_bindgen_c7f5a58273570329

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret9@
foreign import ccall unsafe "hs_bindgen_f5bab8520c61852b" hs_bindgen_f5bab8520c61852b_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret9@
hs_bindgen_f5bab8520c61852b :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))))
hs_bindgen_f5bab8520c61852b =
  RIP.fromFFIType hs_bindgen_f5bab8520c61852b_base

{-# NOINLINE funptr_ret9 #-}
{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse\/reparse.h 273:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret9 :: RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt))))
funptr_ret9 =
  RIP.unsafePerformIO hs_bindgen_f5bab8520c61852b

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret10@
foreign import ccall unsafe "hs_bindgen_e43abe6c9a357e5c" hs_bindgen_e43abe6c9a357e5c_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_get_funptr_ret10@
hs_bindgen_e43abe6c9a357e5c :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))))
hs_bindgen_e43abe6c9a357e5c =
  RIP.fromFFIType hs_bindgen_e43abe6c9a357e5c_base

{-# NOINLINE funptr_ret10 #-}
{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse\/reparse.h 274:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret10 :: RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt))))
funptr_ret10 =
  RIP.unsafePerformIO hs_bindgen_e43abe6c9a357e5c
