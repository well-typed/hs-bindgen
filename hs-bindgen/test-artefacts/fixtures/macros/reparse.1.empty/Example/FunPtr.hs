{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse.h>"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_char1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2bfe3e3f7e57b3ed (void)) ("
  , "  signed int arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &args_char1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_char2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_823cf196a07a68cc (void)) ("
  , "  signed int arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  return &args_char2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_char3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c42bec91132d7124 (void)) ("
  , "  signed int arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  return &args_char3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_short1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_afad3baea1431062 (void)) ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_short2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c3734c97c5e64c56 (void)) ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_short3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d0673a1033816c0b (void)) ("
  , "  signed int arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  return &args_short3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_int1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_88f1182383c6432a (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_int2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_bc197f9358cd11b4 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_int3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b84219e461ae5f25 (void)) ("
  , "  signed int arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  return &args_int3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_long1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_68442e1aadc5a0d3 (void)) ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_long2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5e8e40d28c06a172 (void)) ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_long3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d3db689a6d7b27e0 (void)) ("
  , "  signed int arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  return &args_long3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_float */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5d87ce6909d695da (void)) ("
  , "  signed int arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  return &args_float;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_double */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6ee90e5314b16cf4 (void)) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &args_double;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_bool1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d3783c9e52e30d08 (void)) ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &args_bool1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_struct */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2ac83bf702102758 (void)) ("
  , "  signed int arg1,"
  , "  struct some_struct arg2"
  , ")"
  , "{"
  , "  return &args_struct;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_union */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_700dff64ce96b9c7 (void)) ("
  , "  signed int arg1,"
  , "  union some_union arg2"
  , ")"
  , "{"
  , "  return &args_union;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_enum */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2263a2832efce5d2 (void)) ("
  , "  signed int arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  return &args_enum;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_pointer1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_337624a9df6f893f (void)) ("
  , "  signed int arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return &args_pointer1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_pointer2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e86c13d7a42aaecb (void)) ("
  , "  signed int arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  return &args_pointer2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_pointer3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4f49ea6cc2d47cb9 (void)) ("
  , "  signed int arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  return &args_pointer3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_A */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_4eabc39d58d3eea7 (void)) (void)"
  , "{"
  , "  return &ret_A;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_char1 */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_0e9cabb6650a5827 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_char1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_char2 */"
  , "__attribute__ ((const))"
  , "signed char (*hs_bindgen_38e274f6e0726048 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_char2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_char3 */"
  , "__attribute__ ((const))"
  , "unsigned char (*hs_bindgen_621d5d07f2e26bbd (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_char3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_short1 */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_f6b29f7ddb032607 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_short1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_short2 */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_65df37dcb1977468 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_short2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_short3 */"
  , "__attribute__ ((const))"
  , "unsigned short (*hs_bindgen_c2074f909866d753 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_short3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_int1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_9e5aed81836cdc4e (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_int1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_int2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a466e4b333684935 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_int2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_int3 */"
  , "__attribute__ ((const))"
  , "unsigned int (*hs_bindgen_67adc838c559816b (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_int3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_long1 */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_3795354110c96def (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_long1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_long2 */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_f5d22573fdc7a51c (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_long2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_long3 */"
  , "__attribute__ ((const))"
  , "unsigned long (*hs_bindgen_f8f162e0617e3fee (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_long3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_float */"
  , "__attribute__ ((const))"
  , "float (*hs_bindgen_ae10c7a77dee592c (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_float;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_double */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_1c45beb3e49c876d (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_double;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_bool1 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_36e51b8d16c968e9 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_bool1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_struct */"
  , "__attribute__ ((const))"
  , "struct some_struct (*hs_bindgen_3bfa07891ab1945e (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_struct;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_union */"
  , "__attribute__ ((const))"
  , "union some_union (*hs_bindgen_69860b33562cccb5 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_union;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_enum */"
  , "__attribute__ ((const))"
  , "enum some_enum (*hs_bindgen_413ae65c34d5ab3f (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_enum;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_pointer1 */"
  , "__attribute__ ((const))"
  , "signed int *(*hs_bindgen_d537cf46a47d0485 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_pointer1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_pointer2 */"
  , "__attribute__ ((const))"
  , "signed int **(*hs_bindgen_739306590182d687 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_pointer2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_pointer3 */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_0fa4f0add280f3ad (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_pointer3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_body1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a07bbbe76d7939c7 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &body1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_body2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a60bda1492798fe3 (void)) (void)"
  , "{"
  , "  return &body2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_complex_float */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a77516d301de443d (void)) ("
  , "  signed int arg1,"
  , "  float _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_float;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_args_complex_double */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_24010de2a0ed6183 (void)) ("
  , "  signed int arg1,"
  , "  double _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_double;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_complex_float */"
  , "__attribute__ ((const))"
  , "float _Complex (*hs_bindgen_c0cbf7be8aac1fac (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_complex_float;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_ret_complex_double */"
  , "__attribute__ ((const))"
  , "double _Complex (*hs_bindgen_f14fbe0462b054b4 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_complex_double;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_bespoke_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_dae3a770e76adc8d (void)) ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &bespoke_args1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_bespoke_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ef4bad5843365e9a (void)) ("
  , "  signed int arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &bespoke_args2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_bespoke_ret1 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_4cb0a28ec3761382 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_bespoke_ret2 */"
  , "__attribute__ ((const))"
  , "size_t (*hs_bindgen_73930100c9b16bd9 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_arr_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2da9a2832e690271 (void)) ("
  , "  signed int arg1[]"
  , ")"
  , "{"
  , "  return &arr_args1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_arr_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0f0959bc7ce62917 (void)) ("
  , "  signed int *arg1[]"
  , ")"
  , "{"
  , "  return &arr_args2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_arr_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a3b313aa9fc597e1 (void)) ("
  , "  signed int arg1[5]"
  , ")"
  , "{"
  , "  return &arr_args3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_arr_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9f387bde7802ae1f (void)) ("
  , "  signed int *arg1[5]"
  , ")"
  , "{"
  , "  return &arr_args4;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_funptr_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_124fe40f5bbe1379 (void)) ("
  , "  signed int arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_funptr_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_021c4bc8200b6125 (void)) ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_funptr_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_59aa20565b1e992d (void)) ("
  , "  signed int arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_funptr_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_427ce337265ab6aa (void)) ("
  , "  signed int arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args4;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_funptr_args5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a2ccf8317cb85bbc (void)) ("
  , "  signed int arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args5;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_comments1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_78cdbd93ee108018 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &comments1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_prim_before1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0a00696d53bf51d7 (void)) ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_prim_before2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_847d8de1fdef0d63 (void)) ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_prim_before3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ab9369683a44025a (void)) ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_prim_after1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4c8cf0f3c66496c3 (void)) ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_prim_after2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a1b28d990a0d0723 (void)) ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_prim_after3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7703cb78ab5310dd (void)) ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_withoutSign_before1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a57679d81a13ecab (void)) ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_withoutSign_before2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_36aaf094d002e32b (void)) ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_withoutSign_before3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_65464c79676bc4bd (void)) ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_withoutSign_before4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_817575fe36932178 (void)) ("
  , "  signed int arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before4;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_withoutSign_before5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_32033004afbaef3d (void)) ("
  , "  signed int arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before5;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_withoutSign_before6 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d39d6ad1915299fd (void)) ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before6;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_withoutSign_before7 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e1649462c2586bbc (void)) ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before7;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_withoutSign_before8 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c5a3e2274972de41 (void)) ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before8;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_withoutSign_after1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4654455003e81f77 (void)) ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_withoutSign_after2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0b3bd8d24696293a (void)) ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_withoutSign_after3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_220ab20973f59892 (void)) ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_withoutSign_after4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b993563da63c65c7 (void)) ("
  , "  signed int arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after4;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_withoutSign_after5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_34d4b754627ba4f7 (void)) ("
  , "  signed int arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after5;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_withoutSign_after6 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_32f4457388158d62 (void)) ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after6;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_withoutSign_after7 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_20d8ab69c805160d (void)) ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after7;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_withoutSign_after8 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c3abec34d30decc7 (void)) ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after8;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_pointers_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f0e9db228d72ad57 (void)) ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_pointers_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b40b6075be3b49ac (void)) ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_pointers_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_464404413a369761 (void)) ("
  , "  signed int arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_pointers_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9818a53206f7c636 (void)) ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args4;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_pointers_args5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_453e4eff0f5200e8 (void)) ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args5;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_pointers_ret1 */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_ed6fad795fa8294d (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_pointers_ret2 */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_52d4a25d38622fdb (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_pointers_ret3 */"
  , "__attribute__ ((const))"
  , "signed int *const (*hs_bindgen_e65f7f4766a4354f (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_pointers_ret4 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_e0583a9affc1dc51 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret4;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_pointers_ret5 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_e3c18734433481cb (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret5;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_array_elem1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b6d3b4f3be6373ff (void)) ("
  , "  signed int const arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_array_elem2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8bff41ae0d6eaeaa (void)) ("
  , "  signed int const *arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_const_array_elem3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_11f570604977bc68 (void)) ("
  , "  signed int *const arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_noParams1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_14fa7f8f15d56c6d (void)) (void)"
  , "{"
  , "  return &noParams1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_noParams2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_422decc683401449 (void)) (void)"
  , "{"
  , "  return &noParams2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_noParams3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e8c4551a87246d1c (void)) ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &noParams3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_funptr_ret1 */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_44f4d68423e3e514 (void)) ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret1;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_funptr_ret2 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_9f1c61d989649198 (void)) ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret2;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_funptr_ret3 */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_10d924f22b46c231 (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &funptr_ret3;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_funptr_ret4 */"
  , "__attribute__ ((const))"
  , "char (*(*hs_bindgen_17cfc42831d87b39 (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret4;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_funptr_ret5 */"
  , "__attribute__ ((const))"
  , "signed int *(*(*hs_bindgen_78244ef58fe5d49a (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret5;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_funptr_ret6 */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_a16e9de4b496b79d (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret6;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_funptr_ret7 */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_1145e079d0e78d69 (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret7;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_funptr_ret8 */"
  , "__attribute__ ((const))"
  , "signed int *const (*(*hs_bindgen_962f6126bcb41c35 (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret8;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_funptr_ret9 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_1e58dd0c6272d23c (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret9;"
  , "}"
  , "/* test_macrosreparse_1_empty\NULExample\NULget_funptr_ret10 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_72310ec4ae07cff9 (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret10;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_1_empty Example get_args_char1@
foreign import ccall unsafe "hs_bindgen_2bfe3e3f7e57b3ed" hs_bindgen_2bfe3e3f7e57b3ed_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_char1@
hs_bindgen_2bfe3e3f7e57b3ed :: IO (BG.FunPtr (BG.CInt -> BG.CChar -> IO ()))
hs_bindgen_2bfe3e3f7e57b3ed =
  fmap BG.fromFFIType hs_bindgen_2bfe3e3f7e57b3ed_base

{-# NOINLINE args_char1 #-}
{-| Function declarations

    __C declaration:__ @args_char1@

    __defined at:__ @macros\/reparse.h 17:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char1 :: BG.FunPtr (BG.CInt -> BG.CChar -> IO ())
args_char1 =
  BG.unsafePerformIO hs_bindgen_2bfe3e3f7e57b3ed

-- __unique:__ @test_macrosreparse_1_empty Example get_args_char2@
foreign import ccall unsafe "hs_bindgen_823cf196a07a68cc" hs_bindgen_823cf196a07a68cc_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_char2@
hs_bindgen_823cf196a07a68cc :: IO (BG.FunPtr (BG.CInt -> BG.CSChar -> IO ()))
hs_bindgen_823cf196a07a68cc =
  fmap BG.fromFFIType hs_bindgen_823cf196a07a68cc_base

{-# NOINLINE args_char2 #-}
{-| __C declaration:__ @args_char2@

    __defined at:__ @macros\/reparse.h 18:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char2 :: BG.FunPtr (BG.CInt -> BG.CSChar -> IO ())
args_char2 =
  BG.unsafePerformIO hs_bindgen_823cf196a07a68cc

-- __unique:__ @test_macrosreparse_1_empty Example get_args_char3@
foreign import ccall unsafe "hs_bindgen_c42bec91132d7124" hs_bindgen_c42bec91132d7124_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_char3@
hs_bindgen_c42bec91132d7124 :: IO (BG.FunPtr (BG.CInt -> BG.CUChar -> IO ()))
hs_bindgen_c42bec91132d7124 =
  fmap BG.fromFFIType hs_bindgen_c42bec91132d7124_base

{-# NOINLINE args_char3 #-}
{-| __C declaration:__ @args_char3@

    __defined at:__ @macros\/reparse.h 19:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char3 :: BG.FunPtr (BG.CInt -> BG.CUChar -> IO ())
args_char3 =
  BG.unsafePerformIO hs_bindgen_c42bec91132d7124

-- __unique:__ @test_macrosreparse_1_empty Example get_args_short1@
foreign import ccall unsafe "hs_bindgen_afad3baea1431062" hs_bindgen_afad3baea1431062_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_short1@
hs_bindgen_afad3baea1431062 :: IO (BG.FunPtr (BG.CInt -> BG.CShort -> IO ()))
hs_bindgen_afad3baea1431062 =
  fmap BG.fromFFIType hs_bindgen_afad3baea1431062_base

{-# NOINLINE args_short1 #-}
{-| __C declaration:__ @args_short1@

    __defined at:__ @macros\/reparse.h 21:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short1 :: BG.FunPtr (BG.CInt -> BG.CShort -> IO ())
args_short1 =
  BG.unsafePerformIO hs_bindgen_afad3baea1431062

-- __unique:__ @test_macrosreparse_1_empty Example get_args_short2@
foreign import ccall unsafe "hs_bindgen_c3734c97c5e64c56" hs_bindgen_c3734c97c5e64c56_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_short2@
hs_bindgen_c3734c97c5e64c56 :: IO (BG.FunPtr (BG.CInt -> BG.CShort -> IO ()))
hs_bindgen_c3734c97c5e64c56 =
  fmap BG.fromFFIType hs_bindgen_c3734c97c5e64c56_base

{-# NOINLINE args_short2 #-}
{-| __C declaration:__ @args_short2@

    __defined at:__ @macros\/reparse.h 22:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short2 :: BG.FunPtr (BG.CInt -> BG.CShort -> IO ())
args_short2 =
  BG.unsafePerformIO hs_bindgen_c3734c97c5e64c56

-- __unique:__ @test_macrosreparse_1_empty Example get_args_short3@
foreign import ccall unsafe "hs_bindgen_d0673a1033816c0b" hs_bindgen_d0673a1033816c0b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_short3@
hs_bindgen_d0673a1033816c0b :: IO (BG.FunPtr (BG.CInt -> BG.CUShort -> IO ()))
hs_bindgen_d0673a1033816c0b =
  fmap BG.fromFFIType hs_bindgen_d0673a1033816c0b_base

{-# NOINLINE args_short3 #-}
{-| __C declaration:__ @args_short3@

    __defined at:__ @macros\/reparse.h 23:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short3 :: BG.FunPtr (BG.CInt -> BG.CUShort -> IO ())
args_short3 =
  BG.unsafePerformIO hs_bindgen_d0673a1033816c0b

-- __unique:__ @test_macrosreparse_1_empty Example get_args_int1@
foreign import ccall unsafe "hs_bindgen_88f1182383c6432a" hs_bindgen_88f1182383c6432a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_int1@
hs_bindgen_88f1182383c6432a :: IO (BG.FunPtr (BG.CInt -> BG.CInt -> IO ()))
hs_bindgen_88f1182383c6432a =
  fmap BG.fromFFIType hs_bindgen_88f1182383c6432a_base

{-# NOINLINE args_int1 #-}
{-| __C declaration:__ @args_int1@

    __defined at:__ @macros\/reparse.h 25:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int1 :: BG.FunPtr (BG.CInt -> BG.CInt -> IO ())
args_int1 =
  BG.unsafePerformIO hs_bindgen_88f1182383c6432a

-- __unique:__ @test_macrosreparse_1_empty Example get_args_int2@
foreign import ccall unsafe "hs_bindgen_bc197f9358cd11b4" hs_bindgen_bc197f9358cd11b4_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_int2@
hs_bindgen_bc197f9358cd11b4 :: IO (BG.FunPtr (BG.CInt -> BG.CInt -> IO ()))
hs_bindgen_bc197f9358cd11b4 =
  fmap BG.fromFFIType hs_bindgen_bc197f9358cd11b4_base

{-# NOINLINE args_int2 #-}
{-| __C declaration:__ @args_int2@

    __defined at:__ @macros\/reparse.h 26:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int2 :: BG.FunPtr (BG.CInt -> BG.CInt -> IO ())
args_int2 =
  BG.unsafePerformIO hs_bindgen_bc197f9358cd11b4

-- __unique:__ @test_macrosreparse_1_empty Example get_args_int3@
foreign import ccall unsafe "hs_bindgen_b84219e461ae5f25" hs_bindgen_b84219e461ae5f25_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_int3@
hs_bindgen_b84219e461ae5f25 :: IO (BG.FunPtr (BG.CInt -> BG.CUInt -> IO ()))
hs_bindgen_b84219e461ae5f25 =
  fmap BG.fromFFIType hs_bindgen_b84219e461ae5f25_base

{-# NOINLINE args_int3 #-}
{-| __C declaration:__ @args_int3@

    __defined at:__ @macros\/reparse.h 27:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int3 :: BG.FunPtr (BG.CInt -> BG.CUInt -> IO ())
args_int3 =
  BG.unsafePerformIO hs_bindgen_b84219e461ae5f25

-- __unique:__ @test_macrosreparse_1_empty Example get_args_long1@
foreign import ccall unsafe "hs_bindgen_68442e1aadc5a0d3" hs_bindgen_68442e1aadc5a0d3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_long1@
hs_bindgen_68442e1aadc5a0d3 :: IO (BG.FunPtr (BG.CInt -> BG.CLong -> IO ()))
hs_bindgen_68442e1aadc5a0d3 =
  fmap BG.fromFFIType hs_bindgen_68442e1aadc5a0d3_base

{-# NOINLINE args_long1 #-}
{-| __C declaration:__ @args_long1@

    __defined at:__ @macros\/reparse.h 29:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long1 :: BG.FunPtr (BG.CInt -> BG.CLong -> IO ())
args_long1 =
  BG.unsafePerformIO hs_bindgen_68442e1aadc5a0d3

-- __unique:__ @test_macrosreparse_1_empty Example get_args_long2@
foreign import ccall unsafe "hs_bindgen_5e8e40d28c06a172" hs_bindgen_5e8e40d28c06a172_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_long2@
hs_bindgen_5e8e40d28c06a172 :: IO (BG.FunPtr (BG.CInt -> BG.CLong -> IO ()))
hs_bindgen_5e8e40d28c06a172 =
  fmap BG.fromFFIType hs_bindgen_5e8e40d28c06a172_base

{-# NOINLINE args_long2 #-}
{-| __C declaration:__ @args_long2@

    __defined at:__ @macros\/reparse.h 30:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long2 :: BG.FunPtr (BG.CInt -> BG.CLong -> IO ())
args_long2 =
  BG.unsafePerformIO hs_bindgen_5e8e40d28c06a172

-- __unique:__ @test_macrosreparse_1_empty Example get_args_long3@
foreign import ccall unsafe "hs_bindgen_d3db689a6d7b27e0" hs_bindgen_d3db689a6d7b27e0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_long3@
hs_bindgen_d3db689a6d7b27e0 :: IO (BG.FunPtr (BG.CInt -> BG.CULong -> IO ()))
hs_bindgen_d3db689a6d7b27e0 =
  fmap BG.fromFFIType hs_bindgen_d3db689a6d7b27e0_base

{-# NOINLINE args_long3 #-}
{-| __C declaration:__ @args_long3@

    __defined at:__ @macros\/reparse.h 31:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long3 :: BG.FunPtr (BG.CInt -> BG.CULong -> IO ())
args_long3 =
  BG.unsafePerformIO hs_bindgen_d3db689a6d7b27e0

-- __unique:__ @test_macrosreparse_1_empty Example get_args_float@
foreign import ccall unsafe "hs_bindgen_5d87ce6909d695da" hs_bindgen_5d87ce6909d695da_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_float@
hs_bindgen_5d87ce6909d695da :: IO (BG.FunPtr (BG.CInt -> BG.CFloat -> IO ()))
hs_bindgen_5d87ce6909d695da =
  fmap BG.fromFFIType hs_bindgen_5d87ce6909d695da_base

{-# NOINLINE args_float #-}
{-| __C declaration:__ @args_float@

    __defined at:__ @macros\/reparse.h 33:6@

    __exported by:__ @macros\/reparse.h@
-}
args_float :: BG.FunPtr (BG.CInt -> BG.CFloat -> IO ())
args_float =
  BG.unsafePerformIO hs_bindgen_5d87ce6909d695da

-- __unique:__ @test_macrosreparse_1_empty Example get_args_double@
foreign import ccall unsafe "hs_bindgen_6ee90e5314b16cf4" hs_bindgen_6ee90e5314b16cf4_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_double@
hs_bindgen_6ee90e5314b16cf4 :: IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO ()))
hs_bindgen_6ee90e5314b16cf4 =
  fmap BG.fromFFIType hs_bindgen_6ee90e5314b16cf4_base

{-# NOINLINE args_double #-}
{-| __C declaration:__ @args_double@

    __defined at:__ @macros\/reparse.h 34:6@

    __exported by:__ @macros\/reparse.h@
-}
args_double :: BG.FunPtr (BG.CInt -> BG.CDouble -> IO ())
args_double =
  BG.unsafePerformIO hs_bindgen_6ee90e5314b16cf4

-- __unique:__ @test_macrosreparse_1_empty Example get_args_bool1@
foreign import ccall unsafe "hs_bindgen_d3783c9e52e30d08" hs_bindgen_d3783c9e52e30d08_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_bool1@
hs_bindgen_d3783c9e52e30d08 :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_d3783c9e52e30d08 =
  fmap BG.fromFFIType hs_bindgen_d3783c9e52e30d08_base

{-# NOINLINE args_bool1 #-}
{-| __C declaration:__ @args_bool1@

    __defined at:__ @macros\/reparse.h 35:6@

    __exported by:__ @macros\/reparse.h@
-}
args_bool1 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
args_bool1 =
  BG.unsafePerformIO hs_bindgen_d3783c9e52e30d08

-- __unique:__ @test_macrosreparse_1_empty Example get_args_struct@
foreign import ccall unsafe "hs_bindgen_2ac83bf702102758" hs_bindgen_2ac83bf702102758_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_struct@
hs_bindgen_2ac83bf702102758 :: IO (BG.FunPtr (BG.CInt -> Some_struct -> IO ()))
hs_bindgen_2ac83bf702102758 =
  fmap BG.fromFFIType hs_bindgen_2ac83bf702102758_base

{-# NOINLINE args_struct #-}
{-| __C declaration:__ @args_struct@

    __defined at:__ @macros\/reparse.h 37:6@

    __exported by:__ @macros\/reparse.h@
-}
args_struct :: BG.FunPtr (BG.CInt -> Some_struct -> IO ())
args_struct =
  BG.unsafePerformIO hs_bindgen_2ac83bf702102758

-- __unique:__ @test_macrosreparse_1_empty Example get_args_union@
foreign import ccall unsafe "hs_bindgen_700dff64ce96b9c7" hs_bindgen_700dff64ce96b9c7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_union@
hs_bindgen_700dff64ce96b9c7 :: IO (BG.FunPtr (BG.CInt -> Some_union -> IO ()))
hs_bindgen_700dff64ce96b9c7 =
  fmap BG.fromFFIType hs_bindgen_700dff64ce96b9c7_base

{-# NOINLINE args_union #-}
{-| __C declaration:__ @args_union@

    __defined at:__ @macros\/reparse.h 38:6@

    __exported by:__ @macros\/reparse.h@
-}
args_union :: BG.FunPtr (BG.CInt -> Some_union -> IO ())
args_union =
  BG.unsafePerformIO hs_bindgen_700dff64ce96b9c7

-- __unique:__ @test_macrosreparse_1_empty Example get_args_enum@
foreign import ccall unsafe "hs_bindgen_2263a2832efce5d2" hs_bindgen_2263a2832efce5d2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_enum@
hs_bindgen_2263a2832efce5d2 :: IO (BG.FunPtr (BG.CInt -> Some_enum -> IO ()))
hs_bindgen_2263a2832efce5d2 =
  fmap BG.fromFFIType hs_bindgen_2263a2832efce5d2_base

{-# NOINLINE args_enum #-}
{-| __C declaration:__ @args_enum@

    __defined at:__ @macros\/reparse.h 39:6@

    __exported by:__ @macros\/reparse.h@
-}
args_enum :: BG.FunPtr (BG.CInt -> Some_enum -> IO ())
args_enum =
  BG.unsafePerformIO hs_bindgen_2263a2832efce5d2

-- __unique:__ @test_macrosreparse_1_empty Example get_args_pointer1@
foreign import ccall unsafe "hs_bindgen_337624a9df6f893f" hs_bindgen_337624a9df6f893f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_pointer1@
hs_bindgen_337624a9df6f893f :: IO (BG.FunPtr (BG.CInt -> BG.Ptr BG.CInt -> IO ()))
hs_bindgen_337624a9df6f893f =
  fmap BG.fromFFIType hs_bindgen_337624a9df6f893f_base

{-# NOINLINE args_pointer1 #-}
{-| __C declaration:__ @args_pointer1@

    __defined at:__ @macros\/reparse.h 41:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer1 :: BG.FunPtr (BG.CInt -> BG.Ptr BG.CInt -> IO ())
args_pointer1 =
  BG.unsafePerformIO hs_bindgen_337624a9df6f893f

-- __unique:__ @test_macrosreparse_1_empty Example get_args_pointer2@
foreign import ccall unsafe "hs_bindgen_e86c13d7a42aaecb" hs_bindgen_e86c13d7a42aaecb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_pointer2@
hs_bindgen_e86c13d7a42aaecb :: IO (BG.FunPtr (BG.CInt -> BG.Ptr (BG.Ptr BG.CInt) -> IO ()))
hs_bindgen_e86c13d7a42aaecb =
  fmap BG.fromFFIType hs_bindgen_e86c13d7a42aaecb_base

{-# NOINLINE args_pointer2 #-}
{-| __C declaration:__ @args_pointer2@

    __defined at:__ @macros\/reparse.h 42:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer2 :: BG.FunPtr (BG.CInt -> BG.Ptr (BG.Ptr BG.CInt) -> IO ())
args_pointer2 =
  BG.unsafePerformIO hs_bindgen_e86c13d7a42aaecb

-- __unique:__ @test_macrosreparse_1_empty Example get_args_pointer3@
foreign import ccall unsafe "hs_bindgen_4f49ea6cc2d47cb9" hs_bindgen_4f49ea6cc2d47cb9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_pointer3@
hs_bindgen_4f49ea6cc2d47cb9 :: IO (BG.FunPtr (BG.CInt -> BG.Ptr BG.Void -> IO ()))
hs_bindgen_4f49ea6cc2d47cb9 =
  fmap BG.fromFFIType hs_bindgen_4f49ea6cc2d47cb9_base

{-# NOINLINE args_pointer3 #-}
{-| __C declaration:__ @args_pointer3@

    __defined at:__ @macros\/reparse.h 43:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer3 :: BG.FunPtr (BG.CInt -> BG.Ptr BG.Void -> IO ())
args_pointer3 =
  BG.unsafePerformIO hs_bindgen_4f49ea6cc2d47cb9

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_A@
foreign import ccall unsafe "hs_bindgen_4eabc39d58d3eea7" hs_bindgen_4eabc39d58d3eea7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_A@
hs_bindgen_4eabc39d58d3eea7 :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_4eabc39d58d3eea7 =
  fmap BG.fromFFIType hs_bindgen_4eabc39d58d3eea7_base

{-# NOINLINE ret_A #-}
{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h 47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A :: BG.FunPtr (IO BG.CInt)
ret_A =
  BG.unsafePerformIO hs_bindgen_4eabc39d58d3eea7

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_char1@
foreign import ccall unsafe "hs_bindgen_0e9cabb6650a5827" hs_bindgen_0e9cabb6650a5827_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_char1@
hs_bindgen_0e9cabb6650a5827 :: IO (BG.FunPtr (BG.CInt -> IO BG.CChar))
hs_bindgen_0e9cabb6650a5827 =
  fmap BG.fromFFIType hs_bindgen_0e9cabb6650a5827_base

{-# NOINLINE ret_char1 #-}
{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h 49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1 :: BG.FunPtr (BG.CInt -> IO BG.CChar)
ret_char1 =
  BG.unsafePerformIO hs_bindgen_0e9cabb6650a5827

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_char2@
foreign import ccall unsafe "hs_bindgen_38e274f6e0726048" hs_bindgen_38e274f6e0726048_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_char2@
hs_bindgen_38e274f6e0726048 :: IO (BG.FunPtr (BG.CInt -> IO BG.CSChar))
hs_bindgen_38e274f6e0726048 =
  fmap BG.fromFFIType hs_bindgen_38e274f6e0726048_base

{-# NOINLINE ret_char2 #-}
{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h 50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2 :: BG.FunPtr (BG.CInt -> IO BG.CSChar)
ret_char2 =
  BG.unsafePerformIO hs_bindgen_38e274f6e0726048

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_char3@
foreign import ccall unsafe "hs_bindgen_621d5d07f2e26bbd" hs_bindgen_621d5d07f2e26bbd_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_char3@
hs_bindgen_621d5d07f2e26bbd :: IO (BG.FunPtr (BG.CInt -> IO BG.CUChar))
hs_bindgen_621d5d07f2e26bbd =
  fmap BG.fromFFIType hs_bindgen_621d5d07f2e26bbd_base

{-# NOINLINE ret_char3 #-}
{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h 51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3 :: BG.FunPtr (BG.CInt -> IO BG.CUChar)
ret_char3 =
  BG.unsafePerformIO hs_bindgen_621d5d07f2e26bbd

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_short1@
foreign import ccall unsafe "hs_bindgen_f6b29f7ddb032607" hs_bindgen_f6b29f7ddb032607_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_short1@
hs_bindgen_f6b29f7ddb032607 :: IO (BG.FunPtr (BG.CInt -> IO BG.CShort))
hs_bindgen_f6b29f7ddb032607 =
  fmap BG.fromFFIType hs_bindgen_f6b29f7ddb032607_base

{-# NOINLINE ret_short1 #-}
{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h 53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1 :: BG.FunPtr (BG.CInt -> IO BG.CShort)
ret_short1 =
  BG.unsafePerformIO hs_bindgen_f6b29f7ddb032607

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_short2@
foreign import ccall unsafe "hs_bindgen_65df37dcb1977468" hs_bindgen_65df37dcb1977468_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_short2@
hs_bindgen_65df37dcb1977468 :: IO (BG.FunPtr (BG.CInt -> IO BG.CShort))
hs_bindgen_65df37dcb1977468 =
  fmap BG.fromFFIType hs_bindgen_65df37dcb1977468_base

{-# NOINLINE ret_short2 #-}
{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h 54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2 :: BG.FunPtr (BG.CInt -> IO BG.CShort)
ret_short2 =
  BG.unsafePerformIO hs_bindgen_65df37dcb1977468

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_short3@
foreign import ccall unsafe "hs_bindgen_c2074f909866d753" hs_bindgen_c2074f909866d753_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_short3@
hs_bindgen_c2074f909866d753 :: IO (BG.FunPtr (BG.CInt -> IO BG.CUShort))
hs_bindgen_c2074f909866d753 =
  fmap BG.fromFFIType hs_bindgen_c2074f909866d753_base

{-# NOINLINE ret_short3 #-}
{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h 55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3 :: BG.FunPtr (BG.CInt -> IO BG.CUShort)
ret_short3 =
  BG.unsafePerformIO hs_bindgen_c2074f909866d753

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_int1@
foreign import ccall unsafe "hs_bindgen_9e5aed81836cdc4e" hs_bindgen_9e5aed81836cdc4e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_int1@
hs_bindgen_9e5aed81836cdc4e :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_9e5aed81836cdc4e =
  fmap BG.fromFFIType hs_bindgen_9e5aed81836cdc4e_base

{-# NOINLINE ret_int1 #-}
{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h 57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1 :: BG.FunPtr (BG.CInt -> IO BG.CInt)
ret_int1 =
  BG.unsafePerformIO hs_bindgen_9e5aed81836cdc4e

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_int2@
foreign import ccall unsafe "hs_bindgen_a466e4b333684935" hs_bindgen_a466e4b333684935_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_int2@
hs_bindgen_a466e4b333684935 :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_a466e4b333684935 =
  fmap BG.fromFFIType hs_bindgen_a466e4b333684935_base

{-# NOINLINE ret_int2 #-}
{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h 58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2 :: BG.FunPtr (BG.CInt -> IO BG.CInt)
ret_int2 =
  BG.unsafePerformIO hs_bindgen_a466e4b333684935

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_int3@
foreign import ccall unsafe "hs_bindgen_67adc838c559816b" hs_bindgen_67adc838c559816b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_int3@
hs_bindgen_67adc838c559816b :: IO (BG.FunPtr (BG.CInt -> IO BG.CUInt))
hs_bindgen_67adc838c559816b =
  fmap BG.fromFFIType hs_bindgen_67adc838c559816b_base

{-# NOINLINE ret_int3 #-}
{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h 59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3 :: BG.FunPtr (BG.CInt -> IO BG.CUInt)
ret_int3 =
  BG.unsafePerformIO hs_bindgen_67adc838c559816b

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_long1@
foreign import ccall unsafe "hs_bindgen_3795354110c96def" hs_bindgen_3795354110c96def_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_long1@
hs_bindgen_3795354110c96def :: IO (BG.FunPtr (BG.CInt -> IO BG.CLong))
hs_bindgen_3795354110c96def =
  fmap BG.fromFFIType hs_bindgen_3795354110c96def_base

{-# NOINLINE ret_long1 #-}
{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h 61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1 :: BG.FunPtr (BG.CInt -> IO BG.CLong)
ret_long1 =
  BG.unsafePerformIO hs_bindgen_3795354110c96def

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_long2@
foreign import ccall unsafe "hs_bindgen_f5d22573fdc7a51c" hs_bindgen_f5d22573fdc7a51c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_long2@
hs_bindgen_f5d22573fdc7a51c :: IO (BG.FunPtr (BG.CInt -> IO BG.CLong))
hs_bindgen_f5d22573fdc7a51c =
  fmap BG.fromFFIType hs_bindgen_f5d22573fdc7a51c_base

{-# NOINLINE ret_long2 #-}
{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h 62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2 :: BG.FunPtr (BG.CInt -> IO BG.CLong)
ret_long2 =
  BG.unsafePerformIO hs_bindgen_f5d22573fdc7a51c

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_long3@
foreign import ccall unsafe "hs_bindgen_f8f162e0617e3fee" hs_bindgen_f8f162e0617e3fee_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_long3@
hs_bindgen_f8f162e0617e3fee :: IO (BG.FunPtr (BG.CInt -> IO BG.CULong))
hs_bindgen_f8f162e0617e3fee =
  fmap BG.fromFFIType hs_bindgen_f8f162e0617e3fee_base

{-# NOINLINE ret_long3 #-}
{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h 63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3 :: BG.FunPtr (BG.CInt -> IO BG.CULong)
ret_long3 =
  BG.unsafePerformIO hs_bindgen_f8f162e0617e3fee

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_float@
foreign import ccall unsafe "hs_bindgen_ae10c7a77dee592c" hs_bindgen_ae10c7a77dee592c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_float@
hs_bindgen_ae10c7a77dee592c :: IO (BG.FunPtr (BG.CInt -> IO BG.CFloat))
hs_bindgen_ae10c7a77dee592c =
  fmap BG.fromFFIType hs_bindgen_ae10c7a77dee592c_base

{-# NOINLINE ret_float #-}
{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h 65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float :: BG.FunPtr (BG.CInt -> IO BG.CFloat)
ret_float =
  BG.unsafePerformIO hs_bindgen_ae10c7a77dee592c

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_double@
foreign import ccall unsafe "hs_bindgen_1c45beb3e49c876d" hs_bindgen_1c45beb3e49c876d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_double@
hs_bindgen_1c45beb3e49c876d :: IO (BG.FunPtr (BG.CInt -> IO BG.CDouble))
hs_bindgen_1c45beb3e49c876d =
  fmap BG.fromFFIType hs_bindgen_1c45beb3e49c876d_base

{-# NOINLINE ret_double #-}
{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h 66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double :: BG.FunPtr (BG.CInt -> IO BG.CDouble)
ret_double =
  BG.unsafePerformIO hs_bindgen_1c45beb3e49c876d

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_bool1@
foreign import ccall unsafe "hs_bindgen_36e51b8d16c968e9" hs_bindgen_36e51b8d16c968e9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_bool1@
hs_bindgen_36e51b8d16c968e9 :: IO (BG.FunPtr (BG.CInt -> IO BG.CBool))
hs_bindgen_36e51b8d16c968e9 =
  fmap BG.fromFFIType hs_bindgen_36e51b8d16c968e9_base

{-# NOINLINE ret_bool1 #-}
{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h 67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1 :: BG.FunPtr (BG.CInt -> IO BG.CBool)
ret_bool1 =
  BG.unsafePerformIO hs_bindgen_36e51b8d16c968e9

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_struct@
foreign import ccall unsafe "hs_bindgen_3bfa07891ab1945e" hs_bindgen_3bfa07891ab1945e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_struct@
hs_bindgen_3bfa07891ab1945e :: IO (BG.FunPtr (BG.CInt -> IO Some_struct))
hs_bindgen_3bfa07891ab1945e =
  fmap BG.fromFFIType hs_bindgen_3bfa07891ab1945e_base

{-# NOINLINE ret_struct #-}
{-| __C declaration:__ @ret_struct@

    __defined at:__ @macros\/reparse.h 69:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_struct :: BG.FunPtr (BG.CInt -> IO Some_struct)
ret_struct =
  BG.unsafePerformIO hs_bindgen_3bfa07891ab1945e

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_union@
foreign import ccall unsafe "hs_bindgen_69860b33562cccb5" hs_bindgen_69860b33562cccb5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_union@
hs_bindgen_69860b33562cccb5 :: IO (BG.FunPtr (BG.CInt -> IO Some_union))
hs_bindgen_69860b33562cccb5 =
  fmap BG.fromFFIType hs_bindgen_69860b33562cccb5_base

{-# NOINLINE ret_union #-}
{-| __C declaration:__ @ret_union@

    __defined at:__ @macros\/reparse.h 70:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_union :: BG.FunPtr (BG.CInt -> IO Some_union)
ret_union =
  BG.unsafePerformIO hs_bindgen_69860b33562cccb5

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_enum@
foreign import ccall unsafe "hs_bindgen_413ae65c34d5ab3f" hs_bindgen_413ae65c34d5ab3f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_enum@
hs_bindgen_413ae65c34d5ab3f :: IO (BG.FunPtr (BG.CInt -> IO Some_enum))
hs_bindgen_413ae65c34d5ab3f =
  fmap BG.fromFFIType hs_bindgen_413ae65c34d5ab3f_base

{-# NOINLINE ret_enum #-}
{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h 71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum :: BG.FunPtr (BG.CInt -> IO Some_enum)
ret_enum =
  BG.unsafePerformIO hs_bindgen_413ae65c34d5ab3f

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_pointer1@
foreign import ccall unsafe "hs_bindgen_d537cf46a47d0485" hs_bindgen_d537cf46a47d0485_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_pointer1@
hs_bindgen_d537cf46a47d0485 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt)))
hs_bindgen_d537cf46a47d0485 =
  fmap BG.fromFFIType hs_bindgen_d537cf46a47d0485_base

{-# NOINLINE ret_pointer1 #-}
{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h 73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt))
ret_pointer1 =
  BG.unsafePerformIO hs_bindgen_d537cf46a47d0485

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_pointer2@
foreign import ccall unsafe "hs_bindgen_739306590182d687" hs_bindgen_739306590182d687_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_pointer2@
hs_bindgen_739306590182d687 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr BG.CInt))))
hs_bindgen_739306590182d687 =
  fmap BG.fromFFIType hs_bindgen_739306590182d687_base

{-# NOINLINE ret_pointer2 #-}
{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h 74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr BG.CInt)))
ret_pointer2 =
  BG.unsafePerformIO hs_bindgen_739306590182d687

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_pointer3@
foreign import ccall unsafe "hs_bindgen_0fa4f0add280f3ad" hs_bindgen_0fa4f0add280f3ad_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_pointer3@
hs_bindgen_0fa4f0add280f3ad :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.Void)))
hs_bindgen_0fa4f0add280f3ad =
  fmap BG.fromFFIType hs_bindgen_0fa4f0add280f3ad_base

{-# NOINLINE ret_pointer3 #-}
{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h 75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.Void))
ret_pointer3 =
  BG.unsafePerformIO hs_bindgen_0fa4f0add280f3ad

-- __unique:__ @test_macrosreparse_1_empty Example get_body1@
foreign import ccall unsafe "hs_bindgen_a07bbbe76d7939c7" hs_bindgen_a07bbbe76d7939c7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_body1@
hs_bindgen_a07bbbe76d7939c7 :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_a07bbbe76d7939c7 =
  fmap BG.fromFFIType hs_bindgen_a07bbbe76d7939c7_base

{-# NOINLINE body1 #-}
{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h 79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1 :: BG.FunPtr (BG.CInt -> IO BG.CInt)
body1 =
  BG.unsafePerformIO hs_bindgen_a07bbbe76d7939c7

-- __unique:__ @test_macrosreparse_1_empty Example get_body2@
foreign import ccall unsafe "hs_bindgen_a60bda1492798fe3" hs_bindgen_a60bda1492798fe3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_body2@
hs_bindgen_a60bda1492798fe3 :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_a60bda1492798fe3 =
  fmap BG.fromFFIType hs_bindgen_a60bda1492798fe3_base

{-# NOINLINE body2 #-}
{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h 80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2 :: BG.FunPtr (IO BG.CInt)
body2 =
  BG.unsafePerformIO hs_bindgen_a60bda1492798fe3

-- __unique:__ @test_macrosreparse_1_empty Example get_args_complex_float@
foreign import ccall unsafe "hs_bindgen_a77516d301de443d" hs_bindgen_a77516d301de443d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_complex_float@
hs_bindgen_a77516d301de443d :: IO (BG.FunPtr (BG.CInt -> BG.Complex BG.CFloat -> IO ()))
hs_bindgen_a77516d301de443d =
  fmap BG.fromFFIType hs_bindgen_a77516d301de443d_base

{-# NOINLINE args_complex_float #-}
{-| __C declaration:__ @args_complex_float@

    __defined at:__ @macros\/reparse.h 84:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_float :: BG.FunPtr (BG.CInt -> BG.Complex BG.CFloat -> IO ())
args_complex_float =
  BG.unsafePerformIO hs_bindgen_a77516d301de443d

-- __unique:__ @test_macrosreparse_1_empty Example get_args_complex_double@
foreign import ccall unsafe "hs_bindgen_24010de2a0ed6183" hs_bindgen_24010de2a0ed6183_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_args_complex_double@
hs_bindgen_24010de2a0ed6183 :: IO (BG.FunPtr (BG.CInt -> BG.Complex BG.CDouble -> IO ()))
hs_bindgen_24010de2a0ed6183 =
  fmap BG.fromFFIType hs_bindgen_24010de2a0ed6183_base

{-# NOINLINE args_complex_double #-}
{-| __C declaration:__ @args_complex_double@

    __defined at:__ @macros\/reparse.h 85:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_double :: BG.FunPtr (BG.CInt -> BG.Complex BG.CDouble -> IO ())
args_complex_double =
  BG.unsafePerformIO hs_bindgen_24010de2a0ed6183

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_complex_float@
foreign import ccall unsafe "hs_bindgen_c0cbf7be8aac1fac" hs_bindgen_c0cbf7be8aac1fac_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_complex_float@
hs_bindgen_c0cbf7be8aac1fac :: IO (BG.FunPtr (BG.CInt -> IO (BG.Complex BG.CFloat)))
hs_bindgen_c0cbf7be8aac1fac =
  fmap BG.fromFFIType hs_bindgen_c0cbf7be8aac1fac_base

{-# NOINLINE ret_complex_float #-}
{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @macros\/reparse.h 86:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_float :: BG.FunPtr (BG.CInt -> IO (BG.Complex BG.CFloat))
ret_complex_float =
  BG.unsafePerformIO hs_bindgen_c0cbf7be8aac1fac

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_complex_double@
foreign import ccall unsafe "hs_bindgen_f14fbe0462b054b4" hs_bindgen_f14fbe0462b054b4_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_ret_complex_double@
hs_bindgen_f14fbe0462b054b4 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Complex BG.CDouble)))
hs_bindgen_f14fbe0462b054b4 =
  fmap BG.fromFFIType hs_bindgen_f14fbe0462b054b4_base

{-# NOINLINE ret_complex_double #-}
{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @macros\/reparse.h 87:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_double :: BG.FunPtr (BG.CInt -> IO (BG.Complex BG.CDouble))
ret_complex_double =
  BG.unsafePerformIO hs_bindgen_f14fbe0462b054b4

-- __unique:__ @test_macrosreparse_1_empty Example get_bespoke_args1@
foreign import ccall unsafe "hs_bindgen_dae3a770e76adc8d" hs_bindgen_dae3a770e76adc8d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_bespoke_args1@
hs_bindgen_dae3a770e76adc8d :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_dae3a770e76adc8d =
  fmap BG.fromFFIType hs_bindgen_dae3a770e76adc8d_base

{-# NOINLINE bespoke_args1 #-}
{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @macros\/reparse.h 94:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args1 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
bespoke_args1 =
  BG.unsafePerformIO hs_bindgen_dae3a770e76adc8d

-- __unique:__ @test_macrosreparse_1_empty Example get_bespoke_args2@
foreign import ccall unsafe "hs_bindgen_ef4bad5843365e9a" hs_bindgen_ef4bad5843365e9a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_bespoke_args2@
hs_bindgen_ef4bad5843365e9a :: IO (BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_ef4bad5843365e9a =
  fmap BG.fromFFIType hs_bindgen_ef4bad5843365e9a_base

{-# NOINLINE bespoke_args2 #-}
{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @macros\/reparse.h 95:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args2 :: BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ())
bespoke_args2 =
  BG.unsafePerformIO hs_bindgen_ef4bad5843365e9a

-- __unique:__ @test_macrosreparse_1_empty Example get_bespoke_ret1@
foreign import ccall unsafe "hs_bindgen_4cb0a28ec3761382" hs_bindgen_4cb0a28ec3761382_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_bespoke_ret1@
hs_bindgen_4cb0a28ec3761382 :: IO (BG.FunPtr (BG.CInt -> IO BG.CBool))
hs_bindgen_4cb0a28ec3761382 =
  fmap BG.fromFFIType hs_bindgen_4cb0a28ec3761382_base

{-# NOINLINE bespoke_ret1 #-}
{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h 97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1 :: BG.FunPtr (BG.CInt -> IO BG.CBool)
bespoke_ret1 =
  BG.unsafePerformIO hs_bindgen_4cb0a28ec3761382

-- __unique:__ @test_macrosreparse_1_empty Example get_bespoke_ret2@
foreign import ccall unsafe "hs_bindgen_73930100c9b16bd9" hs_bindgen_73930100c9b16bd9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_bespoke_ret2@
hs_bindgen_73930100c9b16bd9 :: IO (BG.FunPtr (BG.CInt -> IO HsBindgen.Runtime.LibC.CSize))
hs_bindgen_73930100c9b16bd9 =
  fmap BG.fromFFIType hs_bindgen_73930100c9b16bd9_base

{-# NOINLINE bespoke_ret2 #-}
{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h 98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2 :: BG.FunPtr (BG.CInt -> IO HsBindgen.Runtime.LibC.CSize)
bespoke_ret2 =
  BG.unsafePerformIO hs_bindgen_73930100c9b16bd9

-- __unique:__ @test_macrosreparse_1_empty Example get_arr_args1@
foreign import ccall unsafe "hs_bindgen_2da9a2832e690271" hs_bindgen_2da9a2832e690271_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_arr_args1@
hs_bindgen_2da9a2832e690271 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO ()))
hs_bindgen_2da9a2832e690271 =
  fmap BG.fromFFIType hs_bindgen_2da9a2832e690271_base

{-# NOINLINE arr_args1 #-}
{-| Arrays

    __C declaration:__ @arr_args1@

    __defined at:__ @macros\/reparse.h 104:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args1 :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO ())
arr_args1 =
  BG.unsafePerformIO hs_bindgen_2da9a2832e690271

-- __unique:__ @test_macrosreparse_1_empty Example get_arr_args2@
foreign import ccall unsafe "hs_bindgen_0f0959bc7ce62917" hs_bindgen_0f0959bc7ce62917_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_arr_args2@
hs_bindgen_0f0959bc7ce62917 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt))) -> IO ()))
hs_bindgen_0f0959bc7ce62917 =
  fmap BG.fromFFIType hs_bindgen_0f0959bc7ce62917_base

{-# NOINLINE arr_args2 #-}
{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h 105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2 :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt))) -> IO ())
arr_args2 =
  BG.unsafePerformIO hs_bindgen_0f0959bc7ce62917

-- __unique:__ @test_macrosreparse_1_empty Example get_arr_args3@
foreign import ccall unsafe "hs_bindgen_a3b313aa9fc597e1" hs_bindgen_a3b313aa9fc597e1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_arr_args3@
hs_bindgen_a3b313aa9fc597e1 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt)) -> IO ()))
hs_bindgen_a3b313aa9fc597e1 =
  fmap BG.fromFFIType hs_bindgen_a3b313aa9fc597e1_base

{-# NOINLINE arr_args3 #-}
{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h 106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3 :: BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt)) -> IO ())
arr_args3 =
  BG.unsafePerformIO hs_bindgen_a3b313aa9fc597e1

-- __unique:__ @test_macrosreparse_1_empty Example get_arr_args4@
foreign import ccall unsafe "hs_bindgen_9f387bde7802ae1f" hs_bindgen_9f387bde7802ae1f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_arr_args4@
hs_bindgen_9f387bde7802ae1f :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt))) -> IO ()))
hs_bindgen_9f387bde7802ae1f =
  fmap BG.fromFFIType hs_bindgen_9f387bde7802ae1f_base

{-# NOINLINE arr_args4 #-}
{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h 107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4 :: BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt))) -> IO ())
arr_args4 =
  BG.unsafePerformIO hs_bindgen_9f387bde7802ae1f

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_args1@
foreign import ccall unsafe "hs_bindgen_124fe40f5bbe1379" hs_bindgen_124fe40f5bbe1379_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_args1@
hs_bindgen_124fe40f5bbe1379 :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (IO ()) -> IO ()))
hs_bindgen_124fe40f5bbe1379 =
  fmap BG.fromFFIType hs_bindgen_124fe40f5bbe1379_base

{-# NOINLINE funptr_args1 #-}
{-| Function pointers

    __C declaration:__ @funptr_args1@

    __defined at:__ @macros\/reparse.h 126:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args1 :: BG.FunPtr (BG.CInt -> BG.FunPtr (IO ()) -> IO ())
funptr_args1 =
  BG.unsafePerformIO hs_bindgen_124fe40f5bbe1379

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_args2@
foreign import ccall unsafe "hs_bindgen_021c4bc8200b6125" hs_bindgen_021c4bc8200b6125_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_args2@
hs_bindgen_021c4bc8200b6125 :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (IO BG.CInt) -> IO ()))
hs_bindgen_021c4bc8200b6125 =
  fmap BG.fromFFIType hs_bindgen_021c4bc8200b6125_base

{-# NOINLINE funptr_args2 #-}
{-| __C declaration:__ @funptr_args2@

    __defined at:__ @macros\/reparse.h 127:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args2 :: BG.FunPtr (BG.CInt -> BG.FunPtr (IO BG.CInt) -> IO ())
funptr_args2 =
  BG.unsafePerformIO hs_bindgen_021c4bc8200b6125

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_args3@
foreign import ccall unsafe "hs_bindgen_59aa20565b1e992d" hs_bindgen_59aa20565b1e992d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_args3@
hs_bindgen_59aa20565b1e992d :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> IO ()) -> IO ()))
hs_bindgen_59aa20565b1e992d =
  fmap BG.fromFFIType hs_bindgen_59aa20565b1e992d_base

{-# NOINLINE funptr_args3 #-}
{-| __C declaration:__ @funptr_args3@

    __defined at:__ @macros\/reparse.h 128:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args3 :: BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> IO ()) -> IO ())
funptr_args3 =
  BG.unsafePerformIO hs_bindgen_59aa20565b1e992d

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_args4@
foreign import ccall unsafe "hs_bindgen_427ce337265ab6aa" hs_bindgen_427ce337265ab6aa_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_args4@
hs_bindgen_427ce337265ab6aa :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar) -> IO ()))
hs_bindgen_427ce337265ab6aa =
  fmap BG.fromFFIType hs_bindgen_427ce337265ab6aa_base

{-# NOINLINE funptr_args4 #-}
{-| __C declaration:__ @funptr_args4@

    __defined at:__ @macros\/reparse.h 129:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args4 :: BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar) -> IO ())
funptr_args4 =
  BG.unsafePerformIO hs_bindgen_427ce337265ab6aa

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_args5@
foreign import ccall unsafe "hs_bindgen_a2ccf8317cb85bbc" hs_bindgen_a2ccf8317cb85bbc_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_args5@
hs_bindgen_a2ccf8317cb85bbc :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)) -> IO ()))
hs_bindgen_a2ccf8317cb85bbc =
  fmap BG.fromFFIType hs_bindgen_a2ccf8317cb85bbc_base

{-# NOINLINE funptr_args5 #-}
{-| __C declaration:__ @funptr_args5@

    __defined at:__ @macros\/reparse.h 130:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args5 :: BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)) -> IO ())
funptr_args5 =
  BG.unsafePerformIO hs_bindgen_a2ccf8317cb85bbc

-- __unique:__ @test_macrosreparse_1_empty Example get_comments1@
foreign import ccall unsafe "hs_bindgen_78cdbd93ee108018" hs_bindgen_78cdbd93ee108018_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_comments1@
hs_bindgen_78cdbd93ee108018 :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_78cdbd93ee108018 =
  fmap BG.fromFFIType hs_bindgen_78cdbd93ee108018_base

{-# NOINLINE comments1 #-}
{-| Comments in awkward places

    (Prior to language-c we failed to parse there.)

    __C declaration:__ @comments1@

    __defined at:__ @macros\/reparse.h 144:25@

    __exported by:__ @macros\/reparse.h@
-}
comments1 :: BG.FunPtr (BG.CInt -> IO ())
comments1 =
  BG.unsafePerformIO hs_bindgen_78cdbd93ee108018

-- __unique:__ @test_macrosreparse_1_empty Example get_const_prim_before1@
foreign import ccall unsafe "hs_bindgen_0a00696d53bf51d7" hs_bindgen_0a00696d53bf51d7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_prim_before1@
hs_bindgen_0a00696d53bf51d7 :: IO (BG.FunPtr (BG.CInt -> BG.CChar -> IO ()))
hs_bindgen_0a00696d53bf51d7 =
  fmap BG.fromFFIType hs_bindgen_0a00696d53bf51d7_base

{-# NOINLINE const_prim_before1 #-}
{-| @const@ qualifier

    NOTE: These were not parsed correctly prior to the switch to language-c.

    __C declaration:__ @const_prim_before1@

    __defined at:__ @macros\/reparse.h 177:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before1 :: BG.FunPtr (BG.CInt -> BG.CChar -> IO ())
const_prim_before1 =
  BG.unsafePerformIO hs_bindgen_0a00696d53bf51d7

-- __unique:__ @test_macrosreparse_1_empty Example get_const_prim_before2@
foreign import ccall unsafe "hs_bindgen_847d8de1fdef0d63" hs_bindgen_847d8de1fdef0d63_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_prim_before2@
hs_bindgen_847d8de1fdef0d63 :: IO (BG.FunPtr (BG.CInt -> BG.CSChar -> IO ()))
hs_bindgen_847d8de1fdef0d63 =
  fmap BG.fromFFIType hs_bindgen_847d8de1fdef0d63_base

{-# NOINLINE const_prim_before2 #-}
{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @macros\/reparse.h 178:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before2 :: BG.FunPtr (BG.CInt -> BG.CSChar -> IO ())
const_prim_before2 =
  BG.unsafePerformIO hs_bindgen_847d8de1fdef0d63

-- __unique:__ @test_macrosreparse_1_empty Example get_const_prim_before3@
foreign import ccall unsafe "hs_bindgen_ab9369683a44025a" hs_bindgen_ab9369683a44025a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_prim_before3@
hs_bindgen_ab9369683a44025a :: IO (BG.FunPtr (BG.CInt -> BG.CUChar -> IO ()))
hs_bindgen_ab9369683a44025a =
  fmap BG.fromFFIType hs_bindgen_ab9369683a44025a_base

{-# NOINLINE const_prim_before3 #-}
{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @macros\/reparse.h 179:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before3 :: BG.FunPtr (BG.CInt -> BG.CUChar -> IO ())
const_prim_before3 =
  BG.unsafePerformIO hs_bindgen_ab9369683a44025a

-- __unique:__ @test_macrosreparse_1_empty Example get_const_prim_after1@
foreign import ccall unsafe "hs_bindgen_4c8cf0f3c66496c3" hs_bindgen_4c8cf0f3c66496c3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_prim_after1@
hs_bindgen_4c8cf0f3c66496c3 :: IO (BG.FunPtr (BG.CInt -> BG.CChar -> IO ()))
hs_bindgen_4c8cf0f3c66496c3 =
  fmap BG.fromFFIType hs_bindgen_4c8cf0f3c66496c3_base

{-# NOINLINE const_prim_after1 #-}
{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @macros\/reparse.h 180:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after1 :: BG.FunPtr (BG.CInt -> BG.CChar -> IO ())
const_prim_after1 =
  BG.unsafePerformIO hs_bindgen_4c8cf0f3c66496c3

-- __unique:__ @test_macrosreparse_1_empty Example get_const_prim_after2@
foreign import ccall unsafe "hs_bindgen_a1b28d990a0d0723" hs_bindgen_a1b28d990a0d0723_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_prim_after2@
hs_bindgen_a1b28d990a0d0723 :: IO (BG.FunPtr (BG.CInt -> BG.CSChar -> IO ()))
hs_bindgen_a1b28d990a0d0723 =
  fmap BG.fromFFIType hs_bindgen_a1b28d990a0d0723_base

{-# NOINLINE const_prim_after2 #-}
{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @macros\/reparse.h 181:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after2 :: BG.FunPtr (BG.CInt -> BG.CSChar -> IO ())
const_prim_after2 =
  BG.unsafePerformIO hs_bindgen_a1b28d990a0d0723

-- __unique:__ @test_macrosreparse_1_empty Example get_const_prim_after3@
foreign import ccall unsafe "hs_bindgen_7703cb78ab5310dd" hs_bindgen_7703cb78ab5310dd_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_prim_after3@
hs_bindgen_7703cb78ab5310dd :: IO (BG.FunPtr (BG.CInt -> BG.CUChar -> IO ()))
hs_bindgen_7703cb78ab5310dd =
  fmap BG.fromFFIType hs_bindgen_7703cb78ab5310dd_base

{-# NOINLINE const_prim_after3 #-}
{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @macros\/reparse.h 182:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after3 :: BG.FunPtr (BG.CInt -> BG.CUChar -> IO ())
const_prim_after3 =
  BG.unsafePerformIO hs_bindgen_7703cb78ab5310dd

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_before1@
foreign import ccall unsafe "hs_bindgen_a57679d81a13ecab" hs_bindgen_a57679d81a13ecab_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_before1@
hs_bindgen_a57679d81a13ecab :: IO (BG.FunPtr (BG.CInt -> BG.CFloat -> IO ()))
hs_bindgen_a57679d81a13ecab =
  fmap BG.fromFFIType hs_bindgen_a57679d81a13ecab_base

{-# NOINLINE const_withoutSign_before1 #-}
{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @macros\/reparse.h 186:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before1 :: BG.FunPtr (BG.CInt -> BG.CFloat -> IO ())
const_withoutSign_before1 =
  BG.unsafePerformIO hs_bindgen_a57679d81a13ecab

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_before2@
foreign import ccall unsafe "hs_bindgen_36aaf094d002e32b" hs_bindgen_36aaf094d002e32b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_before2@
hs_bindgen_36aaf094d002e32b :: IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO ()))
hs_bindgen_36aaf094d002e32b =
  fmap BG.fromFFIType hs_bindgen_36aaf094d002e32b_base

{-# NOINLINE const_withoutSign_before2 #-}
{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @macros\/reparse.h 187:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before2 :: BG.FunPtr (BG.CInt -> BG.CDouble -> IO ())
const_withoutSign_before2 =
  BG.unsafePerformIO hs_bindgen_36aaf094d002e32b

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_before3@
foreign import ccall unsafe "hs_bindgen_65464c79676bc4bd" hs_bindgen_65464c79676bc4bd_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_before3@
hs_bindgen_65464c79676bc4bd :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_65464c79676bc4bd =
  fmap BG.fromFFIType hs_bindgen_65464c79676bc4bd_base

{-# NOINLINE const_withoutSign_before3 #-}
{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @macros\/reparse.h 188:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before3 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
const_withoutSign_before3 =
  BG.unsafePerformIO hs_bindgen_65464c79676bc4bd

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_before4@
foreign import ccall unsafe "hs_bindgen_817575fe36932178" hs_bindgen_817575fe36932178_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_before4@
hs_bindgen_817575fe36932178 :: IO (BG.FunPtr (BG.CInt -> Some_struct -> IO ()))
hs_bindgen_817575fe36932178 =
  fmap BG.fromFFIType hs_bindgen_817575fe36932178_base

{-# NOINLINE const_withoutSign_before4 #-}
{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @macros\/reparse.h 189:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before4 :: BG.FunPtr (BG.CInt -> Some_struct -> IO ())
const_withoutSign_before4 =
  BG.unsafePerformIO hs_bindgen_817575fe36932178

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_before5@
foreign import ccall unsafe "hs_bindgen_32033004afbaef3d" hs_bindgen_32033004afbaef3d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_before5@
hs_bindgen_32033004afbaef3d :: IO (BG.FunPtr (BG.CInt -> Some_union -> IO ()))
hs_bindgen_32033004afbaef3d =
  fmap BG.fromFFIType hs_bindgen_32033004afbaef3d_base

{-# NOINLINE const_withoutSign_before5 #-}
{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @macros\/reparse.h 190:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before5 :: BG.FunPtr (BG.CInt -> Some_union -> IO ())
const_withoutSign_before5 =
  BG.unsafePerformIO hs_bindgen_32033004afbaef3d

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_before6@
foreign import ccall unsafe "hs_bindgen_d39d6ad1915299fd" hs_bindgen_d39d6ad1915299fd_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_before6@
hs_bindgen_d39d6ad1915299fd :: IO (BG.FunPtr (BG.CInt -> Some_enum -> IO ()))
hs_bindgen_d39d6ad1915299fd =
  fmap BG.fromFFIType hs_bindgen_d39d6ad1915299fd_base

{-# NOINLINE const_withoutSign_before6 #-}
{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @macros\/reparse.h 191:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before6 :: BG.FunPtr (BG.CInt -> Some_enum -> IO ())
const_withoutSign_before6 =
  BG.unsafePerformIO hs_bindgen_d39d6ad1915299fd

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_before7@
foreign import ccall unsafe "hs_bindgen_e1649462c2586bbc" hs_bindgen_e1649462c2586bbc_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_before7@
hs_bindgen_e1649462c2586bbc :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_e1649462c2586bbc =
  fmap BG.fromFFIType hs_bindgen_e1649462c2586bbc_base

{-# NOINLINE const_withoutSign_before7 #-}
{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @macros\/reparse.h 192:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before7 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
const_withoutSign_before7 =
  BG.unsafePerformIO hs_bindgen_e1649462c2586bbc

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_before8@
foreign import ccall unsafe "hs_bindgen_c5a3e2274972de41" hs_bindgen_c5a3e2274972de41_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_before8@
hs_bindgen_c5a3e2274972de41 :: IO (BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_c5a3e2274972de41 =
  fmap BG.fromFFIType hs_bindgen_c5a3e2274972de41_base

{-# NOINLINE const_withoutSign_before8 #-}
{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @macros\/reparse.h 193:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before8 :: BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ())
const_withoutSign_before8 =
  BG.unsafePerformIO hs_bindgen_c5a3e2274972de41

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_after1@
foreign import ccall unsafe "hs_bindgen_4654455003e81f77" hs_bindgen_4654455003e81f77_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_after1@
hs_bindgen_4654455003e81f77 :: IO (BG.FunPtr (BG.CInt -> BG.CFloat -> IO ()))
hs_bindgen_4654455003e81f77 =
  fmap BG.fromFFIType hs_bindgen_4654455003e81f77_base

{-# NOINLINE const_withoutSign_after1 #-}
{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @macros\/reparse.h 195:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after1 :: BG.FunPtr (BG.CInt -> BG.CFloat -> IO ())
const_withoutSign_after1 =
  BG.unsafePerformIO hs_bindgen_4654455003e81f77

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_after2@
foreign import ccall unsafe "hs_bindgen_0b3bd8d24696293a" hs_bindgen_0b3bd8d24696293a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_after2@
hs_bindgen_0b3bd8d24696293a :: IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO ()))
hs_bindgen_0b3bd8d24696293a =
  fmap BG.fromFFIType hs_bindgen_0b3bd8d24696293a_base

{-# NOINLINE const_withoutSign_after2 #-}
{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @macros\/reparse.h 196:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after2 :: BG.FunPtr (BG.CInt -> BG.CDouble -> IO ())
const_withoutSign_after2 =
  BG.unsafePerformIO hs_bindgen_0b3bd8d24696293a

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_after3@
foreign import ccall unsafe "hs_bindgen_220ab20973f59892" hs_bindgen_220ab20973f59892_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_after3@
hs_bindgen_220ab20973f59892 :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_220ab20973f59892 =
  fmap BG.fromFFIType hs_bindgen_220ab20973f59892_base

{-# NOINLINE const_withoutSign_after3 #-}
{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @macros\/reparse.h 197:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after3 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
const_withoutSign_after3 =
  BG.unsafePerformIO hs_bindgen_220ab20973f59892

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_after4@
foreign import ccall unsafe "hs_bindgen_b993563da63c65c7" hs_bindgen_b993563da63c65c7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_after4@
hs_bindgen_b993563da63c65c7 :: IO (BG.FunPtr (BG.CInt -> Some_struct -> IO ()))
hs_bindgen_b993563da63c65c7 =
  fmap BG.fromFFIType hs_bindgen_b993563da63c65c7_base

{-# NOINLINE const_withoutSign_after4 #-}
{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @macros\/reparse.h 198:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after4 :: BG.FunPtr (BG.CInt -> Some_struct -> IO ())
const_withoutSign_after4 =
  BG.unsafePerformIO hs_bindgen_b993563da63c65c7

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_after5@
foreign import ccall unsafe "hs_bindgen_34d4b754627ba4f7" hs_bindgen_34d4b754627ba4f7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_after5@
hs_bindgen_34d4b754627ba4f7 :: IO (BG.FunPtr (BG.CInt -> Some_union -> IO ()))
hs_bindgen_34d4b754627ba4f7 =
  fmap BG.fromFFIType hs_bindgen_34d4b754627ba4f7_base

{-# NOINLINE const_withoutSign_after5 #-}
{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @macros\/reparse.h 199:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after5 :: BG.FunPtr (BG.CInt -> Some_union -> IO ())
const_withoutSign_after5 =
  BG.unsafePerformIO hs_bindgen_34d4b754627ba4f7

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_after6@
foreign import ccall unsafe "hs_bindgen_32f4457388158d62" hs_bindgen_32f4457388158d62_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_after6@
hs_bindgen_32f4457388158d62 :: IO (BG.FunPtr (BG.CInt -> Some_enum -> IO ()))
hs_bindgen_32f4457388158d62 =
  fmap BG.fromFFIType hs_bindgen_32f4457388158d62_base

{-# NOINLINE const_withoutSign_after6 #-}
{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @macros\/reparse.h 200:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after6 :: BG.FunPtr (BG.CInt -> Some_enum -> IO ())
const_withoutSign_after6 =
  BG.unsafePerformIO hs_bindgen_32f4457388158d62

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_after7@
foreign import ccall unsafe "hs_bindgen_20d8ab69c805160d" hs_bindgen_20d8ab69c805160d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_after7@
hs_bindgen_20d8ab69c805160d :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_20d8ab69c805160d =
  fmap BG.fromFFIType hs_bindgen_20d8ab69c805160d_base

{-# NOINLINE const_withoutSign_after7 #-}
{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @macros\/reparse.h 201:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after7 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
const_withoutSign_after7 =
  BG.unsafePerformIO hs_bindgen_20d8ab69c805160d

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_after8@
foreign import ccall unsafe "hs_bindgen_c3abec34d30decc7" hs_bindgen_c3abec34d30decc7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_withoutSign_after8@
hs_bindgen_c3abec34d30decc7 :: IO (BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_c3abec34d30decc7 =
  fmap BG.fromFFIType hs_bindgen_c3abec34d30decc7_base

{-# NOINLINE const_withoutSign_after8 #-}
{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @macros\/reparse.h 202:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after8 :: BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ())
const_withoutSign_after8 =
  BG.unsafePerformIO hs_bindgen_c3abec34d30decc7

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_args1@
foreign import ccall unsafe "hs_bindgen_f0e9db228d72ad57" hs_bindgen_f0e9db228d72ad57_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_args1@
hs_bindgen_f0e9db228d72ad57 :: IO (BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_f0e9db228d72ad57 =
  fmap BG.fromFFIType hs_bindgen_f0e9db228d72ad57_base

{-# NOINLINE const_pointers_args1 #-}
{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @macros\/reparse.h 206:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args1 :: BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args1 =
  BG.unsafePerformIO hs_bindgen_f0e9db228d72ad57

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_args2@
foreign import ccall unsafe "hs_bindgen_b40b6075be3b49ac" hs_bindgen_b40b6075be3b49ac_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_args2@
hs_bindgen_b40b6075be3b49ac :: IO (BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_b40b6075be3b49ac =
  fmap BG.fromFFIType hs_bindgen_b40b6075be3b49ac_base

{-# NOINLINE const_pointers_args2 #-}
{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @macros\/reparse.h 207:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args2 :: BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args2 =
  BG.unsafePerformIO hs_bindgen_b40b6075be3b49ac

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_args3@
foreign import ccall unsafe "hs_bindgen_464404413a369761" hs_bindgen_464404413a369761_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_args3@
hs_bindgen_464404413a369761 :: IO (BG.FunPtr (BG.CInt -> BG.Ptr BG.CInt -> IO ()))
hs_bindgen_464404413a369761 =
  fmap BG.fromFFIType hs_bindgen_464404413a369761_base

{-# NOINLINE const_pointers_args3 #-}
{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @macros\/reparse.h 208:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args3 :: BG.FunPtr (BG.CInt -> BG.Ptr BG.CInt -> IO ())
const_pointers_args3 =
  BG.unsafePerformIO hs_bindgen_464404413a369761

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_args4@
foreign import ccall unsafe "hs_bindgen_9818a53206f7c636" hs_bindgen_9818a53206f7c636_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_args4@
hs_bindgen_9818a53206f7c636 :: IO (BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_9818a53206f7c636 =
  fmap BG.fromFFIType hs_bindgen_9818a53206f7c636_base

{-# NOINLINE const_pointers_args4 #-}
{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @macros\/reparse.h 209:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args4 :: BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args4 =
  BG.unsafePerformIO hs_bindgen_9818a53206f7c636

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_args5@
foreign import ccall unsafe "hs_bindgen_453e4eff0f5200e8" hs_bindgen_453e4eff0f5200e8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_args5@
hs_bindgen_453e4eff0f5200e8 :: IO (BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_453e4eff0f5200e8 =
  fmap BG.fromFFIType hs_bindgen_453e4eff0f5200e8_base

{-# NOINLINE const_pointers_args5 #-}
{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @macros\/reparse.h 210:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args5 :: BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args5 =
  BG.unsafePerformIO hs_bindgen_453e4eff0f5200e8

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_ret1@
foreign import ccall unsafe "hs_bindgen_ed6fad795fa8294d" hs_bindgen_ed6fad795fa8294d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_ret1@
hs_bindgen_ed6fad795fa8294d :: IO (BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_ed6fad795fa8294d =
  fmap BG.fromFFIType hs_bindgen_ed6fad795fa8294d_base

{-# NOINLINE const_pointers_ret1 #-}
{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h 212:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1 :: BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret1 =
  BG.unsafePerformIO hs_bindgen_ed6fad795fa8294d

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_ret2@
foreign import ccall unsafe "hs_bindgen_52d4a25d38622fdb" hs_bindgen_52d4a25d38622fdb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_ret2@
hs_bindgen_52d4a25d38622fdb :: IO (BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_52d4a25d38622fdb =
  fmap BG.fromFFIType hs_bindgen_52d4a25d38622fdb_base

{-# NOINLINE const_pointers_ret2 #-}
{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h 213:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2 :: BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret2 =
  BG.unsafePerformIO hs_bindgen_52d4a25d38622fdb

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_ret3@
foreign import ccall unsafe "hs_bindgen_e65f7f4766a4354f" hs_bindgen_e65f7f4766a4354f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_ret3@
hs_bindgen_e65f7f4766a4354f :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt)))
hs_bindgen_e65f7f4766a4354f =
  fmap BG.fromFFIType hs_bindgen_e65f7f4766a4354f_base

{-# NOINLINE const_pointers_ret3 #-}
{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h 214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt))
const_pointers_ret3 =
  BG.unsafePerformIO hs_bindgen_e65f7f4766a4354f

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_ret4@
foreign import ccall unsafe "hs_bindgen_e0583a9affc1dc51" hs_bindgen_e0583a9affc1dc51_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_ret4@
hs_bindgen_e0583a9affc1dc51 :: IO (BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_e0583a9affc1dc51 =
  fmap BG.fromFFIType hs_bindgen_e0583a9affc1dc51_base

{-# NOINLINE const_pointers_ret4 #-}
{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h 215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4 :: BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret4 =
  BG.unsafePerformIO hs_bindgen_e0583a9affc1dc51

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_ret5@
foreign import ccall unsafe "hs_bindgen_e3c18734433481cb" hs_bindgen_e3c18734433481cb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_pointers_ret5@
hs_bindgen_e3c18734433481cb :: IO (BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_e3c18734433481cb =
  fmap BG.fromFFIType hs_bindgen_e3c18734433481cb_base

{-# NOINLINE const_pointers_ret5 #-}
{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h 216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5 :: BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret5 =
  BG.unsafePerformIO hs_bindgen_e3c18734433481cb

-- __unique:__ @test_macrosreparse_1_empty Example get_const_array_elem1@
foreign import ccall unsafe "hs_bindgen_b6d3b4f3be6373ff" hs_bindgen_b6d3b4f3be6373ff_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_array_elem1@
hs_bindgen_b6d3b4f3be6373ff :: IO (BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO ()))
hs_bindgen_b6d3b4f3be6373ff =
  fmap BG.fromFFIType hs_bindgen_b6d3b4f3be6373ff_base

{-# NOINLINE const_array_elem1 #-}
{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h 244:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 :: BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO ())
const_array_elem1 =
  BG.unsafePerformIO hs_bindgen_b6d3b4f3be6373ff

-- __unique:__ @test_macrosreparse_1_empty Example get_const_array_elem2@
foreign import ccall unsafe "hs_bindgen_8bff41ae0d6eaeaa" hs_bindgen_8bff41ae0d6eaeaa_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_array_elem2@
hs_bindgen_8bff41ae0d6eaeaa :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt))) -> IO ()))
hs_bindgen_8bff41ae0d6eaeaa =
  fmap BG.fromFFIType hs_bindgen_8bff41ae0d6eaeaa_base

{-# NOINLINE const_array_elem2 #-}
{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h 245:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2 :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt))) -> IO ())
const_array_elem2 =
  BG.unsafePerformIO hs_bindgen_8bff41ae0d6eaeaa

-- __unique:__ @test_macrosreparse_1_empty Example get_const_array_elem3@
foreign import ccall unsafe "hs_bindgen_11f570604977bc68" hs_bindgen_11f570604977bc68_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_const_array_elem3@
hs_bindgen_11f570604977bc68 :: IO (BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt))) -> IO ()))
hs_bindgen_11f570604977bc68 =
  fmap BG.fromFFIType hs_bindgen_11f570604977bc68_base

{-# NOINLINE const_array_elem3 #-}
{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h 246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 :: BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt))) -> IO ())
const_array_elem3 =
  BG.unsafePerformIO hs_bindgen_11f570604977bc68

-- __unique:__ @test_macrosreparse_1_empty Example get_noParams1@
foreign import ccall unsafe "hs_bindgen_14fa7f8f15d56c6d" hs_bindgen_14fa7f8f15d56c6d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_noParams1@
hs_bindgen_14fa7f8f15d56c6d :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_14fa7f8f15d56c6d =
  fmap BG.fromFFIType hs_bindgen_14fa7f8f15d56c6d_base

{-# NOINLINE noParams1 #-}
{-| Other examples we reparsed /incorrectly/ before language-c

    __C declaration:__ @noParams1@

    __defined at:__ @macros\/reparse.h 254:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams1 :: BG.FunPtr (IO BG.CInt)
noParams1 =
  BG.unsafePerformIO hs_bindgen_14fa7f8f15d56c6d

-- __unique:__ @test_macrosreparse_1_empty Example get_noParams2@
foreign import ccall unsafe "hs_bindgen_422decc683401449" hs_bindgen_422decc683401449_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_noParams2@
hs_bindgen_422decc683401449 :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_422decc683401449 =
  fmap BG.fromFFIType hs_bindgen_422decc683401449_base

{-# NOINLINE noParams2 #-}
{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h 255:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2 :: BG.FunPtr (IO BG.CInt)
noParams2 =
  BG.unsafePerformIO hs_bindgen_422decc683401449

-- __unique:__ @test_macrosreparse_1_empty Example get_noParams3@
foreign import ccall unsafe "hs_bindgen_e8c4551a87246d1c" hs_bindgen_e8c4551a87246d1c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_noParams3@
hs_bindgen_e8c4551a87246d1c :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (IO BG.CInt) -> IO ()))
hs_bindgen_e8c4551a87246d1c =
  fmap BG.fromFFIType hs_bindgen_e8c4551a87246d1c_base

{-# NOINLINE noParams3 #-}
{-| __C declaration:__ @noParams3@

    __defined at:__ @macros\/reparse.h 256:6@

    __exported by:__ @macros\/reparse.h@
-}
noParams3 :: BG.FunPtr (BG.CInt -> BG.FunPtr (IO BG.CInt) -> IO ())
noParams3 =
  BG.unsafePerformIO hs_bindgen_e8c4551a87246d1c

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret1@
foreign import ccall unsafe "hs_bindgen_44f4d68423e3e514" hs_bindgen_44f4d68423e3e514_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret1@
hs_bindgen_44f4d68423e3e514 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (IO ()))))
hs_bindgen_44f4d68423e3e514 =
  fmap BG.fromFFIType hs_bindgen_44f4d68423e3e514_base

{-# NOINLINE funptr_ret1 #-}
{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h 260:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (IO ())))
funptr_ret1 =
  BG.unsafePerformIO hs_bindgen_44f4d68423e3e514

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret2@
foreign import ccall unsafe "hs_bindgen_9f1c61d989649198" hs_bindgen_9f1c61d989649198_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret2@
hs_bindgen_9f1c61d989649198 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (IO BG.CInt))))
hs_bindgen_9f1c61d989649198 =
  fmap BG.fromFFIType hs_bindgen_9f1c61d989649198_base

{-# NOINLINE funptr_ret2 #-}
{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h 261:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (IO BG.CInt)))
funptr_ret2 =
  BG.unsafePerformIO hs_bindgen_9f1c61d989649198

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret3@
foreign import ccall unsafe "hs_bindgen_10d924f22b46c231" hs_bindgen_10d924f22b46c231_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret3@
hs_bindgen_10d924f22b46c231 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> IO ()))))
hs_bindgen_10d924f22b46c231 =
  fmap BG.fromFFIType hs_bindgen_10d924f22b46c231_base

{-# NOINLINE funptr_ret3 #-}
{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h 262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> IO ())))
funptr_ret3 =
  BG.unsafePerformIO hs_bindgen_10d924f22b46c231

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret4@
foreign import ccall unsafe "hs_bindgen_17cfc42831d87b39" hs_bindgen_17cfc42831d87b39_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret4@
hs_bindgen_17cfc42831d87b39 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))))
hs_bindgen_17cfc42831d87b39 =
  fmap BG.fromFFIType hs_bindgen_17cfc42831d87b39_base

{-# NOINLINE funptr_ret4 #-}
{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h 263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar)))
funptr_ret4 =
  BG.unsafePerformIO hs_bindgen_17cfc42831d87b39

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret5@
foreign import ccall unsafe "hs_bindgen_78244ef58fe5d49a" hs_bindgen_78244ef58fe5d49a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret5@
hs_bindgen_78244ef58fe5d49a :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))))
hs_bindgen_78244ef58fe5d49a =
  fmap BG.fromFFIType hs_bindgen_78244ef58fe5d49a_base

{-# NOINLINE funptr_ret5 #-}
{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h 267:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))))
funptr_ret5 =
  BG.unsafePerformIO hs_bindgen_78244ef58fe5d49a

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret6@
foreign import ccall unsafe "hs_bindgen_a16e9de4b496b79d" hs_bindgen_a16e9de4b496b79d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret6@
hs_bindgen_a16e9de4b496b79d :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_a16e9de4b496b79d =
  fmap BG.fromFFIType hs_bindgen_a16e9de4b496b79d_base

{-# NOINLINE funptr_ret6 #-}
{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h 268:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret6 =
  BG.unsafePerformIO hs_bindgen_a16e9de4b496b79d

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret7@
foreign import ccall unsafe "hs_bindgen_1145e079d0e78d69" hs_bindgen_1145e079d0e78d69_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret7@
hs_bindgen_1145e079d0e78d69 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_1145e079d0e78d69 =
  fmap BG.fromFFIType hs_bindgen_1145e079d0e78d69_base

{-# NOINLINE funptr_ret7 #-}
{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h 269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret7 =
  BG.unsafePerformIO hs_bindgen_1145e079d0e78d69

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret8@
foreign import ccall unsafe "hs_bindgen_962f6126bcb41c35" hs_bindgen_962f6126bcb41c35_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret8@
hs_bindgen_962f6126bcb41c35 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))))
hs_bindgen_962f6126bcb41c35 =
  fmap BG.fromFFIType hs_bindgen_962f6126bcb41c35_base

{-# NOINLINE funptr_ret8 #-}
{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h 270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))))
funptr_ret8 =
  BG.unsafePerformIO hs_bindgen_962f6126bcb41c35

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret9@
foreign import ccall unsafe "hs_bindgen_1e58dd0c6272d23c" hs_bindgen_1e58dd0c6272d23c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret9@
hs_bindgen_1e58dd0c6272d23c :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_1e58dd0c6272d23c =
  fmap BG.fromFFIType hs_bindgen_1e58dd0c6272d23c_base

{-# NOINLINE funptr_ret9 #-}
{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h 271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret9 =
  BG.unsafePerformIO hs_bindgen_1e58dd0c6272d23c

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret10@
foreign import ccall unsafe "hs_bindgen_72310ec4ae07cff9" hs_bindgen_72310ec4ae07cff9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example get_funptr_ret10@
hs_bindgen_72310ec4ae07cff9 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_72310ec4ae07cff9 =
  fmap BG.fromFFIType hs_bindgen_72310ec4ae07cff9_base

{-# NOINLINE funptr_ret10 #-}
{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h 272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret10 =
  BG.unsafePerformIO hs_bindgen_72310ec4ae07cff9
