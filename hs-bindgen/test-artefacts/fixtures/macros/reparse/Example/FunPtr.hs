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
  , "/* test_macrosreparse\NULExample\NULget_args_char1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6fcafc2b9c5358c8 (void)) ("
  , "  A arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &args_char1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_char2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7d5da811928c82b2 (void)) ("
  , "  A arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  return &args_char2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_char3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e3879553ba7add09 (void)) ("
  , "  A arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  return &args_char3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_short1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9ca0380f93a2e76a (void)) ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_short2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_690d018e7cf05d9e (void)) ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_short3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_567eaa0a0b58b480 (void)) ("
  , "  A arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  return &args_short3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_int1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3d34f4bfb8feb797 (void)) ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_int2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_60e0714beb995bff (void)) ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_int3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_08a8b47bde68f109 (void)) ("
  , "  A arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  return &args_int3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_long1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9f7be2560a99069f (void)) ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_long2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7bec6ab993cc8430 (void)) ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_long3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2edeed001a44500a (void)) ("
  , "  A arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  return &args_long3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_float */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4e60f10b8262726b (void)) ("
  , "  A arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  return &args_float;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_double */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1aaac06ad29a6778 (void)) ("
  , "  A arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &args_double;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_bool1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c2b8c71e7ccb86e5 (void)) ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &args_bool1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_struct */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_388fb8d4b6729e1c (void)) ("
  , "  A arg1,"
  , "  struct some_struct arg2"
  , ")"
  , "{"
  , "  return &args_struct;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_union */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c7504cb02883c717 (void)) ("
  , "  A arg1,"
  , "  union some_union arg2"
  , ")"
  , "{"
  , "  return &args_union;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_enum */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f119881032357e11 (void)) ("
  , "  A arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  return &args_enum;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_pointer1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2b1b041c9404e7b0 (void)) ("
  , "  A arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return &args_pointer1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_pointer2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_161e127836aebfdc (void)) ("
  , "  A arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  return &args_pointer2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_pointer3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_141facd965271dba (void)) ("
  , "  A arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  return &args_pointer3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_A */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_975f44422deea845 (void)) (void)"
  , "{"
  , "  return &ret_A;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_char1 */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_4b2e3ebe2eba0853 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_char2 */"
  , "__attribute__ ((const))"
  , "signed char (*hs_bindgen_b65af076b18439b1 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_char3 */"
  , "__attribute__ ((const))"
  , "unsigned char (*hs_bindgen_716f16d7138a5f7a (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_short1 */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_374162a0d7f996c6 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_short2 */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_f958e4856271e9ed (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_short3 */"
  , "__attribute__ ((const))"
  , "unsigned short (*hs_bindgen_6fa7ea9fd94d9626 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_int1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_c1d9de4abd24aa21 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_int2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_3d7fe4b9bbcb3ebb (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_int3 */"
  , "__attribute__ ((const))"
  , "unsigned int (*hs_bindgen_ebe1a790179e5a4e (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_long1 */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_03df01fe9976a201 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_long2 */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_785ad7b473f39e72 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_long3 */"
  , "__attribute__ ((const))"
  , "unsigned long (*hs_bindgen_d7a4dc339e6a2928 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_float */"
  , "__attribute__ ((const))"
  , "float (*hs_bindgen_1b9b45325315c025 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_float;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_double */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_605dc93b0201eeb9 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_double;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_bool1 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_47c345c53b8182c5 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_bool1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_struct */"
  , "__attribute__ ((const))"
  , "struct some_struct (*hs_bindgen_429842617174d1bb (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_struct;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_union */"
  , "__attribute__ ((const))"
  , "union some_union (*hs_bindgen_8ab286c7b08e02f9 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_union;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_enum */"
  , "__attribute__ ((const))"
  , "enum some_enum (*hs_bindgen_c8158d45c5fc6ada (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_enum;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_pointer1 */"
  , "__attribute__ ((const))"
  , "signed int *(*hs_bindgen_bc4c8c7932ff4a5e (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_pointer2 */"
  , "__attribute__ ((const))"
  , "signed int **(*hs_bindgen_5129055a6bb8be29 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_pointer3 */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_557d79219f5ea78e (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_body1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_33be18adba0502b6 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &body1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_body2 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_246e43cadb053f42 (void)) (void)"
  , "{"
  , "  return &body2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_complex_float */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b0616e97db902818 (void)) ("
  , "  A arg1,"
  , "  float _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_float;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_args_complex_double */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_117dbdac5517f462 (void)) ("
  , "  A arg1,"
  , "  double _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_double;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_complex_float */"
  , "__attribute__ ((const))"
  , "float _Complex (*hs_bindgen_bbcfef4bf6d547f9 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_complex_float;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_ret_complex_double */"
  , "__attribute__ ((const))"
  , "double _Complex (*hs_bindgen_e95c8ae3a376d5b3 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_complex_double;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_bespoke_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1e61c012a7118651 (void)) ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &bespoke_args1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_bespoke_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c7c83f1a0ced2aae (void)) ("
  , "  A arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &bespoke_args2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_bespoke_ret1 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_ad569701e220dec4 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_bespoke_ret2 */"
  , "__attribute__ ((const))"
  , "size_t (*hs_bindgen_94ed8d77dbd6264b (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_arr_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9be695c779650a36 (void)) ("
  , "  A arg1[]"
  , ")"
  , "{"
  , "  return &arr_args1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_arr_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_40f53b981ef810db (void)) ("
  , "  A *arg1[]"
  , ")"
  , "{"
  , "  return &arr_args2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_arr_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_28eaac40215082de (void)) ("
  , "  A arg1[5]"
  , ")"
  , "{"
  , "  return &arr_args3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_arr_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b336fad742b74b3d (void)) ("
  , "  A *arg1[5]"
  , ")"
  , "{"
  , "  return &arr_args4;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_funptr_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_73c83b6b1eaec7d1 (void)) ("
  , "  A arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_funptr_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_613cdb3e8987e37d (void)) ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_funptr_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_21dd820dfeaa80cf (void)) ("
  , "  A arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_funptr_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0a97bbfec852a72e (void)) ("
  , "  A arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args4;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_funptr_args5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_96c5fdf3502111c6 (void)) ("
  , "  A arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args5;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_comments1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1c6e5af6fab8eb0c (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &comments1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_prim_before1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0c66b303d14abd34 (void)) ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_prim_before2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1b1e518db6a12c03 (void)) ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_prim_before3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b0d212ee3212eef3 (void)) ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_prim_after1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0a20685281625ec0 (void)) ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_prim_after2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_215b9d9881cfc3bc (void)) ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_prim_after3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3bfa0a0c9f9466f2 (void)) ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_withoutSign_before1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d80eaf9ff8c0ee3a (void)) ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_withoutSign_before2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_983d81879a8362e8 (void)) ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_withoutSign_before3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8d25a7a2211fc8da (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_withoutSign_before4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_29d52174b8dc62c8 (void)) ("
  , "  A arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before4;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_withoutSign_before5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fa149a3de1c7507c (void)) ("
  , "  A arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before5;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_withoutSign_before6 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ae4b21912dc3e9fc (void)) ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before6;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_withoutSign_before7 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2823ede1fece8296 (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before7;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_withoutSign_before8 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_251faa95ed2fbdfe (void)) ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before8;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_withoutSign_after1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_64d880340cdd333b (void)) ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_withoutSign_after2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a0a7ed95c70e65de (void)) ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_withoutSign_after3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_743e0cfcefbe1d8a (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_withoutSign_after4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_18bf3095221fb3ea (void)) ("
  , "  A arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after4;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_withoutSign_after5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2bed6677ba4e137b (void)) ("
  , "  A arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after5;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_withoutSign_after6 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f1507092da27bfd5 (void)) ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after6;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_withoutSign_after7 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2e2acc8439a2c616 (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after7;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_withoutSign_after8 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2c66fc7d02e1638c (void)) ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after8;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_pointers_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_876a113f07d4f5f9 (void)) ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_pointers_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b660566323282737 (void)) ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_pointers_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_994dc853f2619a05 (void)) ("
  , "  A arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_pointers_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1e461cf8b8eb6c2e (void)) ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args4;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_pointers_args5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_de3f03872be78d51 (void)) ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args5;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_pointers_ret1 */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_b436bed7a8df6aa7 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_pointers_ret2 */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_a813e17971e4052b (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_pointers_ret3 */"
  , "__attribute__ ((const))"
  , "signed int *const (*hs_bindgen_ef67874c5a6020ba (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_pointers_ret4 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_290705eae4a932ea (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret4;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_pointers_ret5 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_1aab75ab18dfc1f9 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret5;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_array_elem1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b0b32a29c796f2d8 (void)) ("
  , "  A const arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_array_elem2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c8da796954fe25ed (void)) ("
  , "  A const *arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_const_array_elem3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9fd2564dafc0b34b (void)) ("
  , "  A *const arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_noParams1 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_a95db292e183a1d4 (void)) (void)"
  , "{"
  , "  return &noParams1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_noParams2 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_601cc797be54ac76 (void)) (void)"
  , "{"
  , "  return &noParams2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_noParams3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2c9b26f803b2d2d0 (void)) ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &noParams3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_funptr_ret1 */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_a8db83002fa60823 (void)) ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret1;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_funptr_ret2 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_2c24c1c6748f2c20 (void)) ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret2;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_funptr_ret3 */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_b16880cfb2c2c277 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &funptr_ret3;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_funptr_ret4 */"
  , "__attribute__ ((const))"
  , "char (*(*hs_bindgen_16971b9d43f8b276 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret4;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_funptr_ret5 */"
  , "__attribute__ ((const))"
  , "signed int *(*(*hs_bindgen_559439091def57b7 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret5;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_funptr_ret6 */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_417ef356c0a65410 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret6;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_funptr_ret7 */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_397041070208b29f (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret7;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_funptr_ret8 */"
  , "__attribute__ ((const))"
  , "signed int *const (*(*hs_bindgen_4e74e96bb0ca12e1 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret8;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_funptr_ret9 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_c8c300f87d0d976b (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret9;"
  , "}"
  , "/* test_macrosreparse\NULExample\NULget_funptr_ret10 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_b5fa1f4fdbdf537e (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret10;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse Example get_args_char1@
foreign import ccall unsafe "hs_bindgen_6fcafc2b9c5358c8" hs_bindgen_6fcafc2b9c5358c8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_char1@
hs_bindgen_6fcafc2b9c5358c8 :: IO (BG.FunPtr (A -> BG.CChar -> IO ()))
hs_bindgen_6fcafc2b9c5358c8 =
  fmap BG.fromFFIType hs_bindgen_6fcafc2b9c5358c8_base

{-# NOINLINE args_char1 #-}
{-| Function declarations

    __C declaration:__ @args_char1@

    __defined at:__ @macros\/reparse.h 17:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char1 :: BG.FunPtr (A -> BG.CChar -> IO ())
args_char1 =
  BG.unsafePerformIO hs_bindgen_6fcafc2b9c5358c8

-- __unique:__ @test_macrosreparse Example get_args_char2@
foreign import ccall unsafe "hs_bindgen_7d5da811928c82b2" hs_bindgen_7d5da811928c82b2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_char2@
hs_bindgen_7d5da811928c82b2 :: IO (BG.FunPtr (A -> BG.CSChar -> IO ()))
hs_bindgen_7d5da811928c82b2 =
  fmap BG.fromFFIType hs_bindgen_7d5da811928c82b2_base

{-# NOINLINE args_char2 #-}
{-| __C declaration:__ @args_char2@

    __defined at:__ @macros\/reparse.h 18:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char2 :: BG.FunPtr (A -> BG.CSChar -> IO ())
args_char2 =
  BG.unsafePerformIO hs_bindgen_7d5da811928c82b2

-- __unique:__ @test_macrosreparse Example get_args_char3@
foreign import ccall unsafe "hs_bindgen_e3879553ba7add09" hs_bindgen_e3879553ba7add09_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_char3@
hs_bindgen_e3879553ba7add09 :: IO (BG.FunPtr (A -> BG.CUChar -> IO ()))
hs_bindgen_e3879553ba7add09 =
  fmap BG.fromFFIType hs_bindgen_e3879553ba7add09_base

{-# NOINLINE args_char3 #-}
{-| __C declaration:__ @args_char3@

    __defined at:__ @macros\/reparse.h 19:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char3 :: BG.FunPtr (A -> BG.CUChar -> IO ())
args_char3 =
  BG.unsafePerformIO hs_bindgen_e3879553ba7add09

-- __unique:__ @test_macrosreparse Example get_args_short1@
foreign import ccall unsafe "hs_bindgen_9ca0380f93a2e76a" hs_bindgen_9ca0380f93a2e76a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_short1@
hs_bindgen_9ca0380f93a2e76a :: IO (BG.FunPtr (A -> BG.CShort -> IO ()))
hs_bindgen_9ca0380f93a2e76a =
  fmap BG.fromFFIType hs_bindgen_9ca0380f93a2e76a_base

{-# NOINLINE args_short1 #-}
{-| __C declaration:__ @args_short1@

    __defined at:__ @macros\/reparse.h 21:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short1 :: BG.FunPtr (A -> BG.CShort -> IO ())
args_short1 =
  BG.unsafePerformIO hs_bindgen_9ca0380f93a2e76a

-- __unique:__ @test_macrosreparse Example get_args_short2@
foreign import ccall unsafe "hs_bindgen_690d018e7cf05d9e" hs_bindgen_690d018e7cf05d9e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_short2@
hs_bindgen_690d018e7cf05d9e :: IO (BG.FunPtr (A -> BG.CShort -> IO ()))
hs_bindgen_690d018e7cf05d9e =
  fmap BG.fromFFIType hs_bindgen_690d018e7cf05d9e_base

{-# NOINLINE args_short2 #-}
{-| __C declaration:__ @args_short2@

    __defined at:__ @macros\/reparse.h 22:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short2 :: BG.FunPtr (A -> BG.CShort -> IO ())
args_short2 =
  BG.unsafePerformIO hs_bindgen_690d018e7cf05d9e

-- __unique:__ @test_macrosreparse Example get_args_short3@
foreign import ccall unsafe "hs_bindgen_567eaa0a0b58b480" hs_bindgen_567eaa0a0b58b480_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_short3@
hs_bindgen_567eaa0a0b58b480 :: IO (BG.FunPtr (A -> BG.CUShort -> IO ()))
hs_bindgen_567eaa0a0b58b480 =
  fmap BG.fromFFIType hs_bindgen_567eaa0a0b58b480_base

{-# NOINLINE args_short3 #-}
{-| __C declaration:__ @args_short3@

    __defined at:__ @macros\/reparse.h 23:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short3 :: BG.FunPtr (A -> BG.CUShort -> IO ())
args_short3 =
  BG.unsafePerformIO hs_bindgen_567eaa0a0b58b480

-- __unique:__ @test_macrosreparse Example get_args_int1@
foreign import ccall unsafe "hs_bindgen_3d34f4bfb8feb797" hs_bindgen_3d34f4bfb8feb797_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_int1@
hs_bindgen_3d34f4bfb8feb797 :: IO (BG.FunPtr (A -> BG.CInt -> IO ()))
hs_bindgen_3d34f4bfb8feb797 =
  fmap BG.fromFFIType hs_bindgen_3d34f4bfb8feb797_base

{-# NOINLINE args_int1 #-}
{-| __C declaration:__ @args_int1@

    __defined at:__ @macros\/reparse.h 25:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int1 :: BG.FunPtr (A -> BG.CInt -> IO ())
args_int1 =
  BG.unsafePerformIO hs_bindgen_3d34f4bfb8feb797

-- __unique:__ @test_macrosreparse Example get_args_int2@
foreign import ccall unsafe "hs_bindgen_60e0714beb995bff" hs_bindgen_60e0714beb995bff_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_int2@
hs_bindgen_60e0714beb995bff :: IO (BG.FunPtr (A -> BG.CInt -> IO ()))
hs_bindgen_60e0714beb995bff =
  fmap BG.fromFFIType hs_bindgen_60e0714beb995bff_base

{-# NOINLINE args_int2 #-}
{-| __C declaration:__ @args_int2@

    __defined at:__ @macros\/reparse.h 26:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int2 :: BG.FunPtr (A -> BG.CInt -> IO ())
args_int2 =
  BG.unsafePerformIO hs_bindgen_60e0714beb995bff

-- __unique:__ @test_macrosreparse Example get_args_int3@
foreign import ccall unsafe "hs_bindgen_08a8b47bde68f109" hs_bindgen_08a8b47bde68f109_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_int3@
hs_bindgen_08a8b47bde68f109 :: IO (BG.FunPtr (A -> BG.CUInt -> IO ()))
hs_bindgen_08a8b47bde68f109 =
  fmap BG.fromFFIType hs_bindgen_08a8b47bde68f109_base

{-# NOINLINE args_int3 #-}
{-| __C declaration:__ @args_int3@

    __defined at:__ @macros\/reparse.h 27:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int3 :: BG.FunPtr (A -> BG.CUInt -> IO ())
args_int3 =
  BG.unsafePerformIO hs_bindgen_08a8b47bde68f109

-- __unique:__ @test_macrosreparse Example get_args_long1@
foreign import ccall unsafe "hs_bindgen_9f7be2560a99069f" hs_bindgen_9f7be2560a99069f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_long1@
hs_bindgen_9f7be2560a99069f :: IO (BG.FunPtr (A -> BG.CLong -> IO ()))
hs_bindgen_9f7be2560a99069f =
  fmap BG.fromFFIType hs_bindgen_9f7be2560a99069f_base

{-# NOINLINE args_long1 #-}
{-| __C declaration:__ @args_long1@

    __defined at:__ @macros\/reparse.h 29:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long1 :: BG.FunPtr (A -> BG.CLong -> IO ())
args_long1 =
  BG.unsafePerformIO hs_bindgen_9f7be2560a99069f

-- __unique:__ @test_macrosreparse Example get_args_long2@
foreign import ccall unsafe "hs_bindgen_7bec6ab993cc8430" hs_bindgen_7bec6ab993cc8430_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_long2@
hs_bindgen_7bec6ab993cc8430 :: IO (BG.FunPtr (A -> BG.CLong -> IO ()))
hs_bindgen_7bec6ab993cc8430 =
  fmap BG.fromFFIType hs_bindgen_7bec6ab993cc8430_base

{-# NOINLINE args_long2 #-}
{-| __C declaration:__ @args_long2@

    __defined at:__ @macros\/reparse.h 30:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long2 :: BG.FunPtr (A -> BG.CLong -> IO ())
args_long2 =
  BG.unsafePerformIO hs_bindgen_7bec6ab993cc8430

-- __unique:__ @test_macrosreparse Example get_args_long3@
foreign import ccall unsafe "hs_bindgen_2edeed001a44500a" hs_bindgen_2edeed001a44500a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_long3@
hs_bindgen_2edeed001a44500a :: IO (BG.FunPtr (A -> BG.CULong -> IO ()))
hs_bindgen_2edeed001a44500a =
  fmap BG.fromFFIType hs_bindgen_2edeed001a44500a_base

{-# NOINLINE args_long3 #-}
{-| __C declaration:__ @args_long3@

    __defined at:__ @macros\/reparse.h 31:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long3 :: BG.FunPtr (A -> BG.CULong -> IO ())
args_long3 =
  BG.unsafePerformIO hs_bindgen_2edeed001a44500a

-- __unique:__ @test_macrosreparse Example get_args_float@
foreign import ccall unsafe "hs_bindgen_4e60f10b8262726b" hs_bindgen_4e60f10b8262726b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_float@
hs_bindgen_4e60f10b8262726b :: IO (BG.FunPtr (A -> BG.CFloat -> IO ()))
hs_bindgen_4e60f10b8262726b =
  fmap BG.fromFFIType hs_bindgen_4e60f10b8262726b_base

{-# NOINLINE args_float #-}
{-| __C declaration:__ @args_float@

    __defined at:__ @macros\/reparse.h 33:6@

    __exported by:__ @macros\/reparse.h@
-}
args_float :: BG.FunPtr (A -> BG.CFloat -> IO ())
args_float =
  BG.unsafePerformIO hs_bindgen_4e60f10b8262726b

-- __unique:__ @test_macrosreparse Example get_args_double@
foreign import ccall unsafe "hs_bindgen_1aaac06ad29a6778" hs_bindgen_1aaac06ad29a6778_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_double@
hs_bindgen_1aaac06ad29a6778 :: IO (BG.FunPtr (A -> BG.CDouble -> IO ()))
hs_bindgen_1aaac06ad29a6778 =
  fmap BG.fromFFIType hs_bindgen_1aaac06ad29a6778_base

{-# NOINLINE args_double #-}
{-| __C declaration:__ @args_double@

    __defined at:__ @macros\/reparse.h 34:6@

    __exported by:__ @macros\/reparse.h@
-}
args_double :: BG.FunPtr (A -> BG.CDouble -> IO ())
args_double =
  BG.unsafePerformIO hs_bindgen_1aaac06ad29a6778

-- __unique:__ @test_macrosreparse Example get_args_bool1@
foreign import ccall unsafe "hs_bindgen_c2b8c71e7ccb86e5" hs_bindgen_c2b8c71e7ccb86e5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_bool1@
hs_bindgen_c2b8c71e7ccb86e5 :: IO (BG.FunPtr (A -> BG.CBool -> IO ()))
hs_bindgen_c2b8c71e7ccb86e5 =
  fmap BG.fromFFIType hs_bindgen_c2b8c71e7ccb86e5_base

{-# NOINLINE args_bool1 #-}
{-| __C declaration:__ @args_bool1@

    __defined at:__ @macros\/reparse.h 35:6@

    __exported by:__ @macros\/reparse.h@
-}
args_bool1 :: BG.FunPtr (A -> BG.CBool -> IO ())
args_bool1 =
  BG.unsafePerformIO hs_bindgen_c2b8c71e7ccb86e5

-- __unique:__ @test_macrosreparse Example get_args_struct@
foreign import ccall unsafe "hs_bindgen_388fb8d4b6729e1c" hs_bindgen_388fb8d4b6729e1c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_struct@
hs_bindgen_388fb8d4b6729e1c :: IO (BG.FunPtr (A -> Some_struct -> IO ()))
hs_bindgen_388fb8d4b6729e1c =
  fmap BG.fromFFIType hs_bindgen_388fb8d4b6729e1c_base

{-# NOINLINE args_struct #-}
{-| __C declaration:__ @args_struct@

    __defined at:__ @macros\/reparse.h 37:6@

    __exported by:__ @macros\/reparse.h@
-}
args_struct :: BG.FunPtr (A -> Some_struct -> IO ())
args_struct =
  BG.unsafePerformIO hs_bindgen_388fb8d4b6729e1c

-- __unique:__ @test_macrosreparse Example get_args_union@
foreign import ccall unsafe "hs_bindgen_c7504cb02883c717" hs_bindgen_c7504cb02883c717_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_union@
hs_bindgen_c7504cb02883c717 :: IO (BG.FunPtr (A -> Some_union -> IO ()))
hs_bindgen_c7504cb02883c717 =
  fmap BG.fromFFIType hs_bindgen_c7504cb02883c717_base

{-# NOINLINE args_union #-}
{-| __C declaration:__ @args_union@

    __defined at:__ @macros\/reparse.h 38:6@

    __exported by:__ @macros\/reparse.h@
-}
args_union :: BG.FunPtr (A -> Some_union -> IO ())
args_union =
  BG.unsafePerformIO hs_bindgen_c7504cb02883c717

-- __unique:__ @test_macrosreparse Example get_args_enum@
foreign import ccall unsafe "hs_bindgen_f119881032357e11" hs_bindgen_f119881032357e11_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_enum@
hs_bindgen_f119881032357e11 :: IO (BG.FunPtr (A -> Some_enum -> IO ()))
hs_bindgen_f119881032357e11 =
  fmap BG.fromFFIType hs_bindgen_f119881032357e11_base

{-# NOINLINE args_enum #-}
{-| __C declaration:__ @args_enum@

    __defined at:__ @macros\/reparse.h 39:6@

    __exported by:__ @macros\/reparse.h@
-}
args_enum :: BG.FunPtr (A -> Some_enum -> IO ())
args_enum =
  BG.unsafePerformIO hs_bindgen_f119881032357e11

-- __unique:__ @test_macrosreparse Example get_args_pointer1@
foreign import ccall unsafe "hs_bindgen_2b1b041c9404e7b0" hs_bindgen_2b1b041c9404e7b0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_pointer1@
hs_bindgen_2b1b041c9404e7b0 :: IO (BG.FunPtr (A -> BG.Ptr BG.CInt -> IO ()))
hs_bindgen_2b1b041c9404e7b0 =
  fmap BG.fromFFIType hs_bindgen_2b1b041c9404e7b0_base

{-# NOINLINE args_pointer1 #-}
{-| __C declaration:__ @args_pointer1@

    __defined at:__ @macros\/reparse.h 41:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer1 :: BG.FunPtr (A -> BG.Ptr BG.CInt -> IO ())
args_pointer1 =
  BG.unsafePerformIO hs_bindgen_2b1b041c9404e7b0

-- __unique:__ @test_macrosreparse Example get_args_pointer2@
foreign import ccall unsafe "hs_bindgen_161e127836aebfdc" hs_bindgen_161e127836aebfdc_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_pointer2@
hs_bindgen_161e127836aebfdc :: IO (BG.FunPtr (A -> BG.Ptr (BG.Ptr BG.CInt) -> IO ()))
hs_bindgen_161e127836aebfdc =
  fmap BG.fromFFIType hs_bindgen_161e127836aebfdc_base

{-# NOINLINE args_pointer2 #-}
{-| __C declaration:__ @args_pointer2@

    __defined at:__ @macros\/reparse.h 42:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer2 :: BG.FunPtr (A -> BG.Ptr (BG.Ptr BG.CInt) -> IO ())
args_pointer2 =
  BG.unsafePerformIO hs_bindgen_161e127836aebfdc

-- __unique:__ @test_macrosreparse Example get_args_pointer3@
foreign import ccall unsafe "hs_bindgen_141facd965271dba" hs_bindgen_141facd965271dba_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_pointer3@
hs_bindgen_141facd965271dba :: IO (BG.FunPtr (A -> BG.Ptr BG.Void -> IO ()))
hs_bindgen_141facd965271dba =
  fmap BG.fromFFIType hs_bindgen_141facd965271dba_base

{-# NOINLINE args_pointer3 #-}
{-| __C declaration:__ @args_pointer3@

    __defined at:__ @macros\/reparse.h 43:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer3 :: BG.FunPtr (A -> BG.Ptr BG.Void -> IO ())
args_pointer3 =
  BG.unsafePerformIO hs_bindgen_141facd965271dba

-- __unique:__ @test_macrosreparse Example get_ret_A@
foreign import ccall unsafe "hs_bindgen_975f44422deea845" hs_bindgen_975f44422deea845_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_A@
hs_bindgen_975f44422deea845 :: IO (BG.FunPtr (IO A))
hs_bindgen_975f44422deea845 =
  fmap BG.fromFFIType hs_bindgen_975f44422deea845_base

{-# NOINLINE ret_A #-}
{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h 47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A :: BG.FunPtr (IO A)
ret_A =
  BG.unsafePerformIO hs_bindgen_975f44422deea845

-- __unique:__ @test_macrosreparse Example get_ret_char1@
foreign import ccall unsafe "hs_bindgen_4b2e3ebe2eba0853" hs_bindgen_4b2e3ebe2eba0853_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_char1@
hs_bindgen_4b2e3ebe2eba0853 :: IO (BG.FunPtr (A -> IO BG.CChar))
hs_bindgen_4b2e3ebe2eba0853 =
  fmap BG.fromFFIType hs_bindgen_4b2e3ebe2eba0853_base

{-# NOINLINE ret_char1 #-}
{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h 49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1 :: BG.FunPtr (A -> IO BG.CChar)
ret_char1 =
  BG.unsafePerformIO hs_bindgen_4b2e3ebe2eba0853

-- __unique:__ @test_macrosreparse Example get_ret_char2@
foreign import ccall unsafe "hs_bindgen_b65af076b18439b1" hs_bindgen_b65af076b18439b1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_char2@
hs_bindgen_b65af076b18439b1 :: IO (BG.FunPtr (A -> IO BG.CSChar))
hs_bindgen_b65af076b18439b1 =
  fmap BG.fromFFIType hs_bindgen_b65af076b18439b1_base

{-# NOINLINE ret_char2 #-}
{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h 50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2 :: BG.FunPtr (A -> IO BG.CSChar)
ret_char2 =
  BG.unsafePerformIO hs_bindgen_b65af076b18439b1

-- __unique:__ @test_macrosreparse Example get_ret_char3@
foreign import ccall unsafe "hs_bindgen_716f16d7138a5f7a" hs_bindgen_716f16d7138a5f7a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_char3@
hs_bindgen_716f16d7138a5f7a :: IO (BG.FunPtr (A -> IO BG.CUChar))
hs_bindgen_716f16d7138a5f7a =
  fmap BG.fromFFIType hs_bindgen_716f16d7138a5f7a_base

{-# NOINLINE ret_char3 #-}
{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h 51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3 :: BG.FunPtr (A -> IO BG.CUChar)
ret_char3 =
  BG.unsafePerformIO hs_bindgen_716f16d7138a5f7a

-- __unique:__ @test_macrosreparse Example get_ret_short1@
foreign import ccall unsafe "hs_bindgen_374162a0d7f996c6" hs_bindgen_374162a0d7f996c6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_short1@
hs_bindgen_374162a0d7f996c6 :: IO (BG.FunPtr (A -> IO BG.CShort))
hs_bindgen_374162a0d7f996c6 =
  fmap BG.fromFFIType hs_bindgen_374162a0d7f996c6_base

{-# NOINLINE ret_short1 #-}
{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h 53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1 :: BG.FunPtr (A -> IO BG.CShort)
ret_short1 =
  BG.unsafePerformIO hs_bindgen_374162a0d7f996c6

-- __unique:__ @test_macrosreparse Example get_ret_short2@
foreign import ccall unsafe "hs_bindgen_f958e4856271e9ed" hs_bindgen_f958e4856271e9ed_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_short2@
hs_bindgen_f958e4856271e9ed :: IO (BG.FunPtr (A -> IO BG.CShort))
hs_bindgen_f958e4856271e9ed =
  fmap BG.fromFFIType hs_bindgen_f958e4856271e9ed_base

{-# NOINLINE ret_short2 #-}
{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h 54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2 :: BG.FunPtr (A -> IO BG.CShort)
ret_short2 =
  BG.unsafePerformIO hs_bindgen_f958e4856271e9ed

-- __unique:__ @test_macrosreparse Example get_ret_short3@
foreign import ccall unsafe "hs_bindgen_6fa7ea9fd94d9626" hs_bindgen_6fa7ea9fd94d9626_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_short3@
hs_bindgen_6fa7ea9fd94d9626 :: IO (BG.FunPtr (A -> IO BG.CUShort))
hs_bindgen_6fa7ea9fd94d9626 =
  fmap BG.fromFFIType hs_bindgen_6fa7ea9fd94d9626_base

{-# NOINLINE ret_short3 #-}
{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h 55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3 :: BG.FunPtr (A -> IO BG.CUShort)
ret_short3 =
  BG.unsafePerformIO hs_bindgen_6fa7ea9fd94d9626

-- __unique:__ @test_macrosreparse Example get_ret_int1@
foreign import ccall unsafe "hs_bindgen_c1d9de4abd24aa21" hs_bindgen_c1d9de4abd24aa21_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_int1@
hs_bindgen_c1d9de4abd24aa21 :: IO (BG.FunPtr (A -> IO BG.CInt))
hs_bindgen_c1d9de4abd24aa21 =
  fmap BG.fromFFIType hs_bindgen_c1d9de4abd24aa21_base

{-# NOINLINE ret_int1 #-}
{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h 57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1 :: BG.FunPtr (A -> IO BG.CInt)
ret_int1 =
  BG.unsafePerformIO hs_bindgen_c1d9de4abd24aa21

-- __unique:__ @test_macrosreparse Example get_ret_int2@
foreign import ccall unsafe "hs_bindgen_3d7fe4b9bbcb3ebb" hs_bindgen_3d7fe4b9bbcb3ebb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_int2@
hs_bindgen_3d7fe4b9bbcb3ebb :: IO (BG.FunPtr (A -> IO BG.CInt))
hs_bindgen_3d7fe4b9bbcb3ebb =
  fmap BG.fromFFIType hs_bindgen_3d7fe4b9bbcb3ebb_base

{-# NOINLINE ret_int2 #-}
{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h 58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2 :: BG.FunPtr (A -> IO BG.CInt)
ret_int2 =
  BG.unsafePerformIO hs_bindgen_3d7fe4b9bbcb3ebb

-- __unique:__ @test_macrosreparse Example get_ret_int3@
foreign import ccall unsafe "hs_bindgen_ebe1a790179e5a4e" hs_bindgen_ebe1a790179e5a4e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_int3@
hs_bindgen_ebe1a790179e5a4e :: IO (BG.FunPtr (A -> IO BG.CUInt))
hs_bindgen_ebe1a790179e5a4e =
  fmap BG.fromFFIType hs_bindgen_ebe1a790179e5a4e_base

{-# NOINLINE ret_int3 #-}
{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h 59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3 :: BG.FunPtr (A -> IO BG.CUInt)
ret_int3 =
  BG.unsafePerformIO hs_bindgen_ebe1a790179e5a4e

-- __unique:__ @test_macrosreparse Example get_ret_long1@
foreign import ccall unsafe "hs_bindgen_03df01fe9976a201" hs_bindgen_03df01fe9976a201_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_long1@
hs_bindgen_03df01fe9976a201 :: IO (BG.FunPtr (A -> IO BG.CLong))
hs_bindgen_03df01fe9976a201 =
  fmap BG.fromFFIType hs_bindgen_03df01fe9976a201_base

{-# NOINLINE ret_long1 #-}
{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h 61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1 :: BG.FunPtr (A -> IO BG.CLong)
ret_long1 =
  BG.unsafePerformIO hs_bindgen_03df01fe9976a201

-- __unique:__ @test_macrosreparse Example get_ret_long2@
foreign import ccall unsafe "hs_bindgen_785ad7b473f39e72" hs_bindgen_785ad7b473f39e72_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_long2@
hs_bindgen_785ad7b473f39e72 :: IO (BG.FunPtr (A -> IO BG.CLong))
hs_bindgen_785ad7b473f39e72 =
  fmap BG.fromFFIType hs_bindgen_785ad7b473f39e72_base

{-# NOINLINE ret_long2 #-}
{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h 62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2 :: BG.FunPtr (A -> IO BG.CLong)
ret_long2 =
  BG.unsafePerformIO hs_bindgen_785ad7b473f39e72

-- __unique:__ @test_macrosreparse Example get_ret_long3@
foreign import ccall unsafe "hs_bindgen_d7a4dc339e6a2928" hs_bindgen_d7a4dc339e6a2928_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_long3@
hs_bindgen_d7a4dc339e6a2928 :: IO (BG.FunPtr (A -> IO BG.CULong))
hs_bindgen_d7a4dc339e6a2928 =
  fmap BG.fromFFIType hs_bindgen_d7a4dc339e6a2928_base

{-# NOINLINE ret_long3 #-}
{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h 63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3 :: BG.FunPtr (A -> IO BG.CULong)
ret_long3 =
  BG.unsafePerformIO hs_bindgen_d7a4dc339e6a2928

-- __unique:__ @test_macrosreparse Example get_ret_float@
foreign import ccall unsafe "hs_bindgen_1b9b45325315c025" hs_bindgen_1b9b45325315c025_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_float@
hs_bindgen_1b9b45325315c025 :: IO (BG.FunPtr (A -> IO BG.CFloat))
hs_bindgen_1b9b45325315c025 =
  fmap BG.fromFFIType hs_bindgen_1b9b45325315c025_base

{-# NOINLINE ret_float #-}
{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h 65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float :: BG.FunPtr (A -> IO BG.CFloat)
ret_float =
  BG.unsafePerformIO hs_bindgen_1b9b45325315c025

-- __unique:__ @test_macrosreparse Example get_ret_double@
foreign import ccall unsafe "hs_bindgen_605dc93b0201eeb9" hs_bindgen_605dc93b0201eeb9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_double@
hs_bindgen_605dc93b0201eeb9 :: IO (BG.FunPtr (A -> IO BG.CDouble))
hs_bindgen_605dc93b0201eeb9 =
  fmap BG.fromFFIType hs_bindgen_605dc93b0201eeb9_base

{-# NOINLINE ret_double #-}
{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h 66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double :: BG.FunPtr (A -> IO BG.CDouble)
ret_double =
  BG.unsafePerformIO hs_bindgen_605dc93b0201eeb9

-- __unique:__ @test_macrosreparse Example get_ret_bool1@
foreign import ccall unsafe "hs_bindgen_47c345c53b8182c5" hs_bindgen_47c345c53b8182c5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_bool1@
hs_bindgen_47c345c53b8182c5 :: IO (BG.FunPtr (A -> IO BG.CBool))
hs_bindgen_47c345c53b8182c5 =
  fmap BG.fromFFIType hs_bindgen_47c345c53b8182c5_base

{-# NOINLINE ret_bool1 #-}
{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h 67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1 :: BG.FunPtr (A -> IO BG.CBool)
ret_bool1 =
  BG.unsafePerformIO hs_bindgen_47c345c53b8182c5

-- __unique:__ @test_macrosreparse Example get_ret_struct@
foreign import ccall unsafe "hs_bindgen_429842617174d1bb" hs_bindgen_429842617174d1bb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_struct@
hs_bindgen_429842617174d1bb :: IO (BG.FunPtr (A -> IO Some_struct))
hs_bindgen_429842617174d1bb =
  fmap BG.fromFFIType hs_bindgen_429842617174d1bb_base

{-# NOINLINE ret_struct #-}
{-| __C declaration:__ @ret_struct@

    __defined at:__ @macros\/reparse.h 69:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_struct :: BG.FunPtr (A -> IO Some_struct)
ret_struct =
  BG.unsafePerformIO hs_bindgen_429842617174d1bb

-- __unique:__ @test_macrosreparse Example get_ret_union@
foreign import ccall unsafe "hs_bindgen_8ab286c7b08e02f9" hs_bindgen_8ab286c7b08e02f9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_union@
hs_bindgen_8ab286c7b08e02f9 :: IO (BG.FunPtr (A -> IO Some_union))
hs_bindgen_8ab286c7b08e02f9 =
  fmap BG.fromFFIType hs_bindgen_8ab286c7b08e02f9_base

{-# NOINLINE ret_union #-}
{-| __C declaration:__ @ret_union@

    __defined at:__ @macros\/reparse.h 70:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_union :: BG.FunPtr (A -> IO Some_union)
ret_union =
  BG.unsafePerformIO hs_bindgen_8ab286c7b08e02f9

-- __unique:__ @test_macrosreparse Example get_ret_enum@
foreign import ccall unsafe "hs_bindgen_c8158d45c5fc6ada" hs_bindgen_c8158d45c5fc6ada_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_enum@
hs_bindgen_c8158d45c5fc6ada :: IO (BG.FunPtr (A -> IO Some_enum))
hs_bindgen_c8158d45c5fc6ada =
  fmap BG.fromFFIType hs_bindgen_c8158d45c5fc6ada_base

{-# NOINLINE ret_enum #-}
{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h 71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum :: BG.FunPtr (A -> IO Some_enum)
ret_enum =
  BG.unsafePerformIO hs_bindgen_c8158d45c5fc6ada

-- __unique:__ @test_macrosreparse Example get_ret_pointer1@
foreign import ccall unsafe "hs_bindgen_bc4c8c7932ff4a5e" hs_bindgen_bc4c8c7932ff4a5e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_pointer1@
hs_bindgen_bc4c8c7932ff4a5e :: IO (BG.FunPtr (A -> IO (BG.Ptr BG.CInt)))
hs_bindgen_bc4c8c7932ff4a5e =
  fmap BG.fromFFIType hs_bindgen_bc4c8c7932ff4a5e_base

{-# NOINLINE ret_pointer1 #-}
{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h 73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1 :: BG.FunPtr (A -> IO (BG.Ptr BG.CInt))
ret_pointer1 =
  BG.unsafePerformIO hs_bindgen_bc4c8c7932ff4a5e

-- __unique:__ @test_macrosreparse Example get_ret_pointer2@
foreign import ccall unsafe "hs_bindgen_5129055a6bb8be29" hs_bindgen_5129055a6bb8be29_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_pointer2@
hs_bindgen_5129055a6bb8be29 :: IO (BG.FunPtr (A -> IO (BG.Ptr (BG.Ptr BG.CInt))))
hs_bindgen_5129055a6bb8be29 =
  fmap BG.fromFFIType hs_bindgen_5129055a6bb8be29_base

{-# NOINLINE ret_pointer2 #-}
{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h 74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2 :: BG.FunPtr (A -> IO (BG.Ptr (BG.Ptr BG.CInt)))
ret_pointer2 =
  BG.unsafePerformIO hs_bindgen_5129055a6bb8be29

-- __unique:__ @test_macrosreparse Example get_ret_pointer3@
foreign import ccall unsafe "hs_bindgen_557d79219f5ea78e" hs_bindgen_557d79219f5ea78e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_pointer3@
hs_bindgen_557d79219f5ea78e :: IO (BG.FunPtr (A -> IO (BG.Ptr BG.Void)))
hs_bindgen_557d79219f5ea78e =
  fmap BG.fromFFIType hs_bindgen_557d79219f5ea78e_base

{-# NOINLINE ret_pointer3 #-}
{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h 75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3 :: BG.FunPtr (A -> IO (BG.Ptr BG.Void))
ret_pointer3 =
  BG.unsafePerformIO hs_bindgen_557d79219f5ea78e

-- __unique:__ @test_macrosreparse Example get_body1@
foreign import ccall unsafe "hs_bindgen_33be18adba0502b6" hs_bindgen_33be18adba0502b6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_body1@
hs_bindgen_33be18adba0502b6 :: IO (BG.FunPtr (A -> IO BG.CInt))
hs_bindgen_33be18adba0502b6 =
  fmap BG.fromFFIType hs_bindgen_33be18adba0502b6_base

{-# NOINLINE body1 #-}
{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h 79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1 :: BG.FunPtr (A -> IO BG.CInt)
body1 =
  BG.unsafePerformIO hs_bindgen_33be18adba0502b6

-- __unique:__ @test_macrosreparse Example get_body2@
foreign import ccall unsafe "hs_bindgen_246e43cadb053f42" hs_bindgen_246e43cadb053f42_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_body2@
hs_bindgen_246e43cadb053f42 :: IO (BG.FunPtr (IO A))
hs_bindgen_246e43cadb053f42 =
  fmap BG.fromFFIType hs_bindgen_246e43cadb053f42_base

{-# NOINLINE body2 #-}
{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h 80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2 :: BG.FunPtr (IO A)
body2 =
  BG.unsafePerformIO hs_bindgen_246e43cadb053f42

-- __unique:__ @test_macrosreparse Example get_args_complex_float@
foreign import ccall unsafe "hs_bindgen_b0616e97db902818" hs_bindgen_b0616e97db902818_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_complex_float@
hs_bindgen_b0616e97db902818 :: IO (BG.FunPtr (A -> BG.Complex BG.CFloat -> IO ()))
hs_bindgen_b0616e97db902818 =
  fmap BG.fromFFIType hs_bindgen_b0616e97db902818_base

{-# NOINLINE args_complex_float #-}
{-| __C declaration:__ @args_complex_float@

    __defined at:__ @macros\/reparse.h 84:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_float :: BG.FunPtr (A -> BG.Complex BG.CFloat -> IO ())
args_complex_float =
  BG.unsafePerformIO hs_bindgen_b0616e97db902818

-- __unique:__ @test_macrosreparse Example get_args_complex_double@
foreign import ccall unsafe "hs_bindgen_117dbdac5517f462" hs_bindgen_117dbdac5517f462_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_args_complex_double@
hs_bindgen_117dbdac5517f462 :: IO (BG.FunPtr (A -> BG.Complex BG.CDouble -> IO ()))
hs_bindgen_117dbdac5517f462 =
  fmap BG.fromFFIType hs_bindgen_117dbdac5517f462_base

{-# NOINLINE args_complex_double #-}
{-| __C declaration:__ @args_complex_double@

    __defined at:__ @macros\/reparse.h 85:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_double :: BG.FunPtr (A -> BG.Complex BG.CDouble -> IO ())
args_complex_double =
  BG.unsafePerformIO hs_bindgen_117dbdac5517f462

-- __unique:__ @test_macrosreparse Example get_ret_complex_float@
foreign import ccall unsafe "hs_bindgen_bbcfef4bf6d547f9" hs_bindgen_bbcfef4bf6d547f9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_complex_float@
hs_bindgen_bbcfef4bf6d547f9 :: IO (BG.FunPtr (A -> IO (BG.Complex BG.CFloat)))
hs_bindgen_bbcfef4bf6d547f9 =
  fmap BG.fromFFIType hs_bindgen_bbcfef4bf6d547f9_base

{-# NOINLINE ret_complex_float #-}
{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @macros\/reparse.h 86:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_float :: BG.FunPtr (A -> IO (BG.Complex BG.CFloat))
ret_complex_float =
  BG.unsafePerformIO hs_bindgen_bbcfef4bf6d547f9

-- __unique:__ @test_macrosreparse Example get_ret_complex_double@
foreign import ccall unsafe "hs_bindgen_e95c8ae3a376d5b3" hs_bindgen_e95c8ae3a376d5b3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_ret_complex_double@
hs_bindgen_e95c8ae3a376d5b3 :: IO (BG.FunPtr (A -> IO (BG.Complex BG.CDouble)))
hs_bindgen_e95c8ae3a376d5b3 =
  fmap BG.fromFFIType hs_bindgen_e95c8ae3a376d5b3_base

{-# NOINLINE ret_complex_double #-}
{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @macros\/reparse.h 87:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_double :: BG.FunPtr (A -> IO (BG.Complex BG.CDouble))
ret_complex_double =
  BG.unsafePerformIO hs_bindgen_e95c8ae3a376d5b3

-- __unique:__ @test_macrosreparse Example get_bespoke_args1@
foreign import ccall unsafe "hs_bindgen_1e61c012a7118651" hs_bindgen_1e61c012a7118651_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_bespoke_args1@
hs_bindgen_1e61c012a7118651 :: IO (BG.FunPtr (A -> BG.CBool -> IO ()))
hs_bindgen_1e61c012a7118651 =
  fmap BG.fromFFIType hs_bindgen_1e61c012a7118651_base

{-# NOINLINE bespoke_args1 #-}
{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @macros\/reparse.h 94:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args1 :: BG.FunPtr (A -> BG.CBool -> IO ())
bespoke_args1 =
  BG.unsafePerformIO hs_bindgen_1e61c012a7118651

-- __unique:__ @test_macrosreparse Example get_bespoke_args2@
foreign import ccall unsafe "hs_bindgen_c7c83f1a0ced2aae" hs_bindgen_c7c83f1a0ced2aae_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_bespoke_args2@
hs_bindgen_c7c83f1a0ced2aae :: IO (BG.FunPtr (A -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_c7c83f1a0ced2aae =
  fmap BG.fromFFIType hs_bindgen_c7c83f1a0ced2aae_base

{-# NOINLINE bespoke_args2 #-}
{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @macros\/reparse.h 95:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args2 :: BG.FunPtr (A -> HsBindgen.Runtime.LibC.CSize -> IO ())
bespoke_args2 =
  BG.unsafePerformIO hs_bindgen_c7c83f1a0ced2aae

-- __unique:__ @test_macrosreparse Example get_bespoke_ret1@
foreign import ccall unsafe "hs_bindgen_ad569701e220dec4" hs_bindgen_ad569701e220dec4_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_bespoke_ret1@
hs_bindgen_ad569701e220dec4 :: IO (BG.FunPtr (A -> IO BG.CBool))
hs_bindgen_ad569701e220dec4 =
  fmap BG.fromFFIType hs_bindgen_ad569701e220dec4_base

{-# NOINLINE bespoke_ret1 #-}
{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h 97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1 :: BG.FunPtr (A -> IO BG.CBool)
bespoke_ret1 =
  BG.unsafePerformIO hs_bindgen_ad569701e220dec4

-- __unique:__ @test_macrosreparse Example get_bespoke_ret2@
foreign import ccall unsafe "hs_bindgen_94ed8d77dbd6264b" hs_bindgen_94ed8d77dbd6264b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_bespoke_ret2@
hs_bindgen_94ed8d77dbd6264b :: IO (BG.FunPtr (A -> IO HsBindgen.Runtime.LibC.CSize))
hs_bindgen_94ed8d77dbd6264b =
  fmap BG.fromFFIType hs_bindgen_94ed8d77dbd6264b_base

{-# NOINLINE bespoke_ret2 #-}
{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h 98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2 :: BG.FunPtr (A -> IO HsBindgen.Runtime.LibC.CSize)
bespoke_ret2 =
  BG.unsafePerformIO hs_bindgen_94ed8d77dbd6264b

-- __unique:__ @test_macrosreparse Example get_arr_args1@
foreign import ccall unsafe "hs_bindgen_9be695c779650a36" hs_bindgen_9be695c779650a36_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_arr_args1@
hs_bindgen_9be695c779650a36 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray A)) -> IO ()))
hs_bindgen_9be695c779650a36 =
  fmap BG.fromFFIType hs_bindgen_9be695c779650a36_base

{-# NOINLINE arr_args1 #-}
{-| Arrays

    __C declaration:__ @arr_args1@

    __defined at:__ @macros\/reparse.h 104:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args1 :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray A)) -> IO ())
arr_args1 =
  BG.unsafePerformIO hs_bindgen_9be695c779650a36

-- __unique:__ @test_macrosreparse Example get_arr_args2@
foreign import ccall unsafe "hs_bindgen_40f53b981ef810db" hs_bindgen_40f53b981ef810db_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_arr_args2@
hs_bindgen_40f53b981ef810db :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr A))) -> IO ()))
hs_bindgen_40f53b981ef810db =
  fmap BG.fromFFIType hs_bindgen_40f53b981ef810db_base

{-# NOINLINE arr_args2 #-}
{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h 105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2 :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr A))) -> IO ())
arr_args2 =
  BG.unsafePerformIO hs_bindgen_40f53b981ef810db

-- __unique:__ @test_macrosreparse Example get_arr_args3@
foreign import ccall unsafe "hs_bindgen_28eaac40215082de" hs_bindgen_28eaac40215082de_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_arr_args3@
hs_bindgen_28eaac40215082de :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 A)) -> IO ()))
hs_bindgen_28eaac40215082de =
  fmap BG.fromFFIType hs_bindgen_28eaac40215082de_base

{-# NOINLINE arr_args3 #-}
{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h 106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3 :: BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 A)) -> IO ())
arr_args3 =
  BG.unsafePerformIO hs_bindgen_28eaac40215082de

-- __unique:__ @test_macrosreparse Example get_arr_args4@
foreign import ccall unsafe "hs_bindgen_b336fad742b74b3d" hs_bindgen_b336fad742b74b3d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_arr_args4@
hs_bindgen_b336fad742b74b3d :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr A))) -> IO ()))
hs_bindgen_b336fad742b74b3d =
  fmap BG.fromFFIType hs_bindgen_b336fad742b74b3d_base

{-# NOINLINE arr_args4 #-}
{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h 107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4 :: BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr A))) -> IO ())
arr_args4 =
  BG.unsafePerformIO hs_bindgen_b336fad742b74b3d

-- __unique:__ @test_macrosreparse Example get_funptr_args1@
foreign import ccall unsafe "hs_bindgen_73c83b6b1eaec7d1" hs_bindgen_73c83b6b1eaec7d1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_funptr_args1@
hs_bindgen_73c83b6b1eaec7d1 :: IO (BG.FunPtr (A -> BG.FunPtr (IO ()) -> IO ()))
hs_bindgen_73c83b6b1eaec7d1 =
  fmap BG.fromFFIType hs_bindgen_73c83b6b1eaec7d1_base

{-# NOINLINE funptr_args1 #-}
{-| Function pointers

    __C declaration:__ @funptr_args1@

    __defined at:__ @macros\/reparse.h 126:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args1 :: BG.FunPtr (A -> BG.FunPtr (IO ()) -> IO ())
funptr_args1 =
  BG.unsafePerformIO hs_bindgen_73c83b6b1eaec7d1

-- __unique:__ @test_macrosreparse Example get_funptr_args2@
foreign import ccall unsafe "hs_bindgen_613cdb3e8987e37d" hs_bindgen_613cdb3e8987e37d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_funptr_args2@
hs_bindgen_613cdb3e8987e37d :: IO (BG.FunPtr (A -> BG.FunPtr (IO BG.CInt) -> IO ()))
hs_bindgen_613cdb3e8987e37d =
  fmap BG.fromFFIType hs_bindgen_613cdb3e8987e37d_base

{-# NOINLINE funptr_args2 #-}
{-| __C declaration:__ @funptr_args2@

    __defined at:__ @macros\/reparse.h 127:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args2 :: BG.FunPtr (A -> BG.FunPtr (IO BG.CInt) -> IO ())
funptr_args2 =
  BG.unsafePerformIO hs_bindgen_613cdb3e8987e37d

-- __unique:__ @test_macrosreparse Example get_funptr_args3@
foreign import ccall unsafe "hs_bindgen_21dd820dfeaa80cf" hs_bindgen_21dd820dfeaa80cf_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_funptr_args3@
hs_bindgen_21dd820dfeaa80cf :: IO (BG.FunPtr (A -> BG.FunPtr (BG.CInt -> IO ()) -> IO ()))
hs_bindgen_21dd820dfeaa80cf =
  fmap BG.fromFFIType hs_bindgen_21dd820dfeaa80cf_base

{-# NOINLINE funptr_args3 #-}
{-| __C declaration:__ @funptr_args3@

    __defined at:__ @macros\/reparse.h 128:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args3 :: BG.FunPtr (A -> BG.FunPtr (BG.CInt -> IO ()) -> IO ())
funptr_args3 =
  BG.unsafePerformIO hs_bindgen_21dd820dfeaa80cf

-- __unique:__ @test_macrosreparse Example get_funptr_args4@
foreign import ccall unsafe "hs_bindgen_0a97bbfec852a72e" hs_bindgen_0a97bbfec852a72e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_funptr_args4@
hs_bindgen_0a97bbfec852a72e :: IO (BG.FunPtr (A -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar) -> IO ()))
hs_bindgen_0a97bbfec852a72e =
  fmap BG.fromFFIType hs_bindgen_0a97bbfec852a72e_base

{-# NOINLINE funptr_args4 #-}
{-| __C declaration:__ @funptr_args4@

    __defined at:__ @macros\/reparse.h 129:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args4 :: BG.FunPtr (A -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar) -> IO ())
funptr_args4 =
  BG.unsafePerformIO hs_bindgen_0a97bbfec852a72e

-- __unique:__ @test_macrosreparse Example get_funptr_args5@
foreign import ccall unsafe "hs_bindgen_96c5fdf3502111c6" hs_bindgen_96c5fdf3502111c6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_funptr_args5@
hs_bindgen_96c5fdf3502111c6 :: IO (BG.FunPtr (A -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)) -> IO ()))
hs_bindgen_96c5fdf3502111c6 =
  fmap BG.fromFFIType hs_bindgen_96c5fdf3502111c6_base

{-# NOINLINE funptr_args5 #-}
{-| __C declaration:__ @funptr_args5@

    __defined at:__ @macros\/reparse.h 130:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args5 :: BG.FunPtr (A -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)) -> IO ())
funptr_args5 =
  BG.unsafePerformIO hs_bindgen_96c5fdf3502111c6

-- __unique:__ @test_macrosreparse Example get_comments1@
foreign import ccall unsafe "hs_bindgen_1c6e5af6fab8eb0c" hs_bindgen_1c6e5af6fab8eb0c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_comments1@
hs_bindgen_1c6e5af6fab8eb0c :: IO (BG.FunPtr (A -> IO ()))
hs_bindgen_1c6e5af6fab8eb0c =
  fmap BG.fromFFIType hs_bindgen_1c6e5af6fab8eb0c_base

{-# NOINLINE comments1 #-}
{-| Comments in awkward places

    (Prior to language-c we failed to parse there.)

    __C declaration:__ @comments1@

    __defined at:__ @macros\/reparse.h 144:25@

    __exported by:__ @macros\/reparse.h@
-}
comments1 :: BG.FunPtr (A -> IO ())
comments1 =
  BG.unsafePerformIO hs_bindgen_1c6e5af6fab8eb0c

-- __unique:__ @test_macrosreparse Example get_const_prim_before1@
foreign import ccall unsafe "hs_bindgen_0c66b303d14abd34" hs_bindgen_0c66b303d14abd34_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_prim_before1@
hs_bindgen_0c66b303d14abd34 :: IO (BG.FunPtr (A -> BG.CChar -> IO ()))
hs_bindgen_0c66b303d14abd34 =
  fmap BG.fromFFIType hs_bindgen_0c66b303d14abd34_base

{-# NOINLINE const_prim_before1 #-}
{-| @const@ qualifier

    NOTE: These were not parsed correctly prior to the switch to language-c.

    __C declaration:__ @const_prim_before1@

    __defined at:__ @macros\/reparse.h 177:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before1 :: BG.FunPtr (A -> BG.CChar -> IO ())
const_prim_before1 =
  BG.unsafePerformIO hs_bindgen_0c66b303d14abd34

-- __unique:__ @test_macrosreparse Example get_const_prim_before2@
foreign import ccall unsafe "hs_bindgen_1b1e518db6a12c03" hs_bindgen_1b1e518db6a12c03_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_prim_before2@
hs_bindgen_1b1e518db6a12c03 :: IO (BG.FunPtr (A -> BG.CSChar -> IO ()))
hs_bindgen_1b1e518db6a12c03 =
  fmap BG.fromFFIType hs_bindgen_1b1e518db6a12c03_base

{-# NOINLINE const_prim_before2 #-}
{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @macros\/reparse.h 178:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before2 :: BG.FunPtr (A -> BG.CSChar -> IO ())
const_prim_before2 =
  BG.unsafePerformIO hs_bindgen_1b1e518db6a12c03

-- __unique:__ @test_macrosreparse Example get_const_prim_before3@
foreign import ccall unsafe "hs_bindgen_b0d212ee3212eef3" hs_bindgen_b0d212ee3212eef3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_prim_before3@
hs_bindgen_b0d212ee3212eef3 :: IO (BG.FunPtr (A -> BG.CUChar -> IO ()))
hs_bindgen_b0d212ee3212eef3 =
  fmap BG.fromFFIType hs_bindgen_b0d212ee3212eef3_base

{-# NOINLINE const_prim_before3 #-}
{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @macros\/reparse.h 179:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before3 :: BG.FunPtr (A -> BG.CUChar -> IO ())
const_prim_before3 =
  BG.unsafePerformIO hs_bindgen_b0d212ee3212eef3

-- __unique:__ @test_macrosreparse Example get_const_prim_after1@
foreign import ccall unsafe "hs_bindgen_0a20685281625ec0" hs_bindgen_0a20685281625ec0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_prim_after1@
hs_bindgen_0a20685281625ec0 :: IO (BG.FunPtr (A -> BG.CChar -> IO ()))
hs_bindgen_0a20685281625ec0 =
  fmap BG.fromFFIType hs_bindgen_0a20685281625ec0_base

{-# NOINLINE const_prim_after1 #-}
{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @macros\/reparse.h 180:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after1 :: BG.FunPtr (A -> BG.CChar -> IO ())
const_prim_after1 =
  BG.unsafePerformIO hs_bindgen_0a20685281625ec0

-- __unique:__ @test_macrosreparse Example get_const_prim_after2@
foreign import ccall unsafe "hs_bindgen_215b9d9881cfc3bc" hs_bindgen_215b9d9881cfc3bc_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_prim_after2@
hs_bindgen_215b9d9881cfc3bc :: IO (BG.FunPtr (A -> BG.CSChar -> IO ()))
hs_bindgen_215b9d9881cfc3bc =
  fmap BG.fromFFIType hs_bindgen_215b9d9881cfc3bc_base

{-# NOINLINE const_prim_after2 #-}
{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @macros\/reparse.h 181:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after2 :: BG.FunPtr (A -> BG.CSChar -> IO ())
const_prim_after2 =
  BG.unsafePerformIO hs_bindgen_215b9d9881cfc3bc

-- __unique:__ @test_macrosreparse Example get_const_prim_after3@
foreign import ccall unsafe "hs_bindgen_3bfa0a0c9f9466f2" hs_bindgen_3bfa0a0c9f9466f2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_prim_after3@
hs_bindgen_3bfa0a0c9f9466f2 :: IO (BG.FunPtr (A -> BG.CUChar -> IO ()))
hs_bindgen_3bfa0a0c9f9466f2 =
  fmap BG.fromFFIType hs_bindgen_3bfa0a0c9f9466f2_base

{-# NOINLINE const_prim_after3 #-}
{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @macros\/reparse.h 182:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after3 :: BG.FunPtr (A -> BG.CUChar -> IO ())
const_prim_after3 =
  BG.unsafePerformIO hs_bindgen_3bfa0a0c9f9466f2

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_before1@
foreign import ccall unsafe "hs_bindgen_d80eaf9ff8c0ee3a" hs_bindgen_d80eaf9ff8c0ee3a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_before1@
hs_bindgen_d80eaf9ff8c0ee3a :: IO (BG.FunPtr (A -> BG.CFloat -> IO ()))
hs_bindgen_d80eaf9ff8c0ee3a =
  fmap BG.fromFFIType hs_bindgen_d80eaf9ff8c0ee3a_base

{-# NOINLINE const_withoutSign_before1 #-}
{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @macros\/reparse.h 186:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before1 :: BG.FunPtr (A -> BG.CFloat -> IO ())
const_withoutSign_before1 =
  BG.unsafePerformIO hs_bindgen_d80eaf9ff8c0ee3a

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_before2@
foreign import ccall unsafe "hs_bindgen_983d81879a8362e8" hs_bindgen_983d81879a8362e8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_before2@
hs_bindgen_983d81879a8362e8 :: IO (BG.FunPtr (A -> BG.CDouble -> IO ()))
hs_bindgen_983d81879a8362e8 =
  fmap BG.fromFFIType hs_bindgen_983d81879a8362e8_base

{-# NOINLINE const_withoutSign_before2 #-}
{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @macros\/reparse.h 187:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before2 :: BG.FunPtr (A -> BG.CDouble -> IO ())
const_withoutSign_before2 =
  BG.unsafePerformIO hs_bindgen_983d81879a8362e8

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_before3@
foreign import ccall unsafe "hs_bindgen_8d25a7a2211fc8da" hs_bindgen_8d25a7a2211fc8da_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_before3@
hs_bindgen_8d25a7a2211fc8da :: IO (BG.FunPtr (A -> BG.CBool -> IO ()))
hs_bindgen_8d25a7a2211fc8da =
  fmap BG.fromFFIType hs_bindgen_8d25a7a2211fc8da_base

{-# NOINLINE const_withoutSign_before3 #-}
{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @macros\/reparse.h 188:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before3 :: BG.FunPtr (A -> BG.CBool -> IO ())
const_withoutSign_before3 =
  BG.unsafePerformIO hs_bindgen_8d25a7a2211fc8da

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_before4@
foreign import ccall unsafe "hs_bindgen_29d52174b8dc62c8" hs_bindgen_29d52174b8dc62c8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_before4@
hs_bindgen_29d52174b8dc62c8 :: IO (BG.FunPtr (A -> Some_struct -> IO ()))
hs_bindgen_29d52174b8dc62c8 =
  fmap BG.fromFFIType hs_bindgen_29d52174b8dc62c8_base

{-# NOINLINE const_withoutSign_before4 #-}
{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @macros\/reparse.h 189:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before4 :: BG.FunPtr (A -> Some_struct -> IO ())
const_withoutSign_before4 =
  BG.unsafePerformIO hs_bindgen_29d52174b8dc62c8

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_before5@
foreign import ccall unsafe "hs_bindgen_fa149a3de1c7507c" hs_bindgen_fa149a3de1c7507c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_before5@
hs_bindgen_fa149a3de1c7507c :: IO (BG.FunPtr (A -> Some_union -> IO ()))
hs_bindgen_fa149a3de1c7507c =
  fmap BG.fromFFIType hs_bindgen_fa149a3de1c7507c_base

{-# NOINLINE const_withoutSign_before5 #-}
{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @macros\/reparse.h 190:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before5 :: BG.FunPtr (A -> Some_union -> IO ())
const_withoutSign_before5 =
  BG.unsafePerformIO hs_bindgen_fa149a3de1c7507c

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_before6@
foreign import ccall unsafe "hs_bindgen_ae4b21912dc3e9fc" hs_bindgen_ae4b21912dc3e9fc_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_before6@
hs_bindgen_ae4b21912dc3e9fc :: IO (BG.FunPtr (A -> Some_enum -> IO ()))
hs_bindgen_ae4b21912dc3e9fc =
  fmap BG.fromFFIType hs_bindgen_ae4b21912dc3e9fc_base

{-# NOINLINE const_withoutSign_before6 #-}
{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @macros\/reparse.h 191:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before6 :: BG.FunPtr (A -> Some_enum -> IO ())
const_withoutSign_before6 =
  BG.unsafePerformIO hs_bindgen_ae4b21912dc3e9fc

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_before7@
foreign import ccall unsafe "hs_bindgen_2823ede1fece8296" hs_bindgen_2823ede1fece8296_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_before7@
hs_bindgen_2823ede1fece8296 :: IO (BG.FunPtr (A -> BG.CBool -> IO ()))
hs_bindgen_2823ede1fece8296 =
  fmap BG.fromFFIType hs_bindgen_2823ede1fece8296_base

{-# NOINLINE const_withoutSign_before7 #-}
{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @macros\/reparse.h 192:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before7 :: BG.FunPtr (A -> BG.CBool -> IO ())
const_withoutSign_before7 =
  BG.unsafePerformIO hs_bindgen_2823ede1fece8296

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_before8@
foreign import ccall unsafe "hs_bindgen_251faa95ed2fbdfe" hs_bindgen_251faa95ed2fbdfe_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_before8@
hs_bindgen_251faa95ed2fbdfe :: IO (BG.FunPtr (A -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_251faa95ed2fbdfe =
  fmap BG.fromFFIType hs_bindgen_251faa95ed2fbdfe_base

{-# NOINLINE const_withoutSign_before8 #-}
{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @macros\/reparse.h 193:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before8 :: BG.FunPtr (A -> HsBindgen.Runtime.LibC.CSize -> IO ())
const_withoutSign_before8 =
  BG.unsafePerformIO hs_bindgen_251faa95ed2fbdfe

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_after1@
foreign import ccall unsafe "hs_bindgen_64d880340cdd333b" hs_bindgen_64d880340cdd333b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_after1@
hs_bindgen_64d880340cdd333b :: IO (BG.FunPtr (A -> BG.CFloat -> IO ()))
hs_bindgen_64d880340cdd333b =
  fmap BG.fromFFIType hs_bindgen_64d880340cdd333b_base

{-# NOINLINE const_withoutSign_after1 #-}
{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @macros\/reparse.h 195:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after1 :: BG.FunPtr (A -> BG.CFloat -> IO ())
const_withoutSign_after1 =
  BG.unsafePerformIO hs_bindgen_64d880340cdd333b

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_after2@
foreign import ccall unsafe "hs_bindgen_a0a7ed95c70e65de" hs_bindgen_a0a7ed95c70e65de_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_after2@
hs_bindgen_a0a7ed95c70e65de :: IO (BG.FunPtr (A -> BG.CDouble -> IO ()))
hs_bindgen_a0a7ed95c70e65de =
  fmap BG.fromFFIType hs_bindgen_a0a7ed95c70e65de_base

{-# NOINLINE const_withoutSign_after2 #-}
{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @macros\/reparse.h 196:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after2 :: BG.FunPtr (A -> BG.CDouble -> IO ())
const_withoutSign_after2 =
  BG.unsafePerformIO hs_bindgen_a0a7ed95c70e65de

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_after3@
foreign import ccall unsafe "hs_bindgen_743e0cfcefbe1d8a" hs_bindgen_743e0cfcefbe1d8a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_after3@
hs_bindgen_743e0cfcefbe1d8a :: IO (BG.FunPtr (A -> BG.CBool -> IO ()))
hs_bindgen_743e0cfcefbe1d8a =
  fmap BG.fromFFIType hs_bindgen_743e0cfcefbe1d8a_base

{-# NOINLINE const_withoutSign_after3 #-}
{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @macros\/reparse.h 197:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after3 :: BG.FunPtr (A -> BG.CBool -> IO ())
const_withoutSign_after3 =
  BG.unsafePerformIO hs_bindgen_743e0cfcefbe1d8a

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_after4@
foreign import ccall unsafe "hs_bindgen_18bf3095221fb3ea" hs_bindgen_18bf3095221fb3ea_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_after4@
hs_bindgen_18bf3095221fb3ea :: IO (BG.FunPtr (A -> Some_struct -> IO ()))
hs_bindgen_18bf3095221fb3ea =
  fmap BG.fromFFIType hs_bindgen_18bf3095221fb3ea_base

{-# NOINLINE const_withoutSign_after4 #-}
{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @macros\/reparse.h 198:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after4 :: BG.FunPtr (A -> Some_struct -> IO ())
const_withoutSign_after4 =
  BG.unsafePerformIO hs_bindgen_18bf3095221fb3ea

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_after5@
foreign import ccall unsafe "hs_bindgen_2bed6677ba4e137b" hs_bindgen_2bed6677ba4e137b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_after5@
hs_bindgen_2bed6677ba4e137b :: IO (BG.FunPtr (A -> Some_union -> IO ()))
hs_bindgen_2bed6677ba4e137b =
  fmap BG.fromFFIType hs_bindgen_2bed6677ba4e137b_base

{-# NOINLINE const_withoutSign_after5 #-}
{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @macros\/reparse.h 199:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after5 :: BG.FunPtr (A -> Some_union -> IO ())
const_withoutSign_after5 =
  BG.unsafePerformIO hs_bindgen_2bed6677ba4e137b

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_after6@
foreign import ccall unsafe "hs_bindgen_f1507092da27bfd5" hs_bindgen_f1507092da27bfd5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_after6@
hs_bindgen_f1507092da27bfd5 :: IO (BG.FunPtr (A -> Some_enum -> IO ()))
hs_bindgen_f1507092da27bfd5 =
  fmap BG.fromFFIType hs_bindgen_f1507092da27bfd5_base

{-# NOINLINE const_withoutSign_after6 #-}
{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @macros\/reparse.h 200:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after6 :: BG.FunPtr (A -> Some_enum -> IO ())
const_withoutSign_after6 =
  BG.unsafePerformIO hs_bindgen_f1507092da27bfd5

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_after7@
foreign import ccall unsafe "hs_bindgen_2e2acc8439a2c616" hs_bindgen_2e2acc8439a2c616_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_after7@
hs_bindgen_2e2acc8439a2c616 :: IO (BG.FunPtr (A -> BG.CBool -> IO ()))
hs_bindgen_2e2acc8439a2c616 =
  fmap BG.fromFFIType hs_bindgen_2e2acc8439a2c616_base

{-# NOINLINE const_withoutSign_after7 #-}
{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @macros\/reparse.h 201:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after7 :: BG.FunPtr (A -> BG.CBool -> IO ())
const_withoutSign_after7 =
  BG.unsafePerformIO hs_bindgen_2e2acc8439a2c616

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_after8@
foreign import ccall unsafe "hs_bindgen_2c66fc7d02e1638c" hs_bindgen_2c66fc7d02e1638c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_withoutSign_after8@
hs_bindgen_2c66fc7d02e1638c :: IO (BG.FunPtr (A -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_2c66fc7d02e1638c =
  fmap BG.fromFFIType hs_bindgen_2c66fc7d02e1638c_base

{-# NOINLINE const_withoutSign_after8 #-}
{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @macros\/reparse.h 202:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after8 :: BG.FunPtr (A -> HsBindgen.Runtime.LibC.CSize -> IO ())
const_withoutSign_after8 =
  BG.unsafePerformIO hs_bindgen_2c66fc7d02e1638c

-- __unique:__ @test_macrosreparse Example get_const_pointers_args1@
foreign import ccall unsafe "hs_bindgen_876a113f07d4f5f9" hs_bindgen_876a113f07d4f5f9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_pointers_args1@
hs_bindgen_876a113f07d4f5f9 :: IO (BG.FunPtr (A -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_876a113f07d4f5f9 =
  fmap BG.fromFFIType hs_bindgen_876a113f07d4f5f9_base

{-# NOINLINE const_pointers_args1 #-}
{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @macros\/reparse.h 206:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args1 :: BG.FunPtr (A -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args1 =
  BG.unsafePerformIO hs_bindgen_876a113f07d4f5f9

-- __unique:__ @test_macrosreparse Example get_const_pointers_args2@
foreign import ccall unsafe "hs_bindgen_b660566323282737" hs_bindgen_b660566323282737_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_pointers_args2@
hs_bindgen_b660566323282737 :: IO (BG.FunPtr (A -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_b660566323282737 =
  fmap BG.fromFFIType hs_bindgen_b660566323282737_base

{-# NOINLINE const_pointers_args2 #-}
{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @macros\/reparse.h 207:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args2 :: BG.FunPtr (A -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args2 =
  BG.unsafePerformIO hs_bindgen_b660566323282737

-- __unique:__ @test_macrosreparse Example get_const_pointers_args3@
foreign import ccall unsafe "hs_bindgen_994dc853f2619a05" hs_bindgen_994dc853f2619a05_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_pointers_args3@
hs_bindgen_994dc853f2619a05 :: IO (BG.FunPtr (A -> BG.Ptr BG.CInt -> IO ()))
hs_bindgen_994dc853f2619a05 =
  fmap BG.fromFFIType hs_bindgen_994dc853f2619a05_base

{-# NOINLINE const_pointers_args3 #-}
{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @macros\/reparse.h 208:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args3 :: BG.FunPtr (A -> BG.Ptr BG.CInt -> IO ())
const_pointers_args3 =
  BG.unsafePerformIO hs_bindgen_994dc853f2619a05

-- __unique:__ @test_macrosreparse Example get_const_pointers_args4@
foreign import ccall unsafe "hs_bindgen_1e461cf8b8eb6c2e" hs_bindgen_1e461cf8b8eb6c2e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_pointers_args4@
hs_bindgen_1e461cf8b8eb6c2e :: IO (BG.FunPtr (A -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_1e461cf8b8eb6c2e =
  fmap BG.fromFFIType hs_bindgen_1e461cf8b8eb6c2e_base

{-# NOINLINE const_pointers_args4 #-}
{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @macros\/reparse.h 209:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args4 :: BG.FunPtr (A -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args4 =
  BG.unsafePerformIO hs_bindgen_1e461cf8b8eb6c2e

-- __unique:__ @test_macrosreparse Example get_const_pointers_args5@
foreign import ccall unsafe "hs_bindgen_de3f03872be78d51" hs_bindgen_de3f03872be78d51_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_pointers_args5@
hs_bindgen_de3f03872be78d51 :: IO (BG.FunPtr (A -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_de3f03872be78d51 =
  fmap BG.fromFFIType hs_bindgen_de3f03872be78d51_base

{-# NOINLINE const_pointers_args5 #-}
{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @macros\/reparse.h 210:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args5 :: BG.FunPtr (A -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args5 =
  BG.unsafePerformIO hs_bindgen_de3f03872be78d51

-- __unique:__ @test_macrosreparse Example get_const_pointers_ret1@
foreign import ccall unsafe "hs_bindgen_b436bed7a8df6aa7" hs_bindgen_b436bed7a8df6aa7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_pointers_ret1@
hs_bindgen_b436bed7a8df6aa7 :: IO (BG.FunPtr (A -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_b436bed7a8df6aa7 =
  fmap BG.fromFFIType hs_bindgen_b436bed7a8df6aa7_base

{-# NOINLINE const_pointers_ret1 #-}
{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h 212:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1 :: BG.FunPtr (A -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret1 =
  BG.unsafePerformIO hs_bindgen_b436bed7a8df6aa7

-- __unique:__ @test_macrosreparse Example get_const_pointers_ret2@
foreign import ccall unsafe "hs_bindgen_a813e17971e4052b" hs_bindgen_a813e17971e4052b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_pointers_ret2@
hs_bindgen_a813e17971e4052b :: IO (BG.FunPtr (A -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_a813e17971e4052b =
  fmap BG.fromFFIType hs_bindgen_a813e17971e4052b_base

{-# NOINLINE const_pointers_ret2 #-}
{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h 213:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2 :: BG.FunPtr (A -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret2 =
  BG.unsafePerformIO hs_bindgen_a813e17971e4052b

-- __unique:__ @test_macrosreparse Example get_const_pointers_ret3@
foreign import ccall unsafe "hs_bindgen_ef67874c5a6020ba" hs_bindgen_ef67874c5a6020ba_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_pointers_ret3@
hs_bindgen_ef67874c5a6020ba :: IO (BG.FunPtr (A -> IO (BG.Ptr BG.CInt)))
hs_bindgen_ef67874c5a6020ba =
  fmap BG.fromFFIType hs_bindgen_ef67874c5a6020ba_base

{-# NOINLINE const_pointers_ret3 #-}
{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h 214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3 :: BG.FunPtr (A -> IO (BG.Ptr BG.CInt))
const_pointers_ret3 =
  BG.unsafePerformIO hs_bindgen_ef67874c5a6020ba

-- __unique:__ @test_macrosreparse Example get_const_pointers_ret4@
foreign import ccall unsafe "hs_bindgen_290705eae4a932ea" hs_bindgen_290705eae4a932ea_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_pointers_ret4@
hs_bindgen_290705eae4a932ea :: IO (BG.FunPtr (A -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_290705eae4a932ea =
  fmap BG.fromFFIType hs_bindgen_290705eae4a932ea_base

{-# NOINLINE const_pointers_ret4 #-}
{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h 215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4 :: BG.FunPtr (A -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret4 =
  BG.unsafePerformIO hs_bindgen_290705eae4a932ea

-- __unique:__ @test_macrosreparse Example get_const_pointers_ret5@
foreign import ccall unsafe "hs_bindgen_1aab75ab18dfc1f9" hs_bindgen_1aab75ab18dfc1f9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_pointers_ret5@
hs_bindgen_1aab75ab18dfc1f9 :: IO (BG.FunPtr (A -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_1aab75ab18dfc1f9 =
  fmap BG.fromFFIType hs_bindgen_1aab75ab18dfc1f9_base

{-# NOINLINE const_pointers_ret5 #-}
{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h 216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5 :: BG.FunPtr (A -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret5 =
  BG.unsafePerformIO hs_bindgen_1aab75ab18dfc1f9

-- __unique:__ @test_macrosreparse Example get_const_array_elem1@
foreign import ccall unsafe "hs_bindgen_b0b32a29c796f2d8" hs_bindgen_b0b32a29c796f2d8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_array_elem1@
hs_bindgen_b0b32a29c796f2d8 :: IO (BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray A)) -> IO ()))
hs_bindgen_b0b32a29c796f2d8 =
  fmap BG.fromFFIType hs_bindgen_b0b32a29c796f2d8_base

{-# NOINLINE const_array_elem1 #-}
{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h 244:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 :: BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray A)) -> IO ())
const_array_elem1 =
  BG.unsafePerformIO hs_bindgen_b0b32a29c796f2d8

-- __unique:__ @test_macrosreparse Example get_const_array_elem2@
foreign import ccall unsafe "hs_bindgen_c8da796954fe25ed" hs_bindgen_c8da796954fe25ed_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_array_elem2@
hs_bindgen_c8da796954fe25ed :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst A))) -> IO ()))
hs_bindgen_c8da796954fe25ed =
  fmap BG.fromFFIType hs_bindgen_c8da796954fe25ed_base

{-# NOINLINE const_array_elem2 #-}
{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h 245:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2 :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst A))) -> IO ())
const_array_elem2 =
  BG.unsafePerformIO hs_bindgen_c8da796954fe25ed

-- __unique:__ @test_macrosreparse Example get_const_array_elem3@
foreign import ccall unsafe "hs_bindgen_9fd2564dafc0b34b" hs_bindgen_9fd2564dafc0b34b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_const_array_elem3@
hs_bindgen_9fd2564dafc0b34b :: IO (BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr A))) -> IO ()))
hs_bindgen_9fd2564dafc0b34b =
  fmap BG.fromFFIType hs_bindgen_9fd2564dafc0b34b_base

{-# NOINLINE const_array_elem3 #-}
{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h 246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 :: BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr A))) -> IO ())
const_array_elem3 =
  BG.unsafePerformIO hs_bindgen_9fd2564dafc0b34b

-- __unique:__ @test_macrosreparse Example get_noParams1@
foreign import ccall unsafe "hs_bindgen_a95db292e183a1d4" hs_bindgen_a95db292e183a1d4_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_noParams1@
hs_bindgen_a95db292e183a1d4 :: IO (BG.FunPtr (IO A))
hs_bindgen_a95db292e183a1d4 =
  fmap BG.fromFFIType hs_bindgen_a95db292e183a1d4_base

{-# NOINLINE noParams1 #-}
{-| Other examples we reparsed /incorrectly/ before language-c

    __C declaration:__ @noParams1@

    __defined at:__ @macros\/reparse.h 254:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams1 :: BG.FunPtr (IO A)
noParams1 =
  BG.unsafePerformIO hs_bindgen_a95db292e183a1d4

-- __unique:__ @test_macrosreparse Example get_noParams2@
foreign import ccall unsafe "hs_bindgen_601cc797be54ac76" hs_bindgen_601cc797be54ac76_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_noParams2@
hs_bindgen_601cc797be54ac76 :: IO (BG.FunPtr (IO A))
hs_bindgen_601cc797be54ac76 =
  fmap BG.fromFFIType hs_bindgen_601cc797be54ac76_base

{-# NOINLINE noParams2 #-}
{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h 255:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2 :: BG.FunPtr (IO A)
noParams2 =
  BG.unsafePerformIO hs_bindgen_601cc797be54ac76

-- __unique:__ @test_macrosreparse Example get_noParams3@
foreign import ccall unsafe "hs_bindgen_2c9b26f803b2d2d0" hs_bindgen_2c9b26f803b2d2d0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_noParams3@
hs_bindgen_2c9b26f803b2d2d0 :: IO (BG.FunPtr (A -> BG.FunPtr (IO BG.CInt) -> IO ()))
hs_bindgen_2c9b26f803b2d2d0 =
  fmap BG.fromFFIType hs_bindgen_2c9b26f803b2d2d0_base

{-# NOINLINE noParams3 #-}
{-| __C declaration:__ @noParams3@

    __defined at:__ @macros\/reparse.h 256:6@

    __exported by:__ @macros\/reparse.h@
-}
noParams3 :: BG.FunPtr (A -> BG.FunPtr (IO BG.CInt) -> IO ())
noParams3 =
  BG.unsafePerformIO hs_bindgen_2c9b26f803b2d2d0

-- __unique:__ @test_macrosreparse Example get_funptr_ret1@
foreign import ccall unsafe "hs_bindgen_a8db83002fa60823" hs_bindgen_a8db83002fa60823_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_funptr_ret1@
hs_bindgen_a8db83002fa60823 :: IO (BG.FunPtr (A -> IO (BG.FunPtr (IO ()))))
hs_bindgen_a8db83002fa60823 =
  fmap BG.fromFFIType hs_bindgen_a8db83002fa60823_base

{-# NOINLINE funptr_ret1 #-}
{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h 260:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1 :: BG.FunPtr (A -> IO (BG.FunPtr (IO ())))
funptr_ret1 =
  BG.unsafePerformIO hs_bindgen_a8db83002fa60823

-- __unique:__ @test_macrosreparse Example get_funptr_ret2@
foreign import ccall unsafe "hs_bindgen_2c24c1c6748f2c20" hs_bindgen_2c24c1c6748f2c20_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_funptr_ret2@
hs_bindgen_2c24c1c6748f2c20 :: IO (BG.FunPtr (A -> IO (BG.FunPtr (IO BG.CInt))))
hs_bindgen_2c24c1c6748f2c20 =
  fmap BG.fromFFIType hs_bindgen_2c24c1c6748f2c20_base

{-# NOINLINE funptr_ret2 #-}
{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h 261:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2 :: BG.FunPtr (A -> IO (BG.FunPtr (IO BG.CInt)))
funptr_ret2 =
  BG.unsafePerformIO hs_bindgen_2c24c1c6748f2c20

-- __unique:__ @test_macrosreparse Example get_funptr_ret3@
foreign import ccall unsafe "hs_bindgen_b16880cfb2c2c277" hs_bindgen_b16880cfb2c2c277_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_funptr_ret3@
hs_bindgen_b16880cfb2c2c277 :: IO (BG.FunPtr (A -> IO (BG.FunPtr (BG.CInt -> IO ()))))
hs_bindgen_b16880cfb2c2c277 =
  fmap BG.fromFFIType hs_bindgen_b16880cfb2c2c277_base

{-# NOINLINE funptr_ret3 #-}
{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h 262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3 :: BG.FunPtr (A -> IO (BG.FunPtr (BG.CInt -> IO ())))
funptr_ret3 =
  BG.unsafePerformIO hs_bindgen_b16880cfb2c2c277

-- __unique:__ @test_macrosreparse Example get_funptr_ret4@
foreign import ccall unsafe "hs_bindgen_16971b9d43f8b276" hs_bindgen_16971b9d43f8b276_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_funptr_ret4@
hs_bindgen_16971b9d43f8b276 :: IO (BG.FunPtr (A -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))))
hs_bindgen_16971b9d43f8b276 =
  fmap BG.fromFFIType hs_bindgen_16971b9d43f8b276_base

{-# NOINLINE funptr_ret4 #-}
{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h 263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4 :: BG.FunPtr (A -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar)))
funptr_ret4 =
  BG.unsafePerformIO hs_bindgen_16971b9d43f8b276

-- __unique:__ @test_macrosreparse Example get_funptr_ret5@
foreign import ccall unsafe "hs_bindgen_559439091def57b7" hs_bindgen_559439091def57b7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_funptr_ret5@
hs_bindgen_559439091def57b7 :: IO (BG.FunPtr (A -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))))
hs_bindgen_559439091def57b7 =
  fmap BG.fromFFIType hs_bindgen_559439091def57b7_base

{-# NOINLINE funptr_ret5 #-}
{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h 267:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5 :: BG.FunPtr (A -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))))
funptr_ret5 =
  BG.unsafePerformIO hs_bindgen_559439091def57b7

-- __unique:__ @test_macrosreparse Example get_funptr_ret6@
foreign import ccall unsafe "hs_bindgen_417ef356c0a65410" hs_bindgen_417ef356c0a65410_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_funptr_ret6@
hs_bindgen_417ef356c0a65410 :: IO (BG.FunPtr (A -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_417ef356c0a65410 =
  fmap BG.fromFFIType hs_bindgen_417ef356c0a65410_base

{-# NOINLINE funptr_ret6 #-}
{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h 268:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6 :: BG.FunPtr (A -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret6 =
  BG.unsafePerformIO hs_bindgen_417ef356c0a65410

-- __unique:__ @test_macrosreparse Example get_funptr_ret7@
foreign import ccall unsafe "hs_bindgen_397041070208b29f" hs_bindgen_397041070208b29f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_funptr_ret7@
hs_bindgen_397041070208b29f :: IO (BG.FunPtr (A -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_397041070208b29f =
  fmap BG.fromFFIType hs_bindgen_397041070208b29f_base

{-# NOINLINE funptr_ret7 #-}
{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h 269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7 :: BG.FunPtr (A -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret7 =
  BG.unsafePerformIO hs_bindgen_397041070208b29f

-- __unique:__ @test_macrosreparse Example get_funptr_ret8@
foreign import ccall unsafe "hs_bindgen_4e74e96bb0ca12e1" hs_bindgen_4e74e96bb0ca12e1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_funptr_ret8@
hs_bindgen_4e74e96bb0ca12e1 :: IO (BG.FunPtr (A -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))))
hs_bindgen_4e74e96bb0ca12e1 =
  fmap BG.fromFFIType hs_bindgen_4e74e96bb0ca12e1_base

{-# NOINLINE funptr_ret8 #-}
{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h 270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8 :: BG.FunPtr (A -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))))
funptr_ret8 =
  BG.unsafePerformIO hs_bindgen_4e74e96bb0ca12e1

-- __unique:__ @test_macrosreparse Example get_funptr_ret9@
foreign import ccall unsafe "hs_bindgen_c8c300f87d0d976b" hs_bindgen_c8c300f87d0d976b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_funptr_ret9@
hs_bindgen_c8c300f87d0d976b :: IO (BG.FunPtr (A -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_c8c300f87d0d976b =
  fmap BG.fromFFIType hs_bindgen_c8c300f87d0d976b_base

{-# NOINLINE funptr_ret9 #-}
{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h 271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9 :: BG.FunPtr (A -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret9 =
  BG.unsafePerformIO hs_bindgen_c8c300f87d0d976b

-- __unique:__ @test_macrosreparse Example get_funptr_ret10@
foreign import ccall unsafe "hs_bindgen_b5fa1f4fdbdf537e" hs_bindgen_b5fa1f4fdbdf537e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example get_funptr_ret10@
hs_bindgen_b5fa1f4fdbdf537e :: IO (BG.FunPtr (A -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_b5fa1f4fdbdf537e =
  fmap BG.fromFFIType hs_bindgen_b5fa1f4fdbdf537e_base

{-# NOINLINE funptr_ret10 #-}
{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h 272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10 :: BG.FunPtr (A -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret10 =
  BG.unsafePerformIO hs_bindgen_b5fa1f4fdbdf537e
