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
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_char1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5e1210eea0f91079 (void)) ("
  , "  signed int arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &args_char1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_char2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_90fb0c89282e7826 (void)) ("
  , "  signed int arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  return &args_char2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_char3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e2f7d4fffc5bd534 (void)) ("
  , "  signed int arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  return &args_char3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_short1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a95598bce8b01075 (void)) ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_short2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d3a390046c883b58 (void)) ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_short3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d55e3e898a58823a (void)) ("
  , "  signed int arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  return &args_short3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_int1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_957d483b5ca53592 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_int2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2d461e747155c900 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_int3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_221305a0eed40315 (void)) ("
  , "  signed int arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  return &args_int3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_long1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f972f7dfc04c4643 (void)) ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_long2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1ea099fc0d64eaf0 (void)) ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_long3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4b98dcfcff8d2c16 (void)) ("
  , "  signed int arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  return &args_long3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_float */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c2667f55015837d5 (void)) ("
  , "  signed int arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  return &args_float;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_double */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0100fbb35f12b9ca (void)) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &args_double;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_bool1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cca6089fee39d241 (void)) ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &args_bool1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_struct */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b33b3d1717b0a069 (void)) ("
  , "  signed int arg1,"
  , "  struct some_struct arg2"
  , ")"
  , "{"
  , "  return &args_struct;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_union */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7f32b8254b45acce (void)) ("
  , "  signed int arg1,"
  , "  union some_union arg2"
  , ")"
  , "{"
  , "  return &args_union;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_enum */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_681a6f05ca44d076 (void)) ("
  , "  signed int arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  return &args_enum;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_pointer1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_53c78b7126a5c2d7 (void)) ("
  , "  signed int arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return &args_pointer1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_pointer2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fcc615fabdb3c7c8 (void)) ("
  , "  signed int arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  return &args_pointer2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_pointer3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_84b5eb5d784e7d8b (void)) ("
  , "  signed int arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  return &args_pointer3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_A */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_ab989b187ef104a6 (void)) (void)"
  , "{"
  , "  return &ret_A;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_char1 */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_1dade4ecdbb7d1e3 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_char1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_char2 */"
  , "__attribute__ ((const))"
  , "signed char (*hs_bindgen_1b8112cb53e3ee13 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_char2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_char3 */"
  , "__attribute__ ((const))"
  , "unsigned char (*hs_bindgen_3193a0bc99b2392e (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_char3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_short1 */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_601176fde433c8c8 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_short1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_short2 */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_9fa8533d04e7f294 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_short2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_short3 */"
  , "__attribute__ ((const))"
  , "unsigned short (*hs_bindgen_a9afff97ffaea466 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_short3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_int1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_fc146923a35d31bd (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_int1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_int2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_dabf136faf39eb12 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_int2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_int3 */"
  , "__attribute__ ((const))"
  , "unsigned int (*hs_bindgen_8cf89eb76d70daab (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_int3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_long1 */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_793c67e678975ced (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_long1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_long2 */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_65e1ed59653c64df (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_long2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_long3 */"
  , "__attribute__ ((const))"
  , "unsigned long (*hs_bindgen_9392027014182bb3 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_long3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_float */"
  , "__attribute__ ((const))"
  , "float (*hs_bindgen_6dfef4080131cd2b (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_float;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_double */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_aa07aa565d467d6c (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_double;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_bool1 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_896dc5f00ec1c288 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_bool1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_struct */"
  , "__attribute__ ((const))"
  , "struct some_struct (*hs_bindgen_388388c8aa13ac36 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_struct;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_union */"
  , "__attribute__ ((const))"
  , "union some_union (*hs_bindgen_aa77e44e5e219123 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_union;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_enum */"
  , "__attribute__ ((const))"
  , "enum some_enum (*hs_bindgen_c2cf0eacb5266177 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_enum;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_pointer1 */"
  , "__attribute__ ((const))"
  , "signed int *(*hs_bindgen_e9d880a4a462ef0a (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_pointer1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_pointer2 */"
  , "__attribute__ ((const))"
  , "signed int **(*hs_bindgen_50e9228b4d949c87 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_pointer2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_pointer3 */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_ec736415852addfa (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_pointer3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_body1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_8acc5f80130dd67d (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &body1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_body2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_23201b1ad7870002 (void)) (void)"
  , "{"
  , "  return &body2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_complex_float */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f869d2a6acf014be (void)) ("
  , "  signed int arg1,"
  , "  float _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_float;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_args_complex_double */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_afc936a14b9315fc (void)) ("
  , "  signed int arg1,"
  , "  double _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_double;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_complex_float */"
  , "__attribute__ ((const))"
  , "float _Complex (*hs_bindgen_63752d3ec6f51e06 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_complex_float;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_ret_complex_double */"
  , "__attribute__ ((const))"
  , "double _Complex (*hs_bindgen_310cc703a6a6bafb (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &ret_complex_double;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_bespoke_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0e0e4765fc564a12 (void)) ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &bespoke_args1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_bespoke_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5bfacc5b73ae16d8 (void)) ("
  , "  signed int arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &bespoke_args2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_bespoke_ret1 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_d07549f88c995df7 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_bespoke_ret2 */"
  , "__attribute__ ((const))"
  , "size_t (*hs_bindgen_8b11c06b40a9c3c2 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_arr_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_45247f6fba0c5dbf (void)) ("
  , "  signed int arg1[]"
  , ")"
  , "{"
  , "  return &arr_args1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_arr_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3182f9011daf4358 (void)) ("
  , "  signed int *arg1[]"
  , ")"
  , "{"
  , "  return &arr_args2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_arr_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_00feb5ae4554f46c (void)) ("
  , "  signed int arg1[5]"
  , ")"
  , "{"
  , "  return &arr_args3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_arr_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ea4721eb51a52046 (void)) ("
  , "  signed int *arg1[5]"
  , ")"
  , "{"
  , "  return &arr_args4;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_funptr_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_75a197890a492f5d (void)) ("
  , "  signed int arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_funptr_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_602bd65e56705bb6 (void)) ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_funptr_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8300cda6204eb7b5 (void)) ("
  , "  signed int arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_funptr_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b793c7faf25d1d9c (void)) ("
  , "  signed int arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args4;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_funptr_args5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4f52cc41edcd129e (void)) ("
  , "  signed int arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args5;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_comments1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_eeda8a79d3f2dafa (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &comments1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_prim_before1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_172430e48f221c1b (void)) ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_prim_before2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e1ef4ef42c2f4706 (void)) ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_prim_before3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_482f7f9384e16fdc (void)) ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_prim_after1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7877d994c033a25a (void)) ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_prim_after2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_937c8f934f60558a (void)) ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_prim_after3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e694840b63a92b11 (void)) ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_withoutSign_before1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1c3fc4f799449f13 (void)) ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_withoutSign_before2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_752a8e3a0ea5d652 (void)) ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_withoutSign_before3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9b101d119538264b (void)) ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_withoutSign_before4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_bc2f7744c57f8954 (void)) ("
  , "  signed int arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before4;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_withoutSign_before5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6c667d3b0c229045 (void)) ("
  , "  signed int arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before5;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_withoutSign_before6 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_97f2af9061b1d404 (void)) ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before6;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_withoutSign_before7 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cdbdbfa038d903e1 (void)) ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before7;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_withoutSign_before8 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_174dc83912b6e97a (void)) ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before8;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_withoutSign_after1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_bab771fa5614c60c (void)) ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_withoutSign_after2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_bba83003ba9d0e65 (void)) ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_withoutSign_after3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8b30e6c0e586fbed (void)) ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_withoutSign_after4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e4bd6496bb9ba3fb (void)) ("
  , "  signed int arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after4;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_withoutSign_after5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4fa6cf106ec109d9 (void)) ("
  , "  signed int arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after5;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_withoutSign_after6 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4412b652c7e31a26 (void)) ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after6;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_withoutSign_after7 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_36cac970802d5e2a (void)) ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after7;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_withoutSign_after8 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_568a55ef8c9b5076 (void)) ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after8;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_pointers_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3f7382d6169ece39 (void)) ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_pointers_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e0015f5403a3add6 (void)) ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_pointers_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e9ee81509f90b9ad (void)) ("
  , "  signed int arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_pointers_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2e62954a25f4a719 (void)) ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args4;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_pointers_args5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_db998d61b1058a46 (void)) ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args5;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_pointers_ret1 */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_de5e820b8aa183b8 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_pointers_ret2 */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_69cbe45216c60049 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_pointers_ret3 */"
  , "__attribute__ ((const))"
  , "signed int *const (*hs_bindgen_fc3af5baf478fdab (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_pointers_ret4 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_800ceaaeb41b7c4f (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret4;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_pointers_ret5 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_88afe3ace1c8f032 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret5;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_array_elem1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_99ff740159cf42db (void)) ("
  , "  signed int const arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_array_elem2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f429cac79bc959b8 (void)) ("
  , "  signed int const *arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_const_array_elem3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d5c9dc626ac3c92a (void)) ("
  , "  signed int *const arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_noParams1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a1d7d27beac0cf43 (void)) (void)"
  , "{"
  , "  return &noParams1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_noParams2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_c4d3ce2b57180e06 (void)) (void)"
  , "{"
  , "  return &noParams2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_noParams3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3198dd435d083be6 (void)) ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &noParams3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_funptr_ret1 */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_4c713afd40175b78 (void)) ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret1;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_funptr_ret2 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_d696ef6b004ced7d (void)) ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret2;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_funptr_ret3 */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_837085ddb8062707 (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &funptr_ret3;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_funptr_ret4 */"
  , "__attribute__ ((const))"
  , "char (*(*hs_bindgen_b2faea37032ad23b (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret4;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_funptr_ret5 */"
  , "__attribute__ ((const))"
  , "signed int *(*(*hs_bindgen_41e753039680a766 (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret5;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_funptr_ret6 */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_5d8ba1f50ed61fd3 (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret6;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_funptr_ret7 */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_210a758d713c2fba (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret7;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_funptr_ret8 */"
  , "__attribute__ ((const))"
  , "signed int *const (*(*hs_bindgen_d281895081e937b3 (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret8;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_funptr_ret9 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_086a0d13fcf4ab8e (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret9;"
  , "}"
  , "/* test_macrosreparse_2_raw\NULExample\NULget_funptr_ret10 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_780c1cc5a945167c (void)) ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret10;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_2_raw Example get_args_char1@
foreign import ccall unsafe "hs_bindgen_5e1210eea0f91079" hs_bindgen_5e1210eea0f91079_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_char1@
hs_bindgen_5e1210eea0f91079 :: IO (BG.FunPtr (BG.CInt -> BG.CChar -> IO ()))
hs_bindgen_5e1210eea0f91079 =
  fmap BG.fromFFIType hs_bindgen_5e1210eea0f91079_base

{-# NOINLINE args_char1 #-}
{-| Function declarations

    __C declaration:__ @args_char1@

    __defined at:__ @macros\/reparse.h 17:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char1 :: BG.FunPtr (BG.CInt -> BG.CChar -> IO ())
args_char1 =
  BG.unsafePerformIO hs_bindgen_5e1210eea0f91079

-- __unique:__ @test_macrosreparse_2_raw Example get_args_char2@
foreign import ccall unsafe "hs_bindgen_90fb0c89282e7826" hs_bindgen_90fb0c89282e7826_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_char2@
hs_bindgen_90fb0c89282e7826 :: IO (BG.FunPtr (BG.CInt -> BG.CSChar -> IO ()))
hs_bindgen_90fb0c89282e7826 =
  fmap BG.fromFFIType hs_bindgen_90fb0c89282e7826_base

{-# NOINLINE args_char2 #-}
{-| __C declaration:__ @args_char2@

    __defined at:__ @macros\/reparse.h 18:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char2 :: BG.FunPtr (BG.CInt -> BG.CSChar -> IO ())
args_char2 =
  BG.unsafePerformIO hs_bindgen_90fb0c89282e7826

-- __unique:__ @test_macrosreparse_2_raw Example get_args_char3@
foreign import ccall unsafe "hs_bindgen_e2f7d4fffc5bd534" hs_bindgen_e2f7d4fffc5bd534_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_char3@
hs_bindgen_e2f7d4fffc5bd534 :: IO (BG.FunPtr (BG.CInt -> BG.CUChar -> IO ()))
hs_bindgen_e2f7d4fffc5bd534 =
  fmap BG.fromFFIType hs_bindgen_e2f7d4fffc5bd534_base

{-# NOINLINE args_char3 #-}
{-| __C declaration:__ @args_char3@

    __defined at:__ @macros\/reparse.h 19:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char3 :: BG.FunPtr (BG.CInt -> BG.CUChar -> IO ())
args_char3 =
  BG.unsafePerformIO hs_bindgen_e2f7d4fffc5bd534

-- __unique:__ @test_macrosreparse_2_raw Example get_args_short1@
foreign import ccall unsafe "hs_bindgen_a95598bce8b01075" hs_bindgen_a95598bce8b01075_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_short1@
hs_bindgen_a95598bce8b01075 :: IO (BG.FunPtr (BG.CInt -> BG.CShort -> IO ()))
hs_bindgen_a95598bce8b01075 =
  fmap BG.fromFFIType hs_bindgen_a95598bce8b01075_base

{-# NOINLINE args_short1 #-}
{-| __C declaration:__ @args_short1@

    __defined at:__ @macros\/reparse.h 21:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short1 :: BG.FunPtr (BG.CInt -> BG.CShort -> IO ())
args_short1 =
  BG.unsafePerformIO hs_bindgen_a95598bce8b01075

-- __unique:__ @test_macrosreparse_2_raw Example get_args_short2@
foreign import ccall unsafe "hs_bindgen_d3a390046c883b58" hs_bindgen_d3a390046c883b58_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_short2@
hs_bindgen_d3a390046c883b58 :: IO (BG.FunPtr (BG.CInt -> BG.CShort -> IO ()))
hs_bindgen_d3a390046c883b58 =
  fmap BG.fromFFIType hs_bindgen_d3a390046c883b58_base

{-# NOINLINE args_short2 #-}
{-| __C declaration:__ @args_short2@

    __defined at:__ @macros\/reparse.h 22:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short2 :: BG.FunPtr (BG.CInt -> BG.CShort -> IO ())
args_short2 =
  BG.unsafePerformIO hs_bindgen_d3a390046c883b58

-- __unique:__ @test_macrosreparse_2_raw Example get_args_short3@
foreign import ccall unsafe "hs_bindgen_d55e3e898a58823a" hs_bindgen_d55e3e898a58823a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_short3@
hs_bindgen_d55e3e898a58823a :: IO (BG.FunPtr (BG.CInt -> BG.CUShort -> IO ()))
hs_bindgen_d55e3e898a58823a =
  fmap BG.fromFFIType hs_bindgen_d55e3e898a58823a_base

{-# NOINLINE args_short3 #-}
{-| __C declaration:__ @args_short3@

    __defined at:__ @macros\/reparse.h 23:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short3 :: BG.FunPtr (BG.CInt -> BG.CUShort -> IO ())
args_short3 =
  BG.unsafePerformIO hs_bindgen_d55e3e898a58823a

-- __unique:__ @test_macrosreparse_2_raw Example get_args_int1@
foreign import ccall unsafe "hs_bindgen_957d483b5ca53592" hs_bindgen_957d483b5ca53592_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_int1@
hs_bindgen_957d483b5ca53592 :: IO (BG.FunPtr (BG.CInt -> BG.CInt -> IO ()))
hs_bindgen_957d483b5ca53592 =
  fmap BG.fromFFIType hs_bindgen_957d483b5ca53592_base

{-# NOINLINE args_int1 #-}
{-| __C declaration:__ @args_int1@

    __defined at:__ @macros\/reparse.h 25:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int1 :: BG.FunPtr (BG.CInt -> BG.CInt -> IO ())
args_int1 =
  BG.unsafePerformIO hs_bindgen_957d483b5ca53592

-- __unique:__ @test_macrosreparse_2_raw Example get_args_int2@
foreign import ccall unsafe "hs_bindgen_2d461e747155c900" hs_bindgen_2d461e747155c900_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_int2@
hs_bindgen_2d461e747155c900 :: IO (BG.FunPtr (BG.CInt -> BG.CInt -> IO ()))
hs_bindgen_2d461e747155c900 =
  fmap BG.fromFFIType hs_bindgen_2d461e747155c900_base

{-# NOINLINE args_int2 #-}
{-| __C declaration:__ @args_int2@

    __defined at:__ @macros\/reparse.h 26:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int2 :: BG.FunPtr (BG.CInt -> BG.CInt -> IO ())
args_int2 =
  BG.unsafePerformIO hs_bindgen_2d461e747155c900

-- __unique:__ @test_macrosreparse_2_raw Example get_args_int3@
foreign import ccall unsafe "hs_bindgen_221305a0eed40315" hs_bindgen_221305a0eed40315_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_int3@
hs_bindgen_221305a0eed40315 :: IO (BG.FunPtr (BG.CInt -> BG.CUInt -> IO ()))
hs_bindgen_221305a0eed40315 =
  fmap BG.fromFFIType hs_bindgen_221305a0eed40315_base

{-# NOINLINE args_int3 #-}
{-| __C declaration:__ @args_int3@

    __defined at:__ @macros\/reparse.h 27:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int3 :: BG.FunPtr (BG.CInt -> BG.CUInt -> IO ())
args_int3 =
  BG.unsafePerformIO hs_bindgen_221305a0eed40315

-- __unique:__ @test_macrosreparse_2_raw Example get_args_long1@
foreign import ccall unsafe "hs_bindgen_f972f7dfc04c4643" hs_bindgen_f972f7dfc04c4643_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_long1@
hs_bindgen_f972f7dfc04c4643 :: IO (BG.FunPtr (BG.CInt -> BG.CLong -> IO ()))
hs_bindgen_f972f7dfc04c4643 =
  fmap BG.fromFFIType hs_bindgen_f972f7dfc04c4643_base

{-# NOINLINE args_long1 #-}
{-| __C declaration:__ @args_long1@

    __defined at:__ @macros\/reparse.h 29:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long1 :: BG.FunPtr (BG.CInt -> BG.CLong -> IO ())
args_long1 =
  BG.unsafePerformIO hs_bindgen_f972f7dfc04c4643

-- __unique:__ @test_macrosreparse_2_raw Example get_args_long2@
foreign import ccall unsafe "hs_bindgen_1ea099fc0d64eaf0" hs_bindgen_1ea099fc0d64eaf0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_long2@
hs_bindgen_1ea099fc0d64eaf0 :: IO (BG.FunPtr (BG.CInt -> BG.CLong -> IO ()))
hs_bindgen_1ea099fc0d64eaf0 =
  fmap BG.fromFFIType hs_bindgen_1ea099fc0d64eaf0_base

{-# NOINLINE args_long2 #-}
{-| __C declaration:__ @args_long2@

    __defined at:__ @macros\/reparse.h 30:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long2 :: BG.FunPtr (BG.CInt -> BG.CLong -> IO ())
args_long2 =
  BG.unsafePerformIO hs_bindgen_1ea099fc0d64eaf0

-- __unique:__ @test_macrosreparse_2_raw Example get_args_long3@
foreign import ccall unsafe "hs_bindgen_4b98dcfcff8d2c16" hs_bindgen_4b98dcfcff8d2c16_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_long3@
hs_bindgen_4b98dcfcff8d2c16 :: IO (BG.FunPtr (BG.CInt -> BG.CULong -> IO ()))
hs_bindgen_4b98dcfcff8d2c16 =
  fmap BG.fromFFIType hs_bindgen_4b98dcfcff8d2c16_base

{-# NOINLINE args_long3 #-}
{-| __C declaration:__ @args_long3@

    __defined at:__ @macros\/reparse.h 31:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long3 :: BG.FunPtr (BG.CInt -> BG.CULong -> IO ())
args_long3 =
  BG.unsafePerformIO hs_bindgen_4b98dcfcff8d2c16

-- __unique:__ @test_macrosreparse_2_raw Example get_args_float@
foreign import ccall unsafe "hs_bindgen_c2667f55015837d5" hs_bindgen_c2667f55015837d5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_float@
hs_bindgen_c2667f55015837d5 :: IO (BG.FunPtr (BG.CInt -> BG.CFloat -> IO ()))
hs_bindgen_c2667f55015837d5 =
  fmap BG.fromFFIType hs_bindgen_c2667f55015837d5_base

{-# NOINLINE args_float #-}
{-| __C declaration:__ @args_float@

    __defined at:__ @macros\/reparse.h 33:6@

    __exported by:__ @macros\/reparse.h@
-}
args_float :: BG.FunPtr (BG.CInt -> BG.CFloat -> IO ())
args_float =
  BG.unsafePerformIO hs_bindgen_c2667f55015837d5

-- __unique:__ @test_macrosreparse_2_raw Example get_args_double@
foreign import ccall unsafe "hs_bindgen_0100fbb35f12b9ca" hs_bindgen_0100fbb35f12b9ca_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_double@
hs_bindgen_0100fbb35f12b9ca :: IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO ()))
hs_bindgen_0100fbb35f12b9ca =
  fmap BG.fromFFIType hs_bindgen_0100fbb35f12b9ca_base

{-# NOINLINE args_double #-}
{-| __C declaration:__ @args_double@

    __defined at:__ @macros\/reparse.h 34:6@

    __exported by:__ @macros\/reparse.h@
-}
args_double :: BG.FunPtr (BG.CInt -> BG.CDouble -> IO ())
args_double =
  BG.unsafePerformIO hs_bindgen_0100fbb35f12b9ca

-- __unique:__ @test_macrosreparse_2_raw Example get_args_bool1@
foreign import ccall unsafe "hs_bindgen_cca6089fee39d241" hs_bindgen_cca6089fee39d241_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_bool1@
hs_bindgen_cca6089fee39d241 :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_cca6089fee39d241 =
  fmap BG.fromFFIType hs_bindgen_cca6089fee39d241_base

{-# NOINLINE args_bool1 #-}
{-| __C declaration:__ @args_bool1@

    __defined at:__ @macros\/reparse.h 35:6@

    __exported by:__ @macros\/reparse.h@
-}
args_bool1 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
args_bool1 =
  BG.unsafePerformIO hs_bindgen_cca6089fee39d241

-- __unique:__ @test_macrosreparse_2_raw Example get_args_struct@
foreign import ccall unsafe "hs_bindgen_b33b3d1717b0a069" hs_bindgen_b33b3d1717b0a069_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_struct@
hs_bindgen_b33b3d1717b0a069 :: IO (BG.FunPtr (BG.CInt -> Some_struct -> IO ()))
hs_bindgen_b33b3d1717b0a069 =
  fmap BG.fromFFIType hs_bindgen_b33b3d1717b0a069_base

{-# NOINLINE args_struct #-}
{-| __C declaration:__ @args_struct@

    __defined at:__ @macros\/reparse.h 37:6@

    __exported by:__ @macros\/reparse.h@
-}
args_struct :: BG.FunPtr (BG.CInt -> Some_struct -> IO ())
args_struct =
  BG.unsafePerformIO hs_bindgen_b33b3d1717b0a069

-- __unique:__ @test_macrosreparse_2_raw Example get_args_union@
foreign import ccall unsafe "hs_bindgen_7f32b8254b45acce" hs_bindgen_7f32b8254b45acce_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_union@
hs_bindgen_7f32b8254b45acce :: IO (BG.FunPtr (BG.CInt -> Some_union -> IO ()))
hs_bindgen_7f32b8254b45acce =
  fmap BG.fromFFIType hs_bindgen_7f32b8254b45acce_base

{-# NOINLINE args_union #-}
{-| __C declaration:__ @args_union@

    __defined at:__ @macros\/reparse.h 38:6@

    __exported by:__ @macros\/reparse.h@
-}
args_union :: BG.FunPtr (BG.CInt -> Some_union -> IO ())
args_union =
  BG.unsafePerformIO hs_bindgen_7f32b8254b45acce

-- __unique:__ @test_macrosreparse_2_raw Example get_args_enum@
foreign import ccall unsafe "hs_bindgen_681a6f05ca44d076" hs_bindgen_681a6f05ca44d076_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_enum@
hs_bindgen_681a6f05ca44d076 :: IO (BG.FunPtr (BG.CInt -> Some_enum -> IO ()))
hs_bindgen_681a6f05ca44d076 =
  fmap BG.fromFFIType hs_bindgen_681a6f05ca44d076_base

{-# NOINLINE args_enum #-}
{-| __C declaration:__ @args_enum@

    __defined at:__ @macros\/reparse.h 39:6@

    __exported by:__ @macros\/reparse.h@
-}
args_enum :: BG.FunPtr (BG.CInt -> Some_enum -> IO ())
args_enum =
  BG.unsafePerformIO hs_bindgen_681a6f05ca44d076

-- __unique:__ @test_macrosreparse_2_raw Example get_args_pointer1@
foreign import ccall unsafe "hs_bindgen_53c78b7126a5c2d7" hs_bindgen_53c78b7126a5c2d7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_pointer1@
hs_bindgen_53c78b7126a5c2d7 :: IO (BG.FunPtr (BG.CInt -> BG.Ptr BG.CInt -> IO ()))
hs_bindgen_53c78b7126a5c2d7 =
  fmap BG.fromFFIType hs_bindgen_53c78b7126a5c2d7_base

{-# NOINLINE args_pointer1 #-}
{-| __C declaration:__ @args_pointer1@

    __defined at:__ @macros\/reparse.h 41:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer1 :: BG.FunPtr (BG.CInt -> BG.Ptr BG.CInt -> IO ())
args_pointer1 =
  BG.unsafePerformIO hs_bindgen_53c78b7126a5c2d7

-- __unique:__ @test_macrosreparse_2_raw Example get_args_pointer2@
foreign import ccall unsafe "hs_bindgen_fcc615fabdb3c7c8" hs_bindgen_fcc615fabdb3c7c8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_pointer2@
hs_bindgen_fcc615fabdb3c7c8 :: IO (BG.FunPtr (BG.CInt -> BG.Ptr (BG.Ptr BG.CInt) -> IO ()))
hs_bindgen_fcc615fabdb3c7c8 =
  fmap BG.fromFFIType hs_bindgen_fcc615fabdb3c7c8_base

{-# NOINLINE args_pointer2 #-}
{-| __C declaration:__ @args_pointer2@

    __defined at:__ @macros\/reparse.h 42:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer2 :: BG.FunPtr (BG.CInt -> BG.Ptr (BG.Ptr BG.CInt) -> IO ())
args_pointer2 =
  BG.unsafePerformIO hs_bindgen_fcc615fabdb3c7c8

-- __unique:__ @test_macrosreparse_2_raw Example get_args_pointer3@
foreign import ccall unsafe "hs_bindgen_84b5eb5d784e7d8b" hs_bindgen_84b5eb5d784e7d8b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_pointer3@
hs_bindgen_84b5eb5d784e7d8b :: IO (BG.FunPtr (BG.CInt -> BG.Ptr BG.Void -> IO ()))
hs_bindgen_84b5eb5d784e7d8b =
  fmap BG.fromFFIType hs_bindgen_84b5eb5d784e7d8b_base

{-# NOINLINE args_pointer3 #-}
{-| __C declaration:__ @args_pointer3@

    __defined at:__ @macros\/reparse.h 43:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer3 :: BG.FunPtr (BG.CInt -> BG.Ptr BG.Void -> IO ())
args_pointer3 =
  BG.unsafePerformIO hs_bindgen_84b5eb5d784e7d8b

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_A@
foreign import ccall unsafe "hs_bindgen_ab989b187ef104a6" hs_bindgen_ab989b187ef104a6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_A@
hs_bindgen_ab989b187ef104a6 :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_ab989b187ef104a6 =
  fmap BG.fromFFIType hs_bindgen_ab989b187ef104a6_base

{-# NOINLINE ret_A #-}
{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h 47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A :: BG.FunPtr (IO BG.CInt)
ret_A =
  BG.unsafePerformIO hs_bindgen_ab989b187ef104a6

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_char1@
foreign import ccall unsafe "hs_bindgen_1dade4ecdbb7d1e3" hs_bindgen_1dade4ecdbb7d1e3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_char1@
hs_bindgen_1dade4ecdbb7d1e3 :: IO (BG.FunPtr (BG.CInt -> IO BG.CChar))
hs_bindgen_1dade4ecdbb7d1e3 =
  fmap BG.fromFFIType hs_bindgen_1dade4ecdbb7d1e3_base

{-# NOINLINE ret_char1 #-}
{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h 49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1 :: BG.FunPtr (BG.CInt -> IO BG.CChar)
ret_char1 =
  BG.unsafePerformIO hs_bindgen_1dade4ecdbb7d1e3

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_char2@
foreign import ccall unsafe "hs_bindgen_1b8112cb53e3ee13" hs_bindgen_1b8112cb53e3ee13_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_char2@
hs_bindgen_1b8112cb53e3ee13 :: IO (BG.FunPtr (BG.CInt -> IO BG.CSChar))
hs_bindgen_1b8112cb53e3ee13 =
  fmap BG.fromFFIType hs_bindgen_1b8112cb53e3ee13_base

{-# NOINLINE ret_char2 #-}
{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h 50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2 :: BG.FunPtr (BG.CInt -> IO BG.CSChar)
ret_char2 =
  BG.unsafePerformIO hs_bindgen_1b8112cb53e3ee13

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_char3@
foreign import ccall unsafe "hs_bindgen_3193a0bc99b2392e" hs_bindgen_3193a0bc99b2392e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_char3@
hs_bindgen_3193a0bc99b2392e :: IO (BG.FunPtr (BG.CInt -> IO BG.CUChar))
hs_bindgen_3193a0bc99b2392e =
  fmap BG.fromFFIType hs_bindgen_3193a0bc99b2392e_base

{-# NOINLINE ret_char3 #-}
{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h 51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3 :: BG.FunPtr (BG.CInt -> IO BG.CUChar)
ret_char3 =
  BG.unsafePerformIO hs_bindgen_3193a0bc99b2392e

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_short1@
foreign import ccall unsafe "hs_bindgen_601176fde433c8c8" hs_bindgen_601176fde433c8c8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_short1@
hs_bindgen_601176fde433c8c8 :: IO (BG.FunPtr (BG.CInt -> IO BG.CShort))
hs_bindgen_601176fde433c8c8 =
  fmap BG.fromFFIType hs_bindgen_601176fde433c8c8_base

{-# NOINLINE ret_short1 #-}
{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h 53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1 :: BG.FunPtr (BG.CInt -> IO BG.CShort)
ret_short1 =
  BG.unsafePerformIO hs_bindgen_601176fde433c8c8

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_short2@
foreign import ccall unsafe "hs_bindgen_9fa8533d04e7f294" hs_bindgen_9fa8533d04e7f294_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_short2@
hs_bindgen_9fa8533d04e7f294 :: IO (BG.FunPtr (BG.CInt -> IO BG.CShort))
hs_bindgen_9fa8533d04e7f294 =
  fmap BG.fromFFIType hs_bindgen_9fa8533d04e7f294_base

{-# NOINLINE ret_short2 #-}
{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h 54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2 :: BG.FunPtr (BG.CInt -> IO BG.CShort)
ret_short2 =
  BG.unsafePerformIO hs_bindgen_9fa8533d04e7f294

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_short3@
foreign import ccall unsafe "hs_bindgen_a9afff97ffaea466" hs_bindgen_a9afff97ffaea466_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_short3@
hs_bindgen_a9afff97ffaea466 :: IO (BG.FunPtr (BG.CInt -> IO BG.CUShort))
hs_bindgen_a9afff97ffaea466 =
  fmap BG.fromFFIType hs_bindgen_a9afff97ffaea466_base

{-# NOINLINE ret_short3 #-}
{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h 55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3 :: BG.FunPtr (BG.CInt -> IO BG.CUShort)
ret_short3 =
  BG.unsafePerformIO hs_bindgen_a9afff97ffaea466

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_int1@
foreign import ccall unsafe "hs_bindgen_fc146923a35d31bd" hs_bindgen_fc146923a35d31bd_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_int1@
hs_bindgen_fc146923a35d31bd :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_fc146923a35d31bd =
  fmap BG.fromFFIType hs_bindgen_fc146923a35d31bd_base

{-# NOINLINE ret_int1 #-}
{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h 57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1 :: BG.FunPtr (BG.CInt -> IO BG.CInt)
ret_int1 =
  BG.unsafePerformIO hs_bindgen_fc146923a35d31bd

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_int2@
foreign import ccall unsafe "hs_bindgen_dabf136faf39eb12" hs_bindgen_dabf136faf39eb12_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_int2@
hs_bindgen_dabf136faf39eb12 :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_dabf136faf39eb12 =
  fmap BG.fromFFIType hs_bindgen_dabf136faf39eb12_base

{-# NOINLINE ret_int2 #-}
{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h 58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2 :: BG.FunPtr (BG.CInt -> IO BG.CInt)
ret_int2 =
  BG.unsafePerformIO hs_bindgen_dabf136faf39eb12

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_int3@
foreign import ccall unsafe "hs_bindgen_8cf89eb76d70daab" hs_bindgen_8cf89eb76d70daab_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_int3@
hs_bindgen_8cf89eb76d70daab :: IO (BG.FunPtr (BG.CInt -> IO BG.CUInt))
hs_bindgen_8cf89eb76d70daab =
  fmap BG.fromFFIType hs_bindgen_8cf89eb76d70daab_base

{-# NOINLINE ret_int3 #-}
{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h 59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3 :: BG.FunPtr (BG.CInt -> IO BG.CUInt)
ret_int3 =
  BG.unsafePerformIO hs_bindgen_8cf89eb76d70daab

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_long1@
foreign import ccall unsafe "hs_bindgen_793c67e678975ced" hs_bindgen_793c67e678975ced_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_long1@
hs_bindgen_793c67e678975ced :: IO (BG.FunPtr (BG.CInt -> IO BG.CLong))
hs_bindgen_793c67e678975ced =
  fmap BG.fromFFIType hs_bindgen_793c67e678975ced_base

{-# NOINLINE ret_long1 #-}
{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h 61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1 :: BG.FunPtr (BG.CInt -> IO BG.CLong)
ret_long1 =
  BG.unsafePerformIO hs_bindgen_793c67e678975ced

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_long2@
foreign import ccall unsafe "hs_bindgen_65e1ed59653c64df" hs_bindgen_65e1ed59653c64df_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_long2@
hs_bindgen_65e1ed59653c64df :: IO (BG.FunPtr (BG.CInt -> IO BG.CLong))
hs_bindgen_65e1ed59653c64df =
  fmap BG.fromFFIType hs_bindgen_65e1ed59653c64df_base

{-# NOINLINE ret_long2 #-}
{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h 62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2 :: BG.FunPtr (BG.CInt -> IO BG.CLong)
ret_long2 =
  BG.unsafePerformIO hs_bindgen_65e1ed59653c64df

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_long3@
foreign import ccall unsafe "hs_bindgen_9392027014182bb3" hs_bindgen_9392027014182bb3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_long3@
hs_bindgen_9392027014182bb3 :: IO (BG.FunPtr (BG.CInt -> IO BG.CULong))
hs_bindgen_9392027014182bb3 =
  fmap BG.fromFFIType hs_bindgen_9392027014182bb3_base

{-# NOINLINE ret_long3 #-}
{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h 63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3 :: BG.FunPtr (BG.CInt -> IO BG.CULong)
ret_long3 =
  BG.unsafePerformIO hs_bindgen_9392027014182bb3

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_float@
foreign import ccall unsafe "hs_bindgen_6dfef4080131cd2b" hs_bindgen_6dfef4080131cd2b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_float@
hs_bindgen_6dfef4080131cd2b :: IO (BG.FunPtr (BG.CInt -> IO BG.CFloat))
hs_bindgen_6dfef4080131cd2b =
  fmap BG.fromFFIType hs_bindgen_6dfef4080131cd2b_base

{-# NOINLINE ret_float #-}
{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h 65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float :: BG.FunPtr (BG.CInt -> IO BG.CFloat)
ret_float =
  BG.unsafePerformIO hs_bindgen_6dfef4080131cd2b

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_double@
foreign import ccall unsafe "hs_bindgen_aa07aa565d467d6c" hs_bindgen_aa07aa565d467d6c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_double@
hs_bindgen_aa07aa565d467d6c :: IO (BG.FunPtr (BG.CInt -> IO BG.CDouble))
hs_bindgen_aa07aa565d467d6c =
  fmap BG.fromFFIType hs_bindgen_aa07aa565d467d6c_base

{-# NOINLINE ret_double #-}
{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h 66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double :: BG.FunPtr (BG.CInt -> IO BG.CDouble)
ret_double =
  BG.unsafePerformIO hs_bindgen_aa07aa565d467d6c

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_bool1@
foreign import ccall unsafe "hs_bindgen_896dc5f00ec1c288" hs_bindgen_896dc5f00ec1c288_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_bool1@
hs_bindgen_896dc5f00ec1c288 :: IO (BG.FunPtr (BG.CInt -> IO BG.CBool))
hs_bindgen_896dc5f00ec1c288 =
  fmap BG.fromFFIType hs_bindgen_896dc5f00ec1c288_base

{-# NOINLINE ret_bool1 #-}
{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h 67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1 :: BG.FunPtr (BG.CInt -> IO BG.CBool)
ret_bool1 =
  BG.unsafePerformIO hs_bindgen_896dc5f00ec1c288

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_struct@
foreign import ccall unsafe "hs_bindgen_388388c8aa13ac36" hs_bindgen_388388c8aa13ac36_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_struct@
hs_bindgen_388388c8aa13ac36 :: IO (BG.FunPtr (BG.CInt -> IO Some_struct))
hs_bindgen_388388c8aa13ac36 =
  fmap BG.fromFFIType hs_bindgen_388388c8aa13ac36_base

{-# NOINLINE ret_struct #-}
{-| __C declaration:__ @ret_struct@

    __defined at:__ @macros\/reparse.h 69:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_struct :: BG.FunPtr (BG.CInt -> IO Some_struct)
ret_struct =
  BG.unsafePerformIO hs_bindgen_388388c8aa13ac36

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_union@
foreign import ccall unsafe "hs_bindgen_aa77e44e5e219123" hs_bindgen_aa77e44e5e219123_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_union@
hs_bindgen_aa77e44e5e219123 :: IO (BG.FunPtr (BG.CInt -> IO Some_union))
hs_bindgen_aa77e44e5e219123 =
  fmap BG.fromFFIType hs_bindgen_aa77e44e5e219123_base

{-# NOINLINE ret_union #-}
{-| __C declaration:__ @ret_union@

    __defined at:__ @macros\/reparse.h 70:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_union :: BG.FunPtr (BG.CInt -> IO Some_union)
ret_union =
  BG.unsafePerformIO hs_bindgen_aa77e44e5e219123

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_enum@
foreign import ccall unsafe "hs_bindgen_c2cf0eacb5266177" hs_bindgen_c2cf0eacb5266177_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_enum@
hs_bindgen_c2cf0eacb5266177 :: IO (BG.FunPtr (BG.CInt -> IO Some_enum))
hs_bindgen_c2cf0eacb5266177 =
  fmap BG.fromFFIType hs_bindgen_c2cf0eacb5266177_base

{-# NOINLINE ret_enum #-}
{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h 71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum :: BG.FunPtr (BG.CInt -> IO Some_enum)
ret_enum =
  BG.unsafePerformIO hs_bindgen_c2cf0eacb5266177

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_pointer1@
foreign import ccall unsafe "hs_bindgen_e9d880a4a462ef0a" hs_bindgen_e9d880a4a462ef0a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_pointer1@
hs_bindgen_e9d880a4a462ef0a :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt)))
hs_bindgen_e9d880a4a462ef0a =
  fmap BG.fromFFIType hs_bindgen_e9d880a4a462ef0a_base

{-# NOINLINE ret_pointer1 #-}
{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h 73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt))
ret_pointer1 =
  BG.unsafePerformIO hs_bindgen_e9d880a4a462ef0a

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_pointer2@
foreign import ccall unsafe "hs_bindgen_50e9228b4d949c87" hs_bindgen_50e9228b4d949c87_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_pointer2@
hs_bindgen_50e9228b4d949c87 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr BG.CInt))))
hs_bindgen_50e9228b4d949c87 =
  fmap BG.fromFFIType hs_bindgen_50e9228b4d949c87_base

{-# NOINLINE ret_pointer2 #-}
{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h 74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr BG.CInt)))
ret_pointer2 =
  BG.unsafePerformIO hs_bindgen_50e9228b4d949c87

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_pointer3@
foreign import ccall unsafe "hs_bindgen_ec736415852addfa" hs_bindgen_ec736415852addfa_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_pointer3@
hs_bindgen_ec736415852addfa :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.Void)))
hs_bindgen_ec736415852addfa =
  fmap BG.fromFFIType hs_bindgen_ec736415852addfa_base

{-# NOINLINE ret_pointer3 #-}
{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h 75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.Void))
ret_pointer3 =
  BG.unsafePerformIO hs_bindgen_ec736415852addfa

-- __unique:__ @test_macrosreparse_2_raw Example get_body1@
foreign import ccall unsafe "hs_bindgen_8acc5f80130dd67d" hs_bindgen_8acc5f80130dd67d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_body1@
hs_bindgen_8acc5f80130dd67d :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_8acc5f80130dd67d =
  fmap BG.fromFFIType hs_bindgen_8acc5f80130dd67d_base

{-# NOINLINE body1 #-}
{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h 79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1 :: BG.FunPtr (BG.CInt -> IO BG.CInt)
body1 =
  BG.unsafePerformIO hs_bindgen_8acc5f80130dd67d

-- __unique:__ @test_macrosreparse_2_raw Example get_body2@
foreign import ccall unsafe "hs_bindgen_23201b1ad7870002" hs_bindgen_23201b1ad7870002_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_body2@
hs_bindgen_23201b1ad7870002 :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_23201b1ad7870002 =
  fmap BG.fromFFIType hs_bindgen_23201b1ad7870002_base

{-# NOINLINE body2 #-}
{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h 80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2 :: BG.FunPtr (IO BG.CInt)
body2 =
  BG.unsafePerformIO hs_bindgen_23201b1ad7870002

-- __unique:__ @test_macrosreparse_2_raw Example get_args_complex_float@
foreign import ccall unsafe "hs_bindgen_f869d2a6acf014be" hs_bindgen_f869d2a6acf014be_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_complex_float@
hs_bindgen_f869d2a6acf014be :: IO (BG.FunPtr (BG.CInt -> BG.Complex BG.CFloat -> IO ()))
hs_bindgen_f869d2a6acf014be =
  fmap BG.fromFFIType hs_bindgen_f869d2a6acf014be_base

{-# NOINLINE args_complex_float #-}
{-| __C declaration:__ @args_complex_float@

    __defined at:__ @macros\/reparse.h 84:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_float :: BG.FunPtr (BG.CInt -> BG.Complex BG.CFloat -> IO ())
args_complex_float =
  BG.unsafePerformIO hs_bindgen_f869d2a6acf014be

-- __unique:__ @test_macrosreparse_2_raw Example get_args_complex_double@
foreign import ccall unsafe "hs_bindgen_afc936a14b9315fc" hs_bindgen_afc936a14b9315fc_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_args_complex_double@
hs_bindgen_afc936a14b9315fc :: IO (BG.FunPtr (BG.CInt -> BG.Complex BG.CDouble -> IO ()))
hs_bindgen_afc936a14b9315fc =
  fmap BG.fromFFIType hs_bindgen_afc936a14b9315fc_base

{-# NOINLINE args_complex_double #-}
{-| __C declaration:__ @args_complex_double@

    __defined at:__ @macros\/reparse.h 85:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_double :: BG.FunPtr (BG.CInt -> BG.Complex BG.CDouble -> IO ())
args_complex_double =
  BG.unsafePerformIO hs_bindgen_afc936a14b9315fc

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_complex_float@
foreign import ccall unsafe "hs_bindgen_63752d3ec6f51e06" hs_bindgen_63752d3ec6f51e06_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_complex_float@
hs_bindgen_63752d3ec6f51e06 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Complex BG.CFloat)))
hs_bindgen_63752d3ec6f51e06 =
  fmap BG.fromFFIType hs_bindgen_63752d3ec6f51e06_base

{-# NOINLINE ret_complex_float #-}
{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @macros\/reparse.h 86:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_float :: BG.FunPtr (BG.CInt -> IO (BG.Complex BG.CFloat))
ret_complex_float =
  BG.unsafePerformIO hs_bindgen_63752d3ec6f51e06

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_complex_double@
foreign import ccall unsafe "hs_bindgen_310cc703a6a6bafb" hs_bindgen_310cc703a6a6bafb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_ret_complex_double@
hs_bindgen_310cc703a6a6bafb :: IO (BG.FunPtr (BG.CInt -> IO (BG.Complex BG.CDouble)))
hs_bindgen_310cc703a6a6bafb =
  fmap BG.fromFFIType hs_bindgen_310cc703a6a6bafb_base

{-# NOINLINE ret_complex_double #-}
{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @macros\/reparse.h 87:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_double :: BG.FunPtr (BG.CInt -> IO (BG.Complex BG.CDouble))
ret_complex_double =
  BG.unsafePerformIO hs_bindgen_310cc703a6a6bafb

-- __unique:__ @test_macrosreparse_2_raw Example get_bespoke_args1@
foreign import ccall unsafe "hs_bindgen_0e0e4765fc564a12" hs_bindgen_0e0e4765fc564a12_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_bespoke_args1@
hs_bindgen_0e0e4765fc564a12 :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_0e0e4765fc564a12 =
  fmap BG.fromFFIType hs_bindgen_0e0e4765fc564a12_base

{-# NOINLINE bespoke_args1 #-}
{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @macros\/reparse.h 94:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args1 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
bespoke_args1 =
  BG.unsafePerformIO hs_bindgen_0e0e4765fc564a12

-- __unique:__ @test_macrosreparse_2_raw Example get_bespoke_args2@
foreign import ccall unsafe "hs_bindgen_5bfacc5b73ae16d8" hs_bindgen_5bfacc5b73ae16d8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_bespoke_args2@
hs_bindgen_5bfacc5b73ae16d8 :: IO (BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_5bfacc5b73ae16d8 =
  fmap BG.fromFFIType hs_bindgen_5bfacc5b73ae16d8_base

{-# NOINLINE bespoke_args2 #-}
{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @macros\/reparse.h 95:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args2 :: BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ())
bespoke_args2 =
  BG.unsafePerformIO hs_bindgen_5bfacc5b73ae16d8

-- __unique:__ @test_macrosreparse_2_raw Example get_bespoke_ret1@
foreign import ccall unsafe "hs_bindgen_d07549f88c995df7" hs_bindgen_d07549f88c995df7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_bespoke_ret1@
hs_bindgen_d07549f88c995df7 :: IO (BG.FunPtr (BG.CInt -> IO BG.CBool))
hs_bindgen_d07549f88c995df7 =
  fmap BG.fromFFIType hs_bindgen_d07549f88c995df7_base

{-# NOINLINE bespoke_ret1 #-}
{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h 97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1 :: BG.FunPtr (BG.CInt -> IO BG.CBool)
bespoke_ret1 =
  BG.unsafePerformIO hs_bindgen_d07549f88c995df7

-- __unique:__ @test_macrosreparse_2_raw Example get_bespoke_ret2@
foreign import ccall unsafe "hs_bindgen_8b11c06b40a9c3c2" hs_bindgen_8b11c06b40a9c3c2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_bespoke_ret2@
hs_bindgen_8b11c06b40a9c3c2 :: IO (BG.FunPtr (BG.CInt -> IO HsBindgen.Runtime.LibC.CSize))
hs_bindgen_8b11c06b40a9c3c2 =
  fmap BG.fromFFIType hs_bindgen_8b11c06b40a9c3c2_base

{-# NOINLINE bespoke_ret2 #-}
{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h 98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2 :: BG.FunPtr (BG.CInt -> IO HsBindgen.Runtime.LibC.CSize)
bespoke_ret2 =
  BG.unsafePerformIO hs_bindgen_8b11c06b40a9c3c2

-- __unique:__ @test_macrosreparse_2_raw Example get_arr_args1@
foreign import ccall unsafe "hs_bindgen_45247f6fba0c5dbf" hs_bindgen_45247f6fba0c5dbf_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_arr_args1@
hs_bindgen_45247f6fba0c5dbf :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO ()))
hs_bindgen_45247f6fba0c5dbf =
  fmap BG.fromFFIType hs_bindgen_45247f6fba0c5dbf_base

{-# NOINLINE arr_args1 #-}
{-| Arrays

    __C declaration:__ @arr_args1@

    __defined at:__ @macros\/reparse.h 104:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args1 :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO ())
arr_args1 =
  BG.unsafePerformIO hs_bindgen_45247f6fba0c5dbf

-- __unique:__ @test_macrosreparse_2_raw Example get_arr_args2@
foreign import ccall unsafe "hs_bindgen_3182f9011daf4358" hs_bindgen_3182f9011daf4358_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_arr_args2@
hs_bindgen_3182f9011daf4358 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt))) -> IO ()))
hs_bindgen_3182f9011daf4358 =
  fmap BG.fromFFIType hs_bindgen_3182f9011daf4358_base

{-# NOINLINE arr_args2 #-}
{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h 105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2 :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt))) -> IO ())
arr_args2 =
  BG.unsafePerformIO hs_bindgen_3182f9011daf4358

-- __unique:__ @test_macrosreparse_2_raw Example get_arr_args3@
foreign import ccall unsafe "hs_bindgen_00feb5ae4554f46c" hs_bindgen_00feb5ae4554f46c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_arr_args3@
hs_bindgen_00feb5ae4554f46c :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt)) -> IO ()))
hs_bindgen_00feb5ae4554f46c =
  fmap BG.fromFFIType hs_bindgen_00feb5ae4554f46c_base

{-# NOINLINE arr_args3 #-}
{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h 106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3 :: BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt)) -> IO ())
arr_args3 =
  BG.unsafePerformIO hs_bindgen_00feb5ae4554f46c

-- __unique:__ @test_macrosreparse_2_raw Example get_arr_args4@
foreign import ccall unsafe "hs_bindgen_ea4721eb51a52046" hs_bindgen_ea4721eb51a52046_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_arr_args4@
hs_bindgen_ea4721eb51a52046 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt))) -> IO ()))
hs_bindgen_ea4721eb51a52046 =
  fmap BG.fromFFIType hs_bindgen_ea4721eb51a52046_base

{-# NOINLINE arr_args4 #-}
{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h 107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4 :: BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt))) -> IO ())
arr_args4 =
  BG.unsafePerformIO hs_bindgen_ea4721eb51a52046

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_args1@
foreign import ccall unsafe "hs_bindgen_75a197890a492f5d" hs_bindgen_75a197890a492f5d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_args1@
hs_bindgen_75a197890a492f5d :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (IO ()) -> IO ()))
hs_bindgen_75a197890a492f5d =
  fmap BG.fromFFIType hs_bindgen_75a197890a492f5d_base

{-# NOINLINE funptr_args1 #-}
{-| Function pointers

    __C declaration:__ @funptr_args1@

    __defined at:__ @macros\/reparse.h 126:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args1 :: BG.FunPtr (BG.CInt -> BG.FunPtr (IO ()) -> IO ())
funptr_args1 =
  BG.unsafePerformIO hs_bindgen_75a197890a492f5d

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_args2@
foreign import ccall unsafe "hs_bindgen_602bd65e56705bb6" hs_bindgen_602bd65e56705bb6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_args2@
hs_bindgen_602bd65e56705bb6 :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (IO BG.CInt) -> IO ()))
hs_bindgen_602bd65e56705bb6 =
  fmap BG.fromFFIType hs_bindgen_602bd65e56705bb6_base

{-# NOINLINE funptr_args2 #-}
{-| __C declaration:__ @funptr_args2@

    __defined at:__ @macros\/reparse.h 127:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args2 :: BG.FunPtr (BG.CInt -> BG.FunPtr (IO BG.CInt) -> IO ())
funptr_args2 =
  BG.unsafePerformIO hs_bindgen_602bd65e56705bb6

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_args3@
foreign import ccall unsafe "hs_bindgen_8300cda6204eb7b5" hs_bindgen_8300cda6204eb7b5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_args3@
hs_bindgen_8300cda6204eb7b5 :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> IO ()) -> IO ()))
hs_bindgen_8300cda6204eb7b5 =
  fmap BG.fromFFIType hs_bindgen_8300cda6204eb7b5_base

{-# NOINLINE funptr_args3 #-}
{-| __C declaration:__ @funptr_args3@

    __defined at:__ @macros\/reparse.h 128:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args3 :: BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> IO ()) -> IO ())
funptr_args3 =
  BG.unsafePerformIO hs_bindgen_8300cda6204eb7b5

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_args4@
foreign import ccall unsafe "hs_bindgen_b793c7faf25d1d9c" hs_bindgen_b793c7faf25d1d9c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_args4@
hs_bindgen_b793c7faf25d1d9c :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar) -> IO ()))
hs_bindgen_b793c7faf25d1d9c =
  fmap BG.fromFFIType hs_bindgen_b793c7faf25d1d9c_base

{-# NOINLINE funptr_args4 #-}
{-| __C declaration:__ @funptr_args4@

    __defined at:__ @macros\/reparse.h 129:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args4 :: BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar) -> IO ())
funptr_args4 =
  BG.unsafePerformIO hs_bindgen_b793c7faf25d1d9c

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_args5@
foreign import ccall unsafe "hs_bindgen_4f52cc41edcd129e" hs_bindgen_4f52cc41edcd129e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_args5@
hs_bindgen_4f52cc41edcd129e :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)) -> IO ()))
hs_bindgen_4f52cc41edcd129e =
  fmap BG.fromFFIType hs_bindgen_4f52cc41edcd129e_base

{-# NOINLINE funptr_args5 #-}
{-| __C declaration:__ @funptr_args5@

    __defined at:__ @macros\/reparse.h 130:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args5 :: BG.FunPtr (BG.CInt -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)) -> IO ())
funptr_args5 =
  BG.unsafePerformIO hs_bindgen_4f52cc41edcd129e

-- __unique:__ @test_macrosreparse_2_raw Example get_comments1@
foreign import ccall unsafe "hs_bindgen_eeda8a79d3f2dafa" hs_bindgen_eeda8a79d3f2dafa_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_comments1@
hs_bindgen_eeda8a79d3f2dafa :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_eeda8a79d3f2dafa =
  fmap BG.fromFFIType hs_bindgen_eeda8a79d3f2dafa_base

{-# NOINLINE comments1 #-}
{-| Comments in awkward places

    (Prior to language-c we failed to parse there.)

    __C declaration:__ @comments1@

    __defined at:__ @macros\/reparse.h 144:25@

    __exported by:__ @macros\/reparse.h@
-}
comments1 :: BG.FunPtr (BG.CInt -> IO ())
comments1 =
  BG.unsafePerformIO hs_bindgen_eeda8a79d3f2dafa

-- __unique:__ @test_macrosreparse_2_raw Example get_const_prim_before1@
foreign import ccall unsafe "hs_bindgen_172430e48f221c1b" hs_bindgen_172430e48f221c1b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_prim_before1@
hs_bindgen_172430e48f221c1b :: IO (BG.FunPtr (BG.CInt -> BG.CChar -> IO ()))
hs_bindgen_172430e48f221c1b =
  fmap BG.fromFFIType hs_bindgen_172430e48f221c1b_base

{-# NOINLINE const_prim_before1 #-}
{-| @const@ qualifier

    NOTE: These were not parsed correctly prior to the switch to language-c.

    __C declaration:__ @const_prim_before1@

    __defined at:__ @macros\/reparse.h 177:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before1 :: BG.FunPtr (BG.CInt -> BG.CChar -> IO ())
const_prim_before1 =
  BG.unsafePerformIO hs_bindgen_172430e48f221c1b

-- __unique:__ @test_macrosreparse_2_raw Example get_const_prim_before2@
foreign import ccall unsafe "hs_bindgen_e1ef4ef42c2f4706" hs_bindgen_e1ef4ef42c2f4706_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_prim_before2@
hs_bindgen_e1ef4ef42c2f4706 :: IO (BG.FunPtr (BG.CInt -> BG.CSChar -> IO ()))
hs_bindgen_e1ef4ef42c2f4706 =
  fmap BG.fromFFIType hs_bindgen_e1ef4ef42c2f4706_base

{-# NOINLINE const_prim_before2 #-}
{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @macros\/reparse.h 178:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before2 :: BG.FunPtr (BG.CInt -> BG.CSChar -> IO ())
const_prim_before2 =
  BG.unsafePerformIO hs_bindgen_e1ef4ef42c2f4706

-- __unique:__ @test_macrosreparse_2_raw Example get_const_prim_before3@
foreign import ccall unsafe "hs_bindgen_482f7f9384e16fdc" hs_bindgen_482f7f9384e16fdc_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_prim_before3@
hs_bindgen_482f7f9384e16fdc :: IO (BG.FunPtr (BG.CInt -> BG.CUChar -> IO ()))
hs_bindgen_482f7f9384e16fdc =
  fmap BG.fromFFIType hs_bindgen_482f7f9384e16fdc_base

{-# NOINLINE const_prim_before3 #-}
{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @macros\/reparse.h 179:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before3 :: BG.FunPtr (BG.CInt -> BG.CUChar -> IO ())
const_prim_before3 =
  BG.unsafePerformIO hs_bindgen_482f7f9384e16fdc

-- __unique:__ @test_macrosreparse_2_raw Example get_const_prim_after1@
foreign import ccall unsafe "hs_bindgen_7877d994c033a25a" hs_bindgen_7877d994c033a25a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_prim_after1@
hs_bindgen_7877d994c033a25a :: IO (BG.FunPtr (BG.CInt -> BG.CChar -> IO ()))
hs_bindgen_7877d994c033a25a =
  fmap BG.fromFFIType hs_bindgen_7877d994c033a25a_base

{-# NOINLINE const_prim_after1 #-}
{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @macros\/reparse.h 180:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after1 :: BG.FunPtr (BG.CInt -> BG.CChar -> IO ())
const_prim_after1 =
  BG.unsafePerformIO hs_bindgen_7877d994c033a25a

-- __unique:__ @test_macrosreparse_2_raw Example get_const_prim_after2@
foreign import ccall unsafe "hs_bindgen_937c8f934f60558a" hs_bindgen_937c8f934f60558a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_prim_after2@
hs_bindgen_937c8f934f60558a :: IO (BG.FunPtr (BG.CInt -> BG.CSChar -> IO ()))
hs_bindgen_937c8f934f60558a =
  fmap BG.fromFFIType hs_bindgen_937c8f934f60558a_base

{-# NOINLINE const_prim_after2 #-}
{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @macros\/reparse.h 181:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after2 :: BG.FunPtr (BG.CInt -> BG.CSChar -> IO ())
const_prim_after2 =
  BG.unsafePerformIO hs_bindgen_937c8f934f60558a

-- __unique:__ @test_macrosreparse_2_raw Example get_const_prim_after3@
foreign import ccall unsafe "hs_bindgen_e694840b63a92b11" hs_bindgen_e694840b63a92b11_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_prim_after3@
hs_bindgen_e694840b63a92b11 :: IO (BG.FunPtr (BG.CInt -> BG.CUChar -> IO ()))
hs_bindgen_e694840b63a92b11 =
  fmap BG.fromFFIType hs_bindgen_e694840b63a92b11_base

{-# NOINLINE const_prim_after3 #-}
{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @macros\/reparse.h 182:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after3 :: BG.FunPtr (BG.CInt -> BG.CUChar -> IO ())
const_prim_after3 =
  BG.unsafePerformIO hs_bindgen_e694840b63a92b11

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_before1@
foreign import ccall unsafe "hs_bindgen_1c3fc4f799449f13" hs_bindgen_1c3fc4f799449f13_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_before1@
hs_bindgen_1c3fc4f799449f13 :: IO (BG.FunPtr (BG.CInt -> BG.CFloat -> IO ()))
hs_bindgen_1c3fc4f799449f13 =
  fmap BG.fromFFIType hs_bindgen_1c3fc4f799449f13_base

{-# NOINLINE const_withoutSign_before1 #-}
{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @macros\/reparse.h 186:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before1 :: BG.FunPtr (BG.CInt -> BG.CFloat -> IO ())
const_withoutSign_before1 =
  BG.unsafePerformIO hs_bindgen_1c3fc4f799449f13

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_before2@
foreign import ccall unsafe "hs_bindgen_752a8e3a0ea5d652" hs_bindgen_752a8e3a0ea5d652_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_before2@
hs_bindgen_752a8e3a0ea5d652 :: IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO ()))
hs_bindgen_752a8e3a0ea5d652 =
  fmap BG.fromFFIType hs_bindgen_752a8e3a0ea5d652_base

{-# NOINLINE const_withoutSign_before2 #-}
{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @macros\/reparse.h 187:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before2 :: BG.FunPtr (BG.CInt -> BG.CDouble -> IO ())
const_withoutSign_before2 =
  BG.unsafePerformIO hs_bindgen_752a8e3a0ea5d652

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_before3@
foreign import ccall unsafe "hs_bindgen_9b101d119538264b" hs_bindgen_9b101d119538264b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_before3@
hs_bindgen_9b101d119538264b :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_9b101d119538264b =
  fmap BG.fromFFIType hs_bindgen_9b101d119538264b_base

{-# NOINLINE const_withoutSign_before3 #-}
{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @macros\/reparse.h 188:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before3 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
const_withoutSign_before3 =
  BG.unsafePerformIO hs_bindgen_9b101d119538264b

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_before4@
foreign import ccall unsafe "hs_bindgen_bc2f7744c57f8954" hs_bindgen_bc2f7744c57f8954_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_before4@
hs_bindgen_bc2f7744c57f8954 :: IO (BG.FunPtr (BG.CInt -> Some_struct -> IO ()))
hs_bindgen_bc2f7744c57f8954 =
  fmap BG.fromFFIType hs_bindgen_bc2f7744c57f8954_base

{-# NOINLINE const_withoutSign_before4 #-}
{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @macros\/reparse.h 189:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before4 :: BG.FunPtr (BG.CInt -> Some_struct -> IO ())
const_withoutSign_before4 =
  BG.unsafePerformIO hs_bindgen_bc2f7744c57f8954

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_before5@
foreign import ccall unsafe "hs_bindgen_6c667d3b0c229045" hs_bindgen_6c667d3b0c229045_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_before5@
hs_bindgen_6c667d3b0c229045 :: IO (BG.FunPtr (BG.CInt -> Some_union -> IO ()))
hs_bindgen_6c667d3b0c229045 =
  fmap BG.fromFFIType hs_bindgen_6c667d3b0c229045_base

{-# NOINLINE const_withoutSign_before5 #-}
{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @macros\/reparse.h 190:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before5 :: BG.FunPtr (BG.CInt -> Some_union -> IO ())
const_withoutSign_before5 =
  BG.unsafePerformIO hs_bindgen_6c667d3b0c229045

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_before6@
foreign import ccall unsafe "hs_bindgen_97f2af9061b1d404" hs_bindgen_97f2af9061b1d404_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_before6@
hs_bindgen_97f2af9061b1d404 :: IO (BG.FunPtr (BG.CInt -> Some_enum -> IO ()))
hs_bindgen_97f2af9061b1d404 =
  fmap BG.fromFFIType hs_bindgen_97f2af9061b1d404_base

{-# NOINLINE const_withoutSign_before6 #-}
{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @macros\/reparse.h 191:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before6 :: BG.FunPtr (BG.CInt -> Some_enum -> IO ())
const_withoutSign_before6 =
  BG.unsafePerformIO hs_bindgen_97f2af9061b1d404

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_before7@
foreign import ccall unsafe "hs_bindgen_cdbdbfa038d903e1" hs_bindgen_cdbdbfa038d903e1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_before7@
hs_bindgen_cdbdbfa038d903e1 :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_cdbdbfa038d903e1 =
  fmap BG.fromFFIType hs_bindgen_cdbdbfa038d903e1_base

{-# NOINLINE const_withoutSign_before7 #-}
{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @macros\/reparse.h 192:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before7 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
const_withoutSign_before7 =
  BG.unsafePerformIO hs_bindgen_cdbdbfa038d903e1

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_before8@
foreign import ccall unsafe "hs_bindgen_174dc83912b6e97a" hs_bindgen_174dc83912b6e97a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_before8@
hs_bindgen_174dc83912b6e97a :: IO (BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_174dc83912b6e97a =
  fmap BG.fromFFIType hs_bindgen_174dc83912b6e97a_base

{-# NOINLINE const_withoutSign_before8 #-}
{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @macros\/reparse.h 193:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before8 :: BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ())
const_withoutSign_before8 =
  BG.unsafePerformIO hs_bindgen_174dc83912b6e97a

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_after1@
foreign import ccall unsafe "hs_bindgen_bab771fa5614c60c" hs_bindgen_bab771fa5614c60c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_after1@
hs_bindgen_bab771fa5614c60c :: IO (BG.FunPtr (BG.CInt -> BG.CFloat -> IO ()))
hs_bindgen_bab771fa5614c60c =
  fmap BG.fromFFIType hs_bindgen_bab771fa5614c60c_base

{-# NOINLINE const_withoutSign_after1 #-}
{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @macros\/reparse.h 195:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after1 :: BG.FunPtr (BG.CInt -> BG.CFloat -> IO ())
const_withoutSign_after1 =
  BG.unsafePerformIO hs_bindgen_bab771fa5614c60c

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_after2@
foreign import ccall unsafe "hs_bindgen_bba83003ba9d0e65" hs_bindgen_bba83003ba9d0e65_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_after2@
hs_bindgen_bba83003ba9d0e65 :: IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO ()))
hs_bindgen_bba83003ba9d0e65 =
  fmap BG.fromFFIType hs_bindgen_bba83003ba9d0e65_base

{-# NOINLINE const_withoutSign_after2 #-}
{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @macros\/reparse.h 196:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after2 :: BG.FunPtr (BG.CInt -> BG.CDouble -> IO ())
const_withoutSign_after2 =
  BG.unsafePerformIO hs_bindgen_bba83003ba9d0e65

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_after3@
foreign import ccall unsafe "hs_bindgen_8b30e6c0e586fbed" hs_bindgen_8b30e6c0e586fbed_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_after3@
hs_bindgen_8b30e6c0e586fbed :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_8b30e6c0e586fbed =
  fmap BG.fromFFIType hs_bindgen_8b30e6c0e586fbed_base

{-# NOINLINE const_withoutSign_after3 #-}
{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @macros\/reparse.h 197:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after3 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
const_withoutSign_after3 =
  BG.unsafePerformIO hs_bindgen_8b30e6c0e586fbed

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_after4@
foreign import ccall unsafe "hs_bindgen_e4bd6496bb9ba3fb" hs_bindgen_e4bd6496bb9ba3fb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_after4@
hs_bindgen_e4bd6496bb9ba3fb :: IO (BG.FunPtr (BG.CInt -> Some_struct -> IO ()))
hs_bindgen_e4bd6496bb9ba3fb =
  fmap BG.fromFFIType hs_bindgen_e4bd6496bb9ba3fb_base

{-# NOINLINE const_withoutSign_after4 #-}
{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @macros\/reparse.h 198:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after4 :: BG.FunPtr (BG.CInt -> Some_struct -> IO ())
const_withoutSign_after4 =
  BG.unsafePerformIO hs_bindgen_e4bd6496bb9ba3fb

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_after5@
foreign import ccall unsafe "hs_bindgen_4fa6cf106ec109d9" hs_bindgen_4fa6cf106ec109d9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_after5@
hs_bindgen_4fa6cf106ec109d9 :: IO (BG.FunPtr (BG.CInt -> Some_union -> IO ()))
hs_bindgen_4fa6cf106ec109d9 =
  fmap BG.fromFFIType hs_bindgen_4fa6cf106ec109d9_base

{-# NOINLINE const_withoutSign_after5 #-}
{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @macros\/reparse.h 199:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after5 :: BG.FunPtr (BG.CInt -> Some_union -> IO ())
const_withoutSign_after5 =
  BG.unsafePerformIO hs_bindgen_4fa6cf106ec109d9

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_after6@
foreign import ccall unsafe "hs_bindgen_4412b652c7e31a26" hs_bindgen_4412b652c7e31a26_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_after6@
hs_bindgen_4412b652c7e31a26 :: IO (BG.FunPtr (BG.CInt -> Some_enum -> IO ()))
hs_bindgen_4412b652c7e31a26 =
  fmap BG.fromFFIType hs_bindgen_4412b652c7e31a26_base

{-# NOINLINE const_withoutSign_after6 #-}
{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @macros\/reparse.h 200:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after6 :: BG.FunPtr (BG.CInt -> Some_enum -> IO ())
const_withoutSign_after6 =
  BG.unsafePerformIO hs_bindgen_4412b652c7e31a26

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_after7@
foreign import ccall unsafe "hs_bindgen_36cac970802d5e2a" hs_bindgen_36cac970802d5e2a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_after7@
hs_bindgen_36cac970802d5e2a :: IO (BG.FunPtr (BG.CInt -> BG.CBool -> IO ()))
hs_bindgen_36cac970802d5e2a =
  fmap BG.fromFFIType hs_bindgen_36cac970802d5e2a_base

{-# NOINLINE const_withoutSign_after7 #-}
{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @macros\/reparse.h 201:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after7 :: BG.FunPtr (BG.CInt -> BG.CBool -> IO ())
const_withoutSign_after7 =
  BG.unsafePerformIO hs_bindgen_36cac970802d5e2a

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_after8@
foreign import ccall unsafe "hs_bindgen_568a55ef8c9b5076" hs_bindgen_568a55ef8c9b5076_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_withoutSign_after8@
hs_bindgen_568a55ef8c9b5076 :: IO (BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_568a55ef8c9b5076 =
  fmap BG.fromFFIType hs_bindgen_568a55ef8c9b5076_base

{-# NOINLINE const_withoutSign_after8 #-}
{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @macros\/reparse.h 202:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after8 :: BG.FunPtr (BG.CInt -> HsBindgen.Runtime.LibC.CSize -> IO ())
const_withoutSign_after8 =
  BG.unsafePerformIO hs_bindgen_568a55ef8c9b5076

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_args1@
foreign import ccall unsafe "hs_bindgen_3f7382d6169ece39" hs_bindgen_3f7382d6169ece39_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_args1@
hs_bindgen_3f7382d6169ece39 :: IO (BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_3f7382d6169ece39 =
  fmap BG.fromFFIType hs_bindgen_3f7382d6169ece39_base

{-# NOINLINE const_pointers_args1 #-}
{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @macros\/reparse.h 206:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args1 :: BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args1 =
  BG.unsafePerformIO hs_bindgen_3f7382d6169ece39

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_args2@
foreign import ccall unsafe "hs_bindgen_e0015f5403a3add6" hs_bindgen_e0015f5403a3add6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_args2@
hs_bindgen_e0015f5403a3add6 :: IO (BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_e0015f5403a3add6 =
  fmap BG.fromFFIType hs_bindgen_e0015f5403a3add6_base

{-# NOINLINE const_pointers_args2 #-}
{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @macros\/reparse.h 207:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args2 :: BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args2 =
  BG.unsafePerformIO hs_bindgen_e0015f5403a3add6

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_args3@
foreign import ccall unsafe "hs_bindgen_e9ee81509f90b9ad" hs_bindgen_e9ee81509f90b9ad_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_args3@
hs_bindgen_e9ee81509f90b9ad :: IO (BG.FunPtr (BG.CInt -> BG.Ptr BG.CInt -> IO ()))
hs_bindgen_e9ee81509f90b9ad =
  fmap BG.fromFFIType hs_bindgen_e9ee81509f90b9ad_base

{-# NOINLINE const_pointers_args3 #-}
{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @macros\/reparse.h 208:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args3 :: BG.FunPtr (BG.CInt -> BG.Ptr BG.CInt -> IO ())
const_pointers_args3 =
  BG.unsafePerformIO hs_bindgen_e9ee81509f90b9ad

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_args4@
foreign import ccall unsafe "hs_bindgen_2e62954a25f4a719" hs_bindgen_2e62954a25f4a719_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_args4@
hs_bindgen_2e62954a25f4a719 :: IO (BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_2e62954a25f4a719 =
  fmap BG.fromFFIType hs_bindgen_2e62954a25f4a719_base

{-# NOINLINE const_pointers_args4 #-}
{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @macros\/reparse.h 209:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args4 :: BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args4 =
  BG.unsafePerformIO hs_bindgen_2e62954a25f4a719

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_args5@
foreign import ccall unsafe "hs_bindgen_db998d61b1058a46" hs_bindgen_db998d61b1058a46_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_args5@
hs_bindgen_db998d61b1058a46 :: IO (BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ()))
hs_bindgen_db998d61b1058a46 =
  fmap BG.fromFFIType hs_bindgen_db998d61b1058a46_base

{-# NOINLINE const_pointers_args5 #-}
{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @macros\/reparse.h 210:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args5 :: BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CInt -> IO ())
const_pointers_args5 =
  BG.unsafePerformIO hs_bindgen_db998d61b1058a46

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_ret1@
foreign import ccall unsafe "hs_bindgen_de5e820b8aa183b8" hs_bindgen_de5e820b8aa183b8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_ret1@
hs_bindgen_de5e820b8aa183b8 :: IO (BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_de5e820b8aa183b8 =
  fmap BG.fromFFIType hs_bindgen_de5e820b8aa183b8_base

{-# NOINLINE const_pointers_ret1 #-}
{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h 212:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1 :: BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret1 =
  BG.unsafePerformIO hs_bindgen_de5e820b8aa183b8

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_ret2@
foreign import ccall unsafe "hs_bindgen_69cbe45216c60049" hs_bindgen_69cbe45216c60049_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_ret2@
hs_bindgen_69cbe45216c60049 :: IO (BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_69cbe45216c60049 =
  fmap BG.fromFFIType hs_bindgen_69cbe45216c60049_base

{-# NOINLINE const_pointers_ret2 #-}
{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h 213:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2 :: BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret2 =
  BG.unsafePerformIO hs_bindgen_69cbe45216c60049

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_ret3@
foreign import ccall unsafe "hs_bindgen_fc3af5baf478fdab" hs_bindgen_fc3af5baf478fdab_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_ret3@
hs_bindgen_fc3af5baf478fdab :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt)))
hs_bindgen_fc3af5baf478fdab =
  fmap BG.fromFFIType hs_bindgen_fc3af5baf478fdab_base

{-# NOINLINE const_pointers_ret3 #-}
{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h 214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt))
const_pointers_ret3 =
  BG.unsafePerformIO hs_bindgen_fc3af5baf478fdab

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_ret4@
foreign import ccall unsafe "hs_bindgen_800ceaaeb41b7c4f" hs_bindgen_800ceaaeb41b7c4f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_ret4@
hs_bindgen_800ceaaeb41b7c4f :: IO (BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_800ceaaeb41b7c4f =
  fmap BG.fromFFIType hs_bindgen_800ceaaeb41b7c4f_base

{-# NOINLINE const_pointers_ret4 #-}
{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h 215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4 :: BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret4 =
  BG.unsafePerformIO hs_bindgen_800ceaaeb41b7c4f

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_ret5@
foreign import ccall unsafe "hs_bindgen_88afe3ace1c8f032" hs_bindgen_88afe3ace1c8f032_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_pointers_ret5@
hs_bindgen_88afe3ace1c8f032 :: IO (BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_88afe3ace1c8f032 =
  fmap BG.fromFFIType hs_bindgen_88afe3ace1c8f032_base

{-# NOINLINE const_pointers_ret5 #-}
{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h 216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5 :: BG.FunPtr (BG.CInt -> IO (PtrConst.PtrConst BG.CInt))
const_pointers_ret5 =
  BG.unsafePerformIO hs_bindgen_88afe3ace1c8f032

-- __unique:__ @test_macrosreparse_2_raw Example get_const_array_elem1@
foreign import ccall unsafe "hs_bindgen_99ff740159cf42db" hs_bindgen_99ff740159cf42db_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_array_elem1@
hs_bindgen_99ff740159cf42db :: IO (BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO ()))
hs_bindgen_99ff740159cf42db =
  fmap BG.fromFFIType hs_bindgen_99ff740159cf42db_base

{-# NOINLINE const_array_elem1 #-}
{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h 244:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 :: BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO ())
const_array_elem1 =
  BG.unsafePerformIO hs_bindgen_99ff740159cf42db

-- __unique:__ @test_macrosreparse_2_raw Example get_const_array_elem2@
foreign import ccall unsafe "hs_bindgen_f429cac79bc959b8" hs_bindgen_f429cac79bc959b8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_array_elem2@
hs_bindgen_f429cac79bc959b8 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt))) -> IO ()))
hs_bindgen_f429cac79bc959b8 =
  fmap BG.fromFFIType hs_bindgen_f429cac79bc959b8_base

{-# NOINLINE const_array_elem2 #-}
{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h 245:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2 :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt))) -> IO ())
const_array_elem2 =
  BG.unsafePerformIO hs_bindgen_f429cac79bc959b8

-- __unique:__ @test_macrosreparse_2_raw Example get_const_array_elem3@
foreign import ccall unsafe "hs_bindgen_d5c9dc626ac3c92a" hs_bindgen_d5c9dc626ac3c92a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_const_array_elem3@
hs_bindgen_d5c9dc626ac3c92a :: IO (BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt))) -> IO ()))
hs_bindgen_d5c9dc626ac3c92a =
  fmap BG.fromFFIType hs_bindgen_d5c9dc626ac3c92a_base

{-# NOINLINE const_array_elem3 #-}
{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h 246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 :: BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt))) -> IO ())
const_array_elem3 =
  BG.unsafePerformIO hs_bindgen_d5c9dc626ac3c92a

-- __unique:__ @test_macrosreparse_2_raw Example get_noParams1@
foreign import ccall unsafe "hs_bindgen_a1d7d27beac0cf43" hs_bindgen_a1d7d27beac0cf43_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_noParams1@
hs_bindgen_a1d7d27beac0cf43 :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_a1d7d27beac0cf43 =
  fmap BG.fromFFIType hs_bindgen_a1d7d27beac0cf43_base

{-# NOINLINE noParams1 #-}
{-| Other examples we reparsed /incorrectly/ before language-c

    __C declaration:__ @noParams1@

    __defined at:__ @macros\/reparse.h 254:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams1 :: BG.FunPtr (IO BG.CInt)
noParams1 =
  BG.unsafePerformIO hs_bindgen_a1d7d27beac0cf43

-- __unique:__ @test_macrosreparse_2_raw Example get_noParams2@
foreign import ccall unsafe "hs_bindgen_c4d3ce2b57180e06" hs_bindgen_c4d3ce2b57180e06_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_noParams2@
hs_bindgen_c4d3ce2b57180e06 :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_c4d3ce2b57180e06 =
  fmap BG.fromFFIType hs_bindgen_c4d3ce2b57180e06_base

{-# NOINLINE noParams2 #-}
{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h 255:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2 :: BG.FunPtr (IO BG.CInt)
noParams2 =
  BG.unsafePerformIO hs_bindgen_c4d3ce2b57180e06

-- __unique:__ @test_macrosreparse_2_raw Example get_noParams3@
foreign import ccall unsafe "hs_bindgen_3198dd435d083be6" hs_bindgen_3198dd435d083be6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_noParams3@
hs_bindgen_3198dd435d083be6 :: IO (BG.FunPtr (BG.CInt -> BG.FunPtr (IO BG.CInt) -> IO ()))
hs_bindgen_3198dd435d083be6 =
  fmap BG.fromFFIType hs_bindgen_3198dd435d083be6_base

{-# NOINLINE noParams3 #-}
{-| __C declaration:__ @noParams3@

    __defined at:__ @macros\/reparse.h 256:6@

    __exported by:__ @macros\/reparse.h@
-}
noParams3 :: BG.FunPtr (BG.CInt -> BG.FunPtr (IO BG.CInt) -> IO ())
noParams3 =
  BG.unsafePerformIO hs_bindgen_3198dd435d083be6

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret1@
foreign import ccall unsafe "hs_bindgen_4c713afd40175b78" hs_bindgen_4c713afd40175b78_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret1@
hs_bindgen_4c713afd40175b78 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (IO ()))))
hs_bindgen_4c713afd40175b78 =
  fmap BG.fromFFIType hs_bindgen_4c713afd40175b78_base

{-# NOINLINE funptr_ret1 #-}
{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h 260:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (IO ())))
funptr_ret1 =
  BG.unsafePerformIO hs_bindgen_4c713afd40175b78

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret2@
foreign import ccall unsafe "hs_bindgen_d696ef6b004ced7d" hs_bindgen_d696ef6b004ced7d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret2@
hs_bindgen_d696ef6b004ced7d :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (IO BG.CInt))))
hs_bindgen_d696ef6b004ced7d =
  fmap BG.fromFFIType hs_bindgen_d696ef6b004ced7d_base

{-# NOINLINE funptr_ret2 #-}
{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h 261:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (IO BG.CInt)))
funptr_ret2 =
  BG.unsafePerformIO hs_bindgen_d696ef6b004ced7d

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret3@
foreign import ccall unsafe "hs_bindgen_837085ddb8062707" hs_bindgen_837085ddb8062707_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret3@
hs_bindgen_837085ddb8062707 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> IO ()))))
hs_bindgen_837085ddb8062707 =
  fmap BG.fromFFIType hs_bindgen_837085ddb8062707_base

{-# NOINLINE funptr_ret3 #-}
{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h 262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> IO ())))
funptr_ret3 =
  BG.unsafePerformIO hs_bindgen_837085ddb8062707

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret4@
foreign import ccall unsafe "hs_bindgen_b2faea37032ad23b" hs_bindgen_b2faea37032ad23b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret4@
hs_bindgen_b2faea37032ad23b :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))))
hs_bindgen_b2faea37032ad23b =
  fmap BG.fromFFIType hs_bindgen_b2faea37032ad23b_base

{-# NOINLINE funptr_ret4 #-}
{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h 263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar)))
funptr_ret4 =
  BG.unsafePerformIO hs_bindgen_b2faea37032ad23b

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret5@
foreign import ccall unsafe "hs_bindgen_41e753039680a766" hs_bindgen_41e753039680a766_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret5@
hs_bindgen_41e753039680a766 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))))
hs_bindgen_41e753039680a766 =
  fmap BG.fromFFIType hs_bindgen_41e753039680a766_base

{-# NOINLINE funptr_ret5 #-}
{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h 267:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))))
funptr_ret5 =
  BG.unsafePerformIO hs_bindgen_41e753039680a766

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret6@
foreign import ccall unsafe "hs_bindgen_5d8ba1f50ed61fd3" hs_bindgen_5d8ba1f50ed61fd3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret6@
hs_bindgen_5d8ba1f50ed61fd3 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_5d8ba1f50ed61fd3 =
  fmap BG.fromFFIType hs_bindgen_5d8ba1f50ed61fd3_base

{-# NOINLINE funptr_ret6 #-}
{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h 268:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret6 =
  BG.unsafePerformIO hs_bindgen_5d8ba1f50ed61fd3

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret7@
foreign import ccall unsafe "hs_bindgen_210a758d713c2fba" hs_bindgen_210a758d713c2fba_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret7@
hs_bindgen_210a758d713c2fba :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_210a758d713c2fba =
  fmap BG.fromFFIType hs_bindgen_210a758d713c2fba_base

{-# NOINLINE funptr_ret7 #-}
{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h 269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret7 =
  BG.unsafePerformIO hs_bindgen_210a758d713c2fba

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret8@
foreign import ccall unsafe "hs_bindgen_d281895081e937b3" hs_bindgen_d281895081e937b3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret8@
hs_bindgen_d281895081e937b3 :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))))
hs_bindgen_d281895081e937b3 =
  fmap BG.fromFFIType hs_bindgen_d281895081e937b3_base

{-# NOINLINE funptr_ret8 #-}
{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h 270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))))
funptr_ret8 =
  BG.unsafePerformIO hs_bindgen_d281895081e937b3

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret9@
foreign import ccall unsafe "hs_bindgen_086a0d13fcf4ab8e" hs_bindgen_086a0d13fcf4ab8e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret9@
hs_bindgen_086a0d13fcf4ab8e :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_086a0d13fcf4ab8e =
  fmap BG.fromFFIType hs_bindgen_086a0d13fcf4ab8e_base

{-# NOINLINE funptr_ret9 #-}
{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h 271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret9 =
  BG.unsafePerformIO hs_bindgen_086a0d13fcf4ab8e

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret10@
foreign import ccall unsafe "hs_bindgen_780c1cc5a945167c" hs_bindgen_780c1cc5a945167c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example get_funptr_ret10@
hs_bindgen_780c1cc5a945167c :: IO (BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))))
hs_bindgen_780c1cc5a945167c =
  fmap BG.fromFFIType hs_bindgen_780c1cc5a945167c_base

{-# NOINLINE funptr_ret10 #-}
{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h 272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10 :: BG.FunPtr (BG.CInt -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))))
funptr_ret10 =
  BG.unsafePerformIO hs_bindgen_780c1cc5a945167c
