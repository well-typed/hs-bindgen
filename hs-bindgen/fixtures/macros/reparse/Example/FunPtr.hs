{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Data.Complex
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <macros/reparse.h>"
  , "/* ExampleNothingget_args_char1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_1cbcf8b84924816c (void)) ("
  , "  A arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &args_char1;"
  , "}"
  , "/* ExampleNothingget_args_char2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_ec2d78b82f444fd0 (void)) ("
  , "  A arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  return &args_char2;"
  , "}"
  , "/* ExampleNothingget_args_char3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_1baa18e723594389 (void)) ("
  , "  A arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  return &args_char3;"
  , "}"
  , "/* ExampleNothingget_args_short1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_c96cef4ef5f5e180 (void)) ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short1;"
  , "}"
  , "/* ExampleNothingget_args_short2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_3a683552d4f772c7 (void)) ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short2;"
  , "}"
  , "/* ExampleNothingget_args_short3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_f3284022ac706255 (void)) ("
  , "  A arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  return &args_short3;"
  , "}"
  , "/* ExampleNothingget_args_int1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_5c4d785286ccca6b (void)) ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int1;"
  , "}"
  , "/* ExampleNothingget_args_int2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_e323b837afe40be7 (void)) ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int2;"
  , "}"
  , "/* ExampleNothingget_args_int3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_eb0e5feb8eb4082d (void)) ("
  , "  A arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  return &args_int3;"
  , "}"
  , "/* ExampleNothingget_args_long1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_d7d322f23a65f43b (void)) ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long1;"
  , "}"
  , "/* ExampleNothingget_args_long2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_378c16768a6f6f21 (void)) ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long2;"
  , "}"
  , "/* ExampleNothingget_args_long3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_548dcd4760226ee2 (void)) ("
  , "  A arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  return &args_long3;"
  , "}"
  , "/* ExampleNothingget_args_float_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_701d01261043851b (void)) ("
  , "  A arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  return &args_float;"
  , "}"
  , "/* ExampleNothingget_args_double_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_ff631e42f704e4cd (void)) ("
  , "  A arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &args_double;"
  , "}"
  , "/* ExampleNothingget_args_bool1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_6e289c6cc6d382bf (void)) ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &args_bool1;"
  , "}"
  , "/* ExampleNothingget_args_struct_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_26b20c1b89e46b02 (void)) ("
  , "  A arg1,"
  , "  struct some_struct arg2"
  , ")"
  , "{"
  , "  return &args_struct;"
  , "}"
  , "/* ExampleNothingget_args_union_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_cfd37f06f21b8755 (void)) ("
  , "  A arg1,"
  , "  union some_union arg2"
  , ")"
  , "{"
  , "  return &args_union;"
  , "}"
  , "/* ExampleNothingget_args_enum_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_69882f8f862fffc2 (void)) ("
  , "  A arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  return &args_enum;"
  , "}"
  , "/* ExampleNothingget_args_pointer1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_23bde4e97b66c470 (void)) ("
  , "  A arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return &args_pointer1;"
  , "}"
  , "/* ExampleNothingget_args_pointer2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_fceb546239df3c0a (void)) ("
  , "  A arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  return &args_pointer2;"
  , "}"
  , "/* ExampleNothingget_args_pointer3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_0cb396fb06dd816a (void)) ("
  , "  A arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  return &args_pointer3;"
  , "}"
  , "/* ExampleNothingget_ret_A_ptr */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_test_macrosreparse_a7564eacf3ad149f (void)) (void)"
  , "{"
  , "  return &ret_A;"
  , "}"
  , "/* ExampleNothingget_ret_char1_ptr */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_test_macrosreparse_7b5b646ee4e06777 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char1;"
  , "}"
  , "/* ExampleNothingget_ret_char2_ptr */"
  , "__attribute__ ((const))"
  , "signed char (*hs_bindgen_test_macrosreparse_7c05cbccaf1be8b6 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char2;"
  , "}"
  , "/* ExampleNothingget_ret_char3_ptr */"
  , "__attribute__ ((const))"
  , "unsigned char (*hs_bindgen_test_macrosreparse_0fc74f839f906d7e (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char3;"
  , "}"
  , "/* ExampleNothingget_ret_short1_ptr */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_test_macrosreparse_72ff9f5cb5daaae8 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short1;"
  , "}"
  , "/* ExampleNothingget_ret_short2_ptr */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_test_macrosreparse_eb5427ff3ea0d96e (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short2;"
  , "}"
  , "/* ExampleNothingget_ret_short3_ptr */"
  , "__attribute__ ((const))"
  , "unsigned short (*hs_bindgen_test_macrosreparse_823adc61eed1550c (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short3;"
  , "}"
  , "/* ExampleNothingget_ret_int1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_macrosreparse_79ce8d81113cf766 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int1;"
  , "}"
  , "/* ExampleNothingget_ret_int2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_macrosreparse_d369bd4861f00c84 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int2;"
  , "}"
  , "/* ExampleNothingget_ret_int3_ptr */"
  , "__attribute__ ((const))"
  , "unsigned int (*hs_bindgen_test_macrosreparse_0336d583fc7b5951 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int3;"
  , "}"
  , "/* ExampleNothingget_ret_long1_ptr */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_test_macrosreparse_36845109a4ce7992 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long1;"
  , "}"
  , "/* ExampleNothingget_ret_long2_ptr */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_test_macrosreparse_ac32dbc1e79e704e (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long2;"
  , "}"
  , "/* ExampleNothingget_ret_long3_ptr */"
  , "__attribute__ ((const))"
  , "unsigned long (*hs_bindgen_test_macrosreparse_6fba85ecad7d8d4e (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long3;"
  , "}"
  , "/* ExampleNothingget_ret_float_ptr */"
  , "__attribute__ ((const))"
  , "float (*hs_bindgen_test_macrosreparse_e9ac779a7c943add (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_float;"
  , "}"
  , "/* ExampleNothingget_ret_double_ptr */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_test_macrosreparse_7095a5f5be3ecc0c (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_double;"
  , "}"
  , "/* ExampleNothingget_ret_bool1_ptr */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_test_macrosreparse_c7b5be49f4314899 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_bool1;"
  , "}"
  , "/* ExampleNothingget_ret_struct_ptr */"
  , "__attribute__ ((const))"
  , "struct some_struct (*hs_bindgen_test_macrosreparse_03ec23cf81b62ce3 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_struct;"
  , "}"
  , "/* ExampleNothingget_ret_union_ptr */"
  , "__attribute__ ((const))"
  , "union some_union (*hs_bindgen_test_macrosreparse_5315544d48ea5b07 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_union;"
  , "}"
  , "/* ExampleNothingget_ret_enum_ptr */"
  , "__attribute__ ((const))"
  , "enum some_enum (*hs_bindgen_test_macrosreparse_9fb7ddbcd84c72f1 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_enum;"
  , "}"
  , "/* ExampleNothingget_ret_pointer1_ptr */"
  , "__attribute__ ((const))"
  , "signed int *(*hs_bindgen_test_macrosreparse_0638bcad8813a303 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer1;"
  , "}"
  , "/* ExampleNothingget_ret_pointer2_ptr */"
  , "__attribute__ ((const))"
  , "signed int **(*hs_bindgen_test_macrosreparse_5d9ced9e4887782b (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer2;"
  , "}"
  , "/* ExampleNothingget_ret_pointer3_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_test_macrosreparse_60e99361ec0a4b5b (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer3;"
  , "}"
  , "/* ExampleNothingget_body1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_macrosreparse_cca1935605a94051 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &body1;"
  , "}"
  , "/* ExampleNothingget_body2_ptr */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_test_macrosreparse_a1900daea7e14e95 (void)) (void)"
  , "{"
  , "  return &body2;"
  , "}"
  , "/* ExampleNothingget_args_complex_float_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_c62f1e9d47469a1c (void)) ("
  , "  A arg1,"
  , "  float _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_float;"
  , "}"
  , "/* ExampleNothingget_args_complex_double_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_b2ef5ed0a8ed0697 (void)) ("
  , "  A arg1,"
  , "  double _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_double;"
  , "}"
  , "/* ExampleNothingget_ret_complex_float_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex (*hs_bindgen_test_macrosreparse_e2cc2aa2dd12852d (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_complex_float;"
  , "}"
  , "/* ExampleNothingget_ret_complex_double_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex (*hs_bindgen_test_macrosreparse_c95961d571f78868 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_complex_double;"
  , "}"
  , "/* ExampleNothingget_bespoke_args1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_94c8a2d3574ba283 (void)) ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &bespoke_args1;"
  , "}"
  , "/* ExampleNothingget_bespoke_args2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_2165985767a8d24e (void)) ("
  , "  A arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &bespoke_args2;"
  , "}"
  , "/* ExampleNothingget_bespoke_ret1_ptr */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_test_macrosreparse_7913bf38675bd912 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret1;"
  , "}"
  , "/* ExampleNothingget_bespoke_ret2_ptr */"
  , "__attribute__ ((const))"
  , "size_t (*hs_bindgen_test_macrosreparse_07c419cb648cdf65 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret2;"
  , "}"
  , "/* ExampleNothingget_arr_args1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_ed19e51bcac06a9e (void)) ("
  , "  A arg1[]"
  , ")"
  , "{"
  , "  return &arr_args1;"
  , "}"
  , "/* ExampleNothingget_arr_args2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_de3931a21a8a71fc (void)) ("
  , "  A *arg1[]"
  , ")"
  , "{"
  , "  return &arr_args2;"
  , "}"
  , "/* ExampleNothingget_arr_args3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_2c02effa6288a26b (void)) ("
  , "  A arg1[5]"
  , ")"
  , "{"
  , "  return &arr_args3;"
  , "}"
  , "/* ExampleNothingget_arr_args4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_2144e300082f115c (void)) ("
  , "  A *arg1[5]"
  , ")"
  , "{"
  , "  return &arr_args4;"
  , "}"
  , "/* ExampleNothingget_funptr_args1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_d1645262a53743f6 (void)) ("
  , "  A arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args1;"
  , "}"
  , "/* ExampleNothingget_funptr_args2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_d66507630e4e38e3 (void)) ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args2;"
  , "}"
  , "/* ExampleNothingget_funptr_args3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_3d7907ab53b617cf (void)) ("
  , "  A arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args3;"
  , "}"
  , "/* ExampleNothingget_funptr_args4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_e4d15a9c3b04292a (void)) ("
  , "  A arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args4;"
  , "}"
  , "/* ExampleNothingget_funptr_args5_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_ced7918b6e42102f (void)) ("
  , "  A arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args5;"
  , "}"
  , "/* ExampleNothingget_comments1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_c90ec05081ef4e64 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &comments1;"
  , "}"
  , "/* ExampleNothingget_const_prim_before1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_6ac4b42c66a36448 (void)) ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before1;"
  , "}"
  , "/* ExampleNothingget_const_prim_before2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_f98632ef2e69b003 (void)) ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before2;"
  , "}"
  , "/* ExampleNothingget_const_prim_before3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_cc9db1f6a36b8221 (void)) ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before3;"
  , "}"
  , "/* ExampleNothingget_const_prim_after1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_3e5b7273bf2ecadb (void)) ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after1;"
  , "}"
  , "/* ExampleNothingget_const_prim_after2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_f9b4beeca8253333 (void)) ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after2;"
  , "}"
  , "/* ExampleNothingget_const_prim_after3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_bf14e2fd88b25311 (void)) ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after3;"
  , "}"
  , "/* ExampleNothingget_const_withoutSign_before1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_3649293fcaa1543c (void)) ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before1;"
  , "}"
  , "/* ExampleNothingget_const_withoutSign_before2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_ad5903c28e22dd2c (void)) ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before2;"
  , "}"
  , "/* ExampleNothingget_const_withoutSign_before3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_e7b9bc011ec1dd8a (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before3;"
  , "}"
  , "/* ExampleNothingget_const_withoutSign_before4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_4fd66b696848dd98 (void)) ("
  , "  A arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before4;"
  , "}"
  , "/* ExampleNothingget_const_withoutSign_before5_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_42582e1882927f7e (void)) ("
  , "  A arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before5;"
  , "}"
  , "/* ExampleNothingget_const_withoutSign_before6_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_b6876e53e4b27a98 (void)) ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before6;"
  , "}"
  , "/* ExampleNothingget_const_withoutSign_before7_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_78763cbecd2b0750 (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before7;"
  , "}"
  , "/* ExampleNothingget_const_withoutSign_before8_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_4098c4a4ccd31d36 (void)) ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before8;"
  , "}"
  , "/* ExampleNothingget_const_withoutSign_after1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_e9148eb7b8dac901 (void)) ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after1;"
  , "}"
  , "/* ExampleNothingget_const_withoutSign_after2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_8663653d89116be9 (void)) ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after2;"
  , "}"
  , "/* ExampleNothingget_const_withoutSign_after3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_136dcba145bf241b (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after3;"
  , "}"
  , "/* ExampleNothingget_const_withoutSign_after4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_380e01acce794cab (void)) ("
  , "  A arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after4;"
  , "}"
  , "/* ExampleNothingget_const_withoutSign_after5_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_af0d84d0757f6c2c (void)) ("
  , "  A arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after5;"
  , "}"
  , "/* ExampleNothingget_const_withoutSign_after6_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_df92501d07bf6c5f (void)) ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after6;"
  , "}"
  , "/* ExampleNothingget_const_withoutSign_after7_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_b41148ca40ec8eb5 (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after7;"
  , "}"
  , "/* ExampleNothingget_const_withoutSign_after8_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_560c9dfdb530548b (void)) ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after8;"
  , "}"
  , "/* ExampleNothingget_const_pointers_args1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_a34d16c099748839 (void)) ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args1;"
  , "}"
  , "/* ExampleNothingget_const_pointers_args2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_45235edaf5c3b599 (void)) ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args2;"
  , "}"
  , "/* ExampleNothingget_const_pointers_args3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_3dbcf1c7202f2878 (void)) ("
  , "  A arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args3;"
  , "}"
  , "/* ExampleNothingget_const_pointers_args4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_a6624f6cc0a062af (void)) ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args4;"
  , "}"
  , "/* ExampleNothingget_const_pointers_args5_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_c5f3253c57910315 (void)) ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args5;"
  , "}"
  , "/* ExampleNothingget_const_pointers_ret1_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_test_macrosreparse_1990ded85ea3850d (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret1;"
  , "}"
  , "/* ExampleNothingget_const_pointers_ret2_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_test_macrosreparse_627cc570c3ca7d19 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret2;"
  , "}"
  , "/* ExampleNothingget_const_pointers_ret3_ptr */"
  , "__attribute__ ((const))"
  , "signed int *const (*hs_bindgen_test_macrosreparse_2f449708b5a275b1 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret3;"
  , "}"
  , "/* ExampleNothingget_const_pointers_ret4_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_test_macrosreparse_67662618cd011c8a (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret4;"
  , "}"
  , "/* ExampleNothingget_const_pointers_ret5_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_test_macrosreparse_fcafd9f8ac329995 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret5;"
  , "}"
  , "/* ExampleNothingget_const_array_elem1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_6928906fc9a88dfc (void)) ("
  , "  A const arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem1;"
  , "}"
  , "/* ExampleNothingget_const_array_elem2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_625a37e9c030891a (void)) ("
  , "  A const *arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem2;"
  , "}"
  , "/* ExampleNothingget_const_array_elem3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_5e23f87114cf51fb (void)) ("
  , "  A *const arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem3;"
  , "}"
  , "/* ExampleNothingget_noParams1_ptr */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_test_macrosreparse_d50620a002265139 (void)) (void)"
  , "{"
  , "  return &noParams1;"
  , "}"
  , "/* ExampleNothingget_noParams2_ptr */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_test_macrosreparse_03b0e24786b82ad5 (void)) (void)"
  , "{"
  , "  return &noParams2;"
  , "}"
  , "/* ExampleNothingget_noParams3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_36508fd99a0556c5 (void)) ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &noParams3;"
  , "}"
  , "/* ExampleNothingget_funptr_ret1_ptr */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_test_macrosreparse_6f83a48dd177c25f (void)) ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret1;"
  , "}"
  , "/* ExampleNothingget_funptr_ret2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_macrosreparse_f12efafd1525ef7f (void)) ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret2;"
  , "}"
  , "/* ExampleNothingget_funptr_ret3_ptr */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_test_macrosreparse_b00baa5b9708b9e7 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &funptr_ret3;"
  , "}"
  , "/* ExampleNothingget_funptr_ret4_ptr */"
  , "__attribute__ ((const))"
  , "char (*(*hs_bindgen_test_macrosreparse_c51872479ceff42e (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret4;"
  , "}"
  , "/* ExampleNothingget_funptr_ret5_ptr */"
  , "__attribute__ ((const))"
  , "signed int *(*(*hs_bindgen_test_macrosreparse_3b9b9924b4b4d7ea (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret5;"
  , "}"
  , "/* ExampleNothingget_funptr_ret6_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_test_macrosreparse_3df5ab4b0b306845 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret6;"
  , "}"
  , "/* ExampleNothingget_funptr_ret7_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_test_macrosreparse_2ac4454d93b6f04a (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret7;"
  , "}"
  , "/* ExampleNothingget_funptr_ret8_ptr */"
  , "__attribute__ ((const))"
  , "signed int *const (*(*hs_bindgen_test_macrosreparse_411c5128f18364b3 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret8;"
  , "}"
  , "/* ExampleNothingget_funptr_ret9_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_test_macrosreparse_693a8d16e17d0cdc (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret9;"
  , "}"
  , "/* ExampleNothingget_funptr_ret10_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_test_macrosreparse_9d2da81bbfe49ab6 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret10;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_args_char1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_1cbcf8b84924816c" hs_bindgen_test_macrosreparse_1cbcf8b84924816c ::
     IO (Ptr.FunPtr (A -> FC.CChar -> IO ()))

{-# NOINLINE args_char1_ptr #-}

{-| Function declarations

__C declaration:__ @args_char1@

__defined at:__ @macros\/reparse.h:17:6@

__exported by:__ @macros\/reparse.h@
-}
args_char1_ptr :: Ptr.FunPtr (A -> FC.CChar -> IO ())
args_char1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_1cbcf8b84924816c

{-| __unique:__ @ExampleNothingget_args_char2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_ec2d78b82f444fd0" hs_bindgen_test_macrosreparse_ec2d78b82f444fd0 ::
     IO (Ptr.FunPtr (A -> FC.CSChar -> IO ()))

{-# NOINLINE args_char2_ptr #-}

{-| __C declaration:__ @args_char2@

    __defined at:__ @macros\/reparse.h:18:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char2_ptr :: Ptr.FunPtr (A -> FC.CSChar -> IO ())
args_char2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_ec2d78b82f444fd0

{-| __unique:__ @ExampleNothingget_args_char3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_1baa18e723594389" hs_bindgen_test_macrosreparse_1baa18e723594389 ::
     IO (Ptr.FunPtr (A -> FC.CUChar -> IO ()))

{-# NOINLINE args_char3_ptr #-}

{-| __C declaration:__ @args_char3@

    __defined at:__ @macros\/reparse.h:19:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char3_ptr :: Ptr.FunPtr (A -> FC.CUChar -> IO ())
args_char3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_1baa18e723594389

{-| __unique:__ @ExampleNothingget_args_short1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c96cef4ef5f5e180" hs_bindgen_test_macrosreparse_c96cef4ef5f5e180 ::
     IO (Ptr.FunPtr (A -> FC.CShort -> IO ()))

{-# NOINLINE args_short1_ptr #-}

{-| __C declaration:__ @args_short1@

    __defined at:__ @macros\/reparse.h:21:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short1_ptr :: Ptr.FunPtr (A -> FC.CShort -> IO ())
args_short1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_c96cef4ef5f5e180

{-| __unique:__ @ExampleNothingget_args_short2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_3a683552d4f772c7" hs_bindgen_test_macrosreparse_3a683552d4f772c7 ::
     IO (Ptr.FunPtr (A -> FC.CShort -> IO ()))

{-# NOINLINE args_short2_ptr #-}

{-| __C declaration:__ @args_short2@

    __defined at:__ @macros\/reparse.h:22:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short2_ptr :: Ptr.FunPtr (A -> FC.CShort -> IO ())
args_short2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_3a683552d4f772c7

{-| __unique:__ @ExampleNothingget_args_short3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_f3284022ac706255" hs_bindgen_test_macrosreparse_f3284022ac706255 ::
     IO (Ptr.FunPtr (A -> FC.CUShort -> IO ()))

{-# NOINLINE args_short3_ptr #-}

{-| __C declaration:__ @args_short3@

    __defined at:__ @macros\/reparse.h:23:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short3_ptr :: Ptr.FunPtr (A -> FC.CUShort -> IO ())
args_short3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_f3284022ac706255

{-| __unique:__ @ExampleNothingget_args_int1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_5c4d785286ccca6b" hs_bindgen_test_macrosreparse_5c4d785286ccca6b ::
     IO (Ptr.FunPtr (A -> FC.CInt -> IO ()))

{-# NOINLINE args_int1_ptr #-}

{-| __C declaration:__ @args_int1@

    __defined at:__ @macros\/reparse.h:25:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int1_ptr :: Ptr.FunPtr (A -> FC.CInt -> IO ())
args_int1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_5c4d785286ccca6b

{-| __unique:__ @ExampleNothingget_args_int2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e323b837afe40be7" hs_bindgen_test_macrosreparse_e323b837afe40be7 ::
     IO (Ptr.FunPtr (A -> FC.CInt -> IO ()))

{-# NOINLINE args_int2_ptr #-}

{-| __C declaration:__ @args_int2@

    __defined at:__ @macros\/reparse.h:26:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int2_ptr :: Ptr.FunPtr (A -> FC.CInt -> IO ())
args_int2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_e323b837afe40be7

{-| __unique:__ @ExampleNothingget_args_int3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_eb0e5feb8eb4082d" hs_bindgen_test_macrosreparse_eb0e5feb8eb4082d ::
     IO (Ptr.FunPtr (A -> FC.CUInt -> IO ()))

{-# NOINLINE args_int3_ptr #-}

{-| __C declaration:__ @args_int3@

    __defined at:__ @macros\/reparse.h:27:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int3_ptr :: Ptr.FunPtr (A -> FC.CUInt -> IO ())
args_int3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_eb0e5feb8eb4082d

{-| __unique:__ @ExampleNothingget_args_long1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_d7d322f23a65f43b" hs_bindgen_test_macrosreparse_d7d322f23a65f43b ::
     IO (Ptr.FunPtr (A -> FC.CLong -> IO ()))

{-# NOINLINE args_long1_ptr #-}

{-| __C declaration:__ @args_long1@

    __defined at:__ @macros\/reparse.h:29:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long1_ptr :: Ptr.FunPtr (A -> FC.CLong -> IO ())
args_long1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_d7d322f23a65f43b

{-| __unique:__ @ExampleNothingget_args_long2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_378c16768a6f6f21" hs_bindgen_test_macrosreparse_378c16768a6f6f21 ::
     IO (Ptr.FunPtr (A -> FC.CLong -> IO ()))

{-# NOINLINE args_long2_ptr #-}

{-| __C declaration:__ @args_long2@

    __defined at:__ @macros\/reparse.h:30:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long2_ptr :: Ptr.FunPtr (A -> FC.CLong -> IO ())
args_long2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_378c16768a6f6f21

{-| __unique:__ @ExampleNothingget_args_long3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_548dcd4760226ee2" hs_bindgen_test_macrosreparse_548dcd4760226ee2 ::
     IO (Ptr.FunPtr (A -> FC.CULong -> IO ()))

{-# NOINLINE args_long3_ptr #-}

{-| __C declaration:__ @args_long3@

    __defined at:__ @macros\/reparse.h:31:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long3_ptr :: Ptr.FunPtr (A -> FC.CULong -> IO ())
args_long3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_548dcd4760226ee2

{-| __unique:__ @ExampleNothingget_args_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_701d01261043851b" hs_bindgen_test_macrosreparse_701d01261043851b ::
     IO (Ptr.FunPtr (A -> FC.CFloat -> IO ()))

{-# NOINLINE args_float_ptr #-}

{-| __C declaration:__ @args_float@

    __defined at:__ @macros\/reparse.h:33:6@

    __exported by:__ @macros\/reparse.h@
-}
args_float_ptr :: Ptr.FunPtr (A -> FC.CFloat -> IO ())
args_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_701d01261043851b

{-| __unique:__ @ExampleNothingget_args_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_ff631e42f704e4cd" hs_bindgen_test_macrosreparse_ff631e42f704e4cd ::
     IO (Ptr.FunPtr (A -> FC.CDouble -> IO ()))

{-# NOINLINE args_double_ptr #-}

{-| __C declaration:__ @args_double@

    __defined at:__ @macros\/reparse.h:34:6@

    __exported by:__ @macros\/reparse.h@
-}
args_double_ptr :: Ptr.FunPtr (A -> FC.CDouble -> IO ())
args_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_ff631e42f704e4cd

{-| __unique:__ @ExampleNothingget_args_bool1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_6e289c6cc6d382bf" hs_bindgen_test_macrosreparse_6e289c6cc6d382bf ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE args_bool1_ptr #-}

{-| __C declaration:__ @args_bool1@

    __defined at:__ @macros\/reparse.h:35:6@

    __exported by:__ @macros\/reparse.h@
-}
args_bool1_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
args_bool1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_6e289c6cc6d382bf

{-| __unique:__ @ExampleNothingget_args_struct_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_26b20c1b89e46b02" hs_bindgen_test_macrosreparse_26b20c1b89e46b02 ::
     IO (Ptr.FunPtr (A -> Some_struct -> IO ()))

{-# NOINLINE args_struct_ptr #-}

{-| __C declaration:__ @args_struct@

    __defined at:__ @macros\/reparse.h:37:6@

    __exported by:__ @macros\/reparse.h@
-}
args_struct_ptr :: Ptr.FunPtr (A -> Some_struct -> IO ())
args_struct_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_26b20c1b89e46b02

{-| __unique:__ @ExampleNothingget_args_union_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_cfd37f06f21b8755" hs_bindgen_test_macrosreparse_cfd37f06f21b8755 ::
     IO (Ptr.FunPtr (A -> Some_union -> IO ()))

{-# NOINLINE args_union_ptr #-}

{-| __C declaration:__ @args_union@

    __defined at:__ @macros\/reparse.h:38:6@

    __exported by:__ @macros\/reparse.h@
-}
args_union_ptr :: Ptr.FunPtr (A -> Some_union -> IO ())
args_union_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_cfd37f06f21b8755

{-| __unique:__ @ExampleNothingget_args_enum_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_69882f8f862fffc2" hs_bindgen_test_macrosreparse_69882f8f862fffc2 ::
     IO (Ptr.FunPtr (A -> Some_enum -> IO ()))

{-# NOINLINE args_enum_ptr #-}

{-| __C declaration:__ @args_enum@

    __defined at:__ @macros\/reparse.h:39:6@

    __exported by:__ @macros\/reparse.h@
-}
args_enum_ptr :: Ptr.FunPtr (A -> Some_enum -> IO ())
args_enum_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_69882f8f862fffc2

{-| __unique:__ @ExampleNothingget_args_pointer1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_23bde4e97b66c470" hs_bindgen_test_macrosreparse_23bde4e97b66c470 ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE args_pointer1_ptr #-}

{-| __C declaration:__ @args_pointer1@

    __defined at:__ @macros\/reparse.h:41:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer1_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
args_pointer1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_23bde4e97b66c470

{-| __unique:__ @ExampleNothingget_args_pointer2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_fceb546239df3c0a" hs_bindgen_test_macrosreparse_fceb546239df3c0a ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr (Ptr.Ptr FC.CInt)) -> IO ()))

{-# NOINLINE args_pointer2_ptr #-}

{-| __C declaration:__ @args_pointer2@

    __defined at:__ @macros\/reparse.h:42:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer2_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr (Ptr.Ptr FC.CInt)) -> IO ())
args_pointer2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_fceb546239df3c0a

{-| __unique:__ @ExampleNothingget_args_pointer3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_0cb396fb06dd816a" hs_bindgen_test_macrosreparse_0cb396fb06dd816a ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr Void) -> IO ()))

{-# NOINLINE args_pointer3_ptr #-}

{-| __C declaration:__ @args_pointer3@

    __defined at:__ @macros\/reparse.h:43:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer3_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr Void) -> IO ())
args_pointer3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_0cb396fb06dd816a

{-| __unique:__ @ExampleNothingget_ret_A_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_a7564eacf3ad149f" hs_bindgen_test_macrosreparse_a7564eacf3ad149f ::
     IO (Ptr.FunPtr (IO A))

{-# NOINLINE ret_A_ptr #-}

{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h:47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A_ptr :: Ptr.FunPtr (IO A)
ret_A_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_a7564eacf3ad149f

{-| __unique:__ @ExampleNothingget_ret_char1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_7b5b646ee4e06777" hs_bindgen_test_macrosreparse_7b5b646ee4e06777 ::
     IO (Ptr.FunPtr (A -> IO FC.CChar))

{-# NOINLINE ret_char1_ptr #-}

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h:49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1_ptr :: Ptr.FunPtr (A -> IO FC.CChar)
ret_char1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_7b5b646ee4e06777

{-| __unique:__ @ExampleNothingget_ret_char2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_7c05cbccaf1be8b6" hs_bindgen_test_macrosreparse_7c05cbccaf1be8b6 ::
     IO (Ptr.FunPtr (A -> IO FC.CSChar))

{-# NOINLINE ret_char2_ptr #-}

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h:50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2_ptr :: Ptr.FunPtr (A -> IO FC.CSChar)
ret_char2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_7c05cbccaf1be8b6

{-| __unique:__ @ExampleNothingget_ret_char3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_0fc74f839f906d7e" hs_bindgen_test_macrosreparse_0fc74f839f906d7e ::
     IO (Ptr.FunPtr (A -> IO FC.CUChar))

{-# NOINLINE ret_char3_ptr #-}

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h:51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3_ptr :: Ptr.FunPtr (A -> IO FC.CUChar)
ret_char3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_0fc74f839f906d7e

{-| __unique:__ @ExampleNothingget_ret_short1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_72ff9f5cb5daaae8" hs_bindgen_test_macrosreparse_72ff9f5cb5daaae8 ::
     IO (Ptr.FunPtr (A -> IO FC.CShort))

{-# NOINLINE ret_short1_ptr #-}

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h:53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1_ptr :: Ptr.FunPtr (A -> IO FC.CShort)
ret_short1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_72ff9f5cb5daaae8

{-| __unique:__ @ExampleNothingget_ret_short2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_eb5427ff3ea0d96e" hs_bindgen_test_macrosreparse_eb5427ff3ea0d96e ::
     IO (Ptr.FunPtr (A -> IO FC.CShort))

{-# NOINLINE ret_short2_ptr #-}

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h:54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2_ptr :: Ptr.FunPtr (A -> IO FC.CShort)
ret_short2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_eb5427ff3ea0d96e

{-| __unique:__ @ExampleNothingget_ret_short3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_823adc61eed1550c" hs_bindgen_test_macrosreparse_823adc61eed1550c ::
     IO (Ptr.FunPtr (A -> IO FC.CUShort))

{-# NOINLINE ret_short3_ptr #-}

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h:55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3_ptr :: Ptr.FunPtr (A -> IO FC.CUShort)
ret_short3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_823adc61eed1550c

{-| __unique:__ @ExampleNothingget_ret_int1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_79ce8d81113cf766" hs_bindgen_test_macrosreparse_79ce8d81113cf766 ::
     IO (Ptr.FunPtr (A -> IO FC.CInt))

{-# NOINLINE ret_int1_ptr #-}

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h:57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1_ptr :: Ptr.FunPtr (A -> IO FC.CInt)
ret_int1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_79ce8d81113cf766

{-| __unique:__ @ExampleNothingget_ret_int2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_d369bd4861f00c84" hs_bindgen_test_macrosreparse_d369bd4861f00c84 ::
     IO (Ptr.FunPtr (A -> IO FC.CInt))

{-# NOINLINE ret_int2_ptr #-}

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h:58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2_ptr :: Ptr.FunPtr (A -> IO FC.CInt)
ret_int2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_d369bd4861f00c84

{-| __unique:__ @ExampleNothingget_ret_int3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_0336d583fc7b5951" hs_bindgen_test_macrosreparse_0336d583fc7b5951 ::
     IO (Ptr.FunPtr (A -> IO FC.CUInt))

{-# NOINLINE ret_int3_ptr #-}

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h:59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3_ptr :: Ptr.FunPtr (A -> IO FC.CUInt)
ret_int3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_0336d583fc7b5951

{-| __unique:__ @ExampleNothingget_ret_long1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_36845109a4ce7992" hs_bindgen_test_macrosreparse_36845109a4ce7992 ::
     IO (Ptr.FunPtr (A -> IO FC.CLong))

{-# NOINLINE ret_long1_ptr #-}

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h:61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1_ptr :: Ptr.FunPtr (A -> IO FC.CLong)
ret_long1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_36845109a4ce7992

{-| __unique:__ @ExampleNothingget_ret_long2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_ac32dbc1e79e704e" hs_bindgen_test_macrosreparse_ac32dbc1e79e704e ::
     IO (Ptr.FunPtr (A -> IO FC.CLong))

{-# NOINLINE ret_long2_ptr #-}

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h:62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2_ptr :: Ptr.FunPtr (A -> IO FC.CLong)
ret_long2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_ac32dbc1e79e704e

{-| __unique:__ @ExampleNothingget_ret_long3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_6fba85ecad7d8d4e" hs_bindgen_test_macrosreparse_6fba85ecad7d8d4e ::
     IO (Ptr.FunPtr (A -> IO FC.CULong))

{-# NOINLINE ret_long3_ptr #-}

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h:63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3_ptr :: Ptr.FunPtr (A -> IO FC.CULong)
ret_long3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_6fba85ecad7d8d4e

{-| __unique:__ @ExampleNothingget_ret_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e9ac779a7c943add" hs_bindgen_test_macrosreparse_e9ac779a7c943add ::
     IO (Ptr.FunPtr (A -> IO FC.CFloat))

{-# NOINLINE ret_float_ptr #-}

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h:65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float_ptr :: Ptr.FunPtr (A -> IO FC.CFloat)
ret_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_e9ac779a7c943add

{-| __unique:__ @ExampleNothingget_ret_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_7095a5f5be3ecc0c" hs_bindgen_test_macrosreparse_7095a5f5be3ecc0c ::
     IO (Ptr.FunPtr (A -> IO FC.CDouble))

{-# NOINLINE ret_double_ptr #-}

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h:66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double_ptr :: Ptr.FunPtr (A -> IO FC.CDouble)
ret_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_7095a5f5be3ecc0c

{-| __unique:__ @ExampleNothingget_ret_bool1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c7b5be49f4314899" hs_bindgen_test_macrosreparse_c7b5be49f4314899 ::
     IO (Ptr.FunPtr (A -> IO FC.CBool))

{-# NOINLINE ret_bool1_ptr #-}

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h:67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1_ptr :: Ptr.FunPtr (A -> IO FC.CBool)
ret_bool1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_c7b5be49f4314899

{-| __unique:__ @ExampleNothingget_ret_struct_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_03ec23cf81b62ce3" hs_bindgen_test_macrosreparse_03ec23cf81b62ce3 ::
     IO (Ptr.FunPtr (A -> IO Some_struct))

{-# NOINLINE ret_struct_ptr #-}

{-| __C declaration:__ @ret_struct@

    __defined at:__ @macros\/reparse.h:69:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_struct_ptr :: Ptr.FunPtr (A -> IO Some_struct)
ret_struct_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_03ec23cf81b62ce3

{-| __unique:__ @ExampleNothingget_ret_union_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_5315544d48ea5b07" hs_bindgen_test_macrosreparse_5315544d48ea5b07 ::
     IO (Ptr.FunPtr (A -> IO Some_union))

{-# NOINLINE ret_union_ptr #-}

{-| __C declaration:__ @ret_union@

    __defined at:__ @macros\/reparse.h:70:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_union_ptr :: Ptr.FunPtr (A -> IO Some_union)
ret_union_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_5315544d48ea5b07

{-| __unique:__ @ExampleNothingget_ret_enum_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_9fb7ddbcd84c72f1" hs_bindgen_test_macrosreparse_9fb7ddbcd84c72f1 ::
     IO (Ptr.FunPtr (A -> IO Some_enum))

{-# NOINLINE ret_enum_ptr #-}

{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h:71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum_ptr :: Ptr.FunPtr (A -> IO Some_enum)
ret_enum_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_9fb7ddbcd84c72f1

{-| __unique:__ @ExampleNothingget_ret_pointer1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_0638bcad8813a303" hs_bindgen_test_macrosreparse_0638bcad8813a303 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE ret_pointer1_ptr #-}

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h:73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
ret_pointer1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_0638bcad8813a303

{-| __unique:__ @ExampleNothingget_ret_pointer2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_5d9ced9e4887782b" hs_bindgen_test_macrosreparse_5d9ced9e4887782b ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt))))

{-# NOINLINE ret_pointer2_ptr #-}

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h:74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt)))
ret_pointer2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_5d9ced9e4887782b

{-| __unique:__ @ExampleNothingget_ret_pointer3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_60e99361ec0a4b5b" hs_bindgen_test_macrosreparse_60e99361ec0a4b5b ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr Void)))

{-# NOINLINE ret_pointer3_ptr #-}

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h:75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr Void))
ret_pointer3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_60e99361ec0a4b5b

{-| __unique:__ @ExampleNothingget_body1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_cca1935605a94051" hs_bindgen_test_macrosreparse_cca1935605a94051 ::
     IO (Ptr.FunPtr (A -> IO FC.CInt))

{-# NOINLINE body1_ptr #-}

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h:79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1_ptr :: Ptr.FunPtr (A -> IO FC.CInt)
body1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_cca1935605a94051

{-| __unique:__ @ExampleNothingget_body2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_a1900daea7e14e95" hs_bindgen_test_macrosreparse_a1900daea7e14e95 ::
     IO (Ptr.FunPtr (IO A))

{-# NOINLINE body2_ptr #-}

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h:80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2_ptr :: Ptr.FunPtr (IO A)
body2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_a1900daea7e14e95

{-| __unique:__ @ExampleNothingget_args_complex_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c62f1e9d47469a1c" hs_bindgen_test_macrosreparse_c62f1e9d47469a1c ::
     IO (Ptr.FunPtr (A -> (Data.Complex.Complex FC.CFloat) -> IO ()))

{-# NOINLINE args_complex_float_ptr #-}

{-| __C declaration:__ @args_complex_float@

    __defined at:__ @macros\/reparse.h:84:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_float_ptr :: Ptr.FunPtr (A -> (Data.Complex.Complex FC.CFloat) -> IO ())
args_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_c62f1e9d47469a1c

{-| __unique:__ @ExampleNothingget_args_complex_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_b2ef5ed0a8ed0697" hs_bindgen_test_macrosreparse_b2ef5ed0a8ed0697 ::
     IO (Ptr.FunPtr (A -> (Data.Complex.Complex FC.CDouble) -> IO ()))

{-# NOINLINE args_complex_double_ptr #-}

{-| __C declaration:__ @args_complex_double@

    __defined at:__ @macros\/reparse.h:85:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_double_ptr :: Ptr.FunPtr (A -> (Data.Complex.Complex FC.CDouble) -> IO ())
args_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_b2ef5ed0a8ed0697

{-| __unique:__ @ExampleNothingget_ret_complex_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e2cc2aa2dd12852d" hs_bindgen_test_macrosreparse_e2cc2aa2dd12852d ::
     IO (Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CFloat)))

{-# NOINLINE ret_complex_float_ptr #-}

{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @macros\/reparse.h:86:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_float_ptr :: Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CFloat))
ret_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_e2cc2aa2dd12852d

{-| __unique:__ @ExampleNothingget_ret_complex_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c95961d571f78868" hs_bindgen_test_macrosreparse_c95961d571f78868 ::
     IO (Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CDouble)))

{-# NOINLINE ret_complex_double_ptr #-}

{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @macros\/reparse.h:87:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_double_ptr :: Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CDouble))
ret_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_c95961d571f78868

{-| __unique:__ @ExampleNothingget_bespoke_args1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_94c8a2d3574ba283" hs_bindgen_test_macrosreparse_94c8a2d3574ba283 ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE bespoke_args1_ptr #-}

{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @macros\/reparse.h:94:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args1_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
bespoke_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_94c8a2d3574ba283

{-| __unique:__ @ExampleNothingget_bespoke_args2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_2165985767a8d24e" hs_bindgen_test_macrosreparse_2165985767a8d24e ::
     IO (Ptr.FunPtr (A -> HsBindgen.Runtime.Prelude.CSize -> IO ()))

{-# NOINLINE bespoke_args2_ptr #-}

{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @macros\/reparse.h:95:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args2_ptr :: Ptr.FunPtr (A -> HsBindgen.Runtime.Prelude.CSize -> IO ())
bespoke_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_2165985767a8d24e

{-| __unique:__ @ExampleNothingget_bespoke_ret1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_7913bf38675bd912" hs_bindgen_test_macrosreparse_7913bf38675bd912 ::
     IO (Ptr.FunPtr (A -> IO FC.CBool))

{-# NOINLINE bespoke_ret1_ptr #-}

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h:97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1_ptr :: Ptr.FunPtr (A -> IO FC.CBool)
bespoke_ret1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_7913bf38675bd912

{-| __unique:__ @ExampleNothingget_bespoke_ret2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_07c419cb648cdf65" hs_bindgen_test_macrosreparse_07c419cb648cdf65 ::
     IO (Ptr.FunPtr (A -> IO HsBindgen.Runtime.Prelude.CSize))

{-# NOINLINE bespoke_ret2_ptr #-}

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h:98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2_ptr :: Ptr.FunPtr (A -> IO HsBindgen.Runtime.Prelude.CSize)
bespoke_ret2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_07c419cb648cdf65

{-| __unique:__ @ExampleNothingget_arr_args1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_ed19e51bcac06a9e" hs_bindgen_test_macrosreparse_ed19e51bcac06a9e ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ()))

{-# NOINLINE arr_args1_ptr #-}

{-| Arrays

__C declaration:__ @arr_args1@

__defined at:__ @macros\/reparse.h:104:6@

__exported by:__ @macros\/reparse.h@
-}
arr_args1_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ())
arr_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_ed19e51bcac06a9e

{-| __unique:__ @ExampleNothingget_arr_args2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_de3931a21a8a71fc" hs_bindgen_test_macrosreparse_de3931a21a8a71fc ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ()))

{-# NOINLINE arr_args2_ptr #-}

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h:105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ())
arr_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_de3931a21a8a71fc

{-| __unique:__ @ExampleNothingget_arr_args3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_2c02effa6288a26b" hs_bindgen_test_macrosreparse_2c02effa6288a26b ::
     IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) A) -> IO ()))

{-# NOINLINE arr_args3_ptr #-}

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h:106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) A) -> IO ())
arr_args3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_2c02effa6288a26b

{-| __unique:__ @ExampleNothingget_arr_args4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_2144e300082f115c" hs_bindgen_test_macrosreparse_2144e300082f115c ::
     IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) (Ptr.Ptr A)) -> IO ()))

{-# NOINLINE arr_args4_ptr #-}

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h:107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) (Ptr.Ptr A)) -> IO ())
arr_args4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_2144e300082f115c

{-| __unique:__ @ExampleNothingget_funptr_args1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_d1645262a53743f6" hs_bindgen_test_macrosreparse_d1645262a53743f6 ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (IO ())) -> IO ()))

{-# NOINLINE funptr_args1_ptr #-}

{-| Function pointers

__C declaration:__ @funptr_args1@

__defined at:__ @macros\/reparse.h:126:6@

__exported by:__ @macros\/reparse.h@
-}
funptr_args1_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (IO ())) -> IO ())
funptr_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_d1645262a53743f6

{-| __unique:__ @ExampleNothingget_funptr_args2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_d66507630e4e38e3" hs_bindgen_test_macrosreparse_d66507630e4e38e3 ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ()))

{-# NOINLINE funptr_args2_ptr #-}

{-| __C declaration:__ @funptr_args2@

    __defined at:__ @macros\/reparse.h:127:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args2_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ())
funptr_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_d66507630e4e38e3

{-| __unique:__ @ExampleNothingget_funptr_args3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_3d7907ab53b617cf" hs_bindgen_test_macrosreparse_3d7907ab53b617cf ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> IO ())) -> IO ()))

{-# NOINLINE funptr_args3_ptr #-}

{-| __C declaration:__ @funptr_args3@

    __defined at:__ @macros\/reparse.h:128:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args3_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> IO ())) -> IO ())
funptr_args3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_3d7907ab53b617cf

{-| __unique:__ @ExampleNothingget_funptr_args4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e4d15a9c3b04292a" hs_bindgen_test_macrosreparse_e4d15a9c3b04292a ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)) -> IO ()))

{-# NOINLINE funptr_args4_ptr #-}

{-| __C declaration:__ @funptr_args4@

    __defined at:__ @macros\/reparse.h:129:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args4_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)) -> IO ())
funptr_args4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_e4d15a9c3b04292a

{-| __unique:__ @ExampleNothingget_funptr_args5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_ced7918b6e42102f" hs_bindgen_test_macrosreparse_ced7918b6e42102f ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))) -> IO ()))

{-# NOINLINE funptr_args5_ptr #-}

{-| __C declaration:__ @funptr_args5@

    __defined at:__ @macros\/reparse.h:130:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args5_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))) -> IO ())
funptr_args5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_ced7918b6e42102f

{-| __unique:__ @ExampleNothingget_comments1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c90ec05081ef4e64" hs_bindgen_test_macrosreparse_c90ec05081ef4e64 ::
     IO (Ptr.FunPtr (A -> IO ()))

{-# NOINLINE comments1_ptr #-}

{-| Comments in awkward places

  (Prior to language-c we failed to parse there.)

__C declaration:__ @comments1@

__defined at:__ @macros\/reparse.h:144:25@

__exported by:__ @macros\/reparse.h@
-}
comments1_ptr :: Ptr.FunPtr (A -> IO ())
comments1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_c90ec05081ef4e64

{-| __unique:__ @ExampleNothingget_const_prim_before1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_6ac4b42c66a36448" hs_bindgen_test_macrosreparse_6ac4b42c66a36448 ::
     IO (Ptr.FunPtr (A -> FC.CChar -> IO ()))

{-# NOINLINE const_prim_before1_ptr #-}

{-| `const` qualifier

  NOTE: These were not parsed correctly prior to the switch to language-c.

__C declaration:__ @const_prim_before1@

__defined at:__ @macros\/reparse.h:179:6@

__exported by:__ @macros\/reparse.h@
-}
const_prim_before1_ptr :: Ptr.FunPtr (A -> FC.CChar -> IO ())
const_prim_before1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_6ac4b42c66a36448

{-| __unique:__ @ExampleNothingget_const_prim_before2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_f98632ef2e69b003" hs_bindgen_test_macrosreparse_f98632ef2e69b003 ::
     IO (Ptr.FunPtr (A -> FC.CSChar -> IO ()))

{-# NOINLINE const_prim_before2_ptr #-}

{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @macros\/reparse.h:180:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before2_ptr :: Ptr.FunPtr (A -> FC.CSChar -> IO ())
const_prim_before2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_f98632ef2e69b003

{-| __unique:__ @ExampleNothingget_const_prim_before3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_cc9db1f6a36b8221" hs_bindgen_test_macrosreparse_cc9db1f6a36b8221 ::
     IO (Ptr.FunPtr (A -> FC.CUChar -> IO ()))

{-# NOINLINE const_prim_before3_ptr #-}

{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @macros\/reparse.h:181:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before3_ptr :: Ptr.FunPtr (A -> FC.CUChar -> IO ())
const_prim_before3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_cc9db1f6a36b8221

{-| __unique:__ @ExampleNothingget_const_prim_after1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_3e5b7273bf2ecadb" hs_bindgen_test_macrosreparse_3e5b7273bf2ecadb ::
     IO (Ptr.FunPtr (A -> FC.CChar -> IO ()))

{-# NOINLINE const_prim_after1_ptr #-}

{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @macros\/reparse.h:182:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after1_ptr :: Ptr.FunPtr (A -> FC.CChar -> IO ())
const_prim_after1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_3e5b7273bf2ecadb

{-| __unique:__ @ExampleNothingget_const_prim_after2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_f9b4beeca8253333" hs_bindgen_test_macrosreparse_f9b4beeca8253333 ::
     IO (Ptr.FunPtr (A -> FC.CSChar -> IO ()))

{-# NOINLINE const_prim_after2_ptr #-}

{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @macros\/reparse.h:183:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after2_ptr :: Ptr.FunPtr (A -> FC.CSChar -> IO ())
const_prim_after2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_f9b4beeca8253333

{-| __unique:__ @ExampleNothingget_const_prim_after3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_bf14e2fd88b25311" hs_bindgen_test_macrosreparse_bf14e2fd88b25311 ::
     IO (Ptr.FunPtr (A -> FC.CUChar -> IO ()))

{-# NOINLINE const_prim_after3_ptr #-}

{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @macros\/reparse.h:184:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after3_ptr :: Ptr.FunPtr (A -> FC.CUChar -> IO ())
const_prim_after3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_bf14e2fd88b25311

{-| __unique:__ @ExampleNothingget_const_withoutSign_before1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_3649293fcaa1543c" hs_bindgen_test_macrosreparse_3649293fcaa1543c ::
     IO (Ptr.FunPtr (A -> FC.CFloat -> IO ()))

{-# NOINLINE const_withoutSign_before1_ptr #-}

{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @macros\/reparse.h:188:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before1_ptr :: Ptr.FunPtr (A -> FC.CFloat -> IO ())
const_withoutSign_before1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_3649293fcaa1543c

{-| __unique:__ @ExampleNothingget_const_withoutSign_before2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_ad5903c28e22dd2c" hs_bindgen_test_macrosreparse_ad5903c28e22dd2c ::
     IO (Ptr.FunPtr (A -> FC.CDouble -> IO ()))

{-# NOINLINE const_withoutSign_before2_ptr #-}

{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @macros\/reparse.h:189:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before2_ptr :: Ptr.FunPtr (A -> FC.CDouble -> IO ())
const_withoutSign_before2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_ad5903c28e22dd2c

{-| __unique:__ @ExampleNothingget_const_withoutSign_before3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e7b9bc011ec1dd8a" hs_bindgen_test_macrosreparse_e7b9bc011ec1dd8a ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE const_withoutSign_before3_ptr #-}

{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @macros\/reparse.h:190:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before3_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_before3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_e7b9bc011ec1dd8a

{-| __unique:__ @ExampleNothingget_const_withoutSign_before4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_4fd66b696848dd98" hs_bindgen_test_macrosreparse_4fd66b696848dd98 ::
     IO (Ptr.FunPtr (A -> Some_struct -> IO ()))

{-# NOINLINE const_withoutSign_before4_ptr #-}

{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @macros\/reparse.h:191:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before4_ptr :: Ptr.FunPtr (A -> Some_struct -> IO ())
const_withoutSign_before4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_4fd66b696848dd98

{-| __unique:__ @ExampleNothingget_const_withoutSign_before5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_42582e1882927f7e" hs_bindgen_test_macrosreparse_42582e1882927f7e ::
     IO (Ptr.FunPtr (A -> Some_union -> IO ()))

{-# NOINLINE const_withoutSign_before5_ptr #-}

{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @macros\/reparse.h:192:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before5_ptr :: Ptr.FunPtr (A -> Some_union -> IO ())
const_withoutSign_before5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_42582e1882927f7e

{-| __unique:__ @ExampleNothingget_const_withoutSign_before6_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_b6876e53e4b27a98" hs_bindgen_test_macrosreparse_b6876e53e4b27a98 ::
     IO (Ptr.FunPtr (A -> Some_enum -> IO ()))

{-# NOINLINE const_withoutSign_before6_ptr #-}

{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @macros\/reparse.h:193:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before6_ptr :: Ptr.FunPtr (A -> Some_enum -> IO ())
const_withoutSign_before6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_b6876e53e4b27a98

{-| __unique:__ @ExampleNothingget_const_withoutSign_before7_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_78763cbecd2b0750" hs_bindgen_test_macrosreparse_78763cbecd2b0750 ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE const_withoutSign_before7_ptr #-}

{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @macros\/reparse.h:194:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before7_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_before7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_78763cbecd2b0750

{-| __unique:__ @ExampleNothingget_const_withoutSign_before8_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_4098c4a4ccd31d36" hs_bindgen_test_macrosreparse_4098c4a4ccd31d36 ::
     IO (Ptr.FunPtr (A -> HsBindgen.Runtime.Prelude.CSize -> IO ()))

{-# NOINLINE const_withoutSign_before8_ptr #-}

{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @macros\/reparse.h:195:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before8_ptr :: Ptr.FunPtr (A -> HsBindgen.Runtime.Prelude.CSize -> IO ())
const_withoutSign_before8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_4098c4a4ccd31d36

{-| __unique:__ @ExampleNothingget_const_withoutSign_after1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e9148eb7b8dac901" hs_bindgen_test_macrosreparse_e9148eb7b8dac901 ::
     IO (Ptr.FunPtr (A -> FC.CFloat -> IO ()))

{-# NOINLINE const_withoutSign_after1_ptr #-}

{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @macros\/reparse.h:197:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after1_ptr :: Ptr.FunPtr (A -> FC.CFloat -> IO ())
const_withoutSign_after1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_e9148eb7b8dac901

{-| __unique:__ @ExampleNothingget_const_withoutSign_after2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_8663653d89116be9" hs_bindgen_test_macrosreparse_8663653d89116be9 ::
     IO (Ptr.FunPtr (A -> FC.CDouble -> IO ()))

{-# NOINLINE const_withoutSign_after2_ptr #-}

{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @macros\/reparse.h:198:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after2_ptr :: Ptr.FunPtr (A -> FC.CDouble -> IO ())
const_withoutSign_after2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_8663653d89116be9

{-| __unique:__ @ExampleNothingget_const_withoutSign_after3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_136dcba145bf241b" hs_bindgen_test_macrosreparse_136dcba145bf241b ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE const_withoutSign_after3_ptr #-}

{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @macros\/reparse.h:199:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after3_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_after3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_136dcba145bf241b

{-| __unique:__ @ExampleNothingget_const_withoutSign_after4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_380e01acce794cab" hs_bindgen_test_macrosreparse_380e01acce794cab ::
     IO (Ptr.FunPtr (A -> Some_struct -> IO ()))

{-# NOINLINE const_withoutSign_after4_ptr #-}

{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @macros\/reparse.h:200:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after4_ptr :: Ptr.FunPtr (A -> Some_struct -> IO ())
const_withoutSign_after4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_380e01acce794cab

{-| __unique:__ @ExampleNothingget_const_withoutSign_after5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_af0d84d0757f6c2c" hs_bindgen_test_macrosreparse_af0d84d0757f6c2c ::
     IO (Ptr.FunPtr (A -> Some_union -> IO ()))

{-# NOINLINE const_withoutSign_after5_ptr #-}

{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @macros\/reparse.h:201:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after5_ptr :: Ptr.FunPtr (A -> Some_union -> IO ())
const_withoutSign_after5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_af0d84d0757f6c2c

{-| __unique:__ @ExampleNothingget_const_withoutSign_after6_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_df92501d07bf6c5f" hs_bindgen_test_macrosreparse_df92501d07bf6c5f ::
     IO (Ptr.FunPtr (A -> Some_enum -> IO ()))

{-# NOINLINE const_withoutSign_after6_ptr #-}

{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @macros\/reparse.h:202:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after6_ptr :: Ptr.FunPtr (A -> Some_enum -> IO ())
const_withoutSign_after6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_df92501d07bf6c5f

{-| __unique:__ @ExampleNothingget_const_withoutSign_after7_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_b41148ca40ec8eb5" hs_bindgen_test_macrosreparse_b41148ca40ec8eb5 ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE const_withoutSign_after7_ptr #-}

{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @macros\/reparse.h:203:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after7_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_after7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_b41148ca40ec8eb5

{-| __unique:__ @ExampleNothingget_const_withoutSign_after8_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_560c9dfdb530548b" hs_bindgen_test_macrosreparse_560c9dfdb530548b ::
     IO (Ptr.FunPtr (A -> HsBindgen.Runtime.Prelude.CSize -> IO ()))

{-# NOINLINE const_withoutSign_after8_ptr #-}

{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @macros\/reparse.h:204:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after8_ptr :: Ptr.FunPtr (A -> HsBindgen.Runtime.Prelude.CSize -> IO ())
const_withoutSign_after8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_560c9dfdb530548b

{-| __unique:__ @ExampleNothingget_const_pointers_args1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_a34d16c099748839" hs_bindgen_test_macrosreparse_a34d16c099748839 ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args1_ptr #-}

{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @macros\/reparse.h:208:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args1_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_a34d16c099748839

{-| __unique:__ @ExampleNothingget_const_pointers_args2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_45235edaf5c3b599" hs_bindgen_test_macrosreparse_45235edaf5c3b599 ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args2_ptr #-}

{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @macros\/reparse.h:209:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args2_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_45235edaf5c3b599

{-| __unique:__ @ExampleNothingget_const_pointers_args3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_3dbcf1c7202f2878" hs_bindgen_test_macrosreparse_3dbcf1c7202f2878 ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args3_ptr #-}

{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @macros\/reparse.h:210:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args3_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_3dbcf1c7202f2878

{-| __unique:__ @ExampleNothingget_const_pointers_args4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_a6624f6cc0a062af" hs_bindgen_test_macrosreparse_a6624f6cc0a062af ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args4_ptr #-}

{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @macros\/reparse.h:211:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args4_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_a6624f6cc0a062af

{-| __unique:__ @ExampleNothingget_const_pointers_args5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c5f3253c57910315" hs_bindgen_test_macrosreparse_c5f3253c57910315 ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args5_ptr #-}

{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @macros\/reparse.h:212:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args5_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_c5f3253c57910315

{-| __unique:__ @ExampleNothingget_const_pointers_ret1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_1990ded85ea3850d" hs_bindgen_test_macrosreparse_1990ded85ea3850d ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret1_ptr #-}

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h:214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_1990ded85ea3850d

{-| __unique:__ @ExampleNothingget_const_pointers_ret2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_627cc570c3ca7d19" hs_bindgen_test_macrosreparse_627cc570c3ca7d19 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret2_ptr #-}

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h:215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_627cc570c3ca7d19

{-| __unique:__ @ExampleNothingget_const_pointers_ret3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_2f449708b5a275b1" hs_bindgen_test_macrosreparse_2f449708b5a275b1 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret3_ptr #-}

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h:216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_2f449708b5a275b1

{-| __unique:__ @ExampleNothingget_const_pointers_ret4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_67662618cd011c8a" hs_bindgen_test_macrosreparse_67662618cd011c8a ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret4_ptr #-}

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h:217:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_67662618cd011c8a

{-| __unique:__ @ExampleNothingget_const_pointers_ret5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_fcafd9f8ac329995" hs_bindgen_test_macrosreparse_fcafd9f8ac329995 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret5_ptr #-}

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h:218:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_fcafd9f8ac329995

{-| __unique:__ @ExampleNothingget_const_array_elem1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_6928906fc9a88dfc" hs_bindgen_test_macrosreparse_6928906fc9a88dfc ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ()))

{-# NOINLINE const_array_elem1_ptr #-}

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h:246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ())
const_array_elem1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_6928906fc9a88dfc

{-| __unique:__ @ExampleNothingget_const_array_elem2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_625a37e9c030891a" hs_bindgen_test_macrosreparse_625a37e9c030891a ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ()))

{-# NOINLINE const_array_elem2_ptr #-}

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h:247:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ())
const_array_elem2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_625a37e9c030891a

{-| __unique:__ @ExampleNothingget_const_array_elem3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_5e23f87114cf51fb" hs_bindgen_test_macrosreparse_5e23f87114cf51fb ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ()))

{-# NOINLINE const_array_elem3_ptr #-}

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h:248:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ())
const_array_elem3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_5e23f87114cf51fb

{-| __unique:__ @ExampleNothingget_noParams1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_d50620a002265139" hs_bindgen_test_macrosreparse_d50620a002265139 ::
     IO (Ptr.FunPtr (IO A))

{-# NOINLINE noParams1_ptr #-}

{-| Other examples we reparsed /incorrectly/ before language-c

__C declaration:__ @noParams1@

__defined at:__ @macros\/reparse.h:256:3@

__exported by:__ @macros\/reparse.h@
-}
noParams1_ptr :: Ptr.FunPtr (IO A)
noParams1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_d50620a002265139

{-| __unique:__ @ExampleNothingget_noParams2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_03b0e24786b82ad5" hs_bindgen_test_macrosreparse_03b0e24786b82ad5 ::
     IO (Ptr.FunPtr (IO A))

{-# NOINLINE noParams2_ptr #-}

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h:257:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2_ptr :: Ptr.FunPtr (IO A)
noParams2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_03b0e24786b82ad5

{-| __unique:__ @ExampleNothingget_noParams3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_36508fd99a0556c5" hs_bindgen_test_macrosreparse_36508fd99a0556c5 ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ()))

{-# NOINLINE noParams3_ptr #-}

{-| __C declaration:__ @noParams3@

    __defined at:__ @macros\/reparse.h:258:6@

    __exported by:__ @macros\/reparse.h@
-}
noParams3_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ())
noParams3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_36508fd99a0556c5

{-| __unique:__ @ExampleNothingget_funptr_ret1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_6f83a48dd177c25f" hs_bindgen_test_macrosreparse_6f83a48dd177c25f ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO ()))))

{-# NOINLINE funptr_ret1_ptr #-}

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h:262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO ())))
funptr_ret1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_6f83a48dd177c25f

{-| __unique:__ @ExampleNothingget_funptr_ret2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_f12efafd1525ef7f" hs_bindgen_test_macrosreparse_f12efafd1525ef7f ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO FC.CInt))))

{-# NOINLINE funptr_ret2_ptr #-}

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h:263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO FC.CInt)))
funptr_ret2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_f12efafd1525ef7f

{-| __unique:__ @ExampleNothingget_funptr_ret3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_b00baa5b9708b9e7" hs_bindgen_test_macrosreparse_b00baa5b9708b9e7 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> IO ()))))

{-# NOINLINE funptr_ret3_ptr #-}

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h:264:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> IO ())))
funptr_ret3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_b00baa5b9708b9e7

{-| __unique:__ @ExampleNothingget_funptr_ret4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c51872479ceff42e" hs_bindgen_test_macrosreparse_c51872479ceff42e ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar))))

{-# NOINLINE funptr_ret4_ptr #-}

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h:265:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)))
funptr_ret4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_c51872479ceff42e

{-| __unique:__ @ExampleNothingget_funptr_ret5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_3b9b9924b4b4d7ea" hs_bindgen_test_macrosreparse_3b9b9924b4b4d7ea ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret5_ptr #-}

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h:269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_3b9b9924b4b4d7ea

{-| __unique:__ @ExampleNothingget_funptr_ret6_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_3df5ab4b0b306845" hs_bindgen_test_macrosreparse_3df5ab4b0b306845 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret6_ptr #-}

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h:270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_3df5ab4b0b306845

{-| __unique:__ @ExampleNothingget_funptr_ret7_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_2ac4454d93b6f04a" hs_bindgen_test_macrosreparse_2ac4454d93b6f04a ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret7_ptr #-}

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h:271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_2ac4454d93b6f04a

{-| __unique:__ @ExampleNothingget_funptr_ret8_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_411c5128f18364b3" hs_bindgen_test_macrosreparse_411c5128f18364b3 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret8_ptr #-}

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h:272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_411c5128f18364b3

{-| __unique:__ @ExampleNothingget_funptr_ret9_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_693a8d16e17d0cdc" hs_bindgen_test_macrosreparse_693a8d16e17d0cdc ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret9_ptr #-}

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h:273:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_693a8d16e17d0cdc

{-| __unique:__ @ExampleNothingget_funptr_ret10_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_9d2da81bbfe49ab6" hs_bindgen_test_macrosreparse_9d2da81bbfe49ab6 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret10_ptr #-}

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h:274:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_9d2da81bbfe49ab6
