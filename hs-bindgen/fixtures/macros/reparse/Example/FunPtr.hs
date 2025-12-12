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
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <macros/reparse.h>"
  , "/* test_macrosreparse_Example_get_args_char1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_83aaba90c800683a (void)) ("
  , "  A arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &args_char1;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_char2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e7c58099a677e598 (void)) ("
  , "  A arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  return &args_char2;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_char3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cc33f1bf42bb14f7 (void)) ("
  , "  A arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  return &args_char3;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_short1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_daf63941377bc30d (void)) ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short1;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_short2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f832e83c66e73e1b (void)) ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short2;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_short3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fddcd4eb9a3ac90f (void)) ("
  , "  A arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  return &args_short3;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_int1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8f495550fa03ecd7 (void)) ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int1;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_int2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_488a7dcf2bd33678 (void)) ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int2;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_int3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5c6e85e96378ce0f (void)) ("
  , "  A arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  return &args_int3;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_long1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_afcad03e61d3f83b (void)) ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long1;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_long2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2d32bae595df94c2 (void)) ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long2;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_long3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0f7d316338eac027 (void)) ("
  , "  A arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  return &args_long3;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_float_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9ed3dd630c6a5c91 (void)) ("
  , "  A arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  return &args_float;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_double_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c1afad204f639896 (void)) ("
  , "  A arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &args_double;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_bool1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c7091d8aa6313541 (void)) ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &args_bool1;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_struct_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c14722de6f25d3c0 (void)) ("
  , "  A arg1,"
  , "  struct some_struct arg2"
  , ")"
  , "{"
  , "  return &args_struct;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_union_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a94ca07a5083d898 (void)) ("
  , "  A arg1,"
  , "  union some_union arg2"
  , ")"
  , "{"
  , "  return &args_union;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_enum_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2fdbcc2976b884f7 (void)) ("
  , "  A arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  return &args_enum;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_pointer1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_65b8da715d77e581 (void)) ("
  , "  A arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return &args_pointer1;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_pointer2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_767fe5b679ba43e4 (void)) ("
  , "  A arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  return &args_pointer2;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_pointer3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_852cc5784297324b (void)) ("
  , "  A arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  return &args_pointer3;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_A_ptr */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_89cbc210fb67bc53 (void)) (void)"
  , "{"
  , "  return &ret_A;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_char1_ptr */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_d95a16b3f46326f5 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char1;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_char2_ptr */"
  , "__attribute__ ((const))"
  , "signed char (*hs_bindgen_dbb14b4445c045dc (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char2;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_char3_ptr */"
  , "__attribute__ ((const))"
  , "unsigned char (*hs_bindgen_18d70300449e2a05 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char3;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_short1_ptr */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_7f113070dda67da8 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short1;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_short2_ptr */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_601d9c0a30f1855b (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short2;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_short3_ptr */"
  , "__attribute__ ((const))"
  , "unsigned short (*hs_bindgen_eb1f70424e0c701d (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short3;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_int1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_28a93ce9f2a99cd0 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int1;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_int2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a70295d21f766087 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int2;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_int3_ptr */"
  , "__attribute__ ((const))"
  , "unsigned int (*hs_bindgen_4239c3dd15ab11f3 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int3;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_long1_ptr */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_b24935761b06cfd8 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long1;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_long2_ptr */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_35a17cc5266d3326 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long2;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_long3_ptr */"
  , "__attribute__ ((const))"
  , "unsigned long (*hs_bindgen_59489620015c271e (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long3;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_float_ptr */"
  , "__attribute__ ((const))"
  , "float (*hs_bindgen_52138c45b539427d (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_float;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_double_ptr */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_283d5d098a9c4a59 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_double;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_bool1_ptr */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_382098412cbd94ff (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_bool1;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_struct_ptr */"
  , "__attribute__ ((const))"
  , "struct some_struct (*hs_bindgen_51cf9857b3cc1843 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_struct;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_union_ptr */"
  , "__attribute__ ((const))"
  , "union some_union (*hs_bindgen_3df1073dbf5d79f4 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_union;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_enum_ptr */"
  , "__attribute__ ((const))"
  , "enum some_enum (*hs_bindgen_c0467f7279732ddd (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_enum;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_pointer1_ptr */"
  , "__attribute__ ((const))"
  , "signed int *(*hs_bindgen_f0240baaa70df9bd (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer1;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_pointer2_ptr */"
  , "__attribute__ ((const))"
  , "signed int **(*hs_bindgen_019cbfb4d24d1d91 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer2;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_pointer3_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_a654e9f8ca0d53c5 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer3;"
  , "}"
  , "/* test_macrosreparse_Example_get_body1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_94eff7815581584b (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &body1;"
  , "}"
  , "/* test_macrosreparse_Example_get_body2_ptr */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_f98b0963b05f261c (void)) (void)"
  , "{"
  , "  return &body2;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_complex_float_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e2f3985767c79559 (void)) ("
  , "  A arg1,"
  , "  float _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_float;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_complex_double_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1da23b0894c2e548 (void)) ("
  , "  A arg1,"
  , "  double _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_double;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_complex_float_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex (*hs_bindgen_e3d89b51410d7614 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_complex_float;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_complex_double_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex (*hs_bindgen_7cc277a18abf87b8 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_complex_double;"
  , "}"
  , "/* test_macrosreparse_Example_get_bespoke_args1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_764ddaf3efe7bd53 (void)) ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &bespoke_args1;"
  , "}"
  , "/* test_macrosreparse_Example_get_bespoke_args2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_53ccc1b308cd8384 (void)) ("
  , "  A arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &bespoke_args2;"
  , "}"
  , "/* test_macrosreparse_Example_get_bespoke_ret1_ptr */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_e61f250910ddc098 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret1;"
  , "}"
  , "/* test_macrosreparse_Example_get_bespoke_ret2_ptr */"
  , "__attribute__ ((const))"
  , "size_t (*hs_bindgen_b9864dba6e30c078 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret2;"
  , "}"
  , "/* test_macrosreparse_Example_get_arr_args1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_bb7f94a203c14e76 (void)) ("
  , "  A arg1[]"
  , ")"
  , "{"
  , "  return &arr_args1;"
  , "}"
  , "/* test_macrosreparse_Example_get_arr_args2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ca2476976e4721ef (void)) ("
  , "  A *arg1[]"
  , ")"
  , "{"
  , "  return &arr_args2;"
  , "}"
  , "/* test_macrosreparse_Example_get_arr_args3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3298ac669c00b1cd (void)) ("
  , "  A arg1[5]"
  , ")"
  , "{"
  , "  return &arr_args3;"
  , "}"
  , "/* test_macrosreparse_Example_get_arr_args4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_329a5d4b44b11e6e (void)) ("
  , "  A *arg1[5]"
  , ")"
  , "{"
  , "  return &arr_args4;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_args1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_dac9e3bdccb6a4eb (void)) ("
  , "  A arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args1;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_args2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_83d7f85727e54da4 (void)) ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args2;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_args3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_107e06f31f9dd017 (void)) ("
  , "  A arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args3;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_args4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_51a7f0cfbd57eaf7 (void)) ("
  , "  A arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args4;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_args5_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4a86c3a3b98a00d9 (void)) ("
  , "  A arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args5;"
  , "}"
  , "/* test_macrosreparse_Example_get_comments1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1b13b480c009cf44 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &comments1;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_prim_before1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_410cb526b4cee637 (void)) ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before1;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_prim_before2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3b1e7a350d422127 (void)) ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before2;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_prim_before3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8aab98c0f956e496 (void)) ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before3;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_prim_after1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_aeee0dd2b067cf07 (void)) ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after1;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_prim_after2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ca5ab7dc437ce5d1 (void)) ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after2;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_prim_after3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a6ae03f6051fcb2a (void)) ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after3;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_before1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_aea82678489f8007 (void)) ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before1;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_before2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_79b5f5987a75db98 (void)) ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before2;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_before3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fafbaf6c727e6e6d (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before3;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_before4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c9342430ac667d8a (void)) ("
  , "  A arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before4;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_before5_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_25501097b98452bd (void)) ("
  , "  A arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before5;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_before6_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4b356af92ea4b405 (void)) ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before6;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_before7_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_90b574ff639ebbd5 (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before7;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_before8_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cbb78eb3b806c344 (void)) ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before8;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_after1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f4083b3232462a5b (void)) ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after1;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_after2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cf16d660d9d916df (void)) ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after2;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_after3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_41a40ed22011f536 (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after3;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_after4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4bc0069f381d29c9 (void)) ("
  , "  A arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after4;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_after5_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e064a509e456b021 (void)) ("
  , "  A arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after5;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_after6_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b89597d47b21f2fd (void)) ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after6;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_after7_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_127b2fb737af1d7a (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after7;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_after8_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_05c7bd4fa507a58c (void)) ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after8;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_args1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_dfa6f2ec505f391a (void)) ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args1;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_args2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2f758756849ca2b5 (void)) ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args2;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_args3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1f9d3190b4433852 (void)) ("
  , "  A arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args3;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_args4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1579ab85f0fa217b (void)) ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args4;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_args5_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b4770dc5310bc558 (void)) ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args5;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_ret1_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_8422fbf55ee37cbb (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret1;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_ret2_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_7d62d267cb012ebf (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret2;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_ret3_ptr */"
  , "__attribute__ ((const))"
  , "signed int *const (*hs_bindgen_d56e13b56b7e1cf7 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret3;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_ret4_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_bedc6b38f49c61ea (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret4;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_ret5_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_8d027f9f58006eb9 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret5;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_array_elem1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_882567df89856ac9 (void)) ("
  , "  A const arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem1;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_array_elem2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_70e4ab7a50eb5360 (void)) ("
  , "  A const *arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem2;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_array_elem3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_14a733fd770b7242 (void)) ("
  , "  A *const arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem3;"
  , "}"
  , "/* test_macrosreparse_Example_get_noParams1_ptr */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_8e462fca4a002e73 (void)) (void)"
  , "{"
  , "  return &noParams1;"
  , "}"
  , "/* test_macrosreparse_Example_get_noParams2_ptr */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_42667590c95d450e (void)) (void)"
  , "{"
  , "  return &noParams2;"
  , "}"
  , "/* test_macrosreparse_Example_get_noParams3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_23777cd9313c8c63 (void)) ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &noParams3;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret1_ptr */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_a8f974caf74669f9 (void)) ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret1;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_f13795ebabb26526 (void)) ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret2;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret3_ptr */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_2515837794143ac1 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &funptr_ret3;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret4_ptr */"
  , "__attribute__ ((const))"
  , "char (*(*hs_bindgen_f01ceaf447c3de04 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret4;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret5_ptr */"
  , "__attribute__ ((const))"
  , "signed int *(*(*hs_bindgen_3cb2c77a66e6f46f (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret5;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret6_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_3a28c985fce638f9 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret6;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret7_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_e155fd240d710be2 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret7;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret8_ptr */"
  , "__attribute__ ((const))"
  , "signed int *const (*(*hs_bindgen_61261c2147d69f98 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret8;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret9_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_e3c71dfaf82486c8 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret9;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret10_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_6a47446b9176f0bf (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret10;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_83aaba90c800683a" hs_bindgen_83aaba90c800683a_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_char1_ptr@
hs_bindgen_83aaba90c800683a ::
     IO (Ptr.FunPtr (A -> FC.CChar -> IO ()))
hs_bindgen_83aaba90c800683a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_83aaba90c800683a_base

{-# NOINLINE args_char1_ptr #-}

{-| Function declarations

__C declaration:__ @args_char1@

__defined at:__ @macros\/reparse.h:17:6@

__exported by:__ @macros\/reparse.h@
-}
args_char1_ptr :: Ptr.FunPtr (A -> FC.CChar -> IO ())
args_char1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_83aaba90c800683a

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_e7c58099a677e598" hs_bindgen_e7c58099a677e598_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_char2_ptr@
hs_bindgen_e7c58099a677e598 ::
     IO (Ptr.FunPtr (A -> FC.CSChar -> IO ()))
hs_bindgen_e7c58099a677e598 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_e7c58099a677e598_base

{-# NOINLINE args_char2_ptr #-}

{-| __C declaration:__ @args_char2@

    __defined at:__ @macros\/reparse.h:18:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char2_ptr :: Ptr.FunPtr (A -> FC.CSChar -> IO ())
args_char2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e7c58099a677e598

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_cc33f1bf42bb14f7" hs_bindgen_cc33f1bf42bb14f7_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_char3_ptr@
hs_bindgen_cc33f1bf42bb14f7 ::
     IO (Ptr.FunPtr (A -> FC.CUChar -> IO ()))
hs_bindgen_cc33f1bf42bb14f7 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_cc33f1bf42bb14f7_base

{-# NOINLINE args_char3_ptr #-}

{-| __C declaration:__ @args_char3@

    __defined at:__ @macros\/reparse.h:19:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char3_ptr :: Ptr.FunPtr (A -> FC.CUChar -> IO ())
args_char3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cc33f1bf42bb14f7

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_daf63941377bc30d" hs_bindgen_daf63941377bc30d_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_short1_ptr@
hs_bindgen_daf63941377bc30d ::
     IO (Ptr.FunPtr (A -> FC.CShort -> IO ()))
hs_bindgen_daf63941377bc30d =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_daf63941377bc30d_base

{-# NOINLINE args_short1_ptr #-}

{-| __C declaration:__ @args_short1@

    __defined at:__ @macros\/reparse.h:21:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short1_ptr :: Ptr.FunPtr (A -> FC.CShort -> IO ())
args_short1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_daf63941377bc30d

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f832e83c66e73e1b" hs_bindgen_f832e83c66e73e1b_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_short2_ptr@
hs_bindgen_f832e83c66e73e1b ::
     IO (Ptr.FunPtr (A -> FC.CShort -> IO ()))
hs_bindgen_f832e83c66e73e1b =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_f832e83c66e73e1b_base

{-# NOINLINE args_short2_ptr #-}

{-| __C declaration:__ @args_short2@

    __defined at:__ @macros\/reparse.h:22:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short2_ptr :: Ptr.FunPtr (A -> FC.CShort -> IO ())
args_short2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f832e83c66e73e1b

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_fddcd4eb9a3ac90f" hs_bindgen_fddcd4eb9a3ac90f_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_short3_ptr@
hs_bindgen_fddcd4eb9a3ac90f ::
     IO (Ptr.FunPtr (A -> FC.CUShort -> IO ()))
hs_bindgen_fddcd4eb9a3ac90f =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_fddcd4eb9a3ac90f_base

{-# NOINLINE args_short3_ptr #-}

{-| __C declaration:__ @args_short3@

    __defined at:__ @macros\/reparse.h:23:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short3_ptr :: Ptr.FunPtr (A -> FC.CUShort -> IO ())
args_short3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fddcd4eb9a3ac90f

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_8f495550fa03ecd7" hs_bindgen_8f495550fa03ecd7_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_int1_ptr@
hs_bindgen_8f495550fa03ecd7 ::
     IO (Ptr.FunPtr (A -> FC.CInt -> IO ()))
hs_bindgen_8f495550fa03ecd7 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_8f495550fa03ecd7_base

{-# NOINLINE args_int1_ptr #-}

{-| __C declaration:__ @args_int1@

    __defined at:__ @macros\/reparse.h:25:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int1_ptr :: Ptr.FunPtr (A -> FC.CInt -> IO ())
args_int1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8f495550fa03ecd7

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_488a7dcf2bd33678" hs_bindgen_488a7dcf2bd33678_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_int2_ptr@
hs_bindgen_488a7dcf2bd33678 ::
     IO (Ptr.FunPtr (A -> FC.CInt -> IO ()))
hs_bindgen_488a7dcf2bd33678 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_488a7dcf2bd33678_base

{-# NOINLINE args_int2_ptr #-}

{-| __C declaration:__ @args_int2@

    __defined at:__ @macros\/reparse.h:26:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int2_ptr :: Ptr.FunPtr (A -> FC.CInt -> IO ())
args_int2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_488a7dcf2bd33678

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_5c6e85e96378ce0f" hs_bindgen_5c6e85e96378ce0f_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_int3_ptr@
hs_bindgen_5c6e85e96378ce0f ::
     IO (Ptr.FunPtr (A -> FC.CUInt -> IO ()))
hs_bindgen_5c6e85e96378ce0f =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_5c6e85e96378ce0f_base

{-# NOINLINE args_int3_ptr #-}

{-| __C declaration:__ @args_int3@

    __defined at:__ @macros\/reparse.h:27:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int3_ptr :: Ptr.FunPtr (A -> FC.CUInt -> IO ())
args_int3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5c6e85e96378ce0f

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_afcad03e61d3f83b" hs_bindgen_afcad03e61d3f83b_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_long1_ptr@
hs_bindgen_afcad03e61d3f83b ::
     IO (Ptr.FunPtr (A -> FC.CLong -> IO ()))
hs_bindgen_afcad03e61d3f83b =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_afcad03e61d3f83b_base

{-# NOINLINE args_long1_ptr #-}

{-| __C declaration:__ @args_long1@

    __defined at:__ @macros\/reparse.h:29:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long1_ptr :: Ptr.FunPtr (A -> FC.CLong -> IO ())
args_long1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_afcad03e61d3f83b

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_2d32bae595df94c2" hs_bindgen_2d32bae595df94c2_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_long2_ptr@
hs_bindgen_2d32bae595df94c2 ::
     IO (Ptr.FunPtr (A -> FC.CLong -> IO ()))
hs_bindgen_2d32bae595df94c2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_2d32bae595df94c2_base

{-# NOINLINE args_long2_ptr #-}

{-| __C declaration:__ @args_long2@

    __defined at:__ @macros\/reparse.h:30:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long2_ptr :: Ptr.FunPtr (A -> FC.CLong -> IO ())
args_long2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2d32bae595df94c2

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_0f7d316338eac027" hs_bindgen_0f7d316338eac027_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_long3_ptr@
hs_bindgen_0f7d316338eac027 ::
     IO (Ptr.FunPtr (A -> FC.CULong -> IO ()))
hs_bindgen_0f7d316338eac027 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_0f7d316338eac027_base

{-# NOINLINE args_long3_ptr #-}

{-| __C declaration:__ @args_long3@

    __defined at:__ @macros\/reparse.h:31:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long3_ptr :: Ptr.FunPtr (A -> FC.CULong -> IO ())
args_long3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0f7d316338eac027

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_9ed3dd630c6a5c91" hs_bindgen_9ed3dd630c6a5c91_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_float_ptr@
hs_bindgen_9ed3dd630c6a5c91 ::
     IO (Ptr.FunPtr (A -> FC.CFloat -> IO ()))
hs_bindgen_9ed3dd630c6a5c91 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_9ed3dd630c6a5c91_base

{-# NOINLINE args_float_ptr #-}

{-| __C declaration:__ @args_float@

    __defined at:__ @macros\/reparse.h:33:6@

    __exported by:__ @macros\/reparse.h@
-}
args_float_ptr :: Ptr.FunPtr (A -> FC.CFloat -> IO ())
args_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9ed3dd630c6a5c91

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_c1afad204f639896" hs_bindgen_c1afad204f639896_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_double_ptr@
hs_bindgen_c1afad204f639896 ::
     IO (Ptr.FunPtr (A -> FC.CDouble -> IO ()))
hs_bindgen_c1afad204f639896 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_c1afad204f639896_base

{-# NOINLINE args_double_ptr #-}

{-| __C declaration:__ @args_double@

    __defined at:__ @macros\/reparse.h:34:6@

    __exported by:__ @macros\/reparse.h@
-}
args_double_ptr :: Ptr.FunPtr (A -> FC.CDouble -> IO ())
args_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c1afad204f639896

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_c7091d8aa6313541" hs_bindgen_c7091d8aa6313541_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_bool1_ptr@
hs_bindgen_c7091d8aa6313541 ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))
hs_bindgen_c7091d8aa6313541 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_c7091d8aa6313541_base

{-# NOINLINE args_bool1_ptr #-}

{-| __C declaration:__ @args_bool1@

    __defined at:__ @macros\/reparse.h:35:6@

    __exported by:__ @macros\/reparse.h@
-}
args_bool1_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
args_bool1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c7091d8aa6313541

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_c14722de6f25d3c0" hs_bindgen_c14722de6f25d3c0_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_struct_ptr@
hs_bindgen_c14722de6f25d3c0 ::
     IO (Ptr.FunPtr (A -> Some_struct -> IO ()))
hs_bindgen_c14722de6f25d3c0 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_c14722de6f25d3c0_base

{-# NOINLINE args_struct_ptr #-}

{-| __C declaration:__ @args_struct@

    __defined at:__ @macros\/reparse.h:37:6@

    __exported by:__ @macros\/reparse.h@
-}
args_struct_ptr :: Ptr.FunPtr (A -> Some_struct -> IO ())
args_struct_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c14722de6f25d3c0

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a94ca07a5083d898" hs_bindgen_a94ca07a5083d898_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_union_ptr@
hs_bindgen_a94ca07a5083d898 ::
     IO (Ptr.FunPtr (A -> Some_union -> IO ()))
hs_bindgen_a94ca07a5083d898 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a94ca07a5083d898_base

{-# NOINLINE args_union_ptr #-}

{-| __C declaration:__ @args_union@

    __defined at:__ @macros\/reparse.h:38:6@

    __exported by:__ @macros\/reparse.h@
-}
args_union_ptr :: Ptr.FunPtr (A -> Some_union -> IO ())
args_union_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a94ca07a5083d898

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_2fdbcc2976b884f7" hs_bindgen_2fdbcc2976b884f7_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_enum_ptr@
hs_bindgen_2fdbcc2976b884f7 ::
     IO (Ptr.FunPtr (A -> Some_enum -> IO ()))
hs_bindgen_2fdbcc2976b884f7 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_2fdbcc2976b884f7_base

{-# NOINLINE args_enum_ptr #-}

{-| __C declaration:__ @args_enum@

    __defined at:__ @macros\/reparse.h:39:6@

    __exported by:__ @macros\/reparse.h@
-}
args_enum_ptr :: Ptr.FunPtr (A -> Some_enum -> IO ())
args_enum_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2fdbcc2976b884f7

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_65b8da715d77e581" hs_bindgen_65b8da715d77e581_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_pointer1_ptr@
hs_bindgen_65b8da715d77e581 ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))
hs_bindgen_65b8da715d77e581 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_65b8da715d77e581_base

{-# NOINLINE args_pointer1_ptr #-}

{-| __C declaration:__ @args_pointer1@

    __defined at:__ @macros\/reparse.h:41:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer1_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
args_pointer1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_65b8da715d77e581

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_767fe5b679ba43e4" hs_bindgen_767fe5b679ba43e4_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_pointer2_ptr@
hs_bindgen_767fe5b679ba43e4 ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr (Ptr.Ptr FC.CInt)) -> IO ()))
hs_bindgen_767fe5b679ba43e4 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_767fe5b679ba43e4_base

{-# NOINLINE args_pointer2_ptr #-}

{-| __C declaration:__ @args_pointer2@

    __defined at:__ @macros\/reparse.h:42:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer2_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr (Ptr.Ptr FC.CInt)) -> IO ())
args_pointer2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_767fe5b679ba43e4

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_852cc5784297324b" hs_bindgen_852cc5784297324b_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_pointer3_ptr@
hs_bindgen_852cc5784297324b ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr Void) -> IO ()))
hs_bindgen_852cc5784297324b =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_852cc5784297324b_base

{-# NOINLINE args_pointer3_ptr #-}

{-| __C declaration:__ @args_pointer3@

    __defined at:__ @macros\/reparse.h:43:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer3_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr Void) -> IO ())
args_pointer3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_852cc5784297324b

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_89cbc210fb67bc53" hs_bindgen_89cbc210fb67bc53_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_A_ptr@
hs_bindgen_89cbc210fb67bc53 ::
     IO (Ptr.FunPtr (IO A))
hs_bindgen_89cbc210fb67bc53 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_89cbc210fb67bc53_base

{-# NOINLINE ret_A_ptr #-}

{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h:47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A_ptr :: Ptr.FunPtr (IO A)
ret_A_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_89cbc210fb67bc53

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_d95a16b3f46326f5" hs_bindgen_d95a16b3f46326f5_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_char1_ptr@
hs_bindgen_d95a16b3f46326f5 ::
     IO (Ptr.FunPtr (A -> IO FC.CChar))
hs_bindgen_d95a16b3f46326f5 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_d95a16b3f46326f5_base

{-# NOINLINE ret_char1_ptr #-}

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h:49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1_ptr :: Ptr.FunPtr (A -> IO FC.CChar)
ret_char1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d95a16b3f46326f5

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_dbb14b4445c045dc" hs_bindgen_dbb14b4445c045dc_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_char2_ptr@
hs_bindgen_dbb14b4445c045dc ::
     IO (Ptr.FunPtr (A -> IO FC.CSChar))
hs_bindgen_dbb14b4445c045dc =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_dbb14b4445c045dc_base

{-# NOINLINE ret_char2_ptr #-}

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h:50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2_ptr :: Ptr.FunPtr (A -> IO FC.CSChar)
ret_char2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dbb14b4445c045dc

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_18d70300449e2a05" hs_bindgen_18d70300449e2a05_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_char3_ptr@
hs_bindgen_18d70300449e2a05 ::
     IO (Ptr.FunPtr (A -> IO FC.CUChar))
hs_bindgen_18d70300449e2a05 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_18d70300449e2a05_base

{-# NOINLINE ret_char3_ptr #-}

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h:51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3_ptr :: Ptr.FunPtr (A -> IO FC.CUChar)
ret_char3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_18d70300449e2a05

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_7f113070dda67da8" hs_bindgen_7f113070dda67da8_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_short1_ptr@
hs_bindgen_7f113070dda67da8 ::
     IO (Ptr.FunPtr (A -> IO FC.CShort))
hs_bindgen_7f113070dda67da8 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_7f113070dda67da8_base

{-# NOINLINE ret_short1_ptr #-}

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h:53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1_ptr :: Ptr.FunPtr (A -> IO FC.CShort)
ret_short1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7f113070dda67da8

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_601d9c0a30f1855b" hs_bindgen_601d9c0a30f1855b_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_short2_ptr@
hs_bindgen_601d9c0a30f1855b ::
     IO (Ptr.FunPtr (A -> IO FC.CShort))
hs_bindgen_601d9c0a30f1855b =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_601d9c0a30f1855b_base

{-# NOINLINE ret_short2_ptr #-}

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h:54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2_ptr :: Ptr.FunPtr (A -> IO FC.CShort)
ret_short2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_601d9c0a30f1855b

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_eb1f70424e0c701d" hs_bindgen_eb1f70424e0c701d_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_short3_ptr@
hs_bindgen_eb1f70424e0c701d ::
     IO (Ptr.FunPtr (A -> IO FC.CUShort))
hs_bindgen_eb1f70424e0c701d =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_eb1f70424e0c701d_base

{-# NOINLINE ret_short3_ptr #-}

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h:55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3_ptr :: Ptr.FunPtr (A -> IO FC.CUShort)
ret_short3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_eb1f70424e0c701d

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_28a93ce9f2a99cd0" hs_bindgen_28a93ce9f2a99cd0_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_int1_ptr@
hs_bindgen_28a93ce9f2a99cd0 ::
     IO (Ptr.FunPtr (A -> IO FC.CInt))
hs_bindgen_28a93ce9f2a99cd0 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_28a93ce9f2a99cd0_base

{-# NOINLINE ret_int1_ptr #-}

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h:57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1_ptr :: Ptr.FunPtr (A -> IO FC.CInt)
ret_int1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_28a93ce9f2a99cd0

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a70295d21f766087" hs_bindgen_a70295d21f766087_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_int2_ptr@
hs_bindgen_a70295d21f766087 ::
     IO (Ptr.FunPtr (A -> IO FC.CInt))
hs_bindgen_a70295d21f766087 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a70295d21f766087_base

{-# NOINLINE ret_int2_ptr #-}

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h:58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2_ptr :: Ptr.FunPtr (A -> IO FC.CInt)
ret_int2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a70295d21f766087

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_4239c3dd15ab11f3" hs_bindgen_4239c3dd15ab11f3_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_int3_ptr@
hs_bindgen_4239c3dd15ab11f3 ::
     IO (Ptr.FunPtr (A -> IO FC.CUInt))
hs_bindgen_4239c3dd15ab11f3 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4239c3dd15ab11f3_base

{-# NOINLINE ret_int3_ptr #-}

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h:59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3_ptr :: Ptr.FunPtr (A -> IO FC.CUInt)
ret_int3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4239c3dd15ab11f3

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_b24935761b06cfd8" hs_bindgen_b24935761b06cfd8_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_long1_ptr@
hs_bindgen_b24935761b06cfd8 ::
     IO (Ptr.FunPtr (A -> IO FC.CLong))
hs_bindgen_b24935761b06cfd8 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_b24935761b06cfd8_base

{-# NOINLINE ret_long1_ptr #-}

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h:61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1_ptr :: Ptr.FunPtr (A -> IO FC.CLong)
ret_long1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b24935761b06cfd8

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_35a17cc5266d3326" hs_bindgen_35a17cc5266d3326_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_long2_ptr@
hs_bindgen_35a17cc5266d3326 ::
     IO (Ptr.FunPtr (A -> IO FC.CLong))
hs_bindgen_35a17cc5266d3326 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_35a17cc5266d3326_base

{-# NOINLINE ret_long2_ptr #-}

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h:62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2_ptr :: Ptr.FunPtr (A -> IO FC.CLong)
ret_long2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_35a17cc5266d3326

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_59489620015c271e" hs_bindgen_59489620015c271e_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_long3_ptr@
hs_bindgen_59489620015c271e ::
     IO (Ptr.FunPtr (A -> IO FC.CULong))
hs_bindgen_59489620015c271e =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_59489620015c271e_base

{-# NOINLINE ret_long3_ptr #-}

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h:63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3_ptr :: Ptr.FunPtr (A -> IO FC.CULong)
ret_long3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_59489620015c271e

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_52138c45b539427d" hs_bindgen_52138c45b539427d_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_float_ptr@
hs_bindgen_52138c45b539427d ::
     IO (Ptr.FunPtr (A -> IO FC.CFloat))
hs_bindgen_52138c45b539427d =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_52138c45b539427d_base

{-# NOINLINE ret_float_ptr #-}

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h:65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float_ptr :: Ptr.FunPtr (A -> IO FC.CFloat)
ret_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_52138c45b539427d

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_283d5d098a9c4a59" hs_bindgen_283d5d098a9c4a59_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_double_ptr@
hs_bindgen_283d5d098a9c4a59 ::
     IO (Ptr.FunPtr (A -> IO FC.CDouble))
hs_bindgen_283d5d098a9c4a59 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_283d5d098a9c4a59_base

{-# NOINLINE ret_double_ptr #-}

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h:66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double_ptr :: Ptr.FunPtr (A -> IO FC.CDouble)
ret_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_283d5d098a9c4a59

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_382098412cbd94ff" hs_bindgen_382098412cbd94ff_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_bool1_ptr@
hs_bindgen_382098412cbd94ff ::
     IO (Ptr.FunPtr (A -> IO FC.CBool))
hs_bindgen_382098412cbd94ff =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_382098412cbd94ff_base

{-# NOINLINE ret_bool1_ptr #-}

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h:67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1_ptr :: Ptr.FunPtr (A -> IO FC.CBool)
ret_bool1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_382098412cbd94ff

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_51cf9857b3cc1843" hs_bindgen_51cf9857b3cc1843_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_struct_ptr@
hs_bindgen_51cf9857b3cc1843 ::
     IO (Ptr.FunPtr (A -> IO Some_struct))
hs_bindgen_51cf9857b3cc1843 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_51cf9857b3cc1843_base

{-# NOINLINE ret_struct_ptr #-}

{-| __C declaration:__ @ret_struct@

    __defined at:__ @macros\/reparse.h:69:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_struct_ptr :: Ptr.FunPtr (A -> IO Some_struct)
ret_struct_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_51cf9857b3cc1843

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_3df1073dbf5d79f4" hs_bindgen_3df1073dbf5d79f4_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_union_ptr@
hs_bindgen_3df1073dbf5d79f4 ::
     IO (Ptr.FunPtr (A -> IO Some_union))
hs_bindgen_3df1073dbf5d79f4 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_3df1073dbf5d79f4_base

{-# NOINLINE ret_union_ptr #-}

{-| __C declaration:__ @ret_union@

    __defined at:__ @macros\/reparse.h:70:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_union_ptr :: Ptr.FunPtr (A -> IO Some_union)
ret_union_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3df1073dbf5d79f4

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_c0467f7279732ddd" hs_bindgen_c0467f7279732ddd_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_enum_ptr@
hs_bindgen_c0467f7279732ddd ::
     IO (Ptr.FunPtr (A -> IO Some_enum))
hs_bindgen_c0467f7279732ddd =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_c0467f7279732ddd_base

{-# NOINLINE ret_enum_ptr #-}

{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h:71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum_ptr :: Ptr.FunPtr (A -> IO Some_enum)
ret_enum_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c0467f7279732ddd

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f0240baaa70df9bd" hs_bindgen_f0240baaa70df9bd_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_pointer1_ptr@
hs_bindgen_f0240baaa70df9bd ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))
hs_bindgen_f0240baaa70df9bd =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_f0240baaa70df9bd_base

{-# NOINLINE ret_pointer1_ptr #-}

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h:73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
ret_pointer1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f0240baaa70df9bd

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_019cbfb4d24d1d91" hs_bindgen_019cbfb4d24d1d91_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_pointer2_ptr@
hs_bindgen_019cbfb4d24d1d91 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt))))
hs_bindgen_019cbfb4d24d1d91 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_019cbfb4d24d1d91_base

{-# NOINLINE ret_pointer2_ptr #-}

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h:74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt)))
ret_pointer2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_019cbfb4d24d1d91

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a654e9f8ca0d53c5" hs_bindgen_a654e9f8ca0d53c5_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_pointer3_ptr@
hs_bindgen_a654e9f8ca0d53c5 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr Void)))
hs_bindgen_a654e9f8ca0d53c5 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a654e9f8ca0d53c5_base

{-# NOINLINE ret_pointer3_ptr #-}

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h:75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr Void))
ret_pointer3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a654e9f8ca0d53c5

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_94eff7815581584b" hs_bindgen_94eff7815581584b_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_body1_ptr@
hs_bindgen_94eff7815581584b ::
     IO (Ptr.FunPtr (A -> IO FC.CInt))
hs_bindgen_94eff7815581584b =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_94eff7815581584b_base

{-# NOINLINE body1_ptr #-}

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h:79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1_ptr :: Ptr.FunPtr (A -> IO FC.CInt)
body1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_94eff7815581584b

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f98b0963b05f261c" hs_bindgen_f98b0963b05f261c_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_body2_ptr@
hs_bindgen_f98b0963b05f261c ::
     IO (Ptr.FunPtr (IO A))
hs_bindgen_f98b0963b05f261c =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_f98b0963b05f261c_base

{-# NOINLINE body2_ptr #-}

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h:80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2_ptr :: Ptr.FunPtr (IO A)
body2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f98b0963b05f261c

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_e2f3985767c79559" hs_bindgen_e2f3985767c79559_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_complex_float_ptr@
hs_bindgen_e2f3985767c79559 ::
     IO (Ptr.FunPtr (A -> (Data.Complex.Complex FC.CFloat) -> IO ()))
hs_bindgen_e2f3985767c79559 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_e2f3985767c79559_base

{-# NOINLINE args_complex_float_ptr #-}

{-| __C declaration:__ @args_complex_float@

    __defined at:__ @macros\/reparse.h:84:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_float_ptr :: Ptr.FunPtr (A -> (Data.Complex.Complex FC.CFloat) -> IO ())
args_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e2f3985767c79559

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_1da23b0894c2e548" hs_bindgen_1da23b0894c2e548_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_args_complex_double_ptr@
hs_bindgen_1da23b0894c2e548 ::
     IO (Ptr.FunPtr (A -> (Data.Complex.Complex FC.CDouble) -> IO ()))
hs_bindgen_1da23b0894c2e548 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_1da23b0894c2e548_base

{-# NOINLINE args_complex_double_ptr #-}

{-| __C declaration:__ @args_complex_double@

    __defined at:__ @macros\/reparse.h:85:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_double_ptr :: Ptr.FunPtr (A -> (Data.Complex.Complex FC.CDouble) -> IO ())
args_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1da23b0894c2e548

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_e3d89b51410d7614" hs_bindgen_e3d89b51410d7614_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_complex_float_ptr@
hs_bindgen_e3d89b51410d7614 ::
     IO (Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CFloat)))
hs_bindgen_e3d89b51410d7614 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_e3d89b51410d7614_base

{-# NOINLINE ret_complex_float_ptr #-}

{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @macros\/reparse.h:86:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_float_ptr :: Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CFloat))
ret_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e3d89b51410d7614

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_7cc277a18abf87b8" hs_bindgen_7cc277a18abf87b8_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_ret_complex_double_ptr@
hs_bindgen_7cc277a18abf87b8 ::
     IO (Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CDouble)))
hs_bindgen_7cc277a18abf87b8 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_7cc277a18abf87b8_base

{-# NOINLINE ret_complex_double_ptr #-}

{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @macros\/reparse.h:87:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_double_ptr :: Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CDouble))
ret_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7cc277a18abf87b8

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_764ddaf3efe7bd53" hs_bindgen_764ddaf3efe7bd53_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_bespoke_args1_ptr@
hs_bindgen_764ddaf3efe7bd53 ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))
hs_bindgen_764ddaf3efe7bd53 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_764ddaf3efe7bd53_base

{-# NOINLINE bespoke_args1_ptr #-}

{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @macros\/reparse.h:94:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args1_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
bespoke_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_764ddaf3efe7bd53

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_53ccc1b308cd8384" hs_bindgen_53ccc1b308cd8384_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_bespoke_args2_ptr@
hs_bindgen_53ccc1b308cd8384 ::
     IO (Ptr.FunPtr (A -> HsBindgen.Runtime.Prelude.CSize -> IO ()))
hs_bindgen_53ccc1b308cd8384 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_53ccc1b308cd8384_base

{-# NOINLINE bespoke_args2_ptr #-}

{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @macros\/reparse.h:95:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args2_ptr :: Ptr.FunPtr (A -> HsBindgen.Runtime.Prelude.CSize -> IO ())
bespoke_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_53ccc1b308cd8384

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_e61f250910ddc098" hs_bindgen_e61f250910ddc098_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_bespoke_ret1_ptr@
hs_bindgen_e61f250910ddc098 ::
     IO (Ptr.FunPtr (A -> IO FC.CBool))
hs_bindgen_e61f250910ddc098 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_e61f250910ddc098_base

{-# NOINLINE bespoke_ret1_ptr #-}

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h:97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1_ptr :: Ptr.FunPtr (A -> IO FC.CBool)
bespoke_ret1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e61f250910ddc098

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_b9864dba6e30c078" hs_bindgen_b9864dba6e30c078_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_bespoke_ret2_ptr@
hs_bindgen_b9864dba6e30c078 ::
     IO (Ptr.FunPtr (A -> IO HsBindgen.Runtime.Prelude.CSize))
hs_bindgen_b9864dba6e30c078 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_b9864dba6e30c078_base

{-# NOINLINE bespoke_ret2_ptr #-}

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h:98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2_ptr :: Ptr.FunPtr (A -> IO HsBindgen.Runtime.Prelude.CSize)
bespoke_ret2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b9864dba6e30c078

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_bb7f94a203c14e76" hs_bindgen_bb7f94a203c14e76_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_arr_args1_ptr@
hs_bindgen_bb7f94a203c14e76 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ()))
hs_bindgen_bb7f94a203c14e76 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_bb7f94a203c14e76_base

{-# NOINLINE arr_args1_ptr #-}

{-| Arrays

__C declaration:__ @arr_args1@

__defined at:__ @macros\/reparse.h:104:6@

__exported by:__ @macros\/reparse.h@
-}
arr_args1_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ())
arr_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bb7f94a203c14e76

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_ca2476976e4721ef" hs_bindgen_ca2476976e4721ef_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_arr_args2_ptr@
hs_bindgen_ca2476976e4721ef ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ()))
hs_bindgen_ca2476976e4721ef =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_ca2476976e4721ef_base

{-# NOINLINE arr_args2_ptr #-}

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h:105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ())
arr_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ca2476976e4721ef

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_3298ac669c00b1cd" hs_bindgen_3298ac669c00b1cd_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_arr_args3_ptr@
hs_bindgen_3298ac669c00b1cd ::
     IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) A) -> IO ()))
hs_bindgen_3298ac669c00b1cd =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_3298ac669c00b1cd_base

{-# NOINLINE arr_args3_ptr #-}

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h:106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) A) -> IO ())
arr_args3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3298ac669c00b1cd

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_329a5d4b44b11e6e" hs_bindgen_329a5d4b44b11e6e_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_arr_args4_ptr@
hs_bindgen_329a5d4b44b11e6e ::
     IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) (Ptr.Ptr A)) -> IO ()))
hs_bindgen_329a5d4b44b11e6e =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_329a5d4b44b11e6e_base

{-# NOINLINE arr_args4_ptr #-}

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h:107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) (Ptr.Ptr A)) -> IO ())
arr_args4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_329a5d4b44b11e6e

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_dac9e3bdccb6a4eb" hs_bindgen_dac9e3bdccb6a4eb_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_funptr_args1_ptr@
hs_bindgen_dac9e3bdccb6a4eb ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (IO ())) -> IO ()))
hs_bindgen_dac9e3bdccb6a4eb =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_dac9e3bdccb6a4eb_base

{-# NOINLINE funptr_args1_ptr #-}

{-| Function pointers

__C declaration:__ @funptr_args1@

__defined at:__ @macros\/reparse.h:126:6@

__exported by:__ @macros\/reparse.h@
-}
funptr_args1_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (IO ())) -> IO ())
funptr_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dac9e3bdccb6a4eb

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_83d7f85727e54da4" hs_bindgen_83d7f85727e54da4_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_funptr_args2_ptr@
hs_bindgen_83d7f85727e54da4 ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ()))
hs_bindgen_83d7f85727e54da4 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_83d7f85727e54da4_base

{-# NOINLINE funptr_args2_ptr #-}

{-| __C declaration:__ @funptr_args2@

    __defined at:__ @macros\/reparse.h:127:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args2_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ())
funptr_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_83d7f85727e54da4

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_107e06f31f9dd017" hs_bindgen_107e06f31f9dd017_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_funptr_args3_ptr@
hs_bindgen_107e06f31f9dd017 ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> IO ())) -> IO ()))
hs_bindgen_107e06f31f9dd017 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_107e06f31f9dd017_base

{-# NOINLINE funptr_args3_ptr #-}

{-| __C declaration:__ @funptr_args3@

    __defined at:__ @macros\/reparse.h:128:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args3_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> IO ())) -> IO ())
funptr_args3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_107e06f31f9dd017

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_51a7f0cfbd57eaf7" hs_bindgen_51a7f0cfbd57eaf7_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_funptr_args4_ptr@
hs_bindgen_51a7f0cfbd57eaf7 ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)) -> IO ()))
hs_bindgen_51a7f0cfbd57eaf7 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_51a7f0cfbd57eaf7_base

{-# NOINLINE funptr_args4_ptr #-}

{-| __C declaration:__ @funptr_args4@

    __defined at:__ @macros\/reparse.h:129:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args4_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)) -> IO ())
funptr_args4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_51a7f0cfbd57eaf7

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_4a86c3a3b98a00d9" hs_bindgen_4a86c3a3b98a00d9_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_funptr_args5_ptr@
hs_bindgen_4a86c3a3b98a00d9 ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))) -> IO ()))
hs_bindgen_4a86c3a3b98a00d9 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4a86c3a3b98a00d9_base

{-# NOINLINE funptr_args5_ptr #-}

{-| __C declaration:__ @funptr_args5@

    __defined at:__ @macros\/reparse.h:130:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args5_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))) -> IO ())
funptr_args5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4a86c3a3b98a00d9

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_1b13b480c009cf44" hs_bindgen_1b13b480c009cf44_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_comments1_ptr@
hs_bindgen_1b13b480c009cf44 ::
     IO (Ptr.FunPtr (A -> IO ()))
hs_bindgen_1b13b480c009cf44 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_1b13b480c009cf44_base

{-# NOINLINE comments1_ptr #-}

{-| Comments in awkward places

  (Prior to language-c we failed to parse there.)

__C declaration:__ @comments1@

__defined at:__ @macros\/reparse.h:144:25@

__exported by:__ @macros\/reparse.h@
-}
comments1_ptr :: Ptr.FunPtr (A -> IO ())
comments1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1b13b480c009cf44

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_410cb526b4cee637" hs_bindgen_410cb526b4cee637_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_prim_before1_ptr@
hs_bindgen_410cb526b4cee637 ::
     IO (Ptr.FunPtr (A -> FC.CChar -> IO ()))
hs_bindgen_410cb526b4cee637 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_410cb526b4cee637_base

{-# NOINLINE const_prim_before1_ptr #-}

{-| `const` qualifier

  NOTE: These were not parsed correctly prior to the switch to language-c.

__C declaration:__ @const_prim_before1@

__defined at:__ @macros\/reparse.h:179:6@

__exported by:__ @macros\/reparse.h@
-}
const_prim_before1_ptr :: Ptr.FunPtr (A -> FC.CChar -> IO ())
const_prim_before1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_410cb526b4cee637

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_3b1e7a350d422127" hs_bindgen_3b1e7a350d422127_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_prim_before2_ptr@
hs_bindgen_3b1e7a350d422127 ::
     IO (Ptr.FunPtr (A -> FC.CSChar -> IO ()))
hs_bindgen_3b1e7a350d422127 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_3b1e7a350d422127_base

{-# NOINLINE const_prim_before2_ptr #-}

{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @macros\/reparse.h:180:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before2_ptr :: Ptr.FunPtr (A -> FC.CSChar -> IO ())
const_prim_before2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3b1e7a350d422127

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_8aab98c0f956e496" hs_bindgen_8aab98c0f956e496_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_prim_before3_ptr@
hs_bindgen_8aab98c0f956e496 ::
     IO (Ptr.FunPtr (A -> FC.CUChar -> IO ()))
hs_bindgen_8aab98c0f956e496 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_8aab98c0f956e496_base

{-# NOINLINE const_prim_before3_ptr #-}

{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @macros\/reparse.h:181:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before3_ptr :: Ptr.FunPtr (A -> FC.CUChar -> IO ())
const_prim_before3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8aab98c0f956e496

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_aeee0dd2b067cf07" hs_bindgen_aeee0dd2b067cf07_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_prim_after1_ptr@
hs_bindgen_aeee0dd2b067cf07 ::
     IO (Ptr.FunPtr (A -> FC.CChar -> IO ()))
hs_bindgen_aeee0dd2b067cf07 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_aeee0dd2b067cf07_base

{-# NOINLINE const_prim_after1_ptr #-}

{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @macros\/reparse.h:182:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after1_ptr :: Ptr.FunPtr (A -> FC.CChar -> IO ())
const_prim_after1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_aeee0dd2b067cf07

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_ca5ab7dc437ce5d1" hs_bindgen_ca5ab7dc437ce5d1_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_prim_after2_ptr@
hs_bindgen_ca5ab7dc437ce5d1 ::
     IO (Ptr.FunPtr (A -> FC.CSChar -> IO ()))
hs_bindgen_ca5ab7dc437ce5d1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_ca5ab7dc437ce5d1_base

{-# NOINLINE const_prim_after2_ptr #-}

{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @macros\/reparse.h:183:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after2_ptr :: Ptr.FunPtr (A -> FC.CSChar -> IO ())
const_prim_after2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ca5ab7dc437ce5d1

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a6ae03f6051fcb2a" hs_bindgen_a6ae03f6051fcb2a_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_prim_after3_ptr@
hs_bindgen_a6ae03f6051fcb2a ::
     IO (Ptr.FunPtr (A -> FC.CUChar -> IO ()))
hs_bindgen_a6ae03f6051fcb2a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a6ae03f6051fcb2a_base

{-# NOINLINE const_prim_after3_ptr #-}

{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @macros\/reparse.h:184:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after3_ptr :: Ptr.FunPtr (A -> FC.CUChar -> IO ())
const_prim_after3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a6ae03f6051fcb2a

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_aea82678489f8007" hs_bindgen_aea82678489f8007_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before1_ptr@
hs_bindgen_aea82678489f8007 ::
     IO (Ptr.FunPtr (A -> FC.CFloat -> IO ()))
hs_bindgen_aea82678489f8007 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_aea82678489f8007_base

{-# NOINLINE const_withoutSign_before1_ptr #-}

{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @macros\/reparse.h:188:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before1_ptr :: Ptr.FunPtr (A -> FC.CFloat -> IO ())
const_withoutSign_before1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_aea82678489f8007

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_79b5f5987a75db98" hs_bindgen_79b5f5987a75db98_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before2_ptr@
hs_bindgen_79b5f5987a75db98 ::
     IO (Ptr.FunPtr (A -> FC.CDouble -> IO ()))
hs_bindgen_79b5f5987a75db98 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_79b5f5987a75db98_base

{-# NOINLINE const_withoutSign_before2_ptr #-}

{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @macros\/reparse.h:189:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before2_ptr :: Ptr.FunPtr (A -> FC.CDouble -> IO ())
const_withoutSign_before2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_79b5f5987a75db98

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_fafbaf6c727e6e6d" hs_bindgen_fafbaf6c727e6e6d_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before3_ptr@
hs_bindgen_fafbaf6c727e6e6d ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))
hs_bindgen_fafbaf6c727e6e6d =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_fafbaf6c727e6e6d_base

{-# NOINLINE const_withoutSign_before3_ptr #-}

{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @macros\/reparse.h:190:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before3_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_before3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fafbaf6c727e6e6d

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_c9342430ac667d8a" hs_bindgen_c9342430ac667d8a_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before4_ptr@
hs_bindgen_c9342430ac667d8a ::
     IO (Ptr.FunPtr (A -> Some_struct -> IO ()))
hs_bindgen_c9342430ac667d8a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_c9342430ac667d8a_base

{-# NOINLINE const_withoutSign_before4_ptr #-}

{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @macros\/reparse.h:191:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before4_ptr :: Ptr.FunPtr (A -> Some_struct -> IO ())
const_withoutSign_before4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c9342430ac667d8a

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_25501097b98452bd" hs_bindgen_25501097b98452bd_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before5_ptr@
hs_bindgen_25501097b98452bd ::
     IO (Ptr.FunPtr (A -> Some_union -> IO ()))
hs_bindgen_25501097b98452bd =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_25501097b98452bd_base

{-# NOINLINE const_withoutSign_before5_ptr #-}

{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @macros\/reparse.h:192:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before5_ptr :: Ptr.FunPtr (A -> Some_union -> IO ())
const_withoutSign_before5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_25501097b98452bd

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_4b356af92ea4b405" hs_bindgen_4b356af92ea4b405_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before6_ptr@
hs_bindgen_4b356af92ea4b405 ::
     IO (Ptr.FunPtr (A -> Some_enum -> IO ()))
hs_bindgen_4b356af92ea4b405 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4b356af92ea4b405_base

{-# NOINLINE const_withoutSign_before6_ptr #-}

{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @macros\/reparse.h:193:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before6_ptr :: Ptr.FunPtr (A -> Some_enum -> IO ())
const_withoutSign_before6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4b356af92ea4b405

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_90b574ff639ebbd5" hs_bindgen_90b574ff639ebbd5_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before7_ptr@
hs_bindgen_90b574ff639ebbd5 ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))
hs_bindgen_90b574ff639ebbd5 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_90b574ff639ebbd5_base

{-# NOINLINE const_withoutSign_before7_ptr #-}

{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @macros\/reparse.h:194:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before7_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_before7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_90b574ff639ebbd5

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_cbb78eb3b806c344" hs_bindgen_cbb78eb3b806c344_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before8_ptr@
hs_bindgen_cbb78eb3b806c344 ::
     IO (Ptr.FunPtr (A -> HsBindgen.Runtime.Prelude.CSize -> IO ()))
hs_bindgen_cbb78eb3b806c344 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_cbb78eb3b806c344_base

{-# NOINLINE const_withoutSign_before8_ptr #-}

{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @macros\/reparse.h:195:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before8_ptr :: Ptr.FunPtr (A -> HsBindgen.Runtime.Prelude.CSize -> IO ())
const_withoutSign_before8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cbb78eb3b806c344

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f4083b3232462a5b" hs_bindgen_f4083b3232462a5b_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after1_ptr@
hs_bindgen_f4083b3232462a5b ::
     IO (Ptr.FunPtr (A -> FC.CFloat -> IO ()))
hs_bindgen_f4083b3232462a5b =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_f4083b3232462a5b_base

{-# NOINLINE const_withoutSign_after1_ptr #-}

{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @macros\/reparse.h:197:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after1_ptr :: Ptr.FunPtr (A -> FC.CFloat -> IO ())
const_withoutSign_after1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f4083b3232462a5b

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_cf16d660d9d916df" hs_bindgen_cf16d660d9d916df_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after2_ptr@
hs_bindgen_cf16d660d9d916df ::
     IO (Ptr.FunPtr (A -> FC.CDouble -> IO ()))
hs_bindgen_cf16d660d9d916df =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_cf16d660d9d916df_base

{-# NOINLINE const_withoutSign_after2_ptr #-}

{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @macros\/reparse.h:198:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after2_ptr :: Ptr.FunPtr (A -> FC.CDouble -> IO ())
const_withoutSign_after2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cf16d660d9d916df

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_41a40ed22011f536" hs_bindgen_41a40ed22011f536_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after3_ptr@
hs_bindgen_41a40ed22011f536 ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))
hs_bindgen_41a40ed22011f536 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_41a40ed22011f536_base

{-# NOINLINE const_withoutSign_after3_ptr #-}

{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @macros\/reparse.h:199:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after3_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_after3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_41a40ed22011f536

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_4bc0069f381d29c9" hs_bindgen_4bc0069f381d29c9_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after4_ptr@
hs_bindgen_4bc0069f381d29c9 ::
     IO (Ptr.FunPtr (A -> Some_struct -> IO ()))
hs_bindgen_4bc0069f381d29c9 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4bc0069f381d29c9_base

{-# NOINLINE const_withoutSign_after4_ptr #-}

{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @macros\/reparse.h:200:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after4_ptr :: Ptr.FunPtr (A -> Some_struct -> IO ())
const_withoutSign_after4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4bc0069f381d29c9

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_e064a509e456b021" hs_bindgen_e064a509e456b021_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after5_ptr@
hs_bindgen_e064a509e456b021 ::
     IO (Ptr.FunPtr (A -> Some_union -> IO ()))
hs_bindgen_e064a509e456b021 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_e064a509e456b021_base

{-# NOINLINE const_withoutSign_after5_ptr #-}

{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @macros\/reparse.h:201:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after5_ptr :: Ptr.FunPtr (A -> Some_union -> IO ())
const_withoutSign_after5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e064a509e456b021

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_b89597d47b21f2fd" hs_bindgen_b89597d47b21f2fd_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after6_ptr@
hs_bindgen_b89597d47b21f2fd ::
     IO (Ptr.FunPtr (A -> Some_enum -> IO ()))
hs_bindgen_b89597d47b21f2fd =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_b89597d47b21f2fd_base

{-# NOINLINE const_withoutSign_after6_ptr #-}

{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @macros\/reparse.h:202:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after6_ptr :: Ptr.FunPtr (A -> Some_enum -> IO ())
const_withoutSign_after6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b89597d47b21f2fd

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_127b2fb737af1d7a" hs_bindgen_127b2fb737af1d7a_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after7_ptr@
hs_bindgen_127b2fb737af1d7a ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))
hs_bindgen_127b2fb737af1d7a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_127b2fb737af1d7a_base

{-# NOINLINE const_withoutSign_after7_ptr #-}

{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @macros\/reparse.h:203:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after7_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_after7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_127b2fb737af1d7a

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_05c7bd4fa507a58c" hs_bindgen_05c7bd4fa507a58c_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after8_ptr@
hs_bindgen_05c7bd4fa507a58c ::
     IO (Ptr.FunPtr (A -> HsBindgen.Runtime.Prelude.CSize -> IO ()))
hs_bindgen_05c7bd4fa507a58c =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_05c7bd4fa507a58c_base

{-# NOINLINE const_withoutSign_after8_ptr #-}

{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @macros\/reparse.h:204:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after8_ptr :: Ptr.FunPtr (A -> HsBindgen.Runtime.Prelude.CSize -> IO ())
const_withoutSign_after8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_05c7bd4fa507a58c

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_dfa6f2ec505f391a" hs_bindgen_dfa6f2ec505f391a_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_pointers_args1_ptr@
hs_bindgen_dfa6f2ec505f391a ::
     IO (Ptr.FunPtr (A -> (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt) -> IO ()))
hs_bindgen_dfa6f2ec505f391a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_dfa6f2ec505f391a_base

{-# NOINLINE const_pointers_args1_ptr #-}

{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @macros\/reparse.h:208:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args1_ptr :: Ptr.FunPtr (A -> (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt) -> IO ())
const_pointers_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dfa6f2ec505f391a

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_2f758756849ca2b5" hs_bindgen_2f758756849ca2b5_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_pointers_args2_ptr@
hs_bindgen_2f758756849ca2b5 ::
     IO (Ptr.FunPtr (A -> (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt) -> IO ()))
hs_bindgen_2f758756849ca2b5 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_2f758756849ca2b5_base

{-# NOINLINE const_pointers_args2_ptr #-}

{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @macros\/reparse.h:209:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args2_ptr :: Ptr.FunPtr (A -> (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt) -> IO ())
const_pointers_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2f758756849ca2b5

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_1f9d3190b4433852" hs_bindgen_1f9d3190b4433852_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_pointers_args3_ptr@
hs_bindgen_1f9d3190b4433852 ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))
hs_bindgen_1f9d3190b4433852 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_1f9d3190b4433852_base

{-# NOINLINE const_pointers_args3_ptr #-}

{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @macros\/reparse.h:210:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args3_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1f9d3190b4433852

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_1579ab85f0fa217b" hs_bindgen_1579ab85f0fa217b_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_pointers_args4_ptr@
hs_bindgen_1579ab85f0fa217b ::
     IO (Ptr.FunPtr (A -> (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt) -> IO ()))
hs_bindgen_1579ab85f0fa217b =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_1579ab85f0fa217b_base

{-# NOINLINE const_pointers_args4_ptr #-}

{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @macros\/reparse.h:211:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args4_ptr :: Ptr.FunPtr (A -> (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt) -> IO ())
const_pointers_args4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1579ab85f0fa217b

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_b4770dc5310bc558" hs_bindgen_b4770dc5310bc558_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_pointers_args5_ptr@
hs_bindgen_b4770dc5310bc558 ::
     IO (Ptr.FunPtr (A -> (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt) -> IO ()))
hs_bindgen_b4770dc5310bc558 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_b4770dc5310bc558_base

{-# NOINLINE const_pointers_args5_ptr #-}

{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @macros\/reparse.h:212:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args5_ptr :: Ptr.FunPtr (A -> (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt) -> IO ())
const_pointers_args5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b4770dc5310bc558

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_8422fbf55ee37cbb" hs_bindgen_8422fbf55ee37cbb_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_pointers_ret1_ptr@
hs_bindgen_8422fbf55ee37cbb ::
     IO (Ptr.FunPtr (A -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))
hs_bindgen_8422fbf55ee37cbb =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_8422fbf55ee37cbb_base

{-# NOINLINE const_pointers_ret1_ptr #-}

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h:214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1_ptr :: Ptr.FunPtr (A -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))
const_pointers_ret1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8422fbf55ee37cbb

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_7d62d267cb012ebf" hs_bindgen_7d62d267cb012ebf_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_pointers_ret2_ptr@
hs_bindgen_7d62d267cb012ebf ::
     IO (Ptr.FunPtr (A -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))
hs_bindgen_7d62d267cb012ebf =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_7d62d267cb012ebf_base

{-# NOINLINE const_pointers_ret2_ptr #-}

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h:215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2_ptr :: Ptr.FunPtr (A -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))
const_pointers_ret2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7d62d267cb012ebf

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_d56e13b56b7e1cf7" hs_bindgen_d56e13b56b7e1cf7_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_pointers_ret3_ptr@
hs_bindgen_d56e13b56b7e1cf7 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))
hs_bindgen_d56e13b56b7e1cf7 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_d56e13b56b7e1cf7_base

{-# NOINLINE const_pointers_ret3_ptr #-}

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h:216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d56e13b56b7e1cf7

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_bedc6b38f49c61ea" hs_bindgen_bedc6b38f49c61ea_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_pointers_ret4_ptr@
hs_bindgen_bedc6b38f49c61ea ::
     IO (Ptr.FunPtr (A -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))
hs_bindgen_bedc6b38f49c61ea =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_bedc6b38f49c61ea_base

{-# NOINLINE const_pointers_ret4_ptr #-}

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h:217:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4_ptr :: Ptr.FunPtr (A -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))
const_pointers_ret4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bedc6b38f49c61ea

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_8d027f9f58006eb9" hs_bindgen_8d027f9f58006eb9_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_pointers_ret5_ptr@
hs_bindgen_8d027f9f58006eb9 ::
     IO (Ptr.FunPtr (A -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))
hs_bindgen_8d027f9f58006eb9 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_8d027f9f58006eb9_base

{-# NOINLINE const_pointers_ret5_ptr #-}

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h:218:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5_ptr :: Ptr.FunPtr (A -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))
const_pointers_ret5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8d027f9f58006eb9

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_882567df89856ac9" hs_bindgen_882567df89856ac9_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_array_elem1_ptr@
hs_bindgen_882567df89856ac9 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ()))
hs_bindgen_882567df89856ac9 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_882567df89856ac9_base

{-# NOINLINE const_array_elem1_ptr #-}

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h:246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ())
const_array_elem1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_882567df89856ac9

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_70e4ab7a50eb5360" hs_bindgen_70e4ab7a50eb5360_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_array_elem2_ptr@
hs_bindgen_70e4ab7a50eb5360 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (HsBindgen.Runtime.ConstPtr.ConstPtr A)) -> IO ()))
hs_bindgen_70e4ab7a50eb5360 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_70e4ab7a50eb5360_base

{-# NOINLINE const_array_elem2_ptr #-}

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h:247:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (HsBindgen.Runtime.ConstPtr.ConstPtr A)) -> IO ())
const_array_elem2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_70e4ab7a50eb5360

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_14a733fd770b7242" hs_bindgen_14a733fd770b7242_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_const_array_elem3_ptr@
hs_bindgen_14a733fd770b7242 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ()))
hs_bindgen_14a733fd770b7242 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_14a733fd770b7242_base

{-# NOINLINE const_array_elem3_ptr #-}

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h:248:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ())
const_array_elem3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_14a733fd770b7242

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_8e462fca4a002e73" hs_bindgen_8e462fca4a002e73_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_noParams1_ptr@
hs_bindgen_8e462fca4a002e73 ::
     IO (Ptr.FunPtr (IO A))
hs_bindgen_8e462fca4a002e73 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_8e462fca4a002e73_base

{-# NOINLINE noParams1_ptr #-}

{-| Other examples we reparsed /incorrectly/ before language-c

__C declaration:__ @noParams1@

__defined at:__ @macros\/reparse.h:256:3@

__exported by:__ @macros\/reparse.h@
-}
noParams1_ptr :: Ptr.FunPtr (IO A)
noParams1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8e462fca4a002e73

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_42667590c95d450e" hs_bindgen_42667590c95d450e_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_noParams2_ptr@
hs_bindgen_42667590c95d450e ::
     IO (Ptr.FunPtr (IO A))
hs_bindgen_42667590c95d450e =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_42667590c95d450e_base

{-# NOINLINE noParams2_ptr #-}

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h:257:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2_ptr :: Ptr.FunPtr (IO A)
noParams2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_42667590c95d450e

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_23777cd9313c8c63" hs_bindgen_23777cd9313c8c63_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_noParams3_ptr@
hs_bindgen_23777cd9313c8c63 ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ()))
hs_bindgen_23777cd9313c8c63 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_23777cd9313c8c63_base

{-# NOINLINE noParams3_ptr #-}

{-| __C declaration:__ @noParams3@

    __defined at:__ @macros\/reparse.h:258:6@

    __exported by:__ @macros\/reparse.h@
-}
noParams3_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ())
noParams3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_23777cd9313c8c63

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a8f974caf74669f9" hs_bindgen_a8f974caf74669f9_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_funptr_ret1_ptr@
hs_bindgen_a8f974caf74669f9 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO ()))))
hs_bindgen_a8f974caf74669f9 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a8f974caf74669f9_base

{-# NOINLINE funptr_ret1_ptr #-}

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h:262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO ())))
funptr_ret1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a8f974caf74669f9

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f13795ebabb26526" hs_bindgen_f13795ebabb26526_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_funptr_ret2_ptr@
hs_bindgen_f13795ebabb26526 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO FC.CInt))))
hs_bindgen_f13795ebabb26526 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_f13795ebabb26526_base

{-# NOINLINE funptr_ret2_ptr #-}

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h:263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO FC.CInt)))
funptr_ret2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f13795ebabb26526

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_2515837794143ac1" hs_bindgen_2515837794143ac1_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_funptr_ret3_ptr@
hs_bindgen_2515837794143ac1 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> IO ()))))
hs_bindgen_2515837794143ac1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_2515837794143ac1_base

{-# NOINLINE funptr_ret3_ptr #-}

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h:264:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> IO ())))
funptr_ret3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2515837794143ac1

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f01ceaf447c3de04" hs_bindgen_f01ceaf447c3de04_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_funptr_ret4_ptr@
hs_bindgen_f01ceaf447c3de04 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar))))
hs_bindgen_f01ceaf447c3de04 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_f01ceaf447c3de04_base

{-# NOINLINE funptr_ret4_ptr #-}

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h:265:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)))
funptr_ret4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f01ceaf447c3de04

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_3cb2c77a66e6f46f" hs_bindgen_3cb2c77a66e6f46f_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_funptr_ret5_ptr@
hs_bindgen_3cb2c77a66e6f46f ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))
hs_bindgen_3cb2c77a66e6f46f =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_3cb2c77a66e6f46f_base

{-# NOINLINE funptr_ret5_ptr #-}

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h:269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3cb2c77a66e6f46f

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_3a28c985fce638f9" hs_bindgen_3a28c985fce638f9_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_funptr_ret6_ptr@
hs_bindgen_3a28c985fce638f9 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))))
hs_bindgen_3a28c985fce638f9 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_3a28c985fce638f9_base

{-# NOINLINE funptr_ret6_ptr #-}

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h:270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))))
funptr_ret6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3a28c985fce638f9

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_e155fd240d710be2" hs_bindgen_e155fd240d710be2_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_funptr_ret7_ptr@
hs_bindgen_e155fd240d710be2 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))))
hs_bindgen_e155fd240d710be2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_e155fd240d710be2_base

{-# NOINLINE funptr_ret7_ptr #-}

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h:271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))))
funptr_ret7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e155fd240d710be2

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_61261c2147d69f98" hs_bindgen_61261c2147d69f98_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_funptr_ret8_ptr@
hs_bindgen_61261c2147d69f98 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))
hs_bindgen_61261c2147d69f98 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_61261c2147d69f98_base

{-# NOINLINE funptr_ret8_ptr #-}

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h:272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_61261c2147d69f98

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_e3c71dfaf82486c8" hs_bindgen_e3c71dfaf82486c8_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_funptr_ret9_ptr@
hs_bindgen_e3c71dfaf82486c8 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))))
hs_bindgen_e3c71dfaf82486c8 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_e3c71dfaf82486c8_base

{-# NOINLINE funptr_ret9_ptr #-}

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h:273:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))))
funptr_ret9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e3c71dfaf82486c8

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_6a47446b9176f0bf" hs_bindgen_6a47446b9176f0bf_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosreparse_Example_get_funptr_ret10_ptr@
hs_bindgen_6a47446b9176f0bf ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))))
hs_bindgen_6a47446b9176f0bf =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_6a47446b9176f0bf_base

{-# NOINLINE funptr_ret10_ptr #-}

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h:274:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))))
funptr_ret10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6a47446b9176f0bf
