{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/reparse.h>"
  , "/* test_macrosreparse_Example_get_args_char1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ba0c80bfdbc677bd (void)) ("
  , "  A arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &args_char1;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_char2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b1e8a2d5e3935f61 (void)) ("
  , "  A arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  return &args_char2;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_char3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_deb136b9b9d89650 (void)) ("
  , "  A arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  return &args_char3;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_short1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7a71d1e1867636bf (void)) ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short1;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_short2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ad7132b76947c638 (void)) ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short2;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_short3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f4842ac12be0c136 (void)) ("
  , "  A arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  return &args_short3;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_int1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3dc6ae7bb850c676 (void)) ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int1;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_int2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a76cb45502a6ea40 (void)) ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int2;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_int3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_99f48290fac28118 (void)) ("
  , "  A arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  return &args_int3;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_long1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_60a477b55893fa8e (void)) ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long1;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_long2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9ba7a90039c212ea (void)) ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long2;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_long3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b44cf380afb4d650 (void)) ("
  , "  A arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  return &args_long3;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_float */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b46cc7463fd36e1a (void)) ("
  , "  A arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  return &args_float;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_double */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_eb95876c1227b1d6 (void)) ("
  , "  A arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &args_double;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_bool1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_340ecf91e1edd759 (void)) ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &args_bool1;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_struct */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_940fccf4ff28ad11 (void)) ("
  , "  A arg1,"
  , "  struct some_struct arg2"
  , ")"
  , "{"
  , "  return &args_struct;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_union */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c0ae19f85578b58d (void)) ("
  , "  A arg1,"
  , "  union some_union arg2"
  , ")"
  , "{"
  , "  return &args_union;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_enum */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6f4ca5d36cd98d5e (void)) ("
  , "  A arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  return &args_enum;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_pointer1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8f02d93a83f3e2f3 (void)) ("
  , "  A arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return &args_pointer1;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_pointer2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3217d3d95482b1ac (void)) ("
  , "  A arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  return &args_pointer2;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_pointer3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8e680c501eeba095 (void)) ("
  , "  A arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  return &args_pointer3;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_A */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_dfceb7c79cda3aab (void)) (void)"
  , "{"
  , "  return &ret_A;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_char1 */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_d30074ed19081e69 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char1;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_char2 */"
  , "__attribute__ ((const))"
  , "signed char (*hs_bindgen_c9a2d554e9d3e7e2 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char2;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_char3 */"
  , "__attribute__ ((const))"
  , "unsigned char (*hs_bindgen_261f4f5dd5925788 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char3;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_short1 */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_8d5fba739ef413a8 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short1;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_short2 */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_5dbda022b4ddeeb7 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short2;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_short3 */"
  , "__attribute__ ((const))"
  , "unsigned short (*hs_bindgen_8bfdaeda59194c69 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short3;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_int1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_6b977384ffcfa7c6 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int1;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_int2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_cc47d3f794021505 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int2;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_int3 */"
  , "__attribute__ ((const))"
  , "unsigned int (*hs_bindgen_488fbaf79c234569 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int3;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_long1 */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_8cf14a89b1268b17 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long1;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_long2 */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_617e98b076d9fd82 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long2;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_long3 */"
  , "__attribute__ ((const))"
  , "unsigned long (*hs_bindgen_e1892d8eb6a27221 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long3;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_float */"
  , "__attribute__ ((const))"
  , "float (*hs_bindgen_db7561d69f707657 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_float;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_double */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_e37ad3e87e7cf4de (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_double;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_bool1 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_07705133d0d853ee (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_bool1;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_struct */"
  , "__attribute__ ((const))"
  , "struct some_struct (*hs_bindgen_72cf2ef70b845850 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_struct;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_union */"
  , "__attribute__ ((const))"
  , "union some_union (*hs_bindgen_fe31acee84b319c8 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_union;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_enum */"
  , "__attribute__ ((const))"
  , "enum some_enum (*hs_bindgen_0ec7e00f11946277 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_enum;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_pointer1 */"
  , "__attribute__ ((const))"
  , "signed int *(*hs_bindgen_b09c0f340896fe06 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer1;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_pointer2 */"
  , "__attribute__ ((const))"
  , "signed int **(*hs_bindgen_300fabb661902701 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer2;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_pointer3 */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_12135a6384fcfda7 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer3;"
  , "}"
  , "/* test_macrosreparse_Example_get_body1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_f6ce3e885e11b623 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &body1;"
  , "}"
  , "/* test_macrosreparse_Example_get_body2 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_a7010032c19e6947 (void)) (void)"
  , "{"
  , "  return &body2;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_complex_float */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b9a5de00354a54fe (void)) ("
  , "  A arg1,"
  , "  float _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_float;"
  , "}"
  , "/* test_macrosreparse_Example_get_args_complex_double */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_824a00413581d46c (void)) ("
  , "  A arg1,"
  , "  double _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_double;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_complex_float */"
  , "__attribute__ ((const))"
  , "float _Complex (*hs_bindgen_685d6c0a58e9b874 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_complex_float;"
  , "}"
  , "/* test_macrosreparse_Example_get_ret_complex_double */"
  , "__attribute__ ((const))"
  , "double _Complex (*hs_bindgen_0dea6be8e06122eb (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_complex_double;"
  , "}"
  , "/* test_macrosreparse_Example_get_bespoke_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cd799b883e59eadd (void)) ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &bespoke_args1;"
  , "}"
  , "/* test_macrosreparse_Example_get_bespoke_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_eabc3e2c716b0250 (void)) ("
  , "  A arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &bespoke_args2;"
  , "}"
  , "/* test_macrosreparse_Example_get_bespoke_ret1 */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_8a47565361a0290f (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret1;"
  , "}"
  , "/* test_macrosreparse_Example_get_bespoke_ret2 */"
  , "__attribute__ ((const))"
  , "size_t (*hs_bindgen_384c8ceed3a4ca90 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret2;"
  , "}"
  , "/* test_macrosreparse_Example_get_arr_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b38b526cf0817bf0 (void)) ("
  , "  A arg1[]"
  , ")"
  , "{"
  , "  return &arr_args1;"
  , "}"
  , "/* test_macrosreparse_Example_get_arr_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2c0bca39b42ecbec (void)) ("
  , "  A *arg1[]"
  , ")"
  , "{"
  , "  return &arr_args2;"
  , "}"
  , "/* test_macrosreparse_Example_get_arr_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_82a8c15919ae3f33 (void)) ("
  , "  A arg1[5]"
  , ")"
  , "{"
  , "  return &arr_args3;"
  , "}"
  , "/* test_macrosreparse_Example_get_arr_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_db074918e0d38cb5 (void)) ("
  , "  A *arg1[5]"
  , ")"
  , "{"
  , "  return &arr_args4;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3fd6d1fbd060ffcf (void)) ("
  , "  A arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args1;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_300abb7b10649797 (void)) ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args2;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_78d109e353e05225 (void)) ("
  , "  A arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args3;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ee1815fa9e1c1131 (void)) ("
  , "  A arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args4;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_args5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_146c4cb7cf7def11 (void)) ("
  , "  A arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args5;"
  , "}"
  , "/* test_macrosreparse_Example_get_comments1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7f647ffb81758d69 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &comments1;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_prim_before1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ecf341da5dabe306 (void)) ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before1;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_prim_before2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a0276bc4dac995c4 (void)) ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before2;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_prim_before3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1555fc380f4c34ba (void)) ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before3;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_prim_after1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e88405f039754f3c (void)) ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after1;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_prim_after2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d267bc5333a36861 (void)) ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after2;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_prim_after3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0e08cac4148d54cd (void)) ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after3;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_before1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d7140c03594ba60a (void)) ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before1;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_before2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_92134abe381cef04 (void)) ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before2;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_before3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b80a2c12f1bdb050 (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before3;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_before4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_aa0ffa330554de35 (void)) ("
  , "  A arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before4;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_before5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f59abb8a7dffe11f (void)) ("
  , "  A arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before5;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_before6 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0df28ff2ac7fa1e0 (void)) ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before6;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_before7 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fa8c89b169233a6a (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before7;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_before8 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e707228d3913a299 (void)) ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before8;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_after1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7e7703a610cd6783 (void)) ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after1;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_after2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_988863a241cb28f2 (void)) ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after2;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_after3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c8182ee9a3467005 (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after3;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_after4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_de1e4bbac808bcbc (void)) ("
  , "  A arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after4;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_after5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_746fb03c27f16031 (void)) ("
  , "  A arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after5;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_after6 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_dc174ac598e92a95 (void)) ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after6;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_after7 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2c8ddc46cb100dd9 (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after7;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_withoutSign_after8 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fdf87fbad31246de (void)) ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after8;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_args1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_38808e8aa92d3bcb (void)) ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args1;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_args2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4d37887fd82ab559 (void)) ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args2;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_args3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fd675a45f1682dd5 (void)) ("
  , "  A arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args3;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_args4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_824cb2084c63b803 (void)) ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args4;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_args5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_25bdd13aed8f26c4 (void)) ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args5;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_ret1 */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_f494124b53592961 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret1;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_ret2 */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_32c4b858ef4f3bb8 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret2;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_ret3 */"
  , "__attribute__ ((const))"
  , "signed int *const (*hs_bindgen_110afe05c6d47b7c (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret3;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_ret4 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_042a426acfeaa051 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret4;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_pointers_ret5 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_bc3cdc03ae3274bd (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret5;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_array_elem1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ae95c15a81eacd52 (void)) ("
  , "  A const arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem1;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_array_elem2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9dbbae3758752935 (void)) ("
  , "  A const *arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem2;"
  , "}"
  , "/* test_macrosreparse_Example_get_const_array_elem3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9fdb904f0c0776ed (void)) ("
  , "  A *const arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem3;"
  , "}"
  , "/* test_macrosreparse_Example_get_noParams1 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_cd9806a214acd0fa (void)) (void)"
  , "{"
  , "  return &noParams1;"
  , "}"
  , "/* test_macrosreparse_Example_get_noParams2 */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_7c7bf9b5a41ea4a9 (void)) (void)"
  , "{"
  , "  return &noParams2;"
  , "}"
  , "/* test_macrosreparse_Example_get_noParams3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7f154653725d104d (void)) ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &noParams3;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret1 */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_4460dd1d93c2df6f (void)) ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret1;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret2 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_2b8d04e0fe71178b (void)) ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret2;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret3 */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_27d05f98fe1e869b (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &funptr_ret3;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret4 */"
  , "__attribute__ ((const))"
  , "char (*(*hs_bindgen_f55fb889881240c6 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret4;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret5 */"
  , "__attribute__ ((const))"
  , "signed int *(*(*hs_bindgen_e59cbfc52cec7177 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret5;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret6 */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_f94486b884c7cd44 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret6;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret7 */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_7810d75b42b3bed8 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret7;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret8 */"
  , "__attribute__ ((const))"
  , "signed int *const (*(*hs_bindgen_fc12014317abd6b4 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret8;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret9 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_cf3bf1d8470acad4 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret9;"
  , "}"
  , "/* test_macrosreparse_Example_get_funptr_ret10 */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_44a7eb47cf87f092 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret10;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_Example_get_args_char1@
foreign import ccall unsafe "hs_bindgen_ba0c80bfdbc677bd" hs_bindgen_ba0c80bfdbc677bd_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_char1@
hs_bindgen_ba0c80bfdbc677bd :: IO (RIP.FunPtr (A -> RIP.CChar -> IO ()))
hs_bindgen_ba0c80bfdbc677bd =
  RIP.fromFFIType hs_bindgen_ba0c80bfdbc677bd_base

{-# NOINLINE args_char1 #-}
{-| Function declarations

__C declaration:__ @args_char1@

__defined at:__ @macros\/reparse.h 17:6@

__exported by:__ @macros\/reparse.h@
-}
args_char1 :: RIP.FunPtr (A -> RIP.CChar -> IO ())
args_char1 =
  RIP.unsafePerformIO hs_bindgen_ba0c80bfdbc677bd

-- __unique:__ @test_macrosreparse_Example_get_args_char2@
foreign import ccall unsafe "hs_bindgen_b1e8a2d5e3935f61" hs_bindgen_b1e8a2d5e3935f61_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_char2@
hs_bindgen_b1e8a2d5e3935f61 :: IO (RIP.FunPtr (A -> RIP.CSChar -> IO ()))
hs_bindgen_b1e8a2d5e3935f61 =
  RIP.fromFFIType hs_bindgen_b1e8a2d5e3935f61_base

{-# NOINLINE args_char2 #-}
{-| __C declaration:__ @args_char2@

    __defined at:__ @macros\/reparse.h 18:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char2 :: RIP.FunPtr (A -> RIP.CSChar -> IO ())
args_char2 =
  RIP.unsafePerformIO hs_bindgen_b1e8a2d5e3935f61

-- __unique:__ @test_macrosreparse_Example_get_args_char3@
foreign import ccall unsafe "hs_bindgen_deb136b9b9d89650" hs_bindgen_deb136b9b9d89650_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_char3@
hs_bindgen_deb136b9b9d89650 :: IO (RIP.FunPtr (A -> RIP.CUChar -> IO ()))
hs_bindgen_deb136b9b9d89650 =
  RIP.fromFFIType hs_bindgen_deb136b9b9d89650_base

{-# NOINLINE args_char3 #-}
{-| __C declaration:__ @args_char3@

    __defined at:__ @macros\/reparse.h 19:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char3 :: RIP.FunPtr (A -> RIP.CUChar -> IO ())
args_char3 =
  RIP.unsafePerformIO hs_bindgen_deb136b9b9d89650

-- __unique:__ @test_macrosreparse_Example_get_args_short1@
foreign import ccall unsafe "hs_bindgen_7a71d1e1867636bf" hs_bindgen_7a71d1e1867636bf_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_short1@
hs_bindgen_7a71d1e1867636bf :: IO (RIP.FunPtr (A -> RIP.CShort -> IO ()))
hs_bindgen_7a71d1e1867636bf =
  RIP.fromFFIType hs_bindgen_7a71d1e1867636bf_base

{-# NOINLINE args_short1 #-}
{-| __C declaration:__ @args_short1@

    __defined at:__ @macros\/reparse.h 21:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short1 :: RIP.FunPtr (A -> RIP.CShort -> IO ())
args_short1 =
  RIP.unsafePerformIO hs_bindgen_7a71d1e1867636bf

-- __unique:__ @test_macrosreparse_Example_get_args_short2@
foreign import ccall unsafe "hs_bindgen_ad7132b76947c638" hs_bindgen_ad7132b76947c638_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_short2@
hs_bindgen_ad7132b76947c638 :: IO (RIP.FunPtr (A -> RIP.CShort -> IO ()))
hs_bindgen_ad7132b76947c638 =
  RIP.fromFFIType hs_bindgen_ad7132b76947c638_base

{-# NOINLINE args_short2 #-}
{-| __C declaration:__ @args_short2@

    __defined at:__ @macros\/reparse.h 22:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short2 :: RIP.FunPtr (A -> RIP.CShort -> IO ())
args_short2 =
  RIP.unsafePerformIO hs_bindgen_ad7132b76947c638

-- __unique:__ @test_macrosreparse_Example_get_args_short3@
foreign import ccall unsafe "hs_bindgen_f4842ac12be0c136" hs_bindgen_f4842ac12be0c136_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_short3@
hs_bindgen_f4842ac12be0c136 :: IO (RIP.FunPtr (A -> RIP.CUShort -> IO ()))
hs_bindgen_f4842ac12be0c136 =
  RIP.fromFFIType hs_bindgen_f4842ac12be0c136_base

{-# NOINLINE args_short3 #-}
{-| __C declaration:__ @args_short3@

    __defined at:__ @macros\/reparse.h 23:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short3 :: RIP.FunPtr (A -> RIP.CUShort -> IO ())
args_short3 =
  RIP.unsafePerformIO hs_bindgen_f4842ac12be0c136

-- __unique:__ @test_macrosreparse_Example_get_args_int1@
foreign import ccall unsafe "hs_bindgen_3dc6ae7bb850c676" hs_bindgen_3dc6ae7bb850c676_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_int1@
hs_bindgen_3dc6ae7bb850c676 :: IO (RIP.FunPtr (A -> RIP.CInt -> IO ()))
hs_bindgen_3dc6ae7bb850c676 =
  RIP.fromFFIType hs_bindgen_3dc6ae7bb850c676_base

{-# NOINLINE args_int1 #-}
{-| __C declaration:__ @args_int1@

    __defined at:__ @macros\/reparse.h 25:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int1 :: RIP.FunPtr (A -> RIP.CInt -> IO ())
args_int1 =
  RIP.unsafePerformIO hs_bindgen_3dc6ae7bb850c676

-- __unique:__ @test_macrosreparse_Example_get_args_int2@
foreign import ccall unsafe "hs_bindgen_a76cb45502a6ea40" hs_bindgen_a76cb45502a6ea40_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_int2@
hs_bindgen_a76cb45502a6ea40 :: IO (RIP.FunPtr (A -> RIP.CInt -> IO ()))
hs_bindgen_a76cb45502a6ea40 =
  RIP.fromFFIType hs_bindgen_a76cb45502a6ea40_base

{-# NOINLINE args_int2 #-}
{-| __C declaration:__ @args_int2@

    __defined at:__ @macros\/reparse.h 26:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int2 :: RIP.FunPtr (A -> RIP.CInt -> IO ())
args_int2 =
  RIP.unsafePerformIO hs_bindgen_a76cb45502a6ea40

-- __unique:__ @test_macrosreparse_Example_get_args_int3@
foreign import ccall unsafe "hs_bindgen_99f48290fac28118" hs_bindgen_99f48290fac28118_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_int3@
hs_bindgen_99f48290fac28118 :: IO (RIP.FunPtr (A -> RIP.CUInt -> IO ()))
hs_bindgen_99f48290fac28118 =
  RIP.fromFFIType hs_bindgen_99f48290fac28118_base

{-# NOINLINE args_int3 #-}
{-| __C declaration:__ @args_int3@

    __defined at:__ @macros\/reparse.h 27:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int3 :: RIP.FunPtr (A -> RIP.CUInt -> IO ())
args_int3 =
  RIP.unsafePerformIO hs_bindgen_99f48290fac28118

-- __unique:__ @test_macrosreparse_Example_get_args_long1@
foreign import ccall unsafe "hs_bindgen_60a477b55893fa8e" hs_bindgen_60a477b55893fa8e_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_long1@
hs_bindgen_60a477b55893fa8e :: IO (RIP.FunPtr (A -> RIP.CLong -> IO ()))
hs_bindgen_60a477b55893fa8e =
  RIP.fromFFIType hs_bindgen_60a477b55893fa8e_base

{-# NOINLINE args_long1 #-}
{-| __C declaration:__ @args_long1@

    __defined at:__ @macros\/reparse.h 29:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long1 :: RIP.FunPtr (A -> RIP.CLong -> IO ())
args_long1 =
  RIP.unsafePerformIO hs_bindgen_60a477b55893fa8e

-- __unique:__ @test_macrosreparse_Example_get_args_long2@
foreign import ccall unsafe "hs_bindgen_9ba7a90039c212ea" hs_bindgen_9ba7a90039c212ea_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_long2@
hs_bindgen_9ba7a90039c212ea :: IO (RIP.FunPtr (A -> RIP.CLong -> IO ()))
hs_bindgen_9ba7a90039c212ea =
  RIP.fromFFIType hs_bindgen_9ba7a90039c212ea_base

{-# NOINLINE args_long2 #-}
{-| __C declaration:__ @args_long2@

    __defined at:__ @macros\/reparse.h 30:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long2 :: RIP.FunPtr (A -> RIP.CLong -> IO ())
args_long2 =
  RIP.unsafePerformIO hs_bindgen_9ba7a90039c212ea

-- __unique:__ @test_macrosreparse_Example_get_args_long3@
foreign import ccall unsafe "hs_bindgen_b44cf380afb4d650" hs_bindgen_b44cf380afb4d650_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_long3@
hs_bindgen_b44cf380afb4d650 :: IO (RIP.FunPtr (A -> RIP.CULong -> IO ()))
hs_bindgen_b44cf380afb4d650 =
  RIP.fromFFIType hs_bindgen_b44cf380afb4d650_base

{-# NOINLINE args_long3 #-}
{-| __C declaration:__ @args_long3@

    __defined at:__ @macros\/reparse.h 31:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long3 :: RIP.FunPtr (A -> RIP.CULong -> IO ())
args_long3 =
  RIP.unsafePerformIO hs_bindgen_b44cf380afb4d650

-- __unique:__ @test_macrosreparse_Example_get_args_float@
foreign import ccall unsafe "hs_bindgen_b46cc7463fd36e1a" hs_bindgen_b46cc7463fd36e1a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_float@
hs_bindgen_b46cc7463fd36e1a :: IO (RIP.FunPtr (A -> RIP.CFloat -> IO ()))
hs_bindgen_b46cc7463fd36e1a =
  RIP.fromFFIType hs_bindgen_b46cc7463fd36e1a_base

{-# NOINLINE args_float #-}
{-| __C declaration:__ @args_float@

    __defined at:__ @macros\/reparse.h 33:6@

    __exported by:__ @macros\/reparse.h@
-}
args_float :: RIP.FunPtr (A -> RIP.CFloat -> IO ())
args_float =
  RIP.unsafePerformIO hs_bindgen_b46cc7463fd36e1a

-- __unique:__ @test_macrosreparse_Example_get_args_double@
foreign import ccall unsafe "hs_bindgen_eb95876c1227b1d6" hs_bindgen_eb95876c1227b1d6_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_double@
hs_bindgen_eb95876c1227b1d6 :: IO (RIP.FunPtr (A -> RIP.CDouble -> IO ()))
hs_bindgen_eb95876c1227b1d6 =
  RIP.fromFFIType hs_bindgen_eb95876c1227b1d6_base

{-# NOINLINE args_double #-}
{-| __C declaration:__ @args_double@

    __defined at:__ @macros\/reparse.h 34:6@

    __exported by:__ @macros\/reparse.h@
-}
args_double :: RIP.FunPtr (A -> RIP.CDouble -> IO ())
args_double =
  RIP.unsafePerformIO hs_bindgen_eb95876c1227b1d6

-- __unique:__ @test_macrosreparse_Example_get_args_bool1@
foreign import ccall unsafe "hs_bindgen_340ecf91e1edd759" hs_bindgen_340ecf91e1edd759_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_bool1@
hs_bindgen_340ecf91e1edd759 :: IO (RIP.FunPtr (A -> RIP.CBool -> IO ()))
hs_bindgen_340ecf91e1edd759 =
  RIP.fromFFIType hs_bindgen_340ecf91e1edd759_base

{-# NOINLINE args_bool1 #-}
{-| __C declaration:__ @args_bool1@

    __defined at:__ @macros\/reparse.h 35:6@

    __exported by:__ @macros\/reparse.h@
-}
args_bool1 :: RIP.FunPtr (A -> RIP.CBool -> IO ())
args_bool1 =
  RIP.unsafePerformIO hs_bindgen_340ecf91e1edd759

-- __unique:__ @test_macrosreparse_Example_get_args_struct@
foreign import ccall unsafe "hs_bindgen_940fccf4ff28ad11" hs_bindgen_940fccf4ff28ad11_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_struct@
hs_bindgen_940fccf4ff28ad11 :: IO (RIP.FunPtr (A -> Some_struct -> IO ()))
hs_bindgen_940fccf4ff28ad11 =
  RIP.fromFFIType hs_bindgen_940fccf4ff28ad11_base

{-# NOINLINE args_struct #-}
{-| __C declaration:__ @args_struct@

    __defined at:__ @macros\/reparse.h 37:6@

    __exported by:__ @macros\/reparse.h@
-}
args_struct :: RIP.FunPtr (A -> Some_struct -> IO ())
args_struct =
  RIP.unsafePerformIO hs_bindgen_940fccf4ff28ad11

-- __unique:__ @test_macrosreparse_Example_get_args_union@
foreign import ccall unsafe "hs_bindgen_c0ae19f85578b58d" hs_bindgen_c0ae19f85578b58d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_union@
hs_bindgen_c0ae19f85578b58d :: IO (RIP.FunPtr (A -> Some_union -> IO ()))
hs_bindgen_c0ae19f85578b58d =
  RIP.fromFFIType hs_bindgen_c0ae19f85578b58d_base

{-# NOINLINE args_union #-}
{-| __C declaration:__ @args_union@

    __defined at:__ @macros\/reparse.h 38:6@

    __exported by:__ @macros\/reparse.h@
-}
args_union :: RIP.FunPtr (A -> Some_union -> IO ())
args_union =
  RIP.unsafePerformIO hs_bindgen_c0ae19f85578b58d

-- __unique:__ @test_macrosreparse_Example_get_args_enum@
foreign import ccall unsafe "hs_bindgen_6f4ca5d36cd98d5e" hs_bindgen_6f4ca5d36cd98d5e_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_enum@
hs_bindgen_6f4ca5d36cd98d5e :: IO (RIP.FunPtr (A -> Some_enum -> IO ()))
hs_bindgen_6f4ca5d36cd98d5e =
  RIP.fromFFIType hs_bindgen_6f4ca5d36cd98d5e_base

{-# NOINLINE args_enum #-}
{-| __C declaration:__ @args_enum@

    __defined at:__ @macros\/reparse.h 39:6@

    __exported by:__ @macros\/reparse.h@
-}
args_enum :: RIP.FunPtr (A -> Some_enum -> IO ())
args_enum =
  RIP.unsafePerformIO hs_bindgen_6f4ca5d36cd98d5e

-- __unique:__ @test_macrosreparse_Example_get_args_pointer1@
foreign import ccall unsafe "hs_bindgen_8f02d93a83f3e2f3" hs_bindgen_8f02d93a83f3e2f3_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_pointer1@
hs_bindgen_8f02d93a83f3e2f3 :: IO (RIP.FunPtr (A -> (RIP.Ptr RIP.CInt) -> IO ()))
hs_bindgen_8f02d93a83f3e2f3 =
  RIP.fromFFIType hs_bindgen_8f02d93a83f3e2f3_base

{-# NOINLINE args_pointer1 #-}
{-| __C declaration:__ @args_pointer1@

    __defined at:__ @macros\/reparse.h 41:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer1 :: RIP.FunPtr (A -> (RIP.Ptr RIP.CInt) -> IO ())
args_pointer1 =
  RIP.unsafePerformIO hs_bindgen_8f02d93a83f3e2f3

-- __unique:__ @test_macrosreparse_Example_get_args_pointer2@
foreign import ccall unsafe "hs_bindgen_3217d3d95482b1ac" hs_bindgen_3217d3d95482b1ac_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_pointer2@
hs_bindgen_3217d3d95482b1ac :: IO (RIP.FunPtr (A -> (RIP.Ptr (RIP.Ptr RIP.CInt)) -> IO ()))
hs_bindgen_3217d3d95482b1ac =
  RIP.fromFFIType hs_bindgen_3217d3d95482b1ac_base

{-# NOINLINE args_pointer2 #-}
{-| __C declaration:__ @args_pointer2@

    __defined at:__ @macros\/reparse.h 42:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer2 :: RIP.FunPtr (A -> (RIP.Ptr (RIP.Ptr RIP.CInt)) -> IO ())
args_pointer2 =
  RIP.unsafePerformIO hs_bindgen_3217d3d95482b1ac

-- __unique:__ @test_macrosreparse_Example_get_args_pointer3@
foreign import ccall unsafe "hs_bindgen_8e680c501eeba095" hs_bindgen_8e680c501eeba095_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_pointer3@
hs_bindgen_8e680c501eeba095 :: IO (RIP.FunPtr (A -> (RIP.Ptr RIP.Void) -> IO ()))
hs_bindgen_8e680c501eeba095 =
  RIP.fromFFIType hs_bindgen_8e680c501eeba095_base

{-# NOINLINE args_pointer3 #-}
{-| __C declaration:__ @args_pointer3@

    __defined at:__ @macros\/reparse.h 43:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer3 :: RIP.FunPtr (A -> (RIP.Ptr RIP.Void) -> IO ())
args_pointer3 =
  RIP.unsafePerformIO hs_bindgen_8e680c501eeba095

-- __unique:__ @test_macrosreparse_Example_get_ret_A@
foreign import ccall unsafe "hs_bindgen_dfceb7c79cda3aab" hs_bindgen_dfceb7c79cda3aab_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_A@
hs_bindgen_dfceb7c79cda3aab :: IO (RIP.FunPtr (IO A))
hs_bindgen_dfceb7c79cda3aab =
  RIP.fromFFIType hs_bindgen_dfceb7c79cda3aab_base

{-# NOINLINE ret_A #-}
{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h 47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A :: RIP.FunPtr (IO A)
ret_A =
  RIP.unsafePerformIO hs_bindgen_dfceb7c79cda3aab

-- __unique:__ @test_macrosreparse_Example_get_ret_char1@
foreign import ccall unsafe "hs_bindgen_d30074ed19081e69" hs_bindgen_d30074ed19081e69_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_char1@
hs_bindgen_d30074ed19081e69 :: IO (RIP.FunPtr (A -> IO RIP.CChar))
hs_bindgen_d30074ed19081e69 =
  RIP.fromFFIType hs_bindgen_d30074ed19081e69_base

{-# NOINLINE ret_char1 #-}
{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h 49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1 :: RIP.FunPtr (A -> IO RIP.CChar)
ret_char1 =
  RIP.unsafePerformIO hs_bindgen_d30074ed19081e69

-- __unique:__ @test_macrosreparse_Example_get_ret_char2@
foreign import ccall unsafe "hs_bindgen_c9a2d554e9d3e7e2" hs_bindgen_c9a2d554e9d3e7e2_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_char2@
hs_bindgen_c9a2d554e9d3e7e2 :: IO (RIP.FunPtr (A -> IO RIP.CSChar))
hs_bindgen_c9a2d554e9d3e7e2 =
  RIP.fromFFIType hs_bindgen_c9a2d554e9d3e7e2_base

{-# NOINLINE ret_char2 #-}
{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h 50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2 :: RIP.FunPtr (A -> IO RIP.CSChar)
ret_char2 =
  RIP.unsafePerformIO hs_bindgen_c9a2d554e9d3e7e2

-- __unique:__ @test_macrosreparse_Example_get_ret_char3@
foreign import ccall unsafe "hs_bindgen_261f4f5dd5925788" hs_bindgen_261f4f5dd5925788_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_char3@
hs_bindgen_261f4f5dd5925788 :: IO (RIP.FunPtr (A -> IO RIP.CUChar))
hs_bindgen_261f4f5dd5925788 =
  RIP.fromFFIType hs_bindgen_261f4f5dd5925788_base

{-# NOINLINE ret_char3 #-}
{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h 51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3 :: RIP.FunPtr (A -> IO RIP.CUChar)
ret_char3 =
  RIP.unsafePerformIO hs_bindgen_261f4f5dd5925788

-- __unique:__ @test_macrosreparse_Example_get_ret_short1@
foreign import ccall unsafe "hs_bindgen_8d5fba739ef413a8" hs_bindgen_8d5fba739ef413a8_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_short1@
hs_bindgen_8d5fba739ef413a8 :: IO (RIP.FunPtr (A -> IO RIP.CShort))
hs_bindgen_8d5fba739ef413a8 =
  RIP.fromFFIType hs_bindgen_8d5fba739ef413a8_base

{-# NOINLINE ret_short1 #-}
{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h 53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1 :: RIP.FunPtr (A -> IO RIP.CShort)
ret_short1 =
  RIP.unsafePerformIO hs_bindgen_8d5fba739ef413a8

-- __unique:__ @test_macrosreparse_Example_get_ret_short2@
foreign import ccall unsafe "hs_bindgen_5dbda022b4ddeeb7" hs_bindgen_5dbda022b4ddeeb7_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_short2@
hs_bindgen_5dbda022b4ddeeb7 :: IO (RIP.FunPtr (A -> IO RIP.CShort))
hs_bindgen_5dbda022b4ddeeb7 =
  RIP.fromFFIType hs_bindgen_5dbda022b4ddeeb7_base

{-# NOINLINE ret_short2 #-}
{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h 54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2 :: RIP.FunPtr (A -> IO RIP.CShort)
ret_short2 =
  RIP.unsafePerformIO hs_bindgen_5dbda022b4ddeeb7

-- __unique:__ @test_macrosreparse_Example_get_ret_short3@
foreign import ccall unsafe "hs_bindgen_8bfdaeda59194c69" hs_bindgen_8bfdaeda59194c69_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_short3@
hs_bindgen_8bfdaeda59194c69 :: IO (RIP.FunPtr (A -> IO RIP.CUShort))
hs_bindgen_8bfdaeda59194c69 =
  RIP.fromFFIType hs_bindgen_8bfdaeda59194c69_base

{-# NOINLINE ret_short3 #-}
{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h 55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3 :: RIP.FunPtr (A -> IO RIP.CUShort)
ret_short3 =
  RIP.unsafePerformIO hs_bindgen_8bfdaeda59194c69

-- __unique:__ @test_macrosreparse_Example_get_ret_int1@
foreign import ccall unsafe "hs_bindgen_6b977384ffcfa7c6" hs_bindgen_6b977384ffcfa7c6_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_int1@
hs_bindgen_6b977384ffcfa7c6 :: IO (RIP.FunPtr (A -> IO RIP.CInt))
hs_bindgen_6b977384ffcfa7c6 =
  RIP.fromFFIType hs_bindgen_6b977384ffcfa7c6_base

{-# NOINLINE ret_int1 #-}
{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h 57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1 :: RIP.FunPtr (A -> IO RIP.CInt)
ret_int1 =
  RIP.unsafePerformIO hs_bindgen_6b977384ffcfa7c6

-- __unique:__ @test_macrosreparse_Example_get_ret_int2@
foreign import ccall unsafe "hs_bindgen_cc47d3f794021505" hs_bindgen_cc47d3f794021505_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_int2@
hs_bindgen_cc47d3f794021505 :: IO (RIP.FunPtr (A -> IO RIP.CInt))
hs_bindgen_cc47d3f794021505 =
  RIP.fromFFIType hs_bindgen_cc47d3f794021505_base

{-# NOINLINE ret_int2 #-}
{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h 58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2 :: RIP.FunPtr (A -> IO RIP.CInt)
ret_int2 =
  RIP.unsafePerformIO hs_bindgen_cc47d3f794021505

-- __unique:__ @test_macrosreparse_Example_get_ret_int3@
foreign import ccall unsafe "hs_bindgen_488fbaf79c234569" hs_bindgen_488fbaf79c234569_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_int3@
hs_bindgen_488fbaf79c234569 :: IO (RIP.FunPtr (A -> IO RIP.CUInt))
hs_bindgen_488fbaf79c234569 =
  RIP.fromFFIType hs_bindgen_488fbaf79c234569_base

{-# NOINLINE ret_int3 #-}
{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h 59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3 :: RIP.FunPtr (A -> IO RIP.CUInt)
ret_int3 =
  RIP.unsafePerformIO hs_bindgen_488fbaf79c234569

-- __unique:__ @test_macrosreparse_Example_get_ret_long1@
foreign import ccall unsafe "hs_bindgen_8cf14a89b1268b17" hs_bindgen_8cf14a89b1268b17_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_long1@
hs_bindgen_8cf14a89b1268b17 :: IO (RIP.FunPtr (A -> IO RIP.CLong))
hs_bindgen_8cf14a89b1268b17 =
  RIP.fromFFIType hs_bindgen_8cf14a89b1268b17_base

{-# NOINLINE ret_long1 #-}
{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h 61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1 :: RIP.FunPtr (A -> IO RIP.CLong)
ret_long1 =
  RIP.unsafePerformIO hs_bindgen_8cf14a89b1268b17

-- __unique:__ @test_macrosreparse_Example_get_ret_long2@
foreign import ccall unsafe "hs_bindgen_617e98b076d9fd82" hs_bindgen_617e98b076d9fd82_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_long2@
hs_bindgen_617e98b076d9fd82 :: IO (RIP.FunPtr (A -> IO RIP.CLong))
hs_bindgen_617e98b076d9fd82 =
  RIP.fromFFIType hs_bindgen_617e98b076d9fd82_base

{-# NOINLINE ret_long2 #-}
{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h 62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2 :: RIP.FunPtr (A -> IO RIP.CLong)
ret_long2 =
  RIP.unsafePerformIO hs_bindgen_617e98b076d9fd82

-- __unique:__ @test_macrosreparse_Example_get_ret_long3@
foreign import ccall unsafe "hs_bindgen_e1892d8eb6a27221" hs_bindgen_e1892d8eb6a27221_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_long3@
hs_bindgen_e1892d8eb6a27221 :: IO (RIP.FunPtr (A -> IO RIP.CULong))
hs_bindgen_e1892d8eb6a27221 =
  RIP.fromFFIType hs_bindgen_e1892d8eb6a27221_base

{-# NOINLINE ret_long3 #-}
{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h 63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3 :: RIP.FunPtr (A -> IO RIP.CULong)
ret_long3 =
  RIP.unsafePerformIO hs_bindgen_e1892d8eb6a27221

-- __unique:__ @test_macrosreparse_Example_get_ret_float@
foreign import ccall unsafe "hs_bindgen_db7561d69f707657" hs_bindgen_db7561d69f707657_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_float@
hs_bindgen_db7561d69f707657 :: IO (RIP.FunPtr (A -> IO RIP.CFloat))
hs_bindgen_db7561d69f707657 =
  RIP.fromFFIType hs_bindgen_db7561d69f707657_base

{-# NOINLINE ret_float #-}
{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h 65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float :: RIP.FunPtr (A -> IO RIP.CFloat)
ret_float =
  RIP.unsafePerformIO hs_bindgen_db7561d69f707657

-- __unique:__ @test_macrosreparse_Example_get_ret_double@
foreign import ccall unsafe "hs_bindgen_e37ad3e87e7cf4de" hs_bindgen_e37ad3e87e7cf4de_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_double@
hs_bindgen_e37ad3e87e7cf4de :: IO (RIP.FunPtr (A -> IO RIP.CDouble))
hs_bindgen_e37ad3e87e7cf4de =
  RIP.fromFFIType hs_bindgen_e37ad3e87e7cf4de_base

{-# NOINLINE ret_double #-}
{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h 66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double :: RIP.FunPtr (A -> IO RIP.CDouble)
ret_double =
  RIP.unsafePerformIO hs_bindgen_e37ad3e87e7cf4de

-- __unique:__ @test_macrosreparse_Example_get_ret_bool1@
foreign import ccall unsafe "hs_bindgen_07705133d0d853ee" hs_bindgen_07705133d0d853ee_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_bool1@
hs_bindgen_07705133d0d853ee :: IO (RIP.FunPtr (A -> IO RIP.CBool))
hs_bindgen_07705133d0d853ee =
  RIP.fromFFIType hs_bindgen_07705133d0d853ee_base

{-# NOINLINE ret_bool1 #-}
{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h 67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1 :: RIP.FunPtr (A -> IO RIP.CBool)
ret_bool1 =
  RIP.unsafePerformIO hs_bindgen_07705133d0d853ee

-- __unique:__ @test_macrosreparse_Example_get_ret_struct@
foreign import ccall unsafe "hs_bindgen_72cf2ef70b845850" hs_bindgen_72cf2ef70b845850_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_struct@
hs_bindgen_72cf2ef70b845850 :: IO (RIP.FunPtr (A -> IO Some_struct))
hs_bindgen_72cf2ef70b845850 =
  RIP.fromFFIType hs_bindgen_72cf2ef70b845850_base

{-# NOINLINE ret_struct #-}
{-| __C declaration:__ @ret_struct@

    __defined at:__ @macros\/reparse.h 69:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_struct :: RIP.FunPtr (A -> IO Some_struct)
ret_struct =
  RIP.unsafePerformIO hs_bindgen_72cf2ef70b845850

-- __unique:__ @test_macrosreparse_Example_get_ret_union@
foreign import ccall unsafe "hs_bindgen_fe31acee84b319c8" hs_bindgen_fe31acee84b319c8_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_union@
hs_bindgen_fe31acee84b319c8 :: IO (RIP.FunPtr (A -> IO Some_union))
hs_bindgen_fe31acee84b319c8 =
  RIP.fromFFIType hs_bindgen_fe31acee84b319c8_base

{-# NOINLINE ret_union #-}
{-| __C declaration:__ @ret_union@

    __defined at:__ @macros\/reparse.h 70:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_union :: RIP.FunPtr (A -> IO Some_union)
ret_union =
  RIP.unsafePerformIO hs_bindgen_fe31acee84b319c8

-- __unique:__ @test_macrosreparse_Example_get_ret_enum@
foreign import ccall unsafe "hs_bindgen_0ec7e00f11946277" hs_bindgen_0ec7e00f11946277_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_enum@
hs_bindgen_0ec7e00f11946277 :: IO (RIP.FunPtr (A -> IO Some_enum))
hs_bindgen_0ec7e00f11946277 =
  RIP.fromFFIType hs_bindgen_0ec7e00f11946277_base

{-# NOINLINE ret_enum #-}
{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h 71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum :: RIP.FunPtr (A -> IO Some_enum)
ret_enum =
  RIP.unsafePerformIO hs_bindgen_0ec7e00f11946277

-- __unique:__ @test_macrosreparse_Example_get_ret_pointer1@
foreign import ccall unsafe "hs_bindgen_b09c0f340896fe06" hs_bindgen_b09c0f340896fe06_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_pointer1@
hs_bindgen_b09c0f340896fe06 :: IO (RIP.FunPtr (A -> IO (RIP.Ptr RIP.CInt)))
hs_bindgen_b09c0f340896fe06 =
  RIP.fromFFIType hs_bindgen_b09c0f340896fe06_base

{-# NOINLINE ret_pointer1 #-}
{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h 73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1 :: RIP.FunPtr (A -> IO (RIP.Ptr RIP.CInt))
ret_pointer1 =
  RIP.unsafePerformIO hs_bindgen_b09c0f340896fe06

-- __unique:__ @test_macrosreparse_Example_get_ret_pointer2@
foreign import ccall unsafe "hs_bindgen_300fabb661902701" hs_bindgen_300fabb661902701_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_pointer2@
hs_bindgen_300fabb661902701 :: IO (RIP.FunPtr (A -> IO (RIP.Ptr (RIP.Ptr RIP.CInt))))
hs_bindgen_300fabb661902701 =
  RIP.fromFFIType hs_bindgen_300fabb661902701_base

{-# NOINLINE ret_pointer2 #-}
{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h 74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2 :: RIP.FunPtr (A -> IO (RIP.Ptr (RIP.Ptr RIP.CInt)))
ret_pointer2 =
  RIP.unsafePerformIO hs_bindgen_300fabb661902701

-- __unique:__ @test_macrosreparse_Example_get_ret_pointer3@
foreign import ccall unsafe "hs_bindgen_12135a6384fcfda7" hs_bindgen_12135a6384fcfda7_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_pointer3@
hs_bindgen_12135a6384fcfda7 :: IO (RIP.FunPtr (A -> IO (RIP.Ptr RIP.Void)))
hs_bindgen_12135a6384fcfda7 =
  RIP.fromFFIType hs_bindgen_12135a6384fcfda7_base

{-# NOINLINE ret_pointer3 #-}
{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h 75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3 :: RIP.FunPtr (A -> IO (RIP.Ptr RIP.Void))
ret_pointer3 =
  RIP.unsafePerformIO hs_bindgen_12135a6384fcfda7

-- __unique:__ @test_macrosreparse_Example_get_body1@
foreign import ccall unsafe "hs_bindgen_f6ce3e885e11b623" hs_bindgen_f6ce3e885e11b623_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_body1@
hs_bindgen_f6ce3e885e11b623 :: IO (RIP.FunPtr (A -> IO RIP.CInt))
hs_bindgen_f6ce3e885e11b623 =
  RIP.fromFFIType hs_bindgen_f6ce3e885e11b623_base

{-# NOINLINE body1 #-}
{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h 79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1 :: RIP.FunPtr (A -> IO RIP.CInt)
body1 =
  RIP.unsafePerformIO hs_bindgen_f6ce3e885e11b623

-- __unique:__ @test_macrosreparse_Example_get_body2@
foreign import ccall unsafe "hs_bindgen_a7010032c19e6947" hs_bindgen_a7010032c19e6947_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_body2@
hs_bindgen_a7010032c19e6947 :: IO (RIP.FunPtr (IO A))
hs_bindgen_a7010032c19e6947 =
  RIP.fromFFIType hs_bindgen_a7010032c19e6947_base

{-# NOINLINE body2 #-}
{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h 80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2 :: RIP.FunPtr (IO A)
body2 =
  RIP.unsafePerformIO hs_bindgen_a7010032c19e6947

-- __unique:__ @test_macrosreparse_Example_get_args_complex_float@
foreign import ccall unsafe "hs_bindgen_b9a5de00354a54fe" hs_bindgen_b9a5de00354a54fe_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_complex_float@
hs_bindgen_b9a5de00354a54fe :: IO (RIP.FunPtr (A -> (RIP.Complex RIP.CFloat) -> IO ()))
hs_bindgen_b9a5de00354a54fe =
  RIP.fromFFIType hs_bindgen_b9a5de00354a54fe_base

{-# NOINLINE args_complex_float #-}
{-| __C declaration:__ @args_complex_float@

    __defined at:__ @macros\/reparse.h 84:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_float :: RIP.FunPtr (A -> (RIP.Complex RIP.CFloat) -> IO ())
args_complex_float =
  RIP.unsafePerformIO hs_bindgen_b9a5de00354a54fe

-- __unique:__ @test_macrosreparse_Example_get_args_complex_double@
foreign import ccall unsafe "hs_bindgen_824a00413581d46c" hs_bindgen_824a00413581d46c_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_args_complex_double@
hs_bindgen_824a00413581d46c :: IO (RIP.FunPtr (A -> (RIP.Complex RIP.CDouble) -> IO ()))
hs_bindgen_824a00413581d46c =
  RIP.fromFFIType hs_bindgen_824a00413581d46c_base

{-# NOINLINE args_complex_double #-}
{-| __C declaration:__ @args_complex_double@

    __defined at:__ @macros\/reparse.h 85:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_double :: RIP.FunPtr (A -> (RIP.Complex RIP.CDouble) -> IO ())
args_complex_double =
  RIP.unsafePerformIO hs_bindgen_824a00413581d46c

-- __unique:__ @test_macrosreparse_Example_get_ret_complex_float@
foreign import ccall unsafe "hs_bindgen_685d6c0a58e9b874" hs_bindgen_685d6c0a58e9b874_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_complex_float@
hs_bindgen_685d6c0a58e9b874 :: IO (RIP.FunPtr (A -> IO (RIP.Complex RIP.CFloat)))
hs_bindgen_685d6c0a58e9b874 =
  RIP.fromFFIType hs_bindgen_685d6c0a58e9b874_base

{-# NOINLINE ret_complex_float #-}
{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @macros\/reparse.h 86:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_float :: RIP.FunPtr (A -> IO (RIP.Complex RIP.CFloat))
ret_complex_float =
  RIP.unsafePerformIO hs_bindgen_685d6c0a58e9b874

-- __unique:__ @test_macrosreparse_Example_get_ret_complex_double@
foreign import ccall unsafe "hs_bindgen_0dea6be8e06122eb" hs_bindgen_0dea6be8e06122eb_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_ret_complex_double@
hs_bindgen_0dea6be8e06122eb :: IO (RIP.FunPtr (A -> IO (RIP.Complex RIP.CDouble)))
hs_bindgen_0dea6be8e06122eb =
  RIP.fromFFIType hs_bindgen_0dea6be8e06122eb_base

{-# NOINLINE ret_complex_double #-}
{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @macros\/reparse.h 87:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_double :: RIP.FunPtr (A -> IO (RIP.Complex RIP.CDouble))
ret_complex_double =
  RIP.unsafePerformIO hs_bindgen_0dea6be8e06122eb

-- __unique:__ @test_macrosreparse_Example_get_bespoke_args1@
foreign import ccall unsafe "hs_bindgen_cd799b883e59eadd" hs_bindgen_cd799b883e59eadd_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_bespoke_args1@
hs_bindgen_cd799b883e59eadd :: IO (RIP.FunPtr (A -> RIP.CBool -> IO ()))
hs_bindgen_cd799b883e59eadd =
  RIP.fromFFIType hs_bindgen_cd799b883e59eadd_base

{-# NOINLINE bespoke_args1 #-}
{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @macros\/reparse.h 94:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args1 :: RIP.FunPtr (A -> RIP.CBool -> IO ())
bespoke_args1 =
  RIP.unsafePerformIO hs_bindgen_cd799b883e59eadd

-- __unique:__ @test_macrosreparse_Example_get_bespoke_args2@
foreign import ccall unsafe "hs_bindgen_eabc3e2c716b0250" hs_bindgen_eabc3e2c716b0250_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_bespoke_args2@
hs_bindgen_eabc3e2c716b0250 :: IO (RIP.FunPtr (A -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_eabc3e2c716b0250 =
  RIP.fromFFIType hs_bindgen_eabc3e2c716b0250_base

{-# NOINLINE bespoke_args2 #-}
{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @macros\/reparse.h 95:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args2 :: RIP.FunPtr (A -> HsBindgen.Runtime.LibC.CSize -> IO ())
bespoke_args2 =
  RIP.unsafePerformIO hs_bindgen_eabc3e2c716b0250

-- __unique:__ @test_macrosreparse_Example_get_bespoke_ret1@
foreign import ccall unsafe "hs_bindgen_8a47565361a0290f" hs_bindgen_8a47565361a0290f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_bespoke_ret1@
hs_bindgen_8a47565361a0290f :: IO (RIP.FunPtr (A -> IO RIP.CBool))
hs_bindgen_8a47565361a0290f =
  RIP.fromFFIType hs_bindgen_8a47565361a0290f_base

{-# NOINLINE bespoke_ret1 #-}
{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h 97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1 :: RIP.FunPtr (A -> IO RIP.CBool)
bespoke_ret1 =
  RIP.unsafePerformIO hs_bindgen_8a47565361a0290f

-- __unique:__ @test_macrosreparse_Example_get_bespoke_ret2@
foreign import ccall unsafe "hs_bindgen_384c8ceed3a4ca90" hs_bindgen_384c8ceed3a4ca90_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_bespoke_ret2@
hs_bindgen_384c8ceed3a4ca90 :: IO (RIP.FunPtr (A -> IO HsBindgen.Runtime.LibC.CSize))
hs_bindgen_384c8ceed3a4ca90 =
  RIP.fromFFIType hs_bindgen_384c8ceed3a4ca90_base

{-# NOINLINE bespoke_ret2 #-}
{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h 98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2 :: RIP.FunPtr (A -> IO HsBindgen.Runtime.LibC.CSize)
bespoke_ret2 =
  RIP.unsafePerformIO hs_bindgen_384c8ceed3a4ca90

-- __unique:__ @test_macrosreparse_Example_get_arr_args1@
foreign import ccall unsafe "hs_bindgen_b38b526cf0817bf0" hs_bindgen_b38b526cf0817bf0_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_arr_args1@
hs_bindgen_b38b526cf0817bf0 :: IO (RIP.FunPtr ((IA.IncompleteArray A) -> IO ()))
hs_bindgen_b38b526cf0817bf0 =
  RIP.fromFFIType hs_bindgen_b38b526cf0817bf0_base

{-# NOINLINE arr_args1 #-}
{-| Arrays

__C declaration:__ @arr_args1@

__defined at:__ @macros\/reparse.h 104:6@

__exported by:__ @macros\/reparse.h@
-}
arr_args1 :: RIP.FunPtr ((IA.IncompleteArray A) -> IO ())
arr_args1 =
  RIP.unsafePerformIO hs_bindgen_b38b526cf0817bf0

-- __unique:__ @test_macrosreparse_Example_get_arr_args2@
foreign import ccall unsafe "hs_bindgen_2c0bca39b42ecbec" hs_bindgen_2c0bca39b42ecbec_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_arr_args2@
hs_bindgen_2c0bca39b42ecbec :: IO (RIP.FunPtr ((IA.IncompleteArray (RIP.Ptr A)) -> IO ()))
hs_bindgen_2c0bca39b42ecbec =
  RIP.fromFFIType hs_bindgen_2c0bca39b42ecbec_base

{-# NOINLINE arr_args2 #-}
{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h 105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2 :: RIP.FunPtr ((IA.IncompleteArray (RIP.Ptr A)) -> IO ())
arr_args2 =
  RIP.unsafePerformIO hs_bindgen_2c0bca39b42ecbec

-- __unique:__ @test_macrosreparse_Example_get_arr_args3@
foreign import ccall unsafe "hs_bindgen_82a8c15919ae3f33" hs_bindgen_82a8c15919ae3f33_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_arr_args3@
hs_bindgen_82a8c15919ae3f33 :: IO (RIP.FunPtr (((CA.ConstantArray 5) A) -> IO ()))
hs_bindgen_82a8c15919ae3f33 =
  RIP.fromFFIType hs_bindgen_82a8c15919ae3f33_base

{-# NOINLINE arr_args3 #-}
{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h 106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3 :: RIP.FunPtr (((CA.ConstantArray 5) A) -> IO ())
arr_args3 =
  RIP.unsafePerformIO hs_bindgen_82a8c15919ae3f33

-- __unique:__ @test_macrosreparse_Example_get_arr_args4@
foreign import ccall unsafe "hs_bindgen_db074918e0d38cb5" hs_bindgen_db074918e0d38cb5_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_arr_args4@
hs_bindgen_db074918e0d38cb5 :: IO (RIP.FunPtr (((CA.ConstantArray 5) (RIP.Ptr A)) -> IO ()))
hs_bindgen_db074918e0d38cb5 =
  RIP.fromFFIType hs_bindgen_db074918e0d38cb5_base

{-# NOINLINE arr_args4 #-}
{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h 107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4 :: RIP.FunPtr (((CA.ConstantArray 5) (RIP.Ptr A)) -> IO ())
arr_args4 =
  RIP.unsafePerformIO hs_bindgen_db074918e0d38cb5

-- __unique:__ @test_macrosreparse_Example_get_funptr_args1@
foreign import ccall unsafe "hs_bindgen_3fd6d1fbd060ffcf" hs_bindgen_3fd6d1fbd060ffcf_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_funptr_args1@
hs_bindgen_3fd6d1fbd060ffcf :: IO (RIP.FunPtr (A -> (RIP.FunPtr (IO ())) -> IO ()))
hs_bindgen_3fd6d1fbd060ffcf =
  RIP.fromFFIType hs_bindgen_3fd6d1fbd060ffcf_base

{-# NOINLINE funptr_args1 #-}
{-| Function pointers

__C declaration:__ @funptr_args1@

__defined at:__ @macros\/reparse.h 126:6@

__exported by:__ @macros\/reparse.h@
-}
funptr_args1 :: RIP.FunPtr (A -> (RIP.FunPtr (IO ())) -> IO ())
funptr_args1 =
  RIP.unsafePerformIO hs_bindgen_3fd6d1fbd060ffcf

-- __unique:__ @test_macrosreparse_Example_get_funptr_args2@
foreign import ccall unsafe "hs_bindgen_300abb7b10649797" hs_bindgen_300abb7b10649797_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_funptr_args2@
hs_bindgen_300abb7b10649797 :: IO (RIP.FunPtr (A -> (RIP.FunPtr (IO RIP.CInt)) -> IO ()))
hs_bindgen_300abb7b10649797 =
  RIP.fromFFIType hs_bindgen_300abb7b10649797_base

{-# NOINLINE funptr_args2 #-}
{-| __C declaration:__ @funptr_args2@

    __defined at:__ @macros\/reparse.h 127:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args2 :: RIP.FunPtr (A -> (RIP.FunPtr (IO RIP.CInt)) -> IO ())
funptr_args2 =
  RIP.unsafePerformIO hs_bindgen_300abb7b10649797

-- __unique:__ @test_macrosreparse_Example_get_funptr_args3@
foreign import ccall unsafe "hs_bindgen_78d109e353e05225" hs_bindgen_78d109e353e05225_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_funptr_args3@
hs_bindgen_78d109e353e05225 :: IO (RIP.FunPtr (A -> (RIP.FunPtr (RIP.CInt -> IO ())) -> IO ()))
hs_bindgen_78d109e353e05225 =
  RIP.fromFFIType hs_bindgen_78d109e353e05225_base

{-# NOINLINE funptr_args3 #-}
{-| __C declaration:__ @funptr_args3@

    __defined at:__ @macros\/reparse.h 128:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args3 :: RIP.FunPtr (A -> (RIP.FunPtr (RIP.CInt -> IO ())) -> IO ())
funptr_args3 =
  RIP.unsafePerformIO hs_bindgen_78d109e353e05225

-- __unique:__ @test_macrosreparse_Example_get_funptr_args4@
foreign import ccall unsafe "hs_bindgen_ee1815fa9e1c1131" hs_bindgen_ee1815fa9e1c1131_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_funptr_args4@
hs_bindgen_ee1815fa9e1c1131 :: IO (RIP.FunPtr (A -> (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO RIP.CChar)) -> IO ()))
hs_bindgen_ee1815fa9e1c1131 =
  RIP.fromFFIType hs_bindgen_ee1815fa9e1c1131_base

{-# NOINLINE funptr_args4 #-}
{-| __C declaration:__ @funptr_args4@

    __defined at:__ @macros\/reparse.h 129:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args4 :: RIP.FunPtr (A -> (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO RIP.CChar)) -> IO ())
funptr_args4 =
  RIP.unsafePerformIO hs_bindgen_ee1815fa9e1c1131

-- __unique:__ @test_macrosreparse_Example_get_funptr_args5@
foreign import ccall unsafe "hs_bindgen_146c4cb7cf7def11" hs_bindgen_146c4cb7cf7def11_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_funptr_args5@
hs_bindgen_146c4cb7cf7def11 :: IO (RIP.FunPtr (A -> (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt))) -> IO ()))
hs_bindgen_146c4cb7cf7def11 =
  RIP.fromFFIType hs_bindgen_146c4cb7cf7def11_base

{-# NOINLINE funptr_args5 #-}
{-| __C declaration:__ @funptr_args5@

    __defined at:__ @macros\/reparse.h 130:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args5 :: RIP.FunPtr (A -> (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt))) -> IO ())
funptr_args5 =
  RIP.unsafePerformIO hs_bindgen_146c4cb7cf7def11

-- __unique:__ @test_macrosreparse_Example_get_comments1@
foreign import ccall unsafe "hs_bindgen_7f647ffb81758d69" hs_bindgen_7f647ffb81758d69_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_comments1@
hs_bindgen_7f647ffb81758d69 :: IO (RIP.FunPtr (A -> IO ()))
hs_bindgen_7f647ffb81758d69 =
  RIP.fromFFIType hs_bindgen_7f647ffb81758d69_base

{-# NOINLINE comments1 #-}
{-| Comments in awkward places

  (Prior to language-c we failed to parse there.)

__C declaration:__ @comments1@

__defined at:__ @macros\/reparse.h 144:25@

__exported by:__ @macros\/reparse.h@
-}
comments1 :: RIP.FunPtr (A -> IO ())
comments1 =
  RIP.unsafePerformIO hs_bindgen_7f647ffb81758d69

-- __unique:__ @test_macrosreparse_Example_get_const_prim_before1@
foreign import ccall unsafe "hs_bindgen_ecf341da5dabe306" hs_bindgen_ecf341da5dabe306_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_prim_before1@
hs_bindgen_ecf341da5dabe306 :: IO (RIP.FunPtr (A -> RIP.CChar -> IO ()))
hs_bindgen_ecf341da5dabe306 =
  RIP.fromFFIType hs_bindgen_ecf341da5dabe306_base

{-# NOINLINE const_prim_before1 #-}
{-| `const` qualifier

  NOTE: These were not parsed correctly prior to the switch to language-c.

__C declaration:__ @const_prim_before1@

__defined at:__ @macros\/reparse.h 179:6@

__exported by:__ @macros\/reparse.h@
-}
const_prim_before1 :: RIP.FunPtr (A -> RIP.CChar -> IO ())
const_prim_before1 =
  RIP.unsafePerformIO hs_bindgen_ecf341da5dabe306

-- __unique:__ @test_macrosreparse_Example_get_const_prim_before2@
foreign import ccall unsafe "hs_bindgen_a0276bc4dac995c4" hs_bindgen_a0276bc4dac995c4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_prim_before2@
hs_bindgen_a0276bc4dac995c4 :: IO (RIP.FunPtr (A -> RIP.CSChar -> IO ()))
hs_bindgen_a0276bc4dac995c4 =
  RIP.fromFFIType hs_bindgen_a0276bc4dac995c4_base

{-# NOINLINE const_prim_before2 #-}
{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @macros\/reparse.h 180:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before2 :: RIP.FunPtr (A -> RIP.CSChar -> IO ())
const_prim_before2 =
  RIP.unsafePerformIO hs_bindgen_a0276bc4dac995c4

-- __unique:__ @test_macrosreparse_Example_get_const_prim_before3@
foreign import ccall unsafe "hs_bindgen_1555fc380f4c34ba" hs_bindgen_1555fc380f4c34ba_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_prim_before3@
hs_bindgen_1555fc380f4c34ba :: IO (RIP.FunPtr (A -> RIP.CUChar -> IO ()))
hs_bindgen_1555fc380f4c34ba =
  RIP.fromFFIType hs_bindgen_1555fc380f4c34ba_base

{-# NOINLINE const_prim_before3 #-}
{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @macros\/reparse.h 181:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before3 :: RIP.FunPtr (A -> RIP.CUChar -> IO ())
const_prim_before3 =
  RIP.unsafePerformIO hs_bindgen_1555fc380f4c34ba

-- __unique:__ @test_macrosreparse_Example_get_const_prim_after1@
foreign import ccall unsafe "hs_bindgen_e88405f039754f3c" hs_bindgen_e88405f039754f3c_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_prim_after1@
hs_bindgen_e88405f039754f3c :: IO (RIP.FunPtr (A -> RIP.CChar -> IO ()))
hs_bindgen_e88405f039754f3c =
  RIP.fromFFIType hs_bindgen_e88405f039754f3c_base

{-# NOINLINE const_prim_after1 #-}
{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @macros\/reparse.h 182:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after1 :: RIP.FunPtr (A -> RIP.CChar -> IO ())
const_prim_after1 =
  RIP.unsafePerformIO hs_bindgen_e88405f039754f3c

-- __unique:__ @test_macrosreparse_Example_get_const_prim_after2@
foreign import ccall unsafe "hs_bindgen_d267bc5333a36861" hs_bindgen_d267bc5333a36861_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_prim_after2@
hs_bindgen_d267bc5333a36861 :: IO (RIP.FunPtr (A -> RIP.CSChar -> IO ()))
hs_bindgen_d267bc5333a36861 =
  RIP.fromFFIType hs_bindgen_d267bc5333a36861_base

{-# NOINLINE const_prim_after2 #-}
{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @macros\/reparse.h 183:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after2 :: RIP.FunPtr (A -> RIP.CSChar -> IO ())
const_prim_after2 =
  RIP.unsafePerformIO hs_bindgen_d267bc5333a36861

-- __unique:__ @test_macrosreparse_Example_get_const_prim_after3@
foreign import ccall unsafe "hs_bindgen_0e08cac4148d54cd" hs_bindgen_0e08cac4148d54cd_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_prim_after3@
hs_bindgen_0e08cac4148d54cd :: IO (RIP.FunPtr (A -> RIP.CUChar -> IO ()))
hs_bindgen_0e08cac4148d54cd =
  RIP.fromFFIType hs_bindgen_0e08cac4148d54cd_base

{-# NOINLINE const_prim_after3 #-}
{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @macros\/reparse.h 184:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after3 :: RIP.FunPtr (A -> RIP.CUChar -> IO ())
const_prim_after3 =
  RIP.unsafePerformIO hs_bindgen_0e08cac4148d54cd

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before1@
foreign import ccall unsafe "hs_bindgen_d7140c03594ba60a" hs_bindgen_d7140c03594ba60a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before1@
hs_bindgen_d7140c03594ba60a :: IO (RIP.FunPtr (A -> RIP.CFloat -> IO ()))
hs_bindgen_d7140c03594ba60a =
  RIP.fromFFIType hs_bindgen_d7140c03594ba60a_base

{-# NOINLINE const_withoutSign_before1 #-}
{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @macros\/reparse.h 188:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before1 :: RIP.FunPtr (A -> RIP.CFloat -> IO ())
const_withoutSign_before1 =
  RIP.unsafePerformIO hs_bindgen_d7140c03594ba60a

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before2@
foreign import ccall unsafe "hs_bindgen_92134abe381cef04" hs_bindgen_92134abe381cef04_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before2@
hs_bindgen_92134abe381cef04 :: IO (RIP.FunPtr (A -> RIP.CDouble -> IO ()))
hs_bindgen_92134abe381cef04 =
  RIP.fromFFIType hs_bindgen_92134abe381cef04_base

{-# NOINLINE const_withoutSign_before2 #-}
{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @macros\/reparse.h 189:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before2 :: RIP.FunPtr (A -> RIP.CDouble -> IO ())
const_withoutSign_before2 =
  RIP.unsafePerformIO hs_bindgen_92134abe381cef04

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before3@
foreign import ccall unsafe "hs_bindgen_b80a2c12f1bdb050" hs_bindgen_b80a2c12f1bdb050_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before3@
hs_bindgen_b80a2c12f1bdb050 :: IO (RIP.FunPtr (A -> RIP.CBool -> IO ()))
hs_bindgen_b80a2c12f1bdb050 =
  RIP.fromFFIType hs_bindgen_b80a2c12f1bdb050_base

{-# NOINLINE const_withoutSign_before3 #-}
{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @macros\/reparse.h 190:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before3 :: RIP.FunPtr (A -> RIP.CBool -> IO ())
const_withoutSign_before3 =
  RIP.unsafePerformIO hs_bindgen_b80a2c12f1bdb050

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before4@
foreign import ccall unsafe "hs_bindgen_aa0ffa330554de35" hs_bindgen_aa0ffa330554de35_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before4@
hs_bindgen_aa0ffa330554de35 :: IO (RIP.FunPtr (A -> Some_struct -> IO ()))
hs_bindgen_aa0ffa330554de35 =
  RIP.fromFFIType hs_bindgen_aa0ffa330554de35_base

{-# NOINLINE const_withoutSign_before4 #-}
{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @macros\/reparse.h 191:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before4 :: RIP.FunPtr (A -> Some_struct -> IO ())
const_withoutSign_before4 =
  RIP.unsafePerformIO hs_bindgen_aa0ffa330554de35

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before5@
foreign import ccall unsafe "hs_bindgen_f59abb8a7dffe11f" hs_bindgen_f59abb8a7dffe11f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before5@
hs_bindgen_f59abb8a7dffe11f :: IO (RIP.FunPtr (A -> Some_union -> IO ()))
hs_bindgen_f59abb8a7dffe11f =
  RIP.fromFFIType hs_bindgen_f59abb8a7dffe11f_base

{-# NOINLINE const_withoutSign_before5 #-}
{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @macros\/reparse.h 192:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before5 :: RIP.FunPtr (A -> Some_union -> IO ())
const_withoutSign_before5 =
  RIP.unsafePerformIO hs_bindgen_f59abb8a7dffe11f

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before6@
foreign import ccall unsafe "hs_bindgen_0df28ff2ac7fa1e0" hs_bindgen_0df28ff2ac7fa1e0_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before6@
hs_bindgen_0df28ff2ac7fa1e0 :: IO (RIP.FunPtr (A -> Some_enum -> IO ()))
hs_bindgen_0df28ff2ac7fa1e0 =
  RIP.fromFFIType hs_bindgen_0df28ff2ac7fa1e0_base

{-# NOINLINE const_withoutSign_before6 #-}
{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @macros\/reparse.h 193:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before6 :: RIP.FunPtr (A -> Some_enum -> IO ())
const_withoutSign_before6 =
  RIP.unsafePerformIO hs_bindgen_0df28ff2ac7fa1e0

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before7@
foreign import ccall unsafe "hs_bindgen_fa8c89b169233a6a" hs_bindgen_fa8c89b169233a6a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before7@
hs_bindgen_fa8c89b169233a6a :: IO (RIP.FunPtr (A -> RIP.CBool -> IO ()))
hs_bindgen_fa8c89b169233a6a =
  RIP.fromFFIType hs_bindgen_fa8c89b169233a6a_base

{-# NOINLINE const_withoutSign_before7 #-}
{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @macros\/reparse.h 194:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before7 :: RIP.FunPtr (A -> RIP.CBool -> IO ())
const_withoutSign_before7 =
  RIP.unsafePerformIO hs_bindgen_fa8c89b169233a6a

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before8@
foreign import ccall unsafe "hs_bindgen_e707228d3913a299" hs_bindgen_e707228d3913a299_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_before8@
hs_bindgen_e707228d3913a299 :: IO (RIP.FunPtr (A -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_e707228d3913a299 =
  RIP.fromFFIType hs_bindgen_e707228d3913a299_base

{-# NOINLINE const_withoutSign_before8 #-}
{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @macros\/reparse.h 195:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before8 :: RIP.FunPtr (A -> HsBindgen.Runtime.LibC.CSize -> IO ())
const_withoutSign_before8 =
  RIP.unsafePerformIO hs_bindgen_e707228d3913a299

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after1@
foreign import ccall unsafe "hs_bindgen_7e7703a610cd6783" hs_bindgen_7e7703a610cd6783_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after1@
hs_bindgen_7e7703a610cd6783 :: IO (RIP.FunPtr (A -> RIP.CFloat -> IO ()))
hs_bindgen_7e7703a610cd6783 =
  RIP.fromFFIType hs_bindgen_7e7703a610cd6783_base

{-# NOINLINE const_withoutSign_after1 #-}
{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @macros\/reparse.h 197:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after1 :: RIP.FunPtr (A -> RIP.CFloat -> IO ())
const_withoutSign_after1 =
  RIP.unsafePerformIO hs_bindgen_7e7703a610cd6783

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after2@
foreign import ccall unsafe "hs_bindgen_988863a241cb28f2" hs_bindgen_988863a241cb28f2_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after2@
hs_bindgen_988863a241cb28f2 :: IO (RIP.FunPtr (A -> RIP.CDouble -> IO ()))
hs_bindgen_988863a241cb28f2 =
  RIP.fromFFIType hs_bindgen_988863a241cb28f2_base

{-# NOINLINE const_withoutSign_after2 #-}
{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @macros\/reparse.h 198:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after2 :: RIP.FunPtr (A -> RIP.CDouble -> IO ())
const_withoutSign_after2 =
  RIP.unsafePerformIO hs_bindgen_988863a241cb28f2

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after3@
foreign import ccall unsafe "hs_bindgen_c8182ee9a3467005" hs_bindgen_c8182ee9a3467005_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after3@
hs_bindgen_c8182ee9a3467005 :: IO (RIP.FunPtr (A -> RIP.CBool -> IO ()))
hs_bindgen_c8182ee9a3467005 =
  RIP.fromFFIType hs_bindgen_c8182ee9a3467005_base

{-# NOINLINE const_withoutSign_after3 #-}
{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @macros\/reparse.h 199:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after3 :: RIP.FunPtr (A -> RIP.CBool -> IO ())
const_withoutSign_after3 =
  RIP.unsafePerformIO hs_bindgen_c8182ee9a3467005

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after4@
foreign import ccall unsafe "hs_bindgen_de1e4bbac808bcbc" hs_bindgen_de1e4bbac808bcbc_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after4@
hs_bindgen_de1e4bbac808bcbc :: IO (RIP.FunPtr (A -> Some_struct -> IO ()))
hs_bindgen_de1e4bbac808bcbc =
  RIP.fromFFIType hs_bindgen_de1e4bbac808bcbc_base

{-# NOINLINE const_withoutSign_after4 #-}
{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @macros\/reparse.h 200:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after4 :: RIP.FunPtr (A -> Some_struct -> IO ())
const_withoutSign_after4 =
  RIP.unsafePerformIO hs_bindgen_de1e4bbac808bcbc

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after5@
foreign import ccall unsafe "hs_bindgen_746fb03c27f16031" hs_bindgen_746fb03c27f16031_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after5@
hs_bindgen_746fb03c27f16031 :: IO (RIP.FunPtr (A -> Some_union -> IO ()))
hs_bindgen_746fb03c27f16031 =
  RIP.fromFFIType hs_bindgen_746fb03c27f16031_base

{-# NOINLINE const_withoutSign_after5 #-}
{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @macros\/reparse.h 201:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after5 :: RIP.FunPtr (A -> Some_union -> IO ())
const_withoutSign_after5 =
  RIP.unsafePerformIO hs_bindgen_746fb03c27f16031

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after6@
foreign import ccall unsafe "hs_bindgen_dc174ac598e92a95" hs_bindgen_dc174ac598e92a95_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after6@
hs_bindgen_dc174ac598e92a95 :: IO (RIP.FunPtr (A -> Some_enum -> IO ()))
hs_bindgen_dc174ac598e92a95 =
  RIP.fromFFIType hs_bindgen_dc174ac598e92a95_base

{-# NOINLINE const_withoutSign_after6 #-}
{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @macros\/reparse.h 202:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after6 :: RIP.FunPtr (A -> Some_enum -> IO ())
const_withoutSign_after6 =
  RIP.unsafePerformIO hs_bindgen_dc174ac598e92a95

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after7@
foreign import ccall unsafe "hs_bindgen_2c8ddc46cb100dd9" hs_bindgen_2c8ddc46cb100dd9_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after7@
hs_bindgen_2c8ddc46cb100dd9 :: IO (RIP.FunPtr (A -> RIP.CBool -> IO ()))
hs_bindgen_2c8ddc46cb100dd9 =
  RIP.fromFFIType hs_bindgen_2c8ddc46cb100dd9_base

{-# NOINLINE const_withoutSign_after7 #-}
{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @macros\/reparse.h 203:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after7 :: RIP.FunPtr (A -> RIP.CBool -> IO ())
const_withoutSign_after7 =
  RIP.unsafePerformIO hs_bindgen_2c8ddc46cb100dd9

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after8@
foreign import ccall unsafe "hs_bindgen_fdf87fbad31246de" hs_bindgen_fdf87fbad31246de_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_withoutSign_after8@
hs_bindgen_fdf87fbad31246de :: IO (RIP.FunPtr (A -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_fdf87fbad31246de =
  RIP.fromFFIType hs_bindgen_fdf87fbad31246de_base

{-# NOINLINE const_withoutSign_after8 #-}
{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @macros\/reparse.h 204:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after8 :: RIP.FunPtr (A -> HsBindgen.Runtime.LibC.CSize -> IO ())
const_withoutSign_after8 =
  RIP.unsafePerformIO hs_bindgen_fdf87fbad31246de

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_args1@
foreign import ccall unsafe "hs_bindgen_38808e8aa92d3bcb" hs_bindgen_38808e8aa92d3bcb_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_args1@
hs_bindgen_38808e8aa92d3bcb :: IO (RIP.FunPtr (A -> (PtrConst.PtrConst RIP.CInt) -> IO ()))
hs_bindgen_38808e8aa92d3bcb =
  RIP.fromFFIType hs_bindgen_38808e8aa92d3bcb_base

{-# NOINLINE const_pointers_args1 #-}
{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @macros\/reparse.h 208:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args1 :: RIP.FunPtr (A -> (PtrConst.PtrConst RIP.CInt) -> IO ())
const_pointers_args1 =
  RIP.unsafePerformIO hs_bindgen_38808e8aa92d3bcb

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_args2@
foreign import ccall unsafe "hs_bindgen_4d37887fd82ab559" hs_bindgen_4d37887fd82ab559_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_args2@
hs_bindgen_4d37887fd82ab559 :: IO (RIP.FunPtr (A -> (PtrConst.PtrConst RIP.CInt) -> IO ()))
hs_bindgen_4d37887fd82ab559 =
  RIP.fromFFIType hs_bindgen_4d37887fd82ab559_base

{-# NOINLINE const_pointers_args2 #-}
{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @macros\/reparse.h 209:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args2 :: RIP.FunPtr (A -> (PtrConst.PtrConst RIP.CInt) -> IO ())
const_pointers_args2 =
  RIP.unsafePerformIO hs_bindgen_4d37887fd82ab559

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_args3@
foreign import ccall unsafe "hs_bindgen_fd675a45f1682dd5" hs_bindgen_fd675a45f1682dd5_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_args3@
hs_bindgen_fd675a45f1682dd5 :: IO (RIP.FunPtr (A -> (RIP.Ptr RIP.CInt) -> IO ()))
hs_bindgen_fd675a45f1682dd5 =
  RIP.fromFFIType hs_bindgen_fd675a45f1682dd5_base

{-# NOINLINE const_pointers_args3 #-}
{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @macros\/reparse.h 210:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args3 :: RIP.FunPtr (A -> (RIP.Ptr RIP.CInt) -> IO ())
const_pointers_args3 =
  RIP.unsafePerformIO hs_bindgen_fd675a45f1682dd5

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_args4@
foreign import ccall unsafe "hs_bindgen_824cb2084c63b803" hs_bindgen_824cb2084c63b803_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_args4@
hs_bindgen_824cb2084c63b803 :: IO (RIP.FunPtr (A -> (PtrConst.PtrConst RIP.CInt) -> IO ()))
hs_bindgen_824cb2084c63b803 =
  RIP.fromFFIType hs_bindgen_824cb2084c63b803_base

{-# NOINLINE const_pointers_args4 #-}
{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @macros\/reparse.h 211:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args4 :: RIP.FunPtr (A -> (PtrConst.PtrConst RIP.CInt) -> IO ())
const_pointers_args4 =
  RIP.unsafePerformIO hs_bindgen_824cb2084c63b803

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_args5@
foreign import ccall unsafe "hs_bindgen_25bdd13aed8f26c4" hs_bindgen_25bdd13aed8f26c4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_args5@
hs_bindgen_25bdd13aed8f26c4 :: IO (RIP.FunPtr (A -> (PtrConst.PtrConst RIP.CInt) -> IO ()))
hs_bindgen_25bdd13aed8f26c4 =
  RIP.fromFFIType hs_bindgen_25bdd13aed8f26c4_base

{-# NOINLINE const_pointers_args5 #-}
{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @macros\/reparse.h 212:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args5 :: RIP.FunPtr (A -> (PtrConst.PtrConst RIP.CInt) -> IO ())
const_pointers_args5 =
  RIP.unsafePerformIO hs_bindgen_25bdd13aed8f26c4

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_ret1@
foreign import ccall unsafe "hs_bindgen_f494124b53592961" hs_bindgen_f494124b53592961_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_ret1@
hs_bindgen_f494124b53592961 :: IO (RIP.FunPtr (A -> IO (PtrConst.PtrConst RIP.CInt)))
hs_bindgen_f494124b53592961 =
  RIP.fromFFIType hs_bindgen_f494124b53592961_base

{-# NOINLINE const_pointers_ret1 #-}
{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h 214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1 :: RIP.FunPtr (A -> IO (PtrConst.PtrConst RIP.CInt))
const_pointers_ret1 =
  RIP.unsafePerformIO hs_bindgen_f494124b53592961

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_ret2@
foreign import ccall unsafe "hs_bindgen_32c4b858ef4f3bb8" hs_bindgen_32c4b858ef4f3bb8_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_ret2@
hs_bindgen_32c4b858ef4f3bb8 :: IO (RIP.FunPtr (A -> IO (PtrConst.PtrConst RIP.CInt)))
hs_bindgen_32c4b858ef4f3bb8 =
  RIP.fromFFIType hs_bindgen_32c4b858ef4f3bb8_base

{-# NOINLINE const_pointers_ret2 #-}
{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h 215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2 :: RIP.FunPtr (A -> IO (PtrConst.PtrConst RIP.CInt))
const_pointers_ret2 =
  RIP.unsafePerformIO hs_bindgen_32c4b858ef4f3bb8

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_ret3@
foreign import ccall unsafe "hs_bindgen_110afe05c6d47b7c" hs_bindgen_110afe05c6d47b7c_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_ret3@
hs_bindgen_110afe05c6d47b7c :: IO (RIP.FunPtr (A -> IO (RIP.Ptr RIP.CInt)))
hs_bindgen_110afe05c6d47b7c =
  RIP.fromFFIType hs_bindgen_110afe05c6d47b7c_base

{-# NOINLINE const_pointers_ret3 #-}
{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h 216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3 :: RIP.FunPtr (A -> IO (RIP.Ptr RIP.CInt))
const_pointers_ret3 =
  RIP.unsafePerformIO hs_bindgen_110afe05c6d47b7c

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_ret4@
foreign import ccall unsafe "hs_bindgen_042a426acfeaa051" hs_bindgen_042a426acfeaa051_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_ret4@
hs_bindgen_042a426acfeaa051 :: IO (RIP.FunPtr (A -> IO (PtrConst.PtrConst RIP.CInt)))
hs_bindgen_042a426acfeaa051 =
  RIP.fromFFIType hs_bindgen_042a426acfeaa051_base

{-# NOINLINE const_pointers_ret4 #-}
{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h 217:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4 :: RIP.FunPtr (A -> IO (PtrConst.PtrConst RIP.CInt))
const_pointers_ret4 =
  RIP.unsafePerformIO hs_bindgen_042a426acfeaa051

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_ret5@
foreign import ccall unsafe "hs_bindgen_bc3cdc03ae3274bd" hs_bindgen_bc3cdc03ae3274bd_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_pointers_ret5@
hs_bindgen_bc3cdc03ae3274bd :: IO (RIP.FunPtr (A -> IO (PtrConst.PtrConst RIP.CInt)))
hs_bindgen_bc3cdc03ae3274bd =
  RIP.fromFFIType hs_bindgen_bc3cdc03ae3274bd_base

{-# NOINLINE const_pointers_ret5 #-}
{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h 218:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5 :: RIP.FunPtr (A -> IO (PtrConst.PtrConst RIP.CInt))
const_pointers_ret5 =
  RIP.unsafePerformIO hs_bindgen_bc3cdc03ae3274bd

-- __unique:__ @test_macrosreparse_Example_get_const_array_elem1@
foreign import ccall unsafe "hs_bindgen_ae95c15a81eacd52" hs_bindgen_ae95c15a81eacd52_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_array_elem1@
hs_bindgen_ae95c15a81eacd52 :: IO (RIP.FunPtr ((IA.IncompleteArray A) -> IO ()))
hs_bindgen_ae95c15a81eacd52 =
  RIP.fromFFIType hs_bindgen_ae95c15a81eacd52_base

{-# NOINLINE const_array_elem1 #-}
{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h 246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 :: RIP.FunPtr ((IA.IncompleteArray A) -> IO ())
const_array_elem1 =
  RIP.unsafePerformIO hs_bindgen_ae95c15a81eacd52

-- __unique:__ @test_macrosreparse_Example_get_const_array_elem2@
foreign import ccall unsafe "hs_bindgen_9dbbae3758752935" hs_bindgen_9dbbae3758752935_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_array_elem2@
hs_bindgen_9dbbae3758752935 :: IO (RIP.FunPtr ((IA.IncompleteArray (PtrConst.PtrConst A)) -> IO ()))
hs_bindgen_9dbbae3758752935 =
  RIP.fromFFIType hs_bindgen_9dbbae3758752935_base

{-# NOINLINE const_array_elem2 #-}
{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h 247:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2 :: RIP.FunPtr ((IA.IncompleteArray (PtrConst.PtrConst A)) -> IO ())
const_array_elem2 =
  RIP.unsafePerformIO hs_bindgen_9dbbae3758752935

-- __unique:__ @test_macrosreparse_Example_get_const_array_elem3@
foreign import ccall unsafe "hs_bindgen_9fdb904f0c0776ed" hs_bindgen_9fdb904f0c0776ed_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const_array_elem3@
hs_bindgen_9fdb904f0c0776ed :: IO (RIP.FunPtr ((IA.IncompleteArray (RIP.Ptr A)) -> IO ()))
hs_bindgen_9fdb904f0c0776ed =
  RIP.fromFFIType hs_bindgen_9fdb904f0c0776ed_base

{-# NOINLINE const_array_elem3 #-}
{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h 248:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 :: RIP.FunPtr ((IA.IncompleteArray (RIP.Ptr A)) -> IO ())
const_array_elem3 =
  RIP.unsafePerformIO hs_bindgen_9fdb904f0c0776ed

-- __unique:__ @test_macrosreparse_Example_get_noParams1@
foreign import ccall unsafe "hs_bindgen_cd9806a214acd0fa" hs_bindgen_cd9806a214acd0fa_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_noParams1@
hs_bindgen_cd9806a214acd0fa :: IO (RIP.FunPtr (IO A))
hs_bindgen_cd9806a214acd0fa =
  RIP.fromFFIType hs_bindgen_cd9806a214acd0fa_base

{-# NOINLINE noParams1 #-}
{-| Other examples we reparsed /incorrectly/ before language-c

__C declaration:__ @noParams1@

__defined at:__ @macros\/reparse.h 256:3@

__exported by:__ @macros\/reparse.h@
-}
noParams1 :: RIP.FunPtr (IO A)
noParams1 =
  RIP.unsafePerformIO hs_bindgen_cd9806a214acd0fa

-- __unique:__ @test_macrosreparse_Example_get_noParams2@
foreign import ccall unsafe "hs_bindgen_7c7bf9b5a41ea4a9" hs_bindgen_7c7bf9b5a41ea4a9_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_noParams2@
hs_bindgen_7c7bf9b5a41ea4a9 :: IO (RIP.FunPtr (IO A))
hs_bindgen_7c7bf9b5a41ea4a9 =
  RIP.fromFFIType hs_bindgen_7c7bf9b5a41ea4a9_base

{-# NOINLINE noParams2 #-}
{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h 257:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2 :: RIP.FunPtr (IO A)
noParams2 =
  RIP.unsafePerformIO hs_bindgen_7c7bf9b5a41ea4a9

-- __unique:__ @test_macrosreparse_Example_get_noParams3@
foreign import ccall unsafe "hs_bindgen_7f154653725d104d" hs_bindgen_7f154653725d104d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_noParams3@
hs_bindgen_7f154653725d104d :: IO (RIP.FunPtr (A -> (RIP.FunPtr (IO RIP.CInt)) -> IO ()))
hs_bindgen_7f154653725d104d =
  RIP.fromFFIType hs_bindgen_7f154653725d104d_base

{-# NOINLINE noParams3 #-}
{-| __C declaration:__ @noParams3@

    __defined at:__ @macros\/reparse.h 258:6@

    __exported by:__ @macros\/reparse.h@
-}
noParams3 :: RIP.FunPtr (A -> (RIP.FunPtr (IO RIP.CInt)) -> IO ())
noParams3 =
  RIP.unsafePerformIO hs_bindgen_7f154653725d104d

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret1@
foreign import ccall unsafe "hs_bindgen_4460dd1d93c2df6f" hs_bindgen_4460dd1d93c2df6f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret1@
hs_bindgen_4460dd1d93c2df6f :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (IO ()))))
hs_bindgen_4460dd1d93c2df6f =
  RIP.fromFFIType hs_bindgen_4460dd1d93c2df6f_base

{-# NOINLINE funptr_ret1 #-}
{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h 262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1 :: RIP.FunPtr (A -> IO (RIP.FunPtr (IO ())))
funptr_ret1 =
  RIP.unsafePerformIO hs_bindgen_4460dd1d93c2df6f

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret2@
foreign import ccall unsafe "hs_bindgen_2b8d04e0fe71178b" hs_bindgen_2b8d04e0fe71178b_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret2@
hs_bindgen_2b8d04e0fe71178b :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (IO RIP.CInt))))
hs_bindgen_2b8d04e0fe71178b =
  RIP.fromFFIType hs_bindgen_2b8d04e0fe71178b_base

{-# NOINLINE funptr_ret2 #-}
{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h 263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2 :: RIP.FunPtr (A -> IO (RIP.FunPtr (IO RIP.CInt)))
funptr_ret2 =
  RIP.unsafePerformIO hs_bindgen_2b8d04e0fe71178b

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret3@
foreign import ccall unsafe "hs_bindgen_27d05f98fe1e869b" hs_bindgen_27d05f98fe1e869b_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret3@
hs_bindgen_27d05f98fe1e869b :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> IO ()))))
hs_bindgen_27d05f98fe1e869b =
  RIP.fromFFIType hs_bindgen_27d05f98fe1e869b_base

{-# NOINLINE funptr_ret3 #-}
{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h 264:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3 :: RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> IO ())))
funptr_ret3 =
  RIP.unsafePerformIO hs_bindgen_27d05f98fe1e869b

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret4@
foreign import ccall unsafe "hs_bindgen_f55fb889881240c6" hs_bindgen_f55fb889881240c6_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret4@
hs_bindgen_f55fb889881240c6 :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO RIP.CChar))))
hs_bindgen_f55fb889881240c6 =
  RIP.fromFFIType hs_bindgen_f55fb889881240c6_base

{-# NOINLINE funptr_ret4 #-}
{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h 265:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4 :: RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO RIP.CChar)))
funptr_ret4 =
  RIP.unsafePerformIO hs_bindgen_f55fb889881240c6

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret5@
foreign import ccall unsafe "hs_bindgen_e59cbfc52cec7177" hs_bindgen_e59cbfc52cec7177_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret5@
hs_bindgen_e59cbfc52cec7177 :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt)))))
hs_bindgen_e59cbfc52cec7177 =
  RIP.fromFFIType hs_bindgen_e59cbfc52cec7177_base

{-# NOINLINE funptr_ret5 #-}
{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h 269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5 :: RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt))))
funptr_ret5 =
  RIP.unsafePerformIO hs_bindgen_e59cbfc52cec7177

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret6@
foreign import ccall unsafe "hs_bindgen_f94486b884c7cd44" hs_bindgen_f94486b884c7cd44_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret6@
hs_bindgen_f94486b884c7cd44 :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))))
hs_bindgen_f94486b884c7cd44 =
  RIP.fromFFIType hs_bindgen_f94486b884c7cd44_base

{-# NOINLINE funptr_ret6 #-}
{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h 270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6 :: RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt))))
funptr_ret6 =
  RIP.unsafePerformIO hs_bindgen_f94486b884c7cd44

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret7@
foreign import ccall unsafe "hs_bindgen_7810d75b42b3bed8" hs_bindgen_7810d75b42b3bed8_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret7@
hs_bindgen_7810d75b42b3bed8 :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))))
hs_bindgen_7810d75b42b3bed8 =
  RIP.fromFFIType hs_bindgen_7810d75b42b3bed8_base

{-# NOINLINE funptr_ret7 #-}
{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h 271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7 :: RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt))))
funptr_ret7 =
  RIP.unsafePerformIO hs_bindgen_7810d75b42b3bed8

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret8@
foreign import ccall unsafe "hs_bindgen_fc12014317abd6b4" hs_bindgen_fc12014317abd6b4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret8@
hs_bindgen_fc12014317abd6b4 :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt)))))
hs_bindgen_fc12014317abd6b4 =
  RIP.fromFFIType hs_bindgen_fc12014317abd6b4_base

{-# NOINLINE funptr_ret8 #-}
{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h 272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8 :: RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt))))
funptr_ret8 =
  RIP.unsafePerformIO hs_bindgen_fc12014317abd6b4

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret9@
foreign import ccall unsafe "hs_bindgen_cf3bf1d8470acad4" hs_bindgen_cf3bf1d8470acad4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret9@
hs_bindgen_cf3bf1d8470acad4 :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))))
hs_bindgen_cf3bf1d8470acad4 =
  RIP.fromFFIType hs_bindgen_cf3bf1d8470acad4_base

{-# NOINLINE funptr_ret9 #-}
{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h 273:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9 :: RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt))))
funptr_ret9 =
  RIP.unsafePerformIO hs_bindgen_cf3bf1d8470acad4

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret10@
foreign import ccall unsafe "hs_bindgen_44a7eb47cf87f092" hs_bindgen_44a7eb47cf87f092_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_funptr_ret10@
hs_bindgen_44a7eb47cf87f092 :: IO (RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))))
hs_bindgen_44a7eb47cf87f092 =
  RIP.fromFFIType hs_bindgen_44a7eb47cf87f092_base

{-# NOINLINE funptr_ret10 #-}
{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h 274:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10 :: RIP.FunPtr (A -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt))))
funptr_ret10 =
  RIP.unsafePerformIO hs_bindgen_44a7eb47cf87f092
