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
  , "/* Example_get_args_char1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_9bb786fc1eadf4a4 (void)) ("
  , "  A arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &args_char1;"
  , "}"
  , "/* Example_get_args_char2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_e4ee629f7fc67518 (void)) ("
  , "  A arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  return &args_char2;"
  , "}"
  , "/* Example_get_args_char3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_e6569643d0024113 (void)) ("
  , "  A arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  return &args_char3;"
  , "}"
  , "/* Example_get_args_short1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_1d345791897756aa (void)) ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short1;"
  , "}"
  , "/* Example_get_args_short2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_d27e38c34d2cb75c (void)) ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short2;"
  , "}"
  , "/* Example_get_args_short3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_4e55c5eecff17d32 (void)) ("
  , "  A arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  return &args_short3;"
  , "}"
  , "/* Example_get_args_int1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_f934c015e386eade (void)) ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int1;"
  , "}"
  , "/* Example_get_args_int2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_cddb8e9ceeedf1a2 (void)) ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int2;"
  , "}"
  , "/* Example_get_args_int3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_20a8b568bed432f6 (void)) ("
  , "  A arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  return &args_int3;"
  , "}"
  , "/* Example_get_args_long1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_2dfd2b59c0da0082 (void)) ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long1;"
  , "}"
  , "/* Example_get_args_long2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_771b1579cbb95648 (void)) ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long2;"
  , "}"
  , "/* Example_get_args_long3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_26eaae8b45f2d0c3 (void)) ("
  , "  A arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  return &args_long3;"
  , "}"
  , "/* Example_get_args_float_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_a92e81c68b785b28 (void)) ("
  , "  A arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  return &args_float;"
  , "}"
  , "/* Example_get_args_double_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_d10652d4a85ed811 (void)) ("
  , "  A arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &args_double;"
  , "}"
  , "/* Example_get_args_bool1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_452116ebff72c142 (void)) ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &args_bool1;"
  , "}"
  , "/* Example_get_args_struct_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_de4ae6525b4cf7ab (void)) ("
  , "  A arg1,"
  , "  struct some_struct arg2"
  , ")"
  , "{"
  , "  return &args_struct;"
  , "}"
  , "/* Example_get_args_union_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_e55e33051e707111 (void)) ("
  , "  A arg1,"
  , "  union some_union arg2"
  , ")"
  , "{"
  , "  return &args_union;"
  , "}"
  , "/* Example_get_args_enum_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_4712c7f26562021a (void)) ("
  , "  A arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  return &args_enum;"
  , "}"
  , "/* Example_get_args_pointer1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_5d9b782f640824e7 (void)) ("
  , "  A arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return &args_pointer1;"
  , "}"
  , "/* Example_get_args_pointer2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_cfd19552b2176dfc (void)) ("
  , "  A arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  return &args_pointer2;"
  , "}"
  , "/* Example_get_args_pointer3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_26d448a288ca759f (void)) ("
  , "  A arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  return &args_pointer3;"
  , "}"
  , "/* Example_get_ret_A_ptr */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_test_macrosreparse_b9547cae9e047b1f (void)) (void)"
  , "{"
  , "  return &ret_A;"
  , "}"
  , "/* Example_get_ret_char1_ptr */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_test_macrosreparse_9a1302071a2a8cce (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char1;"
  , "}"
  , "/* Example_get_ret_char2_ptr */"
  , "__attribute__ ((const))"
  , "signed char (*hs_bindgen_test_macrosreparse_cd01539317a8a663 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char2;"
  , "}"
  , "/* Example_get_ret_char3_ptr */"
  , "__attribute__ ((const))"
  , "unsigned char (*hs_bindgen_test_macrosreparse_b8da47545f19e06f (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char3;"
  , "}"
  , "/* Example_get_ret_short1_ptr */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_test_macrosreparse_c1b7895b48ce432d (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short1;"
  , "}"
  , "/* Example_get_ret_short2_ptr */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_test_macrosreparse_a601ca8574ff50e6 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short2;"
  , "}"
  , "/* Example_get_ret_short3_ptr */"
  , "__attribute__ ((const))"
  , "unsigned short (*hs_bindgen_test_macrosreparse_ff947411a019ff3c (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short3;"
  , "}"
  , "/* Example_get_ret_int1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_macrosreparse_7f88eef51e532ea7 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int1;"
  , "}"
  , "/* Example_get_ret_int2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_macrosreparse_4588db9e8f9556b8 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int2;"
  , "}"
  , "/* Example_get_ret_int3_ptr */"
  , "__attribute__ ((const))"
  , "unsigned int (*hs_bindgen_test_macrosreparse_17507f4313b4622f (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int3;"
  , "}"
  , "/* Example_get_ret_long1_ptr */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_test_macrosreparse_18d2aee117ef3357 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long1;"
  , "}"
  , "/* Example_get_ret_long2_ptr */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_test_macrosreparse_b3d75b43adf369ff (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long2;"
  , "}"
  , "/* Example_get_ret_long3_ptr */"
  , "__attribute__ ((const))"
  , "unsigned long (*hs_bindgen_test_macrosreparse_4d45e5ea9c7dc6d1 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long3;"
  , "}"
  , "/* Example_get_ret_float_ptr */"
  , "__attribute__ ((const))"
  , "float (*hs_bindgen_test_macrosreparse_8936a8d4587c3d2a (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_float;"
  , "}"
  , "/* Example_get_ret_double_ptr */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_test_macrosreparse_587a1106df194fac (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_double;"
  , "}"
  , "/* Example_get_ret_bool1_ptr */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_test_macrosreparse_a2e76d906e895d52 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_bool1;"
  , "}"
  , "/* Example_get_ret_struct_ptr */"
  , "__attribute__ ((const))"
  , "struct some_struct (*hs_bindgen_test_macrosreparse_cd5235d2ab3b01e6 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_struct;"
  , "}"
  , "/* Example_get_ret_union_ptr */"
  , "__attribute__ ((const))"
  , "union some_union (*hs_bindgen_test_macrosreparse_2c2965c809dc1eaf (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_union;"
  , "}"
  , "/* Example_get_ret_enum_ptr */"
  , "__attribute__ ((const))"
  , "enum some_enum (*hs_bindgen_test_macrosreparse_4b63686f9884c0b6 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_enum;"
  , "}"
  , "/* Example_get_ret_pointer1_ptr */"
  , "__attribute__ ((const))"
  , "signed int *(*hs_bindgen_test_macrosreparse_2cdd00bac75fbfcc (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer1;"
  , "}"
  , "/* Example_get_ret_pointer2_ptr */"
  , "__attribute__ ((const))"
  , "signed int **(*hs_bindgen_test_macrosreparse_9bb32d52132f4fea (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer2;"
  , "}"
  , "/* Example_get_ret_pointer3_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_test_macrosreparse_30356ad0d31ec0c7 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer3;"
  , "}"
  , "/* Example_get_body1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_macrosreparse_ccd217a873c281d2 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &body1;"
  , "}"
  , "/* Example_get_body2_ptr */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_test_macrosreparse_837a8725707ae069 (void)) (void)"
  , "{"
  , "  return &body2;"
  , "}"
  , "/* Example_get_args_complex_float_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_3d483c50511b5e18 (void)) ("
  , "  A arg1,"
  , "  float _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_float;"
  , "}"
  , "/* Example_get_args_complex_double_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_eebf44dc776073fb (void)) ("
  , "  A arg1,"
  , "  double _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_double;"
  , "}"
  , "/* Example_get_ret_complex_float_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex (*hs_bindgen_test_macrosreparse_8575286a916f275b (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_complex_float;"
  , "}"
  , "/* Example_get_ret_complex_double_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex (*hs_bindgen_test_macrosreparse_a7e83c9db567bd82 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_complex_double;"
  , "}"
  , "/* Example_get_bespoke_args1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_fdd7ed339385e147 (void)) ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &bespoke_args1;"
  , "}"
  , "/* Example_get_bespoke_args2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_1478d30ab7eee36b (void)) ("
  , "  A arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &bespoke_args2;"
  , "}"
  , "/* Example_get_bespoke_ret1_ptr */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_test_macrosreparse_84b947d89bd6e93a (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret1;"
  , "}"
  , "/* Example_get_bespoke_ret2_ptr */"
  , "__attribute__ ((const))"
  , "size_t (*hs_bindgen_test_macrosreparse_590c8690e7c58094 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret2;"
  , "}"
  , "/* Example_get_arr_args1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_3ebb72bc91129578 (void)) ("
  , "  A arg1[]"
  , ")"
  , "{"
  , "  return &arr_args1;"
  , "}"
  , "/* Example_get_arr_args2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_869365b9f11dd3ae (void)) ("
  , "  A *arg1[]"
  , ")"
  , "{"
  , "  return &arr_args2;"
  , "}"
  , "/* Example_get_arr_args3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_f9d2fcf0aaf3f8fd (void)) ("
  , "  A arg1[5]"
  , ")"
  , "{"
  , "  return &arr_args3;"
  , "}"
  , "/* Example_get_arr_args4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_50d5e26092202412 (void)) ("
  , "  A *arg1[5]"
  , ")"
  , "{"
  , "  return &arr_args4;"
  , "}"
  , "/* Example_get_funptr_args1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_b8f57a71008f5cdb (void)) ("
  , "  A arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args1;"
  , "}"
  , "/* Example_get_funptr_args2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_93c1fd210a776d0a (void)) ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args2;"
  , "}"
  , "/* Example_get_funptr_args3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_dc22c7db682bc9e7 (void)) ("
  , "  A arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args3;"
  , "}"
  , "/* Example_get_funptr_args4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_66a759af6b1308eb (void)) ("
  , "  A arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args4;"
  , "}"
  , "/* Example_get_funptr_args5_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_2fdd8861439448a5 (void)) ("
  , "  A arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args5;"
  , "}"
  , "/* Example_get_comments1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_cd4eae568d107251 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &comments1;"
  , "}"
  , "/* Example_get_const_prim_before1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_bd1fdb1521b4cde1 (void)) ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before1;"
  , "}"
  , "/* Example_get_const_prim_before2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_1415f2307bbc245c (void)) ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before2;"
  , "}"
  , "/* Example_get_const_prim_before3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_b171276e5584d97a (void)) ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before3;"
  , "}"
  , "/* Example_get_const_prim_after1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_f0fdc81dde59ef5e (void)) ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after1;"
  , "}"
  , "/* Example_get_const_prim_after2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_dae0a2f9381f120a (void)) ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after2;"
  , "}"
  , "/* Example_get_const_prim_after3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_6edecc6a7c241a58 (void)) ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after3;"
  , "}"
  , "/* Example_get_const_withoutSign_before1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_80ce262b42b218cd (void)) ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before1;"
  , "}"
  , "/* Example_get_const_withoutSign_before2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_d2170e94cec45b8f (void)) ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before2;"
  , "}"
  , "/* Example_get_const_withoutSign_before3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_8aa6ea4003a403ad (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before3;"
  , "}"
  , "/* Example_get_const_withoutSign_before4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_c3cdab57d9b9d62e (void)) ("
  , "  A arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before4;"
  , "}"
  , "/* Example_get_const_withoutSign_before5_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_199849c0a1153129 (void)) ("
  , "  A arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before5;"
  , "}"
  , "/* Example_get_const_withoutSign_before6_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_42a7e050efc56190 (void)) ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before6;"
  , "}"
  , "/* Example_get_const_withoutSign_before7_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_9e4b537757b29915 (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before7;"
  , "}"
  , "/* Example_get_const_withoutSign_before8_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_336e48090218bdda (void)) ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before8;"
  , "}"
  , "/* Example_get_const_withoutSign_after1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_f9b59aa3adcc5ce2 (void)) ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after1;"
  , "}"
  , "/* Example_get_const_withoutSign_after2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_26e40e5125a4cf97 (void)) ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after2;"
  , "}"
  , "/* Example_get_const_withoutSign_after3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_aa9ac598937e743c (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after3;"
  , "}"
  , "/* Example_get_const_withoutSign_after4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_9cc2e8c3e7d55939 (void)) ("
  , "  A arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after4;"
  , "}"
  , "/* Example_get_const_withoutSign_after5_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_ab482795bc44866a (void)) ("
  , "  A arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after5;"
  , "}"
  , "/* Example_get_const_withoutSign_after6_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_cfc86b45d820a783 (void)) ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after6;"
  , "}"
  , "/* Example_get_const_withoutSign_after7_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_09f90445c16832d3 (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after7;"
  , "}"
  , "/* Example_get_const_withoutSign_after8_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_743a6397a072f432 (void)) ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after8;"
  , "}"
  , "/* Example_get_const_pointers_args1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_16cae2060865df51 (void)) ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args1;"
  , "}"
  , "/* Example_get_const_pointers_args2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_7cc9a8cc774f3cd1 (void)) ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args2;"
  , "}"
  , "/* Example_get_const_pointers_args3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_e05ae98a450c89b3 (void)) ("
  , "  A arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args3;"
  , "}"
  , "/* Example_get_const_pointers_args4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_621fb43dbc6a2a61 (void)) ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args4;"
  , "}"
  , "/* Example_get_const_pointers_args5_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_3f2fd0d04dfc4f0f (void)) ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args5;"
  , "}"
  , "/* Example_get_const_pointers_ret1_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_test_macrosreparse_27f63895274854b1 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret1;"
  , "}"
  , "/* Example_get_const_pointers_ret2_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_test_macrosreparse_9996807631e67058 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret2;"
  , "}"
  , "/* Example_get_const_pointers_ret3_ptr */"
  , "__attribute__ ((const))"
  , "signed int *const (*hs_bindgen_test_macrosreparse_8dfea84c52c73f98 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret3;"
  , "}"
  , "/* Example_get_const_pointers_ret4_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_test_macrosreparse_8d9f7b64cc6c86a4 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret4;"
  , "}"
  , "/* Example_get_const_pointers_ret5_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_test_macrosreparse_c4fb4751d1538163 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret5;"
  , "}"
  , "/* Example_get_const_array_elem1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_201c636c1a669cae (void)) ("
  , "  A const arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem1;"
  , "}"
  , "/* Example_get_const_array_elem2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_310e61a7064c2f86 (void)) ("
  , "  A const *arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem2;"
  , "}"
  , "/* Example_get_const_array_elem3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_bdf71d7dbcbcdc4e (void)) ("
  , "  A *const arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem3;"
  , "}"
  , "/* Example_get_noParams1_ptr */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_test_macrosreparse_d60eea4b30b28bf0 (void)) (void)"
  , "{"
  , "  return &noParams1;"
  , "}"
  , "/* Example_get_noParams2_ptr */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_test_macrosreparse_a360a48bac09ffe7 (void)) (void)"
  , "{"
  , "  return &noParams2;"
  , "}"
  , "/* Example_get_noParams3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosreparse_f87d967820876838 (void)) ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &noParams3;"
  , "}"
  , "/* Example_get_funptr_ret1_ptr */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_test_macrosreparse_e179cb34ae49459c (void)) ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret1;"
  , "}"
  , "/* Example_get_funptr_ret2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_macrosreparse_fee89ee9abb96b9a (void)) ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret2;"
  , "}"
  , "/* Example_get_funptr_ret3_ptr */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_test_macrosreparse_37b12e354695a3ec (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &funptr_ret3;"
  , "}"
  , "/* Example_get_funptr_ret4_ptr */"
  , "__attribute__ ((const))"
  , "char (*(*hs_bindgen_test_macrosreparse_8cf98ff5ff321106 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret4;"
  , "}"
  , "/* Example_get_funptr_ret5_ptr */"
  , "__attribute__ ((const))"
  , "signed int *(*(*hs_bindgen_test_macrosreparse_9cc98e8fa059dba7 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret5;"
  , "}"
  , "/* Example_get_funptr_ret6_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_test_macrosreparse_eca290f9e8706bbd (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret6;"
  , "}"
  , "/* Example_get_funptr_ret7_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_test_macrosreparse_64208169decd5404 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret7;"
  , "}"
  , "/* Example_get_funptr_ret8_ptr */"
  , "__attribute__ ((const))"
  , "signed int *const (*(*hs_bindgen_test_macrosreparse_719cc70cf1f95293 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret8;"
  , "}"
  , "/* Example_get_funptr_ret9_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_test_macrosreparse_c7dd3791224cb869 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret9;"
  , "}"
  , "/* Example_get_funptr_ret10_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_test_macrosreparse_dbf4b490a14f9fe7 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret10;"
  , "}"
  ]))

{-| __unique:__ @Example_get_args_char1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_9bb786fc1eadf4a4" hs_bindgen_test_macrosreparse_9bb786fc1eadf4a4 ::
     IO (Ptr.FunPtr (A -> FC.CChar -> IO ()))

{-# NOINLINE args_char1_ptr #-}

{-| Function declarations

__C declaration:__ @args_char1@

__defined at:__ @macros\/reparse.h:17:6@

__exported by:__ @macros\/reparse.h@
-}
args_char1_ptr :: Ptr.FunPtr (A -> FC.CChar -> IO ())
args_char1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_9bb786fc1eadf4a4

{-| __unique:__ @Example_get_args_char2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e4ee629f7fc67518" hs_bindgen_test_macrosreparse_e4ee629f7fc67518 ::
     IO (Ptr.FunPtr (A -> FC.CSChar -> IO ()))

{-# NOINLINE args_char2_ptr #-}

{-| __C declaration:__ @args_char2@

    __defined at:__ @macros\/reparse.h:18:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char2_ptr :: Ptr.FunPtr (A -> FC.CSChar -> IO ())
args_char2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_e4ee629f7fc67518

{-| __unique:__ @Example_get_args_char3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e6569643d0024113" hs_bindgen_test_macrosreparse_e6569643d0024113 ::
     IO (Ptr.FunPtr (A -> FC.CUChar -> IO ()))

{-# NOINLINE args_char3_ptr #-}

{-| __C declaration:__ @args_char3@

    __defined at:__ @macros\/reparse.h:19:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char3_ptr :: Ptr.FunPtr (A -> FC.CUChar -> IO ())
args_char3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_e6569643d0024113

{-| __unique:__ @Example_get_args_short1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_1d345791897756aa" hs_bindgen_test_macrosreparse_1d345791897756aa ::
     IO (Ptr.FunPtr (A -> FC.CShort -> IO ()))

{-# NOINLINE args_short1_ptr #-}

{-| __C declaration:__ @args_short1@

    __defined at:__ @macros\/reparse.h:21:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short1_ptr :: Ptr.FunPtr (A -> FC.CShort -> IO ())
args_short1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_1d345791897756aa

{-| __unique:__ @Example_get_args_short2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_d27e38c34d2cb75c" hs_bindgen_test_macrosreparse_d27e38c34d2cb75c ::
     IO (Ptr.FunPtr (A -> FC.CShort -> IO ()))

{-# NOINLINE args_short2_ptr #-}

{-| __C declaration:__ @args_short2@

    __defined at:__ @macros\/reparse.h:22:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short2_ptr :: Ptr.FunPtr (A -> FC.CShort -> IO ())
args_short2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_d27e38c34d2cb75c

{-| __unique:__ @Example_get_args_short3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_4e55c5eecff17d32" hs_bindgen_test_macrosreparse_4e55c5eecff17d32 ::
     IO (Ptr.FunPtr (A -> FC.CUShort -> IO ()))

{-# NOINLINE args_short3_ptr #-}

{-| __C declaration:__ @args_short3@

    __defined at:__ @macros\/reparse.h:23:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short3_ptr :: Ptr.FunPtr (A -> FC.CUShort -> IO ())
args_short3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_4e55c5eecff17d32

{-| __unique:__ @Example_get_args_int1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_f934c015e386eade" hs_bindgen_test_macrosreparse_f934c015e386eade ::
     IO (Ptr.FunPtr (A -> FC.CInt -> IO ()))

{-# NOINLINE args_int1_ptr #-}

{-| __C declaration:__ @args_int1@

    __defined at:__ @macros\/reparse.h:25:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int1_ptr :: Ptr.FunPtr (A -> FC.CInt -> IO ())
args_int1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_f934c015e386eade

{-| __unique:__ @Example_get_args_int2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_cddb8e9ceeedf1a2" hs_bindgen_test_macrosreparse_cddb8e9ceeedf1a2 ::
     IO (Ptr.FunPtr (A -> FC.CInt -> IO ()))

{-# NOINLINE args_int2_ptr #-}

{-| __C declaration:__ @args_int2@

    __defined at:__ @macros\/reparse.h:26:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int2_ptr :: Ptr.FunPtr (A -> FC.CInt -> IO ())
args_int2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_cddb8e9ceeedf1a2

{-| __unique:__ @Example_get_args_int3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_20a8b568bed432f6" hs_bindgen_test_macrosreparse_20a8b568bed432f6 ::
     IO (Ptr.FunPtr (A -> FC.CUInt -> IO ()))

{-# NOINLINE args_int3_ptr #-}

{-| __C declaration:__ @args_int3@

    __defined at:__ @macros\/reparse.h:27:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int3_ptr :: Ptr.FunPtr (A -> FC.CUInt -> IO ())
args_int3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_20a8b568bed432f6

{-| __unique:__ @Example_get_args_long1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_2dfd2b59c0da0082" hs_bindgen_test_macrosreparse_2dfd2b59c0da0082 ::
     IO (Ptr.FunPtr (A -> FC.CLong -> IO ()))

{-# NOINLINE args_long1_ptr #-}

{-| __C declaration:__ @args_long1@

    __defined at:__ @macros\/reparse.h:29:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long1_ptr :: Ptr.FunPtr (A -> FC.CLong -> IO ())
args_long1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_2dfd2b59c0da0082

{-| __unique:__ @Example_get_args_long2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_771b1579cbb95648" hs_bindgen_test_macrosreparse_771b1579cbb95648 ::
     IO (Ptr.FunPtr (A -> FC.CLong -> IO ()))

{-# NOINLINE args_long2_ptr #-}

{-| __C declaration:__ @args_long2@

    __defined at:__ @macros\/reparse.h:30:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long2_ptr :: Ptr.FunPtr (A -> FC.CLong -> IO ())
args_long2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_771b1579cbb95648

{-| __unique:__ @Example_get_args_long3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_26eaae8b45f2d0c3" hs_bindgen_test_macrosreparse_26eaae8b45f2d0c3 ::
     IO (Ptr.FunPtr (A -> FC.CULong -> IO ()))

{-# NOINLINE args_long3_ptr #-}

{-| __C declaration:__ @args_long3@

    __defined at:__ @macros\/reparse.h:31:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long3_ptr :: Ptr.FunPtr (A -> FC.CULong -> IO ())
args_long3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_26eaae8b45f2d0c3

{-| __unique:__ @Example_get_args_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_a92e81c68b785b28" hs_bindgen_test_macrosreparse_a92e81c68b785b28 ::
     IO (Ptr.FunPtr (A -> FC.CFloat -> IO ()))

{-# NOINLINE args_float_ptr #-}

{-| __C declaration:__ @args_float@

    __defined at:__ @macros\/reparse.h:33:6@

    __exported by:__ @macros\/reparse.h@
-}
args_float_ptr :: Ptr.FunPtr (A -> FC.CFloat -> IO ())
args_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_a92e81c68b785b28

{-| __unique:__ @Example_get_args_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_d10652d4a85ed811" hs_bindgen_test_macrosreparse_d10652d4a85ed811 ::
     IO (Ptr.FunPtr (A -> FC.CDouble -> IO ()))

{-# NOINLINE args_double_ptr #-}

{-| __C declaration:__ @args_double@

    __defined at:__ @macros\/reparse.h:34:6@

    __exported by:__ @macros\/reparse.h@
-}
args_double_ptr :: Ptr.FunPtr (A -> FC.CDouble -> IO ())
args_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_d10652d4a85ed811

{-| __unique:__ @Example_get_args_bool1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_452116ebff72c142" hs_bindgen_test_macrosreparse_452116ebff72c142 ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE args_bool1_ptr #-}

{-| __C declaration:__ @args_bool1@

    __defined at:__ @macros\/reparse.h:35:6@

    __exported by:__ @macros\/reparse.h@
-}
args_bool1_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
args_bool1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_452116ebff72c142

{-| __unique:__ @Example_get_args_struct_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_de4ae6525b4cf7ab" hs_bindgen_test_macrosreparse_de4ae6525b4cf7ab ::
     IO (Ptr.FunPtr (A -> Some_struct -> IO ()))

{-# NOINLINE args_struct_ptr #-}

{-| __C declaration:__ @args_struct@

    __defined at:__ @macros\/reparse.h:37:6@

    __exported by:__ @macros\/reparse.h@
-}
args_struct_ptr :: Ptr.FunPtr (A -> Some_struct -> IO ())
args_struct_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_de4ae6525b4cf7ab

{-| __unique:__ @Example_get_args_union_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e55e33051e707111" hs_bindgen_test_macrosreparse_e55e33051e707111 ::
     IO (Ptr.FunPtr (A -> Some_union -> IO ()))

{-# NOINLINE args_union_ptr #-}

{-| __C declaration:__ @args_union@

    __defined at:__ @macros\/reparse.h:38:6@

    __exported by:__ @macros\/reparse.h@
-}
args_union_ptr :: Ptr.FunPtr (A -> Some_union -> IO ())
args_union_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_e55e33051e707111

{-| __unique:__ @Example_get_args_enum_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_4712c7f26562021a" hs_bindgen_test_macrosreparse_4712c7f26562021a ::
     IO (Ptr.FunPtr (A -> Some_enum -> IO ()))

{-# NOINLINE args_enum_ptr #-}

{-| __C declaration:__ @args_enum@

    __defined at:__ @macros\/reparse.h:39:6@

    __exported by:__ @macros\/reparse.h@
-}
args_enum_ptr :: Ptr.FunPtr (A -> Some_enum -> IO ())
args_enum_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_4712c7f26562021a

{-| __unique:__ @Example_get_args_pointer1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_5d9b782f640824e7" hs_bindgen_test_macrosreparse_5d9b782f640824e7 ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE args_pointer1_ptr #-}

{-| __C declaration:__ @args_pointer1@

    __defined at:__ @macros\/reparse.h:41:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer1_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
args_pointer1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_5d9b782f640824e7

{-| __unique:__ @Example_get_args_pointer2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_cfd19552b2176dfc" hs_bindgen_test_macrosreparse_cfd19552b2176dfc ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr (Ptr.Ptr FC.CInt)) -> IO ()))

{-# NOINLINE args_pointer2_ptr #-}

{-| __C declaration:__ @args_pointer2@

    __defined at:__ @macros\/reparse.h:42:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer2_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr (Ptr.Ptr FC.CInt)) -> IO ())
args_pointer2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_cfd19552b2176dfc

{-| __unique:__ @Example_get_args_pointer3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_26d448a288ca759f" hs_bindgen_test_macrosreparse_26d448a288ca759f ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr Void) -> IO ()))

{-# NOINLINE args_pointer3_ptr #-}

{-| __C declaration:__ @args_pointer3@

    __defined at:__ @macros\/reparse.h:43:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer3_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr Void) -> IO ())
args_pointer3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_26d448a288ca759f

{-| __unique:__ @Example_get_ret_A_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_b9547cae9e047b1f" hs_bindgen_test_macrosreparse_b9547cae9e047b1f ::
     IO (Ptr.FunPtr (IO A))

{-# NOINLINE ret_A_ptr #-}

{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h:47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A_ptr :: Ptr.FunPtr (IO A)
ret_A_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_b9547cae9e047b1f

{-| __unique:__ @Example_get_ret_char1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_9a1302071a2a8cce" hs_bindgen_test_macrosreparse_9a1302071a2a8cce ::
     IO (Ptr.FunPtr (A -> IO FC.CChar))

{-# NOINLINE ret_char1_ptr #-}

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h:49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1_ptr :: Ptr.FunPtr (A -> IO FC.CChar)
ret_char1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_9a1302071a2a8cce

{-| __unique:__ @Example_get_ret_char2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_cd01539317a8a663" hs_bindgen_test_macrosreparse_cd01539317a8a663 ::
     IO (Ptr.FunPtr (A -> IO FC.CSChar))

{-# NOINLINE ret_char2_ptr #-}

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h:50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2_ptr :: Ptr.FunPtr (A -> IO FC.CSChar)
ret_char2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_cd01539317a8a663

{-| __unique:__ @Example_get_ret_char3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_b8da47545f19e06f" hs_bindgen_test_macrosreparse_b8da47545f19e06f ::
     IO (Ptr.FunPtr (A -> IO FC.CUChar))

{-# NOINLINE ret_char3_ptr #-}

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h:51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3_ptr :: Ptr.FunPtr (A -> IO FC.CUChar)
ret_char3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_b8da47545f19e06f

{-| __unique:__ @Example_get_ret_short1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c1b7895b48ce432d" hs_bindgen_test_macrosreparse_c1b7895b48ce432d ::
     IO (Ptr.FunPtr (A -> IO FC.CShort))

{-# NOINLINE ret_short1_ptr #-}

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h:53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1_ptr :: Ptr.FunPtr (A -> IO FC.CShort)
ret_short1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_c1b7895b48ce432d

{-| __unique:__ @Example_get_ret_short2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_a601ca8574ff50e6" hs_bindgen_test_macrosreparse_a601ca8574ff50e6 ::
     IO (Ptr.FunPtr (A -> IO FC.CShort))

{-# NOINLINE ret_short2_ptr #-}

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h:54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2_ptr :: Ptr.FunPtr (A -> IO FC.CShort)
ret_short2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_a601ca8574ff50e6

{-| __unique:__ @Example_get_ret_short3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_ff947411a019ff3c" hs_bindgen_test_macrosreparse_ff947411a019ff3c ::
     IO (Ptr.FunPtr (A -> IO FC.CUShort))

{-# NOINLINE ret_short3_ptr #-}

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h:55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3_ptr :: Ptr.FunPtr (A -> IO FC.CUShort)
ret_short3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_ff947411a019ff3c

{-| __unique:__ @Example_get_ret_int1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_7f88eef51e532ea7" hs_bindgen_test_macrosreparse_7f88eef51e532ea7 ::
     IO (Ptr.FunPtr (A -> IO FC.CInt))

{-# NOINLINE ret_int1_ptr #-}

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h:57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1_ptr :: Ptr.FunPtr (A -> IO FC.CInt)
ret_int1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_7f88eef51e532ea7

{-| __unique:__ @Example_get_ret_int2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_4588db9e8f9556b8" hs_bindgen_test_macrosreparse_4588db9e8f9556b8 ::
     IO (Ptr.FunPtr (A -> IO FC.CInt))

{-# NOINLINE ret_int2_ptr #-}

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h:58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2_ptr :: Ptr.FunPtr (A -> IO FC.CInt)
ret_int2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_4588db9e8f9556b8

{-| __unique:__ @Example_get_ret_int3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_17507f4313b4622f" hs_bindgen_test_macrosreparse_17507f4313b4622f ::
     IO (Ptr.FunPtr (A -> IO FC.CUInt))

{-# NOINLINE ret_int3_ptr #-}

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h:59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3_ptr :: Ptr.FunPtr (A -> IO FC.CUInt)
ret_int3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_17507f4313b4622f

{-| __unique:__ @Example_get_ret_long1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_18d2aee117ef3357" hs_bindgen_test_macrosreparse_18d2aee117ef3357 ::
     IO (Ptr.FunPtr (A -> IO FC.CLong))

{-# NOINLINE ret_long1_ptr #-}

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h:61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1_ptr :: Ptr.FunPtr (A -> IO FC.CLong)
ret_long1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_18d2aee117ef3357

{-| __unique:__ @Example_get_ret_long2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_b3d75b43adf369ff" hs_bindgen_test_macrosreparse_b3d75b43adf369ff ::
     IO (Ptr.FunPtr (A -> IO FC.CLong))

{-# NOINLINE ret_long2_ptr #-}

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h:62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2_ptr :: Ptr.FunPtr (A -> IO FC.CLong)
ret_long2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_b3d75b43adf369ff

{-| __unique:__ @Example_get_ret_long3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_4d45e5ea9c7dc6d1" hs_bindgen_test_macrosreparse_4d45e5ea9c7dc6d1 ::
     IO (Ptr.FunPtr (A -> IO FC.CULong))

{-# NOINLINE ret_long3_ptr #-}

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h:63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3_ptr :: Ptr.FunPtr (A -> IO FC.CULong)
ret_long3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_4d45e5ea9c7dc6d1

{-| __unique:__ @Example_get_ret_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_8936a8d4587c3d2a" hs_bindgen_test_macrosreparse_8936a8d4587c3d2a ::
     IO (Ptr.FunPtr (A -> IO FC.CFloat))

{-# NOINLINE ret_float_ptr #-}

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h:65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float_ptr :: Ptr.FunPtr (A -> IO FC.CFloat)
ret_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_8936a8d4587c3d2a

{-| __unique:__ @Example_get_ret_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_587a1106df194fac" hs_bindgen_test_macrosreparse_587a1106df194fac ::
     IO (Ptr.FunPtr (A -> IO FC.CDouble))

{-# NOINLINE ret_double_ptr #-}

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h:66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double_ptr :: Ptr.FunPtr (A -> IO FC.CDouble)
ret_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_587a1106df194fac

{-| __unique:__ @Example_get_ret_bool1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_a2e76d906e895d52" hs_bindgen_test_macrosreparse_a2e76d906e895d52 ::
     IO (Ptr.FunPtr (A -> IO FC.CBool))

{-# NOINLINE ret_bool1_ptr #-}

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h:67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1_ptr :: Ptr.FunPtr (A -> IO FC.CBool)
ret_bool1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_a2e76d906e895d52

{-| __unique:__ @Example_get_ret_struct_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_cd5235d2ab3b01e6" hs_bindgen_test_macrosreparse_cd5235d2ab3b01e6 ::
     IO (Ptr.FunPtr (A -> IO Some_struct))

{-# NOINLINE ret_struct_ptr #-}

{-| __C declaration:__ @ret_struct@

    __defined at:__ @macros\/reparse.h:69:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_struct_ptr :: Ptr.FunPtr (A -> IO Some_struct)
ret_struct_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_cd5235d2ab3b01e6

{-| __unique:__ @Example_get_ret_union_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_2c2965c809dc1eaf" hs_bindgen_test_macrosreparse_2c2965c809dc1eaf ::
     IO (Ptr.FunPtr (A -> IO Some_union))

{-# NOINLINE ret_union_ptr #-}

{-| __C declaration:__ @ret_union@

    __defined at:__ @macros\/reparse.h:70:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_union_ptr :: Ptr.FunPtr (A -> IO Some_union)
ret_union_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_2c2965c809dc1eaf

{-| __unique:__ @Example_get_ret_enum_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_4b63686f9884c0b6" hs_bindgen_test_macrosreparse_4b63686f9884c0b6 ::
     IO (Ptr.FunPtr (A -> IO Some_enum))

{-# NOINLINE ret_enum_ptr #-}

{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h:71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum_ptr :: Ptr.FunPtr (A -> IO Some_enum)
ret_enum_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_4b63686f9884c0b6

{-| __unique:__ @Example_get_ret_pointer1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_2cdd00bac75fbfcc" hs_bindgen_test_macrosreparse_2cdd00bac75fbfcc ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE ret_pointer1_ptr #-}

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h:73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
ret_pointer1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_2cdd00bac75fbfcc

{-| __unique:__ @Example_get_ret_pointer2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_9bb32d52132f4fea" hs_bindgen_test_macrosreparse_9bb32d52132f4fea ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt))))

{-# NOINLINE ret_pointer2_ptr #-}

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h:74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt)))
ret_pointer2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_9bb32d52132f4fea

{-| __unique:__ @Example_get_ret_pointer3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_30356ad0d31ec0c7" hs_bindgen_test_macrosreparse_30356ad0d31ec0c7 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr Void)))

{-# NOINLINE ret_pointer3_ptr #-}

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h:75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr Void))
ret_pointer3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_30356ad0d31ec0c7

{-| __unique:__ @Example_get_body1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_ccd217a873c281d2" hs_bindgen_test_macrosreparse_ccd217a873c281d2 ::
     IO (Ptr.FunPtr (A -> IO FC.CInt))

{-# NOINLINE body1_ptr #-}

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h:79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1_ptr :: Ptr.FunPtr (A -> IO FC.CInt)
body1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_ccd217a873c281d2

{-| __unique:__ @Example_get_body2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_837a8725707ae069" hs_bindgen_test_macrosreparse_837a8725707ae069 ::
     IO (Ptr.FunPtr (IO A))

{-# NOINLINE body2_ptr #-}

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h:80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2_ptr :: Ptr.FunPtr (IO A)
body2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_837a8725707ae069

{-| __unique:__ @Example_get_args_complex_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_3d483c50511b5e18" hs_bindgen_test_macrosreparse_3d483c50511b5e18 ::
     IO (Ptr.FunPtr (A -> (Data.Complex.Complex FC.CFloat) -> IO ()))

{-# NOINLINE args_complex_float_ptr #-}

{-| __C declaration:__ @args_complex_float@

    __defined at:__ @macros\/reparse.h:84:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_float_ptr :: Ptr.FunPtr (A -> (Data.Complex.Complex FC.CFloat) -> IO ())
args_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_3d483c50511b5e18

{-| __unique:__ @Example_get_args_complex_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_eebf44dc776073fb" hs_bindgen_test_macrosreparse_eebf44dc776073fb ::
     IO (Ptr.FunPtr (A -> (Data.Complex.Complex FC.CDouble) -> IO ()))

{-# NOINLINE args_complex_double_ptr #-}

{-| __C declaration:__ @args_complex_double@

    __defined at:__ @macros\/reparse.h:85:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_double_ptr :: Ptr.FunPtr (A -> (Data.Complex.Complex FC.CDouble) -> IO ())
args_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_eebf44dc776073fb

{-| __unique:__ @Example_get_ret_complex_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_8575286a916f275b" hs_bindgen_test_macrosreparse_8575286a916f275b ::
     IO (Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CFloat)))

{-# NOINLINE ret_complex_float_ptr #-}

{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @macros\/reparse.h:86:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_float_ptr :: Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CFloat))
ret_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_8575286a916f275b

{-| __unique:__ @Example_get_ret_complex_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_a7e83c9db567bd82" hs_bindgen_test_macrosreparse_a7e83c9db567bd82 ::
     IO (Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CDouble)))

{-# NOINLINE ret_complex_double_ptr #-}

{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @macros\/reparse.h:87:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_double_ptr :: Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CDouble))
ret_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_a7e83c9db567bd82

{-| __unique:__ @Example_get_bespoke_args1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_fdd7ed339385e147" hs_bindgen_test_macrosreparse_fdd7ed339385e147 ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE bespoke_args1_ptr #-}

{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @macros\/reparse.h:94:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args1_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
bespoke_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_fdd7ed339385e147

{-| __unique:__ @Example_get_bespoke_args2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_1478d30ab7eee36b" hs_bindgen_test_macrosreparse_1478d30ab7eee36b ::
     IO (Ptr.FunPtr (A -> HsBindgen.Runtime.Prelude.CSize -> IO ()))

{-# NOINLINE bespoke_args2_ptr #-}

{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @macros\/reparse.h:95:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args2_ptr :: Ptr.FunPtr (A -> HsBindgen.Runtime.Prelude.CSize -> IO ())
bespoke_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_1478d30ab7eee36b

{-| __unique:__ @Example_get_bespoke_ret1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_84b947d89bd6e93a" hs_bindgen_test_macrosreparse_84b947d89bd6e93a ::
     IO (Ptr.FunPtr (A -> IO FC.CBool))

{-# NOINLINE bespoke_ret1_ptr #-}

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h:97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1_ptr :: Ptr.FunPtr (A -> IO FC.CBool)
bespoke_ret1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_84b947d89bd6e93a

{-| __unique:__ @Example_get_bespoke_ret2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_590c8690e7c58094" hs_bindgen_test_macrosreparse_590c8690e7c58094 ::
     IO (Ptr.FunPtr (A -> IO HsBindgen.Runtime.Prelude.CSize))

{-# NOINLINE bespoke_ret2_ptr #-}

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h:98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2_ptr :: Ptr.FunPtr (A -> IO HsBindgen.Runtime.Prelude.CSize)
bespoke_ret2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_590c8690e7c58094

{-| __unique:__ @Example_get_arr_args1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_3ebb72bc91129578" hs_bindgen_test_macrosreparse_3ebb72bc91129578 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ()))

{-# NOINLINE arr_args1_ptr #-}

{-| Arrays

__C declaration:__ @arr_args1@

__defined at:__ @macros\/reparse.h:104:6@

__exported by:__ @macros\/reparse.h@
-}
arr_args1_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ())
arr_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_3ebb72bc91129578

{-| __unique:__ @Example_get_arr_args2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_869365b9f11dd3ae" hs_bindgen_test_macrosreparse_869365b9f11dd3ae ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ()))

{-# NOINLINE arr_args2_ptr #-}

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h:105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ())
arr_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_869365b9f11dd3ae

{-| __unique:__ @Example_get_arr_args3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_f9d2fcf0aaf3f8fd" hs_bindgen_test_macrosreparse_f9d2fcf0aaf3f8fd ::
     IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) A) -> IO ()))

{-# NOINLINE arr_args3_ptr #-}

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h:106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) A) -> IO ())
arr_args3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_f9d2fcf0aaf3f8fd

{-| __unique:__ @Example_get_arr_args4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_50d5e26092202412" hs_bindgen_test_macrosreparse_50d5e26092202412 ::
     IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) (Ptr.Ptr A)) -> IO ()))

{-# NOINLINE arr_args4_ptr #-}

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h:107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) (Ptr.Ptr A)) -> IO ())
arr_args4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_50d5e26092202412

{-| __unique:__ @Example_get_funptr_args1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_b8f57a71008f5cdb" hs_bindgen_test_macrosreparse_b8f57a71008f5cdb ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (IO ())) -> IO ()))

{-# NOINLINE funptr_args1_ptr #-}

{-| Function pointers

__C declaration:__ @funptr_args1@

__defined at:__ @macros\/reparse.h:126:6@

__exported by:__ @macros\/reparse.h@
-}
funptr_args1_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (IO ())) -> IO ())
funptr_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_b8f57a71008f5cdb

{-| __unique:__ @Example_get_funptr_args2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_93c1fd210a776d0a" hs_bindgen_test_macrosreparse_93c1fd210a776d0a ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ()))

{-# NOINLINE funptr_args2_ptr #-}

{-| __C declaration:__ @funptr_args2@

    __defined at:__ @macros\/reparse.h:127:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args2_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ())
funptr_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_93c1fd210a776d0a

{-| __unique:__ @Example_get_funptr_args3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_dc22c7db682bc9e7" hs_bindgen_test_macrosreparse_dc22c7db682bc9e7 ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> IO ())) -> IO ()))

{-# NOINLINE funptr_args3_ptr #-}

{-| __C declaration:__ @funptr_args3@

    __defined at:__ @macros\/reparse.h:128:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args3_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> IO ())) -> IO ())
funptr_args3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_dc22c7db682bc9e7

{-| __unique:__ @Example_get_funptr_args4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_66a759af6b1308eb" hs_bindgen_test_macrosreparse_66a759af6b1308eb ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)) -> IO ()))

{-# NOINLINE funptr_args4_ptr #-}

{-| __C declaration:__ @funptr_args4@

    __defined at:__ @macros\/reparse.h:129:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args4_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)) -> IO ())
funptr_args4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_66a759af6b1308eb

{-| __unique:__ @Example_get_funptr_args5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_2fdd8861439448a5" hs_bindgen_test_macrosreparse_2fdd8861439448a5 ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))) -> IO ()))

{-# NOINLINE funptr_args5_ptr #-}

{-| __C declaration:__ @funptr_args5@

    __defined at:__ @macros\/reparse.h:130:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args5_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))) -> IO ())
funptr_args5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_2fdd8861439448a5

{-| __unique:__ @Example_get_comments1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_cd4eae568d107251" hs_bindgen_test_macrosreparse_cd4eae568d107251 ::
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
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_cd4eae568d107251

{-| __unique:__ @Example_get_const_prim_before1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_bd1fdb1521b4cde1" hs_bindgen_test_macrosreparse_bd1fdb1521b4cde1 ::
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
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_bd1fdb1521b4cde1

{-| __unique:__ @Example_get_const_prim_before2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_1415f2307bbc245c" hs_bindgen_test_macrosreparse_1415f2307bbc245c ::
     IO (Ptr.FunPtr (A -> FC.CSChar -> IO ()))

{-# NOINLINE const_prim_before2_ptr #-}

{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @macros\/reparse.h:180:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before2_ptr :: Ptr.FunPtr (A -> FC.CSChar -> IO ())
const_prim_before2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_1415f2307bbc245c

{-| __unique:__ @Example_get_const_prim_before3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_b171276e5584d97a" hs_bindgen_test_macrosreparse_b171276e5584d97a ::
     IO (Ptr.FunPtr (A -> FC.CUChar -> IO ()))

{-# NOINLINE const_prim_before3_ptr #-}

{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @macros\/reparse.h:181:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before3_ptr :: Ptr.FunPtr (A -> FC.CUChar -> IO ())
const_prim_before3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_b171276e5584d97a

{-| __unique:__ @Example_get_const_prim_after1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_f0fdc81dde59ef5e" hs_bindgen_test_macrosreparse_f0fdc81dde59ef5e ::
     IO (Ptr.FunPtr (A -> FC.CChar -> IO ()))

{-# NOINLINE const_prim_after1_ptr #-}

{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @macros\/reparse.h:182:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after1_ptr :: Ptr.FunPtr (A -> FC.CChar -> IO ())
const_prim_after1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_f0fdc81dde59ef5e

{-| __unique:__ @Example_get_const_prim_after2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_dae0a2f9381f120a" hs_bindgen_test_macrosreparse_dae0a2f9381f120a ::
     IO (Ptr.FunPtr (A -> FC.CSChar -> IO ()))

{-# NOINLINE const_prim_after2_ptr #-}

{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @macros\/reparse.h:183:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after2_ptr :: Ptr.FunPtr (A -> FC.CSChar -> IO ())
const_prim_after2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_dae0a2f9381f120a

{-| __unique:__ @Example_get_const_prim_after3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_6edecc6a7c241a58" hs_bindgen_test_macrosreparse_6edecc6a7c241a58 ::
     IO (Ptr.FunPtr (A -> FC.CUChar -> IO ()))

{-# NOINLINE const_prim_after3_ptr #-}

{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @macros\/reparse.h:184:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after3_ptr :: Ptr.FunPtr (A -> FC.CUChar -> IO ())
const_prim_after3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_6edecc6a7c241a58

{-| __unique:__ @Example_get_const_withoutSign_before1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_80ce262b42b218cd" hs_bindgen_test_macrosreparse_80ce262b42b218cd ::
     IO (Ptr.FunPtr (A -> FC.CFloat -> IO ()))

{-# NOINLINE const_withoutSign_before1_ptr #-}

{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @macros\/reparse.h:188:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before1_ptr :: Ptr.FunPtr (A -> FC.CFloat -> IO ())
const_withoutSign_before1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_80ce262b42b218cd

{-| __unique:__ @Example_get_const_withoutSign_before2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_d2170e94cec45b8f" hs_bindgen_test_macrosreparse_d2170e94cec45b8f ::
     IO (Ptr.FunPtr (A -> FC.CDouble -> IO ()))

{-# NOINLINE const_withoutSign_before2_ptr #-}

{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @macros\/reparse.h:189:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before2_ptr :: Ptr.FunPtr (A -> FC.CDouble -> IO ())
const_withoutSign_before2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_d2170e94cec45b8f

{-| __unique:__ @Example_get_const_withoutSign_before3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_8aa6ea4003a403ad" hs_bindgen_test_macrosreparse_8aa6ea4003a403ad ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE const_withoutSign_before3_ptr #-}

{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @macros\/reparse.h:190:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before3_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_before3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_8aa6ea4003a403ad

{-| __unique:__ @Example_get_const_withoutSign_before4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c3cdab57d9b9d62e" hs_bindgen_test_macrosreparse_c3cdab57d9b9d62e ::
     IO (Ptr.FunPtr (A -> Some_struct -> IO ()))

{-# NOINLINE const_withoutSign_before4_ptr #-}

{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @macros\/reparse.h:191:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before4_ptr :: Ptr.FunPtr (A -> Some_struct -> IO ())
const_withoutSign_before4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_c3cdab57d9b9d62e

{-| __unique:__ @Example_get_const_withoutSign_before5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_199849c0a1153129" hs_bindgen_test_macrosreparse_199849c0a1153129 ::
     IO (Ptr.FunPtr (A -> Some_union -> IO ()))

{-# NOINLINE const_withoutSign_before5_ptr #-}

{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @macros\/reparse.h:192:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before5_ptr :: Ptr.FunPtr (A -> Some_union -> IO ())
const_withoutSign_before5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_199849c0a1153129

{-| __unique:__ @Example_get_const_withoutSign_before6_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_42a7e050efc56190" hs_bindgen_test_macrosreparse_42a7e050efc56190 ::
     IO (Ptr.FunPtr (A -> Some_enum -> IO ()))

{-# NOINLINE const_withoutSign_before6_ptr #-}

{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @macros\/reparse.h:193:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before6_ptr :: Ptr.FunPtr (A -> Some_enum -> IO ())
const_withoutSign_before6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_42a7e050efc56190

{-| __unique:__ @Example_get_const_withoutSign_before7_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_9e4b537757b29915" hs_bindgen_test_macrosreparse_9e4b537757b29915 ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE const_withoutSign_before7_ptr #-}

{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @macros\/reparse.h:194:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before7_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_before7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_9e4b537757b29915

{-| __unique:__ @Example_get_const_withoutSign_before8_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_336e48090218bdda" hs_bindgen_test_macrosreparse_336e48090218bdda ::
     IO (Ptr.FunPtr (A -> HsBindgen.Runtime.Prelude.CSize -> IO ()))

{-# NOINLINE const_withoutSign_before8_ptr #-}

{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @macros\/reparse.h:195:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before8_ptr :: Ptr.FunPtr (A -> HsBindgen.Runtime.Prelude.CSize -> IO ())
const_withoutSign_before8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_336e48090218bdda

{-| __unique:__ @Example_get_const_withoutSign_after1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_f9b59aa3adcc5ce2" hs_bindgen_test_macrosreparse_f9b59aa3adcc5ce2 ::
     IO (Ptr.FunPtr (A -> FC.CFloat -> IO ()))

{-# NOINLINE const_withoutSign_after1_ptr #-}

{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @macros\/reparse.h:197:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after1_ptr :: Ptr.FunPtr (A -> FC.CFloat -> IO ())
const_withoutSign_after1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_f9b59aa3adcc5ce2

{-| __unique:__ @Example_get_const_withoutSign_after2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_26e40e5125a4cf97" hs_bindgen_test_macrosreparse_26e40e5125a4cf97 ::
     IO (Ptr.FunPtr (A -> FC.CDouble -> IO ()))

{-# NOINLINE const_withoutSign_after2_ptr #-}

{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @macros\/reparse.h:198:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after2_ptr :: Ptr.FunPtr (A -> FC.CDouble -> IO ())
const_withoutSign_after2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_26e40e5125a4cf97

{-| __unique:__ @Example_get_const_withoutSign_after3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_aa9ac598937e743c" hs_bindgen_test_macrosreparse_aa9ac598937e743c ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE const_withoutSign_after3_ptr #-}

{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @macros\/reparse.h:199:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after3_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_after3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_aa9ac598937e743c

{-| __unique:__ @Example_get_const_withoutSign_after4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_9cc2e8c3e7d55939" hs_bindgen_test_macrosreparse_9cc2e8c3e7d55939 ::
     IO (Ptr.FunPtr (A -> Some_struct -> IO ()))

{-# NOINLINE const_withoutSign_after4_ptr #-}

{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @macros\/reparse.h:200:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after4_ptr :: Ptr.FunPtr (A -> Some_struct -> IO ())
const_withoutSign_after4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_9cc2e8c3e7d55939

{-| __unique:__ @Example_get_const_withoutSign_after5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_ab482795bc44866a" hs_bindgen_test_macrosreparse_ab482795bc44866a ::
     IO (Ptr.FunPtr (A -> Some_union -> IO ()))

{-# NOINLINE const_withoutSign_after5_ptr #-}

{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @macros\/reparse.h:201:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after5_ptr :: Ptr.FunPtr (A -> Some_union -> IO ())
const_withoutSign_after5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_ab482795bc44866a

{-| __unique:__ @Example_get_const_withoutSign_after6_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_cfc86b45d820a783" hs_bindgen_test_macrosreparse_cfc86b45d820a783 ::
     IO (Ptr.FunPtr (A -> Some_enum -> IO ()))

{-# NOINLINE const_withoutSign_after6_ptr #-}

{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @macros\/reparse.h:202:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after6_ptr :: Ptr.FunPtr (A -> Some_enum -> IO ())
const_withoutSign_after6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_cfc86b45d820a783

{-| __unique:__ @Example_get_const_withoutSign_after7_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_09f90445c16832d3" hs_bindgen_test_macrosreparse_09f90445c16832d3 ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE const_withoutSign_after7_ptr #-}

{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @macros\/reparse.h:203:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after7_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_after7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_09f90445c16832d3

{-| __unique:__ @Example_get_const_withoutSign_after8_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_743a6397a072f432" hs_bindgen_test_macrosreparse_743a6397a072f432 ::
     IO (Ptr.FunPtr (A -> HsBindgen.Runtime.Prelude.CSize -> IO ()))

{-# NOINLINE const_withoutSign_after8_ptr #-}

{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @macros\/reparse.h:204:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after8_ptr :: Ptr.FunPtr (A -> HsBindgen.Runtime.Prelude.CSize -> IO ())
const_withoutSign_after8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_743a6397a072f432

{-| __unique:__ @Example_get_const_pointers_args1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_16cae2060865df51" hs_bindgen_test_macrosreparse_16cae2060865df51 ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args1_ptr #-}

{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @macros\/reparse.h:208:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args1_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_16cae2060865df51

{-| __unique:__ @Example_get_const_pointers_args2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_7cc9a8cc774f3cd1" hs_bindgen_test_macrosreparse_7cc9a8cc774f3cd1 ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args2_ptr #-}

{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @macros\/reparse.h:209:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args2_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_7cc9a8cc774f3cd1

{-| __unique:__ @Example_get_const_pointers_args3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e05ae98a450c89b3" hs_bindgen_test_macrosreparse_e05ae98a450c89b3 ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args3_ptr #-}

{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @macros\/reparse.h:210:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args3_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_e05ae98a450c89b3

{-| __unique:__ @Example_get_const_pointers_args4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_621fb43dbc6a2a61" hs_bindgen_test_macrosreparse_621fb43dbc6a2a61 ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args4_ptr #-}

{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @macros\/reparse.h:211:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args4_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_621fb43dbc6a2a61

{-| __unique:__ @Example_get_const_pointers_args5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_3f2fd0d04dfc4f0f" hs_bindgen_test_macrosreparse_3f2fd0d04dfc4f0f ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args5_ptr #-}

{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @macros\/reparse.h:212:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args5_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_3f2fd0d04dfc4f0f

{-| __unique:__ @Example_get_const_pointers_ret1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_27f63895274854b1" hs_bindgen_test_macrosreparse_27f63895274854b1 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret1_ptr #-}

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h:214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_27f63895274854b1

{-| __unique:__ @Example_get_const_pointers_ret2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_9996807631e67058" hs_bindgen_test_macrosreparse_9996807631e67058 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret2_ptr #-}

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h:215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_9996807631e67058

{-| __unique:__ @Example_get_const_pointers_ret3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_8dfea84c52c73f98" hs_bindgen_test_macrosreparse_8dfea84c52c73f98 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret3_ptr #-}

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h:216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_8dfea84c52c73f98

{-| __unique:__ @Example_get_const_pointers_ret4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_8d9f7b64cc6c86a4" hs_bindgen_test_macrosreparse_8d9f7b64cc6c86a4 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret4_ptr #-}

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h:217:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_8d9f7b64cc6c86a4

{-| __unique:__ @Example_get_const_pointers_ret5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c4fb4751d1538163" hs_bindgen_test_macrosreparse_c4fb4751d1538163 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret5_ptr #-}

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h:218:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_c4fb4751d1538163

{-| __unique:__ @Example_get_const_array_elem1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_201c636c1a669cae" hs_bindgen_test_macrosreparse_201c636c1a669cae ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ()))

{-# NOINLINE const_array_elem1_ptr #-}

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h:246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ())
const_array_elem1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_201c636c1a669cae

{-| __unique:__ @Example_get_const_array_elem2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_310e61a7064c2f86" hs_bindgen_test_macrosreparse_310e61a7064c2f86 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ()))

{-# NOINLINE const_array_elem2_ptr #-}

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h:247:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ())
const_array_elem2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_310e61a7064c2f86

{-| __unique:__ @Example_get_const_array_elem3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_bdf71d7dbcbcdc4e" hs_bindgen_test_macrosreparse_bdf71d7dbcbcdc4e ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ()))

{-# NOINLINE const_array_elem3_ptr #-}

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h:248:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ())
const_array_elem3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_bdf71d7dbcbcdc4e

{-| __unique:__ @Example_get_noParams1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_d60eea4b30b28bf0" hs_bindgen_test_macrosreparse_d60eea4b30b28bf0 ::
     IO (Ptr.FunPtr (IO A))

{-# NOINLINE noParams1_ptr #-}

{-| Other examples we reparsed /incorrectly/ before language-c

__C declaration:__ @noParams1@

__defined at:__ @macros\/reparse.h:256:3@

__exported by:__ @macros\/reparse.h@
-}
noParams1_ptr :: Ptr.FunPtr (IO A)
noParams1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_d60eea4b30b28bf0

{-| __unique:__ @Example_get_noParams2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_a360a48bac09ffe7" hs_bindgen_test_macrosreparse_a360a48bac09ffe7 ::
     IO (Ptr.FunPtr (IO A))

{-# NOINLINE noParams2_ptr #-}

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h:257:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2_ptr :: Ptr.FunPtr (IO A)
noParams2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_a360a48bac09ffe7

{-| __unique:__ @Example_get_noParams3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_f87d967820876838" hs_bindgen_test_macrosreparse_f87d967820876838 ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ()))

{-# NOINLINE noParams3_ptr #-}

{-| __C declaration:__ @noParams3@

    __defined at:__ @macros\/reparse.h:258:6@

    __exported by:__ @macros\/reparse.h@
-}
noParams3_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ())
noParams3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_f87d967820876838

{-| __unique:__ @Example_get_funptr_ret1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e179cb34ae49459c" hs_bindgen_test_macrosreparse_e179cb34ae49459c ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO ()))))

{-# NOINLINE funptr_ret1_ptr #-}

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h:262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO ())))
funptr_ret1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_e179cb34ae49459c

{-| __unique:__ @Example_get_funptr_ret2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_fee89ee9abb96b9a" hs_bindgen_test_macrosreparse_fee89ee9abb96b9a ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO FC.CInt))))

{-# NOINLINE funptr_ret2_ptr #-}

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h:263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO FC.CInt)))
funptr_ret2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_fee89ee9abb96b9a

{-| __unique:__ @Example_get_funptr_ret3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_37b12e354695a3ec" hs_bindgen_test_macrosreparse_37b12e354695a3ec ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> IO ()))))

{-# NOINLINE funptr_ret3_ptr #-}

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h:264:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> IO ())))
funptr_ret3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_37b12e354695a3ec

{-| __unique:__ @Example_get_funptr_ret4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_8cf98ff5ff321106" hs_bindgen_test_macrosreparse_8cf98ff5ff321106 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar))))

{-# NOINLINE funptr_ret4_ptr #-}

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h:265:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)))
funptr_ret4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_8cf98ff5ff321106

{-| __unique:__ @Example_get_funptr_ret5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_9cc98e8fa059dba7" hs_bindgen_test_macrosreparse_9cc98e8fa059dba7 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret5_ptr #-}

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h:269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_9cc98e8fa059dba7

{-| __unique:__ @Example_get_funptr_ret6_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_eca290f9e8706bbd" hs_bindgen_test_macrosreparse_eca290f9e8706bbd ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret6_ptr #-}

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h:270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_eca290f9e8706bbd

{-| __unique:__ @Example_get_funptr_ret7_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_64208169decd5404" hs_bindgen_test_macrosreparse_64208169decd5404 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret7_ptr #-}

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h:271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_64208169decd5404

{-| __unique:__ @Example_get_funptr_ret8_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_719cc70cf1f95293" hs_bindgen_test_macrosreparse_719cc70cf1f95293 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret8_ptr #-}

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h:272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_719cc70cf1f95293

{-| __unique:__ @Example_get_funptr_ret9_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c7dd3791224cb869" hs_bindgen_test_macrosreparse_c7dd3791224cb869 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret9_ptr #-}

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h:273:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_c7dd3791224cb869

{-| __unique:__ @Example_get_funptr_ret10_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_dbf4b490a14f9fe7" hs_bindgen_test_macrosreparse_dbf4b490a14f9fe7 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret10_ptr #-}

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h:274:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosreparse_dbf4b490a14f9fe7
