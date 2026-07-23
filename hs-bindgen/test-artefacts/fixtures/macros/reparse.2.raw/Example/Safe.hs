{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.args_char1
    , Example.Safe.args_char2
    , Example.Safe.args_char3
    , Example.Safe.args_short1
    , Example.Safe.args_short2
    , Example.Safe.args_short3
    , Example.Safe.args_int1
    , Example.Safe.args_int2
    , Example.Safe.args_int3
    , Example.Safe.args_long1
    , Example.Safe.args_long2
    , Example.Safe.args_long3
    , Example.Safe.args_float
    , Example.Safe.args_double
    , Example.Safe.args_bool1
    , Example.Safe.args_struct
    , Example.Safe.args_union
    , Example.Safe.args_enum
    , Example.Safe.args_pointer1
    , Example.Safe.args_pointer2
    , Example.Safe.args_pointer3
    , Example.Safe.ret_A
    , Example.Safe.ret_char1
    , Example.Safe.ret_char2
    , Example.Safe.ret_char3
    , Example.Safe.ret_short1
    , Example.Safe.ret_short2
    , Example.Safe.ret_short3
    , Example.Safe.ret_int1
    , Example.Safe.ret_int2
    , Example.Safe.ret_int3
    , Example.Safe.ret_long1
    , Example.Safe.ret_long2
    , Example.Safe.ret_long3
    , Example.Safe.ret_float
    , Example.Safe.ret_double
    , Example.Safe.ret_bool1
    , Example.Safe.ret_struct
    , Example.Safe.ret_union
    , Example.Safe.ret_enum
    , Example.Safe.ret_pointer1
    , Example.Safe.ret_pointer2
    , Example.Safe.ret_pointer3
    , Example.Safe.body1
    , Example.Safe.body2
    , Example.Safe.args_complex_float
    , Example.Safe.args_complex_double
    , Example.Safe.ret_complex_float
    , Example.Safe.ret_complex_double
    , Example.Safe.bespoke_args1
    , Example.Safe.bespoke_args2
    , Example.Safe.bespoke_ret1
    , Example.Safe.bespoke_ret2
    , Example.Safe.arr_args1
    , Example.Safe.arr_args2
    , Example.Safe.arr_args3
    , Example.Safe.arr_args4
    , Example.Safe.funptr_args1
    , Example.Safe.funptr_args2
    , Example.Safe.funptr_args3
    , Example.Safe.funptr_args4
    , Example.Safe.funptr_args5
    , Example.Safe.comments1
    , Example.Safe.const_prim_before1
    , Example.Safe.const_prim_before2
    , Example.Safe.const_prim_before3
    , Example.Safe.const_prim_after1
    , Example.Safe.const_prim_after2
    , Example.Safe.const_prim_after3
    , Example.Safe.const_withoutSign_before1
    , Example.Safe.const_withoutSign_before2
    , Example.Safe.const_withoutSign_before3
    , Example.Safe.const_withoutSign_before4
    , Example.Safe.const_withoutSign_before5
    , Example.Safe.const_withoutSign_before6
    , Example.Safe.const_withoutSign_before7
    , Example.Safe.const_withoutSign_before8
    , Example.Safe.const_withoutSign_after1
    , Example.Safe.const_withoutSign_after2
    , Example.Safe.const_withoutSign_after3
    , Example.Safe.const_withoutSign_after4
    , Example.Safe.const_withoutSign_after5
    , Example.Safe.const_withoutSign_after6
    , Example.Safe.const_withoutSign_after7
    , Example.Safe.const_withoutSign_after8
    , Example.Safe.const_pointers_args1
    , Example.Safe.const_pointers_args2
    , Example.Safe.const_pointers_args3
    , Example.Safe.const_pointers_args4
    , Example.Safe.const_pointers_args5
    , Example.Safe.const_pointers_ret1
    , Example.Safe.const_pointers_ret2
    , Example.Safe.const_pointers_ret3
    , Example.Safe.const_pointers_ret4
    , Example.Safe.const_pointers_ret5
    , Example.Safe.const_array_elem1
    , Example.Safe.const_array_elem2
    , Example.Safe.const_array_elem3
    , Example.Safe.noParams1
    , Example.Safe.noParams2
    , Example.Safe.noParams3
    , Example.Safe.funptr_ret1
    , Example.Safe.funptr_ret2
    , Example.Safe.funptr_ret3
    , Example.Safe.funptr_ret4
    , Example.Safe.funptr_ret5
    , Example.Safe.funptr_ret6
    , Example.Safe.funptr_ret7
    , Example.Safe.funptr_ret8
    , Example.Safe.funptr_ret9
    , Example.Safe.funptr_ret10
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
  , "void hs_bindgen_14b56322053baa3f ("
  , "  signed int arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  (args_char1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_05193644052d4324 ("
  , "  signed int arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  (args_char2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_5b68ff22474d4775 ("
  , "  signed int arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  (args_char3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_5a7d0b078ac418a1 ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6d391e69fc74dd4b ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d3d74caa6f6d43ab ("
  , "  signed int arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  (args_short3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d005292650dc9976 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2dde5b36f080e07d ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e7207b4f601fa789 ("
  , "  signed int arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  (args_int3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_bc717d375990f9e5 ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_db1215183538b75a ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3ac74c7cbfc79685 ("
  , "  signed int arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  (args_long3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a2388e64c2a84027 ("
  , "  signed int arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  (args_float)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e5ad67302deb9698 ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  (args_double)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_bad730ef968d4941 ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (args_bool1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_24f835bba72f92a3 ("
  , "  signed int arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  (args_struct)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_d06661c0e9399cf0 ("
  , "  signed int arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  (args_union)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_d37243b7a8d9d58d ("
  , "  signed int arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  (args_enum)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b3bd6759641d67eb ("
  , "  signed int arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  (args_pointer1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_97b00bc8a470fc69 ("
  , "  signed int arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  (args_pointer2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_07e7d187d95f6ca2 ("
  , "  signed int arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  (args_pointer3)(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_144178893e488066 (void)"
  , "{"
  , "  return (ret_A)();"
  , "}"
  , "char hs_bindgen_d7d2bc68373545e4 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char1)(arg1);"
  , "}"
  , "signed char hs_bindgen_aa47ca542df45b7b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char2)(arg1);"
  , "}"
  , "unsigned char hs_bindgen_bcc15cd6faf7b787 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char3)(arg1);"
  , "}"
  , "signed short hs_bindgen_4ba7217a94e5e369 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short1)(arg1);"
  , "}"
  , "signed short hs_bindgen_ea4e41167ce3686a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short2)(arg1);"
  , "}"
  , "unsigned short hs_bindgen_b1021a4826d326a1 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short3)(arg1);"
  , "}"
  , "signed int hs_bindgen_c23956482c29e1c5 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int1)(arg1);"
  , "}"
  , "signed int hs_bindgen_76475f626bf1d429 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int2)(arg1);"
  , "}"
  , "unsigned int hs_bindgen_e86e93b147924e4a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int3)(arg1);"
  , "}"
  , "signed long hs_bindgen_2e5e9b9d09a32b33 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long1)(arg1);"
  , "}"
  , "signed long hs_bindgen_9541cb65d260ceec ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long2)(arg1);"
  , "}"
  , "unsigned long hs_bindgen_fca665017c551d32 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long3)(arg1);"
  , "}"
  , "float hs_bindgen_06152a76d70abec3 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_float)(arg1);"
  , "}"
  , "double hs_bindgen_fcbb5b65a6e8dee4 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_double)(arg1);"
  , "}"
  , "_Bool hs_bindgen_d2265e448c2759e9 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_bool1)(arg1);"
  , "}"
  , "void hs_bindgen_e6010e994241dd23 ("
  , "  signed int arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_struct)(arg1);"
  , "}"
  , "void hs_bindgen_00be97ea5e3606e4 ("
  , "  signed int arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_union)(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_7ad2540b7cf6b817 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_enum)(arg1);"
  , "}"
  , "signed int *hs_bindgen_6dba6060a7ca10de ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer1)(arg1);"
  , "}"
  , "signed int **hs_bindgen_7af2522218790e65 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer2)(arg1);"
  , "}"
  , "void *hs_bindgen_88562aa8f53d0780 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer3)(arg1);"
  , "}"
  , "signed int hs_bindgen_b9b614c706b250e5 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (body1)(arg1);"
  , "}"
  , "signed int hs_bindgen_b0559ae62627b030 (void)"
  , "{"
  , "  return (body2)();"
  , "}"
  , "void hs_bindgen_ddfc5cefdbda545b ("
  , "  signed int arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_float)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_cbf4ca3c6e7f80aa ("
  , "  signed int arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_double)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_0f1f6ea0917ce0ca ("
  , "  signed int arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_float)(arg1);"
  , "}"
  , "void hs_bindgen_955f1c0b383d9a7c ("
  , "  signed int arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_double)(arg1);"
  , "}"
  , "void hs_bindgen_61695282305a53ac ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (bespoke_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4c244649666e88b3 ("
  , "  signed int arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  (bespoke_args2)(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_1b4cd50ca08ff13a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret1)(arg1);"
  , "}"
  , "size_t hs_bindgen_71b4f4a34ec749c5 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret2)(arg1);"
  , "}"
  , "void hs_bindgen_8588314c8f45b7e1 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (arr_args1)(arg1);"
  , "}"
  , "void hs_bindgen_2eb08d216276c1b7 ("
  , "  signed int **arg1"
  , ")"
  , "{"
  , "  (arr_args2)(arg1);"
  , "}"
  , "void hs_bindgen_9e0ebe19b98832b7 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (arr_args3)(arg1);"
  , "}"
  , "void hs_bindgen_46bbe5c4f9a7db28 ("
  , "  signed int **arg1"
  , ")"
  , "{"
  , "  (arr_args4)(arg1);"
  , "}"
  , "void hs_bindgen_38985300d47b4487 ("
  , "  signed int arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_23d1626952d2a132 ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_69073b33110db81a ("
  , "  signed int arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9e6495d8345f7848 ("
  , "  signed int arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d5ab1b462ff6c176 ("
  , "  signed int arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args5)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_17cfdc4afa81f262 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (comments1)(arg1);"
  , "}"
  , "void hs_bindgen_62dad052d7ac99e6 ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6a954623e8847292 ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_5c4e5530b1f695b7 ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d219b88b917d72cf ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_79a466a4f64a4db1 ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a13884c8125a5f80 ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_5433c1896cba42d3 ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8300ce11589589f2 ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_be304ec839983d7b ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_445a2c238c734060 ("
  , "  signed int arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_b5a05c2c1589c243 ("
  , "  signed int arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_8217e6e9e19e7627 ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_cbcb3411df130b42 ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_fb0ddd12de072fd8 ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_91e81e2ec29aa772 ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6703a37e4fd098b1 ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_054259024219dbcc ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_eb4880610136c7bb ("
  , "  signed int arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_c0478c5f65c7e32e ("
  , "  signed int arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_764dd29ada03ac05 ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_62f2db62740aa47c ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f9349cbf0c433984 ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_05103ac5c64a70c5 ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2adbdab03502d7c3 ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_02735f713f3663a0 ("
  , "  signed int arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_7be4b56f499736ec ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_07806340121e9d7a ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args5)(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_964ecbd6daab3ac8 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret1)(arg1);"
  , "}"
  , "signed int const *hs_bindgen_425aa26fa3f15a85 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret2)(arg1);"
  , "}"
  , "signed int *const hs_bindgen_35dc6a2376f4669c ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret3)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_b4704a8e3fa5c1a7 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret4)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_eee06f696926bceb ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret5)(arg1);"
  , "}"
  , "void hs_bindgen_85897c18c4db8c38 ("
  , "  signed int const *arg1"
  , ")"
  , "{"
  , "  (const_array_elem1)(arg1);"
  , "}"
  , "void hs_bindgen_7d19c28372388bd5 ("
  , "  signed int const **arg1"
  , ")"
  , "{"
  , "  (const_array_elem2)(arg1);"
  , "}"
  , "void hs_bindgen_b72db4cfe06fec75 ("
  , "  signed int *const *arg1"
  , ")"
  , "{"
  , "  (const_array_elem3)(arg1);"
  , "}"
  , "signed int hs_bindgen_8c2d8b6c768235a6 (void)"
  , "{"
  , "  return (noParams1)();"
  , "}"
  , "signed int hs_bindgen_f406093f4244470f (void)"
  , "{"
  , "  return (noParams2)();"
  , "}"
  , "void hs_bindgen_7cfe278ffc125a35 ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (noParams3)(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_6f95516796104481 ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_c823e0311ec509b4 ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret2)(arg1);"
  , "}"
  , "void (*hs_bindgen_94550ec4aba19f7b ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (funptr_ret3)(arg1);"
  , "}"
  , "char (*hs_bindgen_ed3e6ebe628da423 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret4)(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_cb5a94e6610ff83d ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret5)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_8cd3005394354b86 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret6)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_0f1393247fcdeb7a ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret7)(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_7da05d4b5c843853 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret8)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_f6f304c980603aae ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret9)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_263377d0e4be3901 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret10)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_char1@
foreign import ccall safe "hs_bindgen_14b56322053baa3f" hs_bindgen_14b56322053baa3f_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_char1@
hs_bindgen_14b56322053baa3f ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_14b56322053baa3f =
  BG.fromFFIType hs_bindgen_14b56322053baa3f_base

{-| Function declarations

    __C declaration:__ @args_char1@

    __defined at:__ @macros\/reparse.h 17:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_char1 = hs_bindgen_14b56322053baa3f

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_char2@
foreign import ccall safe "hs_bindgen_05193644052d4324" hs_bindgen_05193644052d4324_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_char2@
hs_bindgen_05193644052d4324 ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_05193644052d4324 =
  BG.fromFFIType hs_bindgen_05193644052d4324_base

{-| __C declaration:__ @args_char2@

    __defined at:__ @macros\/reparse.h 18:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CSChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_char2 = hs_bindgen_05193644052d4324

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_char3@
foreign import ccall safe "hs_bindgen_5b68ff22474d4775" hs_bindgen_5b68ff22474d4775_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_char3@
hs_bindgen_5b68ff22474d4775 ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_5b68ff22474d4775 =
  BG.fromFFIType hs_bindgen_5b68ff22474d4775_base

{-| __C declaration:__ @args_char3@

    __defined at:__ @macros\/reparse.h 19:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CUChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_char3 = hs_bindgen_5b68ff22474d4775

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_short1@
foreign import ccall safe "hs_bindgen_5a7d0b078ac418a1" hs_bindgen_5a7d0b078ac418a1_base ::
     BG.Int32
  -> BG.Int16
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_short1@
hs_bindgen_5a7d0b078ac418a1 ::
     BG.CInt
  -> BG.CShort
  -> IO ()
hs_bindgen_5a7d0b078ac418a1 =
  BG.fromFFIType hs_bindgen_5a7d0b078ac418a1_base

{-| __C declaration:__ @args_short1@

    __defined at:__ @macros\/reparse.h 21:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CShort
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_short1 = hs_bindgen_5a7d0b078ac418a1

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_short2@
foreign import ccall safe "hs_bindgen_6d391e69fc74dd4b" hs_bindgen_6d391e69fc74dd4b_base ::
     BG.Int32
  -> BG.Int16
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_short2@
hs_bindgen_6d391e69fc74dd4b ::
     BG.CInt
  -> BG.CShort
  -> IO ()
hs_bindgen_6d391e69fc74dd4b =
  BG.fromFFIType hs_bindgen_6d391e69fc74dd4b_base

{-| __C declaration:__ @args_short2@

    __defined at:__ @macros\/reparse.h 22:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CShort
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_short2 = hs_bindgen_6d391e69fc74dd4b

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_short3@
foreign import ccall safe "hs_bindgen_d3d74caa6f6d43ab" hs_bindgen_d3d74caa6f6d43ab_base ::
     BG.Int32
  -> BG.Word16
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_short3@
hs_bindgen_d3d74caa6f6d43ab ::
     BG.CInt
  -> BG.CUShort
  -> IO ()
hs_bindgen_d3d74caa6f6d43ab =
  BG.fromFFIType hs_bindgen_d3d74caa6f6d43ab_base

{-| __C declaration:__ @args_short3@

    __defined at:__ @macros\/reparse.h 23:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CUShort
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_short3 = hs_bindgen_d3d74caa6f6d43ab

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_int1@
foreign import ccall safe "hs_bindgen_d005292650dc9976" hs_bindgen_d005292650dc9976_base ::
     BG.Int32
  -> BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_int1@
hs_bindgen_d005292650dc9976 ::
     BG.CInt
  -> BG.CInt
  -> IO ()
hs_bindgen_d005292650dc9976 =
  BG.fromFFIType hs_bindgen_d005292650dc9976_base

{-| __C declaration:__ @args_int1@

    __defined at:__ @macros\/reparse.h 25:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_int1 = hs_bindgen_d005292650dc9976

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_int2@
foreign import ccall safe "hs_bindgen_2dde5b36f080e07d" hs_bindgen_2dde5b36f080e07d_base ::
     BG.Int32
  -> BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_int2@
hs_bindgen_2dde5b36f080e07d ::
     BG.CInt
  -> BG.CInt
  -> IO ()
hs_bindgen_2dde5b36f080e07d =
  BG.fromFFIType hs_bindgen_2dde5b36f080e07d_base

{-| __C declaration:__ @args_int2@

    __defined at:__ @macros\/reparse.h 26:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_int2 = hs_bindgen_2dde5b36f080e07d

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_int3@
foreign import ccall safe "hs_bindgen_e7207b4f601fa789" hs_bindgen_e7207b4f601fa789_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_int3@
hs_bindgen_e7207b4f601fa789 ::
     BG.CInt
  -> BG.CUInt
  -> IO ()
hs_bindgen_e7207b4f601fa789 =
  BG.fromFFIType hs_bindgen_e7207b4f601fa789_base

{-| __C declaration:__ @args_int3@

    __defined at:__ @macros\/reparse.h 27:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CUInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_int3 = hs_bindgen_e7207b4f601fa789

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_long1@
foreign import ccall safe "hs_bindgen_bc717d375990f9e5" hs_bindgen_bc717d375990f9e5_base ::
     BG.Int32
  -> BG.Int64
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_long1@
hs_bindgen_bc717d375990f9e5 ::
     BG.CInt
  -> BG.CLong
  -> IO ()
hs_bindgen_bc717d375990f9e5 =
  BG.fromFFIType hs_bindgen_bc717d375990f9e5_base

{-| __C declaration:__ @args_long1@

    __defined at:__ @macros\/reparse.h 29:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CLong
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_long1 = hs_bindgen_bc717d375990f9e5

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_long2@
foreign import ccall safe "hs_bindgen_db1215183538b75a" hs_bindgen_db1215183538b75a_base ::
     BG.Int32
  -> BG.Int64
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_long2@
hs_bindgen_db1215183538b75a ::
     BG.CInt
  -> BG.CLong
  -> IO ()
hs_bindgen_db1215183538b75a =
  BG.fromFFIType hs_bindgen_db1215183538b75a_base

{-| __C declaration:__ @args_long2@

    __defined at:__ @macros\/reparse.h 30:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CLong
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_long2 = hs_bindgen_db1215183538b75a

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_long3@
foreign import ccall safe "hs_bindgen_3ac74c7cbfc79685" hs_bindgen_3ac74c7cbfc79685_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_long3@
hs_bindgen_3ac74c7cbfc79685 ::
     BG.CInt
  -> BG.CULong
  -> IO ()
hs_bindgen_3ac74c7cbfc79685 =
  BG.fromFFIType hs_bindgen_3ac74c7cbfc79685_base

{-| __C declaration:__ @args_long3@

    __defined at:__ @macros\/reparse.h 31:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CULong
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_long3 = hs_bindgen_3ac74c7cbfc79685

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_float@
foreign import ccall safe "hs_bindgen_a2388e64c2a84027" hs_bindgen_a2388e64c2a84027_base ::
     BG.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_float@
hs_bindgen_a2388e64c2a84027 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_a2388e64c2a84027 =
  BG.fromFFIType hs_bindgen_a2388e64c2a84027_base

{-| __C declaration:__ @args_float@

    __defined at:__ @macros\/reparse.h 33:6@

    __exported by:__ @macros\/reparse.h@
-}
args_float ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_float = hs_bindgen_a2388e64c2a84027

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_double@
foreign import ccall safe "hs_bindgen_e5ad67302deb9698" hs_bindgen_e5ad67302deb9698_base ::
     BG.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_double@
hs_bindgen_e5ad67302deb9698 ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_e5ad67302deb9698 =
  BG.fromFFIType hs_bindgen_e5ad67302deb9698_base

{-| __C declaration:__ @args_double@

    __defined at:__ @macros\/reparse.h 34:6@

    __exported by:__ @macros\/reparse.h@
-}
args_double ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_double = hs_bindgen_e5ad67302deb9698

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_bool1@
foreign import ccall safe "hs_bindgen_bad730ef968d4941" hs_bindgen_bad730ef968d4941_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_bool1@
hs_bindgen_bad730ef968d4941 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_bad730ef968d4941 =
  BG.fromFFIType hs_bindgen_bad730ef968d4941_base

{-| __C declaration:__ @args_bool1@

    __defined at:__ @macros\/reparse.h 35:6@

    __exported by:__ @macros\/reparse.h@
-}
args_bool1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_bool1 = hs_bindgen_bad730ef968d4941

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_struct@
foreign import ccall safe "hs_bindgen_24f835bba72f92a3" hs_bindgen_24f835bba72f92a3_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_struct@
hs_bindgen_24f835bba72f92a3 ::
     BG.CInt
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_24f835bba72f92a3 =
  BG.fromFFIType hs_bindgen_24f835bba72f92a3_base

{-| __C declaration:__ @args_struct@

    __defined at:__ @macros\/reparse.h 37:6@

    __exported by:__ @macros\/reparse.h@
-}
args_struct ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> Some_struct
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_struct =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_24f835bba72f92a3 arg10 arg22)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_union@
foreign import ccall safe "hs_bindgen_d06661c0e9399cf0" hs_bindgen_d06661c0e9399cf0_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_union@
hs_bindgen_d06661c0e9399cf0 ::
     BG.CInt
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_d06661c0e9399cf0 =
  BG.fromFFIType hs_bindgen_d06661c0e9399cf0_base

{-| __C declaration:__ @args_union@

    __defined at:__ @macros\/reparse.h 38:6@

    __exported by:__ @macros\/reparse.h@
-}
args_union ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> Some_union
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_union =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_d06661c0e9399cf0 arg10 arg22)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_enum@
foreign import ccall safe "hs_bindgen_d37243b7a8d9d58d" hs_bindgen_d37243b7a8d9d58d_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_enum@
hs_bindgen_d37243b7a8d9d58d ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_d37243b7a8d9d58d =
  BG.fromFFIType hs_bindgen_d37243b7a8d9d58d_base

{-| __C declaration:__ @args_enum@

    __defined at:__ @macros\/reparse.h 39:6@

    __exported by:__ @macros\/reparse.h@
-}
args_enum ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> Some_enum
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_enum = hs_bindgen_d37243b7a8d9d58d

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_pointer1@
foreign import ccall safe "hs_bindgen_b3bd6759641d67eb" hs_bindgen_b3bd6759641d67eb_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_pointer1@
hs_bindgen_b3bd6759641d67eb ::
     BG.CInt
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_b3bd6759641d67eb =
  BG.fromFFIType hs_bindgen_b3bd6759641d67eb_base

{-| __C declaration:__ @args_pointer1@

    __defined at:__ @macros\/reparse.h 41:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.Ptr BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_pointer1 = hs_bindgen_b3bd6759641d67eb

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_pointer2@
foreign import ccall safe "hs_bindgen_97b00bc8a470fc69" hs_bindgen_97b00bc8a470fc69_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_pointer2@
hs_bindgen_97b00bc8a470fc69 ::
     BG.CInt
  -> BG.Ptr (BG.Ptr BG.CInt)
  -> IO ()
hs_bindgen_97b00bc8a470fc69 =
  BG.fromFFIType hs_bindgen_97b00bc8a470fc69_base

{-| __C declaration:__ @args_pointer2@

    __defined at:__ @macros\/reparse.h 42:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.Ptr (BG.Ptr BG.CInt)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_pointer2 = hs_bindgen_97b00bc8a470fc69

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_pointer3@
foreign import ccall safe "hs_bindgen_07e7d187d95f6ca2" hs_bindgen_07e7d187d95f6ca2_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_pointer3@
hs_bindgen_07e7d187d95f6ca2 ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()
hs_bindgen_07e7d187d95f6ca2 =
  BG.fromFFIType hs_bindgen_07e7d187d95f6ca2_base

{-| __C declaration:__ @args_pointer3@

    __defined at:__ @macros\/reparse.h 43:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.Ptr BG.Void
     -- ^ __C declaration:__ @arg3@
  -> IO ()
args_pointer3 = hs_bindgen_07e7d187d95f6ca2

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_A@
foreign import ccall safe "hs_bindgen_144178893e488066" hs_bindgen_144178893e488066_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_A@
hs_bindgen_144178893e488066 :: IO BG.CInt
hs_bindgen_144178893e488066 =
  BG.fromFFIType hs_bindgen_144178893e488066_base

{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h 47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A :: IO BG.CInt
ret_A = hs_bindgen_144178893e488066

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_char1@
foreign import ccall safe "hs_bindgen_d7d2bc68373545e4" hs_bindgen_d7d2bc68373545e4_base ::
     BG.Int32
  -> IO BG.Int8

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_char1@
hs_bindgen_d7d2bc68373545e4 ::
     BG.CInt
  -> IO BG.CChar
hs_bindgen_d7d2bc68373545e4 =
  BG.fromFFIType hs_bindgen_d7d2bc68373545e4_base

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h 49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CChar
ret_char1 = hs_bindgen_d7d2bc68373545e4

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_char2@
foreign import ccall safe "hs_bindgen_aa47ca542df45b7b" hs_bindgen_aa47ca542df45b7b_base ::
     BG.Int32
  -> IO BG.Int8

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_char2@
hs_bindgen_aa47ca542df45b7b ::
     BG.CInt
  -> IO BG.CSChar
hs_bindgen_aa47ca542df45b7b =
  BG.fromFFIType hs_bindgen_aa47ca542df45b7b_base

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h 50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CSChar
ret_char2 = hs_bindgen_aa47ca542df45b7b

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_char3@
foreign import ccall safe "hs_bindgen_bcc15cd6faf7b787" hs_bindgen_bcc15cd6faf7b787_base ::
     BG.Int32
  -> IO BG.Word8

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_char3@
hs_bindgen_bcc15cd6faf7b787 ::
     BG.CInt
  -> IO BG.CUChar
hs_bindgen_bcc15cd6faf7b787 =
  BG.fromFFIType hs_bindgen_bcc15cd6faf7b787_base

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h 51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUChar
ret_char3 = hs_bindgen_bcc15cd6faf7b787

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_short1@
foreign import ccall safe "hs_bindgen_4ba7217a94e5e369" hs_bindgen_4ba7217a94e5e369_base ::
     BG.Int32
  -> IO BG.Int16

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_short1@
hs_bindgen_4ba7217a94e5e369 ::
     BG.CInt
  -> IO BG.CShort
hs_bindgen_4ba7217a94e5e369 =
  BG.fromFFIType hs_bindgen_4ba7217a94e5e369_base

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h 53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short1 = hs_bindgen_4ba7217a94e5e369

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_short2@
foreign import ccall safe "hs_bindgen_ea4e41167ce3686a" hs_bindgen_ea4e41167ce3686a_base ::
     BG.Int32
  -> IO BG.Int16

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_short2@
hs_bindgen_ea4e41167ce3686a ::
     BG.CInt
  -> IO BG.CShort
hs_bindgen_ea4e41167ce3686a =
  BG.fromFFIType hs_bindgen_ea4e41167ce3686a_base

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h 54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short2 = hs_bindgen_ea4e41167ce3686a

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_short3@
foreign import ccall safe "hs_bindgen_b1021a4826d326a1" hs_bindgen_b1021a4826d326a1_base ::
     BG.Int32
  -> IO BG.Word16

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_short3@
hs_bindgen_b1021a4826d326a1 ::
     BG.CInt
  -> IO BG.CUShort
hs_bindgen_b1021a4826d326a1 =
  BG.fromFFIType hs_bindgen_b1021a4826d326a1_base

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h 55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUShort
ret_short3 = hs_bindgen_b1021a4826d326a1

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_int1@
foreign import ccall safe "hs_bindgen_c23956482c29e1c5" hs_bindgen_c23956482c29e1c5_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_int1@
hs_bindgen_c23956482c29e1c5 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_c23956482c29e1c5 =
  BG.fromFFIType hs_bindgen_c23956482c29e1c5_base

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h 57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int1 = hs_bindgen_c23956482c29e1c5

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_int2@
foreign import ccall safe "hs_bindgen_76475f626bf1d429" hs_bindgen_76475f626bf1d429_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_int2@
hs_bindgen_76475f626bf1d429 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_76475f626bf1d429 =
  BG.fromFFIType hs_bindgen_76475f626bf1d429_base

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h 58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int2 = hs_bindgen_76475f626bf1d429

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_int3@
foreign import ccall safe "hs_bindgen_e86e93b147924e4a" hs_bindgen_e86e93b147924e4a_base ::
     BG.Int32
  -> IO BG.Word32

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_int3@
hs_bindgen_e86e93b147924e4a ::
     BG.CInt
  -> IO BG.CUInt
hs_bindgen_e86e93b147924e4a =
  BG.fromFFIType hs_bindgen_e86e93b147924e4a_base

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h 59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUInt
ret_int3 = hs_bindgen_e86e93b147924e4a

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_long1@
foreign import ccall safe "hs_bindgen_2e5e9b9d09a32b33" hs_bindgen_2e5e9b9d09a32b33_base ::
     BG.Int32
  -> IO BG.Int64

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_long1@
hs_bindgen_2e5e9b9d09a32b33 ::
     BG.CInt
  -> IO BG.CLong
hs_bindgen_2e5e9b9d09a32b33 =
  BG.fromFFIType hs_bindgen_2e5e9b9d09a32b33_base

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h 61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long1 = hs_bindgen_2e5e9b9d09a32b33

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_long2@
foreign import ccall safe "hs_bindgen_9541cb65d260ceec" hs_bindgen_9541cb65d260ceec_base ::
     BG.Int32
  -> IO BG.Int64

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_long2@
hs_bindgen_9541cb65d260ceec ::
     BG.CInt
  -> IO BG.CLong
hs_bindgen_9541cb65d260ceec =
  BG.fromFFIType hs_bindgen_9541cb65d260ceec_base

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h 62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long2 = hs_bindgen_9541cb65d260ceec

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_long3@
foreign import ccall safe "hs_bindgen_fca665017c551d32" hs_bindgen_fca665017c551d32_base ::
     BG.Int32
  -> IO BG.Word64

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_long3@
hs_bindgen_fca665017c551d32 ::
     BG.CInt
  -> IO BG.CULong
hs_bindgen_fca665017c551d32 =
  BG.fromFFIType hs_bindgen_fca665017c551d32_base

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h 63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CULong
ret_long3 = hs_bindgen_fca665017c551d32

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_float@
foreign import ccall safe "hs_bindgen_06152a76d70abec3" hs_bindgen_06152a76d70abec3_base ::
     BG.Int32
  -> IO Float

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_float@
hs_bindgen_06152a76d70abec3 ::
     BG.CInt
  -> IO BG.CFloat
hs_bindgen_06152a76d70abec3 =
  BG.fromFFIType hs_bindgen_06152a76d70abec3_base

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h 65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CFloat
ret_float = hs_bindgen_06152a76d70abec3

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_double@
foreign import ccall safe "hs_bindgen_fcbb5b65a6e8dee4" hs_bindgen_fcbb5b65a6e8dee4_base ::
     BG.Int32
  -> IO Double

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_double@
hs_bindgen_fcbb5b65a6e8dee4 ::
     BG.CInt
  -> IO BG.CDouble
hs_bindgen_fcbb5b65a6e8dee4 =
  BG.fromFFIType hs_bindgen_fcbb5b65a6e8dee4_base

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h 66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CDouble
ret_double = hs_bindgen_fcbb5b65a6e8dee4

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_bool1@
foreign import ccall safe "hs_bindgen_d2265e448c2759e9" hs_bindgen_d2265e448c2759e9_base ::
     BG.Int32
  -> IO BG.Word8

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_bool1@
hs_bindgen_d2265e448c2759e9 ::
     BG.CInt
  -> IO BG.CBool
hs_bindgen_d2265e448c2759e9 =
  BG.fromFFIType hs_bindgen_d2265e448c2759e9_base

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h 67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
ret_bool1 = hs_bindgen_d2265e448c2759e9

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_struct@
foreign import ccall safe "hs_bindgen_e6010e994241dd23" hs_bindgen_e6010e994241dd23_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_struct@
hs_bindgen_e6010e994241dd23 ::
     BG.CInt
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_e6010e994241dd23 =
  BG.fromFFIType hs_bindgen_e6010e994241dd23_base

{-| __C declaration:__ @ret_struct@

    __defined at:__ @macros\/reparse.h 69:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_struct ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO Some_struct
ret_struct =
  \arg10 ->
    BG.allocaAndPeek (\res1 ->
                        hs_bindgen_e6010e994241dd23 arg10 res1)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_union@
foreign import ccall safe "hs_bindgen_00be97ea5e3606e4" hs_bindgen_00be97ea5e3606e4_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_union@
hs_bindgen_00be97ea5e3606e4 ::
     BG.CInt
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_00be97ea5e3606e4 =
  BG.fromFFIType hs_bindgen_00be97ea5e3606e4_base

{-| __C declaration:__ @ret_union@

    __defined at:__ @macros\/reparse.h 70:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_union ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO Some_union
ret_union =
  \arg10 ->
    BG.allocaAndPeek (\res1 ->
                        hs_bindgen_00be97ea5e3606e4 arg10 res1)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_enum@
foreign import ccall safe "hs_bindgen_7ad2540b7cf6b817" hs_bindgen_7ad2540b7cf6b817_base ::
     BG.Int32
  -> IO BG.Word32

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_enum@
hs_bindgen_7ad2540b7cf6b817 ::
     BG.CInt
  -> IO Some_enum
hs_bindgen_7ad2540b7cf6b817 =
  BG.fromFFIType hs_bindgen_7ad2540b7cf6b817_base

{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h 71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO Some_enum
ret_enum = hs_bindgen_7ad2540b7cf6b817

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_pointer1@
foreign import ccall safe "hs_bindgen_6dba6060a7ca10de" hs_bindgen_6dba6060a7ca10de_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_pointer1@
hs_bindgen_6dba6060a7ca10de ::
     BG.CInt
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_6dba6060a7ca10de =
  BG.fromFFIType hs_bindgen_6dba6060a7ca10de_base

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h 73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
ret_pointer1 = hs_bindgen_6dba6060a7ca10de

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_pointer2@
foreign import ccall safe "hs_bindgen_7af2522218790e65" hs_bindgen_7af2522218790e65_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_pointer2@
hs_bindgen_7af2522218790e65 ::
     BG.CInt
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
hs_bindgen_7af2522218790e65 =
  BG.fromFFIType hs_bindgen_7af2522218790e65_base

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h 74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
ret_pointer2 = hs_bindgen_7af2522218790e65

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_pointer3@
foreign import ccall safe "hs_bindgen_88562aa8f53d0780" hs_bindgen_88562aa8f53d0780_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_pointer3@
hs_bindgen_88562aa8f53d0780 ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)
hs_bindgen_88562aa8f53d0780 =
  BG.fromFFIType hs_bindgen_88562aa8f53d0780_base

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h 75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.Void)
ret_pointer3 = hs_bindgen_88562aa8f53d0780

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_body1@
foreign import ccall safe "hs_bindgen_b9b614c706b250e5" hs_bindgen_b9b614c706b250e5_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_body1@
hs_bindgen_b9b614c706b250e5 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_b9b614c706b250e5 =
  BG.fromFFIType hs_bindgen_b9b614c706b250e5_base

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h 79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
body1 = hs_bindgen_b9b614c706b250e5

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_body2@
foreign import ccall safe "hs_bindgen_b0559ae62627b030" hs_bindgen_b0559ae62627b030_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_body2@
hs_bindgen_b0559ae62627b030 :: IO BG.CInt
hs_bindgen_b0559ae62627b030 =
  BG.fromFFIType hs_bindgen_b0559ae62627b030_base

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h 80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2 :: IO BG.CInt
body2 = hs_bindgen_b0559ae62627b030

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_complex_float@
foreign import ccall safe "hs_bindgen_ddfc5cefdbda545b" hs_bindgen_ddfc5cefdbda545b_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_complex_float@
hs_bindgen_ddfc5cefdbda545b ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_ddfc5cefdbda545b =
  BG.fromFFIType hs_bindgen_ddfc5cefdbda545b_base

{-| __C declaration:__ @args_complex_float@

    __defined at:__ @macros\/reparse.h 84:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_float ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.Complex BG.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_complex_float =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_ddfc5cefdbda545b arg10 arg22)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_complex_double@
foreign import ccall safe "hs_bindgen_cbf4ca3c6e7f80aa" hs_bindgen_cbf4ca3c6e7f80aa_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_args_complex_double@
hs_bindgen_cbf4ca3c6e7f80aa ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_cbf4ca3c6e7f80aa =
  BG.fromFFIType hs_bindgen_cbf4ca3c6e7f80aa_base

{-| __C declaration:__ @args_complex_double@

    __defined at:__ @macros\/reparse.h 85:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_double ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.Complex BG.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_complex_double =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_cbf4ca3c6e7f80aa arg10 arg22)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_complex_float@
foreign import ccall safe "hs_bindgen_0f1f6ea0917ce0ca" hs_bindgen_0f1f6ea0917ce0ca_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_complex_float@
hs_bindgen_0f1f6ea0917ce0ca ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_0f1f6ea0917ce0ca =
  BG.fromFFIType hs_bindgen_0f1f6ea0917ce0ca_base

{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @macros\/reparse.h 86:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_float ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Complex BG.CFloat)
ret_complex_float =
  \arg10 ->
    BG.allocaAndPeek (\res1 ->
                        hs_bindgen_0f1f6ea0917ce0ca arg10 res1)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_complex_double@
foreign import ccall safe "hs_bindgen_955f1c0b383d9a7c" hs_bindgen_955f1c0b383d9a7c_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_ret_complex_double@
hs_bindgen_955f1c0b383d9a7c ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_955f1c0b383d9a7c =
  BG.fromFFIType hs_bindgen_955f1c0b383d9a7c_base

{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @macros\/reparse.h 87:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_double ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Complex BG.CDouble)
ret_complex_double =
  \arg10 ->
    BG.allocaAndPeek (\res1 ->
                        hs_bindgen_955f1c0b383d9a7c arg10 res1)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_bespoke_args1@
foreign import ccall safe "hs_bindgen_61695282305a53ac" hs_bindgen_61695282305a53ac_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_bespoke_args1@
hs_bindgen_61695282305a53ac ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_61695282305a53ac =
  BG.fromFFIType hs_bindgen_61695282305a53ac_base

{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @macros\/reparse.h 94:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
bespoke_args1 = hs_bindgen_61695282305a53ac

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_bespoke_args2@
foreign import ccall safe "hs_bindgen_4c244649666e88b3" hs_bindgen_4c244649666e88b3_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_bespoke_args2@
hs_bindgen_4c244649666e88b3 ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_4c244649666e88b3 =
  BG.fromFFIType hs_bindgen_4c244649666e88b3_base

{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @macros\/reparse.h 95:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.LibC.CSize
     -- ^ __C declaration:__ @arg2@
  -> IO ()
bespoke_args2 = hs_bindgen_4c244649666e88b3

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_bespoke_ret1@
foreign import ccall safe "hs_bindgen_1b4cd50ca08ff13a" hs_bindgen_1b4cd50ca08ff13a_base ::
     BG.Int32
  -> IO BG.Word8

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_bespoke_ret1@
hs_bindgen_1b4cd50ca08ff13a ::
     BG.CInt
  -> IO BG.CBool
hs_bindgen_1b4cd50ca08ff13a =
  BG.fromFFIType hs_bindgen_1b4cd50ca08ff13a_base

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h 97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
bespoke_ret1 = hs_bindgen_1b4cd50ca08ff13a

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_bespoke_ret2@
foreign import ccall safe "hs_bindgen_71b4f4a34ec749c5" hs_bindgen_71b4f4a34ec749c5_base ::
     BG.Int32
  -> IO BG.Word64

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_bespoke_ret2@
hs_bindgen_71b4f4a34ec749c5 ::
     BG.CInt
  -> IO HsBindgen.Runtime.LibC.CSize
hs_bindgen_71b4f4a34ec749c5 =
  BG.fromFFIType hs_bindgen_71b4f4a34ec749c5_base

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h 98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO HsBindgen.Runtime.LibC.CSize
bespoke_ret2 = hs_bindgen_71b4f4a34ec749c5

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_arr_args1@
foreign import ccall safe "hs_bindgen_8588314c8f45b7e1" hs_bindgen_8588314c8f45b7e1_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_arr_args1@
hs_bindgen_8588314c8f45b7e1 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt))
  -> IO ()
hs_bindgen_8588314c8f45b7e1 =
  BG.fromFFIType hs_bindgen_8588314c8f45b7e1_base

{-| Arrays

    __C declaration:__ @arr_args1@

    __defined at:__ @macros\/reparse.h 104:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args1 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args1 = hs_bindgen_8588314c8f45b7e1

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_arr_args2@
foreign import ccall safe "hs_bindgen_2eb08d216276c1b7" hs_bindgen_2eb08d216276c1b7_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_arr_args2@
hs_bindgen_2eb08d216276c1b7 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_2eb08d216276c1b7 =
  BG.fromFFIType hs_bindgen_2eb08d216276c1b7_base

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h 105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args2 = hs_bindgen_2eb08d216276c1b7

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_arr_args3@
foreign import ccall safe "hs_bindgen_9e0ebe19b98832b7" hs_bindgen_9e0ebe19b98832b7_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_arr_args3@
hs_bindgen_9e0ebe19b98832b7 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt))
  -> IO ()
hs_bindgen_9e0ebe19b98832b7 =
  BG.fromFFIType hs_bindgen_9e0ebe19b98832b7_base

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h 106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args3 = hs_bindgen_9e0ebe19b98832b7

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_arr_args4@
foreign import ccall safe "hs_bindgen_46bbe5c4f9a7db28" hs_bindgen_46bbe5c4f9a7db28_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_arr_args4@
hs_bindgen_46bbe5c4f9a7db28 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_46bbe5c4f9a7db28 =
  BG.fromFFIType hs_bindgen_46bbe5c4f9a7db28_base

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h 107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args4 = hs_bindgen_46bbe5c4f9a7db28

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_args1@
foreign import ccall safe "hs_bindgen_38985300d47b4487" hs_bindgen_38985300d47b4487_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_args1@
hs_bindgen_38985300d47b4487 ::
     BG.CInt
  -> BG.FunPtr (IO ())
  -> IO ()
hs_bindgen_38985300d47b4487 =
  BG.fromFFIType hs_bindgen_38985300d47b4487_base

{-| Function pointers

    __C declaration:__ @funptr_args1@

    __defined at:__ @macros\/reparse.h 126:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.FunPtr (IO ())
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args1 = hs_bindgen_38985300d47b4487

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_args2@
foreign import ccall safe "hs_bindgen_23d1626952d2a132" hs_bindgen_23d1626952d2a132_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_args2@
hs_bindgen_23d1626952d2a132 ::
     BG.CInt
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_23d1626952d2a132 =
  BG.fromFFIType hs_bindgen_23d1626952d2a132_base

{-| __C declaration:__ @funptr_args2@

    __defined at:__ @macros\/reparse.h 127:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.FunPtr (IO BG.CInt)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args2 = hs_bindgen_23d1626952d2a132

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_args3@
foreign import ccall safe "hs_bindgen_69073b33110db81a" hs_bindgen_69073b33110db81a_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_args3@
hs_bindgen_69073b33110db81a ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> IO ())
  -> IO ()
hs_bindgen_69073b33110db81a =
  BG.fromFFIType hs_bindgen_69073b33110db81a_base

{-| __C declaration:__ @funptr_args3@

    __defined at:__ @macros\/reparse.h 128:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.FunPtr (BG.CInt -> IO ())
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args3 = hs_bindgen_69073b33110db81a

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_args4@
foreign import ccall safe "hs_bindgen_9e6495d8345f7848" hs_bindgen_9e6495d8345f7848_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_args4@
hs_bindgen_9e6495d8345f7848 ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar)
  -> IO ()
hs_bindgen_9e6495d8345f7848 =
  BG.fromFFIType hs_bindgen_9e6495d8345f7848_base

{-| __C declaration:__ @funptr_args4@

    __defined at:__ @macros\/reparse.h 129:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args4 = hs_bindgen_9e6495d8345f7848

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_args5@
foreign import ccall safe "hs_bindgen_d5ab1b462ff6c176" hs_bindgen_d5ab1b462ff6c176_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_args5@
hs_bindgen_d5ab1b462ff6c176 ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))
  -> IO ()
hs_bindgen_d5ab1b462ff6c176 =
  BG.fromFFIType hs_bindgen_d5ab1b462ff6c176_base

{-| __C declaration:__ @funptr_args5@

    __defined at:__ @macros\/reparse.h 130:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args5 = hs_bindgen_d5ab1b462ff6c176

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_comments1@
foreign import ccall safe "hs_bindgen_17cfdc4afa81f262" hs_bindgen_17cfdc4afa81f262_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_comments1@
hs_bindgen_17cfdc4afa81f262 ::
     BG.CInt
  -> IO ()
hs_bindgen_17cfdc4afa81f262 =
  BG.fromFFIType hs_bindgen_17cfdc4afa81f262_base

{-| Comments in awkward places

    (Prior to language-c we failed to parse there.)

    __C declaration:__ @comments1@

    __defined at:__ @macros\/reparse.h 144:25@

    __exported by:__ @macros\/reparse.h@
-}
comments1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO ()
comments1 = hs_bindgen_17cfdc4afa81f262

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_prim_before1@
foreign import ccall safe "hs_bindgen_62dad052d7ac99e6" hs_bindgen_62dad052d7ac99e6_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_prim_before1@
hs_bindgen_62dad052d7ac99e6 ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_62dad052d7ac99e6 =
  BG.fromFFIType hs_bindgen_62dad052d7ac99e6_base

{-| @const@ qualifier

    NOTE: These were not parsed correctly prior to the switch to language-c.

    __C declaration:__ @const_prim_before1@

    __defined at:__ @macros\/reparse.h 177:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_before1 = hs_bindgen_62dad052d7ac99e6

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_prim_before2@
foreign import ccall safe "hs_bindgen_6a954623e8847292" hs_bindgen_6a954623e8847292_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_prim_before2@
hs_bindgen_6a954623e8847292 ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_6a954623e8847292 =
  BG.fromFFIType hs_bindgen_6a954623e8847292_base

{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @macros\/reparse.h 178:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CSChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_before2 = hs_bindgen_6a954623e8847292

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_prim_before3@
foreign import ccall safe "hs_bindgen_5c4e5530b1f695b7" hs_bindgen_5c4e5530b1f695b7_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_prim_before3@
hs_bindgen_5c4e5530b1f695b7 ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_5c4e5530b1f695b7 =
  BG.fromFFIType hs_bindgen_5c4e5530b1f695b7_base

{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @macros\/reparse.h 179:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CUChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_before3 = hs_bindgen_5c4e5530b1f695b7

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_prim_after1@
foreign import ccall safe "hs_bindgen_d219b88b917d72cf" hs_bindgen_d219b88b917d72cf_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_prim_after1@
hs_bindgen_d219b88b917d72cf ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_d219b88b917d72cf =
  BG.fromFFIType hs_bindgen_d219b88b917d72cf_base

{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @macros\/reparse.h 180:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_after1 = hs_bindgen_d219b88b917d72cf

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_prim_after2@
foreign import ccall safe "hs_bindgen_79a466a4f64a4db1" hs_bindgen_79a466a4f64a4db1_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_prim_after2@
hs_bindgen_79a466a4f64a4db1 ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_79a466a4f64a4db1 =
  BG.fromFFIType hs_bindgen_79a466a4f64a4db1_base

{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @macros\/reparse.h 181:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CSChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_after2 = hs_bindgen_79a466a4f64a4db1

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_prim_after3@
foreign import ccall safe "hs_bindgen_a13884c8125a5f80" hs_bindgen_a13884c8125a5f80_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_prim_after3@
hs_bindgen_a13884c8125a5f80 ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_a13884c8125a5f80 =
  BG.fromFFIType hs_bindgen_a13884c8125a5f80_base

{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @macros\/reparse.h 182:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CUChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_after3 = hs_bindgen_a13884c8125a5f80

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_before1@
foreign import ccall safe "hs_bindgen_5433c1896cba42d3" hs_bindgen_5433c1896cba42d3_base ::
     BG.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_before1@
hs_bindgen_5433c1896cba42d3 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_5433c1896cba42d3 =
  BG.fromFFIType hs_bindgen_5433c1896cba42d3_base

{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @macros\/reparse.h 186:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before1 =
  hs_bindgen_5433c1896cba42d3

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_before2@
foreign import ccall safe "hs_bindgen_8300ce11589589f2" hs_bindgen_8300ce11589589f2_base ::
     BG.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_before2@
hs_bindgen_8300ce11589589f2 ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_8300ce11589589f2 =
  BG.fromFFIType hs_bindgen_8300ce11589589f2_base

{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @macros\/reparse.h 187:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before2 =
  hs_bindgen_8300ce11589589f2

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_before3@
foreign import ccall safe "hs_bindgen_be304ec839983d7b" hs_bindgen_be304ec839983d7b_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_before3@
hs_bindgen_be304ec839983d7b ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_be304ec839983d7b =
  BG.fromFFIType hs_bindgen_be304ec839983d7b_base

{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @macros\/reparse.h 188:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before3 =
  hs_bindgen_be304ec839983d7b

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_before4@
foreign import ccall safe "hs_bindgen_445a2c238c734060" hs_bindgen_445a2c238c734060_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_before4@
hs_bindgen_445a2c238c734060 ::
     BG.CInt
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_445a2c238c734060 =
  BG.fromFFIType hs_bindgen_445a2c238c734060_base

{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @macros\/reparse.h 189:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> Some_struct
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before4 =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_445a2c238c734060 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_before5@
foreign import ccall safe "hs_bindgen_b5a05c2c1589c243" hs_bindgen_b5a05c2c1589c243_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_before5@
hs_bindgen_b5a05c2c1589c243 ::
     BG.CInt
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_b5a05c2c1589c243 =
  BG.fromFFIType hs_bindgen_b5a05c2c1589c243_base

{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @macros\/reparse.h 190:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> Some_union
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before5 =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_b5a05c2c1589c243 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_before6@
foreign import ccall safe "hs_bindgen_8217e6e9e19e7627" hs_bindgen_8217e6e9e19e7627_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_before6@
hs_bindgen_8217e6e9e19e7627 ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_8217e6e9e19e7627 =
  BG.fromFFIType hs_bindgen_8217e6e9e19e7627_base

{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @macros\/reparse.h 191:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before6 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> Some_enum
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before6 =
  hs_bindgen_8217e6e9e19e7627

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_before7@
foreign import ccall safe "hs_bindgen_cbcb3411df130b42" hs_bindgen_cbcb3411df130b42_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_before7@
hs_bindgen_cbcb3411df130b42 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_cbcb3411df130b42 =
  BG.fromFFIType hs_bindgen_cbcb3411df130b42_base

{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @macros\/reparse.h 192:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before7 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before7 =
  hs_bindgen_cbcb3411df130b42

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_before8@
foreign import ccall safe "hs_bindgen_fb0ddd12de072fd8" hs_bindgen_fb0ddd12de072fd8_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_before8@
hs_bindgen_fb0ddd12de072fd8 ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_fb0ddd12de072fd8 =
  BG.fromFFIType hs_bindgen_fb0ddd12de072fd8_base

{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @macros\/reparse.h 193:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before8 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.LibC.CSize
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before8 =
  hs_bindgen_fb0ddd12de072fd8

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_after1@
foreign import ccall safe "hs_bindgen_91e81e2ec29aa772" hs_bindgen_91e81e2ec29aa772_base ::
     BG.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_after1@
hs_bindgen_91e81e2ec29aa772 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_91e81e2ec29aa772 =
  BG.fromFFIType hs_bindgen_91e81e2ec29aa772_base

{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @macros\/reparse.h 195:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after1 =
  hs_bindgen_91e81e2ec29aa772

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_after2@
foreign import ccall safe "hs_bindgen_6703a37e4fd098b1" hs_bindgen_6703a37e4fd098b1_base ::
     BG.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_after2@
hs_bindgen_6703a37e4fd098b1 ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_6703a37e4fd098b1 =
  BG.fromFFIType hs_bindgen_6703a37e4fd098b1_base

{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @macros\/reparse.h 196:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after2 =
  hs_bindgen_6703a37e4fd098b1

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_after3@
foreign import ccall safe "hs_bindgen_054259024219dbcc" hs_bindgen_054259024219dbcc_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_after3@
hs_bindgen_054259024219dbcc ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_054259024219dbcc =
  BG.fromFFIType hs_bindgen_054259024219dbcc_base

{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @macros\/reparse.h 197:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after3 =
  hs_bindgen_054259024219dbcc

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_after4@
foreign import ccall safe "hs_bindgen_eb4880610136c7bb" hs_bindgen_eb4880610136c7bb_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_after4@
hs_bindgen_eb4880610136c7bb ::
     BG.CInt
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_eb4880610136c7bb =
  BG.fromFFIType hs_bindgen_eb4880610136c7bb_base

{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @macros\/reparse.h 198:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> Some_struct
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after4 =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_eb4880610136c7bb arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_after5@
foreign import ccall safe "hs_bindgen_c0478c5f65c7e32e" hs_bindgen_c0478c5f65c7e32e_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_after5@
hs_bindgen_c0478c5f65c7e32e ::
     BG.CInt
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_c0478c5f65c7e32e =
  BG.fromFFIType hs_bindgen_c0478c5f65c7e32e_base

{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @macros\/reparse.h 199:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> Some_union
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after5 =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_c0478c5f65c7e32e arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_after6@
foreign import ccall safe "hs_bindgen_764dd29ada03ac05" hs_bindgen_764dd29ada03ac05_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_after6@
hs_bindgen_764dd29ada03ac05 ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_764dd29ada03ac05 =
  BG.fromFFIType hs_bindgen_764dd29ada03ac05_base

{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @macros\/reparse.h 200:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after6 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> Some_enum
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after6 =
  hs_bindgen_764dd29ada03ac05

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_after7@
foreign import ccall safe "hs_bindgen_62f2db62740aa47c" hs_bindgen_62f2db62740aa47c_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_after7@
hs_bindgen_62f2db62740aa47c ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_62f2db62740aa47c =
  BG.fromFFIType hs_bindgen_62f2db62740aa47c_base

{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @macros\/reparse.h 201:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after7 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after7 =
  hs_bindgen_62f2db62740aa47c

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_after8@
foreign import ccall safe "hs_bindgen_f9349cbf0c433984" hs_bindgen_f9349cbf0c433984_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_withoutSign_after8@
hs_bindgen_f9349cbf0c433984 ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_f9349cbf0c433984 =
  BG.fromFFIType hs_bindgen_f9349cbf0c433984_base

{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @macros\/reparse.h 202:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after8 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.LibC.CSize
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after8 =
  hs_bindgen_f9349cbf0c433984

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_args1@
foreign import ccall safe "hs_bindgen_05103ac5c64a70c5" hs_bindgen_05103ac5c64a70c5_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_args1@
hs_bindgen_05103ac5c64a70c5 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_05103ac5c64a70c5 =
  BG.fromFFIType hs_bindgen_05103ac5c64a70c5_base

{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @macros\/reparse.h 206:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> PtrConst.PtrConst BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args1 = hs_bindgen_05103ac5c64a70c5

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_args2@
foreign import ccall safe "hs_bindgen_2adbdab03502d7c3" hs_bindgen_2adbdab03502d7c3_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_args2@
hs_bindgen_2adbdab03502d7c3 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_2adbdab03502d7c3 =
  BG.fromFFIType hs_bindgen_2adbdab03502d7c3_base

{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @macros\/reparse.h 207:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> PtrConst.PtrConst BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args2 = hs_bindgen_2adbdab03502d7c3

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_args3@
foreign import ccall safe "hs_bindgen_02735f713f3663a0" hs_bindgen_02735f713f3663a0_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_args3@
hs_bindgen_02735f713f3663a0 ::
     BG.CInt
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_02735f713f3663a0 =
  BG.fromFFIType hs_bindgen_02735f713f3663a0_base

{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @macros\/reparse.h 208:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.Ptr BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args3 = hs_bindgen_02735f713f3663a0

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_args4@
foreign import ccall safe "hs_bindgen_7be4b56f499736ec" hs_bindgen_7be4b56f499736ec_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_args4@
hs_bindgen_7be4b56f499736ec ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_7be4b56f499736ec =
  BG.fromFFIType hs_bindgen_7be4b56f499736ec_base

{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @macros\/reparse.h 209:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> PtrConst.PtrConst BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args4 = hs_bindgen_7be4b56f499736ec

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_args5@
foreign import ccall safe "hs_bindgen_07806340121e9d7a" hs_bindgen_07806340121e9d7a_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_args5@
hs_bindgen_07806340121e9d7a ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_07806340121e9d7a =
  BG.fromFFIType hs_bindgen_07806340121e9d7a_base

{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @macros\/reparse.h 210:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> PtrConst.PtrConst BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args5 = hs_bindgen_07806340121e9d7a

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_ret1@
foreign import ccall safe "hs_bindgen_964ecbd6daab3ac8" hs_bindgen_964ecbd6daab3ac8_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_ret1@
hs_bindgen_964ecbd6daab3ac8 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_964ecbd6daab3ac8 =
  BG.fromFFIType hs_bindgen_964ecbd6daab3ac8_base

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h 212:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret1 = hs_bindgen_964ecbd6daab3ac8

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_ret2@
foreign import ccall safe "hs_bindgen_425aa26fa3f15a85" hs_bindgen_425aa26fa3f15a85_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_ret2@
hs_bindgen_425aa26fa3f15a85 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_425aa26fa3f15a85 =
  BG.fromFFIType hs_bindgen_425aa26fa3f15a85_base

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h 213:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret2 = hs_bindgen_425aa26fa3f15a85

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_ret3@
foreign import ccall safe "hs_bindgen_35dc6a2376f4669c" hs_bindgen_35dc6a2376f4669c_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_ret3@
hs_bindgen_35dc6a2376f4669c ::
     BG.CInt
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_35dc6a2376f4669c =
  BG.fromFFIType hs_bindgen_35dc6a2376f4669c_base

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h 214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
const_pointers_ret3 = hs_bindgen_35dc6a2376f4669c

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_ret4@
foreign import ccall safe "hs_bindgen_b4704a8e3fa5c1a7" hs_bindgen_b4704a8e3fa5c1a7_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_ret4@
hs_bindgen_b4704a8e3fa5c1a7 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_b4704a8e3fa5c1a7 =
  BG.fromFFIType hs_bindgen_b4704a8e3fa5c1a7_base

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h 215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret4 = hs_bindgen_b4704a8e3fa5c1a7

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_ret5@
foreign import ccall safe "hs_bindgen_eee06f696926bceb" hs_bindgen_eee06f696926bceb_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_pointers_ret5@
hs_bindgen_eee06f696926bceb ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_eee06f696926bceb =
  BG.fromFFIType hs_bindgen_eee06f696926bceb_base

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h 216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret5 = hs_bindgen_eee06f696926bceb

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_array_elem1@
foreign import ccall safe "hs_bindgen_85897c18c4db8c38" hs_bindgen_85897c18c4db8c38_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_array_elem1@
hs_bindgen_85897c18c4db8c38 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt))
  -> IO ()
hs_bindgen_85897c18c4db8c38 =
  BG.fromFFIType hs_bindgen_85897c18c4db8c38_base

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h 244:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem1 = hs_bindgen_85897c18c4db8c38

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_array_elem2@
foreign import ccall safe "hs_bindgen_7d19c28372388bd5" hs_bindgen_7d19c28372388bd5_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_array_elem2@
hs_bindgen_7d19c28372388bd5 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt)))
  -> IO ()
hs_bindgen_7d19c28372388bd5 =
  BG.fromFFIType hs_bindgen_7d19c28372388bd5_base

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h 245:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem2 = hs_bindgen_7d19c28372388bd5

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_array_elem3@
foreign import ccall safe "hs_bindgen_b72db4cfe06fec75" hs_bindgen_b72db4cfe06fec75_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_const_array_elem3@
hs_bindgen_b72db4cfe06fec75 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_b72db4cfe06fec75 =
  BG.fromFFIType hs_bindgen_b72db4cfe06fec75_base

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h 246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem3 = hs_bindgen_b72db4cfe06fec75

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_noParams1@
foreign import ccall safe "hs_bindgen_8c2d8b6c768235a6" hs_bindgen_8c2d8b6c768235a6_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_noParams1@
hs_bindgen_8c2d8b6c768235a6 :: IO BG.CInt
hs_bindgen_8c2d8b6c768235a6 =
  BG.fromFFIType hs_bindgen_8c2d8b6c768235a6_base

{-| Other examples we reparsed /incorrectly/ before language-c

    __C declaration:__ @noParams1@

    __defined at:__ @macros\/reparse.h 254:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams1 :: IO BG.CInt
noParams1 = hs_bindgen_8c2d8b6c768235a6

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_noParams2@
foreign import ccall safe "hs_bindgen_f406093f4244470f" hs_bindgen_f406093f4244470f_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_noParams2@
hs_bindgen_f406093f4244470f :: IO BG.CInt
hs_bindgen_f406093f4244470f =
  BG.fromFFIType hs_bindgen_f406093f4244470f_base

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h 255:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2 :: IO BG.CInt
noParams2 = hs_bindgen_f406093f4244470f

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_noParams3@
foreign import ccall safe "hs_bindgen_7cfe278ffc125a35" hs_bindgen_7cfe278ffc125a35_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_noParams3@
hs_bindgen_7cfe278ffc125a35 ::
     BG.CInt
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_7cfe278ffc125a35 =
  BG.fromFFIType hs_bindgen_7cfe278ffc125a35_base

{-| __C declaration:__ @noParams3@

    __defined at:__ @macros\/reparse.h 256:6@

    __exported by:__ @macros\/reparse.h@
-}
noParams3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> BG.FunPtr (IO BG.CInt)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
noParams3 = hs_bindgen_7cfe278ffc125a35

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret1@
foreign import ccall safe "hs_bindgen_6f95516796104481" hs_bindgen_6f95516796104481_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret1@
hs_bindgen_6f95516796104481 ::
     BG.CInt
  -> IO (BG.FunPtr (IO ()))
hs_bindgen_6f95516796104481 =
  BG.fromFFIType hs_bindgen_6f95516796104481_base

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h 260:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO ()))
funptr_ret1 = hs_bindgen_6f95516796104481

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret2@
foreign import ccall safe "hs_bindgen_c823e0311ec509b4" hs_bindgen_c823e0311ec509b4_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret2@
hs_bindgen_c823e0311ec509b4 ::
     BG.CInt
  -> IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_c823e0311ec509b4 =
  BG.fromFFIType hs_bindgen_c823e0311ec509b4_base

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h 261:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO BG.CInt))
funptr_ret2 = hs_bindgen_c823e0311ec509b4

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret3@
foreign import ccall safe "hs_bindgen_94550ec4aba19f7b" hs_bindgen_94550ec4aba19f7b_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret3@
hs_bindgen_94550ec4aba19f7b ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_94550ec4aba19f7b =
  BG.fromFFIType hs_bindgen_94550ec4aba19f7b_base

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h 262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
funptr_ret3 = hs_bindgen_94550ec4aba19f7b

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret4@
foreign import ccall safe "hs_bindgen_ed3e6ebe628da423" hs_bindgen_ed3e6ebe628da423_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret4@
hs_bindgen_ed3e6ebe628da423 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
hs_bindgen_ed3e6ebe628da423 =
  BG.fromFFIType hs_bindgen_ed3e6ebe628da423_base

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h 263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
funptr_ret4 = hs_bindgen_ed3e6ebe628da423

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret5@
foreign import ccall safe "hs_bindgen_cb5a94e6610ff83d" hs_bindgen_cb5a94e6610ff83d_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret5@
hs_bindgen_cb5a94e6610ff83d ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_cb5a94e6610ff83d =
  BG.fromFFIType hs_bindgen_cb5a94e6610ff83d_base

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h 267:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret5 = hs_bindgen_cb5a94e6610ff83d

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret6@
foreign import ccall safe "hs_bindgen_8cd3005394354b86" hs_bindgen_8cd3005394354b86_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret6@
hs_bindgen_8cd3005394354b86 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_8cd3005394354b86 =
  BG.fromFFIType hs_bindgen_8cd3005394354b86_base

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h 268:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret6 = hs_bindgen_8cd3005394354b86

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret7@
foreign import ccall safe "hs_bindgen_0f1393247fcdeb7a" hs_bindgen_0f1393247fcdeb7a_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret7@
hs_bindgen_0f1393247fcdeb7a ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_0f1393247fcdeb7a =
  BG.fromFFIType hs_bindgen_0f1393247fcdeb7a_base

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h 269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret7 = hs_bindgen_0f1393247fcdeb7a

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret8@
foreign import ccall safe "hs_bindgen_7da05d4b5c843853" hs_bindgen_7da05d4b5c843853_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret8@
hs_bindgen_7da05d4b5c843853 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_7da05d4b5c843853 =
  BG.fromFFIType hs_bindgen_7da05d4b5c843853_base

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h 270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret8 = hs_bindgen_7da05d4b5c843853

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret9@
foreign import ccall safe "hs_bindgen_f6f304c980603aae" hs_bindgen_f6f304c980603aae_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret9@
hs_bindgen_f6f304c980603aae ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_f6f304c980603aae =
  BG.fromFFIType hs_bindgen_f6f304c980603aae_base

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h 271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret9 = hs_bindgen_f6f304c980603aae

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret10@
foreign import ccall safe "hs_bindgen_263377d0e4be3901" hs_bindgen_263377d0e4be3901_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Safe_funptr_ret10@
hs_bindgen_263377d0e4be3901 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_263377d0e4be3901 =
  BG.fromFFIType hs_bindgen_263377d0e4be3901_base

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h 272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret10 = hs_bindgen_263377d0e4be3901
