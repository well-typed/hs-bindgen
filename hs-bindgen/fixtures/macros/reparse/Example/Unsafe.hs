{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Data.Complex
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <macros/reparse.h>"
  , "void hs_bindgen_test_macrosreparse_a10d23a1cebc3f58 ("
  , "  A arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  args_char1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_a76a90b5f6e68b22 ("
  , "  A arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  args_char2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_8d42e2ffb839cfb7 ("
  , "  A arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  args_char3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_0919acaf21bc8eb1 ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  args_short1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_42f4e1b66fbe1d85 ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  args_short2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_cfd148e6676f4393 ("
  , "  A arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  args_short3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_b55222b08f54d08a ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  args_int1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_5ab884050f61f378 ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  args_int2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_5b3642adbf8d8c09 ("
  , "  A arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  args_int3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_fb02cb0320aff007 ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  args_long1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_c6b81a1422f5535e ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  args_long2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_7279876c6cff5eed ("
  , "  A arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  args_long3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_7dec78ee43c784cf ("
  , "  A arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  args_float(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_af9629b17c5c01eb ("
  , "  A arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  args_double(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_b1e345616dae25b7 ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  args_bool1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_e20137c4ab18a66e ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  args_struct(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_8468152f3130816a ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  args_union(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_2ee1baf211b5f628 ("
  , "  A arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  args_enum(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_42e8807b857ec8be ("
  , "  A arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  args_pointer1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_decf0f1fad98cc09 ("
  , "  A arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  args_pointer2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_c2bfa7966be9fc8a ("
  , "  A arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  args_pointer3(arg1, arg2);"
  , "}"
  , "A hs_bindgen_test_macrosreparse_733ed36b28b7932b (void)"
  , "{"
  , "  return ret_A();"
  , "}"
  , "char hs_bindgen_test_macrosreparse_954b53887fa8a7bf ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char1(arg1);"
  , "}"
  , "signed char hs_bindgen_test_macrosreparse_20d1e28fced60632 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char2(arg1);"
  , "}"
  , "unsigned char hs_bindgen_test_macrosreparse_e55f76ceed24192d ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char3(arg1);"
  , "}"
  , "signed short hs_bindgen_test_macrosreparse_1b8d9484010475fd ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short1(arg1);"
  , "}"
  , "signed short hs_bindgen_test_macrosreparse_9b4291556b50f99f ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short2(arg1);"
  , "}"
  , "unsigned short hs_bindgen_test_macrosreparse_658bd87f6fba088a ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short3(arg1);"
  , "}"
  , "signed int hs_bindgen_test_macrosreparse_f39ba97cac5f7b69 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int1(arg1);"
  , "}"
  , "signed int hs_bindgen_test_macrosreparse_2811bb2a6c369ff8 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int2(arg1);"
  , "}"
  , "unsigned int hs_bindgen_test_macrosreparse_4d1047d184259f2a ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int3(arg1);"
  , "}"
  , "signed long hs_bindgen_test_macrosreparse_fe651b499cb756e7 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long1(arg1);"
  , "}"
  , "signed long hs_bindgen_test_macrosreparse_e1e78ae00ab5d6fb ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long2(arg1);"
  , "}"
  , "unsigned long hs_bindgen_test_macrosreparse_dc6449bb75895cea ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long3(arg1);"
  , "}"
  , "float hs_bindgen_test_macrosreparse_7ae67ab94cf0f147 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_float(arg1);"
  , "}"
  , "double hs_bindgen_test_macrosreparse_8a715139fcb185f1 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_double(arg1);"
  , "}"
  , "_Bool hs_bindgen_test_macrosreparse_330b3d59b2b9e0ac ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_bool1(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_843df9379b58bc51 ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_struct(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_d1fb1f1235b044ef ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_union(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_test_macrosreparse_5706a52e565b1a0c ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_enum(arg1);"
  , "}"
  , "signed int *hs_bindgen_test_macrosreparse_1539645657f24f97 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer1(arg1);"
  , "}"
  , "signed int **hs_bindgen_test_macrosreparse_14db602035a357c9 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer2(arg1);"
  , "}"
  , "void *hs_bindgen_test_macrosreparse_347fc9fe6ee0e39f ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer3(arg1);"
  , "}"
  , "signed int hs_bindgen_test_macrosreparse_31d344eb39edbb32 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return body1(arg1);"
  , "}"
  , "A hs_bindgen_test_macrosreparse_9a49ad9d6fd009aa (void)"
  , "{"
  , "  return body2();"
  , "}"
  , "void hs_bindgen_test_macrosreparse_f09e648ac9470faf ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  args_complex_float(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_a334455360f1e746 ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  args_complex_double(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_0c94b79e37a671f3 ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_complex_float(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_52e016b143848038 ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_complex_double(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_28f85791b3039264 ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  bespoke_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_833c75e57b012dcc ("
  , "  A arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  bespoke_args2(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_test_macrosreparse_434418d9d1f41c66 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return bespoke_ret1(arg1);"
  , "}"
  , "size_t hs_bindgen_test_macrosreparse_7a13d4c1ed935df0 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return bespoke_ret2(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_802c66e1efc0f556 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  arr_args1(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_f516070848930af9 ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  arr_args2(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_c0db4046bcf7da77 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  arr_args3(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_881ede0e81c9ed45 ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  arr_args4(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_91151b594cc92e09 ("
  , "  A arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  funptr_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_543290455260832c ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  funptr_args2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_fbdf924574cb6295 ("
  , "  A arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  funptr_args3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_5e0a2c10ccd9a8c4 ("
  , "  A arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  funptr_args4(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_c13f66d86b4b5ef6 ("
  , "  A arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  funptr_args5(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_0b54f704cff3ab9b ("
  , "  A arg1"
  , ")"
  , "{"
  , "  comments1(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_4a390ee488c3a1b1 ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  const_prim_before1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_349252e982c28bae ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  const_prim_before2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_7ffeb1784fe8b2f2 ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  const_prim_before3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_0dae8ba3b65c77d2 ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  const_prim_after1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_dc74f73eff3fac62 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  const_prim_after2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_aeea75713b67f6d8 ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  const_prim_after3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_9f70419bf10f327e ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_7867bb0d71ef4b6d ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_80de805eb016225b ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_69cef8742b4b119b ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before4(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_1ad5aadb8be4d493 ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before5(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_7106059de99b7682 ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before6(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_b61cf3c21bf8b00b ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before7(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_47573f5eb4cb92a9 ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before8(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_ffb1a87ed1f94b31 ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_1ec7c37faacfcd64 ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_34233036f1e22371 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_4c909292e290aa0a ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after4(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_ab1abd31c91696b9 ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after5(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_342c1139871906f3 ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after6(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_446758003fdc3418 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after7(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_161f0b5d4c06966c ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after8(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_ec8e91fa9341dad6 ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  const_pointers_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_cf24549623cd56c1 ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  const_pointers_args2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_71cfb5062c931668 ("
  , "  A arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_74894da577575f87 ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args4(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_5493c91c677fe8d0 ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args5(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_test_macrosreparse_a302fca87b1aa099 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret1(arg1);"
  , "}"
  , "signed int const *hs_bindgen_test_macrosreparse_825f0c60f6c63862 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret2(arg1);"
  , "}"
  , "signed int *const hs_bindgen_test_macrosreparse_c36f7d270a11e1cd ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret3(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_test_macrosreparse_4a82390c6e38a4ad ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret4(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_test_macrosreparse_763d600d2f5c49bb ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret5(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_4b3bef3ab591a329 ("
  , "  A const *arg1"
  , ")"
  , "{"
  , "  const_array_elem1(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_dd69ef198e368a38 ("
  , "  A const **arg1"
  , ")"
  , "{"
  , "  const_array_elem2(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_1842bc8653aa9c3f ("
  , "  A *const *arg1"
  , ")"
  , "{"
  , "  const_array_elem3(arg1);"
  , "}"
  , "A hs_bindgen_test_macrosreparse_4c99a8a7824a66d4 (void)"
  , "{"
  , "  return noParams1();"
  , "}"
  , "A hs_bindgen_test_macrosreparse_7ae14613ab7f3b03 (void)"
  , "{"
  , "  return noParams2();"
  , "}"
  , "void hs_bindgen_test_macrosreparse_2f4d972da222d332 ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  noParams3(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_test_macrosreparse_dbe5f5ae726e36b3 ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return funptr_ret1(arg1);"
  , "}"
  , "signed int (*hs_bindgen_test_macrosreparse_081cb5fbeb6f4506 ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return funptr_ret2(arg1);"
  , "}"
  , "void (*hs_bindgen_test_macrosreparse_4fc16e9f894820ff ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return funptr_ret3(arg1);"
  , "}"
  , "char (*hs_bindgen_test_macrosreparse_9e8aa8193619dbbe ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret4(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_test_macrosreparse_4b914fec0c848647 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret5(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_test_macrosreparse_6c188a6c3899a751 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret6(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_test_macrosreparse_e4468a6e0afe686b ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret7(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_test_macrosreparse_16740b4fc6d6c8ec ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret8(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_test_macrosreparse_d82b69157b543190 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret9(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_test_macrosreparse_317f5f7c8c2496cd ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret10(arg1);"
  , "}"
  ]))

{-| Function declarations

__C declaration:__ @args_char1@

__defined at:__ @macros\/reparse.h:17:6@

__exported by:__ @macros\/reparse.h@

__unique:__ @ExampleJust Unsafeargs_char1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_a10d23a1cebc3f58" args_char1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_char2@

    __defined at:__ @macros\/reparse.h:18:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeargs_char2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_a76a90b5f6e68b22" args_char2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CSChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_char3@

    __defined at:__ @macros\/reparse.h:19:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeargs_char3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_8d42e2ffb839cfb7" args_char3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_short1@

    __defined at:__ @macros\/reparse.h:21:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeargs_short1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_0919acaf21bc8eb1" args_short1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CShort
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_short2@

    __defined at:__ @macros\/reparse.h:22:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeargs_short2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_42f4e1b66fbe1d85" args_short2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CShort
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_short3@

    __defined at:__ @macros\/reparse.h:23:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeargs_short3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_cfd148e6676f4393" args_short3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUShort
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_int1@

    __defined at:__ @macros\/reparse.h:25:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeargs_int1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_b55222b08f54d08a" args_int1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_int2@

    __defined at:__ @macros\/reparse.h:26:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeargs_int2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_5ab884050f61f378" args_int2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_int3@

    __defined at:__ @macros\/reparse.h:27:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeargs_int3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_5b3642adbf8d8c09" args_int3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_long1@

    __defined at:__ @macros\/reparse.h:29:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeargs_long1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_fb02cb0320aff007" args_long1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CLong
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_long2@

    __defined at:__ @macros\/reparse.h:30:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeargs_long2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c6b81a1422f5535e" args_long2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CLong
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_long3@

    __defined at:__ @macros\/reparse.h:31:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeargs_long3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_7279876c6cff5eed" args_long3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CULong
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_float@

    __defined at:__ @macros\/reparse.h:33:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeargs_float@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_7dec78ee43c784cf" args_float ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CFloat
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_double@

    __defined at:__ @macros\/reparse.h:34:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeargs_double@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_af9629b17c5c01eb" args_double ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_bool1@

    __defined at:__ @macros\/reparse.h:35:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeargs_bool1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_b1e345616dae25b7" args_bool1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| Pointer-based API for 'args_struct'

__unique:__ @ExampleJust Unsafeargs_struct@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e20137c4ab18a66e" args_struct_wrapper ::
     A
  -> Ptr.Ptr Some_struct
  -> IO ()

{-| __C declaration:__ @args_struct@

    __defined at:__ @macros\/reparse.h:37:6@

    __exported by:__ @macros\/reparse.h@
-}
args_struct ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_struct
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()
args_struct =
  \x0 ->
    \x1 -> F.with x1 (\y2 -> args_struct_wrapper x0 y2)

{-| Pointer-based API for 'args_union'

__unique:__ @ExampleJust Unsafeargs_union@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_8468152f3130816a" args_union_wrapper ::
     A
  -> Ptr.Ptr Some_union
  -> IO ()

{-| __C declaration:__ @args_union@

    __defined at:__ @macros\/reparse.h:38:6@

    __exported by:__ @macros\/reparse.h@
-}
args_union ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_union
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()
args_union =
  \x0 ->
    \x1 -> F.with x1 (\y2 -> args_union_wrapper x0 y2)

{-| __C declaration:__ @args_enum@

    __defined at:__ @macros\/reparse.h:39:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeargs_enum@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_2ee1baf211b5f628" args_enum ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_enum
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_pointer1@

    __defined at:__ @macros\/reparse.h:41:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeargs_pointer1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_42e8807b857ec8be" args_pointer1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_pointer2@

    __defined at:__ @macros\/reparse.h:42:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeargs_pointer2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_decf0f1fad98cc09" args_pointer2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr (Ptr.Ptr FC.CInt)
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_pointer3@

    __defined at:__ @macros\/reparse.h:43:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeargs_pointer3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c2bfa7966be9fc8a" args_pointer3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr Void
     {- ^ __C declaration:__ @arg3@
     -}
  -> IO ()

{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h:47:3@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_A@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_733ed36b28b7932b" ret_A ::
     IO A

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h:49:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_char1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_954b53887fa8a7bf" ret_char1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CChar

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h:50:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_char2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_20d1e28fced60632" ret_char2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CSChar

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h:51:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_char3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e55f76ceed24192d" ret_char3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CUChar

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h:53:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_short1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_1b8d9484010475fd" ret_short1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CShort

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h:54:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_short2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_9b4291556b50f99f" ret_short2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CShort

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h:55:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_short3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_658bd87f6fba088a" ret_short3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CUShort

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h:57:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_int1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_f39ba97cac5f7b69" ret_int1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h:58:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_int2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_2811bb2a6c369ff8" ret_int2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h:59:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_int3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_4d1047d184259f2a" ret_int3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CUInt

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h:61:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_long1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_fe651b499cb756e7" ret_long1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CLong

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h:62:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_long2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e1e78ae00ab5d6fb" ret_long2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CLong

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h:63:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_long3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_dc6449bb75895cea" ret_long3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CULong

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h:65:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_float@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_7ae67ab94cf0f147" ret_float ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CFloat

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h:66:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_double@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_8a715139fcb185f1" ret_double ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CDouble

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h:67:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_bool1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_330b3d59b2b9e0ac" ret_bool1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CBool

{-| Pointer-based API for 'ret_struct'

__unique:__ @ExampleJust Unsaferet_struct@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_843df9379b58bc51" ret_struct_wrapper ::
     A
  -> Ptr.Ptr Some_struct
  -> IO ()

{-| __C declaration:__ @ret_struct@

    __defined at:__ @macros\/reparse.h:69:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_struct ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO Some_struct
ret_struct =
  \x0 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\z1 ->
                                            ret_struct_wrapper x0 z1)

{-| Pointer-based API for 'ret_union'

__unique:__ @ExampleJust Unsaferet_union@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_d1fb1f1235b044ef" ret_union_wrapper ::
     A
  -> Ptr.Ptr Some_union
  -> IO ()

{-| __C declaration:__ @ret_union@

    __defined at:__ @macros\/reparse.h:70:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_union ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO Some_union
ret_union =
  \x0 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\z1 ->
                                            ret_union_wrapper x0 z1)

{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h:71:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_enum@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_5706a52e565b1a0c" ret_enum ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO Some_enum

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h:73:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_pointer1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_1539645657f24f97" ret_pointer1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h:74:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_pointer2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_14db602035a357c9" ret_pointer2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h:75:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsaferet_pointer3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_347fc9fe6ee0e39f" ret_pointer3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h:79:5@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafebody1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_31d344eb39edbb32" body1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h:80:3@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafebody2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_9a49ad9d6fd009aa" body2 ::
     IO A

{-| Pointer-based API for 'args_complex_float'

__unique:__ @ExampleJust Unsafeargs_complex_float@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_f09e648ac9470faf" args_complex_float_wrapper ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> IO ()

{-| __C declaration:__ @args_complex_float@

    __defined at:__ @macros\/reparse.h:84:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_float ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Data.Complex.Complex FC.CFloat
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()
args_complex_float =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 -> args_complex_float_wrapper x0 y2)

{-| Pointer-based API for 'args_complex_double'

__unique:__ @ExampleJust Unsafeargs_complex_double@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_a334455360f1e746" args_complex_double_wrapper ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> IO ()

{-| __C declaration:__ @args_complex_double@

    __defined at:__ @macros\/reparse.h:85:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_double ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Data.Complex.Complex FC.CDouble
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()
args_complex_double =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 -> args_complex_double_wrapper x0 y2)

{-| Pointer-based API for 'ret_complex_float'

__unique:__ @ExampleJust Unsaferet_complex_float@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_0c94b79e37a671f3" ret_complex_float_wrapper ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> IO ()

{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @macros\/reparse.h:86:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_float ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Data.Complex.Complex FC.CFloat)
ret_complex_float =
  \x0 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\z1 ->
                                            ret_complex_float_wrapper x0 z1)

{-| Pointer-based API for 'ret_complex_double'

__unique:__ @ExampleJust Unsaferet_complex_double@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_52e016b143848038" ret_complex_double_wrapper ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> IO ()

{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @macros\/reparse.h:87:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_double ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Data.Complex.Complex FC.CDouble)
ret_complex_double =
  \x0 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\z1 ->
                                            ret_complex_double_wrapper x0 z1)

{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @macros\/reparse.h:94:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafebespoke_args1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_28f85791b3039264" bespoke_args1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @macros\/reparse.h:95:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafebespoke_args2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_833c75e57b012dcc" bespoke_args2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h:97:8@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafebespoke_ret1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_434418d9d1f41c66" bespoke_ret1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CBool

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h:98:8@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafebespoke_ret2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_7a13d4c1ed935df0" bespoke_ret2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO HsBindgen.Runtime.Prelude.CSize

{-| Arrays

__C declaration:__ @arr_args1@

__defined at:__ @macros\/reparse.h:104:6@

__exported by:__ @macros\/reparse.h@

__unique:__ @ExampleJust Unsafearr_args1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_802c66e1efc0f556" arr_args1 ::
     Ptr.Ptr A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h:105:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafearr_args2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_f516070848930af9" arr_args2 ::
     Ptr.Ptr (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h:106:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafearr_args3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c0db4046bcf7da77" arr_args3 ::
     Ptr.Ptr A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h:107:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafearr_args4@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_881ede0e81c9ed45" arr_args4 ::
     Ptr.Ptr (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| Function pointers

__C declaration:__ @funptr_args1@

__defined at:__ @macros\/reparse.h:126:6@

__exported by:__ @macros\/reparse.h@

__unique:__ @ExampleJust Unsafefunptr_args1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_91151b594cc92e09" funptr_args1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (IO ())
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @funptr_args2@

    __defined at:__ @macros\/reparse.h:127:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafefunptr_args2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_543290455260832c" funptr_args2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (IO FC.CInt)
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @funptr_args3@

    __defined at:__ @macros\/reparse.h:128:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafefunptr_args3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_fbdf924574cb6295" funptr_args3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (FC.CInt -> IO ())
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @funptr_args4@

    __defined at:__ @macros\/reparse.h:129:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafefunptr_args4@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_5e0a2c10ccd9a8c4" funptr_args4 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @funptr_args5@

    __defined at:__ @macros\/reparse.h:130:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafefunptr_args5@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c13f66d86b4b5ef6" funptr_args5 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| Comments in awkward places

  (Prior to language-c we failed to parse there.)

__C declaration:__ @comments1@

__defined at:__ @macros\/reparse.h:144:25@

__exported by:__ @macros\/reparse.h@

__unique:__ @ExampleJust Unsafecomments1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_0b54f704cff3ab9b" comments1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| `const` qualifier

  NOTE: These were not parsed correctly prior to the switch to language-c.

__C declaration:__ @const_prim_before1@

__defined at:__ @macros\/reparse.h:179:6@

__exported by:__ @macros\/reparse.h@

__unique:__ @ExampleJust Unsafeconst_prim_before1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_4a390ee488c3a1b1" const_prim_before1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @macros\/reparse.h:180:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_prim_before2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_349252e982c28bae" const_prim_before2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CSChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @macros\/reparse.h:181:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_prim_before3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_7ffeb1784fe8b2f2" const_prim_before3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @macros\/reparse.h:182:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_prim_after1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_0dae8ba3b65c77d2" const_prim_after1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @macros\/reparse.h:183:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_prim_after2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_dc74f73eff3fac62" const_prim_after2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CSChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @macros\/reparse.h:184:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_prim_after3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_aeea75713b67f6d8" const_prim_after3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @macros\/reparse.h:188:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_withoutSign_before1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_9f70419bf10f327e" const_withoutSign_before1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CFloat
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @macros\/reparse.h:189:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_withoutSign_before2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_7867bb0d71ef4b6d" const_withoutSign_before2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @macros\/reparse.h:190:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_withoutSign_before3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_80de805eb016225b" const_withoutSign_before3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| Pointer-based API for 'const_withoutSign_before4'

__unique:__ @ExampleJust Unsafeconst_withoutSign_before4@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_69cef8742b4b119b" const_withoutSign_before4_wrapper ::
     A
  -> Ptr.Ptr Some_struct
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @macros\/reparse.h:191:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before4 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_struct
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()
const_withoutSign_before4 =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 ->
                   const_withoutSign_before4_wrapper x0 y2)

{-| Pointer-based API for 'const_withoutSign_before5'

__unique:__ @ExampleJust Unsafeconst_withoutSign_before5@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_1ad5aadb8be4d493" const_withoutSign_before5_wrapper ::
     A
  -> Ptr.Ptr Some_union
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @macros\/reparse.h:192:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before5 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_union
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()
const_withoutSign_before5 =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 ->
                   const_withoutSign_before5_wrapper x0 y2)

{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @macros\/reparse.h:193:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_withoutSign_before6@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_7106059de99b7682" const_withoutSign_before6 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_enum
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @macros\/reparse.h:194:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_withoutSign_before7@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_b61cf3c21bf8b00b" const_withoutSign_before7 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @macros\/reparse.h:195:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_withoutSign_before8@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_47573f5eb4cb92a9" const_withoutSign_before8 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @macros\/reparse.h:197:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_withoutSign_after1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_ffb1a87ed1f94b31" const_withoutSign_after1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CFloat
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @macros\/reparse.h:198:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_withoutSign_after2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_1ec7c37faacfcd64" const_withoutSign_after2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @macros\/reparse.h:199:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_withoutSign_after3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_34233036f1e22371" const_withoutSign_after3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| Pointer-based API for 'const_withoutSign_after4'

__unique:__ @ExampleJust Unsafeconst_withoutSign_after4@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_4c909292e290aa0a" const_withoutSign_after4_wrapper ::
     A
  -> Ptr.Ptr Some_struct
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @macros\/reparse.h:200:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after4 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_struct
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()
const_withoutSign_after4 =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 ->
                   const_withoutSign_after4_wrapper x0 y2)

{-| Pointer-based API for 'const_withoutSign_after5'

__unique:__ @ExampleJust Unsafeconst_withoutSign_after5@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_ab1abd31c91696b9" const_withoutSign_after5_wrapper ::
     A
  -> Ptr.Ptr Some_union
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @macros\/reparse.h:201:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after5 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_union
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()
const_withoutSign_after5 =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 ->
                   const_withoutSign_after5_wrapper x0 y2)

{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @macros\/reparse.h:202:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_withoutSign_after6@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_342c1139871906f3" const_withoutSign_after6 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_enum
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @macros\/reparse.h:203:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_withoutSign_after7@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_446758003fdc3418" const_withoutSign_after7 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @macros\/reparse.h:204:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_withoutSign_after8@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_161f0b5d4c06966c" const_withoutSign_after8 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @macros\/reparse.h:208:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_pointers_args1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_ec8e91fa9341dad6" const_pointers_args1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @macros\/reparse.h:209:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_pointers_args2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_cf24549623cd56c1" const_pointers_args2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @macros\/reparse.h:210:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_pointers_args3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_71cfb5062c931668" const_pointers_args3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @macros\/reparse.h:211:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_pointers_args4@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_74894da577575f87" const_pointers_args4 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @macros\/reparse.h:212:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_pointers_args5@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_5493c91c677fe8d0" const_pointers_args5 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h:214:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_pointers_ret1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_a302fca87b1aa099" const_pointers_ret1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h:215:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_pointers_ret2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_825f0c60f6c63862" const_pointers_ret2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h:216:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_pointers_ret3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c36f7d270a11e1cd" const_pointers_ret3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h:217:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_pointers_ret4@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_4a82390c6e38a4ad" const_pointers_ret4 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h:218:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_pointers_ret5@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_763d600d2f5c49bb" const_pointers_ret5 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| Pointer-based API for 'const_array_elem1'

__unique:__ @ExampleJust Unsafeconst_array_elem1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_4b3bef3ab591a329" const_array_elem1_wrapper ::
     Ptr.Ptr A
  -> IO ()

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h:246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 ::
     HsBindgen.Runtime.IncompleteArray.IncompleteArray A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()
const_array_elem1 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    const_array_elem1_wrapper ptr1)

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h:247:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafeconst_array_elem2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_dd69ef198e368a38" const_array_elem2 ::
     Ptr.Ptr (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| Pointer-based API for 'const_array_elem3'

__unique:__ @ExampleJust Unsafeconst_array_elem3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_1842bc8653aa9c3f" const_array_elem3_wrapper ::
     Ptr.Ptr (Ptr.Ptr A)
  -> IO ()

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h:248:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 ::
     HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()
const_array_elem3 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    const_array_elem3_wrapper ptr1)

{-| Other examples we reparsed /incorrectly/ before language-c

__C declaration:__ @noParams1@

__defined at:__ @macros\/reparse.h:256:3@

__exported by:__ @macros\/reparse.h@

__unique:__ @ExampleJust UnsafenoParams1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_4c99a8a7824a66d4" noParams1 ::
     IO A

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h:257:3@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust UnsafenoParams2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_7ae14613ab7f3b03" noParams2 ::
     IO A

{-| __C declaration:__ @noParams3@

    __defined at:__ @macros\/reparse.h:258:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust UnsafenoParams3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_2f4d972da222d332" noParams3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (IO FC.CInt)
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h:262:8@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafefunptr_ret1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_dbe5f5ae726e36b3" funptr_ret1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (IO ()))

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h:263:8@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafefunptr_ret2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_081cb5fbeb6f4506" funptr_ret2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (IO FC.CInt))

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h:264:8@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafefunptr_ret3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_4fc16e9f894820ff" funptr_ret3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> IO ()))

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h:265:8@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafefunptr_ret4@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_9e8aa8193619dbbe" funptr_ret4 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar))

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h:269:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafefunptr_ret5@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_4b914fec0c848647" funptr_ret5 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h:270:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafefunptr_ret6@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_6c188a6c3899a751" funptr_ret6 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h:271:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafefunptr_ret7@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e4468a6e0afe686b" funptr_ret7 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h:272:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafefunptr_ret8@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_16740b4fc6d6c8ec" funptr_ret8 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h:273:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafefunptr_ret9@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_d82b69157b543190" funptr_ret9 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h:274:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @ExampleJust Unsafefunptr_ret10@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_317f5f7c8c2496cd" funptr_ret10 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))
