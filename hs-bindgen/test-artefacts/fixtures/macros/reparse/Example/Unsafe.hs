{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.args_char1
    , Example.Unsafe.args_char2
    , Example.Unsafe.args_char3
    , Example.Unsafe.args_short1
    , Example.Unsafe.args_short2
    , Example.Unsafe.args_short3
    , Example.Unsafe.args_int1
    , Example.Unsafe.args_int2
    , Example.Unsafe.args_int3
    , Example.Unsafe.args_long1
    , Example.Unsafe.args_long2
    , Example.Unsafe.args_long3
    , Example.Unsafe.args_float
    , Example.Unsafe.args_double
    , Example.Unsafe.args_bool1
    , Example.Unsafe.args_struct
    , Example.Unsafe.args_union
    , Example.Unsafe.args_enum
    , Example.Unsafe.args_pointer1
    , Example.Unsafe.args_pointer2
    , Example.Unsafe.args_pointer3
    , Example.Unsafe.ret_A
    , Example.Unsafe.ret_char1
    , Example.Unsafe.ret_char2
    , Example.Unsafe.ret_char3
    , Example.Unsafe.ret_short1
    , Example.Unsafe.ret_short2
    , Example.Unsafe.ret_short3
    , Example.Unsafe.ret_int1
    , Example.Unsafe.ret_int2
    , Example.Unsafe.ret_int3
    , Example.Unsafe.ret_long1
    , Example.Unsafe.ret_long2
    , Example.Unsafe.ret_long3
    , Example.Unsafe.ret_float
    , Example.Unsafe.ret_double
    , Example.Unsafe.ret_bool1
    , Example.Unsafe.ret_struct
    , Example.Unsafe.ret_union
    , Example.Unsafe.ret_enum
    , Example.Unsafe.ret_pointer1
    , Example.Unsafe.ret_pointer2
    , Example.Unsafe.ret_pointer3
    , Example.Unsafe.body1
    , Example.Unsafe.body2
    , Example.Unsafe.args_complex_float
    , Example.Unsafe.args_complex_double
    , Example.Unsafe.ret_complex_float
    , Example.Unsafe.ret_complex_double
    , Example.Unsafe.bespoke_args1
    , Example.Unsafe.bespoke_args2
    , Example.Unsafe.bespoke_ret1
    , Example.Unsafe.bespoke_ret2
    , Example.Unsafe.arr_args1
    , Example.Unsafe.arr_args2
    , Example.Unsafe.arr_args3
    , Example.Unsafe.arr_args4
    , Example.Unsafe.funptr_args1
    , Example.Unsafe.funptr_args2
    , Example.Unsafe.funptr_args3
    , Example.Unsafe.funptr_args4
    , Example.Unsafe.funptr_args5
    , Example.Unsafe.comments1
    , Example.Unsafe.const_prim_before1
    , Example.Unsafe.const_prim_before2
    , Example.Unsafe.const_prim_before3
    , Example.Unsafe.const_prim_after1
    , Example.Unsafe.const_prim_after2
    , Example.Unsafe.const_prim_after3
    , Example.Unsafe.const_withoutSign_before1
    , Example.Unsafe.const_withoutSign_before2
    , Example.Unsafe.const_withoutSign_before3
    , Example.Unsafe.const_withoutSign_before4
    , Example.Unsafe.const_withoutSign_before5
    , Example.Unsafe.const_withoutSign_before6
    , Example.Unsafe.const_withoutSign_before7
    , Example.Unsafe.const_withoutSign_before8
    , Example.Unsafe.const_withoutSign_after1
    , Example.Unsafe.const_withoutSign_after2
    , Example.Unsafe.const_withoutSign_after3
    , Example.Unsafe.const_withoutSign_after4
    , Example.Unsafe.const_withoutSign_after5
    , Example.Unsafe.const_withoutSign_after6
    , Example.Unsafe.const_withoutSign_after7
    , Example.Unsafe.const_withoutSign_after8
    , Example.Unsafe.const_pointers_args1
    , Example.Unsafe.const_pointers_args2
    , Example.Unsafe.const_pointers_args3
    , Example.Unsafe.const_pointers_args4
    , Example.Unsafe.const_pointers_args5
    , Example.Unsafe.const_pointers_ret1
    , Example.Unsafe.const_pointers_ret2
    , Example.Unsafe.const_pointers_ret3
    , Example.Unsafe.const_pointers_ret4
    , Example.Unsafe.const_pointers_ret5
    , Example.Unsafe.const_array_elem1
    , Example.Unsafe.const_array_elem2
    , Example.Unsafe.const_array_elem3
    , Example.Unsafe.noParams1
    , Example.Unsafe.noParams2
    , Example.Unsafe.noParams3
    , Example.Unsafe.funptr_ret1
    , Example.Unsafe.funptr_ret2
    , Example.Unsafe.funptr_ret3
    , Example.Unsafe.funptr_ret4
    , Example.Unsafe.funptr_ret5
    , Example.Unsafe.funptr_ret6
    , Example.Unsafe.funptr_ret7
    , Example.Unsafe.funptr_ret8
    , Example.Unsafe.funptr_ret9
    , Example.Unsafe.funptr_ret10
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
  , "void hs_bindgen_7120bf28be59dc98 ("
  , "  A arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  (args_char1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_5b4ad7d44e3f086f ("
  , "  A arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  (args_char2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b286c71056368aae ("
  , "  A arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  (args_char3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_195843b1e639fa42 ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0ed2b2e8501a6583 ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_dbf6a21fc347a71f ("
  , "  A arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  (args_short3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_fd5aeac17d10721d ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_61ff15fee2bf0fe2 ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8b496c277123dccb ("
  , "  A arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  (args_int3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1b9edf583e26451f ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1f3feefab236a8fc ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0863a21bb1a0b701 ("
  , "  A arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  (args_long3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c1bce724238dfe36 ("
  , "  A arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  (args_float)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_14ae2e463e944013 ("
  , "  A arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  (args_double)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_61893a4cc1289d48 ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (args_bool1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4f87d1bec68f8cae ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  (args_struct)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_39ba431aa922b6d8 ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  (args_union)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_1e94271befd55624 ("
  , "  A arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  (args_enum)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_92d5f1f809157cfe ("
  , "  A arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  (args_pointer1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f3f1a149a0dfc8b6 ("
  , "  A arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  (args_pointer2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_be52bde2004c812a ("
  , "  A arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  (args_pointer3)(arg1, arg2);"
  , "}"
  , "A hs_bindgen_1ee51fe72dfbf7aa (void)"
  , "{"
  , "  return (ret_A)();"
  , "}"
  , "char hs_bindgen_68c41d478ac517fb ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_char1)(arg1);"
  , "}"
  , "signed char hs_bindgen_b5c7fa2085ff0c7a ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_char2)(arg1);"
  , "}"
  , "unsigned char hs_bindgen_2648f9c7ea349022 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_char3)(arg1);"
  , "}"
  , "signed short hs_bindgen_3feeb960b95ae4a5 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_short1)(arg1);"
  , "}"
  , "signed short hs_bindgen_df4d769b14e6056b ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_short2)(arg1);"
  , "}"
  , "unsigned short hs_bindgen_3ce64e1d56024105 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_short3)(arg1);"
  , "}"
  , "signed int hs_bindgen_37b30743bce1e0e4 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_int1)(arg1);"
  , "}"
  , "signed int hs_bindgen_da790785d79d0051 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_int2)(arg1);"
  , "}"
  , "unsigned int hs_bindgen_bb4a991d62deb32b ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_int3)(arg1);"
  , "}"
  , "signed long hs_bindgen_2470ae82515dce9f ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_long1)(arg1);"
  , "}"
  , "signed long hs_bindgen_4b434b0c95d3d37c ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_long2)(arg1);"
  , "}"
  , "unsigned long hs_bindgen_c819a4fff50a42a6 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_long3)(arg1);"
  , "}"
  , "float hs_bindgen_136c64b637e7bbb3 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_float)(arg1);"
  , "}"
  , "double hs_bindgen_c4c2254fb77ba24e ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_double)(arg1);"
  , "}"
  , "_Bool hs_bindgen_c24e296433d24518 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_bool1)(arg1);"
  , "}"
  , "void hs_bindgen_2035194cb0d0bc26 ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_struct)(arg1);"
  , "}"
  , "void hs_bindgen_4627f5e3ad35e47f ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_union)(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_22b5c57b479fb99a ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_enum)(arg1);"
  , "}"
  , "signed int *hs_bindgen_051b24e54e95ce42 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_pointer1)(arg1);"
  , "}"
  , "signed int **hs_bindgen_9fc7c9f02aa9b6ae ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_pointer2)(arg1);"
  , "}"
  , "void *hs_bindgen_d55a1ed789d0387e ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_pointer3)(arg1);"
  , "}"
  , "signed int hs_bindgen_5b9127f71e6169d4 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (body1)(arg1);"
  , "}"
  , "A hs_bindgen_a73f0a2b63fb6bc9 (void)"
  , "{"
  , "  return (body2)();"
  , "}"
  , "void hs_bindgen_895f65284d018f8c ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_float)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_95e729d8ff7288e5 ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_double)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_f780f4433c613cf4 ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_float)(arg1);"
  , "}"
  , "void hs_bindgen_0767e558f4d043ca ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_double)(arg1);"
  , "}"
  , "void hs_bindgen_79eb94018bda9f5a ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (bespoke_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_df255661241764bc ("
  , "  A arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  (bespoke_args2)(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_234d2f6a3e972014 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret1)(arg1);"
  , "}"
  , "size_t hs_bindgen_1fc3e6760c0579ec ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret2)(arg1);"
  , "}"
  , "void hs_bindgen_4e7f7359247e8427 ("
  , "  A arg1[]"
  , ")"
  , "{"
  , "  (arr_args1)(arg1);"
  , "}"
  , "void hs_bindgen_21c3769b774c4fd2 ("
  , "  A *arg1[]"
  , ")"
  , "{"
  , "  (arr_args2)(arg1);"
  , "}"
  , "void hs_bindgen_fd1f50253a7f7b5b ("
  , "  A arg1[5]"
  , ")"
  , "{"
  , "  (arr_args3)(arg1);"
  , "}"
  , "void hs_bindgen_2a7aee1f072882bc ("
  , "  A *arg1[5]"
  , ")"
  , "{"
  , "  (arr_args4)(arg1);"
  , "}"
  , "void hs_bindgen_7fb2b902258e0b54 ("
  , "  A arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_106466b4fcfd40fd ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_77722e234ed79be7 ("
  , "  A arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_304e180456b9aa07 ("
  , "  A arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0d545f6023860270 ("
  , "  A arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args5)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f00d052491ecc40c ("
  , "  A arg1"
  , ")"
  , "{"
  , "  (comments1)(arg1);"
  , "}"
  , "void hs_bindgen_8afc86e6d0b1fc02 ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_372464519af88007 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4468eb2b7b86cc70 ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a4066167e4eac3fd ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b67a87e388874d70 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_05c4dd640c52aa53 ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ad9b37edd62f7fc0 ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d5772c034da42704 ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e9e661d8d7d5674b ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_18a86617539b0cbb ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_3bef29627e6a7c17 ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_60056ee7f5cdcfcb ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d911d05eedf544f0 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e1f447f72f4b7d48 ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_868032666da9209c ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a23b22dbfdd59e29 ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a5a6be2116e77b14 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_afa897f76b1ad851 ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_cee24fcdc4d5d759 ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_0f72739d4e75813a ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_cb8d13328f97418c ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_cc817cc1ec0b5db8 ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0297c1c8701428e2 ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a8a508f487c4ff4c ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4d7ff5fabe5f5355 ("
  , "  A arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e4967b54bde0a75f ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_70df10cdcb3ad9ba ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args5)(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_a0550dc996ed14e4 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret1)(arg1);"
  , "}"
  , "signed int const *hs_bindgen_bddd020480d5475f ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret2)(arg1);"
  , "}"
  , "signed int *const hs_bindgen_66ba83341a10488f ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret3)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_5365bdd9bb9b376b ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret4)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_98b79f695f2953cb ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret5)(arg1);"
  , "}"
  , "void hs_bindgen_6759e0a2cb860c2e ("
  , "  A const arg1[]"
  , ")"
  , "{"
  , "  (const_array_elem1)(arg1);"
  , "}"
  , "void hs_bindgen_6cfc311ce355ef8d ("
  , "  A const *arg1[]"
  , ")"
  , "{"
  , "  (const_array_elem2)(arg1);"
  , "}"
  , "void hs_bindgen_a82b183c857e54bf ("
  , "  A *const arg1[]"
  , ")"
  , "{"
  , "  (const_array_elem3)(arg1);"
  , "}"
  , "A hs_bindgen_546885c1cf5918e4 (void)"
  , "{"
  , "  return (noParams1)();"
  , "}"
  , "A hs_bindgen_448ffd4af386843c (void)"
  , "{"
  , "  return (noParams2)();"
  , "}"
  , "void hs_bindgen_d39a1f2aa35317eb ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (noParams3)(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_e18fdd7d1b192d86 ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_b76e1caa4d3ac3eb ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret2)(arg1);"
  , "}"
  , "void (*hs_bindgen_9c27f176dd302129 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (funptr_ret3)(arg1);"
  , "}"
  , "char (*hs_bindgen_550b40d1cf52abe9 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret4)(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_948a3667e352d2d4 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret5)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_a4848da8e7fc3ee4 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret6)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_6b9e78d0d74a8524 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret7)(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_a907856761fe94ab ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret8)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_e47275c0159a6da9 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret9)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_fb11b16d0e3bd82a ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret10)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse Example Unsafe_args_char1@
foreign import ccall unsafe "hs_bindgen_7120bf28be59dc98" hs_bindgen_7120bf28be59dc98_base ::
     BG.CInt
  -> BG.CChar
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_char1@
hs_bindgen_7120bf28be59dc98 ::
     A
  -> BG.CChar
  -> IO ()
hs_bindgen_7120bf28be59dc98 =
  \x0 ->
    \x1 ->
      hs_bindgen_7120bf28be59dc98_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| Function declarations

    __C declaration:__ @args_char1@

    __defined at:__ @macros\/reparse.h 17:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_char1 = hs_bindgen_7120bf28be59dc98

-- __unique:__ @test_macrosreparse Example Unsafe_args_char2@
foreign import ccall unsafe "hs_bindgen_5b4ad7d44e3f086f" hs_bindgen_5b4ad7d44e3f086f_base ::
     BG.CInt
  -> BG.CSChar
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_char2@
hs_bindgen_5b4ad7d44e3f086f ::
     A
  -> BG.CSChar
  -> IO ()
hs_bindgen_5b4ad7d44e3f086f =
  \x0 ->
    \x1 ->
      hs_bindgen_5b4ad7d44e3f086f_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_char2@

    __defined at:__ @macros\/reparse.h 18:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CSChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_char2 = hs_bindgen_5b4ad7d44e3f086f

-- __unique:__ @test_macrosreparse Example Unsafe_args_char3@
foreign import ccall unsafe "hs_bindgen_b286c71056368aae" hs_bindgen_b286c71056368aae_base ::
     BG.CInt
  -> BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_char3@
hs_bindgen_b286c71056368aae ::
     A
  -> BG.CUChar
  -> IO ()
hs_bindgen_b286c71056368aae =
  \x0 ->
    \x1 ->
      hs_bindgen_b286c71056368aae_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_char3@

    __defined at:__ @macros\/reparse.h 19:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CUChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_char3 = hs_bindgen_b286c71056368aae

-- __unique:__ @test_macrosreparse Example Unsafe_args_short1@
foreign import ccall unsafe "hs_bindgen_195843b1e639fa42" hs_bindgen_195843b1e639fa42_base ::
     BG.CInt
  -> BG.CShort
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_short1@
hs_bindgen_195843b1e639fa42 ::
     A
  -> BG.CShort
  -> IO ()
hs_bindgen_195843b1e639fa42 =
  \x0 ->
    \x1 ->
      hs_bindgen_195843b1e639fa42_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_short1@

    __defined at:__ @macros\/reparse.h 21:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CShort
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_short1 = hs_bindgen_195843b1e639fa42

-- __unique:__ @test_macrosreparse Example Unsafe_args_short2@
foreign import ccall unsafe "hs_bindgen_0ed2b2e8501a6583" hs_bindgen_0ed2b2e8501a6583_base ::
     BG.CInt
  -> BG.CShort
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_short2@
hs_bindgen_0ed2b2e8501a6583 ::
     A
  -> BG.CShort
  -> IO ()
hs_bindgen_0ed2b2e8501a6583 =
  \x0 ->
    \x1 ->
      hs_bindgen_0ed2b2e8501a6583_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_short2@

    __defined at:__ @macros\/reparse.h 22:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CShort
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_short2 = hs_bindgen_0ed2b2e8501a6583

-- __unique:__ @test_macrosreparse Example Unsafe_args_short3@
foreign import ccall unsafe "hs_bindgen_dbf6a21fc347a71f" hs_bindgen_dbf6a21fc347a71f_base ::
     BG.CInt
  -> BG.CUShort
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_short3@
hs_bindgen_dbf6a21fc347a71f ::
     A
  -> BG.CUShort
  -> IO ()
hs_bindgen_dbf6a21fc347a71f =
  \x0 ->
    \x1 ->
      hs_bindgen_dbf6a21fc347a71f_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_short3@

    __defined at:__ @macros\/reparse.h 23:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CUShort
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_short3 = hs_bindgen_dbf6a21fc347a71f

-- __unique:__ @test_macrosreparse Example Unsafe_args_int1@
foreign import ccall unsafe "hs_bindgen_fd5aeac17d10721d" hs_bindgen_fd5aeac17d10721d_base ::
     BG.CInt
  -> BG.CInt
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_int1@
hs_bindgen_fd5aeac17d10721d ::
     A
  -> BG.CInt
  -> IO ()
hs_bindgen_fd5aeac17d10721d =
  \x0 ->
    \x1 ->
      hs_bindgen_fd5aeac17d10721d_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_int1@

    __defined at:__ @macros\/reparse.h 25:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_int1 = hs_bindgen_fd5aeac17d10721d

-- __unique:__ @test_macrosreparse Example Unsafe_args_int2@
foreign import ccall unsafe "hs_bindgen_61ff15fee2bf0fe2" hs_bindgen_61ff15fee2bf0fe2_base ::
     BG.CInt
  -> BG.CInt
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_int2@
hs_bindgen_61ff15fee2bf0fe2 ::
     A
  -> BG.CInt
  -> IO ()
hs_bindgen_61ff15fee2bf0fe2 =
  \x0 ->
    \x1 ->
      hs_bindgen_61ff15fee2bf0fe2_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_int2@

    __defined at:__ @macros\/reparse.h 26:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_int2 = hs_bindgen_61ff15fee2bf0fe2

-- __unique:__ @test_macrosreparse Example Unsafe_args_int3@
foreign import ccall unsafe "hs_bindgen_8b496c277123dccb" hs_bindgen_8b496c277123dccb_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_int3@
hs_bindgen_8b496c277123dccb ::
     A
  -> BG.CUInt
  -> IO ()
hs_bindgen_8b496c277123dccb =
  \x0 ->
    \x1 ->
      hs_bindgen_8b496c277123dccb_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_int3@

    __defined at:__ @macros\/reparse.h 27:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CUInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_int3 = hs_bindgen_8b496c277123dccb

-- __unique:__ @test_macrosreparse Example Unsafe_args_long1@
foreign import ccall unsafe "hs_bindgen_1b9edf583e26451f" hs_bindgen_1b9edf583e26451f_base ::
     BG.CInt
  -> BG.CLong
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_long1@
hs_bindgen_1b9edf583e26451f ::
     A
  -> BG.CLong
  -> IO ()
hs_bindgen_1b9edf583e26451f =
  \x0 ->
    \x1 ->
      hs_bindgen_1b9edf583e26451f_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_long1@

    __defined at:__ @macros\/reparse.h 29:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CLong
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_long1 = hs_bindgen_1b9edf583e26451f

-- __unique:__ @test_macrosreparse Example Unsafe_args_long2@
foreign import ccall unsafe "hs_bindgen_1f3feefab236a8fc" hs_bindgen_1f3feefab236a8fc_base ::
     BG.CInt
  -> BG.CLong
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_long2@
hs_bindgen_1f3feefab236a8fc ::
     A
  -> BG.CLong
  -> IO ()
hs_bindgen_1f3feefab236a8fc =
  \x0 ->
    \x1 ->
      hs_bindgen_1f3feefab236a8fc_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_long2@

    __defined at:__ @macros\/reparse.h 30:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CLong
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_long2 = hs_bindgen_1f3feefab236a8fc

-- __unique:__ @test_macrosreparse Example Unsafe_args_long3@
foreign import ccall unsafe "hs_bindgen_0863a21bb1a0b701" hs_bindgen_0863a21bb1a0b701_base ::
     BG.CInt
  -> BG.CULong
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_long3@
hs_bindgen_0863a21bb1a0b701 ::
     A
  -> BG.CULong
  -> IO ()
hs_bindgen_0863a21bb1a0b701 =
  \x0 ->
    \x1 ->
      hs_bindgen_0863a21bb1a0b701_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_long3@

    __defined at:__ @macros\/reparse.h 31:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CULong
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_long3 = hs_bindgen_0863a21bb1a0b701

-- __unique:__ @test_macrosreparse Example Unsafe_args_float@
foreign import ccall unsafe "hs_bindgen_c1bce724238dfe36" hs_bindgen_c1bce724238dfe36_base ::
     BG.CInt
  -> BG.CFloat
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_float@
hs_bindgen_c1bce724238dfe36 ::
     A
  -> BG.CFloat
  -> IO ()
hs_bindgen_c1bce724238dfe36 =
  \x0 ->
    \x1 ->
      hs_bindgen_c1bce724238dfe36_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_float@

    __defined at:__ @macros\/reparse.h 33:6@

    __exported by:__ @macros\/reparse.h@
-}
args_float ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_float = hs_bindgen_c1bce724238dfe36

-- __unique:__ @test_macrosreparse Example Unsafe_args_double@
foreign import ccall unsafe "hs_bindgen_14ae2e463e944013" hs_bindgen_14ae2e463e944013_base ::
     BG.CInt
  -> BG.CDouble
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_double@
hs_bindgen_14ae2e463e944013 ::
     A
  -> BG.CDouble
  -> IO ()
hs_bindgen_14ae2e463e944013 =
  \x0 ->
    \x1 ->
      hs_bindgen_14ae2e463e944013_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_double@

    __defined at:__ @macros\/reparse.h 34:6@

    __exported by:__ @macros\/reparse.h@
-}
args_double ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_double = hs_bindgen_14ae2e463e944013

-- __unique:__ @test_macrosreparse Example Unsafe_args_bool1@
foreign import ccall unsafe "hs_bindgen_61893a4cc1289d48" hs_bindgen_61893a4cc1289d48_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_bool1@
hs_bindgen_61893a4cc1289d48 ::
     A
  -> BG.CBool
  -> IO ()
hs_bindgen_61893a4cc1289d48 =
  \x0 ->
    \x1 ->
      hs_bindgen_61893a4cc1289d48_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_bool1@

    __defined at:__ @macros\/reparse.h 35:6@

    __exported by:__ @macros\/reparse.h@
-}
args_bool1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_bool1 = hs_bindgen_61893a4cc1289d48

-- __unique:__ @test_macrosreparse Example Unsafe_args_struct@
foreign import ccall unsafe "hs_bindgen_4f87d1bec68f8cae" hs_bindgen_4f87d1bec68f8cae_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_struct@
hs_bindgen_4f87d1bec68f8cae ::
     A
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_4f87d1bec68f8cae =
  \x0 ->
    \x1 ->
      hs_bindgen_4f87d1bec68f8cae_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_struct@

    __defined at:__ @macros\/reparse.h 37:6@

    __exported by:__ @macros\/reparse.h@
-}
args_struct ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_struct
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_struct =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_4f87d1bec68f8cae arg10 arg22)

-- __unique:__ @test_macrosreparse Example Unsafe_args_union@
foreign import ccall unsafe "hs_bindgen_39ba431aa922b6d8" hs_bindgen_39ba431aa922b6d8_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_union@
hs_bindgen_39ba431aa922b6d8 ::
     A
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_39ba431aa922b6d8 =
  \x0 ->
    \x1 ->
      hs_bindgen_39ba431aa922b6d8_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_union@

    __defined at:__ @macros\/reparse.h 38:6@

    __exported by:__ @macros\/reparse.h@
-}
args_union ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_union
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_union =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_39ba431aa922b6d8 arg10 arg22)

-- __unique:__ @test_macrosreparse Example Unsafe_args_enum@
foreign import ccall unsafe "hs_bindgen_1e94271befd55624" hs_bindgen_1e94271befd55624_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_enum@
hs_bindgen_1e94271befd55624 ::
     A
  -> Some_enum
  -> IO ()
hs_bindgen_1e94271befd55624 =
  \x0 ->
    \x1 ->
      hs_bindgen_1e94271befd55624_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_enum@

    __defined at:__ @macros\/reparse.h 39:6@

    __exported by:__ @macros\/reparse.h@
-}
args_enum ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_enum
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_enum = hs_bindgen_1e94271befd55624

-- __unique:__ @test_macrosreparse Example Unsafe_args_pointer1@
foreign import ccall unsafe "hs_bindgen_92d5f1f809157cfe" hs_bindgen_92d5f1f809157cfe_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_pointer1@
hs_bindgen_92d5f1f809157cfe ::
     A
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_92d5f1f809157cfe =
  \x0 ->
    \x1 ->
      hs_bindgen_92d5f1f809157cfe_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_pointer1@

    __defined at:__ @macros\/reparse.h 41:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.Ptr BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_pointer1 = hs_bindgen_92d5f1f809157cfe

-- __unique:__ @test_macrosreparse Example Unsafe_args_pointer2@
foreign import ccall unsafe "hs_bindgen_f3f1a149a0dfc8b6" hs_bindgen_f3f1a149a0dfc8b6_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_pointer2@
hs_bindgen_f3f1a149a0dfc8b6 ::
     A
  -> BG.Ptr (BG.Ptr BG.CInt)
  -> IO ()
hs_bindgen_f3f1a149a0dfc8b6 =
  \x0 ->
    \x1 ->
      hs_bindgen_f3f1a149a0dfc8b6_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_pointer2@

    __defined at:__ @macros\/reparse.h 42:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.Ptr (BG.Ptr BG.CInt)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_pointer2 = hs_bindgen_f3f1a149a0dfc8b6

-- __unique:__ @test_macrosreparse Example Unsafe_args_pointer3@
foreign import ccall unsafe "hs_bindgen_be52bde2004c812a" hs_bindgen_be52bde2004c812a_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_pointer3@
hs_bindgen_be52bde2004c812a ::
     A
  -> BG.Ptr BG.Void
  -> IO ()
hs_bindgen_be52bde2004c812a =
  \x0 ->
    \x1 ->
      hs_bindgen_be52bde2004c812a_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_pointer3@

    __defined at:__ @macros\/reparse.h 43:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.Ptr BG.Void
     -- ^ __C declaration:__ @arg3@
  -> IO ()
args_pointer3 = hs_bindgen_be52bde2004c812a

-- __unique:__ @test_macrosreparse Example Unsafe_ret_A@
foreign import ccall unsafe "hs_bindgen_1ee51fe72dfbf7aa" hs_bindgen_1ee51fe72dfbf7aa_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse Example Unsafe_ret_A@
hs_bindgen_1ee51fe72dfbf7aa :: IO A
hs_bindgen_1ee51fe72dfbf7aa =
  fmap BG.fromFFIType hs_bindgen_1ee51fe72dfbf7aa_base

{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h 47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A :: IO A
ret_A = hs_bindgen_1ee51fe72dfbf7aa

-- __unique:__ @test_macrosreparse Example Unsafe_ret_char1@
foreign import ccall unsafe "hs_bindgen_68c41d478ac517fb" hs_bindgen_68c41d478ac517fb_base ::
     BG.CInt
  -> IO BG.CChar

-- __unique:__ @test_macrosreparse Example Unsafe_ret_char1@
hs_bindgen_68c41d478ac517fb ::
     A
  -> IO BG.CChar
hs_bindgen_68c41d478ac517fb =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_68c41d478ac517fb_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h 49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CChar
ret_char1 = hs_bindgen_68c41d478ac517fb

-- __unique:__ @test_macrosreparse Example Unsafe_ret_char2@
foreign import ccall unsafe "hs_bindgen_b5c7fa2085ff0c7a" hs_bindgen_b5c7fa2085ff0c7a_base ::
     BG.CInt
  -> IO BG.CSChar

-- __unique:__ @test_macrosreparse Example Unsafe_ret_char2@
hs_bindgen_b5c7fa2085ff0c7a ::
     A
  -> IO BG.CSChar
hs_bindgen_b5c7fa2085ff0c7a =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_b5c7fa2085ff0c7a_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h 50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CSChar
ret_char2 = hs_bindgen_b5c7fa2085ff0c7a

-- __unique:__ @test_macrosreparse Example Unsafe_ret_char3@
foreign import ccall unsafe "hs_bindgen_2648f9c7ea349022" hs_bindgen_2648f9c7ea349022_base ::
     BG.CInt
  -> IO BG.CUChar

-- __unique:__ @test_macrosreparse Example Unsafe_ret_char3@
hs_bindgen_2648f9c7ea349022 ::
     A
  -> IO BG.CUChar
hs_bindgen_2648f9c7ea349022 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_2648f9c7ea349022_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h 51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUChar
ret_char3 = hs_bindgen_2648f9c7ea349022

-- __unique:__ @test_macrosreparse Example Unsafe_ret_short1@
foreign import ccall unsafe "hs_bindgen_3feeb960b95ae4a5" hs_bindgen_3feeb960b95ae4a5_base ::
     BG.CInt
  -> IO BG.CShort

-- __unique:__ @test_macrosreparse Example Unsafe_ret_short1@
hs_bindgen_3feeb960b95ae4a5 ::
     A
  -> IO BG.CShort
hs_bindgen_3feeb960b95ae4a5 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_3feeb960b95ae4a5_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h 53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short1 = hs_bindgen_3feeb960b95ae4a5

-- __unique:__ @test_macrosreparse Example Unsafe_ret_short2@
foreign import ccall unsafe "hs_bindgen_df4d769b14e6056b" hs_bindgen_df4d769b14e6056b_base ::
     BG.CInt
  -> IO BG.CShort

-- __unique:__ @test_macrosreparse Example Unsafe_ret_short2@
hs_bindgen_df4d769b14e6056b ::
     A
  -> IO BG.CShort
hs_bindgen_df4d769b14e6056b =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_df4d769b14e6056b_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h 54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short2 = hs_bindgen_df4d769b14e6056b

-- __unique:__ @test_macrosreparse Example Unsafe_ret_short3@
foreign import ccall unsafe "hs_bindgen_3ce64e1d56024105" hs_bindgen_3ce64e1d56024105_base ::
     BG.CInt
  -> IO BG.CUShort

-- __unique:__ @test_macrosreparse Example Unsafe_ret_short3@
hs_bindgen_3ce64e1d56024105 ::
     A
  -> IO BG.CUShort
hs_bindgen_3ce64e1d56024105 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_3ce64e1d56024105_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h 55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUShort
ret_short3 = hs_bindgen_3ce64e1d56024105

-- __unique:__ @test_macrosreparse Example Unsafe_ret_int1@
foreign import ccall unsafe "hs_bindgen_37b30743bce1e0e4" hs_bindgen_37b30743bce1e0e4_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparse Example Unsafe_ret_int1@
hs_bindgen_37b30743bce1e0e4 ::
     A
  -> IO BG.CInt
hs_bindgen_37b30743bce1e0e4 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_37b30743bce1e0e4_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h 57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int1 = hs_bindgen_37b30743bce1e0e4

-- __unique:__ @test_macrosreparse Example Unsafe_ret_int2@
foreign import ccall unsafe "hs_bindgen_da790785d79d0051" hs_bindgen_da790785d79d0051_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparse Example Unsafe_ret_int2@
hs_bindgen_da790785d79d0051 ::
     A
  -> IO BG.CInt
hs_bindgen_da790785d79d0051 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_da790785d79d0051_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h 58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int2 = hs_bindgen_da790785d79d0051

-- __unique:__ @test_macrosreparse Example Unsafe_ret_int3@
foreign import ccall unsafe "hs_bindgen_bb4a991d62deb32b" hs_bindgen_bb4a991d62deb32b_base ::
     BG.CInt
  -> IO BG.CUInt

-- __unique:__ @test_macrosreparse Example Unsafe_ret_int3@
hs_bindgen_bb4a991d62deb32b ::
     A
  -> IO BG.CUInt
hs_bindgen_bb4a991d62deb32b =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_bb4a991d62deb32b_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h 59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUInt
ret_int3 = hs_bindgen_bb4a991d62deb32b

-- __unique:__ @test_macrosreparse Example Unsafe_ret_long1@
foreign import ccall unsafe "hs_bindgen_2470ae82515dce9f" hs_bindgen_2470ae82515dce9f_base ::
     BG.CInt
  -> IO BG.CLong

-- __unique:__ @test_macrosreparse Example Unsafe_ret_long1@
hs_bindgen_2470ae82515dce9f ::
     A
  -> IO BG.CLong
hs_bindgen_2470ae82515dce9f =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_2470ae82515dce9f_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h 61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long1 = hs_bindgen_2470ae82515dce9f

-- __unique:__ @test_macrosreparse Example Unsafe_ret_long2@
foreign import ccall unsafe "hs_bindgen_4b434b0c95d3d37c" hs_bindgen_4b434b0c95d3d37c_base ::
     BG.CInt
  -> IO BG.CLong

-- __unique:__ @test_macrosreparse Example Unsafe_ret_long2@
hs_bindgen_4b434b0c95d3d37c ::
     A
  -> IO BG.CLong
hs_bindgen_4b434b0c95d3d37c =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_4b434b0c95d3d37c_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h 62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long2 = hs_bindgen_4b434b0c95d3d37c

-- __unique:__ @test_macrosreparse Example Unsafe_ret_long3@
foreign import ccall unsafe "hs_bindgen_c819a4fff50a42a6" hs_bindgen_c819a4fff50a42a6_base ::
     BG.CInt
  -> IO BG.CULong

-- __unique:__ @test_macrosreparse Example Unsafe_ret_long3@
hs_bindgen_c819a4fff50a42a6 ::
     A
  -> IO BG.CULong
hs_bindgen_c819a4fff50a42a6 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_c819a4fff50a42a6_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h 63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CULong
ret_long3 = hs_bindgen_c819a4fff50a42a6

-- __unique:__ @test_macrosreparse Example Unsafe_ret_float@
foreign import ccall unsafe "hs_bindgen_136c64b637e7bbb3" hs_bindgen_136c64b637e7bbb3_base ::
     BG.CInt
  -> IO BG.CFloat

-- __unique:__ @test_macrosreparse Example Unsafe_ret_float@
hs_bindgen_136c64b637e7bbb3 ::
     A
  -> IO BG.CFloat
hs_bindgen_136c64b637e7bbb3 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_136c64b637e7bbb3_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h 65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CFloat
ret_float = hs_bindgen_136c64b637e7bbb3

-- __unique:__ @test_macrosreparse Example Unsafe_ret_double@
foreign import ccall unsafe "hs_bindgen_c4c2254fb77ba24e" hs_bindgen_c4c2254fb77ba24e_base ::
     BG.CInt
  -> IO BG.CDouble

-- __unique:__ @test_macrosreparse Example Unsafe_ret_double@
hs_bindgen_c4c2254fb77ba24e ::
     A
  -> IO BG.CDouble
hs_bindgen_c4c2254fb77ba24e =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_c4c2254fb77ba24e_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h 66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CDouble
ret_double = hs_bindgen_c4c2254fb77ba24e

-- __unique:__ @test_macrosreparse Example Unsafe_ret_bool1@
foreign import ccall unsafe "hs_bindgen_c24e296433d24518" hs_bindgen_c24e296433d24518_base ::
     BG.CInt
  -> IO BG.CBool

-- __unique:__ @test_macrosreparse Example Unsafe_ret_bool1@
hs_bindgen_c24e296433d24518 ::
     A
  -> IO BG.CBool
hs_bindgen_c24e296433d24518 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_c24e296433d24518_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h 67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
ret_bool1 = hs_bindgen_c24e296433d24518

-- __unique:__ @test_macrosreparse Example Unsafe_ret_struct@
foreign import ccall unsafe "hs_bindgen_2035194cb0d0bc26" hs_bindgen_2035194cb0d0bc26_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_ret_struct@
hs_bindgen_2035194cb0d0bc26 ::
     A
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_2035194cb0d0bc26 =
  \x0 ->
    \x1 ->
      hs_bindgen_2035194cb0d0bc26_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @ret_struct@

    __defined at:__ @macros\/reparse.h 69:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_struct ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO Some_struct
ret_struct =
  \arg10 ->
    BG.allocaAndPeek (\res1 ->
                        hs_bindgen_2035194cb0d0bc26 arg10 res1)

-- __unique:__ @test_macrosreparse Example Unsafe_ret_union@
foreign import ccall unsafe "hs_bindgen_4627f5e3ad35e47f" hs_bindgen_4627f5e3ad35e47f_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_ret_union@
hs_bindgen_4627f5e3ad35e47f ::
     A
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_4627f5e3ad35e47f =
  \x0 ->
    \x1 ->
      hs_bindgen_4627f5e3ad35e47f_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @ret_union@

    __defined at:__ @macros\/reparse.h 70:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_union ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO Some_union
ret_union =
  \arg10 ->
    BG.allocaAndPeek (\res1 ->
                        hs_bindgen_4627f5e3ad35e47f arg10 res1)

-- __unique:__ @test_macrosreparse Example Unsafe_ret_enum@
foreign import ccall unsafe "hs_bindgen_22b5c57b479fb99a" hs_bindgen_22b5c57b479fb99a_base ::
     BG.CInt
  -> IO BG.CUInt

-- __unique:__ @test_macrosreparse Example Unsafe_ret_enum@
hs_bindgen_22b5c57b479fb99a ::
     A
  -> IO Some_enum
hs_bindgen_22b5c57b479fb99a =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_22b5c57b479fb99a_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h 71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO Some_enum
ret_enum = hs_bindgen_22b5c57b479fb99a

-- __unique:__ @test_macrosreparse Example Unsafe_ret_pointer1@
foreign import ccall unsafe "hs_bindgen_051b24e54e95ce42" hs_bindgen_051b24e54e95ce42_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse Example Unsafe_ret_pointer1@
hs_bindgen_051b24e54e95ce42 ::
     A
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_051b24e54e95ce42 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_051b24e54e95ce42_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h 73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
ret_pointer1 = hs_bindgen_051b24e54e95ce42

-- __unique:__ @test_macrosreparse Example Unsafe_ret_pointer2@
foreign import ccall unsafe "hs_bindgen_9fc7c9f02aa9b6ae" hs_bindgen_9fc7c9f02aa9b6ae_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse Example Unsafe_ret_pointer2@
hs_bindgen_9fc7c9f02aa9b6ae ::
     A
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
hs_bindgen_9fc7c9f02aa9b6ae =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_9fc7c9f02aa9b6ae_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h 74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
ret_pointer2 = hs_bindgen_9fc7c9f02aa9b6ae

-- __unique:__ @test_macrosreparse Example Unsafe_ret_pointer3@
foreign import ccall unsafe "hs_bindgen_d55a1ed789d0387e" hs_bindgen_d55a1ed789d0387e_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse Example Unsafe_ret_pointer3@
hs_bindgen_d55a1ed789d0387e ::
     A
  -> IO (BG.Ptr BG.Void)
hs_bindgen_d55a1ed789d0387e =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_d55a1ed789d0387e_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h 75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.Void)
ret_pointer3 = hs_bindgen_d55a1ed789d0387e

-- __unique:__ @test_macrosreparse Example Unsafe_body1@
foreign import ccall unsafe "hs_bindgen_5b9127f71e6169d4" hs_bindgen_5b9127f71e6169d4_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparse Example Unsafe_body1@
hs_bindgen_5b9127f71e6169d4 ::
     A
  -> IO BG.CInt
hs_bindgen_5b9127f71e6169d4 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_5b9127f71e6169d4_base (BG.toFFIType x0))

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h 79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
body1 = hs_bindgen_5b9127f71e6169d4

-- __unique:__ @test_macrosreparse Example Unsafe_body2@
foreign import ccall unsafe "hs_bindgen_a73f0a2b63fb6bc9" hs_bindgen_a73f0a2b63fb6bc9_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse Example Unsafe_body2@
hs_bindgen_a73f0a2b63fb6bc9 :: IO A
hs_bindgen_a73f0a2b63fb6bc9 =
  fmap BG.fromFFIType hs_bindgen_a73f0a2b63fb6bc9_base

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h 80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2 :: IO A
body2 = hs_bindgen_a73f0a2b63fb6bc9

-- __unique:__ @test_macrosreparse Example Unsafe_args_complex_float@
foreign import ccall unsafe "hs_bindgen_895f65284d018f8c" hs_bindgen_895f65284d018f8c_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_complex_float@
hs_bindgen_895f65284d018f8c ::
     A
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_895f65284d018f8c =
  \x0 ->
    \x1 ->
      hs_bindgen_895f65284d018f8c_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_complex_float@

    __defined at:__ @macros\/reparse.h 84:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_float ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.Complex BG.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_complex_float =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_895f65284d018f8c arg10 arg22)

-- __unique:__ @test_macrosreparse Example Unsafe_args_complex_double@
foreign import ccall unsafe "hs_bindgen_95e729d8ff7288e5" hs_bindgen_95e729d8ff7288e5_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_args_complex_double@
hs_bindgen_95e729d8ff7288e5 ::
     A
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_95e729d8ff7288e5 =
  \x0 ->
    \x1 ->
      hs_bindgen_95e729d8ff7288e5_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @args_complex_double@

    __defined at:__ @macros\/reparse.h 85:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_double ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.Complex BG.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_complex_double =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_95e729d8ff7288e5 arg10 arg22)

-- __unique:__ @test_macrosreparse Example Unsafe_ret_complex_float@
foreign import ccall unsafe "hs_bindgen_f780f4433c613cf4" hs_bindgen_f780f4433c613cf4_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_ret_complex_float@
hs_bindgen_f780f4433c613cf4 ::
     A
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_f780f4433c613cf4 =
  \x0 ->
    \x1 ->
      hs_bindgen_f780f4433c613cf4_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @macros\/reparse.h 86:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_float ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Complex BG.CFloat)
ret_complex_float =
  \arg10 ->
    BG.allocaAndPeek (\res1 ->
                        hs_bindgen_f780f4433c613cf4 arg10 res1)

-- __unique:__ @test_macrosreparse Example Unsafe_ret_complex_double@
foreign import ccall unsafe "hs_bindgen_0767e558f4d043ca" hs_bindgen_0767e558f4d043ca_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_ret_complex_double@
hs_bindgen_0767e558f4d043ca ::
     A
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_0767e558f4d043ca =
  \x0 ->
    \x1 ->
      hs_bindgen_0767e558f4d043ca_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @macros\/reparse.h 87:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_double ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Complex BG.CDouble)
ret_complex_double =
  \arg10 ->
    BG.allocaAndPeek (\res1 ->
                        hs_bindgen_0767e558f4d043ca arg10 res1)

-- __unique:__ @test_macrosreparse Example Unsafe_bespoke_args1@
foreign import ccall unsafe "hs_bindgen_79eb94018bda9f5a" hs_bindgen_79eb94018bda9f5a_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_bespoke_args1@
hs_bindgen_79eb94018bda9f5a ::
     A
  -> BG.CBool
  -> IO ()
hs_bindgen_79eb94018bda9f5a =
  \x0 ->
    \x1 ->
      hs_bindgen_79eb94018bda9f5a_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @macros\/reparse.h 94:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
bespoke_args1 = hs_bindgen_79eb94018bda9f5a

-- __unique:__ @test_macrosreparse Example Unsafe_bespoke_args2@
foreign import ccall unsafe "hs_bindgen_df255661241764bc" hs_bindgen_df255661241764bc_base ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_bespoke_args2@
hs_bindgen_df255661241764bc ::
     A
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_df255661241764bc =
  \x0 ->
    \x1 ->
      hs_bindgen_df255661241764bc_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @macros\/reparse.h 95:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.LibC.CSize
     -- ^ __C declaration:__ @arg2@
  -> IO ()
bespoke_args2 = hs_bindgen_df255661241764bc

-- __unique:__ @test_macrosreparse Example Unsafe_bespoke_ret1@
foreign import ccall unsafe "hs_bindgen_234d2f6a3e972014" hs_bindgen_234d2f6a3e972014_base ::
     BG.CInt
  -> IO BG.CBool

-- __unique:__ @test_macrosreparse Example Unsafe_bespoke_ret1@
hs_bindgen_234d2f6a3e972014 ::
     A
  -> IO BG.CBool
hs_bindgen_234d2f6a3e972014 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_234d2f6a3e972014_base (BG.toFFIType x0))

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h 97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
bespoke_ret1 = hs_bindgen_234d2f6a3e972014

-- __unique:__ @test_macrosreparse Example Unsafe_bespoke_ret2@
foreign import ccall unsafe "hs_bindgen_1fc3e6760c0579ec" hs_bindgen_1fc3e6760c0579ec_base ::
     BG.CInt
  -> IO HsBindgen.Runtime.LibC.CSize

-- __unique:__ @test_macrosreparse Example Unsafe_bespoke_ret2@
hs_bindgen_1fc3e6760c0579ec ::
     A
  -> IO HsBindgen.Runtime.LibC.CSize
hs_bindgen_1fc3e6760c0579ec =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_1fc3e6760c0579ec_base (BG.toFFIType x0))

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h 98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO HsBindgen.Runtime.LibC.CSize
bespoke_ret2 = hs_bindgen_1fc3e6760c0579ec

-- __unique:__ @test_macrosreparse Example Unsafe_arr_args1@
foreign import ccall unsafe "hs_bindgen_4e7f7359247e8427" hs_bindgen_4e7f7359247e8427_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_arr_args1@
hs_bindgen_4e7f7359247e8427 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray A))
  -> IO ()
hs_bindgen_4e7f7359247e8427 =
  \x0 ->
    hs_bindgen_4e7f7359247e8427_base (BG.toFFIType x0)

{-| Arrays

    __C declaration:__ @arr_args1@

    __defined at:__ @macros\/reparse.h 104:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args1 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray A))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args1 = hs_bindgen_4e7f7359247e8427

-- __unique:__ @test_macrosreparse Example Unsafe_arr_args2@
foreign import ccall unsafe "hs_bindgen_21c3769b774c4fd2" hs_bindgen_21c3769b774c4fd2_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_arr_args2@
hs_bindgen_21c3769b774c4fd2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr A)))
  -> IO ()
hs_bindgen_21c3769b774c4fd2 =
  \x0 ->
    hs_bindgen_21c3769b774c4fd2_base (BG.toFFIType x0)

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h 105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args2 = hs_bindgen_21c3769b774c4fd2

-- __unique:__ @test_macrosreparse Example Unsafe_arr_args3@
foreign import ccall unsafe "hs_bindgen_fd1f50253a7f7b5b" hs_bindgen_fd1f50253a7f7b5b_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_arr_args3@
hs_bindgen_fd1f50253a7f7b5b ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 A))
  -> IO ()
hs_bindgen_fd1f50253a7f7b5b =
  \x0 ->
    hs_bindgen_fd1f50253a7f7b5b_base (BG.toFFIType x0)

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h 106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 A))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args3 = hs_bindgen_fd1f50253a7f7b5b

-- __unique:__ @test_macrosreparse Example Unsafe_arr_args4@
foreign import ccall unsafe "hs_bindgen_2a7aee1f072882bc" hs_bindgen_2a7aee1f072882bc_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_arr_args4@
hs_bindgen_2a7aee1f072882bc ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr A)))
  -> IO ()
hs_bindgen_2a7aee1f072882bc =
  \x0 ->
    hs_bindgen_2a7aee1f072882bc_base (BG.toFFIType x0)

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h 107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args4 = hs_bindgen_2a7aee1f072882bc

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_args1@
foreign import ccall unsafe "hs_bindgen_7fb2b902258e0b54" hs_bindgen_7fb2b902258e0b54_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_args1@
hs_bindgen_7fb2b902258e0b54 ::
     A
  -> BG.FunPtr (IO ())
  -> IO ()
hs_bindgen_7fb2b902258e0b54 =
  \x0 ->
    \x1 ->
      hs_bindgen_7fb2b902258e0b54_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| Function pointers

    __C declaration:__ @funptr_args1@

    __defined at:__ @macros\/reparse.h 126:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.FunPtr (IO ())
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args1 = hs_bindgen_7fb2b902258e0b54

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_args2@
foreign import ccall unsafe "hs_bindgen_106466b4fcfd40fd" hs_bindgen_106466b4fcfd40fd_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_args2@
hs_bindgen_106466b4fcfd40fd ::
     A
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_106466b4fcfd40fd =
  \x0 ->
    \x1 ->
      hs_bindgen_106466b4fcfd40fd_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @funptr_args2@

    __defined at:__ @macros\/reparse.h 127:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.FunPtr (IO BG.CInt)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args2 = hs_bindgen_106466b4fcfd40fd

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_args3@
foreign import ccall unsafe "hs_bindgen_77722e234ed79be7" hs_bindgen_77722e234ed79be7_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_args3@
hs_bindgen_77722e234ed79be7 ::
     A
  -> BG.FunPtr (BG.CInt -> IO ())
  -> IO ()
hs_bindgen_77722e234ed79be7 =
  \x0 ->
    \x1 ->
      hs_bindgen_77722e234ed79be7_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @funptr_args3@

    __defined at:__ @macros\/reparse.h 128:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.FunPtr (BG.CInt -> IO ())
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args3 = hs_bindgen_77722e234ed79be7

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_args4@
foreign import ccall unsafe "hs_bindgen_304e180456b9aa07" hs_bindgen_304e180456b9aa07_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_args4@
hs_bindgen_304e180456b9aa07 ::
     A
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar)
  -> IO ()
hs_bindgen_304e180456b9aa07 =
  \x0 ->
    \x1 ->
      hs_bindgen_304e180456b9aa07_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @funptr_args4@

    __defined at:__ @macros\/reparse.h 129:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args4 = hs_bindgen_304e180456b9aa07

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_args5@
foreign import ccall unsafe "hs_bindgen_0d545f6023860270" hs_bindgen_0d545f6023860270_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_args5@
hs_bindgen_0d545f6023860270 ::
     A
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))
  -> IO ()
hs_bindgen_0d545f6023860270 =
  \x0 ->
    \x1 ->
      hs_bindgen_0d545f6023860270_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @funptr_args5@

    __defined at:__ @macros\/reparse.h 130:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args5 = hs_bindgen_0d545f6023860270

-- __unique:__ @test_macrosreparse Example Unsafe_comments1@
foreign import ccall unsafe "hs_bindgen_f00d052491ecc40c" hs_bindgen_f00d052491ecc40c_base ::
     BG.CInt
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_comments1@
hs_bindgen_f00d052491ecc40c ::
     A
  -> IO ()
hs_bindgen_f00d052491ecc40c =
  \x0 ->
    hs_bindgen_f00d052491ecc40c_base (BG.toFFIType x0)

{-| Comments in awkward places

    (Prior to language-c we failed to parse there.)

    __C declaration:__ @comments1@

    __defined at:__ @macros\/reparse.h 144:25@

    __exported by:__ @macros\/reparse.h@
-}
comments1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO ()
comments1 = hs_bindgen_f00d052491ecc40c

-- __unique:__ @test_macrosreparse Example Unsafe_const_prim_before1@
foreign import ccall unsafe "hs_bindgen_8afc86e6d0b1fc02" hs_bindgen_8afc86e6d0b1fc02_base ::
     BG.CInt
  -> BG.CChar
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_prim_before1@
hs_bindgen_8afc86e6d0b1fc02 ::
     A
  -> BG.CChar
  -> IO ()
hs_bindgen_8afc86e6d0b1fc02 =
  \x0 ->
    \x1 ->
      hs_bindgen_8afc86e6d0b1fc02_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| @const@ qualifier

    NOTE: These were not parsed correctly prior to the switch to language-c.

    __C declaration:__ @const_prim_before1@

    __defined at:__ @macros\/reparse.h 177:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_before1 = hs_bindgen_8afc86e6d0b1fc02

-- __unique:__ @test_macrosreparse Example Unsafe_const_prim_before2@
foreign import ccall unsafe "hs_bindgen_372464519af88007" hs_bindgen_372464519af88007_base ::
     BG.CInt
  -> BG.CSChar
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_prim_before2@
hs_bindgen_372464519af88007 ::
     A
  -> BG.CSChar
  -> IO ()
hs_bindgen_372464519af88007 =
  \x0 ->
    \x1 ->
      hs_bindgen_372464519af88007_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @macros\/reparse.h 178:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CSChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_before2 = hs_bindgen_372464519af88007

-- __unique:__ @test_macrosreparse Example Unsafe_const_prim_before3@
foreign import ccall unsafe "hs_bindgen_4468eb2b7b86cc70" hs_bindgen_4468eb2b7b86cc70_base ::
     BG.CInt
  -> BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_prim_before3@
hs_bindgen_4468eb2b7b86cc70 ::
     A
  -> BG.CUChar
  -> IO ()
hs_bindgen_4468eb2b7b86cc70 =
  \x0 ->
    \x1 ->
      hs_bindgen_4468eb2b7b86cc70_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @macros\/reparse.h 179:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CUChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_before3 = hs_bindgen_4468eb2b7b86cc70

-- __unique:__ @test_macrosreparse Example Unsafe_const_prim_after1@
foreign import ccall unsafe "hs_bindgen_a4066167e4eac3fd" hs_bindgen_a4066167e4eac3fd_base ::
     BG.CInt
  -> BG.CChar
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_prim_after1@
hs_bindgen_a4066167e4eac3fd ::
     A
  -> BG.CChar
  -> IO ()
hs_bindgen_a4066167e4eac3fd =
  \x0 ->
    \x1 ->
      hs_bindgen_a4066167e4eac3fd_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @macros\/reparse.h 180:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_after1 = hs_bindgen_a4066167e4eac3fd

-- __unique:__ @test_macrosreparse Example Unsafe_const_prim_after2@
foreign import ccall unsafe "hs_bindgen_b67a87e388874d70" hs_bindgen_b67a87e388874d70_base ::
     BG.CInt
  -> BG.CSChar
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_prim_after2@
hs_bindgen_b67a87e388874d70 ::
     A
  -> BG.CSChar
  -> IO ()
hs_bindgen_b67a87e388874d70 =
  \x0 ->
    \x1 ->
      hs_bindgen_b67a87e388874d70_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @macros\/reparse.h 181:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CSChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_after2 = hs_bindgen_b67a87e388874d70

-- __unique:__ @test_macrosreparse Example Unsafe_const_prim_after3@
foreign import ccall unsafe "hs_bindgen_05c4dd640c52aa53" hs_bindgen_05c4dd640c52aa53_base ::
     BG.CInt
  -> BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_prim_after3@
hs_bindgen_05c4dd640c52aa53 ::
     A
  -> BG.CUChar
  -> IO ()
hs_bindgen_05c4dd640c52aa53 =
  \x0 ->
    \x1 ->
      hs_bindgen_05c4dd640c52aa53_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @macros\/reparse.h 182:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CUChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_after3 = hs_bindgen_05c4dd640c52aa53

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_before1@
foreign import ccall unsafe "hs_bindgen_ad9b37edd62f7fc0" hs_bindgen_ad9b37edd62f7fc0_base ::
     BG.CInt
  -> BG.CFloat
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_before1@
hs_bindgen_ad9b37edd62f7fc0 ::
     A
  -> BG.CFloat
  -> IO ()
hs_bindgen_ad9b37edd62f7fc0 =
  \x0 ->
    \x1 ->
      hs_bindgen_ad9b37edd62f7fc0_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @macros\/reparse.h 186:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before1 =
  hs_bindgen_ad9b37edd62f7fc0

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_before2@
foreign import ccall unsafe "hs_bindgen_d5772c034da42704" hs_bindgen_d5772c034da42704_base ::
     BG.CInt
  -> BG.CDouble
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_before2@
hs_bindgen_d5772c034da42704 ::
     A
  -> BG.CDouble
  -> IO ()
hs_bindgen_d5772c034da42704 =
  \x0 ->
    \x1 ->
      hs_bindgen_d5772c034da42704_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @macros\/reparse.h 187:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before2 =
  hs_bindgen_d5772c034da42704

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_before3@
foreign import ccall unsafe "hs_bindgen_e9e661d8d7d5674b" hs_bindgen_e9e661d8d7d5674b_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_before3@
hs_bindgen_e9e661d8d7d5674b ::
     A
  -> BG.CBool
  -> IO ()
hs_bindgen_e9e661d8d7d5674b =
  \x0 ->
    \x1 ->
      hs_bindgen_e9e661d8d7d5674b_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @macros\/reparse.h 188:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before3 =
  hs_bindgen_e9e661d8d7d5674b

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_before4@
foreign import ccall unsafe "hs_bindgen_18a86617539b0cbb" hs_bindgen_18a86617539b0cbb_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_before4@
hs_bindgen_18a86617539b0cbb ::
     A
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_18a86617539b0cbb =
  \x0 ->
    \x1 ->
      hs_bindgen_18a86617539b0cbb_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @macros\/reparse.h 189:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_struct
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before4 =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_18a86617539b0cbb arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_before5@
foreign import ccall unsafe "hs_bindgen_3bef29627e6a7c17" hs_bindgen_3bef29627e6a7c17_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_before5@
hs_bindgen_3bef29627e6a7c17 ::
     A
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_3bef29627e6a7c17 =
  \x0 ->
    \x1 ->
      hs_bindgen_3bef29627e6a7c17_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @macros\/reparse.h 190:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_union
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before5 =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_3bef29627e6a7c17 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_before6@
foreign import ccall unsafe "hs_bindgen_60056ee7f5cdcfcb" hs_bindgen_60056ee7f5cdcfcb_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_before6@
hs_bindgen_60056ee7f5cdcfcb ::
     A
  -> Some_enum
  -> IO ()
hs_bindgen_60056ee7f5cdcfcb =
  \x0 ->
    \x1 ->
      hs_bindgen_60056ee7f5cdcfcb_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @macros\/reparse.h 191:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before6 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_enum
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before6 =
  hs_bindgen_60056ee7f5cdcfcb

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_before7@
foreign import ccall unsafe "hs_bindgen_d911d05eedf544f0" hs_bindgen_d911d05eedf544f0_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_before7@
hs_bindgen_d911d05eedf544f0 ::
     A
  -> BG.CBool
  -> IO ()
hs_bindgen_d911d05eedf544f0 =
  \x0 ->
    \x1 ->
      hs_bindgen_d911d05eedf544f0_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @macros\/reparse.h 192:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before7 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before7 =
  hs_bindgen_d911d05eedf544f0

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_before8@
foreign import ccall unsafe "hs_bindgen_e1f447f72f4b7d48" hs_bindgen_e1f447f72f4b7d48_base ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_before8@
hs_bindgen_e1f447f72f4b7d48 ::
     A
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_e1f447f72f4b7d48 =
  \x0 ->
    \x1 ->
      hs_bindgen_e1f447f72f4b7d48_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @macros\/reparse.h 193:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before8 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.LibC.CSize
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before8 =
  hs_bindgen_e1f447f72f4b7d48

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_after1@
foreign import ccall unsafe "hs_bindgen_868032666da9209c" hs_bindgen_868032666da9209c_base ::
     BG.CInt
  -> BG.CFloat
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_after1@
hs_bindgen_868032666da9209c ::
     A
  -> BG.CFloat
  -> IO ()
hs_bindgen_868032666da9209c =
  \x0 ->
    \x1 ->
      hs_bindgen_868032666da9209c_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @macros\/reparse.h 195:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after1 =
  hs_bindgen_868032666da9209c

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_after2@
foreign import ccall unsafe "hs_bindgen_a23b22dbfdd59e29" hs_bindgen_a23b22dbfdd59e29_base ::
     BG.CInt
  -> BG.CDouble
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_after2@
hs_bindgen_a23b22dbfdd59e29 ::
     A
  -> BG.CDouble
  -> IO ()
hs_bindgen_a23b22dbfdd59e29 =
  \x0 ->
    \x1 ->
      hs_bindgen_a23b22dbfdd59e29_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @macros\/reparse.h 196:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after2 =
  hs_bindgen_a23b22dbfdd59e29

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_after3@
foreign import ccall unsafe "hs_bindgen_a5a6be2116e77b14" hs_bindgen_a5a6be2116e77b14_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_after3@
hs_bindgen_a5a6be2116e77b14 ::
     A
  -> BG.CBool
  -> IO ()
hs_bindgen_a5a6be2116e77b14 =
  \x0 ->
    \x1 ->
      hs_bindgen_a5a6be2116e77b14_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @macros\/reparse.h 197:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after3 =
  hs_bindgen_a5a6be2116e77b14

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_after4@
foreign import ccall unsafe "hs_bindgen_afa897f76b1ad851" hs_bindgen_afa897f76b1ad851_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_after4@
hs_bindgen_afa897f76b1ad851 ::
     A
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_afa897f76b1ad851 =
  \x0 ->
    \x1 ->
      hs_bindgen_afa897f76b1ad851_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @macros\/reparse.h 198:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_struct
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after4 =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_afa897f76b1ad851 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_after5@
foreign import ccall unsafe "hs_bindgen_cee24fcdc4d5d759" hs_bindgen_cee24fcdc4d5d759_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_after5@
hs_bindgen_cee24fcdc4d5d759 ::
     A
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_cee24fcdc4d5d759 =
  \x0 ->
    \x1 ->
      hs_bindgen_cee24fcdc4d5d759_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @macros\/reparse.h 199:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_union
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after5 =
  \arg10 ->
    \arg21 ->
      BG.with arg21 (\arg22 ->
                       hs_bindgen_cee24fcdc4d5d759 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_after6@
foreign import ccall unsafe "hs_bindgen_0f72739d4e75813a" hs_bindgen_0f72739d4e75813a_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_after6@
hs_bindgen_0f72739d4e75813a ::
     A
  -> Some_enum
  -> IO ()
hs_bindgen_0f72739d4e75813a =
  \x0 ->
    \x1 ->
      hs_bindgen_0f72739d4e75813a_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @macros\/reparse.h 200:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after6 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_enum
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after6 =
  hs_bindgen_0f72739d4e75813a

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_after7@
foreign import ccall unsafe "hs_bindgen_cb8d13328f97418c" hs_bindgen_cb8d13328f97418c_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_after7@
hs_bindgen_cb8d13328f97418c ::
     A
  -> BG.CBool
  -> IO ()
hs_bindgen_cb8d13328f97418c =
  \x0 ->
    \x1 ->
      hs_bindgen_cb8d13328f97418c_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @macros\/reparse.h 201:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after7 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after7 =
  hs_bindgen_cb8d13328f97418c

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_after8@
foreign import ccall unsafe "hs_bindgen_cc817cc1ec0b5db8" hs_bindgen_cc817cc1ec0b5db8_base ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_withoutSign_after8@
hs_bindgen_cc817cc1ec0b5db8 ::
     A
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_cc817cc1ec0b5db8 =
  \x0 ->
    \x1 ->
      hs_bindgen_cc817cc1ec0b5db8_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @macros\/reparse.h 202:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after8 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.LibC.CSize
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after8 =
  hs_bindgen_cc817cc1ec0b5db8

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_args1@
foreign import ccall unsafe "hs_bindgen_0297c1c8701428e2" hs_bindgen_0297c1c8701428e2_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_args1@
hs_bindgen_0297c1c8701428e2 ::
     A
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_0297c1c8701428e2 =
  \x0 ->
    \x1 ->
      hs_bindgen_0297c1c8701428e2_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @macros\/reparse.h 206:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> PtrConst.PtrConst BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args1 = hs_bindgen_0297c1c8701428e2

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_args2@
foreign import ccall unsafe "hs_bindgen_a8a508f487c4ff4c" hs_bindgen_a8a508f487c4ff4c_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_args2@
hs_bindgen_a8a508f487c4ff4c ::
     A
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_a8a508f487c4ff4c =
  \x0 ->
    \x1 ->
      hs_bindgen_a8a508f487c4ff4c_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @macros\/reparse.h 207:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> PtrConst.PtrConst BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args2 = hs_bindgen_a8a508f487c4ff4c

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_args3@
foreign import ccall unsafe "hs_bindgen_4d7ff5fabe5f5355" hs_bindgen_4d7ff5fabe5f5355_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_args3@
hs_bindgen_4d7ff5fabe5f5355 ::
     A
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_4d7ff5fabe5f5355 =
  \x0 ->
    \x1 ->
      hs_bindgen_4d7ff5fabe5f5355_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @macros\/reparse.h 208:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.Ptr BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args3 = hs_bindgen_4d7ff5fabe5f5355

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_args4@
foreign import ccall unsafe "hs_bindgen_e4967b54bde0a75f" hs_bindgen_e4967b54bde0a75f_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_args4@
hs_bindgen_e4967b54bde0a75f ::
     A
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_e4967b54bde0a75f =
  \x0 ->
    \x1 ->
      hs_bindgen_e4967b54bde0a75f_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @macros\/reparse.h 209:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> PtrConst.PtrConst BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args4 = hs_bindgen_e4967b54bde0a75f

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_args5@
foreign import ccall unsafe "hs_bindgen_70df10cdcb3ad9ba" hs_bindgen_70df10cdcb3ad9ba_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_args5@
hs_bindgen_70df10cdcb3ad9ba ::
     A
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_70df10cdcb3ad9ba =
  \x0 ->
    \x1 ->
      hs_bindgen_70df10cdcb3ad9ba_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @macros\/reparse.h 210:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> PtrConst.PtrConst BG.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args5 = hs_bindgen_70df10cdcb3ad9ba

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_ret1@
foreign import ccall unsafe "hs_bindgen_a0550dc996ed14e4" hs_bindgen_a0550dc996ed14e4_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_ret1@
hs_bindgen_a0550dc996ed14e4 ::
     A
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_a0550dc996ed14e4 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_a0550dc996ed14e4_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h 212:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret1 = hs_bindgen_a0550dc996ed14e4

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_ret2@
foreign import ccall unsafe "hs_bindgen_bddd020480d5475f" hs_bindgen_bddd020480d5475f_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_ret2@
hs_bindgen_bddd020480d5475f ::
     A
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_bddd020480d5475f =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_bddd020480d5475f_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h 213:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret2 = hs_bindgen_bddd020480d5475f

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_ret3@
foreign import ccall unsafe "hs_bindgen_66ba83341a10488f" hs_bindgen_66ba83341a10488f_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_ret3@
hs_bindgen_66ba83341a10488f ::
     A
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_66ba83341a10488f =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_66ba83341a10488f_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h 214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
const_pointers_ret3 = hs_bindgen_66ba83341a10488f

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_ret4@
foreign import ccall unsafe "hs_bindgen_5365bdd9bb9b376b" hs_bindgen_5365bdd9bb9b376b_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_ret4@
hs_bindgen_5365bdd9bb9b376b ::
     A
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_5365bdd9bb9b376b =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_5365bdd9bb9b376b_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h 215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret4 = hs_bindgen_5365bdd9bb9b376b

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_ret5@
foreign import ccall unsafe "hs_bindgen_98b79f695f2953cb" hs_bindgen_98b79f695f2953cb_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse Example Unsafe_const_pointers_ret5@
hs_bindgen_98b79f695f2953cb ::
     A
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_98b79f695f2953cb =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_98b79f695f2953cb_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h 216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret5 = hs_bindgen_98b79f695f2953cb

-- __unique:__ @test_macrosreparse Example Unsafe_const_array_elem1@
foreign import ccall unsafe "hs_bindgen_6759e0a2cb860c2e" hs_bindgen_6759e0a2cb860c2e_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_array_elem1@
hs_bindgen_6759e0a2cb860c2e ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray A))
  -> IO ()
hs_bindgen_6759e0a2cb860c2e =
  \x0 ->
    hs_bindgen_6759e0a2cb860c2e_base (BG.toFFIType x0)

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h 244:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray A))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem1 = hs_bindgen_6759e0a2cb860c2e

-- __unique:__ @test_macrosreparse Example Unsafe_const_array_elem2@
foreign import ccall unsafe "hs_bindgen_6cfc311ce355ef8d" hs_bindgen_6cfc311ce355ef8d_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_array_elem2@
hs_bindgen_6cfc311ce355ef8d ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst A)))
  -> IO ()
hs_bindgen_6cfc311ce355ef8d =
  \x0 ->
    hs_bindgen_6cfc311ce355ef8d_base (BG.toFFIType x0)

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h 245:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem2 = hs_bindgen_6cfc311ce355ef8d

-- __unique:__ @test_macrosreparse Example Unsafe_const_array_elem3@
foreign import ccall unsafe "hs_bindgen_a82b183c857e54bf" hs_bindgen_a82b183c857e54bf_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_const_array_elem3@
hs_bindgen_a82b183c857e54bf ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr A)))
  -> IO ()
hs_bindgen_a82b183c857e54bf =
  \x0 ->
    hs_bindgen_a82b183c857e54bf_base (BG.toFFIType x0)

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h 246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem3 = hs_bindgen_a82b183c857e54bf

-- __unique:__ @test_macrosreparse Example Unsafe_noParams1@
foreign import ccall unsafe "hs_bindgen_546885c1cf5918e4" hs_bindgen_546885c1cf5918e4_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse Example Unsafe_noParams1@
hs_bindgen_546885c1cf5918e4 :: IO A
hs_bindgen_546885c1cf5918e4 =
  fmap BG.fromFFIType hs_bindgen_546885c1cf5918e4_base

{-| Other examples we reparsed /incorrectly/ before language-c

    __C declaration:__ @noParams1@

    __defined at:__ @macros\/reparse.h 254:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams1 :: IO A
noParams1 = hs_bindgen_546885c1cf5918e4

-- __unique:__ @test_macrosreparse Example Unsafe_noParams2@
foreign import ccall unsafe "hs_bindgen_448ffd4af386843c" hs_bindgen_448ffd4af386843c_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse Example Unsafe_noParams2@
hs_bindgen_448ffd4af386843c :: IO A
hs_bindgen_448ffd4af386843c =
  fmap BG.fromFFIType hs_bindgen_448ffd4af386843c_base

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h 255:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2 :: IO A
noParams2 = hs_bindgen_448ffd4af386843c

-- __unique:__ @test_macrosreparse Example Unsafe_noParams3@
foreign import ccall unsafe "hs_bindgen_d39a1f2aa35317eb" hs_bindgen_d39a1f2aa35317eb_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Unsafe_noParams3@
hs_bindgen_d39a1f2aa35317eb ::
     A
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_d39a1f2aa35317eb =
  \x0 ->
    \x1 ->
      hs_bindgen_d39a1f2aa35317eb_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @noParams3@

    __defined at:__ @macros\/reparse.h 256:6@

    __exported by:__ @macros\/reparse.h@
-}
noParams3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> BG.FunPtr (IO BG.CInt)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
noParams3 = hs_bindgen_d39a1f2aa35317eb

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret1@
foreign import ccall unsafe "hs_bindgen_e18fdd7d1b192d86" hs_bindgen_e18fdd7d1b192d86_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret1@
hs_bindgen_e18fdd7d1b192d86 ::
     A
  -> IO (BG.FunPtr (IO ()))
hs_bindgen_e18fdd7d1b192d86 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_e18fdd7d1b192d86_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h 260:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO ()))
funptr_ret1 = hs_bindgen_e18fdd7d1b192d86

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret2@
foreign import ccall unsafe "hs_bindgen_b76e1caa4d3ac3eb" hs_bindgen_b76e1caa4d3ac3eb_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret2@
hs_bindgen_b76e1caa4d3ac3eb ::
     A
  -> IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_b76e1caa4d3ac3eb =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_b76e1caa4d3ac3eb_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h 261:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO BG.CInt))
funptr_ret2 = hs_bindgen_b76e1caa4d3ac3eb

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret3@
foreign import ccall unsafe "hs_bindgen_9c27f176dd302129" hs_bindgen_9c27f176dd302129_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret3@
hs_bindgen_9c27f176dd302129 ::
     A
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_9c27f176dd302129 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_9c27f176dd302129_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h 262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
funptr_ret3 = hs_bindgen_9c27f176dd302129

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret4@
foreign import ccall unsafe "hs_bindgen_550b40d1cf52abe9" hs_bindgen_550b40d1cf52abe9_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret4@
hs_bindgen_550b40d1cf52abe9 ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
hs_bindgen_550b40d1cf52abe9 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_550b40d1cf52abe9_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h 263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
funptr_ret4 = hs_bindgen_550b40d1cf52abe9

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret5@
foreign import ccall unsafe "hs_bindgen_948a3667e352d2d4" hs_bindgen_948a3667e352d2d4_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret5@
hs_bindgen_948a3667e352d2d4 ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_948a3667e352d2d4 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_948a3667e352d2d4_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h 267:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret5 = hs_bindgen_948a3667e352d2d4

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret6@
foreign import ccall unsafe "hs_bindgen_a4848da8e7fc3ee4" hs_bindgen_a4848da8e7fc3ee4_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret6@
hs_bindgen_a4848da8e7fc3ee4 ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_a4848da8e7fc3ee4 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_a4848da8e7fc3ee4_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h 268:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret6 = hs_bindgen_a4848da8e7fc3ee4

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret7@
foreign import ccall unsafe "hs_bindgen_6b9e78d0d74a8524" hs_bindgen_6b9e78d0d74a8524_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret7@
hs_bindgen_6b9e78d0d74a8524 ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_6b9e78d0d74a8524 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_6b9e78d0d74a8524_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h 269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret7 = hs_bindgen_6b9e78d0d74a8524

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret8@
foreign import ccall unsafe "hs_bindgen_a907856761fe94ab" hs_bindgen_a907856761fe94ab_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret8@
hs_bindgen_a907856761fe94ab ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_a907856761fe94ab =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_a907856761fe94ab_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h 270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret8 = hs_bindgen_a907856761fe94ab

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret9@
foreign import ccall unsafe "hs_bindgen_e47275c0159a6da9" hs_bindgen_e47275c0159a6da9_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret9@
hs_bindgen_e47275c0159a6da9 ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_e47275c0159a6da9 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_e47275c0159a6da9_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h 271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret9 = hs_bindgen_e47275c0159a6da9

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret10@
foreign import ccall unsafe "hs_bindgen_fb11b16d0e3bd82a" hs_bindgen_fb11b16d0e3bd82a_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Unsafe_funptr_ret10@
hs_bindgen_fb11b16d0e3bd82a ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_fb11b16d0e3bd82a =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_fb11b16d0e3bd82a_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h 272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret10 = hs_bindgen_fb11b16d0e3bd82a
