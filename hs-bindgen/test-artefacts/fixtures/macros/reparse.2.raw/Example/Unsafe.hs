{-# LANGUAGE DataKinds #-}
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

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse.h>"
  , "void hs_bindgen_dbcf45ae88784a43 ("
  , "  signed int arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  (args_char1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c72087f2b9c0a851 ("
  , "  signed int arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  (args_char2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d23034ca037d7ee2 ("
  , "  signed int arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  (args_char3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_7aba582bac3cdd4c ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_72e80c0c19b73bd1 ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1d779b6db021542e ("
  , "  signed int arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  (args_short3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_dadb0390936fa7a6 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_cc4dd71da3d83f3e ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c1aa1895338c9c06 ("
  , "  signed int arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  (args_int3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ea3ab52d35940c9c ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1ead52aa138d51cf ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_93b713dccaf06fed ("
  , "  signed int arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  (args_long3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d091d2dc36843d19 ("
  , "  signed int arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  (args_float)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6ea5acb5ec6f40c4 ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  (args_double)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_689ef6ac4e916b3a ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (args_bool1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3d21d6b68f84eba6 ("
  , "  signed int arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  (args_struct)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_6c2746bae9e4dab9 ("
  , "  signed int arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  (args_union)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_6c549405e9baffcf ("
  , "  signed int arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  (args_enum)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_34dfa5eaf97951ec ("
  , "  signed int arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  (args_pointer1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0d71e7152a20efbd ("
  , "  signed int arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  (args_pointer2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1ac8f30e6858a673 ("
  , "  signed int arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  (args_pointer3)(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_74c085d1fc547d65 (void)"
  , "{"
  , "  return (ret_A)();"
  , "}"
  , "char hs_bindgen_c1173d352155de62 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char1)(arg1);"
  , "}"
  , "signed char hs_bindgen_9029d84b81d6df97 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char2)(arg1);"
  , "}"
  , "unsigned char hs_bindgen_5076ef968e0b1da6 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char3)(arg1);"
  , "}"
  , "signed short hs_bindgen_6c9415155d57e014 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short1)(arg1);"
  , "}"
  , "signed short hs_bindgen_24f9af60f9d4b0c4 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short2)(arg1);"
  , "}"
  , "unsigned short hs_bindgen_7712cc06577c000f ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short3)(arg1);"
  , "}"
  , "signed int hs_bindgen_a4669df8abb549dc ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int1)(arg1);"
  , "}"
  , "signed int hs_bindgen_d471d89e2802f599 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int2)(arg1);"
  , "}"
  , "unsigned int hs_bindgen_551866464fb7e076 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int3)(arg1);"
  , "}"
  , "signed long hs_bindgen_1cb0ba330b8fc708 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long1)(arg1);"
  , "}"
  , "signed long hs_bindgen_84d65e5f2bb20468 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long2)(arg1);"
  , "}"
  , "unsigned long hs_bindgen_8e5c7c7b2f385152 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long3)(arg1);"
  , "}"
  , "float hs_bindgen_be7b5328fb1e67e4 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_float)(arg1);"
  , "}"
  , "double hs_bindgen_67ccecb1f4c2b26b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_double)(arg1);"
  , "}"
  , "_Bool hs_bindgen_8f3058d29b19c7f8 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_bool1)(arg1);"
  , "}"
  , "void hs_bindgen_db41d471db80076d ("
  , "  signed int arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_struct)(arg1);"
  , "}"
  , "void hs_bindgen_a2761d3935650b5c ("
  , "  signed int arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_union)(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_c567484984a48bdd ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_enum)(arg1);"
  , "}"
  , "signed int *hs_bindgen_a0436717a3858ec2 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer1)(arg1);"
  , "}"
  , "signed int **hs_bindgen_92e30e7cf97b7ae8 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer2)(arg1);"
  , "}"
  , "void *hs_bindgen_cec80a82e028945d ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer3)(arg1);"
  , "}"
  , "signed int hs_bindgen_373d2dbedb7f3ea1 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (body1)(arg1);"
  , "}"
  , "signed int hs_bindgen_678c705d71b66fd0 (void)"
  , "{"
  , "  return (body2)();"
  , "}"
  , "void hs_bindgen_4a4cdf60b6a4885d ("
  , "  signed int arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_float)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_4b113ac08bbb35bd ("
  , "  signed int arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_double)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_63483820a803fad0 ("
  , "  signed int arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_float)(arg1);"
  , "}"
  , "void hs_bindgen_00dc8901b39e6dd3 ("
  , "  signed int arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_double)(arg1);"
  , "}"
  , "void hs_bindgen_c4123c64e3799da5 ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (bespoke_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_56568d64374ea82a ("
  , "  signed int arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  (bespoke_args2)(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_e39afd1aad07e0fc ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret1)(arg1);"
  , "}"
  , "size_t hs_bindgen_06a7ca6b10b6f1c5 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret2)(arg1);"
  , "}"
  , "void hs_bindgen_085cd2205e0daeff ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (arr_args1)(arg1);"
  , "}"
  , "void hs_bindgen_0df3319af542c072 ("
  , "  signed int **arg1"
  , ")"
  , "{"
  , "  (arr_args2)(arg1);"
  , "}"
  , "void hs_bindgen_4839d1f4a2742c03 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (arr_args3)(arg1);"
  , "}"
  , "void hs_bindgen_c7e68dd8759c969a ("
  , "  signed int **arg1"
  , ")"
  , "{"
  , "  (arr_args4)(arg1);"
  , "}"
  , "void hs_bindgen_bf24f137d6217c14 ("
  , "  signed int arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2b6cd604640b023f ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9c35d7346db373f0 ("
  , "  signed int arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_60245cd22b3cd59b ("
  , "  signed int arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_bcd5e0817da0463b ("
  , "  signed int arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args5)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_fd3d542dfefd036e ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (comments1)(arg1);"
  , "}"
  , "void hs_bindgen_0819c4409f0cb04f ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d31ae17d88140355 ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f0ed5f637cefe96f ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0215a515d1fa3dfa ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e103c029e3630b33 ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_bef8a67258cb736b ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_589e465944877183 ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_fef23ca0f7241d44 ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_12ed6589e368a6d9 ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_acfc6cbb94266577 ("
  , "  signed int arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_e452087578f98911 ("
  , "  signed int arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_a943f6ee50043676 ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_425f71a0e411bca9 ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_297d1f2aa423722a ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_136c1c8ee6a77c65 ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_876526a42ba48417 ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b309fa8d82dd7b72 ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b7b3fe289c724859 ("
  , "  signed int arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_55b1796019967748 ("
  , "  signed int arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_b10d1e8f7c40c632 ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_04406c6b24b54172 ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ae945fd854416c29 ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_315fb7a4e52f51bb ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c8fe0d37027b1cbd ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_585e1489b3f5af58 ("
  , "  signed int arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b9568a159c32cefc ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_85f146afc2a27268 ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args5)(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_ea6a7e1d0274f90d ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret1)(arg1);"
  , "}"
  , "signed int const *hs_bindgen_ae320a2e858ccc98 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret2)(arg1);"
  , "}"
  , "signed int *const hs_bindgen_3ca6c6656d62db21 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret3)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_83d0056641627b70 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret4)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_89f54f00e5f1ec61 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret5)(arg1);"
  , "}"
  , "void hs_bindgen_445aea27fa0b55be ("
  , "  signed int const *arg1"
  , ")"
  , "{"
  , "  (const_array_elem1)(arg1);"
  , "}"
  , "void hs_bindgen_2f9fd49dce178c29 ("
  , "  signed int const **arg1"
  , ")"
  , "{"
  , "  (const_array_elem2)(arg1);"
  , "}"
  , "void hs_bindgen_99716bef732a1af1 ("
  , "  signed int *const *arg1"
  , ")"
  , "{"
  , "  (const_array_elem3)(arg1);"
  , "}"
  , "signed int hs_bindgen_1721fcf9bca3ea61 (void)"
  , "{"
  , "  return (noParams1)();"
  , "}"
  , "signed int hs_bindgen_95fe913b1eec4f4e (void)"
  , "{"
  , "  return (noParams2)();"
  , "}"
  , "void hs_bindgen_d15b46c7c30f29ac ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (noParams3)(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_fe09e75d56f9869e ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_7f6cbcd68ce4b354 ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret2)(arg1);"
  , "}"
  , "void (*hs_bindgen_1bcf333c8e82d009 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (funptr_ret3)(arg1);"
  , "}"
  , "char (*hs_bindgen_4793bc049c8510a4 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret4)(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_abb2d85efd4fcb3e ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret5)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_6cf3159cf7618393 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret6)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_ae99f146e73b6c1a ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret7)(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_5ac0a15c42b191c7 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret8)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_0dbef4dde3b3d205 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret9)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_c226f0b9bbf08c49 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret10)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_char1@
foreign import ccall unsafe "hs_bindgen_dbcf45ae88784a43" hs_bindgen_dbcf45ae88784a43_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_char1@
hs_bindgen_dbcf45ae88784a43 ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_dbcf45ae88784a43 =
  BG.fromFFIType hs_bindgen_dbcf45ae88784a43_base

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
args_char1 = hs_bindgen_dbcf45ae88784a43

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_char2@
foreign import ccall unsafe "hs_bindgen_c72087f2b9c0a851" hs_bindgen_c72087f2b9c0a851_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_char2@
hs_bindgen_c72087f2b9c0a851 ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_c72087f2b9c0a851 =
  BG.fromFFIType hs_bindgen_c72087f2b9c0a851_base

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
args_char2 = hs_bindgen_c72087f2b9c0a851

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_char3@
foreign import ccall unsafe "hs_bindgen_d23034ca037d7ee2" hs_bindgen_d23034ca037d7ee2_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_char3@
hs_bindgen_d23034ca037d7ee2 ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_d23034ca037d7ee2 =
  BG.fromFFIType hs_bindgen_d23034ca037d7ee2_base

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
args_char3 = hs_bindgen_d23034ca037d7ee2

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_short1@
foreign import ccall unsafe "hs_bindgen_7aba582bac3cdd4c" hs_bindgen_7aba582bac3cdd4c_base ::
     BG.Int32
  -> BG.Int16
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_short1@
hs_bindgen_7aba582bac3cdd4c ::
     BG.CInt
  -> BG.CShort
  -> IO ()
hs_bindgen_7aba582bac3cdd4c =
  BG.fromFFIType hs_bindgen_7aba582bac3cdd4c_base

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
args_short1 = hs_bindgen_7aba582bac3cdd4c

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_short2@
foreign import ccall unsafe "hs_bindgen_72e80c0c19b73bd1" hs_bindgen_72e80c0c19b73bd1_base ::
     BG.Int32
  -> BG.Int16
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_short2@
hs_bindgen_72e80c0c19b73bd1 ::
     BG.CInt
  -> BG.CShort
  -> IO ()
hs_bindgen_72e80c0c19b73bd1 =
  BG.fromFFIType hs_bindgen_72e80c0c19b73bd1_base

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
args_short2 = hs_bindgen_72e80c0c19b73bd1

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_short3@
foreign import ccall unsafe "hs_bindgen_1d779b6db021542e" hs_bindgen_1d779b6db021542e_base ::
     BG.Int32
  -> BG.Word16
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_short3@
hs_bindgen_1d779b6db021542e ::
     BG.CInt
  -> BG.CUShort
  -> IO ()
hs_bindgen_1d779b6db021542e =
  BG.fromFFIType hs_bindgen_1d779b6db021542e_base

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
args_short3 = hs_bindgen_1d779b6db021542e

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_int1@
foreign import ccall unsafe "hs_bindgen_dadb0390936fa7a6" hs_bindgen_dadb0390936fa7a6_base ::
     BG.Int32
  -> BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_int1@
hs_bindgen_dadb0390936fa7a6 ::
     BG.CInt
  -> BG.CInt
  -> IO ()
hs_bindgen_dadb0390936fa7a6 =
  BG.fromFFIType hs_bindgen_dadb0390936fa7a6_base

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
args_int1 = hs_bindgen_dadb0390936fa7a6

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_int2@
foreign import ccall unsafe "hs_bindgen_cc4dd71da3d83f3e" hs_bindgen_cc4dd71da3d83f3e_base ::
     BG.Int32
  -> BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_int2@
hs_bindgen_cc4dd71da3d83f3e ::
     BG.CInt
  -> BG.CInt
  -> IO ()
hs_bindgen_cc4dd71da3d83f3e =
  BG.fromFFIType hs_bindgen_cc4dd71da3d83f3e_base

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
args_int2 = hs_bindgen_cc4dd71da3d83f3e

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_int3@
foreign import ccall unsafe "hs_bindgen_c1aa1895338c9c06" hs_bindgen_c1aa1895338c9c06_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_int3@
hs_bindgen_c1aa1895338c9c06 ::
     BG.CInt
  -> BG.CUInt
  -> IO ()
hs_bindgen_c1aa1895338c9c06 =
  BG.fromFFIType hs_bindgen_c1aa1895338c9c06_base

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
args_int3 = hs_bindgen_c1aa1895338c9c06

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_long1@
foreign import ccall unsafe "hs_bindgen_ea3ab52d35940c9c" hs_bindgen_ea3ab52d35940c9c_base ::
     BG.Int32
  -> BG.Int64
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_long1@
hs_bindgen_ea3ab52d35940c9c ::
     BG.CInt
  -> BG.CLong
  -> IO ()
hs_bindgen_ea3ab52d35940c9c =
  BG.fromFFIType hs_bindgen_ea3ab52d35940c9c_base

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
args_long1 = hs_bindgen_ea3ab52d35940c9c

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_long2@
foreign import ccall unsafe "hs_bindgen_1ead52aa138d51cf" hs_bindgen_1ead52aa138d51cf_base ::
     BG.Int32
  -> BG.Int64
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_long2@
hs_bindgen_1ead52aa138d51cf ::
     BG.CInt
  -> BG.CLong
  -> IO ()
hs_bindgen_1ead52aa138d51cf =
  BG.fromFFIType hs_bindgen_1ead52aa138d51cf_base

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
args_long2 = hs_bindgen_1ead52aa138d51cf

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_long3@
foreign import ccall unsafe "hs_bindgen_93b713dccaf06fed" hs_bindgen_93b713dccaf06fed_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_long3@
hs_bindgen_93b713dccaf06fed ::
     BG.CInt
  -> BG.CULong
  -> IO ()
hs_bindgen_93b713dccaf06fed =
  BG.fromFFIType hs_bindgen_93b713dccaf06fed_base

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
args_long3 = hs_bindgen_93b713dccaf06fed

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_float@
foreign import ccall unsafe "hs_bindgen_d091d2dc36843d19" hs_bindgen_d091d2dc36843d19_base ::
     BG.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_float@
hs_bindgen_d091d2dc36843d19 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_d091d2dc36843d19 =
  BG.fromFFIType hs_bindgen_d091d2dc36843d19_base

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
args_float = hs_bindgen_d091d2dc36843d19

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_double@
foreign import ccall unsafe "hs_bindgen_6ea5acb5ec6f40c4" hs_bindgen_6ea5acb5ec6f40c4_base ::
     BG.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_double@
hs_bindgen_6ea5acb5ec6f40c4 ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_6ea5acb5ec6f40c4 =
  BG.fromFFIType hs_bindgen_6ea5acb5ec6f40c4_base

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
args_double = hs_bindgen_6ea5acb5ec6f40c4

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_bool1@
foreign import ccall unsafe "hs_bindgen_689ef6ac4e916b3a" hs_bindgen_689ef6ac4e916b3a_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_bool1@
hs_bindgen_689ef6ac4e916b3a ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_689ef6ac4e916b3a =
  BG.fromFFIType hs_bindgen_689ef6ac4e916b3a_base

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
args_bool1 = hs_bindgen_689ef6ac4e916b3a

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_struct@
foreign import ccall unsafe "hs_bindgen_3d21d6b68f84eba6" hs_bindgen_3d21d6b68f84eba6_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_struct@
hs_bindgen_3d21d6b68f84eba6 ::
     BG.CInt
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_3d21d6b68f84eba6 =
  BG.fromFFIType hs_bindgen_3d21d6b68f84eba6_base

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
                       hs_bindgen_3d21d6b68f84eba6 arg10 arg22)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_union@
foreign import ccall unsafe "hs_bindgen_6c2746bae9e4dab9" hs_bindgen_6c2746bae9e4dab9_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_union@
hs_bindgen_6c2746bae9e4dab9 ::
     BG.CInt
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_6c2746bae9e4dab9 =
  BG.fromFFIType hs_bindgen_6c2746bae9e4dab9_base

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
                       hs_bindgen_6c2746bae9e4dab9 arg10 arg22)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_enum@
foreign import ccall unsafe "hs_bindgen_6c549405e9baffcf" hs_bindgen_6c549405e9baffcf_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_enum@
hs_bindgen_6c549405e9baffcf ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_6c549405e9baffcf =
  BG.fromFFIType hs_bindgen_6c549405e9baffcf_base

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
args_enum = hs_bindgen_6c549405e9baffcf

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_pointer1@
foreign import ccall unsafe "hs_bindgen_34dfa5eaf97951ec" hs_bindgen_34dfa5eaf97951ec_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_pointer1@
hs_bindgen_34dfa5eaf97951ec ::
     BG.CInt
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_34dfa5eaf97951ec =
  BG.fromFFIType hs_bindgen_34dfa5eaf97951ec_base

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
args_pointer1 = hs_bindgen_34dfa5eaf97951ec

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_pointer2@
foreign import ccall unsafe "hs_bindgen_0d71e7152a20efbd" hs_bindgen_0d71e7152a20efbd_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_pointer2@
hs_bindgen_0d71e7152a20efbd ::
     BG.CInt
  -> BG.Ptr (BG.Ptr BG.CInt)
  -> IO ()
hs_bindgen_0d71e7152a20efbd =
  BG.fromFFIType hs_bindgen_0d71e7152a20efbd_base

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
args_pointer2 = hs_bindgen_0d71e7152a20efbd

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_pointer3@
foreign import ccall unsafe "hs_bindgen_1ac8f30e6858a673" hs_bindgen_1ac8f30e6858a673_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_pointer3@
hs_bindgen_1ac8f30e6858a673 ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()
hs_bindgen_1ac8f30e6858a673 =
  BG.fromFFIType hs_bindgen_1ac8f30e6858a673_base

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
args_pointer3 = hs_bindgen_1ac8f30e6858a673

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_A@
foreign import ccall unsafe "hs_bindgen_74c085d1fc547d65" hs_bindgen_74c085d1fc547d65_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_A@
hs_bindgen_74c085d1fc547d65 :: IO BG.CInt
hs_bindgen_74c085d1fc547d65 =
  BG.fromFFIType hs_bindgen_74c085d1fc547d65_base

{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h 47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A :: IO BG.CInt
ret_A = hs_bindgen_74c085d1fc547d65

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_char1@
foreign import ccall unsafe "hs_bindgen_c1173d352155de62" hs_bindgen_c1173d352155de62_base ::
     BG.Int32
  -> IO BG.Int8

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_char1@
hs_bindgen_c1173d352155de62 ::
     BG.CInt
  -> IO BG.CChar
hs_bindgen_c1173d352155de62 =
  BG.fromFFIType hs_bindgen_c1173d352155de62_base

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h 49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CChar
ret_char1 = hs_bindgen_c1173d352155de62

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_char2@
foreign import ccall unsafe "hs_bindgen_9029d84b81d6df97" hs_bindgen_9029d84b81d6df97_base ::
     BG.Int32
  -> IO BG.Int8

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_char2@
hs_bindgen_9029d84b81d6df97 ::
     BG.CInt
  -> IO BG.CSChar
hs_bindgen_9029d84b81d6df97 =
  BG.fromFFIType hs_bindgen_9029d84b81d6df97_base

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h 50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CSChar
ret_char2 = hs_bindgen_9029d84b81d6df97

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_char3@
foreign import ccall unsafe "hs_bindgen_5076ef968e0b1da6" hs_bindgen_5076ef968e0b1da6_base ::
     BG.Int32
  -> IO BG.Word8

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_char3@
hs_bindgen_5076ef968e0b1da6 ::
     BG.CInt
  -> IO BG.CUChar
hs_bindgen_5076ef968e0b1da6 =
  BG.fromFFIType hs_bindgen_5076ef968e0b1da6_base

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h 51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUChar
ret_char3 = hs_bindgen_5076ef968e0b1da6

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_short1@
foreign import ccall unsafe "hs_bindgen_6c9415155d57e014" hs_bindgen_6c9415155d57e014_base ::
     BG.Int32
  -> IO BG.Int16

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_short1@
hs_bindgen_6c9415155d57e014 ::
     BG.CInt
  -> IO BG.CShort
hs_bindgen_6c9415155d57e014 =
  BG.fromFFIType hs_bindgen_6c9415155d57e014_base

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h 53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short1 = hs_bindgen_6c9415155d57e014

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_short2@
foreign import ccall unsafe "hs_bindgen_24f9af60f9d4b0c4" hs_bindgen_24f9af60f9d4b0c4_base ::
     BG.Int32
  -> IO BG.Int16

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_short2@
hs_bindgen_24f9af60f9d4b0c4 ::
     BG.CInt
  -> IO BG.CShort
hs_bindgen_24f9af60f9d4b0c4 =
  BG.fromFFIType hs_bindgen_24f9af60f9d4b0c4_base

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h 54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short2 = hs_bindgen_24f9af60f9d4b0c4

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_short3@
foreign import ccall unsafe "hs_bindgen_7712cc06577c000f" hs_bindgen_7712cc06577c000f_base ::
     BG.Int32
  -> IO BG.Word16

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_short3@
hs_bindgen_7712cc06577c000f ::
     BG.CInt
  -> IO BG.CUShort
hs_bindgen_7712cc06577c000f =
  BG.fromFFIType hs_bindgen_7712cc06577c000f_base

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h 55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUShort
ret_short3 = hs_bindgen_7712cc06577c000f

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_int1@
foreign import ccall unsafe "hs_bindgen_a4669df8abb549dc" hs_bindgen_a4669df8abb549dc_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_int1@
hs_bindgen_a4669df8abb549dc ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_a4669df8abb549dc =
  BG.fromFFIType hs_bindgen_a4669df8abb549dc_base

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h 57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int1 = hs_bindgen_a4669df8abb549dc

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_int2@
foreign import ccall unsafe "hs_bindgen_d471d89e2802f599" hs_bindgen_d471d89e2802f599_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_int2@
hs_bindgen_d471d89e2802f599 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_d471d89e2802f599 =
  BG.fromFFIType hs_bindgen_d471d89e2802f599_base

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h 58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int2 = hs_bindgen_d471d89e2802f599

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_int3@
foreign import ccall unsafe "hs_bindgen_551866464fb7e076" hs_bindgen_551866464fb7e076_base ::
     BG.Int32
  -> IO BG.Word32

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_int3@
hs_bindgen_551866464fb7e076 ::
     BG.CInt
  -> IO BG.CUInt
hs_bindgen_551866464fb7e076 =
  BG.fromFFIType hs_bindgen_551866464fb7e076_base

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h 59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUInt
ret_int3 = hs_bindgen_551866464fb7e076

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_long1@
foreign import ccall unsafe "hs_bindgen_1cb0ba330b8fc708" hs_bindgen_1cb0ba330b8fc708_base ::
     BG.Int32
  -> IO BG.Int64

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_long1@
hs_bindgen_1cb0ba330b8fc708 ::
     BG.CInt
  -> IO BG.CLong
hs_bindgen_1cb0ba330b8fc708 =
  BG.fromFFIType hs_bindgen_1cb0ba330b8fc708_base

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h 61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long1 = hs_bindgen_1cb0ba330b8fc708

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_long2@
foreign import ccall unsafe "hs_bindgen_84d65e5f2bb20468" hs_bindgen_84d65e5f2bb20468_base ::
     BG.Int32
  -> IO BG.Int64

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_long2@
hs_bindgen_84d65e5f2bb20468 ::
     BG.CInt
  -> IO BG.CLong
hs_bindgen_84d65e5f2bb20468 =
  BG.fromFFIType hs_bindgen_84d65e5f2bb20468_base

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h 62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long2 = hs_bindgen_84d65e5f2bb20468

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_long3@
foreign import ccall unsafe "hs_bindgen_8e5c7c7b2f385152" hs_bindgen_8e5c7c7b2f385152_base ::
     BG.Int32
  -> IO BG.Word64

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_long3@
hs_bindgen_8e5c7c7b2f385152 ::
     BG.CInt
  -> IO BG.CULong
hs_bindgen_8e5c7c7b2f385152 =
  BG.fromFFIType hs_bindgen_8e5c7c7b2f385152_base

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h 63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CULong
ret_long3 = hs_bindgen_8e5c7c7b2f385152

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_float@
foreign import ccall unsafe "hs_bindgen_be7b5328fb1e67e4" hs_bindgen_be7b5328fb1e67e4_base ::
     BG.Int32
  -> IO Float

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_float@
hs_bindgen_be7b5328fb1e67e4 ::
     BG.CInt
  -> IO BG.CFloat
hs_bindgen_be7b5328fb1e67e4 =
  BG.fromFFIType hs_bindgen_be7b5328fb1e67e4_base

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h 65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CFloat
ret_float = hs_bindgen_be7b5328fb1e67e4

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_double@
foreign import ccall unsafe "hs_bindgen_67ccecb1f4c2b26b" hs_bindgen_67ccecb1f4c2b26b_base ::
     BG.Int32
  -> IO Double

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_double@
hs_bindgen_67ccecb1f4c2b26b ::
     BG.CInt
  -> IO BG.CDouble
hs_bindgen_67ccecb1f4c2b26b =
  BG.fromFFIType hs_bindgen_67ccecb1f4c2b26b_base

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h 66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CDouble
ret_double = hs_bindgen_67ccecb1f4c2b26b

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_bool1@
foreign import ccall unsafe "hs_bindgen_8f3058d29b19c7f8" hs_bindgen_8f3058d29b19c7f8_base ::
     BG.Int32
  -> IO BG.Word8

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_bool1@
hs_bindgen_8f3058d29b19c7f8 ::
     BG.CInt
  -> IO BG.CBool
hs_bindgen_8f3058d29b19c7f8 =
  BG.fromFFIType hs_bindgen_8f3058d29b19c7f8_base

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h 67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
ret_bool1 = hs_bindgen_8f3058d29b19c7f8

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_struct@
foreign import ccall unsafe "hs_bindgen_db41d471db80076d" hs_bindgen_db41d471db80076d_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_struct@
hs_bindgen_db41d471db80076d ::
     BG.CInt
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_db41d471db80076d =
  BG.fromFFIType hs_bindgen_db41d471db80076d_base

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
                        hs_bindgen_db41d471db80076d arg10 res1)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_union@
foreign import ccall unsafe "hs_bindgen_a2761d3935650b5c" hs_bindgen_a2761d3935650b5c_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_union@
hs_bindgen_a2761d3935650b5c ::
     BG.CInt
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_a2761d3935650b5c =
  BG.fromFFIType hs_bindgen_a2761d3935650b5c_base

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
                        hs_bindgen_a2761d3935650b5c arg10 res1)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_enum@
foreign import ccall unsafe "hs_bindgen_c567484984a48bdd" hs_bindgen_c567484984a48bdd_base ::
     BG.Int32
  -> IO BG.Word32

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_enum@
hs_bindgen_c567484984a48bdd ::
     BG.CInt
  -> IO Some_enum
hs_bindgen_c567484984a48bdd =
  BG.fromFFIType hs_bindgen_c567484984a48bdd_base

{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h 71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO Some_enum
ret_enum = hs_bindgen_c567484984a48bdd

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_pointer1@
foreign import ccall unsafe "hs_bindgen_a0436717a3858ec2" hs_bindgen_a0436717a3858ec2_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_pointer1@
hs_bindgen_a0436717a3858ec2 ::
     BG.CInt
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_a0436717a3858ec2 =
  BG.fromFFIType hs_bindgen_a0436717a3858ec2_base

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h 73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
ret_pointer1 = hs_bindgen_a0436717a3858ec2

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_pointer2@
foreign import ccall unsafe "hs_bindgen_92e30e7cf97b7ae8" hs_bindgen_92e30e7cf97b7ae8_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_pointer2@
hs_bindgen_92e30e7cf97b7ae8 ::
     BG.CInt
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
hs_bindgen_92e30e7cf97b7ae8 =
  BG.fromFFIType hs_bindgen_92e30e7cf97b7ae8_base

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h 74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
ret_pointer2 = hs_bindgen_92e30e7cf97b7ae8

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_pointer3@
foreign import ccall unsafe "hs_bindgen_cec80a82e028945d" hs_bindgen_cec80a82e028945d_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_pointer3@
hs_bindgen_cec80a82e028945d ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)
hs_bindgen_cec80a82e028945d =
  BG.fromFFIType hs_bindgen_cec80a82e028945d_base

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h 75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.Void)
ret_pointer3 = hs_bindgen_cec80a82e028945d

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_body1@
foreign import ccall unsafe "hs_bindgen_373d2dbedb7f3ea1" hs_bindgen_373d2dbedb7f3ea1_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_body1@
hs_bindgen_373d2dbedb7f3ea1 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_373d2dbedb7f3ea1 =
  BG.fromFFIType hs_bindgen_373d2dbedb7f3ea1_base

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h 79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
body1 = hs_bindgen_373d2dbedb7f3ea1

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_body2@
foreign import ccall unsafe "hs_bindgen_678c705d71b66fd0" hs_bindgen_678c705d71b66fd0_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_body2@
hs_bindgen_678c705d71b66fd0 :: IO BG.CInt
hs_bindgen_678c705d71b66fd0 =
  BG.fromFFIType hs_bindgen_678c705d71b66fd0_base

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h 80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2 :: IO BG.CInt
body2 = hs_bindgen_678c705d71b66fd0

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_complex_float@
foreign import ccall unsafe "hs_bindgen_4a4cdf60b6a4885d" hs_bindgen_4a4cdf60b6a4885d_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_complex_float@
hs_bindgen_4a4cdf60b6a4885d ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_4a4cdf60b6a4885d =
  BG.fromFFIType hs_bindgen_4a4cdf60b6a4885d_base

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
                       hs_bindgen_4a4cdf60b6a4885d arg10 arg22)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_complex_double@
foreign import ccall unsafe "hs_bindgen_4b113ac08bbb35bd" hs_bindgen_4b113ac08bbb35bd_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_args_complex_double@
hs_bindgen_4b113ac08bbb35bd ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_4b113ac08bbb35bd =
  BG.fromFFIType hs_bindgen_4b113ac08bbb35bd_base

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
                       hs_bindgen_4b113ac08bbb35bd arg10 arg22)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_complex_float@
foreign import ccall unsafe "hs_bindgen_63483820a803fad0" hs_bindgen_63483820a803fad0_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_complex_float@
hs_bindgen_63483820a803fad0 ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_63483820a803fad0 =
  BG.fromFFIType hs_bindgen_63483820a803fad0_base

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
                        hs_bindgen_63483820a803fad0 arg10 res1)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_complex_double@
foreign import ccall unsafe "hs_bindgen_00dc8901b39e6dd3" hs_bindgen_00dc8901b39e6dd3_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_ret_complex_double@
hs_bindgen_00dc8901b39e6dd3 ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_00dc8901b39e6dd3 =
  BG.fromFFIType hs_bindgen_00dc8901b39e6dd3_base

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
                        hs_bindgen_00dc8901b39e6dd3 arg10 res1)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_bespoke_args1@
foreign import ccall unsafe "hs_bindgen_c4123c64e3799da5" hs_bindgen_c4123c64e3799da5_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_bespoke_args1@
hs_bindgen_c4123c64e3799da5 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_c4123c64e3799da5 =
  BG.fromFFIType hs_bindgen_c4123c64e3799da5_base

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
bespoke_args1 = hs_bindgen_c4123c64e3799da5

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_bespoke_args2@
foreign import ccall unsafe "hs_bindgen_56568d64374ea82a" hs_bindgen_56568d64374ea82a_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_bespoke_args2@
hs_bindgen_56568d64374ea82a ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_56568d64374ea82a =
  BG.fromFFIType hs_bindgen_56568d64374ea82a_base

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
bespoke_args2 = hs_bindgen_56568d64374ea82a

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_bespoke_ret1@
foreign import ccall unsafe "hs_bindgen_e39afd1aad07e0fc" hs_bindgen_e39afd1aad07e0fc_base ::
     BG.Int32
  -> IO BG.Word8

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_bespoke_ret1@
hs_bindgen_e39afd1aad07e0fc ::
     BG.CInt
  -> IO BG.CBool
hs_bindgen_e39afd1aad07e0fc =
  BG.fromFFIType hs_bindgen_e39afd1aad07e0fc_base

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h 97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
bespoke_ret1 = hs_bindgen_e39afd1aad07e0fc

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_bespoke_ret2@
foreign import ccall unsafe "hs_bindgen_06a7ca6b10b6f1c5" hs_bindgen_06a7ca6b10b6f1c5_base ::
     BG.Int32
  -> IO BG.Word64

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_bespoke_ret2@
hs_bindgen_06a7ca6b10b6f1c5 ::
     BG.CInt
  -> IO HsBindgen.Runtime.LibC.CSize
hs_bindgen_06a7ca6b10b6f1c5 =
  BG.fromFFIType hs_bindgen_06a7ca6b10b6f1c5_base

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h 98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO HsBindgen.Runtime.LibC.CSize
bespoke_ret2 = hs_bindgen_06a7ca6b10b6f1c5

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_arr_args1@
foreign import ccall unsafe "hs_bindgen_085cd2205e0daeff" hs_bindgen_085cd2205e0daeff_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_arr_args1@
hs_bindgen_085cd2205e0daeff ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt))
  -> IO ()
hs_bindgen_085cd2205e0daeff =
  BG.fromFFIType hs_bindgen_085cd2205e0daeff_base

{-| Arrays

    __C declaration:__ @arr_args1@

    __defined at:__ @macros\/reparse.h 104:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args1 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args1 = hs_bindgen_085cd2205e0daeff

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_arr_args2@
foreign import ccall unsafe "hs_bindgen_0df3319af542c072" hs_bindgen_0df3319af542c072_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_arr_args2@
hs_bindgen_0df3319af542c072 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_0df3319af542c072 =
  BG.fromFFIType hs_bindgen_0df3319af542c072_base

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h 105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args2 = hs_bindgen_0df3319af542c072

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_arr_args3@
foreign import ccall unsafe "hs_bindgen_4839d1f4a2742c03" hs_bindgen_4839d1f4a2742c03_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_arr_args3@
hs_bindgen_4839d1f4a2742c03 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt))
  -> IO ()
hs_bindgen_4839d1f4a2742c03 =
  BG.fromFFIType hs_bindgen_4839d1f4a2742c03_base

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h 106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args3 = hs_bindgen_4839d1f4a2742c03

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_arr_args4@
foreign import ccall unsafe "hs_bindgen_c7e68dd8759c969a" hs_bindgen_c7e68dd8759c969a_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_arr_args4@
hs_bindgen_c7e68dd8759c969a ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_c7e68dd8759c969a =
  BG.fromFFIType hs_bindgen_c7e68dd8759c969a_base

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h 107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args4 = hs_bindgen_c7e68dd8759c969a

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_args1@
foreign import ccall unsafe "hs_bindgen_bf24f137d6217c14" hs_bindgen_bf24f137d6217c14_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_args1@
hs_bindgen_bf24f137d6217c14 ::
     BG.CInt
  -> BG.FunPtr (IO ())
  -> IO ()
hs_bindgen_bf24f137d6217c14 =
  BG.fromFFIType hs_bindgen_bf24f137d6217c14_base

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
funptr_args1 = hs_bindgen_bf24f137d6217c14

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_args2@
foreign import ccall unsafe "hs_bindgen_2b6cd604640b023f" hs_bindgen_2b6cd604640b023f_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_args2@
hs_bindgen_2b6cd604640b023f ::
     BG.CInt
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_2b6cd604640b023f =
  BG.fromFFIType hs_bindgen_2b6cd604640b023f_base

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
funptr_args2 = hs_bindgen_2b6cd604640b023f

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_args3@
foreign import ccall unsafe "hs_bindgen_9c35d7346db373f0" hs_bindgen_9c35d7346db373f0_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_args3@
hs_bindgen_9c35d7346db373f0 ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> IO ())
  -> IO ()
hs_bindgen_9c35d7346db373f0 =
  BG.fromFFIType hs_bindgen_9c35d7346db373f0_base

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
funptr_args3 = hs_bindgen_9c35d7346db373f0

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_args4@
foreign import ccall unsafe "hs_bindgen_60245cd22b3cd59b" hs_bindgen_60245cd22b3cd59b_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_args4@
hs_bindgen_60245cd22b3cd59b ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar)
  -> IO ()
hs_bindgen_60245cd22b3cd59b =
  BG.fromFFIType hs_bindgen_60245cd22b3cd59b_base

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
funptr_args4 = hs_bindgen_60245cd22b3cd59b

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_args5@
foreign import ccall unsafe "hs_bindgen_bcd5e0817da0463b" hs_bindgen_bcd5e0817da0463b_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_args5@
hs_bindgen_bcd5e0817da0463b ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))
  -> IO ()
hs_bindgen_bcd5e0817da0463b =
  BG.fromFFIType hs_bindgen_bcd5e0817da0463b_base

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
funptr_args5 = hs_bindgen_bcd5e0817da0463b

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_comments1@
foreign import ccall unsafe "hs_bindgen_fd3d542dfefd036e" hs_bindgen_fd3d542dfefd036e_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_comments1@
hs_bindgen_fd3d542dfefd036e ::
     BG.CInt
  -> IO ()
hs_bindgen_fd3d542dfefd036e =
  BG.fromFFIType hs_bindgen_fd3d542dfefd036e_base

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
comments1 = hs_bindgen_fd3d542dfefd036e

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_prim_before1@
foreign import ccall unsafe "hs_bindgen_0819c4409f0cb04f" hs_bindgen_0819c4409f0cb04f_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_prim_before1@
hs_bindgen_0819c4409f0cb04f ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_0819c4409f0cb04f =
  BG.fromFFIType hs_bindgen_0819c4409f0cb04f_base

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
const_prim_before1 = hs_bindgen_0819c4409f0cb04f

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_prim_before2@
foreign import ccall unsafe "hs_bindgen_d31ae17d88140355" hs_bindgen_d31ae17d88140355_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_prim_before2@
hs_bindgen_d31ae17d88140355 ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_d31ae17d88140355 =
  BG.fromFFIType hs_bindgen_d31ae17d88140355_base

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
const_prim_before2 = hs_bindgen_d31ae17d88140355

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_prim_before3@
foreign import ccall unsafe "hs_bindgen_f0ed5f637cefe96f" hs_bindgen_f0ed5f637cefe96f_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_prim_before3@
hs_bindgen_f0ed5f637cefe96f ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_f0ed5f637cefe96f =
  BG.fromFFIType hs_bindgen_f0ed5f637cefe96f_base

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
const_prim_before3 = hs_bindgen_f0ed5f637cefe96f

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_prim_after1@
foreign import ccall unsafe "hs_bindgen_0215a515d1fa3dfa" hs_bindgen_0215a515d1fa3dfa_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_prim_after1@
hs_bindgen_0215a515d1fa3dfa ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_0215a515d1fa3dfa =
  BG.fromFFIType hs_bindgen_0215a515d1fa3dfa_base

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
const_prim_after1 = hs_bindgen_0215a515d1fa3dfa

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_prim_after2@
foreign import ccall unsafe "hs_bindgen_e103c029e3630b33" hs_bindgen_e103c029e3630b33_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_prim_after2@
hs_bindgen_e103c029e3630b33 ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_e103c029e3630b33 =
  BG.fromFFIType hs_bindgen_e103c029e3630b33_base

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
const_prim_after2 = hs_bindgen_e103c029e3630b33

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_prim_after3@
foreign import ccall unsafe "hs_bindgen_bef8a67258cb736b" hs_bindgen_bef8a67258cb736b_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_prim_after3@
hs_bindgen_bef8a67258cb736b ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_bef8a67258cb736b =
  BG.fromFFIType hs_bindgen_bef8a67258cb736b_base

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
const_prim_after3 = hs_bindgen_bef8a67258cb736b

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_before1@
foreign import ccall unsafe "hs_bindgen_589e465944877183" hs_bindgen_589e465944877183_base ::
     BG.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_before1@
hs_bindgen_589e465944877183 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_589e465944877183 =
  BG.fromFFIType hs_bindgen_589e465944877183_base

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
  hs_bindgen_589e465944877183

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_before2@
foreign import ccall unsafe "hs_bindgen_fef23ca0f7241d44" hs_bindgen_fef23ca0f7241d44_base ::
     BG.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_before2@
hs_bindgen_fef23ca0f7241d44 ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_fef23ca0f7241d44 =
  BG.fromFFIType hs_bindgen_fef23ca0f7241d44_base

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
  hs_bindgen_fef23ca0f7241d44

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_before3@
foreign import ccall unsafe "hs_bindgen_12ed6589e368a6d9" hs_bindgen_12ed6589e368a6d9_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_before3@
hs_bindgen_12ed6589e368a6d9 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_12ed6589e368a6d9 =
  BG.fromFFIType hs_bindgen_12ed6589e368a6d9_base

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
  hs_bindgen_12ed6589e368a6d9

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_before4@
foreign import ccall unsafe "hs_bindgen_acfc6cbb94266577" hs_bindgen_acfc6cbb94266577_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_before4@
hs_bindgen_acfc6cbb94266577 ::
     BG.CInt
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_acfc6cbb94266577 =
  BG.fromFFIType hs_bindgen_acfc6cbb94266577_base

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
                       hs_bindgen_acfc6cbb94266577 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_before5@
foreign import ccall unsafe "hs_bindgen_e452087578f98911" hs_bindgen_e452087578f98911_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_before5@
hs_bindgen_e452087578f98911 ::
     BG.CInt
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_e452087578f98911 =
  BG.fromFFIType hs_bindgen_e452087578f98911_base

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
                       hs_bindgen_e452087578f98911 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_before6@
foreign import ccall unsafe "hs_bindgen_a943f6ee50043676" hs_bindgen_a943f6ee50043676_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_before6@
hs_bindgen_a943f6ee50043676 ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_a943f6ee50043676 =
  BG.fromFFIType hs_bindgen_a943f6ee50043676_base

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
  hs_bindgen_a943f6ee50043676

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_before7@
foreign import ccall unsafe "hs_bindgen_425f71a0e411bca9" hs_bindgen_425f71a0e411bca9_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_before7@
hs_bindgen_425f71a0e411bca9 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_425f71a0e411bca9 =
  BG.fromFFIType hs_bindgen_425f71a0e411bca9_base

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
  hs_bindgen_425f71a0e411bca9

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_before8@
foreign import ccall unsafe "hs_bindgen_297d1f2aa423722a" hs_bindgen_297d1f2aa423722a_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_before8@
hs_bindgen_297d1f2aa423722a ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_297d1f2aa423722a =
  BG.fromFFIType hs_bindgen_297d1f2aa423722a_base

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
  hs_bindgen_297d1f2aa423722a

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_after1@
foreign import ccall unsafe "hs_bindgen_136c1c8ee6a77c65" hs_bindgen_136c1c8ee6a77c65_base ::
     BG.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_after1@
hs_bindgen_136c1c8ee6a77c65 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_136c1c8ee6a77c65 =
  BG.fromFFIType hs_bindgen_136c1c8ee6a77c65_base

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
  hs_bindgen_136c1c8ee6a77c65

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_after2@
foreign import ccall unsafe "hs_bindgen_876526a42ba48417" hs_bindgen_876526a42ba48417_base ::
     BG.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_after2@
hs_bindgen_876526a42ba48417 ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_876526a42ba48417 =
  BG.fromFFIType hs_bindgen_876526a42ba48417_base

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
  hs_bindgen_876526a42ba48417

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_after3@
foreign import ccall unsafe "hs_bindgen_b309fa8d82dd7b72" hs_bindgen_b309fa8d82dd7b72_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_after3@
hs_bindgen_b309fa8d82dd7b72 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_b309fa8d82dd7b72 =
  BG.fromFFIType hs_bindgen_b309fa8d82dd7b72_base

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
  hs_bindgen_b309fa8d82dd7b72

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_after4@
foreign import ccall unsafe "hs_bindgen_b7b3fe289c724859" hs_bindgen_b7b3fe289c724859_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_after4@
hs_bindgen_b7b3fe289c724859 ::
     BG.CInt
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_b7b3fe289c724859 =
  BG.fromFFIType hs_bindgen_b7b3fe289c724859_base

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
                       hs_bindgen_b7b3fe289c724859 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_after5@
foreign import ccall unsafe "hs_bindgen_55b1796019967748" hs_bindgen_55b1796019967748_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_after5@
hs_bindgen_55b1796019967748 ::
     BG.CInt
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_55b1796019967748 =
  BG.fromFFIType hs_bindgen_55b1796019967748_base

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
                       hs_bindgen_55b1796019967748 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_after6@
foreign import ccall unsafe "hs_bindgen_b10d1e8f7c40c632" hs_bindgen_b10d1e8f7c40c632_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_after6@
hs_bindgen_b10d1e8f7c40c632 ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_b10d1e8f7c40c632 =
  BG.fromFFIType hs_bindgen_b10d1e8f7c40c632_base

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
  hs_bindgen_b10d1e8f7c40c632

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_after7@
foreign import ccall unsafe "hs_bindgen_04406c6b24b54172" hs_bindgen_04406c6b24b54172_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_after7@
hs_bindgen_04406c6b24b54172 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_04406c6b24b54172 =
  BG.fromFFIType hs_bindgen_04406c6b24b54172_base

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
  hs_bindgen_04406c6b24b54172

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_after8@
foreign import ccall unsafe "hs_bindgen_ae945fd854416c29" hs_bindgen_ae945fd854416c29_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_withoutSign_after8@
hs_bindgen_ae945fd854416c29 ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_ae945fd854416c29 =
  BG.fromFFIType hs_bindgen_ae945fd854416c29_base

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
  hs_bindgen_ae945fd854416c29

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_args1@
foreign import ccall unsafe "hs_bindgen_315fb7a4e52f51bb" hs_bindgen_315fb7a4e52f51bb_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_args1@
hs_bindgen_315fb7a4e52f51bb ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_315fb7a4e52f51bb =
  BG.fromFFIType hs_bindgen_315fb7a4e52f51bb_base

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
const_pointers_args1 = hs_bindgen_315fb7a4e52f51bb

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_args2@
foreign import ccall unsafe "hs_bindgen_c8fe0d37027b1cbd" hs_bindgen_c8fe0d37027b1cbd_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_args2@
hs_bindgen_c8fe0d37027b1cbd ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_c8fe0d37027b1cbd =
  BG.fromFFIType hs_bindgen_c8fe0d37027b1cbd_base

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
const_pointers_args2 = hs_bindgen_c8fe0d37027b1cbd

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_args3@
foreign import ccall unsafe "hs_bindgen_585e1489b3f5af58" hs_bindgen_585e1489b3f5af58_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_args3@
hs_bindgen_585e1489b3f5af58 ::
     BG.CInt
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_585e1489b3f5af58 =
  BG.fromFFIType hs_bindgen_585e1489b3f5af58_base

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
const_pointers_args3 = hs_bindgen_585e1489b3f5af58

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_args4@
foreign import ccall unsafe "hs_bindgen_b9568a159c32cefc" hs_bindgen_b9568a159c32cefc_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_args4@
hs_bindgen_b9568a159c32cefc ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_b9568a159c32cefc =
  BG.fromFFIType hs_bindgen_b9568a159c32cefc_base

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
const_pointers_args4 = hs_bindgen_b9568a159c32cefc

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_args5@
foreign import ccall unsafe "hs_bindgen_85f146afc2a27268" hs_bindgen_85f146afc2a27268_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_args5@
hs_bindgen_85f146afc2a27268 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_85f146afc2a27268 =
  BG.fromFFIType hs_bindgen_85f146afc2a27268_base

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
const_pointers_args5 = hs_bindgen_85f146afc2a27268

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_ret1@
foreign import ccall unsafe "hs_bindgen_ea6a7e1d0274f90d" hs_bindgen_ea6a7e1d0274f90d_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_ret1@
hs_bindgen_ea6a7e1d0274f90d ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_ea6a7e1d0274f90d =
  BG.fromFFIType hs_bindgen_ea6a7e1d0274f90d_base

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h 212:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret1 = hs_bindgen_ea6a7e1d0274f90d

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_ret2@
foreign import ccall unsafe "hs_bindgen_ae320a2e858ccc98" hs_bindgen_ae320a2e858ccc98_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_ret2@
hs_bindgen_ae320a2e858ccc98 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_ae320a2e858ccc98 =
  BG.fromFFIType hs_bindgen_ae320a2e858ccc98_base

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h 213:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret2 = hs_bindgen_ae320a2e858ccc98

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_ret3@
foreign import ccall unsafe "hs_bindgen_3ca6c6656d62db21" hs_bindgen_3ca6c6656d62db21_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_ret3@
hs_bindgen_3ca6c6656d62db21 ::
     BG.CInt
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_3ca6c6656d62db21 =
  BG.fromFFIType hs_bindgen_3ca6c6656d62db21_base

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h 214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
const_pointers_ret3 = hs_bindgen_3ca6c6656d62db21

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_ret4@
foreign import ccall unsafe "hs_bindgen_83d0056641627b70" hs_bindgen_83d0056641627b70_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_ret4@
hs_bindgen_83d0056641627b70 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_83d0056641627b70 =
  BG.fromFFIType hs_bindgen_83d0056641627b70_base

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h 215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret4 = hs_bindgen_83d0056641627b70

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_ret5@
foreign import ccall unsafe "hs_bindgen_89f54f00e5f1ec61" hs_bindgen_89f54f00e5f1ec61_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_pointers_ret5@
hs_bindgen_89f54f00e5f1ec61 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_89f54f00e5f1ec61 =
  BG.fromFFIType hs_bindgen_89f54f00e5f1ec61_base

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h 216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret5 = hs_bindgen_89f54f00e5f1ec61

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_array_elem1@
foreign import ccall unsafe "hs_bindgen_445aea27fa0b55be" hs_bindgen_445aea27fa0b55be_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_array_elem1@
hs_bindgen_445aea27fa0b55be ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt))
  -> IO ()
hs_bindgen_445aea27fa0b55be =
  BG.fromFFIType hs_bindgen_445aea27fa0b55be_base

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h 244:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem1 = hs_bindgen_445aea27fa0b55be

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_array_elem2@
foreign import ccall unsafe "hs_bindgen_2f9fd49dce178c29" hs_bindgen_2f9fd49dce178c29_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_array_elem2@
hs_bindgen_2f9fd49dce178c29 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt)))
  -> IO ()
hs_bindgen_2f9fd49dce178c29 =
  BG.fromFFIType hs_bindgen_2f9fd49dce178c29_base

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h 245:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem2 = hs_bindgen_2f9fd49dce178c29

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_array_elem3@
foreign import ccall unsafe "hs_bindgen_99716bef732a1af1" hs_bindgen_99716bef732a1af1_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_const_array_elem3@
hs_bindgen_99716bef732a1af1 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_99716bef732a1af1 =
  BG.fromFFIType hs_bindgen_99716bef732a1af1_base

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h 246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem3 = hs_bindgen_99716bef732a1af1

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_noParams1@
foreign import ccall unsafe "hs_bindgen_1721fcf9bca3ea61" hs_bindgen_1721fcf9bca3ea61_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_noParams1@
hs_bindgen_1721fcf9bca3ea61 :: IO BG.CInt
hs_bindgen_1721fcf9bca3ea61 =
  BG.fromFFIType hs_bindgen_1721fcf9bca3ea61_base

{-| Other examples we reparsed /incorrectly/ before language-c

    __C declaration:__ @noParams1@

    __defined at:__ @macros\/reparse.h 254:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams1 :: IO BG.CInt
noParams1 = hs_bindgen_1721fcf9bca3ea61

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_noParams2@
foreign import ccall unsafe "hs_bindgen_95fe913b1eec4f4e" hs_bindgen_95fe913b1eec4f4e_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_noParams2@
hs_bindgen_95fe913b1eec4f4e :: IO BG.CInt
hs_bindgen_95fe913b1eec4f4e =
  BG.fromFFIType hs_bindgen_95fe913b1eec4f4e_base

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h 255:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2 :: IO BG.CInt
noParams2 = hs_bindgen_95fe913b1eec4f4e

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_noParams3@
foreign import ccall unsafe "hs_bindgen_d15b46c7c30f29ac" hs_bindgen_d15b46c7c30f29ac_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_noParams3@
hs_bindgen_d15b46c7c30f29ac ::
     BG.CInt
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_d15b46c7c30f29ac =
  BG.fromFFIType hs_bindgen_d15b46c7c30f29ac_base

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
noParams3 = hs_bindgen_d15b46c7c30f29ac

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret1@
foreign import ccall unsafe "hs_bindgen_fe09e75d56f9869e" hs_bindgen_fe09e75d56f9869e_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret1@
hs_bindgen_fe09e75d56f9869e ::
     BG.CInt
  -> IO (BG.FunPtr (IO ()))
hs_bindgen_fe09e75d56f9869e =
  BG.fromFFIType hs_bindgen_fe09e75d56f9869e_base

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h 260:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO ()))
funptr_ret1 = hs_bindgen_fe09e75d56f9869e

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret2@
foreign import ccall unsafe "hs_bindgen_7f6cbcd68ce4b354" hs_bindgen_7f6cbcd68ce4b354_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret2@
hs_bindgen_7f6cbcd68ce4b354 ::
     BG.CInt
  -> IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_7f6cbcd68ce4b354 =
  BG.fromFFIType hs_bindgen_7f6cbcd68ce4b354_base

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h 261:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO BG.CInt))
funptr_ret2 = hs_bindgen_7f6cbcd68ce4b354

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret3@
foreign import ccall unsafe "hs_bindgen_1bcf333c8e82d009" hs_bindgen_1bcf333c8e82d009_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret3@
hs_bindgen_1bcf333c8e82d009 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_1bcf333c8e82d009 =
  BG.fromFFIType hs_bindgen_1bcf333c8e82d009_base

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h 262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
funptr_ret3 = hs_bindgen_1bcf333c8e82d009

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret4@
foreign import ccall unsafe "hs_bindgen_4793bc049c8510a4" hs_bindgen_4793bc049c8510a4_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret4@
hs_bindgen_4793bc049c8510a4 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
hs_bindgen_4793bc049c8510a4 =
  BG.fromFFIType hs_bindgen_4793bc049c8510a4_base

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h 263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
funptr_ret4 = hs_bindgen_4793bc049c8510a4

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret5@
foreign import ccall unsafe "hs_bindgen_abb2d85efd4fcb3e" hs_bindgen_abb2d85efd4fcb3e_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret5@
hs_bindgen_abb2d85efd4fcb3e ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_abb2d85efd4fcb3e =
  BG.fromFFIType hs_bindgen_abb2d85efd4fcb3e_base

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h 267:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret5 = hs_bindgen_abb2d85efd4fcb3e

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret6@
foreign import ccall unsafe "hs_bindgen_6cf3159cf7618393" hs_bindgen_6cf3159cf7618393_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret6@
hs_bindgen_6cf3159cf7618393 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_6cf3159cf7618393 =
  BG.fromFFIType hs_bindgen_6cf3159cf7618393_base

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h 268:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret6 = hs_bindgen_6cf3159cf7618393

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret7@
foreign import ccall unsafe "hs_bindgen_ae99f146e73b6c1a" hs_bindgen_ae99f146e73b6c1a_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret7@
hs_bindgen_ae99f146e73b6c1a ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_ae99f146e73b6c1a =
  BG.fromFFIType hs_bindgen_ae99f146e73b6c1a_base

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h 269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret7 = hs_bindgen_ae99f146e73b6c1a

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret8@
foreign import ccall unsafe "hs_bindgen_5ac0a15c42b191c7" hs_bindgen_5ac0a15c42b191c7_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret8@
hs_bindgen_5ac0a15c42b191c7 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_5ac0a15c42b191c7 =
  BG.fromFFIType hs_bindgen_5ac0a15c42b191c7_base

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h 270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret8 = hs_bindgen_5ac0a15c42b191c7

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret9@
foreign import ccall unsafe "hs_bindgen_0dbef4dde3b3d205" hs_bindgen_0dbef4dde3b3d205_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret9@
hs_bindgen_0dbef4dde3b3d205 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_0dbef4dde3b3d205 =
  BG.fromFFIType hs_bindgen_0dbef4dde3b3d205_base

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h 271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret9 = hs_bindgen_0dbef4dde3b3d205

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret10@
foreign import ccall unsafe "hs_bindgen_c226f0b9bbf08c49" hs_bindgen_c226f0b9bbf08c49_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_Unsafe_funptr_ret10@
hs_bindgen_c226f0b9bbf08c49 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_c226f0b9bbf08c49 =
  BG.fromFFIType hs_bindgen_c226f0b9bbf08c49_base

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h 272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret10 = hs_bindgen_c226f0b9bbf08c49
