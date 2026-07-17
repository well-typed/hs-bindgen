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
  , "void hs_bindgen_f15610128336b06a ("
  , "  A arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  (args_char1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_087f45ca0a284a03 ("
  , "  A arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  (args_char2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f6cb5c5a728c2404 ("
  , "  A arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  (args_char3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d485767e0caa1f7c ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_833c96c437533e02 ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0e1eedc3fcbcea7a ("
  , "  A arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  (args_short3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_906f0ac7dfd36ab8 ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0edbc9b995b2a589 ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a5c223f58a255115 ("
  , "  A arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  (args_int3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_41d1229384b9a529 ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a9a4b09fd3bd83db ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_31dc2e680b3f3eff ("
  , "  A arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  (args_long3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3d400757b5cbf4b7 ("
  , "  A arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  (args_float)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_70df07e39900487e ("
  , "  A arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  (args_double)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0b7c534fe683f843 ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (args_bool1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b20e084f7b7941b5 ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  (args_struct)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_23aff33f33b6bdd1 ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  (args_union)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_fdd58ae14ce15ed5 ("
  , "  A arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  (args_enum)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_42ce2ec4fd2eda72 ("
  , "  A arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  (args_pointer1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_01b2f6502d340abe ("
  , "  A arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  (args_pointer2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3e64133f9aaebbf1 ("
  , "  A arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  (args_pointer3)(arg1, arg2);"
  , "}"
  , "A hs_bindgen_c830401b459192fb (void)"
  , "{"
  , "  return (ret_A)();"
  , "}"
  , "char hs_bindgen_18b24c6e67a5412e ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_char1)(arg1);"
  , "}"
  , "signed char hs_bindgen_2da1160aeef9ff64 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_char2)(arg1);"
  , "}"
  , "unsigned char hs_bindgen_e3183f9de1b9f231 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_char3)(arg1);"
  , "}"
  , "signed short hs_bindgen_c313966d4478e3f4 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_short1)(arg1);"
  , "}"
  , "signed short hs_bindgen_737fbec310eb0719 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_short2)(arg1);"
  , "}"
  , "unsigned short hs_bindgen_b5bd9e111020db4e ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_short3)(arg1);"
  , "}"
  , "signed int hs_bindgen_a30224259287f5f8 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_int1)(arg1);"
  , "}"
  , "signed int hs_bindgen_b5be09caf8cf5750 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_int2)(arg1);"
  , "}"
  , "unsigned int hs_bindgen_698e3f97470d83be ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_int3)(arg1);"
  , "}"
  , "signed long hs_bindgen_c7e0705dd09be530 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_long1)(arg1);"
  , "}"
  , "signed long hs_bindgen_74b1f5b8c56ff22c ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_long2)(arg1);"
  , "}"
  , "unsigned long hs_bindgen_c2d07eaaab82d408 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_long3)(arg1);"
  , "}"
  , "float hs_bindgen_0edfbc7067faa1f7 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_float)(arg1);"
  , "}"
  , "double hs_bindgen_786ca672396b33be ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_double)(arg1);"
  , "}"
  , "_Bool hs_bindgen_2e99f19b59650996 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_bool1)(arg1);"
  , "}"
  , "void hs_bindgen_6c999121eed8178f ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_struct)(arg1);"
  , "}"
  , "void hs_bindgen_481ee5d2d9bd34db ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_union)(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_8bb240ba453b700d ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_enum)(arg1);"
  , "}"
  , "signed int *hs_bindgen_c346ed2cd20b9af1 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_pointer1)(arg1);"
  , "}"
  , "signed int **hs_bindgen_a21f618658151728 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_pointer2)(arg1);"
  , "}"
  , "void *hs_bindgen_2d8c6e2d2f395342 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_pointer3)(arg1);"
  , "}"
  , "signed int hs_bindgen_b030d02030ed80bc ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (body1)(arg1);"
  , "}"
  , "A hs_bindgen_be50427e6a63df54 (void)"
  , "{"
  , "  return (body2)();"
  , "}"
  , "void hs_bindgen_627a52a5c7617083 ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_float)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_392babebc1d83503 ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_double)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_8958183ede73dea8 ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_float)(arg1);"
  , "}"
  , "void hs_bindgen_a95fabfd391a99aa ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_double)(arg1);"
  , "}"
  , "void hs_bindgen_ad9f8630dd04a203 ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (bespoke_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4b34178a505131e2 ("
  , "  A arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  (bespoke_args2)(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_94b225a6394496c1 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret1)(arg1);"
  , "}"
  , "size_t hs_bindgen_7c9a1792426b84a1 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret2)(arg1);"
  , "}"
  , "void hs_bindgen_e20689fe39004225 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  (arr_args1)(arg1);"
  , "}"
  , "void hs_bindgen_084796e4bfd3f4cd ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  (arr_args2)(arg1);"
  , "}"
  , "void hs_bindgen_a3d1560aaa4352df ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  (arr_args3)(arg1);"
  , "}"
  , "void hs_bindgen_88659ccccc6c1f5f ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  (arr_args4)(arg1);"
  , "}"
  , "void hs_bindgen_3448d03cfd41161a ("
  , "  A arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_92d7386f0a327d25 ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2881f594f98043e6 ("
  , "  A arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1e85a05df4251f62 ("
  , "  A arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ccf4db7511f0d6d6 ("
  , "  A arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args5)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4c756db60673d221 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  (comments1)(arg1);"
  , "}"
  , "void hs_bindgen_278568d7a2a3a4b6 ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_87ee56525e5ea20c ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c0b99594235bd99e ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d718b682f157fc18 ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f2c5b3d5eca68433 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ae2d994e06667b23 ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6940b58e7f4397a7 ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_00b6fe2282e779b1 ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6517cc8d39aead93 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_68c7661e95060488 ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_42b3b0bf73a7a51a ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_c4aabe9834aac12f ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_486090a7fb4e34d4 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_23fa742b614176dd ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0aacd8a5d48f296d ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_16ec2102221485b7 ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9aa934d44ec3790c ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_043d2869e29bedcf ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_b5f9bca1de9d69de ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_77d641d518b2504f ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_691b4f2909140b49 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ae74c8dcdc2ec9eb ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_07606c41eadf9146 ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3836769f3a3416ac ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_12f19ea593aefd3f ("
  , "  A arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_5a50e98897696d57 ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_666701f7cb61bd15 ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args5)(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_b94fbc3dfd285563 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret1)(arg1);"
  , "}"
  , "signed int const *hs_bindgen_33e2960e26b79450 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret2)(arg1);"
  , "}"
  , "signed int *const hs_bindgen_50c6e2fe4f3fb777 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret3)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_edc014695d896c8d ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret4)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_6d3308cc5847f033 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret5)(arg1);"
  , "}"
  , "void hs_bindgen_678576320923a4d1 ("
  , "  A const *arg1"
  , ")"
  , "{"
  , "  (const_array_elem1)(arg1);"
  , "}"
  , "void hs_bindgen_b317941dde4eeff2 ("
  , "  A const **arg1"
  , ")"
  , "{"
  , "  (const_array_elem2)(arg1);"
  , "}"
  , "void hs_bindgen_707e602e6beb1bb6 ("
  , "  A *const *arg1"
  , ")"
  , "{"
  , "  (const_array_elem3)(arg1);"
  , "}"
  , "A hs_bindgen_93fecb4eb766c262 (void)"
  , "{"
  , "  return (noParams1)();"
  , "}"
  , "A hs_bindgen_4350965157c891f5 (void)"
  , "{"
  , "  return (noParams2)();"
  , "}"
  , "void hs_bindgen_c4f59272a2b1c3b5 ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (noParams3)(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_387a04c01e23c320 ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_6f0c14cd3478dc19 ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret2)(arg1);"
  , "}"
  , "void (*hs_bindgen_08e8661d277cf7be ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (funptr_ret3)(arg1);"
  , "}"
  , "char (*hs_bindgen_609b5d953b68da92 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret4)(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_13e6ae43abf40aee ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret5)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_a4a3a86f28ca6299 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret6)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_eae9dff04c88d00b ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret7)(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_894457d90a2fc8db ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret8)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_c893eb15ad9bc68c ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret9)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_d96c258298a44b28 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret10)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_Example_Safe_args_char1@
foreign import ccall safe "hs_bindgen_f15610128336b06a" hs_bindgen_f15610128336b06a_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_char1@
hs_bindgen_f15610128336b06a ::
     A
  -> BG.CChar
  -> IO ()
hs_bindgen_f15610128336b06a =
  BG.fromFFIType hs_bindgen_f15610128336b06a_base

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
args_char1 = hs_bindgen_f15610128336b06a

-- __unique:__ @test_macrosreparse_Example_Safe_args_char2@
foreign import ccall safe "hs_bindgen_087f45ca0a284a03" hs_bindgen_087f45ca0a284a03_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_char2@
hs_bindgen_087f45ca0a284a03 ::
     A
  -> BG.CSChar
  -> IO ()
hs_bindgen_087f45ca0a284a03 =
  BG.fromFFIType hs_bindgen_087f45ca0a284a03_base

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
args_char2 = hs_bindgen_087f45ca0a284a03

-- __unique:__ @test_macrosreparse_Example_Safe_args_char3@
foreign import ccall safe "hs_bindgen_f6cb5c5a728c2404" hs_bindgen_f6cb5c5a728c2404_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_char3@
hs_bindgen_f6cb5c5a728c2404 ::
     A
  -> BG.CUChar
  -> IO ()
hs_bindgen_f6cb5c5a728c2404 =
  BG.fromFFIType hs_bindgen_f6cb5c5a728c2404_base

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
args_char3 = hs_bindgen_f6cb5c5a728c2404

-- __unique:__ @test_macrosreparse_Example_Safe_args_short1@
foreign import ccall safe "hs_bindgen_d485767e0caa1f7c" hs_bindgen_d485767e0caa1f7c_base ::
     BG.Int32
  -> BG.Int16
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_short1@
hs_bindgen_d485767e0caa1f7c ::
     A
  -> BG.CShort
  -> IO ()
hs_bindgen_d485767e0caa1f7c =
  BG.fromFFIType hs_bindgen_d485767e0caa1f7c_base

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
args_short1 = hs_bindgen_d485767e0caa1f7c

-- __unique:__ @test_macrosreparse_Example_Safe_args_short2@
foreign import ccall safe "hs_bindgen_833c96c437533e02" hs_bindgen_833c96c437533e02_base ::
     BG.Int32
  -> BG.Int16
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_short2@
hs_bindgen_833c96c437533e02 ::
     A
  -> BG.CShort
  -> IO ()
hs_bindgen_833c96c437533e02 =
  BG.fromFFIType hs_bindgen_833c96c437533e02_base

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
args_short2 = hs_bindgen_833c96c437533e02

-- __unique:__ @test_macrosreparse_Example_Safe_args_short3@
foreign import ccall safe "hs_bindgen_0e1eedc3fcbcea7a" hs_bindgen_0e1eedc3fcbcea7a_base ::
     BG.Int32
  -> BG.Word16
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_short3@
hs_bindgen_0e1eedc3fcbcea7a ::
     A
  -> BG.CUShort
  -> IO ()
hs_bindgen_0e1eedc3fcbcea7a =
  BG.fromFFIType hs_bindgen_0e1eedc3fcbcea7a_base

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
args_short3 = hs_bindgen_0e1eedc3fcbcea7a

-- __unique:__ @test_macrosreparse_Example_Safe_args_int1@
foreign import ccall safe "hs_bindgen_906f0ac7dfd36ab8" hs_bindgen_906f0ac7dfd36ab8_base ::
     BG.Int32
  -> BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_int1@
hs_bindgen_906f0ac7dfd36ab8 ::
     A
  -> BG.CInt
  -> IO ()
hs_bindgen_906f0ac7dfd36ab8 =
  BG.fromFFIType hs_bindgen_906f0ac7dfd36ab8_base

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
args_int1 = hs_bindgen_906f0ac7dfd36ab8

-- __unique:__ @test_macrosreparse_Example_Safe_args_int2@
foreign import ccall safe "hs_bindgen_0edbc9b995b2a589" hs_bindgen_0edbc9b995b2a589_base ::
     BG.Int32
  -> BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_int2@
hs_bindgen_0edbc9b995b2a589 ::
     A
  -> BG.CInt
  -> IO ()
hs_bindgen_0edbc9b995b2a589 =
  BG.fromFFIType hs_bindgen_0edbc9b995b2a589_base

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
args_int2 = hs_bindgen_0edbc9b995b2a589

-- __unique:__ @test_macrosreparse_Example_Safe_args_int3@
foreign import ccall safe "hs_bindgen_a5c223f58a255115" hs_bindgen_a5c223f58a255115_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_int3@
hs_bindgen_a5c223f58a255115 ::
     A
  -> BG.CUInt
  -> IO ()
hs_bindgen_a5c223f58a255115 =
  BG.fromFFIType hs_bindgen_a5c223f58a255115_base

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
args_int3 = hs_bindgen_a5c223f58a255115

-- __unique:__ @test_macrosreparse_Example_Safe_args_long1@
foreign import ccall safe "hs_bindgen_41d1229384b9a529" hs_bindgen_41d1229384b9a529_base ::
     BG.Int32
  -> BG.Int64
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_long1@
hs_bindgen_41d1229384b9a529 ::
     A
  -> BG.CLong
  -> IO ()
hs_bindgen_41d1229384b9a529 =
  BG.fromFFIType hs_bindgen_41d1229384b9a529_base

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
args_long1 = hs_bindgen_41d1229384b9a529

-- __unique:__ @test_macrosreparse_Example_Safe_args_long2@
foreign import ccall safe "hs_bindgen_a9a4b09fd3bd83db" hs_bindgen_a9a4b09fd3bd83db_base ::
     BG.Int32
  -> BG.Int64
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_long2@
hs_bindgen_a9a4b09fd3bd83db ::
     A
  -> BG.CLong
  -> IO ()
hs_bindgen_a9a4b09fd3bd83db =
  BG.fromFFIType hs_bindgen_a9a4b09fd3bd83db_base

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
args_long2 = hs_bindgen_a9a4b09fd3bd83db

-- __unique:__ @test_macrosreparse_Example_Safe_args_long3@
foreign import ccall safe "hs_bindgen_31dc2e680b3f3eff" hs_bindgen_31dc2e680b3f3eff_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_long3@
hs_bindgen_31dc2e680b3f3eff ::
     A
  -> BG.CULong
  -> IO ()
hs_bindgen_31dc2e680b3f3eff =
  BG.fromFFIType hs_bindgen_31dc2e680b3f3eff_base

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
args_long3 = hs_bindgen_31dc2e680b3f3eff

-- __unique:__ @test_macrosreparse_Example_Safe_args_float@
foreign import ccall safe "hs_bindgen_3d400757b5cbf4b7" hs_bindgen_3d400757b5cbf4b7_base ::
     BG.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_float@
hs_bindgen_3d400757b5cbf4b7 ::
     A
  -> BG.CFloat
  -> IO ()
hs_bindgen_3d400757b5cbf4b7 =
  BG.fromFFIType hs_bindgen_3d400757b5cbf4b7_base

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
args_float = hs_bindgen_3d400757b5cbf4b7

-- __unique:__ @test_macrosreparse_Example_Safe_args_double@
foreign import ccall safe "hs_bindgen_70df07e39900487e" hs_bindgen_70df07e39900487e_base ::
     BG.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_double@
hs_bindgen_70df07e39900487e ::
     A
  -> BG.CDouble
  -> IO ()
hs_bindgen_70df07e39900487e =
  BG.fromFFIType hs_bindgen_70df07e39900487e_base

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
args_double = hs_bindgen_70df07e39900487e

-- __unique:__ @test_macrosreparse_Example_Safe_args_bool1@
foreign import ccall safe "hs_bindgen_0b7c534fe683f843" hs_bindgen_0b7c534fe683f843_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_bool1@
hs_bindgen_0b7c534fe683f843 ::
     A
  -> BG.CBool
  -> IO ()
hs_bindgen_0b7c534fe683f843 =
  BG.fromFFIType hs_bindgen_0b7c534fe683f843_base

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
args_bool1 = hs_bindgen_0b7c534fe683f843

-- __unique:__ @test_macrosreparse_Example_Safe_args_struct@
foreign import ccall safe "hs_bindgen_b20e084f7b7941b5" hs_bindgen_b20e084f7b7941b5_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_struct@
hs_bindgen_b20e084f7b7941b5 ::
     A
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_b20e084f7b7941b5 =
  BG.fromFFIType hs_bindgen_b20e084f7b7941b5_base

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
                       hs_bindgen_b20e084f7b7941b5 arg10 arg22)

-- __unique:__ @test_macrosreparse_Example_Safe_args_union@
foreign import ccall safe "hs_bindgen_23aff33f33b6bdd1" hs_bindgen_23aff33f33b6bdd1_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_union@
hs_bindgen_23aff33f33b6bdd1 ::
     A
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_23aff33f33b6bdd1 =
  BG.fromFFIType hs_bindgen_23aff33f33b6bdd1_base

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
                       hs_bindgen_23aff33f33b6bdd1 arg10 arg22)

-- __unique:__ @test_macrosreparse_Example_Safe_args_enum@
foreign import ccall safe "hs_bindgen_fdd58ae14ce15ed5" hs_bindgen_fdd58ae14ce15ed5_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_enum@
hs_bindgen_fdd58ae14ce15ed5 ::
     A
  -> Some_enum
  -> IO ()
hs_bindgen_fdd58ae14ce15ed5 =
  BG.fromFFIType hs_bindgen_fdd58ae14ce15ed5_base

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
args_enum = hs_bindgen_fdd58ae14ce15ed5

-- __unique:__ @test_macrosreparse_Example_Safe_args_pointer1@
foreign import ccall safe "hs_bindgen_42ce2ec4fd2eda72" hs_bindgen_42ce2ec4fd2eda72_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_pointer1@
hs_bindgen_42ce2ec4fd2eda72 ::
     A
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_42ce2ec4fd2eda72 =
  BG.fromFFIType hs_bindgen_42ce2ec4fd2eda72_base

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
args_pointer1 = hs_bindgen_42ce2ec4fd2eda72

-- __unique:__ @test_macrosreparse_Example_Safe_args_pointer2@
foreign import ccall safe "hs_bindgen_01b2f6502d340abe" hs_bindgen_01b2f6502d340abe_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_pointer2@
hs_bindgen_01b2f6502d340abe ::
     A
  -> BG.Ptr (BG.Ptr BG.CInt)
  -> IO ()
hs_bindgen_01b2f6502d340abe =
  BG.fromFFIType hs_bindgen_01b2f6502d340abe_base

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
args_pointer2 = hs_bindgen_01b2f6502d340abe

-- __unique:__ @test_macrosreparse_Example_Safe_args_pointer3@
foreign import ccall safe "hs_bindgen_3e64133f9aaebbf1" hs_bindgen_3e64133f9aaebbf1_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_pointer3@
hs_bindgen_3e64133f9aaebbf1 ::
     A
  -> BG.Ptr BG.Void
  -> IO ()
hs_bindgen_3e64133f9aaebbf1 =
  BG.fromFFIType hs_bindgen_3e64133f9aaebbf1_base

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
args_pointer3 = hs_bindgen_3e64133f9aaebbf1

-- __unique:__ @test_macrosreparse_Example_Safe_ret_A@
foreign import ccall safe "hs_bindgen_c830401b459192fb" hs_bindgen_c830401b459192fb_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_Example_Safe_ret_A@
hs_bindgen_c830401b459192fb :: IO A
hs_bindgen_c830401b459192fb =
  BG.fromFFIType hs_bindgen_c830401b459192fb_base

{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h 47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A :: IO A
ret_A = hs_bindgen_c830401b459192fb

-- __unique:__ @test_macrosreparse_Example_Safe_ret_char1@
foreign import ccall safe "hs_bindgen_18b24c6e67a5412e" hs_bindgen_18b24c6e67a5412e_base ::
     BG.Int32
  -> IO BG.Int8

-- __unique:__ @test_macrosreparse_Example_Safe_ret_char1@
hs_bindgen_18b24c6e67a5412e ::
     A
  -> IO BG.CChar
hs_bindgen_18b24c6e67a5412e =
  BG.fromFFIType hs_bindgen_18b24c6e67a5412e_base

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h 49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CChar
ret_char1 = hs_bindgen_18b24c6e67a5412e

-- __unique:__ @test_macrosreparse_Example_Safe_ret_char2@
foreign import ccall safe "hs_bindgen_2da1160aeef9ff64" hs_bindgen_2da1160aeef9ff64_base ::
     BG.Int32
  -> IO BG.Int8

-- __unique:__ @test_macrosreparse_Example_Safe_ret_char2@
hs_bindgen_2da1160aeef9ff64 ::
     A
  -> IO BG.CSChar
hs_bindgen_2da1160aeef9ff64 =
  BG.fromFFIType hs_bindgen_2da1160aeef9ff64_base

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h 50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CSChar
ret_char2 = hs_bindgen_2da1160aeef9ff64

-- __unique:__ @test_macrosreparse_Example_Safe_ret_char3@
foreign import ccall safe "hs_bindgen_e3183f9de1b9f231" hs_bindgen_e3183f9de1b9f231_base ::
     BG.Int32
  -> IO BG.Word8

-- __unique:__ @test_macrosreparse_Example_Safe_ret_char3@
hs_bindgen_e3183f9de1b9f231 ::
     A
  -> IO BG.CUChar
hs_bindgen_e3183f9de1b9f231 =
  BG.fromFFIType hs_bindgen_e3183f9de1b9f231_base

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h 51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUChar
ret_char3 = hs_bindgen_e3183f9de1b9f231

-- __unique:__ @test_macrosreparse_Example_Safe_ret_short1@
foreign import ccall safe "hs_bindgen_c313966d4478e3f4" hs_bindgen_c313966d4478e3f4_base ::
     BG.Int32
  -> IO BG.Int16

-- __unique:__ @test_macrosreparse_Example_Safe_ret_short1@
hs_bindgen_c313966d4478e3f4 ::
     A
  -> IO BG.CShort
hs_bindgen_c313966d4478e3f4 =
  BG.fromFFIType hs_bindgen_c313966d4478e3f4_base

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h 53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short1 = hs_bindgen_c313966d4478e3f4

-- __unique:__ @test_macrosreparse_Example_Safe_ret_short2@
foreign import ccall safe "hs_bindgen_737fbec310eb0719" hs_bindgen_737fbec310eb0719_base ::
     BG.Int32
  -> IO BG.Int16

-- __unique:__ @test_macrosreparse_Example_Safe_ret_short2@
hs_bindgen_737fbec310eb0719 ::
     A
  -> IO BG.CShort
hs_bindgen_737fbec310eb0719 =
  BG.fromFFIType hs_bindgen_737fbec310eb0719_base

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h 54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short2 = hs_bindgen_737fbec310eb0719

-- __unique:__ @test_macrosreparse_Example_Safe_ret_short3@
foreign import ccall safe "hs_bindgen_b5bd9e111020db4e" hs_bindgen_b5bd9e111020db4e_base ::
     BG.Int32
  -> IO BG.Word16

-- __unique:__ @test_macrosreparse_Example_Safe_ret_short3@
hs_bindgen_b5bd9e111020db4e ::
     A
  -> IO BG.CUShort
hs_bindgen_b5bd9e111020db4e =
  BG.fromFFIType hs_bindgen_b5bd9e111020db4e_base

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h 55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUShort
ret_short3 = hs_bindgen_b5bd9e111020db4e

-- __unique:__ @test_macrosreparse_Example_Safe_ret_int1@
foreign import ccall safe "hs_bindgen_a30224259287f5f8" hs_bindgen_a30224259287f5f8_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparse_Example_Safe_ret_int1@
hs_bindgen_a30224259287f5f8 ::
     A
  -> IO BG.CInt
hs_bindgen_a30224259287f5f8 =
  BG.fromFFIType hs_bindgen_a30224259287f5f8_base

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h 57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int1 = hs_bindgen_a30224259287f5f8

-- __unique:__ @test_macrosreparse_Example_Safe_ret_int2@
foreign import ccall safe "hs_bindgen_b5be09caf8cf5750" hs_bindgen_b5be09caf8cf5750_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparse_Example_Safe_ret_int2@
hs_bindgen_b5be09caf8cf5750 ::
     A
  -> IO BG.CInt
hs_bindgen_b5be09caf8cf5750 =
  BG.fromFFIType hs_bindgen_b5be09caf8cf5750_base

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h 58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int2 = hs_bindgen_b5be09caf8cf5750

-- __unique:__ @test_macrosreparse_Example_Safe_ret_int3@
foreign import ccall safe "hs_bindgen_698e3f97470d83be" hs_bindgen_698e3f97470d83be_base ::
     BG.Int32
  -> IO BG.Word32

-- __unique:__ @test_macrosreparse_Example_Safe_ret_int3@
hs_bindgen_698e3f97470d83be ::
     A
  -> IO BG.CUInt
hs_bindgen_698e3f97470d83be =
  BG.fromFFIType hs_bindgen_698e3f97470d83be_base

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h 59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUInt
ret_int3 = hs_bindgen_698e3f97470d83be

-- __unique:__ @test_macrosreparse_Example_Safe_ret_long1@
foreign import ccall safe "hs_bindgen_c7e0705dd09be530" hs_bindgen_c7e0705dd09be530_base ::
     BG.Int32
  -> IO BG.Int64

-- __unique:__ @test_macrosreparse_Example_Safe_ret_long1@
hs_bindgen_c7e0705dd09be530 ::
     A
  -> IO BG.CLong
hs_bindgen_c7e0705dd09be530 =
  BG.fromFFIType hs_bindgen_c7e0705dd09be530_base

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h 61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long1 = hs_bindgen_c7e0705dd09be530

-- __unique:__ @test_macrosreparse_Example_Safe_ret_long2@
foreign import ccall safe "hs_bindgen_74b1f5b8c56ff22c" hs_bindgen_74b1f5b8c56ff22c_base ::
     BG.Int32
  -> IO BG.Int64

-- __unique:__ @test_macrosreparse_Example_Safe_ret_long2@
hs_bindgen_74b1f5b8c56ff22c ::
     A
  -> IO BG.CLong
hs_bindgen_74b1f5b8c56ff22c =
  BG.fromFFIType hs_bindgen_74b1f5b8c56ff22c_base

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h 62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long2 = hs_bindgen_74b1f5b8c56ff22c

-- __unique:__ @test_macrosreparse_Example_Safe_ret_long3@
foreign import ccall safe "hs_bindgen_c2d07eaaab82d408" hs_bindgen_c2d07eaaab82d408_base ::
     BG.Int32
  -> IO BG.Word64

-- __unique:__ @test_macrosreparse_Example_Safe_ret_long3@
hs_bindgen_c2d07eaaab82d408 ::
     A
  -> IO BG.CULong
hs_bindgen_c2d07eaaab82d408 =
  BG.fromFFIType hs_bindgen_c2d07eaaab82d408_base

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h 63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CULong
ret_long3 = hs_bindgen_c2d07eaaab82d408

-- __unique:__ @test_macrosreparse_Example_Safe_ret_float@
foreign import ccall safe "hs_bindgen_0edfbc7067faa1f7" hs_bindgen_0edfbc7067faa1f7_base ::
     BG.Int32
  -> IO Float

-- __unique:__ @test_macrosreparse_Example_Safe_ret_float@
hs_bindgen_0edfbc7067faa1f7 ::
     A
  -> IO BG.CFloat
hs_bindgen_0edfbc7067faa1f7 =
  BG.fromFFIType hs_bindgen_0edfbc7067faa1f7_base

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h 65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CFloat
ret_float = hs_bindgen_0edfbc7067faa1f7

-- __unique:__ @test_macrosreparse_Example_Safe_ret_double@
foreign import ccall safe "hs_bindgen_786ca672396b33be" hs_bindgen_786ca672396b33be_base ::
     BG.Int32
  -> IO Double

-- __unique:__ @test_macrosreparse_Example_Safe_ret_double@
hs_bindgen_786ca672396b33be ::
     A
  -> IO BG.CDouble
hs_bindgen_786ca672396b33be =
  BG.fromFFIType hs_bindgen_786ca672396b33be_base

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h 66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CDouble
ret_double = hs_bindgen_786ca672396b33be

-- __unique:__ @test_macrosreparse_Example_Safe_ret_bool1@
foreign import ccall safe "hs_bindgen_2e99f19b59650996" hs_bindgen_2e99f19b59650996_base ::
     BG.Int32
  -> IO BG.Word8

-- __unique:__ @test_macrosreparse_Example_Safe_ret_bool1@
hs_bindgen_2e99f19b59650996 ::
     A
  -> IO BG.CBool
hs_bindgen_2e99f19b59650996 =
  BG.fromFFIType hs_bindgen_2e99f19b59650996_base

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h 67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
ret_bool1 = hs_bindgen_2e99f19b59650996

-- __unique:__ @test_macrosreparse_Example_Safe_ret_struct@
foreign import ccall safe "hs_bindgen_6c999121eed8178f" hs_bindgen_6c999121eed8178f_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_ret_struct@
hs_bindgen_6c999121eed8178f ::
     A
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_6c999121eed8178f =
  BG.fromFFIType hs_bindgen_6c999121eed8178f_base

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
                        hs_bindgen_6c999121eed8178f arg10 res1)

-- __unique:__ @test_macrosreparse_Example_Safe_ret_union@
foreign import ccall safe "hs_bindgen_481ee5d2d9bd34db" hs_bindgen_481ee5d2d9bd34db_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_ret_union@
hs_bindgen_481ee5d2d9bd34db ::
     A
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_481ee5d2d9bd34db =
  BG.fromFFIType hs_bindgen_481ee5d2d9bd34db_base

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
                        hs_bindgen_481ee5d2d9bd34db arg10 res1)

-- __unique:__ @test_macrosreparse_Example_Safe_ret_enum@
foreign import ccall safe "hs_bindgen_8bb240ba453b700d" hs_bindgen_8bb240ba453b700d_base ::
     BG.Int32
  -> IO BG.Word32

-- __unique:__ @test_macrosreparse_Example_Safe_ret_enum@
hs_bindgen_8bb240ba453b700d ::
     A
  -> IO Some_enum
hs_bindgen_8bb240ba453b700d =
  BG.fromFFIType hs_bindgen_8bb240ba453b700d_base

{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h 71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO Some_enum
ret_enum = hs_bindgen_8bb240ba453b700d

-- __unique:__ @test_macrosreparse_Example_Safe_ret_pointer1@
foreign import ccall safe "hs_bindgen_c346ed2cd20b9af1" hs_bindgen_c346ed2cd20b9af1_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_Example_Safe_ret_pointer1@
hs_bindgen_c346ed2cd20b9af1 ::
     A
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_c346ed2cd20b9af1 =
  BG.fromFFIType hs_bindgen_c346ed2cd20b9af1_base

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h 73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
ret_pointer1 = hs_bindgen_c346ed2cd20b9af1

-- __unique:__ @test_macrosreparse_Example_Safe_ret_pointer2@
foreign import ccall safe "hs_bindgen_a21f618658151728" hs_bindgen_a21f618658151728_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_Example_Safe_ret_pointer2@
hs_bindgen_a21f618658151728 ::
     A
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
hs_bindgen_a21f618658151728 =
  BG.fromFFIType hs_bindgen_a21f618658151728_base

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h 74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
ret_pointer2 = hs_bindgen_a21f618658151728

-- __unique:__ @test_macrosreparse_Example_Safe_ret_pointer3@
foreign import ccall safe "hs_bindgen_2d8c6e2d2f395342" hs_bindgen_2d8c6e2d2f395342_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_Example_Safe_ret_pointer3@
hs_bindgen_2d8c6e2d2f395342 ::
     A
  -> IO (BG.Ptr BG.Void)
hs_bindgen_2d8c6e2d2f395342 =
  BG.fromFFIType hs_bindgen_2d8c6e2d2f395342_base

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h 75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.Void)
ret_pointer3 = hs_bindgen_2d8c6e2d2f395342

-- __unique:__ @test_macrosreparse_Example_Safe_body1@
foreign import ccall safe "hs_bindgen_b030d02030ed80bc" hs_bindgen_b030d02030ed80bc_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparse_Example_Safe_body1@
hs_bindgen_b030d02030ed80bc ::
     A
  -> IO BG.CInt
hs_bindgen_b030d02030ed80bc =
  BG.fromFFIType hs_bindgen_b030d02030ed80bc_base

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h 79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
body1 = hs_bindgen_b030d02030ed80bc

-- __unique:__ @test_macrosreparse_Example_Safe_body2@
foreign import ccall safe "hs_bindgen_be50427e6a63df54" hs_bindgen_be50427e6a63df54_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_Example_Safe_body2@
hs_bindgen_be50427e6a63df54 :: IO A
hs_bindgen_be50427e6a63df54 =
  BG.fromFFIType hs_bindgen_be50427e6a63df54_base

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h 80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2 :: IO A
body2 = hs_bindgen_be50427e6a63df54

-- __unique:__ @test_macrosreparse_Example_Safe_args_complex_float@
foreign import ccall safe "hs_bindgen_627a52a5c7617083" hs_bindgen_627a52a5c7617083_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_complex_float@
hs_bindgen_627a52a5c7617083 ::
     A
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_627a52a5c7617083 =
  BG.fromFFIType hs_bindgen_627a52a5c7617083_base

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
                       hs_bindgen_627a52a5c7617083 arg10 arg22)

-- __unique:__ @test_macrosreparse_Example_Safe_args_complex_double@
foreign import ccall safe "hs_bindgen_392babebc1d83503" hs_bindgen_392babebc1d83503_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_args_complex_double@
hs_bindgen_392babebc1d83503 ::
     A
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_392babebc1d83503 =
  BG.fromFFIType hs_bindgen_392babebc1d83503_base

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
                       hs_bindgen_392babebc1d83503 arg10 arg22)

-- __unique:__ @test_macrosreparse_Example_Safe_ret_complex_float@
foreign import ccall safe "hs_bindgen_8958183ede73dea8" hs_bindgen_8958183ede73dea8_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_ret_complex_float@
hs_bindgen_8958183ede73dea8 ::
     A
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_8958183ede73dea8 =
  BG.fromFFIType hs_bindgen_8958183ede73dea8_base

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
                        hs_bindgen_8958183ede73dea8 arg10 res1)

-- __unique:__ @test_macrosreparse_Example_Safe_ret_complex_double@
foreign import ccall safe "hs_bindgen_a95fabfd391a99aa" hs_bindgen_a95fabfd391a99aa_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_ret_complex_double@
hs_bindgen_a95fabfd391a99aa ::
     A
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_a95fabfd391a99aa =
  BG.fromFFIType hs_bindgen_a95fabfd391a99aa_base

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
                        hs_bindgen_a95fabfd391a99aa arg10 res1)

-- __unique:__ @test_macrosreparse_Example_Safe_bespoke_args1@
foreign import ccall safe "hs_bindgen_ad9f8630dd04a203" hs_bindgen_ad9f8630dd04a203_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_bespoke_args1@
hs_bindgen_ad9f8630dd04a203 ::
     A
  -> BG.CBool
  -> IO ()
hs_bindgen_ad9f8630dd04a203 =
  BG.fromFFIType hs_bindgen_ad9f8630dd04a203_base

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
bespoke_args1 = hs_bindgen_ad9f8630dd04a203

-- __unique:__ @test_macrosreparse_Example_Safe_bespoke_args2@
foreign import ccall safe "hs_bindgen_4b34178a505131e2" hs_bindgen_4b34178a505131e2_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_bespoke_args2@
hs_bindgen_4b34178a505131e2 ::
     A
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_4b34178a505131e2 =
  BG.fromFFIType hs_bindgen_4b34178a505131e2_base

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
bespoke_args2 = hs_bindgen_4b34178a505131e2

-- __unique:__ @test_macrosreparse_Example_Safe_bespoke_ret1@
foreign import ccall safe "hs_bindgen_94b225a6394496c1" hs_bindgen_94b225a6394496c1_base ::
     BG.Int32
  -> IO BG.Word8

-- __unique:__ @test_macrosreparse_Example_Safe_bespoke_ret1@
hs_bindgen_94b225a6394496c1 ::
     A
  -> IO BG.CBool
hs_bindgen_94b225a6394496c1 =
  BG.fromFFIType hs_bindgen_94b225a6394496c1_base

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h 97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
bespoke_ret1 = hs_bindgen_94b225a6394496c1

-- __unique:__ @test_macrosreparse_Example_Safe_bespoke_ret2@
foreign import ccall safe "hs_bindgen_7c9a1792426b84a1" hs_bindgen_7c9a1792426b84a1_base ::
     BG.Int32
  -> IO BG.Word64

-- __unique:__ @test_macrosreparse_Example_Safe_bespoke_ret2@
hs_bindgen_7c9a1792426b84a1 ::
     A
  -> IO HsBindgen.Runtime.LibC.CSize
hs_bindgen_7c9a1792426b84a1 =
  BG.fromFFIType hs_bindgen_7c9a1792426b84a1_base

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h 98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO HsBindgen.Runtime.LibC.CSize
bespoke_ret2 = hs_bindgen_7c9a1792426b84a1

-- __unique:__ @test_macrosreparse_Example_Safe_arr_args1@
foreign import ccall safe "hs_bindgen_e20689fe39004225" hs_bindgen_e20689fe39004225_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_arr_args1@
hs_bindgen_e20689fe39004225 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray A))
  -> IO ()
hs_bindgen_e20689fe39004225 =
  BG.fromFFIType hs_bindgen_e20689fe39004225_base

{-| Arrays

    __C declaration:__ @arr_args1@

    __defined at:__ @macros\/reparse.h 104:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args1 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray A))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args1 = hs_bindgen_e20689fe39004225

-- __unique:__ @test_macrosreparse_Example_Safe_arr_args2@
foreign import ccall safe "hs_bindgen_084796e4bfd3f4cd" hs_bindgen_084796e4bfd3f4cd_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_arr_args2@
hs_bindgen_084796e4bfd3f4cd ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr A)))
  -> IO ()
hs_bindgen_084796e4bfd3f4cd =
  BG.fromFFIType hs_bindgen_084796e4bfd3f4cd_base

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h 105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args2 = hs_bindgen_084796e4bfd3f4cd

-- __unique:__ @test_macrosreparse_Example_Safe_arr_args3@
foreign import ccall safe "hs_bindgen_a3d1560aaa4352df" hs_bindgen_a3d1560aaa4352df_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_arr_args3@
hs_bindgen_a3d1560aaa4352df ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 A))
  -> IO ()
hs_bindgen_a3d1560aaa4352df =
  BG.fromFFIType hs_bindgen_a3d1560aaa4352df_base

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h 106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 A))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args3 = hs_bindgen_a3d1560aaa4352df

-- __unique:__ @test_macrosreparse_Example_Safe_arr_args4@
foreign import ccall safe "hs_bindgen_88659ccccc6c1f5f" hs_bindgen_88659ccccc6c1f5f_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_arr_args4@
hs_bindgen_88659ccccc6c1f5f ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr A)))
  -> IO ()
hs_bindgen_88659ccccc6c1f5f =
  BG.fromFFIType hs_bindgen_88659ccccc6c1f5f_base

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h 107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args4 = hs_bindgen_88659ccccc6c1f5f

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_args1@
foreign import ccall safe "hs_bindgen_3448d03cfd41161a" hs_bindgen_3448d03cfd41161a_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_args1@
hs_bindgen_3448d03cfd41161a ::
     A
  -> BG.FunPtr (IO ())
  -> IO ()
hs_bindgen_3448d03cfd41161a =
  BG.fromFFIType hs_bindgen_3448d03cfd41161a_base

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
funptr_args1 = hs_bindgen_3448d03cfd41161a

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_args2@
foreign import ccall safe "hs_bindgen_92d7386f0a327d25" hs_bindgen_92d7386f0a327d25_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_args2@
hs_bindgen_92d7386f0a327d25 ::
     A
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_92d7386f0a327d25 =
  BG.fromFFIType hs_bindgen_92d7386f0a327d25_base

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
funptr_args2 = hs_bindgen_92d7386f0a327d25

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_args3@
foreign import ccall safe "hs_bindgen_2881f594f98043e6" hs_bindgen_2881f594f98043e6_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_args3@
hs_bindgen_2881f594f98043e6 ::
     A
  -> BG.FunPtr (BG.CInt -> IO ())
  -> IO ()
hs_bindgen_2881f594f98043e6 =
  BG.fromFFIType hs_bindgen_2881f594f98043e6_base

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
funptr_args3 = hs_bindgen_2881f594f98043e6

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_args4@
foreign import ccall safe "hs_bindgen_1e85a05df4251f62" hs_bindgen_1e85a05df4251f62_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_args4@
hs_bindgen_1e85a05df4251f62 ::
     A
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar)
  -> IO ()
hs_bindgen_1e85a05df4251f62 =
  BG.fromFFIType hs_bindgen_1e85a05df4251f62_base

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
funptr_args4 = hs_bindgen_1e85a05df4251f62

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_args5@
foreign import ccall safe "hs_bindgen_ccf4db7511f0d6d6" hs_bindgen_ccf4db7511f0d6d6_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_args5@
hs_bindgen_ccf4db7511f0d6d6 ::
     A
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))
  -> IO ()
hs_bindgen_ccf4db7511f0d6d6 =
  BG.fromFFIType hs_bindgen_ccf4db7511f0d6d6_base

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
funptr_args5 = hs_bindgen_ccf4db7511f0d6d6

-- __unique:__ @test_macrosreparse_Example_Safe_comments1@
foreign import ccall safe "hs_bindgen_4c756db60673d221" hs_bindgen_4c756db60673d221_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_comments1@
hs_bindgen_4c756db60673d221 ::
     A
  -> IO ()
hs_bindgen_4c756db60673d221 =
  BG.fromFFIType hs_bindgen_4c756db60673d221_base

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
comments1 = hs_bindgen_4c756db60673d221

-- __unique:__ @test_macrosreparse_Example_Safe_const_prim_before1@
foreign import ccall safe "hs_bindgen_278568d7a2a3a4b6" hs_bindgen_278568d7a2a3a4b6_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_prim_before1@
hs_bindgen_278568d7a2a3a4b6 ::
     A
  -> BG.CChar
  -> IO ()
hs_bindgen_278568d7a2a3a4b6 =
  BG.fromFFIType hs_bindgen_278568d7a2a3a4b6_base

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
const_prim_before1 = hs_bindgen_278568d7a2a3a4b6

-- __unique:__ @test_macrosreparse_Example_Safe_const_prim_before2@
foreign import ccall safe "hs_bindgen_87ee56525e5ea20c" hs_bindgen_87ee56525e5ea20c_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_prim_before2@
hs_bindgen_87ee56525e5ea20c ::
     A
  -> BG.CSChar
  -> IO ()
hs_bindgen_87ee56525e5ea20c =
  BG.fromFFIType hs_bindgen_87ee56525e5ea20c_base

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
const_prim_before2 = hs_bindgen_87ee56525e5ea20c

-- __unique:__ @test_macrosreparse_Example_Safe_const_prim_before3@
foreign import ccall safe "hs_bindgen_c0b99594235bd99e" hs_bindgen_c0b99594235bd99e_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_prim_before3@
hs_bindgen_c0b99594235bd99e ::
     A
  -> BG.CUChar
  -> IO ()
hs_bindgen_c0b99594235bd99e =
  BG.fromFFIType hs_bindgen_c0b99594235bd99e_base

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
const_prim_before3 = hs_bindgen_c0b99594235bd99e

-- __unique:__ @test_macrosreparse_Example_Safe_const_prim_after1@
foreign import ccall safe "hs_bindgen_d718b682f157fc18" hs_bindgen_d718b682f157fc18_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_prim_after1@
hs_bindgen_d718b682f157fc18 ::
     A
  -> BG.CChar
  -> IO ()
hs_bindgen_d718b682f157fc18 =
  BG.fromFFIType hs_bindgen_d718b682f157fc18_base

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
const_prim_after1 = hs_bindgen_d718b682f157fc18

-- __unique:__ @test_macrosreparse_Example_Safe_const_prim_after2@
foreign import ccall safe "hs_bindgen_f2c5b3d5eca68433" hs_bindgen_f2c5b3d5eca68433_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_prim_after2@
hs_bindgen_f2c5b3d5eca68433 ::
     A
  -> BG.CSChar
  -> IO ()
hs_bindgen_f2c5b3d5eca68433 =
  BG.fromFFIType hs_bindgen_f2c5b3d5eca68433_base

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
const_prim_after2 = hs_bindgen_f2c5b3d5eca68433

-- __unique:__ @test_macrosreparse_Example_Safe_const_prim_after3@
foreign import ccall safe "hs_bindgen_ae2d994e06667b23" hs_bindgen_ae2d994e06667b23_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_prim_after3@
hs_bindgen_ae2d994e06667b23 ::
     A
  -> BG.CUChar
  -> IO ()
hs_bindgen_ae2d994e06667b23 =
  BG.fromFFIType hs_bindgen_ae2d994e06667b23_base

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
const_prim_after3 = hs_bindgen_ae2d994e06667b23

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before1@
foreign import ccall safe "hs_bindgen_6940b58e7f4397a7" hs_bindgen_6940b58e7f4397a7_base ::
     BG.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before1@
hs_bindgen_6940b58e7f4397a7 ::
     A
  -> BG.CFloat
  -> IO ()
hs_bindgen_6940b58e7f4397a7 =
  BG.fromFFIType hs_bindgen_6940b58e7f4397a7_base

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
  hs_bindgen_6940b58e7f4397a7

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before2@
foreign import ccall safe "hs_bindgen_00b6fe2282e779b1" hs_bindgen_00b6fe2282e779b1_base ::
     BG.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before2@
hs_bindgen_00b6fe2282e779b1 ::
     A
  -> BG.CDouble
  -> IO ()
hs_bindgen_00b6fe2282e779b1 =
  BG.fromFFIType hs_bindgen_00b6fe2282e779b1_base

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
  hs_bindgen_00b6fe2282e779b1

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before3@
foreign import ccall safe "hs_bindgen_6517cc8d39aead93" hs_bindgen_6517cc8d39aead93_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before3@
hs_bindgen_6517cc8d39aead93 ::
     A
  -> BG.CBool
  -> IO ()
hs_bindgen_6517cc8d39aead93 =
  BG.fromFFIType hs_bindgen_6517cc8d39aead93_base

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
  hs_bindgen_6517cc8d39aead93

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before4@
foreign import ccall safe "hs_bindgen_68c7661e95060488" hs_bindgen_68c7661e95060488_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before4@
hs_bindgen_68c7661e95060488 ::
     A
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_68c7661e95060488 =
  BG.fromFFIType hs_bindgen_68c7661e95060488_base

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
                       hs_bindgen_68c7661e95060488 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before5@
foreign import ccall safe "hs_bindgen_42b3b0bf73a7a51a" hs_bindgen_42b3b0bf73a7a51a_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before5@
hs_bindgen_42b3b0bf73a7a51a ::
     A
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_42b3b0bf73a7a51a =
  BG.fromFFIType hs_bindgen_42b3b0bf73a7a51a_base

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
                       hs_bindgen_42b3b0bf73a7a51a arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before6@
foreign import ccall safe "hs_bindgen_c4aabe9834aac12f" hs_bindgen_c4aabe9834aac12f_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before6@
hs_bindgen_c4aabe9834aac12f ::
     A
  -> Some_enum
  -> IO ()
hs_bindgen_c4aabe9834aac12f =
  BG.fromFFIType hs_bindgen_c4aabe9834aac12f_base

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
  hs_bindgen_c4aabe9834aac12f

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before7@
foreign import ccall safe "hs_bindgen_486090a7fb4e34d4" hs_bindgen_486090a7fb4e34d4_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before7@
hs_bindgen_486090a7fb4e34d4 ::
     A
  -> BG.CBool
  -> IO ()
hs_bindgen_486090a7fb4e34d4 =
  BG.fromFFIType hs_bindgen_486090a7fb4e34d4_base

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
  hs_bindgen_486090a7fb4e34d4

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before8@
foreign import ccall safe "hs_bindgen_23fa742b614176dd" hs_bindgen_23fa742b614176dd_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before8@
hs_bindgen_23fa742b614176dd ::
     A
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_23fa742b614176dd =
  BG.fromFFIType hs_bindgen_23fa742b614176dd_base

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
  hs_bindgen_23fa742b614176dd

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after1@
foreign import ccall safe "hs_bindgen_0aacd8a5d48f296d" hs_bindgen_0aacd8a5d48f296d_base ::
     BG.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after1@
hs_bindgen_0aacd8a5d48f296d ::
     A
  -> BG.CFloat
  -> IO ()
hs_bindgen_0aacd8a5d48f296d =
  BG.fromFFIType hs_bindgen_0aacd8a5d48f296d_base

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
  hs_bindgen_0aacd8a5d48f296d

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after2@
foreign import ccall safe "hs_bindgen_16ec2102221485b7" hs_bindgen_16ec2102221485b7_base ::
     BG.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after2@
hs_bindgen_16ec2102221485b7 ::
     A
  -> BG.CDouble
  -> IO ()
hs_bindgen_16ec2102221485b7 =
  BG.fromFFIType hs_bindgen_16ec2102221485b7_base

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
  hs_bindgen_16ec2102221485b7

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after3@
foreign import ccall safe "hs_bindgen_9aa934d44ec3790c" hs_bindgen_9aa934d44ec3790c_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after3@
hs_bindgen_9aa934d44ec3790c ::
     A
  -> BG.CBool
  -> IO ()
hs_bindgen_9aa934d44ec3790c =
  BG.fromFFIType hs_bindgen_9aa934d44ec3790c_base

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
  hs_bindgen_9aa934d44ec3790c

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after4@
foreign import ccall safe "hs_bindgen_043d2869e29bedcf" hs_bindgen_043d2869e29bedcf_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after4@
hs_bindgen_043d2869e29bedcf ::
     A
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_043d2869e29bedcf =
  BG.fromFFIType hs_bindgen_043d2869e29bedcf_base

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
                       hs_bindgen_043d2869e29bedcf arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after5@
foreign import ccall safe "hs_bindgen_b5f9bca1de9d69de" hs_bindgen_b5f9bca1de9d69de_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after5@
hs_bindgen_b5f9bca1de9d69de ::
     A
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_b5f9bca1de9d69de =
  BG.fromFFIType hs_bindgen_b5f9bca1de9d69de_base

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
                       hs_bindgen_b5f9bca1de9d69de arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after6@
foreign import ccall safe "hs_bindgen_77d641d518b2504f" hs_bindgen_77d641d518b2504f_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after6@
hs_bindgen_77d641d518b2504f ::
     A
  -> Some_enum
  -> IO ()
hs_bindgen_77d641d518b2504f =
  BG.fromFFIType hs_bindgen_77d641d518b2504f_base

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
  hs_bindgen_77d641d518b2504f

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after7@
foreign import ccall safe "hs_bindgen_691b4f2909140b49" hs_bindgen_691b4f2909140b49_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after7@
hs_bindgen_691b4f2909140b49 ::
     A
  -> BG.CBool
  -> IO ()
hs_bindgen_691b4f2909140b49 =
  BG.fromFFIType hs_bindgen_691b4f2909140b49_base

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
  hs_bindgen_691b4f2909140b49

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after8@
foreign import ccall safe "hs_bindgen_ae74c8dcdc2ec9eb" hs_bindgen_ae74c8dcdc2ec9eb_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after8@
hs_bindgen_ae74c8dcdc2ec9eb ::
     A
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_ae74c8dcdc2ec9eb =
  BG.fromFFIType hs_bindgen_ae74c8dcdc2ec9eb_base

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
  hs_bindgen_ae74c8dcdc2ec9eb

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_args1@
foreign import ccall safe "hs_bindgen_07606c41eadf9146" hs_bindgen_07606c41eadf9146_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_args1@
hs_bindgen_07606c41eadf9146 ::
     A
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_07606c41eadf9146 =
  BG.fromFFIType hs_bindgen_07606c41eadf9146_base

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
const_pointers_args1 = hs_bindgen_07606c41eadf9146

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_args2@
foreign import ccall safe "hs_bindgen_3836769f3a3416ac" hs_bindgen_3836769f3a3416ac_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_args2@
hs_bindgen_3836769f3a3416ac ::
     A
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_3836769f3a3416ac =
  BG.fromFFIType hs_bindgen_3836769f3a3416ac_base

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
const_pointers_args2 = hs_bindgen_3836769f3a3416ac

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_args3@
foreign import ccall safe "hs_bindgen_12f19ea593aefd3f" hs_bindgen_12f19ea593aefd3f_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_args3@
hs_bindgen_12f19ea593aefd3f ::
     A
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_12f19ea593aefd3f =
  BG.fromFFIType hs_bindgen_12f19ea593aefd3f_base

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
const_pointers_args3 = hs_bindgen_12f19ea593aefd3f

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_args4@
foreign import ccall safe "hs_bindgen_5a50e98897696d57" hs_bindgen_5a50e98897696d57_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_args4@
hs_bindgen_5a50e98897696d57 ::
     A
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_5a50e98897696d57 =
  BG.fromFFIType hs_bindgen_5a50e98897696d57_base

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
const_pointers_args4 = hs_bindgen_5a50e98897696d57

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_args5@
foreign import ccall safe "hs_bindgen_666701f7cb61bd15" hs_bindgen_666701f7cb61bd15_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_args5@
hs_bindgen_666701f7cb61bd15 ::
     A
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_666701f7cb61bd15 =
  BG.fromFFIType hs_bindgen_666701f7cb61bd15_base

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
const_pointers_args5 = hs_bindgen_666701f7cb61bd15

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_ret1@
foreign import ccall safe "hs_bindgen_b94fbc3dfd285563" hs_bindgen_b94fbc3dfd285563_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_ret1@
hs_bindgen_b94fbc3dfd285563 ::
     A
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_b94fbc3dfd285563 =
  BG.fromFFIType hs_bindgen_b94fbc3dfd285563_base

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h 212:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret1 = hs_bindgen_b94fbc3dfd285563

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_ret2@
foreign import ccall safe "hs_bindgen_33e2960e26b79450" hs_bindgen_33e2960e26b79450_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_ret2@
hs_bindgen_33e2960e26b79450 ::
     A
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_33e2960e26b79450 =
  BG.fromFFIType hs_bindgen_33e2960e26b79450_base

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h 213:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret2 = hs_bindgen_33e2960e26b79450

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_ret3@
foreign import ccall safe "hs_bindgen_50c6e2fe4f3fb777" hs_bindgen_50c6e2fe4f3fb777_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_ret3@
hs_bindgen_50c6e2fe4f3fb777 ::
     A
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_50c6e2fe4f3fb777 =
  BG.fromFFIType hs_bindgen_50c6e2fe4f3fb777_base

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h 214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
const_pointers_ret3 = hs_bindgen_50c6e2fe4f3fb777

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_ret4@
foreign import ccall safe "hs_bindgen_edc014695d896c8d" hs_bindgen_edc014695d896c8d_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_ret4@
hs_bindgen_edc014695d896c8d ::
     A
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_edc014695d896c8d =
  BG.fromFFIType hs_bindgen_edc014695d896c8d_base

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h 215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret4 = hs_bindgen_edc014695d896c8d

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_ret5@
foreign import ccall safe "hs_bindgen_6d3308cc5847f033" hs_bindgen_6d3308cc5847f033_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_Example_Safe_const_pointers_ret5@
hs_bindgen_6d3308cc5847f033 ::
     A
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_6d3308cc5847f033 =
  BG.fromFFIType hs_bindgen_6d3308cc5847f033_base

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h 216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret5 = hs_bindgen_6d3308cc5847f033

-- __unique:__ @test_macrosreparse_Example_Safe_const_array_elem1@
foreign import ccall safe "hs_bindgen_678576320923a4d1" hs_bindgen_678576320923a4d1_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_array_elem1@
hs_bindgen_678576320923a4d1 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray A))
  -> IO ()
hs_bindgen_678576320923a4d1 =
  BG.fromFFIType hs_bindgen_678576320923a4d1_base

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h 244:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray A))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem1 = hs_bindgen_678576320923a4d1

-- __unique:__ @test_macrosreparse_Example_Safe_const_array_elem2@
foreign import ccall safe "hs_bindgen_b317941dde4eeff2" hs_bindgen_b317941dde4eeff2_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_array_elem2@
hs_bindgen_b317941dde4eeff2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst A)))
  -> IO ()
hs_bindgen_b317941dde4eeff2 =
  BG.fromFFIType hs_bindgen_b317941dde4eeff2_base

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h 245:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem2 = hs_bindgen_b317941dde4eeff2

-- __unique:__ @test_macrosreparse_Example_Safe_const_array_elem3@
foreign import ccall safe "hs_bindgen_707e602e6beb1bb6" hs_bindgen_707e602e6beb1bb6_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_const_array_elem3@
hs_bindgen_707e602e6beb1bb6 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr A)))
  -> IO ()
hs_bindgen_707e602e6beb1bb6 =
  BG.fromFFIType hs_bindgen_707e602e6beb1bb6_base

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h 246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem3 = hs_bindgen_707e602e6beb1bb6

-- __unique:__ @test_macrosreparse_Example_Safe_noParams1@
foreign import ccall safe "hs_bindgen_93fecb4eb766c262" hs_bindgen_93fecb4eb766c262_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_Example_Safe_noParams1@
hs_bindgen_93fecb4eb766c262 :: IO A
hs_bindgen_93fecb4eb766c262 =
  BG.fromFFIType hs_bindgen_93fecb4eb766c262_base

{-| Other examples we reparsed /incorrectly/ before language-c

    __C declaration:__ @noParams1@

    __defined at:__ @macros\/reparse.h 254:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams1 :: IO A
noParams1 = hs_bindgen_93fecb4eb766c262

-- __unique:__ @test_macrosreparse_Example_Safe_noParams2@
foreign import ccall safe "hs_bindgen_4350965157c891f5" hs_bindgen_4350965157c891f5_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_Example_Safe_noParams2@
hs_bindgen_4350965157c891f5 :: IO A
hs_bindgen_4350965157c891f5 =
  BG.fromFFIType hs_bindgen_4350965157c891f5_base

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h 255:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2 :: IO A
noParams2 = hs_bindgen_4350965157c891f5

-- __unique:__ @test_macrosreparse_Example_Safe_noParams3@
foreign import ccall safe "hs_bindgen_c4f59272a2b1c3b5" hs_bindgen_c4f59272a2b1c3b5_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_Example_Safe_noParams3@
hs_bindgen_c4f59272a2b1c3b5 ::
     A
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_c4f59272a2b1c3b5 =
  BG.fromFFIType hs_bindgen_c4f59272a2b1c3b5_base

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
noParams3 = hs_bindgen_c4f59272a2b1c3b5

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret1@
foreign import ccall safe "hs_bindgen_387a04c01e23c320" hs_bindgen_387a04c01e23c320_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret1@
hs_bindgen_387a04c01e23c320 ::
     A
  -> IO (BG.FunPtr (IO ()))
hs_bindgen_387a04c01e23c320 =
  BG.fromFFIType hs_bindgen_387a04c01e23c320_base

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h 260:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO ()))
funptr_ret1 = hs_bindgen_387a04c01e23c320

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret2@
foreign import ccall safe "hs_bindgen_6f0c14cd3478dc19" hs_bindgen_6f0c14cd3478dc19_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret2@
hs_bindgen_6f0c14cd3478dc19 ::
     A
  -> IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_6f0c14cd3478dc19 =
  BG.fromFFIType hs_bindgen_6f0c14cd3478dc19_base

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h 261:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO BG.CInt))
funptr_ret2 = hs_bindgen_6f0c14cd3478dc19

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret3@
foreign import ccall safe "hs_bindgen_08e8661d277cf7be" hs_bindgen_08e8661d277cf7be_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret3@
hs_bindgen_08e8661d277cf7be ::
     A
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_08e8661d277cf7be =
  BG.fromFFIType hs_bindgen_08e8661d277cf7be_base

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h 262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
funptr_ret3 = hs_bindgen_08e8661d277cf7be

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret4@
foreign import ccall safe "hs_bindgen_609b5d953b68da92" hs_bindgen_609b5d953b68da92_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret4@
hs_bindgen_609b5d953b68da92 ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
hs_bindgen_609b5d953b68da92 =
  BG.fromFFIType hs_bindgen_609b5d953b68da92_base

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h 263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
funptr_ret4 = hs_bindgen_609b5d953b68da92

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret5@
foreign import ccall safe "hs_bindgen_13e6ae43abf40aee" hs_bindgen_13e6ae43abf40aee_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret5@
hs_bindgen_13e6ae43abf40aee ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_13e6ae43abf40aee =
  BG.fromFFIType hs_bindgen_13e6ae43abf40aee_base

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h 267:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret5 = hs_bindgen_13e6ae43abf40aee

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret6@
foreign import ccall safe "hs_bindgen_a4a3a86f28ca6299" hs_bindgen_a4a3a86f28ca6299_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret6@
hs_bindgen_a4a3a86f28ca6299 ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_a4a3a86f28ca6299 =
  BG.fromFFIType hs_bindgen_a4a3a86f28ca6299_base

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h 268:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret6 = hs_bindgen_a4a3a86f28ca6299

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret7@
foreign import ccall safe "hs_bindgen_eae9dff04c88d00b" hs_bindgen_eae9dff04c88d00b_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret7@
hs_bindgen_eae9dff04c88d00b ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_eae9dff04c88d00b =
  BG.fromFFIType hs_bindgen_eae9dff04c88d00b_base

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h 269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret7 = hs_bindgen_eae9dff04c88d00b

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret8@
foreign import ccall safe "hs_bindgen_894457d90a2fc8db" hs_bindgen_894457d90a2fc8db_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret8@
hs_bindgen_894457d90a2fc8db ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_894457d90a2fc8db =
  BG.fromFFIType hs_bindgen_894457d90a2fc8db_base

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h 270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret8 = hs_bindgen_894457d90a2fc8db

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret9@
foreign import ccall safe "hs_bindgen_c893eb15ad9bc68c" hs_bindgen_c893eb15ad9bc68c_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret9@
hs_bindgen_c893eb15ad9bc68c ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_c893eb15ad9bc68c =
  BG.fromFFIType hs_bindgen_c893eb15ad9bc68c_base

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h 271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret9 = hs_bindgen_c893eb15ad9bc68c

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret10@
foreign import ccall safe "hs_bindgen_d96c258298a44b28" hs_bindgen_d96c258298a44b28_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_Example_Safe_funptr_ret10@
hs_bindgen_d96c258298a44b28 ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_d96c258298a44b28 =
  BG.fromFFIType hs_bindgen_d96c258298a44b28_base

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h 272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret10 = hs_bindgen_d96c258298a44b28
