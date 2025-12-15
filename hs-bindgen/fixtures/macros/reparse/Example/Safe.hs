{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Data.Complex
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <macros/reparse.h>"
  , "void hs_bindgen_f15610128336b06a ("
  , "  A arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  args_char1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_087f45ca0a284a03 ("
  , "  A arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  args_char2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f6cb5c5a728c2404 ("
  , "  A arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  args_char3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d485767e0caa1f7c ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  args_short1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_833c96c437533e02 ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  args_short2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0e1eedc3fcbcea7a ("
  , "  A arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  args_short3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_906f0ac7dfd36ab8 ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  args_int1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0edbc9b995b2a589 ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  args_int2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a5c223f58a255115 ("
  , "  A arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  args_int3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_41d1229384b9a529 ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  args_long1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a9a4b09fd3bd83db ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  args_long2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_31dc2e680b3f3eff ("
  , "  A arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  args_long3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3d400757b5cbf4b7 ("
  , "  A arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  args_float(arg1, arg2);"
  , "}"
  , "void hs_bindgen_70df07e39900487e ("
  , "  A arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  args_double(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0b7c534fe683f843 ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  args_bool1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b20e084f7b7941b5 ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  args_struct(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_23aff33f33b6bdd1 ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  args_union(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_fdd58ae14ce15ed5 ("
  , "  A arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  args_enum(arg1, arg2);"
  , "}"
  , "void hs_bindgen_42ce2ec4fd2eda72 ("
  , "  A arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  args_pointer1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_01b2f6502d340abe ("
  , "  A arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  args_pointer2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3e64133f9aaebbf1 ("
  , "  A arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  args_pointer3(arg1, arg2);"
  , "}"
  , "A hs_bindgen_c830401b459192fb (void)"
  , "{"
  , "  return ret_A();"
  , "}"
  , "char hs_bindgen_18b24c6e67a5412e ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char1(arg1);"
  , "}"
  , "signed char hs_bindgen_2da1160aeef9ff64 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char2(arg1);"
  , "}"
  , "unsigned char hs_bindgen_e3183f9de1b9f231 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char3(arg1);"
  , "}"
  , "signed short hs_bindgen_c313966d4478e3f4 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short1(arg1);"
  , "}"
  , "signed short hs_bindgen_737fbec310eb0719 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short2(arg1);"
  , "}"
  , "unsigned short hs_bindgen_b5bd9e111020db4e ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short3(arg1);"
  , "}"
  , "signed int hs_bindgen_a30224259287f5f8 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int1(arg1);"
  , "}"
  , "signed int hs_bindgen_b5be09caf8cf5750 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int2(arg1);"
  , "}"
  , "unsigned int hs_bindgen_698e3f97470d83be ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int3(arg1);"
  , "}"
  , "signed long hs_bindgen_c7e0705dd09be530 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long1(arg1);"
  , "}"
  , "signed long hs_bindgen_74b1f5b8c56ff22c ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long2(arg1);"
  , "}"
  , "unsigned long hs_bindgen_c2d07eaaab82d408 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long3(arg1);"
  , "}"
  , "float hs_bindgen_0edfbc7067faa1f7 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_float(arg1);"
  , "}"
  , "double hs_bindgen_786ca672396b33be ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_double(arg1);"
  , "}"
  , "_Bool hs_bindgen_2e99f19b59650996 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_bool1(arg1);"
  , "}"
  , "void hs_bindgen_6c999121eed8178f ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_struct(arg1);"
  , "}"
  , "void hs_bindgen_481ee5d2d9bd34db ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_union(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_8bb240ba453b700d ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_enum(arg1);"
  , "}"
  , "signed int *hs_bindgen_c346ed2cd20b9af1 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer1(arg1);"
  , "}"
  , "signed int **hs_bindgen_a21f618658151728 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer2(arg1);"
  , "}"
  , "void *hs_bindgen_2d8c6e2d2f395342 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer3(arg1);"
  , "}"
  , "signed int hs_bindgen_b030d02030ed80bc ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return body1(arg1);"
  , "}"
  , "A hs_bindgen_be50427e6a63df54 (void)"
  , "{"
  , "  return body2();"
  , "}"
  , "void hs_bindgen_627a52a5c7617083 ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  args_complex_float(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_392babebc1d83503 ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  args_complex_double(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_8958183ede73dea8 ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_complex_float(arg1);"
  , "}"
  , "void hs_bindgen_a95fabfd391a99aa ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_complex_double(arg1);"
  , "}"
  , "void hs_bindgen_ad9f8630dd04a203 ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  bespoke_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4b34178a505131e2 ("
  , "  A arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  bespoke_args2(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_94b225a6394496c1 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return bespoke_ret1(arg1);"
  , "}"
  , "size_t hs_bindgen_7c9a1792426b84a1 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return bespoke_ret2(arg1);"
  , "}"
  , "void hs_bindgen_e20689fe39004225 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  arr_args1(arg1);"
  , "}"
  , "void hs_bindgen_084796e4bfd3f4cd ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  arr_args2(arg1);"
  , "}"
  , "void hs_bindgen_a3d1560aaa4352df ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  arr_args3(arg1);"
  , "}"
  , "void hs_bindgen_88659ccccc6c1f5f ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  arr_args4(arg1);"
  , "}"
  , "void hs_bindgen_3448d03cfd41161a ("
  , "  A arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  funptr_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_92d7386f0a327d25 ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  funptr_args2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2881f594f98043e6 ("
  , "  A arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  funptr_args3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1e85a05df4251f62 ("
  , "  A arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  funptr_args4(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ccf4db7511f0d6d6 ("
  , "  A arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  funptr_args5(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4c756db60673d221 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  comments1(arg1);"
  , "}"
  , "void hs_bindgen_278568d7a2a3a4b6 ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  const_prim_before1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_87ee56525e5ea20c ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  const_prim_before2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c0b99594235bd99e ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  const_prim_before3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d718b682f157fc18 ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  const_prim_after1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f2c5b3d5eca68433 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  const_prim_after2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ae2d994e06667b23 ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  const_prim_after3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6940b58e7f4397a7 ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_00b6fe2282e779b1 ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6517cc8d39aead93 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_68c7661e95060488 ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before4(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_42b3b0bf73a7a51a ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before5(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_c4aabe9834aac12f ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before6(arg1, arg2);"
  , "}"
  , "void hs_bindgen_486090a7fb4e34d4 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before7(arg1, arg2);"
  , "}"
  , "void hs_bindgen_23fa742b614176dd ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before8(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0aacd8a5d48f296d ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_16ec2102221485b7 ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9aa934d44ec3790c ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_043d2869e29bedcf ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after4(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_b5f9bca1de9d69de ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after5(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_77d641d518b2504f ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after6(arg1, arg2);"
  , "}"
  , "void hs_bindgen_691b4f2909140b49 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after7(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ae74c8dcdc2ec9eb ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after8(arg1, arg2);"
  , "}"
  , "void hs_bindgen_07606c41eadf9146 ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  const_pointers_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3836769f3a3416ac ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  const_pointers_args2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_12f19ea593aefd3f ("
  , "  A arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_5a50e98897696d57 ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args4(arg1, arg2);"
  , "}"
  , "void hs_bindgen_666701f7cb61bd15 ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args5(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_b94fbc3dfd285563 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret1(arg1);"
  , "}"
  , "signed int const *hs_bindgen_33e2960e26b79450 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret2(arg1);"
  , "}"
  , "signed int *const hs_bindgen_50c6e2fe4f3fb777 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret3(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_edc014695d896c8d ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret4(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_6d3308cc5847f033 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret5(arg1);"
  , "}"
  , "void hs_bindgen_678576320923a4d1 ("
  , "  A const *arg1"
  , ")"
  , "{"
  , "  const_array_elem1(arg1);"
  , "}"
  , "void hs_bindgen_b317941dde4eeff2 ("
  , "  A const **arg1"
  , ")"
  , "{"
  , "  const_array_elem2(arg1);"
  , "}"
  , "void hs_bindgen_707e602e6beb1bb6 ("
  , "  A *const *arg1"
  , ")"
  , "{"
  , "  const_array_elem3(arg1);"
  , "}"
  , "A hs_bindgen_93fecb4eb766c262 (void)"
  , "{"
  , "  return noParams1();"
  , "}"
  , "A hs_bindgen_4350965157c891f5 (void)"
  , "{"
  , "  return noParams2();"
  , "}"
  , "void hs_bindgen_c4f59272a2b1c3b5 ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  noParams3(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_387a04c01e23c320 ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return funptr_ret1(arg1);"
  , "}"
  , "signed int (*hs_bindgen_6f0c14cd3478dc19 ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return funptr_ret2(arg1);"
  , "}"
  , "void (*hs_bindgen_08e8661d277cf7be ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return funptr_ret3(arg1);"
  , "}"
  , "char (*hs_bindgen_609b5d953b68da92 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret4(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_13e6ae43abf40aee ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret5(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_a4a3a86f28ca6299 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret6(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_eae9dff04c88d00b ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret7(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_894457d90a2fc8db ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret8(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_c893eb15ad9bc68c ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret9(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_d96c258298a44b28 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret10(arg1);"
  , "}"
  ]))

-- | __unique:__ @test_macrosreparse_Example_Safe_args_char1@
foreign import ccall safe "hs_bindgen_f15610128336b06a" hs_bindgen_f15610128336b06a ::
     A
  -> FC.CChar
  -> IO ()

{-| Function declarations

__C declaration:__ @args_char1@

__defined at:__ @macros\/reparse.h:17:6@

__exported by:__ @macros\/reparse.h@
-}
args_char1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_char1 = hs_bindgen_f15610128336b06a

-- | __unique:__ @test_macrosreparse_Example_Safe_args_char2@
foreign import ccall safe "hs_bindgen_087f45ca0a284a03" hs_bindgen_087f45ca0a284a03 ::
     A
  -> FC.CSChar
  -> IO ()

{-| __C declaration:__ @args_char2@

    __defined at:__ @macros\/reparse.h:18:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CSChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_char2 = hs_bindgen_087f45ca0a284a03

-- | __unique:__ @test_macrosreparse_Example_Safe_args_char3@
foreign import ccall safe "hs_bindgen_f6cb5c5a728c2404" hs_bindgen_f6cb5c5a728c2404 ::
     A
  -> FC.CUChar
  -> IO ()

{-| __C declaration:__ @args_char3@

    __defined at:__ @macros\/reparse.h:19:6@

    __exported by:__ @macros\/reparse.h@
-}
args_char3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CUChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_char3 = hs_bindgen_f6cb5c5a728c2404

-- | __unique:__ @test_macrosreparse_Example_Safe_args_short1@
foreign import ccall safe "hs_bindgen_d485767e0caa1f7c" hs_bindgen_d485767e0caa1f7c ::
     A
  -> FC.CShort
  -> IO ()

{-| __C declaration:__ @args_short1@

    __defined at:__ @macros\/reparse.h:21:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CShort
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_short1 = hs_bindgen_d485767e0caa1f7c

-- | __unique:__ @test_macrosreparse_Example_Safe_args_short2@
foreign import ccall safe "hs_bindgen_833c96c437533e02" hs_bindgen_833c96c437533e02 ::
     A
  -> FC.CShort
  -> IO ()

{-| __C declaration:__ @args_short2@

    __defined at:__ @macros\/reparse.h:22:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CShort
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_short2 = hs_bindgen_833c96c437533e02

-- | __unique:__ @test_macrosreparse_Example_Safe_args_short3@
foreign import ccall safe "hs_bindgen_0e1eedc3fcbcea7a" hs_bindgen_0e1eedc3fcbcea7a ::
     A
  -> FC.CUShort
  -> IO ()

{-| __C declaration:__ @args_short3@

    __defined at:__ @macros\/reparse.h:23:6@

    __exported by:__ @macros\/reparse.h@
-}
args_short3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CUShort
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_short3 = hs_bindgen_0e1eedc3fcbcea7a

-- | __unique:__ @test_macrosreparse_Example_Safe_args_int1@
foreign import ccall safe "hs_bindgen_906f0ac7dfd36ab8" hs_bindgen_906f0ac7dfd36ab8 ::
     A
  -> FC.CInt
  -> IO ()

{-| __C declaration:__ @args_int1@

    __defined at:__ @macros\/reparse.h:25:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_int1 = hs_bindgen_906f0ac7dfd36ab8

-- | __unique:__ @test_macrosreparse_Example_Safe_args_int2@
foreign import ccall safe "hs_bindgen_0edbc9b995b2a589" hs_bindgen_0edbc9b995b2a589 ::
     A
  -> FC.CInt
  -> IO ()

{-| __C declaration:__ @args_int2@

    __defined at:__ @macros\/reparse.h:26:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_int2 = hs_bindgen_0edbc9b995b2a589

-- | __unique:__ @test_macrosreparse_Example_Safe_args_int3@
foreign import ccall safe "hs_bindgen_a5c223f58a255115" hs_bindgen_a5c223f58a255115 ::
     A
  -> FC.CUInt
  -> IO ()

{-| __C declaration:__ @args_int3@

    __defined at:__ @macros\/reparse.h:27:6@

    __exported by:__ @macros\/reparse.h@
-}
args_int3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CUInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_int3 = hs_bindgen_a5c223f58a255115

-- | __unique:__ @test_macrosreparse_Example_Safe_args_long1@
foreign import ccall safe "hs_bindgen_41d1229384b9a529" hs_bindgen_41d1229384b9a529 ::
     A
  -> FC.CLong
  -> IO ()

{-| __C declaration:__ @args_long1@

    __defined at:__ @macros\/reparse.h:29:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CLong
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_long1 = hs_bindgen_41d1229384b9a529

-- | __unique:__ @test_macrosreparse_Example_Safe_args_long2@
foreign import ccall safe "hs_bindgen_a9a4b09fd3bd83db" hs_bindgen_a9a4b09fd3bd83db ::
     A
  -> FC.CLong
  -> IO ()

{-| __C declaration:__ @args_long2@

    __defined at:__ @macros\/reparse.h:30:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CLong
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_long2 = hs_bindgen_a9a4b09fd3bd83db

-- | __unique:__ @test_macrosreparse_Example_Safe_args_long3@
foreign import ccall safe "hs_bindgen_31dc2e680b3f3eff" hs_bindgen_31dc2e680b3f3eff ::
     A
  -> FC.CULong
  -> IO ()

{-| __C declaration:__ @args_long3@

    __defined at:__ @macros\/reparse.h:31:6@

    __exported by:__ @macros\/reparse.h@
-}
args_long3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CULong
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_long3 = hs_bindgen_31dc2e680b3f3eff

-- | __unique:__ @test_macrosreparse_Example_Safe_args_float@
foreign import ccall safe "hs_bindgen_3d400757b5cbf4b7" hs_bindgen_3d400757b5cbf4b7 ::
     A
  -> FC.CFloat
  -> IO ()

{-| __C declaration:__ @args_float@

    __defined at:__ @macros\/reparse.h:33:6@

    __exported by:__ @macros\/reparse.h@
-}
args_float ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_float = hs_bindgen_3d400757b5cbf4b7

-- | __unique:__ @test_macrosreparse_Example_Safe_args_double@
foreign import ccall safe "hs_bindgen_70df07e39900487e" hs_bindgen_70df07e39900487e ::
     A
  -> FC.CDouble
  -> IO ()

{-| __C declaration:__ @args_double@

    __defined at:__ @macros\/reparse.h:34:6@

    __exported by:__ @macros\/reparse.h@
-}
args_double ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_double = hs_bindgen_70df07e39900487e

-- | __unique:__ @test_macrosreparse_Example_Safe_args_bool1@
foreign import ccall safe "hs_bindgen_0b7c534fe683f843" hs_bindgen_0b7c534fe683f843 ::
     A
  -> FC.CBool
  -> IO ()

{-| __C declaration:__ @args_bool1@

    __defined at:__ @macros\/reparse.h:35:6@

    __exported by:__ @macros\/reparse.h@
-}
args_bool1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_bool1 = hs_bindgen_0b7c534fe683f843

-- | __unique:__ @test_macrosreparse_Example_Safe_args_struct@
foreign import ccall safe "hs_bindgen_b20e084f7b7941b5" hs_bindgen_b20e084f7b7941b5 ::
     A
  -> Ptr.Ptr Some_struct
  -> IO ()

{-| Pointer-based API for 'args_struct'
-}
args_struct_wrapper ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.Ptr Some_struct
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_struct_wrapper = hs_bindgen_b20e084f7b7941b5

{-| __C declaration:__ @args_struct@

    __defined at:__ @macros\/reparse.h:37:6@

    __exported by:__ @macros\/reparse.h@
-}
args_struct ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_struct
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_struct =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 -> hs_bindgen_b20e084f7b7941b5 x0 y2)

-- | __unique:__ @test_macrosreparse_Example_Safe_args_union@
foreign import ccall safe "hs_bindgen_23aff33f33b6bdd1" hs_bindgen_23aff33f33b6bdd1 ::
     A
  -> Ptr.Ptr Some_union
  -> IO ()

{-| Pointer-based API for 'args_union'
-}
args_union_wrapper ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.Ptr Some_union
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_union_wrapper = hs_bindgen_23aff33f33b6bdd1

{-| __C declaration:__ @args_union@

    __defined at:__ @macros\/reparse.h:38:6@

    __exported by:__ @macros\/reparse.h@
-}
args_union ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_union
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_union =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 -> hs_bindgen_23aff33f33b6bdd1 x0 y2)

-- | __unique:__ @test_macrosreparse_Example_Safe_args_enum@
foreign import ccall safe "hs_bindgen_fdd58ae14ce15ed5" hs_bindgen_fdd58ae14ce15ed5 ::
     A
  -> Some_enum
  -> IO ()

{-| __C declaration:__ @args_enum@

    __defined at:__ @macros\/reparse.h:39:6@

    __exported by:__ @macros\/reparse.h@
-}
args_enum ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_enum
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_enum = hs_bindgen_fdd58ae14ce15ed5

-- | __unique:__ @test_macrosreparse_Example_Safe_args_pointer1@
foreign import ccall safe "hs_bindgen_42ce2ec4fd2eda72" hs_bindgen_42ce2ec4fd2eda72 ::
     A
  -> Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @args_pointer1@

    __defined at:__ @macros\/reparse.h:41:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_pointer1 = hs_bindgen_42ce2ec4fd2eda72

-- | __unique:__ @test_macrosreparse_Example_Safe_args_pointer2@
foreign import ccall safe "hs_bindgen_01b2f6502d340abe" hs_bindgen_01b2f6502d340abe ::
     A
  -> Ptr.Ptr (Ptr.Ptr FC.CInt)
  -> IO ()

{-| __C declaration:__ @args_pointer2@

    __defined at:__ @macros\/reparse.h:42:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.Ptr (Ptr.Ptr FC.CInt)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_pointer2 = hs_bindgen_01b2f6502d340abe

-- | __unique:__ @test_macrosreparse_Example_Safe_args_pointer3@
foreign import ccall safe "hs_bindgen_3e64133f9aaebbf1" hs_bindgen_3e64133f9aaebbf1 ::
     A
  -> Ptr.Ptr Void
  -> IO ()

{-| __C declaration:__ @args_pointer3@

    __defined at:__ @macros\/reparse.h:43:6@

    __exported by:__ @macros\/reparse.h@
-}
args_pointer3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.Ptr Void
     -- ^ __C declaration:__ @arg3@
  -> IO ()
args_pointer3 = hs_bindgen_3e64133f9aaebbf1

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_A@
foreign import ccall safe "hs_bindgen_c830401b459192fb" hs_bindgen_c830401b459192fb ::
     IO A

{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h:47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A ::
     IO A
ret_A = hs_bindgen_c830401b459192fb

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_char1@
foreign import ccall safe "hs_bindgen_18b24c6e67a5412e" hs_bindgen_18b24c6e67a5412e ::
     A
  -> IO FC.CChar

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h:49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CChar
ret_char1 = hs_bindgen_18b24c6e67a5412e

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_char2@
foreign import ccall safe "hs_bindgen_2da1160aeef9ff64" hs_bindgen_2da1160aeef9ff64 ::
     A
  -> IO FC.CSChar

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h:50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CSChar
ret_char2 = hs_bindgen_2da1160aeef9ff64

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_char3@
foreign import ccall safe "hs_bindgen_e3183f9de1b9f231" hs_bindgen_e3183f9de1b9f231 ::
     A
  -> IO FC.CUChar

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h:51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CUChar
ret_char3 = hs_bindgen_e3183f9de1b9f231

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_short1@
foreign import ccall safe "hs_bindgen_c313966d4478e3f4" hs_bindgen_c313966d4478e3f4 ::
     A
  -> IO FC.CShort

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h:53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CShort
ret_short1 = hs_bindgen_c313966d4478e3f4

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_short2@
foreign import ccall safe "hs_bindgen_737fbec310eb0719" hs_bindgen_737fbec310eb0719 ::
     A
  -> IO FC.CShort

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h:54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CShort
ret_short2 = hs_bindgen_737fbec310eb0719

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_short3@
foreign import ccall safe "hs_bindgen_b5bd9e111020db4e" hs_bindgen_b5bd9e111020db4e ::
     A
  -> IO FC.CUShort

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h:55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CUShort
ret_short3 = hs_bindgen_b5bd9e111020db4e

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_int1@
foreign import ccall safe "hs_bindgen_a30224259287f5f8" hs_bindgen_a30224259287f5f8 ::
     A
  -> IO FC.CInt

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h:57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CInt
ret_int1 = hs_bindgen_a30224259287f5f8

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_int2@
foreign import ccall safe "hs_bindgen_b5be09caf8cf5750" hs_bindgen_b5be09caf8cf5750 ::
     A
  -> IO FC.CInt

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h:58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CInt
ret_int2 = hs_bindgen_b5be09caf8cf5750

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_int3@
foreign import ccall safe "hs_bindgen_698e3f97470d83be" hs_bindgen_698e3f97470d83be ::
     A
  -> IO FC.CUInt

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h:59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CUInt
ret_int3 = hs_bindgen_698e3f97470d83be

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_long1@
foreign import ccall safe "hs_bindgen_c7e0705dd09be530" hs_bindgen_c7e0705dd09be530 ::
     A
  -> IO FC.CLong

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h:61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CLong
ret_long1 = hs_bindgen_c7e0705dd09be530

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_long2@
foreign import ccall safe "hs_bindgen_74b1f5b8c56ff22c" hs_bindgen_74b1f5b8c56ff22c ::
     A
  -> IO FC.CLong

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h:62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CLong
ret_long2 = hs_bindgen_74b1f5b8c56ff22c

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_long3@
foreign import ccall safe "hs_bindgen_c2d07eaaab82d408" hs_bindgen_c2d07eaaab82d408 ::
     A
  -> IO FC.CULong

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h:63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CULong
ret_long3 = hs_bindgen_c2d07eaaab82d408

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_float@
foreign import ccall safe "hs_bindgen_0edfbc7067faa1f7" hs_bindgen_0edfbc7067faa1f7 ::
     A
  -> IO FC.CFloat

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h:65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CFloat
ret_float = hs_bindgen_0edfbc7067faa1f7

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_double@
foreign import ccall safe "hs_bindgen_786ca672396b33be" hs_bindgen_786ca672396b33be ::
     A
  -> IO FC.CDouble

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h:66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CDouble
ret_double = hs_bindgen_786ca672396b33be

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_bool1@
foreign import ccall safe "hs_bindgen_2e99f19b59650996" hs_bindgen_2e99f19b59650996 ::
     A
  -> IO FC.CBool

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h:67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CBool
ret_bool1 = hs_bindgen_2e99f19b59650996

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_struct@
foreign import ccall safe "hs_bindgen_6c999121eed8178f" hs_bindgen_6c999121eed8178f ::
     A
  -> Ptr.Ptr Some_struct
  -> IO ()

{-| Pointer-based API for 'ret_struct'
-}
ret_struct_wrapper ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.Ptr Some_struct
  -> IO ()
ret_struct_wrapper = hs_bindgen_6c999121eed8178f

{-| __C declaration:__ @ret_struct@

    __defined at:__ @macros\/reparse.h:69:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_struct ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO Some_struct
ret_struct =
  \x0 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\z1 ->
                                            hs_bindgen_6c999121eed8178f x0 z1)

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_union@
foreign import ccall safe "hs_bindgen_481ee5d2d9bd34db" hs_bindgen_481ee5d2d9bd34db ::
     A
  -> Ptr.Ptr Some_union
  -> IO ()

{-| Pointer-based API for 'ret_union'
-}
ret_union_wrapper ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.Ptr Some_union
  -> IO ()
ret_union_wrapper = hs_bindgen_481ee5d2d9bd34db

{-| __C declaration:__ @ret_union@

    __defined at:__ @macros\/reparse.h:70:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_union ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO Some_union
ret_union =
  \x0 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\z1 ->
                                            hs_bindgen_481ee5d2d9bd34db x0 z1)

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_enum@
foreign import ccall safe "hs_bindgen_8bb240ba453b700d" hs_bindgen_8bb240ba453b700d ::
     A
  -> IO Some_enum

{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h:71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO Some_enum
ret_enum = hs_bindgen_8bb240ba453b700d

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_pointer1@
foreign import ccall safe "hs_bindgen_c346ed2cd20b9af1" hs_bindgen_c346ed2cd20b9af1 ::
     A
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h:73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.Ptr FC.CInt)
ret_pointer1 = hs_bindgen_c346ed2cd20b9af1

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_pointer2@
foreign import ccall safe "hs_bindgen_a21f618658151728" hs_bindgen_a21f618658151728 ::
     A
  -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h:74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt))
ret_pointer2 = hs_bindgen_a21f618658151728

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_pointer3@
foreign import ccall safe "hs_bindgen_2d8c6e2d2f395342" hs_bindgen_2d8c6e2d2f395342 ::
     A
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h:75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.Ptr Void)
ret_pointer3 = hs_bindgen_2d8c6e2d2f395342

-- | __unique:__ @test_macrosreparse_Example_Safe_body1@
foreign import ccall safe "hs_bindgen_b030d02030ed80bc" hs_bindgen_b030d02030ed80bc ::
     A
  -> IO FC.CInt

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h:79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CInt
body1 = hs_bindgen_b030d02030ed80bc

-- | __unique:__ @test_macrosreparse_Example_Safe_body2@
foreign import ccall safe "hs_bindgen_be50427e6a63df54" hs_bindgen_be50427e6a63df54 ::
     IO A

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h:80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2 ::
     IO A
body2 = hs_bindgen_be50427e6a63df54

-- | __unique:__ @test_macrosreparse_Example_Safe_args_complex_float@
foreign import ccall safe "hs_bindgen_627a52a5c7617083" hs_bindgen_627a52a5c7617083 ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> IO ()

{-| Pointer-based API for 'args_complex_float'
-}
args_complex_float_wrapper ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_complex_float_wrapper =
  hs_bindgen_627a52a5c7617083

{-| __C declaration:__ @args_complex_float@

    __defined at:__ @macros\/reparse.h:84:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_float ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Data.Complex.Complex FC.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_complex_float =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 -> hs_bindgen_627a52a5c7617083 x0 y2)

-- | __unique:__ @test_macrosreparse_Example_Safe_args_complex_double@
foreign import ccall safe "hs_bindgen_392babebc1d83503" hs_bindgen_392babebc1d83503 ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> IO ()

{-| Pointer-based API for 'args_complex_double'
-}
args_complex_double_wrapper ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_complex_double_wrapper =
  hs_bindgen_392babebc1d83503

{-| __C declaration:__ @args_complex_double@

    __defined at:__ @macros\/reparse.h:85:6@

    __exported by:__ @macros\/reparse.h@
-}
args_complex_double ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Data.Complex.Complex FC.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_complex_double =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 -> hs_bindgen_392babebc1d83503 x0 y2)

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_complex_float@
foreign import ccall safe "hs_bindgen_8958183ede73dea8" hs_bindgen_8958183ede73dea8 ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> IO ()

{-| Pointer-based API for 'ret_complex_float'
-}
ret_complex_float_wrapper ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> IO ()
ret_complex_float_wrapper =
  hs_bindgen_8958183ede73dea8

{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @macros\/reparse.h:86:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_float ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Data.Complex.Complex FC.CFloat)
ret_complex_float =
  \x0 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\z1 ->
                                            hs_bindgen_8958183ede73dea8 x0 z1)

-- | __unique:__ @test_macrosreparse_Example_Safe_ret_complex_double@
foreign import ccall safe "hs_bindgen_a95fabfd391a99aa" hs_bindgen_a95fabfd391a99aa ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> IO ()

{-| Pointer-based API for 'ret_complex_double'
-}
ret_complex_double_wrapper ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> IO ()
ret_complex_double_wrapper =
  hs_bindgen_a95fabfd391a99aa

{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @macros\/reparse.h:87:17@

    __exported by:__ @macros\/reparse.h@
-}
ret_complex_double ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Data.Complex.Complex FC.CDouble)
ret_complex_double =
  \x0 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\z1 ->
                                            hs_bindgen_a95fabfd391a99aa x0 z1)

-- | __unique:__ @test_macrosreparse_Example_Safe_bespoke_args1@
foreign import ccall safe "hs_bindgen_ad9f8630dd04a203" hs_bindgen_ad9f8630dd04a203 ::
     A
  -> FC.CBool
  -> IO ()

{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @macros\/reparse.h:94:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
bespoke_args1 = hs_bindgen_ad9f8630dd04a203

-- | __unique:__ @test_macrosreparse_Example_Safe_bespoke_args2@
foreign import ccall safe "hs_bindgen_4b34178a505131e2" hs_bindgen_4b34178a505131e2 ::
     A
  -> HsBindgen.Runtime.Prelude.CSize
  -> IO ()

{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @macros\/reparse.h:95:6@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_args2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.Prelude.CSize
     -- ^ __C declaration:__ @arg2@
  -> IO ()
bespoke_args2 = hs_bindgen_4b34178a505131e2

-- | __unique:__ @test_macrosreparse_Example_Safe_bespoke_ret1@
foreign import ccall safe "hs_bindgen_94b225a6394496c1" hs_bindgen_94b225a6394496c1 ::
     A
  -> IO FC.CBool

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h:97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO FC.CBool
bespoke_ret1 = hs_bindgen_94b225a6394496c1

-- | __unique:__ @test_macrosreparse_Example_Safe_bespoke_ret2@
foreign import ccall safe "hs_bindgen_7c9a1792426b84a1" hs_bindgen_7c9a1792426b84a1 ::
     A
  -> IO HsBindgen.Runtime.Prelude.CSize

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h:98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO HsBindgen.Runtime.Prelude.CSize
bespoke_ret2 = hs_bindgen_7c9a1792426b84a1

-- | __unique:__ @test_macrosreparse_Example_Safe_arr_args1@
foreign import ccall safe "hs_bindgen_e20689fe39004225" hs_bindgen_e20689fe39004225 ::
     Ptr.Ptr A
  -> IO ()

{-| Arrays

__C declaration:__ @arr_args1@

__defined at:__ @macros\/reparse.h:104:6@

__exported by:__ @macros\/reparse.h@
-}
arr_args1 ::
     Ptr.Ptr A
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args1 = hs_bindgen_e20689fe39004225

-- | __unique:__ @test_macrosreparse_Example_Safe_arr_args2@
foreign import ccall safe "hs_bindgen_084796e4bfd3f4cd" hs_bindgen_084796e4bfd3f4cd ::
     Ptr.Ptr (Ptr.Ptr A)
  -> IO ()

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h:105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2 ::
     Ptr.Ptr (Ptr.Ptr A)
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args2 = hs_bindgen_084796e4bfd3f4cd

-- | __unique:__ @test_macrosreparse_Example_Safe_arr_args3@
foreign import ccall safe "hs_bindgen_a3d1560aaa4352df" hs_bindgen_a3d1560aaa4352df ::
     Ptr.Ptr A
  -> IO ()

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h:106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3 ::
     Ptr.Ptr A
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args3 = hs_bindgen_a3d1560aaa4352df

-- | __unique:__ @test_macrosreparse_Example_Safe_arr_args4@
foreign import ccall safe "hs_bindgen_88659ccccc6c1f5f" hs_bindgen_88659ccccc6c1f5f ::
     Ptr.Ptr (Ptr.Ptr A)
  -> IO ()

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h:107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4 ::
     Ptr.Ptr (Ptr.Ptr A)
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args4 = hs_bindgen_88659ccccc6c1f5f

-- | __unique:__ @test_macrosreparse_Example_Safe_funptr_args1@
foreign import ccall safe "hs_bindgen_3448d03cfd41161a" hs_bindgen_3448d03cfd41161a ::
     A
  -> Ptr.FunPtr (IO ())
  -> IO ()

{-| Function pointers

__C declaration:__ @funptr_args1@

__defined at:__ @macros\/reparse.h:126:6@

__exported by:__ @macros\/reparse.h@
-}
funptr_args1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.FunPtr (IO ())
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args1 = hs_bindgen_3448d03cfd41161a

-- | __unique:__ @test_macrosreparse_Example_Safe_funptr_args2@
foreign import ccall safe "hs_bindgen_92d7386f0a327d25" hs_bindgen_92d7386f0a327d25 ::
     A
  -> Ptr.FunPtr (IO FC.CInt)
  -> IO ()

{-| __C declaration:__ @funptr_args2@

    __defined at:__ @macros\/reparse.h:127:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.FunPtr (IO FC.CInt)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args2 = hs_bindgen_92d7386f0a327d25

-- | __unique:__ @test_macrosreparse_Example_Safe_funptr_args3@
foreign import ccall safe "hs_bindgen_2881f594f98043e6" hs_bindgen_2881f594f98043e6 ::
     A
  -> Ptr.FunPtr (FC.CInt -> IO ())
  -> IO ()

{-| __C declaration:__ @funptr_args3@

    __defined at:__ @macros\/reparse.h:128:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.FunPtr (FC.CInt -> IO ())
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args3 = hs_bindgen_2881f594f98043e6

-- | __unique:__ @test_macrosreparse_Example_Safe_funptr_args4@
foreign import ccall safe "hs_bindgen_1e85a05df4251f62" hs_bindgen_1e85a05df4251f62 ::
     A
  -> Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)
  -> IO ()

{-| __C declaration:__ @funptr_args4@

    __defined at:__ @macros\/reparse.h:129:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args4 = hs_bindgen_1e85a05df4251f62

-- | __unique:__ @test_macrosreparse_Example_Safe_funptr_args5@
foreign import ccall safe "hs_bindgen_ccf4db7511f0d6d6" hs_bindgen_ccf4db7511f0d6d6 ::
     A
  -> Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))
  -> IO ()

{-| __C declaration:__ @funptr_args5@

    __defined at:__ @macros\/reparse.h:130:6@

    __exported by:__ @macros\/reparse.h@
-}
funptr_args5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args5 = hs_bindgen_ccf4db7511f0d6d6

-- | __unique:__ @test_macrosreparse_Example_Safe_comments1@
foreign import ccall safe "hs_bindgen_4c756db60673d221" hs_bindgen_4c756db60673d221 ::
     A
  -> IO ()

{-| Comments in awkward places

  (Prior to language-c we failed to parse there.)

__C declaration:__ @comments1@

__defined at:__ @macros\/reparse.h:144:25@

__exported by:__ @macros\/reparse.h@
-}
comments1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO ()
comments1 = hs_bindgen_4c756db60673d221

-- | __unique:__ @test_macrosreparse_Example_Safe_const_prim_before1@
foreign import ccall safe "hs_bindgen_278568d7a2a3a4b6" hs_bindgen_278568d7a2a3a4b6 ::
     A
  -> FC.CChar
  -> IO ()

{-| `const` qualifier

  NOTE: These were not parsed correctly prior to the switch to language-c.

__C declaration:__ @const_prim_before1@

__defined at:__ @macros\/reparse.h:179:6@

__exported by:__ @macros\/reparse.h@
-}
const_prim_before1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_before1 = hs_bindgen_278568d7a2a3a4b6

-- | __unique:__ @test_macrosreparse_Example_Safe_const_prim_before2@
foreign import ccall safe "hs_bindgen_87ee56525e5ea20c" hs_bindgen_87ee56525e5ea20c ::
     A
  -> FC.CSChar
  -> IO ()

{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @macros\/reparse.h:180:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CSChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_before2 = hs_bindgen_87ee56525e5ea20c

-- | __unique:__ @test_macrosreparse_Example_Safe_const_prim_before3@
foreign import ccall safe "hs_bindgen_c0b99594235bd99e" hs_bindgen_c0b99594235bd99e ::
     A
  -> FC.CUChar
  -> IO ()

{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @macros\/reparse.h:181:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_before3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CUChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_before3 = hs_bindgen_c0b99594235bd99e

-- | __unique:__ @test_macrosreparse_Example_Safe_const_prim_after1@
foreign import ccall safe "hs_bindgen_d718b682f157fc18" hs_bindgen_d718b682f157fc18 ::
     A
  -> FC.CChar
  -> IO ()

{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @macros\/reparse.h:182:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_after1 = hs_bindgen_d718b682f157fc18

-- | __unique:__ @test_macrosreparse_Example_Safe_const_prim_after2@
foreign import ccall safe "hs_bindgen_f2c5b3d5eca68433" hs_bindgen_f2c5b3d5eca68433 ::
     A
  -> FC.CSChar
  -> IO ()

{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @macros\/reparse.h:183:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CSChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_after2 = hs_bindgen_f2c5b3d5eca68433

-- | __unique:__ @test_macrosreparse_Example_Safe_const_prim_after3@
foreign import ccall safe "hs_bindgen_ae2d994e06667b23" hs_bindgen_ae2d994e06667b23 ::
     A
  -> FC.CUChar
  -> IO ()

{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @macros\/reparse.h:184:6@

    __exported by:__ @macros\/reparse.h@
-}
const_prim_after3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CUChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_after3 = hs_bindgen_ae2d994e06667b23

-- | __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before1@
foreign import ccall safe "hs_bindgen_6940b58e7f4397a7" hs_bindgen_6940b58e7f4397a7 ::
     A
  -> FC.CFloat
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @macros\/reparse.h:188:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before1 =
  hs_bindgen_6940b58e7f4397a7

-- | __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before2@
foreign import ccall safe "hs_bindgen_00b6fe2282e779b1" hs_bindgen_00b6fe2282e779b1 ::
     A
  -> FC.CDouble
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @macros\/reparse.h:189:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before2 =
  hs_bindgen_00b6fe2282e779b1

-- | __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before3@
foreign import ccall safe "hs_bindgen_6517cc8d39aead93" hs_bindgen_6517cc8d39aead93 ::
     A
  -> FC.CBool
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @macros\/reparse.h:190:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before3 =
  hs_bindgen_6517cc8d39aead93

-- | __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before4@
foreign import ccall safe "hs_bindgen_68c7661e95060488" hs_bindgen_68c7661e95060488 ::
     A
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Some_struct
  -> IO ()

{-| Pointer-based API for 'const_withoutSign_before4'
-}
const_withoutSign_before4_wrapper ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Some_struct
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before4_wrapper =
  hs_bindgen_68c7661e95060488

{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @macros\/reparse.h:191:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_struct
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before4 =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 ->
                   hs_bindgen_68c7661e95060488 x0 (HsBindgen.Runtime.ConstPtr.ConstPtr y2))

-- | __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before5@
foreign import ccall safe "hs_bindgen_42b3b0bf73a7a51a" hs_bindgen_42b3b0bf73a7a51a ::
     A
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Some_union
  -> IO ()

{-| Pointer-based API for 'const_withoutSign_before5'
-}
const_withoutSign_before5_wrapper ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Some_union
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before5_wrapper =
  hs_bindgen_42b3b0bf73a7a51a

{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @macros\/reparse.h:192:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_union
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before5 =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 ->
                   hs_bindgen_42b3b0bf73a7a51a x0 (HsBindgen.Runtime.ConstPtr.ConstPtr y2))

-- | __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before6@
foreign import ccall safe "hs_bindgen_c4aabe9834aac12f" hs_bindgen_c4aabe9834aac12f ::
     A
  -> Some_enum
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @macros\/reparse.h:193:6@

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

-- | __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before7@
foreign import ccall safe "hs_bindgen_486090a7fb4e34d4" hs_bindgen_486090a7fb4e34d4 ::
     A
  -> FC.CBool
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @macros\/reparse.h:194:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before7 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before7 =
  hs_bindgen_486090a7fb4e34d4

-- | __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_before8@
foreign import ccall safe "hs_bindgen_23fa742b614176dd" hs_bindgen_23fa742b614176dd ::
     A
  -> HsBindgen.Runtime.Prelude.CSize
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @macros\/reparse.h:195:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_before8 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.Prelude.CSize
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before8 =
  hs_bindgen_23fa742b614176dd

-- | __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after1@
foreign import ccall safe "hs_bindgen_0aacd8a5d48f296d" hs_bindgen_0aacd8a5d48f296d ::
     A
  -> FC.CFloat
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @macros\/reparse.h:197:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after1 =
  hs_bindgen_0aacd8a5d48f296d

-- | __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after2@
foreign import ccall safe "hs_bindgen_16ec2102221485b7" hs_bindgen_16ec2102221485b7 ::
     A
  -> FC.CDouble
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @macros\/reparse.h:198:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after2 =
  hs_bindgen_16ec2102221485b7

-- | __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after3@
foreign import ccall safe "hs_bindgen_9aa934d44ec3790c" hs_bindgen_9aa934d44ec3790c ::
     A
  -> FC.CBool
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @macros\/reparse.h:199:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after3 =
  hs_bindgen_9aa934d44ec3790c

-- | __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after4@
foreign import ccall safe "hs_bindgen_043d2869e29bedcf" hs_bindgen_043d2869e29bedcf ::
     A
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Some_struct
  -> IO ()

{-| Pointer-based API for 'const_withoutSign_after4'
-}
const_withoutSign_after4_wrapper ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Some_struct
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after4_wrapper =
  hs_bindgen_043d2869e29bedcf

{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @macros\/reparse.h:200:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_struct
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after4 =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 ->
                   hs_bindgen_043d2869e29bedcf x0 (HsBindgen.Runtime.ConstPtr.ConstPtr y2))

-- | __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after5@
foreign import ccall safe "hs_bindgen_b5f9bca1de9d69de" hs_bindgen_b5f9bca1de9d69de ::
     A
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Some_union
  -> IO ()

{-| Pointer-based API for 'const_withoutSign_after5'
-}
const_withoutSign_after5_wrapper ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Some_union
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after5_wrapper =
  hs_bindgen_b5f9bca1de9d69de

{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @macros\/reparse.h:201:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_union
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after5 =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 ->
                   hs_bindgen_b5f9bca1de9d69de x0 (HsBindgen.Runtime.ConstPtr.ConstPtr y2))

-- | __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after6@
foreign import ccall safe "hs_bindgen_77d641d518b2504f" hs_bindgen_77d641d518b2504f ::
     A
  -> Some_enum
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @macros\/reparse.h:202:6@

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

-- | __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after7@
foreign import ccall safe "hs_bindgen_691b4f2909140b49" hs_bindgen_691b4f2909140b49 ::
     A
  -> FC.CBool
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @macros\/reparse.h:203:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after7 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> FC.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after7 =
  hs_bindgen_691b4f2909140b49

-- | __unique:__ @test_macrosreparse_Example_Safe_const_withoutSign_after8@
foreign import ccall safe "hs_bindgen_ae74c8dcdc2ec9eb" hs_bindgen_ae74c8dcdc2ec9eb ::
     A
  -> HsBindgen.Runtime.Prelude.CSize
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @macros\/reparse.h:204:6@

    __exported by:__ @macros\/reparse.h@
-}
const_withoutSign_after8 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.Prelude.CSize
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after8 =
  hs_bindgen_ae74c8dcdc2ec9eb

-- | __unique:__ @test_macrosreparse_Example_Safe_const_pointers_args1@
foreign import ccall safe "hs_bindgen_07606c41eadf9146" hs_bindgen_07606c41eadf9146 ::
     A
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
  -> IO ()

{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @macros\/reparse.h:208:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args1 = hs_bindgen_07606c41eadf9146

-- | __unique:__ @test_macrosreparse_Example_Safe_const_pointers_args2@
foreign import ccall safe "hs_bindgen_3836769f3a3416ac" hs_bindgen_3836769f3a3416ac ::
     A
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
  -> IO ()

{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @macros\/reparse.h:209:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args2 = hs_bindgen_3836769f3a3416ac

-- | __unique:__ @test_macrosreparse_Example_Safe_const_pointers_args3@
foreign import ccall safe "hs_bindgen_12f19ea593aefd3f" hs_bindgen_12f19ea593aefd3f ::
     A
  -> Ptr.Ptr FC.CInt
  -> IO ()

{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @macros\/reparse.h:210:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args3 = hs_bindgen_12f19ea593aefd3f

-- | __unique:__ @test_macrosreparse_Example_Safe_const_pointers_args4@
foreign import ccall safe "hs_bindgen_5a50e98897696d57" hs_bindgen_5a50e98897696d57 ::
     A
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
  -> IO ()

{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @macros\/reparse.h:211:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args4 = hs_bindgen_5a50e98897696d57

-- | __unique:__ @test_macrosreparse_Example_Safe_const_pointers_args5@
foreign import ccall safe "hs_bindgen_666701f7cb61bd15" hs_bindgen_666701f7cb61bd15 ::
     A
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
  -> IO ()

{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @macros\/reparse.h:212:6@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_args5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args5 = hs_bindgen_666701f7cb61bd15

-- | __unique:__ @test_macrosreparse_Example_Safe_const_pointers_ret1@
foreign import ccall safe "hs_bindgen_b94fbc3dfd285563" hs_bindgen_b94fbc3dfd285563 ::
     A
  -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h:214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
const_pointers_ret1 = hs_bindgen_b94fbc3dfd285563

-- | __unique:__ @test_macrosreparse_Example_Safe_const_pointers_ret2@
foreign import ccall safe "hs_bindgen_33e2960e26b79450" hs_bindgen_33e2960e26b79450 ::
     A
  -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h:215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
const_pointers_ret2 = hs_bindgen_33e2960e26b79450

-- | __unique:__ @test_macrosreparse_Example_Safe_const_pointers_ret3@
foreign import ccall safe "hs_bindgen_50c6e2fe4f3fb777" hs_bindgen_50c6e2fe4f3fb777 ::
     A
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h:216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.Ptr FC.CInt)
const_pointers_ret3 = hs_bindgen_50c6e2fe4f3fb777

-- | __unique:__ @test_macrosreparse_Example_Safe_const_pointers_ret4@
foreign import ccall safe "hs_bindgen_edc014695d896c8d" hs_bindgen_edc014695d896c8d ::
     A
  -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h:217:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
const_pointers_ret4 = hs_bindgen_edc014695d896c8d

-- | __unique:__ @test_macrosreparse_Example_Safe_const_pointers_ret5@
foreign import ccall safe "hs_bindgen_6d3308cc5847f033" hs_bindgen_6d3308cc5847f033 ::
     A
  -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h:218:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
const_pointers_ret5 = hs_bindgen_6d3308cc5847f033

-- | __unique:__ @test_macrosreparse_Example_Safe_const_array_elem1@
foreign import ccall safe "hs_bindgen_678576320923a4d1" hs_bindgen_678576320923a4d1 ::
     HsBindgen.Runtime.ConstPtr.ConstPtr A
  -> IO ()

{-| Pointer-based API for 'const_array_elem1'
-}
const_array_elem1_wrapper ::
     HsBindgen.Runtime.ConstPtr.ConstPtr A
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem1_wrapper =
  hs_bindgen_678576320923a4d1

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h:246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 ::
     HsBindgen.Runtime.IncompleteArray.IncompleteArray A
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem1 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    hs_bindgen_678576320923a4d1 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr1))

-- | __unique:__ @test_macrosreparse_Example_Safe_const_array_elem2@
foreign import ccall safe "hs_bindgen_b317941dde4eeff2" hs_bindgen_b317941dde4eeff2 ::
     Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr A)
  -> IO ()

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h:247:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2 ::
     Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr A)
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem2 = hs_bindgen_b317941dde4eeff2

-- | __unique:__ @test_macrosreparse_Example_Safe_const_array_elem3@
foreign import ccall safe "hs_bindgen_707e602e6beb1bb6" hs_bindgen_707e602e6beb1bb6 ::
     HsBindgen.Runtime.ConstPtr.ConstPtr (Ptr.Ptr A)
  -> IO ()

{-| Pointer-based API for 'const_array_elem3'
-}
const_array_elem3_wrapper ::
     HsBindgen.Runtime.ConstPtr.ConstPtr (Ptr.Ptr A)
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem3_wrapper =
  hs_bindgen_707e602e6beb1bb6

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h:248:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 ::
     HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem3 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    hs_bindgen_707e602e6beb1bb6 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr1))

-- | __unique:__ @test_macrosreparse_Example_Safe_noParams1@
foreign import ccall safe "hs_bindgen_93fecb4eb766c262" hs_bindgen_93fecb4eb766c262 ::
     IO A

{-| Other examples we reparsed /incorrectly/ before language-c

__C declaration:__ @noParams1@

__defined at:__ @macros\/reparse.h:256:3@

__exported by:__ @macros\/reparse.h@
-}
noParams1 ::
     IO A
noParams1 = hs_bindgen_93fecb4eb766c262

-- | __unique:__ @test_macrosreparse_Example_Safe_noParams2@
foreign import ccall safe "hs_bindgen_4350965157c891f5" hs_bindgen_4350965157c891f5 ::
     IO A

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h:257:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2 ::
     IO A
noParams2 = hs_bindgen_4350965157c891f5

-- | __unique:__ @test_macrosreparse_Example_Safe_noParams3@
foreign import ccall safe "hs_bindgen_c4f59272a2b1c3b5" hs_bindgen_c4f59272a2b1c3b5 ::
     A
  -> Ptr.FunPtr (IO FC.CInt)
  -> IO ()

{-| __C declaration:__ @noParams3@

    __defined at:__ @macros\/reparse.h:258:6@

    __exported by:__ @macros\/reparse.h@
-}
noParams3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Ptr.FunPtr (IO FC.CInt)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
noParams3 = hs_bindgen_c4f59272a2b1c3b5

-- | __unique:__ @test_macrosreparse_Example_Safe_funptr_ret1@
foreign import ccall safe "hs_bindgen_387a04c01e23c320" hs_bindgen_387a04c01e23c320 ::
     A
  -> IO (Ptr.FunPtr (IO ()))

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h:262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (IO ()))
funptr_ret1 = hs_bindgen_387a04c01e23c320

-- | __unique:__ @test_macrosreparse_Example_Safe_funptr_ret2@
foreign import ccall safe "hs_bindgen_6f0c14cd3478dc19" hs_bindgen_6f0c14cd3478dc19 ::
     A
  -> IO (Ptr.FunPtr (IO FC.CInt))

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h:263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (IO FC.CInt))
funptr_ret2 = hs_bindgen_6f0c14cd3478dc19

-- | __unique:__ @test_macrosreparse_Example_Safe_funptr_ret3@
foreign import ccall safe "hs_bindgen_08e8661d277cf7be" hs_bindgen_08e8661d277cf7be ::
     A
  -> IO (Ptr.FunPtr (FC.CInt -> IO ()))

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h:264:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (FC.CInt -> IO ()))
funptr_ret3 = hs_bindgen_08e8661d277cf7be

-- | __unique:__ @test_macrosreparse_Example_Safe_funptr_ret4@
foreign import ccall safe "hs_bindgen_609b5d953b68da92" hs_bindgen_609b5d953b68da92 ::
     A
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar))

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h:265:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar))
funptr_ret4 = hs_bindgen_609b5d953b68da92

-- | __unique:__ @test_macrosreparse_Example_Safe_funptr_ret5@
foreign import ccall safe "hs_bindgen_13e6ae43abf40aee" hs_bindgen_13e6ae43abf40aee ::
     A
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h:269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))
funptr_ret5 = hs_bindgen_13e6ae43abf40aee

-- | __unique:__ @test_macrosreparse_Example_Safe_funptr_ret6@
foreign import ccall safe "hs_bindgen_a4a3a86f28ca6299" hs_bindgen_a4a3a86f28ca6299 ::
     A
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h:270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))
funptr_ret6 = hs_bindgen_a4a3a86f28ca6299

-- | __unique:__ @test_macrosreparse_Example_Safe_funptr_ret7@
foreign import ccall safe "hs_bindgen_eae9dff04c88d00b" hs_bindgen_eae9dff04c88d00b ::
     A
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h:271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))
funptr_ret7 = hs_bindgen_eae9dff04c88d00b

-- | __unique:__ @test_macrosreparse_Example_Safe_funptr_ret8@
foreign import ccall safe "hs_bindgen_894457d90a2fc8db" hs_bindgen_894457d90a2fc8db ::
     A
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h:272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))
funptr_ret8 = hs_bindgen_894457d90a2fc8db

-- | __unique:__ @test_macrosreparse_Example_Safe_funptr_ret9@
foreign import ccall safe "hs_bindgen_c893eb15ad9bc68c" hs_bindgen_c893eb15ad9bc68c ::
     A
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h:273:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))
funptr_ret9 = hs_bindgen_c893eb15ad9bc68c

-- | __unique:__ @test_macrosreparse_Example_Safe_funptr_ret10@
foreign import ccall safe "hs_bindgen_d96c258298a44b28" hs_bindgen_d96c258298a44b28 ::
     A
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h:274:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)))
funptr_ret10 = hs_bindgen_d96c258298a44b28
