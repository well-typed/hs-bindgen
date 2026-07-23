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
  , "void hs_bindgen_c6ccfbc5ac54d3e3 ("
  , "  signed int arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  (args_char1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1b7126ecf0f5ab6e ("
  , "  signed int arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  (args_char2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8ecc879598b6d3b0 ("
  , "  signed int arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  (args_char3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8e221273096c13a0 ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3de3762c82599242 ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_14352acc60454580 ("
  , "  signed int arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  (args_short3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_96096b97ae514d84 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d049eccfdf55015b ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9c1d57b596281f26 ("
  , "  signed int arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  (args_int3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_31684dbfb298f18c ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_705e74ceba937567 ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_193cb3a5430102f3 ("
  , "  signed int arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  (args_long3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f1b87ec98a54b583 ("
  , "  signed int arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  (args_float)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_da17ba710f71da80 ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  (args_double)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9e15dd8dbce9cd83 ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (args_bool1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_dc772c3e54b3ef41 ("
  , "  signed int arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  (args_struct)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_aabbe3419dcd3eb9 ("
  , "  signed int arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  (args_union)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_731297a830e4c2df ("
  , "  signed int arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  (args_enum)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_31fc55b0aea434ba ("
  , "  signed int arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  (args_pointer1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_35aa32eabe6cbe1e ("
  , "  signed int arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  (args_pointer2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c9941830193c9c20 ("
  , "  signed int arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  (args_pointer3)(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_56c5534530515933 (void)"
  , "{"
  , "  return (ret_A)();"
  , "}"
  , "char hs_bindgen_e36cc4960e15ece8 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char1)(arg1);"
  , "}"
  , "signed char hs_bindgen_39c01c6ee1b7364f ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char2)(arg1);"
  , "}"
  , "unsigned char hs_bindgen_6a35179874816c94 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char3)(arg1);"
  , "}"
  , "signed short hs_bindgen_9bb2bb254d5dc1c0 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short1)(arg1);"
  , "}"
  , "signed short hs_bindgen_6797eb02f4ac3ebe ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short2)(arg1);"
  , "}"
  , "unsigned short hs_bindgen_615bd7df3e0134a2 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short3)(arg1);"
  , "}"
  , "signed int hs_bindgen_4fe4545529e753b8 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int1)(arg1);"
  , "}"
  , "signed int hs_bindgen_f6e5d129c69c5afe ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int2)(arg1);"
  , "}"
  , "unsigned int hs_bindgen_9551ec72ba55d7a7 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int3)(arg1);"
  , "}"
  , "signed long hs_bindgen_e0d1c177d41a7ce8 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long1)(arg1);"
  , "}"
  , "signed long hs_bindgen_42fbed00ce6b7b08 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long2)(arg1);"
  , "}"
  , "unsigned long hs_bindgen_ed9a248d2e643a4b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long3)(arg1);"
  , "}"
  , "float hs_bindgen_ff0e9b4eaea0329a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_float)(arg1);"
  , "}"
  , "double hs_bindgen_164a83641655c836 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_double)(arg1);"
  , "}"
  , "_Bool hs_bindgen_53d6d79b1b401e62 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_bool1)(arg1);"
  , "}"
  , "void hs_bindgen_75308badf37284ce ("
  , "  signed int arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_struct)(arg1);"
  , "}"
  , "void hs_bindgen_b9ab05b4990fb8dd ("
  , "  signed int arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_union)(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_c46aafe01e3abbca ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_enum)(arg1);"
  , "}"
  , "signed int *hs_bindgen_afcc67a9788584d9 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer1)(arg1);"
  , "}"
  , "signed int **hs_bindgen_234d0979dbe93733 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer2)(arg1);"
  , "}"
  , "void *hs_bindgen_6f3f7c624556cfb3 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer3)(arg1);"
  , "}"
  , "signed int hs_bindgen_c9d515b8a02ca235 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (body1)(arg1);"
  , "}"
  , "signed int hs_bindgen_d6e10c899b553af9 (void)"
  , "{"
  , "  return (body2)();"
  , "}"
  , "void hs_bindgen_433860c3d6edf28b ("
  , "  signed int arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_float)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_8e44029c12a7dddb ("
  , "  signed int arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_double)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_0dc01ff2cd8fc565 ("
  , "  signed int arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_float)(arg1);"
  , "}"
  , "void hs_bindgen_8f2ebb486a336f80 ("
  , "  signed int arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_double)(arg1);"
  , "}"
  , "void hs_bindgen_26be4a823dc56e01 ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (bespoke_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6212b2750a2d5997 ("
  , "  signed int arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  (bespoke_args2)(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_e553247e80e3e159 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret1)(arg1);"
  , "}"
  , "size_t hs_bindgen_72a7f6cf38bcb00c ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret2)(arg1);"
  , "}"
  , "void hs_bindgen_dc7cacd7dbb61f43 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (arr_args1)(arg1);"
  , "}"
  , "void hs_bindgen_09d9010cbf3c8d41 ("
  , "  signed int **arg1"
  , ")"
  , "{"
  , "  (arr_args2)(arg1);"
  , "}"
  , "void hs_bindgen_37e1f318a8ae501c ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (arr_args3)(arg1);"
  , "}"
  , "void hs_bindgen_d9440a8b6d2848ff ("
  , "  signed int **arg1"
  , ")"
  , "{"
  , "  (arr_args4)(arg1);"
  , "}"
  , "void hs_bindgen_ca61df80135a11f9 ("
  , "  signed int arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2e803412f5250381 ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1839323a7de53e04 ("
  , "  signed int arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1966f50d5deb0a77 ("
  , "  signed int arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3eacaa3550feb706 ("
  , "  signed int arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args5)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_42f3fad04f4a62c7 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (comments1)(arg1);"
  , "}"
  , "void hs_bindgen_40fa2adcbadaabf4 ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6560ce91395cf6bf ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_fab04df1b737308d ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3762ecefd71c40d3 ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b3ecb163df7d32c8 ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_706270fd83faddb0 ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ff637beb32dc5931 ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3e0e5c2fa1b03a68 ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_81eefcf233fd167c ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1c849843004ee65e ("
  , "  signed int arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_c85d4a53a6e1e963 ("
  , "  signed int arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_796c19d2df50b65f ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0950b2ec97db5b00 ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_375085e61e85a2a8 ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2bb6a1d152a87f63 ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2753c469629474c2 ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_5c96f0d6c5fd53b1 ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e8d4acc6356ca49d ("
  , "  signed int arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_6762b5473d8d350a ("
  , "  signed int arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_55d5e45034c9f572 ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_70dae000d43d8228 ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_85e787c823b6c95f ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_efb5ca5187af5610 ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e306bbd2d4a374fc ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_960be6e90be0e7c1 ("
  , "  signed int arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_00950eca61f28e52 ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_96b40db09db83748 ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args5)(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_035d9da3770aeff8 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret1)(arg1);"
  , "}"
  , "signed int const *hs_bindgen_0d195040558fbef7 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret2)(arg1);"
  , "}"
  , "signed int *const hs_bindgen_9d5a35ded6d43b33 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret3)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_b00ca0cfd4037848 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret4)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_da92f752b03da901 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret5)(arg1);"
  , "}"
  , "void hs_bindgen_fdc3e3307cac5fd0 ("
  , "  signed int const *arg1"
  , ")"
  , "{"
  , "  (const_array_elem1)(arg1);"
  , "}"
  , "void hs_bindgen_cecdce9d64ac85d4 ("
  , "  signed int const **arg1"
  , ")"
  , "{"
  , "  (const_array_elem2)(arg1);"
  , "}"
  , "void hs_bindgen_9ee7979e72b4baf6 ("
  , "  signed int *const *arg1"
  , ")"
  , "{"
  , "  (const_array_elem3)(arg1);"
  , "}"
  , "signed int hs_bindgen_c6e158d421dda2ac (void)"
  , "{"
  , "  return (noParams1)();"
  , "}"
  , "signed int hs_bindgen_a34e536ea5931d76 (void)"
  , "{"
  , "  return (noParams2)();"
  , "}"
  , "void hs_bindgen_80f6772cd6dfe2a9 ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (noParams3)(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_8938b867c00625aa ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_03ba9af419f7c750 ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret2)(arg1);"
  , "}"
  , "void (*hs_bindgen_c53c44ca84d8cb81 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (funptr_ret3)(arg1);"
  , "}"
  , "char (*hs_bindgen_fb5431b884cc209e ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret4)(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_b45419cebf6cb43d ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret5)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_5c14b480ba3cab1e ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret6)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_b8f93b6ff6ce2b1c ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret7)(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_74e1d2bf9ed6b1c5 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret8)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_8e68b5e49b407e0b ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret9)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_1fc54cf36aa5566f ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret10)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_char1@
foreign import ccall unsafe "hs_bindgen_c6ccfbc5ac54d3e3" hs_bindgen_c6ccfbc5ac54d3e3_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_char1@
hs_bindgen_c6ccfbc5ac54d3e3 ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_c6ccfbc5ac54d3e3 =
  BG.fromFFIType hs_bindgen_c6ccfbc5ac54d3e3_base

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
args_char1 = hs_bindgen_c6ccfbc5ac54d3e3

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_char2@
foreign import ccall unsafe "hs_bindgen_1b7126ecf0f5ab6e" hs_bindgen_1b7126ecf0f5ab6e_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_char2@
hs_bindgen_1b7126ecf0f5ab6e ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_1b7126ecf0f5ab6e =
  BG.fromFFIType hs_bindgen_1b7126ecf0f5ab6e_base

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
args_char2 = hs_bindgen_1b7126ecf0f5ab6e

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_char3@
foreign import ccall unsafe "hs_bindgen_8ecc879598b6d3b0" hs_bindgen_8ecc879598b6d3b0_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_char3@
hs_bindgen_8ecc879598b6d3b0 ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_8ecc879598b6d3b0 =
  BG.fromFFIType hs_bindgen_8ecc879598b6d3b0_base

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
args_char3 = hs_bindgen_8ecc879598b6d3b0

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_short1@
foreign import ccall unsafe "hs_bindgen_8e221273096c13a0" hs_bindgen_8e221273096c13a0_base ::
     BG.Int32
  -> BG.Int16
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_short1@
hs_bindgen_8e221273096c13a0 ::
     BG.CInt
  -> BG.CShort
  -> IO ()
hs_bindgen_8e221273096c13a0 =
  BG.fromFFIType hs_bindgen_8e221273096c13a0_base

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
args_short1 = hs_bindgen_8e221273096c13a0

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_short2@
foreign import ccall unsafe "hs_bindgen_3de3762c82599242" hs_bindgen_3de3762c82599242_base ::
     BG.Int32
  -> BG.Int16
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_short2@
hs_bindgen_3de3762c82599242 ::
     BG.CInt
  -> BG.CShort
  -> IO ()
hs_bindgen_3de3762c82599242 =
  BG.fromFFIType hs_bindgen_3de3762c82599242_base

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
args_short2 = hs_bindgen_3de3762c82599242

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_short3@
foreign import ccall unsafe "hs_bindgen_14352acc60454580" hs_bindgen_14352acc60454580_base ::
     BG.Int32
  -> BG.Word16
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_short3@
hs_bindgen_14352acc60454580 ::
     BG.CInt
  -> BG.CUShort
  -> IO ()
hs_bindgen_14352acc60454580 =
  BG.fromFFIType hs_bindgen_14352acc60454580_base

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
args_short3 = hs_bindgen_14352acc60454580

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_int1@
foreign import ccall unsafe "hs_bindgen_96096b97ae514d84" hs_bindgen_96096b97ae514d84_base ::
     BG.Int32
  -> BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_int1@
hs_bindgen_96096b97ae514d84 ::
     BG.CInt
  -> BG.CInt
  -> IO ()
hs_bindgen_96096b97ae514d84 =
  BG.fromFFIType hs_bindgen_96096b97ae514d84_base

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
args_int1 = hs_bindgen_96096b97ae514d84

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_int2@
foreign import ccall unsafe "hs_bindgen_d049eccfdf55015b" hs_bindgen_d049eccfdf55015b_base ::
     BG.Int32
  -> BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_int2@
hs_bindgen_d049eccfdf55015b ::
     BG.CInt
  -> BG.CInt
  -> IO ()
hs_bindgen_d049eccfdf55015b =
  BG.fromFFIType hs_bindgen_d049eccfdf55015b_base

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
args_int2 = hs_bindgen_d049eccfdf55015b

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_int3@
foreign import ccall unsafe "hs_bindgen_9c1d57b596281f26" hs_bindgen_9c1d57b596281f26_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_int3@
hs_bindgen_9c1d57b596281f26 ::
     BG.CInt
  -> BG.CUInt
  -> IO ()
hs_bindgen_9c1d57b596281f26 =
  BG.fromFFIType hs_bindgen_9c1d57b596281f26_base

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
args_int3 = hs_bindgen_9c1d57b596281f26

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_long1@
foreign import ccall unsafe "hs_bindgen_31684dbfb298f18c" hs_bindgen_31684dbfb298f18c_base ::
     BG.Int32
  -> BG.Int64
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_long1@
hs_bindgen_31684dbfb298f18c ::
     BG.CInt
  -> BG.CLong
  -> IO ()
hs_bindgen_31684dbfb298f18c =
  BG.fromFFIType hs_bindgen_31684dbfb298f18c_base

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
args_long1 = hs_bindgen_31684dbfb298f18c

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_long2@
foreign import ccall unsafe "hs_bindgen_705e74ceba937567" hs_bindgen_705e74ceba937567_base ::
     BG.Int32
  -> BG.Int64
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_long2@
hs_bindgen_705e74ceba937567 ::
     BG.CInt
  -> BG.CLong
  -> IO ()
hs_bindgen_705e74ceba937567 =
  BG.fromFFIType hs_bindgen_705e74ceba937567_base

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
args_long2 = hs_bindgen_705e74ceba937567

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_long3@
foreign import ccall unsafe "hs_bindgen_193cb3a5430102f3" hs_bindgen_193cb3a5430102f3_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_long3@
hs_bindgen_193cb3a5430102f3 ::
     BG.CInt
  -> BG.CULong
  -> IO ()
hs_bindgen_193cb3a5430102f3 =
  BG.fromFFIType hs_bindgen_193cb3a5430102f3_base

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
args_long3 = hs_bindgen_193cb3a5430102f3

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_float@
foreign import ccall unsafe "hs_bindgen_f1b87ec98a54b583" hs_bindgen_f1b87ec98a54b583_base ::
     BG.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_float@
hs_bindgen_f1b87ec98a54b583 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_f1b87ec98a54b583 =
  BG.fromFFIType hs_bindgen_f1b87ec98a54b583_base

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
args_float = hs_bindgen_f1b87ec98a54b583

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_double@
foreign import ccall unsafe "hs_bindgen_da17ba710f71da80" hs_bindgen_da17ba710f71da80_base ::
     BG.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_double@
hs_bindgen_da17ba710f71da80 ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_da17ba710f71da80 =
  BG.fromFFIType hs_bindgen_da17ba710f71da80_base

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
args_double = hs_bindgen_da17ba710f71da80

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_bool1@
foreign import ccall unsafe "hs_bindgen_9e15dd8dbce9cd83" hs_bindgen_9e15dd8dbce9cd83_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_bool1@
hs_bindgen_9e15dd8dbce9cd83 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_9e15dd8dbce9cd83 =
  BG.fromFFIType hs_bindgen_9e15dd8dbce9cd83_base

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
args_bool1 = hs_bindgen_9e15dd8dbce9cd83

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_struct@
foreign import ccall unsafe "hs_bindgen_dc772c3e54b3ef41" hs_bindgen_dc772c3e54b3ef41_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_struct@
hs_bindgen_dc772c3e54b3ef41 ::
     BG.CInt
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_dc772c3e54b3ef41 =
  BG.fromFFIType hs_bindgen_dc772c3e54b3ef41_base

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
                       hs_bindgen_dc772c3e54b3ef41 arg10 arg22)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_union@
foreign import ccall unsafe "hs_bindgen_aabbe3419dcd3eb9" hs_bindgen_aabbe3419dcd3eb9_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_union@
hs_bindgen_aabbe3419dcd3eb9 ::
     BG.CInt
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_aabbe3419dcd3eb9 =
  BG.fromFFIType hs_bindgen_aabbe3419dcd3eb9_base

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
                       hs_bindgen_aabbe3419dcd3eb9 arg10 arg22)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_enum@
foreign import ccall unsafe "hs_bindgen_731297a830e4c2df" hs_bindgen_731297a830e4c2df_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_enum@
hs_bindgen_731297a830e4c2df ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_731297a830e4c2df =
  BG.fromFFIType hs_bindgen_731297a830e4c2df_base

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
args_enum = hs_bindgen_731297a830e4c2df

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_pointer1@
foreign import ccall unsafe "hs_bindgen_31fc55b0aea434ba" hs_bindgen_31fc55b0aea434ba_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_pointer1@
hs_bindgen_31fc55b0aea434ba ::
     BG.CInt
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_31fc55b0aea434ba =
  BG.fromFFIType hs_bindgen_31fc55b0aea434ba_base

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
args_pointer1 = hs_bindgen_31fc55b0aea434ba

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_pointer2@
foreign import ccall unsafe "hs_bindgen_35aa32eabe6cbe1e" hs_bindgen_35aa32eabe6cbe1e_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_pointer2@
hs_bindgen_35aa32eabe6cbe1e ::
     BG.CInt
  -> BG.Ptr (BG.Ptr BG.CInt)
  -> IO ()
hs_bindgen_35aa32eabe6cbe1e =
  BG.fromFFIType hs_bindgen_35aa32eabe6cbe1e_base

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
args_pointer2 = hs_bindgen_35aa32eabe6cbe1e

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_pointer3@
foreign import ccall unsafe "hs_bindgen_c9941830193c9c20" hs_bindgen_c9941830193c9c20_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_pointer3@
hs_bindgen_c9941830193c9c20 ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()
hs_bindgen_c9941830193c9c20 =
  BG.fromFFIType hs_bindgen_c9941830193c9c20_base

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
args_pointer3 = hs_bindgen_c9941830193c9c20

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_A@
foreign import ccall unsafe "hs_bindgen_56c5534530515933" hs_bindgen_56c5534530515933_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_A@
hs_bindgen_56c5534530515933 :: IO BG.CInt
hs_bindgen_56c5534530515933 =
  BG.fromFFIType hs_bindgen_56c5534530515933_base

{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h 47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A :: IO BG.CInt
ret_A = hs_bindgen_56c5534530515933

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_char1@
foreign import ccall unsafe "hs_bindgen_e36cc4960e15ece8" hs_bindgen_e36cc4960e15ece8_base ::
     BG.Int32
  -> IO BG.Int8

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_char1@
hs_bindgen_e36cc4960e15ece8 ::
     BG.CInt
  -> IO BG.CChar
hs_bindgen_e36cc4960e15ece8 =
  BG.fromFFIType hs_bindgen_e36cc4960e15ece8_base

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h 49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CChar
ret_char1 = hs_bindgen_e36cc4960e15ece8

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_char2@
foreign import ccall unsafe "hs_bindgen_39c01c6ee1b7364f" hs_bindgen_39c01c6ee1b7364f_base ::
     BG.Int32
  -> IO BG.Int8

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_char2@
hs_bindgen_39c01c6ee1b7364f ::
     BG.CInt
  -> IO BG.CSChar
hs_bindgen_39c01c6ee1b7364f =
  BG.fromFFIType hs_bindgen_39c01c6ee1b7364f_base

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h 50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CSChar
ret_char2 = hs_bindgen_39c01c6ee1b7364f

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_char3@
foreign import ccall unsafe "hs_bindgen_6a35179874816c94" hs_bindgen_6a35179874816c94_base ::
     BG.Int32
  -> IO BG.Word8

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_char3@
hs_bindgen_6a35179874816c94 ::
     BG.CInt
  -> IO BG.CUChar
hs_bindgen_6a35179874816c94 =
  BG.fromFFIType hs_bindgen_6a35179874816c94_base

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h 51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUChar
ret_char3 = hs_bindgen_6a35179874816c94

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_short1@
foreign import ccall unsafe "hs_bindgen_9bb2bb254d5dc1c0" hs_bindgen_9bb2bb254d5dc1c0_base ::
     BG.Int32
  -> IO BG.Int16

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_short1@
hs_bindgen_9bb2bb254d5dc1c0 ::
     BG.CInt
  -> IO BG.CShort
hs_bindgen_9bb2bb254d5dc1c0 =
  BG.fromFFIType hs_bindgen_9bb2bb254d5dc1c0_base

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h 53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short1 = hs_bindgen_9bb2bb254d5dc1c0

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_short2@
foreign import ccall unsafe "hs_bindgen_6797eb02f4ac3ebe" hs_bindgen_6797eb02f4ac3ebe_base ::
     BG.Int32
  -> IO BG.Int16

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_short2@
hs_bindgen_6797eb02f4ac3ebe ::
     BG.CInt
  -> IO BG.CShort
hs_bindgen_6797eb02f4ac3ebe =
  BG.fromFFIType hs_bindgen_6797eb02f4ac3ebe_base

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h 54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short2 = hs_bindgen_6797eb02f4ac3ebe

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_short3@
foreign import ccall unsafe "hs_bindgen_615bd7df3e0134a2" hs_bindgen_615bd7df3e0134a2_base ::
     BG.Int32
  -> IO BG.Word16

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_short3@
hs_bindgen_615bd7df3e0134a2 ::
     BG.CInt
  -> IO BG.CUShort
hs_bindgen_615bd7df3e0134a2 =
  BG.fromFFIType hs_bindgen_615bd7df3e0134a2_base

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h 55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUShort
ret_short3 = hs_bindgen_615bd7df3e0134a2

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_int1@
foreign import ccall unsafe "hs_bindgen_4fe4545529e753b8" hs_bindgen_4fe4545529e753b8_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_int1@
hs_bindgen_4fe4545529e753b8 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_4fe4545529e753b8 =
  BG.fromFFIType hs_bindgen_4fe4545529e753b8_base

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h 57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int1 = hs_bindgen_4fe4545529e753b8

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_int2@
foreign import ccall unsafe "hs_bindgen_f6e5d129c69c5afe" hs_bindgen_f6e5d129c69c5afe_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_int2@
hs_bindgen_f6e5d129c69c5afe ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_f6e5d129c69c5afe =
  BG.fromFFIType hs_bindgen_f6e5d129c69c5afe_base

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h 58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int2 = hs_bindgen_f6e5d129c69c5afe

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_int3@
foreign import ccall unsafe "hs_bindgen_9551ec72ba55d7a7" hs_bindgen_9551ec72ba55d7a7_base ::
     BG.Int32
  -> IO BG.Word32

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_int3@
hs_bindgen_9551ec72ba55d7a7 ::
     BG.CInt
  -> IO BG.CUInt
hs_bindgen_9551ec72ba55d7a7 =
  BG.fromFFIType hs_bindgen_9551ec72ba55d7a7_base

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h 59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUInt
ret_int3 = hs_bindgen_9551ec72ba55d7a7

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_long1@
foreign import ccall unsafe "hs_bindgen_e0d1c177d41a7ce8" hs_bindgen_e0d1c177d41a7ce8_base ::
     BG.Int32
  -> IO BG.Int64

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_long1@
hs_bindgen_e0d1c177d41a7ce8 ::
     BG.CInt
  -> IO BG.CLong
hs_bindgen_e0d1c177d41a7ce8 =
  BG.fromFFIType hs_bindgen_e0d1c177d41a7ce8_base

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h 61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long1 = hs_bindgen_e0d1c177d41a7ce8

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_long2@
foreign import ccall unsafe "hs_bindgen_42fbed00ce6b7b08" hs_bindgen_42fbed00ce6b7b08_base ::
     BG.Int32
  -> IO BG.Int64

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_long2@
hs_bindgen_42fbed00ce6b7b08 ::
     BG.CInt
  -> IO BG.CLong
hs_bindgen_42fbed00ce6b7b08 =
  BG.fromFFIType hs_bindgen_42fbed00ce6b7b08_base

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h 62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long2 = hs_bindgen_42fbed00ce6b7b08

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_long3@
foreign import ccall unsafe "hs_bindgen_ed9a248d2e643a4b" hs_bindgen_ed9a248d2e643a4b_base ::
     BG.Int32
  -> IO BG.Word64

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_long3@
hs_bindgen_ed9a248d2e643a4b ::
     BG.CInt
  -> IO BG.CULong
hs_bindgen_ed9a248d2e643a4b =
  BG.fromFFIType hs_bindgen_ed9a248d2e643a4b_base

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h 63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CULong
ret_long3 = hs_bindgen_ed9a248d2e643a4b

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_float@
foreign import ccall unsafe "hs_bindgen_ff0e9b4eaea0329a" hs_bindgen_ff0e9b4eaea0329a_base ::
     BG.Int32
  -> IO Float

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_float@
hs_bindgen_ff0e9b4eaea0329a ::
     BG.CInt
  -> IO BG.CFloat
hs_bindgen_ff0e9b4eaea0329a =
  BG.fromFFIType hs_bindgen_ff0e9b4eaea0329a_base

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h 65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CFloat
ret_float = hs_bindgen_ff0e9b4eaea0329a

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_double@
foreign import ccall unsafe "hs_bindgen_164a83641655c836" hs_bindgen_164a83641655c836_base ::
     BG.Int32
  -> IO Double

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_double@
hs_bindgen_164a83641655c836 ::
     BG.CInt
  -> IO BG.CDouble
hs_bindgen_164a83641655c836 =
  BG.fromFFIType hs_bindgen_164a83641655c836_base

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h 66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CDouble
ret_double = hs_bindgen_164a83641655c836

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_bool1@
foreign import ccall unsafe "hs_bindgen_53d6d79b1b401e62" hs_bindgen_53d6d79b1b401e62_base ::
     BG.Int32
  -> IO BG.Word8

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_bool1@
hs_bindgen_53d6d79b1b401e62 ::
     BG.CInt
  -> IO BG.CBool
hs_bindgen_53d6d79b1b401e62 =
  BG.fromFFIType hs_bindgen_53d6d79b1b401e62_base

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h 67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
ret_bool1 = hs_bindgen_53d6d79b1b401e62

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_struct@
foreign import ccall unsafe "hs_bindgen_75308badf37284ce" hs_bindgen_75308badf37284ce_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_struct@
hs_bindgen_75308badf37284ce ::
     BG.CInt
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_75308badf37284ce =
  BG.fromFFIType hs_bindgen_75308badf37284ce_base

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
                        hs_bindgen_75308badf37284ce arg10 res1)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_union@
foreign import ccall unsafe "hs_bindgen_b9ab05b4990fb8dd" hs_bindgen_b9ab05b4990fb8dd_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_union@
hs_bindgen_b9ab05b4990fb8dd ::
     BG.CInt
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_b9ab05b4990fb8dd =
  BG.fromFFIType hs_bindgen_b9ab05b4990fb8dd_base

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
                        hs_bindgen_b9ab05b4990fb8dd arg10 res1)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_enum@
foreign import ccall unsafe "hs_bindgen_c46aafe01e3abbca" hs_bindgen_c46aafe01e3abbca_base ::
     BG.Int32
  -> IO BG.Word32

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_enum@
hs_bindgen_c46aafe01e3abbca ::
     BG.CInt
  -> IO Some_enum
hs_bindgen_c46aafe01e3abbca =
  BG.fromFFIType hs_bindgen_c46aafe01e3abbca_base

{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h 71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO Some_enum
ret_enum = hs_bindgen_c46aafe01e3abbca

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_pointer1@
foreign import ccall unsafe "hs_bindgen_afcc67a9788584d9" hs_bindgen_afcc67a9788584d9_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_pointer1@
hs_bindgen_afcc67a9788584d9 ::
     BG.CInt
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_afcc67a9788584d9 =
  BG.fromFFIType hs_bindgen_afcc67a9788584d9_base

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h 73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
ret_pointer1 = hs_bindgen_afcc67a9788584d9

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_pointer2@
foreign import ccall unsafe "hs_bindgen_234d0979dbe93733" hs_bindgen_234d0979dbe93733_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_pointer2@
hs_bindgen_234d0979dbe93733 ::
     BG.CInt
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
hs_bindgen_234d0979dbe93733 =
  BG.fromFFIType hs_bindgen_234d0979dbe93733_base

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h 74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
ret_pointer2 = hs_bindgen_234d0979dbe93733

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_pointer3@
foreign import ccall unsafe "hs_bindgen_6f3f7c624556cfb3" hs_bindgen_6f3f7c624556cfb3_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_pointer3@
hs_bindgen_6f3f7c624556cfb3 ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)
hs_bindgen_6f3f7c624556cfb3 =
  BG.fromFFIType hs_bindgen_6f3f7c624556cfb3_base

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h 75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.Void)
ret_pointer3 = hs_bindgen_6f3f7c624556cfb3

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_body1@
foreign import ccall unsafe "hs_bindgen_c9d515b8a02ca235" hs_bindgen_c9d515b8a02ca235_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_body1@
hs_bindgen_c9d515b8a02ca235 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_c9d515b8a02ca235 =
  BG.fromFFIType hs_bindgen_c9d515b8a02ca235_base

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h 79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
body1 = hs_bindgen_c9d515b8a02ca235

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_body2@
foreign import ccall unsafe "hs_bindgen_d6e10c899b553af9" hs_bindgen_d6e10c899b553af9_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_body2@
hs_bindgen_d6e10c899b553af9 :: IO BG.CInt
hs_bindgen_d6e10c899b553af9 =
  BG.fromFFIType hs_bindgen_d6e10c899b553af9_base

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h 80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2 :: IO BG.CInt
body2 = hs_bindgen_d6e10c899b553af9

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_complex_float@
foreign import ccall unsafe "hs_bindgen_433860c3d6edf28b" hs_bindgen_433860c3d6edf28b_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_complex_float@
hs_bindgen_433860c3d6edf28b ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_433860c3d6edf28b =
  BG.fromFFIType hs_bindgen_433860c3d6edf28b_base

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
                       hs_bindgen_433860c3d6edf28b arg10 arg22)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_complex_double@
foreign import ccall unsafe "hs_bindgen_8e44029c12a7dddb" hs_bindgen_8e44029c12a7dddb_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_args_complex_double@
hs_bindgen_8e44029c12a7dddb ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_8e44029c12a7dddb =
  BG.fromFFIType hs_bindgen_8e44029c12a7dddb_base

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
                       hs_bindgen_8e44029c12a7dddb arg10 arg22)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_complex_float@
foreign import ccall unsafe "hs_bindgen_0dc01ff2cd8fc565" hs_bindgen_0dc01ff2cd8fc565_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_complex_float@
hs_bindgen_0dc01ff2cd8fc565 ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_0dc01ff2cd8fc565 =
  BG.fromFFIType hs_bindgen_0dc01ff2cd8fc565_base

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
                        hs_bindgen_0dc01ff2cd8fc565 arg10 res1)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_complex_double@
foreign import ccall unsafe "hs_bindgen_8f2ebb486a336f80" hs_bindgen_8f2ebb486a336f80_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_ret_complex_double@
hs_bindgen_8f2ebb486a336f80 ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_8f2ebb486a336f80 =
  BG.fromFFIType hs_bindgen_8f2ebb486a336f80_base

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
                        hs_bindgen_8f2ebb486a336f80 arg10 res1)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_bespoke_args1@
foreign import ccall unsafe "hs_bindgen_26be4a823dc56e01" hs_bindgen_26be4a823dc56e01_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_bespoke_args1@
hs_bindgen_26be4a823dc56e01 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_26be4a823dc56e01 =
  BG.fromFFIType hs_bindgen_26be4a823dc56e01_base

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
bespoke_args1 = hs_bindgen_26be4a823dc56e01

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_bespoke_args2@
foreign import ccall unsafe "hs_bindgen_6212b2750a2d5997" hs_bindgen_6212b2750a2d5997_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_bespoke_args2@
hs_bindgen_6212b2750a2d5997 ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_6212b2750a2d5997 =
  BG.fromFFIType hs_bindgen_6212b2750a2d5997_base

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
bespoke_args2 = hs_bindgen_6212b2750a2d5997

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_bespoke_ret1@
foreign import ccall unsafe "hs_bindgen_e553247e80e3e159" hs_bindgen_e553247e80e3e159_base ::
     BG.Int32
  -> IO BG.Word8

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_bespoke_ret1@
hs_bindgen_e553247e80e3e159 ::
     BG.CInt
  -> IO BG.CBool
hs_bindgen_e553247e80e3e159 =
  BG.fromFFIType hs_bindgen_e553247e80e3e159_base

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h 97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
bespoke_ret1 = hs_bindgen_e553247e80e3e159

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_bespoke_ret2@
foreign import ccall unsafe "hs_bindgen_72a7f6cf38bcb00c" hs_bindgen_72a7f6cf38bcb00c_base ::
     BG.Int32
  -> IO BG.Word64

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_bespoke_ret2@
hs_bindgen_72a7f6cf38bcb00c ::
     BG.CInt
  -> IO HsBindgen.Runtime.LibC.CSize
hs_bindgen_72a7f6cf38bcb00c =
  BG.fromFFIType hs_bindgen_72a7f6cf38bcb00c_base

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h 98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO HsBindgen.Runtime.LibC.CSize
bespoke_ret2 = hs_bindgen_72a7f6cf38bcb00c

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_arr_args1@
foreign import ccall unsafe "hs_bindgen_dc7cacd7dbb61f43" hs_bindgen_dc7cacd7dbb61f43_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_arr_args1@
hs_bindgen_dc7cacd7dbb61f43 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt))
  -> IO ()
hs_bindgen_dc7cacd7dbb61f43 =
  BG.fromFFIType hs_bindgen_dc7cacd7dbb61f43_base

{-| Arrays

    __C declaration:__ @arr_args1@

    __defined at:__ @macros\/reparse.h 104:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args1 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args1 = hs_bindgen_dc7cacd7dbb61f43

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_arr_args2@
foreign import ccall unsafe "hs_bindgen_09d9010cbf3c8d41" hs_bindgen_09d9010cbf3c8d41_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_arr_args2@
hs_bindgen_09d9010cbf3c8d41 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_09d9010cbf3c8d41 =
  BG.fromFFIType hs_bindgen_09d9010cbf3c8d41_base

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h 105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args2 = hs_bindgen_09d9010cbf3c8d41

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_arr_args3@
foreign import ccall unsafe "hs_bindgen_37e1f318a8ae501c" hs_bindgen_37e1f318a8ae501c_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_arr_args3@
hs_bindgen_37e1f318a8ae501c ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt))
  -> IO ()
hs_bindgen_37e1f318a8ae501c =
  BG.fromFFIType hs_bindgen_37e1f318a8ae501c_base

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h 106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args3 = hs_bindgen_37e1f318a8ae501c

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_arr_args4@
foreign import ccall unsafe "hs_bindgen_d9440a8b6d2848ff" hs_bindgen_d9440a8b6d2848ff_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_arr_args4@
hs_bindgen_d9440a8b6d2848ff ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_d9440a8b6d2848ff =
  BG.fromFFIType hs_bindgen_d9440a8b6d2848ff_base

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h 107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args4 = hs_bindgen_d9440a8b6d2848ff

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_args1@
foreign import ccall unsafe "hs_bindgen_ca61df80135a11f9" hs_bindgen_ca61df80135a11f9_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_args1@
hs_bindgen_ca61df80135a11f9 ::
     BG.CInt
  -> BG.FunPtr (IO ())
  -> IO ()
hs_bindgen_ca61df80135a11f9 =
  BG.fromFFIType hs_bindgen_ca61df80135a11f9_base

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
funptr_args1 = hs_bindgen_ca61df80135a11f9

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_args2@
foreign import ccall unsafe "hs_bindgen_2e803412f5250381" hs_bindgen_2e803412f5250381_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_args2@
hs_bindgen_2e803412f5250381 ::
     BG.CInt
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_2e803412f5250381 =
  BG.fromFFIType hs_bindgen_2e803412f5250381_base

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
funptr_args2 = hs_bindgen_2e803412f5250381

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_args3@
foreign import ccall unsafe "hs_bindgen_1839323a7de53e04" hs_bindgen_1839323a7de53e04_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_args3@
hs_bindgen_1839323a7de53e04 ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> IO ())
  -> IO ()
hs_bindgen_1839323a7de53e04 =
  BG.fromFFIType hs_bindgen_1839323a7de53e04_base

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
funptr_args3 = hs_bindgen_1839323a7de53e04

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_args4@
foreign import ccall unsafe "hs_bindgen_1966f50d5deb0a77" hs_bindgen_1966f50d5deb0a77_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_args4@
hs_bindgen_1966f50d5deb0a77 ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar)
  -> IO ()
hs_bindgen_1966f50d5deb0a77 =
  BG.fromFFIType hs_bindgen_1966f50d5deb0a77_base

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
funptr_args4 = hs_bindgen_1966f50d5deb0a77

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_args5@
foreign import ccall unsafe "hs_bindgen_3eacaa3550feb706" hs_bindgen_3eacaa3550feb706_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_args5@
hs_bindgen_3eacaa3550feb706 ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))
  -> IO ()
hs_bindgen_3eacaa3550feb706 =
  BG.fromFFIType hs_bindgen_3eacaa3550feb706_base

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
funptr_args5 = hs_bindgen_3eacaa3550feb706

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_comments1@
foreign import ccall unsafe "hs_bindgen_42f3fad04f4a62c7" hs_bindgen_42f3fad04f4a62c7_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_comments1@
hs_bindgen_42f3fad04f4a62c7 ::
     BG.CInt
  -> IO ()
hs_bindgen_42f3fad04f4a62c7 =
  BG.fromFFIType hs_bindgen_42f3fad04f4a62c7_base

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
comments1 = hs_bindgen_42f3fad04f4a62c7

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_prim_before1@
foreign import ccall unsafe "hs_bindgen_40fa2adcbadaabf4" hs_bindgen_40fa2adcbadaabf4_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_prim_before1@
hs_bindgen_40fa2adcbadaabf4 ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_40fa2adcbadaabf4 =
  BG.fromFFIType hs_bindgen_40fa2adcbadaabf4_base

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
const_prim_before1 = hs_bindgen_40fa2adcbadaabf4

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_prim_before2@
foreign import ccall unsafe "hs_bindgen_6560ce91395cf6bf" hs_bindgen_6560ce91395cf6bf_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_prim_before2@
hs_bindgen_6560ce91395cf6bf ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_6560ce91395cf6bf =
  BG.fromFFIType hs_bindgen_6560ce91395cf6bf_base

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
const_prim_before2 = hs_bindgen_6560ce91395cf6bf

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_prim_before3@
foreign import ccall unsafe "hs_bindgen_fab04df1b737308d" hs_bindgen_fab04df1b737308d_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_prim_before3@
hs_bindgen_fab04df1b737308d ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_fab04df1b737308d =
  BG.fromFFIType hs_bindgen_fab04df1b737308d_base

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
const_prim_before3 = hs_bindgen_fab04df1b737308d

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_prim_after1@
foreign import ccall unsafe "hs_bindgen_3762ecefd71c40d3" hs_bindgen_3762ecefd71c40d3_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_prim_after1@
hs_bindgen_3762ecefd71c40d3 ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_3762ecefd71c40d3 =
  BG.fromFFIType hs_bindgen_3762ecefd71c40d3_base

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
const_prim_after1 = hs_bindgen_3762ecefd71c40d3

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_prim_after2@
foreign import ccall unsafe "hs_bindgen_b3ecb163df7d32c8" hs_bindgen_b3ecb163df7d32c8_base ::
     BG.Int32
  -> BG.Int8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_prim_after2@
hs_bindgen_b3ecb163df7d32c8 ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_b3ecb163df7d32c8 =
  BG.fromFFIType hs_bindgen_b3ecb163df7d32c8_base

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
const_prim_after2 = hs_bindgen_b3ecb163df7d32c8

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_prim_after3@
foreign import ccall unsafe "hs_bindgen_706270fd83faddb0" hs_bindgen_706270fd83faddb0_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_prim_after3@
hs_bindgen_706270fd83faddb0 ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_706270fd83faddb0 =
  BG.fromFFIType hs_bindgen_706270fd83faddb0_base

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
const_prim_after3 = hs_bindgen_706270fd83faddb0

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_before1@
foreign import ccall unsafe "hs_bindgen_ff637beb32dc5931" hs_bindgen_ff637beb32dc5931_base ::
     BG.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_before1@
hs_bindgen_ff637beb32dc5931 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_ff637beb32dc5931 =
  BG.fromFFIType hs_bindgen_ff637beb32dc5931_base

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
  hs_bindgen_ff637beb32dc5931

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_before2@
foreign import ccall unsafe "hs_bindgen_3e0e5c2fa1b03a68" hs_bindgen_3e0e5c2fa1b03a68_base ::
     BG.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_before2@
hs_bindgen_3e0e5c2fa1b03a68 ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_3e0e5c2fa1b03a68 =
  BG.fromFFIType hs_bindgen_3e0e5c2fa1b03a68_base

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
  hs_bindgen_3e0e5c2fa1b03a68

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_before3@
foreign import ccall unsafe "hs_bindgen_81eefcf233fd167c" hs_bindgen_81eefcf233fd167c_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_before3@
hs_bindgen_81eefcf233fd167c ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_81eefcf233fd167c =
  BG.fromFFIType hs_bindgen_81eefcf233fd167c_base

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
  hs_bindgen_81eefcf233fd167c

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_before4@
foreign import ccall unsafe "hs_bindgen_1c849843004ee65e" hs_bindgen_1c849843004ee65e_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_before4@
hs_bindgen_1c849843004ee65e ::
     BG.CInt
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_1c849843004ee65e =
  BG.fromFFIType hs_bindgen_1c849843004ee65e_base

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
                       hs_bindgen_1c849843004ee65e arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_before5@
foreign import ccall unsafe "hs_bindgen_c85d4a53a6e1e963" hs_bindgen_c85d4a53a6e1e963_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_before5@
hs_bindgen_c85d4a53a6e1e963 ::
     BG.CInt
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_c85d4a53a6e1e963 =
  BG.fromFFIType hs_bindgen_c85d4a53a6e1e963_base

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
                       hs_bindgen_c85d4a53a6e1e963 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_before6@
foreign import ccall unsafe "hs_bindgen_796c19d2df50b65f" hs_bindgen_796c19d2df50b65f_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_before6@
hs_bindgen_796c19d2df50b65f ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_796c19d2df50b65f =
  BG.fromFFIType hs_bindgen_796c19d2df50b65f_base

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
  hs_bindgen_796c19d2df50b65f

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_before7@
foreign import ccall unsafe "hs_bindgen_0950b2ec97db5b00" hs_bindgen_0950b2ec97db5b00_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_before7@
hs_bindgen_0950b2ec97db5b00 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_0950b2ec97db5b00 =
  BG.fromFFIType hs_bindgen_0950b2ec97db5b00_base

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
  hs_bindgen_0950b2ec97db5b00

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_before8@
foreign import ccall unsafe "hs_bindgen_375085e61e85a2a8" hs_bindgen_375085e61e85a2a8_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_before8@
hs_bindgen_375085e61e85a2a8 ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_375085e61e85a2a8 =
  BG.fromFFIType hs_bindgen_375085e61e85a2a8_base

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
  hs_bindgen_375085e61e85a2a8

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_after1@
foreign import ccall unsafe "hs_bindgen_2bb6a1d152a87f63" hs_bindgen_2bb6a1d152a87f63_base ::
     BG.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_after1@
hs_bindgen_2bb6a1d152a87f63 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_2bb6a1d152a87f63 =
  BG.fromFFIType hs_bindgen_2bb6a1d152a87f63_base

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
  hs_bindgen_2bb6a1d152a87f63

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_after2@
foreign import ccall unsafe "hs_bindgen_2753c469629474c2" hs_bindgen_2753c469629474c2_base ::
     BG.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_after2@
hs_bindgen_2753c469629474c2 ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_2753c469629474c2 =
  BG.fromFFIType hs_bindgen_2753c469629474c2_base

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
  hs_bindgen_2753c469629474c2

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_after3@
foreign import ccall unsafe "hs_bindgen_5c96f0d6c5fd53b1" hs_bindgen_5c96f0d6c5fd53b1_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_after3@
hs_bindgen_5c96f0d6c5fd53b1 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_5c96f0d6c5fd53b1 =
  BG.fromFFIType hs_bindgen_5c96f0d6c5fd53b1_base

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
  hs_bindgen_5c96f0d6c5fd53b1

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_after4@
foreign import ccall unsafe "hs_bindgen_e8d4acc6356ca49d" hs_bindgen_e8d4acc6356ca49d_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_after4@
hs_bindgen_e8d4acc6356ca49d ::
     BG.CInt
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_e8d4acc6356ca49d =
  BG.fromFFIType hs_bindgen_e8d4acc6356ca49d_base

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
                       hs_bindgen_e8d4acc6356ca49d arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_after5@
foreign import ccall unsafe "hs_bindgen_6762b5473d8d350a" hs_bindgen_6762b5473d8d350a_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_after5@
hs_bindgen_6762b5473d8d350a ::
     BG.CInt
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_6762b5473d8d350a =
  BG.fromFFIType hs_bindgen_6762b5473d8d350a_base

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
                       hs_bindgen_6762b5473d8d350a arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_after6@
foreign import ccall unsafe "hs_bindgen_55d5e45034c9f572" hs_bindgen_55d5e45034c9f572_base ::
     BG.Int32
  -> BG.Word32
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_after6@
hs_bindgen_55d5e45034c9f572 ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_55d5e45034c9f572 =
  BG.fromFFIType hs_bindgen_55d5e45034c9f572_base

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
  hs_bindgen_55d5e45034c9f572

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_after7@
foreign import ccall unsafe "hs_bindgen_70dae000d43d8228" hs_bindgen_70dae000d43d8228_base ::
     BG.Int32
  -> BG.Word8
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_after7@
hs_bindgen_70dae000d43d8228 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_70dae000d43d8228 =
  BG.fromFFIType hs_bindgen_70dae000d43d8228_base

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
  hs_bindgen_70dae000d43d8228

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_after8@
foreign import ccall unsafe "hs_bindgen_85e787c823b6c95f" hs_bindgen_85e787c823b6c95f_base ::
     BG.Int32
  -> BG.Word64
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_withoutSign_after8@
hs_bindgen_85e787c823b6c95f ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_85e787c823b6c95f =
  BG.fromFFIType hs_bindgen_85e787c823b6c95f_base

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
  hs_bindgen_85e787c823b6c95f

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_args1@
foreign import ccall unsafe "hs_bindgen_efb5ca5187af5610" hs_bindgen_efb5ca5187af5610_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_args1@
hs_bindgen_efb5ca5187af5610 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_efb5ca5187af5610 =
  BG.fromFFIType hs_bindgen_efb5ca5187af5610_base

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
const_pointers_args1 = hs_bindgen_efb5ca5187af5610

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_args2@
foreign import ccall unsafe "hs_bindgen_e306bbd2d4a374fc" hs_bindgen_e306bbd2d4a374fc_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_args2@
hs_bindgen_e306bbd2d4a374fc ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_e306bbd2d4a374fc =
  BG.fromFFIType hs_bindgen_e306bbd2d4a374fc_base

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
const_pointers_args2 = hs_bindgen_e306bbd2d4a374fc

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_args3@
foreign import ccall unsafe "hs_bindgen_960be6e90be0e7c1" hs_bindgen_960be6e90be0e7c1_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_args3@
hs_bindgen_960be6e90be0e7c1 ::
     BG.CInt
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_960be6e90be0e7c1 =
  BG.fromFFIType hs_bindgen_960be6e90be0e7c1_base

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
const_pointers_args3 = hs_bindgen_960be6e90be0e7c1

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_args4@
foreign import ccall unsafe "hs_bindgen_00950eca61f28e52" hs_bindgen_00950eca61f28e52_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_args4@
hs_bindgen_00950eca61f28e52 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_00950eca61f28e52 =
  BG.fromFFIType hs_bindgen_00950eca61f28e52_base

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
const_pointers_args4 = hs_bindgen_00950eca61f28e52

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_args5@
foreign import ccall unsafe "hs_bindgen_96b40db09db83748" hs_bindgen_96b40db09db83748_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_args5@
hs_bindgen_96b40db09db83748 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_96b40db09db83748 =
  BG.fromFFIType hs_bindgen_96b40db09db83748_base

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
const_pointers_args5 = hs_bindgen_96b40db09db83748

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_ret1@
foreign import ccall unsafe "hs_bindgen_035d9da3770aeff8" hs_bindgen_035d9da3770aeff8_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_ret1@
hs_bindgen_035d9da3770aeff8 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_035d9da3770aeff8 =
  BG.fromFFIType hs_bindgen_035d9da3770aeff8_base

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h 212:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret1 = hs_bindgen_035d9da3770aeff8

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_ret2@
foreign import ccall unsafe "hs_bindgen_0d195040558fbef7" hs_bindgen_0d195040558fbef7_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_ret2@
hs_bindgen_0d195040558fbef7 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_0d195040558fbef7 =
  BG.fromFFIType hs_bindgen_0d195040558fbef7_base

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h 213:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret2 = hs_bindgen_0d195040558fbef7

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_ret3@
foreign import ccall unsafe "hs_bindgen_9d5a35ded6d43b33" hs_bindgen_9d5a35ded6d43b33_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_ret3@
hs_bindgen_9d5a35ded6d43b33 ::
     BG.CInt
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_9d5a35ded6d43b33 =
  BG.fromFFIType hs_bindgen_9d5a35ded6d43b33_base

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h 214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
const_pointers_ret3 = hs_bindgen_9d5a35ded6d43b33

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_ret4@
foreign import ccall unsafe "hs_bindgen_b00ca0cfd4037848" hs_bindgen_b00ca0cfd4037848_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_ret4@
hs_bindgen_b00ca0cfd4037848 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_b00ca0cfd4037848 =
  BG.fromFFIType hs_bindgen_b00ca0cfd4037848_base

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h 215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret4 = hs_bindgen_b00ca0cfd4037848

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_ret5@
foreign import ccall unsafe "hs_bindgen_da92f752b03da901" hs_bindgen_da92f752b03da901_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_pointers_ret5@
hs_bindgen_da92f752b03da901 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_da92f752b03da901 =
  BG.fromFFIType hs_bindgen_da92f752b03da901_base

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h 216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret5 = hs_bindgen_da92f752b03da901

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_array_elem1@
foreign import ccall unsafe "hs_bindgen_fdc3e3307cac5fd0" hs_bindgen_fdc3e3307cac5fd0_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_array_elem1@
hs_bindgen_fdc3e3307cac5fd0 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt))
  -> IO ()
hs_bindgen_fdc3e3307cac5fd0 =
  BG.fromFFIType hs_bindgen_fdc3e3307cac5fd0_base

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h 244:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem1 = hs_bindgen_fdc3e3307cac5fd0

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_array_elem2@
foreign import ccall unsafe "hs_bindgen_cecdce9d64ac85d4" hs_bindgen_cecdce9d64ac85d4_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_array_elem2@
hs_bindgen_cecdce9d64ac85d4 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt)))
  -> IO ()
hs_bindgen_cecdce9d64ac85d4 =
  BG.fromFFIType hs_bindgen_cecdce9d64ac85d4_base

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h 245:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem2 = hs_bindgen_cecdce9d64ac85d4

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_array_elem3@
foreign import ccall unsafe "hs_bindgen_9ee7979e72b4baf6" hs_bindgen_9ee7979e72b4baf6_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_const_array_elem3@
hs_bindgen_9ee7979e72b4baf6 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_9ee7979e72b4baf6 =
  BG.fromFFIType hs_bindgen_9ee7979e72b4baf6_base

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h 246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem3 = hs_bindgen_9ee7979e72b4baf6

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_noParams1@
foreign import ccall unsafe "hs_bindgen_c6e158d421dda2ac" hs_bindgen_c6e158d421dda2ac_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_noParams1@
hs_bindgen_c6e158d421dda2ac :: IO BG.CInt
hs_bindgen_c6e158d421dda2ac =
  BG.fromFFIType hs_bindgen_c6e158d421dda2ac_base

{-| Other examples we reparsed /incorrectly/ before language-c

    __C declaration:__ @noParams1@

    __defined at:__ @macros\/reparse.h 254:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams1 :: IO BG.CInt
noParams1 = hs_bindgen_c6e158d421dda2ac

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_noParams2@
foreign import ccall unsafe "hs_bindgen_a34e536ea5931d76" hs_bindgen_a34e536ea5931d76_base ::
     IO BG.Int32

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_noParams2@
hs_bindgen_a34e536ea5931d76 :: IO BG.CInt
hs_bindgen_a34e536ea5931d76 =
  BG.fromFFIType hs_bindgen_a34e536ea5931d76_base

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h 255:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2 :: IO BG.CInt
noParams2 = hs_bindgen_a34e536ea5931d76

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_noParams3@
foreign import ccall unsafe "hs_bindgen_80f6772cd6dfe2a9" hs_bindgen_80f6772cd6dfe2a9_base ::
     BG.Int32
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_noParams3@
hs_bindgen_80f6772cd6dfe2a9 ::
     BG.CInt
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_80f6772cd6dfe2a9 =
  BG.fromFFIType hs_bindgen_80f6772cd6dfe2a9_base

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
noParams3 = hs_bindgen_80f6772cd6dfe2a9

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret1@
foreign import ccall unsafe "hs_bindgen_8938b867c00625aa" hs_bindgen_8938b867c00625aa_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret1@
hs_bindgen_8938b867c00625aa ::
     BG.CInt
  -> IO (BG.FunPtr (IO ()))
hs_bindgen_8938b867c00625aa =
  BG.fromFFIType hs_bindgen_8938b867c00625aa_base

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h 260:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO ()))
funptr_ret1 = hs_bindgen_8938b867c00625aa

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret2@
foreign import ccall unsafe "hs_bindgen_03ba9af419f7c750" hs_bindgen_03ba9af419f7c750_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret2@
hs_bindgen_03ba9af419f7c750 ::
     BG.CInt
  -> IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_03ba9af419f7c750 =
  BG.fromFFIType hs_bindgen_03ba9af419f7c750_base

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h 261:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO BG.CInt))
funptr_ret2 = hs_bindgen_03ba9af419f7c750

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret3@
foreign import ccall unsafe "hs_bindgen_c53c44ca84d8cb81" hs_bindgen_c53c44ca84d8cb81_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret3@
hs_bindgen_c53c44ca84d8cb81 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_c53c44ca84d8cb81 =
  BG.fromFFIType hs_bindgen_c53c44ca84d8cb81_base

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h 262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
funptr_ret3 = hs_bindgen_c53c44ca84d8cb81

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret4@
foreign import ccall unsafe "hs_bindgen_fb5431b884cc209e" hs_bindgen_fb5431b884cc209e_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret4@
hs_bindgen_fb5431b884cc209e ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
hs_bindgen_fb5431b884cc209e =
  BG.fromFFIType hs_bindgen_fb5431b884cc209e_base

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h 263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
funptr_ret4 = hs_bindgen_fb5431b884cc209e

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret5@
foreign import ccall unsafe "hs_bindgen_b45419cebf6cb43d" hs_bindgen_b45419cebf6cb43d_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret5@
hs_bindgen_b45419cebf6cb43d ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_b45419cebf6cb43d =
  BG.fromFFIType hs_bindgen_b45419cebf6cb43d_base

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h 267:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret5 = hs_bindgen_b45419cebf6cb43d

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret6@
foreign import ccall unsafe "hs_bindgen_5c14b480ba3cab1e" hs_bindgen_5c14b480ba3cab1e_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret6@
hs_bindgen_5c14b480ba3cab1e ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_5c14b480ba3cab1e =
  BG.fromFFIType hs_bindgen_5c14b480ba3cab1e_base

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h 268:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret6 = hs_bindgen_5c14b480ba3cab1e

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret7@
foreign import ccall unsafe "hs_bindgen_b8f93b6ff6ce2b1c" hs_bindgen_b8f93b6ff6ce2b1c_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret7@
hs_bindgen_b8f93b6ff6ce2b1c ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_b8f93b6ff6ce2b1c =
  BG.fromFFIType hs_bindgen_b8f93b6ff6ce2b1c_base

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h 269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret7 = hs_bindgen_b8f93b6ff6ce2b1c

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret8@
foreign import ccall unsafe "hs_bindgen_74e1d2bf9ed6b1c5" hs_bindgen_74e1d2bf9ed6b1c5_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret8@
hs_bindgen_74e1d2bf9ed6b1c5 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_74e1d2bf9ed6b1c5 =
  BG.fromFFIType hs_bindgen_74e1d2bf9ed6b1c5_base

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h 270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret8 = hs_bindgen_74e1d2bf9ed6b1c5

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret9@
foreign import ccall unsafe "hs_bindgen_8e68b5e49b407e0b" hs_bindgen_8e68b5e49b407e0b_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret9@
hs_bindgen_8e68b5e49b407e0b ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_8e68b5e49b407e0b =
  BG.fromFFIType hs_bindgen_8e68b5e49b407e0b_base

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h 271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret9 = hs_bindgen_8e68b5e49b407e0b

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret10@
foreign import ccall unsafe "hs_bindgen_1fc54cf36aa5566f" hs_bindgen_1fc54cf36aa5566f_base ::
     BG.Int32
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_Unsafe_funptr_ret10@
hs_bindgen_1fc54cf36aa5566f ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_1fc54cf36aa5566f =
  BG.fromFFIType hs_bindgen_1fc54cf36aa5566f_base

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h 272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret10 = hs_bindgen_1fc54cf36aa5566f
