{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse.h>"
  , "void hs_bindgen_7cd4fb42733b9250 ("
  , "  signed int arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  (args_char1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_59d0024d1e6b3592 ("
  , "  signed int arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  (args_char2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b1bc15e37276b8e7 ("
  , "  signed int arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  (args_char3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8edf04bc249e39c6 ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_28df7a3765ee87b9 ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d6e871231bf242dc ("
  , "  signed int arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  (args_short3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_81581e8ba505b518 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c1831b4e11576930 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a95654c437007224 ("
  , "  signed int arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  (args_int3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f3b90abdc236433a ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8c213d206b813b0f ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1e599fd755525995 ("
  , "  signed int arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  (args_long3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a3741723bfd077cc ("
  , "  signed int arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  (args_float)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_67367df13e2d7f8d ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  (args_double)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c25d1a654adcd012 ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (args_bool1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f69965ab2390725d ("
  , "  signed int arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  (args_struct)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_c9a90a049b2f7e36 ("
  , "  signed int arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  (args_union)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_015b78cf928bfc4a ("
  , "  signed int arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  (args_enum)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d60fc94ef9084472 ("
  , "  signed int arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  (args_pointer1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e1bd11d4000e78dd ("
  , "  signed int arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  (args_pointer2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0218eda079f85811 ("
  , "  signed int arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  (args_pointer3)(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_6f717c3d45ac0826 (void)"
  , "{"
  , "  return (ret_A)();"
  , "}"
  , "char hs_bindgen_b265a85cb13a3714 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char1)(arg1);"
  , "}"
  , "signed char hs_bindgen_dafab1728f4782aa ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char2)(arg1);"
  , "}"
  , "unsigned char hs_bindgen_6c949580eea124f6 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char3)(arg1);"
  , "}"
  , "signed short hs_bindgen_8a1f0f589fa5e6ba ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short1)(arg1);"
  , "}"
  , "signed short hs_bindgen_8822b48ae4a02f39 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short2)(arg1);"
  , "}"
  , "unsigned short hs_bindgen_44ee8df57dc2c660 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short3)(arg1);"
  , "}"
  , "signed int hs_bindgen_e6824cdc32010651 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int1)(arg1);"
  , "}"
  , "signed int hs_bindgen_c10c9bbc842f96fe ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int2)(arg1);"
  , "}"
  , "unsigned int hs_bindgen_7271658d4d3763b7 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int3)(arg1);"
  , "}"
  , "signed long hs_bindgen_501603bb5524fa00 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long1)(arg1);"
  , "}"
  , "signed long hs_bindgen_ad1ff90dfa92181b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long2)(arg1);"
  , "}"
  , "unsigned long hs_bindgen_ac855ad90f566801 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long3)(arg1);"
  , "}"
  , "float hs_bindgen_8d6a055c0537353d ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_float)(arg1);"
  , "}"
  , "double hs_bindgen_74db6ec0193d162b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_double)(arg1);"
  , "}"
  , "_Bool hs_bindgen_04ac98fc47084d4a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_bool1)(arg1);"
  , "}"
  , "void hs_bindgen_87c8df854cb83248 ("
  , "  signed int arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_struct)(arg1);"
  , "}"
  , "void hs_bindgen_c9d41f6ec8552245 ("
  , "  signed int arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_union)(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_d7949315e12b378a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_enum)(arg1);"
  , "}"
  , "signed int *hs_bindgen_e578084d2442304a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer1)(arg1);"
  , "}"
  , "signed int **hs_bindgen_087d016604cbb071 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer2)(arg1);"
  , "}"
  , "void *hs_bindgen_a5515de085c1694e ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer3)(arg1);"
  , "}"
  , "signed int hs_bindgen_ad8f922ca5fd4492 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (body1)(arg1);"
  , "}"
  , "signed int hs_bindgen_006b9a65f3b5fd7b (void)"
  , "{"
  , "  return (body2)();"
  , "}"
  , "void hs_bindgen_71353f34ac72501e ("
  , "  signed int arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_float)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_7e7e013681771442 ("
  , "  signed int arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_double)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_8d1d455a1977835c ("
  , "  signed int arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_float)(arg1);"
  , "}"
  , "void hs_bindgen_3e5cbc667a775bba ("
  , "  signed int arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_double)(arg1);"
  , "}"
  , "void hs_bindgen_d585f15b3d1be58e ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (bespoke_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_39421ecbc2568b72 ("
  , "  signed int arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  (bespoke_args2)(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_5f99662d9d79771a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret1)(arg1);"
  , "}"
  , "size_t hs_bindgen_72db3209eda47b4e ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret2)(arg1);"
  , "}"
  , "void hs_bindgen_9908d04d7c286fa9 ("
  , "  signed int arg1[]"
  , ")"
  , "{"
  , "  (arr_args1)(arg1);"
  , "}"
  , "void hs_bindgen_d808b5c72794b7fb ("
  , "  signed int *arg1[]"
  , ")"
  , "{"
  , "  (arr_args2)(arg1);"
  , "}"
  , "void hs_bindgen_a3043995480dc3c7 ("
  , "  signed int arg1[5]"
  , ")"
  , "{"
  , "  (arr_args3)(arg1);"
  , "}"
  , "void hs_bindgen_eaaa3aa8df344e37 ("
  , "  signed int *arg1[5]"
  , ")"
  , "{"
  , "  (arr_args4)(arg1);"
  , "}"
  , "void hs_bindgen_86e5488952a832f2 ("
  , "  signed int arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4a71cc92db6973bd ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_5707842a1cf30ab9 ("
  , "  signed int arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ec3f1d5abdd36f96 ("
  , "  signed int arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_dca64973a93f6621 ("
  , "  signed int arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args5)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a193cd70b32b5f13 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (comments1)(arg1);"
  , "}"
  , "void hs_bindgen_412e4980d7ac8631 ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_933eadeedf6bf0f7 ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e791974bd8a43111 ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e728e8a8d01615d7 ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e0c8637468d97f01 ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3c23b6ee9b0dd3a5 ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_96c4e34cd8bb8e17 ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_78976b65f960adb7 ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6442f484870b498e ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_36cd5314ad34356d ("
  , "  signed int arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_e5e2292a0d6193a3 ("
  , "  signed int arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_ea0a75d094db765e ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_59163b17198cc53d ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_55ab156fb365279f ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_220f14133b0ccd33 ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_829f969c97ab4d34 ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f19ec2ea4dadc1b8 ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_75a26a0fa2d77097 ("
  , "  signed int arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_313ee3b2640f39a4 ("
  , "  signed int arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_9362037465858f18 ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8443bb74c444d10d ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_bac50d77b1418e14 ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_712dc1f0f32c8c44 ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_90355094b871f8ef ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e23d28351613e5d3 ("
  , "  signed int arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ec4147bad69bedd8 ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_82c92ffdbf494af2 ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args5)(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_f806bbcad8e0495c ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret1)(arg1);"
  , "}"
  , "signed int const *hs_bindgen_af8f0bcf949618f4 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret2)(arg1);"
  , "}"
  , "signed int *const hs_bindgen_9748e1e80a09647c ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret3)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_cc89d05287cb4319 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret4)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_5de9d5d3dd272d80 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret5)(arg1);"
  , "}"
  , "void hs_bindgen_1870bdff8b748c5f ("
  , "  signed int const arg1[]"
  , ")"
  , "{"
  , "  (const_array_elem1)(arg1);"
  , "}"
  , "void hs_bindgen_f232c98309b5749f ("
  , "  signed int const *arg1[]"
  , ")"
  , "{"
  , "  (const_array_elem2)(arg1);"
  , "}"
  , "void hs_bindgen_66d7048a1fdf1ca4 ("
  , "  signed int *const arg1[]"
  , ")"
  , "{"
  , "  (const_array_elem3)(arg1);"
  , "}"
  , "signed int hs_bindgen_c671fc6cdc77dcb2 (void)"
  , "{"
  , "  return (noParams1)();"
  , "}"
  , "signed int hs_bindgen_74de93eb9dfe14ce (void)"
  , "{"
  , "  return (noParams2)();"
  , "}"
  , "void hs_bindgen_4d951e605b009e92 ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (noParams3)(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_05c6646047255af2 ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_f1aee360f7ee4a05 ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret2)(arg1);"
  , "}"
  , "void (*hs_bindgen_fe51c3b8653fb966 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (funptr_ret3)(arg1);"
  , "}"
  , "char (*hs_bindgen_0d481c2be48e47b5 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret4)(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_8d733067996c6da3 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret5)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_157d813289af079d ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret6)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_8e577c6750e29e93 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret7)(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_35c2200ef10c11d3 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret8)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_3273d59c8a07b3d7 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret9)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_bfd584bc16ad62bd ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret10)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_char1@
foreign import ccall safe "hs_bindgen_7cd4fb42733b9250" hs_bindgen_7cd4fb42733b9250_base ::
     BG.CInt
  -> BG.CChar
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_char1@
hs_bindgen_7cd4fb42733b9250 ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_7cd4fb42733b9250 =
  \x0 ->
    \x1 ->
      hs_bindgen_7cd4fb42733b9250_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_char1 = hs_bindgen_7cd4fb42733b9250

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_char2@
foreign import ccall safe "hs_bindgen_59d0024d1e6b3592" hs_bindgen_59d0024d1e6b3592_base ::
     BG.CInt
  -> BG.CSChar
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_char2@
hs_bindgen_59d0024d1e6b3592 ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_59d0024d1e6b3592 =
  \x0 ->
    \x1 ->
      hs_bindgen_59d0024d1e6b3592_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_char2 = hs_bindgen_59d0024d1e6b3592

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_char3@
foreign import ccall safe "hs_bindgen_b1bc15e37276b8e7" hs_bindgen_b1bc15e37276b8e7_base ::
     BG.CInt
  -> BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_char3@
hs_bindgen_b1bc15e37276b8e7 ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_b1bc15e37276b8e7 =
  \x0 ->
    \x1 ->
      hs_bindgen_b1bc15e37276b8e7_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_char3 = hs_bindgen_b1bc15e37276b8e7

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_short1@
foreign import ccall safe "hs_bindgen_8edf04bc249e39c6" hs_bindgen_8edf04bc249e39c6_base ::
     BG.CInt
  -> BG.CShort
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_short1@
hs_bindgen_8edf04bc249e39c6 ::
     BG.CInt
  -> BG.CShort
  -> IO ()
hs_bindgen_8edf04bc249e39c6 =
  \x0 ->
    \x1 ->
      hs_bindgen_8edf04bc249e39c6_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_short1 = hs_bindgen_8edf04bc249e39c6

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_short2@
foreign import ccall safe "hs_bindgen_28df7a3765ee87b9" hs_bindgen_28df7a3765ee87b9_base ::
     BG.CInt
  -> BG.CShort
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_short2@
hs_bindgen_28df7a3765ee87b9 ::
     BG.CInt
  -> BG.CShort
  -> IO ()
hs_bindgen_28df7a3765ee87b9 =
  \x0 ->
    \x1 ->
      hs_bindgen_28df7a3765ee87b9_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_short2 = hs_bindgen_28df7a3765ee87b9

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_short3@
foreign import ccall safe "hs_bindgen_d6e871231bf242dc" hs_bindgen_d6e871231bf242dc_base ::
     BG.CInt
  -> BG.CUShort
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_short3@
hs_bindgen_d6e871231bf242dc ::
     BG.CInt
  -> BG.CUShort
  -> IO ()
hs_bindgen_d6e871231bf242dc =
  \x0 ->
    \x1 ->
      hs_bindgen_d6e871231bf242dc_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_short3 = hs_bindgen_d6e871231bf242dc

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_int1@
foreign import ccall safe "hs_bindgen_81581e8ba505b518" hs_bindgen_81581e8ba505b518_base ::
     BG.CInt
  -> BG.CInt
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_int1@
hs_bindgen_81581e8ba505b518 ::
     BG.CInt
  -> BG.CInt
  -> IO ()
hs_bindgen_81581e8ba505b518 =
  \x0 ->
    \x1 ->
      hs_bindgen_81581e8ba505b518_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_int1 = hs_bindgen_81581e8ba505b518

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_int2@
foreign import ccall safe "hs_bindgen_c1831b4e11576930" hs_bindgen_c1831b4e11576930_base ::
     BG.CInt
  -> BG.CInt
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_int2@
hs_bindgen_c1831b4e11576930 ::
     BG.CInt
  -> BG.CInt
  -> IO ()
hs_bindgen_c1831b4e11576930 =
  \x0 ->
    \x1 ->
      hs_bindgen_c1831b4e11576930_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_int2 = hs_bindgen_c1831b4e11576930

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_int3@
foreign import ccall safe "hs_bindgen_a95654c437007224" hs_bindgen_a95654c437007224_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_int3@
hs_bindgen_a95654c437007224 ::
     BG.CInt
  -> BG.CUInt
  -> IO ()
hs_bindgen_a95654c437007224 =
  \x0 ->
    \x1 ->
      hs_bindgen_a95654c437007224_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_int3 = hs_bindgen_a95654c437007224

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_long1@
foreign import ccall safe "hs_bindgen_f3b90abdc236433a" hs_bindgen_f3b90abdc236433a_base ::
     BG.CInt
  -> BG.CLong
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_long1@
hs_bindgen_f3b90abdc236433a ::
     BG.CInt
  -> BG.CLong
  -> IO ()
hs_bindgen_f3b90abdc236433a =
  \x0 ->
    \x1 ->
      hs_bindgen_f3b90abdc236433a_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_long1 = hs_bindgen_f3b90abdc236433a

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_long2@
foreign import ccall safe "hs_bindgen_8c213d206b813b0f" hs_bindgen_8c213d206b813b0f_base ::
     BG.CInt
  -> BG.CLong
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_long2@
hs_bindgen_8c213d206b813b0f ::
     BG.CInt
  -> BG.CLong
  -> IO ()
hs_bindgen_8c213d206b813b0f =
  \x0 ->
    \x1 ->
      hs_bindgen_8c213d206b813b0f_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_long2 = hs_bindgen_8c213d206b813b0f

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_long3@
foreign import ccall safe "hs_bindgen_1e599fd755525995" hs_bindgen_1e599fd755525995_base ::
     BG.CInt
  -> BG.CULong
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_long3@
hs_bindgen_1e599fd755525995 ::
     BG.CInt
  -> BG.CULong
  -> IO ()
hs_bindgen_1e599fd755525995 =
  \x0 ->
    \x1 ->
      hs_bindgen_1e599fd755525995_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_long3 = hs_bindgen_1e599fd755525995

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_float@
foreign import ccall safe "hs_bindgen_a3741723bfd077cc" hs_bindgen_a3741723bfd077cc_base ::
     BG.CInt
  -> BG.CFloat
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_float@
hs_bindgen_a3741723bfd077cc ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_a3741723bfd077cc =
  \x0 ->
    \x1 ->
      hs_bindgen_a3741723bfd077cc_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_float = hs_bindgen_a3741723bfd077cc

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_double@
foreign import ccall safe "hs_bindgen_67367df13e2d7f8d" hs_bindgen_67367df13e2d7f8d_base ::
     BG.CInt
  -> BG.CDouble
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_double@
hs_bindgen_67367df13e2d7f8d ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_67367df13e2d7f8d =
  \x0 ->
    \x1 ->
      hs_bindgen_67367df13e2d7f8d_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_double = hs_bindgen_67367df13e2d7f8d

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_bool1@
foreign import ccall safe "hs_bindgen_c25d1a654adcd012" hs_bindgen_c25d1a654adcd012_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_bool1@
hs_bindgen_c25d1a654adcd012 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_c25d1a654adcd012 =
  \x0 ->
    \x1 ->
      hs_bindgen_c25d1a654adcd012_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_bool1 = hs_bindgen_c25d1a654adcd012

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_struct@
foreign import ccall safe "hs_bindgen_f69965ab2390725d" hs_bindgen_f69965ab2390725d_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_struct@
hs_bindgen_f69965ab2390725d ::
     BG.CInt
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_f69965ab2390725d =
  \x0 ->
    \x1 ->
      hs_bindgen_f69965ab2390725d_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_f69965ab2390725d arg10 arg22)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_union@
foreign import ccall safe "hs_bindgen_c9a90a049b2f7e36" hs_bindgen_c9a90a049b2f7e36_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_union@
hs_bindgen_c9a90a049b2f7e36 ::
     BG.CInt
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_c9a90a049b2f7e36 =
  \x0 ->
    \x1 ->
      hs_bindgen_c9a90a049b2f7e36_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_c9a90a049b2f7e36 arg10 arg22)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_enum@
foreign import ccall safe "hs_bindgen_015b78cf928bfc4a" hs_bindgen_015b78cf928bfc4a_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_enum@
hs_bindgen_015b78cf928bfc4a ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_015b78cf928bfc4a =
  \x0 ->
    \x1 ->
      hs_bindgen_015b78cf928bfc4a_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_enum = hs_bindgen_015b78cf928bfc4a

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_pointer1@
foreign import ccall safe "hs_bindgen_d60fc94ef9084472" hs_bindgen_d60fc94ef9084472_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_pointer1@
hs_bindgen_d60fc94ef9084472 ::
     BG.CInt
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_d60fc94ef9084472 =
  \x0 ->
    \x1 ->
      hs_bindgen_d60fc94ef9084472_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_pointer1 = hs_bindgen_d60fc94ef9084472

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_pointer2@
foreign import ccall safe "hs_bindgen_e1bd11d4000e78dd" hs_bindgen_e1bd11d4000e78dd_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_pointer2@
hs_bindgen_e1bd11d4000e78dd ::
     BG.CInt
  -> BG.Ptr (BG.Ptr BG.CInt)
  -> IO ()
hs_bindgen_e1bd11d4000e78dd =
  \x0 ->
    \x1 ->
      hs_bindgen_e1bd11d4000e78dd_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_pointer2 = hs_bindgen_e1bd11d4000e78dd

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_pointer3@
foreign import ccall safe "hs_bindgen_0218eda079f85811" hs_bindgen_0218eda079f85811_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_pointer3@
hs_bindgen_0218eda079f85811 ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()
hs_bindgen_0218eda079f85811 =
  \x0 ->
    \x1 ->
      hs_bindgen_0218eda079f85811_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_pointer3 = hs_bindgen_0218eda079f85811

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_A@
foreign import ccall safe "hs_bindgen_6f717c3d45ac0826" hs_bindgen_6f717c3d45ac0826_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_A@
hs_bindgen_6f717c3d45ac0826 :: IO BG.CInt
hs_bindgen_6f717c3d45ac0826 =
  fmap BG.fromFFIType hs_bindgen_6f717c3d45ac0826_base

{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h 47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A :: IO BG.CInt
ret_A = hs_bindgen_6f717c3d45ac0826

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_char1@
foreign import ccall safe "hs_bindgen_b265a85cb13a3714" hs_bindgen_b265a85cb13a3714_base ::
     BG.CInt
  -> IO BG.CChar

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_char1@
hs_bindgen_b265a85cb13a3714 ::
     BG.CInt
  -> IO BG.CChar
hs_bindgen_b265a85cb13a3714 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_b265a85cb13a3714_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h 49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CChar
ret_char1 = hs_bindgen_b265a85cb13a3714

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_char2@
foreign import ccall safe "hs_bindgen_dafab1728f4782aa" hs_bindgen_dafab1728f4782aa_base ::
     BG.CInt
  -> IO BG.CSChar

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_char2@
hs_bindgen_dafab1728f4782aa ::
     BG.CInt
  -> IO BG.CSChar
hs_bindgen_dafab1728f4782aa =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_dafab1728f4782aa_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h 50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CSChar
ret_char2 = hs_bindgen_dafab1728f4782aa

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_char3@
foreign import ccall safe "hs_bindgen_6c949580eea124f6" hs_bindgen_6c949580eea124f6_base ::
     BG.CInt
  -> IO BG.CUChar

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_char3@
hs_bindgen_6c949580eea124f6 ::
     BG.CInt
  -> IO BG.CUChar
hs_bindgen_6c949580eea124f6 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_6c949580eea124f6_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h 51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUChar
ret_char3 = hs_bindgen_6c949580eea124f6

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_short1@
foreign import ccall safe "hs_bindgen_8a1f0f589fa5e6ba" hs_bindgen_8a1f0f589fa5e6ba_base ::
     BG.CInt
  -> IO BG.CShort

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_short1@
hs_bindgen_8a1f0f589fa5e6ba ::
     BG.CInt
  -> IO BG.CShort
hs_bindgen_8a1f0f589fa5e6ba =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_8a1f0f589fa5e6ba_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h 53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short1 = hs_bindgen_8a1f0f589fa5e6ba

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_short2@
foreign import ccall safe "hs_bindgen_8822b48ae4a02f39" hs_bindgen_8822b48ae4a02f39_base ::
     BG.CInt
  -> IO BG.CShort

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_short2@
hs_bindgen_8822b48ae4a02f39 ::
     BG.CInt
  -> IO BG.CShort
hs_bindgen_8822b48ae4a02f39 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_8822b48ae4a02f39_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h 54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short2 = hs_bindgen_8822b48ae4a02f39

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_short3@
foreign import ccall safe "hs_bindgen_44ee8df57dc2c660" hs_bindgen_44ee8df57dc2c660_base ::
     BG.CInt
  -> IO BG.CUShort

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_short3@
hs_bindgen_44ee8df57dc2c660 ::
     BG.CInt
  -> IO BG.CUShort
hs_bindgen_44ee8df57dc2c660 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_44ee8df57dc2c660_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h 55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUShort
ret_short3 = hs_bindgen_44ee8df57dc2c660

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_int1@
foreign import ccall safe "hs_bindgen_e6824cdc32010651" hs_bindgen_e6824cdc32010651_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_int1@
hs_bindgen_e6824cdc32010651 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_e6824cdc32010651 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_e6824cdc32010651_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h 57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int1 = hs_bindgen_e6824cdc32010651

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_int2@
foreign import ccall safe "hs_bindgen_c10c9bbc842f96fe" hs_bindgen_c10c9bbc842f96fe_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_int2@
hs_bindgen_c10c9bbc842f96fe ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_c10c9bbc842f96fe =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_c10c9bbc842f96fe_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h 58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int2 = hs_bindgen_c10c9bbc842f96fe

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_int3@
foreign import ccall safe "hs_bindgen_7271658d4d3763b7" hs_bindgen_7271658d4d3763b7_base ::
     BG.CInt
  -> IO BG.CUInt

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_int3@
hs_bindgen_7271658d4d3763b7 ::
     BG.CInt
  -> IO BG.CUInt
hs_bindgen_7271658d4d3763b7 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_7271658d4d3763b7_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h 59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUInt
ret_int3 = hs_bindgen_7271658d4d3763b7

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_long1@
foreign import ccall safe "hs_bindgen_501603bb5524fa00" hs_bindgen_501603bb5524fa00_base ::
     BG.CInt
  -> IO BG.CLong

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_long1@
hs_bindgen_501603bb5524fa00 ::
     BG.CInt
  -> IO BG.CLong
hs_bindgen_501603bb5524fa00 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_501603bb5524fa00_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h 61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long1 = hs_bindgen_501603bb5524fa00

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_long2@
foreign import ccall safe "hs_bindgen_ad1ff90dfa92181b" hs_bindgen_ad1ff90dfa92181b_base ::
     BG.CInt
  -> IO BG.CLong

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_long2@
hs_bindgen_ad1ff90dfa92181b ::
     BG.CInt
  -> IO BG.CLong
hs_bindgen_ad1ff90dfa92181b =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_ad1ff90dfa92181b_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h 62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long2 = hs_bindgen_ad1ff90dfa92181b

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_long3@
foreign import ccall safe "hs_bindgen_ac855ad90f566801" hs_bindgen_ac855ad90f566801_base ::
     BG.CInt
  -> IO BG.CULong

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_long3@
hs_bindgen_ac855ad90f566801 ::
     BG.CInt
  -> IO BG.CULong
hs_bindgen_ac855ad90f566801 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_ac855ad90f566801_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h 63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CULong
ret_long3 = hs_bindgen_ac855ad90f566801

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_float@
foreign import ccall safe "hs_bindgen_8d6a055c0537353d" hs_bindgen_8d6a055c0537353d_base ::
     BG.CInt
  -> IO BG.CFloat

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_float@
hs_bindgen_8d6a055c0537353d ::
     BG.CInt
  -> IO BG.CFloat
hs_bindgen_8d6a055c0537353d =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_8d6a055c0537353d_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h 65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CFloat
ret_float = hs_bindgen_8d6a055c0537353d

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_double@
foreign import ccall safe "hs_bindgen_74db6ec0193d162b" hs_bindgen_74db6ec0193d162b_base ::
     BG.CInt
  -> IO BG.CDouble

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_double@
hs_bindgen_74db6ec0193d162b ::
     BG.CInt
  -> IO BG.CDouble
hs_bindgen_74db6ec0193d162b =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_74db6ec0193d162b_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h 66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CDouble
ret_double = hs_bindgen_74db6ec0193d162b

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_bool1@
foreign import ccall safe "hs_bindgen_04ac98fc47084d4a" hs_bindgen_04ac98fc47084d4a_base ::
     BG.CInt
  -> IO BG.CBool

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_bool1@
hs_bindgen_04ac98fc47084d4a ::
     BG.CInt
  -> IO BG.CBool
hs_bindgen_04ac98fc47084d4a =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_04ac98fc47084d4a_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h 67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
ret_bool1 = hs_bindgen_04ac98fc47084d4a

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_struct@
foreign import ccall safe "hs_bindgen_87c8df854cb83248" hs_bindgen_87c8df854cb83248_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_struct@
hs_bindgen_87c8df854cb83248 ::
     BG.CInt
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_87c8df854cb83248 =
  \x0 ->
    \x1 ->
      hs_bindgen_87c8df854cb83248_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_87c8df854cb83248 arg10 res1)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_union@
foreign import ccall safe "hs_bindgen_c9d41f6ec8552245" hs_bindgen_c9d41f6ec8552245_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_union@
hs_bindgen_c9d41f6ec8552245 ::
     BG.CInt
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_c9d41f6ec8552245 =
  \x0 ->
    \x1 ->
      hs_bindgen_c9d41f6ec8552245_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_c9d41f6ec8552245 arg10 res1)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_enum@
foreign import ccall safe "hs_bindgen_d7949315e12b378a" hs_bindgen_d7949315e12b378a_base ::
     BG.CInt
  -> IO BG.CUInt

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_enum@
hs_bindgen_d7949315e12b378a ::
     BG.CInt
  -> IO Some_enum
hs_bindgen_d7949315e12b378a =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_d7949315e12b378a_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h 71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO Some_enum
ret_enum = hs_bindgen_d7949315e12b378a

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_pointer1@
foreign import ccall safe "hs_bindgen_e578084d2442304a" hs_bindgen_e578084d2442304a_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_pointer1@
hs_bindgen_e578084d2442304a ::
     BG.CInt
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_e578084d2442304a =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_e578084d2442304a_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h 73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
ret_pointer1 = hs_bindgen_e578084d2442304a

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_pointer2@
foreign import ccall safe "hs_bindgen_087d016604cbb071" hs_bindgen_087d016604cbb071_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_pointer2@
hs_bindgen_087d016604cbb071 ::
     BG.CInt
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
hs_bindgen_087d016604cbb071 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_087d016604cbb071_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h 74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
ret_pointer2 = hs_bindgen_087d016604cbb071

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_pointer3@
foreign import ccall safe "hs_bindgen_a5515de085c1694e" hs_bindgen_a5515de085c1694e_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_pointer3@
hs_bindgen_a5515de085c1694e ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)
hs_bindgen_a5515de085c1694e =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_a5515de085c1694e_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h 75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.Void)
ret_pointer3 = hs_bindgen_a5515de085c1694e

-- __unique:__ @test_macrosreparse_2_raw Example Safe_body1@
foreign import ccall safe "hs_bindgen_ad8f922ca5fd4492" hs_bindgen_ad8f922ca5fd4492_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparse_2_raw Example Safe_body1@
hs_bindgen_ad8f922ca5fd4492 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_ad8f922ca5fd4492 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_ad8f922ca5fd4492_base (BG.toFFIType x0))

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h 79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
body1 = hs_bindgen_ad8f922ca5fd4492

-- __unique:__ @test_macrosreparse_2_raw Example Safe_body2@
foreign import ccall safe "hs_bindgen_006b9a65f3b5fd7b" hs_bindgen_006b9a65f3b5fd7b_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse_2_raw Example Safe_body2@
hs_bindgen_006b9a65f3b5fd7b :: IO BG.CInt
hs_bindgen_006b9a65f3b5fd7b =
  fmap BG.fromFFIType hs_bindgen_006b9a65f3b5fd7b_base

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h 80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2 :: IO BG.CInt
body2 = hs_bindgen_006b9a65f3b5fd7b

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_complex_float@
foreign import ccall safe "hs_bindgen_71353f34ac72501e" hs_bindgen_71353f34ac72501e_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_complex_float@
hs_bindgen_71353f34ac72501e ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_71353f34ac72501e =
  \x0 ->
    \x1 ->
      hs_bindgen_71353f34ac72501e_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_71353f34ac72501e arg10 arg22)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_complex_double@
foreign import ccall safe "hs_bindgen_7e7e013681771442" hs_bindgen_7e7e013681771442_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_args_complex_double@
hs_bindgen_7e7e013681771442 ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_7e7e013681771442 =
  \x0 ->
    \x1 ->
      hs_bindgen_7e7e013681771442_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_7e7e013681771442 arg10 arg22)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_complex_float@
foreign import ccall safe "hs_bindgen_8d1d455a1977835c" hs_bindgen_8d1d455a1977835c_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_complex_float@
hs_bindgen_8d1d455a1977835c ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_8d1d455a1977835c =
  \x0 ->
    \x1 ->
      hs_bindgen_8d1d455a1977835c_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_8d1d455a1977835c arg10 res1)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_complex_double@
foreign import ccall safe "hs_bindgen_3e5cbc667a775bba" hs_bindgen_3e5cbc667a775bba_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_ret_complex_double@
hs_bindgen_3e5cbc667a775bba ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_3e5cbc667a775bba =
  \x0 ->
    \x1 ->
      hs_bindgen_3e5cbc667a775bba_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_3e5cbc667a775bba arg10 res1)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_bespoke_args1@
foreign import ccall safe "hs_bindgen_d585f15b3d1be58e" hs_bindgen_d585f15b3d1be58e_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_bespoke_args1@
hs_bindgen_d585f15b3d1be58e ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_d585f15b3d1be58e =
  \x0 ->
    \x1 ->
      hs_bindgen_d585f15b3d1be58e_base (BG.toFFIType x0) (BG.toFFIType x1)

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
bespoke_args1 = hs_bindgen_d585f15b3d1be58e

-- __unique:__ @test_macrosreparse_2_raw Example Safe_bespoke_args2@
foreign import ccall safe "hs_bindgen_39421ecbc2568b72" hs_bindgen_39421ecbc2568b72_base ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_bespoke_args2@
hs_bindgen_39421ecbc2568b72 ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_39421ecbc2568b72 =
  \x0 ->
    \x1 ->
      hs_bindgen_39421ecbc2568b72_base (BG.toFFIType x0) (BG.toFFIType x1)

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
bespoke_args2 = hs_bindgen_39421ecbc2568b72

-- __unique:__ @test_macrosreparse_2_raw Example Safe_bespoke_ret1@
foreign import ccall safe "hs_bindgen_5f99662d9d79771a" hs_bindgen_5f99662d9d79771a_base ::
     BG.CInt
  -> IO BG.CBool

-- __unique:__ @test_macrosreparse_2_raw Example Safe_bespoke_ret1@
hs_bindgen_5f99662d9d79771a ::
     BG.CInt
  -> IO BG.CBool
hs_bindgen_5f99662d9d79771a =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_5f99662d9d79771a_base (BG.toFFIType x0))

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h 97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
bespoke_ret1 = hs_bindgen_5f99662d9d79771a

-- __unique:__ @test_macrosreparse_2_raw Example Safe_bespoke_ret2@
foreign import ccall safe "hs_bindgen_72db3209eda47b4e" hs_bindgen_72db3209eda47b4e_base ::
     BG.CInt
  -> IO HsBindgen.Runtime.LibC.CSize

-- __unique:__ @test_macrosreparse_2_raw Example Safe_bespoke_ret2@
hs_bindgen_72db3209eda47b4e ::
     BG.CInt
  -> IO HsBindgen.Runtime.LibC.CSize
hs_bindgen_72db3209eda47b4e =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_72db3209eda47b4e_base (BG.toFFIType x0))

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h 98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO HsBindgen.Runtime.LibC.CSize
bespoke_ret2 = hs_bindgen_72db3209eda47b4e

-- __unique:__ @test_macrosreparse_2_raw Example Safe_arr_args1@
foreign import ccall safe "hs_bindgen_9908d04d7c286fa9" hs_bindgen_9908d04d7c286fa9_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_arr_args1@
hs_bindgen_9908d04d7c286fa9 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt))
  -> IO ()
hs_bindgen_9908d04d7c286fa9 =
  \x0 ->
    hs_bindgen_9908d04d7c286fa9_base (BG.toFFIType x0)

{-| Arrays

    __C declaration:__ @arr_args1@

    __defined at:__ @macros\/reparse.h 104:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args1 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args1 = hs_bindgen_9908d04d7c286fa9

-- __unique:__ @test_macrosreparse_2_raw Example Safe_arr_args2@
foreign import ccall safe "hs_bindgen_d808b5c72794b7fb" hs_bindgen_d808b5c72794b7fb_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_arr_args2@
hs_bindgen_d808b5c72794b7fb ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_d808b5c72794b7fb =
  \x0 ->
    hs_bindgen_d808b5c72794b7fb_base (BG.toFFIType x0)

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h 105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args2 = hs_bindgen_d808b5c72794b7fb

-- __unique:__ @test_macrosreparse_2_raw Example Safe_arr_args3@
foreign import ccall safe "hs_bindgen_a3043995480dc3c7" hs_bindgen_a3043995480dc3c7_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_arr_args3@
hs_bindgen_a3043995480dc3c7 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt))
  -> IO ()
hs_bindgen_a3043995480dc3c7 =
  \x0 ->
    hs_bindgen_a3043995480dc3c7_base (BG.toFFIType x0)

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h 106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args3 = hs_bindgen_a3043995480dc3c7

-- __unique:__ @test_macrosreparse_2_raw Example Safe_arr_args4@
foreign import ccall safe "hs_bindgen_eaaa3aa8df344e37" hs_bindgen_eaaa3aa8df344e37_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_arr_args4@
hs_bindgen_eaaa3aa8df344e37 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_eaaa3aa8df344e37 =
  \x0 ->
    hs_bindgen_eaaa3aa8df344e37_base (BG.toFFIType x0)

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h 107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args4 = hs_bindgen_eaaa3aa8df344e37

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_args1@
foreign import ccall safe "hs_bindgen_86e5488952a832f2" hs_bindgen_86e5488952a832f2_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_args1@
hs_bindgen_86e5488952a832f2 ::
     BG.CInt
  -> BG.FunPtr (IO ())
  -> IO ()
hs_bindgen_86e5488952a832f2 =
  \x0 ->
    \x1 ->
      hs_bindgen_86e5488952a832f2_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args1 = hs_bindgen_86e5488952a832f2

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_args2@
foreign import ccall safe "hs_bindgen_4a71cc92db6973bd" hs_bindgen_4a71cc92db6973bd_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_args2@
hs_bindgen_4a71cc92db6973bd ::
     BG.CInt
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_4a71cc92db6973bd =
  \x0 ->
    \x1 ->
      hs_bindgen_4a71cc92db6973bd_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args2 = hs_bindgen_4a71cc92db6973bd

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_args3@
foreign import ccall safe "hs_bindgen_5707842a1cf30ab9" hs_bindgen_5707842a1cf30ab9_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_args3@
hs_bindgen_5707842a1cf30ab9 ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> IO ())
  -> IO ()
hs_bindgen_5707842a1cf30ab9 =
  \x0 ->
    \x1 ->
      hs_bindgen_5707842a1cf30ab9_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args3 = hs_bindgen_5707842a1cf30ab9

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_args4@
foreign import ccall safe "hs_bindgen_ec3f1d5abdd36f96" hs_bindgen_ec3f1d5abdd36f96_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_args4@
hs_bindgen_ec3f1d5abdd36f96 ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar)
  -> IO ()
hs_bindgen_ec3f1d5abdd36f96 =
  \x0 ->
    \x1 ->
      hs_bindgen_ec3f1d5abdd36f96_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args4 = hs_bindgen_ec3f1d5abdd36f96

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_args5@
foreign import ccall safe "hs_bindgen_dca64973a93f6621" hs_bindgen_dca64973a93f6621_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_args5@
hs_bindgen_dca64973a93f6621 ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))
  -> IO ()
hs_bindgen_dca64973a93f6621 =
  \x0 ->
    \x1 ->
      hs_bindgen_dca64973a93f6621_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args5 = hs_bindgen_dca64973a93f6621

-- __unique:__ @test_macrosreparse_2_raw Example Safe_comments1@
foreign import ccall safe "hs_bindgen_a193cd70b32b5f13" hs_bindgen_a193cd70b32b5f13_base ::
     BG.CInt
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_comments1@
hs_bindgen_a193cd70b32b5f13 ::
     BG.CInt
  -> IO ()
hs_bindgen_a193cd70b32b5f13 =
  \x0 ->
    hs_bindgen_a193cd70b32b5f13_base (BG.toFFIType x0)

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
comments1 = hs_bindgen_a193cd70b32b5f13

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_prim_before1@
foreign import ccall safe "hs_bindgen_412e4980d7ac8631" hs_bindgen_412e4980d7ac8631_base ::
     BG.CInt
  -> BG.CChar
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_prim_before1@
hs_bindgen_412e4980d7ac8631 ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_412e4980d7ac8631 =
  \x0 ->
    \x1 ->
      hs_bindgen_412e4980d7ac8631_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_before1 = hs_bindgen_412e4980d7ac8631

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_prim_before2@
foreign import ccall safe "hs_bindgen_933eadeedf6bf0f7" hs_bindgen_933eadeedf6bf0f7_base ::
     BG.CInt
  -> BG.CSChar
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_prim_before2@
hs_bindgen_933eadeedf6bf0f7 ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_933eadeedf6bf0f7 =
  \x0 ->
    \x1 ->
      hs_bindgen_933eadeedf6bf0f7_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_before2 = hs_bindgen_933eadeedf6bf0f7

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_prim_before3@
foreign import ccall safe "hs_bindgen_e791974bd8a43111" hs_bindgen_e791974bd8a43111_base ::
     BG.CInt
  -> BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_prim_before3@
hs_bindgen_e791974bd8a43111 ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_e791974bd8a43111 =
  \x0 ->
    \x1 ->
      hs_bindgen_e791974bd8a43111_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_before3 = hs_bindgen_e791974bd8a43111

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_prim_after1@
foreign import ccall safe "hs_bindgen_e728e8a8d01615d7" hs_bindgen_e728e8a8d01615d7_base ::
     BG.CInt
  -> BG.CChar
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_prim_after1@
hs_bindgen_e728e8a8d01615d7 ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_e728e8a8d01615d7 =
  \x0 ->
    \x1 ->
      hs_bindgen_e728e8a8d01615d7_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_after1 = hs_bindgen_e728e8a8d01615d7

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_prim_after2@
foreign import ccall safe "hs_bindgen_e0c8637468d97f01" hs_bindgen_e0c8637468d97f01_base ::
     BG.CInt
  -> BG.CSChar
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_prim_after2@
hs_bindgen_e0c8637468d97f01 ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_e0c8637468d97f01 =
  \x0 ->
    \x1 ->
      hs_bindgen_e0c8637468d97f01_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_after2 = hs_bindgen_e0c8637468d97f01

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_prim_after3@
foreign import ccall safe "hs_bindgen_3c23b6ee9b0dd3a5" hs_bindgen_3c23b6ee9b0dd3a5_base ::
     BG.CInt
  -> BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_prim_after3@
hs_bindgen_3c23b6ee9b0dd3a5 ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_3c23b6ee9b0dd3a5 =
  \x0 ->
    \x1 ->
      hs_bindgen_3c23b6ee9b0dd3a5_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_after3 = hs_bindgen_3c23b6ee9b0dd3a5

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_before1@
foreign import ccall safe "hs_bindgen_96c4e34cd8bb8e17" hs_bindgen_96c4e34cd8bb8e17_base ::
     BG.CInt
  -> BG.CFloat
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_before1@
hs_bindgen_96c4e34cd8bb8e17 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_96c4e34cd8bb8e17 =
  \x0 ->
    \x1 ->
      hs_bindgen_96c4e34cd8bb8e17_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_96c4e34cd8bb8e17

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_before2@
foreign import ccall safe "hs_bindgen_78976b65f960adb7" hs_bindgen_78976b65f960adb7_base ::
     BG.CInt
  -> BG.CDouble
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_before2@
hs_bindgen_78976b65f960adb7 ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_78976b65f960adb7 =
  \x0 ->
    \x1 ->
      hs_bindgen_78976b65f960adb7_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_78976b65f960adb7

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_before3@
foreign import ccall safe "hs_bindgen_6442f484870b498e" hs_bindgen_6442f484870b498e_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_before3@
hs_bindgen_6442f484870b498e ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_6442f484870b498e =
  \x0 ->
    \x1 ->
      hs_bindgen_6442f484870b498e_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_6442f484870b498e

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_before4@
foreign import ccall safe "hs_bindgen_36cd5314ad34356d" hs_bindgen_36cd5314ad34356d_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_before4@
hs_bindgen_36cd5314ad34356d ::
     BG.CInt
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_36cd5314ad34356d =
  \x0 ->
    \x1 ->
      hs_bindgen_36cd5314ad34356d_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_36cd5314ad34356d arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_before5@
foreign import ccall safe "hs_bindgen_e5e2292a0d6193a3" hs_bindgen_e5e2292a0d6193a3_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_before5@
hs_bindgen_e5e2292a0d6193a3 ::
     BG.CInt
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_e5e2292a0d6193a3 =
  \x0 ->
    \x1 ->
      hs_bindgen_e5e2292a0d6193a3_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_e5e2292a0d6193a3 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_before6@
foreign import ccall safe "hs_bindgen_ea0a75d094db765e" hs_bindgen_ea0a75d094db765e_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_before6@
hs_bindgen_ea0a75d094db765e ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_ea0a75d094db765e =
  \x0 ->
    \x1 ->
      hs_bindgen_ea0a75d094db765e_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_ea0a75d094db765e

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_before7@
foreign import ccall safe "hs_bindgen_59163b17198cc53d" hs_bindgen_59163b17198cc53d_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_before7@
hs_bindgen_59163b17198cc53d ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_59163b17198cc53d =
  \x0 ->
    \x1 ->
      hs_bindgen_59163b17198cc53d_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_59163b17198cc53d

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_before8@
foreign import ccall safe "hs_bindgen_55ab156fb365279f" hs_bindgen_55ab156fb365279f_base ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_before8@
hs_bindgen_55ab156fb365279f ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_55ab156fb365279f =
  \x0 ->
    \x1 ->
      hs_bindgen_55ab156fb365279f_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_55ab156fb365279f

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_after1@
foreign import ccall safe "hs_bindgen_220f14133b0ccd33" hs_bindgen_220f14133b0ccd33_base ::
     BG.CInt
  -> BG.CFloat
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_after1@
hs_bindgen_220f14133b0ccd33 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_220f14133b0ccd33 =
  \x0 ->
    \x1 ->
      hs_bindgen_220f14133b0ccd33_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_220f14133b0ccd33

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_after2@
foreign import ccall safe "hs_bindgen_829f969c97ab4d34" hs_bindgen_829f969c97ab4d34_base ::
     BG.CInt
  -> BG.CDouble
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_after2@
hs_bindgen_829f969c97ab4d34 ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_829f969c97ab4d34 =
  \x0 ->
    \x1 ->
      hs_bindgen_829f969c97ab4d34_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_829f969c97ab4d34

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_after3@
foreign import ccall safe "hs_bindgen_f19ec2ea4dadc1b8" hs_bindgen_f19ec2ea4dadc1b8_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_after3@
hs_bindgen_f19ec2ea4dadc1b8 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_f19ec2ea4dadc1b8 =
  \x0 ->
    \x1 ->
      hs_bindgen_f19ec2ea4dadc1b8_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_f19ec2ea4dadc1b8

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_after4@
foreign import ccall safe "hs_bindgen_75a26a0fa2d77097" hs_bindgen_75a26a0fa2d77097_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_after4@
hs_bindgen_75a26a0fa2d77097 ::
     BG.CInt
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_75a26a0fa2d77097 =
  \x0 ->
    \x1 ->
      hs_bindgen_75a26a0fa2d77097_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_75a26a0fa2d77097 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_after5@
foreign import ccall safe "hs_bindgen_313ee3b2640f39a4" hs_bindgen_313ee3b2640f39a4_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_after5@
hs_bindgen_313ee3b2640f39a4 ::
     BG.CInt
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_313ee3b2640f39a4 =
  \x0 ->
    \x1 ->
      hs_bindgen_313ee3b2640f39a4_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_313ee3b2640f39a4 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_after6@
foreign import ccall safe "hs_bindgen_9362037465858f18" hs_bindgen_9362037465858f18_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_after6@
hs_bindgen_9362037465858f18 ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_9362037465858f18 =
  \x0 ->
    \x1 ->
      hs_bindgen_9362037465858f18_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_9362037465858f18

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_after7@
foreign import ccall safe "hs_bindgen_8443bb74c444d10d" hs_bindgen_8443bb74c444d10d_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_after7@
hs_bindgen_8443bb74c444d10d ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_8443bb74c444d10d =
  \x0 ->
    \x1 ->
      hs_bindgen_8443bb74c444d10d_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_8443bb74c444d10d

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_after8@
foreign import ccall safe "hs_bindgen_bac50d77b1418e14" hs_bindgen_bac50d77b1418e14_base ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_withoutSign_after8@
hs_bindgen_bac50d77b1418e14 ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_bac50d77b1418e14 =
  \x0 ->
    \x1 ->
      hs_bindgen_bac50d77b1418e14_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_bac50d77b1418e14

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_args1@
foreign import ccall safe "hs_bindgen_712dc1f0f32c8c44" hs_bindgen_712dc1f0f32c8c44_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_args1@
hs_bindgen_712dc1f0f32c8c44 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_712dc1f0f32c8c44 =
  \x0 ->
    \x1 ->
      hs_bindgen_712dc1f0f32c8c44_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args1 = hs_bindgen_712dc1f0f32c8c44

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_args2@
foreign import ccall safe "hs_bindgen_90355094b871f8ef" hs_bindgen_90355094b871f8ef_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_args2@
hs_bindgen_90355094b871f8ef ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_90355094b871f8ef =
  \x0 ->
    \x1 ->
      hs_bindgen_90355094b871f8ef_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args2 = hs_bindgen_90355094b871f8ef

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_args3@
foreign import ccall safe "hs_bindgen_e23d28351613e5d3" hs_bindgen_e23d28351613e5d3_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_args3@
hs_bindgen_e23d28351613e5d3 ::
     BG.CInt
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_e23d28351613e5d3 =
  \x0 ->
    \x1 ->
      hs_bindgen_e23d28351613e5d3_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args3 = hs_bindgen_e23d28351613e5d3

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_args4@
foreign import ccall safe "hs_bindgen_ec4147bad69bedd8" hs_bindgen_ec4147bad69bedd8_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_args4@
hs_bindgen_ec4147bad69bedd8 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_ec4147bad69bedd8 =
  \x0 ->
    \x1 ->
      hs_bindgen_ec4147bad69bedd8_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args4 = hs_bindgen_ec4147bad69bedd8

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_args5@
foreign import ccall safe "hs_bindgen_82c92ffdbf494af2" hs_bindgen_82c92ffdbf494af2_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_args5@
hs_bindgen_82c92ffdbf494af2 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_82c92ffdbf494af2 =
  \x0 ->
    \x1 ->
      hs_bindgen_82c92ffdbf494af2_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args5 = hs_bindgen_82c92ffdbf494af2

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_ret1@
foreign import ccall safe "hs_bindgen_f806bbcad8e0495c" hs_bindgen_f806bbcad8e0495c_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_ret1@
hs_bindgen_f806bbcad8e0495c ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_f806bbcad8e0495c =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_f806bbcad8e0495c_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h 212:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret1 = hs_bindgen_f806bbcad8e0495c

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_ret2@
foreign import ccall safe "hs_bindgen_af8f0bcf949618f4" hs_bindgen_af8f0bcf949618f4_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_ret2@
hs_bindgen_af8f0bcf949618f4 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_af8f0bcf949618f4 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_af8f0bcf949618f4_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h 213:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret2 = hs_bindgen_af8f0bcf949618f4

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_ret3@
foreign import ccall safe "hs_bindgen_9748e1e80a09647c" hs_bindgen_9748e1e80a09647c_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_ret3@
hs_bindgen_9748e1e80a09647c ::
     BG.CInt
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_9748e1e80a09647c =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_9748e1e80a09647c_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h 214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
const_pointers_ret3 = hs_bindgen_9748e1e80a09647c

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_ret4@
foreign import ccall safe "hs_bindgen_cc89d05287cb4319" hs_bindgen_cc89d05287cb4319_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_ret4@
hs_bindgen_cc89d05287cb4319 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_cc89d05287cb4319 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_cc89d05287cb4319_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h 215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret4 = hs_bindgen_cc89d05287cb4319

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_ret5@
foreign import ccall safe "hs_bindgen_5de9d5d3dd272d80" hs_bindgen_5de9d5d3dd272d80_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_pointers_ret5@
hs_bindgen_5de9d5d3dd272d80 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_5de9d5d3dd272d80 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_5de9d5d3dd272d80_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h 216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret5 = hs_bindgen_5de9d5d3dd272d80

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_array_elem1@
foreign import ccall safe "hs_bindgen_1870bdff8b748c5f" hs_bindgen_1870bdff8b748c5f_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_array_elem1@
hs_bindgen_1870bdff8b748c5f ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt))
  -> IO ()
hs_bindgen_1870bdff8b748c5f =
  \x0 ->
    hs_bindgen_1870bdff8b748c5f_base (BG.toFFIType x0)

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h 244:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem1 = hs_bindgen_1870bdff8b748c5f

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_array_elem2@
foreign import ccall safe "hs_bindgen_f232c98309b5749f" hs_bindgen_f232c98309b5749f_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_array_elem2@
hs_bindgen_f232c98309b5749f ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt)))
  -> IO ()
hs_bindgen_f232c98309b5749f =
  \x0 ->
    hs_bindgen_f232c98309b5749f_base (BG.toFFIType x0)

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h 245:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem2 = hs_bindgen_f232c98309b5749f

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_array_elem3@
foreign import ccall safe "hs_bindgen_66d7048a1fdf1ca4" hs_bindgen_66d7048a1fdf1ca4_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_const_array_elem3@
hs_bindgen_66d7048a1fdf1ca4 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_66d7048a1fdf1ca4 =
  \x0 ->
    hs_bindgen_66d7048a1fdf1ca4_base (BG.toFFIType x0)

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h 246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem3 = hs_bindgen_66d7048a1fdf1ca4

-- __unique:__ @test_macrosreparse_2_raw Example Safe_noParams1@
foreign import ccall safe "hs_bindgen_c671fc6cdc77dcb2" hs_bindgen_c671fc6cdc77dcb2_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse_2_raw Example Safe_noParams1@
hs_bindgen_c671fc6cdc77dcb2 :: IO BG.CInt
hs_bindgen_c671fc6cdc77dcb2 =
  fmap BG.fromFFIType hs_bindgen_c671fc6cdc77dcb2_base

{-| Other examples we reparsed /incorrectly/ before language-c

    __C declaration:__ @noParams1@

    __defined at:__ @macros\/reparse.h 254:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams1 :: IO BG.CInt
noParams1 = hs_bindgen_c671fc6cdc77dcb2

-- __unique:__ @test_macrosreparse_2_raw Example Safe_noParams2@
foreign import ccall safe "hs_bindgen_74de93eb9dfe14ce" hs_bindgen_74de93eb9dfe14ce_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse_2_raw Example Safe_noParams2@
hs_bindgen_74de93eb9dfe14ce :: IO BG.CInt
hs_bindgen_74de93eb9dfe14ce =
  fmap BG.fromFFIType hs_bindgen_74de93eb9dfe14ce_base

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h 255:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2 :: IO BG.CInt
noParams2 = hs_bindgen_74de93eb9dfe14ce

-- __unique:__ @test_macrosreparse_2_raw Example Safe_noParams3@
foreign import ccall safe "hs_bindgen_4d951e605b009e92" hs_bindgen_4d951e605b009e92_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_2_raw Example Safe_noParams3@
hs_bindgen_4d951e605b009e92 ::
     BG.CInt
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_4d951e605b009e92 =
  \x0 ->
    \x1 ->
      hs_bindgen_4d951e605b009e92_base (BG.toFFIType x0) (BG.toFFIType x1)

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
noParams3 = hs_bindgen_4d951e605b009e92

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret1@
foreign import ccall safe "hs_bindgen_05c6646047255af2" hs_bindgen_05c6646047255af2_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret1@
hs_bindgen_05c6646047255af2 ::
     BG.CInt
  -> IO (BG.FunPtr (IO ()))
hs_bindgen_05c6646047255af2 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_05c6646047255af2_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h 260:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO ()))
funptr_ret1 = hs_bindgen_05c6646047255af2

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret2@
foreign import ccall safe "hs_bindgen_f1aee360f7ee4a05" hs_bindgen_f1aee360f7ee4a05_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret2@
hs_bindgen_f1aee360f7ee4a05 ::
     BG.CInt
  -> IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_f1aee360f7ee4a05 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_f1aee360f7ee4a05_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h 261:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO BG.CInt))
funptr_ret2 = hs_bindgen_f1aee360f7ee4a05

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret3@
foreign import ccall safe "hs_bindgen_fe51c3b8653fb966" hs_bindgen_fe51c3b8653fb966_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret3@
hs_bindgen_fe51c3b8653fb966 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_fe51c3b8653fb966 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_fe51c3b8653fb966_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h 262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
funptr_ret3 = hs_bindgen_fe51c3b8653fb966

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret4@
foreign import ccall safe "hs_bindgen_0d481c2be48e47b5" hs_bindgen_0d481c2be48e47b5_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret4@
hs_bindgen_0d481c2be48e47b5 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
hs_bindgen_0d481c2be48e47b5 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_0d481c2be48e47b5_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h 263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
funptr_ret4 = hs_bindgen_0d481c2be48e47b5

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret5@
foreign import ccall safe "hs_bindgen_8d733067996c6da3" hs_bindgen_8d733067996c6da3_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret5@
hs_bindgen_8d733067996c6da3 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_8d733067996c6da3 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_8d733067996c6da3_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h 267:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret5 = hs_bindgen_8d733067996c6da3

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret6@
foreign import ccall safe "hs_bindgen_157d813289af079d" hs_bindgen_157d813289af079d_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret6@
hs_bindgen_157d813289af079d ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_157d813289af079d =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_157d813289af079d_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h 268:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret6 = hs_bindgen_157d813289af079d

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret7@
foreign import ccall safe "hs_bindgen_8e577c6750e29e93" hs_bindgen_8e577c6750e29e93_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret7@
hs_bindgen_8e577c6750e29e93 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_8e577c6750e29e93 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_8e577c6750e29e93_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h 269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret7 = hs_bindgen_8e577c6750e29e93

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret8@
foreign import ccall safe "hs_bindgen_35c2200ef10c11d3" hs_bindgen_35c2200ef10c11d3_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret8@
hs_bindgen_35c2200ef10c11d3 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_35c2200ef10c11d3 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_35c2200ef10c11d3_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h 270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret8 = hs_bindgen_35c2200ef10c11d3

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret9@
foreign import ccall safe "hs_bindgen_3273d59c8a07b3d7" hs_bindgen_3273d59c8a07b3d7_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret9@
hs_bindgen_3273d59c8a07b3d7 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_3273d59c8a07b3d7 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_3273d59c8a07b3d7_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h 271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret9 = hs_bindgen_3273d59c8a07b3d7

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret10@
foreign import ccall safe "hs_bindgen_bfd584bc16ad62bd" hs_bindgen_bfd584bc16ad62bd_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw Example Safe_funptr_ret10@
hs_bindgen_bfd584bc16ad62bd ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_bfd584bc16ad62bd =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_bfd584bc16ad62bd_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h 272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret10 = hs_bindgen_bfd584bc16ad62bd
