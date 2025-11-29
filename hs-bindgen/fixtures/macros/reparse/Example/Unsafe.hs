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
  , "void hs_bindgen_test_macrosreparse_af1e1143c62302ea ("
  , "  A arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  args_char1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_501d0ff552f7301c ("
  , "  A arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  args_char2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_98e0b146df409568 ("
  , "  A arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  args_char3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_ad560219cf7921c6 ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  args_short1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_04c283940976aa1b ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  args_short2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_631e43506d128728 ("
  , "  A arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  args_short3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_e65c52d6dc020edb ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  args_int1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_5613dce389a65445 ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  args_int2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_67bd898c81d7227b ("
  , "  A arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  args_int3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_6a91ce784cb313a1 ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  args_long1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_72b02b0109e66233 ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  args_long2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_e5c904d1b3127af3 ("
  , "  A arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  args_long3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_8758b6c44499ce72 ("
  , "  A arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  args_float(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_ec4651f6652e02ef ("
  , "  A arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  args_double(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_36d53c81cd2321f0 ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  args_bool1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_15f65fb8670a8e84 ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  args_struct(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_7552254e39bca7ed ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  args_union(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_ddb265f7d6790bd7 ("
  , "  A arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  args_enum(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_d3f6bc69f7849c86 ("
  , "  A arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  args_pointer1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_c10bfa07bae664af ("
  , "  A arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  args_pointer2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_b8c5578f988e252a ("
  , "  A arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  args_pointer3(arg1, arg2);"
  , "}"
  , "A hs_bindgen_test_macrosreparse_33c90c173b4373c6 (void)"
  , "{"
  , "  return ret_A();"
  , "}"
  , "char hs_bindgen_test_macrosreparse_41e7f9960a6c4da7 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char1(arg1);"
  , "}"
  , "signed char hs_bindgen_test_macrosreparse_fdfda7d745496186 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char2(arg1);"
  , "}"
  , "unsigned char hs_bindgen_test_macrosreparse_b7d6fa41ff94fa89 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char3(arg1);"
  , "}"
  , "signed short hs_bindgen_test_macrosreparse_3644e3e8675d1c72 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short1(arg1);"
  , "}"
  , "signed short hs_bindgen_test_macrosreparse_e4c658bda7f2ad12 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short2(arg1);"
  , "}"
  , "unsigned short hs_bindgen_test_macrosreparse_ecbd825192aeb075 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short3(arg1);"
  , "}"
  , "signed int hs_bindgen_test_macrosreparse_60c2d983e70ac13e ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int1(arg1);"
  , "}"
  , "signed int hs_bindgen_test_macrosreparse_58c898f04ed4555d ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int2(arg1);"
  , "}"
  , "unsigned int hs_bindgen_test_macrosreparse_affd4023d970277a ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int3(arg1);"
  , "}"
  , "signed long hs_bindgen_test_macrosreparse_55077a464aa6ca25 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long1(arg1);"
  , "}"
  , "signed long hs_bindgen_test_macrosreparse_a10ceca015a9ebc5 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long2(arg1);"
  , "}"
  , "unsigned long hs_bindgen_test_macrosreparse_10734646f86cf27a ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long3(arg1);"
  , "}"
  , "float hs_bindgen_test_macrosreparse_ac893d85500e2435 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_float(arg1);"
  , "}"
  , "double hs_bindgen_test_macrosreparse_b225f833cfed0b10 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_double(arg1);"
  , "}"
  , "_Bool hs_bindgen_test_macrosreparse_22ba9bcd56728b25 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_bool1(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_bd18b53404e9d7c7 ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_struct(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_f5792450dce09087 ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_union(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_test_macrosreparse_beaece2937ca4210 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_enum(arg1);"
  , "}"
  , "signed int *hs_bindgen_test_macrosreparse_2ad561ee435b5c5e ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer1(arg1);"
  , "}"
  , "signed int **hs_bindgen_test_macrosreparse_a8df6812644abfc0 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer2(arg1);"
  , "}"
  , "void *hs_bindgen_test_macrosreparse_28954ab5829737a3 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer3(arg1);"
  , "}"
  , "signed int hs_bindgen_test_macrosreparse_51c10a689a11e107 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return body1(arg1);"
  , "}"
  , "A hs_bindgen_test_macrosreparse_321319d374762480 (void)"
  , "{"
  , "  return body2();"
  , "}"
  , "void hs_bindgen_test_macrosreparse_947a3dd67c8bce3a ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  args_complex_float(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_692fec14a8e08284 ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  args_complex_double(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_e04771d66d857d80 ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_complex_float(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_0d685a120da48afe ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_complex_double(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_7b036e6b403c3a65 ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  bespoke_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_8c77de65e3d0fa85 ("
  , "  A arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  bespoke_args2(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_test_macrosreparse_376de198a78d44b0 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return bespoke_ret1(arg1);"
  , "}"
  , "size_t hs_bindgen_test_macrosreparse_06adb59fd726c1db ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return bespoke_ret2(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_87c6bbf3c36aca98 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  arr_args1(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_34928e18adf21c0a ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  arr_args2(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_bc329fe5f6e4c4e8 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  arr_args3(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_be4ff8a62a045bad ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  arr_args4(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_db7b0088cf1fc452 ("
  , "  A arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  funptr_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_4c8f22976a3bc644 ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  funptr_args2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_45b43973c3b04e01 ("
  , "  A arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  funptr_args3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_769674dfbbf6569e ("
  , "  A arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  funptr_args4(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_f175875e2635455e ("
  , "  A arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  funptr_args5(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_463a652dca2fc03a ("
  , "  A arg1"
  , ")"
  , "{"
  , "  comments1(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_ce2ada6523d8552f ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  const_prim_before1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_90dec95fbf41bcd6 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  const_prim_before2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_d153ebb7b57e4a83 ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  const_prim_before3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_59fea5c0bcef05fd ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  const_prim_after1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_ea8258d2ac6e96d9 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  const_prim_after2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_bd7e219e9ff23fb3 ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  const_prim_after3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_35e3ffd7e7d7fd15 ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_8d797998e439c1a8 ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_a4ac00c834e54f4b ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_e74f528cd3f292b0 ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before4(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_e8cf90493802e8a3 ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before5(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_78255d23a87936e3 ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before6(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_e4bbd1c3025c39b3 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before7(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_cd0bf3fcc1a085de ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before8(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_e5ad0fd6c1c3fb22 ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_e00177702c028786 ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_4211bb97b8e5ec9d ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_70a7c65b8189c544 ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after4(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_7a4913f72072a2cd ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after5(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_068b4fa4f0b7c12d ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after6(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_f2126533e5ce3ef7 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after7(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_cd9dc671fca386f7 ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after8(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_9c5fd3b54457ecaf ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  const_pointers_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_0e20cab2a42cd76b ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  const_pointers_args2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_c9b9487dbe2d10f7 ("
  , "  A arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_bf44d5f56595c465 ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args4(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_f26e1110a2acb141 ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args5(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_test_macrosreparse_933d622d42a60fdc ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret1(arg1);"
  , "}"
  , "signed int const *hs_bindgen_test_macrosreparse_c51590c060ca121d ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret2(arg1);"
  , "}"
  , "signed int *const hs_bindgen_test_macrosreparse_6f33756ca0c65fec ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret3(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_test_macrosreparse_bf1480b7e32fddd5 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret4(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_test_macrosreparse_0b0d7702340c358f ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret5(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_57cf6165fd843fa0 ("
  , "  A const *arg1"
  , ")"
  , "{"
  , "  const_array_elem1(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_d6100076dff26395 ("
  , "  A const **arg1"
  , ")"
  , "{"
  , "  const_array_elem2(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_1f08907d684817a5 ("
  , "  A *const *arg1"
  , ")"
  , "{"
  , "  const_array_elem3(arg1);"
  , "}"
  , "A hs_bindgen_test_macrosreparse_6871249df314a1eb (void)"
  , "{"
  , "  return noParams1();"
  , "}"
  , "A hs_bindgen_test_macrosreparse_4c45c6083bc5aec1 (void)"
  , "{"
  , "  return noParams2();"
  , "}"
  , "void hs_bindgen_test_macrosreparse_906feff84b3ac80e ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  noParams3(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_test_macrosreparse_730ece1626cf42e8 ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return funptr_ret1(arg1);"
  , "}"
  , "signed int (*hs_bindgen_test_macrosreparse_9758f5635e06a923 ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return funptr_ret2(arg1);"
  , "}"
  , "void (*hs_bindgen_test_macrosreparse_e38910e62fbdae04 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return funptr_ret3(arg1);"
  , "}"
  , "char (*hs_bindgen_test_macrosreparse_872917b0121e2e59 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret4(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_test_macrosreparse_158b72a6ad76f269 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret5(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_test_macrosreparse_d93f25dfebe176cd ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret6(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_test_macrosreparse_c337ebe392f0b63b ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret7(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_test_macrosreparse_fd30dddab3efcfbc ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret8(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_test_macrosreparse_3c50debb733b1543 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret9(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_test_macrosreparse_81b1cb42017a813e ("
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

__unique:__ @Example_Unsafe_args_char1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_af1e1143c62302ea" args_char1 ::
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

    __unique:__ @Example_Unsafe_args_char2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_501d0ff552f7301c" args_char2 ::
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

    __unique:__ @Example_Unsafe_args_char3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_98e0b146df409568" args_char3 ::
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

    __unique:__ @Example_Unsafe_args_short1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_ad560219cf7921c6" args_short1 ::
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

    __unique:__ @Example_Unsafe_args_short2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_04c283940976aa1b" args_short2 ::
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

    __unique:__ @Example_Unsafe_args_short3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_631e43506d128728" args_short3 ::
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

    __unique:__ @Example_Unsafe_args_int1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e65c52d6dc020edb" args_int1 ::
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

    __unique:__ @Example_Unsafe_args_int2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_5613dce389a65445" args_int2 ::
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

    __unique:__ @Example_Unsafe_args_int3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_67bd898c81d7227b" args_int3 ::
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

    __unique:__ @Example_Unsafe_args_long1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_6a91ce784cb313a1" args_long1 ::
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

    __unique:__ @Example_Unsafe_args_long2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_72b02b0109e66233" args_long2 ::
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

    __unique:__ @Example_Unsafe_args_long3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e5c904d1b3127af3" args_long3 ::
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

    __unique:__ @Example_Unsafe_args_float@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_8758b6c44499ce72" args_float ::
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

    __unique:__ @Example_Unsafe_args_double@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_ec4651f6652e02ef" args_double ::
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

    __unique:__ @Example_Unsafe_args_bool1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_36d53c81cd2321f0" args_bool1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| Pointer-based API for 'args_struct'

__unique:__ @Example_Unsafe_args_struct@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_15f65fb8670a8e84" args_struct_wrapper ::
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

__unique:__ @Example_Unsafe_args_union@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_7552254e39bca7ed" args_union_wrapper ::
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

    __unique:__ @Example_Unsafe_args_enum@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_ddb265f7d6790bd7" args_enum ::
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

    __unique:__ @Example_Unsafe_args_pointer1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_d3f6bc69f7849c86" args_pointer1 ::
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

    __unique:__ @Example_Unsafe_args_pointer2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c10bfa07bae664af" args_pointer2 ::
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

    __unique:__ @Example_Unsafe_args_pointer3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_b8c5578f988e252a" args_pointer3 ::
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

    __unique:__ @Example_Unsafe_ret_A@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_33c90c173b4373c6" ret_A ::
     IO A

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h:49:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_ret_char1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_41e7f9960a6c4da7" ret_char1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CChar

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h:50:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_ret_char2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_fdfda7d745496186" ret_char2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CSChar

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h:51:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_ret_char3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_b7d6fa41ff94fa89" ret_char3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CUChar

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h:53:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_ret_short1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_3644e3e8675d1c72" ret_short1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CShort

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h:54:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_ret_short2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e4c658bda7f2ad12" ret_short2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CShort

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h:55:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_ret_short3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_ecbd825192aeb075" ret_short3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CUShort

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h:57:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_ret_int1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_60c2d983e70ac13e" ret_int1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h:58:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_ret_int2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_58c898f04ed4555d" ret_int2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h:59:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_ret_int3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_affd4023d970277a" ret_int3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CUInt

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h:61:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_ret_long1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_55077a464aa6ca25" ret_long1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CLong

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h:62:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_ret_long2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_a10ceca015a9ebc5" ret_long2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CLong

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h:63:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_ret_long3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_10734646f86cf27a" ret_long3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CULong

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h:65:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_ret_float@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_ac893d85500e2435" ret_float ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CFloat

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h:66:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_ret_double@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_b225f833cfed0b10" ret_double ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CDouble

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h:67:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_ret_bool1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_22ba9bcd56728b25" ret_bool1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CBool

{-| Pointer-based API for 'ret_struct'

__unique:__ @Example_Unsafe_ret_struct@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_bd18b53404e9d7c7" ret_struct_wrapper ::
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

__unique:__ @Example_Unsafe_ret_union@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_f5792450dce09087" ret_union_wrapper ::
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

    __unique:__ @Example_Unsafe_ret_enum@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_beaece2937ca4210" ret_enum ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO Some_enum

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h:73:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_ret_pointer1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_2ad561ee435b5c5e" ret_pointer1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h:74:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_ret_pointer2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_a8df6812644abfc0" ret_pointer2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h:75:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_ret_pointer3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_28954ab5829737a3" ret_pointer3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h:79:5@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_body1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_51c10a689a11e107" body1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h:80:3@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_body2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_321319d374762480" body2 ::
     IO A

{-| Pointer-based API for 'args_complex_float'

__unique:__ @Example_Unsafe_args_complex_float@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_947a3dd67c8bce3a" args_complex_float_wrapper ::
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

__unique:__ @Example_Unsafe_args_complex_double@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_692fec14a8e08284" args_complex_double_wrapper ::
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

__unique:__ @Example_Unsafe_ret_complex_float@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e04771d66d857d80" ret_complex_float_wrapper ::
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

__unique:__ @Example_Unsafe_ret_complex_double@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_0d685a120da48afe" ret_complex_double_wrapper ::
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

    __unique:__ @Example_Unsafe_bespoke_args1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_7b036e6b403c3a65" bespoke_args1 ::
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

    __unique:__ @Example_Unsafe_bespoke_args2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_8c77de65e3d0fa85" bespoke_args2 ::
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

    __unique:__ @Example_Unsafe_bespoke_ret1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_376de198a78d44b0" bespoke_ret1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CBool

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h:98:8@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_bespoke_ret2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_06adb59fd726c1db" bespoke_ret2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO HsBindgen.Runtime.Prelude.CSize

{-| Arrays

__C declaration:__ @arr_args1@

__defined at:__ @macros\/reparse.h:104:6@

__exported by:__ @macros\/reparse.h@

__unique:__ @Example_Unsafe_arr_args1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_87c6bbf3c36aca98" arr_args1 ::
     Ptr.Ptr A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h:105:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_arr_args2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_34928e18adf21c0a" arr_args2 ::
     Ptr.Ptr (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h:106:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_arr_args3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_bc329fe5f6e4c4e8" arr_args3 ::
     Ptr.Ptr A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h:107:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_arr_args4@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_be4ff8a62a045bad" arr_args4 ::
     Ptr.Ptr (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| Function pointers

__C declaration:__ @funptr_args1@

__defined at:__ @macros\/reparse.h:126:6@

__exported by:__ @macros\/reparse.h@

__unique:__ @Example_Unsafe_funptr_args1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_db7b0088cf1fc452" funptr_args1 ::
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

    __unique:__ @Example_Unsafe_funptr_args2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_4c8f22976a3bc644" funptr_args2 ::
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

    __unique:__ @Example_Unsafe_funptr_args3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_45b43973c3b04e01" funptr_args3 ::
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

    __unique:__ @Example_Unsafe_funptr_args4@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_769674dfbbf6569e" funptr_args4 ::
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

    __unique:__ @Example_Unsafe_funptr_args5@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_f175875e2635455e" funptr_args5 ::
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

__unique:__ @Example_Unsafe_comments1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_463a652dca2fc03a" comments1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| `const` qualifier

  NOTE: These were not parsed correctly prior to the switch to language-c.

__C declaration:__ @const_prim_before1@

__defined at:__ @macros\/reparse.h:179:6@

__exported by:__ @macros\/reparse.h@

__unique:__ @Example_Unsafe_const_prim_before1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_ce2ada6523d8552f" const_prim_before1 ::
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

    __unique:__ @Example_Unsafe_const_prim_before2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_90dec95fbf41bcd6" const_prim_before2 ::
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

    __unique:__ @Example_Unsafe_const_prim_before3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_d153ebb7b57e4a83" const_prim_before3 ::
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

    __unique:__ @Example_Unsafe_const_prim_after1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_59fea5c0bcef05fd" const_prim_after1 ::
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

    __unique:__ @Example_Unsafe_const_prim_after2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_ea8258d2ac6e96d9" const_prim_after2 ::
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

    __unique:__ @Example_Unsafe_const_prim_after3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_bd7e219e9ff23fb3" const_prim_after3 ::
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

    __unique:__ @Example_Unsafe_const_withoutSign_before1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_35e3ffd7e7d7fd15" const_withoutSign_before1 ::
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

    __unique:__ @Example_Unsafe_const_withoutSign_before2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_8d797998e439c1a8" const_withoutSign_before2 ::
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

    __unique:__ @Example_Unsafe_const_withoutSign_before3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_a4ac00c834e54f4b" const_withoutSign_before3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| Pointer-based API for 'const_withoutSign_before4'

__unique:__ @Example_Unsafe_const_withoutSign_before4@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e74f528cd3f292b0" const_withoutSign_before4_wrapper ::
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

__unique:__ @Example_Unsafe_const_withoutSign_before5@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e8cf90493802e8a3" const_withoutSign_before5_wrapper ::
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

    __unique:__ @Example_Unsafe_const_withoutSign_before6@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_78255d23a87936e3" const_withoutSign_before6 ::
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

    __unique:__ @Example_Unsafe_const_withoutSign_before7@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e4bbd1c3025c39b3" const_withoutSign_before7 ::
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

    __unique:__ @Example_Unsafe_const_withoutSign_before8@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_cd0bf3fcc1a085de" const_withoutSign_before8 ::
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

    __unique:__ @Example_Unsafe_const_withoutSign_after1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e5ad0fd6c1c3fb22" const_withoutSign_after1 ::
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

    __unique:__ @Example_Unsafe_const_withoutSign_after2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e00177702c028786" const_withoutSign_after2 ::
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

    __unique:__ @Example_Unsafe_const_withoutSign_after3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_4211bb97b8e5ec9d" const_withoutSign_after3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| Pointer-based API for 'const_withoutSign_after4'

__unique:__ @Example_Unsafe_const_withoutSign_after4@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_70a7c65b8189c544" const_withoutSign_after4_wrapper ::
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

__unique:__ @Example_Unsafe_const_withoutSign_after5@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_7a4913f72072a2cd" const_withoutSign_after5_wrapper ::
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

    __unique:__ @Example_Unsafe_const_withoutSign_after6@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_068b4fa4f0b7c12d" const_withoutSign_after6 ::
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

    __unique:__ @Example_Unsafe_const_withoutSign_after7@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_f2126533e5ce3ef7" const_withoutSign_after7 ::
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

    __unique:__ @Example_Unsafe_const_withoutSign_after8@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_cd9dc671fca386f7" const_withoutSign_after8 ::
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

    __unique:__ @Example_Unsafe_const_pointers_args1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_9c5fd3b54457ecaf" const_pointers_args1 ::
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

    __unique:__ @Example_Unsafe_const_pointers_args2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_0e20cab2a42cd76b" const_pointers_args2 ::
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

    __unique:__ @Example_Unsafe_const_pointers_args3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c9b9487dbe2d10f7" const_pointers_args3 ::
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

    __unique:__ @Example_Unsafe_const_pointers_args4@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_bf44d5f56595c465" const_pointers_args4 ::
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

    __unique:__ @Example_Unsafe_const_pointers_args5@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_f26e1110a2acb141" const_pointers_args5 ::
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

    __unique:__ @Example_Unsafe_const_pointers_ret1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_933d622d42a60fdc" const_pointers_ret1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h:215:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_const_pointers_ret2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c51590c060ca121d" const_pointers_ret2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h:216:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_const_pointers_ret3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_6f33756ca0c65fec" const_pointers_ret3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h:217:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_const_pointers_ret4@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_bf1480b7e32fddd5" const_pointers_ret4 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h:218:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_const_pointers_ret5@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_0b0d7702340c358f" const_pointers_ret5 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| Pointer-based API for 'const_array_elem1'

__unique:__ @Example_Unsafe_const_array_elem1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_57cf6165fd843fa0" const_array_elem1_wrapper ::
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

    __unique:__ @Example_Unsafe_const_array_elem2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_d6100076dff26395" const_array_elem2 ::
     Ptr.Ptr (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| Pointer-based API for 'const_array_elem3'

__unique:__ @Example_Unsafe_const_array_elem3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_1f08907d684817a5" const_array_elem3_wrapper ::
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

__unique:__ @Example_Unsafe_noParams1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_6871249df314a1eb" noParams1 ::
     IO A

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h:257:3@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_noParams2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_4c45c6083bc5aec1" noParams2 ::
     IO A

{-| __C declaration:__ @noParams3@

    __defined at:__ @macros\/reparse.h:258:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_noParams3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_906feff84b3ac80e" noParams3 ::
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

    __unique:__ @Example_Unsafe_funptr_ret1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_730ece1626cf42e8" funptr_ret1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (IO ()))

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h:263:8@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_funptr_ret2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_9758f5635e06a923" funptr_ret2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (IO FC.CInt))

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h:264:8@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_funptr_ret3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_e38910e62fbdae04" funptr_ret3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> IO ()))

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h:265:8@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_funptr_ret4@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_872917b0121e2e59" funptr_ret4 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar))

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h:269:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_funptr_ret5@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_158b72a6ad76f269" funptr_ret5 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h:270:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_funptr_ret6@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_d93f25dfebe176cd" funptr_ret6 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h:271:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_funptr_ret7@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_c337ebe392f0b63b" funptr_ret7 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h:272:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_funptr_ret8@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_fd30dddab3efcfbc" funptr_ret8 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h:273:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_funptr_ret9@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_3c50debb733b1543" funptr_ret9 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h:274:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Unsafe_funptr_ret10@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosreparse_81b1cb42017a813e" funptr_ret10 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))
