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
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <program-analysis/reparse.h>"
  , "void hs_bindgen_test_programanalysisreparse_394853579d622671 ("
  , "  A arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  args_char1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_da98fe949f347bb4 ("
  , "  A arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  args_char2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_1b54575fa299f64d ("
  , "  A arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  args_char3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_5eb574c361d453a5 ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  args_short1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_47d5b6ac9938a676 ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  args_short2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_7b3f1c99ea5c31ce ("
  , "  A arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  args_short3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_3103fa698febc2e4 ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  args_int1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_f73bd21e02a58e0f ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  args_int2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_6d36b4892d340141 ("
  , "  A arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  args_int3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_c80fdb5f86f0e67e ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  args_long1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_b0db0696cda23a78 ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  args_long2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_78868ddd9e2ed516 ("
  , "  A arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  args_long3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_555dbd0a04bc0304 ("
  , "  A arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  args_float(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_fe4a06766df0d1e6 ("
  , "  A arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  args_double(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_a607c108df5a1598 ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  args_bool1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_2867c64e14a8b4b4 ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  args_struct(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_dbccce7991402835 ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  args_union(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_58e9df8b58217744 ("
  , "  A arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  args_enum(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_da0ab238a099dc49 ("
  , "  A arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  args_pointer1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_abd9ce8bdda564f4 ("
  , "  A arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  args_pointer2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_ed43d3d8eb25de8f ("
  , "  A arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  args_pointer3(arg1, arg2);"
  , "}"
  , "A hs_bindgen_test_programanalysisreparse_a45e66140bccd9e3 (void)"
  , "{"
  , "  return ret_A();"
  , "}"
  , "char hs_bindgen_test_programanalysisreparse_44f364f98d9773fa ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char1(arg1);"
  , "}"
  , "signed char hs_bindgen_test_programanalysisreparse_c0ccab4edfec7750 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char2(arg1);"
  , "}"
  , "unsigned char hs_bindgen_test_programanalysisreparse_22570fd6296f553c ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char3(arg1);"
  , "}"
  , "signed short hs_bindgen_test_programanalysisreparse_60148c950d753d1d ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short1(arg1);"
  , "}"
  , "signed short hs_bindgen_test_programanalysisreparse_1f5d60c2fc8391f8 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short2(arg1);"
  , "}"
  , "unsigned short hs_bindgen_test_programanalysisreparse_c35d296d9df5f67d ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short3(arg1);"
  , "}"
  , "signed int hs_bindgen_test_programanalysisreparse_edcb4249e75b3e31 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int1(arg1);"
  , "}"
  , "signed int hs_bindgen_test_programanalysisreparse_28e6902c5d5c160d ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int2(arg1);"
  , "}"
  , "unsigned int hs_bindgen_test_programanalysisreparse_42873a9aa50685f7 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int3(arg1);"
  , "}"
  , "signed long hs_bindgen_test_programanalysisreparse_ef0217b739070465 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long1(arg1);"
  , "}"
  , "signed long hs_bindgen_test_programanalysisreparse_0b8baea451432efe ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long2(arg1);"
  , "}"
  , "unsigned long hs_bindgen_test_programanalysisreparse_264e25c048487a65 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long3(arg1);"
  , "}"
  , "float hs_bindgen_test_programanalysisreparse_d4f4783fa3bcf0fd ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_float(arg1);"
  , "}"
  , "double hs_bindgen_test_programanalysisreparse_94ae038975c7b6e8 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_double(arg1);"
  , "}"
  , "_Bool hs_bindgen_test_programanalysisreparse_c6e57fb4c8ccc002 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_bool1(arg1);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_4d9a2038e7abf410 ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_struct(arg1);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_6614cf4950ce1e7c ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_union(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_test_programanalysisreparse_4a8e0e395958b0ed ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_enum(arg1);"
  , "}"
  , "signed int *hs_bindgen_test_programanalysisreparse_59ba78873de08998 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer1(arg1);"
  , "}"
  , "signed int **hs_bindgen_test_programanalysisreparse_f075faf1943231c1 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer2(arg1);"
  , "}"
  , "void *hs_bindgen_test_programanalysisreparse_a865c16cbfd0f2b1 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer3(arg1);"
  , "}"
  , "signed int hs_bindgen_test_programanalysisreparse_0dd10e2baacf20e1 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return body1(arg1);"
  , "}"
  , "A hs_bindgen_test_programanalysisreparse_e40f2da3eda8e4ab (void)"
  , "{"
  , "  return body2();"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_cabceb6db44b7d81 ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  args_complex_float(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_ef2a09e9cd3eec0c ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  args_complex_double(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_26ada386c6fc7617 ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_complex_float(arg1);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_b99f35785e9f9b5c ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_complex_double(arg1);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_4c1c48b67908e0ad ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  bespoke_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_7bd6c9115d303872 ("
  , "  A arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  bespoke_args2(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_test_programanalysisreparse_994d22fc993523bf ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return bespoke_ret1(arg1);"
  , "}"
  , "size_t hs_bindgen_test_programanalysisreparse_c7649a4aa2e14a89 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return bespoke_ret2(arg1);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_69045f97d21cfcd3 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  arr_args1(arg1);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_75b7d6fa15700a72 ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  arr_args2(arg1);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_64bcbef92728339f ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  arr_args3(arg1);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_1635b68f717cc6df ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  arr_args4(arg1);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_09ca38f534ba1397 ("
  , "  A arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  funptr_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_43e32eb1c4511130 ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  funptr_args2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_639eb292178302da ("
  , "  A arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  funptr_args3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_deaef357745591d1 ("
  , "  A arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  funptr_args4(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_bd58865f6f33ce14 ("
  , "  A arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  funptr_args5(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_a609cc59a266965e ("
  , "  A arg1"
  , ")"
  , "{"
  , "  comments1(arg1);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_55af49e081d3af5c ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  const_prim_before1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_17a6476d46f98f53 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  const_prim_before2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_1fce4989a8ceca6d ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  const_prim_before3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_0b8f21af35a88318 ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  const_prim_after1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_069042df961b78f1 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  const_prim_after2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_14ab286beb6d7436 ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  const_prim_after3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_3f5a39bd9a93581a ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_5a190462ddfe8168 ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_70a40d82bb46c21c ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_81ce15a9b9be53f8 ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before4(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_39c58819fdca0585 ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before5(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_4c5a32cae31a651c ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before6(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_0341b56dd9dde729 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before7(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_d43c37be4d91bd6c ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before8(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_f456a2b015543748 ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_729c897c9a9bfc92 ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_5e46eebba0299b49 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_18b842a824ecef09 ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after4(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_75feb5260081201b ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after5(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_3375ca58017bcbeb ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after6(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_5da184668d3e18ba ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after7(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_3cae7641509d64fa ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after8(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_4dcc1a25c1ecaaa2 ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  const_pointers_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_fec0ae82f5b0ad81 ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  const_pointers_args2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_fe0313a1b7b08d51 ("
  , "  A arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_33f3fc94401a8bfe ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args4(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_391d9682b9dc51ac ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args5(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_test_programanalysisreparse_8bf23a9981153f56 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret1(arg1);"
  , "}"
  , "signed int const *hs_bindgen_test_programanalysisreparse_849c6f0a166afc3c ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret2(arg1);"
  , "}"
  , "signed int *const hs_bindgen_test_programanalysisreparse_2bcb395289e776af ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret3(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_test_programanalysisreparse_10d30d6be4435bb5 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret4(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_test_programanalysisreparse_4e0a9385778eeea9 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret5(arg1);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_216b4842313741d6 ("
  , "  A const *arg1"
  , ")"
  , "{"
  , "  const_array_elem1(arg1);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_1c0be30090d3f0b4 ("
  , "  A const **arg1"
  , ")"
  , "{"
  , "  const_array_elem2(arg1);"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_30c17e3a462eeecc ("
  , "  A *const *arg1"
  , ")"
  , "{"
  , "  const_array_elem3(arg1);"
  , "}"
  , "A hs_bindgen_test_programanalysisreparse_8ab2f7d7d9185985 (void)"
  , "{"
  , "  return noParams1();"
  , "}"
  , "A hs_bindgen_test_programanalysisreparse_3154e7cc23e3e0f3 (void)"
  , "{"
  , "  return noParams2();"
  , "}"
  , "void hs_bindgen_test_programanalysisreparse_3d23de5d2d770dfe ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  noParams3(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_test_programanalysisreparse_cfe9601f75800453 ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return funptr_ret1(arg1);"
  , "}"
  , "signed int (*hs_bindgen_test_programanalysisreparse_ca3824f5cf114f19 ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return funptr_ret2(arg1);"
  , "}"
  , "void (*hs_bindgen_test_programanalysisreparse_2de886ad95f674f5 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return funptr_ret3(arg1);"
  , "}"
  , "char (*hs_bindgen_test_programanalysisreparse_b4856eab77ec0cbf ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret4(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_test_programanalysisreparse_39273a56f1a80904 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret5(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_test_programanalysisreparse_5e2dfa9b8f6075ee ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret6(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_test_programanalysisreparse_0b65a87e08b5a1a7 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret7(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_test_programanalysisreparse_629050352113405f ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret8(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_test_programanalysisreparse_a1dcda8f782ad284 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret9(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_test_programanalysisreparse_ca08b018fda612eb ("
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

__defined at:__ @program-analysis\/reparse.h:17:6@

__exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_394853579d622671" args_char1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_char2@

    __defined at:__ @program-analysis\/reparse.h:18:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_da98fe949f347bb4" args_char2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CSChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_char3@

    __defined at:__ @program-analysis\/reparse.h:19:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_1b54575fa299f64d" args_char3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_short1@

    __defined at:__ @program-analysis\/reparse.h:21:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_5eb574c361d453a5" args_short1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CShort
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_short2@

    __defined at:__ @program-analysis\/reparse.h:22:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_47d5b6ac9938a676" args_short2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CShort
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_short3@

    __defined at:__ @program-analysis\/reparse.h:23:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_7b3f1c99ea5c31ce" args_short3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUShort
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_int1@

    __defined at:__ @program-analysis\/reparse.h:25:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_3103fa698febc2e4" args_int1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_int2@

    __defined at:__ @program-analysis\/reparse.h:26:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_f73bd21e02a58e0f" args_int2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_int3@

    __defined at:__ @program-analysis\/reparse.h:27:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_6d36b4892d340141" args_int3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_long1@

    __defined at:__ @program-analysis\/reparse.h:29:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_c80fdb5f86f0e67e" args_long1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CLong
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_long2@

    __defined at:__ @program-analysis\/reparse.h:30:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_b0db0696cda23a78" args_long2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CLong
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_long3@

    __defined at:__ @program-analysis\/reparse.h:31:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_78868ddd9e2ed516" args_long3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CULong
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_float@

    __defined at:__ @program-analysis\/reparse.h:33:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_555dbd0a04bc0304" args_float ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CFloat
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_double@

    __defined at:__ @program-analysis\/reparse.h:34:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_fe4a06766df0d1e6" args_double ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_bool1@

    __defined at:__ @program-analysis\/reparse.h:35:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_a607c108df5a1598" args_bool1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| Pointer-based API for 'args_struct'

-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_2867c64e14a8b4b4" args_struct_wrapper ::
     A
  -> Ptr.Ptr Some_struct
  -> IO ()

{-| __C declaration:__ @args_struct@

    __defined at:__ @program-analysis\/reparse.h:37:6@

    __exported by:__ @program-analysis\/reparse.h@
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

-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_dbccce7991402835" args_union_wrapper ::
     A
  -> Ptr.Ptr Some_union
  -> IO ()

{-| __C declaration:__ @args_union@

    __defined at:__ @program-analysis\/reparse.h:38:6@

    __exported by:__ @program-analysis\/reparse.h@
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

    __defined at:__ @program-analysis\/reparse.h:39:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_58e9df8b58217744" args_enum ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_enum
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_pointer1@

    __defined at:__ @program-analysis\/reparse.h:41:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_da0ab238a099dc49" args_pointer1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_pointer2@

    __defined at:__ @program-analysis\/reparse.h:42:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_abd9ce8bdda564f4" args_pointer2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr (Ptr.Ptr FC.CInt)
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_pointer3@

    __defined at:__ @program-analysis\/reparse.h:43:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_ed43d3d8eb25de8f" args_pointer3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr Void
     {- ^ __C declaration:__ @arg3@
     -}
  -> IO ()

{-| __C declaration:__ @ret_A@

    __defined at:__ @program-analysis\/reparse.h:47:3@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_a45e66140bccd9e3" ret_A ::
     IO A

{-| __C declaration:__ @ret_char1@

    __defined at:__ @program-analysis\/reparse.h:49:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_44f364f98d9773fa" ret_char1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CChar

{-| __C declaration:__ @ret_char2@

    __defined at:__ @program-analysis\/reparse.h:50:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_c0ccab4edfec7750" ret_char2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CSChar

{-| __C declaration:__ @ret_char3@

    __defined at:__ @program-analysis\/reparse.h:51:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_22570fd6296f553c" ret_char3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CUChar

{-| __C declaration:__ @ret_short1@

    __defined at:__ @program-analysis\/reparse.h:53:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_60148c950d753d1d" ret_short1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CShort

{-| __C declaration:__ @ret_short2@

    __defined at:__ @program-analysis\/reparse.h:54:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_1f5d60c2fc8391f8" ret_short2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CShort

{-| __C declaration:__ @ret_short3@

    __defined at:__ @program-analysis\/reparse.h:55:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_c35d296d9df5f67d" ret_short3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CUShort

{-| __C declaration:__ @ret_int1@

    __defined at:__ @program-analysis\/reparse.h:57:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_edcb4249e75b3e31" ret_int1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @ret_int2@

    __defined at:__ @program-analysis\/reparse.h:58:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_28e6902c5d5c160d" ret_int2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @ret_int3@

    __defined at:__ @program-analysis\/reparse.h:59:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_42873a9aa50685f7" ret_int3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CUInt

{-| __C declaration:__ @ret_long1@

    __defined at:__ @program-analysis\/reparse.h:61:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_ef0217b739070465" ret_long1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CLong

{-| __C declaration:__ @ret_long2@

    __defined at:__ @program-analysis\/reparse.h:62:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_0b8baea451432efe" ret_long2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CLong

{-| __C declaration:__ @ret_long3@

    __defined at:__ @program-analysis\/reparse.h:63:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_264e25c048487a65" ret_long3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CULong

{-| __C declaration:__ @ret_float@

    __defined at:__ @program-analysis\/reparse.h:65:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_d4f4783fa3bcf0fd" ret_float ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CFloat

{-| __C declaration:__ @ret_double@

    __defined at:__ @program-analysis\/reparse.h:66:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_94ae038975c7b6e8" ret_double ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CDouble

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @program-analysis\/reparse.h:67:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_c6e57fb4c8ccc002" ret_bool1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CBool

{-| Pointer-based API for 'ret_struct'

-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_4d9a2038e7abf410" ret_struct_wrapper ::
     A
  -> Ptr.Ptr Some_struct
  -> IO ()

{-| __C declaration:__ @ret_struct@

    __defined at:__ @program-analysis\/reparse.h:69:20@

    __exported by:__ @program-analysis\/reparse.h@
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

-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_6614cf4950ce1e7c" ret_union_wrapper ::
     A
  -> Ptr.Ptr Some_union
  -> IO ()

{-| __C declaration:__ @ret_union@

    __defined at:__ @program-analysis\/reparse.h:70:20@

    __exported by:__ @program-analysis\/reparse.h@
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

    __defined at:__ @program-analysis\/reparse.h:71:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_4a8e0e395958b0ed" ret_enum ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO Some_enum

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @program-analysis\/reparse.h:73:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_59ba78873de08998" ret_pointer1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @program-analysis\/reparse.h:74:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_f075faf1943231c1" ret_pointer2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @program-analysis\/reparse.h:75:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_a865c16cbfd0f2b1" ret_pointer3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @body1@

    __defined at:__ @program-analysis\/reparse.h:79:5@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_0dd10e2baacf20e1" body1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @body2@

    __defined at:__ @program-analysis\/reparse.h:80:3@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_e40f2da3eda8e4ab" body2 ::
     IO A

{-| Pointer-based API for 'args_complex_float'

-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_cabceb6db44b7d81" args_complex_float_wrapper ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> IO ()

{-| __C declaration:__ @args_complex_float@

    __defined at:__ @program-analysis\/reparse.h:84:6@

    __exported by:__ @program-analysis\/reparse.h@
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

-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_ef2a09e9cd3eec0c" args_complex_double_wrapper ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> IO ()

{-| __C declaration:__ @args_complex_double@

    __defined at:__ @program-analysis\/reparse.h:85:6@

    __exported by:__ @program-analysis\/reparse.h@
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

-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_26ada386c6fc7617" ret_complex_float_wrapper ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> IO ()

{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @program-analysis\/reparse.h:86:17@

    __exported by:__ @program-analysis\/reparse.h@
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

-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_b99f35785e9f9b5c" ret_complex_double_wrapper ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> IO ()

{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @program-analysis\/reparse.h:87:17@

    __exported by:__ @program-analysis\/reparse.h@
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

    __defined at:__ @program-analysis\/reparse.h:94:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_4c1c48b67908e0ad" bespoke_args1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @program-analysis\/reparse.h:95:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_7bd6c9115d303872" bespoke_args2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @program-analysis\/reparse.h:97:8@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_994d22fc993523bf" bespoke_ret1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CBool

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @program-analysis\/reparse.h:98:8@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_c7649a4aa2e14a89" bespoke_ret2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO HsBindgen.Runtime.Prelude.CSize

{-| Arrays

__C declaration:__ @arr_args1@

__defined at:__ @program-analysis\/reparse.h:104:6@

__exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_69045f97d21cfcd3" arr_args1 ::
     Ptr.Ptr A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| __C declaration:__ @arr_args2@

    __defined at:__ @program-analysis\/reparse.h:105:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_75b7d6fa15700a72" arr_args2 ::
     Ptr.Ptr (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| __C declaration:__ @arr_args3@

    __defined at:__ @program-analysis\/reparse.h:106:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_64bcbef92728339f" arr_args3 ::
     Ptr.Ptr A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| __C declaration:__ @arr_args4@

    __defined at:__ @program-analysis\/reparse.h:107:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_1635b68f717cc6df" arr_args4 ::
     Ptr.Ptr (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| Function pointers

__C declaration:__ @funptr_args1@

__defined at:__ @program-analysis\/reparse.h:126:6@

__exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_09ca38f534ba1397" funptr_args1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (IO ())
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @funptr_args2@

    __defined at:__ @program-analysis\/reparse.h:127:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_43e32eb1c4511130" funptr_args2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (IO FC.CInt)
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @funptr_args3@

    __defined at:__ @program-analysis\/reparse.h:128:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_639eb292178302da" funptr_args3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (FC.CInt -> IO ())
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @funptr_args4@

    __defined at:__ @program-analysis\/reparse.h:129:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_deaef357745591d1" funptr_args4 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @funptr_args5@

    __defined at:__ @program-analysis\/reparse.h:130:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_bd58865f6f33ce14" funptr_args5 ::
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

__defined at:__ @program-analysis\/reparse.h:144:25@

__exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_a609cc59a266965e" comments1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| `const` qualifier

  NOTE: These were not parsed correctly prior to the switch to language-c.

__C declaration:__ @const_prim_before1@

__defined at:__ @program-analysis\/reparse.h:179:6@

__exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_55af49e081d3af5c" const_prim_before1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @program-analysis\/reparse.h:180:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_17a6476d46f98f53" const_prim_before2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CSChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @program-analysis\/reparse.h:181:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_1fce4989a8ceca6d" const_prim_before3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @program-analysis\/reparse.h:182:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_0b8f21af35a88318" const_prim_after1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @program-analysis\/reparse.h:183:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_069042df961b78f1" const_prim_after2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CSChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @program-analysis\/reparse.h:184:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_14ab286beb6d7436" const_prim_after3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @program-analysis\/reparse.h:188:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_3f5a39bd9a93581a" const_withoutSign_before1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CFloat
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @program-analysis\/reparse.h:189:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_5a190462ddfe8168" const_withoutSign_before2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @program-analysis\/reparse.h:190:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_70a40d82bb46c21c" const_withoutSign_before3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| Pointer-based API for 'const_withoutSign_before4'

-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_81ce15a9b9be53f8" const_withoutSign_before4_wrapper ::
     A
  -> Ptr.Ptr Some_struct
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @program-analysis\/reparse.h:191:6@

    __exported by:__ @program-analysis\/reparse.h@
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

-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_39c58819fdca0585" const_withoutSign_before5_wrapper ::
     A
  -> Ptr.Ptr Some_union
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @program-analysis\/reparse.h:192:6@

    __exported by:__ @program-analysis\/reparse.h@
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

    __defined at:__ @program-analysis\/reparse.h:193:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_4c5a32cae31a651c" const_withoutSign_before6 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_enum
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @program-analysis\/reparse.h:194:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_0341b56dd9dde729" const_withoutSign_before7 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @program-analysis\/reparse.h:195:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_d43c37be4d91bd6c" const_withoutSign_before8 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @program-analysis\/reparse.h:197:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_f456a2b015543748" const_withoutSign_after1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CFloat
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @program-analysis\/reparse.h:198:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_729c897c9a9bfc92" const_withoutSign_after2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @program-analysis\/reparse.h:199:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_5e46eebba0299b49" const_withoutSign_after3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| Pointer-based API for 'const_withoutSign_after4'

-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_18b842a824ecef09" const_withoutSign_after4_wrapper ::
     A
  -> Ptr.Ptr Some_struct
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @program-analysis\/reparse.h:200:6@

    __exported by:__ @program-analysis\/reparse.h@
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

-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_75feb5260081201b" const_withoutSign_after5_wrapper ::
     A
  -> Ptr.Ptr Some_union
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @program-analysis\/reparse.h:201:6@

    __exported by:__ @program-analysis\/reparse.h@
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

    __defined at:__ @program-analysis\/reparse.h:202:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_3375ca58017bcbeb" const_withoutSign_after6 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_enum
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @program-analysis\/reparse.h:203:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_5da184668d3e18ba" const_withoutSign_after7 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @program-analysis\/reparse.h:204:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_3cae7641509d64fa" const_withoutSign_after8 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @program-analysis\/reparse.h:208:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_4dcc1a25c1ecaaa2" const_pointers_args1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @program-analysis\/reparse.h:209:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_fec0ae82f5b0ad81" const_pointers_args2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @program-analysis\/reparse.h:210:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_fe0313a1b7b08d51" const_pointers_args3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @program-analysis\/reparse.h:211:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_33f3fc94401a8bfe" const_pointers_args4 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @program-analysis\/reparse.h:212:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_391d9682b9dc51ac" const_pointers_args5 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @program-analysis\/reparse.h:214:19@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_8bf23a9981153f56" const_pointers_ret1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @program-analysis\/reparse.h:215:19@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_849c6f0a166afc3c" const_pointers_ret2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @program-analysis\/reparse.h:216:19@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_2bcb395289e776af" const_pointers_ret3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @program-analysis\/reparse.h:217:19@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_10d30d6be4435bb5" const_pointers_ret4 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @program-analysis\/reparse.h:218:19@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_4e0a9385778eeea9" const_pointers_ret5 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| Pointer-based API for 'const_array_elem1'

-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_216b4842313741d6" const_array_elem1_wrapper ::
     Ptr.Ptr A
  -> IO ()

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @program-analysis\/reparse.h:246:6@

    __exported by:__ @program-analysis\/reparse.h@
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

    __defined at:__ @program-analysis\/reparse.h:247:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_1c0be30090d3f0b4" const_array_elem2 ::
     Ptr.Ptr (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| Pointer-based API for 'const_array_elem3'

-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_30c17e3a462eeecc" const_array_elem3_wrapper ::
     Ptr.Ptr (Ptr.Ptr A)
  -> IO ()

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @program-analysis\/reparse.h:248:6@

    __exported by:__ @program-analysis\/reparse.h@
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

__defined at:__ @program-analysis\/reparse.h:256:3@

__exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_8ab2f7d7d9185985" noParams1 ::
     IO A

{-| __C declaration:__ @noParams2@

    __defined at:__ @program-analysis\/reparse.h:257:3@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_3154e7cc23e3e0f3" noParams2 ::
     IO A

{-| __C declaration:__ @noParams3@

    __defined at:__ @program-analysis\/reparse.h:258:6@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_3d23de5d2d770dfe" noParams3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (IO FC.CInt)
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @program-analysis\/reparse.h:262:8@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_cfe9601f75800453" funptr_ret1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (IO ()))

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @program-analysis\/reparse.h:263:8@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_ca3824f5cf114f19" funptr_ret2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (IO FC.CInt))

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @program-analysis\/reparse.h:264:8@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_2de886ad95f674f5" funptr_ret3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> IO ()))

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @program-analysis\/reparse.h:265:8@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_b4856eab77ec0cbf" funptr_ret4 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar))

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @program-analysis\/reparse.h:269:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_39273a56f1a80904" funptr_ret5 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @program-analysis\/reparse.h:270:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_5e2dfa9b8f6075ee" funptr_ret6 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @program-analysis\/reparse.h:271:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_0b65a87e08b5a1a7" funptr_ret7 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @program-analysis\/reparse.h:272:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_629050352113405f" funptr_ret8 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @program-analysis\/reparse.h:273:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_a1dcda8f782ad284" funptr_ret9 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @program-analysis\/reparse.h:274:20@

    __exported by:__ @program-analysis\/reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_programanalysisreparse_ca08b018fda612eb" funptr_ret10 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))
