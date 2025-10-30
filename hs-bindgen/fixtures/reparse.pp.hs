{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK prune #-}

module Example where

import qualified Data.Array.Byte
import qualified Data.Bits as Bits
import qualified Data.Complex
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import qualified HsBindgen.Runtime.SizedByteArray
import qualified Text.Read
import Data.Bits (FiniteBits)
import Data.Void (Void)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, return, showsPrec)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <reparse.h>"
  , "void hs_bindgen_test_reparse_394853579d622671 ("
  , "  A arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  args_char1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_da98fe949f347bb4 ("
  , "  A arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  args_char2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_1b54575fa299f64d ("
  , "  A arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  args_char3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_5eb574c361d453a5 ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  args_short1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_47d5b6ac9938a676 ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  args_short2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_7b3f1c99ea5c31ce ("
  , "  A arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  args_short3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_3103fa698febc2e4 ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  args_int1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_f73bd21e02a58e0f ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  args_int2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_6d36b4892d340141 ("
  , "  A arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  args_int3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_c80fdb5f86f0e67e ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  args_long1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_b0db0696cda23a78 ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  args_long2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_78868ddd9e2ed516 ("
  , "  A arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  args_long3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_555dbd0a04bc0304 ("
  , "  A arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  args_float(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_fe4a06766df0d1e6 ("
  , "  A arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  args_double(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_a607c108df5a1598 ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  args_bool1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_2867c64e14a8b4b4 ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  args_struct(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_dbccce7991402835 ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  args_union(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_58e9df8b58217744 ("
  , "  A arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  args_enum(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_da0ab238a099dc49 ("
  , "  A arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  args_pointer1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_abd9ce8bdda564f4 ("
  , "  A arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  args_pointer2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_ed43d3d8eb25de8f ("
  , "  A arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  args_pointer3(arg1, arg2);"
  , "}"
  , "A hs_bindgen_test_reparse_a45e66140bccd9e3 (void)"
  , "{"
  , "  return ret_A();"
  , "}"
  , "char hs_bindgen_test_reparse_44f364f98d9773fa ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char1(arg1);"
  , "}"
  , "signed char hs_bindgen_test_reparse_c0ccab4edfec7750 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char2(arg1);"
  , "}"
  , "unsigned char hs_bindgen_test_reparse_22570fd6296f553c ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char3(arg1);"
  , "}"
  , "signed short hs_bindgen_test_reparse_60148c950d753d1d ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short1(arg1);"
  , "}"
  , "signed short hs_bindgen_test_reparse_1f5d60c2fc8391f8 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short2(arg1);"
  , "}"
  , "unsigned short hs_bindgen_test_reparse_c35d296d9df5f67d ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short3(arg1);"
  , "}"
  , "signed int hs_bindgen_test_reparse_edcb4249e75b3e31 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int1(arg1);"
  , "}"
  , "signed int hs_bindgen_test_reparse_28e6902c5d5c160d ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int2(arg1);"
  , "}"
  , "unsigned int hs_bindgen_test_reparse_42873a9aa50685f7 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int3(arg1);"
  , "}"
  , "signed long hs_bindgen_test_reparse_ef0217b739070465 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long1(arg1);"
  , "}"
  , "signed long hs_bindgen_test_reparse_0b8baea451432efe ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long2(arg1);"
  , "}"
  , "unsigned long hs_bindgen_test_reparse_264e25c048487a65 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long3(arg1);"
  , "}"
  , "float hs_bindgen_test_reparse_d4f4783fa3bcf0fd ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_float(arg1);"
  , "}"
  , "double hs_bindgen_test_reparse_94ae038975c7b6e8 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_double(arg1);"
  , "}"
  , "_Bool hs_bindgen_test_reparse_c6e57fb4c8ccc002 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_bool1(arg1);"
  , "}"
  , "void hs_bindgen_test_reparse_4d9a2038e7abf410 ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_struct(arg1);"
  , "}"
  , "void hs_bindgen_test_reparse_6614cf4950ce1e7c ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_union(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_test_reparse_4a8e0e395958b0ed ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_enum(arg1);"
  , "}"
  , "signed int *hs_bindgen_test_reparse_59ba78873de08998 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer1(arg1);"
  , "}"
  , "signed int **hs_bindgen_test_reparse_f075faf1943231c1 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer2(arg1);"
  , "}"
  , "void *hs_bindgen_test_reparse_a865c16cbfd0f2b1 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer3(arg1);"
  , "}"
  , "signed int hs_bindgen_test_reparse_0dd10e2baacf20e1 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return body1(arg1);"
  , "}"
  , "A hs_bindgen_test_reparse_e40f2da3eda8e4ab (void)"
  , "{"
  , "  return body2();"
  , "}"
  , "void hs_bindgen_test_reparse_cabceb6db44b7d81 ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  args_complex_float(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_ef2a09e9cd3eec0c ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  args_complex_double(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_26ada386c6fc7617 ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_complex_float(arg1);"
  , "}"
  , "void hs_bindgen_test_reparse_b99f35785e9f9b5c ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_complex_double(arg1);"
  , "}"
  , "void hs_bindgen_test_reparse_4c1c48b67908e0ad ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  bespoke_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_7bd6c9115d303872 ("
  , "  A arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  bespoke_args2(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_test_reparse_994d22fc993523bf ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return bespoke_ret1(arg1);"
  , "}"
  , "size_t hs_bindgen_test_reparse_c7649a4aa2e14a89 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return bespoke_ret2(arg1);"
  , "}"
  , "void hs_bindgen_test_reparse_69045f97d21cfcd3 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  arr_args1(arg1);"
  , "}"
  , "void hs_bindgen_test_reparse_75b7d6fa15700a72 ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  arr_args2(arg1);"
  , "}"
  , "void hs_bindgen_test_reparse_64bcbef92728339f ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  arr_args3(arg1);"
  , "}"
  , "void hs_bindgen_test_reparse_1635b68f717cc6df ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  arr_args4(arg1);"
  , "}"
  , "void hs_bindgen_test_reparse_09ca38f534ba1397 ("
  , "  A arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  funptr_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_43e32eb1c4511130 ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  funptr_args2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_639eb292178302da ("
  , "  A arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  funptr_args3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_deaef357745591d1 ("
  , "  A arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  funptr_args4(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_bd58865f6f33ce14 ("
  , "  A arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  funptr_args5(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_a609cc59a266965e ("
  , "  A arg1"
  , ")"
  , "{"
  , "  comments1(arg1);"
  , "}"
  , "void hs_bindgen_test_reparse_55af49e081d3af5c ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  const_prim_before1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_17a6476d46f98f53 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  const_prim_before2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_1fce4989a8ceca6d ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  const_prim_before3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_0b8f21af35a88318 ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  const_prim_after1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_069042df961b78f1 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  const_prim_after2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_14ab286beb6d7436 ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  const_prim_after3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_3f5a39bd9a93581a ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_5a190462ddfe8168 ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_70a40d82bb46c21c ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_81ce15a9b9be53f8 ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before4(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_39c58819fdca0585 ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before5(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_4c5a32cae31a651c ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before6(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_0341b56dd9dde729 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before7(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_d43c37be4d91bd6c ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before8(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_f456a2b015543748 ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_729c897c9a9bfc92 ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_5e46eebba0299b49 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_18b842a824ecef09 ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after4(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_75feb5260081201b ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after5(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_3375ca58017bcbeb ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after6(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_5da184668d3e18ba ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after7(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_3cae7641509d64fa ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after8(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_4dcc1a25c1ecaaa2 ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  const_pointers_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_fec0ae82f5b0ad81 ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  const_pointers_args2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_fe0313a1b7b08d51 ("
  , "  A arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_33f3fc94401a8bfe ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args4(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_reparse_391d9682b9dc51ac ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args5(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_test_reparse_8bf23a9981153f56 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret1(arg1);"
  , "}"
  , "signed int const *hs_bindgen_test_reparse_849c6f0a166afc3c ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret2(arg1);"
  , "}"
  , "signed int *const hs_bindgen_test_reparse_2bcb395289e776af ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret3(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_test_reparse_10d30d6be4435bb5 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret4(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_test_reparse_4e0a9385778eeea9 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret5(arg1);"
  , "}"
  , "void hs_bindgen_test_reparse_216b4842313741d6 ("
  , "  A const *arg1"
  , ")"
  , "{"
  , "  const_array_elem1(arg1);"
  , "}"
  , "void hs_bindgen_test_reparse_1c0be30090d3f0b4 ("
  , "  A const **arg1"
  , ")"
  , "{"
  , "  const_array_elem2(arg1);"
  , "}"
  , "void hs_bindgen_test_reparse_30c17e3a462eeecc ("
  , "  A *const *arg1"
  , ")"
  , "{"
  , "  const_array_elem3(arg1);"
  , "}"
  , "A hs_bindgen_test_reparse_8ab2f7d7d9185985 (void)"
  , "{"
  , "  return noParams1();"
  , "}"
  , "A hs_bindgen_test_reparse_3154e7cc23e3e0f3 (void)"
  , "{"
  , "  return noParams2();"
  , "}"
  , "void hs_bindgen_test_reparse_3d23de5d2d770dfe ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  noParams3(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_test_reparse_cfe9601f75800453 ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return funptr_ret1(arg1);"
  , "}"
  , "signed int (*hs_bindgen_test_reparse_ca3824f5cf114f19 ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return funptr_ret2(arg1);"
  , "}"
  , "void (*hs_bindgen_test_reparse_2de886ad95f674f5 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return funptr_ret3(arg1);"
  , "}"
  , "char (*hs_bindgen_test_reparse_b4856eab77ec0cbf ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret4(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_test_reparse_39273a56f1a80904 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret5(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_test_reparse_5e2dfa9b8f6075ee ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret6(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_test_reparse_0b65a87e08b5a1a7 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret7(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_test_reparse_629050352113405f ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret8(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_test_reparse_a1dcda8f782ad284 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret9(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_test_reparse_ca08b018fda612eb ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret10(arg1);"
  , "}"
  , "/* get_args_char1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_1cbcf8b84924816c (void)) ("
  , "  A arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &args_char1;"
  , "}"
  , "/* get_args_char2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_ec2d78b82f444fd0 (void)) ("
  , "  A arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  return &args_char2;"
  , "}"
  , "/* get_args_char3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_1baa18e723594389 (void)) ("
  , "  A arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  return &args_char3;"
  , "}"
  , "/* get_args_short1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_c96cef4ef5f5e180 (void)) ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short1;"
  , "}"
  , "/* get_args_short2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_3a683552d4f772c7 (void)) ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  return &args_short2;"
  , "}"
  , "/* get_args_short3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_f3284022ac706255 (void)) ("
  , "  A arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  return &args_short3;"
  , "}"
  , "/* get_args_int1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_5c4d785286ccca6b (void)) ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int1;"
  , "}"
  , "/* get_args_int2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_e323b837afe40be7 (void)) ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &args_int2;"
  , "}"
  , "/* get_args_int3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_eb0e5feb8eb4082d (void)) ("
  , "  A arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  return &args_int3;"
  , "}"
  , "/* get_args_long1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_d7d322f23a65f43b (void)) ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long1;"
  , "}"
  , "/* get_args_long2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_378c16768a6f6f21 (void)) ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  return &args_long2;"
  , "}"
  , "/* get_args_long3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_548dcd4760226ee2 (void)) ("
  , "  A arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  return &args_long3;"
  , "}"
  , "/* get_args_float_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_701d01261043851b (void)) ("
  , "  A arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  return &args_float;"
  , "}"
  , "/* get_args_double_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_ff631e42f704e4cd (void)) ("
  , "  A arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &args_double;"
  , "}"
  , "/* get_args_bool1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_6e289c6cc6d382bf (void)) ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &args_bool1;"
  , "}"
  , "/* get_args_struct_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_26b20c1b89e46b02 (void)) ("
  , "  A arg1,"
  , "  struct some_struct arg2"
  , ")"
  , "{"
  , "  return &args_struct;"
  , "}"
  , "/* get_args_union_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_cfd37f06f21b8755 (void)) ("
  , "  A arg1,"
  , "  union some_union arg2"
  , ")"
  , "{"
  , "  return &args_union;"
  , "}"
  , "/* get_args_enum_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_69882f8f862fffc2 (void)) ("
  , "  A arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  return &args_enum;"
  , "}"
  , "/* get_args_pointer1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_23bde4e97b66c470 (void)) ("
  , "  A arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return &args_pointer1;"
  , "}"
  , "/* get_args_pointer2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_fceb546239df3c0a (void)) ("
  , "  A arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  return &args_pointer2;"
  , "}"
  , "/* get_args_pointer3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_0cb396fb06dd816a (void)) ("
  , "  A arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  return &args_pointer3;"
  , "}"
  , "/* get_ret_A_ptr */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_test_reparse_a7564eacf3ad149f (void)) (void)"
  , "{"
  , "  return &ret_A;"
  , "}"
  , "/* get_ret_char1_ptr */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_test_reparse_7b5b646ee4e06777 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char1;"
  , "}"
  , "/* get_ret_char2_ptr */"
  , "__attribute__ ((const))"
  , "signed char (*hs_bindgen_test_reparse_7c05cbccaf1be8b6 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char2;"
  , "}"
  , "/* get_ret_char3_ptr */"
  , "__attribute__ ((const))"
  , "unsigned char (*hs_bindgen_test_reparse_0fc74f839f906d7e (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_char3;"
  , "}"
  , "/* get_ret_short1_ptr */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_test_reparse_72ff9f5cb5daaae8 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short1;"
  , "}"
  , "/* get_ret_short2_ptr */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_test_reparse_eb5427ff3ea0d96e (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short2;"
  , "}"
  , "/* get_ret_short3_ptr */"
  , "__attribute__ ((const))"
  , "unsigned short (*hs_bindgen_test_reparse_823adc61eed1550c (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_short3;"
  , "}"
  , "/* get_ret_int1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_reparse_79ce8d81113cf766 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int1;"
  , "}"
  , "/* get_ret_int2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_reparse_d369bd4861f00c84 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int2;"
  , "}"
  , "/* get_ret_int3_ptr */"
  , "__attribute__ ((const))"
  , "unsigned int (*hs_bindgen_test_reparse_0336d583fc7b5951 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_int3;"
  , "}"
  , "/* get_ret_long1_ptr */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_test_reparse_36845109a4ce7992 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long1;"
  , "}"
  , "/* get_ret_long2_ptr */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_test_reparse_ac32dbc1e79e704e (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long2;"
  , "}"
  , "/* get_ret_long3_ptr */"
  , "__attribute__ ((const))"
  , "unsigned long (*hs_bindgen_test_reparse_6fba85ecad7d8d4e (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_long3;"
  , "}"
  , "/* get_ret_float_ptr */"
  , "__attribute__ ((const))"
  , "float (*hs_bindgen_test_reparse_e9ac779a7c943add (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_float;"
  , "}"
  , "/* get_ret_double_ptr */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_test_reparse_7095a5f5be3ecc0c (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_double;"
  , "}"
  , "/* get_ret_bool1_ptr */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_test_reparse_c7b5be49f4314899 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_bool1;"
  , "}"
  , "/* get_ret_struct_ptr */"
  , "__attribute__ ((const))"
  , "struct some_struct (*hs_bindgen_test_reparse_03ec23cf81b62ce3 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_struct;"
  , "}"
  , "/* get_ret_union_ptr */"
  , "__attribute__ ((const))"
  , "union some_union (*hs_bindgen_test_reparse_5315544d48ea5b07 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_union;"
  , "}"
  , "/* get_ret_enum_ptr */"
  , "__attribute__ ((const))"
  , "enum some_enum (*hs_bindgen_test_reparse_9fb7ddbcd84c72f1 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_enum;"
  , "}"
  , "/* get_ret_pointer1_ptr */"
  , "__attribute__ ((const))"
  , "signed int *(*hs_bindgen_test_reparse_0638bcad8813a303 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer1;"
  , "}"
  , "/* get_ret_pointer2_ptr */"
  , "__attribute__ ((const))"
  , "signed int **(*hs_bindgen_test_reparse_5d9ced9e4887782b (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer2;"
  , "}"
  , "/* get_ret_pointer3_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_test_reparse_60e99361ec0a4b5b (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_pointer3;"
  , "}"
  , "/* get_body1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_reparse_cca1935605a94051 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &body1;"
  , "}"
  , "/* get_body2_ptr */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_test_reparse_a1900daea7e14e95 (void)) (void)"
  , "{"
  , "  return &body2;"
  , "}"
  , "/* get_args_complex_float_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_c62f1e9d47469a1c (void)) ("
  , "  A arg1,"
  , "  float _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_float;"
  , "}"
  , "/* get_args_complex_double_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_b2ef5ed0a8ed0697 (void)) ("
  , "  A arg1,"
  , "  double _Complex arg2"
  , ")"
  , "{"
  , "  return &args_complex_double;"
  , "}"
  , "/* get_ret_complex_float_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex (*hs_bindgen_test_reparse_e2cc2aa2dd12852d (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_complex_float;"
  , "}"
  , "/* get_ret_complex_double_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex (*hs_bindgen_test_reparse_c95961d571f78868 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &ret_complex_double;"
  , "}"
  , "/* get_bespoke_args1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_94c8a2d3574ba283 (void)) ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &bespoke_args1;"
  , "}"
  , "/* get_bespoke_args2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_2165985767a8d24e (void)) ("
  , "  A arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &bespoke_args2;"
  , "}"
  , "/* get_bespoke_ret1_ptr */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_test_reparse_7913bf38675bd912 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret1;"
  , "}"
  , "/* get_bespoke_ret2_ptr */"
  , "__attribute__ ((const))"
  , "size_t (*hs_bindgen_test_reparse_07c419cb648cdf65 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &bespoke_ret2;"
  , "}"
  , "/* get_arr_args1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_ed19e51bcac06a9e (void)) ("
  , "  A arg1[]"
  , ")"
  , "{"
  , "  return &arr_args1;"
  , "}"
  , "/* get_arr_args2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_de3931a21a8a71fc (void)) ("
  , "  A *arg1[]"
  , ")"
  , "{"
  , "  return &arr_args2;"
  , "}"
  , "/* get_arr_args3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_2c02effa6288a26b (void)) ("
  , "  A arg1[5]"
  , ")"
  , "{"
  , "  return &arr_args3;"
  , "}"
  , "/* get_arr_args4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_2144e300082f115c (void)) ("
  , "  A *arg1[5]"
  , ")"
  , "{"
  , "  return &arr_args4;"
  , "}"
  , "/* get_funptr_args1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_d1645262a53743f6 (void)) ("
  , "  A arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args1;"
  , "}"
  , "/* get_funptr_args2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_d66507630e4e38e3 (void)) ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &funptr_args2;"
  , "}"
  , "/* get_funptr_args3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_3d7907ab53b617cf (void)) ("
  , "  A arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args3;"
  , "}"
  , "/* get_funptr_args4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_e4d15a9c3b04292a (void)) ("
  , "  A arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args4;"
  , "}"
  , "/* get_funptr_args5_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_ced7918b6e42102f (void)) ("
  , "  A arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  return &funptr_args5;"
  , "}"
  , "/* get_comments1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_c90ec05081ef4e64 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &comments1;"
  , "}"
  , "/* get_const_prim_before1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_6ac4b42c66a36448 (void)) ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before1;"
  , "}"
  , "/* get_const_prim_before2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_f98632ef2e69b003 (void)) ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before2;"
  , "}"
  , "/* get_const_prim_before3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_cc9db1f6a36b8221 (void)) ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_before3;"
  , "}"
  , "/* get_const_prim_after1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_3e5b7273bf2ecadb (void)) ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after1;"
  , "}"
  , "/* get_const_prim_after2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_f9b4beeca8253333 (void)) ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after2;"
  , "}"
  , "/* get_const_prim_after3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_bf14e2fd88b25311 (void)) ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  return &const_prim_after3;"
  , "}"
  , "/* get_const_withoutSign_before1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_3649293fcaa1543c (void)) ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before1;"
  , "}"
  , "/* get_const_withoutSign_before2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_ad5903c28e22dd2c (void)) ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before2;"
  , "}"
  , "/* get_const_withoutSign_before3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_e7b9bc011ec1dd8a (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before3;"
  , "}"
  , "/* get_const_withoutSign_before4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_4fd66b696848dd98 (void)) ("
  , "  A arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before4;"
  , "}"
  , "/* get_const_withoutSign_before5_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_42582e1882927f7e (void)) ("
  , "  A arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before5;"
  , "}"
  , "/* get_const_withoutSign_before6_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_b6876e53e4b27a98 (void)) ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before6;"
  , "}"
  , "/* get_const_withoutSign_before7_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_78763cbecd2b0750 (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before7;"
  , "}"
  , "/* get_const_withoutSign_before8_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_4098c4a4ccd31d36 (void)) ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_before8;"
  , "}"
  , "/* get_const_withoutSign_after1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_e9148eb7b8dac901 (void)) ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after1;"
  , "}"
  , "/* get_const_withoutSign_after2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_8663653d89116be9 (void)) ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after2;"
  , "}"
  , "/* get_const_withoutSign_after3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_136dcba145bf241b (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after3;"
  , "}"
  , "/* get_const_withoutSign_after4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_380e01acce794cab (void)) ("
  , "  A arg1,"
  , "  struct some_struct const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after4;"
  , "}"
  , "/* get_const_withoutSign_after5_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_af0d84d0757f6c2c (void)) ("
  , "  A arg1,"
  , "  union some_union const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after5;"
  , "}"
  , "/* get_const_withoutSign_after6_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_df92501d07bf6c5f (void)) ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after6;"
  , "}"
  , "/* get_const_withoutSign_after7_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_b41148ca40ec8eb5 (void)) ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after7;"
  , "}"
  , "/* get_const_withoutSign_after8_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_560c9dfdb530548b (void)) ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  return &const_withoutSign_after8;"
  , "}"
  , "/* get_const_pointers_args1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_a34d16c099748839 (void)) ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args1;"
  , "}"
  , "/* get_const_pointers_args2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_45235edaf5c3b599 (void)) ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args2;"
  , "}"
  , "/* get_const_pointers_args3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_3dbcf1c7202f2878 (void)) ("
  , "  A arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args3;"
  , "}"
  , "/* get_const_pointers_args4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_a6624f6cc0a062af (void)) ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args4;"
  , "}"
  , "/* get_const_pointers_args5_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_c5f3253c57910315 (void)) ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  return &const_pointers_args5;"
  , "}"
  , "/* get_const_pointers_ret1_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_test_reparse_1990ded85ea3850d (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret1;"
  , "}"
  , "/* get_const_pointers_ret2_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *(*hs_bindgen_test_reparse_627cc570c3ca7d19 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret2;"
  , "}"
  , "/* get_const_pointers_ret3_ptr */"
  , "__attribute__ ((const))"
  , "signed int *const (*hs_bindgen_test_reparse_2f449708b5a275b1 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret3;"
  , "}"
  , "/* get_const_pointers_ret4_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_test_reparse_67662618cd011c8a (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret4;"
  , "}"
  , "/* get_const_pointers_ret5_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const (*hs_bindgen_test_reparse_fcafd9f8ac329995 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &const_pointers_ret5;"
  , "}"
  , "/* get_const_array_elem1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_6928906fc9a88dfc (void)) ("
  , "  A const arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem1;"
  , "}"
  , "/* get_const_array_elem2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_625a37e9c030891a (void)) ("
  , "  A const *arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem2;"
  , "}"
  , "/* get_const_array_elem3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_5e23f87114cf51fb (void)) ("
  , "  A *const arg1[]"
  , ")"
  , "{"
  , "  return &const_array_elem3;"
  , "}"
  , "/* get_noParams1_ptr */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_test_reparse_d50620a002265139 (void)) (void)"
  , "{"
  , "  return &noParams1;"
  , "}"
  , "/* get_noParams2_ptr */"
  , "__attribute__ ((const))"
  , "A (*hs_bindgen_test_reparse_03b0e24786b82ad5 (void)) (void)"
  , "{"
  , "  return &noParams2;"
  , "}"
  , "/* get_noParams3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_reparse_36508fd99a0556c5 (void)) ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  return &noParams3;"
  , "}"
  , "/* get_funptr_ret1_ptr */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_test_reparse_6f83a48dd177c25f (void)) ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret1;"
  , "}"
  , "/* get_funptr_ret2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_reparse_f12efafd1525ef7f (void)) ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return &funptr_ret2;"
  , "}"
  , "/* get_funptr_ret3_ptr */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_test_reparse_b00baa5b9708b9e7 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &funptr_ret3;"
  , "}"
  , "/* get_funptr_ret4_ptr */"
  , "__attribute__ ((const))"
  , "char (*(*hs_bindgen_test_reparse_c51872479ceff42e (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret4;"
  , "}"
  , "/* get_funptr_ret5_ptr */"
  , "__attribute__ ((const))"
  , "signed int *(*(*hs_bindgen_test_reparse_3b9b9924b4b4d7ea (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret5;"
  , "}"
  , "/* get_funptr_ret6_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_test_reparse_3df5ab4b0b306845 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret6;"
  , "}"
  , "/* get_funptr_ret7_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *(*(*hs_bindgen_test_reparse_2ac4454d93b6f04a (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret7;"
  , "}"
  , "/* get_funptr_ret8_ptr */"
  , "__attribute__ ((const))"
  , "signed int *const (*(*hs_bindgen_test_reparse_411c5128f18364b3 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret8;"
  , "}"
  , "/* get_funptr_ret9_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_test_reparse_693a8d16e17d0cdc (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret9;"
  , "}"
  , "/* get_funptr_ret10_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const (*(*hs_bindgen_test_reparse_9d2da81bbfe49ab6 (void)) ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &funptr_ret10;"
  , "}"
  ]))

{-| __C declaration:__ @A@

    __defined at:__ @reparse.h:3:9@

    __exported by:__ @reparse.h@
-}
newtype A = A
  { un_A :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @some_struct@

    __defined at:__ @reparse.h:7:8@

    __exported by:__ @reparse.h@
-}
data Some_struct = Some_struct
  {}
  deriving stock (Eq, Show)

instance F.Storable Some_struct where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Some_struct

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Some_struct -> return ()

{-| __C declaration:__ @some_union@

    __defined at:__ @reparse.h:8:7@

    __exported by:__ @reparse.h@
-}
newtype Some_union = Some_union
  { un_Some_union :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 0) 1 instance F.Storable Some_union

{-| __C declaration:__ @some_enum@

    __defined at:__ @reparse.h:9:6@

    __exported by:__ @reparse.h@
-}
newtype Some_enum = Some_enum
  { un_Some_enum :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable Some_enum where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Some_enum
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Some_enum un_Some_enum2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Some_enum2

instance HsBindgen.Runtime.CEnum.CEnum Some_enum where

  type CEnumZ Some_enum = FC.CUInt

  toCEnum = Some_enum

  fromCEnum = un_Some_enum

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [(0, Data.List.NonEmpty.singleton "ENUM_A")]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Some_enum"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Some_enum"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Some_enum where

  minDeclaredValue = ENUM_A

  maxDeclaredValue = ENUM_A

instance Show Some_enum where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Some_enum where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @ENUM_A@

    __defined at:__ @reparse.h:9:18@

    __exported by:__ @reparse.h@
-}
pattern ENUM_A :: Some_enum
pattern ENUM_A = Some_enum 0

{-| __C declaration:__ @arr_typedef1@

    __defined at:__ @reparse.h:109:13@

    __exported by:__ @reparse.h@
-}
newtype Arr_typedef1 = Arr_typedef1
  { un_Arr_typedef1 :: HsBindgen.Runtime.IncompleteArray.IncompleteArray A
  }
  deriving stock (Eq, Show)

{-| __C declaration:__ @arr_typedef2@

    __defined at:__ @reparse.h:110:13@

    __exported by:__ @reparse.h@
-}
newtype Arr_typedef2 = Arr_typedef2
  { un_Arr_typedef2 :: HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)
  }
  deriving stock (Eq, Show)

{-| __C declaration:__ @arr_typedef3@

    __defined at:__ @reparse.h:111:13@

    __exported by:__ @reparse.h@
-}
newtype Arr_typedef3 = Arr_typedef3
  { un_Arr_typedef3 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 5) A
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @arr_typedef4@

    __defined at:__ @reparse.h:112:13@

    __exported by:__ @reparse.h@
-}
newtype Arr_typedef4 = Arr_typedef4
  { un_Arr_typedef4 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 5) (Ptr.Ptr A)
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| Typedefs

__C declaration:__ @typedef1@

__defined at:__ @reparse.h:118:14@

__exported by:__ @reparse.h@
-}
newtype Typedef1 = Typedef1
  { un_Typedef1 :: A
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @typedef2@

    __defined at:__ @reparse.h:119:14@

    __exported by:__ @reparse.h@
-}
newtype Typedef2 = Typedef2
  { un_Typedef2 :: Ptr.Ptr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @typedef3@

    __defined at:__ @reparse.h:120:14@

    __exported by:__ @reparse.h@
-}
newtype Typedef3 = Typedef3
  { un_Typedef3 :: Ptr.Ptr (Ptr.Ptr A)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Funptr_typedef1'

__defined at:__ @reparse.h:132:16@

__exported by:__ @reparse.h@
-}
newtype Funptr_typedef1_Deref = Funptr_typedef1_Deref
  { un_Funptr_typedef1_Deref :: IO A
  }

foreign import ccall safe "wrapper" toFunptr_typedef1_Deref ::
     Funptr_typedef1_Deref
  -> IO (Ptr.FunPtr Funptr_typedef1_Deref)

foreign import ccall safe "dynamic" fromFunptr_typedef1_Deref ::
     Ptr.FunPtr Funptr_typedef1_Deref
  -> Funptr_typedef1_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef1_Deref where

  toFunPtr = toFunptr_typedef1_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef1_Deref where

  fromFunPtr = fromFunptr_typedef1_Deref

{-| __C declaration:__ @funptr_typedef1@

    __defined at:__ @reparse.h:132:16@

    __exported by:__ @reparse.h@
-}
newtype Funptr_typedef1 = Funptr_typedef1
  { un_Funptr_typedef1 :: Ptr.FunPtr Funptr_typedef1_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Funptr_typedef2'

__defined at:__ @reparse.h:133:16@

__exported by:__ @reparse.h@
-}
newtype Funptr_typedef2_Deref = Funptr_typedef2_Deref
  { un_Funptr_typedef2_Deref :: IO (Ptr.Ptr A)
  }

foreign import ccall safe "wrapper" toFunptr_typedef2_Deref ::
     Funptr_typedef2_Deref
  -> IO (Ptr.FunPtr Funptr_typedef2_Deref)

foreign import ccall safe "dynamic" fromFunptr_typedef2_Deref ::
     Ptr.FunPtr Funptr_typedef2_Deref
  -> Funptr_typedef2_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef2_Deref where

  toFunPtr = toFunptr_typedef2_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef2_Deref where

  fromFunPtr = fromFunptr_typedef2_Deref

{-| __C declaration:__ @funptr_typedef2@

    __defined at:__ @reparse.h:133:16@

    __exported by:__ @reparse.h@
-}
newtype Funptr_typedef2 = Funptr_typedef2
  { un_Funptr_typedef2 :: Ptr.FunPtr Funptr_typedef2_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Funptr_typedef3'

__defined at:__ @reparse.h:134:16@

__exported by:__ @reparse.h@
-}
newtype Funptr_typedef3_Deref = Funptr_typedef3_Deref
  { un_Funptr_typedef3_Deref :: IO (Ptr.Ptr (Ptr.Ptr A))
  }

foreign import ccall safe "wrapper" toFunptr_typedef3_Deref ::
     Funptr_typedef3_Deref
  -> IO (Ptr.FunPtr Funptr_typedef3_Deref)

foreign import ccall safe "dynamic" fromFunptr_typedef3_Deref ::
     Ptr.FunPtr Funptr_typedef3_Deref
  -> Funptr_typedef3_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef3_Deref where

  toFunPtr = toFunptr_typedef3_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef3_Deref where

  fromFunPtr = fromFunptr_typedef3_Deref

{-| __C declaration:__ @funptr_typedef3@

    __defined at:__ @reparse.h:134:16@

    __exported by:__ @reparse.h@
-}
newtype Funptr_typedef3 = Funptr_typedef3
  { un_Funptr_typedef3 :: Ptr.FunPtr Funptr_typedef3_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Funptr_typedef4'

__defined at:__ @reparse.h:135:16@

__exported by:__ @reparse.h@
-}
newtype Funptr_typedef4_Deref = Funptr_typedef4_Deref
  { un_Funptr_typedef4_Deref :: FC.CInt -> FC.CDouble -> IO A
  }

foreign import ccall safe "wrapper" toFunptr_typedef4_Deref ::
     Funptr_typedef4_Deref
  -> IO (Ptr.FunPtr Funptr_typedef4_Deref)

foreign import ccall safe "dynamic" fromFunptr_typedef4_Deref ::
     Ptr.FunPtr Funptr_typedef4_Deref
  -> Funptr_typedef4_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef4_Deref where

  toFunPtr = toFunptr_typedef4_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef4_Deref where

  fromFunPtr = fromFunptr_typedef4_Deref

{-| __C declaration:__ @funptr_typedef4@

    __defined at:__ @reparse.h:135:16@

    __exported by:__ @reparse.h@
-}
newtype Funptr_typedef4 = Funptr_typedef4
  { un_Funptr_typedef4 :: Ptr.FunPtr Funptr_typedef4_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Funptr_typedef5'

__defined at:__ @reparse.h:136:16@

__exported by:__ @reparse.h@
-}
newtype Funptr_typedef5_Deref = Funptr_typedef5_Deref
  { un_Funptr_typedef5_Deref :: FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)
  }

foreign import ccall safe "wrapper" toFunptr_typedef5_Deref ::
     Funptr_typedef5_Deref
  -> IO (Ptr.FunPtr Funptr_typedef5_Deref)

foreign import ccall safe "dynamic" fromFunptr_typedef5_Deref ::
     Ptr.FunPtr Funptr_typedef5_Deref
  -> Funptr_typedef5_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef5_Deref where

  toFunPtr = toFunptr_typedef5_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef5_Deref where

  fromFunPtr = fromFunptr_typedef5_Deref

{-| __C declaration:__ @funptr_typedef5@

    __defined at:__ @reparse.h:136:16@

    __exported by:__ @reparse.h@
-}
newtype Funptr_typedef5 = Funptr_typedef5
  { un_Funptr_typedef5 :: Ptr.FunPtr Funptr_typedef5_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @comments2@

    __defined at:__ @reparse.h:145:30@

    __exported by:__ @reparse.h@
-}
newtype Comments2 = Comments2
  { un_Comments2 :: A
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| Struct fields

__C declaration:__ @example_struct@

__defined at:__ @reparse.h:151:8@

__exported by:__ @reparse.h@
-}
data Example_struct = Example_struct
  { example_struct_field1 :: A
    {- ^ __C declaration:__ @field1@

         __defined at:__ @reparse.h:152:8@

         __exported by:__ @reparse.h@
    -}
  , example_struct_field2 :: Ptr.Ptr A
    {- ^ __C declaration:__ @field2@

         __defined at:__ @reparse.h:153:8@

         __exported by:__ @reparse.h@
    -}
  , example_struct_field3 :: Ptr.Ptr (Ptr.Ptr A)
    {- ^ __C declaration:__ @field3@

         __defined at:__ @reparse.h:154:8@

         __exported by:__ @reparse.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Example_struct where

  sizeOf = \_ -> (24 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Example_struct
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Example_struct
            example_struct_field12
            example_struct_field23
            example_struct_field34 ->
                 F.pokeByteOff ptr0 (0 :: Int) example_struct_field12
              >> F.pokeByteOff ptr0 (8 :: Int) example_struct_field23
              >> F.pokeByteOff ptr0 (16 :: Int) example_struct_field34

{-| __C declaration:__ @const_typedef1@

    __defined at:__ @reparse.h:220:25@

    __exported by:__ @reparse.h@
-}
newtype Const_typedef1 = Const_typedef1
  { un_Const_typedef1 :: A
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @const_typedef2@

    __defined at:__ @reparse.h:221:25@

    __exported by:__ @reparse.h@
-}
newtype Const_typedef2 = Const_typedef2
  { un_Const_typedef2 :: A
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @const_typedef3@

    __defined at:__ @reparse.h:222:25@

    __exported by:__ @reparse.h@
-}
newtype Const_typedef3 = Const_typedef3
  { un_Const_typedef3 :: Ptr.Ptr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @const_typedef4@

    __defined at:__ @reparse.h:223:25@

    __exported by:__ @reparse.h@
-}
newtype Const_typedef4 = Const_typedef4
  { un_Const_typedef4 :: Ptr.Ptr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @const_typedef5@

    __defined at:__ @reparse.h:224:25@

    __exported by:__ @reparse.h@
-}
newtype Const_typedef5 = Const_typedef5
  { un_Const_typedef5 :: Ptr.Ptr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @const_typedef6@

    __defined at:__ @reparse.h:225:25@

    __exported by:__ @reparse.h@
-}
newtype Const_typedef6 = Const_typedef6
  { un_Const_typedef6 :: Ptr.Ptr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @const_typedef7@

    __defined at:__ @reparse.h:226:25@

    __exported by:__ @reparse.h@
-}
newtype Const_typedef7 = Const_typedef7
  { un_Const_typedef7 :: Ptr.Ptr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @example_struct_with_const@

    __defined at:__ @reparse.h:228:8@

    __exported by:__ @reparse.h@
-}
data Example_struct_with_const = Example_struct_with_const
  { example_struct_with_const_const_field1 :: A
    {- ^ __C declaration:__ @const_field1@

         __defined at:__ @reparse.h:229:19@

         __exported by:__ @reparse.h@
    -}
  , example_struct_with_const_const_field2 :: A
    {- ^ __C declaration:__ @const_field2@

         __defined at:__ @reparse.h:230:19@

         __exported by:__ @reparse.h@
    -}
  , example_struct_with_const_const_field3 :: Ptr.Ptr A
    {- ^ __C declaration:__ @const_field3@

         __defined at:__ @reparse.h:231:19@

         __exported by:__ @reparse.h@
    -}
  , example_struct_with_const_const_field4 :: Ptr.Ptr A
    {- ^ __C declaration:__ @const_field4@

         __defined at:__ @reparse.h:232:19@

         __exported by:__ @reparse.h@
    -}
  , example_struct_with_const_const_field5 :: Ptr.Ptr A
    {- ^ __C declaration:__ @const_field5@

         __defined at:__ @reparse.h:233:19@

         __exported by:__ @reparse.h@
    -}
  , example_struct_with_const_const_field6 :: Ptr.Ptr A
    {- ^ __C declaration:__ @const_field6@

         __defined at:__ @reparse.h:234:19@

         __exported by:__ @reparse.h@
    -}
  , example_struct_with_const_const_field7 :: Ptr.Ptr A
    {- ^ __C declaration:__ @const_field7@

         __defined at:__ @reparse.h:235:19@

         __exported by:__ @reparse.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Example_struct_with_const where

  sizeOf = \_ -> (48 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Example_struct_with_const
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)
      <*> F.peekByteOff ptr0 (24 :: Int)
      <*> F.peekByteOff ptr0 (32 :: Int)
      <*> F.peekByteOff ptr0 (40 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Example_struct_with_const
            example_struct_with_const_const_field12
            example_struct_with_const_const_field23
            example_struct_with_const_const_field34
            example_struct_with_const_const_field45
            example_struct_with_const_const_field56
            example_struct_with_const_const_field67
            example_struct_with_const_const_field78 ->
                 F.pokeByteOff ptr0 (0 :: Int) example_struct_with_const_const_field12
              >> F.pokeByteOff ptr0 (4 :: Int) example_struct_with_const_const_field23
              >> F.pokeByteOff ptr0 (8 :: Int) example_struct_with_const_const_field34
              >> F.pokeByteOff ptr0 (16 :: Int) example_struct_with_const_const_field45
              >> F.pokeByteOff ptr0 (24 :: Int) example_struct_with_const_const_field56
              >> F.pokeByteOff ptr0 (32 :: Int) example_struct_with_const_const_field67
              >> F.pokeByteOff ptr0 (40 :: Int) example_struct_with_const_const_field78

{-| Auxiliary type used by 'Const_funptr1'

__defined at:__ @reparse.h:238:27@

__exported by:__ @reparse.h@
-}
newtype Const_funptr1_Deref = Const_funptr1_Deref
  { un_Const_funptr1_Deref :: FC.CInt -> FC.CDouble -> IO A
  }

foreign import ccall safe "wrapper" toConst_funptr1_Deref ::
     Const_funptr1_Deref
  -> IO (Ptr.FunPtr Const_funptr1_Deref)

foreign import ccall safe "dynamic" fromConst_funptr1_Deref ::
     Ptr.FunPtr Const_funptr1_Deref
  -> Const_funptr1_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr1_Deref where

  toFunPtr = toConst_funptr1_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr1_Deref where

  fromFunPtr = fromConst_funptr1_Deref

{-| __C declaration:__ @const_funptr1@

    __defined at:__ @reparse.h:238:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr1 = Const_funptr1
  { un_Const_funptr1 :: Ptr.FunPtr Const_funptr1_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Const_funptr2'

__defined at:__ @reparse.h:239:27@

__exported by:__ @reparse.h@
-}
newtype Const_funptr2_Deref = Const_funptr2_Deref
  { un_Const_funptr2_Deref :: FC.CInt -> FC.CDouble -> IO A
  }

foreign import ccall safe "wrapper" toConst_funptr2_Deref ::
     Const_funptr2_Deref
  -> IO (Ptr.FunPtr Const_funptr2_Deref)

foreign import ccall safe "dynamic" fromConst_funptr2_Deref ::
     Ptr.FunPtr Const_funptr2_Deref
  -> Const_funptr2_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr2_Deref where

  toFunPtr = toConst_funptr2_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr2_Deref where

  fromFunPtr = fromConst_funptr2_Deref

{-| __C declaration:__ @const_funptr2@

    __defined at:__ @reparse.h:239:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr2 = Const_funptr2
  { un_Const_funptr2 :: Ptr.FunPtr Const_funptr2_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Const_funptr3'

__defined at:__ @reparse.h:240:27@

__exported by:__ @reparse.h@
-}
newtype Const_funptr3_Deref = Const_funptr3_Deref
  { un_Const_funptr3_Deref :: FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)
  }

foreign import ccall safe "wrapper" toConst_funptr3_Deref ::
     Const_funptr3_Deref
  -> IO (Ptr.FunPtr Const_funptr3_Deref)

foreign import ccall safe "dynamic" fromConst_funptr3_Deref ::
     Ptr.FunPtr Const_funptr3_Deref
  -> Const_funptr3_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr3_Deref where

  toFunPtr = toConst_funptr3_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr3_Deref where

  fromFunPtr = fromConst_funptr3_Deref

{-| __C declaration:__ @const_funptr3@

    __defined at:__ @reparse.h:240:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr3 = Const_funptr3
  { un_Const_funptr3 :: Ptr.FunPtr Const_funptr3_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Const_funptr4'

__defined at:__ @reparse.h:241:27@

__exported by:__ @reparse.h@
-}
newtype Const_funptr4_Deref = Const_funptr4_Deref
  { un_Const_funptr4_Deref :: FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)
  }

foreign import ccall safe "wrapper" toConst_funptr4_Deref ::
     Const_funptr4_Deref
  -> IO (Ptr.FunPtr Const_funptr4_Deref)

foreign import ccall safe "dynamic" fromConst_funptr4_Deref ::
     Ptr.FunPtr Const_funptr4_Deref
  -> Const_funptr4_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr4_Deref where

  toFunPtr = toConst_funptr4_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr4_Deref where

  fromFunPtr = fromConst_funptr4_Deref

{-| __C declaration:__ @const_funptr4@

    __defined at:__ @reparse.h:241:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr4 = Const_funptr4
  { un_Const_funptr4 :: Ptr.FunPtr Const_funptr4_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Const_funptr5'

__defined at:__ @reparse.h:242:27@

__exported by:__ @reparse.h@
-}
newtype Const_funptr5_Deref = Const_funptr5_Deref
  { un_Const_funptr5_Deref :: FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)
  }

foreign import ccall safe "wrapper" toConst_funptr5_Deref ::
     Const_funptr5_Deref
  -> IO (Ptr.FunPtr Const_funptr5_Deref)

foreign import ccall safe "dynamic" fromConst_funptr5_Deref ::
     Ptr.FunPtr Const_funptr5_Deref
  -> Const_funptr5_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr5_Deref where

  toFunPtr = toConst_funptr5_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr5_Deref where

  fromFunPtr = fromConst_funptr5_Deref

{-| __C declaration:__ @const_funptr5@

    __defined at:__ @reparse.h:242:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr5 = Const_funptr5
  { un_Const_funptr5 :: Ptr.FunPtr Const_funptr5_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Const_funptr6'

__defined at:__ @reparse.h:243:27@

__exported by:__ @reparse.h@
-}
newtype Const_funptr6_Deref = Const_funptr6_Deref
  { un_Const_funptr6_Deref :: FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)
  }

foreign import ccall safe "wrapper" toConst_funptr6_Deref ::
     Const_funptr6_Deref
  -> IO (Ptr.FunPtr Const_funptr6_Deref)

foreign import ccall safe "dynamic" fromConst_funptr6_Deref ::
     Ptr.FunPtr Const_funptr6_Deref
  -> Const_funptr6_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr6_Deref where

  toFunPtr = toConst_funptr6_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr6_Deref where

  fromFunPtr = fromConst_funptr6_Deref

{-| __C declaration:__ @const_funptr6@

    __defined at:__ @reparse.h:243:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr6 = Const_funptr6
  { un_Const_funptr6 :: Ptr.FunPtr Const_funptr6_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Const_funptr7'

__defined at:__ @reparse.h:244:27@

__exported by:__ @reparse.h@
-}
newtype Const_funptr7_Deref = Const_funptr7_Deref
  { un_Const_funptr7_Deref :: FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)
  }

foreign import ccall safe "wrapper" toConst_funptr7_Deref ::
     Const_funptr7_Deref
  -> IO (Ptr.FunPtr Const_funptr7_Deref)

foreign import ccall safe "dynamic" fromConst_funptr7_Deref ::
     Ptr.FunPtr Const_funptr7_Deref
  -> Const_funptr7_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr7_Deref where

  toFunPtr = toConst_funptr7_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr7_Deref where

  fromFunPtr = fromConst_funptr7_Deref

{-| __C declaration:__ @const_funptr7@

    __defined at:__ @reparse.h:244:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr7 = Const_funptr7
  { un_Const_funptr7 :: Ptr.FunPtr Const_funptr7_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @BOOL@

    __defined at:__ @reparse.h:280:9@

    __exported by:__ @reparse.h@
-}
newtype BOOL = BOOL
  { un_BOOL :: FC.CBool
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @INT@

    __defined at:__ @reparse.h:281:9@

    __exported by:__ @reparse.h@
-}
newtype INT = INT
  { un_INT :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @INTP@

    __defined at:__ @reparse.h:282:9@

    __exported by:__ @reparse.h@
-}
newtype INTP = INTP
  { un_INTP :: Ptr.Ptr FC.CInt
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @INTCP@

    __defined at:__ @reparse.h:283:9@

    __exported by:__ @reparse.h@
-}
newtype INTCP = INTCP
  { un_INTCP :: Ptr.Ptr FC.CInt
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Function declarations

__C declaration:__ @args_char1@

__defined at:__ @reparse.h:17:6@

__exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_394853579d622671" args_char1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_char2@

    __defined at:__ @reparse.h:18:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_da98fe949f347bb4" args_char2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CSChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_char3@

    __defined at:__ @reparse.h:19:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_1b54575fa299f64d" args_char3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_short1@

    __defined at:__ @reparse.h:21:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_5eb574c361d453a5" args_short1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CShort
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_short2@

    __defined at:__ @reparse.h:22:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_47d5b6ac9938a676" args_short2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CShort
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_short3@

    __defined at:__ @reparse.h:23:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_7b3f1c99ea5c31ce" args_short3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUShort
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_int1@

    __defined at:__ @reparse.h:25:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_3103fa698febc2e4" args_int1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_int2@

    __defined at:__ @reparse.h:26:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_f73bd21e02a58e0f" args_int2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_int3@

    __defined at:__ @reparse.h:27:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_6d36b4892d340141" args_int3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_long1@

    __defined at:__ @reparse.h:29:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_c80fdb5f86f0e67e" args_long1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CLong
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_long2@

    __defined at:__ @reparse.h:30:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_b0db0696cda23a78" args_long2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CLong
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_long3@

    __defined at:__ @reparse.h:31:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_78868ddd9e2ed516" args_long3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CULong
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_float@

    __defined at:__ @reparse.h:33:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_555dbd0a04bc0304" args_float ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CFloat
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_double@

    __defined at:__ @reparse.h:34:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_fe4a06766df0d1e6" args_double ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_bool1@

    __defined at:__ @reparse.h:35:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_a607c108df5a1598" args_bool1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| Pointer-based API for 'args_struct'

-}
foreign import ccall safe "hs_bindgen_test_reparse_2867c64e14a8b4b4" args_struct_wrapper ::
     A
  -> Ptr.Ptr Some_struct
  -> IO ()

{-| __C declaration:__ @args_struct@

    __defined at:__ @reparse.h:37:6@

    __exported by:__ @reparse.h@
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
foreign import ccall safe "hs_bindgen_test_reparse_dbccce7991402835" args_union_wrapper ::
     A
  -> Ptr.Ptr Some_union
  -> IO ()

{-| __C declaration:__ @args_union@

    __defined at:__ @reparse.h:38:6@

    __exported by:__ @reparse.h@
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

    __defined at:__ @reparse.h:39:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_58e9df8b58217744" args_enum ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_enum
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_pointer1@

    __defined at:__ @reparse.h:41:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_da0ab238a099dc49" args_pointer1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_pointer2@

    __defined at:__ @reparse.h:42:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_abd9ce8bdda564f4" args_pointer2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr (Ptr.Ptr FC.CInt)
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_pointer3@

    __defined at:__ @reparse.h:43:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_ed43d3d8eb25de8f" args_pointer3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr Void
     {- ^ __C declaration:__ @arg3@
     -}
  -> IO ()

{-| __C declaration:__ @ret_A@

    __defined at:__ @reparse.h:47:3@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_a45e66140bccd9e3" ret_A ::
     IO A

{-| __C declaration:__ @ret_char1@

    __defined at:__ @reparse.h:49:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_44f364f98d9773fa" ret_char1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CChar

{-| __C declaration:__ @ret_char2@

    __defined at:__ @reparse.h:50:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_c0ccab4edfec7750" ret_char2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CSChar

{-| __C declaration:__ @ret_char3@

    __defined at:__ @reparse.h:51:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_22570fd6296f553c" ret_char3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CUChar

{-| __C declaration:__ @ret_short1@

    __defined at:__ @reparse.h:53:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_60148c950d753d1d" ret_short1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CShort

{-| __C declaration:__ @ret_short2@

    __defined at:__ @reparse.h:54:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_1f5d60c2fc8391f8" ret_short2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CShort

{-| __C declaration:__ @ret_short3@

    __defined at:__ @reparse.h:55:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_c35d296d9df5f67d" ret_short3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CUShort

{-| __C declaration:__ @ret_int1@

    __defined at:__ @reparse.h:57:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_edcb4249e75b3e31" ret_int1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @ret_int2@

    __defined at:__ @reparse.h:58:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_28e6902c5d5c160d" ret_int2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @ret_int3@

    __defined at:__ @reparse.h:59:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_42873a9aa50685f7" ret_int3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CUInt

{-| __C declaration:__ @ret_long1@

    __defined at:__ @reparse.h:61:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_ef0217b739070465" ret_long1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CLong

{-| __C declaration:__ @ret_long2@

    __defined at:__ @reparse.h:62:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_0b8baea451432efe" ret_long2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CLong

{-| __C declaration:__ @ret_long3@

    __defined at:__ @reparse.h:63:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_264e25c048487a65" ret_long3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CULong

{-| __C declaration:__ @ret_float@

    __defined at:__ @reparse.h:65:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_d4f4783fa3bcf0fd" ret_float ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CFloat

{-| __C declaration:__ @ret_double@

    __defined at:__ @reparse.h:66:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_94ae038975c7b6e8" ret_double ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CDouble

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @reparse.h:67:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_c6e57fb4c8ccc002" ret_bool1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CBool

{-| Pointer-based API for 'ret_struct'

-}
foreign import ccall safe "hs_bindgen_test_reparse_4d9a2038e7abf410" ret_struct_wrapper ::
     A
  -> Ptr.Ptr Some_struct
  -> IO ()

{-| __C declaration:__ @ret_struct@

    __defined at:__ @reparse.h:69:20@

    __exported by:__ @reparse.h@
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
foreign import ccall safe "hs_bindgen_test_reparse_6614cf4950ce1e7c" ret_union_wrapper ::
     A
  -> Ptr.Ptr Some_union
  -> IO ()

{-| __C declaration:__ @ret_union@

    __defined at:__ @reparse.h:70:20@

    __exported by:__ @reparse.h@
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

    __defined at:__ @reparse.h:71:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_4a8e0e395958b0ed" ret_enum ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO Some_enum

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @reparse.h:73:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_59ba78873de08998" ret_pointer1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @reparse.h:74:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_f075faf1943231c1" ret_pointer2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @reparse.h:75:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_a865c16cbfd0f2b1" ret_pointer3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @body1@

    __defined at:__ @reparse.h:79:5@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_0dd10e2baacf20e1" body1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @body2@

    __defined at:__ @reparse.h:80:3@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_e40f2da3eda8e4ab" body2 ::
     IO A

{-| Pointer-based API for 'args_complex_float'

-}
foreign import ccall safe "hs_bindgen_test_reparse_cabceb6db44b7d81" args_complex_float_wrapper ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> IO ()

{-| __C declaration:__ @args_complex_float@

    __defined at:__ @reparse.h:84:6@

    __exported by:__ @reparse.h@
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
foreign import ccall safe "hs_bindgen_test_reparse_ef2a09e9cd3eec0c" args_complex_double_wrapper ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> IO ()

{-| __C declaration:__ @args_complex_double@

    __defined at:__ @reparse.h:85:6@

    __exported by:__ @reparse.h@
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
foreign import ccall safe "hs_bindgen_test_reparse_26ada386c6fc7617" ret_complex_float_wrapper ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> IO ()

{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @reparse.h:86:17@

    __exported by:__ @reparse.h@
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
foreign import ccall safe "hs_bindgen_test_reparse_b99f35785e9f9b5c" ret_complex_double_wrapper ::
     A
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> IO ()

{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @reparse.h:87:17@

    __exported by:__ @reparse.h@
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

    __defined at:__ @reparse.h:94:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_4c1c48b67908e0ad" bespoke_args1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @reparse.h:95:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_7bd6c9115d303872" bespoke_args2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CSize
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @reparse.h:97:8@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_994d22fc993523bf" bespoke_ret1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CBool

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @reparse.h:98:8@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_c7649a4aa2e14a89" bespoke_ret2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CSize

{-| Arrays

__C declaration:__ @arr_args1@

__defined at:__ @reparse.h:104:6@

__exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_69045f97d21cfcd3" arr_args1 ::
     Ptr.Ptr A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| __C declaration:__ @arr_args2@

    __defined at:__ @reparse.h:105:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_75b7d6fa15700a72" arr_args2 ::
     Ptr.Ptr (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| __C declaration:__ @arr_args3@

    __defined at:__ @reparse.h:106:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_64bcbef92728339f" arr_args3 ::
     Ptr.Ptr A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| __C declaration:__ @arr_args4@

    __defined at:__ @reparse.h:107:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_1635b68f717cc6df" arr_args4 ::
     Ptr.Ptr (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| Function pointers

__C declaration:__ @funptr_args1@

__defined at:__ @reparse.h:126:6@

__exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_09ca38f534ba1397" funptr_args1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (IO ())
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @funptr_args2@

    __defined at:__ @reparse.h:127:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_43e32eb1c4511130" funptr_args2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (IO FC.CInt)
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @funptr_args3@

    __defined at:__ @reparse.h:128:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_639eb292178302da" funptr_args3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (FC.CInt -> IO ())
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @funptr_args4@

    __defined at:__ @reparse.h:129:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_deaef357745591d1" funptr_args4 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @funptr_args5@

    __defined at:__ @reparse.h:130:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_bd58865f6f33ce14" funptr_args5 ::
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

__defined at:__ @reparse.h:144:25@

__exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_a609cc59a266965e" comments1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| `const` qualifier

  NOTE: These were not parsed correctly prior to the switch to language-c.

__C declaration:__ @const_prim_before1@

__defined at:__ @reparse.h:179:6@

__exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_55af49e081d3af5c" const_prim_before1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @reparse.h:180:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_17a6476d46f98f53" const_prim_before2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CSChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @reparse.h:181:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_1fce4989a8ceca6d" const_prim_before3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @reparse.h:182:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_0b8f21af35a88318" const_prim_after1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @reparse.h:183:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_069042df961b78f1" const_prim_after2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CSChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @reparse.h:184:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_14ab286beb6d7436" const_prim_after3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @reparse.h:188:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_3f5a39bd9a93581a" const_withoutSign_before1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CFloat
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @reparse.h:189:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_5a190462ddfe8168" const_withoutSign_before2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @reparse.h:190:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_70a40d82bb46c21c" const_withoutSign_before3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| Pointer-based API for 'const_withoutSign_before4'

-}
foreign import ccall safe "hs_bindgen_test_reparse_81ce15a9b9be53f8" const_withoutSign_before4_wrapper ::
     A
  -> Ptr.Ptr Some_struct
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @reparse.h:191:6@

    __exported by:__ @reparse.h@
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
foreign import ccall safe "hs_bindgen_test_reparse_39c58819fdca0585" const_withoutSign_before5_wrapper ::
     A
  -> Ptr.Ptr Some_union
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @reparse.h:192:6@

    __exported by:__ @reparse.h@
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

    __defined at:__ @reparse.h:193:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_4c5a32cae31a651c" const_withoutSign_before6 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_enum
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @reparse.h:194:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_0341b56dd9dde729" const_withoutSign_before7 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @reparse.h:195:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_d43c37be4d91bd6c" const_withoutSign_before8 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CSize
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @reparse.h:197:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_f456a2b015543748" const_withoutSign_after1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CFloat
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @reparse.h:198:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_729c897c9a9bfc92" const_withoutSign_after2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @reparse.h:199:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_5e46eebba0299b49" const_withoutSign_after3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| Pointer-based API for 'const_withoutSign_after4'

-}
foreign import ccall safe "hs_bindgen_test_reparse_18b842a824ecef09" const_withoutSign_after4_wrapper ::
     A
  -> Ptr.Ptr Some_struct
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @reparse.h:200:6@

    __exported by:__ @reparse.h@
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
foreign import ccall safe "hs_bindgen_test_reparse_75feb5260081201b" const_withoutSign_after5_wrapper ::
     A
  -> Ptr.Ptr Some_union
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @reparse.h:201:6@

    __exported by:__ @reparse.h@
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

    __defined at:__ @reparse.h:202:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_3375ca58017bcbeb" const_withoutSign_after6 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_enum
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @reparse.h:203:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_5da184668d3e18ba" const_withoutSign_after7 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @reparse.h:204:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_3cae7641509d64fa" const_withoutSign_after8 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CSize
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @reparse.h:208:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_4dcc1a25c1ecaaa2" const_pointers_args1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @reparse.h:209:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_fec0ae82f5b0ad81" const_pointers_args2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @reparse.h:210:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_fe0313a1b7b08d51" const_pointers_args3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @reparse.h:211:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_33f3fc94401a8bfe" const_pointers_args4 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @reparse.h:212:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_391d9682b9dc51ac" const_pointers_args5 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @reparse.h:214:19@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_8bf23a9981153f56" const_pointers_ret1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @reparse.h:215:19@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_849c6f0a166afc3c" const_pointers_ret2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @reparse.h:216:19@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_2bcb395289e776af" const_pointers_ret3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @reparse.h:217:19@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_10d30d6be4435bb5" const_pointers_ret4 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @reparse.h:218:19@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_4e0a9385778eeea9" const_pointers_ret5 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| Pointer-based API for 'const_array_elem1'

-}
foreign import ccall safe "hs_bindgen_test_reparse_216b4842313741d6" const_array_elem1_wrapper ::
     Ptr.Ptr A
  -> IO ()

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @reparse.h:246:6@

    __exported by:__ @reparse.h@
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

    __defined at:__ @reparse.h:247:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_1c0be30090d3f0b4" const_array_elem2 ::
     Ptr.Ptr (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| Pointer-based API for 'const_array_elem3'

-}
foreign import ccall safe "hs_bindgen_test_reparse_30c17e3a462eeecc" const_array_elem3_wrapper ::
     Ptr.Ptr (Ptr.Ptr A)
  -> IO ()

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @reparse.h:248:6@

    __exported by:__ @reparse.h@
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

__defined at:__ @reparse.h:256:3@

__exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_8ab2f7d7d9185985" noParams1 ::
     IO A

{-| __C declaration:__ @noParams2@

    __defined at:__ @reparse.h:257:3@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_3154e7cc23e3e0f3" noParams2 ::
     IO A

{-| __C declaration:__ @noParams3@

    __defined at:__ @reparse.h:258:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_3d23de5d2d770dfe" noParams3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (IO FC.CInt)
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @reparse.h:262:8@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_cfe9601f75800453" funptr_ret1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (IO ()))

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @reparse.h:263:8@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_ca3824f5cf114f19" funptr_ret2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (IO FC.CInt))

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @reparse.h:264:8@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_2de886ad95f674f5" funptr_ret3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> IO ()))

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @reparse.h:265:8@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_b4856eab77ec0cbf" funptr_ret4 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar))

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @reparse.h:269:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_39273a56f1a80904" funptr_ret5 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @reparse.h:270:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_5e2dfa9b8f6075ee" funptr_ret6 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @reparse.h:271:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_0b65a87e08b5a1a7" funptr_ret7 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @reparse.h:272:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_629050352113405f" funptr_ret8 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @reparse.h:273:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_a1dcda8f782ad284" funptr_ret9 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @reparse.h:274:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_ca08b018fda612eb" funptr_ret10 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

foreign import ccall unsafe "hs_bindgen_test_reparse_1cbcf8b84924816c" hs_bindgen_test_reparse_1cbcf8b84924816c ::
     IO (Ptr.FunPtr (A -> FC.CChar -> IO ()))

{-# NOINLINE args_char1_ptr #-}

{-| Function declarations

__C declaration:__ @args_char1@

__defined at:__ @reparse.h:17:6@

__exported by:__ @reparse.h@
-}
args_char1_ptr :: Ptr.FunPtr (A -> FC.CChar -> IO ())
args_char1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_1cbcf8b84924816c

foreign import ccall unsafe "hs_bindgen_test_reparse_ec2d78b82f444fd0" hs_bindgen_test_reparse_ec2d78b82f444fd0 ::
     IO (Ptr.FunPtr (A -> FC.CSChar -> IO ()))

{-# NOINLINE args_char2_ptr #-}

{-| __C declaration:__ @args_char2@

    __defined at:__ @reparse.h:18:6@

    __exported by:__ @reparse.h@
-}
args_char2_ptr :: Ptr.FunPtr (A -> FC.CSChar -> IO ())
args_char2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_ec2d78b82f444fd0

foreign import ccall unsafe "hs_bindgen_test_reparse_1baa18e723594389" hs_bindgen_test_reparse_1baa18e723594389 ::
     IO (Ptr.FunPtr (A -> FC.CUChar -> IO ()))

{-# NOINLINE args_char3_ptr #-}

{-| __C declaration:__ @args_char3@

    __defined at:__ @reparse.h:19:6@

    __exported by:__ @reparse.h@
-}
args_char3_ptr :: Ptr.FunPtr (A -> FC.CUChar -> IO ())
args_char3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_1baa18e723594389

foreign import ccall unsafe "hs_bindgen_test_reparse_c96cef4ef5f5e180" hs_bindgen_test_reparse_c96cef4ef5f5e180 ::
     IO (Ptr.FunPtr (A -> FC.CShort -> IO ()))

{-# NOINLINE args_short1_ptr #-}

{-| __C declaration:__ @args_short1@

    __defined at:__ @reparse.h:21:6@

    __exported by:__ @reparse.h@
-}
args_short1_ptr :: Ptr.FunPtr (A -> FC.CShort -> IO ())
args_short1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_c96cef4ef5f5e180

foreign import ccall unsafe "hs_bindgen_test_reparse_3a683552d4f772c7" hs_bindgen_test_reparse_3a683552d4f772c7 ::
     IO (Ptr.FunPtr (A -> FC.CShort -> IO ()))

{-# NOINLINE args_short2_ptr #-}

{-| __C declaration:__ @args_short2@

    __defined at:__ @reparse.h:22:6@

    __exported by:__ @reparse.h@
-}
args_short2_ptr :: Ptr.FunPtr (A -> FC.CShort -> IO ())
args_short2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_3a683552d4f772c7

foreign import ccall unsafe "hs_bindgen_test_reparse_f3284022ac706255" hs_bindgen_test_reparse_f3284022ac706255 ::
     IO (Ptr.FunPtr (A -> FC.CUShort -> IO ()))

{-# NOINLINE args_short3_ptr #-}

{-| __C declaration:__ @args_short3@

    __defined at:__ @reparse.h:23:6@

    __exported by:__ @reparse.h@
-}
args_short3_ptr :: Ptr.FunPtr (A -> FC.CUShort -> IO ())
args_short3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_f3284022ac706255

foreign import ccall unsafe "hs_bindgen_test_reparse_5c4d785286ccca6b" hs_bindgen_test_reparse_5c4d785286ccca6b ::
     IO (Ptr.FunPtr (A -> FC.CInt -> IO ()))

{-# NOINLINE args_int1_ptr #-}

{-| __C declaration:__ @args_int1@

    __defined at:__ @reparse.h:25:6@

    __exported by:__ @reparse.h@
-}
args_int1_ptr :: Ptr.FunPtr (A -> FC.CInt -> IO ())
args_int1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_5c4d785286ccca6b

foreign import ccall unsafe "hs_bindgen_test_reparse_e323b837afe40be7" hs_bindgen_test_reparse_e323b837afe40be7 ::
     IO (Ptr.FunPtr (A -> FC.CInt -> IO ()))

{-# NOINLINE args_int2_ptr #-}

{-| __C declaration:__ @args_int2@

    __defined at:__ @reparse.h:26:6@

    __exported by:__ @reparse.h@
-}
args_int2_ptr :: Ptr.FunPtr (A -> FC.CInt -> IO ())
args_int2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_e323b837afe40be7

foreign import ccall unsafe "hs_bindgen_test_reparse_eb0e5feb8eb4082d" hs_bindgen_test_reparse_eb0e5feb8eb4082d ::
     IO (Ptr.FunPtr (A -> FC.CUInt -> IO ()))

{-# NOINLINE args_int3_ptr #-}

{-| __C declaration:__ @args_int3@

    __defined at:__ @reparse.h:27:6@

    __exported by:__ @reparse.h@
-}
args_int3_ptr :: Ptr.FunPtr (A -> FC.CUInt -> IO ())
args_int3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_eb0e5feb8eb4082d

foreign import ccall unsafe "hs_bindgen_test_reparse_d7d322f23a65f43b" hs_bindgen_test_reparse_d7d322f23a65f43b ::
     IO (Ptr.FunPtr (A -> FC.CLong -> IO ()))

{-# NOINLINE args_long1_ptr #-}

{-| __C declaration:__ @args_long1@

    __defined at:__ @reparse.h:29:6@

    __exported by:__ @reparse.h@
-}
args_long1_ptr :: Ptr.FunPtr (A -> FC.CLong -> IO ())
args_long1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_d7d322f23a65f43b

foreign import ccall unsafe "hs_bindgen_test_reparse_378c16768a6f6f21" hs_bindgen_test_reparse_378c16768a6f6f21 ::
     IO (Ptr.FunPtr (A -> FC.CLong -> IO ()))

{-# NOINLINE args_long2_ptr #-}

{-| __C declaration:__ @args_long2@

    __defined at:__ @reparse.h:30:6@

    __exported by:__ @reparse.h@
-}
args_long2_ptr :: Ptr.FunPtr (A -> FC.CLong -> IO ())
args_long2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_378c16768a6f6f21

foreign import ccall unsafe "hs_bindgen_test_reparse_548dcd4760226ee2" hs_bindgen_test_reparse_548dcd4760226ee2 ::
     IO (Ptr.FunPtr (A -> FC.CULong -> IO ()))

{-# NOINLINE args_long3_ptr #-}

{-| __C declaration:__ @args_long3@

    __defined at:__ @reparse.h:31:6@

    __exported by:__ @reparse.h@
-}
args_long3_ptr :: Ptr.FunPtr (A -> FC.CULong -> IO ())
args_long3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_548dcd4760226ee2

foreign import ccall unsafe "hs_bindgen_test_reparse_701d01261043851b" hs_bindgen_test_reparse_701d01261043851b ::
     IO (Ptr.FunPtr (A -> FC.CFloat -> IO ()))

{-# NOINLINE args_float_ptr #-}

{-| __C declaration:__ @args_float@

    __defined at:__ @reparse.h:33:6@

    __exported by:__ @reparse.h@
-}
args_float_ptr :: Ptr.FunPtr (A -> FC.CFloat -> IO ())
args_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_701d01261043851b

foreign import ccall unsafe "hs_bindgen_test_reparse_ff631e42f704e4cd" hs_bindgen_test_reparse_ff631e42f704e4cd ::
     IO (Ptr.FunPtr (A -> FC.CDouble -> IO ()))

{-# NOINLINE args_double_ptr #-}

{-| __C declaration:__ @args_double@

    __defined at:__ @reparse.h:34:6@

    __exported by:__ @reparse.h@
-}
args_double_ptr :: Ptr.FunPtr (A -> FC.CDouble -> IO ())
args_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_ff631e42f704e4cd

foreign import ccall unsafe "hs_bindgen_test_reparse_6e289c6cc6d382bf" hs_bindgen_test_reparse_6e289c6cc6d382bf ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE args_bool1_ptr #-}

{-| __C declaration:__ @args_bool1@

    __defined at:__ @reparse.h:35:6@

    __exported by:__ @reparse.h@
-}
args_bool1_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
args_bool1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_6e289c6cc6d382bf

foreign import ccall unsafe "hs_bindgen_test_reparse_26b20c1b89e46b02" hs_bindgen_test_reparse_26b20c1b89e46b02 ::
     IO (Ptr.FunPtr (A -> Some_struct -> IO ()))

{-# NOINLINE args_struct_ptr #-}

{-| __C declaration:__ @args_struct@

    __defined at:__ @reparse.h:37:6@

    __exported by:__ @reparse.h@
-}
args_struct_ptr :: Ptr.FunPtr (A -> Some_struct -> IO ())
args_struct_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_26b20c1b89e46b02

foreign import ccall unsafe "hs_bindgen_test_reparse_cfd37f06f21b8755" hs_bindgen_test_reparse_cfd37f06f21b8755 ::
     IO (Ptr.FunPtr (A -> Some_union -> IO ()))

{-# NOINLINE args_union_ptr #-}

{-| __C declaration:__ @args_union@

    __defined at:__ @reparse.h:38:6@

    __exported by:__ @reparse.h@
-}
args_union_ptr :: Ptr.FunPtr (A -> Some_union -> IO ())
args_union_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_cfd37f06f21b8755

foreign import ccall unsafe "hs_bindgen_test_reparse_69882f8f862fffc2" hs_bindgen_test_reparse_69882f8f862fffc2 ::
     IO (Ptr.FunPtr (A -> Some_enum -> IO ()))

{-# NOINLINE args_enum_ptr #-}

{-| __C declaration:__ @args_enum@

    __defined at:__ @reparse.h:39:6@

    __exported by:__ @reparse.h@
-}
args_enum_ptr :: Ptr.FunPtr (A -> Some_enum -> IO ())
args_enum_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_69882f8f862fffc2

foreign import ccall unsafe "hs_bindgen_test_reparse_23bde4e97b66c470" hs_bindgen_test_reparse_23bde4e97b66c470 ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE args_pointer1_ptr #-}

{-| __C declaration:__ @args_pointer1@

    __defined at:__ @reparse.h:41:6@

    __exported by:__ @reparse.h@
-}
args_pointer1_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
args_pointer1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_23bde4e97b66c470

foreign import ccall unsafe "hs_bindgen_test_reparse_fceb546239df3c0a" hs_bindgen_test_reparse_fceb546239df3c0a ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr (Ptr.Ptr FC.CInt)) -> IO ()))

{-# NOINLINE args_pointer2_ptr #-}

{-| __C declaration:__ @args_pointer2@

    __defined at:__ @reparse.h:42:6@

    __exported by:__ @reparse.h@
-}
args_pointer2_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr (Ptr.Ptr FC.CInt)) -> IO ())
args_pointer2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_fceb546239df3c0a

foreign import ccall unsafe "hs_bindgen_test_reparse_0cb396fb06dd816a" hs_bindgen_test_reparse_0cb396fb06dd816a ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr Void) -> IO ()))

{-# NOINLINE args_pointer3_ptr #-}

{-| __C declaration:__ @args_pointer3@

    __defined at:__ @reparse.h:43:6@

    __exported by:__ @reparse.h@
-}
args_pointer3_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr Void) -> IO ())
args_pointer3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_0cb396fb06dd816a

foreign import ccall unsafe "hs_bindgen_test_reparse_a7564eacf3ad149f" hs_bindgen_test_reparse_a7564eacf3ad149f ::
     IO (Ptr.FunPtr (IO A))

{-# NOINLINE ret_A_ptr #-}

{-| __C declaration:__ @ret_A@

    __defined at:__ @reparse.h:47:3@

    __exported by:__ @reparse.h@
-}
ret_A_ptr :: Ptr.FunPtr (IO A)
ret_A_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_a7564eacf3ad149f

foreign import ccall unsafe "hs_bindgen_test_reparse_7b5b646ee4e06777" hs_bindgen_test_reparse_7b5b646ee4e06777 ::
     IO (Ptr.FunPtr (A -> IO FC.CChar))

{-# NOINLINE ret_char1_ptr #-}

{-| __C declaration:__ @ret_char1@

    __defined at:__ @reparse.h:49:20@

    __exported by:__ @reparse.h@
-}
ret_char1_ptr :: Ptr.FunPtr (A -> IO FC.CChar)
ret_char1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_7b5b646ee4e06777

foreign import ccall unsafe "hs_bindgen_test_reparse_7c05cbccaf1be8b6" hs_bindgen_test_reparse_7c05cbccaf1be8b6 ::
     IO (Ptr.FunPtr (A -> IO FC.CSChar))

{-# NOINLINE ret_char2_ptr #-}

{-| __C declaration:__ @ret_char2@

    __defined at:__ @reparse.h:50:20@

    __exported by:__ @reparse.h@
-}
ret_char2_ptr :: Ptr.FunPtr (A -> IO FC.CSChar)
ret_char2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_7c05cbccaf1be8b6

foreign import ccall unsafe "hs_bindgen_test_reparse_0fc74f839f906d7e" hs_bindgen_test_reparse_0fc74f839f906d7e ::
     IO (Ptr.FunPtr (A -> IO FC.CUChar))

{-# NOINLINE ret_char3_ptr #-}

{-| __C declaration:__ @ret_char3@

    __defined at:__ @reparse.h:51:20@

    __exported by:__ @reparse.h@
-}
ret_char3_ptr :: Ptr.FunPtr (A -> IO FC.CUChar)
ret_char3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_0fc74f839f906d7e

foreign import ccall unsafe "hs_bindgen_test_reparse_72ff9f5cb5daaae8" hs_bindgen_test_reparse_72ff9f5cb5daaae8 ::
     IO (Ptr.FunPtr (A -> IO FC.CShort))

{-# NOINLINE ret_short1_ptr #-}

{-| __C declaration:__ @ret_short1@

    __defined at:__ @reparse.h:53:20@

    __exported by:__ @reparse.h@
-}
ret_short1_ptr :: Ptr.FunPtr (A -> IO FC.CShort)
ret_short1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_72ff9f5cb5daaae8

foreign import ccall unsafe "hs_bindgen_test_reparse_eb5427ff3ea0d96e" hs_bindgen_test_reparse_eb5427ff3ea0d96e ::
     IO (Ptr.FunPtr (A -> IO FC.CShort))

{-# NOINLINE ret_short2_ptr #-}

{-| __C declaration:__ @ret_short2@

    __defined at:__ @reparse.h:54:20@

    __exported by:__ @reparse.h@
-}
ret_short2_ptr :: Ptr.FunPtr (A -> IO FC.CShort)
ret_short2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_eb5427ff3ea0d96e

foreign import ccall unsafe "hs_bindgen_test_reparse_823adc61eed1550c" hs_bindgen_test_reparse_823adc61eed1550c ::
     IO (Ptr.FunPtr (A -> IO FC.CUShort))

{-# NOINLINE ret_short3_ptr #-}

{-| __C declaration:__ @ret_short3@

    __defined at:__ @reparse.h:55:20@

    __exported by:__ @reparse.h@
-}
ret_short3_ptr :: Ptr.FunPtr (A -> IO FC.CUShort)
ret_short3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_823adc61eed1550c

foreign import ccall unsafe "hs_bindgen_test_reparse_79ce8d81113cf766" hs_bindgen_test_reparse_79ce8d81113cf766 ::
     IO (Ptr.FunPtr (A -> IO FC.CInt))

{-# NOINLINE ret_int1_ptr #-}

{-| __C declaration:__ @ret_int1@

    __defined at:__ @reparse.h:57:20@

    __exported by:__ @reparse.h@
-}
ret_int1_ptr :: Ptr.FunPtr (A -> IO FC.CInt)
ret_int1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_79ce8d81113cf766

foreign import ccall unsafe "hs_bindgen_test_reparse_d369bd4861f00c84" hs_bindgen_test_reparse_d369bd4861f00c84 ::
     IO (Ptr.FunPtr (A -> IO FC.CInt))

{-# NOINLINE ret_int2_ptr #-}

{-| __C declaration:__ @ret_int2@

    __defined at:__ @reparse.h:58:20@

    __exported by:__ @reparse.h@
-}
ret_int2_ptr :: Ptr.FunPtr (A -> IO FC.CInt)
ret_int2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_d369bd4861f00c84

foreign import ccall unsafe "hs_bindgen_test_reparse_0336d583fc7b5951" hs_bindgen_test_reparse_0336d583fc7b5951 ::
     IO (Ptr.FunPtr (A -> IO FC.CUInt))

{-# NOINLINE ret_int3_ptr #-}

{-| __C declaration:__ @ret_int3@

    __defined at:__ @reparse.h:59:20@

    __exported by:__ @reparse.h@
-}
ret_int3_ptr :: Ptr.FunPtr (A -> IO FC.CUInt)
ret_int3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_0336d583fc7b5951

foreign import ccall unsafe "hs_bindgen_test_reparse_36845109a4ce7992" hs_bindgen_test_reparse_36845109a4ce7992 ::
     IO (Ptr.FunPtr (A -> IO FC.CLong))

{-# NOINLINE ret_long1_ptr #-}

{-| __C declaration:__ @ret_long1@

    __defined at:__ @reparse.h:61:20@

    __exported by:__ @reparse.h@
-}
ret_long1_ptr :: Ptr.FunPtr (A -> IO FC.CLong)
ret_long1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_36845109a4ce7992

foreign import ccall unsafe "hs_bindgen_test_reparse_ac32dbc1e79e704e" hs_bindgen_test_reparse_ac32dbc1e79e704e ::
     IO (Ptr.FunPtr (A -> IO FC.CLong))

{-# NOINLINE ret_long2_ptr #-}

{-| __C declaration:__ @ret_long2@

    __defined at:__ @reparse.h:62:20@

    __exported by:__ @reparse.h@
-}
ret_long2_ptr :: Ptr.FunPtr (A -> IO FC.CLong)
ret_long2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_ac32dbc1e79e704e

foreign import ccall unsafe "hs_bindgen_test_reparse_6fba85ecad7d8d4e" hs_bindgen_test_reparse_6fba85ecad7d8d4e ::
     IO (Ptr.FunPtr (A -> IO FC.CULong))

{-# NOINLINE ret_long3_ptr #-}

{-| __C declaration:__ @ret_long3@

    __defined at:__ @reparse.h:63:20@

    __exported by:__ @reparse.h@
-}
ret_long3_ptr :: Ptr.FunPtr (A -> IO FC.CULong)
ret_long3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_6fba85ecad7d8d4e

foreign import ccall unsafe "hs_bindgen_test_reparse_e9ac779a7c943add" hs_bindgen_test_reparse_e9ac779a7c943add ::
     IO (Ptr.FunPtr (A -> IO FC.CFloat))

{-# NOINLINE ret_float_ptr #-}

{-| __C declaration:__ @ret_float@

    __defined at:__ @reparse.h:65:20@

    __exported by:__ @reparse.h@
-}
ret_float_ptr :: Ptr.FunPtr (A -> IO FC.CFloat)
ret_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_e9ac779a7c943add

foreign import ccall unsafe "hs_bindgen_test_reparse_7095a5f5be3ecc0c" hs_bindgen_test_reparse_7095a5f5be3ecc0c ::
     IO (Ptr.FunPtr (A -> IO FC.CDouble))

{-# NOINLINE ret_double_ptr #-}

{-| __C declaration:__ @ret_double@

    __defined at:__ @reparse.h:66:20@

    __exported by:__ @reparse.h@
-}
ret_double_ptr :: Ptr.FunPtr (A -> IO FC.CDouble)
ret_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_7095a5f5be3ecc0c

foreign import ccall unsafe "hs_bindgen_test_reparse_c7b5be49f4314899" hs_bindgen_test_reparse_c7b5be49f4314899 ::
     IO (Ptr.FunPtr (A -> IO FC.CBool))

{-# NOINLINE ret_bool1_ptr #-}

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @reparse.h:67:20@

    __exported by:__ @reparse.h@
-}
ret_bool1_ptr :: Ptr.FunPtr (A -> IO FC.CBool)
ret_bool1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_c7b5be49f4314899

foreign import ccall unsafe "hs_bindgen_test_reparse_03ec23cf81b62ce3" hs_bindgen_test_reparse_03ec23cf81b62ce3 ::
     IO (Ptr.FunPtr (A -> IO Some_struct))

{-# NOINLINE ret_struct_ptr #-}

{-| __C declaration:__ @ret_struct@

    __defined at:__ @reparse.h:69:20@

    __exported by:__ @reparse.h@
-}
ret_struct_ptr :: Ptr.FunPtr (A -> IO Some_struct)
ret_struct_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_03ec23cf81b62ce3

foreign import ccall unsafe "hs_bindgen_test_reparse_5315544d48ea5b07" hs_bindgen_test_reparse_5315544d48ea5b07 ::
     IO (Ptr.FunPtr (A -> IO Some_union))

{-# NOINLINE ret_union_ptr #-}

{-| __C declaration:__ @ret_union@

    __defined at:__ @reparse.h:70:20@

    __exported by:__ @reparse.h@
-}
ret_union_ptr :: Ptr.FunPtr (A -> IO Some_union)
ret_union_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_5315544d48ea5b07

foreign import ccall unsafe "hs_bindgen_test_reparse_9fb7ddbcd84c72f1" hs_bindgen_test_reparse_9fb7ddbcd84c72f1 ::
     IO (Ptr.FunPtr (A -> IO Some_enum))

{-# NOINLINE ret_enum_ptr #-}

{-| __C declaration:__ @ret_enum@

    __defined at:__ @reparse.h:71:20@

    __exported by:__ @reparse.h@
-}
ret_enum_ptr :: Ptr.FunPtr (A -> IO Some_enum)
ret_enum_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_9fb7ddbcd84c72f1

foreign import ccall unsafe "hs_bindgen_test_reparse_0638bcad8813a303" hs_bindgen_test_reparse_0638bcad8813a303 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE ret_pointer1_ptr #-}

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @reparse.h:73:20@

    __exported by:__ @reparse.h@
-}
ret_pointer1_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
ret_pointer1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_0638bcad8813a303

foreign import ccall unsafe "hs_bindgen_test_reparse_5d9ced9e4887782b" hs_bindgen_test_reparse_5d9ced9e4887782b ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt))))

{-# NOINLINE ret_pointer2_ptr #-}

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @reparse.h:74:20@

    __exported by:__ @reparse.h@
-}
ret_pointer2_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt)))
ret_pointer2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_5d9ced9e4887782b

foreign import ccall unsafe "hs_bindgen_test_reparse_60e99361ec0a4b5b" hs_bindgen_test_reparse_60e99361ec0a4b5b ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr Void)))

{-# NOINLINE ret_pointer3_ptr #-}

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @reparse.h:75:20@

    __exported by:__ @reparse.h@
-}
ret_pointer3_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr Void))
ret_pointer3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_60e99361ec0a4b5b

foreign import ccall unsafe "hs_bindgen_test_reparse_cca1935605a94051" hs_bindgen_test_reparse_cca1935605a94051 ::
     IO (Ptr.FunPtr (A -> IO FC.CInt))

{-# NOINLINE body1_ptr #-}

{-| __C declaration:__ @body1@

    __defined at:__ @reparse.h:79:5@

    __exported by:__ @reparse.h@
-}
body1_ptr :: Ptr.FunPtr (A -> IO FC.CInt)
body1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_cca1935605a94051

foreign import ccall unsafe "hs_bindgen_test_reparse_a1900daea7e14e95" hs_bindgen_test_reparse_a1900daea7e14e95 ::
     IO (Ptr.FunPtr (IO A))

{-# NOINLINE body2_ptr #-}

{-| __C declaration:__ @body2@

    __defined at:__ @reparse.h:80:3@

    __exported by:__ @reparse.h@
-}
body2_ptr :: Ptr.FunPtr (IO A)
body2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_a1900daea7e14e95

foreign import ccall unsafe "hs_bindgen_test_reparse_c62f1e9d47469a1c" hs_bindgen_test_reparse_c62f1e9d47469a1c ::
     IO (Ptr.FunPtr (A -> (Data.Complex.Complex FC.CFloat) -> IO ()))

{-# NOINLINE args_complex_float_ptr #-}

{-| __C declaration:__ @args_complex_float@

    __defined at:__ @reparse.h:84:6@

    __exported by:__ @reparse.h@
-}
args_complex_float_ptr :: Ptr.FunPtr (A -> (Data.Complex.Complex FC.CFloat) -> IO ())
args_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_c62f1e9d47469a1c

foreign import ccall unsafe "hs_bindgen_test_reparse_b2ef5ed0a8ed0697" hs_bindgen_test_reparse_b2ef5ed0a8ed0697 ::
     IO (Ptr.FunPtr (A -> (Data.Complex.Complex FC.CDouble) -> IO ()))

{-# NOINLINE args_complex_double_ptr #-}

{-| __C declaration:__ @args_complex_double@

    __defined at:__ @reparse.h:85:6@

    __exported by:__ @reparse.h@
-}
args_complex_double_ptr :: Ptr.FunPtr (A -> (Data.Complex.Complex FC.CDouble) -> IO ())
args_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_b2ef5ed0a8ed0697

foreign import ccall unsafe "hs_bindgen_test_reparse_e2cc2aa2dd12852d" hs_bindgen_test_reparse_e2cc2aa2dd12852d ::
     IO (Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CFloat)))

{-# NOINLINE ret_complex_float_ptr #-}

{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @reparse.h:86:17@

    __exported by:__ @reparse.h@
-}
ret_complex_float_ptr :: Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CFloat))
ret_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_e2cc2aa2dd12852d

foreign import ccall unsafe "hs_bindgen_test_reparse_c95961d571f78868" hs_bindgen_test_reparse_c95961d571f78868 ::
     IO (Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CDouble)))

{-# NOINLINE ret_complex_double_ptr #-}

{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @reparse.h:87:17@

    __exported by:__ @reparse.h@
-}
ret_complex_double_ptr :: Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CDouble))
ret_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_c95961d571f78868

foreign import ccall unsafe "hs_bindgen_test_reparse_94c8a2d3574ba283" hs_bindgen_test_reparse_94c8a2d3574ba283 ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE bespoke_args1_ptr #-}

{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @reparse.h:94:6@

    __exported by:__ @reparse.h@
-}
bespoke_args1_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
bespoke_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_94c8a2d3574ba283

foreign import ccall unsafe "hs_bindgen_test_reparse_2165985767a8d24e" hs_bindgen_test_reparse_2165985767a8d24e ::
     IO (Ptr.FunPtr (A -> FC.CSize -> IO ()))

{-# NOINLINE bespoke_args2_ptr #-}

{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @reparse.h:95:6@

    __exported by:__ @reparse.h@
-}
bespoke_args2_ptr :: Ptr.FunPtr (A -> FC.CSize -> IO ())
bespoke_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_2165985767a8d24e

foreign import ccall unsafe "hs_bindgen_test_reparse_7913bf38675bd912" hs_bindgen_test_reparse_7913bf38675bd912 ::
     IO (Ptr.FunPtr (A -> IO FC.CBool))

{-# NOINLINE bespoke_ret1_ptr #-}

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @reparse.h:97:8@

    __exported by:__ @reparse.h@
-}
bespoke_ret1_ptr :: Ptr.FunPtr (A -> IO FC.CBool)
bespoke_ret1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_7913bf38675bd912

foreign import ccall unsafe "hs_bindgen_test_reparse_07c419cb648cdf65" hs_bindgen_test_reparse_07c419cb648cdf65 ::
     IO (Ptr.FunPtr (A -> IO FC.CSize))

{-# NOINLINE bespoke_ret2_ptr #-}

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @reparse.h:98:8@

    __exported by:__ @reparse.h@
-}
bespoke_ret2_ptr :: Ptr.FunPtr (A -> IO FC.CSize)
bespoke_ret2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_07c419cb648cdf65

foreign import ccall unsafe "hs_bindgen_test_reparse_ed19e51bcac06a9e" hs_bindgen_test_reparse_ed19e51bcac06a9e ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ()))

{-# NOINLINE arr_args1_ptr #-}

{-| Arrays

__C declaration:__ @arr_args1@

__defined at:__ @reparse.h:104:6@

__exported by:__ @reparse.h@
-}
arr_args1_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ())
arr_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_ed19e51bcac06a9e

foreign import ccall unsafe "hs_bindgen_test_reparse_de3931a21a8a71fc" hs_bindgen_test_reparse_de3931a21a8a71fc ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ()))

{-# NOINLINE arr_args2_ptr #-}

{-| __C declaration:__ @arr_args2@

    __defined at:__ @reparse.h:105:6@

    __exported by:__ @reparse.h@
-}
arr_args2_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ())
arr_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_de3931a21a8a71fc

foreign import ccall unsafe "hs_bindgen_test_reparse_2c02effa6288a26b" hs_bindgen_test_reparse_2c02effa6288a26b ::
     IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) A) -> IO ()))

{-# NOINLINE arr_args3_ptr #-}

{-| __C declaration:__ @arr_args3@

    __defined at:__ @reparse.h:106:6@

    __exported by:__ @reparse.h@
-}
arr_args3_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) A) -> IO ())
arr_args3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_2c02effa6288a26b

foreign import ccall unsafe "hs_bindgen_test_reparse_2144e300082f115c" hs_bindgen_test_reparse_2144e300082f115c ::
     IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) (Ptr.Ptr A)) -> IO ()))

{-# NOINLINE arr_args4_ptr #-}

{-| __C declaration:__ @arr_args4@

    __defined at:__ @reparse.h:107:6@

    __exported by:__ @reparse.h@
-}
arr_args4_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) (Ptr.Ptr A)) -> IO ())
arr_args4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_2144e300082f115c

foreign import ccall unsafe "hs_bindgen_test_reparse_d1645262a53743f6" hs_bindgen_test_reparse_d1645262a53743f6 ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (IO ())) -> IO ()))

{-# NOINLINE funptr_args1_ptr #-}

{-| Function pointers

__C declaration:__ @funptr_args1@

__defined at:__ @reparse.h:126:6@

__exported by:__ @reparse.h@
-}
funptr_args1_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (IO ())) -> IO ())
funptr_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_d1645262a53743f6

foreign import ccall unsafe "hs_bindgen_test_reparse_d66507630e4e38e3" hs_bindgen_test_reparse_d66507630e4e38e3 ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ()))

{-# NOINLINE funptr_args2_ptr #-}

{-| __C declaration:__ @funptr_args2@

    __defined at:__ @reparse.h:127:6@

    __exported by:__ @reparse.h@
-}
funptr_args2_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ())
funptr_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_d66507630e4e38e3

foreign import ccall unsafe "hs_bindgen_test_reparse_3d7907ab53b617cf" hs_bindgen_test_reparse_3d7907ab53b617cf ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> IO ())) -> IO ()))

{-# NOINLINE funptr_args3_ptr #-}

{-| __C declaration:__ @funptr_args3@

    __defined at:__ @reparse.h:128:6@

    __exported by:__ @reparse.h@
-}
funptr_args3_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> IO ())) -> IO ())
funptr_args3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_3d7907ab53b617cf

foreign import ccall unsafe "hs_bindgen_test_reparse_e4d15a9c3b04292a" hs_bindgen_test_reparse_e4d15a9c3b04292a ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)) -> IO ()))

{-# NOINLINE funptr_args4_ptr #-}

{-| __C declaration:__ @funptr_args4@

    __defined at:__ @reparse.h:129:6@

    __exported by:__ @reparse.h@
-}
funptr_args4_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)) -> IO ())
funptr_args4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_e4d15a9c3b04292a

foreign import ccall unsafe "hs_bindgen_test_reparse_ced7918b6e42102f" hs_bindgen_test_reparse_ced7918b6e42102f ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))) -> IO ()))

{-# NOINLINE funptr_args5_ptr #-}

{-| __C declaration:__ @funptr_args5@

    __defined at:__ @reparse.h:130:6@

    __exported by:__ @reparse.h@
-}
funptr_args5_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))) -> IO ())
funptr_args5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_ced7918b6e42102f

foreign import ccall unsafe "hs_bindgen_test_reparse_c90ec05081ef4e64" hs_bindgen_test_reparse_c90ec05081ef4e64 ::
     IO (Ptr.FunPtr (A -> IO ()))

{-# NOINLINE comments1_ptr #-}

{-| Comments in awkward places

  (Prior to language-c we failed to parse there.)

__C declaration:__ @comments1@

__defined at:__ @reparse.h:144:25@

__exported by:__ @reparse.h@
-}
comments1_ptr :: Ptr.FunPtr (A -> IO ())
comments1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_c90ec05081ef4e64

foreign import ccall unsafe "hs_bindgen_test_reparse_6ac4b42c66a36448" hs_bindgen_test_reparse_6ac4b42c66a36448 ::
     IO (Ptr.FunPtr (A -> FC.CChar -> IO ()))

{-# NOINLINE const_prim_before1_ptr #-}

{-| `const` qualifier

  NOTE: These were not parsed correctly prior to the switch to language-c.

__C declaration:__ @const_prim_before1@

__defined at:__ @reparse.h:179:6@

__exported by:__ @reparse.h@
-}
const_prim_before1_ptr :: Ptr.FunPtr (A -> FC.CChar -> IO ())
const_prim_before1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_6ac4b42c66a36448

foreign import ccall unsafe "hs_bindgen_test_reparse_f98632ef2e69b003" hs_bindgen_test_reparse_f98632ef2e69b003 ::
     IO (Ptr.FunPtr (A -> FC.CSChar -> IO ()))

{-# NOINLINE const_prim_before2_ptr #-}

{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @reparse.h:180:6@

    __exported by:__ @reparse.h@
-}
const_prim_before2_ptr :: Ptr.FunPtr (A -> FC.CSChar -> IO ())
const_prim_before2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_f98632ef2e69b003

foreign import ccall unsafe "hs_bindgen_test_reparse_cc9db1f6a36b8221" hs_bindgen_test_reparse_cc9db1f6a36b8221 ::
     IO (Ptr.FunPtr (A -> FC.CUChar -> IO ()))

{-# NOINLINE const_prim_before3_ptr #-}

{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @reparse.h:181:6@

    __exported by:__ @reparse.h@
-}
const_prim_before3_ptr :: Ptr.FunPtr (A -> FC.CUChar -> IO ())
const_prim_before3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_cc9db1f6a36b8221

foreign import ccall unsafe "hs_bindgen_test_reparse_3e5b7273bf2ecadb" hs_bindgen_test_reparse_3e5b7273bf2ecadb ::
     IO (Ptr.FunPtr (A -> FC.CChar -> IO ()))

{-# NOINLINE const_prim_after1_ptr #-}

{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @reparse.h:182:6@

    __exported by:__ @reparse.h@
-}
const_prim_after1_ptr :: Ptr.FunPtr (A -> FC.CChar -> IO ())
const_prim_after1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_3e5b7273bf2ecadb

foreign import ccall unsafe "hs_bindgen_test_reparse_f9b4beeca8253333" hs_bindgen_test_reparse_f9b4beeca8253333 ::
     IO (Ptr.FunPtr (A -> FC.CSChar -> IO ()))

{-# NOINLINE const_prim_after2_ptr #-}

{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @reparse.h:183:6@

    __exported by:__ @reparse.h@
-}
const_prim_after2_ptr :: Ptr.FunPtr (A -> FC.CSChar -> IO ())
const_prim_after2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_f9b4beeca8253333

foreign import ccall unsafe "hs_bindgen_test_reparse_bf14e2fd88b25311" hs_bindgen_test_reparse_bf14e2fd88b25311 ::
     IO (Ptr.FunPtr (A -> FC.CUChar -> IO ()))

{-# NOINLINE const_prim_after3_ptr #-}

{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @reparse.h:184:6@

    __exported by:__ @reparse.h@
-}
const_prim_after3_ptr :: Ptr.FunPtr (A -> FC.CUChar -> IO ())
const_prim_after3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_bf14e2fd88b25311

foreign import ccall unsafe "hs_bindgen_test_reparse_3649293fcaa1543c" hs_bindgen_test_reparse_3649293fcaa1543c ::
     IO (Ptr.FunPtr (A -> FC.CFloat -> IO ()))

{-# NOINLINE const_withoutSign_before1_ptr #-}

{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @reparse.h:188:6@

    __exported by:__ @reparse.h@
-}
const_withoutSign_before1_ptr :: Ptr.FunPtr (A -> FC.CFloat -> IO ())
const_withoutSign_before1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_3649293fcaa1543c

foreign import ccall unsafe "hs_bindgen_test_reparse_ad5903c28e22dd2c" hs_bindgen_test_reparse_ad5903c28e22dd2c ::
     IO (Ptr.FunPtr (A -> FC.CDouble -> IO ()))

{-# NOINLINE const_withoutSign_before2_ptr #-}

{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @reparse.h:189:6@

    __exported by:__ @reparse.h@
-}
const_withoutSign_before2_ptr :: Ptr.FunPtr (A -> FC.CDouble -> IO ())
const_withoutSign_before2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_ad5903c28e22dd2c

foreign import ccall unsafe "hs_bindgen_test_reparse_e7b9bc011ec1dd8a" hs_bindgen_test_reparse_e7b9bc011ec1dd8a ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE const_withoutSign_before3_ptr #-}

{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @reparse.h:190:6@

    __exported by:__ @reparse.h@
-}
const_withoutSign_before3_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_before3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_e7b9bc011ec1dd8a

foreign import ccall unsafe "hs_bindgen_test_reparse_4fd66b696848dd98" hs_bindgen_test_reparse_4fd66b696848dd98 ::
     IO (Ptr.FunPtr (A -> Some_struct -> IO ()))

{-# NOINLINE const_withoutSign_before4_ptr #-}

{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @reparse.h:191:6@

    __exported by:__ @reparse.h@
-}
const_withoutSign_before4_ptr :: Ptr.FunPtr (A -> Some_struct -> IO ())
const_withoutSign_before4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_4fd66b696848dd98

foreign import ccall unsafe "hs_bindgen_test_reparse_42582e1882927f7e" hs_bindgen_test_reparse_42582e1882927f7e ::
     IO (Ptr.FunPtr (A -> Some_union -> IO ()))

{-# NOINLINE const_withoutSign_before5_ptr #-}

{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @reparse.h:192:6@

    __exported by:__ @reparse.h@
-}
const_withoutSign_before5_ptr :: Ptr.FunPtr (A -> Some_union -> IO ())
const_withoutSign_before5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_42582e1882927f7e

foreign import ccall unsafe "hs_bindgen_test_reparse_b6876e53e4b27a98" hs_bindgen_test_reparse_b6876e53e4b27a98 ::
     IO (Ptr.FunPtr (A -> Some_enum -> IO ()))

{-# NOINLINE const_withoutSign_before6_ptr #-}

{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @reparse.h:193:6@

    __exported by:__ @reparse.h@
-}
const_withoutSign_before6_ptr :: Ptr.FunPtr (A -> Some_enum -> IO ())
const_withoutSign_before6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_b6876e53e4b27a98

foreign import ccall unsafe "hs_bindgen_test_reparse_78763cbecd2b0750" hs_bindgen_test_reparse_78763cbecd2b0750 ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE const_withoutSign_before7_ptr #-}

{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @reparse.h:194:6@

    __exported by:__ @reparse.h@
-}
const_withoutSign_before7_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_before7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_78763cbecd2b0750

foreign import ccall unsafe "hs_bindgen_test_reparse_4098c4a4ccd31d36" hs_bindgen_test_reparse_4098c4a4ccd31d36 ::
     IO (Ptr.FunPtr (A -> FC.CSize -> IO ()))

{-# NOINLINE const_withoutSign_before8_ptr #-}

{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @reparse.h:195:6@

    __exported by:__ @reparse.h@
-}
const_withoutSign_before8_ptr :: Ptr.FunPtr (A -> FC.CSize -> IO ())
const_withoutSign_before8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_4098c4a4ccd31d36

foreign import ccall unsafe "hs_bindgen_test_reparse_e9148eb7b8dac901" hs_bindgen_test_reparse_e9148eb7b8dac901 ::
     IO (Ptr.FunPtr (A -> FC.CFloat -> IO ()))

{-# NOINLINE const_withoutSign_after1_ptr #-}

{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @reparse.h:197:6@

    __exported by:__ @reparse.h@
-}
const_withoutSign_after1_ptr :: Ptr.FunPtr (A -> FC.CFloat -> IO ())
const_withoutSign_after1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_e9148eb7b8dac901

foreign import ccall unsafe "hs_bindgen_test_reparse_8663653d89116be9" hs_bindgen_test_reparse_8663653d89116be9 ::
     IO (Ptr.FunPtr (A -> FC.CDouble -> IO ()))

{-# NOINLINE const_withoutSign_after2_ptr #-}

{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @reparse.h:198:6@

    __exported by:__ @reparse.h@
-}
const_withoutSign_after2_ptr :: Ptr.FunPtr (A -> FC.CDouble -> IO ())
const_withoutSign_after2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_8663653d89116be9

foreign import ccall unsafe "hs_bindgen_test_reparse_136dcba145bf241b" hs_bindgen_test_reparse_136dcba145bf241b ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE const_withoutSign_after3_ptr #-}

{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @reparse.h:199:6@

    __exported by:__ @reparse.h@
-}
const_withoutSign_after3_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_after3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_136dcba145bf241b

foreign import ccall unsafe "hs_bindgen_test_reparse_380e01acce794cab" hs_bindgen_test_reparse_380e01acce794cab ::
     IO (Ptr.FunPtr (A -> Some_struct -> IO ()))

{-# NOINLINE const_withoutSign_after4_ptr #-}

{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @reparse.h:200:6@

    __exported by:__ @reparse.h@
-}
const_withoutSign_after4_ptr :: Ptr.FunPtr (A -> Some_struct -> IO ())
const_withoutSign_after4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_380e01acce794cab

foreign import ccall unsafe "hs_bindgen_test_reparse_af0d84d0757f6c2c" hs_bindgen_test_reparse_af0d84d0757f6c2c ::
     IO (Ptr.FunPtr (A -> Some_union -> IO ()))

{-# NOINLINE const_withoutSign_after5_ptr #-}

{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @reparse.h:201:6@

    __exported by:__ @reparse.h@
-}
const_withoutSign_after5_ptr :: Ptr.FunPtr (A -> Some_union -> IO ())
const_withoutSign_after5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_af0d84d0757f6c2c

foreign import ccall unsafe "hs_bindgen_test_reparse_df92501d07bf6c5f" hs_bindgen_test_reparse_df92501d07bf6c5f ::
     IO (Ptr.FunPtr (A -> Some_enum -> IO ()))

{-# NOINLINE const_withoutSign_after6_ptr #-}

{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @reparse.h:202:6@

    __exported by:__ @reparse.h@
-}
const_withoutSign_after6_ptr :: Ptr.FunPtr (A -> Some_enum -> IO ())
const_withoutSign_after6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_df92501d07bf6c5f

foreign import ccall unsafe "hs_bindgen_test_reparse_b41148ca40ec8eb5" hs_bindgen_test_reparse_b41148ca40ec8eb5 ::
     IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE const_withoutSign_after7_ptr #-}

{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @reparse.h:203:6@

    __exported by:__ @reparse.h@
-}
const_withoutSign_after7_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_after7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_b41148ca40ec8eb5

foreign import ccall unsafe "hs_bindgen_test_reparse_560c9dfdb530548b" hs_bindgen_test_reparse_560c9dfdb530548b ::
     IO (Ptr.FunPtr (A -> FC.CSize -> IO ()))

{-# NOINLINE const_withoutSign_after8_ptr #-}

{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @reparse.h:204:6@

    __exported by:__ @reparse.h@
-}
const_withoutSign_after8_ptr :: Ptr.FunPtr (A -> FC.CSize -> IO ())
const_withoutSign_after8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_560c9dfdb530548b

foreign import ccall unsafe "hs_bindgen_test_reparse_a34d16c099748839" hs_bindgen_test_reparse_a34d16c099748839 ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args1_ptr #-}

{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @reparse.h:208:6@

    __exported by:__ @reparse.h@
-}
const_pointers_args1_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_a34d16c099748839

foreign import ccall unsafe "hs_bindgen_test_reparse_45235edaf5c3b599" hs_bindgen_test_reparse_45235edaf5c3b599 ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args2_ptr #-}

{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @reparse.h:209:6@

    __exported by:__ @reparse.h@
-}
const_pointers_args2_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_45235edaf5c3b599

foreign import ccall unsafe "hs_bindgen_test_reparse_3dbcf1c7202f2878" hs_bindgen_test_reparse_3dbcf1c7202f2878 ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args3_ptr #-}

{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @reparse.h:210:6@

    __exported by:__ @reparse.h@
-}
const_pointers_args3_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_3dbcf1c7202f2878

foreign import ccall unsafe "hs_bindgen_test_reparse_a6624f6cc0a062af" hs_bindgen_test_reparse_a6624f6cc0a062af ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args4_ptr #-}

{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @reparse.h:211:6@

    __exported by:__ @reparse.h@
-}
const_pointers_args4_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_a6624f6cc0a062af

foreign import ccall unsafe "hs_bindgen_test_reparse_c5f3253c57910315" hs_bindgen_test_reparse_c5f3253c57910315 ::
     IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args5_ptr #-}

{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @reparse.h:212:6@

    __exported by:__ @reparse.h@
-}
const_pointers_args5_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_c5f3253c57910315

foreign import ccall unsafe "hs_bindgen_test_reparse_1990ded85ea3850d" hs_bindgen_test_reparse_1990ded85ea3850d ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret1_ptr #-}

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @reparse.h:214:19@

    __exported by:__ @reparse.h@
-}
const_pointers_ret1_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_1990ded85ea3850d

foreign import ccall unsafe "hs_bindgen_test_reparse_627cc570c3ca7d19" hs_bindgen_test_reparse_627cc570c3ca7d19 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret2_ptr #-}

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @reparse.h:215:19@

    __exported by:__ @reparse.h@
-}
const_pointers_ret2_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_627cc570c3ca7d19

foreign import ccall unsafe "hs_bindgen_test_reparse_2f449708b5a275b1" hs_bindgen_test_reparse_2f449708b5a275b1 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret3_ptr #-}

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @reparse.h:216:19@

    __exported by:__ @reparse.h@
-}
const_pointers_ret3_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_2f449708b5a275b1

foreign import ccall unsafe "hs_bindgen_test_reparse_67662618cd011c8a" hs_bindgen_test_reparse_67662618cd011c8a ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret4_ptr #-}

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @reparse.h:217:19@

    __exported by:__ @reparse.h@
-}
const_pointers_ret4_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_67662618cd011c8a

foreign import ccall unsafe "hs_bindgen_test_reparse_fcafd9f8ac329995" hs_bindgen_test_reparse_fcafd9f8ac329995 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret5_ptr #-}

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @reparse.h:218:19@

    __exported by:__ @reparse.h@
-}
const_pointers_ret5_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_fcafd9f8ac329995

foreign import ccall unsafe "hs_bindgen_test_reparse_6928906fc9a88dfc" hs_bindgen_test_reparse_6928906fc9a88dfc ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ()))

{-# NOINLINE const_array_elem1_ptr #-}

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @reparse.h:246:6@

    __exported by:__ @reparse.h@
-}
const_array_elem1_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ())
const_array_elem1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_6928906fc9a88dfc

foreign import ccall unsafe "hs_bindgen_test_reparse_625a37e9c030891a" hs_bindgen_test_reparse_625a37e9c030891a ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ()))

{-# NOINLINE const_array_elem2_ptr #-}

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @reparse.h:247:6@

    __exported by:__ @reparse.h@
-}
const_array_elem2_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ())
const_array_elem2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_625a37e9c030891a

foreign import ccall unsafe "hs_bindgen_test_reparse_5e23f87114cf51fb" hs_bindgen_test_reparse_5e23f87114cf51fb ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ()))

{-# NOINLINE const_array_elem3_ptr #-}

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @reparse.h:248:6@

    __exported by:__ @reparse.h@
-}
const_array_elem3_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ())
const_array_elem3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_5e23f87114cf51fb

foreign import ccall unsafe "hs_bindgen_test_reparse_d50620a002265139" hs_bindgen_test_reparse_d50620a002265139 ::
     IO (Ptr.FunPtr (IO A))

{-# NOINLINE noParams1_ptr #-}

{-| Other examples we reparsed /incorrectly/ before language-c

__C declaration:__ @noParams1@

__defined at:__ @reparse.h:256:3@

__exported by:__ @reparse.h@
-}
noParams1_ptr :: Ptr.FunPtr (IO A)
noParams1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_d50620a002265139

foreign import ccall unsafe "hs_bindgen_test_reparse_03b0e24786b82ad5" hs_bindgen_test_reparse_03b0e24786b82ad5 ::
     IO (Ptr.FunPtr (IO A))

{-# NOINLINE noParams2_ptr #-}

{-| __C declaration:__ @noParams2@

    __defined at:__ @reparse.h:257:3@

    __exported by:__ @reparse.h@
-}
noParams2_ptr :: Ptr.FunPtr (IO A)
noParams2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_03b0e24786b82ad5

foreign import ccall unsafe "hs_bindgen_test_reparse_36508fd99a0556c5" hs_bindgen_test_reparse_36508fd99a0556c5 ::
     IO (Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ()))

{-# NOINLINE noParams3_ptr #-}

{-| __C declaration:__ @noParams3@

    __defined at:__ @reparse.h:258:6@

    __exported by:__ @reparse.h@
-}
noParams3_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ())
noParams3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_36508fd99a0556c5

foreign import ccall unsafe "hs_bindgen_test_reparse_6f83a48dd177c25f" hs_bindgen_test_reparse_6f83a48dd177c25f ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO ()))))

{-# NOINLINE funptr_ret1_ptr #-}

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @reparse.h:262:8@

    __exported by:__ @reparse.h@
-}
funptr_ret1_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO ())))
funptr_ret1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_6f83a48dd177c25f

foreign import ccall unsafe "hs_bindgen_test_reparse_f12efafd1525ef7f" hs_bindgen_test_reparse_f12efafd1525ef7f ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO FC.CInt))))

{-# NOINLINE funptr_ret2_ptr #-}

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @reparse.h:263:8@

    __exported by:__ @reparse.h@
-}
funptr_ret2_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO FC.CInt)))
funptr_ret2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_f12efafd1525ef7f

foreign import ccall unsafe "hs_bindgen_test_reparse_b00baa5b9708b9e7" hs_bindgen_test_reparse_b00baa5b9708b9e7 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> IO ()))))

{-# NOINLINE funptr_ret3_ptr #-}

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @reparse.h:264:8@

    __exported by:__ @reparse.h@
-}
funptr_ret3_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> IO ())))
funptr_ret3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_b00baa5b9708b9e7

foreign import ccall unsafe "hs_bindgen_test_reparse_c51872479ceff42e" hs_bindgen_test_reparse_c51872479ceff42e ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar))))

{-# NOINLINE funptr_ret4_ptr #-}

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @reparse.h:265:8@

    __exported by:__ @reparse.h@
-}
funptr_ret4_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)))
funptr_ret4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_c51872479ceff42e

foreign import ccall unsafe "hs_bindgen_test_reparse_3b9b9924b4b4d7ea" hs_bindgen_test_reparse_3b9b9924b4b4d7ea ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret5_ptr #-}

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @reparse.h:269:20@

    __exported by:__ @reparse.h@
-}
funptr_ret5_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_3b9b9924b4b4d7ea

foreign import ccall unsafe "hs_bindgen_test_reparse_3df5ab4b0b306845" hs_bindgen_test_reparse_3df5ab4b0b306845 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret6_ptr #-}

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @reparse.h:270:20@

    __exported by:__ @reparse.h@
-}
funptr_ret6_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_3df5ab4b0b306845

foreign import ccall unsafe "hs_bindgen_test_reparse_2ac4454d93b6f04a" hs_bindgen_test_reparse_2ac4454d93b6f04a ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret7_ptr #-}

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @reparse.h:271:20@

    __exported by:__ @reparse.h@
-}
funptr_ret7_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_2ac4454d93b6f04a

foreign import ccall unsafe "hs_bindgen_test_reparse_411c5128f18364b3" hs_bindgen_test_reparse_411c5128f18364b3 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret8_ptr #-}

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @reparse.h:272:20@

    __exported by:__ @reparse.h@
-}
funptr_ret8_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_411c5128f18364b3

foreign import ccall unsafe "hs_bindgen_test_reparse_693a8d16e17d0cdc" hs_bindgen_test_reparse_693a8d16e17d0cdc ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret9_ptr #-}

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @reparse.h:273:20@

    __exported by:__ @reparse.h@
-}
funptr_ret9_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_693a8d16e17d0cdc

foreign import ccall unsafe "hs_bindgen_test_reparse_9d2da81bbfe49ab6" hs_bindgen_test_reparse_9d2da81bbfe49ab6 ::
     IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret10_ptr #-}

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @reparse.h:274:20@

    __exported by:__ @reparse.h@
-}
funptr_ret10_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_9d2da81bbfe49ab6
