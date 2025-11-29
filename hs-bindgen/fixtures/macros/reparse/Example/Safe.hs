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
  [ "#include <macros/reparse.h>"
  , "void hs_bindgen_test_macrosreparse_38cdab2aec8f0b35 ("
  , "  A arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  args_char1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_4906a892366301df ("
  , "  A arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  args_char2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_83d97d5c7bf83a03 ("
  , "  A arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  args_char3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_868a8dddc84b4da8 ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  args_short1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_1ddf9aea0d730f1d ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  args_short2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_c1b933437c0f32c4 ("
  , "  A arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  args_short3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_17c2707bc6abfc8c ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  args_int1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_0945695296eeb9c7 ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  args_int2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_2967af8d3fa721db ("
  , "  A arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  args_int3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_f2db8c0b8c3003f7 ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  args_long1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_5cf50677cf422bf2 ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  args_long2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_3b383a08327bc269 ("
  , "  A arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  args_long3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_fbcd3bb7c6710aea ("
  , "  A arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  args_float(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_e492f4a345ad588e ("
  , "  A arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  args_double(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_3703a3044e58bc48 ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  args_bool1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_d953ee5009ebbb8f ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  args_struct(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_1cae6b3a38a586f4 ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  args_union(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_5868540d5d95eef6 ("
  , "  A arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  args_enum(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_5b890b08a3e0cb60 ("
  , "  A arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  args_pointer1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_924ccb64763c1858 ("
  , "  A arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  args_pointer2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_cb2efbfa1ad590f2 ("
  , "  A arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  args_pointer3(arg1, arg2);"
  , "}"
  , "A hs_bindgen_test_macrosreparse_b552adefba6b80c1 (void)"
  , "{"
  , "  return ret_A();"
  , "}"
  , "char hs_bindgen_test_macrosreparse_f50b617faa9e2ff1 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char1(arg1);"
  , "}"
  , "signed char hs_bindgen_test_macrosreparse_e89d3e48cbe94396 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char2(arg1);"
  , "}"
  , "unsigned char hs_bindgen_test_macrosreparse_3ee91cd8397a0730 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_char3(arg1);"
  , "}"
  , "signed short hs_bindgen_test_macrosreparse_43a2914bd218640d ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short1(arg1);"
  , "}"
  , "signed short hs_bindgen_test_macrosreparse_de4a029f63817646 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short2(arg1);"
  , "}"
  , "unsigned short hs_bindgen_test_macrosreparse_e70abba070f8d0f6 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_short3(arg1);"
  , "}"
  , "signed int hs_bindgen_test_macrosreparse_2d38cec255cac53f ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int1(arg1);"
  , "}"
  , "signed int hs_bindgen_test_macrosreparse_67abdf53056eab91 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int2(arg1);"
  , "}"
  , "unsigned int hs_bindgen_test_macrosreparse_a4df979fadc945c2 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_int3(arg1);"
  , "}"
  , "signed long hs_bindgen_test_macrosreparse_33830e42954d81ca ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long1(arg1);"
  , "}"
  , "signed long hs_bindgen_test_macrosreparse_f80fd9155f754907 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long2(arg1);"
  , "}"
  , "unsigned long hs_bindgen_test_macrosreparse_ae132dec45c19b2a ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_long3(arg1);"
  , "}"
  , "float hs_bindgen_test_macrosreparse_1f1fc441dc5ddd89 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_float(arg1);"
  , "}"
  , "double hs_bindgen_test_macrosreparse_5c9fbd8e4ff3cbf5 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_double(arg1);"
  , "}"
  , "_Bool hs_bindgen_test_macrosreparse_3a5312ce5ff941ab ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_bool1(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_43a7a83473dc7a5c ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_struct(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_51453da4afc80213 ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_union(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_test_macrosreparse_02b1567b400309a1 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_enum(arg1);"
  , "}"
  , "signed int *hs_bindgen_test_macrosreparse_3231b38ebebfbf1c ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer1(arg1);"
  , "}"
  , "signed int **hs_bindgen_test_macrosreparse_3521a329abb651a4 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer2(arg1);"
  , "}"
  , "void *hs_bindgen_test_macrosreparse_53dc89073adcdfb2 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return ret_pointer3(arg1);"
  , "}"
  , "signed int hs_bindgen_test_macrosreparse_863e4bdf9850e2bd ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return body1(arg1);"
  , "}"
  , "A hs_bindgen_test_macrosreparse_0707a17037eea4b6 (void)"
  , "{"
  , "  return body2();"
  , "}"
  , "void hs_bindgen_test_macrosreparse_6b8791901f0ec2c5 ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  args_complex_float(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_2c1ca33447b09ca5 ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  args_complex_double(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_c9e9f8e3ce7e2755 ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_complex_float(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_7651ea67dfeef65f ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = ret_complex_double(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_3872304264c357fc ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  bespoke_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_a16b3de52a6a975f ("
  , "  A arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  bespoke_args2(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_test_macrosreparse_c489fa9fcd021f8d ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return bespoke_ret1(arg1);"
  , "}"
  , "size_t hs_bindgen_test_macrosreparse_0dfae5ef18aba0aa ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return bespoke_ret2(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_6761304a61691f36 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  arr_args1(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_f68d1f72c050a09b ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  arr_args2(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_586847d3e2e89cfa ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  arr_args3(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_2d5c02acf96a3cb8 ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  arr_args4(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_62a1a532cfb69f46 ("
  , "  A arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  funptr_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_2280f18ae8f5ddaf ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  funptr_args2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_7a1f36cfa8fa4185 ("
  , "  A arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  funptr_args3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_36f550abfb2c8ab4 ("
  , "  A arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  funptr_args4(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_8c71c92f3eedc6f3 ("
  , "  A arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  funptr_args5(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_7128856d4c9cac61 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  comments1(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_e701bca4bffd3d3c ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  const_prim_before1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_03f00e051e744e48 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  const_prim_before2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_8925c09d7e36b05d ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  const_prim_before3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_78dbe2c956349997 ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  const_prim_after1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_4c9b9f354288e005 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  const_prim_after2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_632611e6dfa5514e ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  const_prim_after3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_1749816a667415f7 ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_6ff98ed3a86886ed ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_1020f6ddadd957eb ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_f2f88bfc3e1218a8 ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before4(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_789fb96f1b647675 ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before5(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_8d835e1bb2382144 ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before6(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_1f275117b5f5f038 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before7(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_2b20e2ac7ad6c2e1 ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_before8(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_a90804139c7595db ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_b2b95aed088947ab ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_7d70ce767617908a ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_406a9d6fe3328a86 ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after4(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_6c39b377bad28ba7 ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after5(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_38e4aec83b37dda3 ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after6(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_60ae95299afc4d81 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after7(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_714d97d9f36136d0 ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  const_withoutSign_after8(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_c4ed87ba846b245a ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  const_pointers_args1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_1aa373fdf7ea9fac ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  const_pointers_args2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_85903dc37e41beda ("
  , "  A arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_bcdef8b48457fb1a ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args4(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_cbd8be012c2f3452 ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  const_pointers_args5(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_test_macrosreparse_f1898043698d3c55 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret1(arg1);"
  , "}"
  , "signed int const *hs_bindgen_test_macrosreparse_e96464714653a087 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret2(arg1);"
  , "}"
  , "signed int *const hs_bindgen_test_macrosreparse_8175cd3886009531 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret3(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_test_macrosreparse_6c20ed246ede9502 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret4(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_test_macrosreparse_ca6f7733fdbad1aa ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return const_pointers_ret5(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_bde8ef87aa1ab4c6 ("
  , "  A const *arg1"
  , ")"
  , "{"
  , "  const_array_elem1(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_8c3567010d7f7cd2 ("
  , "  A const **arg1"
  , ")"
  , "{"
  , "  const_array_elem2(arg1);"
  , "}"
  , "void hs_bindgen_test_macrosreparse_5c12d929414f4a7f ("
  , "  A *const *arg1"
  , ")"
  , "{"
  , "  const_array_elem3(arg1);"
  , "}"
  , "A hs_bindgen_test_macrosreparse_a320abfd17914a92 (void)"
  , "{"
  , "  return noParams1();"
  , "}"
  , "A hs_bindgen_test_macrosreparse_c75bd24a30568905 (void)"
  , "{"
  , "  return noParams2();"
  , "}"
  , "void hs_bindgen_test_macrosreparse_f16e400d45e1912c ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  noParams3(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_test_macrosreparse_4c4a4f16117ded79 ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return funptr_ret1(arg1);"
  , "}"
  , "signed int (*hs_bindgen_test_macrosreparse_21d495427b451d5d ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return funptr_ret2(arg1);"
  , "}"
  , "void (*hs_bindgen_test_macrosreparse_e8582d910744ab54 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return funptr_ret3(arg1);"
  , "}"
  , "char (*hs_bindgen_test_macrosreparse_707a8f00c67149f4 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret4(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_test_macrosreparse_e30ec7432e4cd49e ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret5(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_test_macrosreparse_c521dcfb4fc28c16 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret6(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_test_macrosreparse_f5462b96b54cdbc6 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret7(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_test_macrosreparse_d85483d2a7b00e9e ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret8(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_test_macrosreparse_cad2e369c69d3d8a ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return funptr_ret9(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_test_macrosreparse_33588a22e52e064f ("
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

__unique:__ @Example_Safe_args_char1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_38cdab2aec8f0b35" args_char1 ::
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

    __unique:__ @Example_Safe_args_char2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_4906a892366301df" args_char2 ::
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

    __unique:__ @Example_Safe_args_char3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_83d97d5c7bf83a03" args_char3 ::
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

    __unique:__ @Example_Safe_args_short1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_868a8dddc84b4da8" args_short1 ::
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

    __unique:__ @Example_Safe_args_short2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_1ddf9aea0d730f1d" args_short2 ::
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

    __unique:__ @Example_Safe_args_short3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_c1b933437c0f32c4" args_short3 ::
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

    __unique:__ @Example_Safe_args_int1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_17c2707bc6abfc8c" args_int1 ::
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

    __unique:__ @Example_Safe_args_int2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_0945695296eeb9c7" args_int2 ::
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

    __unique:__ @Example_Safe_args_int3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_2967af8d3fa721db" args_int3 ::
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

    __unique:__ @Example_Safe_args_long1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_f2db8c0b8c3003f7" args_long1 ::
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

    __unique:__ @Example_Safe_args_long2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_5cf50677cf422bf2" args_long2 ::
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

    __unique:__ @Example_Safe_args_long3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_3b383a08327bc269" args_long3 ::
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

    __unique:__ @Example_Safe_args_float@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_fbcd3bb7c6710aea" args_float ::
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

    __unique:__ @Example_Safe_args_double@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_e492f4a345ad588e" args_double ::
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

    __unique:__ @Example_Safe_args_bool1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_3703a3044e58bc48" args_bool1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| Pointer-based API for 'args_struct'

__unique:__ @Example_Safe_args_struct@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_d953ee5009ebbb8f" args_struct_wrapper ::
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

__unique:__ @Example_Safe_args_union@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_1cae6b3a38a586f4" args_union_wrapper ::
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

    __unique:__ @Example_Safe_args_enum@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_5868540d5d95eef6" args_enum ::
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

    __unique:__ @Example_Safe_args_pointer1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_5b890b08a3e0cb60" args_pointer1 ::
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

    __unique:__ @Example_Safe_args_pointer2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_924ccb64763c1858" args_pointer2 ::
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

    __unique:__ @Example_Safe_args_pointer3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_cb2efbfa1ad590f2" args_pointer3 ::
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

    __unique:__ @Example_Safe_ret_A@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_b552adefba6b80c1" ret_A ::
     IO A

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h:49:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_ret_char1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_f50b617faa9e2ff1" ret_char1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CChar

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h:50:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_ret_char2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_e89d3e48cbe94396" ret_char2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CSChar

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h:51:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_ret_char3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_3ee91cd8397a0730" ret_char3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CUChar

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h:53:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_ret_short1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_43a2914bd218640d" ret_short1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CShort

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h:54:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_ret_short2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_de4a029f63817646" ret_short2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CShort

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h:55:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_ret_short3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_e70abba070f8d0f6" ret_short3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CUShort

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h:57:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_ret_int1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_2d38cec255cac53f" ret_int1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h:58:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_ret_int2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_67abdf53056eab91" ret_int2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h:59:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_ret_int3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_a4df979fadc945c2" ret_int3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CUInt

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h:61:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_ret_long1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_33830e42954d81ca" ret_long1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CLong

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h:62:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_ret_long2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_f80fd9155f754907" ret_long2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CLong

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h:63:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_ret_long3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_ae132dec45c19b2a" ret_long3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CULong

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h:65:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_ret_float@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_1f1fc441dc5ddd89" ret_float ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CFloat

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h:66:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_ret_double@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_5c9fbd8e4ff3cbf5" ret_double ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CDouble

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h:67:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_ret_bool1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_3a5312ce5ff941ab" ret_bool1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CBool

{-| Pointer-based API for 'ret_struct'

__unique:__ @Example_Safe_ret_struct@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_43a7a83473dc7a5c" ret_struct_wrapper ::
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

__unique:__ @Example_Safe_ret_union@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_51453da4afc80213" ret_union_wrapper ::
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

    __unique:__ @Example_Safe_ret_enum@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_02b1567b400309a1" ret_enum ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO Some_enum

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h:73:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_ret_pointer1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_3231b38ebebfbf1c" ret_pointer1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h:74:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_ret_pointer2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_3521a329abb651a4" ret_pointer2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h:75:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_ret_pointer3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_53dc89073adcdfb2" ret_pointer3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h:79:5@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_body1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_863e4bdf9850e2bd" body1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h:80:3@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_body2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_0707a17037eea4b6" body2 ::
     IO A

{-| Pointer-based API for 'args_complex_float'

__unique:__ @Example_Safe_args_complex_float@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_6b8791901f0ec2c5" args_complex_float_wrapper ::
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

__unique:__ @Example_Safe_args_complex_double@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_2c1ca33447b09ca5" args_complex_double_wrapper ::
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

__unique:__ @Example_Safe_ret_complex_float@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_c9e9f8e3ce7e2755" ret_complex_float_wrapper ::
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

__unique:__ @Example_Safe_ret_complex_double@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_7651ea67dfeef65f" ret_complex_double_wrapper ::
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

    __unique:__ @Example_Safe_bespoke_args1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_3872304264c357fc" bespoke_args1 ::
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

    __unique:__ @Example_Safe_bespoke_args2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_a16b3de52a6a975f" bespoke_args2 ::
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

    __unique:__ @Example_Safe_bespoke_ret1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_c489fa9fcd021f8d" bespoke_ret1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CBool

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h:98:8@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_bespoke_ret2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_0dfae5ef18aba0aa" bespoke_ret2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO HsBindgen.Runtime.Prelude.CSize

{-| Arrays

__C declaration:__ @arr_args1@

__defined at:__ @macros\/reparse.h:104:6@

__exported by:__ @macros\/reparse.h@

__unique:__ @Example_Safe_arr_args1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_6761304a61691f36" arr_args1 ::
     Ptr.Ptr A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h:105:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_arr_args2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_f68d1f72c050a09b" arr_args2 ::
     Ptr.Ptr (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h:106:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_arr_args3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_586847d3e2e89cfa" arr_args3 ::
     Ptr.Ptr A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h:107:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_arr_args4@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_2d5c02acf96a3cb8" arr_args4 ::
     Ptr.Ptr (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| Function pointers

__C declaration:__ @funptr_args1@

__defined at:__ @macros\/reparse.h:126:6@

__exported by:__ @macros\/reparse.h@

__unique:__ @Example_Safe_funptr_args1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_62a1a532cfb69f46" funptr_args1 ::
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

    __unique:__ @Example_Safe_funptr_args2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_2280f18ae8f5ddaf" funptr_args2 ::
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

    __unique:__ @Example_Safe_funptr_args3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_7a1f36cfa8fa4185" funptr_args3 ::
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

    __unique:__ @Example_Safe_funptr_args4@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_36f550abfb2c8ab4" funptr_args4 ::
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

    __unique:__ @Example_Safe_funptr_args5@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_8c71c92f3eedc6f3" funptr_args5 ::
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

__unique:__ @Example_Safe_comments1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_7128856d4c9cac61" comments1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| `const` qualifier

  NOTE: These were not parsed correctly prior to the switch to language-c.

__C declaration:__ @const_prim_before1@

__defined at:__ @macros\/reparse.h:179:6@

__exported by:__ @macros\/reparse.h@

__unique:__ @Example_Safe_const_prim_before1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_e701bca4bffd3d3c" const_prim_before1 ::
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

    __unique:__ @Example_Safe_const_prim_before2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_03f00e051e744e48" const_prim_before2 ::
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

    __unique:__ @Example_Safe_const_prim_before3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_8925c09d7e36b05d" const_prim_before3 ::
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

    __unique:__ @Example_Safe_const_prim_after1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_78dbe2c956349997" const_prim_after1 ::
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

    __unique:__ @Example_Safe_const_prim_after2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_4c9b9f354288e005" const_prim_after2 ::
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

    __unique:__ @Example_Safe_const_prim_after3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_632611e6dfa5514e" const_prim_after3 ::
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

    __unique:__ @Example_Safe_const_withoutSign_before1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_1749816a667415f7" const_withoutSign_before1 ::
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

    __unique:__ @Example_Safe_const_withoutSign_before2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_6ff98ed3a86886ed" const_withoutSign_before2 ::
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

    __unique:__ @Example_Safe_const_withoutSign_before3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_1020f6ddadd957eb" const_withoutSign_before3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| Pointer-based API for 'const_withoutSign_before4'

__unique:__ @Example_Safe_const_withoutSign_before4@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_f2f88bfc3e1218a8" const_withoutSign_before4_wrapper ::
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

__unique:__ @Example_Safe_const_withoutSign_before5@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_789fb96f1b647675" const_withoutSign_before5_wrapper ::
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

    __unique:__ @Example_Safe_const_withoutSign_before6@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_8d835e1bb2382144" const_withoutSign_before6 ::
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

    __unique:__ @Example_Safe_const_withoutSign_before7@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_1f275117b5f5f038" const_withoutSign_before7 ::
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

    __unique:__ @Example_Safe_const_withoutSign_before8@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_2b20e2ac7ad6c2e1" const_withoutSign_before8 ::
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

    __unique:__ @Example_Safe_const_withoutSign_after1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_a90804139c7595db" const_withoutSign_after1 ::
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

    __unique:__ @Example_Safe_const_withoutSign_after2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_b2b95aed088947ab" const_withoutSign_after2 ::
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

    __unique:__ @Example_Safe_const_withoutSign_after3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_7d70ce767617908a" const_withoutSign_after3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| Pointer-based API for 'const_withoutSign_after4'

__unique:__ @Example_Safe_const_withoutSign_after4@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_406a9d6fe3328a86" const_withoutSign_after4_wrapper ::
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

__unique:__ @Example_Safe_const_withoutSign_after5@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_6c39b377bad28ba7" const_withoutSign_after5_wrapper ::
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

    __unique:__ @Example_Safe_const_withoutSign_after6@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_38e4aec83b37dda3" const_withoutSign_after6 ::
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

    __unique:__ @Example_Safe_const_withoutSign_after7@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_60ae95299afc4d81" const_withoutSign_after7 ::
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

    __unique:__ @Example_Safe_const_withoutSign_after8@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_714d97d9f36136d0" const_withoutSign_after8 ::
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

    __unique:__ @Example_Safe_const_pointers_args1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_c4ed87ba846b245a" const_pointers_args1 ::
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

    __unique:__ @Example_Safe_const_pointers_args2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_1aa373fdf7ea9fac" const_pointers_args2 ::
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

    __unique:__ @Example_Safe_const_pointers_args3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_85903dc37e41beda" const_pointers_args3 ::
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

    __unique:__ @Example_Safe_const_pointers_args4@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_bcdef8b48457fb1a" const_pointers_args4 ::
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

    __unique:__ @Example_Safe_const_pointers_args5@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_cbd8be012c2f3452" const_pointers_args5 ::
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

    __unique:__ @Example_Safe_const_pointers_ret1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_f1898043698d3c55" const_pointers_ret1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h:215:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_const_pointers_ret2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_e96464714653a087" const_pointers_ret2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h:216:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_const_pointers_ret3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_8175cd3886009531" const_pointers_ret3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h:217:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_const_pointers_ret4@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_6c20ed246ede9502" const_pointers_ret4 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h:218:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_const_pointers_ret5@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_ca6f7733fdbad1aa" const_pointers_ret5 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| Pointer-based API for 'const_array_elem1'

__unique:__ @Example_Safe_const_array_elem1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_bde8ef87aa1ab4c6" const_array_elem1_wrapper ::
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

    __unique:__ @Example_Safe_const_array_elem2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_8c3567010d7f7cd2" const_array_elem2 ::
     Ptr.Ptr (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| Pointer-based API for 'const_array_elem3'

__unique:__ @Example_Safe_const_array_elem3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_5c12d929414f4a7f" const_array_elem3_wrapper ::
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

__unique:__ @Example_Safe_noParams1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_a320abfd17914a92" noParams1 ::
     IO A

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h:257:3@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_noParams2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_c75bd24a30568905" noParams2 ::
     IO A

{-| __C declaration:__ @noParams3@

    __defined at:__ @macros\/reparse.h:258:6@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_noParams3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_f16e400d45e1912c" noParams3 ::
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

    __unique:__ @Example_Safe_funptr_ret1@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_4c4a4f16117ded79" funptr_ret1 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (IO ()))

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h:263:8@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_funptr_ret2@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_21d495427b451d5d" funptr_ret2 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (IO FC.CInt))

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h:264:8@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_funptr_ret3@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_e8582d910744ab54" funptr_ret3 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> IO ()))

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h:265:8@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_funptr_ret4@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_707a8f00c67149f4" funptr_ret4 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar))

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h:269:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_funptr_ret5@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_e30ec7432e4cd49e" funptr_ret5 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h:270:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_funptr_ret6@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_c521dcfb4fc28c16" funptr_ret6 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h:271:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_funptr_ret7@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_f5462b96b54cdbc6" funptr_ret7 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h:272:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_funptr_ret8@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_d85483d2a7b00e9e" funptr_ret8 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h:273:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_funptr_ret9@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_cad2e369c69d3d8a" funptr_ret9 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h:274:20@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @Example_Safe_funptr_ret10@
-}
foreign import ccall safe "hs_bindgen_test_macrosreparse_33588a22e52e064f" funptr_ret10 ::
     A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))
