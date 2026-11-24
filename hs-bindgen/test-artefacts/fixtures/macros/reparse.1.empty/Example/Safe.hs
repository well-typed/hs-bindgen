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
  , "void hs_bindgen_54963e3aae2388b8 ("
  , "  signed int arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  (args_char1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_62073552f76ff03c ("
  , "  signed int arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  (args_char2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_cecb42184f8ea235 ("
  , "  signed int arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  (args_char3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3739d770e0777144 ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_99f7078b0a509e25 ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d227b14017c51974 ("
  , "  signed int arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  (args_short3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9af1ce0326871cd8 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_28933d12947391da ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_5ccd0c1c9031ffdd ("
  , "  signed int arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  (args_int3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f35cd6740be21567 ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_39236a3eb32d14a9 ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f9ec6d5b93569e48 ("
  , "  signed int arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  (args_long3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d537620e6ef3b422 ("
  , "  signed int arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  (args_float)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b36fb283c88dc5c6 ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  (args_double)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8018fdb8935528bd ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (args_bool1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a499a7905953ab61 ("
  , "  signed int arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  (args_struct)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_85dea9d33d9bab4e ("
  , "  signed int arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  (args_union)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_09cf48b070873cfc ("
  , "  signed int arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  (args_enum)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c72bb2a5ebfa749a ("
  , "  signed int arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  (args_pointer1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9aa2fcd4bbd7f16e ("
  , "  signed int arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  (args_pointer2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_61f9226bc89b31df ("
  , "  signed int arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  (args_pointer3)(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_8831a24bc16d7d8d (void)"
  , "{"
  , "  return (ret_A)();"
  , "}"
  , "char hs_bindgen_5633b28b9d25f0f5 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char1)(arg1);"
  , "}"
  , "signed char hs_bindgen_2cedc94dfbbedc0c ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char2)(arg1);"
  , "}"
  , "unsigned char hs_bindgen_500de2a3f906016c ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char3)(arg1);"
  , "}"
  , "signed short hs_bindgen_360cbf2f994cc28e ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short1)(arg1);"
  , "}"
  , "signed short hs_bindgen_8b1d3f34754d7fea ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short2)(arg1);"
  , "}"
  , "unsigned short hs_bindgen_2812505769cd174d ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short3)(arg1);"
  , "}"
  , "signed int hs_bindgen_f33171933241b063 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int1)(arg1);"
  , "}"
  , "signed int hs_bindgen_d29dc789230f84ca ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int2)(arg1);"
  , "}"
  , "unsigned int hs_bindgen_d1a4d8b93cb1a7ea ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int3)(arg1);"
  , "}"
  , "signed long hs_bindgen_048b4efe41e87d67 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long1)(arg1);"
  , "}"
  , "signed long hs_bindgen_fdac55d3fe047308 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long2)(arg1);"
  , "}"
  , "unsigned long hs_bindgen_f52376869e803792 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long3)(arg1);"
  , "}"
  , "float hs_bindgen_e4b3b2607af6112d ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_float)(arg1);"
  , "}"
  , "double hs_bindgen_4711c220b0501864 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_double)(arg1);"
  , "}"
  , "_Bool hs_bindgen_9adc4b949da19a09 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_bool1)(arg1);"
  , "}"
  , "void hs_bindgen_7ec861566695d10f ("
  , "  signed int arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_struct)(arg1);"
  , "}"
  , "void hs_bindgen_c2d2c07d61e1eb62 ("
  , "  signed int arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_union)(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_9bbae50a9049fad1 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_enum)(arg1);"
  , "}"
  , "signed int *hs_bindgen_8bc5607c7eb9d356 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer1)(arg1);"
  , "}"
  , "signed int **hs_bindgen_39a5de2852bda562 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer2)(arg1);"
  , "}"
  , "void *hs_bindgen_7cbd215b52db604a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer3)(arg1);"
  , "}"
  , "signed int hs_bindgen_e6c7de8b65fe1cae ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (body1)(arg1);"
  , "}"
  , "signed int hs_bindgen_029c97b6c0f885cd (void)"
  , "{"
  , "  return (body2)();"
  , "}"
  , "void hs_bindgen_2f837a6c02b23968 ("
  , "  signed int arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_float)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_3e1d1baf7c0bf148 ("
  , "  signed int arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_double)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_d01c548cf92ab155 ("
  , "  signed int arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_float)(arg1);"
  , "}"
  , "void hs_bindgen_ac6ce5f028b601fa ("
  , "  signed int arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_double)(arg1);"
  , "}"
  , "void hs_bindgen_fcd7c1a5fc12763b ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (bespoke_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8fbc62add0dbcec5 ("
  , "  signed int arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  (bespoke_args2)(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_c47056a55d53106e ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret1)(arg1);"
  , "}"
  , "size_t hs_bindgen_0798688451cfe9fd ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret2)(arg1);"
  , "}"
  , "void hs_bindgen_9ee8f1795d91928b ("
  , "  signed int arg1[]"
  , ")"
  , "{"
  , "  (arr_args1)(arg1);"
  , "}"
  , "void hs_bindgen_301f3e9340c43f26 ("
  , "  signed int *arg1[]"
  , ")"
  , "{"
  , "  (arr_args2)(arg1);"
  , "}"
  , "void hs_bindgen_68f0ebfd931bd7cb ("
  , "  signed int arg1[5]"
  , ")"
  , "{"
  , "  (arr_args3)(arg1);"
  , "}"
  , "void hs_bindgen_ed84a9f5de7c7ba4 ("
  , "  signed int *arg1[5]"
  , ")"
  , "{"
  , "  (arr_args4)(arg1);"
  , "}"
  , "void hs_bindgen_9345ee27c3cfe983 ("
  , "  signed int arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_abd82230bfbe0838 ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e4f5f3a7183a2133 ("
  , "  signed int arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_229e92cfbb3ac759 ("
  , "  signed int arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_81590293bb562ceb ("
  , "  signed int arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args5)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0250e6bccd4bce6a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (comments1)(arg1);"
  , "}"
  , "void hs_bindgen_a5a78939680ab68a ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9132cdddac20b280 ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b9763f73430d85b5 ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_728aaecc12f1683d ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b613caeda65f536a ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_eb4be61d1a8c8938 ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_314f33c34e0461f9 ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1c0c7b0271a0f4f2 ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ad9c0c5d6147c74b ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8f704f4891985378 ("
  , "  signed int arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_a182a67a40ea4095 ("
  , "  signed int arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_30c295cd344411fc ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4208b13d7d48bb5d ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_99f196b4ff936c79 ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1dac78b951f1ccca ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_99abeceb02cd105e ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_7cce0acc1bed7424 ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_91570f3665452ec3 ("
  , "  signed int arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_fd1fdf811d1a4c2a ("
  , "  signed int arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_720398bc1ae40ef7 ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ec3c1cc195885cbd ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9be6911ba093b982 ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_44e9a5835e342626 ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e6422023ae983f47 ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d53a5dac318288be ("
  , "  signed int arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d9617b20966b94c7 ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_421266d9a11ec453 ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args5)(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_0741d01df1d77bca ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret1)(arg1);"
  , "}"
  , "signed int const *hs_bindgen_9bb907f5a9f27ec4 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret2)(arg1);"
  , "}"
  , "signed int *const hs_bindgen_d40d79f5eb81613b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret3)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_ae3a7bce1635a452 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret4)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_e9822e54cd42abd1 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret5)(arg1);"
  , "}"
  , "void hs_bindgen_07c91e79dae2475d ("
  , "  signed int const arg1[]"
  , ")"
  , "{"
  , "  (const_array_elem1)(arg1);"
  , "}"
  , "void hs_bindgen_f8f3fe83e121f030 ("
  , "  signed int const *arg1[]"
  , ")"
  , "{"
  , "  (const_array_elem2)(arg1);"
  , "}"
  , "void hs_bindgen_d95e9e6469a6b6d7 ("
  , "  signed int *const arg1[]"
  , ")"
  , "{"
  , "  (const_array_elem3)(arg1);"
  , "}"
  , "signed int hs_bindgen_d9eba1b532f5fe32 (void)"
  , "{"
  , "  return (noParams1)();"
  , "}"
  , "signed int hs_bindgen_83e24e70c374fa3f (void)"
  , "{"
  , "  return (noParams2)();"
  , "}"
  , "void hs_bindgen_e31cd37caf95eafb ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (noParams3)(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_bdd4a6a2cda990c6 ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_c602095d4bab5664 ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret2)(arg1);"
  , "}"
  , "void (*hs_bindgen_96be7558d494a657 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (funptr_ret3)(arg1);"
  , "}"
  , "char (*hs_bindgen_56223c665cfb28ac ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret4)(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_3c78cc60eefc439f ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret5)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_6e26f40164d44434 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret6)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_8394035dfd669e5e ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret7)(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_21bf97251465e190 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret8)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_071425e04d63795c ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret9)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_5d17bf2c3be76953 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret10)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_char1@
foreign import ccall safe "hs_bindgen_54963e3aae2388b8" hs_bindgen_54963e3aae2388b8_base ::
     BG.CInt
  -> BG.CChar
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_char1@
hs_bindgen_54963e3aae2388b8 ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_54963e3aae2388b8 =
  \x0 ->
    \x1 ->
      hs_bindgen_54963e3aae2388b8_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_char1 = hs_bindgen_54963e3aae2388b8

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_char2@
foreign import ccall safe "hs_bindgen_62073552f76ff03c" hs_bindgen_62073552f76ff03c_base ::
     BG.CInt
  -> BG.CSChar
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_char2@
hs_bindgen_62073552f76ff03c ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_62073552f76ff03c =
  \x0 ->
    \x1 ->
      hs_bindgen_62073552f76ff03c_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_char2 = hs_bindgen_62073552f76ff03c

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_char3@
foreign import ccall safe "hs_bindgen_cecb42184f8ea235" hs_bindgen_cecb42184f8ea235_base ::
     BG.CInt
  -> BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_char3@
hs_bindgen_cecb42184f8ea235 ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_cecb42184f8ea235 =
  \x0 ->
    \x1 ->
      hs_bindgen_cecb42184f8ea235_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_char3 = hs_bindgen_cecb42184f8ea235

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_short1@
foreign import ccall safe "hs_bindgen_3739d770e0777144" hs_bindgen_3739d770e0777144_base ::
     BG.CInt
  -> BG.CShort
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_short1@
hs_bindgen_3739d770e0777144 ::
     BG.CInt
  -> BG.CShort
  -> IO ()
hs_bindgen_3739d770e0777144 =
  \x0 ->
    \x1 ->
      hs_bindgen_3739d770e0777144_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_short1 = hs_bindgen_3739d770e0777144

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_short2@
foreign import ccall safe "hs_bindgen_99f7078b0a509e25" hs_bindgen_99f7078b0a509e25_base ::
     BG.CInt
  -> BG.CShort
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_short2@
hs_bindgen_99f7078b0a509e25 ::
     BG.CInt
  -> BG.CShort
  -> IO ()
hs_bindgen_99f7078b0a509e25 =
  \x0 ->
    \x1 ->
      hs_bindgen_99f7078b0a509e25_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_short2 = hs_bindgen_99f7078b0a509e25

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_short3@
foreign import ccall safe "hs_bindgen_d227b14017c51974" hs_bindgen_d227b14017c51974_base ::
     BG.CInt
  -> BG.CUShort
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_short3@
hs_bindgen_d227b14017c51974 ::
     BG.CInt
  -> BG.CUShort
  -> IO ()
hs_bindgen_d227b14017c51974 =
  \x0 ->
    \x1 ->
      hs_bindgen_d227b14017c51974_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_short3 = hs_bindgen_d227b14017c51974

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_int1@
foreign import ccall safe "hs_bindgen_9af1ce0326871cd8" hs_bindgen_9af1ce0326871cd8_base ::
     BG.CInt
  -> BG.CInt
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_int1@
hs_bindgen_9af1ce0326871cd8 ::
     BG.CInt
  -> BG.CInt
  -> IO ()
hs_bindgen_9af1ce0326871cd8 =
  \x0 ->
    \x1 ->
      hs_bindgen_9af1ce0326871cd8_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_int1 = hs_bindgen_9af1ce0326871cd8

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_int2@
foreign import ccall safe "hs_bindgen_28933d12947391da" hs_bindgen_28933d12947391da_base ::
     BG.CInt
  -> BG.CInt
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_int2@
hs_bindgen_28933d12947391da ::
     BG.CInt
  -> BG.CInt
  -> IO ()
hs_bindgen_28933d12947391da =
  \x0 ->
    \x1 ->
      hs_bindgen_28933d12947391da_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_int2 = hs_bindgen_28933d12947391da

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_int3@
foreign import ccall safe "hs_bindgen_5ccd0c1c9031ffdd" hs_bindgen_5ccd0c1c9031ffdd_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_int3@
hs_bindgen_5ccd0c1c9031ffdd ::
     BG.CInt
  -> BG.CUInt
  -> IO ()
hs_bindgen_5ccd0c1c9031ffdd =
  \x0 ->
    \x1 ->
      hs_bindgen_5ccd0c1c9031ffdd_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_int3 = hs_bindgen_5ccd0c1c9031ffdd

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_long1@
foreign import ccall safe "hs_bindgen_f35cd6740be21567" hs_bindgen_f35cd6740be21567_base ::
     BG.CInt
  -> BG.CLong
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_long1@
hs_bindgen_f35cd6740be21567 ::
     BG.CInt
  -> BG.CLong
  -> IO ()
hs_bindgen_f35cd6740be21567 =
  \x0 ->
    \x1 ->
      hs_bindgen_f35cd6740be21567_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_long1 = hs_bindgen_f35cd6740be21567

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_long2@
foreign import ccall safe "hs_bindgen_39236a3eb32d14a9" hs_bindgen_39236a3eb32d14a9_base ::
     BG.CInt
  -> BG.CLong
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_long2@
hs_bindgen_39236a3eb32d14a9 ::
     BG.CInt
  -> BG.CLong
  -> IO ()
hs_bindgen_39236a3eb32d14a9 =
  \x0 ->
    \x1 ->
      hs_bindgen_39236a3eb32d14a9_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_long2 = hs_bindgen_39236a3eb32d14a9

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_long3@
foreign import ccall safe "hs_bindgen_f9ec6d5b93569e48" hs_bindgen_f9ec6d5b93569e48_base ::
     BG.CInt
  -> BG.CULong
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_long3@
hs_bindgen_f9ec6d5b93569e48 ::
     BG.CInt
  -> BG.CULong
  -> IO ()
hs_bindgen_f9ec6d5b93569e48 =
  \x0 ->
    \x1 ->
      hs_bindgen_f9ec6d5b93569e48_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_long3 = hs_bindgen_f9ec6d5b93569e48

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_float@
foreign import ccall safe "hs_bindgen_d537620e6ef3b422" hs_bindgen_d537620e6ef3b422_base ::
     BG.CInt
  -> BG.CFloat
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_float@
hs_bindgen_d537620e6ef3b422 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_d537620e6ef3b422 =
  \x0 ->
    \x1 ->
      hs_bindgen_d537620e6ef3b422_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_float = hs_bindgen_d537620e6ef3b422

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_double@
foreign import ccall safe "hs_bindgen_b36fb283c88dc5c6" hs_bindgen_b36fb283c88dc5c6_base ::
     BG.CInt
  -> BG.CDouble
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_double@
hs_bindgen_b36fb283c88dc5c6 ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_b36fb283c88dc5c6 =
  \x0 ->
    \x1 ->
      hs_bindgen_b36fb283c88dc5c6_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_double = hs_bindgen_b36fb283c88dc5c6

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_bool1@
foreign import ccall safe "hs_bindgen_8018fdb8935528bd" hs_bindgen_8018fdb8935528bd_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_bool1@
hs_bindgen_8018fdb8935528bd ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_8018fdb8935528bd =
  \x0 ->
    \x1 ->
      hs_bindgen_8018fdb8935528bd_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_bool1 = hs_bindgen_8018fdb8935528bd

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_struct@
foreign import ccall safe "hs_bindgen_a499a7905953ab61" hs_bindgen_a499a7905953ab61_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_struct@
hs_bindgen_a499a7905953ab61 ::
     BG.CInt
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_a499a7905953ab61 =
  \x0 ->
    \x1 ->
      hs_bindgen_a499a7905953ab61_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_a499a7905953ab61 arg10 arg22)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_union@
foreign import ccall safe "hs_bindgen_85dea9d33d9bab4e" hs_bindgen_85dea9d33d9bab4e_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_union@
hs_bindgen_85dea9d33d9bab4e ::
     BG.CInt
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_85dea9d33d9bab4e =
  \x0 ->
    \x1 ->
      hs_bindgen_85dea9d33d9bab4e_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_85dea9d33d9bab4e arg10 arg22)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_enum@
foreign import ccall safe "hs_bindgen_09cf48b070873cfc" hs_bindgen_09cf48b070873cfc_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_enum@
hs_bindgen_09cf48b070873cfc ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_09cf48b070873cfc =
  \x0 ->
    \x1 ->
      hs_bindgen_09cf48b070873cfc_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_enum = hs_bindgen_09cf48b070873cfc

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_pointer1@
foreign import ccall safe "hs_bindgen_c72bb2a5ebfa749a" hs_bindgen_c72bb2a5ebfa749a_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_pointer1@
hs_bindgen_c72bb2a5ebfa749a ::
     BG.CInt
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_c72bb2a5ebfa749a =
  \x0 ->
    \x1 ->
      hs_bindgen_c72bb2a5ebfa749a_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_pointer1 = hs_bindgen_c72bb2a5ebfa749a

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_pointer2@
foreign import ccall safe "hs_bindgen_9aa2fcd4bbd7f16e" hs_bindgen_9aa2fcd4bbd7f16e_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_pointer2@
hs_bindgen_9aa2fcd4bbd7f16e ::
     BG.CInt
  -> BG.Ptr (BG.Ptr BG.CInt)
  -> IO ()
hs_bindgen_9aa2fcd4bbd7f16e =
  \x0 ->
    \x1 ->
      hs_bindgen_9aa2fcd4bbd7f16e_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_pointer2 = hs_bindgen_9aa2fcd4bbd7f16e

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_pointer3@
foreign import ccall safe "hs_bindgen_61f9226bc89b31df" hs_bindgen_61f9226bc89b31df_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_pointer3@
hs_bindgen_61f9226bc89b31df ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()
hs_bindgen_61f9226bc89b31df =
  \x0 ->
    \x1 ->
      hs_bindgen_61f9226bc89b31df_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_pointer3 = hs_bindgen_61f9226bc89b31df

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_A@
foreign import ccall safe "hs_bindgen_8831a24bc16d7d8d" hs_bindgen_8831a24bc16d7d8d_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_A@
hs_bindgen_8831a24bc16d7d8d :: IO BG.CInt
hs_bindgen_8831a24bc16d7d8d =
  fmap BG.fromFFIType hs_bindgen_8831a24bc16d7d8d_base

{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h 47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A :: IO BG.CInt
ret_A = hs_bindgen_8831a24bc16d7d8d

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_char1@
foreign import ccall safe "hs_bindgen_5633b28b9d25f0f5" hs_bindgen_5633b28b9d25f0f5_base ::
     BG.CInt
  -> IO BG.CChar

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_char1@
hs_bindgen_5633b28b9d25f0f5 ::
     BG.CInt
  -> IO BG.CChar
hs_bindgen_5633b28b9d25f0f5 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_5633b28b9d25f0f5_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h 49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CChar
ret_char1 = hs_bindgen_5633b28b9d25f0f5

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_char2@
foreign import ccall safe "hs_bindgen_2cedc94dfbbedc0c" hs_bindgen_2cedc94dfbbedc0c_base ::
     BG.CInt
  -> IO BG.CSChar

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_char2@
hs_bindgen_2cedc94dfbbedc0c ::
     BG.CInt
  -> IO BG.CSChar
hs_bindgen_2cedc94dfbbedc0c =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_2cedc94dfbbedc0c_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h 50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CSChar
ret_char2 = hs_bindgen_2cedc94dfbbedc0c

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_char3@
foreign import ccall safe "hs_bindgen_500de2a3f906016c" hs_bindgen_500de2a3f906016c_base ::
     BG.CInt
  -> IO BG.CUChar

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_char3@
hs_bindgen_500de2a3f906016c ::
     BG.CInt
  -> IO BG.CUChar
hs_bindgen_500de2a3f906016c =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_500de2a3f906016c_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h 51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUChar
ret_char3 = hs_bindgen_500de2a3f906016c

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_short1@
foreign import ccall safe "hs_bindgen_360cbf2f994cc28e" hs_bindgen_360cbf2f994cc28e_base ::
     BG.CInt
  -> IO BG.CShort

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_short1@
hs_bindgen_360cbf2f994cc28e ::
     BG.CInt
  -> IO BG.CShort
hs_bindgen_360cbf2f994cc28e =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_360cbf2f994cc28e_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h 53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short1 = hs_bindgen_360cbf2f994cc28e

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_short2@
foreign import ccall safe "hs_bindgen_8b1d3f34754d7fea" hs_bindgen_8b1d3f34754d7fea_base ::
     BG.CInt
  -> IO BG.CShort

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_short2@
hs_bindgen_8b1d3f34754d7fea ::
     BG.CInt
  -> IO BG.CShort
hs_bindgen_8b1d3f34754d7fea =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_8b1d3f34754d7fea_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h 54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short2 = hs_bindgen_8b1d3f34754d7fea

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_short3@
foreign import ccall safe "hs_bindgen_2812505769cd174d" hs_bindgen_2812505769cd174d_base ::
     BG.CInt
  -> IO BG.CUShort

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_short3@
hs_bindgen_2812505769cd174d ::
     BG.CInt
  -> IO BG.CUShort
hs_bindgen_2812505769cd174d =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_2812505769cd174d_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h 55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUShort
ret_short3 = hs_bindgen_2812505769cd174d

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_int1@
foreign import ccall safe "hs_bindgen_f33171933241b063" hs_bindgen_f33171933241b063_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_int1@
hs_bindgen_f33171933241b063 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_f33171933241b063 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_f33171933241b063_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h 57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int1 = hs_bindgen_f33171933241b063

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_int2@
foreign import ccall safe "hs_bindgen_d29dc789230f84ca" hs_bindgen_d29dc789230f84ca_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_int2@
hs_bindgen_d29dc789230f84ca ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_d29dc789230f84ca =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_d29dc789230f84ca_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h 58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int2 = hs_bindgen_d29dc789230f84ca

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_int3@
foreign import ccall safe "hs_bindgen_d1a4d8b93cb1a7ea" hs_bindgen_d1a4d8b93cb1a7ea_base ::
     BG.CInt
  -> IO BG.CUInt

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_int3@
hs_bindgen_d1a4d8b93cb1a7ea ::
     BG.CInt
  -> IO BG.CUInt
hs_bindgen_d1a4d8b93cb1a7ea =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_d1a4d8b93cb1a7ea_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h 59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUInt
ret_int3 = hs_bindgen_d1a4d8b93cb1a7ea

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_long1@
foreign import ccall safe "hs_bindgen_048b4efe41e87d67" hs_bindgen_048b4efe41e87d67_base ::
     BG.CInt
  -> IO BG.CLong

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_long1@
hs_bindgen_048b4efe41e87d67 ::
     BG.CInt
  -> IO BG.CLong
hs_bindgen_048b4efe41e87d67 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_048b4efe41e87d67_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h 61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long1 = hs_bindgen_048b4efe41e87d67

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_long2@
foreign import ccall safe "hs_bindgen_fdac55d3fe047308" hs_bindgen_fdac55d3fe047308_base ::
     BG.CInt
  -> IO BG.CLong

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_long2@
hs_bindgen_fdac55d3fe047308 ::
     BG.CInt
  -> IO BG.CLong
hs_bindgen_fdac55d3fe047308 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_fdac55d3fe047308_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h 62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long2 = hs_bindgen_fdac55d3fe047308

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_long3@
foreign import ccall safe "hs_bindgen_f52376869e803792" hs_bindgen_f52376869e803792_base ::
     BG.CInt
  -> IO BG.CULong

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_long3@
hs_bindgen_f52376869e803792 ::
     BG.CInt
  -> IO BG.CULong
hs_bindgen_f52376869e803792 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_f52376869e803792_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h 63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CULong
ret_long3 = hs_bindgen_f52376869e803792

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_float@
foreign import ccall safe "hs_bindgen_e4b3b2607af6112d" hs_bindgen_e4b3b2607af6112d_base ::
     BG.CInt
  -> IO BG.CFloat

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_float@
hs_bindgen_e4b3b2607af6112d ::
     BG.CInt
  -> IO BG.CFloat
hs_bindgen_e4b3b2607af6112d =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_e4b3b2607af6112d_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h 65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CFloat
ret_float = hs_bindgen_e4b3b2607af6112d

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_double@
foreign import ccall safe "hs_bindgen_4711c220b0501864" hs_bindgen_4711c220b0501864_base ::
     BG.CInt
  -> IO BG.CDouble

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_double@
hs_bindgen_4711c220b0501864 ::
     BG.CInt
  -> IO BG.CDouble
hs_bindgen_4711c220b0501864 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_4711c220b0501864_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h 66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CDouble
ret_double = hs_bindgen_4711c220b0501864

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_bool1@
foreign import ccall safe "hs_bindgen_9adc4b949da19a09" hs_bindgen_9adc4b949da19a09_base ::
     BG.CInt
  -> IO BG.CBool

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_bool1@
hs_bindgen_9adc4b949da19a09 ::
     BG.CInt
  -> IO BG.CBool
hs_bindgen_9adc4b949da19a09 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_9adc4b949da19a09_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h 67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
ret_bool1 = hs_bindgen_9adc4b949da19a09

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_struct@
foreign import ccall safe "hs_bindgen_7ec861566695d10f" hs_bindgen_7ec861566695d10f_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_struct@
hs_bindgen_7ec861566695d10f ::
     BG.CInt
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_7ec861566695d10f =
  \x0 ->
    \x1 ->
      hs_bindgen_7ec861566695d10f_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_7ec861566695d10f arg10 res1)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_union@
foreign import ccall safe "hs_bindgen_c2d2c07d61e1eb62" hs_bindgen_c2d2c07d61e1eb62_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_union@
hs_bindgen_c2d2c07d61e1eb62 ::
     BG.CInt
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_c2d2c07d61e1eb62 =
  \x0 ->
    \x1 ->
      hs_bindgen_c2d2c07d61e1eb62_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_c2d2c07d61e1eb62 arg10 res1)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_enum@
foreign import ccall safe "hs_bindgen_9bbae50a9049fad1" hs_bindgen_9bbae50a9049fad1_base ::
     BG.CInt
  -> IO BG.CUInt

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_enum@
hs_bindgen_9bbae50a9049fad1 ::
     BG.CInt
  -> IO Some_enum
hs_bindgen_9bbae50a9049fad1 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_9bbae50a9049fad1_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h 71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO Some_enum
ret_enum = hs_bindgen_9bbae50a9049fad1

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_pointer1@
foreign import ccall safe "hs_bindgen_8bc5607c7eb9d356" hs_bindgen_8bc5607c7eb9d356_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_pointer1@
hs_bindgen_8bc5607c7eb9d356 ::
     BG.CInt
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_8bc5607c7eb9d356 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_8bc5607c7eb9d356_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h 73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
ret_pointer1 = hs_bindgen_8bc5607c7eb9d356

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_pointer2@
foreign import ccall safe "hs_bindgen_39a5de2852bda562" hs_bindgen_39a5de2852bda562_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_pointer2@
hs_bindgen_39a5de2852bda562 ::
     BG.CInt
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
hs_bindgen_39a5de2852bda562 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_39a5de2852bda562_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h 74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
ret_pointer2 = hs_bindgen_39a5de2852bda562

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_pointer3@
foreign import ccall safe "hs_bindgen_7cbd215b52db604a" hs_bindgen_7cbd215b52db604a_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_pointer3@
hs_bindgen_7cbd215b52db604a ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)
hs_bindgen_7cbd215b52db604a =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_7cbd215b52db604a_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h 75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.Void)
ret_pointer3 = hs_bindgen_7cbd215b52db604a

-- __unique:__ @test_macrosreparse_1_empty Example Safe_body1@
foreign import ccall safe "hs_bindgen_e6c7de8b65fe1cae" hs_bindgen_e6c7de8b65fe1cae_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparse_1_empty Example Safe_body1@
hs_bindgen_e6c7de8b65fe1cae ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_e6c7de8b65fe1cae =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_e6c7de8b65fe1cae_base (BG.toFFIType x0))

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h 79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
body1 = hs_bindgen_e6c7de8b65fe1cae

-- __unique:__ @test_macrosreparse_1_empty Example Safe_body2@
foreign import ccall safe "hs_bindgen_029c97b6c0f885cd" hs_bindgen_029c97b6c0f885cd_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse_1_empty Example Safe_body2@
hs_bindgen_029c97b6c0f885cd :: IO BG.CInt
hs_bindgen_029c97b6c0f885cd =
  fmap BG.fromFFIType hs_bindgen_029c97b6c0f885cd_base

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h 80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2 :: IO BG.CInt
body2 = hs_bindgen_029c97b6c0f885cd

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_complex_float@
foreign import ccall safe "hs_bindgen_2f837a6c02b23968" hs_bindgen_2f837a6c02b23968_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_complex_float@
hs_bindgen_2f837a6c02b23968 ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_2f837a6c02b23968 =
  \x0 ->
    \x1 ->
      hs_bindgen_2f837a6c02b23968_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_2f837a6c02b23968 arg10 arg22)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_complex_double@
foreign import ccall safe "hs_bindgen_3e1d1baf7c0bf148" hs_bindgen_3e1d1baf7c0bf148_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_args_complex_double@
hs_bindgen_3e1d1baf7c0bf148 ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_3e1d1baf7c0bf148 =
  \x0 ->
    \x1 ->
      hs_bindgen_3e1d1baf7c0bf148_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_3e1d1baf7c0bf148 arg10 arg22)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_complex_float@
foreign import ccall safe "hs_bindgen_d01c548cf92ab155" hs_bindgen_d01c548cf92ab155_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_complex_float@
hs_bindgen_d01c548cf92ab155 ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_d01c548cf92ab155 =
  \x0 ->
    \x1 ->
      hs_bindgen_d01c548cf92ab155_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_d01c548cf92ab155 arg10 res1)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_complex_double@
foreign import ccall safe "hs_bindgen_ac6ce5f028b601fa" hs_bindgen_ac6ce5f028b601fa_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_ret_complex_double@
hs_bindgen_ac6ce5f028b601fa ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_ac6ce5f028b601fa =
  \x0 ->
    \x1 ->
      hs_bindgen_ac6ce5f028b601fa_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_ac6ce5f028b601fa arg10 res1)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_bespoke_args1@
foreign import ccall safe "hs_bindgen_fcd7c1a5fc12763b" hs_bindgen_fcd7c1a5fc12763b_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_bespoke_args1@
hs_bindgen_fcd7c1a5fc12763b ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_fcd7c1a5fc12763b =
  \x0 ->
    \x1 ->
      hs_bindgen_fcd7c1a5fc12763b_base (BG.toFFIType x0) (BG.toFFIType x1)

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
bespoke_args1 = hs_bindgen_fcd7c1a5fc12763b

-- __unique:__ @test_macrosreparse_1_empty Example Safe_bespoke_args2@
foreign import ccall safe "hs_bindgen_8fbc62add0dbcec5" hs_bindgen_8fbc62add0dbcec5_base ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_bespoke_args2@
hs_bindgen_8fbc62add0dbcec5 ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_8fbc62add0dbcec5 =
  \x0 ->
    \x1 ->
      hs_bindgen_8fbc62add0dbcec5_base (BG.toFFIType x0) (BG.toFFIType x1)

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
bespoke_args2 = hs_bindgen_8fbc62add0dbcec5

-- __unique:__ @test_macrosreparse_1_empty Example Safe_bespoke_ret1@
foreign import ccall safe "hs_bindgen_c47056a55d53106e" hs_bindgen_c47056a55d53106e_base ::
     BG.CInt
  -> IO BG.CBool

-- __unique:__ @test_macrosreparse_1_empty Example Safe_bespoke_ret1@
hs_bindgen_c47056a55d53106e ::
     BG.CInt
  -> IO BG.CBool
hs_bindgen_c47056a55d53106e =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_c47056a55d53106e_base (BG.toFFIType x0))

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h 97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
bespoke_ret1 = hs_bindgen_c47056a55d53106e

-- __unique:__ @test_macrosreparse_1_empty Example Safe_bespoke_ret2@
foreign import ccall safe "hs_bindgen_0798688451cfe9fd" hs_bindgen_0798688451cfe9fd_base ::
     BG.CInt
  -> IO HsBindgen.Runtime.LibC.CSize

-- __unique:__ @test_macrosreparse_1_empty Example Safe_bespoke_ret2@
hs_bindgen_0798688451cfe9fd ::
     BG.CInt
  -> IO HsBindgen.Runtime.LibC.CSize
hs_bindgen_0798688451cfe9fd =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_0798688451cfe9fd_base (BG.toFFIType x0))

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h 98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO HsBindgen.Runtime.LibC.CSize
bespoke_ret2 = hs_bindgen_0798688451cfe9fd

-- __unique:__ @test_macrosreparse_1_empty Example Safe_arr_args1@
foreign import ccall safe "hs_bindgen_9ee8f1795d91928b" hs_bindgen_9ee8f1795d91928b_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_arr_args1@
hs_bindgen_9ee8f1795d91928b ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt))
  -> IO ()
hs_bindgen_9ee8f1795d91928b =
  \x0 ->
    hs_bindgen_9ee8f1795d91928b_base (BG.toFFIType x0)

{-| Arrays

    __C declaration:__ @arr_args1@

    __defined at:__ @macros\/reparse.h 104:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args1 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args1 = hs_bindgen_9ee8f1795d91928b

-- __unique:__ @test_macrosreparse_1_empty Example Safe_arr_args2@
foreign import ccall safe "hs_bindgen_301f3e9340c43f26" hs_bindgen_301f3e9340c43f26_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_arr_args2@
hs_bindgen_301f3e9340c43f26 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_301f3e9340c43f26 =
  \x0 ->
    hs_bindgen_301f3e9340c43f26_base (BG.toFFIType x0)

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h 105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args2 = hs_bindgen_301f3e9340c43f26

-- __unique:__ @test_macrosreparse_1_empty Example Safe_arr_args3@
foreign import ccall safe "hs_bindgen_68f0ebfd931bd7cb" hs_bindgen_68f0ebfd931bd7cb_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_arr_args3@
hs_bindgen_68f0ebfd931bd7cb ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt))
  -> IO ()
hs_bindgen_68f0ebfd931bd7cb =
  \x0 ->
    hs_bindgen_68f0ebfd931bd7cb_base (BG.toFFIType x0)

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h 106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args3 = hs_bindgen_68f0ebfd931bd7cb

-- __unique:__ @test_macrosreparse_1_empty Example Safe_arr_args4@
foreign import ccall safe "hs_bindgen_ed84a9f5de7c7ba4" hs_bindgen_ed84a9f5de7c7ba4_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_arr_args4@
hs_bindgen_ed84a9f5de7c7ba4 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_ed84a9f5de7c7ba4 =
  \x0 ->
    hs_bindgen_ed84a9f5de7c7ba4_base (BG.toFFIType x0)

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h 107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args4 = hs_bindgen_ed84a9f5de7c7ba4

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_args1@
foreign import ccall safe "hs_bindgen_9345ee27c3cfe983" hs_bindgen_9345ee27c3cfe983_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_args1@
hs_bindgen_9345ee27c3cfe983 ::
     BG.CInt
  -> BG.FunPtr (IO ())
  -> IO ()
hs_bindgen_9345ee27c3cfe983 =
  \x0 ->
    \x1 ->
      hs_bindgen_9345ee27c3cfe983_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args1 = hs_bindgen_9345ee27c3cfe983

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_args2@
foreign import ccall safe "hs_bindgen_abd82230bfbe0838" hs_bindgen_abd82230bfbe0838_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_args2@
hs_bindgen_abd82230bfbe0838 ::
     BG.CInt
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_abd82230bfbe0838 =
  \x0 ->
    \x1 ->
      hs_bindgen_abd82230bfbe0838_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args2 = hs_bindgen_abd82230bfbe0838

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_args3@
foreign import ccall safe "hs_bindgen_e4f5f3a7183a2133" hs_bindgen_e4f5f3a7183a2133_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_args3@
hs_bindgen_e4f5f3a7183a2133 ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> IO ())
  -> IO ()
hs_bindgen_e4f5f3a7183a2133 =
  \x0 ->
    \x1 ->
      hs_bindgen_e4f5f3a7183a2133_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args3 = hs_bindgen_e4f5f3a7183a2133

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_args4@
foreign import ccall safe "hs_bindgen_229e92cfbb3ac759" hs_bindgen_229e92cfbb3ac759_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_args4@
hs_bindgen_229e92cfbb3ac759 ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar)
  -> IO ()
hs_bindgen_229e92cfbb3ac759 =
  \x0 ->
    \x1 ->
      hs_bindgen_229e92cfbb3ac759_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args4 = hs_bindgen_229e92cfbb3ac759

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_args5@
foreign import ccall safe "hs_bindgen_81590293bb562ceb" hs_bindgen_81590293bb562ceb_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_args5@
hs_bindgen_81590293bb562ceb ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))
  -> IO ()
hs_bindgen_81590293bb562ceb =
  \x0 ->
    \x1 ->
      hs_bindgen_81590293bb562ceb_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args5 = hs_bindgen_81590293bb562ceb

-- __unique:__ @test_macrosreparse_1_empty Example Safe_comments1@
foreign import ccall safe "hs_bindgen_0250e6bccd4bce6a" hs_bindgen_0250e6bccd4bce6a_base ::
     BG.CInt
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_comments1@
hs_bindgen_0250e6bccd4bce6a ::
     BG.CInt
  -> IO ()
hs_bindgen_0250e6bccd4bce6a =
  \x0 ->
    hs_bindgen_0250e6bccd4bce6a_base (BG.toFFIType x0)

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
comments1 = hs_bindgen_0250e6bccd4bce6a

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_prim_before1@
foreign import ccall safe "hs_bindgen_a5a78939680ab68a" hs_bindgen_a5a78939680ab68a_base ::
     BG.CInt
  -> BG.CChar
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_prim_before1@
hs_bindgen_a5a78939680ab68a ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_a5a78939680ab68a =
  \x0 ->
    \x1 ->
      hs_bindgen_a5a78939680ab68a_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_before1 = hs_bindgen_a5a78939680ab68a

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_prim_before2@
foreign import ccall safe "hs_bindgen_9132cdddac20b280" hs_bindgen_9132cdddac20b280_base ::
     BG.CInt
  -> BG.CSChar
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_prim_before2@
hs_bindgen_9132cdddac20b280 ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_9132cdddac20b280 =
  \x0 ->
    \x1 ->
      hs_bindgen_9132cdddac20b280_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_before2 = hs_bindgen_9132cdddac20b280

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_prim_before3@
foreign import ccall safe "hs_bindgen_b9763f73430d85b5" hs_bindgen_b9763f73430d85b5_base ::
     BG.CInt
  -> BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_prim_before3@
hs_bindgen_b9763f73430d85b5 ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_b9763f73430d85b5 =
  \x0 ->
    \x1 ->
      hs_bindgen_b9763f73430d85b5_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_before3 = hs_bindgen_b9763f73430d85b5

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_prim_after1@
foreign import ccall safe "hs_bindgen_728aaecc12f1683d" hs_bindgen_728aaecc12f1683d_base ::
     BG.CInt
  -> BG.CChar
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_prim_after1@
hs_bindgen_728aaecc12f1683d ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_728aaecc12f1683d =
  \x0 ->
    \x1 ->
      hs_bindgen_728aaecc12f1683d_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_after1 = hs_bindgen_728aaecc12f1683d

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_prim_after2@
foreign import ccall safe "hs_bindgen_b613caeda65f536a" hs_bindgen_b613caeda65f536a_base ::
     BG.CInt
  -> BG.CSChar
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_prim_after2@
hs_bindgen_b613caeda65f536a ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_b613caeda65f536a =
  \x0 ->
    \x1 ->
      hs_bindgen_b613caeda65f536a_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_after2 = hs_bindgen_b613caeda65f536a

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_prim_after3@
foreign import ccall safe "hs_bindgen_eb4be61d1a8c8938" hs_bindgen_eb4be61d1a8c8938_base ::
     BG.CInt
  -> BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_prim_after3@
hs_bindgen_eb4be61d1a8c8938 ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_eb4be61d1a8c8938 =
  \x0 ->
    \x1 ->
      hs_bindgen_eb4be61d1a8c8938_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_after3 = hs_bindgen_eb4be61d1a8c8938

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_before1@
foreign import ccall safe "hs_bindgen_314f33c34e0461f9" hs_bindgen_314f33c34e0461f9_base ::
     BG.CInt
  -> BG.CFloat
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_before1@
hs_bindgen_314f33c34e0461f9 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_314f33c34e0461f9 =
  \x0 ->
    \x1 ->
      hs_bindgen_314f33c34e0461f9_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_314f33c34e0461f9

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_before2@
foreign import ccall safe "hs_bindgen_1c0c7b0271a0f4f2" hs_bindgen_1c0c7b0271a0f4f2_base ::
     BG.CInt
  -> BG.CDouble
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_before2@
hs_bindgen_1c0c7b0271a0f4f2 ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_1c0c7b0271a0f4f2 =
  \x0 ->
    \x1 ->
      hs_bindgen_1c0c7b0271a0f4f2_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_1c0c7b0271a0f4f2

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_before3@
foreign import ccall safe "hs_bindgen_ad9c0c5d6147c74b" hs_bindgen_ad9c0c5d6147c74b_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_before3@
hs_bindgen_ad9c0c5d6147c74b ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_ad9c0c5d6147c74b =
  \x0 ->
    \x1 ->
      hs_bindgen_ad9c0c5d6147c74b_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_ad9c0c5d6147c74b

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_before4@
foreign import ccall safe "hs_bindgen_8f704f4891985378" hs_bindgen_8f704f4891985378_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_before4@
hs_bindgen_8f704f4891985378 ::
     BG.CInt
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_8f704f4891985378 =
  \x0 ->
    \x1 ->
      hs_bindgen_8f704f4891985378_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_8f704f4891985378 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_before5@
foreign import ccall safe "hs_bindgen_a182a67a40ea4095" hs_bindgen_a182a67a40ea4095_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_before5@
hs_bindgen_a182a67a40ea4095 ::
     BG.CInt
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_a182a67a40ea4095 =
  \x0 ->
    \x1 ->
      hs_bindgen_a182a67a40ea4095_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_a182a67a40ea4095 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_before6@
foreign import ccall safe "hs_bindgen_30c295cd344411fc" hs_bindgen_30c295cd344411fc_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_before6@
hs_bindgen_30c295cd344411fc ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_30c295cd344411fc =
  \x0 ->
    \x1 ->
      hs_bindgen_30c295cd344411fc_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_30c295cd344411fc

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_before7@
foreign import ccall safe "hs_bindgen_4208b13d7d48bb5d" hs_bindgen_4208b13d7d48bb5d_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_before7@
hs_bindgen_4208b13d7d48bb5d ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_4208b13d7d48bb5d =
  \x0 ->
    \x1 ->
      hs_bindgen_4208b13d7d48bb5d_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_4208b13d7d48bb5d

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_before8@
foreign import ccall safe "hs_bindgen_99f196b4ff936c79" hs_bindgen_99f196b4ff936c79_base ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_before8@
hs_bindgen_99f196b4ff936c79 ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_99f196b4ff936c79 =
  \x0 ->
    \x1 ->
      hs_bindgen_99f196b4ff936c79_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_99f196b4ff936c79

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_after1@
foreign import ccall safe "hs_bindgen_1dac78b951f1ccca" hs_bindgen_1dac78b951f1ccca_base ::
     BG.CInt
  -> BG.CFloat
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_after1@
hs_bindgen_1dac78b951f1ccca ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_1dac78b951f1ccca =
  \x0 ->
    \x1 ->
      hs_bindgen_1dac78b951f1ccca_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_1dac78b951f1ccca

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_after2@
foreign import ccall safe "hs_bindgen_99abeceb02cd105e" hs_bindgen_99abeceb02cd105e_base ::
     BG.CInt
  -> BG.CDouble
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_after2@
hs_bindgen_99abeceb02cd105e ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_99abeceb02cd105e =
  \x0 ->
    \x1 ->
      hs_bindgen_99abeceb02cd105e_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_99abeceb02cd105e

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_after3@
foreign import ccall safe "hs_bindgen_7cce0acc1bed7424" hs_bindgen_7cce0acc1bed7424_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_after3@
hs_bindgen_7cce0acc1bed7424 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_7cce0acc1bed7424 =
  \x0 ->
    \x1 ->
      hs_bindgen_7cce0acc1bed7424_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_7cce0acc1bed7424

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_after4@
foreign import ccall safe "hs_bindgen_91570f3665452ec3" hs_bindgen_91570f3665452ec3_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_after4@
hs_bindgen_91570f3665452ec3 ::
     BG.CInt
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_91570f3665452ec3 =
  \x0 ->
    \x1 ->
      hs_bindgen_91570f3665452ec3_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_91570f3665452ec3 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_after5@
foreign import ccall safe "hs_bindgen_fd1fdf811d1a4c2a" hs_bindgen_fd1fdf811d1a4c2a_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_after5@
hs_bindgen_fd1fdf811d1a4c2a ::
     BG.CInt
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_fd1fdf811d1a4c2a =
  \x0 ->
    \x1 ->
      hs_bindgen_fd1fdf811d1a4c2a_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_fd1fdf811d1a4c2a arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_after6@
foreign import ccall safe "hs_bindgen_720398bc1ae40ef7" hs_bindgen_720398bc1ae40ef7_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_after6@
hs_bindgen_720398bc1ae40ef7 ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_720398bc1ae40ef7 =
  \x0 ->
    \x1 ->
      hs_bindgen_720398bc1ae40ef7_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_720398bc1ae40ef7

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_after7@
foreign import ccall safe "hs_bindgen_ec3c1cc195885cbd" hs_bindgen_ec3c1cc195885cbd_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_after7@
hs_bindgen_ec3c1cc195885cbd ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_ec3c1cc195885cbd =
  \x0 ->
    \x1 ->
      hs_bindgen_ec3c1cc195885cbd_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_ec3c1cc195885cbd

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_after8@
foreign import ccall safe "hs_bindgen_9be6911ba093b982" hs_bindgen_9be6911ba093b982_base ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_withoutSign_after8@
hs_bindgen_9be6911ba093b982 ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_9be6911ba093b982 =
  \x0 ->
    \x1 ->
      hs_bindgen_9be6911ba093b982_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_9be6911ba093b982

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_args1@
foreign import ccall safe "hs_bindgen_44e9a5835e342626" hs_bindgen_44e9a5835e342626_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_args1@
hs_bindgen_44e9a5835e342626 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_44e9a5835e342626 =
  \x0 ->
    \x1 ->
      hs_bindgen_44e9a5835e342626_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args1 = hs_bindgen_44e9a5835e342626

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_args2@
foreign import ccall safe "hs_bindgen_e6422023ae983f47" hs_bindgen_e6422023ae983f47_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_args2@
hs_bindgen_e6422023ae983f47 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_e6422023ae983f47 =
  \x0 ->
    \x1 ->
      hs_bindgen_e6422023ae983f47_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args2 = hs_bindgen_e6422023ae983f47

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_args3@
foreign import ccall safe "hs_bindgen_d53a5dac318288be" hs_bindgen_d53a5dac318288be_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_args3@
hs_bindgen_d53a5dac318288be ::
     BG.CInt
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_d53a5dac318288be =
  \x0 ->
    \x1 ->
      hs_bindgen_d53a5dac318288be_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args3 = hs_bindgen_d53a5dac318288be

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_args4@
foreign import ccall safe "hs_bindgen_d9617b20966b94c7" hs_bindgen_d9617b20966b94c7_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_args4@
hs_bindgen_d9617b20966b94c7 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_d9617b20966b94c7 =
  \x0 ->
    \x1 ->
      hs_bindgen_d9617b20966b94c7_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args4 = hs_bindgen_d9617b20966b94c7

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_args5@
foreign import ccall safe "hs_bindgen_421266d9a11ec453" hs_bindgen_421266d9a11ec453_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_args5@
hs_bindgen_421266d9a11ec453 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_421266d9a11ec453 =
  \x0 ->
    \x1 ->
      hs_bindgen_421266d9a11ec453_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args5 = hs_bindgen_421266d9a11ec453

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_ret1@
foreign import ccall safe "hs_bindgen_0741d01df1d77bca" hs_bindgen_0741d01df1d77bca_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_ret1@
hs_bindgen_0741d01df1d77bca ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_0741d01df1d77bca =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_0741d01df1d77bca_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h 212:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret1 = hs_bindgen_0741d01df1d77bca

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_ret2@
foreign import ccall safe "hs_bindgen_9bb907f5a9f27ec4" hs_bindgen_9bb907f5a9f27ec4_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_ret2@
hs_bindgen_9bb907f5a9f27ec4 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_9bb907f5a9f27ec4 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_9bb907f5a9f27ec4_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h 213:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret2 = hs_bindgen_9bb907f5a9f27ec4

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_ret3@
foreign import ccall safe "hs_bindgen_d40d79f5eb81613b" hs_bindgen_d40d79f5eb81613b_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_ret3@
hs_bindgen_d40d79f5eb81613b ::
     BG.CInt
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_d40d79f5eb81613b =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_d40d79f5eb81613b_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h 214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
const_pointers_ret3 = hs_bindgen_d40d79f5eb81613b

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_ret4@
foreign import ccall safe "hs_bindgen_ae3a7bce1635a452" hs_bindgen_ae3a7bce1635a452_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_ret4@
hs_bindgen_ae3a7bce1635a452 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_ae3a7bce1635a452 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_ae3a7bce1635a452_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h 215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret4 = hs_bindgen_ae3a7bce1635a452

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_ret5@
foreign import ccall safe "hs_bindgen_e9822e54cd42abd1" hs_bindgen_e9822e54cd42abd1_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_pointers_ret5@
hs_bindgen_e9822e54cd42abd1 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_e9822e54cd42abd1 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_e9822e54cd42abd1_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h 216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret5 = hs_bindgen_e9822e54cd42abd1

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_array_elem1@
foreign import ccall safe "hs_bindgen_07c91e79dae2475d" hs_bindgen_07c91e79dae2475d_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_array_elem1@
hs_bindgen_07c91e79dae2475d ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt))
  -> IO ()
hs_bindgen_07c91e79dae2475d =
  \x0 ->
    hs_bindgen_07c91e79dae2475d_base (BG.toFFIType x0)

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h 244:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem1 = hs_bindgen_07c91e79dae2475d

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_array_elem2@
foreign import ccall safe "hs_bindgen_f8f3fe83e121f030" hs_bindgen_f8f3fe83e121f030_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_array_elem2@
hs_bindgen_f8f3fe83e121f030 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt)))
  -> IO ()
hs_bindgen_f8f3fe83e121f030 =
  \x0 ->
    hs_bindgen_f8f3fe83e121f030_base (BG.toFFIType x0)

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h 245:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem2 = hs_bindgen_f8f3fe83e121f030

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_array_elem3@
foreign import ccall safe "hs_bindgen_d95e9e6469a6b6d7" hs_bindgen_d95e9e6469a6b6d7_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_const_array_elem3@
hs_bindgen_d95e9e6469a6b6d7 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_d95e9e6469a6b6d7 =
  \x0 ->
    hs_bindgen_d95e9e6469a6b6d7_base (BG.toFFIType x0)

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h 246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem3 = hs_bindgen_d95e9e6469a6b6d7

-- __unique:__ @test_macrosreparse_1_empty Example Safe_noParams1@
foreign import ccall safe "hs_bindgen_d9eba1b532f5fe32" hs_bindgen_d9eba1b532f5fe32_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse_1_empty Example Safe_noParams1@
hs_bindgen_d9eba1b532f5fe32 :: IO BG.CInt
hs_bindgen_d9eba1b532f5fe32 =
  fmap BG.fromFFIType hs_bindgen_d9eba1b532f5fe32_base

{-| Other examples we reparsed /incorrectly/ before language-c

    __C declaration:__ @noParams1@

    __defined at:__ @macros\/reparse.h 254:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams1 :: IO BG.CInt
noParams1 = hs_bindgen_d9eba1b532f5fe32

-- __unique:__ @test_macrosreparse_1_empty Example Safe_noParams2@
foreign import ccall safe "hs_bindgen_83e24e70c374fa3f" hs_bindgen_83e24e70c374fa3f_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse_1_empty Example Safe_noParams2@
hs_bindgen_83e24e70c374fa3f :: IO BG.CInt
hs_bindgen_83e24e70c374fa3f =
  fmap BG.fromFFIType hs_bindgen_83e24e70c374fa3f_base

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h 255:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2 :: IO BG.CInt
noParams2 = hs_bindgen_83e24e70c374fa3f

-- __unique:__ @test_macrosreparse_1_empty Example Safe_noParams3@
foreign import ccall safe "hs_bindgen_e31cd37caf95eafb" hs_bindgen_e31cd37caf95eafb_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Safe_noParams3@
hs_bindgen_e31cd37caf95eafb ::
     BG.CInt
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_e31cd37caf95eafb =
  \x0 ->
    \x1 ->
      hs_bindgen_e31cd37caf95eafb_base (BG.toFFIType x0) (BG.toFFIType x1)

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
noParams3 = hs_bindgen_e31cd37caf95eafb

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret1@
foreign import ccall safe "hs_bindgen_bdd4a6a2cda990c6" hs_bindgen_bdd4a6a2cda990c6_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret1@
hs_bindgen_bdd4a6a2cda990c6 ::
     BG.CInt
  -> IO (BG.FunPtr (IO ()))
hs_bindgen_bdd4a6a2cda990c6 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_bdd4a6a2cda990c6_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h 260:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO ()))
funptr_ret1 = hs_bindgen_bdd4a6a2cda990c6

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret2@
foreign import ccall safe "hs_bindgen_c602095d4bab5664" hs_bindgen_c602095d4bab5664_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret2@
hs_bindgen_c602095d4bab5664 ::
     BG.CInt
  -> IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_c602095d4bab5664 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_c602095d4bab5664_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h 261:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO BG.CInt))
funptr_ret2 = hs_bindgen_c602095d4bab5664

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret3@
foreign import ccall safe "hs_bindgen_96be7558d494a657" hs_bindgen_96be7558d494a657_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret3@
hs_bindgen_96be7558d494a657 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_96be7558d494a657 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_96be7558d494a657_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h 262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
funptr_ret3 = hs_bindgen_96be7558d494a657

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret4@
foreign import ccall safe "hs_bindgen_56223c665cfb28ac" hs_bindgen_56223c665cfb28ac_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret4@
hs_bindgen_56223c665cfb28ac ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
hs_bindgen_56223c665cfb28ac =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_56223c665cfb28ac_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h 263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
funptr_ret4 = hs_bindgen_56223c665cfb28ac

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret5@
foreign import ccall safe "hs_bindgen_3c78cc60eefc439f" hs_bindgen_3c78cc60eefc439f_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret5@
hs_bindgen_3c78cc60eefc439f ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_3c78cc60eefc439f =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_3c78cc60eefc439f_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h 267:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret5 = hs_bindgen_3c78cc60eefc439f

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret6@
foreign import ccall safe "hs_bindgen_6e26f40164d44434" hs_bindgen_6e26f40164d44434_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret6@
hs_bindgen_6e26f40164d44434 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_6e26f40164d44434 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_6e26f40164d44434_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h 268:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret6 = hs_bindgen_6e26f40164d44434

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret7@
foreign import ccall safe "hs_bindgen_8394035dfd669e5e" hs_bindgen_8394035dfd669e5e_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret7@
hs_bindgen_8394035dfd669e5e ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_8394035dfd669e5e =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_8394035dfd669e5e_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h 269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret7 = hs_bindgen_8394035dfd669e5e

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret8@
foreign import ccall safe "hs_bindgen_21bf97251465e190" hs_bindgen_21bf97251465e190_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret8@
hs_bindgen_21bf97251465e190 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_21bf97251465e190 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_21bf97251465e190_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h 270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret8 = hs_bindgen_21bf97251465e190

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret9@
foreign import ccall safe "hs_bindgen_071425e04d63795c" hs_bindgen_071425e04d63795c_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret9@
hs_bindgen_071425e04d63795c ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_071425e04d63795c =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_071425e04d63795c_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h 271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret9 = hs_bindgen_071425e04d63795c

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret10@
foreign import ccall safe "hs_bindgen_5d17bf2c3be76953" hs_bindgen_5d17bf2c3be76953_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Safe_funptr_ret10@
hs_bindgen_5d17bf2c3be76953 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_5d17bf2c3be76953 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_5d17bf2c3be76953_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h 272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret10 = hs_bindgen_5d17bf2c3be76953
