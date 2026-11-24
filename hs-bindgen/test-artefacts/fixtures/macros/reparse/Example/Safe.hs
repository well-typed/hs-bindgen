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
  , "void hs_bindgen_3a476058cda938af ("
  , "  A arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  (args_char1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d9b1a0fc59c071f5 ("
  , "  A arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  (args_char2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6d6f6445e5eafdd7 ("
  , "  A arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  (args_char3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d7b459b4d7f50043 ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e60c46f104778289 ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1e6b0f6166983369 ("
  , "  A arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  (args_short3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8931e6ab8080e136 ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_db1e1d27cc5a442b ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_15d0657460dcb4ae ("
  , "  A arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  (args_int3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0cc7c367620ffd1c ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d41859c96706a6f6 ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a96b8f1d65aa4f94 ("
  , "  A arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  (args_long3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9620daf2a2194c2a ("
  , "  A arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  (args_float)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e3c77f9b3bf27e75 ("
  , "  A arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  (args_double)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6feb6b7d0766d6a0 ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (args_bool1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_78ca051f0b26fa01 ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  (args_struct)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_8cf009f297f4d932 ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  (args_union)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_16fe4b90a185c1b0 ("
  , "  A arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  (args_enum)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0b3fb7ec47cad9f1 ("
  , "  A arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  (args_pointer1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c6b7bccf92fe7afb ("
  , "  A arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  (args_pointer2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c81adf35587ea25c ("
  , "  A arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  (args_pointer3)(arg1, arg2);"
  , "}"
  , "A hs_bindgen_33c55ff6cccf4953 (void)"
  , "{"
  , "  return (ret_A)();"
  , "}"
  , "char hs_bindgen_3cad41966d0c6aa7 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_char1)(arg1);"
  , "}"
  , "signed char hs_bindgen_cdafe95ef3e05818 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_char2)(arg1);"
  , "}"
  , "unsigned char hs_bindgen_aea89c292d4fa838 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_char3)(arg1);"
  , "}"
  , "signed short hs_bindgen_89a7ec2e95530405 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_short1)(arg1);"
  , "}"
  , "signed short hs_bindgen_878d7cdc7d26ea81 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_short2)(arg1);"
  , "}"
  , "unsigned short hs_bindgen_28ac686ccd8a3d56 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_short3)(arg1);"
  , "}"
  , "signed int hs_bindgen_4f3f9fd93e762c5a ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_int1)(arg1);"
  , "}"
  , "signed int hs_bindgen_67fbe1b60197b785 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_int2)(arg1);"
  , "}"
  , "unsigned int hs_bindgen_c1a6340942c45046 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_int3)(arg1);"
  , "}"
  , "signed long hs_bindgen_1358d587fc2f6ce7 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_long1)(arg1);"
  , "}"
  , "signed long hs_bindgen_c93ab7cbdbf0b5df ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_long2)(arg1);"
  , "}"
  , "unsigned long hs_bindgen_43649e5c5cfd1c31 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_long3)(arg1);"
  , "}"
  , "float hs_bindgen_e877d96c7c18cd1e ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_float)(arg1);"
  , "}"
  , "double hs_bindgen_a083fcf3a5438f1d ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_double)(arg1);"
  , "}"
  , "_Bool hs_bindgen_e2f92a2777eb7633 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_bool1)(arg1);"
  , "}"
  , "void hs_bindgen_fb68480121d945a0 ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_struct)(arg1);"
  , "}"
  , "void hs_bindgen_c711cd771dd85590 ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_union)(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_545d43da370dca97 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_enum)(arg1);"
  , "}"
  , "signed int *hs_bindgen_7de5d2c13b70e1d4 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_pointer1)(arg1);"
  , "}"
  , "signed int **hs_bindgen_51c53c5d0f4a60da ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_pointer2)(arg1);"
  , "}"
  , "void *hs_bindgen_4487d564df82ca92 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_pointer3)(arg1);"
  , "}"
  , "signed int hs_bindgen_bfb360f46bd2a1c0 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (body1)(arg1);"
  , "}"
  , "A hs_bindgen_6caf43a7b112e612 (void)"
  , "{"
  , "  return (body2)();"
  , "}"
  , "void hs_bindgen_65a4d4f2930d29c6 ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_float)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_e8dee68c0d7eda09 ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_double)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_30fac12ff3d8dc83 ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_float)(arg1);"
  , "}"
  , "void hs_bindgen_d51aa0776f51f088 ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_double)(arg1);"
  , "}"
  , "void hs_bindgen_1a0701096cdaec45 ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (bespoke_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_09dfe450c3e481aa ("
  , "  A arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  (bespoke_args2)(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_61f376dc402e84da ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret1)(arg1);"
  , "}"
  , "size_t hs_bindgen_66ba9638c1c516e0 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret2)(arg1);"
  , "}"
  , "void hs_bindgen_056a40b117cb0eca ("
  , "  A arg1[]"
  , ")"
  , "{"
  , "  (arr_args1)(arg1);"
  , "}"
  , "void hs_bindgen_b22f691f9d26234a ("
  , "  A *arg1[]"
  , ")"
  , "{"
  , "  (arr_args2)(arg1);"
  , "}"
  , "void hs_bindgen_f0e709795e066d46 ("
  , "  A arg1[5]"
  , ")"
  , "{"
  , "  (arr_args3)(arg1);"
  , "}"
  , "void hs_bindgen_4ed186148e919afa ("
  , "  A *arg1[5]"
  , ")"
  , "{"
  , "  (arr_args4)(arg1);"
  , "}"
  , "void hs_bindgen_dcfd81389676918b ("
  , "  A arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8dd1f360424fa332 ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8f94af486503dfe1 ("
  , "  A arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_dc9615b72723e985 ("
  , "  A arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_09458e053913c7fd ("
  , "  A arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args5)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4daadd7cf3faa7dd ("
  , "  A arg1"
  , ")"
  , "{"
  , "  (comments1)(arg1);"
  , "}"
  , "void hs_bindgen_5f94b1ed55c285dc ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8e407664f38dce84 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9be108c96938d38e ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9ee3ac90e80a39b2 ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9f6602dd835605b9 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6ae76ad97b923cb7 ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_864aea6b054800eb ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_cba07f437b6a83d5 ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e4b05eb9316faa11 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1641ea63d7cf16bf ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_99341d1f0f66e983 ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_25931db5d22f5024 ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_edc1fe66df8e06e1 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2c34f0edfb1ef769 ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_44c39e44cd8e1ab6 ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_cb4c1b1e13ccea8c ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f4c3a56d7a7aa77f ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_fdbf42e6f627f3df ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_e134b318a024c652 ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_b75eb57498380787 ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f6b7a0773d24a96c ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_5bbc86f02e668a8e ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6f80a5ce0ecdb409 ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9602684e37564ec3 ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8500c3b7013b4ace ("
  , "  A arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a1112056987b1aa1 ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d9b34592aa13daf5 ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args5)(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_5bb31b051b32c12a ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret1)(arg1);"
  , "}"
  , "signed int const *hs_bindgen_790c75f565a590c2 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret2)(arg1);"
  , "}"
  , "signed int *const hs_bindgen_e04d4996fbe914e8 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret3)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_3a22b33ced27c18c ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret4)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_1d34f929d08623a8 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret5)(arg1);"
  , "}"
  , "void hs_bindgen_5857a4aef0d7365d ("
  , "  A const arg1[]"
  , ")"
  , "{"
  , "  (const_array_elem1)(arg1);"
  , "}"
  , "void hs_bindgen_a6f8291807edf5d3 ("
  , "  A const *arg1[]"
  , ")"
  , "{"
  , "  (const_array_elem2)(arg1);"
  , "}"
  , "void hs_bindgen_7ed879af0d6a37c8 ("
  , "  A *const arg1[]"
  , ")"
  , "{"
  , "  (const_array_elem3)(arg1);"
  , "}"
  , "A hs_bindgen_38eda369e92f2c78 (void)"
  , "{"
  , "  return (noParams1)();"
  , "}"
  , "A hs_bindgen_159789b013e14fc1 (void)"
  , "{"
  , "  return (noParams2)();"
  , "}"
  , "void hs_bindgen_3bff7c28e14b8cc5 ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (noParams3)(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_4c0491976152fbfa ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_b742d6ab451be38c ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret2)(arg1);"
  , "}"
  , "void (*hs_bindgen_f53b80280c85cb13 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (funptr_ret3)(arg1);"
  , "}"
  , "char (*hs_bindgen_c9ae2f0f42e51b1d ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret4)(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_83ed96e7baee5141 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret5)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_592be16b23695a01 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret6)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_0eb8787a6e05fcf6 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret7)(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_3da136b1dfc9cef5 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret8)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_8903c97931572f4c ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret9)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_353b41c2a087e8d9 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret10)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse Example Safe_args_char1@
foreign import ccall safe "hs_bindgen_3a476058cda938af" hs_bindgen_3a476058cda938af_base ::
     BG.CInt
  -> BG.CChar
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_char1@
hs_bindgen_3a476058cda938af ::
     A
  -> BG.CChar
  -> IO ()
hs_bindgen_3a476058cda938af =
  \x0 ->
    \x1 ->
      hs_bindgen_3a476058cda938af_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_char1 = hs_bindgen_3a476058cda938af

-- __unique:__ @test_macrosreparse Example Safe_args_char2@
foreign import ccall safe "hs_bindgen_d9b1a0fc59c071f5" hs_bindgen_d9b1a0fc59c071f5_base ::
     BG.CInt
  -> BG.CSChar
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_char2@
hs_bindgen_d9b1a0fc59c071f5 ::
     A
  -> BG.CSChar
  -> IO ()
hs_bindgen_d9b1a0fc59c071f5 =
  \x0 ->
    \x1 ->
      hs_bindgen_d9b1a0fc59c071f5_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_char2 = hs_bindgen_d9b1a0fc59c071f5

-- __unique:__ @test_macrosreparse Example Safe_args_char3@
foreign import ccall safe "hs_bindgen_6d6f6445e5eafdd7" hs_bindgen_6d6f6445e5eafdd7_base ::
     BG.CInt
  -> BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_char3@
hs_bindgen_6d6f6445e5eafdd7 ::
     A
  -> BG.CUChar
  -> IO ()
hs_bindgen_6d6f6445e5eafdd7 =
  \x0 ->
    \x1 ->
      hs_bindgen_6d6f6445e5eafdd7_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_char3 = hs_bindgen_6d6f6445e5eafdd7

-- __unique:__ @test_macrosreparse Example Safe_args_short1@
foreign import ccall safe "hs_bindgen_d7b459b4d7f50043" hs_bindgen_d7b459b4d7f50043_base ::
     BG.CInt
  -> BG.CShort
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_short1@
hs_bindgen_d7b459b4d7f50043 ::
     A
  -> BG.CShort
  -> IO ()
hs_bindgen_d7b459b4d7f50043 =
  \x0 ->
    \x1 ->
      hs_bindgen_d7b459b4d7f50043_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_short1 = hs_bindgen_d7b459b4d7f50043

-- __unique:__ @test_macrosreparse Example Safe_args_short2@
foreign import ccall safe "hs_bindgen_e60c46f104778289" hs_bindgen_e60c46f104778289_base ::
     BG.CInt
  -> BG.CShort
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_short2@
hs_bindgen_e60c46f104778289 ::
     A
  -> BG.CShort
  -> IO ()
hs_bindgen_e60c46f104778289 =
  \x0 ->
    \x1 ->
      hs_bindgen_e60c46f104778289_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_short2 = hs_bindgen_e60c46f104778289

-- __unique:__ @test_macrosreparse Example Safe_args_short3@
foreign import ccall safe "hs_bindgen_1e6b0f6166983369" hs_bindgen_1e6b0f6166983369_base ::
     BG.CInt
  -> BG.CUShort
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_short3@
hs_bindgen_1e6b0f6166983369 ::
     A
  -> BG.CUShort
  -> IO ()
hs_bindgen_1e6b0f6166983369 =
  \x0 ->
    \x1 ->
      hs_bindgen_1e6b0f6166983369_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_short3 = hs_bindgen_1e6b0f6166983369

-- __unique:__ @test_macrosreparse Example Safe_args_int1@
foreign import ccall safe "hs_bindgen_8931e6ab8080e136" hs_bindgen_8931e6ab8080e136_base ::
     BG.CInt
  -> BG.CInt
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_int1@
hs_bindgen_8931e6ab8080e136 ::
     A
  -> BG.CInt
  -> IO ()
hs_bindgen_8931e6ab8080e136 =
  \x0 ->
    \x1 ->
      hs_bindgen_8931e6ab8080e136_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_int1 = hs_bindgen_8931e6ab8080e136

-- __unique:__ @test_macrosreparse Example Safe_args_int2@
foreign import ccall safe "hs_bindgen_db1e1d27cc5a442b" hs_bindgen_db1e1d27cc5a442b_base ::
     BG.CInt
  -> BG.CInt
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_int2@
hs_bindgen_db1e1d27cc5a442b ::
     A
  -> BG.CInt
  -> IO ()
hs_bindgen_db1e1d27cc5a442b =
  \x0 ->
    \x1 ->
      hs_bindgen_db1e1d27cc5a442b_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_int2 = hs_bindgen_db1e1d27cc5a442b

-- __unique:__ @test_macrosreparse Example Safe_args_int3@
foreign import ccall safe "hs_bindgen_15d0657460dcb4ae" hs_bindgen_15d0657460dcb4ae_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_int3@
hs_bindgen_15d0657460dcb4ae ::
     A
  -> BG.CUInt
  -> IO ()
hs_bindgen_15d0657460dcb4ae =
  \x0 ->
    \x1 ->
      hs_bindgen_15d0657460dcb4ae_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_int3 = hs_bindgen_15d0657460dcb4ae

-- __unique:__ @test_macrosreparse Example Safe_args_long1@
foreign import ccall safe "hs_bindgen_0cc7c367620ffd1c" hs_bindgen_0cc7c367620ffd1c_base ::
     BG.CInt
  -> BG.CLong
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_long1@
hs_bindgen_0cc7c367620ffd1c ::
     A
  -> BG.CLong
  -> IO ()
hs_bindgen_0cc7c367620ffd1c =
  \x0 ->
    \x1 ->
      hs_bindgen_0cc7c367620ffd1c_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_long1 = hs_bindgen_0cc7c367620ffd1c

-- __unique:__ @test_macrosreparse Example Safe_args_long2@
foreign import ccall safe "hs_bindgen_d41859c96706a6f6" hs_bindgen_d41859c96706a6f6_base ::
     BG.CInt
  -> BG.CLong
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_long2@
hs_bindgen_d41859c96706a6f6 ::
     A
  -> BG.CLong
  -> IO ()
hs_bindgen_d41859c96706a6f6 =
  \x0 ->
    \x1 ->
      hs_bindgen_d41859c96706a6f6_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_long2 = hs_bindgen_d41859c96706a6f6

-- __unique:__ @test_macrosreparse Example Safe_args_long3@
foreign import ccall safe "hs_bindgen_a96b8f1d65aa4f94" hs_bindgen_a96b8f1d65aa4f94_base ::
     BG.CInt
  -> BG.CULong
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_long3@
hs_bindgen_a96b8f1d65aa4f94 ::
     A
  -> BG.CULong
  -> IO ()
hs_bindgen_a96b8f1d65aa4f94 =
  \x0 ->
    \x1 ->
      hs_bindgen_a96b8f1d65aa4f94_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_long3 = hs_bindgen_a96b8f1d65aa4f94

-- __unique:__ @test_macrosreparse Example Safe_args_float@
foreign import ccall safe "hs_bindgen_9620daf2a2194c2a" hs_bindgen_9620daf2a2194c2a_base ::
     BG.CInt
  -> BG.CFloat
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_float@
hs_bindgen_9620daf2a2194c2a ::
     A
  -> BG.CFloat
  -> IO ()
hs_bindgen_9620daf2a2194c2a =
  \x0 ->
    \x1 ->
      hs_bindgen_9620daf2a2194c2a_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_float = hs_bindgen_9620daf2a2194c2a

-- __unique:__ @test_macrosreparse Example Safe_args_double@
foreign import ccall safe "hs_bindgen_e3c77f9b3bf27e75" hs_bindgen_e3c77f9b3bf27e75_base ::
     BG.CInt
  -> BG.CDouble
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_double@
hs_bindgen_e3c77f9b3bf27e75 ::
     A
  -> BG.CDouble
  -> IO ()
hs_bindgen_e3c77f9b3bf27e75 =
  \x0 ->
    \x1 ->
      hs_bindgen_e3c77f9b3bf27e75_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_double = hs_bindgen_e3c77f9b3bf27e75

-- __unique:__ @test_macrosreparse Example Safe_args_bool1@
foreign import ccall safe "hs_bindgen_6feb6b7d0766d6a0" hs_bindgen_6feb6b7d0766d6a0_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_bool1@
hs_bindgen_6feb6b7d0766d6a0 ::
     A
  -> BG.CBool
  -> IO ()
hs_bindgen_6feb6b7d0766d6a0 =
  \x0 ->
    \x1 ->
      hs_bindgen_6feb6b7d0766d6a0_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_bool1 = hs_bindgen_6feb6b7d0766d6a0

-- __unique:__ @test_macrosreparse Example Safe_args_struct@
foreign import ccall safe "hs_bindgen_78ca051f0b26fa01" hs_bindgen_78ca051f0b26fa01_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_struct@
hs_bindgen_78ca051f0b26fa01 ::
     A
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_78ca051f0b26fa01 =
  \x0 ->
    \x1 ->
      hs_bindgen_78ca051f0b26fa01_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_78ca051f0b26fa01 arg10 arg22)

-- __unique:__ @test_macrosreparse Example Safe_args_union@
foreign import ccall safe "hs_bindgen_8cf009f297f4d932" hs_bindgen_8cf009f297f4d932_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_union@
hs_bindgen_8cf009f297f4d932 ::
     A
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_8cf009f297f4d932 =
  \x0 ->
    \x1 ->
      hs_bindgen_8cf009f297f4d932_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_8cf009f297f4d932 arg10 arg22)

-- __unique:__ @test_macrosreparse Example Safe_args_enum@
foreign import ccall safe "hs_bindgen_16fe4b90a185c1b0" hs_bindgen_16fe4b90a185c1b0_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_enum@
hs_bindgen_16fe4b90a185c1b0 ::
     A
  -> Some_enum
  -> IO ()
hs_bindgen_16fe4b90a185c1b0 =
  \x0 ->
    \x1 ->
      hs_bindgen_16fe4b90a185c1b0_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_enum = hs_bindgen_16fe4b90a185c1b0

-- __unique:__ @test_macrosreparse Example Safe_args_pointer1@
foreign import ccall safe "hs_bindgen_0b3fb7ec47cad9f1" hs_bindgen_0b3fb7ec47cad9f1_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_pointer1@
hs_bindgen_0b3fb7ec47cad9f1 ::
     A
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_0b3fb7ec47cad9f1 =
  \x0 ->
    \x1 ->
      hs_bindgen_0b3fb7ec47cad9f1_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_pointer1 = hs_bindgen_0b3fb7ec47cad9f1

-- __unique:__ @test_macrosreparse Example Safe_args_pointer2@
foreign import ccall safe "hs_bindgen_c6b7bccf92fe7afb" hs_bindgen_c6b7bccf92fe7afb_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_pointer2@
hs_bindgen_c6b7bccf92fe7afb ::
     A
  -> BG.Ptr (BG.Ptr BG.CInt)
  -> IO ()
hs_bindgen_c6b7bccf92fe7afb =
  \x0 ->
    \x1 ->
      hs_bindgen_c6b7bccf92fe7afb_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_pointer2 = hs_bindgen_c6b7bccf92fe7afb

-- __unique:__ @test_macrosreparse Example Safe_args_pointer3@
foreign import ccall safe "hs_bindgen_c81adf35587ea25c" hs_bindgen_c81adf35587ea25c_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_pointer3@
hs_bindgen_c81adf35587ea25c ::
     A
  -> BG.Ptr BG.Void
  -> IO ()
hs_bindgen_c81adf35587ea25c =
  \x0 ->
    \x1 ->
      hs_bindgen_c81adf35587ea25c_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_pointer3 = hs_bindgen_c81adf35587ea25c

-- __unique:__ @test_macrosreparse Example Safe_ret_A@
foreign import ccall safe "hs_bindgen_33c55ff6cccf4953" hs_bindgen_33c55ff6cccf4953_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse Example Safe_ret_A@
hs_bindgen_33c55ff6cccf4953 :: IO A
hs_bindgen_33c55ff6cccf4953 =
  fmap BG.fromFFIType hs_bindgen_33c55ff6cccf4953_base

{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h 47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A :: IO A
ret_A = hs_bindgen_33c55ff6cccf4953

-- __unique:__ @test_macrosreparse Example Safe_ret_char1@
foreign import ccall safe "hs_bindgen_3cad41966d0c6aa7" hs_bindgen_3cad41966d0c6aa7_base ::
     BG.CInt
  -> IO BG.CChar

-- __unique:__ @test_macrosreparse Example Safe_ret_char1@
hs_bindgen_3cad41966d0c6aa7 ::
     A
  -> IO BG.CChar
hs_bindgen_3cad41966d0c6aa7 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_3cad41966d0c6aa7_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h 49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CChar
ret_char1 = hs_bindgen_3cad41966d0c6aa7

-- __unique:__ @test_macrosreparse Example Safe_ret_char2@
foreign import ccall safe "hs_bindgen_cdafe95ef3e05818" hs_bindgen_cdafe95ef3e05818_base ::
     BG.CInt
  -> IO BG.CSChar

-- __unique:__ @test_macrosreparse Example Safe_ret_char2@
hs_bindgen_cdafe95ef3e05818 ::
     A
  -> IO BG.CSChar
hs_bindgen_cdafe95ef3e05818 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_cdafe95ef3e05818_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h 50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CSChar
ret_char2 = hs_bindgen_cdafe95ef3e05818

-- __unique:__ @test_macrosreparse Example Safe_ret_char3@
foreign import ccall safe "hs_bindgen_aea89c292d4fa838" hs_bindgen_aea89c292d4fa838_base ::
     BG.CInt
  -> IO BG.CUChar

-- __unique:__ @test_macrosreparse Example Safe_ret_char3@
hs_bindgen_aea89c292d4fa838 ::
     A
  -> IO BG.CUChar
hs_bindgen_aea89c292d4fa838 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_aea89c292d4fa838_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h 51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUChar
ret_char3 = hs_bindgen_aea89c292d4fa838

-- __unique:__ @test_macrosreparse Example Safe_ret_short1@
foreign import ccall safe "hs_bindgen_89a7ec2e95530405" hs_bindgen_89a7ec2e95530405_base ::
     BG.CInt
  -> IO BG.CShort

-- __unique:__ @test_macrosreparse Example Safe_ret_short1@
hs_bindgen_89a7ec2e95530405 ::
     A
  -> IO BG.CShort
hs_bindgen_89a7ec2e95530405 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_89a7ec2e95530405_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h 53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short1 = hs_bindgen_89a7ec2e95530405

-- __unique:__ @test_macrosreparse Example Safe_ret_short2@
foreign import ccall safe "hs_bindgen_878d7cdc7d26ea81" hs_bindgen_878d7cdc7d26ea81_base ::
     BG.CInt
  -> IO BG.CShort

-- __unique:__ @test_macrosreparse Example Safe_ret_short2@
hs_bindgen_878d7cdc7d26ea81 ::
     A
  -> IO BG.CShort
hs_bindgen_878d7cdc7d26ea81 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_878d7cdc7d26ea81_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h 54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short2 = hs_bindgen_878d7cdc7d26ea81

-- __unique:__ @test_macrosreparse Example Safe_ret_short3@
foreign import ccall safe "hs_bindgen_28ac686ccd8a3d56" hs_bindgen_28ac686ccd8a3d56_base ::
     BG.CInt
  -> IO BG.CUShort

-- __unique:__ @test_macrosreparse Example Safe_ret_short3@
hs_bindgen_28ac686ccd8a3d56 ::
     A
  -> IO BG.CUShort
hs_bindgen_28ac686ccd8a3d56 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_28ac686ccd8a3d56_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h 55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUShort
ret_short3 = hs_bindgen_28ac686ccd8a3d56

-- __unique:__ @test_macrosreparse Example Safe_ret_int1@
foreign import ccall safe "hs_bindgen_4f3f9fd93e762c5a" hs_bindgen_4f3f9fd93e762c5a_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparse Example Safe_ret_int1@
hs_bindgen_4f3f9fd93e762c5a ::
     A
  -> IO BG.CInt
hs_bindgen_4f3f9fd93e762c5a =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_4f3f9fd93e762c5a_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h 57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int1 = hs_bindgen_4f3f9fd93e762c5a

-- __unique:__ @test_macrosreparse Example Safe_ret_int2@
foreign import ccall safe "hs_bindgen_67fbe1b60197b785" hs_bindgen_67fbe1b60197b785_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparse Example Safe_ret_int2@
hs_bindgen_67fbe1b60197b785 ::
     A
  -> IO BG.CInt
hs_bindgen_67fbe1b60197b785 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_67fbe1b60197b785_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h 58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int2 = hs_bindgen_67fbe1b60197b785

-- __unique:__ @test_macrosreparse Example Safe_ret_int3@
foreign import ccall safe "hs_bindgen_c1a6340942c45046" hs_bindgen_c1a6340942c45046_base ::
     BG.CInt
  -> IO BG.CUInt

-- __unique:__ @test_macrosreparse Example Safe_ret_int3@
hs_bindgen_c1a6340942c45046 ::
     A
  -> IO BG.CUInt
hs_bindgen_c1a6340942c45046 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_c1a6340942c45046_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h 59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUInt
ret_int3 = hs_bindgen_c1a6340942c45046

-- __unique:__ @test_macrosreparse Example Safe_ret_long1@
foreign import ccall safe "hs_bindgen_1358d587fc2f6ce7" hs_bindgen_1358d587fc2f6ce7_base ::
     BG.CInt
  -> IO BG.CLong

-- __unique:__ @test_macrosreparse Example Safe_ret_long1@
hs_bindgen_1358d587fc2f6ce7 ::
     A
  -> IO BG.CLong
hs_bindgen_1358d587fc2f6ce7 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_1358d587fc2f6ce7_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h 61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long1 = hs_bindgen_1358d587fc2f6ce7

-- __unique:__ @test_macrosreparse Example Safe_ret_long2@
foreign import ccall safe "hs_bindgen_c93ab7cbdbf0b5df" hs_bindgen_c93ab7cbdbf0b5df_base ::
     BG.CInt
  -> IO BG.CLong

-- __unique:__ @test_macrosreparse Example Safe_ret_long2@
hs_bindgen_c93ab7cbdbf0b5df ::
     A
  -> IO BG.CLong
hs_bindgen_c93ab7cbdbf0b5df =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_c93ab7cbdbf0b5df_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h 62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long2 = hs_bindgen_c93ab7cbdbf0b5df

-- __unique:__ @test_macrosreparse Example Safe_ret_long3@
foreign import ccall safe "hs_bindgen_43649e5c5cfd1c31" hs_bindgen_43649e5c5cfd1c31_base ::
     BG.CInt
  -> IO BG.CULong

-- __unique:__ @test_macrosreparse Example Safe_ret_long3@
hs_bindgen_43649e5c5cfd1c31 ::
     A
  -> IO BG.CULong
hs_bindgen_43649e5c5cfd1c31 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_43649e5c5cfd1c31_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h 63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CULong
ret_long3 = hs_bindgen_43649e5c5cfd1c31

-- __unique:__ @test_macrosreparse Example Safe_ret_float@
foreign import ccall safe "hs_bindgen_e877d96c7c18cd1e" hs_bindgen_e877d96c7c18cd1e_base ::
     BG.CInt
  -> IO BG.CFloat

-- __unique:__ @test_macrosreparse Example Safe_ret_float@
hs_bindgen_e877d96c7c18cd1e ::
     A
  -> IO BG.CFloat
hs_bindgen_e877d96c7c18cd1e =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_e877d96c7c18cd1e_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h 65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CFloat
ret_float = hs_bindgen_e877d96c7c18cd1e

-- __unique:__ @test_macrosreparse Example Safe_ret_double@
foreign import ccall safe "hs_bindgen_a083fcf3a5438f1d" hs_bindgen_a083fcf3a5438f1d_base ::
     BG.CInt
  -> IO BG.CDouble

-- __unique:__ @test_macrosreparse Example Safe_ret_double@
hs_bindgen_a083fcf3a5438f1d ::
     A
  -> IO BG.CDouble
hs_bindgen_a083fcf3a5438f1d =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_a083fcf3a5438f1d_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h 66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CDouble
ret_double = hs_bindgen_a083fcf3a5438f1d

-- __unique:__ @test_macrosreparse Example Safe_ret_bool1@
foreign import ccall safe "hs_bindgen_e2f92a2777eb7633" hs_bindgen_e2f92a2777eb7633_base ::
     BG.CInt
  -> IO BG.CBool

-- __unique:__ @test_macrosreparse Example Safe_ret_bool1@
hs_bindgen_e2f92a2777eb7633 ::
     A
  -> IO BG.CBool
hs_bindgen_e2f92a2777eb7633 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_e2f92a2777eb7633_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h 67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
ret_bool1 = hs_bindgen_e2f92a2777eb7633

-- __unique:__ @test_macrosreparse Example Safe_ret_struct@
foreign import ccall safe "hs_bindgen_fb68480121d945a0" hs_bindgen_fb68480121d945a0_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_ret_struct@
hs_bindgen_fb68480121d945a0 ::
     A
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_fb68480121d945a0 =
  \x0 ->
    \x1 ->
      hs_bindgen_fb68480121d945a0_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_fb68480121d945a0 arg10 res1)

-- __unique:__ @test_macrosreparse Example Safe_ret_union@
foreign import ccall safe "hs_bindgen_c711cd771dd85590" hs_bindgen_c711cd771dd85590_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_ret_union@
hs_bindgen_c711cd771dd85590 ::
     A
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_c711cd771dd85590 =
  \x0 ->
    \x1 ->
      hs_bindgen_c711cd771dd85590_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_c711cd771dd85590 arg10 res1)

-- __unique:__ @test_macrosreparse Example Safe_ret_enum@
foreign import ccall safe "hs_bindgen_545d43da370dca97" hs_bindgen_545d43da370dca97_base ::
     BG.CInt
  -> IO BG.CUInt

-- __unique:__ @test_macrosreparse Example Safe_ret_enum@
hs_bindgen_545d43da370dca97 ::
     A
  -> IO Some_enum
hs_bindgen_545d43da370dca97 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_545d43da370dca97_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h 71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO Some_enum
ret_enum = hs_bindgen_545d43da370dca97

-- __unique:__ @test_macrosreparse Example Safe_ret_pointer1@
foreign import ccall safe "hs_bindgen_7de5d2c13b70e1d4" hs_bindgen_7de5d2c13b70e1d4_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse Example Safe_ret_pointer1@
hs_bindgen_7de5d2c13b70e1d4 ::
     A
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_7de5d2c13b70e1d4 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_7de5d2c13b70e1d4_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h 73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
ret_pointer1 = hs_bindgen_7de5d2c13b70e1d4

-- __unique:__ @test_macrosreparse Example Safe_ret_pointer2@
foreign import ccall safe "hs_bindgen_51c53c5d0f4a60da" hs_bindgen_51c53c5d0f4a60da_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse Example Safe_ret_pointer2@
hs_bindgen_51c53c5d0f4a60da ::
     A
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
hs_bindgen_51c53c5d0f4a60da =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_51c53c5d0f4a60da_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h 74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
ret_pointer2 = hs_bindgen_51c53c5d0f4a60da

-- __unique:__ @test_macrosreparse Example Safe_ret_pointer3@
foreign import ccall safe "hs_bindgen_4487d564df82ca92" hs_bindgen_4487d564df82ca92_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse Example Safe_ret_pointer3@
hs_bindgen_4487d564df82ca92 ::
     A
  -> IO (BG.Ptr BG.Void)
hs_bindgen_4487d564df82ca92 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_4487d564df82ca92_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h 75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.Void)
ret_pointer3 = hs_bindgen_4487d564df82ca92

-- __unique:__ @test_macrosreparse Example Safe_body1@
foreign import ccall safe "hs_bindgen_bfb360f46bd2a1c0" hs_bindgen_bfb360f46bd2a1c0_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparse Example Safe_body1@
hs_bindgen_bfb360f46bd2a1c0 ::
     A
  -> IO BG.CInt
hs_bindgen_bfb360f46bd2a1c0 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_bfb360f46bd2a1c0_base (BG.toFFIType x0))

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h 79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
body1 = hs_bindgen_bfb360f46bd2a1c0

-- __unique:__ @test_macrosreparse Example Safe_body2@
foreign import ccall safe "hs_bindgen_6caf43a7b112e612" hs_bindgen_6caf43a7b112e612_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse Example Safe_body2@
hs_bindgen_6caf43a7b112e612 :: IO A
hs_bindgen_6caf43a7b112e612 =
  fmap BG.fromFFIType hs_bindgen_6caf43a7b112e612_base

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h 80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2 :: IO A
body2 = hs_bindgen_6caf43a7b112e612

-- __unique:__ @test_macrosreparse Example Safe_args_complex_float@
foreign import ccall safe "hs_bindgen_65a4d4f2930d29c6" hs_bindgen_65a4d4f2930d29c6_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_complex_float@
hs_bindgen_65a4d4f2930d29c6 ::
     A
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_65a4d4f2930d29c6 =
  \x0 ->
    \x1 ->
      hs_bindgen_65a4d4f2930d29c6_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_65a4d4f2930d29c6 arg10 arg22)

-- __unique:__ @test_macrosreparse Example Safe_args_complex_double@
foreign import ccall safe "hs_bindgen_e8dee68c0d7eda09" hs_bindgen_e8dee68c0d7eda09_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_args_complex_double@
hs_bindgen_e8dee68c0d7eda09 ::
     A
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_e8dee68c0d7eda09 =
  \x0 ->
    \x1 ->
      hs_bindgen_e8dee68c0d7eda09_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_e8dee68c0d7eda09 arg10 arg22)

-- __unique:__ @test_macrosreparse Example Safe_ret_complex_float@
foreign import ccall safe "hs_bindgen_30fac12ff3d8dc83" hs_bindgen_30fac12ff3d8dc83_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_ret_complex_float@
hs_bindgen_30fac12ff3d8dc83 ::
     A
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_30fac12ff3d8dc83 =
  \x0 ->
    \x1 ->
      hs_bindgen_30fac12ff3d8dc83_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_30fac12ff3d8dc83 arg10 res1)

-- __unique:__ @test_macrosreparse Example Safe_ret_complex_double@
foreign import ccall safe "hs_bindgen_d51aa0776f51f088" hs_bindgen_d51aa0776f51f088_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_ret_complex_double@
hs_bindgen_d51aa0776f51f088 ::
     A
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_d51aa0776f51f088 =
  \x0 ->
    \x1 ->
      hs_bindgen_d51aa0776f51f088_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_d51aa0776f51f088 arg10 res1)

-- __unique:__ @test_macrosreparse Example Safe_bespoke_args1@
foreign import ccall safe "hs_bindgen_1a0701096cdaec45" hs_bindgen_1a0701096cdaec45_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_bespoke_args1@
hs_bindgen_1a0701096cdaec45 ::
     A
  -> BG.CBool
  -> IO ()
hs_bindgen_1a0701096cdaec45 =
  \x0 ->
    \x1 ->
      hs_bindgen_1a0701096cdaec45_base (BG.toFFIType x0) (BG.toFFIType x1)

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
bespoke_args1 = hs_bindgen_1a0701096cdaec45

-- __unique:__ @test_macrosreparse Example Safe_bespoke_args2@
foreign import ccall safe "hs_bindgen_09dfe450c3e481aa" hs_bindgen_09dfe450c3e481aa_base ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_bespoke_args2@
hs_bindgen_09dfe450c3e481aa ::
     A
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_09dfe450c3e481aa =
  \x0 ->
    \x1 ->
      hs_bindgen_09dfe450c3e481aa_base (BG.toFFIType x0) (BG.toFFIType x1)

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
bespoke_args2 = hs_bindgen_09dfe450c3e481aa

-- __unique:__ @test_macrosreparse Example Safe_bespoke_ret1@
foreign import ccall safe "hs_bindgen_61f376dc402e84da" hs_bindgen_61f376dc402e84da_base ::
     BG.CInt
  -> IO BG.CBool

-- __unique:__ @test_macrosreparse Example Safe_bespoke_ret1@
hs_bindgen_61f376dc402e84da ::
     A
  -> IO BG.CBool
hs_bindgen_61f376dc402e84da =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_61f376dc402e84da_base (BG.toFFIType x0))

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h 97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
bespoke_ret1 = hs_bindgen_61f376dc402e84da

-- __unique:__ @test_macrosreparse Example Safe_bespoke_ret2@
foreign import ccall safe "hs_bindgen_66ba9638c1c516e0" hs_bindgen_66ba9638c1c516e0_base ::
     BG.CInt
  -> IO HsBindgen.Runtime.LibC.CSize

-- __unique:__ @test_macrosreparse Example Safe_bespoke_ret2@
hs_bindgen_66ba9638c1c516e0 ::
     A
  -> IO HsBindgen.Runtime.LibC.CSize
hs_bindgen_66ba9638c1c516e0 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_66ba9638c1c516e0_base (BG.toFFIType x0))

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h 98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO HsBindgen.Runtime.LibC.CSize
bespoke_ret2 = hs_bindgen_66ba9638c1c516e0

-- __unique:__ @test_macrosreparse Example Safe_arr_args1@
foreign import ccall safe "hs_bindgen_056a40b117cb0eca" hs_bindgen_056a40b117cb0eca_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_arr_args1@
hs_bindgen_056a40b117cb0eca ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray A))
  -> IO ()
hs_bindgen_056a40b117cb0eca =
  \x0 ->
    hs_bindgen_056a40b117cb0eca_base (BG.toFFIType x0)

{-| Arrays

    __C declaration:__ @arr_args1@

    __defined at:__ @macros\/reparse.h 104:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args1 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray A))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args1 = hs_bindgen_056a40b117cb0eca

-- __unique:__ @test_macrosreparse Example Safe_arr_args2@
foreign import ccall safe "hs_bindgen_b22f691f9d26234a" hs_bindgen_b22f691f9d26234a_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_arr_args2@
hs_bindgen_b22f691f9d26234a ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr A)))
  -> IO ()
hs_bindgen_b22f691f9d26234a =
  \x0 ->
    hs_bindgen_b22f691f9d26234a_base (BG.toFFIType x0)

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h 105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args2 = hs_bindgen_b22f691f9d26234a

-- __unique:__ @test_macrosreparse Example Safe_arr_args3@
foreign import ccall safe "hs_bindgen_f0e709795e066d46" hs_bindgen_f0e709795e066d46_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_arr_args3@
hs_bindgen_f0e709795e066d46 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 A))
  -> IO ()
hs_bindgen_f0e709795e066d46 =
  \x0 ->
    hs_bindgen_f0e709795e066d46_base (BG.toFFIType x0)

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h 106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 A))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args3 = hs_bindgen_f0e709795e066d46

-- __unique:__ @test_macrosreparse Example Safe_arr_args4@
foreign import ccall safe "hs_bindgen_4ed186148e919afa" hs_bindgen_4ed186148e919afa_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_arr_args4@
hs_bindgen_4ed186148e919afa ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr A)))
  -> IO ()
hs_bindgen_4ed186148e919afa =
  \x0 ->
    hs_bindgen_4ed186148e919afa_base (BG.toFFIType x0)

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h 107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args4 = hs_bindgen_4ed186148e919afa

-- __unique:__ @test_macrosreparse Example Safe_funptr_args1@
foreign import ccall safe "hs_bindgen_dcfd81389676918b" hs_bindgen_dcfd81389676918b_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_funptr_args1@
hs_bindgen_dcfd81389676918b ::
     A
  -> BG.FunPtr (IO ())
  -> IO ()
hs_bindgen_dcfd81389676918b =
  \x0 ->
    \x1 ->
      hs_bindgen_dcfd81389676918b_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args1 = hs_bindgen_dcfd81389676918b

-- __unique:__ @test_macrosreparse Example Safe_funptr_args2@
foreign import ccall safe "hs_bindgen_8dd1f360424fa332" hs_bindgen_8dd1f360424fa332_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_funptr_args2@
hs_bindgen_8dd1f360424fa332 ::
     A
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_8dd1f360424fa332 =
  \x0 ->
    \x1 ->
      hs_bindgen_8dd1f360424fa332_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args2 = hs_bindgen_8dd1f360424fa332

-- __unique:__ @test_macrosreparse Example Safe_funptr_args3@
foreign import ccall safe "hs_bindgen_8f94af486503dfe1" hs_bindgen_8f94af486503dfe1_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_funptr_args3@
hs_bindgen_8f94af486503dfe1 ::
     A
  -> BG.FunPtr (BG.CInt -> IO ())
  -> IO ()
hs_bindgen_8f94af486503dfe1 =
  \x0 ->
    \x1 ->
      hs_bindgen_8f94af486503dfe1_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args3 = hs_bindgen_8f94af486503dfe1

-- __unique:__ @test_macrosreparse Example Safe_funptr_args4@
foreign import ccall safe "hs_bindgen_dc9615b72723e985" hs_bindgen_dc9615b72723e985_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_funptr_args4@
hs_bindgen_dc9615b72723e985 ::
     A
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar)
  -> IO ()
hs_bindgen_dc9615b72723e985 =
  \x0 ->
    \x1 ->
      hs_bindgen_dc9615b72723e985_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args4 = hs_bindgen_dc9615b72723e985

-- __unique:__ @test_macrosreparse Example Safe_funptr_args5@
foreign import ccall safe "hs_bindgen_09458e053913c7fd" hs_bindgen_09458e053913c7fd_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_funptr_args5@
hs_bindgen_09458e053913c7fd ::
     A
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))
  -> IO ()
hs_bindgen_09458e053913c7fd =
  \x0 ->
    \x1 ->
      hs_bindgen_09458e053913c7fd_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args5 = hs_bindgen_09458e053913c7fd

-- __unique:__ @test_macrosreparse Example Safe_comments1@
foreign import ccall safe "hs_bindgen_4daadd7cf3faa7dd" hs_bindgen_4daadd7cf3faa7dd_base ::
     BG.CInt
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_comments1@
hs_bindgen_4daadd7cf3faa7dd ::
     A
  -> IO ()
hs_bindgen_4daadd7cf3faa7dd =
  \x0 ->
    hs_bindgen_4daadd7cf3faa7dd_base (BG.toFFIType x0)

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
comments1 = hs_bindgen_4daadd7cf3faa7dd

-- __unique:__ @test_macrosreparse Example Safe_const_prim_before1@
foreign import ccall safe "hs_bindgen_5f94b1ed55c285dc" hs_bindgen_5f94b1ed55c285dc_base ::
     BG.CInt
  -> BG.CChar
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_prim_before1@
hs_bindgen_5f94b1ed55c285dc ::
     A
  -> BG.CChar
  -> IO ()
hs_bindgen_5f94b1ed55c285dc =
  \x0 ->
    \x1 ->
      hs_bindgen_5f94b1ed55c285dc_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_before1 = hs_bindgen_5f94b1ed55c285dc

-- __unique:__ @test_macrosreparse Example Safe_const_prim_before2@
foreign import ccall safe "hs_bindgen_8e407664f38dce84" hs_bindgen_8e407664f38dce84_base ::
     BG.CInt
  -> BG.CSChar
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_prim_before2@
hs_bindgen_8e407664f38dce84 ::
     A
  -> BG.CSChar
  -> IO ()
hs_bindgen_8e407664f38dce84 =
  \x0 ->
    \x1 ->
      hs_bindgen_8e407664f38dce84_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_before2 = hs_bindgen_8e407664f38dce84

-- __unique:__ @test_macrosreparse Example Safe_const_prim_before3@
foreign import ccall safe "hs_bindgen_9be108c96938d38e" hs_bindgen_9be108c96938d38e_base ::
     BG.CInt
  -> BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_prim_before3@
hs_bindgen_9be108c96938d38e ::
     A
  -> BG.CUChar
  -> IO ()
hs_bindgen_9be108c96938d38e =
  \x0 ->
    \x1 ->
      hs_bindgen_9be108c96938d38e_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_before3 = hs_bindgen_9be108c96938d38e

-- __unique:__ @test_macrosreparse Example Safe_const_prim_after1@
foreign import ccall safe "hs_bindgen_9ee3ac90e80a39b2" hs_bindgen_9ee3ac90e80a39b2_base ::
     BG.CInt
  -> BG.CChar
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_prim_after1@
hs_bindgen_9ee3ac90e80a39b2 ::
     A
  -> BG.CChar
  -> IO ()
hs_bindgen_9ee3ac90e80a39b2 =
  \x0 ->
    \x1 ->
      hs_bindgen_9ee3ac90e80a39b2_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_after1 = hs_bindgen_9ee3ac90e80a39b2

-- __unique:__ @test_macrosreparse Example Safe_const_prim_after2@
foreign import ccall safe "hs_bindgen_9f6602dd835605b9" hs_bindgen_9f6602dd835605b9_base ::
     BG.CInt
  -> BG.CSChar
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_prim_after2@
hs_bindgen_9f6602dd835605b9 ::
     A
  -> BG.CSChar
  -> IO ()
hs_bindgen_9f6602dd835605b9 =
  \x0 ->
    \x1 ->
      hs_bindgen_9f6602dd835605b9_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_after2 = hs_bindgen_9f6602dd835605b9

-- __unique:__ @test_macrosreparse Example Safe_const_prim_after3@
foreign import ccall safe "hs_bindgen_6ae76ad97b923cb7" hs_bindgen_6ae76ad97b923cb7_base ::
     BG.CInt
  -> BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_prim_after3@
hs_bindgen_6ae76ad97b923cb7 ::
     A
  -> BG.CUChar
  -> IO ()
hs_bindgen_6ae76ad97b923cb7 =
  \x0 ->
    \x1 ->
      hs_bindgen_6ae76ad97b923cb7_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_after3 = hs_bindgen_6ae76ad97b923cb7

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_before1@
foreign import ccall safe "hs_bindgen_864aea6b054800eb" hs_bindgen_864aea6b054800eb_base ::
     BG.CInt
  -> BG.CFloat
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_before1@
hs_bindgen_864aea6b054800eb ::
     A
  -> BG.CFloat
  -> IO ()
hs_bindgen_864aea6b054800eb =
  \x0 ->
    \x1 ->
      hs_bindgen_864aea6b054800eb_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_864aea6b054800eb

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_before2@
foreign import ccall safe "hs_bindgen_cba07f437b6a83d5" hs_bindgen_cba07f437b6a83d5_base ::
     BG.CInt
  -> BG.CDouble
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_before2@
hs_bindgen_cba07f437b6a83d5 ::
     A
  -> BG.CDouble
  -> IO ()
hs_bindgen_cba07f437b6a83d5 =
  \x0 ->
    \x1 ->
      hs_bindgen_cba07f437b6a83d5_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_cba07f437b6a83d5

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_before3@
foreign import ccall safe "hs_bindgen_e4b05eb9316faa11" hs_bindgen_e4b05eb9316faa11_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_before3@
hs_bindgen_e4b05eb9316faa11 ::
     A
  -> BG.CBool
  -> IO ()
hs_bindgen_e4b05eb9316faa11 =
  \x0 ->
    \x1 ->
      hs_bindgen_e4b05eb9316faa11_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_e4b05eb9316faa11

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_before4@
foreign import ccall safe "hs_bindgen_1641ea63d7cf16bf" hs_bindgen_1641ea63d7cf16bf_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_before4@
hs_bindgen_1641ea63d7cf16bf ::
     A
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_1641ea63d7cf16bf =
  \x0 ->
    \x1 ->
      hs_bindgen_1641ea63d7cf16bf_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_1641ea63d7cf16bf arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_before5@
foreign import ccall safe "hs_bindgen_99341d1f0f66e983" hs_bindgen_99341d1f0f66e983_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_before5@
hs_bindgen_99341d1f0f66e983 ::
     A
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_99341d1f0f66e983 =
  \x0 ->
    \x1 ->
      hs_bindgen_99341d1f0f66e983_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_99341d1f0f66e983 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_before6@
foreign import ccall safe "hs_bindgen_25931db5d22f5024" hs_bindgen_25931db5d22f5024_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_before6@
hs_bindgen_25931db5d22f5024 ::
     A
  -> Some_enum
  -> IO ()
hs_bindgen_25931db5d22f5024 =
  \x0 ->
    \x1 ->
      hs_bindgen_25931db5d22f5024_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_25931db5d22f5024

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_before7@
foreign import ccall safe "hs_bindgen_edc1fe66df8e06e1" hs_bindgen_edc1fe66df8e06e1_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_before7@
hs_bindgen_edc1fe66df8e06e1 ::
     A
  -> BG.CBool
  -> IO ()
hs_bindgen_edc1fe66df8e06e1 =
  \x0 ->
    \x1 ->
      hs_bindgen_edc1fe66df8e06e1_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_edc1fe66df8e06e1

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_before8@
foreign import ccall safe "hs_bindgen_2c34f0edfb1ef769" hs_bindgen_2c34f0edfb1ef769_base ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_before8@
hs_bindgen_2c34f0edfb1ef769 ::
     A
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_2c34f0edfb1ef769 =
  \x0 ->
    \x1 ->
      hs_bindgen_2c34f0edfb1ef769_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_2c34f0edfb1ef769

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_after1@
foreign import ccall safe "hs_bindgen_44c39e44cd8e1ab6" hs_bindgen_44c39e44cd8e1ab6_base ::
     BG.CInt
  -> BG.CFloat
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_after1@
hs_bindgen_44c39e44cd8e1ab6 ::
     A
  -> BG.CFloat
  -> IO ()
hs_bindgen_44c39e44cd8e1ab6 =
  \x0 ->
    \x1 ->
      hs_bindgen_44c39e44cd8e1ab6_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_44c39e44cd8e1ab6

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_after2@
foreign import ccall safe "hs_bindgen_cb4c1b1e13ccea8c" hs_bindgen_cb4c1b1e13ccea8c_base ::
     BG.CInt
  -> BG.CDouble
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_after2@
hs_bindgen_cb4c1b1e13ccea8c ::
     A
  -> BG.CDouble
  -> IO ()
hs_bindgen_cb4c1b1e13ccea8c =
  \x0 ->
    \x1 ->
      hs_bindgen_cb4c1b1e13ccea8c_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_cb4c1b1e13ccea8c

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_after3@
foreign import ccall safe "hs_bindgen_f4c3a56d7a7aa77f" hs_bindgen_f4c3a56d7a7aa77f_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_after3@
hs_bindgen_f4c3a56d7a7aa77f ::
     A
  -> BG.CBool
  -> IO ()
hs_bindgen_f4c3a56d7a7aa77f =
  \x0 ->
    \x1 ->
      hs_bindgen_f4c3a56d7a7aa77f_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_f4c3a56d7a7aa77f

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_after4@
foreign import ccall safe "hs_bindgen_fdbf42e6f627f3df" hs_bindgen_fdbf42e6f627f3df_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_after4@
hs_bindgen_fdbf42e6f627f3df ::
     A
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_fdbf42e6f627f3df =
  \x0 ->
    \x1 ->
      hs_bindgen_fdbf42e6f627f3df_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_fdbf42e6f627f3df arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_after5@
foreign import ccall safe "hs_bindgen_e134b318a024c652" hs_bindgen_e134b318a024c652_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_after5@
hs_bindgen_e134b318a024c652 ::
     A
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_e134b318a024c652 =
  \x0 ->
    \x1 ->
      hs_bindgen_e134b318a024c652_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_e134b318a024c652 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_after6@
foreign import ccall safe "hs_bindgen_b75eb57498380787" hs_bindgen_b75eb57498380787_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_after6@
hs_bindgen_b75eb57498380787 ::
     A
  -> Some_enum
  -> IO ()
hs_bindgen_b75eb57498380787 =
  \x0 ->
    \x1 ->
      hs_bindgen_b75eb57498380787_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_b75eb57498380787

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_after7@
foreign import ccall safe "hs_bindgen_f6b7a0773d24a96c" hs_bindgen_f6b7a0773d24a96c_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_after7@
hs_bindgen_f6b7a0773d24a96c ::
     A
  -> BG.CBool
  -> IO ()
hs_bindgen_f6b7a0773d24a96c =
  \x0 ->
    \x1 ->
      hs_bindgen_f6b7a0773d24a96c_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_f6b7a0773d24a96c

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_after8@
foreign import ccall safe "hs_bindgen_5bbc86f02e668a8e" hs_bindgen_5bbc86f02e668a8e_base ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_withoutSign_after8@
hs_bindgen_5bbc86f02e668a8e ::
     A
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_5bbc86f02e668a8e =
  \x0 ->
    \x1 ->
      hs_bindgen_5bbc86f02e668a8e_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_5bbc86f02e668a8e

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_args1@
foreign import ccall safe "hs_bindgen_6f80a5ce0ecdb409" hs_bindgen_6f80a5ce0ecdb409_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_args1@
hs_bindgen_6f80a5ce0ecdb409 ::
     A
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_6f80a5ce0ecdb409 =
  \x0 ->
    \x1 ->
      hs_bindgen_6f80a5ce0ecdb409_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args1 = hs_bindgen_6f80a5ce0ecdb409

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_args2@
foreign import ccall safe "hs_bindgen_9602684e37564ec3" hs_bindgen_9602684e37564ec3_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_args2@
hs_bindgen_9602684e37564ec3 ::
     A
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_9602684e37564ec3 =
  \x0 ->
    \x1 ->
      hs_bindgen_9602684e37564ec3_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args2 = hs_bindgen_9602684e37564ec3

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_args3@
foreign import ccall safe "hs_bindgen_8500c3b7013b4ace" hs_bindgen_8500c3b7013b4ace_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_args3@
hs_bindgen_8500c3b7013b4ace ::
     A
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_8500c3b7013b4ace =
  \x0 ->
    \x1 ->
      hs_bindgen_8500c3b7013b4ace_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args3 = hs_bindgen_8500c3b7013b4ace

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_args4@
foreign import ccall safe "hs_bindgen_a1112056987b1aa1" hs_bindgen_a1112056987b1aa1_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_args4@
hs_bindgen_a1112056987b1aa1 ::
     A
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_a1112056987b1aa1 =
  \x0 ->
    \x1 ->
      hs_bindgen_a1112056987b1aa1_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args4 = hs_bindgen_a1112056987b1aa1

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_args5@
foreign import ccall safe "hs_bindgen_d9b34592aa13daf5" hs_bindgen_d9b34592aa13daf5_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_args5@
hs_bindgen_d9b34592aa13daf5 ::
     A
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_d9b34592aa13daf5 =
  \x0 ->
    \x1 ->
      hs_bindgen_d9b34592aa13daf5_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args5 = hs_bindgen_d9b34592aa13daf5

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_ret1@
foreign import ccall safe "hs_bindgen_5bb31b051b32c12a" hs_bindgen_5bb31b051b32c12a_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_ret1@
hs_bindgen_5bb31b051b32c12a ::
     A
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_5bb31b051b32c12a =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_5bb31b051b32c12a_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h 212:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret1 = hs_bindgen_5bb31b051b32c12a

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_ret2@
foreign import ccall safe "hs_bindgen_790c75f565a590c2" hs_bindgen_790c75f565a590c2_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_ret2@
hs_bindgen_790c75f565a590c2 ::
     A
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_790c75f565a590c2 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_790c75f565a590c2_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h 213:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret2 = hs_bindgen_790c75f565a590c2

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_ret3@
foreign import ccall safe "hs_bindgen_e04d4996fbe914e8" hs_bindgen_e04d4996fbe914e8_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_ret3@
hs_bindgen_e04d4996fbe914e8 ::
     A
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_e04d4996fbe914e8 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_e04d4996fbe914e8_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h 214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
const_pointers_ret3 = hs_bindgen_e04d4996fbe914e8

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_ret4@
foreign import ccall safe "hs_bindgen_3a22b33ced27c18c" hs_bindgen_3a22b33ced27c18c_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_ret4@
hs_bindgen_3a22b33ced27c18c ::
     A
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_3a22b33ced27c18c =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_3a22b33ced27c18c_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h 215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret4 = hs_bindgen_3a22b33ced27c18c

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_ret5@
foreign import ccall safe "hs_bindgen_1d34f929d08623a8" hs_bindgen_1d34f929d08623a8_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse Example Safe_const_pointers_ret5@
hs_bindgen_1d34f929d08623a8 ::
     A
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_1d34f929d08623a8 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_1d34f929d08623a8_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h 216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret5 = hs_bindgen_1d34f929d08623a8

-- __unique:__ @test_macrosreparse Example Safe_const_array_elem1@
foreign import ccall safe "hs_bindgen_5857a4aef0d7365d" hs_bindgen_5857a4aef0d7365d_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_array_elem1@
hs_bindgen_5857a4aef0d7365d ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray A))
  -> IO ()
hs_bindgen_5857a4aef0d7365d =
  \x0 ->
    hs_bindgen_5857a4aef0d7365d_base (BG.toFFIType x0)

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h 244:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray A))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem1 = hs_bindgen_5857a4aef0d7365d

-- __unique:__ @test_macrosreparse Example Safe_const_array_elem2@
foreign import ccall safe "hs_bindgen_a6f8291807edf5d3" hs_bindgen_a6f8291807edf5d3_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_array_elem2@
hs_bindgen_a6f8291807edf5d3 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst A)))
  -> IO ()
hs_bindgen_a6f8291807edf5d3 =
  \x0 ->
    hs_bindgen_a6f8291807edf5d3_base (BG.toFFIType x0)

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h 245:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem2 = hs_bindgen_a6f8291807edf5d3

-- __unique:__ @test_macrosreparse Example Safe_const_array_elem3@
foreign import ccall safe "hs_bindgen_7ed879af0d6a37c8" hs_bindgen_7ed879af0d6a37c8_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_const_array_elem3@
hs_bindgen_7ed879af0d6a37c8 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr A)))
  -> IO ()
hs_bindgen_7ed879af0d6a37c8 =
  \x0 ->
    hs_bindgen_7ed879af0d6a37c8_base (BG.toFFIType x0)

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h 246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem3 = hs_bindgen_7ed879af0d6a37c8

-- __unique:__ @test_macrosreparse Example Safe_noParams1@
foreign import ccall safe "hs_bindgen_38eda369e92f2c78" hs_bindgen_38eda369e92f2c78_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse Example Safe_noParams1@
hs_bindgen_38eda369e92f2c78 :: IO A
hs_bindgen_38eda369e92f2c78 =
  fmap BG.fromFFIType hs_bindgen_38eda369e92f2c78_base

{-| Other examples we reparsed /incorrectly/ before language-c

    __C declaration:__ @noParams1@

    __defined at:__ @macros\/reparse.h 254:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams1 :: IO A
noParams1 = hs_bindgen_38eda369e92f2c78

-- __unique:__ @test_macrosreparse Example Safe_noParams2@
foreign import ccall safe "hs_bindgen_159789b013e14fc1" hs_bindgen_159789b013e14fc1_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse Example Safe_noParams2@
hs_bindgen_159789b013e14fc1 :: IO A
hs_bindgen_159789b013e14fc1 =
  fmap BG.fromFFIType hs_bindgen_159789b013e14fc1_base

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h 255:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2 :: IO A
noParams2 = hs_bindgen_159789b013e14fc1

-- __unique:__ @test_macrosreparse Example Safe_noParams3@
foreign import ccall safe "hs_bindgen_3bff7c28e14b8cc5" hs_bindgen_3bff7c28e14b8cc5_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse Example Safe_noParams3@
hs_bindgen_3bff7c28e14b8cc5 ::
     A
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_3bff7c28e14b8cc5 =
  \x0 ->
    \x1 ->
      hs_bindgen_3bff7c28e14b8cc5_base (BG.toFFIType x0) (BG.toFFIType x1)

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
noParams3 = hs_bindgen_3bff7c28e14b8cc5

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret1@
foreign import ccall safe "hs_bindgen_4c0491976152fbfa" hs_bindgen_4c0491976152fbfa_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret1@
hs_bindgen_4c0491976152fbfa ::
     A
  -> IO (BG.FunPtr (IO ()))
hs_bindgen_4c0491976152fbfa =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_4c0491976152fbfa_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h 260:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO ()))
funptr_ret1 = hs_bindgen_4c0491976152fbfa

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret2@
foreign import ccall safe "hs_bindgen_b742d6ab451be38c" hs_bindgen_b742d6ab451be38c_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret2@
hs_bindgen_b742d6ab451be38c ::
     A
  -> IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_b742d6ab451be38c =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_b742d6ab451be38c_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h 261:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO BG.CInt))
funptr_ret2 = hs_bindgen_b742d6ab451be38c

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret3@
foreign import ccall safe "hs_bindgen_f53b80280c85cb13" hs_bindgen_f53b80280c85cb13_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret3@
hs_bindgen_f53b80280c85cb13 ::
     A
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_f53b80280c85cb13 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_f53b80280c85cb13_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h 262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
funptr_ret3 = hs_bindgen_f53b80280c85cb13

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret4@
foreign import ccall safe "hs_bindgen_c9ae2f0f42e51b1d" hs_bindgen_c9ae2f0f42e51b1d_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret4@
hs_bindgen_c9ae2f0f42e51b1d ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
hs_bindgen_c9ae2f0f42e51b1d =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_c9ae2f0f42e51b1d_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h 263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
funptr_ret4 = hs_bindgen_c9ae2f0f42e51b1d

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret5@
foreign import ccall safe "hs_bindgen_83ed96e7baee5141" hs_bindgen_83ed96e7baee5141_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret5@
hs_bindgen_83ed96e7baee5141 ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_83ed96e7baee5141 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_83ed96e7baee5141_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h 267:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret5 = hs_bindgen_83ed96e7baee5141

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret6@
foreign import ccall safe "hs_bindgen_592be16b23695a01" hs_bindgen_592be16b23695a01_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret6@
hs_bindgen_592be16b23695a01 ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_592be16b23695a01 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_592be16b23695a01_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h 268:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret6 = hs_bindgen_592be16b23695a01

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret7@
foreign import ccall safe "hs_bindgen_0eb8787a6e05fcf6" hs_bindgen_0eb8787a6e05fcf6_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret7@
hs_bindgen_0eb8787a6e05fcf6 ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_0eb8787a6e05fcf6 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_0eb8787a6e05fcf6_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h 269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret7 = hs_bindgen_0eb8787a6e05fcf6

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret8@
foreign import ccall safe "hs_bindgen_3da136b1dfc9cef5" hs_bindgen_3da136b1dfc9cef5_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret8@
hs_bindgen_3da136b1dfc9cef5 ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_3da136b1dfc9cef5 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_3da136b1dfc9cef5_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h 270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret8 = hs_bindgen_3da136b1dfc9cef5

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret9@
foreign import ccall safe "hs_bindgen_8903c97931572f4c" hs_bindgen_8903c97931572f4c_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret9@
hs_bindgen_8903c97931572f4c ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_8903c97931572f4c =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_8903c97931572f4c_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h 271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret9 = hs_bindgen_8903c97931572f4c

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret10@
foreign import ccall safe "hs_bindgen_353b41c2a087e8d9" hs_bindgen_353b41c2a087e8d9_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse Example Safe_funptr_ret10@
hs_bindgen_353b41c2a087e8d9 ::
     A
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_353b41c2a087e8d9 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_353b41c2a087e8d9_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h 272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret10 = hs_bindgen_353b41c2a087e8d9
