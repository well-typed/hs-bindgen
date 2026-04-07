{-# LANGUAGE CApiFFI #-}
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
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/reparse/reparse.h>"
  , "void hs_bindgen_7300f3f51adae15c ("
  , "  A arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  (args_char1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_673fd05464d57bdd ("
  , "  A arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  (args_char2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9175dd9abd8a886c ("
  , "  A arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  (args_char3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_60f218314a9ed3fa ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d92790399ad76a0e ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_058c73a6636cfb3a ("
  , "  A arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  (args_short3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6fe35ac93ca11422 ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a77540a36cfe40e5 ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_40ad68fb27cd3f24 ("
  , "  A arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  (args_int3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a93b1ec4f770fd56 ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a8687cfe3cc65135 ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a86c9969ea0c781f ("
  , "  A arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  (args_long3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4a27ee96fc894aa9 ("
  , "  A arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  (args_float)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_92e9670c707d1bdb ("
  , "  A arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  (args_double)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9d4148035aeceefa ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (args_bool1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9a48e1913885c941 ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  (args_struct)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_777178899172e252 ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  (args_union)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_4cfefd723e945b89 ("
  , "  A arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  (args_enum)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b5ac12e87c362f0f ("
  , "  A arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  (args_pointer1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c38fc0d7cad12220 ("
  , "  A arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  (args_pointer2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_83b707c7aa722053 ("
  , "  A arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  (args_pointer3)(arg1, arg2);"
  , "}"
  , "A hs_bindgen_0d88a1f76e33133e (void)"
  , "{"
  , "  return (ret_A)();"
  , "}"
  , "char hs_bindgen_e4ef5e4c8dbbc21e ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_char1)(arg1);"
  , "}"
  , "signed char hs_bindgen_c0d4d3b8751b05a7 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_char2)(arg1);"
  , "}"
  , "unsigned char hs_bindgen_19c3e3cfda7c5bce ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_char3)(arg1);"
  , "}"
  , "signed short hs_bindgen_7aa830bbfbe3a43e ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_short1)(arg1);"
  , "}"
  , "signed short hs_bindgen_1f189581541fd3bc ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_short2)(arg1);"
  , "}"
  , "unsigned short hs_bindgen_fa492940be60391b ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_short3)(arg1);"
  , "}"
  , "signed int hs_bindgen_cd731622732df2df ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_int1)(arg1);"
  , "}"
  , "signed int hs_bindgen_09673f4c0ce194b2 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_int2)(arg1);"
  , "}"
  , "unsigned int hs_bindgen_0681b01af482ca27 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_int3)(arg1);"
  , "}"
  , "signed long hs_bindgen_c1f23937e655be32 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_long1)(arg1);"
  , "}"
  , "signed long hs_bindgen_94a27f0010178d12 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_long2)(arg1);"
  , "}"
  , "unsigned long hs_bindgen_df321aea201d84e6 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_long3)(arg1);"
  , "}"
  , "float hs_bindgen_20dd428313357b6e ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_float)(arg1);"
  , "}"
  , "double hs_bindgen_c7bbf89055b899f1 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_double)(arg1);"
  , "}"
  , "_Bool hs_bindgen_4b6e82c0d61328e2 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_bool1)(arg1);"
  , "}"
  , "void hs_bindgen_9924601a9048c382 ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_struct)(arg1);"
  , "}"
  , "void hs_bindgen_cacd71e50451ac1f ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_union)(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_f434d54dc9b5d094 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_enum)(arg1);"
  , "}"
  , "signed int *hs_bindgen_3f2c44cf9c5acbe8 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_pointer1)(arg1);"
  , "}"
  , "signed int **hs_bindgen_0f3af1d884b9ac05 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_pointer2)(arg1);"
  , "}"
  , "void *hs_bindgen_180161ece0461270 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_pointer3)(arg1);"
  , "}"
  , "signed int hs_bindgen_b9ac9fb4935708b6 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (body1)(arg1);"
  , "}"
  , "A hs_bindgen_d08979583681395e (void)"
  , "{"
  , "  return (body2)();"
  , "}"
  , "void hs_bindgen_4d999bcdb0d1517c ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_float)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_733f392dd2c77354 ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_double)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_6378963ac4749650 ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_float)(arg1);"
  , "}"
  , "void hs_bindgen_e029174ffba0cb1b ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_double)(arg1);"
  , "}"
  , "void hs_bindgen_b6d94a91e83245f3 ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (bespoke_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_889aa1687887dcfb ("
  , "  A arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  (bespoke_args2)(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_ddf24177e64085b9 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret1)(arg1);"
  , "}"
  , "size_t hs_bindgen_6c18b91090cb7170 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret2)(arg1);"
  , "}"
  , "void hs_bindgen_bb4a2de322d47795 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  (arr_args1)(arg1);"
  , "}"
  , "void hs_bindgen_8cc86812d49627ce ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  (arr_args2)(arg1);"
  , "}"
  , "void hs_bindgen_58289498d7b62ad5 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  (arr_args3)(arg1);"
  , "}"
  , "void hs_bindgen_4f2a87ec8926cd26 ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  (arr_args4)(arg1);"
  , "}"
  , "void hs_bindgen_49008a3838e7a16e ("
  , "  A arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d701e537258d799d ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3d1a4beef06884f1 ("
  , "  A arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9fde2512e053c2d0 ("
  , "  A arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_63150f90a4e11132 ("
  , "  A arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args5)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f3a1764c80b34a26 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  (comments1)(arg1);"
  , "}"
  , "void hs_bindgen_8dbd01efb942800d ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_967f7745f7c0acc6 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_bf0aaae4744c3a7a ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a37934f9a35ca0db ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c512f59c2140c625 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8af7b8bd42893e12 ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_07d794d17f820e7f ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2a4e36129669cfa0 ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_fa77fe540c3b7334 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6c467c299f33eca5 ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_c59c02c0ebf8bfc5 ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_c1dd895ae3529648 ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f2c9b2d2d388a8a8 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_296058d43fed7a6b ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_510331f29b9ae920 ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_07e66c26404743a3 ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d3b2829d60dbe168 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_599418a687621aa3 ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_d3a584c113e7e6ea ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_24af6f93ac47bf9d ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_cf974bc83588c3f5 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1a0e5dafa0153215 ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1b56b5573f58b7fe ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1f127945f63455a6 ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_39281240c6229337 ("
  , "  A arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c6bd9dcf4f30e11b ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_beebc587c94f8c4d ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args5)(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_250b4912deb77109 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret1)(arg1);"
  , "}"
  , "signed int const *hs_bindgen_a91eda868a08ecf1 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret2)(arg1);"
  , "}"
  , "signed int *const hs_bindgen_0e7b1c04ebb3f513 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret3)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_fde71036fa5dacbd ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret4)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_85a2276d5fd00383 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret5)(arg1);"
  , "}"
  , "void hs_bindgen_3f9c896418fc9b15 ("
  , "  A const *arg1"
  , ")"
  , "{"
  , "  (const_array_elem1)(arg1);"
  , "}"
  , "void hs_bindgen_4b583f0afa3f36f5 ("
  , "  A const **arg1"
  , ")"
  , "{"
  , "  (const_array_elem2)(arg1);"
  , "}"
  , "void hs_bindgen_e32dd6cc77bbb150 ("
  , "  A *const *arg1"
  , ")"
  , "{"
  , "  (const_array_elem3)(arg1);"
  , "}"
  , "A hs_bindgen_2c2b8a37bf0cde5d (void)"
  , "{"
  , "  return (noParams1)();"
  , "}"
  , "A hs_bindgen_b7c7603d0ad95879 (void)"
  , "{"
  , "  return (noParams2)();"
  , "}"
  , "void hs_bindgen_77b4278f1c7b8a98 ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (noParams3)(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_f1c8054ab1730c9f ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_93bd64a6f82c4278 ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret2)(arg1);"
  , "}"
  , "void (*hs_bindgen_f65c45d4720c0496 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (funptr_ret3)(arg1);"
  , "}"
  , "char (*hs_bindgen_6e9bdbacecf02bfb ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret4)(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_972869bb0c07101a ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret5)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_fb671f26c99f4ec0 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret6)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_968c75466212172c ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret7)(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_a2e98f1659edc185 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret8)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_83226810b69d386c ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret9)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_13b1831634eeec91 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret10)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_char1@
foreign import ccall safe "hs_bindgen_7300f3f51adae15c" hs_bindgen_7300f3f51adae15c_base ::
     RIP.Int32
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_char1@
hs_bindgen_7300f3f51adae15c ::
     A
  -> RIP.CChar
  -> IO ()
hs_bindgen_7300f3f51adae15c =
  RIP.fromFFIType hs_bindgen_7300f3f51adae15c_base

{-| Function declarations

__C declaration:__ @args_char1@

__defined at:__ @macros\/reparse\/reparse.h 17:6@

__exported by:__ @macros\/reparse\/reparse.h@
-}
args_char1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_char1 = hs_bindgen_7300f3f51adae15c

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_char2@
foreign import ccall safe "hs_bindgen_673fd05464d57bdd" hs_bindgen_673fd05464d57bdd_base ::
     RIP.Int32
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_char2@
hs_bindgen_673fd05464d57bdd ::
     A
  -> RIP.CSChar
  -> IO ()
hs_bindgen_673fd05464d57bdd =
  RIP.fromFFIType hs_bindgen_673fd05464d57bdd_base

{-| __C declaration:__ @args_char2@

    __defined at:__ @macros\/reparse\/reparse.h 18:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_char2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CSChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_char2 = hs_bindgen_673fd05464d57bdd

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_char3@
foreign import ccall safe "hs_bindgen_9175dd9abd8a886c" hs_bindgen_9175dd9abd8a886c_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_char3@
hs_bindgen_9175dd9abd8a886c ::
     A
  -> RIP.CUChar
  -> IO ()
hs_bindgen_9175dd9abd8a886c =
  RIP.fromFFIType hs_bindgen_9175dd9abd8a886c_base

{-| __C declaration:__ @args_char3@

    __defined at:__ @macros\/reparse\/reparse.h 19:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_char3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CUChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_char3 = hs_bindgen_9175dd9abd8a886c

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_short1@
foreign import ccall safe "hs_bindgen_60f218314a9ed3fa" hs_bindgen_60f218314a9ed3fa_base ::
     RIP.Int32
  -> RIP.Int16
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_short1@
hs_bindgen_60f218314a9ed3fa ::
     A
  -> RIP.CShort
  -> IO ()
hs_bindgen_60f218314a9ed3fa =
  RIP.fromFFIType hs_bindgen_60f218314a9ed3fa_base

{-| __C declaration:__ @args_short1@

    __defined at:__ @macros\/reparse\/reparse.h 21:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_short1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CShort
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_short1 = hs_bindgen_60f218314a9ed3fa

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_short2@
foreign import ccall safe "hs_bindgen_d92790399ad76a0e" hs_bindgen_d92790399ad76a0e_base ::
     RIP.Int32
  -> RIP.Int16
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_short2@
hs_bindgen_d92790399ad76a0e ::
     A
  -> RIP.CShort
  -> IO ()
hs_bindgen_d92790399ad76a0e =
  RIP.fromFFIType hs_bindgen_d92790399ad76a0e_base

{-| __C declaration:__ @args_short2@

    __defined at:__ @macros\/reparse\/reparse.h 22:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_short2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CShort
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_short2 = hs_bindgen_d92790399ad76a0e

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_short3@
foreign import ccall safe "hs_bindgen_058c73a6636cfb3a" hs_bindgen_058c73a6636cfb3a_base ::
     RIP.Int32
  -> RIP.Word16
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_short3@
hs_bindgen_058c73a6636cfb3a ::
     A
  -> RIP.CUShort
  -> IO ()
hs_bindgen_058c73a6636cfb3a =
  RIP.fromFFIType hs_bindgen_058c73a6636cfb3a_base

{-| __C declaration:__ @args_short3@

    __defined at:__ @macros\/reparse\/reparse.h 23:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_short3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CUShort
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_short3 = hs_bindgen_058c73a6636cfb3a

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_int1@
foreign import ccall safe "hs_bindgen_6fe35ac93ca11422" hs_bindgen_6fe35ac93ca11422_base ::
     RIP.Int32
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_int1@
hs_bindgen_6fe35ac93ca11422 ::
     A
  -> RIP.CInt
  -> IO ()
hs_bindgen_6fe35ac93ca11422 =
  RIP.fromFFIType hs_bindgen_6fe35ac93ca11422_base

{-| __C declaration:__ @args_int1@

    __defined at:__ @macros\/reparse\/reparse.h 25:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_int1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_int1 = hs_bindgen_6fe35ac93ca11422

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_int2@
foreign import ccall safe "hs_bindgen_a77540a36cfe40e5" hs_bindgen_a77540a36cfe40e5_base ::
     RIP.Int32
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_int2@
hs_bindgen_a77540a36cfe40e5 ::
     A
  -> RIP.CInt
  -> IO ()
hs_bindgen_a77540a36cfe40e5 =
  RIP.fromFFIType hs_bindgen_a77540a36cfe40e5_base

{-| __C declaration:__ @args_int2@

    __defined at:__ @macros\/reparse\/reparse.h 26:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_int2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_int2 = hs_bindgen_a77540a36cfe40e5

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_int3@
foreign import ccall safe "hs_bindgen_40ad68fb27cd3f24" hs_bindgen_40ad68fb27cd3f24_base ::
     RIP.Int32
  -> RIP.Word32
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_int3@
hs_bindgen_40ad68fb27cd3f24 ::
     A
  -> RIP.CUInt
  -> IO ()
hs_bindgen_40ad68fb27cd3f24 =
  RIP.fromFFIType hs_bindgen_40ad68fb27cd3f24_base

{-| __C declaration:__ @args_int3@

    __defined at:__ @macros\/reparse\/reparse.h 27:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_int3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CUInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_int3 = hs_bindgen_40ad68fb27cd3f24

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_long1@
foreign import ccall safe "hs_bindgen_a93b1ec4f770fd56" hs_bindgen_a93b1ec4f770fd56_base ::
     RIP.Int32
  -> RIP.Int64
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_long1@
hs_bindgen_a93b1ec4f770fd56 ::
     A
  -> RIP.CLong
  -> IO ()
hs_bindgen_a93b1ec4f770fd56 =
  RIP.fromFFIType hs_bindgen_a93b1ec4f770fd56_base

{-| __C declaration:__ @args_long1@

    __defined at:__ @macros\/reparse\/reparse.h 29:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_long1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CLong
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_long1 = hs_bindgen_a93b1ec4f770fd56

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_long2@
foreign import ccall safe "hs_bindgen_a8687cfe3cc65135" hs_bindgen_a8687cfe3cc65135_base ::
     RIP.Int32
  -> RIP.Int64
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_long2@
hs_bindgen_a8687cfe3cc65135 ::
     A
  -> RIP.CLong
  -> IO ()
hs_bindgen_a8687cfe3cc65135 =
  RIP.fromFFIType hs_bindgen_a8687cfe3cc65135_base

{-| __C declaration:__ @args_long2@

    __defined at:__ @macros\/reparse\/reparse.h 30:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_long2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CLong
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_long2 = hs_bindgen_a8687cfe3cc65135

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_long3@
foreign import ccall safe "hs_bindgen_a86c9969ea0c781f" hs_bindgen_a86c9969ea0c781f_base ::
     RIP.Int32
  -> RIP.Word64
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_long3@
hs_bindgen_a86c9969ea0c781f ::
     A
  -> RIP.CULong
  -> IO ()
hs_bindgen_a86c9969ea0c781f =
  RIP.fromFFIType hs_bindgen_a86c9969ea0c781f_base

{-| __C declaration:__ @args_long3@

    __defined at:__ @macros\/reparse\/reparse.h 31:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_long3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CULong
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_long3 = hs_bindgen_a86c9969ea0c781f

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_float@
foreign import ccall safe "hs_bindgen_4a27ee96fc894aa9" hs_bindgen_4a27ee96fc894aa9_base ::
     RIP.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_float@
hs_bindgen_4a27ee96fc894aa9 ::
     A
  -> RIP.CFloat
  -> IO ()
hs_bindgen_4a27ee96fc894aa9 =
  RIP.fromFFIType hs_bindgen_4a27ee96fc894aa9_base

{-| __C declaration:__ @args_float@

    __defined at:__ @macros\/reparse\/reparse.h 33:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_float ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_float = hs_bindgen_4a27ee96fc894aa9

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_double@
foreign import ccall safe "hs_bindgen_92e9670c707d1bdb" hs_bindgen_92e9670c707d1bdb_base ::
     RIP.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_double@
hs_bindgen_92e9670c707d1bdb ::
     A
  -> RIP.CDouble
  -> IO ()
hs_bindgen_92e9670c707d1bdb =
  RIP.fromFFIType hs_bindgen_92e9670c707d1bdb_base

{-| __C declaration:__ @args_double@

    __defined at:__ @macros\/reparse\/reparse.h 34:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_double ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_double = hs_bindgen_92e9670c707d1bdb

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_bool1@
foreign import ccall safe "hs_bindgen_9d4148035aeceefa" hs_bindgen_9d4148035aeceefa_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_bool1@
hs_bindgen_9d4148035aeceefa ::
     A
  -> RIP.CBool
  -> IO ()
hs_bindgen_9d4148035aeceefa =
  RIP.fromFFIType hs_bindgen_9d4148035aeceefa_base

{-| __C declaration:__ @args_bool1@

    __defined at:__ @macros\/reparse\/reparse.h 35:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_bool1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_bool1 = hs_bindgen_9d4148035aeceefa

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_struct@
foreign import ccall safe "hs_bindgen_9a48e1913885c941" hs_bindgen_9a48e1913885c941_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_struct@
hs_bindgen_9a48e1913885c941 ::
     A
  -> RIP.Ptr Some_struct
  -> IO ()
hs_bindgen_9a48e1913885c941 =
  RIP.fromFFIType hs_bindgen_9a48e1913885c941_base

{-| __C declaration:__ @args_struct@

    __defined at:__ @macros\/reparse\/reparse.h 37:6@

    __exported by:__ @macros\/reparse\/reparse.h@
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
      RIP.with arg21 (\arg22 ->
                        hs_bindgen_9a48e1913885c941 arg10 arg22)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_union@
foreign import ccall safe "hs_bindgen_777178899172e252" hs_bindgen_777178899172e252_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_union@
hs_bindgen_777178899172e252 ::
     A
  -> RIP.Ptr Some_union
  -> IO ()
hs_bindgen_777178899172e252 =
  RIP.fromFFIType hs_bindgen_777178899172e252_base

{-| __C declaration:__ @args_union@

    __defined at:__ @macros\/reparse\/reparse.h 38:6@

    __exported by:__ @macros\/reparse\/reparse.h@
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
      RIP.with arg21 (\arg22 ->
                        hs_bindgen_777178899172e252 arg10 arg22)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_enum@
foreign import ccall safe "hs_bindgen_4cfefd723e945b89" hs_bindgen_4cfefd723e945b89_base ::
     RIP.Int32
  -> RIP.Word32
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_enum@
hs_bindgen_4cfefd723e945b89 ::
     A
  -> Some_enum
  -> IO ()
hs_bindgen_4cfefd723e945b89 =
  RIP.fromFFIType hs_bindgen_4cfefd723e945b89_base

{-| __C declaration:__ @args_enum@

    __defined at:__ @macros\/reparse\/reparse.h 39:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_enum ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_enum
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_enum = hs_bindgen_4cfefd723e945b89

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_pointer1@
foreign import ccall safe "hs_bindgen_b5ac12e87c362f0f" hs_bindgen_b5ac12e87c362f0f_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_pointer1@
hs_bindgen_b5ac12e87c362f0f ::
     A
  -> RIP.Ptr RIP.CInt
  -> IO ()
hs_bindgen_b5ac12e87c362f0f =
  RIP.fromFFIType hs_bindgen_b5ac12e87c362f0f_base

{-| __C declaration:__ @args_pointer1@

    __defined at:__ @macros\/reparse\/reparse.h 41:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_pointer1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.Ptr RIP.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_pointer1 = hs_bindgen_b5ac12e87c362f0f

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_pointer2@
foreign import ccall safe "hs_bindgen_c38fc0d7cad12220" hs_bindgen_c38fc0d7cad12220_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_pointer2@
hs_bindgen_c38fc0d7cad12220 ::
     A
  -> RIP.Ptr (RIP.Ptr RIP.CInt)
  -> IO ()
hs_bindgen_c38fc0d7cad12220 =
  RIP.fromFFIType hs_bindgen_c38fc0d7cad12220_base

{-| __C declaration:__ @args_pointer2@

    __defined at:__ @macros\/reparse\/reparse.h 42:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_pointer2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.Ptr (RIP.Ptr RIP.CInt)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_pointer2 = hs_bindgen_c38fc0d7cad12220

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_pointer3@
foreign import ccall safe "hs_bindgen_83b707c7aa722053" hs_bindgen_83b707c7aa722053_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_pointer3@
hs_bindgen_83b707c7aa722053 ::
     A
  -> RIP.Ptr RIP.Void
  -> IO ()
hs_bindgen_83b707c7aa722053 =
  RIP.fromFFIType hs_bindgen_83b707c7aa722053_base

{-| __C declaration:__ @args_pointer3@

    __defined at:__ @macros\/reparse\/reparse.h 43:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_pointer3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.Ptr RIP.Void
     -- ^ __C declaration:__ @arg3@
  -> IO ()
args_pointer3 = hs_bindgen_83b707c7aa722053

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_A@
foreign import ccall safe "hs_bindgen_0d88a1f76e33133e" hs_bindgen_0d88a1f76e33133e_base ::
     IO RIP.Int32

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_A@
hs_bindgen_0d88a1f76e33133e :: IO A
hs_bindgen_0d88a1f76e33133e =
  RIP.fromFFIType hs_bindgen_0d88a1f76e33133e_base

{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse\/reparse.h 47:3@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_A :: IO A
ret_A = hs_bindgen_0d88a1f76e33133e

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_char1@
foreign import ccall safe "hs_bindgen_e4ef5e4c8dbbc21e" hs_bindgen_e4ef5e4c8dbbc21e_base ::
     RIP.Int32
  -> IO RIP.Int8

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_char1@
hs_bindgen_e4ef5e4c8dbbc21e ::
     A
  -> IO RIP.CChar
hs_bindgen_e4ef5e4c8dbbc21e =
  RIP.fromFFIType hs_bindgen_e4ef5e4c8dbbc21e_base

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse\/reparse.h 49:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_char1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CChar
ret_char1 = hs_bindgen_e4ef5e4c8dbbc21e

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_char2@
foreign import ccall safe "hs_bindgen_c0d4d3b8751b05a7" hs_bindgen_c0d4d3b8751b05a7_base ::
     RIP.Int32
  -> IO RIP.Int8

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_char2@
hs_bindgen_c0d4d3b8751b05a7 ::
     A
  -> IO RIP.CSChar
hs_bindgen_c0d4d3b8751b05a7 =
  RIP.fromFFIType hs_bindgen_c0d4d3b8751b05a7_base

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse\/reparse.h 50:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_char2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CSChar
ret_char2 = hs_bindgen_c0d4d3b8751b05a7

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_char3@
foreign import ccall safe "hs_bindgen_19c3e3cfda7c5bce" hs_bindgen_19c3e3cfda7c5bce_base ::
     RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_char3@
hs_bindgen_19c3e3cfda7c5bce ::
     A
  -> IO RIP.CUChar
hs_bindgen_19c3e3cfda7c5bce =
  RIP.fromFFIType hs_bindgen_19c3e3cfda7c5bce_base

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse\/reparse.h 51:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_char3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CUChar
ret_char3 = hs_bindgen_19c3e3cfda7c5bce

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_short1@
foreign import ccall safe "hs_bindgen_7aa830bbfbe3a43e" hs_bindgen_7aa830bbfbe3a43e_base ::
     RIP.Int32
  -> IO RIP.Int16

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_short1@
hs_bindgen_7aa830bbfbe3a43e ::
     A
  -> IO RIP.CShort
hs_bindgen_7aa830bbfbe3a43e =
  RIP.fromFFIType hs_bindgen_7aa830bbfbe3a43e_base

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse\/reparse.h 53:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_short1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CShort
ret_short1 = hs_bindgen_7aa830bbfbe3a43e

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_short2@
foreign import ccall safe "hs_bindgen_1f189581541fd3bc" hs_bindgen_1f189581541fd3bc_base ::
     RIP.Int32
  -> IO RIP.Int16

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_short2@
hs_bindgen_1f189581541fd3bc ::
     A
  -> IO RIP.CShort
hs_bindgen_1f189581541fd3bc =
  RIP.fromFFIType hs_bindgen_1f189581541fd3bc_base

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse\/reparse.h 54:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_short2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CShort
ret_short2 = hs_bindgen_1f189581541fd3bc

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_short3@
foreign import ccall safe "hs_bindgen_fa492940be60391b" hs_bindgen_fa492940be60391b_base ::
     RIP.Int32
  -> IO RIP.Word16

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_short3@
hs_bindgen_fa492940be60391b ::
     A
  -> IO RIP.CUShort
hs_bindgen_fa492940be60391b =
  RIP.fromFFIType hs_bindgen_fa492940be60391b_base

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse\/reparse.h 55:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_short3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CUShort
ret_short3 = hs_bindgen_fa492940be60391b

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_int1@
foreign import ccall safe "hs_bindgen_cd731622732df2df" hs_bindgen_cd731622732df2df_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_int1@
hs_bindgen_cd731622732df2df ::
     A
  -> IO RIP.CInt
hs_bindgen_cd731622732df2df =
  RIP.fromFFIType hs_bindgen_cd731622732df2df_base

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse\/reparse.h 57:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_int1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CInt
ret_int1 = hs_bindgen_cd731622732df2df

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_int2@
foreign import ccall safe "hs_bindgen_09673f4c0ce194b2" hs_bindgen_09673f4c0ce194b2_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_int2@
hs_bindgen_09673f4c0ce194b2 ::
     A
  -> IO RIP.CInt
hs_bindgen_09673f4c0ce194b2 =
  RIP.fromFFIType hs_bindgen_09673f4c0ce194b2_base

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse\/reparse.h 58:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_int2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CInt
ret_int2 = hs_bindgen_09673f4c0ce194b2

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_int3@
foreign import ccall safe "hs_bindgen_0681b01af482ca27" hs_bindgen_0681b01af482ca27_base ::
     RIP.Int32
  -> IO RIP.Word32

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_int3@
hs_bindgen_0681b01af482ca27 ::
     A
  -> IO RIP.CUInt
hs_bindgen_0681b01af482ca27 =
  RIP.fromFFIType hs_bindgen_0681b01af482ca27_base

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse\/reparse.h 59:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_int3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CUInt
ret_int3 = hs_bindgen_0681b01af482ca27

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_long1@
foreign import ccall safe "hs_bindgen_c1f23937e655be32" hs_bindgen_c1f23937e655be32_base ::
     RIP.Int32
  -> IO RIP.Int64

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_long1@
hs_bindgen_c1f23937e655be32 ::
     A
  -> IO RIP.CLong
hs_bindgen_c1f23937e655be32 =
  RIP.fromFFIType hs_bindgen_c1f23937e655be32_base

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse\/reparse.h 61:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_long1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CLong
ret_long1 = hs_bindgen_c1f23937e655be32

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_long2@
foreign import ccall safe "hs_bindgen_94a27f0010178d12" hs_bindgen_94a27f0010178d12_base ::
     RIP.Int32
  -> IO RIP.Int64

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_long2@
hs_bindgen_94a27f0010178d12 ::
     A
  -> IO RIP.CLong
hs_bindgen_94a27f0010178d12 =
  RIP.fromFFIType hs_bindgen_94a27f0010178d12_base

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse\/reparse.h 62:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_long2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CLong
ret_long2 = hs_bindgen_94a27f0010178d12

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_long3@
foreign import ccall safe "hs_bindgen_df321aea201d84e6" hs_bindgen_df321aea201d84e6_base ::
     RIP.Int32
  -> IO RIP.Word64

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_long3@
hs_bindgen_df321aea201d84e6 ::
     A
  -> IO RIP.CULong
hs_bindgen_df321aea201d84e6 =
  RIP.fromFFIType hs_bindgen_df321aea201d84e6_base

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse\/reparse.h 63:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_long3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CULong
ret_long3 = hs_bindgen_df321aea201d84e6

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_float@
foreign import ccall safe "hs_bindgen_20dd428313357b6e" hs_bindgen_20dd428313357b6e_base ::
     RIP.Int32
  -> IO Float

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_float@
hs_bindgen_20dd428313357b6e ::
     A
  -> IO RIP.CFloat
hs_bindgen_20dd428313357b6e =
  RIP.fromFFIType hs_bindgen_20dd428313357b6e_base

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse\/reparse.h 65:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_float ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CFloat
ret_float = hs_bindgen_20dd428313357b6e

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_double@
foreign import ccall safe "hs_bindgen_c7bbf89055b899f1" hs_bindgen_c7bbf89055b899f1_base ::
     RIP.Int32
  -> IO Double

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_double@
hs_bindgen_c7bbf89055b899f1 ::
     A
  -> IO RIP.CDouble
hs_bindgen_c7bbf89055b899f1 =
  RIP.fromFFIType hs_bindgen_c7bbf89055b899f1_base

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse\/reparse.h 66:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_double ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CDouble
ret_double = hs_bindgen_c7bbf89055b899f1

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_bool1@
foreign import ccall safe "hs_bindgen_4b6e82c0d61328e2" hs_bindgen_4b6e82c0d61328e2_base ::
     RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_bool1@
hs_bindgen_4b6e82c0d61328e2 ::
     A
  -> IO RIP.CBool
hs_bindgen_4b6e82c0d61328e2 =
  RIP.fromFFIType hs_bindgen_4b6e82c0d61328e2_base

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse\/reparse.h 67:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_bool1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CBool
ret_bool1 = hs_bindgen_4b6e82c0d61328e2

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_struct@
foreign import ccall safe "hs_bindgen_9924601a9048c382" hs_bindgen_9924601a9048c382_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_struct@
hs_bindgen_9924601a9048c382 ::
     A
  -> RIP.Ptr Some_struct
  -> IO ()
hs_bindgen_9924601a9048c382 =
  RIP.fromFFIType hs_bindgen_9924601a9048c382_base

{-| __C declaration:__ @ret_struct@

    __defined at:__ @macros\/reparse\/reparse.h 69:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_struct ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO Some_struct
ret_struct =
  \arg10 ->
    RIP.allocaAndPeek (\res1 ->
                         hs_bindgen_9924601a9048c382 arg10 res1)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_union@
foreign import ccall safe "hs_bindgen_cacd71e50451ac1f" hs_bindgen_cacd71e50451ac1f_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_union@
hs_bindgen_cacd71e50451ac1f ::
     A
  -> RIP.Ptr Some_union
  -> IO ()
hs_bindgen_cacd71e50451ac1f =
  RIP.fromFFIType hs_bindgen_cacd71e50451ac1f_base

{-| __C declaration:__ @ret_union@

    __defined at:__ @macros\/reparse\/reparse.h 70:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_union ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO Some_union
ret_union =
  \arg10 ->
    RIP.allocaAndPeek (\res1 ->
                         hs_bindgen_cacd71e50451ac1f arg10 res1)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_enum@
foreign import ccall safe "hs_bindgen_f434d54dc9b5d094" hs_bindgen_f434d54dc9b5d094_base ::
     RIP.Int32
  -> IO RIP.Word32

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_enum@
hs_bindgen_f434d54dc9b5d094 ::
     A
  -> IO Some_enum
hs_bindgen_f434d54dc9b5d094 =
  RIP.fromFFIType hs_bindgen_f434d54dc9b5d094_base

{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse\/reparse.h 71:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_enum ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO Some_enum
ret_enum = hs_bindgen_f434d54dc9b5d094

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_pointer1@
foreign import ccall safe "hs_bindgen_3f2c44cf9c5acbe8" hs_bindgen_3f2c44cf9c5acbe8_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_pointer1@
hs_bindgen_3f2c44cf9c5acbe8 ::
     A
  -> IO (RIP.Ptr RIP.CInt)
hs_bindgen_3f2c44cf9c5acbe8 =
  RIP.fromFFIType hs_bindgen_3f2c44cf9c5acbe8_base

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse\/reparse.h 73:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_pointer1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.Ptr RIP.CInt)
ret_pointer1 = hs_bindgen_3f2c44cf9c5acbe8

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_pointer2@
foreign import ccall safe "hs_bindgen_0f3af1d884b9ac05" hs_bindgen_0f3af1d884b9ac05_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_pointer2@
hs_bindgen_0f3af1d884b9ac05 ::
     A
  -> IO (RIP.Ptr (RIP.Ptr RIP.CInt))
hs_bindgen_0f3af1d884b9ac05 =
  RIP.fromFFIType hs_bindgen_0f3af1d884b9ac05_base

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse\/reparse.h 74:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_pointer2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.Ptr (RIP.Ptr RIP.CInt))
ret_pointer2 = hs_bindgen_0f3af1d884b9ac05

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_pointer3@
foreign import ccall safe "hs_bindgen_180161ece0461270" hs_bindgen_180161ece0461270_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_pointer3@
hs_bindgen_180161ece0461270 ::
     A
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_180161ece0461270 =
  RIP.fromFFIType hs_bindgen_180161ece0461270_base

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse\/reparse.h 75:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_pointer3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.Ptr RIP.Void)
ret_pointer3 = hs_bindgen_180161ece0461270

-- __unique:__ @test_macrosreparsereparse_Example_Safe_body1@
foreign import ccall safe "hs_bindgen_b9ac9fb4935708b6" hs_bindgen_b9ac9fb4935708b6_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsereparse_Example_Safe_body1@
hs_bindgen_b9ac9fb4935708b6 ::
     A
  -> IO RIP.CInt
hs_bindgen_b9ac9fb4935708b6 =
  RIP.fromFFIType hs_bindgen_b9ac9fb4935708b6_base

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse\/reparse.h 79:5@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
body1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CInt
body1 = hs_bindgen_b9ac9fb4935708b6

-- __unique:__ @test_macrosreparsereparse_Example_Safe_body2@
foreign import ccall safe "hs_bindgen_d08979583681395e" hs_bindgen_d08979583681395e_base ::
     IO RIP.Int32

-- __unique:__ @test_macrosreparsereparse_Example_Safe_body2@
hs_bindgen_d08979583681395e :: IO A
hs_bindgen_d08979583681395e =
  RIP.fromFFIType hs_bindgen_d08979583681395e_base

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse\/reparse.h 80:3@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
body2 :: IO A
body2 = hs_bindgen_d08979583681395e

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_complex_float@
foreign import ccall safe "hs_bindgen_4d999bcdb0d1517c" hs_bindgen_4d999bcdb0d1517c_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_complex_float@
hs_bindgen_4d999bcdb0d1517c ::
     A
  -> RIP.Ptr (RIP.Complex RIP.CFloat)
  -> IO ()
hs_bindgen_4d999bcdb0d1517c =
  RIP.fromFFIType hs_bindgen_4d999bcdb0d1517c_base

{-| __C declaration:__ @args_complex_float@

    __defined at:__ @macros\/reparse\/reparse.h 84:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_complex_float ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.Complex RIP.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_complex_float =
  \arg10 ->
    \arg21 ->
      RIP.with arg21 (\arg22 ->
                        hs_bindgen_4d999bcdb0d1517c arg10 arg22)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_complex_double@
foreign import ccall safe "hs_bindgen_733f392dd2c77354" hs_bindgen_733f392dd2c77354_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_args_complex_double@
hs_bindgen_733f392dd2c77354 ::
     A
  -> RIP.Ptr (RIP.Complex RIP.CDouble)
  -> IO ()
hs_bindgen_733f392dd2c77354 =
  RIP.fromFFIType hs_bindgen_733f392dd2c77354_base

{-| __C declaration:__ @args_complex_double@

    __defined at:__ @macros\/reparse\/reparse.h 85:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
args_complex_double ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.Complex RIP.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
args_complex_double =
  \arg10 ->
    \arg21 ->
      RIP.with arg21 (\arg22 ->
                        hs_bindgen_733f392dd2c77354 arg10 arg22)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_complex_float@
foreign import ccall safe "hs_bindgen_6378963ac4749650" hs_bindgen_6378963ac4749650_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_complex_float@
hs_bindgen_6378963ac4749650 ::
     A
  -> RIP.Ptr (RIP.Complex RIP.CFloat)
  -> IO ()
hs_bindgen_6378963ac4749650 =
  RIP.fromFFIType hs_bindgen_6378963ac4749650_base

{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @macros\/reparse\/reparse.h 86:17@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_complex_float ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.Complex RIP.CFloat)
ret_complex_float =
  \arg10 ->
    RIP.allocaAndPeek (\res1 ->
                         hs_bindgen_6378963ac4749650 arg10 res1)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_complex_double@
foreign import ccall safe "hs_bindgen_e029174ffba0cb1b" hs_bindgen_e029174ffba0cb1b_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_ret_complex_double@
hs_bindgen_e029174ffba0cb1b ::
     A
  -> RIP.Ptr (RIP.Complex RIP.CDouble)
  -> IO ()
hs_bindgen_e029174ffba0cb1b =
  RIP.fromFFIType hs_bindgen_e029174ffba0cb1b_base

{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @macros\/reparse\/reparse.h 87:17@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_complex_double ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.Complex RIP.CDouble)
ret_complex_double =
  \arg10 ->
    RIP.allocaAndPeek (\res1 ->
                         hs_bindgen_e029174ffba0cb1b arg10 res1)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_bespoke_args1@
foreign import ccall safe "hs_bindgen_b6d94a91e83245f3" hs_bindgen_b6d94a91e83245f3_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_bespoke_args1@
hs_bindgen_b6d94a91e83245f3 ::
     A
  -> RIP.CBool
  -> IO ()
hs_bindgen_b6d94a91e83245f3 =
  RIP.fromFFIType hs_bindgen_b6d94a91e83245f3_base

{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @macros\/reparse\/reparse.h 94:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
bespoke_args1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
bespoke_args1 = hs_bindgen_b6d94a91e83245f3

-- __unique:__ @test_macrosreparsereparse_Example_Safe_bespoke_args2@
foreign import ccall safe "hs_bindgen_889aa1687887dcfb" hs_bindgen_889aa1687887dcfb_base ::
     RIP.Int32
  -> RIP.Word64
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_bespoke_args2@
hs_bindgen_889aa1687887dcfb ::
     A
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_889aa1687887dcfb =
  RIP.fromFFIType hs_bindgen_889aa1687887dcfb_base

{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @macros\/reparse\/reparse.h 95:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
bespoke_args2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.LibC.CSize
     -- ^ __C declaration:__ @arg2@
  -> IO ()
bespoke_args2 = hs_bindgen_889aa1687887dcfb

-- __unique:__ @test_macrosreparsereparse_Example_Safe_bespoke_ret1@
foreign import ccall safe "hs_bindgen_ddf24177e64085b9" hs_bindgen_ddf24177e64085b9_base ::
     RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_macrosreparsereparse_Example_Safe_bespoke_ret1@
hs_bindgen_ddf24177e64085b9 ::
     A
  -> IO RIP.CBool
hs_bindgen_ddf24177e64085b9 =
  RIP.fromFFIType hs_bindgen_ddf24177e64085b9_base

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse\/reparse.h 97:8@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
bespoke_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CBool
bespoke_ret1 = hs_bindgen_ddf24177e64085b9

-- __unique:__ @test_macrosreparsereparse_Example_Safe_bespoke_ret2@
foreign import ccall safe "hs_bindgen_6c18b91090cb7170" hs_bindgen_6c18b91090cb7170_base ::
     RIP.Int32
  -> IO RIP.Word64

-- __unique:__ @test_macrosreparsereparse_Example_Safe_bespoke_ret2@
hs_bindgen_6c18b91090cb7170 ::
     A
  -> IO HsBindgen.Runtime.LibC.CSize
hs_bindgen_6c18b91090cb7170 =
  RIP.fromFFIType hs_bindgen_6c18b91090cb7170_base

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse\/reparse.h 98:8@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
bespoke_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO HsBindgen.Runtime.LibC.CSize
bespoke_ret2 = hs_bindgen_6c18b91090cb7170

-- __unique:__ @test_macrosreparsereparse_Example_Safe_arr_args1@
foreign import ccall safe "hs_bindgen_bb4a2de322d47795" hs_bindgen_bb4a2de322d47795_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_arr_args1@
hs_bindgen_bb4a2de322d47795 ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray A))
  -> IO ()
hs_bindgen_bb4a2de322d47795 =
  RIP.fromFFIType hs_bindgen_bb4a2de322d47795_base

{-| Arrays

__C declaration:__ @arr_args1@

__defined at:__ @macros\/reparse\/reparse.h 104:6@

__exported by:__ @macros\/reparse\/reparse.h@
-}
arr_args1 ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray A))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args1 = hs_bindgen_bb4a2de322d47795

-- __unique:__ @test_macrosreparsereparse_Example_Safe_arr_args2@
foreign import ccall safe "hs_bindgen_8cc86812d49627ce" hs_bindgen_8cc86812d49627ce_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_arr_args2@
hs_bindgen_8cc86812d49627ce ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray (RIP.Ptr A)))
  -> IO ()
hs_bindgen_8cc86812d49627ce =
  RIP.fromFFIType hs_bindgen_8cc86812d49627ce_base

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse\/reparse.h 105:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
arr_args2 ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray (RIP.Ptr A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args2 = hs_bindgen_8cc86812d49627ce

-- __unique:__ @test_macrosreparsereparse_Example_Safe_arr_args3@
foreign import ccall safe "hs_bindgen_58289498d7b62ad5" hs_bindgen_58289498d7b62ad5_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_arr_args3@
hs_bindgen_58289498d7b62ad5 ::
     RIP.Ptr (IsA.Elem ((CA.ConstantArray 5) A))
  -> IO ()
hs_bindgen_58289498d7b62ad5 =
  RIP.fromFFIType hs_bindgen_58289498d7b62ad5_base

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse\/reparse.h 106:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
arr_args3 ::
     RIP.Ptr (IsA.Elem ((CA.ConstantArray 5) A))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args3 = hs_bindgen_58289498d7b62ad5

-- __unique:__ @test_macrosreparsereparse_Example_Safe_arr_args4@
foreign import ccall safe "hs_bindgen_4f2a87ec8926cd26" hs_bindgen_4f2a87ec8926cd26_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_arr_args4@
hs_bindgen_4f2a87ec8926cd26 ::
     RIP.Ptr (IsA.Elem ((CA.ConstantArray 5) (RIP.Ptr A)))
  -> IO ()
hs_bindgen_4f2a87ec8926cd26 =
  RIP.fromFFIType hs_bindgen_4f2a87ec8926cd26_base

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse\/reparse.h 107:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
arr_args4 ::
     RIP.Ptr (IsA.Elem ((CA.ConstantArray 5) (RIP.Ptr A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args4 = hs_bindgen_4f2a87ec8926cd26

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_args1@
foreign import ccall safe "hs_bindgen_49008a3838e7a16e" hs_bindgen_49008a3838e7a16e_base ::
     RIP.Int32
  -> RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_args1@
hs_bindgen_49008a3838e7a16e ::
     A
  -> RIP.FunPtr (IO ())
  -> IO ()
hs_bindgen_49008a3838e7a16e =
  RIP.fromFFIType hs_bindgen_49008a3838e7a16e_base

{-| Function pointers

__C declaration:__ @funptr_args1@

__defined at:__ @macros\/reparse\/reparse.h 126:6@

__exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_args1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.FunPtr (IO ())
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args1 = hs_bindgen_49008a3838e7a16e

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_args2@
foreign import ccall safe "hs_bindgen_d701e537258d799d" hs_bindgen_d701e537258d799d_base ::
     RIP.Int32
  -> RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_args2@
hs_bindgen_d701e537258d799d ::
     A
  -> RIP.FunPtr (IO RIP.CInt)
  -> IO ()
hs_bindgen_d701e537258d799d =
  RIP.fromFFIType hs_bindgen_d701e537258d799d_base

{-| __C declaration:__ @funptr_args2@

    __defined at:__ @macros\/reparse\/reparse.h 127:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_args2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.FunPtr (IO RIP.CInt)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args2 = hs_bindgen_d701e537258d799d

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_args3@
foreign import ccall safe "hs_bindgen_3d1a4beef06884f1" hs_bindgen_3d1a4beef06884f1_base ::
     RIP.Int32
  -> RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_args3@
hs_bindgen_3d1a4beef06884f1 ::
     A
  -> RIP.FunPtr (RIP.CInt -> IO ())
  -> IO ()
hs_bindgen_3d1a4beef06884f1 =
  RIP.fromFFIType hs_bindgen_3d1a4beef06884f1_base

{-| __C declaration:__ @funptr_args3@

    __defined at:__ @macros\/reparse\/reparse.h 128:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_args3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.FunPtr (RIP.CInt -> IO ())
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args3 = hs_bindgen_3d1a4beef06884f1

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_args4@
foreign import ccall safe "hs_bindgen_9fde2512e053c2d0" hs_bindgen_9fde2512e053c2d0_base ::
     RIP.Int32
  -> RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_args4@
hs_bindgen_9fde2512e053c2d0 ::
     A
  -> RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO RIP.CChar)
  -> IO ()
hs_bindgen_9fde2512e053c2d0 =
  RIP.fromFFIType hs_bindgen_9fde2512e053c2d0_base

{-| __C declaration:__ @funptr_args4@

    __defined at:__ @macros\/reparse\/reparse.h 129:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_args4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO RIP.CChar)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args4 = hs_bindgen_9fde2512e053c2d0

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_args5@
foreign import ccall safe "hs_bindgen_63150f90a4e11132" hs_bindgen_63150f90a4e11132_base ::
     RIP.Int32
  -> RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_args5@
hs_bindgen_63150f90a4e11132 ::
     A
  -> RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt))
  -> IO ()
hs_bindgen_63150f90a4e11132 =
  RIP.fromFFIType hs_bindgen_63150f90a4e11132_base

{-| __C declaration:__ @funptr_args5@

    __defined at:__ @macros\/reparse\/reparse.h 130:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_args5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt))
     -- ^ __C declaration:__ @arg2@
  -> IO ()
funptr_args5 = hs_bindgen_63150f90a4e11132

-- __unique:__ @test_macrosreparsereparse_Example_Safe_comments1@
foreign import ccall safe "hs_bindgen_f3a1764c80b34a26" hs_bindgen_f3a1764c80b34a26_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_comments1@
hs_bindgen_f3a1764c80b34a26 ::
     A
  -> IO ()
hs_bindgen_f3a1764c80b34a26 =
  RIP.fromFFIType hs_bindgen_f3a1764c80b34a26_base

{-| Comments in awkward places

  (Prior to language-c we failed to parse there.)

__C declaration:__ @comments1@

__defined at:__ @macros\/reparse\/reparse.h 144:25@

__exported by:__ @macros\/reparse\/reparse.h@
-}
comments1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO ()
comments1 = hs_bindgen_f3a1764c80b34a26

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_prim_before1@
foreign import ccall safe "hs_bindgen_8dbd01efb942800d" hs_bindgen_8dbd01efb942800d_base ::
     RIP.Int32
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_prim_before1@
hs_bindgen_8dbd01efb942800d ::
     A
  -> RIP.CChar
  -> IO ()
hs_bindgen_8dbd01efb942800d =
  RIP.fromFFIType hs_bindgen_8dbd01efb942800d_base

{-| `const` qualifier

  NOTE: These were not parsed correctly prior to the switch to language-c.

__C declaration:__ @const_prim_before1@

__defined at:__ @macros\/reparse\/reparse.h 179:6@

__exported by:__ @macros\/reparse\/reparse.h@
-}
const_prim_before1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_before1 = hs_bindgen_8dbd01efb942800d

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_prim_before2@
foreign import ccall safe "hs_bindgen_967f7745f7c0acc6" hs_bindgen_967f7745f7c0acc6_base ::
     RIP.Int32
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_prim_before2@
hs_bindgen_967f7745f7c0acc6 ::
     A
  -> RIP.CSChar
  -> IO ()
hs_bindgen_967f7745f7c0acc6 =
  RIP.fromFFIType hs_bindgen_967f7745f7c0acc6_base

{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @macros\/reparse\/reparse.h 180:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_prim_before2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CSChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_before2 = hs_bindgen_967f7745f7c0acc6

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_prim_before3@
foreign import ccall safe "hs_bindgen_bf0aaae4744c3a7a" hs_bindgen_bf0aaae4744c3a7a_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_prim_before3@
hs_bindgen_bf0aaae4744c3a7a ::
     A
  -> RIP.CUChar
  -> IO ()
hs_bindgen_bf0aaae4744c3a7a =
  RIP.fromFFIType hs_bindgen_bf0aaae4744c3a7a_base

{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @macros\/reparse\/reparse.h 181:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_prim_before3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CUChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_before3 = hs_bindgen_bf0aaae4744c3a7a

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_prim_after1@
foreign import ccall safe "hs_bindgen_a37934f9a35ca0db" hs_bindgen_a37934f9a35ca0db_base ::
     RIP.Int32
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_prim_after1@
hs_bindgen_a37934f9a35ca0db ::
     A
  -> RIP.CChar
  -> IO ()
hs_bindgen_a37934f9a35ca0db =
  RIP.fromFFIType hs_bindgen_a37934f9a35ca0db_base

{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @macros\/reparse\/reparse.h 182:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_prim_after1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_after1 = hs_bindgen_a37934f9a35ca0db

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_prim_after2@
foreign import ccall safe "hs_bindgen_c512f59c2140c625" hs_bindgen_c512f59c2140c625_base ::
     RIP.Int32
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_prim_after2@
hs_bindgen_c512f59c2140c625 ::
     A
  -> RIP.CSChar
  -> IO ()
hs_bindgen_c512f59c2140c625 =
  RIP.fromFFIType hs_bindgen_c512f59c2140c625_base

{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @macros\/reparse\/reparse.h 183:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_prim_after2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CSChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_after2 = hs_bindgen_c512f59c2140c625

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_prim_after3@
foreign import ccall safe "hs_bindgen_8af7b8bd42893e12" hs_bindgen_8af7b8bd42893e12_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_prim_after3@
hs_bindgen_8af7b8bd42893e12 ::
     A
  -> RIP.CUChar
  -> IO ()
hs_bindgen_8af7b8bd42893e12 =
  RIP.fromFFIType hs_bindgen_8af7b8bd42893e12_base

{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @macros\/reparse\/reparse.h 184:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_prim_after3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CUChar
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_prim_after3 = hs_bindgen_8af7b8bd42893e12

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_before1@
foreign import ccall safe "hs_bindgen_07d794d17f820e7f" hs_bindgen_07d794d17f820e7f_base ::
     RIP.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_before1@
hs_bindgen_07d794d17f820e7f ::
     A
  -> RIP.CFloat
  -> IO ()
hs_bindgen_07d794d17f820e7f =
  RIP.fromFFIType hs_bindgen_07d794d17f820e7f_base

{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @macros\/reparse\/reparse.h 188:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_before1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before1 =
  hs_bindgen_07d794d17f820e7f

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_before2@
foreign import ccall safe "hs_bindgen_2a4e36129669cfa0" hs_bindgen_2a4e36129669cfa0_base ::
     RIP.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_before2@
hs_bindgen_2a4e36129669cfa0 ::
     A
  -> RIP.CDouble
  -> IO ()
hs_bindgen_2a4e36129669cfa0 =
  RIP.fromFFIType hs_bindgen_2a4e36129669cfa0_base

{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @macros\/reparse\/reparse.h 189:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_before2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before2 =
  hs_bindgen_2a4e36129669cfa0

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_before3@
foreign import ccall safe "hs_bindgen_fa77fe540c3b7334" hs_bindgen_fa77fe540c3b7334_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_before3@
hs_bindgen_fa77fe540c3b7334 ::
     A
  -> RIP.CBool
  -> IO ()
hs_bindgen_fa77fe540c3b7334 =
  RIP.fromFFIType hs_bindgen_fa77fe540c3b7334_base

{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @macros\/reparse\/reparse.h 190:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_before3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before3 =
  hs_bindgen_fa77fe540c3b7334

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_before4@
foreign import ccall safe "hs_bindgen_6c467c299f33eca5" hs_bindgen_6c467c299f33eca5_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_before4@
hs_bindgen_6c467c299f33eca5 ::
     A
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_6c467c299f33eca5 =
  RIP.fromFFIType hs_bindgen_6c467c299f33eca5_base

{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @macros\/reparse\/reparse.h 191:6@

    __exported by:__ @macros\/reparse\/reparse.h@
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
      RIP.with arg21 (\arg22 ->
                        hs_bindgen_6c467c299f33eca5 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_before5@
foreign import ccall safe "hs_bindgen_c59c02c0ebf8bfc5" hs_bindgen_c59c02c0ebf8bfc5_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_before5@
hs_bindgen_c59c02c0ebf8bfc5 ::
     A
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_c59c02c0ebf8bfc5 =
  RIP.fromFFIType hs_bindgen_c59c02c0ebf8bfc5_base

{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @macros\/reparse\/reparse.h 192:6@

    __exported by:__ @macros\/reparse\/reparse.h@
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
      RIP.with arg21 (\arg22 ->
                        hs_bindgen_c59c02c0ebf8bfc5 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_before6@
foreign import ccall safe "hs_bindgen_c1dd895ae3529648" hs_bindgen_c1dd895ae3529648_base ::
     RIP.Int32
  -> RIP.Word32
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_before6@
hs_bindgen_c1dd895ae3529648 ::
     A
  -> Some_enum
  -> IO ()
hs_bindgen_c1dd895ae3529648 =
  RIP.fromFFIType hs_bindgen_c1dd895ae3529648_base

{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @macros\/reparse\/reparse.h 193:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_before6 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_enum
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before6 =
  hs_bindgen_c1dd895ae3529648

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_before7@
foreign import ccall safe "hs_bindgen_f2c9b2d2d388a8a8" hs_bindgen_f2c9b2d2d388a8a8_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_before7@
hs_bindgen_f2c9b2d2d388a8a8 ::
     A
  -> RIP.CBool
  -> IO ()
hs_bindgen_f2c9b2d2d388a8a8 =
  RIP.fromFFIType hs_bindgen_f2c9b2d2d388a8a8_base

{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @macros\/reparse\/reparse.h 194:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_before7 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before7 =
  hs_bindgen_f2c9b2d2d388a8a8

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_before8@
foreign import ccall safe "hs_bindgen_296058d43fed7a6b" hs_bindgen_296058d43fed7a6b_base ::
     RIP.Int32
  -> RIP.Word64
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_before8@
hs_bindgen_296058d43fed7a6b ::
     A
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_296058d43fed7a6b =
  RIP.fromFFIType hs_bindgen_296058d43fed7a6b_base

{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @macros\/reparse\/reparse.h 195:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_before8 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.LibC.CSize
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_before8 =
  hs_bindgen_296058d43fed7a6b

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_after1@
foreign import ccall safe "hs_bindgen_510331f29b9ae920" hs_bindgen_510331f29b9ae920_base ::
     RIP.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_after1@
hs_bindgen_510331f29b9ae920 ::
     A
  -> RIP.CFloat
  -> IO ()
hs_bindgen_510331f29b9ae920 =
  RIP.fromFFIType hs_bindgen_510331f29b9ae920_base

{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @macros\/reparse\/reparse.h 197:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_after1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CFloat
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after1 =
  hs_bindgen_510331f29b9ae920

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_after2@
foreign import ccall safe "hs_bindgen_07e66c26404743a3" hs_bindgen_07e66c26404743a3_base ::
     RIP.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_after2@
hs_bindgen_07e66c26404743a3 ::
     A
  -> RIP.CDouble
  -> IO ()
hs_bindgen_07e66c26404743a3 =
  RIP.fromFFIType hs_bindgen_07e66c26404743a3_base

{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @macros\/reparse\/reparse.h 198:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_after2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CDouble
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after2 =
  hs_bindgen_07e66c26404743a3

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_after3@
foreign import ccall safe "hs_bindgen_d3b2829d60dbe168" hs_bindgen_d3b2829d60dbe168_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_after3@
hs_bindgen_d3b2829d60dbe168 ::
     A
  -> RIP.CBool
  -> IO ()
hs_bindgen_d3b2829d60dbe168 =
  RIP.fromFFIType hs_bindgen_d3b2829d60dbe168_base

{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @macros\/reparse\/reparse.h 199:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_after3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after3 =
  hs_bindgen_d3b2829d60dbe168

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_after4@
foreign import ccall safe "hs_bindgen_599418a687621aa3" hs_bindgen_599418a687621aa3_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_after4@
hs_bindgen_599418a687621aa3 ::
     A
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_599418a687621aa3 =
  RIP.fromFFIType hs_bindgen_599418a687621aa3_base

{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @macros\/reparse\/reparse.h 200:6@

    __exported by:__ @macros\/reparse\/reparse.h@
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
      RIP.with arg21 (\arg22 ->
                        hs_bindgen_599418a687621aa3 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_after5@
foreign import ccall safe "hs_bindgen_d3a584c113e7e6ea" hs_bindgen_d3a584c113e7e6ea_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_after5@
hs_bindgen_d3a584c113e7e6ea ::
     A
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_d3a584c113e7e6ea =
  RIP.fromFFIType hs_bindgen_d3a584c113e7e6ea_base

{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @macros\/reparse\/reparse.h 201:6@

    __exported by:__ @macros\/reparse\/reparse.h@
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
      RIP.with arg21 (\arg22 ->
                        hs_bindgen_d3a584c113e7e6ea arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_after6@
foreign import ccall safe "hs_bindgen_24af6f93ac47bf9d" hs_bindgen_24af6f93ac47bf9d_base ::
     RIP.Int32
  -> RIP.Word32
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_after6@
hs_bindgen_24af6f93ac47bf9d ::
     A
  -> Some_enum
  -> IO ()
hs_bindgen_24af6f93ac47bf9d =
  RIP.fromFFIType hs_bindgen_24af6f93ac47bf9d_base

{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @macros\/reparse\/reparse.h 202:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_after6 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> Some_enum
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after6 =
  hs_bindgen_24af6f93ac47bf9d

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_after7@
foreign import ccall safe "hs_bindgen_cf974bc83588c3f5" hs_bindgen_cf974bc83588c3f5_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_after7@
hs_bindgen_cf974bc83588c3f5 ::
     A
  -> RIP.CBool
  -> IO ()
hs_bindgen_cf974bc83588c3f5 =
  RIP.fromFFIType hs_bindgen_cf974bc83588c3f5_base

{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @macros\/reparse\/reparse.h 203:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_after7 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.CBool
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after7 =
  hs_bindgen_cf974bc83588c3f5

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_after8@
foreign import ccall safe "hs_bindgen_1a0e5dafa0153215" hs_bindgen_1a0e5dafa0153215_base ::
     RIP.Int32
  -> RIP.Word64
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_withoutSign_after8@
hs_bindgen_1a0e5dafa0153215 ::
     A
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_1a0e5dafa0153215 =
  RIP.fromFFIType hs_bindgen_1a0e5dafa0153215_base

{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @macros\/reparse\/reparse.h 204:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_withoutSign_after8 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> HsBindgen.Runtime.LibC.CSize
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_withoutSign_after8 =
  hs_bindgen_1a0e5dafa0153215

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_args1@
foreign import ccall safe "hs_bindgen_1b56b5573f58b7fe" hs_bindgen_1b56b5573f58b7fe_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_args1@
hs_bindgen_1b56b5573f58b7fe ::
     A
  -> PtrConst.PtrConst RIP.CInt
  -> IO ()
hs_bindgen_1b56b5573f58b7fe =
  RIP.fromFFIType hs_bindgen_1b56b5573f58b7fe_base

{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @macros\/reparse\/reparse.h 208:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_args1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> PtrConst.PtrConst RIP.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args1 = hs_bindgen_1b56b5573f58b7fe

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_args2@
foreign import ccall safe "hs_bindgen_1f127945f63455a6" hs_bindgen_1f127945f63455a6_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_args2@
hs_bindgen_1f127945f63455a6 ::
     A
  -> PtrConst.PtrConst RIP.CInt
  -> IO ()
hs_bindgen_1f127945f63455a6 =
  RIP.fromFFIType hs_bindgen_1f127945f63455a6_base

{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @macros\/reparse\/reparse.h 209:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_args2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> PtrConst.PtrConst RIP.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args2 = hs_bindgen_1f127945f63455a6

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_args3@
foreign import ccall safe "hs_bindgen_39281240c6229337" hs_bindgen_39281240c6229337_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_args3@
hs_bindgen_39281240c6229337 ::
     A
  -> RIP.Ptr RIP.CInt
  -> IO ()
hs_bindgen_39281240c6229337 =
  RIP.fromFFIType hs_bindgen_39281240c6229337_base

{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @macros\/reparse\/reparse.h 210:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_args3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.Ptr RIP.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args3 = hs_bindgen_39281240c6229337

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_args4@
foreign import ccall safe "hs_bindgen_c6bd9dcf4f30e11b" hs_bindgen_c6bd9dcf4f30e11b_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_args4@
hs_bindgen_c6bd9dcf4f30e11b ::
     A
  -> PtrConst.PtrConst RIP.CInt
  -> IO ()
hs_bindgen_c6bd9dcf4f30e11b =
  RIP.fromFFIType hs_bindgen_c6bd9dcf4f30e11b_base

{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @macros\/reparse\/reparse.h 211:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_args4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> PtrConst.PtrConst RIP.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args4 = hs_bindgen_c6bd9dcf4f30e11b

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_args5@
foreign import ccall safe "hs_bindgen_beebc587c94f8c4d" hs_bindgen_beebc587c94f8c4d_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_args5@
hs_bindgen_beebc587c94f8c4d ::
     A
  -> PtrConst.PtrConst RIP.CInt
  -> IO ()
hs_bindgen_beebc587c94f8c4d =
  RIP.fromFFIType hs_bindgen_beebc587c94f8c4d_base

{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @macros\/reparse\/reparse.h 212:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_args5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> PtrConst.PtrConst RIP.CInt
     -- ^ __C declaration:__ @arg2@
  -> IO ()
const_pointers_args5 = hs_bindgen_beebc587c94f8c4d

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_ret1@
foreign import ccall safe "hs_bindgen_250b4912deb77109" hs_bindgen_250b4912deb77109_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_ret1@
hs_bindgen_250b4912deb77109 ::
     A
  -> IO (PtrConst.PtrConst RIP.CInt)
hs_bindgen_250b4912deb77109 =
  RIP.fromFFIType hs_bindgen_250b4912deb77109_base

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse\/reparse.h 214:19@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst RIP.CInt)
const_pointers_ret1 = hs_bindgen_250b4912deb77109

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_ret2@
foreign import ccall safe "hs_bindgen_a91eda868a08ecf1" hs_bindgen_a91eda868a08ecf1_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_ret2@
hs_bindgen_a91eda868a08ecf1 ::
     A
  -> IO (PtrConst.PtrConst RIP.CInt)
hs_bindgen_a91eda868a08ecf1 =
  RIP.fromFFIType hs_bindgen_a91eda868a08ecf1_base

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse\/reparse.h 215:19@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst RIP.CInt)
const_pointers_ret2 = hs_bindgen_a91eda868a08ecf1

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_ret3@
foreign import ccall safe "hs_bindgen_0e7b1c04ebb3f513" hs_bindgen_0e7b1c04ebb3f513_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_ret3@
hs_bindgen_0e7b1c04ebb3f513 ::
     A
  -> IO (RIP.Ptr RIP.CInt)
hs_bindgen_0e7b1c04ebb3f513 =
  RIP.fromFFIType hs_bindgen_0e7b1c04ebb3f513_base

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse\/reparse.h 216:19@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_ret3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.Ptr RIP.CInt)
const_pointers_ret3 = hs_bindgen_0e7b1c04ebb3f513

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_ret4@
foreign import ccall safe "hs_bindgen_fde71036fa5dacbd" hs_bindgen_fde71036fa5dacbd_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_ret4@
hs_bindgen_fde71036fa5dacbd ::
     A
  -> IO (PtrConst.PtrConst RIP.CInt)
hs_bindgen_fde71036fa5dacbd =
  RIP.fromFFIType hs_bindgen_fde71036fa5dacbd_base

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse\/reparse.h 217:19@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_ret4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst RIP.CInt)
const_pointers_ret4 = hs_bindgen_fde71036fa5dacbd

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_ret5@
foreign import ccall safe "hs_bindgen_85a2276d5fd00383" hs_bindgen_85a2276d5fd00383_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_pointers_ret5@
hs_bindgen_85a2276d5fd00383 ::
     A
  -> IO (PtrConst.PtrConst RIP.CInt)
hs_bindgen_85a2276d5fd00383 =
  RIP.fromFFIType hs_bindgen_85a2276d5fd00383_base

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse\/reparse.h 218:19@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_ret5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst RIP.CInt)
const_pointers_ret5 = hs_bindgen_85a2276d5fd00383

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_array_elem1@
foreign import ccall safe "hs_bindgen_3f9c896418fc9b15" hs_bindgen_3f9c896418fc9b15_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_array_elem1@
hs_bindgen_3f9c896418fc9b15 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray A))
  -> IO ()
hs_bindgen_3f9c896418fc9b15 =
  RIP.fromFFIType hs_bindgen_3f9c896418fc9b15_base

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse\/reparse.h 246:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_array_elem1 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray A))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem1 = hs_bindgen_3f9c896418fc9b15

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_array_elem2@
foreign import ccall safe "hs_bindgen_4b583f0afa3f36f5" hs_bindgen_4b583f0afa3f36f5_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_array_elem2@
hs_bindgen_4b583f0afa3f36f5 ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst A)))
  -> IO ()
hs_bindgen_4b583f0afa3f36f5 =
  RIP.fromFFIType hs_bindgen_4b583f0afa3f36f5_base

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse\/reparse.h 247:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_array_elem2 ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem2 = hs_bindgen_4b583f0afa3f36f5

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_array_elem3@
foreign import ccall safe "hs_bindgen_e32dd6cc77bbb150" hs_bindgen_e32dd6cc77bbb150_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_const_array_elem3@
hs_bindgen_e32dd6cc77bbb150 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (RIP.Ptr A)))
  -> IO ()
hs_bindgen_e32dd6cc77bbb150 =
  RIP.fromFFIType hs_bindgen_e32dd6cc77bbb150_base

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse\/reparse.h 248:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_array_elem3 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (RIP.Ptr A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem3 = hs_bindgen_e32dd6cc77bbb150

-- __unique:__ @test_macrosreparsereparse_Example_Safe_noParams1@
foreign import ccall safe "hs_bindgen_2c2b8a37bf0cde5d" hs_bindgen_2c2b8a37bf0cde5d_base ::
     IO RIP.Int32

-- __unique:__ @test_macrosreparsereparse_Example_Safe_noParams1@
hs_bindgen_2c2b8a37bf0cde5d :: IO A
hs_bindgen_2c2b8a37bf0cde5d =
  RIP.fromFFIType hs_bindgen_2c2b8a37bf0cde5d_base

{-| Other examples we reparsed /incorrectly/ before language-c

__C declaration:__ @noParams1@

__defined at:__ @macros\/reparse\/reparse.h 256:3@

__exported by:__ @macros\/reparse\/reparse.h@
-}
noParams1 :: IO A
noParams1 = hs_bindgen_2c2b8a37bf0cde5d

-- __unique:__ @test_macrosreparsereparse_Example_Safe_noParams2@
foreign import ccall safe "hs_bindgen_b7c7603d0ad95879" hs_bindgen_b7c7603d0ad95879_base ::
     IO RIP.Int32

-- __unique:__ @test_macrosreparsereparse_Example_Safe_noParams2@
hs_bindgen_b7c7603d0ad95879 :: IO A
hs_bindgen_b7c7603d0ad95879 =
  RIP.fromFFIType hs_bindgen_b7c7603d0ad95879_base

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse\/reparse.h 257:3@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
noParams2 :: IO A
noParams2 = hs_bindgen_b7c7603d0ad95879

-- __unique:__ @test_macrosreparsereparse_Example_Safe_noParams3@
foreign import ccall safe "hs_bindgen_77b4278f1c7b8a98" hs_bindgen_77b4278f1c7b8a98_base ::
     RIP.Int32
  -> RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Safe_noParams3@
hs_bindgen_77b4278f1c7b8a98 ::
     A
  -> RIP.FunPtr (IO RIP.CInt)
  -> IO ()
hs_bindgen_77b4278f1c7b8a98 =
  RIP.fromFFIType hs_bindgen_77b4278f1c7b8a98_base

{-| __C declaration:__ @noParams3@

    __defined at:__ @macros\/reparse\/reparse.h 258:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
noParams3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> RIP.FunPtr (IO RIP.CInt)
     -- ^ __C declaration:__ @arg2@
  -> IO ()
noParams3 = hs_bindgen_77b4278f1c7b8a98

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret1@
foreign import ccall safe "hs_bindgen_f1c8054ab1730c9f" hs_bindgen_f1c8054ab1730c9f_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret1@
hs_bindgen_f1c8054ab1730c9f ::
     A
  -> IO (RIP.FunPtr (IO ()))
hs_bindgen_f1c8054ab1730c9f =
  RIP.fromFFIType hs_bindgen_f1c8054ab1730c9f_base

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse\/reparse.h 262:8@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (IO ()))
funptr_ret1 = hs_bindgen_f1c8054ab1730c9f

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret2@
foreign import ccall safe "hs_bindgen_93bd64a6f82c4278" hs_bindgen_93bd64a6f82c4278_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret2@
hs_bindgen_93bd64a6f82c4278 ::
     A
  -> IO (RIP.FunPtr (IO RIP.CInt))
hs_bindgen_93bd64a6f82c4278 =
  RIP.fromFFIType hs_bindgen_93bd64a6f82c4278_base

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse\/reparse.h 263:8@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (IO RIP.CInt))
funptr_ret2 = hs_bindgen_93bd64a6f82c4278

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret3@
foreign import ccall safe "hs_bindgen_f65c45d4720c0496" hs_bindgen_f65c45d4720c0496_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret3@
hs_bindgen_f65c45d4720c0496 ::
     A
  -> IO (RIP.FunPtr (RIP.CInt -> IO ()))
hs_bindgen_f65c45d4720c0496 =
  RIP.fromFFIType hs_bindgen_f65c45d4720c0496_base

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse\/reparse.h 264:8@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (RIP.CInt -> IO ()))
funptr_ret3 = hs_bindgen_f65c45d4720c0496

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret4@
foreign import ccall safe "hs_bindgen_6e9bdbacecf02bfb" hs_bindgen_6e9bdbacecf02bfb_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret4@
hs_bindgen_6e9bdbacecf02bfb ::
     A
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO RIP.CChar))
hs_bindgen_6e9bdbacecf02bfb =
  RIP.fromFFIType hs_bindgen_6e9bdbacecf02bfb_base

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse\/reparse.h 265:8@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO RIP.CChar))
funptr_ret4 = hs_bindgen_6e9bdbacecf02bfb

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret5@
foreign import ccall safe "hs_bindgen_972869bb0c07101a" hs_bindgen_972869bb0c07101a_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret5@
hs_bindgen_972869bb0c07101a ::
     A
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt)))
hs_bindgen_972869bb0c07101a =
  RIP.fromFFIType hs_bindgen_972869bb0c07101a_base

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse\/reparse.h 269:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt)))
funptr_ret5 = hs_bindgen_972869bb0c07101a

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret6@
foreign import ccall safe "hs_bindgen_fb671f26c99f4ec0" hs_bindgen_fb671f26c99f4ec0_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret6@
hs_bindgen_fb671f26c99f4ec0 ::
     A
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))
hs_bindgen_fb671f26c99f4ec0 =
  RIP.fromFFIType hs_bindgen_fb671f26c99f4ec0_base

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse\/reparse.h 270:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret6 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))
funptr_ret6 = hs_bindgen_fb671f26c99f4ec0

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret7@
foreign import ccall safe "hs_bindgen_968c75466212172c" hs_bindgen_968c75466212172c_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret7@
hs_bindgen_968c75466212172c ::
     A
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))
hs_bindgen_968c75466212172c =
  RIP.fromFFIType hs_bindgen_968c75466212172c_base

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse\/reparse.h 271:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret7 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))
funptr_ret7 = hs_bindgen_968c75466212172c

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret8@
foreign import ccall safe "hs_bindgen_a2e98f1659edc185" hs_bindgen_a2e98f1659edc185_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret8@
hs_bindgen_a2e98f1659edc185 ::
     A
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt)))
hs_bindgen_a2e98f1659edc185 =
  RIP.fromFFIType hs_bindgen_a2e98f1659edc185_base

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse\/reparse.h 272:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret8 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt)))
funptr_ret8 = hs_bindgen_a2e98f1659edc185

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret9@
foreign import ccall safe "hs_bindgen_83226810b69d386c" hs_bindgen_83226810b69d386c_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret9@
hs_bindgen_83226810b69d386c ::
     A
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))
hs_bindgen_83226810b69d386c =
  RIP.fromFFIType hs_bindgen_83226810b69d386c_base

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse\/reparse.h 273:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret9 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))
funptr_ret9 = hs_bindgen_83226810b69d386c

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret10@
foreign import ccall safe "hs_bindgen_13b1831634eeec91" hs_bindgen_13b1831634eeec91_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Safe_funptr_ret10@
hs_bindgen_13b1831634eeec91 ::
     A
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))
hs_bindgen_13b1831634eeec91 =
  RIP.fromFFIType hs_bindgen_13b1831634eeec91_base

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse\/reparse.h 274:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret10 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))
funptr_ret10 = hs_bindgen_13b1831634eeec91
