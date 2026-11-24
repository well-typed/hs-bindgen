{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse.h>"
  , "void hs_bindgen_9c141058a0b87406 ("
  , "  signed int arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  (args_char1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_24b3bc630fcd384e ("
  , "  signed int arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  (args_char2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6a60d55d0505fc32 ("
  , "  signed int arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  (args_char3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_92f45908fc0fa8b9 ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_954f76c224053402 ("
  , "  signed int arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_fc068ff68911fc5e ("
  , "  signed int arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  (args_short3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b85caf7db511cb41 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3f7851375745fc60 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_31affccad616114d ("
  , "  signed int arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  (args_int3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_23ef3352b5af4fa2 ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_955087cce9467e82 ("
  , "  signed int arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_92ae49c95d7816a9 ("
  , "  signed int arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  (args_long3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_40bcbc6c992b4792 ("
  , "  signed int arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  (args_float)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8b43e7ba21811662 ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  (args_double)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_99e6c424fbbd0de0 ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (args_bool1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_be5a043e6448a6ff ("
  , "  signed int arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  (args_struct)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_4df9dd17b242df7d ("
  , "  signed int arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  (args_union)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_1a6cbcdd0fd075a3 ("
  , "  signed int arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  (args_enum)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8466b284c3d4431b ("
  , "  signed int arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  (args_pointer1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3b85cdb3382c54f2 ("
  , "  signed int arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  (args_pointer2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e33fcd9863cc6288 ("
  , "  signed int arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  (args_pointer3)(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_513ee7f0e57addc4 (void)"
  , "{"
  , "  return (ret_A)();"
  , "}"
  , "char hs_bindgen_4264d96ca29ff80b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char1)(arg1);"
  , "}"
  , "signed char hs_bindgen_2149aa5b45bda802 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char2)(arg1);"
  , "}"
  , "unsigned char hs_bindgen_be6546432714ba7b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_char3)(arg1);"
  , "}"
  , "signed short hs_bindgen_df7d134e4138e60b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short1)(arg1);"
  , "}"
  , "signed short hs_bindgen_e6c53e4add223550 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short2)(arg1);"
  , "}"
  , "unsigned short hs_bindgen_52726b17ac086823 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_short3)(arg1);"
  , "}"
  , "signed int hs_bindgen_a509502c7a89b972 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int1)(arg1);"
  , "}"
  , "signed int hs_bindgen_729e6154a67de433 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int2)(arg1);"
  , "}"
  , "unsigned int hs_bindgen_d366c6ac7ba918d0 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_int3)(arg1);"
  , "}"
  , "signed long hs_bindgen_3db5ceaacf431dfc ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long1)(arg1);"
  , "}"
  , "signed long hs_bindgen_0619cc33c91ba89f ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long2)(arg1);"
  , "}"
  , "unsigned long hs_bindgen_544c8c26a369c5fd ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_long3)(arg1);"
  , "}"
  , "float hs_bindgen_0c167aabaf605fa0 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_float)(arg1);"
  , "}"
  , "double hs_bindgen_669bb877a0b38399 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_double)(arg1);"
  , "}"
  , "_Bool hs_bindgen_1a78afed9902f3dd ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_bool1)(arg1);"
  , "}"
  , "void hs_bindgen_8450f45e7a3d4474 ("
  , "  signed int arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_struct)(arg1);"
  , "}"
  , "void hs_bindgen_c0f3b36b7f529e39 ("
  , "  signed int arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_union)(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_056dd0d1a9f2d549 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_enum)(arg1);"
  , "}"
  , "signed int *hs_bindgen_1c2f0cb441d75bf1 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer1)(arg1);"
  , "}"
  , "signed int **hs_bindgen_8bd14c81cd0815dd ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer2)(arg1);"
  , "}"
  , "void *hs_bindgen_03585f3ce3184214 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (ret_pointer3)(arg1);"
  , "}"
  , "signed int hs_bindgen_522455bed0164d60 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (body1)(arg1);"
  , "}"
  , "signed int hs_bindgen_a85103ac19db8ed7 (void)"
  , "{"
  , "  return (body2)();"
  , "}"
  , "void hs_bindgen_9042c6b8c0c91365 ("
  , "  signed int arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_float)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_5d251ab9fe4ecfe8 ("
  , "  signed int arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_double)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_f4eced6daa1ea234 ("
  , "  signed int arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_float)(arg1);"
  , "}"
  , "void hs_bindgen_cba773e73a0e728a ("
  , "  signed int arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_double)(arg1);"
  , "}"
  , "void hs_bindgen_30e771db06ea15e4 ("
  , "  signed int arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (bespoke_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_5bab15758ceb1225 ("
  , "  signed int arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  (bespoke_args2)(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_77956329ffd3c392 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret1)(arg1);"
  , "}"
  , "size_t hs_bindgen_8207923662f04d2a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret2)(arg1);"
  , "}"
  , "void hs_bindgen_00a15b83a67898d0 ("
  , "  signed int arg1[]"
  , ")"
  , "{"
  , "  (arr_args1)(arg1);"
  , "}"
  , "void hs_bindgen_16598c9875843c93 ("
  , "  signed int *arg1[]"
  , ")"
  , "{"
  , "  (arr_args2)(arg1);"
  , "}"
  , "void hs_bindgen_624d90b7f84a8c16 ("
  , "  signed int arg1[5]"
  , ")"
  , "{"
  , "  (arr_args3)(arg1);"
  , "}"
  , "void hs_bindgen_24b495eae0ec3b12 ("
  , "  signed int *arg1[5]"
  , ")"
  , "{"
  , "  (arr_args4)(arg1);"
  , "}"
  , "void hs_bindgen_717257f54a5265c1 ("
  , "  signed int arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4bfafcca9b11bcf9 ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_66fe19b0eca209c0 ("
  , "  signed int arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_df821c6c63281837 ("
  , "  signed int arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_df0ef6e56db8362a ("
  , "  signed int arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args5)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d69eb5037b8ddc21 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (comments1)(arg1);"
  , "}"
  , "void hs_bindgen_58980995448953c1 ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3bc97d60462ff786 ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d24584345ecfca1b ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_23bf36ae20407d2a ("
  , "  signed int arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_01319d392def267f ("
  , "  signed int arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8f6c812963a89b7a ("
  , "  signed int arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_40bd67a0cc536508 ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2bb682df4c433aac ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_80c7819860d5da62 ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f8b477980ba2c648 ("
  , "  signed int arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_ad63cef18af38159 ("
  , "  signed int arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_0c52ffc54bdedfee ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f22aa63ea90a16f1 ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_7e9e1c6fb88e2605 ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6ccb480bf3f85817 ("
  , "  signed int arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a52ffd55ec948935 ("
  , "  signed int arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_480d382b01069870 ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_22c0aef39317ea7a ("
  , "  signed int arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_9813fa26fcd84d87 ("
  , "  signed int arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_57929639e21b6021 ("
  , "  signed int arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e5819093ad177172 ("
  , "  signed int arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_16d2e737dde0cdad ("
  , "  signed int arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_7b6273f76aee9d99 ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_14eea7087c075ad2 ("
  , "  signed int arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4fb0cdd1bf87276b ("
  , "  signed int arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d6adcaa5ac4f2c63 ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_48726e87fe933d81 ("
  , "  signed int arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args5)(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_34d6770542ed5cfb ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret1)(arg1);"
  , "}"
  , "signed int const *hs_bindgen_a7cb3cdd6a21e67b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret2)(arg1);"
  , "}"
  , "signed int *const hs_bindgen_3f091917b4017290 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret3)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_f809746522523301 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret4)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_b39bdc77bce45435 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret5)(arg1);"
  , "}"
  , "void hs_bindgen_8a3177ed097eb11e ("
  , "  signed int const arg1[]"
  , ")"
  , "{"
  , "  (const_array_elem1)(arg1);"
  , "}"
  , "void hs_bindgen_884c45b9c9202a23 ("
  , "  signed int const *arg1[]"
  , ")"
  , "{"
  , "  (const_array_elem2)(arg1);"
  , "}"
  , "void hs_bindgen_ef00489eabd9372d ("
  , "  signed int *const arg1[]"
  , ")"
  , "{"
  , "  (const_array_elem3)(arg1);"
  , "}"
  , "signed int hs_bindgen_ac87d663b48567cc (void)"
  , "{"
  , "  return (noParams1)();"
  , "}"
  , "signed int hs_bindgen_76a35330ffc46573 (void)"
  , "{"
  , "  return (noParams2)();"
  , "}"
  , "void hs_bindgen_3a7955710fb46bd7 ("
  , "  signed int arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (noParams3)(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_be338e12dad77c8d ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_6521ceb04d05fb68 ("
  , "  signed int arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret2)(arg1);"
  , "}"
  , "void (*hs_bindgen_a6bbfb214bdc9399 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (funptr_ret3)(arg1);"
  , "}"
  , "char (*hs_bindgen_ceb193d5c9f9f59c ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret4)(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_a6a108ec08d429a6 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret5)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_a1f262043255334c ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret6)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_7db6e756cb39dbf8 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret7)(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_4210a13033563230 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret8)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_0b02073743b4ddd5 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret9)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_1d0f3906df1077d1 ("
  , "  signed int arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret10)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_char1@
foreign import ccall unsafe "hs_bindgen_9c141058a0b87406" hs_bindgen_9c141058a0b87406_base ::
     BG.CInt
  -> BG.CChar
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_char1@
hs_bindgen_9c141058a0b87406 ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_9c141058a0b87406 =
  \x0 ->
    \x1 ->
      hs_bindgen_9c141058a0b87406_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_char1 = hs_bindgen_9c141058a0b87406

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_char2@
foreign import ccall unsafe "hs_bindgen_24b3bc630fcd384e" hs_bindgen_24b3bc630fcd384e_base ::
     BG.CInt
  -> BG.CSChar
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_char2@
hs_bindgen_24b3bc630fcd384e ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_24b3bc630fcd384e =
  \x0 ->
    \x1 ->
      hs_bindgen_24b3bc630fcd384e_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_char2 = hs_bindgen_24b3bc630fcd384e

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_char3@
foreign import ccall unsafe "hs_bindgen_6a60d55d0505fc32" hs_bindgen_6a60d55d0505fc32_base ::
     BG.CInt
  -> BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_char3@
hs_bindgen_6a60d55d0505fc32 ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_6a60d55d0505fc32 =
  \x0 ->
    \x1 ->
      hs_bindgen_6a60d55d0505fc32_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_char3 = hs_bindgen_6a60d55d0505fc32

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_short1@
foreign import ccall unsafe "hs_bindgen_92f45908fc0fa8b9" hs_bindgen_92f45908fc0fa8b9_base ::
     BG.CInt
  -> BG.CShort
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_short1@
hs_bindgen_92f45908fc0fa8b9 ::
     BG.CInt
  -> BG.CShort
  -> IO ()
hs_bindgen_92f45908fc0fa8b9 =
  \x0 ->
    \x1 ->
      hs_bindgen_92f45908fc0fa8b9_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_short1 = hs_bindgen_92f45908fc0fa8b9

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_short2@
foreign import ccall unsafe "hs_bindgen_954f76c224053402" hs_bindgen_954f76c224053402_base ::
     BG.CInt
  -> BG.CShort
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_short2@
hs_bindgen_954f76c224053402 ::
     BG.CInt
  -> BG.CShort
  -> IO ()
hs_bindgen_954f76c224053402 =
  \x0 ->
    \x1 ->
      hs_bindgen_954f76c224053402_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_short2 = hs_bindgen_954f76c224053402

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_short3@
foreign import ccall unsafe "hs_bindgen_fc068ff68911fc5e" hs_bindgen_fc068ff68911fc5e_base ::
     BG.CInt
  -> BG.CUShort
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_short3@
hs_bindgen_fc068ff68911fc5e ::
     BG.CInt
  -> BG.CUShort
  -> IO ()
hs_bindgen_fc068ff68911fc5e =
  \x0 ->
    \x1 ->
      hs_bindgen_fc068ff68911fc5e_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_short3 = hs_bindgen_fc068ff68911fc5e

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_int1@
foreign import ccall unsafe "hs_bindgen_b85caf7db511cb41" hs_bindgen_b85caf7db511cb41_base ::
     BG.CInt
  -> BG.CInt
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_int1@
hs_bindgen_b85caf7db511cb41 ::
     BG.CInt
  -> BG.CInt
  -> IO ()
hs_bindgen_b85caf7db511cb41 =
  \x0 ->
    \x1 ->
      hs_bindgen_b85caf7db511cb41_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_int1 = hs_bindgen_b85caf7db511cb41

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_int2@
foreign import ccall unsafe "hs_bindgen_3f7851375745fc60" hs_bindgen_3f7851375745fc60_base ::
     BG.CInt
  -> BG.CInt
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_int2@
hs_bindgen_3f7851375745fc60 ::
     BG.CInt
  -> BG.CInt
  -> IO ()
hs_bindgen_3f7851375745fc60 =
  \x0 ->
    \x1 ->
      hs_bindgen_3f7851375745fc60_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_int2 = hs_bindgen_3f7851375745fc60

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_int3@
foreign import ccall unsafe "hs_bindgen_31affccad616114d" hs_bindgen_31affccad616114d_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_int3@
hs_bindgen_31affccad616114d ::
     BG.CInt
  -> BG.CUInt
  -> IO ()
hs_bindgen_31affccad616114d =
  \x0 ->
    \x1 ->
      hs_bindgen_31affccad616114d_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_int3 = hs_bindgen_31affccad616114d

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_long1@
foreign import ccall unsafe "hs_bindgen_23ef3352b5af4fa2" hs_bindgen_23ef3352b5af4fa2_base ::
     BG.CInt
  -> BG.CLong
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_long1@
hs_bindgen_23ef3352b5af4fa2 ::
     BG.CInt
  -> BG.CLong
  -> IO ()
hs_bindgen_23ef3352b5af4fa2 =
  \x0 ->
    \x1 ->
      hs_bindgen_23ef3352b5af4fa2_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_long1 = hs_bindgen_23ef3352b5af4fa2

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_long2@
foreign import ccall unsafe "hs_bindgen_955087cce9467e82" hs_bindgen_955087cce9467e82_base ::
     BG.CInt
  -> BG.CLong
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_long2@
hs_bindgen_955087cce9467e82 ::
     BG.CInt
  -> BG.CLong
  -> IO ()
hs_bindgen_955087cce9467e82 =
  \x0 ->
    \x1 ->
      hs_bindgen_955087cce9467e82_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_long2 = hs_bindgen_955087cce9467e82

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_long3@
foreign import ccall unsafe "hs_bindgen_92ae49c95d7816a9" hs_bindgen_92ae49c95d7816a9_base ::
     BG.CInt
  -> BG.CULong
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_long3@
hs_bindgen_92ae49c95d7816a9 ::
     BG.CInt
  -> BG.CULong
  -> IO ()
hs_bindgen_92ae49c95d7816a9 =
  \x0 ->
    \x1 ->
      hs_bindgen_92ae49c95d7816a9_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_long3 = hs_bindgen_92ae49c95d7816a9

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_float@
foreign import ccall unsafe "hs_bindgen_40bcbc6c992b4792" hs_bindgen_40bcbc6c992b4792_base ::
     BG.CInt
  -> BG.CFloat
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_float@
hs_bindgen_40bcbc6c992b4792 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_40bcbc6c992b4792 =
  \x0 ->
    \x1 ->
      hs_bindgen_40bcbc6c992b4792_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_float = hs_bindgen_40bcbc6c992b4792

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_double@
foreign import ccall unsafe "hs_bindgen_8b43e7ba21811662" hs_bindgen_8b43e7ba21811662_base ::
     BG.CInt
  -> BG.CDouble
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_double@
hs_bindgen_8b43e7ba21811662 ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_8b43e7ba21811662 =
  \x0 ->
    \x1 ->
      hs_bindgen_8b43e7ba21811662_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_double = hs_bindgen_8b43e7ba21811662

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_bool1@
foreign import ccall unsafe "hs_bindgen_99e6c424fbbd0de0" hs_bindgen_99e6c424fbbd0de0_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_bool1@
hs_bindgen_99e6c424fbbd0de0 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_99e6c424fbbd0de0 =
  \x0 ->
    \x1 ->
      hs_bindgen_99e6c424fbbd0de0_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_bool1 = hs_bindgen_99e6c424fbbd0de0

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_struct@
foreign import ccall unsafe "hs_bindgen_be5a043e6448a6ff" hs_bindgen_be5a043e6448a6ff_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_struct@
hs_bindgen_be5a043e6448a6ff ::
     BG.CInt
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_be5a043e6448a6ff =
  \x0 ->
    \x1 ->
      hs_bindgen_be5a043e6448a6ff_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_be5a043e6448a6ff arg10 arg22)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_union@
foreign import ccall unsafe "hs_bindgen_4df9dd17b242df7d" hs_bindgen_4df9dd17b242df7d_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_union@
hs_bindgen_4df9dd17b242df7d ::
     BG.CInt
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_4df9dd17b242df7d =
  \x0 ->
    \x1 ->
      hs_bindgen_4df9dd17b242df7d_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_4df9dd17b242df7d arg10 arg22)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_enum@
foreign import ccall unsafe "hs_bindgen_1a6cbcdd0fd075a3" hs_bindgen_1a6cbcdd0fd075a3_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_enum@
hs_bindgen_1a6cbcdd0fd075a3 ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_1a6cbcdd0fd075a3 =
  \x0 ->
    \x1 ->
      hs_bindgen_1a6cbcdd0fd075a3_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_enum = hs_bindgen_1a6cbcdd0fd075a3

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_pointer1@
foreign import ccall unsafe "hs_bindgen_8466b284c3d4431b" hs_bindgen_8466b284c3d4431b_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_pointer1@
hs_bindgen_8466b284c3d4431b ::
     BG.CInt
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_8466b284c3d4431b =
  \x0 ->
    \x1 ->
      hs_bindgen_8466b284c3d4431b_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_pointer1 = hs_bindgen_8466b284c3d4431b

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_pointer2@
foreign import ccall unsafe "hs_bindgen_3b85cdb3382c54f2" hs_bindgen_3b85cdb3382c54f2_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_pointer2@
hs_bindgen_3b85cdb3382c54f2 ::
     BG.CInt
  -> BG.Ptr (BG.Ptr BG.CInt)
  -> IO ()
hs_bindgen_3b85cdb3382c54f2 =
  \x0 ->
    \x1 ->
      hs_bindgen_3b85cdb3382c54f2_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_pointer2 = hs_bindgen_3b85cdb3382c54f2

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_pointer3@
foreign import ccall unsafe "hs_bindgen_e33fcd9863cc6288" hs_bindgen_e33fcd9863cc6288_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_pointer3@
hs_bindgen_e33fcd9863cc6288 ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()
hs_bindgen_e33fcd9863cc6288 =
  \x0 ->
    \x1 ->
      hs_bindgen_e33fcd9863cc6288_base (BG.toFFIType x0) (BG.toFFIType x1)

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
args_pointer3 = hs_bindgen_e33fcd9863cc6288

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_A@
foreign import ccall unsafe "hs_bindgen_513ee7f0e57addc4" hs_bindgen_513ee7f0e57addc4_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_A@
hs_bindgen_513ee7f0e57addc4 :: IO BG.CInt
hs_bindgen_513ee7f0e57addc4 =
  fmap BG.fromFFIType hs_bindgen_513ee7f0e57addc4_base

{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse.h 47:3@

    __exported by:__ @macros\/reparse.h@
-}
ret_A :: IO BG.CInt
ret_A = hs_bindgen_513ee7f0e57addc4

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_char1@
foreign import ccall unsafe "hs_bindgen_4264d96ca29ff80b" hs_bindgen_4264d96ca29ff80b_base ::
     BG.CInt
  -> IO BG.CChar

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_char1@
hs_bindgen_4264d96ca29ff80b ::
     BG.CInt
  -> IO BG.CChar
hs_bindgen_4264d96ca29ff80b =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_4264d96ca29ff80b_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse.h 49:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CChar
ret_char1 = hs_bindgen_4264d96ca29ff80b

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_char2@
foreign import ccall unsafe "hs_bindgen_2149aa5b45bda802" hs_bindgen_2149aa5b45bda802_base ::
     BG.CInt
  -> IO BG.CSChar

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_char2@
hs_bindgen_2149aa5b45bda802 ::
     BG.CInt
  -> IO BG.CSChar
hs_bindgen_2149aa5b45bda802 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_2149aa5b45bda802_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse.h 50:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CSChar
ret_char2 = hs_bindgen_2149aa5b45bda802

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_char3@
foreign import ccall unsafe "hs_bindgen_be6546432714ba7b" hs_bindgen_be6546432714ba7b_base ::
     BG.CInt
  -> IO BG.CUChar

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_char3@
hs_bindgen_be6546432714ba7b ::
     BG.CInt
  -> IO BG.CUChar
hs_bindgen_be6546432714ba7b =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_be6546432714ba7b_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse.h 51:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_char3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUChar
ret_char3 = hs_bindgen_be6546432714ba7b

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_short1@
foreign import ccall unsafe "hs_bindgen_df7d134e4138e60b" hs_bindgen_df7d134e4138e60b_base ::
     BG.CInt
  -> IO BG.CShort

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_short1@
hs_bindgen_df7d134e4138e60b ::
     BG.CInt
  -> IO BG.CShort
hs_bindgen_df7d134e4138e60b =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_df7d134e4138e60b_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse.h 53:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short1 = hs_bindgen_df7d134e4138e60b

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_short2@
foreign import ccall unsafe "hs_bindgen_e6c53e4add223550" hs_bindgen_e6c53e4add223550_base ::
     BG.CInt
  -> IO BG.CShort

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_short2@
hs_bindgen_e6c53e4add223550 ::
     BG.CInt
  -> IO BG.CShort
hs_bindgen_e6c53e4add223550 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_e6c53e4add223550_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse.h 54:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CShort
ret_short2 = hs_bindgen_e6c53e4add223550

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_short3@
foreign import ccall unsafe "hs_bindgen_52726b17ac086823" hs_bindgen_52726b17ac086823_base ::
     BG.CInt
  -> IO BG.CUShort

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_short3@
hs_bindgen_52726b17ac086823 ::
     BG.CInt
  -> IO BG.CUShort
hs_bindgen_52726b17ac086823 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_52726b17ac086823_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse.h 55:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_short3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUShort
ret_short3 = hs_bindgen_52726b17ac086823

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_int1@
foreign import ccall unsafe "hs_bindgen_a509502c7a89b972" hs_bindgen_a509502c7a89b972_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_int1@
hs_bindgen_a509502c7a89b972 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_a509502c7a89b972 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_a509502c7a89b972_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse.h 57:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int1 = hs_bindgen_a509502c7a89b972

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_int2@
foreign import ccall unsafe "hs_bindgen_729e6154a67de433" hs_bindgen_729e6154a67de433_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_int2@
hs_bindgen_729e6154a67de433 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_729e6154a67de433 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_729e6154a67de433_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse.h 58:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
ret_int2 = hs_bindgen_729e6154a67de433

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_int3@
foreign import ccall unsafe "hs_bindgen_d366c6ac7ba918d0" hs_bindgen_d366c6ac7ba918d0_base ::
     BG.CInt
  -> IO BG.CUInt

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_int3@
hs_bindgen_d366c6ac7ba918d0 ::
     BG.CInt
  -> IO BG.CUInt
hs_bindgen_d366c6ac7ba918d0 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_d366c6ac7ba918d0_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse.h 59:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_int3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CUInt
ret_int3 = hs_bindgen_d366c6ac7ba918d0

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_long1@
foreign import ccall unsafe "hs_bindgen_3db5ceaacf431dfc" hs_bindgen_3db5ceaacf431dfc_base ::
     BG.CInt
  -> IO BG.CLong

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_long1@
hs_bindgen_3db5ceaacf431dfc ::
     BG.CInt
  -> IO BG.CLong
hs_bindgen_3db5ceaacf431dfc =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_3db5ceaacf431dfc_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse.h 61:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long1 = hs_bindgen_3db5ceaacf431dfc

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_long2@
foreign import ccall unsafe "hs_bindgen_0619cc33c91ba89f" hs_bindgen_0619cc33c91ba89f_base ::
     BG.CInt
  -> IO BG.CLong

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_long2@
hs_bindgen_0619cc33c91ba89f ::
     BG.CInt
  -> IO BG.CLong
hs_bindgen_0619cc33c91ba89f =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_0619cc33c91ba89f_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse.h 62:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CLong
ret_long2 = hs_bindgen_0619cc33c91ba89f

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_long3@
foreign import ccall unsafe "hs_bindgen_544c8c26a369c5fd" hs_bindgen_544c8c26a369c5fd_base ::
     BG.CInt
  -> IO BG.CULong

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_long3@
hs_bindgen_544c8c26a369c5fd ::
     BG.CInt
  -> IO BG.CULong
hs_bindgen_544c8c26a369c5fd =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_544c8c26a369c5fd_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse.h 63:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_long3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CULong
ret_long3 = hs_bindgen_544c8c26a369c5fd

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_float@
foreign import ccall unsafe "hs_bindgen_0c167aabaf605fa0" hs_bindgen_0c167aabaf605fa0_base ::
     BG.CInt
  -> IO BG.CFloat

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_float@
hs_bindgen_0c167aabaf605fa0 ::
     BG.CInt
  -> IO BG.CFloat
hs_bindgen_0c167aabaf605fa0 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_0c167aabaf605fa0_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse.h 65:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_float ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CFloat
ret_float = hs_bindgen_0c167aabaf605fa0

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_double@
foreign import ccall unsafe "hs_bindgen_669bb877a0b38399" hs_bindgen_669bb877a0b38399_base ::
     BG.CInt
  -> IO BG.CDouble

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_double@
hs_bindgen_669bb877a0b38399 ::
     BG.CInt
  -> IO BG.CDouble
hs_bindgen_669bb877a0b38399 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_669bb877a0b38399_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse.h 66:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_double ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CDouble
ret_double = hs_bindgen_669bb877a0b38399

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_bool1@
foreign import ccall unsafe "hs_bindgen_1a78afed9902f3dd" hs_bindgen_1a78afed9902f3dd_base ::
     BG.CInt
  -> IO BG.CBool

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_bool1@
hs_bindgen_1a78afed9902f3dd ::
     BG.CInt
  -> IO BG.CBool
hs_bindgen_1a78afed9902f3dd =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_1a78afed9902f3dd_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse.h 67:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_bool1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
ret_bool1 = hs_bindgen_1a78afed9902f3dd

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_struct@
foreign import ccall unsafe "hs_bindgen_8450f45e7a3d4474" hs_bindgen_8450f45e7a3d4474_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_struct@
hs_bindgen_8450f45e7a3d4474 ::
     BG.CInt
  -> BG.Ptr Some_struct
  -> IO ()
hs_bindgen_8450f45e7a3d4474 =
  \x0 ->
    \x1 ->
      hs_bindgen_8450f45e7a3d4474_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_8450f45e7a3d4474 arg10 res1)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_union@
foreign import ccall unsafe "hs_bindgen_c0f3b36b7f529e39" hs_bindgen_c0f3b36b7f529e39_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_union@
hs_bindgen_c0f3b36b7f529e39 ::
     BG.CInt
  -> BG.Ptr Some_union
  -> IO ()
hs_bindgen_c0f3b36b7f529e39 =
  \x0 ->
    \x1 ->
      hs_bindgen_c0f3b36b7f529e39_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_c0f3b36b7f529e39 arg10 res1)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_enum@
foreign import ccall unsafe "hs_bindgen_056dd0d1a9f2d549" hs_bindgen_056dd0d1a9f2d549_base ::
     BG.CInt
  -> IO BG.CUInt

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_enum@
hs_bindgen_056dd0d1a9f2d549 ::
     BG.CInt
  -> IO Some_enum
hs_bindgen_056dd0d1a9f2d549 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_056dd0d1a9f2d549_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse.h 71:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_enum ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO Some_enum
ret_enum = hs_bindgen_056dd0d1a9f2d549

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_pointer1@
foreign import ccall unsafe "hs_bindgen_1c2f0cb441d75bf1" hs_bindgen_1c2f0cb441d75bf1_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_pointer1@
hs_bindgen_1c2f0cb441d75bf1 ::
     BG.CInt
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_1c2f0cb441d75bf1 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_1c2f0cb441d75bf1_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse.h 73:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
ret_pointer1 = hs_bindgen_1c2f0cb441d75bf1

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_pointer2@
foreign import ccall unsafe "hs_bindgen_8bd14c81cd0815dd" hs_bindgen_8bd14c81cd0815dd_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_pointer2@
hs_bindgen_8bd14c81cd0815dd ::
     BG.CInt
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
hs_bindgen_8bd14c81cd0815dd =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_8bd14c81cd0815dd_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse.h 74:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
ret_pointer2 = hs_bindgen_8bd14c81cd0815dd

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_pointer3@
foreign import ccall unsafe "hs_bindgen_03585f3ce3184214" hs_bindgen_03585f3ce3184214_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_pointer3@
hs_bindgen_03585f3ce3184214 ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)
hs_bindgen_03585f3ce3184214 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_03585f3ce3184214_base (BG.toFFIType x0))

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse.h 75:20@

    __exported by:__ @macros\/reparse.h@
-}
ret_pointer3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.Void)
ret_pointer3 = hs_bindgen_03585f3ce3184214

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_body1@
foreign import ccall unsafe "hs_bindgen_522455bed0164d60" hs_bindgen_522455bed0164d60_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_body1@
hs_bindgen_522455bed0164d60 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_522455bed0164d60 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_522455bed0164d60_base (BG.toFFIType x0))

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse.h 79:5@

    __exported by:__ @macros\/reparse.h@
-}
body1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CInt
body1 = hs_bindgen_522455bed0164d60

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_body2@
foreign import ccall unsafe "hs_bindgen_a85103ac19db8ed7" hs_bindgen_a85103ac19db8ed7_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_body2@
hs_bindgen_a85103ac19db8ed7 :: IO BG.CInt
hs_bindgen_a85103ac19db8ed7 =
  fmap BG.fromFFIType hs_bindgen_a85103ac19db8ed7_base

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse.h 80:3@

    __exported by:__ @macros\/reparse.h@
-}
body2 :: IO BG.CInt
body2 = hs_bindgen_a85103ac19db8ed7

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_complex_float@
foreign import ccall unsafe "hs_bindgen_9042c6b8c0c91365" hs_bindgen_9042c6b8c0c91365_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_complex_float@
hs_bindgen_9042c6b8c0c91365 ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_9042c6b8c0c91365 =
  \x0 ->
    \x1 ->
      hs_bindgen_9042c6b8c0c91365_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_9042c6b8c0c91365 arg10 arg22)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_complex_double@
foreign import ccall unsafe "hs_bindgen_5d251ab9fe4ecfe8" hs_bindgen_5d251ab9fe4ecfe8_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_args_complex_double@
hs_bindgen_5d251ab9fe4ecfe8 ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_5d251ab9fe4ecfe8 =
  \x0 ->
    \x1 ->
      hs_bindgen_5d251ab9fe4ecfe8_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_5d251ab9fe4ecfe8 arg10 arg22)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_complex_float@
foreign import ccall unsafe "hs_bindgen_f4eced6daa1ea234" hs_bindgen_f4eced6daa1ea234_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_complex_float@
hs_bindgen_f4eced6daa1ea234 ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_f4eced6daa1ea234 =
  \x0 ->
    \x1 ->
      hs_bindgen_f4eced6daa1ea234_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_f4eced6daa1ea234 arg10 res1)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_complex_double@
foreign import ccall unsafe "hs_bindgen_cba773e73a0e728a" hs_bindgen_cba773e73a0e728a_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_ret_complex_double@
hs_bindgen_cba773e73a0e728a ::
     BG.CInt
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_cba773e73a0e728a =
  \x0 ->
    \x1 ->
      hs_bindgen_cba773e73a0e728a_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                        hs_bindgen_cba773e73a0e728a arg10 res1)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_bespoke_args1@
foreign import ccall unsafe "hs_bindgen_30e771db06ea15e4" hs_bindgen_30e771db06ea15e4_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_bespoke_args1@
hs_bindgen_30e771db06ea15e4 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_30e771db06ea15e4 =
  \x0 ->
    \x1 ->
      hs_bindgen_30e771db06ea15e4_base (BG.toFFIType x0) (BG.toFFIType x1)

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
bespoke_args1 = hs_bindgen_30e771db06ea15e4

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_bespoke_args2@
foreign import ccall unsafe "hs_bindgen_5bab15758ceb1225" hs_bindgen_5bab15758ceb1225_base ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_bespoke_args2@
hs_bindgen_5bab15758ceb1225 ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_5bab15758ceb1225 =
  \x0 ->
    \x1 ->
      hs_bindgen_5bab15758ceb1225_base (BG.toFFIType x0) (BG.toFFIType x1)

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
bespoke_args2 = hs_bindgen_5bab15758ceb1225

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_bespoke_ret1@
foreign import ccall unsafe "hs_bindgen_77956329ffd3c392" hs_bindgen_77956329ffd3c392_base ::
     BG.CInt
  -> IO BG.CBool

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_bespoke_ret1@
hs_bindgen_77956329ffd3c392 ::
     BG.CInt
  -> IO BG.CBool
hs_bindgen_77956329ffd3c392 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_77956329ffd3c392_base (BG.toFFIType x0))

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse.h 97:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO BG.CBool
bespoke_ret1 = hs_bindgen_77956329ffd3c392

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_bespoke_ret2@
foreign import ccall unsafe "hs_bindgen_8207923662f04d2a" hs_bindgen_8207923662f04d2a_base ::
     BG.CInt
  -> IO HsBindgen.Runtime.LibC.CSize

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_bespoke_ret2@
hs_bindgen_8207923662f04d2a ::
     BG.CInt
  -> IO HsBindgen.Runtime.LibC.CSize
hs_bindgen_8207923662f04d2a =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_8207923662f04d2a_base (BG.toFFIType x0))

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse.h 98:8@

    __exported by:__ @macros\/reparse.h@
-}
bespoke_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO HsBindgen.Runtime.LibC.CSize
bespoke_ret2 = hs_bindgen_8207923662f04d2a

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_arr_args1@
foreign import ccall unsafe "hs_bindgen_00a15b83a67898d0" hs_bindgen_00a15b83a67898d0_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_arr_args1@
hs_bindgen_00a15b83a67898d0 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt))
  -> IO ()
hs_bindgen_00a15b83a67898d0 =
  \x0 ->
    hs_bindgen_00a15b83a67898d0_base (BG.toFFIType x0)

{-| Arrays

    __C declaration:__ @arr_args1@

    __defined at:__ @macros\/reparse.h 104:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args1 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args1 = hs_bindgen_00a15b83a67898d0

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_arr_args2@
foreign import ccall unsafe "hs_bindgen_16598c9875843c93" hs_bindgen_16598c9875843c93_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_arr_args2@
hs_bindgen_16598c9875843c93 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_16598c9875843c93 =
  \x0 ->
    hs_bindgen_16598c9875843c93_base (BG.toFFIType x0)

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse.h 105:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args2 = hs_bindgen_16598c9875843c93

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_arr_args3@
foreign import ccall unsafe "hs_bindgen_624d90b7f84a8c16" hs_bindgen_624d90b7f84a8c16_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_arr_args3@
hs_bindgen_624d90b7f84a8c16 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt))
  -> IO ()
hs_bindgen_624d90b7f84a8c16 =
  \x0 ->
    hs_bindgen_624d90b7f84a8c16_base (BG.toFFIType x0)

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse.h 106:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args3 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args3 = hs_bindgen_624d90b7f84a8c16

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_arr_args4@
foreign import ccall unsafe "hs_bindgen_24b495eae0ec3b12" hs_bindgen_24b495eae0ec3b12_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_arr_args4@
hs_bindgen_24b495eae0ec3b12 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_24b495eae0ec3b12 =
  \x0 ->
    hs_bindgen_24b495eae0ec3b12_base (BG.toFFIType x0)

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse.h 107:6@

    __exported by:__ @macros\/reparse.h@
-}
arr_args4 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args4 = hs_bindgen_24b495eae0ec3b12

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_args1@
foreign import ccall unsafe "hs_bindgen_717257f54a5265c1" hs_bindgen_717257f54a5265c1_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_args1@
hs_bindgen_717257f54a5265c1 ::
     BG.CInt
  -> BG.FunPtr (IO ())
  -> IO ()
hs_bindgen_717257f54a5265c1 =
  \x0 ->
    \x1 ->
      hs_bindgen_717257f54a5265c1_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args1 = hs_bindgen_717257f54a5265c1

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_args2@
foreign import ccall unsafe "hs_bindgen_4bfafcca9b11bcf9" hs_bindgen_4bfafcca9b11bcf9_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_args2@
hs_bindgen_4bfafcca9b11bcf9 ::
     BG.CInt
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_4bfafcca9b11bcf9 =
  \x0 ->
    \x1 ->
      hs_bindgen_4bfafcca9b11bcf9_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args2 = hs_bindgen_4bfafcca9b11bcf9

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_args3@
foreign import ccall unsafe "hs_bindgen_66fe19b0eca209c0" hs_bindgen_66fe19b0eca209c0_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_args3@
hs_bindgen_66fe19b0eca209c0 ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> IO ())
  -> IO ()
hs_bindgen_66fe19b0eca209c0 =
  \x0 ->
    \x1 ->
      hs_bindgen_66fe19b0eca209c0_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args3 = hs_bindgen_66fe19b0eca209c0

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_args4@
foreign import ccall unsafe "hs_bindgen_df821c6c63281837" hs_bindgen_df821c6c63281837_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_args4@
hs_bindgen_df821c6c63281837 ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar)
  -> IO ()
hs_bindgen_df821c6c63281837 =
  \x0 ->
    \x1 ->
      hs_bindgen_df821c6c63281837_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args4 = hs_bindgen_df821c6c63281837

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_args5@
foreign import ccall unsafe "hs_bindgen_df0ef6e56db8362a" hs_bindgen_df0ef6e56db8362a_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_args5@
hs_bindgen_df0ef6e56db8362a ::
     BG.CInt
  -> BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))
  -> IO ()
hs_bindgen_df0ef6e56db8362a =
  \x0 ->
    \x1 ->
      hs_bindgen_df0ef6e56db8362a_base (BG.toFFIType x0) (BG.toFFIType x1)

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
funptr_args5 = hs_bindgen_df0ef6e56db8362a

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_comments1@
foreign import ccall unsafe "hs_bindgen_d69eb5037b8ddc21" hs_bindgen_d69eb5037b8ddc21_base ::
     BG.CInt
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_comments1@
hs_bindgen_d69eb5037b8ddc21 ::
     BG.CInt
  -> IO ()
hs_bindgen_d69eb5037b8ddc21 =
  \x0 ->
    hs_bindgen_d69eb5037b8ddc21_base (BG.toFFIType x0)

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
comments1 = hs_bindgen_d69eb5037b8ddc21

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_prim_before1@
foreign import ccall unsafe "hs_bindgen_58980995448953c1" hs_bindgen_58980995448953c1_base ::
     BG.CInt
  -> BG.CChar
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_prim_before1@
hs_bindgen_58980995448953c1 ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_58980995448953c1 =
  \x0 ->
    \x1 ->
      hs_bindgen_58980995448953c1_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_before1 = hs_bindgen_58980995448953c1

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_prim_before2@
foreign import ccall unsafe "hs_bindgen_3bc97d60462ff786" hs_bindgen_3bc97d60462ff786_base ::
     BG.CInt
  -> BG.CSChar
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_prim_before2@
hs_bindgen_3bc97d60462ff786 ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_3bc97d60462ff786 =
  \x0 ->
    \x1 ->
      hs_bindgen_3bc97d60462ff786_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_before2 = hs_bindgen_3bc97d60462ff786

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_prim_before3@
foreign import ccall unsafe "hs_bindgen_d24584345ecfca1b" hs_bindgen_d24584345ecfca1b_base ::
     BG.CInt
  -> BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_prim_before3@
hs_bindgen_d24584345ecfca1b ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_d24584345ecfca1b =
  \x0 ->
    \x1 ->
      hs_bindgen_d24584345ecfca1b_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_before3 = hs_bindgen_d24584345ecfca1b

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_prim_after1@
foreign import ccall unsafe "hs_bindgen_23bf36ae20407d2a" hs_bindgen_23bf36ae20407d2a_base ::
     BG.CInt
  -> BG.CChar
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_prim_after1@
hs_bindgen_23bf36ae20407d2a ::
     BG.CInt
  -> BG.CChar
  -> IO ()
hs_bindgen_23bf36ae20407d2a =
  \x0 ->
    \x1 ->
      hs_bindgen_23bf36ae20407d2a_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_after1 = hs_bindgen_23bf36ae20407d2a

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_prim_after2@
foreign import ccall unsafe "hs_bindgen_01319d392def267f" hs_bindgen_01319d392def267f_base ::
     BG.CInt
  -> BG.CSChar
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_prim_after2@
hs_bindgen_01319d392def267f ::
     BG.CInt
  -> BG.CSChar
  -> IO ()
hs_bindgen_01319d392def267f =
  \x0 ->
    \x1 ->
      hs_bindgen_01319d392def267f_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_after2 = hs_bindgen_01319d392def267f

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_prim_after3@
foreign import ccall unsafe "hs_bindgen_8f6c812963a89b7a" hs_bindgen_8f6c812963a89b7a_base ::
     BG.CInt
  -> BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_prim_after3@
hs_bindgen_8f6c812963a89b7a ::
     BG.CInt
  -> BG.CUChar
  -> IO ()
hs_bindgen_8f6c812963a89b7a =
  \x0 ->
    \x1 ->
      hs_bindgen_8f6c812963a89b7a_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_prim_after3 = hs_bindgen_8f6c812963a89b7a

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_before1@
foreign import ccall unsafe "hs_bindgen_40bd67a0cc536508" hs_bindgen_40bd67a0cc536508_base ::
     BG.CInt
  -> BG.CFloat
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_before1@
hs_bindgen_40bd67a0cc536508 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_40bd67a0cc536508 =
  \x0 ->
    \x1 ->
      hs_bindgen_40bd67a0cc536508_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_40bd67a0cc536508

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_before2@
foreign import ccall unsafe "hs_bindgen_2bb682df4c433aac" hs_bindgen_2bb682df4c433aac_base ::
     BG.CInt
  -> BG.CDouble
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_before2@
hs_bindgen_2bb682df4c433aac ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_2bb682df4c433aac =
  \x0 ->
    \x1 ->
      hs_bindgen_2bb682df4c433aac_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_2bb682df4c433aac

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_before3@
foreign import ccall unsafe "hs_bindgen_80c7819860d5da62" hs_bindgen_80c7819860d5da62_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_before3@
hs_bindgen_80c7819860d5da62 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_80c7819860d5da62 =
  \x0 ->
    \x1 ->
      hs_bindgen_80c7819860d5da62_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_80c7819860d5da62

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_before4@
foreign import ccall unsafe "hs_bindgen_f8b477980ba2c648" hs_bindgen_f8b477980ba2c648_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_before4@
hs_bindgen_f8b477980ba2c648 ::
     BG.CInt
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_f8b477980ba2c648 =
  \x0 ->
    \x1 ->
      hs_bindgen_f8b477980ba2c648_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_f8b477980ba2c648 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_before5@
foreign import ccall unsafe "hs_bindgen_ad63cef18af38159" hs_bindgen_ad63cef18af38159_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_before5@
hs_bindgen_ad63cef18af38159 ::
     BG.CInt
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_ad63cef18af38159 =
  \x0 ->
    \x1 ->
      hs_bindgen_ad63cef18af38159_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_ad63cef18af38159 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_before6@
foreign import ccall unsafe "hs_bindgen_0c52ffc54bdedfee" hs_bindgen_0c52ffc54bdedfee_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_before6@
hs_bindgen_0c52ffc54bdedfee ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_0c52ffc54bdedfee =
  \x0 ->
    \x1 ->
      hs_bindgen_0c52ffc54bdedfee_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_0c52ffc54bdedfee

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_before7@
foreign import ccall unsafe "hs_bindgen_f22aa63ea90a16f1" hs_bindgen_f22aa63ea90a16f1_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_before7@
hs_bindgen_f22aa63ea90a16f1 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_f22aa63ea90a16f1 =
  \x0 ->
    \x1 ->
      hs_bindgen_f22aa63ea90a16f1_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_f22aa63ea90a16f1

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_before8@
foreign import ccall unsafe "hs_bindgen_7e9e1c6fb88e2605" hs_bindgen_7e9e1c6fb88e2605_base ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_before8@
hs_bindgen_7e9e1c6fb88e2605 ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_7e9e1c6fb88e2605 =
  \x0 ->
    \x1 ->
      hs_bindgen_7e9e1c6fb88e2605_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_7e9e1c6fb88e2605

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_after1@
foreign import ccall unsafe "hs_bindgen_6ccb480bf3f85817" hs_bindgen_6ccb480bf3f85817_base ::
     BG.CInt
  -> BG.CFloat
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_after1@
hs_bindgen_6ccb480bf3f85817 ::
     BG.CInt
  -> BG.CFloat
  -> IO ()
hs_bindgen_6ccb480bf3f85817 =
  \x0 ->
    \x1 ->
      hs_bindgen_6ccb480bf3f85817_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_6ccb480bf3f85817

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_after2@
foreign import ccall unsafe "hs_bindgen_a52ffd55ec948935" hs_bindgen_a52ffd55ec948935_base ::
     BG.CInt
  -> BG.CDouble
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_after2@
hs_bindgen_a52ffd55ec948935 ::
     BG.CInt
  -> BG.CDouble
  -> IO ()
hs_bindgen_a52ffd55ec948935 =
  \x0 ->
    \x1 ->
      hs_bindgen_a52ffd55ec948935_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_a52ffd55ec948935

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_after3@
foreign import ccall unsafe "hs_bindgen_480d382b01069870" hs_bindgen_480d382b01069870_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_after3@
hs_bindgen_480d382b01069870 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_480d382b01069870 =
  \x0 ->
    \x1 ->
      hs_bindgen_480d382b01069870_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_480d382b01069870

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_after4@
foreign import ccall unsafe "hs_bindgen_22c0aef39317ea7a" hs_bindgen_22c0aef39317ea7a_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_after4@
hs_bindgen_22c0aef39317ea7a ::
     BG.CInt
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_22c0aef39317ea7a =
  \x0 ->
    \x1 ->
      hs_bindgen_22c0aef39317ea7a_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_22c0aef39317ea7a arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_after5@
foreign import ccall unsafe "hs_bindgen_9813fa26fcd84d87" hs_bindgen_9813fa26fcd84d87_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_after5@
hs_bindgen_9813fa26fcd84d87 ::
     BG.CInt
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_9813fa26fcd84d87 =
  \x0 ->
    \x1 ->
      hs_bindgen_9813fa26fcd84d87_base (BG.toFFIType x0) (BG.toFFIType x1)

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
                       hs_bindgen_9813fa26fcd84d87 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_after6@
foreign import ccall unsafe "hs_bindgen_57929639e21b6021" hs_bindgen_57929639e21b6021_base ::
     BG.CInt
  -> BG.CUInt
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_after6@
hs_bindgen_57929639e21b6021 ::
     BG.CInt
  -> Some_enum
  -> IO ()
hs_bindgen_57929639e21b6021 =
  \x0 ->
    \x1 ->
      hs_bindgen_57929639e21b6021_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_57929639e21b6021

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_after7@
foreign import ccall unsafe "hs_bindgen_e5819093ad177172" hs_bindgen_e5819093ad177172_base ::
     BG.CInt
  -> BG.CBool
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_after7@
hs_bindgen_e5819093ad177172 ::
     BG.CInt
  -> BG.CBool
  -> IO ()
hs_bindgen_e5819093ad177172 =
  \x0 ->
    \x1 ->
      hs_bindgen_e5819093ad177172_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_e5819093ad177172

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_after8@
foreign import ccall unsafe "hs_bindgen_16d2e737dde0cdad" hs_bindgen_16d2e737dde0cdad_base ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_withoutSign_after8@
hs_bindgen_16d2e737dde0cdad ::
     BG.CInt
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_16d2e737dde0cdad =
  \x0 ->
    \x1 ->
      hs_bindgen_16d2e737dde0cdad_base (BG.toFFIType x0) (BG.toFFIType x1)

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
  hs_bindgen_16d2e737dde0cdad

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_args1@
foreign import ccall unsafe "hs_bindgen_7b6273f76aee9d99" hs_bindgen_7b6273f76aee9d99_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_args1@
hs_bindgen_7b6273f76aee9d99 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_7b6273f76aee9d99 =
  \x0 ->
    \x1 ->
      hs_bindgen_7b6273f76aee9d99_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args1 = hs_bindgen_7b6273f76aee9d99

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_args2@
foreign import ccall unsafe "hs_bindgen_14eea7087c075ad2" hs_bindgen_14eea7087c075ad2_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_args2@
hs_bindgen_14eea7087c075ad2 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_14eea7087c075ad2 =
  \x0 ->
    \x1 ->
      hs_bindgen_14eea7087c075ad2_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args2 = hs_bindgen_14eea7087c075ad2

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_args3@
foreign import ccall unsafe "hs_bindgen_4fb0cdd1bf87276b" hs_bindgen_4fb0cdd1bf87276b_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_args3@
hs_bindgen_4fb0cdd1bf87276b ::
     BG.CInt
  -> BG.Ptr BG.CInt
  -> IO ()
hs_bindgen_4fb0cdd1bf87276b =
  \x0 ->
    \x1 ->
      hs_bindgen_4fb0cdd1bf87276b_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args3 = hs_bindgen_4fb0cdd1bf87276b

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_args4@
foreign import ccall unsafe "hs_bindgen_d6adcaa5ac4f2c63" hs_bindgen_d6adcaa5ac4f2c63_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_args4@
hs_bindgen_d6adcaa5ac4f2c63 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_d6adcaa5ac4f2c63 =
  \x0 ->
    \x1 ->
      hs_bindgen_d6adcaa5ac4f2c63_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args4 = hs_bindgen_d6adcaa5ac4f2c63

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_args5@
foreign import ccall unsafe "hs_bindgen_48726e87fe933d81" hs_bindgen_48726e87fe933d81_base ::
     BG.CInt
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_args5@
hs_bindgen_48726e87fe933d81 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> IO ()
hs_bindgen_48726e87fe933d81 =
  \x0 ->
    \x1 ->
      hs_bindgen_48726e87fe933d81_base (BG.toFFIType x0) (BG.toFFIType x1)

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
const_pointers_args5 = hs_bindgen_48726e87fe933d81

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_ret1@
foreign import ccall unsafe "hs_bindgen_34d6770542ed5cfb" hs_bindgen_34d6770542ed5cfb_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_ret1@
hs_bindgen_34d6770542ed5cfb ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_34d6770542ed5cfb =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_34d6770542ed5cfb_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse.h 212:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret1 = hs_bindgen_34d6770542ed5cfb

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_ret2@
foreign import ccall unsafe "hs_bindgen_a7cb3cdd6a21e67b" hs_bindgen_a7cb3cdd6a21e67b_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_ret2@
hs_bindgen_a7cb3cdd6a21e67b ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_a7cb3cdd6a21e67b =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_a7cb3cdd6a21e67b_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse.h 213:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret2 = hs_bindgen_a7cb3cdd6a21e67b

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_ret3@
foreign import ccall unsafe "hs_bindgen_3f091917b4017290" hs_bindgen_3f091917b4017290_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_ret3@
hs_bindgen_3f091917b4017290 ::
     BG.CInt
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_3f091917b4017290 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_3f091917b4017290_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse.h 214:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.Ptr BG.CInt)
const_pointers_ret3 = hs_bindgen_3f091917b4017290

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_ret4@
foreign import ccall unsafe "hs_bindgen_f809746522523301" hs_bindgen_f809746522523301_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_ret4@
hs_bindgen_f809746522523301 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_f809746522523301 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_f809746522523301_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse.h 215:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret4 = hs_bindgen_f809746522523301

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_ret5@
foreign import ccall unsafe "hs_bindgen_b39bdc77bce45435" hs_bindgen_b39bdc77bce45435_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_pointers_ret5@
hs_bindgen_b39bdc77bce45435 ::
     BG.CInt
  -> IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_b39bdc77bce45435 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_b39bdc77bce45435_base (BG.toFFIType x0))

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse.h 216:19@

    __exported by:__ @macros\/reparse.h@
-}
const_pointers_ret5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst BG.CInt)
const_pointers_ret5 = hs_bindgen_b39bdc77bce45435

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_array_elem1@
foreign import ccall unsafe "hs_bindgen_8a3177ed097eb11e" hs_bindgen_8a3177ed097eb11e_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_array_elem1@
hs_bindgen_8a3177ed097eb11e ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt))
  -> IO ()
hs_bindgen_8a3177ed097eb11e =
  \x0 ->
    hs_bindgen_8a3177ed097eb11e_base (BG.toFFIType x0)

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse.h 244:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem1 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem1 = hs_bindgen_8a3177ed097eb11e

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_array_elem2@
foreign import ccall unsafe "hs_bindgen_884c45b9c9202a23" hs_bindgen_884c45b9c9202a23_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_array_elem2@
hs_bindgen_884c45b9c9202a23 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt)))
  -> IO ()
hs_bindgen_884c45b9c9202a23 =
  \x0 ->
    hs_bindgen_884c45b9c9202a23_base (BG.toFFIType x0)

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse.h 245:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem2 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem2 = hs_bindgen_884c45b9c9202a23

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_array_elem3@
foreign import ccall unsafe "hs_bindgen_ef00489eabd9372d" hs_bindgen_ef00489eabd9372d_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_const_array_elem3@
hs_bindgen_ef00489eabd9372d ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
  -> IO ()
hs_bindgen_ef00489eabd9372d =
  \x0 ->
    hs_bindgen_ef00489eabd9372d_base (BG.toFFIType x0)

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse.h 246:6@

    __exported by:__ @macros\/reparse.h@
-}
const_array_elem3 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (BG.Ptr BG.CInt)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem3 = hs_bindgen_ef00489eabd9372d

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_noParams1@
foreign import ccall unsafe "hs_bindgen_ac87d663b48567cc" hs_bindgen_ac87d663b48567cc_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_noParams1@
hs_bindgen_ac87d663b48567cc :: IO BG.CInt
hs_bindgen_ac87d663b48567cc =
  fmap BG.fromFFIType hs_bindgen_ac87d663b48567cc_base

{-| Other examples we reparsed /incorrectly/ before language-c

    __C declaration:__ @noParams1@

    __defined at:__ @macros\/reparse.h 254:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams1 :: IO BG.CInt
noParams1 = hs_bindgen_ac87d663b48567cc

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_noParams2@
foreign import ccall unsafe "hs_bindgen_76a35330ffc46573" hs_bindgen_76a35330ffc46573_base ::
     IO BG.CInt

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_noParams2@
hs_bindgen_76a35330ffc46573 :: IO BG.CInt
hs_bindgen_76a35330ffc46573 =
  fmap BG.fromFFIType hs_bindgen_76a35330ffc46573_base

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse.h 255:3@

    __exported by:__ @macros\/reparse.h@
-}
noParams2 :: IO BG.CInt
noParams2 = hs_bindgen_76a35330ffc46573

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_noParams3@
foreign import ccall unsafe "hs_bindgen_3a7955710fb46bd7" hs_bindgen_3a7955710fb46bd7_base ::
     BG.CInt
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_noParams3@
hs_bindgen_3a7955710fb46bd7 ::
     BG.CInt
  -> BG.FunPtr (IO BG.CInt)
  -> IO ()
hs_bindgen_3a7955710fb46bd7 =
  \x0 ->
    \x1 ->
      hs_bindgen_3a7955710fb46bd7_base (BG.toFFIType x0) (BG.toFFIType x1)

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
noParams3 = hs_bindgen_3a7955710fb46bd7

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret1@
foreign import ccall unsafe "hs_bindgen_be338e12dad77c8d" hs_bindgen_be338e12dad77c8d_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret1@
hs_bindgen_be338e12dad77c8d ::
     BG.CInt
  -> IO (BG.FunPtr (IO ()))
hs_bindgen_be338e12dad77c8d =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_be338e12dad77c8d_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse.h 260:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret1 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO ()))
funptr_ret1 = hs_bindgen_be338e12dad77c8d

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret2@
foreign import ccall unsafe "hs_bindgen_6521ceb04d05fb68" hs_bindgen_6521ceb04d05fb68_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret2@
hs_bindgen_6521ceb04d05fb68 ::
     BG.CInt
  -> IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_6521ceb04d05fb68 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_6521ceb04d05fb68_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse.h 261:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret2 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (IO BG.CInt))
funptr_ret2 = hs_bindgen_6521ceb04d05fb68

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret3@
foreign import ccall unsafe "hs_bindgen_a6bbfb214bdc9399" hs_bindgen_a6bbfb214bdc9399_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret3@
hs_bindgen_a6bbfb214bdc9399 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_a6bbfb214bdc9399 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_a6bbfb214bdc9399_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse.h 262:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret3 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> IO ()))
funptr_ret3 = hs_bindgen_a6bbfb214bdc9399

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret4@
foreign import ccall unsafe "hs_bindgen_ceb193d5c9f9f59c" hs_bindgen_ceb193d5c9f9f59c_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret4@
hs_bindgen_ceb193d5c9f9f59c ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
hs_bindgen_ceb193d5c9f9f59c =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_ceb193d5c9f9f59c_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse.h 263:8@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret4 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO BG.CChar))
funptr_ret4 = hs_bindgen_ceb193d5c9f9f59c

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret5@
foreign import ccall unsafe "hs_bindgen_a6a108ec08d429a6" hs_bindgen_a6a108ec08d429a6_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret5@
hs_bindgen_a6a108ec08d429a6 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_a6a108ec08d429a6 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_a6a108ec08d429a6_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse.h 267:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret5 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret5 = hs_bindgen_a6a108ec08d429a6

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret6@
foreign import ccall unsafe "hs_bindgen_a1f262043255334c" hs_bindgen_a1f262043255334c_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret6@
hs_bindgen_a1f262043255334c ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_a1f262043255334c =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_a1f262043255334c_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse.h 268:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret6 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret6 = hs_bindgen_a1f262043255334c

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret7@
foreign import ccall unsafe "hs_bindgen_7db6e756cb39dbf8" hs_bindgen_7db6e756cb39dbf8_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret7@
hs_bindgen_7db6e756cb39dbf8 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_7db6e756cb39dbf8 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_7db6e756cb39dbf8_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse.h 269:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret7 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret7 = hs_bindgen_7db6e756cb39dbf8

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret8@
foreign import ccall unsafe "hs_bindgen_4210a13033563230" hs_bindgen_4210a13033563230_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret8@
hs_bindgen_4210a13033563230 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
hs_bindgen_4210a13033563230 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_4210a13033563230_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse.h 270:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret8 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)))
funptr_ret8 = hs_bindgen_4210a13033563230

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret9@
foreign import ccall unsafe "hs_bindgen_0b02073743b4ddd5" hs_bindgen_0b02073743b4ddd5_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret9@
hs_bindgen_0b02073743b4ddd5 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_0b02073743b4ddd5 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_0b02073743b4ddd5_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse.h 271:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret9 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret9 = hs_bindgen_0b02073743b4ddd5

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret10@
foreign import ccall unsafe "hs_bindgen_1d0f3906df1077d1" hs_bindgen_1d0f3906df1077d1_base ::
     BG.CInt
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty Example Unsafe_funptr_ret10@
hs_bindgen_1d0f3906df1077d1 ::
     BG.CInt
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
hs_bindgen_1d0f3906df1077d1 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_1d0f3906df1077d1_base (BG.toFFIType x0))

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse.h 272:20@

    __exported by:__ @macros\/reparse.h@
-}
funptr_ret10 ::
     BG.CInt
     -- ^ __C declaration:__ @arg1@
  -> IO (BG.FunPtr (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)))
funptr_ret10 = hs_bindgen_1d0f3906df1077d1
