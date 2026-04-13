{-# LANGUAGE CApiFFI #-}
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
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/reparse/reparse.h>"
  , "void hs_bindgen_460e7fb658b47e9b ("
  , "  A arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  (args_char1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_bcdc5d3b4fcf134c ("
  , "  A arg1,"
  , "  signed char arg2"
  , ")"
  , "{"
  , "  (args_char2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3945fe2352e08419 ("
  , "  A arg1,"
  , "  unsigned char arg2"
  , ")"
  , "{"
  , "  (args_char3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8473240c808c9c25 ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a99b3f3d27d1d634 ("
  , "  A arg1,"
  , "  signed short arg2"
  , ")"
  , "{"
  , "  (args_short2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_676f60932b4103cd ("
  , "  A arg1,"
  , "  unsigned short arg2"
  , ")"
  , "{"
  , "  (args_short3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f01f3fdd4bab2602 ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e22ee7b306d070fe ("
  , "  A arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (args_int2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_be3b0db66df59e13 ("
  , "  A arg1,"
  , "  unsigned int arg2"
  , ")"
  , "{"
  , "  (args_int3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a6bffd35b7be4030 ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_fbd31067f9fba352 ("
  , "  A arg1,"
  , "  signed long arg2"
  , ")"
  , "{"
  , "  (args_long2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_74e72dd19ac89311 ("
  , "  A arg1,"
  , "  unsigned long arg2"
  , ")"
  , "{"
  , "  (args_long3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3f755bf68e8ae89b ("
  , "  A arg1,"
  , "  float arg2"
  , ")"
  , "{"
  , "  (args_float)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b655d78473b77a93 ("
  , "  A arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  (args_double)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8a9f79213eb2e379 ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (args_bool1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_07eea2e8406c843d ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  (args_struct)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_5f4927e2bb1867dd ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  (args_union)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_2c2049ef404298a2 ("
  , "  A arg1,"
  , "  enum some_enum arg2"
  , ")"
  , "{"
  , "  (args_enum)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_69c4385332f84662 ("
  , "  A arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  (args_pointer1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ca4e232742d36476 ("
  , "  A arg1,"
  , "  signed int **arg2"
  , ")"
  , "{"
  , "  (args_pointer2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8bcc278199cfd082 ("
  , "  A arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  (args_pointer3)(arg1, arg2);"
  , "}"
  , "A hs_bindgen_09c9ac0b5dfe0799 (void)"
  , "{"
  , "  return (ret_A)();"
  , "}"
  , "char hs_bindgen_d6f8af7763a6c2c4 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_char1)(arg1);"
  , "}"
  , "signed char hs_bindgen_690a5a180d2f56f8 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_char2)(arg1);"
  , "}"
  , "unsigned char hs_bindgen_2aa902e2bd1fdfc6 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_char3)(arg1);"
  , "}"
  , "signed short hs_bindgen_e09928a71c52ac3c ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_short1)(arg1);"
  , "}"
  , "signed short hs_bindgen_096857e231b66301 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_short2)(arg1);"
  , "}"
  , "unsigned short hs_bindgen_4f714262fa5481dd ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_short3)(arg1);"
  , "}"
  , "signed int hs_bindgen_06bb7b14a590d5da ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_int1)(arg1);"
  , "}"
  , "signed int hs_bindgen_bbb678d408513a3e ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_int2)(arg1);"
  , "}"
  , "unsigned int hs_bindgen_26c8b6d93bb1cbb8 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_int3)(arg1);"
  , "}"
  , "signed long hs_bindgen_166ec9de0ba6872c ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_long1)(arg1);"
  , "}"
  , "signed long hs_bindgen_24288ce1fb5717fd ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_long2)(arg1);"
  , "}"
  , "unsigned long hs_bindgen_f6b924ab56a68563 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_long3)(arg1);"
  , "}"
  , "float hs_bindgen_631381acb79a0c16 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_float)(arg1);"
  , "}"
  , "double hs_bindgen_2c13977f9a686567 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_double)(arg1);"
  , "}"
  , "_Bool hs_bindgen_fa00837e91fcb28e ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_bool1)(arg1);"
  , "}"
  , "void hs_bindgen_8c2f27edb0844f6f ("
  , "  A arg1,"
  , "  struct some_struct *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_struct)(arg1);"
  , "}"
  , "void hs_bindgen_075eba6e653b1e77 ("
  , "  A arg1,"
  , "  union some_union *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_union)(arg1);"
  , "}"
  , "enum some_enum hs_bindgen_d24ca05277d86321 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_enum)(arg1);"
  , "}"
  , "signed int *hs_bindgen_8b8137d2d216adaa ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_pointer1)(arg1);"
  , "}"
  , "signed int **hs_bindgen_9cc91cd81871702f ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_pointer2)(arg1);"
  , "}"
  , "void *hs_bindgen_95489c088e6ab546 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (ret_pointer3)(arg1);"
  , "}"
  , "signed int hs_bindgen_4a2abe76938abf43 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (body1)(arg1);"
  , "}"
  , "A hs_bindgen_8b29007fcb697c25 (void)"
  , "{"
  , "  return (body2)();"
  , "}"
  , "void hs_bindgen_e0c33978335f65bc ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_float)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_a7670557e1b51a62 ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  (args_complex_double)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_106b0103af79bd03 ("
  , "  A arg1,"
  , "  float _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_float)(arg1);"
  , "}"
  , "void hs_bindgen_605b16bb46338d7a ("
  , "  A arg1,"
  , "  double _Complex *arg2"
  , ")"
  , "{"
  , "  *arg2 = (ret_complex_double)(arg1);"
  , "}"
  , "void hs_bindgen_af3122ddd03619db ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (bespoke_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c64666a0f5462c01 ("
  , "  A arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  (bespoke_args2)(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_62c0105b0a3b0e90 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret1)(arg1);"
  , "}"
  , "size_t hs_bindgen_6e98636dcad638ea ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (bespoke_ret2)(arg1);"
  , "}"
  , "void hs_bindgen_08dea8d98ff039d8 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  (arr_args1)(arg1);"
  , "}"
  , "void hs_bindgen_a9d90769b712b0ae ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  (arr_args2)(arg1);"
  , "}"
  , "void hs_bindgen_35310a933bc48e94 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  (arr_args3)(arg1);"
  , "}"
  , "void hs_bindgen_299f9da613a8d020 ("
  , "  A **arg1"
  , ")"
  , "{"
  , "  (arr_args4)(arg1);"
  , "}"
  , "void hs_bindgen_04a94602ff36823f ("
  , "  A arg1,"
  , "  void (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f35f821458786a50 ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (funptr_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b62e35b8b42f6c72 ("
  , "  A arg1,"
  , "  void (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4a20f35683112bbf ("
  , "  A arg1,"
  , "  char (*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f1d463403b067ed0 ("
  , "  A arg1,"
  , "  signed int *(*arg2) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , ")"
  , "{"
  , "  (funptr_args5)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6e28d41852baa736 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  (comments1)(arg1);"
  , "}"
  , "void hs_bindgen_3fa5c5a5148a9824 ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_461b29b08c844b7c ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_48a2759c1b58a757 ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_018db92a1fdc0d50 ("
  , "  A arg1,"
  , "  char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a2c532d8f7f39797 ("
  , "  A arg1,"
  , "  signed char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ef823e9dcc43fc36 ("
  , "  A arg1,"
  , "  unsigned char const arg2"
  , ")"
  , "{"
  , "  (const_prim_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a2674612dbe7213a ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_64cbf4ecc90ce01e ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_51d96a6c013cde65 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6db406eb239af0dd ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_92509acd98e266b1 ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_8a68a36f7692ffec ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_3f35c090df3d9ea9 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_11937823df53d81c ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_before8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_293ae8d24cbffa78 ("
  , "  A arg1,"
  , "  float const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_533859b45983018a ("
  , "  A arg1,"
  , "  double const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4dbb63bf826187f3 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a27df069efecacb6 ("
  , "  A arg1,"
  , "  struct some_struct const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after4)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_d91b7cd58ad58d7a ("
  , "  A arg1,"
  , "  union some_union const *arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after5)(arg1, *arg2);"
  , "}"
  , "void hs_bindgen_d1d894d1536c7540 ("
  , "  A arg1,"
  , "  enum some_enum const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after6)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b10294ffc1a88b54 ("
  , "  A arg1,"
  , "  _Bool const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after7)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4f6beef21554a89c ("
  , "  A arg1,"
  , "  size_t const arg2"
  , ")"
  , "{"
  , "  (const_withoutSign_after8)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_69312ba543a639b9 ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ade674ed0d522b2c ("
  , "  A arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  (const_pointers_args2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9ac414106b3e9dcc ("
  , "  A arg1,"
  , "  signed int *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6f54b8081c103a22 ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args4)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_07bdc9016b39f512 ("
  , "  A arg1,"
  , "  signed int const *const arg2"
  , ")"
  , "{"
  , "  (const_pointers_args5)(arg1, arg2);"
  , "}"
  , "signed int const *hs_bindgen_61ae256f76ba18fb ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret1)(arg1);"
  , "}"
  , "signed int const *hs_bindgen_8301f1156ba87562 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret2)(arg1);"
  , "}"
  , "signed int *const hs_bindgen_8188e773ffb5dc3c ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret3)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_6a2eee73cb0d7eba ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret4)(arg1);"
  , "}"
  , "signed int const *const hs_bindgen_83129cee1b8daed1 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return (const_pointers_ret5)(arg1);"
  , "}"
  , "void hs_bindgen_ac03ac62a99836c9 ("
  , "  A const *arg1"
  , ")"
  , "{"
  , "  (const_array_elem1)(arg1);"
  , "}"
  , "void hs_bindgen_332eb4f162aae214 ("
  , "  A const **arg1"
  , ")"
  , "{"
  , "  (const_array_elem2)(arg1);"
  , "}"
  , "void hs_bindgen_678c169d19173222 ("
  , "  A *const *arg1"
  , ")"
  , "{"
  , "  (const_array_elem3)(arg1);"
  , "}"
  , "A hs_bindgen_eb8239a8e7f57e7b (void)"
  , "{"
  , "  return (noParams1)();"
  , "}"
  , "A hs_bindgen_56ef4fc40eb6685c (void)"
  , "{"
  , "  return (noParams2)();"
  , "}"
  , "void hs_bindgen_e901eaca204bbd3b ("
  , "  A arg1,"
  , "  signed int (*arg2) (void)"
  , ")"
  , "{"
  , "  (noParams3)(arg1, arg2);"
  , "}"
  , "void (*hs_bindgen_18221abe0529c70a ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_83435e385a318b88 ("
  , "  A arg1"
  , ")) (void)"
  , "{"
  , "  return (funptr_ret2)(arg1);"
  , "}"
  , "void (*hs_bindgen_b8a6302e29fba35d ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (funptr_ret3)(arg1);"
  , "}"
  , "char (*hs_bindgen_ade99ff1ca3e1ab4 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret4)(arg1);"
  , "}"
  , "signed int *(*hs_bindgen_d94322ca936c5318 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret5)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_259eac69be80e6e5 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret6)(arg1);"
  , "}"
  , "signed int const *(*hs_bindgen_6062b9d1a54078cd ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret7)(arg1);"
  , "}"
  , "signed int *const (*hs_bindgen_8d832ca80d7b94e5 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret8)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_5c6db1c615f7065d ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret9)(arg1);"
  , "}"
  , "signed int const *const (*hs_bindgen_7d06f58b43dabd28 ("
  , "  A arg1"
  , ")) ("
  , "  signed int arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (funptr_ret10)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_char1@
foreign import ccall unsafe "hs_bindgen_460e7fb658b47e9b" hs_bindgen_460e7fb658b47e9b_base ::
     RIP.Int32
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_char1@
hs_bindgen_460e7fb658b47e9b ::
     A
  -> RIP.CChar
  -> IO ()
hs_bindgen_460e7fb658b47e9b =
  RIP.fromFFIType hs_bindgen_460e7fb658b47e9b_base

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
args_char1 = hs_bindgen_460e7fb658b47e9b

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_char2@
foreign import ccall unsafe "hs_bindgen_bcdc5d3b4fcf134c" hs_bindgen_bcdc5d3b4fcf134c_base ::
     RIP.Int32
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_char2@
hs_bindgen_bcdc5d3b4fcf134c ::
     A
  -> RIP.CSChar
  -> IO ()
hs_bindgen_bcdc5d3b4fcf134c =
  RIP.fromFFIType hs_bindgen_bcdc5d3b4fcf134c_base

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
args_char2 = hs_bindgen_bcdc5d3b4fcf134c

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_char3@
foreign import ccall unsafe "hs_bindgen_3945fe2352e08419" hs_bindgen_3945fe2352e08419_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_char3@
hs_bindgen_3945fe2352e08419 ::
     A
  -> RIP.CUChar
  -> IO ()
hs_bindgen_3945fe2352e08419 =
  RIP.fromFFIType hs_bindgen_3945fe2352e08419_base

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
args_char3 = hs_bindgen_3945fe2352e08419

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_short1@
foreign import ccall unsafe "hs_bindgen_8473240c808c9c25" hs_bindgen_8473240c808c9c25_base ::
     RIP.Int32
  -> RIP.Int16
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_short1@
hs_bindgen_8473240c808c9c25 ::
     A
  -> RIP.CShort
  -> IO ()
hs_bindgen_8473240c808c9c25 =
  RIP.fromFFIType hs_bindgen_8473240c808c9c25_base

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
args_short1 = hs_bindgen_8473240c808c9c25

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_short2@
foreign import ccall unsafe "hs_bindgen_a99b3f3d27d1d634" hs_bindgen_a99b3f3d27d1d634_base ::
     RIP.Int32
  -> RIP.Int16
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_short2@
hs_bindgen_a99b3f3d27d1d634 ::
     A
  -> RIP.CShort
  -> IO ()
hs_bindgen_a99b3f3d27d1d634 =
  RIP.fromFFIType hs_bindgen_a99b3f3d27d1d634_base

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
args_short2 = hs_bindgen_a99b3f3d27d1d634

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_short3@
foreign import ccall unsafe "hs_bindgen_676f60932b4103cd" hs_bindgen_676f60932b4103cd_base ::
     RIP.Int32
  -> RIP.Word16
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_short3@
hs_bindgen_676f60932b4103cd ::
     A
  -> RIP.CUShort
  -> IO ()
hs_bindgen_676f60932b4103cd =
  RIP.fromFFIType hs_bindgen_676f60932b4103cd_base

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
args_short3 = hs_bindgen_676f60932b4103cd

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_int1@
foreign import ccall unsafe "hs_bindgen_f01f3fdd4bab2602" hs_bindgen_f01f3fdd4bab2602_base ::
     RIP.Int32
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_int1@
hs_bindgen_f01f3fdd4bab2602 ::
     A
  -> RIP.CInt
  -> IO ()
hs_bindgen_f01f3fdd4bab2602 =
  RIP.fromFFIType hs_bindgen_f01f3fdd4bab2602_base

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
args_int1 = hs_bindgen_f01f3fdd4bab2602

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_int2@
foreign import ccall unsafe "hs_bindgen_e22ee7b306d070fe" hs_bindgen_e22ee7b306d070fe_base ::
     RIP.Int32
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_int2@
hs_bindgen_e22ee7b306d070fe ::
     A
  -> RIP.CInt
  -> IO ()
hs_bindgen_e22ee7b306d070fe =
  RIP.fromFFIType hs_bindgen_e22ee7b306d070fe_base

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
args_int2 = hs_bindgen_e22ee7b306d070fe

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_int3@
foreign import ccall unsafe "hs_bindgen_be3b0db66df59e13" hs_bindgen_be3b0db66df59e13_base ::
     RIP.Int32
  -> RIP.Word32
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_int3@
hs_bindgen_be3b0db66df59e13 ::
     A
  -> RIP.CUInt
  -> IO ()
hs_bindgen_be3b0db66df59e13 =
  RIP.fromFFIType hs_bindgen_be3b0db66df59e13_base

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
args_int3 = hs_bindgen_be3b0db66df59e13

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_long1@
foreign import ccall unsafe "hs_bindgen_a6bffd35b7be4030" hs_bindgen_a6bffd35b7be4030_base ::
     RIP.Int32
  -> RIP.Int64
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_long1@
hs_bindgen_a6bffd35b7be4030 ::
     A
  -> RIP.CLong
  -> IO ()
hs_bindgen_a6bffd35b7be4030 =
  RIP.fromFFIType hs_bindgen_a6bffd35b7be4030_base

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
args_long1 = hs_bindgen_a6bffd35b7be4030

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_long2@
foreign import ccall unsafe "hs_bindgen_fbd31067f9fba352" hs_bindgen_fbd31067f9fba352_base ::
     RIP.Int32
  -> RIP.Int64
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_long2@
hs_bindgen_fbd31067f9fba352 ::
     A
  -> RIP.CLong
  -> IO ()
hs_bindgen_fbd31067f9fba352 =
  RIP.fromFFIType hs_bindgen_fbd31067f9fba352_base

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
args_long2 = hs_bindgen_fbd31067f9fba352

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_long3@
foreign import ccall unsafe "hs_bindgen_74e72dd19ac89311" hs_bindgen_74e72dd19ac89311_base ::
     RIP.Int32
  -> RIP.Word64
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_long3@
hs_bindgen_74e72dd19ac89311 ::
     A
  -> RIP.CULong
  -> IO ()
hs_bindgen_74e72dd19ac89311 =
  RIP.fromFFIType hs_bindgen_74e72dd19ac89311_base

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
args_long3 = hs_bindgen_74e72dd19ac89311

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_float@
foreign import ccall unsafe "hs_bindgen_3f755bf68e8ae89b" hs_bindgen_3f755bf68e8ae89b_base ::
     RIP.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_float@
hs_bindgen_3f755bf68e8ae89b ::
     A
  -> RIP.CFloat
  -> IO ()
hs_bindgen_3f755bf68e8ae89b =
  RIP.fromFFIType hs_bindgen_3f755bf68e8ae89b_base

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
args_float = hs_bindgen_3f755bf68e8ae89b

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_double@
foreign import ccall unsafe "hs_bindgen_b655d78473b77a93" hs_bindgen_b655d78473b77a93_base ::
     RIP.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_double@
hs_bindgen_b655d78473b77a93 ::
     A
  -> RIP.CDouble
  -> IO ()
hs_bindgen_b655d78473b77a93 =
  RIP.fromFFIType hs_bindgen_b655d78473b77a93_base

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
args_double = hs_bindgen_b655d78473b77a93

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_bool1@
foreign import ccall unsafe "hs_bindgen_8a9f79213eb2e379" hs_bindgen_8a9f79213eb2e379_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_bool1@
hs_bindgen_8a9f79213eb2e379 ::
     A
  -> RIP.CBool
  -> IO ()
hs_bindgen_8a9f79213eb2e379 =
  RIP.fromFFIType hs_bindgen_8a9f79213eb2e379_base

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
args_bool1 = hs_bindgen_8a9f79213eb2e379

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_struct@
foreign import ccall unsafe "hs_bindgen_07eea2e8406c843d" hs_bindgen_07eea2e8406c843d_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_struct@
hs_bindgen_07eea2e8406c843d ::
     A
  -> RIP.Ptr Some_struct
  -> IO ()
hs_bindgen_07eea2e8406c843d =
  RIP.fromFFIType hs_bindgen_07eea2e8406c843d_base

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
                        hs_bindgen_07eea2e8406c843d arg10 arg22)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_union@
foreign import ccall unsafe "hs_bindgen_5f4927e2bb1867dd" hs_bindgen_5f4927e2bb1867dd_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_union@
hs_bindgen_5f4927e2bb1867dd ::
     A
  -> RIP.Ptr Some_union
  -> IO ()
hs_bindgen_5f4927e2bb1867dd =
  RIP.fromFFIType hs_bindgen_5f4927e2bb1867dd_base

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
                        hs_bindgen_5f4927e2bb1867dd arg10 arg22)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_enum@
foreign import ccall unsafe "hs_bindgen_2c2049ef404298a2" hs_bindgen_2c2049ef404298a2_base ::
     RIP.Int32
  -> RIP.Word32
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_enum@
hs_bindgen_2c2049ef404298a2 ::
     A
  -> Some_enum
  -> IO ()
hs_bindgen_2c2049ef404298a2 =
  RIP.fromFFIType hs_bindgen_2c2049ef404298a2_base

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
args_enum = hs_bindgen_2c2049ef404298a2

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_pointer1@
foreign import ccall unsafe "hs_bindgen_69c4385332f84662" hs_bindgen_69c4385332f84662_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_pointer1@
hs_bindgen_69c4385332f84662 ::
     A
  -> RIP.Ptr RIP.CInt
  -> IO ()
hs_bindgen_69c4385332f84662 =
  RIP.fromFFIType hs_bindgen_69c4385332f84662_base

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
args_pointer1 = hs_bindgen_69c4385332f84662

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_pointer2@
foreign import ccall unsafe "hs_bindgen_ca4e232742d36476" hs_bindgen_ca4e232742d36476_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_pointer2@
hs_bindgen_ca4e232742d36476 ::
     A
  -> RIP.Ptr (RIP.Ptr RIP.CInt)
  -> IO ()
hs_bindgen_ca4e232742d36476 =
  RIP.fromFFIType hs_bindgen_ca4e232742d36476_base

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
args_pointer2 = hs_bindgen_ca4e232742d36476

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_pointer3@
foreign import ccall unsafe "hs_bindgen_8bcc278199cfd082" hs_bindgen_8bcc278199cfd082_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_pointer3@
hs_bindgen_8bcc278199cfd082 ::
     A
  -> RIP.Ptr RIP.Void
  -> IO ()
hs_bindgen_8bcc278199cfd082 =
  RIP.fromFFIType hs_bindgen_8bcc278199cfd082_base

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
args_pointer3 = hs_bindgen_8bcc278199cfd082

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_A@
foreign import ccall unsafe "hs_bindgen_09c9ac0b5dfe0799" hs_bindgen_09c9ac0b5dfe0799_base ::
     IO RIP.Int32

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_A@
hs_bindgen_09c9ac0b5dfe0799 :: IO A
hs_bindgen_09c9ac0b5dfe0799 =
  RIP.fromFFIType hs_bindgen_09c9ac0b5dfe0799_base

{-| __C declaration:__ @ret_A@

    __defined at:__ @macros\/reparse\/reparse.h 47:3@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_A :: IO A
ret_A = hs_bindgen_09c9ac0b5dfe0799

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_char1@
foreign import ccall unsafe "hs_bindgen_d6f8af7763a6c2c4" hs_bindgen_d6f8af7763a6c2c4_base ::
     RIP.Int32
  -> IO RIP.Int8

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_char1@
hs_bindgen_d6f8af7763a6c2c4 ::
     A
  -> IO RIP.CChar
hs_bindgen_d6f8af7763a6c2c4 =
  RIP.fromFFIType hs_bindgen_d6f8af7763a6c2c4_base

{-| __C declaration:__ @ret_char1@

    __defined at:__ @macros\/reparse\/reparse.h 49:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_char1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CChar
ret_char1 = hs_bindgen_d6f8af7763a6c2c4

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_char2@
foreign import ccall unsafe "hs_bindgen_690a5a180d2f56f8" hs_bindgen_690a5a180d2f56f8_base ::
     RIP.Int32
  -> IO RIP.Int8

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_char2@
hs_bindgen_690a5a180d2f56f8 ::
     A
  -> IO RIP.CSChar
hs_bindgen_690a5a180d2f56f8 =
  RIP.fromFFIType hs_bindgen_690a5a180d2f56f8_base

{-| __C declaration:__ @ret_char2@

    __defined at:__ @macros\/reparse\/reparse.h 50:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_char2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CSChar
ret_char2 = hs_bindgen_690a5a180d2f56f8

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_char3@
foreign import ccall unsafe "hs_bindgen_2aa902e2bd1fdfc6" hs_bindgen_2aa902e2bd1fdfc6_base ::
     RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_char3@
hs_bindgen_2aa902e2bd1fdfc6 ::
     A
  -> IO RIP.CUChar
hs_bindgen_2aa902e2bd1fdfc6 =
  RIP.fromFFIType hs_bindgen_2aa902e2bd1fdfc6_base

{-| __C declaration:__ @ret_char3@

    __defined at:__ @macros\/reparse\/reparse.h 51:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_char3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CUChar
ret_char3 = hs_bindgen_2aa902e2bd1fdfc6

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_short1@
foreign import ccall unsafe "hs_bindgen_e09928a71c52ac3c" hs_bindgen_e09928a71c52ac3c_base ::
     RIP.Int32
  -> IO RIP.Int16

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_short1@
hs_bindgen_e09928a71c52ac3c ::
     A
  -> IO RIP.CShort
hs_bindgen_e09928a71c52ac3c =
  RIP.fromFFIType hs_bindgen_e09928a71c52ac3c_base

{-| __C declaration:__ @ret_short1@

    __defined at:__ @macros\/reparse\/reparse.h 53:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_short1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CShort
ret_short1 = hs_bindgen_e09928a71c52ac3c

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_short2@
foreign import ccall unsafe "hs_bindgen_096857e231b66301" hs_bindgen_096857e231b66301_base ::
     RIP.Int32
  -> IO RIP.Int16

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_short2@
hs_bindgen_096857e231b66301 ::
     A
  -> IO RIP.CShort
hs_bindgen_096857e231b66301 =
  RIP.fromFFIType hs_bindgen_096857e231b66301_base

{-| __C declaration:__ @ret_short2@

    __defined at:__ @macros\/reparse\/reparse.h 54:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_short2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CShort
ret_short2 = hs_bindgen_096857e231b66301

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_short3@
foreign import ccall unsafe "hs_bindgen_4f714262fa5481dd" hs_bindgen_4f714262fa5481dd_base ::
     RIP.Int32
  -> IO RIP.Word16

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_short3@
hs_bindgen_4f714262fa5481dd ::
     A
  -> IO RIP.CUShort
hs_bindgen_4f714262fa5481dd =
  RIP.fromFFIType hs_bindgen_4f714262fa5481dd_base

{-| __C declaration:__ @ret_short3@

    __defined at:__ @macros\/reparse\/reparse.h 55:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_short3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CUShort
ret_short3 = hs_bindgen_4f714262fa5481dd

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_int1@
foreign import ccall unsafe "hs_bindgen_06bb7b14a590d5da" hs_bindgen_06bb7b14a590d5da_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_int1@
hs_bindgen_06bb7b14a590d5da ::
     A
  -> IO RIP.CInt
hs_bindgen_06bb7b14a590d5da =
  RIP.fromFFIType hs_bindgen_06bb7b14a590d5da_base

{-| __C declaration:__ @ret_int1@

    __defined at:__ @macros\/reparse\/reparse.h 57:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_int1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CInt
ret_int1 = hs_bindgen_06bb7b14a590d5da

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_int2@
foreign import ccall unsafe "hs_bindgen_bbb678d408513a3e" hs_bindgen_bbb678d408513a3e_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_int2@
hs_bindgen_bbb678d408513a3e ::
     A
  -> IO RIP.CInt
hs_bindgen_bbb678d408513a3e =
  RIP.fromFFIType hs_bindgen_bbb678d408513a3e_base

{-| __C declaration:__ @ret_int2@

    __defined at:__ @macros\/reparse\/reparse.h 58:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_int2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CInt
ret_int2 = hs_bindgen_bbb678d408513a3e

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_int3@
foreign import ccall unsafe "hs_bindgen_26c8b6d93bb1cbb8" hs_bindgen_26c8b6d93bb1cbb8_base ::
     RIP.Int32
  -> IO RIP.Word32

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_int3@
hs_bindgen_26c8b6d93bb1cbb8 ::
     A
  -> IO RIP.CUInt
hs_bindgen_26c8b6d93bb1cbb8 =
  RIP.fromFFIType hs_bindgen_26c8b6d93bb1cbb8_base

{-| __C declaration:__ @ret_int3@

    __defined at:__ @macros\/reparse\/reparse.h 59:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_int3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CUInt
ret_int3 = hs_bindgen_26c8b6d93bb1cbb8

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_long1@
foreign import ccall unsafe "hs_bindgen_166ec9de0ba6872c" hs_bindgen_166ec9de0ba6872c_base ::
     RIP.Int32
  -> IO RIP.Int64

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_long1@
hs_bindgen_166ec9de0ba6872c ::
     A
  -> IO RIP.CLong
hs_bindgen_166ec9de0ba6872c =
  RIP.fromFFIType hs_bindgen_166ec9de0ba6872c_base

{-| __C declaration:__ @ret_long1@

    __defined at:__ @macros\/reparse\/reparse.h 61:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_long1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CLong
ret_long1 = hs_bindgen_166ec9de0ba6872c

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_long2@
foreign import ccall unsafe "hs_bindgen_24288ce1fb5717fd" hs_bindgen_24288ce1fb5717fd_base ::
     RIP.Int32
  -> IO RIP.Int64

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_long2@
hs_bindgen_24288ce1fb5717fd ::
     A
  -> IO RIP.CLong
hs_bindgen_24288ce1fb5717fd =
  RIP.fromFFIType hs_bindgen_24288ce1fb5717fd_base

{-| __C declaration:__ @ret_long2@

    __defined at:__ @macros\/reparse\/reparse.h 62:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_long2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CLong
ret_long2 = hs_bindgen_24288ce1fb5717fd

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_long3@
foreign import ccall unsafe "hs_bindgen_f6b924ab56a68563" hs_bindgen_f6b924ab56a68563_base ::
     RIP.Int32
  -> IO RIP.Word64

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_long3@
hs_bindgen_f6b924ab56a68563 ::
     A
  -> IO RIP.CULong
hs_bindgen_f6b924ab56a68563 =
  RIP.fromFFIType hs_bindgen_f6b924ab56a68563_base

{-| __C declaration:__ @ret_long3@

    __defined at:__ @macros\/reparse\/reparse.h 63:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_long3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CULong
ret_long3 = hs_bindgen_f6b924ab56a68563

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_float@
foreign import ccall unsafe "hs_bindgen_631381acb79a0c16" hs_bindgen_631381acb79a0c16_base ::
     RIP.Int32
  -> IO Float

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_float@
hs_bindgen_631381acb79a0c16 ::
     A
  -> IO RIP.CFloat
hs_bindgen_631381acb79a0c16 =
  RIP.fromFFIType hs_bindgen_631381acb79a0c16_base

{-| __C declaration:__ @ret_float@

    __defined at:__ @macros\/reparse\/reparse.h 65:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_float ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CFloat
ret_float = hs_bindgen_631381acb79a0c16

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_double@
foreign import ccall unsafe "hs_bindgen_2c13977f9a686567" hs_bindgen_2c13977f9a686567_base ::
     RIP.Int32
  -> IO Double

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_double@
hs_bindgen_2c13977f9a686567 ::
     A
  -> IO RIP.CDouble
hs_bindgen_2c13977f9a686567 =
  RIP.fromFFIType hs_bindgen_2c13977f9a686567_base

{-| __C declaration:__ @ret_double@

    __defined at:__ @macros\/reparse\/reparse.h 66:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_double ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CDouble
ret_double = hs_bindgen_2c13977f9a686567

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_bool1@
foreign import ccall unsafe "hs_bindgen_fa00837e91fcb28e" hs_bindgen_fa00837e91fcb28e_base ::
     RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_bool1@
hs_bindgen_fa00837e91fcb28e ::
     A
  -> IO RIP.CBool
hs_bindgen_fa00837e91fcb28e =
  RIP.fromFFIType hs_bindgen_fa00837e91fcb28e_base

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @macros\/reparse\/reparse.h 67:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_bool1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CBool
ret_bool1 = hs_bindgen_fa00837e91fcb28e

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_struct@
foreign import ccall unsafe "hs_bindgen_8c2f27edb0844f6f" hs_bindgen_8c2f27edb0844f6f_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_struct@
hs_bindgen_8c2f27edb0844f6f ::
     A
  -> RIP.Ptr Some_struct
  -> IO ()
hs_bindgen_8c2f27edb0844f6f =
  RIP.fromFFIType hs_bindgen_8c2f27edb0844f6f_base

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
                         hs_bindgen_8c2f27edb0844f6f arg10 res1)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_union@
foreign import ccall unsafe "hs_bindgen_075eba6e653b1e77" hs_bindgen_075eba6e653b1e77_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_union@
hs_bindgen_075eba6e653b1e77 ::
     A
  -> RIP.Ptr Some_union
  -> IO ()
hs_bindgen_075eba6e653b1e77 =
  RIP.fromFFIType hs_bindgen_075eba6e653b1e77_base

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
                         hs_bindgen_075eba6e653b1e77 arg10 res1)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_enum@
foreign import ccall unsafe "hs_bindgen_d24ca05277d86321" hs_bindgen_d24ca05277d86321_base ::
     RIP.Int32
  -> IO RIP.Word32

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_enum@
hs_bindgen_d24ca05277d86321 ::
     A
  -> IO Some_enum
hs_bindgen_d24ca05277d86321 =
  RIP.fromFFIType hs_bindgen_d24ca05277d86321_base

{-| __C declaration:__ @ret_enum@

    __defined at:__ @macros\/reparse\/reparse.h 71:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_enum ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO Some_enum
ret_enum = hs_bindgen_d24ca05277d86321

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_pointer1@
foreign import ccall unsafe "hs_bindgen_8b8137d2d216adaa" hs_bindgen_8b8137d2d216adaa_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_pointer1@
hs_bindgen_8b8137d2d216adaa ::
     A
  -> IO (RIP.Ptr RIP.CInt)
hs_bindgen_8b8137d2d216adaa =
  RIP.fromFFIType hs_bindgen_8b8137d2d216adaa_base

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @macros\/reparse\/reparse.h 73:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_pointer1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.Ptr RIP.CInt)
ret_pointer1 = hs_bindgen_8b8137d2d216adaa

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_pointer2@
foreign import ccall unsafe "hs_bindgen_9cc91cd81871702f" hs_bindgen_9cc91cd81871702f_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_pointer2@
hs_bindgen_9cc91cd81871702f ::
     A
  -> IO (RIP.Ptr (RIP.Ptr RIP.CInt))
hs_bindgen_9cc91cd81871702f =
  RIP.fromFFIType hs_bindgen_9cc91cd81871702f_base

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @macros\/reparse\/reparse.h 74:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_pointer2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.Ptr (RIP.Ptr RIP.CInt))
ret_pointer2 = hs_bindgen_9cc91cd81871702f

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_pointer3@
foreign import ccall unsafe "hs_bindgen_95489c088e6ab546" hs_bindgen_95489c088e6ab546_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_pointer3@
hs_bindgen_95489c088e6ab546 ::
     A
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_95489c088e6ab546 =
  RIP.fromFFIType hs_bindgen_95489c088e6ab546_base

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @macros\/reparse\/reparse.h 75:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
ret_pointer3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.Ptr RIP.Void)
ret_pointer3 = hs_bindgen_95489c088e6ab546

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_body1@
foreign import ccall unsafe "hs_bindgen_4a2abe76938abf43" hs_bindgen_4a2abe76938abf43_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_body1@
hs_bindgen_4a2abe76938abf43 ::
     A
  -> IO RIP.CInt
hs_bindgen_4a2abe76938abf43 =
  RIP.fromFFIType hs_bindgen_4a2abe76938abf43_base

{-| __C declaration:__ @body1@

    __defined at:__ @macros\/reparse\/reparse.h 79:5@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
body1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CInt
body1 = hs_bindgen_4a2abe76938abf43

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_body2@
foreign import ccall unsafe "hs_bindgen_8b29007fcb697c25" hs_bindgen_8b29007fcb697c25_base ::
     IO RIP.Int32

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_body2@
hs_bindgen_8b29007fcb697c25 :: IO A
hs_bindgen_8b29007fcb697c25 =
  RIP.fromFFIType hs_bindgen_8b29007fcb697c25_base

{-| __C declaration:__ @body2@

    __defined at:__ @macros\/reparse\/reparse.h 80:3@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
body2 :: IO A
body2 = hs_bindgen_8b29007fcb697c25

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_complex_float@
foreign import ccall unsafe "hs_bindgen_e0c33978335f65bc" hs_bindgen_e0c33978335f65bc_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_complex_float@
hs_bindgen_e0c33978335f65bc ::
     A
  -> RIP.Ptr (RIP.Complex RIP.CFloat)
  -> IO ()
hs_bindgen_e0c33978335f65bc =
  RIP.fromFFIType hs_bindgen_e0c33978335f65bc_base

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
                        hs_bindgen_e0c33978335f65bc arg10 arg22)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_complex_double@
foreign import ccall unsafe "hs_bindgen_a7670557e1b51a62" hs_bindgen_a7670557e1b51a62_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_args_complex_double@
hs_bindgen_a7670557e1b51a62 ::
     A
  -> RIP.Ptr (RIP.Complex RIP.CDouble)
  -> IO ()
hs_bindgen_a7670557e1b51a62 =
  RIP.fromFFIType hs_bindgen_a7670557e1b51a62_base

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
                        hs_bindgen_a7670557e1b51a62 arg10 arg22)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_complex_float@
foreign import ccall unsafe "hs_bindgen_106b0103af79bd03" hs_bindgen_106b0103af79bd03_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_complex_float@
hs_bindgen_106b0103af79bd03 ::
     A
  -> RIP.Ptr (RIP.Complex RIP.CFloat)
  -> IO ()
hs_bindgen_106b0103af79bd03 =
  RIP.fromFFIType hs_bindgen_106b0103af79bd03_base

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
                         hs_bindgen_106b0103af79bd03 arg10 res1)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_complex_double@
foreign import ccall unsafe "hs_bindgen_605b16bb46338d7a" hs_bindgen_605b16bb46338d7a_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_ret_complex_double@
hs_bindgen_605b16bb46338d7a ::
     A
  -> RIP.Ptr (RIP.Complex RIP.CDouble)
  -> IO ()
hs_bindgen_605b16bb46338d7a =
  RIP.fromFFIType hs_bindgen_605b16bb46338d7a_base

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
                         hs_bindgen_605b16bb46338d7a arg10 res1)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_bespoke_args1@
foreign import ccall unsafe "hs_bindgen_af3122ddd03619db" hs_bindgen_af3122ddd03619db_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_bespoke_args1@
hs_bindgen_af3122ddd03619db ::
     A
  -> RIP.CBool
  -> IO ()
hs_bindgen_af3122ddd03619db =
  RIP.fromFFIType hs_bindgen_af3122ddd03619db_base

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
bespoke_args1 = hs_bindgen_af3122ddd03619db

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_bespoke_args2@
foreign import ccall unsafe "hs_bindgen_c64666a0f5462c01" hs_bindgen_c64666a0f5462c01_base ::
     RIP.Int32
  -> RIP.Word64
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_bespoke_args2@
hs_bindgen_c64666a0f5462c01 ::
     A
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_c64666a0f5462c01 =
  RIP.fromFFIType hs_bindgen_c64666a0f5462c01_base

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
bespoke_args2 = hs_bindgen_c64666a0f5462c01

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_bespoke_ret1@
foreign import ccall unsafe "hs_bindgen_62c0105b0a3b0e90" hs_bindgen_62c0105b0a3b0e90_base ::
     RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_bespoke_ret1@
hs_bindgen_62c0105b0a3b0e90 ::
     A
  -> IO RIP.CBool
hs_bindgen_62c0105b0a3b0e90 =
  RIP.fromFFIType hs_bindgen_62c0105b0a3b0e90_base

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @macros\/reparse\/reparse.h 97:8@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
bespoke_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO RIP.CBool
bespoke_ret1 = hs_bindgen_62c0105b0a3b0e90

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_bespoke_ret2@
foreign import ccall unsafe "hs_bindgen_6e98636dcad638ea" hs_bindgen_6e98636dcad638ea_base ::
     RIP.Int32
  -> IO RIP.Word64

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_bespoke_ret2@
hs_bindgen_6e98636dcad638ea ::
     A
  -> IO HsBindgen.Runtime.LibC.CSize
hs_bindgen_6e98636dcad638ea =
  RIP.fromFFIType hs_bindgen_6e98636dcad638ea_base

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @macros\/reparse\/reparse.h 98:8@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
bespoke_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO HsBindgen.Runtime.LibC.CSize
bespoke_ret2 = hs_bindgen_6e98636dcad638ea

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_arr_args1@
foreign import ccall unsafe "hs_bindgen_08dea8d98ff039d8" hs_bindgen_08dea8d98ff039d8_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_arr_args1@
hs_bindgen_08dea8d98ff039d8 ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray A))
  -> IO ()
hs_bindgen_08dea8d98ff039d8 =
  RIP.fromFFIType hs_bindgen_08dea8d98ff039d8_base

{-| Arrays

__C declaration:__ @arr_args1@

__defined at:__ @macros\/reparse\/reparse.h 104:6@

__exported by:__ @macros\/reparse\/reparse.h@
-}
arr_args1 ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray A))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args1 = hs_bindgen_08dea8d98ff039d8

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_arr_args2@
foreign import ccall unsafe "hs_bindgen_a9d90769b712b0ae" hs_bindgen_a9d90769b712b0ae_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_arr_args2@
hs_bindgen_a9d90769b712b0ae ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray (RIP.Ptr A)))
  -> IO ()
hs_bindgen_a9d90769b712b0ae =
  RIP.fromFFIType hs_bindgen_a9d90769b712b0ae_base

{-| __C declaration:__ @arr_args2@

    __defined at:__ @macros\/reparse\/reparse.h 105:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
arr_args2 ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray (RIP.Ptr A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args2 = hs_bindgen_a9d90769b712b0ae

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_arr_args3@
foreign import ccall unsafe "hs_bindgen_35310a933bc48e94" hs_bindgen_35310a933bc48e94_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_arr_args3@
hs_bindgen_35310a933bc48e94 ::
     RIP.Ptr (IsA.Elem ((CA.ConstantArray 5) A))
  -> IO ()
hs_bindgen_35310a933bc48e94 =
  RIP.fromFFIType hs_bindgen_35310a933bc48e94_base

{-| __C declaration:__ @arr_args3@

    __defined at:__ @macros\/reparse\/reparse.h 106:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
arr_args3 ::
     RIP.Ptr (IsA.Elem ((CA.ConstantArray 5) A))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args3 = hs_bindgen_35310a933bc48e94

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_arr_args4@
foreign import ccall unsafe "hs_bindgen_299f9da613a8d020" hs_bindgen_299f9da613a8d020_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_arr_args4@
hs_bindgen_299f9da613a8d020 ::
     RIP.Ptr (IsA.Elem ((CA.ConstantArray 5) (RIP.Ptr A)))
  -> IO ()
hs_bindgen_299f9da613a8d020 =
  RIP.fromFFIType hs_bindgen_299f9da613a8d020_base

{-| __C declaration:__ @arr_args4@

    __defined at:__ @macros\/reparse\/reparse.h 107:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
arr_args4 ::
     RIP.Ptr (IsA.Elem ((CA.ConstantArray 5) (RIP.Ptr A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
arr_args4 = hs_bindgen_299f9da613a8d020

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_args1@
foreign import ccall unsafe "hs_bindgen_04a94602ff36823f" hs_bindgen_04a94602ff36823f_base ::
     RIP.Int32
  -> RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_args1@
hs_bindgen_04a94602ff36823f ::
     A
  -> RIP.FunPtr (IO ())
  -> IO ()
hs_bindgen_04a94602ff36823f =
  RIP.fromFFIType hs_bindgen_04a94602ff36823f_base

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
funptr_args1 = hs_bindgen_04a94602ff36823f

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_args2@
foreign import ccall unsafe "hs_bindgen_f35f821458786a50" hs_bindgen_f35f821458786a50_base ::
     RIP.Int32
  -> RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_args2@
hs_bindgen_f35f821458786a50 ::
     A
  -> RIP.FunPtr (IO RIP.CInt)
  -> IO ()
hs_bindgen_f35f821458786a50 =
  RIP.fromFFIType hs_bindgen_f35f821458786a50_base

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
funptr_args2 = hs_bindgen_f35f821458786a50

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_args3@
foreign import ccall unsafe "hs_bindgen_b62e35b8b42f6c72" hs_bindgen_b62e35b8b42f6c72_base ::
     RIP.Int32
  -> RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_args3@
hs_bindgen_b62e35b8b42f6c72 ::
     A
  -> RIP.FunPtr (RIP.CInt -> IO ())
  -> IO ()
hs_bindgen_b62e35b8b42f6c72 =
  RIP.fromFFIType hs_bindgen_b62e35b8b42f6c72_base

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
funptr_args3 = hs_bindgen_b62e35b8b42f6c72

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_args4@
foreign import ccall unsafe "hs_bindgen_4a20f35683112bbf" hs_bindgen_4a20f35683112bbf_base ::
     RIP.Int32
  -> RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_args4@
hs_bindgen_4a20f35683112bbf ::
     A
  -> RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO RIP.CChar)
  -> IO ()
hs_bindgen_4a20f35683112bbf =
  RIP.fromFFIType hs_bindgen_4a20f35683112bbf_base

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
funptr_args4 = hs_bindgen_4a20f35683112bbf

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_args5@
foreign import ccall unsafe "hs_bindgen_f1d463403b067ed0" hs_bindgen_f1d463403b067ed0_base ::
     RIP.Int32
  -> RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_args5@
hs_bindgen_f1d463403b067ed0 ::
     A
  -> RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt))
  -> IO ()
hs_bindgen_f1d463403b067ed0 =
  RIP.fromFFIType hs_bindgen_f1d463403b067ed0_base

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
funptr_args5 = hs_bindgen_f1d463403b067ed0

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_comments1@
foreign import ccall unsafe "hs_bindgen_6e28d41852baa736" hs_bindgen_6e28d41852baa736_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_comments1@
hs_bindgen_6e28d41852baa736 ::
     A
  -> IO ()
hs_bindgen_6e28d41852baa736 =
  RIP.fromFFIType hs_bindgen_6e28d41852baa736_base

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
comments1 = hs_bindgen_6e28d41852baa736

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_prim_before1@
foreign import ccall unsafe "hs_bindgen_3fa5c5a5148a9824" hs_bindgen_3fa5c5a5148a9824_base ::
     RIP.Int32
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_prim_before1@
hs_bindgen_3fa5c5a5148a9824 ::
     A
  -> RIP.CChar
  -> IO ()
hs_bindgen_3fa5c5a5148a9824 =
  RIP.fromFFIType hs_bindgen_3fa5c5a5148a9824_base

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
const_prim_before1 = hs_bindgen_3fa5c5a5148a9824

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_prim_before2@
foreign import ccall unsafe "hs_bindgen_461b29b08c844b7c" hs_bindgen_461b29b08c844b7c_base ::
     RIP.Int32
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_prim_before2@
hs_bindgen_461b29b08c844b7c ::
     A
  -> RIP.CSChar
  -> IO ()
hs_bindgen_461b29b08c844b7c =
  RIP.fromFFIType hs_bindgen_461b29b08c844b7c_base

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
const_prim_before2 = hs_bindgen_461b29b08c844b7c

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_prim_before3@
foreign import ccall unsafe "hs_bindgen_48a2759c1b58a757" hs_bindgen_48a2759c1b58a757_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_prim_before3@
hs_bindgen_48a2759c1b58a757 ::
     A
  -> RIP.CUChar
  -> IO ()
hs_bindgen_48a2759c1b58a757 =
  RIP.fromFFIType hs_bindgen_48a2759c1b58a757_base

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
const_prim_before3 = hs_bindgen_48a2759c1b58a757

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_prim_after1@
foreign import ccall unsafe "hs_bindgen_018db92a1fdc0d50" hs_bindgen_018db92a1fdc0d50_base ::
     RIP.Int32
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_prim_after1@
hs_bindgen_018db92a1fdc0d50 ::
     A
  -> RIP.CChar
  -> IO ()
hs_bindgen_018db92a1fdc0d50 =
  RIP.fromFFIType hs_bindgen_018db92a1fdc0d50_base

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
const_prim_after1 = hs_bindgen_018db92a1fdc0d50

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_prim_after2@
foreign import ccall unsafe "hs_bindgen_a2c532d8f7f39797" hs_bindgen_a2c532d8f7f39797_base ::
     RIP.Int32
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_prim_after2@
hs_bindgen_a2c532d8f7f39797 ::
     A
  -> RIP.CSChar
  -> IO ()
hs_bindgen_a2c532d8f7f39797 =
  RIP.fromFFIType hs_bindgen_a2c532d8f7f39797_base

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
const_prim_after2 = hs_bindgen_a2c532d8f7f39797

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_prim_after3@
foreign import ccall unsafe "hs_bindgen_ef823e9dcc43fc36" hs_bindgen_ef823e9dcc43fc36_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_prim_after3@
hs_bindgen_ef823e9dcc43fc36 ::
     A
  -> RIP.CUChar
  -> IO ()
hs_bindgen_ef823e9dcc43fc36 =
  RIP.fromFFIType hs_bindgen_ef823e9dcc43fc36_base

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
const_prim_after3 = hs_bindgen_ef823e9dcc43fc36

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_before1@
foreign import ccall unsafe "hs_bindgen_a2674612dbe7213a" hs_bindgen_a2674612dbe7213a_base ::
     RIP.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_before1@
hs_bindgen_a2674612dbe7213a ::
     A
  -> RIP.CFloat
  -> IO ()
hs_bindgen_a2674612dbe7213a =
  RIP.fromFFIType hs_bindgen_a2674612dbe7213a_base

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
  hs_bindgen_a2674612dbe7213a

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_before2@
foreign import ccall unsafe "hs_bindgen_64cbf4ecc90ce01e" hs_bindgen_64cbf4ecc90ce01e_base ::
     RIP.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_before2@
hs_bindgen_64cbf4ecc90ce01e ::
     A
  -> RIP.CDouble
  -> IO ()
hs_bindgen_64cbf4ecc90ce01e =
  RIP.fromFFIType hs_bindgen_64cbf4ecc90ce01e_base

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
  hs_bindgen_64cbf4ecc90ce01e

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_before3@
foreign import ccall unsafe "hs_bindgen_51d96a6c013cde65" hs_bindgen_51d96a6c013cde65_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_before3@
hs_bindgen_51d96a6c013cde65 ::
     A
  -> RIP.CBool
  -> IO ()
hs_bindgen_51d96a6c013cde65 =
  RIP.fromFFIType hs_bindgen_51d96a6c013cde65_base

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
  hs_bindgen_51d96a6c013cde65

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_before4@
foreign import ccall unsafe "hs_bindgen_6db406eb239af0dd" hs_bindgen_6db406eb239af0dd_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_before4@
hs_bindgen_6db406eb239af0dd ::
     A
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_6db406eb239af0dd =
  RIP.fromFFIType hs_bindgen_6db406eb239af0dd_base

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
                        hs_bindgen_6db406eb239af0dd arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_before5@
foreign import ccall unsafe "hs_bindgen_92509acd98e266b1" hs_bindgen_92509acd98e266b1_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_before5@
hs_bindgen_92509acd98e266b1 ::
     A
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_92509acd98e266b1 =
  RIP.fromFFIType hs_bindgen_92509acd98e266b1_base

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
                        hs_bindgen_92509acd98e266b1 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_before6@
foreign import ccall unsafe "hs_bindgen_8a68a36f7692ffec" hs_bindgen_8a68a36f7692ffec_base ::
     RIP.Int32
  -> RIP.Word32
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_before6@
hs_bindgen_8a68a36f7692ffec ::
     A
  -> Some_enum
  -> IO ()
hs_bindgen_8a68a36f7692ffec =
  RIP.fromFFIType hs_bindgen_8a68a36f7692ffec_base

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
  hs_bindgen_8a68a36f7692ffec

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_before7@
foreign import ccall unsafe "hs_bindgen_3f35c090df3d9ea9" hs_bindgen_3f35c090df3d9ea9_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_before7@
hs_bindgen_3f35c090df3d9ea9 ::
     A
  -> RIP.CBool
  -> IO ()
hs_bindgen_3f35c090df3d9ea9 =
  RIP.fromFFIType hs_bindgen_3f35c090df3d9ea9_base

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
  hs_bindgen_3f35c090df3d9ea9

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_before8@
foreign import ccall unsafe "hs_bindgen_11937823df53d81c" hs_bindgen_11937823df53d81c_base ::
     RIP.Int32
  -> RIP.Word64
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_before8@
hs_bindgen_11937823df53d81c ::
     A
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_11937823df53d81c =
  RIP.fromFFIType hs_bindgen_11937823df53d81c_base

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
  hs_bindgen_11937823df53d81c

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_after1@
foreign import ccall unsafe "hs_bindgen_293ae8d24cbffa78" hs_bindgen_293ae8d24cbffa78_base ::
     RIP.Int32
  -> Float
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_after1@
hs_bindgen_293ae8d24cbffa78 ::
     A
  -> RIP.CFloat
  -> IO ()
hs_bindgen_293ae8d24cbffa78 =
  RIP.fromFFIType hs_bindgen_293ae8d24cbffa78_base

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
  hs_bindgen_293ae8d24cbffa78

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_after2@
foreign import ccall unsafe "hs_bindgen_533859b45983018a" hs_bindgen_533859b45983018a_base ::
     RIP.Int32
  -> Double
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_after2@
hs_bindgen_533859b45983018a ::
     A
  -> RIP.CDouble
  -> IO ()
hs_bindgen_533859b45983018a =
  RIP.fromFFIType hs_bindgen_533859b45983018a_base

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
  hs_bindgen_533859b45983018a

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_after3@
foreign import ccall unsafe "hs_bindgen_4dbb63bf826187f3" hs_bindgen_4dbb63bf826187f3_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_after3@
hs_bindgen_4dbb63bf826187f3 ::
     A
  -> RIP.CBool
  -> IO ()
hs_bindgen_4dbb63bf826187f3 =
  RIP.fromFFIType hs_bindgen_4dbb63bf826187f3_base

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
  hs_bindgen_4dbb63bf826187f3

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_after4@
foreign import ccall unsafe "hs_bindgen_a27df069efecacb6" hs_bindgen_a27df069efecacb6_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_after4@
hs_bindgen_a27df069efecacb6 ::
     A
  -> PtrConst.PtrConst Some_struct
  -> IO ()
hs_bindgen_a27df069efecacb6 =
  RIP.fromFFIType hs_bindgen_a27df069efecacb6_base

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
                        hs_bindgen_a27df069efecacb6 arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_after5@
foreign import ccall unsafe "hs_bindgen_d91b7cd58ad58d7a" hs_bindgen_d91b7cd58ad58d7a_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_after5@
hs_bindgen_d91b7cd58ad58d7a ::
     A
  -> PtrConst.PtrConst Some_union
  -> IO ()
hs_bindgen_d91b7cd58ad58d7a =
  RIP.fromFFIType hs_bindgen_d91b7cd58ad58d7a_base

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
                        hs_bindgen_d91b7cd58ad58d7a arg10 (PtrConst.unsafeFromPtr arg22))

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_after6@
foreign import ccall unsafe "hs_bindgen_d1d894d1536c7540" hs_bindgen_d1d894d1536c7540_base ::
     RIP.Int32
  -> RIP.Word32
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_after6@
hs_bindgen_d1d894d1536c7540 ::
     A
  -> Some_enum
  -> IO ()
hs_bindgen_d1d894d1536c7540 =
  RIP.fromFFIType hs_bindgen_d1d894d1536c7540_base

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
  hs_bindgen_d1d894d1536c7540

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_after7@
foreign import ccall unsafe "hs_bindgen_b10294ffc1a88b54" hs_bindgen_b10294ffc1a88b54_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_after7@
hs_bindgen_b10294ffc1a88b54 ::
     A
  -> RIP.CBool
  -> IO ()
hs_bindgen_b10294ffc1a88b54 =
  RIP.fromFFIType hs_bindgen_b10294ffc1a88b54_base

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
  hs_bindgen_b10294ffc1a88b54

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_after8@
foreign import ccall unsafe "hs_bindgen_4f6beef21554a89c" hs_bindgen_4f6beef21554a89c_base ::
     RIP.Int32
  -> RIP.Word64
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_withoutSign_after8@
hs_bindgen_4f6beef21554a89c ::
     A
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_4f6beef21554a89c =
  RIP.fromFFIType hs_bindgen_4f6beef21554a89c_base

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
  hs_bindgen_4f6beef21554a89c

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_args1@
foreign import ccall unsafe "hs_bindgen_69312ba543a639b9" hs_bindgen_69312ba543a639b9_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_args1@
hs_bindgen_69312ba543a639b9 ::
     A
  -> PtrConst.PtrConst RIP.CInt
  -> IO ()
hs_bindgen_69312ba543a639b9 =
  RIP.fromFFIType hs_bindgen_69312ba543a639b9_base

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
const_pointers_args1 = hs_bindgen_69312ba543a639b9

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_args2@
foreign import ccall unsafe "hs_bindgen_ade674ed0d522b2c" hs_bindgen_ade674ed0d522b2c_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_args2@
hs_bindgen_ade674ed0d522b2c ::
     A
  -> PtrConst.PtrConst RIP.CInt
  -> IO ()
hs_bindgen_ade674ed0d522b2c =
  RIP.fromFFIType hs_bindgen_ade674ed0d522b2c_base

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
const_pointers_args2 = hs_bindgen_ade674ed0d522b2c

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_args3@
foreign import ccall unsafe "hs_bindgen_9ac414106b3e9dcc" hs_bindgen_9ac414106b3e9dcc_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_args3@
hs_bindgen_9ac414106b3e9dcc ::
     A
  -> RIP.Ptr RIP.CInt
  -> IO ()
hs_bindgen_9ac414106b3e9dcc =
  RIP.fromFFIType hs_bindgen_9ac414106b3e9dcc_base

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
const_pointers_args3 = hs_bindgen_9ac414106b3e9dcc

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_args4@
foreign import ccall unsafe "hs_bindgen_6f54b8081c103a22" hs_bindgen_6f54b8081c103a22_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_args4@
hs_bindgen_6f54b8081c103a22 ::
     A
  -> PtrConst.PtrConst RIP.CInt
  -> IO ()
hs_bindgen_6f54b8081c103a22 =
  RIP.fromFFIType hs_bindgen_6f54b8081c103a22_base

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
const_pointers_args4 = hs_bindgen_6f54b8081c103a22

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_args5@
foreign import ccall unsafe "hs_bindgen_07bdc9016b39f512" hs_bindgen_07bdc9016b39f512_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_args5@
hs_bindgen_07bdc9016b39f512 ::
     A
  -> PtrConst.PtrConst RIP.CInt
  -> IO ()
hs_bindgen_07bdc9016b39f512 =
  RIP.fromFFIType hs_bindgen_07bdc9016b39f512_base

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
const_pointers_args5 = hs_bindgen_07bdc9016b39f512

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_ret1@
foreign import ccall unsafe "hs_bindgen_61ae256f76ba18fb" hs_bindgen_61ae256f76ba18fb_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_ret1@
hs_bindgen_61ae256f76ba18fb ::
     A
  -> IO (PtrConst.PtrConst RIP.CInt)
hs_bindgen_61ae256f76ba18fb =
  RIP.fromFFIType hs_bindgen_61ae256f76ba18fb_base

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @macros\/reparse\/reparse.h 214:19@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst RIP.CInt)
const_pointers_ret1 = hs_bindgen_61ae256f76ba18fb

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_ret2@
foreign import ccall unsafe "hs_bindgen_8301f1156ba87562" hs_bindgen_8301f1156ba87562_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_ret2@
hs_bindgen_8301f1156ba87562 ::
     A
  -> IO (PtrConst.PtrConst RIP.CInt)
hs_bindgen_8301f1156ba87562 =
  RIP.fromFFIType hs_bindgen_8301f1156ba87562_base

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @macros\/reparse\/reparse.h 215:19@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst RIP.CInt)
const_pointers_ret2 = hs_bindgen_8301f1156ba87562

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_ret3@
foreign import ccall unsafe "hs_bindgen_8188e773ffb5dc3c" hs_bindgen_8188e773ffb5dc3c_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_ret3@
hs_bindgen_8188e773ffb5dc3c ::
     A
  -> IO (RIP.Ptr RIP.CInt)
hs_bindgen_8188e773ffb5dc3c =
  RIP.fromFFIType hs_bindgen_8188e773ffb5dc3c_base

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @macros\/reparse\/reparse.h 216:19@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_ret3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.Ptr RIP.CInt)
const_pointers_ret3 = hs_bindgen_8188e773ffb5dc3c

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_ret4@
foreign import ccall unsafe "hs_bindgen_6a2eee73cb0d7eba" hs_bindgen_6a2eee73cb0d7eba_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_ret4@
hs_bindgen_6a2eee73cb0d7eba ::
     A
  -> IO (PtrConst.PtrConst RIP.CInt)
hs_bindgen_6a2eee73cb0d7eba =
  RIP.fromFFIType hs_bindgen_6a2eee73cb0d7eba_base

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @macros\/reparse\/reparse.h 217:19@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_ret4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst RIP.CInt)
const_pointers_ret4 = hs_bindgen_6a2eee73cb0d7eba

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_ret5@
foreign import ccall unsafe "hs_bindgen_83129cee1b8daed1" hs_bindgen_83129cee1b8daed1_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_pointers_ret5@
hs_bindgen_83129cee1b8daed1 ::
     A
  -> IO (PtrConst.PtrConst RIP.CInt)
hs_bindgen_83129cee1b8daed1 =
  RIP.fromFFIType hs_bindgen_83129cee1b8daed1_base

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @macros\/reparse\/reparse.h 218:19@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_pointers_ret5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (PtrConst.PtrConst RIP.CInt)
const_pointers_ret5 = hs_bindgen_83129cee1b8daed1

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_array_elem1@
foreign import ccall unsafe "hs_bindgen_ac03ac62a99836c9" hs_bindgen_ac03ac62a99836c9_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_array_elem1@
hs_bindgen_ac03ac62a99836c9 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray A))
  -> IO ()
hs_bindgen_ac03ac62a99836c9 =
  RIP.fromFFIType hs_bindgen_ac03ac62a99836c9_base

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @macros\/reparse\/reparse.h 246:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_array_elem1 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray A))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem1 = hs_bindgen_ac03ac62a99836c9

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_array_elem2@
foreign import ccall unsafe "hs_bindgen_332eb4f162aae214" hs_bindgen_332eb4f162aae214_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_array_elem2@
hs_bindgen_332eb4f162aae214 ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst A)))
  -> IO ()
hs_bindgen_332eb4f162aae214 =
  RIP.fromFFIType hs_bindgen_332eb4f162aae214_base

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @macros\/reparse\/reparse.h 247:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_array_elem2 ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray (PtrConst.PtrConst A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem2 = hs_bindgen_332eb4f162aae214

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_array_elem3@
foreign import ccall unsafe "hs_bindgen_678c169d19173222" hs_bindgen_678c169d19173222_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_const_array_elem3@
hs_bindgen_678c169d19173222 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (RIP.Ptr A)))
  -> IO ()
hs_bindgen_678c169d19173222 =
  RIP.fromFFIType hs_bindgen_678c169d19173222_base

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @macros\/reparse\/reparse.h 248:6@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
const_array_elem3 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (RIP.Ptr A)))
     -- ^ __C declaration:__ @arg1@
  -> IO ()
const_array_elem3 = hs_bindgen_678c169d19173222

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_noParams1@
foreign import ccall unsafe "hs_bindgen_eb8239a8e7f57e7b" hs_bindgen_eb8239a8e7f57e7b_base ::
     IO RIP.Int32

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_noParams1@
hs_bindgen_eb8239a8e7f57e7b :: IO A
hs_bindgen_eb8239a8e7f57e7b =
  RIP.fromFFIType hs_bindgen_eb8239a8e7f57e7b_base

{-| Other examples we reparsed /incorrectly/ before language-c

__C declaration:__ @noParams1@

__defined at:__ @macros\/reparse\/reparse.h 256:3@

__exported by:__ @macros\/reparse\/reparse.h@
-}
noParams1 :: IO A
noParams1 = hs_bindgen_eb8239a8e7f57e7b

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_noParams2@
foreign import ccall unsafe "hs_bindgen_56ef4fc40eb6685c" hs_bindgen_56ef4fc40eb6685c_base ::
     IO RIP.Int32

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_noParams2@
hs_bindgen_56ef4fc40eb6685c :: IO A
hs_bindgen_56ef4fc40eb6685c =
  RIP.fromFFIType hs_bindgen_56ef4fc40eb6685c_base

{-| __C declaration:__ @noParams2@

    __defined at:__ @macros\/reparse\/reparse.h 257:3@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
noParams2 :: IO A
noParams2 = hs_bindgen_56ef4fc40eb6685c

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_noParams3@
foreign import ccall unsafe "hs_bindgen_e901eaca204bbd3b" hs_bindgen_e901eaca204bbd3b_base ::
     RIP.Int32
  -> RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_noParams3@
hs_bindgen_e901eaca204bbd3b ::
     A
  -> RIP.FunPtr (IO RIP.CInt)
  -> IO ()
hs_bindgen_e901eaca204bbd3b =
  RIP.fromFFIType hs_bindgen_e901eaca204bbd3b_base

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
noParams3 = hs_bindgen_e901eaca204bbd3b

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret1@
foreign import ccall unsafe "hs_bindgen_18221abe0529c70a" hs_bindgen_18221abe0529c70a_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret1@
hs_bindgen_18221abe0529c70a ::
     A
  -> IO (RIP.FunPtr (IO ()))
hs_bindgen_18221abe0529c70a =
  RIP.fromFFIType hs_bindgen_18221abe0529c70a_base

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @macros\/reparse\/reparse.h 262:8@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret1 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (IO ()))
funptr_ret1 = hs_bindgen_18221abe0529c70a

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret2@
foreign import ccall unsafe "hs_bindgen_83435e385a318b88" hs_bindgen_83435e385a318b88_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret2@
hs_bindgen_83435e385a318b88 ::
     A
  -> IO (RIP.FunPtr (IO RIP.CInt))
hs_bindgen_83435e385a318b88 =
  RIP.fromFFIType hs_bindgen_83435e385a318b88_base

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @macros\/reparse\/reparse.h 263:8@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret2 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (IO RIP.CInt))
funptr_ret2 = hs_bindgen_83435e385a318b88

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret3@
foreign import ccall unsafe "hs_bindgen_b8a6302e29fba35d" hs_bindgen_b8a6302e29fba35d_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret3@
hs_bindgen_b8a6302e29fba35d ::
     A
  -> IO (RIP.FunPtr (RIP.CInt -> IO ()))
hs_bindgen_b8a6302e29fba35d =
  RIP.fromFFIType hs_bindgen_b8a6302e29fba35d_base

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @macros\/reparse\/reparse.h 264:8@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret3 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (RIP.CInt -> IO ()))
funptr_ret3 = hs_bindgen_b8a6302e29fba35d

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret4@
foreign import ccall unsafe "hs_bindgen_ade99ff1ca3e1ab4" hs_bindgen_ade99ff1ca3e1ab4_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret4@
hs_bindgen_ade99ff1ca3e1ab4 ::
     A
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO RIP.CChar))
hs_bindgen_ade99ff1ca3e1ab4 =
  RIP.fromFFIType hs_bindgen_ade99ff1ca3e1ab4_base

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @macros\/reparse\/reparse.h 265:8@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret4 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO RIP.CChar))
funptr_ret4 = hs_bindgen_ade99ff1ca3e1ab4

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret5@
foreign import ccall unsafe "hs_bindgen_d94322ca936c5318" hs_bindgen_d94322ca936c5318_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret5@
hs_bindgen_d94322ca936c5318 ::
     A
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt)))
hs_bindgen_d94322ca936c5318 =
  RIP.fromFFIType hs_bindgen_d94322ca936c5318_base

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @macros\/reparse\/reparse.h 269:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret5 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt)))
funptr_ret5 = hs_bindgen_d94322ca936c5318

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret6@
foreign import ccall unsafe "hs_bindgen_259eac69be80e6e5" hs_bindgen_259eac69be80e6e5_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret6@
hs_bindgen_259eac69be80e6e5 ::
     A
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))
hs_bindgen_259eac69be80e6e5 =
  RIP.fromFFIType hs_bindgen_259eac69be80e6e5_base

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @macros\/reparse\/reparse.h 270:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret6 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))
funptr_ret6 = hs_bindgen_259eac69be80e6e5

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret7@
foreign import ccall unsafe "hs_bindgen_6062b9d1a54078cd" hs_bindgen_6062b9d1a54078cd_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret7@
hs_bindgen_6062b9d1a54078cd ::
     A
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))
hs_bindgen_6062b9d1a54078cd =
  RIP.fromFFIType hs_bindgen_6062b9d1a54078cd_base

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @macros\/reparse\/reparse.h 271:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret7 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))
funptr_ret7 = hs_bindgen_6062b9d1a54078cd

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret8@
foreign import ccall unsafe "hs_bindgen_8d832ca80d7b94e5" hs_bindgen_8d832ca80d7b94e5_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret8@
hs_bindgen_8d832ca80d7b94e5 ::
     A
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt)))
hs_bindgen_8d832ca80d7b94e5 =
  RIP.fromFFIType hs_bindgen_8d832ca80d7b94e5_base

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @macros\/reparse\/reparse.h 272:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret8 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr RIP.CInt)))
funptr_ret8 = hs_bindgen_8d832ca80d7b94e5

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret9@
foreign import ccall unsafe "hs_bindgen_5c6db1c615f7065d" hs_bindgen_5c6db1c615f7065d_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret9@
hs_bindgen_5c6db1c615f7065d ::
     A
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))
hs_bindgen_5c6db1c615f7065d =
  RIP.fromFFIType hs_bindgen_5c6db1c615f7065d_base

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @macros\/reparse\/reparse.h 273:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret9 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))
funptr_ret9 = hs_bindgen_5c6db1c615f7065d

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret10@
foreign import ccall unsafe "hs_bindgen_7d06f58b43dabd28" hs_bindgen_7d06f58b43dabd28_base ::
     RIP.Int32
  -> IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsereparse_Example_Unsafe_funptr_ret10@
hs_bindgen_7d06f58b43dabd28 ::
     A
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))
hs_bindgen_7d06f58b43dabd28 =
  RIP.fromFFIType hs_bindgen_7d06f58b43dabd28_base

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @macros\/reparse\/reparse.h 274:20@

    __exported by:__ @macros\/reparse\/reparse.h@
-}
funptr_ret10 ::
     A
     -- ^ __C declaration:__ @arg1@
  -> IO (RIP.FunPtr (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst RIP.CInt)))
funptr_ret10 = hs_bindgen_7d06f58b43dabd28
