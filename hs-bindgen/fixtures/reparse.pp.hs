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

module Example where

import qualified Data.Array.Byte
import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Complex
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import Data.Void (Void)
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.SizedByteArray
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, return, showsPrec)
import qualified Text.Read

$(CAPI.addCSource "#include <reparse.h>\nvoid hs_bindgen_test_reparse_10171eb304e5cc62 (A arg1, char arg2) { args_char1(arg1, arg2); }\n/* get_args_char1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_016baba9b269ce3e (void)) (A arg1, char arg2) { return &args_char1; } \nvoid hs_bindgen_test_reparse_0a5b69870f97616a (A arg1, signed char arg2) { args_char2(arg1, arg2); }\n/* get_args_char2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_deda633a655edee7 (void)) (A arg1, signed char arg2) { return &args_char2; } \nvoid hs_bindgen_test_reparse_2152f20110364cbb (A arg1, unsigned char arg2) { args_char3(arg1, arg2); }\n/* get_args_char3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_929c40236948e098 (void)) (A arg1, unsigned char arg2) { return &args_char3; } \nvoid hs_bindgen_test_reparse_bad35762dd252147 (A arg1, signed short arg2) { args_short1(arg1, arg2); }\n/* get_args_short1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_6765935911469fba (void)) (A arg1, signed short arg2) { return &args_short1; } \nvoid hs_bindgen_test_reparse_8705b73515e5d5e7 (A arg1, signed short arg2) { args_short2(arg1, arg2); }\n/* get_args_short2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_945292112fa4a0ee (void)) (A arg1, signed short arg2) { return &args_short2; } \nvoid hs_bindgen_test_reparse_ae877590f2712156 (A arg1, unsigned short arg2) { args_short3(arg1, arg2); }\n/* get_args_short3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_0a3b985c5f6b183e (void)) (A arg1, unsigned short arg2) { return &args_short3; } \nvoid hs_bindgen_test_reparse_5bcc52df98b32e80 (A arg1, signed int arg2) { args_int1(arg1, arg2); }\n/* get_args_int1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_0b9730298592ed7b (void)) (A arg1, signed int arg2) { return &args_int1; } \nvoid hs_bindgen_test_reparse_d5542d284cc6174c (A arg1, signed int arg2) { args_int2(arg1, arg2); }\n/* get_args_int2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_b35d311c59b97357 (void)) (A arg1, signed int arg2) { return &args_int2; } \nvoid hs_bindgen_test_reparse_940dd7ba49a6be4a (A arg1, unsigned int arg2) { args_int3(arg1, arg2); }\n/* get_args_int3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_deb09915f72c9f94 (void)) (A arg1, unsigned int arg2) { return &args_int3; } \nvoid hs_bindgen_test_reparse_79b01356e07fd6c5 (A arg1, signed long arg2) { args_long1(arg1, arg2); }\n/* get_args_long1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_da0f1cd5ae89314a (void)) (A arg1, signed long arg2) { return &args_long1; } \nvoid hs_bindgen_test_reparse_d5bdd91db9738145 (A arg1, signed long arg2) { args_long2(arg1, arg2); }\n/* get_args_long2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_758d9779edb07735 (void)) (A arg1, signed long arg2) { return &args_long2; } \nvoid hs_bindgen_test_reparse_9302b433a1667eb4 (A arg1, unsigned long arg2) { args_long3(arg1, arg2); }\n/* get_args_long3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_db9704dc54e0b830 (void)) (A arg1, unsigned long arg2) { return &args_long3; } \nvoid hs_bindgen_test_reparse_316fd20ab67d2e28 (A arg1, float arg2) { args_float(arg1, arg2); }\n/* get_args_float_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_7bd676a3aba9914d (void)) (A arg1, float arg2) { return &args_float; } \nvoid hs_bindgen_test_reparse_82242119ea26cfe9 (A arg1, double arg2) { args_double(arg1, arg2); }\n/* get_args_double_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_bc71e4aa8bc9e516 (void)) (A arg1, double arg2) { return &args_double; } \nvoid hs_bindgen_test_reparse_8ac94b2768601ba9 (A arg1, _Bool arg2) { args_bool1(arg1, arg2); }\n/* get_args_bool1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_e8148b6b7b0284e3 (void)) (A arg1, _Bool arg2) { return &args_bool1; } \nvoid hs_bindgen_test_reparse_fc804a81327ef4f8 (A arg1, struct some_struct *arg2) { args_struct(arg1, *arg2); }\n/* get_args_struct_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_b679dc2bfc141ddd (void)) (A arg1, struct some_struct arg2) { return &args_struct; } \nvoid hs_bindgen_test_reparse_fd5beb002748e594 (A arg1, union some_union *arg2) { args_union(arg1, *arg2); }\n/* get_args_union_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_9fc41568c0ef334a (void)) (A arg1, union some_union arg2) { return &args_union; } \nvoid hs_bindgen_test_reparse_52c129b6f3ed3c8f (A arg1, enum some_enum arg2) { args_enum(arg1, arg2); }\n/* get_args_enum_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_a38701ffd6bcd8be (void)) (A arg1, enum some_enum arg2) { return &args_enum; } \nvoid hs_bindgen_test_reparse_9a2762d56bf1de6c (A arg1, signed int *arg2) { args_pointer1(arg1, arg2); }\n/* get_args_pointer1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_a64bc25bdd7e894f (void)) (A arg1, signed int *arg2) { return &args_pointer1; } \nvoid hs_bindgen_test_reparse_ec864c09e80e67e1 (A arg1, signed int **arg2) { args_pointer2(arg1, arg2); }\n/* get_args_pointer2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_9edb8a2162d37629 (void)) (A arg1, signed int **arg2) { return &args_pointer2; } \nvoid hs_bindgen_test_reparse_09151e71162f3239 (A arg1, void *arg2) { args_pointer3(arg1, arg2); }\n/* get_args_pointer3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_8a930c71a84a9846 (void)) (A arg1, void *arg2) { return &args_pointer3; } \nA hs_bindgen_test_reparse_721fac62084456f3 (void) { return ret_A(); }\n/* get_ret_A_ptr */ __attribute__ ((const)) A (*hs_bindgen_test_reparse_877842edc48bfc6f (void)) (void) { return &ret_A; } \nchar hs_bindgen_test_reparse_82bdf9fd8ea5cec3 (A arg1) { return ret_char1(arg1); }\n/* get_ret_char1_ptr */ __attribute__ ((const)) char (*hs_bindgen_test_reparse_adbd59964b60242f (void)) (A arg1) { return &ret_char1; } \nsigned char hs_bindgen_test_reparse_cc0da46ef0c52b0d (A arg1) { return ret_char2(arg1); }\n/* get_ret_char2_ptr */ __attribute__ ((const)) signed char (*hs_bindgen_test_reparse_59bbea2dfed561af (void)) (A arg1) { return &ret_char2; } \nunsigned char hs_bindgen_test_reparse_225667bb52779eb7 (A arg1) { return ret_char3(arg1); }\n/* get_ret_char3_ptr */ __attribute__ ((const)) unsigned char (*hs_bindgen_test_reparse_0801d372d3906773 (void)) (A arg1) { return &ret_char3; } \nsigned short hs_bindgen_test_reparse_9322ef5e958be938 (A arg1) { return ret_short1(arg1); }\n/* get_ret_short1_ptr */ __attribute__ ((const)) signed short (*hs_bindgen_test_reparse_ccfcc25e4950bc67 (void)) (A arg1) { return &ret_short1; } \nsigned short hs_bindgen_test_reparse_ef12449372a2af8b (A arg1) { return ret_short2(arg1); }\n/* get_ret_short2_ptr */ __attribute__ ((const)) signed short (*hs_bindgen_test_reparse_4bf91229e062d6be (void)) (A arg1) { return &ret_short2; } \nunsigned short hs_bindgen_test_reparse_a7f196b1d51beccf (A arg1) { return ret_short3(arg1); }\n/* get_ret_short3_ptr */ __attribute__ ((const)) unsigned short (*hs_bindgen_test_reparse_d7104088c29df31d (void)) (A arg1) { return &ret_short3; } \nsigned int hs_bindgen_test_reparse_9e2b08760143bbc2 (A arg1) { return ret_int1(arg1); }\n/* get_ret_int1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_reparse_5c37e045e5cc329f (void)) (A arg1) { return &ret_int1; } \nsigned int hs_bindgen_test_reparse_d7ec0cdbec9f2174 (A arg1) { return ret_int2(arg1); }\n/* get_ret_int2_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_reparse_9e2af05c9cea58ba (void)) (A arg1) { return &ret_int2; } \nunsigned int hs_bindgen_test_reparse_b4415511dbc7b6ab (A arg1) { return ret_int3(arg1); }\n/* get_ret_int3_ptr */ __attribute__ ((const)) unsigned int (*hs_bindgen_test_reparse_b04741094c75560d (void)) (A arg1) { return &ret_int3; } \nsigned long hs_bindgen_test_reparse_62ed9e11769772fe (A arg1) { return ret_long1(arg1); }\n/* get_ret_long1_ptr */ __attribute__ ((const)) signed long (*hs_bindgen_test_reparse_8f79f8c1226c822b (void)) (A arg1) { return &ret_long1; } \nsigned long hs_bindgen_test_reparse_16ef6708c7b0d6a3 (A arg1) { return ret_long2(arg1); }\n/* get_ret_long2_ptr */ __attribute__ ((const)) signed long (*hs_bindgen_test_reparse_2ba044632bcfc084 (void)) (A arg1) { return &ret_long2; } \nunsigned long hs_bindgen_test_reparse_097510d70490cc54 (A arg1) { return ret_long3(arg1); }\n/* get_ret_long3_ptr */ __attribute__ ((const)) unsigned long (*hs_bindgen_test_reparse_1129f4b482d7d20c (void)) (A arg1) { return &ret_long3; } \nfloat hs_bindgen_test_reparse_5777c7f34d2aabaf (A arg1) { return ret_float(arg1); }\n/* get_ret_float_ptr */ __attribute__ ((const)) float (*hs_bindgen_test_reparse_eed7871d97729b4d (void)) (A arg1) { return &ret_float; } \ndouble hs_bindgen_test_reparse_61e16d3cc5a31728 (A arg1) { return ret_double(arg1); }\n/* get_ret_double_ptr */ __attribute__ ((const)) double (*hs_bindgen_test_reparse_c29a0524e261854e (void)) (A arg1) { return &ret_double; } \n_Bool hs_bindgen_test_reparse_da83bbf3ce08a187 (A arg1) { return ret_bool1(arg1); }\n/* get_ret_bool1_ptr */ __attribute__ ((const)) _Bool (*hs_bindgen_test_reparse_4c0272348355fd7b (void)) (A arg1) { return &ret_bool1; } \nvoid hs_bindgen_test_reparse_98263067da352564 (A arg1, struct some_struct *arg2) { *arg2 = ret_struct(arg1); }\n/* get_ret_struct_ptr */ __attribute__ ((const)) struct some_struct (*hs_bindgen_test_reparse_aed8a54ad301c1ed (void)) (A arg1) { return &ret_struct; } \nvoid hs_bindgen_test_reparse_3daeab025170869e (A arg1, union some_union *arg2) { *arg2 = ret_union(arg1); }\n/* get_ret_union_ptr */ __attribute__ ((const)) union some_union (*hs_bindgen_test_reparse_682e09e159d012ae (void)) (A arg1) { return &ret_union; } \nenum some_enum hs_bindgen_test_reparse_1afc70c95eeaaf09 (A arg1) { return ret_enum(arg1); }\n/* get_ret_enum_ptr */ __attribute__ ((const)) enum some_enum (*hs_bindgen_test_reparse_1eb9e1744c110315 (void)) (A arg1) { return &ret_enum; } \nsigned int *hs_bindgen_test_reparse_031974ad82756999 (A arg1) { return ret_pointer1(arg1); }\n/* get_ret_pointer1_ptr */ __attribute__ ((const)) signed int *(*hs_bindgen_test_reparse_e0b897b0d664dd33 (void)) (A arg1) { return &ret_pointer1; } \nsigned int **hs_bindgen_test_reparse_3a4b95d14fe97f80 (A arg1) { return ret_pointer2(arg1); }\n/* get_ret_pointer2_ptr */ __attribute__ ((const)) signed int **(*hs_bindgen_test_reparse_82fad80fb47b7236 (void)) (A arg1) { return &ret_pointer2; } \nvoid *hs_bindgen_test_reparse_96fb446aa96eafd2 (A arg1) { return ret_pointer3(arg1); }\n/* get_ret_pointer3_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_reparse_efd348436d86573f (void)) (A arg1) { return &ret_pointer3; } \nsigned int hs_bindgen_test_reparse_49dcc244bd02f90d (A arg1) { return body1(arg1); }\n/* get_body1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_reparse_997e04203154d0e9 (void)) (A arg1) { return &body1; } \nA hs_bindgen_test_reparse_4712ad809c27d477 (void) { return body2(); }\n/* get_body2_ptr */ __attribute__ ((const)) A (*hs_bindgen_test_reparse_37bd37a0a14228bd (void)) (void) { return &body2; } \nvoid hs_bindgen_test_reparse_e390229cde8bd386 (A arg1, float _Complex *arg2) { args_complex_float(arg1, *arg2); }\n/* get_args_complex_float_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_bbd43e0b7f262423 (void)) (A arg1, float _Complex arg2) { return &args_complex_float; } \nvoid hs_bindgen_test_reparse_230de832b9c63bed (A arg1, double _Complex *arg2) { args_complex_double(arg1, *arg2); }\n/* get_args_complex_double_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_d654095e32e85ea0 (void)) (A arg1, double _Complex arg2) { return &args_complex_double; } \nvoid hs_bindgen_test_reparse_79af5d3f50e20e35 (A arg1, float _Complex *arg2) { *arg2 = ret_complex_float(arg1); }\n/* get_ret_complex_float_ptr */ __attribute__ ((const)) float _Complex (*hs_bindgen_test_reparse_c2911fd8888e3b7d (void)) (A arg1) { return &ret_complex_float; } \nvoid hs_bindgen_test_reparse_ed71779b9e9b1c7a (A arg1, double _Complex *arg2) { *arg2 = ret_complex_double(arg1); }\n/* get_ret_complex_double_ptr */ __attribute__ ((const)) double _Complex (*hs_bindgen_test_reparse_368c2a0e22b110dc (void)) (A arg1) { return &ret_complex_double; } \nvoid hs_bindgen_test_reparse_6118e8e73dd68f55 (A arg1, _Bool arg2) { bespoke_args1(arg1, arg2); }\n/* get_bespoke_args1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_93a5557905f490f6 (void)) (A arg1, _Bool arg2) { return &bespoke_args1; } \nvoid hs_bindgen_test_reparse_0968e3a6efd957cb (A arg1, size_t arg2) { bespoke_args2(arg1, arg2); }\n/* get_bespoke_args2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_daf6570af7b7ecc5 (void)) (A arg1, size_t arg2) { return &bespoke_args2; } \n_Bool hs_bindgen_test_reparse_c6e601a29d4614cd (A arg1) { return bespoke_ret1(arg1); }\n/* get_bespoke_ret1_ptr */ __attribute__ ((const)) _Bool (*hs_bindgen_test_reparse_146864821fa56426 (void)) (A arg1) { return &bespoke_ret1; } \nsize_t hs_bindgen_test_reparse_4dd147df19305554 (A arg1) { return bespoke_ret2(arg1); }\n/* get_bespoke_ret2_ptr */ __attribute__ ((const)) size_t (*hs_bindgen_test_reparse_9d181c086663dcbe (void)) (A arg1) { return &bespoke_ret2; } \nvoid hs_bindgen_test_reparse_9b04f6f1111852bd (A *arg1) { arr_args1(arg1); }\n/* get_arr_args1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_8a6079727ebf0474 (void)) (A arg1[]) { return &arr_args1; } \nvoid hs_bindgen_test_reparse_0434a82b86093487 (A **arg1) { arr_args2(arg1); }\n/* get_arr_args2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_babe3a26240cd0c7 (void)) (A *arg1[]) { return &arr_args2; } \nvoid hs_bindgen_test_reparse_91ebb7f6b5169d37 (A *arg1) { arr_args3(arg1); }\n/* get_arr_args3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_9dd1b6a92f93c7dd (void)) (A arg1[5]) { return &arr_args3; } \nvoid hs_bindgen_test_reparse_6707badb9b46b3ba (A **arg1) { arr_args4(arg1); }\n/* get_arr_args4_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_e1f7f09307f08420 (void)) (A *arg1[5]) { return &arr_args4; } \nvoid hs_bindgen_test_reparse_c31bf8a78aa68a4d (A arg1, void (*arg2) (void)) { funptr_args1(arg1, arg2); }\n/* get_funptr_args1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_1c0e4678c987c05d (void)) (A arg1, void (*arg2) (void)) { return &funptr_args1; } \nvoid hs_bindgen_test_reparse_98fbd01a7d3daae5 (A arg1, signed int (*arg2) (void)) { funptr_args2(arg1, arg2); }\n/* get_funptr_args2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_7b060727d5fd9def (void)) (A arg1, signed int (*arg2) (void)) { return &funptr_args2; } \nvoid hs_bindgen_test_reparse_1c1752238030d660 (A arg1, void (*arg2) (signed int arg1)) { funptr_args3(arg1, arg2); }\n/* get_funptr_args3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_daeb6d5a2f5d54e9 (void)) (A arg1, void (*arg2) (signed int arg1)) { return &funptr_args3; } \nvoid hs_bindgen_test_reparse_8c6106c42b1ae638 (A arg1, char (*arg2) (signed int arg1, double arg2)) { funptr_args4(arg1, arg2); }\n/* get_funptr_args4_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_a43eec054dfe7fd4 (void)) (A arg1, char (*arg2) (signed int arg1, double arg2)) { return &funptr_args4; } \nvoid hs_bindgen_test_reparse_367f3da78499ae2e (A arg1, signed int *(*arg2) (signed int arg1, double arg2)) { funptr_args5(arg1, arg2); }\n/* get_funptr_args5_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_464f5f738234f93e (void)) (A arg1, signed int *(*arg2) (signed int arg1, double arg2)) { return &funptr_args5; } \nvoid hs_bindgen_test_reparse_c5d7b3da7f176eba (A arg1) { comments1(arg1); }\n/* get_comments1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_c8f28692a6df33a4 (void)) (A arg1) { return &comments1; } \nvoid hs_bindgen_test_reparse_c33e21f21d3c0189 (A arg1, char const arg2) { const_prim_before1(arg1, arg2); }\n/* get_const_prim_before1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_e11e4b5f44bc145c (void)) (A arg1, char const arg2) { return &const_prim_before1; } \nvoid hs_bindgen_test_reparse_96a22f4d00d1024e (A arg1, signed char const arg2) { const_prim_before2(arg1, arg2); }\n/* get_const_prim_before2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_2735f43b5743f988 (void)) (A arg1, signed char const arg2) { return &const_prim_before2; } \nvoid hs_bindgen_test_reparse_0a38436fbc3be248 (A arg1, unsigned char const arg2) { const_prim_before3(arg1, arg2); }\n/* get_const_prim_before3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_e4cb72d2f1df53fd (void)) (A arg1, unsigned char const arg2) { return &const_prim_before3; } \nvoid hs_bindgen_test_reparse_718696d7ee1d51a0 (A arg1, char const arg2) { const_prim_after1(arg1, arg2); }\n/* get_const_prim_after1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_c17bd5a020927e9e (void)) (A arg1, char const arg2) { return &const_prim_after1; } \nvoid hs_bindgen_test_reparse_9c95677f7c61a6f5 (A arg1, signed char const arg2) { const_prim_after2(arg1, arg2); }\n/* get_const_prim_after2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_aaadc77459f026a4 (void)) (A arg1, signed char const arg2) { return &const_prim_after2; } \nvoid hs_bindgen_test_reparse_2b5b3d1fc6859ba3 (A arg1, unsigned char const arg2) { const_prim_after3(arg1, arg2); }\n/* get_const_prim_after3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_d08efe58f7551f9c (void)) (A arg1, unsigned char const arg2) { return &const_prim_after3; } \nvoid hs_bindgen_test_reparse_9d57b2fa38d5f7bc (A arg1, float const arg2) { const_withoutSign_before1(arg1, arg2); }\n/* get_const_withoutSign_before1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_7327c6cd454253cf (void)) (A arg1, float const arg2) { return &const_withoutSign_before1; } \nvoid hs_bindgen_test_reparse_52eba7cca7385d7d (A arg1, double const arg2) { const_withoutSign_before2(arg1, arg2); }\n/* get_const_withoutSign_before2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_1ff1052f6d3279e6 (void)) (A arg1, double const arg2) { return &const_withoutSign_before2; } \nvoid hs_bindgen_test_reparse_96b5756c596ae9a2 (A arg1, _Bool const arg2) { const_withoutSign_before3(arg1, arg2); }\n/* get_const_withoutSign_before3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_72fd083ea0be94e7 (void)) (A arg1, _Bool const arg2) { return &const_withoutSign_before3; } \nvoid hs_bindgen_test_reparse_4dee4a646d2da9ed (A arg1, struct some_struct const arg2) { const_withoutSign_before4(arg1, arg2); }\n/* get_const_withoutSign_before4_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_9087b446588f2208 (void)) (A arg1, struct some_struct const arg2) { return &const_withoutSign_before4; } \nvoid hs_bindgen_test_reparse_c944f84b146c925d (A arg1, union some_union const arg2) { const_withoutSign_before5(arg1, arg2); }\n/* get_const_withoutSign_before5_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_c8da645320ac5f3d (void)) (A arg1, union some_union const arg2) { return &const_withoutSign_before5; } \nvoid hs_bindgen_test_reparse_5fc3275406c102f7 (A arg1, enum some_enum const arg2) { const_withoutSign_before6(arg1, arg2); }\n/* get_const_withoutSign_before6_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_e87b52f038c210ec (void)) (A arg1, enum some_enum const arg2) { return &const_withoutSign_before6; } \nvoid hs_bindgen_test_reparse_ea6deb36fcb8d482 (A arg1, _Bool const arg2) { const_withoutSign_before7(arg1, arg2); }\n/* get_const_withoutSign_before7_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_e6e725a648481bf4 (void)) (A arg1, _Bool const arg2) { return &const_withoutSign_before7; } \nvoid hs_bindgen_test_reparse_7940b984b4ebe7f5 (A arg1, size_t const arg2) { const_withoutSign_before8(arg1, arg2); }\n/* get_const_withoutSign_before8_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_cf5b220806fc5356 (void)) (A arg1, size_t const arg2) { return &const_withoutSign_before8; } \nvoid hs_bindgen_test_reparse_0aa519784a037e2f (A arg1, float const arg2) { const_withoutSign_after1(arg1, arg2); }\n/* get_const_withoutSign_after1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_92eb08cf64fe2e87 (void)) (A arg1, float const arg2) { return &const_withoutSign_after1; } \nvoid hs_bindgen_test_reparse_ee60f796acb178b7 (A arg1, double const arg2) { const_withoutSign_after2(arg1, arg2); }\n/* get_const_withoutSign_after2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_8c0f96fa4264c653 (void)) (A arg1, double const arg2) { return &const_withoutSign_after2; } \nvoid hs_bindgen_test_reparse_56c75f3b604d0b43 (A arg1, _Bool const arg2) { const_withoutSign_after3(arg1, arg2); }\n/* get_const_withoutSign_after3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_0eef09188651c221 (void)) (A arg1, _Bool const arg2) { return &const_withoutSign_after3; } \nvoid hs_bindgen_test_reparse_d3022bf1fee59add (A arg1, struct some_struct const arg2) { const_withoutSign_after4(arg1, arg2); }\n/* get_const_withoutSign_after4_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_a383627604fb7e55 (void)) (A arg1, struct some_struct const arg2) { return &const_withoutSign_after4; } \nvoid hs_bindgen_test_reparse_79a3d4ac2b99caa8 (A arg1, union some_union const arg2) { const_withoutSign_after5(arg1, arg2); }\n/* get_const_withoutSign_after5_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_8de35e5f14909162 (void)) (A arg1, union some_union const arg2) { return &const_withoutSign_after5; } \nvoid hs_bindgen_test_reparse_53ec664f2511ac04 (A arg1, enum some_enum const arg2) { const_withoutSign_after6(arg1, arg2); }\n/* get_const_withoutSign_after6_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_66230a5bf8ef3e40 (void)) (A arg1, enum some_enum const arg2) { return &const_withoutSign_after6; } \nvoid hs_bindgen_test_reparse_d07aafa2007b7654 (A arg1, _Bool const arg2) { const_withoutSign_after7(arg1, arg2); }\n/* get_const_withoutSign_after7_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_a2bb46ca9f36cc46 (void)) (A arg1, _Bool const arg2) { return &const_withoutSign_after7; } \nvoid hs_bindgen_test_reparse_5f3b8ee6bb9b0dfb (A arg1, size_t const arg2) { const_withoutSign_after8(arg1, arg2); }\n/* get_const_withoutSign_after8_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_4777111d7551332e (void)) (A arg1, size_t const arg2) { return &const_withoutSign_after8; } \nvoid hs_bindgen_test_reparse_a79f56af6e9ef2f7 (A arg1, signed int const *arg2) { const_pointers_args1(arg1, arg2); }\n/* get_const_pointers_args1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_ad5aa005051441d6 (void)) (A arg1, signed int const *arg2) { return &const_pointers_args1; } \nvoid hs_bindgen_test_reparse_022f5aa1f6eb033a (A arg1, signed int const *arg2) { const_pointers_args2(arg1, arg2); }\n/* get_const_pointers_args2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_205f50942ba9b45e (void)) (A arg1, signed int const *arg2) { return &const_pointers_args2; } \nvoid hs_bindgen_test_reparse_39ad759f70b99e37 (A arg1, signed int *const arg2) { const_pointers_args3(arg1, arg2); }\n/* get_const_pointers_args3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_c2ddece5d773a650 (void)) (A arg1, signed int *const arg2) { return &const_pointers_args3; } \nvoid hs_bindgen_test_reparse_d4a5973dc0abcd97 (A arg1, signed int const *const arg2) { const_pointers_args4(arg1, arg2); }\n/* get_const_pointers_args4_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_ed9daaf3a67948dd (void)) (A arg1, signed int const *const arg2) { return &const_pointers_args4; } \nvoid hs_bindgen_test_reparse_1ed4abb42ab52f46 (A arg1, signed int const *const arg2) { const_pointers_args5(arg1, arg2); }\n/* get_const_pointers_args5_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_4b329c596dbf249e (void)) (A arg1, signed int const *const arg2) { return &const_pointers_args5; } \nsigned int const *hs_bindgen_test_reparse_7d43de2d59af106e (A arg1) { return const_pointers_ret1(arg1); }\n/* get_const_pointers_ret1_ptr */ __attribute__ ((const)) signed int const *(*hs_bindgen_test_reparse_fb3f484345aeb2d2 (void)) (A arg1) { return &const_pointers_ret1; } \nsigned int const *hs_bindgen_test_reparse_b7a5da1fec42008c (A arg1) { return const_pointers_ret2(arg1); }\n/* get_const_pointers_ret2_ptr */ __attribute__ ((const)) signed int const *(*hs_bindgen_test_reparse_4242fece463aa185 (void)) (A arg1) { return &const_pointers_ret2; } \nsigned int *const hs_bindgen_test_reparse_ea139108aca83634 (A arg1) { return const_pointers_ret3(arg1); }\n/* get_const_pointers_ret3_ptr */ __attribute__ ((const)) signed int *const (*hs_bindgen_test_reparse_d88b6e752a8081c3 (void)) (A arg1) { return &const_pointers_ret3; } \nsigned int const *const hs_bindgen_test_reparse_ab3c3cf6c30a91a4 (A arg1) { return const_pointers_ret4(arg1); }\n/* get_const_pointers_ret4_ptr */ __attribute__ ((const)) signed int const *const (*hs_bindgen_test_reparse_ae606f6ad11597ce (void)) (A arg1) { return &const_pointers_ret4; } \nsigned int const *const hs_bindgen_test_reparse_e32a2dec62135c7a (A arg1) { return const_pointers_ret5(arg1); }\n/* get_const_pointers_ret5_ptr */ __attribute__ ((const)) signed int const *const (*hs_bindgen_test_reparse_c86348709b464f9b (void)) (A arg1) { return &const_pointers_ret5; } \nvoid hs_bindgen_test_reparse_02f253d2b51e801b (A const *arg1) { const_array_elem1(arg1); }\n/* get_const_array_elem1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_74accfb2c8dbad9b (void)) (A const arg1[]) { return &const_array_elem1; } \nvoid hs_bindgen_test_reparse_ae594e2b9ac5b1c3 (A const **arg1) { const_array_elem2(arg1); }\n/* get_const_array_elem2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_e545f64064b17b40 (void)) (A const *arg1[]) { return &const_array_elem2; } \nvoid hs_bindgen_test_reparse_2149e2b9eadf2ded (A *const *arg1) { const_array_elem3(arg1); }\n/* get_const_array_elem3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_9467829a46b5dfc3 (void)) (A *const arg1[]) { return &const_array_elem3; } \nA hs_bindgen_test_reparse_14dbb45095c3c138 (void) { return noParams1(); }\n/* get_noParams1_ptr */ __attribute__ ((const)) A (*hs_bindgen_test_reparse_af69ae434bc46f7f (void)) (void) { return &noParams1; } \nA hs_bindgen_test_reparse_667f19a2bcb7ada6 (void) { return noParams2(); }\n/* get_noParams2_ptr */ __attribute__ ((const)) A (*hs_bindgen_test_reparse_2f061e057ad050d2 (void)) (void) { return &noParams2; } \nvoid hs_bindgen_test_reparse_34332de6ec849f2a (A arg1, signed int (*arg2) (void)) { noParams3(arg1, arg2); }\n/* get_noParams3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_reparse_a179b59c001d5529 (void)) (A arg1, signed int (*arg2) (void)) { return &noParams3; } \nvoid (*hs_bindgen_test_reparse_7c42b6acbe3f8845 (A arg1)) (void) { return funptr_ret1(arg1); }\n/* get_funptr_ret1_ptr */ __attribute__ ((const)) void (*(*hs_bindgen_test_reparse_ae380911ef8bd65a (void)) (A arg1)) (void) { return &funptr_ret1; } \nsigned int (*hs_bindgen_test_reparse_de4479feaa4c1b63 (A arg1)) (void) { return funptr_ret2(arg1); }\n/* get_funptr_ret2_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_reparse_0e60d042fc021fa0 (void)) (A arg1)) (void) { return &funptr_ret2; } \nvoid (*hs_bindgen_test_reparse_8ca1c491149e7a82 (A arg1)) (signed int arg1) { return funptr_ret3(arg1); }\n/* get_funptr_ret3_ptr */ __attribute__ ((const)) void (*(*hs_bindgen_test_reparse_ee8815fca40a6e75 (void)) (A arg1)) (signed int arg1) { return &funptr_ret3; } \nchar (*hs_bindgen_test_reparse_28a86a376fb47aef (A arg1)) (signed int arg1, double arg2) { return funptr_ret4(arg1); }\n/* get_funptr_ret4_ptr */ __attribute__ ((const)) char (*(*hs_bindgen_test_reparse_5d6841bf202a780e (void)) (A arg1)) (signed int arg1, double arg2) { return &funptr_ret4; } \nsigned int *(*hs_bindgen_test_reparse_677dad7824f8f926 (A arg1)) (signed int arg1, double arg2) { return funptr_ret5(arg1); }\n/* get_funptr_ret5_ptr */ __attribute__ ((const)) signed int *(*(*hs_bindgen_test_reparse_43e62fcd431c5993 (void)) (A arg1)) (signed int arg1, double arg2) { return &funptr_ret5; } \nsigned int const *(*hs_bindgen_test_reparse_32eeedbdca340214 (A arg1)) (signed int arg1, double arg2) { return funptr_ret6(arg1); }\n/* get_funptr_ret6_ptr */ __attribute__ ((const)) signed int const *(*(*hs_bindgen_test_reparse_ae01ac1f8293cbcd (void)) (A arg1)) (signed int arg1, double arg2) { return &funptr_ret6; } \nsigned int const *(*hs_bindgen_test_reparse_6c9848f47db6c013 (A arg1)) (signed int arg1, double arg2) { return funptr_ret7(arg1); }\n/* get_funptr_ret7_ptr */ __attribute__ ((const)) signed int const *(*(*hs_bindgen_test_reparse_f6edab58ac911c39 (void)) (A arg1)) (signed int arg1, double arg2) { return &funptr_ret7; } \nsigned int *const (*hs_bindgen_test_reparse_685ed1da05a496ec (A arg1)) (signed int arg1, double arg2) { return funptr_ret8(arg1); }\n/* get_funptr_ret8_ptr */ __attribute__ ((const)) signed int *const (*(*hs_bindgen_test_reparse_e16f7f0071bb9009 (void)) (A arg1)) (signed int arg1, double arg2) { return &funptr_ret8; } \nsigned int const *const (*hs_bindgen_test_reparse_d838f03e8f8b0e9a (A arg1)) (signed int arg1, double arg2) { return funptr_ret9(arg1); }\n/* get_funptr_ret9_ptr */ __attribute__ ((const)) signed int const *const (*(*hs_bindgen_test_reparse_d7e96ea609f2a0d5 (void)) (A arg1)) (signed int arg1, double arg2) { return &funptr_ret9; } \nsigned int const *const (*hs_bindgen_test_reparse_e44ba9f0163468d8 (A arg1)) (signed int arg1, double arg2) { return funptr_ret10(arg1); }\n/* get_funptr_ret10_ptr */ __attribute__ ((const)) signed int const *const (*(*hs_bindgen_test_reparse_163513e7413372dd (void)) (A arg1)) (signed int arg1, double arg2) { return &funptr_ret10; } \n")

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

foreign import ccall safe "hs_bindgen_test_reparse_10171eb304e5cc62" args_char1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

foreign import ccall unsafe "hs_bindgen_test_reparse_016baba9b269ce3e" hs_bindgen_test_reparse_016baba9b269ce3e
  :: IO (Ptr.FunPtr (A -> FC.CChar -> IO ()))

{-# NOINLINE args_char1_ptr #-}

args_char1_ptr :: Ptr.FunPtr (A -> FC.CChar -> IO ())
args_char1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_016baba9b269ce3e

{-| __C declaration:__ @args_char2@

    __defined at:__ @reparse.h:18:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_0a5b69870f97616a" args_char2
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CSChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_char2@

    __defined at:__ @reparse.h:18:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_deda633a655edee7" hs_bindgen_test_reparse_deda633a655edee7
  :: IO (Ptr.FunPtr (A -> FC.CSChar -> IO ()))

{-# NOINLINE args_char2_ptr #-}

args_char2_ptr :: Ptr.FunPtr (A -> FC.CSChar -> IO ())
args_char2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_deda633a655edee7

{-| __C declaration:__ @args_char3@

    __defined at:__ @reparse.h:19:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_2152f20110364cbb" args_char3
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_char3@

    __defined at:__ @reparse.h:19:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_929c40236948e098" hs_bindgen_test_reparse_929c40236948e098
  :: IO (Ptr.FunPtr (A -> FC.CUChar -> IO ()))

{-# NOINLINE args_char3_ptr #-}

args_char3_ptr :: Ptr.FunPtr (A -> FC.CUChar -> IO ())
args_char3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_929c40236948e098

{-| __C declaration:__ @args_short1@

    __defined at:__ @reparse.h:21:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_bad35762dd252147" args_short1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CShort
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_short1@

    __defined at:__ @reparse.h:21:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_6765935911469fba" hs_bindgen_test_reparse_6765935911469fba
  :: IO (Ptr.FunPtr (A -> FC.CShort -> IO ()))

{-# NOINLINE args_short1_ptr #-}

args_short1_ptr :: Ptr.FunPtr (A -> FC.CShort -> IO ())
args_short1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_6765935911469fba

{-| __C declaration:__ @args_short2@

    __defined at:__ @reparse.h:22:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_8705b73515e5d5e7" args_short2
  :: A
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
foreign import ccall unsafe "hs_bindgen_test_reparse_945292112fa4a0ee" hs_bindgen_test_reparse_945292112fa4a0ee
  :: IO (Ptr.FunPtr (A -> FC.CShort -> IO ()))

{-# NOINLINE args_short2_ptr #-}

args_short2_ptr :: Ptr.FunPtr (A -> FC.CShort -> IO ())
args_short2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_945292112fa4a0ee

{-| __C declaration:__ @args_short3@

    __defined at:__ @reparse.h:23:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_ae877590f2712156" args_short3
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUShort
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_short3@

    __defined at:__ @reparse.h:23:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_0a3b985c5f6b183e" hs_bindgen_test_reparse_0a3b985c5f6b183e
  :: IO (Ptr.FunPtr (A -> FC.CUShort -> IO ()))

{-# NOINLINE args_short3_ptr #-}

args_short3_ptr :: Ptr.FunPtr (A -> FC.CUShort -> IO ())
args_short3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_0a3b985c5f6b183e

{-| __C declaration:__ @args_int1@

    __defined at:__ @reparse.h:25:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_5bcc52df98b32e80" args_int1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_int1@

    __defined at:__ @reparse.h:25:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_0b9730298592ed7b" hs_bindgen_test_reparse_0b9730298592ed7b
  :: IO (Ptr.FunPtr (A -> FC.CInt -> IO ()))

{-# NOINLINE args_int1_ptr #-}

args_int1_ptr :: Ptr.FunPtr (A -> FC.CInt -> IO ())
args_int1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_0b9730298592ed7b

{-| __C declaration:__ @args_int2@

    __defined at:__ @reparse.h:26:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_d5542d284cc6174c" args_int2
  :: A
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
foreign import ccall unsafe "hs_bindgen_test_reparse_b35d311c59b97357" hs_bindgen_test_reparse_b35d311c59b97357
  :: IO (Ptr.FunPtr (A -> FC.CInt -> IO ()))

{-# NOINLINE args_int2_ptr #-}

args_int2_ptr :: Ptr.FunPtr (A -> FC.CInt -> IO ())
args_int2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_b35d311c59b97357

{-| __C declaration:__ @args_int3@

    __defined at:__ @reparse.h:27:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_940dd7ba49a6be4a" args_int3
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_int3@

    __defined at:__ @reparse.h:27:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_deb09915f72c9f94" hs_bindgen_test_reparse_deb09915f72c9f94
  :: IO (Ptr.FunPtr (A -> FC.CUInt -> IO ()))

{-# NOINLINE args_int3_ptr #-}

args_int3_ptr :: Ptr.FunPtr (A -> FC.CUInt -> IO ())
args_int3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_deb09915f72c9f94

{-| __C declaration:__ @args_long1@

    __defined at:__ @reparse.h:29:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_79b01356e07fd6c5" args_long1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CLong
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_long1@

    __defined at:__ @reparse.h:29:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_da0f1cd5ae89314a" hs_bindgen_test_reparse_da0f1cd5ae89314a
  :: IO (Ptr.FunPtr (A -> FC.CLong -> IO ()))

{-# NOINLINE args_long1_ptr #-}

args_long1_ptr :: Ptr.FunPtr (A -> FC.CLong -> IO ())
args_long1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_da0f1cd5ae89314a

{-| __C declaration:__ @args_long2@

    __defined at:__ @reparse.h:30:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_d5bdd91db9738145" args_long2
  :: A
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
foreign import ccall unsafe "hs_bindgen_test_reparse_758d9779edb07735" hs_bindgen_test_reparse_758d9779edb07735
  :: IO (Ptr.FunPtr (A -> FC.CLong -> IO ()))

{-# NOINLINE args_long2_ptr #-}

args_long2_ptr :: Ptr.FunPtr (A -> FC.CLong -> IO ())
args_long2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_758d9779edb07735

{-| __C declaration:__ @args_long3@

    __defined at:__ @reparse.h:31:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_9302b433a1667eb4" args_long3
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CULong
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_long3@

    __defined at:__ @reparse.h:31:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_db9704dc54e0b830" hs_bindgen_test_reparse_db9704dc54e0b830
  :: IO (Ptr.FunPtr (A -> FC.CULong -> IO ()))

{-# NOINLINE args_long3_ptr #-}

args_long3_ptr :: Ptr.FunPtr (A -> FC.CULong -> IO ())
args_long3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_db9704dc54e0b830

{-| __C declaration:__ @args_float@

    __defined at:__ @reparse.h:33:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_316fd20ab67d2e28" args_float
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CFloat
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_float@

    __defined at:__ @reparse.h:33:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_7bd676a3aba9914d" hs_bindgen_test_reparse_7bd676a3aba9914d
  :: IO (Ptr.FunPtr (A -> FC.CFloat -> IO ()))

{-# NOINLINE args_float_ptr #-}

args_float_ptr :: Ptr.FunPtr (A -> FC.CFloat -> IO ())
args_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_7bd676a3aba9914d

{-| __C declaration:__ @args_double@

    __defined at:__ @reparse.h:34:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_82242119ea26cfe9" args_double
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_double@

    __defined at:__ @reparse.h:34:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_bc71e4aa8bc9e516" hs_bindgen_test_reparse_bc71e4aa8bc9e516
  :: IO (Ptr.FunPtr (A -> FC.CDouble -> IO ()))

{-# NOINLINE args_double_ptr #-}

args_double_ptr :: Ptr.FunPtr (A -> FC.CDouble -> IO ())
args_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_bc71e4aa8bc9e516

{-| __C declaration:__ @args_bool1@

    __defined at:__ @reparse.h:35:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_8ac94b2768601ba9" args_bool1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_bool1@

    __defined at:__ @reparse.h:35:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_e8148b6b7b0284e3" hs_bindgen_test_reparse_e8148b6b7b0284e3
  :: IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE args_bool1_ptr #-}

args_bool1_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
args_bool1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_e8148b6b7b0284e3

{-| __C declaration:__ @args_struct@

    __defined at:__ @reparse.h:37:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_fc804a81327ef4f8" args_struct_wrapper
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr Some_struct
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

args_struct :: A -> Some_struct -> IO ()
args_struct =
  \x0 ->
    \x1 -> F.with x1 (\y2 -> args_struct_wrapper x0 y2)

{-| __C declaration:__ @args_struct@

    __defined at:__ @reparse.h:37:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_b679dc2bfc141ddd" hs_bindgen_test_reparse_b679dc2bfc141ddd
  :: IO (Ptr.FunPtr (A -> Some_struct -> IO ()))

{-# NOINLINE args_struct_ptr #-}

args_struct_ptr :: Ptr.FunPtr (A -> Some_struct -> IO ())
args_struct_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_b679dc2bfc141ddd

{-| __C declaration:__ @args_union@

    __defined at:__ @reparse.h:38:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_fd5beb002748e594" args_union_wrapper
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr Some_union
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

args_union :: A -> Some_union -> IO ()
args_union =
  \x0 ->
    \x1 -> F.with x1 (\y2 -> args_union_wrapper x0 y2)

{-| __C declaration:__ @args_union@

    __defined at:__ @reparse.h:38:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_9fc41568c0ef334a" hs_bindgen_test_reparse_9fc41568c0ef334a
  :: IO (Ptr.FunPtr (A -> Some_union -> IO ()))

{-# NOINLINE args_union_ptr #-}

args_union_ptr :: Ptr.FunPtr (A -> Some_union -> IO ())
args_union_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_9fc41568c0ef334a

{-| __C declaration:__ @args_enum@

    __defined at:__ @reparse.h:39:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_52c129b6f3ed3c8f" args_enum
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_enum
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_enum@

    __defined at:__ @reparse.h:39:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_a38701ffd6bcd8be" hs_bindgen_test_reparse_a38701ffd6bcd8be
  :: IO (Ptr.FunPtr (A -> Some_enum -> IO ()))

{-# NOINLINE args_enum_ptr #-}

args_enum_ptr :: Ptr.FunPtr (A -> Some_enum -> IO ())
args_enum_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_a38701ffd6bcd8be

{-| __C declaration:__ @args_pointer1@

    __defined at:__ @reparse.h:41:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_9a2762d56bf1de6c" args_pointer1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_pointer1@

    __defined at:__ @reparse.h:41:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_a64bc25bdd7e894f" hs_bindgen_test_reparse_a64bc25bdd7e894f
  :: IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE args_pointer1_ptr #-}

args_pointer1_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
args_pointer1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_a64bc25bdd7e894f

{-| __C declaration:__ @args_pointer2@

    __defined at:__ @reparse.h:42:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_ec864c09e80e67e1" args_pointer2
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr (Ptr.Ptr FC.CInt)
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @args_pointer2@

    __defined at:__ @reparse.h:42:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_9edb8a2162d37629" hs_bindgen_test_reparse_9edb8a2162d37629
  :: IO (Ptr.FunPtr (A -> (Ptr.Ptr (Ptr.Ptr FC.CInt)) -> IO ()))

{-# NOINLINE args_pointer2_ptr #-}

args_pointer2_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr (Ptr.Ptr FC.CInt)) -> IO ())
args_pointer2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_9edb8a2162d37629

{-| __C declaration:__ @args_pointer3@

    __defined at:__ @reparse.h:43:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_09151e71162f3239" args_pointer3
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr Void
     {- ^ __C declaration:__ @arg3@
     -}
  -> IO ()

{-| __C declaration:__ @args_pointer3@

    __defined at:__ @reparse.h:43:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_8a930c71a84a9846" hs_bindgen_test_reparse_8a930c71a84a9846
  :: IO (Ptr.FunPtr (A -> (Ptr.Ptr Void) -> IO ()))

{-# NOINLINE args_pointer3_ptr #-}

args_pointer3_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr Void) -> IO ())
args_pointer3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_8a930c71a84a9846

{-| __C declaration:__ @ret_A@

    __defined at:__ @reparse.h:47:3@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_721fac62084456f3" ret_A
  :: IO A

{-| __C declaration:__ @ret_A@

    __defined at:__ @reparse.h:47:3@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_877842edc48bfc6f" hs_bindgen_test_reparse_877842edc48bfc6f
  :: IO (Ptr.FunPtr (IO A))

{-# NOINLINE ret_A_ptr #-}

ret_A_ptr :: Ptr.FunPtr (IO A)
ret_A_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_877842edc48bfc6f

{-| __C declaration:__ @ret_char1@

    __defined at:__ @reparse.h:49:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_82bdf9fd8ea5cec3" ret_char1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CChar

{-| __C declaration:__ @ret_char1@

    __defined at:__ @reparse.h:49:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_adbd59964b60242f" hs_bindgen_test_reparse_adbd59964b60242f
  :: IO (Ptr.FunPtr (A -> IO FC.CChar))

{-# NOINLINE ret_char1_ptr #-}

ret_char1_ptr :: Ptr.FunPtr (A -> IO FC.CChar)
ret_char1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_adbd59964b60242f

{-| __C declaration:__ @ret_char2@

    __defined at:__ @reparse.h:50:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_cc0da46ef0c52b0d" ret_char2
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CSChar

{-| __C declaration:__ @ret_char2@

    __defined at:__ @reparse.h:50:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_59bbea2dfed561af" hs_bindgen_test_reparse_59bbea2dfed561af
  :: IO (Ptr.FunPtr (A -> IO FC.CSChar))

{-# NOINLINE ret_char2_ptr #-}

ret_char2_ptr :: Ptr.FunPtr (A -> IO FC.CSChar)
ret_char2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_59bbea2dfed561af

{-| __C declaration:__ @ret_char3@

    __defined at:__ @reparse.h:51:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_225667bb52779eb7" ret_char3
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CUChar

{-| __C declaration:__ @ret_char3@

    __defined at:__ @reparse.h:51:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_0801d372d3906773" hs_bindgen_test_reparse_0801d372d3906773
  :: IO (Ptr.FunPtr (A -> IO FC.CUChar))

{-# NOINLINE ret_char3_ptr #-}

ret_char3_ptr :: Ptr.FunPtr (A -> IO FC.CUChar)
ret_char3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_0801d372d3906773

{-| __C declaration:__ @ret_short1@

    __defined at:__ @reparse.h:53:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_9322ef5e958be938" ret_short1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CShort

{-| __C declaration:__ @ret_short1@

    __defined at:__ @reparse.h:53:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_ccfcc25e4950bc67" hs_bindgen_test_reparse_ccfcc25e4950bc67
  :: IO (Ptr.FunPtr (A -> IO FC.CShort))

{-# NOINLINE ret_short1_ptr #-}

ret_short1_ptr :: Ptr.FunPtr (A -> IO FC.CShort)
ret_short1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_ccfcc25e4950bc67

{-| __C declaration:__ @ret_short2@

    __defined at:__ @reparse.h:54:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_ef12449372a2af8b" ret_short2
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CShort

{-| __C declaration:__ @ret_short2@

    __defined at:__ @reparse.h:54:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_4bf91229e062d6be" hs_bindgen_test_reparse_4bf91229e062d6be
  :: IO (Ptr.FunPtr (A -> IO FC.CShort))

{-# NOINLINE ret_short2_ptr #-}

ret_short2_ptr :: Ptr.FunPtr (A -> IO FC.CShort)
ret_short2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_4bf91229e062d6be

{-| __C declaration:__ @ret_short3@

    __defined at:__ @reparse.h:55:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_a7f196b1d51beccf" ret_short3
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CUShort

{-| __C declaration:__ @ret_short3@

    __defined at:__ @reparse.h:55:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_d7104088c29df31d" hs_bindgen_test_reparse_d7104088c29df31d
  :: IO (Ptr.FunPtr (A -> IO FC.CUShort))

{-# NOINLINE ret_short3_ptr #-}

ret_short3_ptr :: Ptr.FunPtr (A -> IO FC.CUShort)
ret_short3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_d7104088c29df31d

{-| __C declaration:__ @ret_int1@

    __defined at:__ @reparse.h:57:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_9e2b08760143bbc2" ret_int1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @ret_int1@

    __defined at:__ @reparse.h:57:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_5c37e045e5cc329f" hs_bindgen_test_reparse_5c37e045e5cc329f
  :: IO (Ptr.FunPtr (A -> IO FC.CInt))

{-# NOINLINE ret_int1_ptr #-}

ret_int1_ptr :: Ptr.FunPtr (A -> IO FC.CInt)
ret_int1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_5c37e045e5cc329f

{-| __C declaration:__ @ret_int2@

    __defined at:__ @reparse.h:58:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_d7ec0cdbec9f2174" ret_int2
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @ret_int2@

    __defined at:__ @reparse.h:58:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_9e2af05c9cea58ba" hs_bindgen_test_reparse_9e2af05c9cea58ba
  :: IO (Ptr.FunPtr (A -> IO FC.CInt))

{-# NOINLINE ret_int2_ptr #-}

ret_int2_ptr :: Ptr.FunPtr (A -> IO FC.CInt)
ret_int2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_9e2af05c9cea58ba

{-| __C declaration:__ @ret_int3@

    __defined at:__ @reparse.h:59:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_b4415511dbc7b6ab" ret_int3
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CUInt

{-| __C declaration:__ @ret_int3@

    __defined at:__ @reparse.h:59:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_b04741094c75560d" hs_bindgen_test_reparse_b04741094c75560d
  :: IO (Ptr.FunPtr (A -> IO FC.CUInt))

{-# NOINLINE ret_int3_ptr #-}

ret_int3_ptr :: Ptr.FunPtr (A -> IO FC.CUInt)
ret_int3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_b04741094c75560d

{-| __C declaration:__ @ret_long1@

    __defined at:__ @reparse.h:61:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_62ed9e11769772fe" ret_long1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CLong

{-| __C declaration:__ @ret_long1@

    __defined at:__ @reparse.h:61:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_8f79f8c1226c822b" hs_bindgen_test_reparse_8f79f8c1226c822b
  :: IO (Ptr.FunPtr (A -> IO FC.CLong))

{-# NOINLINE ret_long1_ptr #-}

ret_long1_ptr :: Ptr.FunPtr (A -> IO FC.CLong)
ret_long1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_8f79f8c1226c822b

{-| __C declaration:__ @ret_long2@

    __defined at:__ @reparse.h:62:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_16ef6708c7b0d6a3" ret_long2
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CLong

{-| __C declaration:__ @ret_long2@

    __defined at:__ @reparse.h:62:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_2ba044632bcfc084" hs_bindgen_test_reparse_2ba044632bcfc084
  :: IO (Ptr.FunPtr (A -> IO FC.CLong))

{-# NOINLINE ret_long2_ptr #-}

ret_long2_ptr :: Ptr.FunPtr (A -> IO FC.CLong)
ret_long2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_2ba044632bcfc084

{-| __C declaration:__ @ret_long3@

    __defined at:__ @reparse.h:63:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_097510d70490cc54" ret_long3
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CULong

{-| __C declaration:__ @ret_long3@

    __defined at:__ @reparse.h:63:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_1129f4b482d7d20c" hs_bindgen_test_reparse_1129f4b482d7d20c
  :: IO (Ptr.FunPtr (A -> IO FC.CULong))

{-# NOINLINE ret_long3_ptr #-}

ret_long3_ptr :: Ptr.FunPtr (A -> IO FC.CULong)
ret_long3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_1129f4b482d7d20c

{-| __C declaration:__ @ret_float@

    __defined at:__ @reparse.h:65:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_5777c7f34d2aabaf" ret_float
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CFloat

{-| __C declaration:__ @ret_float@

    __defined at:__ @reparse.h:65:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_eed7871d97729b4d" hs_bindgen_test_reparse_eed7871d97729b4d
  :: IO (Ptr.FunPtr (A -> IO FC.CFloat))

{-# NOINLINE ret_float_ptr #-}

ret_float_ptr :: Ptr.FunPtr (A -> IO FC.CFloat)
ret_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_eed7871d97729b4d

{-| __C declaration:__ @ret_double@

    __defined at:__ @reparse.h:66:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_61e16d3cc5a31728" ret_double
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CDouble

{-| __C declaration:__ @ret_double@

    __defined at:__ @reparse.h:66:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_c29a0524e261854e" hs_bindgen_test_reparse_c29a0524e261854e
  :: IO (Ptr.FunPtr (A -> IO FC.CDouble))

{-# NOINLINE ret_double_ptr #-}

ret_double_ptr :: Ptr.FunPtr (A -> IO FC.CDouble)
ret_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_c29a0524e261854e

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @reparse.h:67:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_da83bbf3ce08a187" ret_bool1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CBool

{-| __C declaration:__ @ret_bool1@

    __defined at:__ @reparse.h:67:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_4c0272348355fd7b" hs_bindgen_test_reparse_4c0272348355fd7b
  :: IO (Ptr.FunPtr (A -> IO FC.CBool))

{-# NOINLINE ret_bool1_ptr #-}

ret_bool1_ptr :: Ptr.FunPtr (A -> IO FC.CBool)
ret_bool1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_4c0272348355fd7b

{-| __C declaration:__ @ret_struct@

    __defined at:__ @reparse.h:69:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_98263067da352564" ret_struct_wrapper
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr Some_struct
  -> IO ()

ret_struct :: A -> IO Some_struct
ret_struct =
  \x0 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\z1 ->
                                            ret_struct_wrapper x0 z1)

{-| __C declaration:__ @ret_struct@

    __defined at:__ @reparse.h:69:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_aed8a54ad301c1ed" hs_bindgen_test_reparse_aed8a54ad301c1ed
  :: IO (Ptr.FunPtr (A -> IO Some_struct))

{-# NOINLINE ret_struct_ptr #-}

ret_struct_ptr :: Ptr.FunPtr (A -> IO Some_struct)
ret_struct_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_aed8a54ad301c1ed

{-| __C declaration:__ @ret_union@

    __defined at:__ @reparse.h:70:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_3daeab025170869e" ret_union_wrapper
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr Some_union
  -> IO ()

ret_union :: A -> IO Some_union
ret_union =
  \x0 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\z1 ->
                                            ret_union_wrapper x0 z1)

{-| __C declaration:__ @ret_union@

    __defined at:__ @reparse.h:70:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_682e09e159d012ae" hs_bindgen_test_reparse_682e09e159d012ae
  :: IO (Ptr.FunPtr (A -> IO Some_union))

{-# NOINLINE ret_union_ptr #-}

ret_union_ptr :: Ptr.FunPtr (A -> IO Some_union)
ret_union_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_682e09e159d012ae

{-| __C declaration:__ @ret_enum@

    __defined at:__ @reparse.h:71:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_1afc70c95eeaaf09" ret_enum
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO Some_enum

{-| __C declaration:__ @ret_enum@

    __defined at:__ @reparse.h:71:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_1eb9e1744c110315" hs_bindgen_test_reparse_1eb9e1744c110315
  :: IO (Ptr.FunPtr (A -> IO Some_enum))

{-# NOINLINE ret_enum_ptr #-}

ret_enum_ptr :: Ptr.FunPtr (A -> IO Some_enum)
ret_enum_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_1eb9e1744c110315

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @reparse.h:73:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_031974ad82756999" ret_pointer1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @ret_pointer1@

    __defined at:__ @reparse.h:73:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_e0b897b0d664dd33" hs_bindgen_test_reparse_e0b897b0d664dd33
  :: IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE ret_pointer1_ptr #-}

ret_pointer1_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
ret_pointer1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_e0b897b0d664dd33

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @reparse.h:74:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_3a4b95d14fe97f80" ret_pointer2
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-| __C declaration:__ @ret_pointer2@

    __defined at:__ @reparse.h:74:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_82fad80fb47b7236" hs_bindgen_test_reparse_82fad80fb47b7236
  :: IO (Ptr.FunPtr (A -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt))))

{-# NOINLINE ret_pointer2_ptr #-}

ret_pointer2_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr (Ptr.Ptr FC.CInt)))
ret_pointer2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_82fad80fb47b7236

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @reparse.h:75:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_96fb446aa96eafd2" ret_pointer3
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @ret_pointer3@

    __defined at:__ @reparse.h:75:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_efd348436d86573f" hs_bindgen_test_reparse_efd348436d86573f
  :: IO (Ptr.FunPtr (A -> IO (Ptr.Ptr Void)))

{-# NOINLINE ret_pointer3_ptr #-}

ret_pointer3_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr Void))
ret_pointer3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_efd348436d86573f

{-| __C declaration:__ @body1@

    __defined at:__ @reparse.h:79:5@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_49dcc244bd02f90d" body1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @body1@

    __defined at:__ @reparse.h:79:5@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_997e04203154d0e9" hs_bindgen_test_reparse_997e04203154d0e9
  :: IO (Ptr.FunPtr (A -> IO FC.CInt))

{-# NOINLINE body1_ptr #-}

body1_ptr :: Ptr.FunPtr (A -> IO FC.CInt)
body1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_997e04203154d0e9

{-| __C declaration:__ @body2@

    __defined at:__ @reparse.h:80:3@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_4712ad809c27d477" body2
  :: IO A

{-| __C declaration:__ @body2@

    __defined at:__ @reparse.h:80:3@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_37bd37a0a14228bd" hs_bindgen_test_reparse_37bd37a0a14228bd
  :: IO (Ptr.FunPtr (IO A))

{-# NOINLINE body2_ptr #-}

body2_ptr :: Ptr.FunPtr (IO A)
body2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_37bd37a0a14228bd

{-| __C declaration:__ @args_complex_float@

    __defined at:__ @reparse.h:84:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_e390229cde8bd386" args_complex_float_wrapper
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

args_complex_float :: A -> (Data.Complex.Complex FC.CFloat) -> IO ()
args_complex_float =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 -> args_complex_float_wrapper x0 y2)

{-| __C declaration:__ @args_complex_float@

    __defined at:__ @reparse.h:84:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_bbd43e0b7f262423" hs_bindgen_test_reparse_bbd43e0b7f262423
  :: IO (Ptr.FunPtr (A -> (Data.Complex.Complex FC.CFloat) -> IO ()))

{-# NOINLINE args_complex_float_ptr #-}

args_complex_float_ptr :: Ptr.FunPtr (A -> (Data.Complex.Complex FC.CFloat) -> IO ())
args_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_bbd43e0b7f262423

{-| __C declaration:__ @args_complex_double@

    __defined at:__ @reparse.h:85:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_230de832b9c63bed" args_complex_double_wrapper
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

args_complex_double :: A -> (Data.Complex.Complex FC.CDouble) -> IO ()
args_complex_double =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 -> args_complex_double_wrapper x0 y2)

{-| __C declaration:__ @args_complex_double@

    __defined at:__ @reparse.h:85:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_d654095e32e85ea0" hs_bindgen_test_reparse_d654095e32e85ea0
  :: IO (Ptr.FunPtr (A -> (Data.Complex.Complex FC.CDouble) -> IO ()))

{-# NOINLINE args_complex_double_ptr #-}

args_complex_double_ptr :: Ptr.FunPtr (A -> (Data.Complex.Complex FC.CDouble) -> IO ())
args_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_d654095e32e85ea0

{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @reparse.h:86:17@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_79af5d3f50e20e35" ret_complex_float_wrapper
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> IO ()

ret_complex_float :: A -> IO (Data.Complex.Complex FC.CFloat)
ret_complex_float =
  \x0 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\z1 ->
                                            ret_complex_float_wrapper x0 z1)

{-| __C declaration:__ @ret_complex_float@

    __defined at:__ @reparse.h:86:17@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_c2911fd8888e3b7d" hs_bindgen_test_reparse_c2911fd8888e3b7d
  :: IO (Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CFloat)))

{-# NOINLINE ret_complex_float_ptr #-}

ret_complex_float_ptr :: Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CFloat))
ret_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_c2911fd8888e3b7d

{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @reparse.h:87:17@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_ed71779b9e9b1c7a" ret_complex_double_wrapper
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> IO ()

ret_complex_double :: A -> IO (Data.Complex.Complex FC.CDouble)
ret_complex_double =
  \x0 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\z1 ->
                                            ret_complex_double_wrapper x0 z1)

{-| __C declaration:__ @ret_complex_double@

    __defined at:__ @reparse.h:87:17@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_368c2a0e22b110dc" hs_bindgen_test_reparse_368c2a0e22b110dc
  :: IO (Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CDouble)))

{-# NOINLINE ret_complex_double_ptr #-}

ret_complex_double_ptr :: Ptr.FunPtr (A -> IO (Data.Complex.Complex FC.CDouble))
ret_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_368c2a0e22b110dc

{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @reparse.h:94:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_6118e8e73dd68f55" bespoke_args1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @bespoke_args1@

    __defined at:__ @reparse.h:94:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_93a5557905f490f6" hs_bindgen_test_reparse_93a5557905f490f6
  :: IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE bespoke_args1_ptr #-}

bespoke_args1_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
bespoke_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_93a5557905f490f6

{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @reparse.h:95:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_0968e3a6efd957cb" bespoke_args2
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CSize
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @bespoke_args2@

    __defined at:__ @reparse.h:95:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_daf6570af7b7ecc5" hs_bindgen_test_reparse_daf6570af7b7ecc5
  :: IO (Ptr.FunPtr (A -> FC.CSize -> IO ()))

{-# NOINLINE bespoke_args2_ptr #-}

bespoke_args2_ptr :: Ptr.FunPtr (A -> FC.CSize -> IO ())
bespoke_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_daf6570af7b7ecc5

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @reparse.h:97:8@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_c6e601a29d4614cd" bespoke_ret1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CBool

{-| __C declaration:__ @bespoke_ret1@

    __defined at:__ @reparse.h:97:8@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_146864821fa56426" hs_bindgen_test_reparse_146864821fa56426
  :: IO (Ptr.FunPtr (A -> IO FC.CBool))

{-# NOINLINE bespoke_ret1_ptr #-}

bespoke_ret1_ptr :: Ptr.FunPtr (A -> IO FC.CBool)
bespoke_ret1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_146864821fa56426

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @reparse.h:98:8@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_4dd147df19305554" bespoke_ret2
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO FC.CSize

{-| __C declaration:__ @bespoke_ret2@

    __defined at:__ @reparse.h:98:8@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_9d181c086663dcbe" hs_bindgen_test_reparse_9d181c086663dcbe
  :: IO (Ptr.FunPtr (A -> IO FC.CSize))

{-# NOINLINE bespoke_ret2_ptr #-}

bespoke_ret2_ptr :: Ptr.FunPtr (A -> IO FC.CSize)
bespoke_ret2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_9d181c086663dcbe

foreign import ccall safe "hs_bindgen_test_reparse_9b04f6f1111852bd" arr_args1_wrapper
  :: Ptr.Ptr A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

arr_args1 :: (HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ()
arr_args1 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    arr_args1_wrapper ptr1)

foreign import ccall unsafe "hs_bindgen_test_reparse_8a6079727ebf0474" hs_bindgen_test_reparse_8a6079727ebf0474
  :: IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ()))

{-# NOINLINE arr_args1_ptr #-}

arr_args1_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ())
arr_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_8a6079727ebf0474

{-| __C declaration:__ @arr_args2@

    __defined at:__ @reparse.h:105:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_0434a82b86093487" arr_args2_wrapper
  :: Ptr.Ptr (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

arr_args2 :: (HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ()
arr_args2 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    arr_args2_wrapper ptr1)

{-| __C declaration:__ @arr_args2@

    __defined at:__ @reparse.h:105:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_babe3a26240cd0c7" hs_bindgen_test_reparse_babe3a26240cd0c7
  :: IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ()))

{-# NOINLINE arr_args2_ptr #-}

arr_args2_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ())
arr_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_babe3a26240cd0c7

{-| __C declaration:__ @arr_args3@

    __defined at:__ @reparse.h:106:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_91ebb7f6b5169d37" arr_args3_wrapper
  :: Ptr.Ptr A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

arr_args3 :: ((HsBindgen.Runtime.ConstantArray.ConstantArray 5) A) -> IO ()
arr_args3 =
  \x0 ->
    HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr1 ->
                                                  arr_args3_wrapper ptr1)

{-| __C declaration:__ @arr_args3@

    __defined at:__ @reparse.h:106:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_9dd1b6a92f93c7dd" hs_bindgen_test_reparse_9dd1b6a92f93c7dd
  :: IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) A) -> IO ()))

{-# NOINLINE arr_args3_ptr #-}

arr_args3_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) A) -> IO ())
arr_args3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_9dd1b6a92f93c7dd

{-| __C declaration:__ @arr_args4@

    __defined at:__ @reparse.h:107:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_6707badb9b46b3ba" arr_args4_wrapper
  :: Ptr.Ptr (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

arr_args4 :: ((HsBindgen.Runtime.ConstantArray.ConstantArray 5) (Ptr.Ptr A)) -> IO ()
arr_args4 =
  \x0 ->
    HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr1 ->
                                                  arr_args4_wrapper ptr1)

{-| __C declaration:__ @arr_args4@

    __defined at:__ @reparse.h:107:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_e1f7f09307f08420" hs_bindgen_test_reparse_e1f7f09307f08420
  :: IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) (Ptr.Ptr A)) -> IO ()))

{-# NOINLINE arr_args4_ptr #-}

arr_args4_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 5) (Ptr.Ptr A)) -> IO ())
arr_args4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_e1f7f09307f08420

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

foreign import ccall safe "hs_bindgen_test_reparse_c31bf8a78aa68a4d" funptr_args1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (IO ())
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

foreign import ccall unsafe "hs_bindgen_test_reparse_1c0e4678c987c05d" hs_bindgen_test_reparse_1c0e4678c987c05d
  :: IO (Ptr.FunPtr (A -> (Ptr.FunPtr (IO ())) -> IO ()))

{-# NOINLINE funptr_args1_ptr #-}

funptr_args1_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (IO ())) -> IO ())
funptr_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_1c0e4678c987c05d

{-| __C declaration:__ @funptr_args2@

    __defined at:__ @reparse.h:127:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_98fbd01a7d3daae5" funptr_args2
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (IO FC.CInt)
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @funptr_args2@

    __defined at:__ @reparse.h:127:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_7b060727d5fd9def" hs_bindgen_test_reparse_7b060727d5fd9def
  :: IO (Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ()))

{-# NOINLINE funptr_args2_ptr #-}

funptr_args2_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ())
funptr_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_7b060727d5fd9def

{-| __C declaration:__ @funptr_args3@

    __defined at:__ @reparse.h:128:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_1c1752238030d660" funptr_args3
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (FC.CInt -> IO ())
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @funptr_args3@

    __defined at:__ @reparse.h:128:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_daeb6d5a2f5d54e9" hs_bindgen_test_reparse_daeb6d5a2f5d54e9
  :: IO (Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> IO ())) -> IO ()))

{-# NOINLINE funptr_args3_ptr #-}

funptr_args3_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> IO ())) -> IO ())
funptr_args3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_daeb6d5a2f5d54e9

{-| __C declaration:__ @funptr_args4@

    __defined at:__ @reparse.h:129:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_8c6106c42b1ae638" funptr_args4
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @funptr_args4@

    __defined at:__ @reparse.h:129:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_a43eec054dfe7fd4" hs_bindgen_test_reparse_a43eec054dfe7fd4
  :: IO (Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)) -> IO ()))

{-# NOINLINE funptr_args4_ptr #-}

funptr_args4_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)) -> IO ())
funptr_args4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_a43eec054dfe7fd4

{-| __C declaration:__ @funptr_args5@

    __defined at:__ @reparse.h:130:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_367f3da78499ae2e" funptr_args5
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @funptr_args5@

    __defined at:__ @reparse.h:130:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_464f5f738234f93e" hs_bindgen_test_reparse_464f5f738234f93e
  :: IO (Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))) -> IO ()))

{-# NOINLINE funptr_args5_ptr #-}

funptr_args5_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))) -> IO ())
funptr_args5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_464f5f738234f93e

{-| __C declaration:__ @funptr_typedef1@

    __defined at:__ @reparse.h:132:16@

    __exported by:__ @reparse.h@
-}
newtype Funptr_typedef1 = Funptr_typedef1
  { un_Funptr_typedef1 :: Ptr.FunPtr (IO A)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @funptr_typedef2@

    __defined at:__ @reparse.h:133:16@

    __exported by:__ @reparse.h@
-}
newtype Funptr_typedef2 = Funptr_typedef2
  { un_Funptr_typedef2 :: Ptr.FunPtr (IO (Ptr.Ptr A))
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @funptr_typedef3@

    __defined at:__ @reparse.h:134:16@

    __exported by:__ @reparse.h@
-}
newtype Funptr_typedef3 = Funptr_typedef3
  { un_Funptr_typedef3 :: Ptr.FunPtr (IO (Ptr.Ptr (Ptr.Ptr A)))
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @funptr_typedef4@

    __defined at:__ @reparse.h:135:16@

    __exported by:__ @reparse.h@
-}
newtype Funptr_typedef4 = Funptr_typedef4
  { un_Funptr_typedef4 :: Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO A)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @funptr_typedef5@

    __defined at:__ @reparse.h:136:16@

    __exported by:__ @reparse.h@
-}
newtype Funptr_typedef5 = Funptr_typedef5
  { un_Funptr_typedef5 :: Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A))
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Comments in awkward places

  (Prior to language-c we failed to parse there.)

__C declaration:__ @comments1@

__defined at:__ @reparse.h:144:25@

__exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_c5d7b3da7f176eba" comments1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

{-| Comments in awkward places

  (Prior to language-c we failed to parse there.)

__C declaration:__ @comments1@

__defined at:__ @reparse.h:144:25@

__exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_c8f28692a6df33a4" hs_bindgen_test_reparse_c8f28692a6df33a4
  :: IO (Ptr.FunPtr (A -> IO ()))

{-# NOINLINE comments1_ptr #-}

comments1_ptr :: Ptr.FunPtr (A -> IO ())
comments1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_c8f28692a6df33a4

{-| __C declaration:__ @comments2@

    __defined at:__ @reparse.h:145:30@

    __exported by:__ @reparse.h@
-}
newtype Comments2 = Comments2
  { un_Comments2 :: A
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

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

{-| `const` qualifier

  NOTE: These were not parsed correctly prior to the switch to language-c.

__C declaration:__ @const_prim_before1@

__defined at:__ @reparse.h:179:6@

__exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_c33e21f21d3c0189" const_prim_before1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| `const` qualifier

  NOTE: These were not parsed correctly prior to the switch to language-c.

__C declaration:__ @const_prim_before1@

__defined at:__ @reparse.h:179:6@

__exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_e11e4b5f44bc145c" hs_bindgen_test_reparse_e11e4b5f44bc145c
  :: IO (Ptr.FunPtr (A -> FC.CChar -> IO ()))

{-# NOINLINE const_prim_before1_ptr #-}

const_prim_before1_ptr :: Ptr.FunPtr (A -> FC.CChar -> IO ())
const_prim_before1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_e11e4b5f44bc145c

{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @reparse.h:180:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_96a22f4d00d1024e" const_prim_before2
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CSChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_before2@

    __defined at:__ @reparse.h:180:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_2735f43b5743f988" hs_bindgen_test_reparse_2735f43b5743f988
  :: IO (Ptr.FunPtr (A -> FC.CSChar -> IO ()))

{-# NOINLINE const_prim_before2_ptr #-}

const_prim_before2_ptr :: Ptr.FunPtr (A -> FC.CSChar -> IO ())
const_prim_before2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_2735f43b5743f988

{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @reparse.h:181:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_0a38436fbc3be248" const_prim_before3
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_before3@

    __defined at:__ @reparse.h:181:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_e4cb72d2f1df53fd" hs_bindgen_test_reparse_e4cb72d2f1df53fd
  :: IO (Ptr.FunPtr (A -> FC.CUChar -> IO ()))

{-# NOINLINE const_prim_before3_ptr #-}

const_prim_before3_ptr :: Ptr.FunPtr (A -> FC.CUChar -> IO ())
const_prim_before3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_e4cb72d2f1df53fd

{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @reparse.h:182:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_718696d7ee1d51a0" const_prim_after1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_after1@

    __defined at:__ @reparse.h:182:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_c17bd5a020927e9e" hs_bindgen_test_reparse_c17bd5a020927e9e
  :: IO (Ptr.FunPtr (A -> FC.CChar -> IO ()))

{-# NOINLINE const_prim_after1_ptr #-}

const_prim_after1_ptr :: Ptr.FunPtr (A -> FC.CChar -> IO ())
const_prim_after1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_c17bd5a020927e9e

{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @reparse.h:183:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_9c95677f7c61a6f5" const_prim_after2
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CSChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_after2@

    __defined at:__ @reparse.h:183:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_aaadc77459f026a4" hs_bindgen_test_reparse_aaadc77459f026a4
  :: IO (Ptr.FunPtr (A -> FC.CSChar -> IO ()))

{-# NOINLINE const_prim_after2_ptr #-}

const_prim_after2_ptr :: Ptr.FunPtr (A -> FC.CSChar -> IO ())
const_prim_after2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_aaadc77459f026a4

{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @reparse.h:184:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_2b5b3d1fc6859ba3" const_prim_after3
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CUChar
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_prim_after3@

    __defined at:__ @reparse.h:184:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_d08efe58f7551f9c" hs_bindgen_test_reparse_d08efe58f7551f9c
  :: IO (Ptr.FunPtr (A -> FC.CUChar -> IO ()))

{-# NOINLINE const_prim_after3_ptr #-}

const_prim_after3_ptr :: Ptr.FunPtr (A -> FC.CUChar -> IO ())
const_prim_after3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_d08efe58f7551f9c

{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @reparse.h:188:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_9d57b2fa38d5f7bc" const_withoutSign_before1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CFloat
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before1@

    __defined at:__ @reparse.h:188:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_7327c6cd454253cf" hs_bindgen_test_reparse_7327c6cd454253cf
  :: IO (Ptr.FunPtr (A -> FC.CFloat -> IO ()))

{-# NOINLINE const_withoutSign_before1_ptr #-}

const_withoutSign_before1_ptr :: Ptr.FunPtr (A -> FC.CFloat -> IO ())
const_withoutSign_before1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_7327c6cd454253cf

{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @reparse.h:189:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_52eba7cca7385d7d" const_withoutSign_before2
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before2@

    __defined at:__ @reparse.h:189:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_1ff1052f6d3279e6" hs_bindgen_test_reparse_1ff1052f6d3279e6
  :: IO (Ptr.FunPtr (A -> FC.CDouble -> IO ()))

{-# NOINLINE const_withoutSign_before2_ptr #-}

const_withoutSign_before2_ptr :: Ptr.FunPtr (A -> FC.CDouble -> IO ())
const_withoutSign_before2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_1ff1052f6d3279e6

{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @reparse.h:190:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_96b5756c596ae9a2" const_withoutSign_before3
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before3@

    __defined at:__ @reparse.h:190:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_72fd083ea0be94e7" hs_bindgen_test_reparse_72fd083ea0be94e7
  :: IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE const_withoutSign_before3_ptr #-}

const_withoutSign_before3_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_before3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_72fd083ea0be94e7

{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @reparse.h:191:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_4dee4a646d2da9ed" const_withoutSign_before4
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_struct
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before4@

    __defined at:__ @reparse.h:191:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_9087b446588f2208" hs_bindgen_test_reparse_9087b446588f2208
  :: IO (Ptr.FunPtr (A -> Some_struct -> IO ()))

{-# NOINLINE const_withoutSign_before4_ptr #-}

const_withoutSign_before4_ptr :: Ptr.FunPtr (A -> Some_struct -> IO ())
const_withoutSign_before4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_9087b446588f2208

{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @reparse.h:192:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_c944f84b146c925d" const_withoutSign_before5
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_union
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before5@

    __defined at:__ @reparse.h:192:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_c8da645320ac5f3d" hs_bindgen_test_reparse_c8da645320ac5f3d
  :: IO (Ptr.FunPtr (A -> Some_union -> IO ()))

{-# NOINLINE const_withoutSign_before5_ptr #-}

const_withoutSign_before5_ptr :: Ptr.FunPtr (A -> Some_union -> IO ())
const_withoutSign_before5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_c8da645320ac5f3d

{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @reparse.h:193:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_5fc3275406c102f7" const_withoutSign_before6
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_enum
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before6@

    __defined at:__ @reparse.h:193:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_e87b52f038c210ec" hs_bindgen_test_reparse_e87b52f038c210ec
  :: IO (Ptr.FunPtr (A -> Some_enum -> IO ()))

{-# NOINLINE const_withoutSign_before6_ptr #-}

const_withoutSign_before6_ptr :: Ptr.FunPtr (A -> Some_enum -> IO ())
const_withoutSign_before6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_e87b52f038c210ec

{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @reparse.h:194:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_ea6deb36fcb8d482" const_withoutSign_before7
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before7@

    __defined at:__ @reparse.h:194:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_e6e725a648481bf4" hs_bindgen_test_reparse_e6e725a648481bf4
  :: IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE const_withoutSign_before7_ptr #-}

const_withoutSign_before7_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_before7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_e6e725a648481bf4

{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @reparse.h:195:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_7940b984b4ebe7f5" const_withoutSign_before8
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CSize
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_before8@

    __defined at:__ @reparse.h:195:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_cf5b220806fc5356" hs_bindgen_test_reparse_cf5b220806fc5356
  :: IO (Ptr.FunPtr (A -> FC.CSize -> IO ()))

{-# NOINLINE const_withoutSign_before8_ptr #-}

const_withoutSign_before8_ptr :: Ptr.FunPtr (A -> FC.CSize -> IO ())
const_withoutSign_before8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_cf5b220806fc5356

{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @reparse.h:197:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_0aa519784a037e2f" const_withoutSign_after1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CFloat
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after1@

    __defined at:__ @reparse.h:197:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_92eb08cf64fe2e87" hs_bindgen_test_reparse_92eb08cf64fe2e87
  :: IO (Ptr.FunPtr (A -> FC.CFloat -> IO ()))

{-# NOINLINE const_withoutSign_after1_ptr #-}

const_withoutSign_after1_ptr :: Ptr.FunPtr (A -> FC.CFloat -> IO ())
const_withoutSign_after1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_92eb08cf64fe2e87

{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @reparse.h:198:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_ee60f796acb178b7" const_withoutSign_after2
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after2@

    __defined at:__ @reparse.h:198:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_8c0f96fa4264c653" hs_bindgen_test_reparse_8c0f96fa4264c653
  :: IO (Ptr.FunPtr (A -> FC.CDouble -> IO ()))

{-# NOINLINE const_withoutSign_after2_ptr #-}

const_withoutSign_after2_ptr :: Ptr.FunPtr (A -> FC.CDouble -> IO ())
const_withoutSign_after2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_8c0f96fa4264c653

{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @reparse.h:199:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_56c75f3b604d0b43" const_withoutSign_after3
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after3@

    __defined at:__ @reparse.h:199:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_0eef09188651c221" hs_bindgen_test_reparse_0eef09188651c221
  :: IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE const_withoutSign_after3_ptr #-}

const_withoutSign_after3_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_after3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_0eef09188651c221

{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @reparse.h:200:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_d3022bf1fee59add" const_withoutSign_after4
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_struct
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after4@

    __defined at:__ @reparse.h:200:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_a383627604fb7e55" hs_bindgen_test_reparse_a383627604fb7e55
  :: IO (Ptr.FunPtr (A -> Some_struct -> IO ()))

{-# NOINLINE const_withoutSign_after4_ptr #-}

const_withoutSign_after4_ptr :: Ptr.FunPtr (A -> Some_struct -> IO ())
const_withoutSign_after4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_a383627604fb7e55

{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @reparse.h:201:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_79a3d4ac2b99caa8" const_withoutSign_after5
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_union
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after5@

    __defined at:__ @reparse.h:201:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_8de35e5f14909162" hs_bindgen_test_reparse_8de35e5f14909162
  :: IO (Ptr.FunPtr (A -> Some_union -> IO ()))

{-# NOINLINE const_withoutSign_after5_ptr #-}

const_withoutSign_after5_ptr :: Ptr.FunPtr (A -> Some_union -> IO ())
const_withoutSign_after5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_8de35e5f14909162

{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @reparse.h:202:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_53ec664f2511ac04" const_withoutSign_after6
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Some_enum
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after6@

    __defined at:__ @reparse.h:202:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_66230a5bf8ef3e40" hs_bindgen_test_reparse_66230a5bf8ef3e40
  :: IO (Ptr.FunPtr (A -> Some_enum -> IO ()))

{-# NOINLINE const_withoutSign_after6_ptr #-}

const_withoutSign_after6_ptr :: Ptr.FunPtr (A -> Some_enum -> IO ())
const_withoutSign_after6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_66230a5bf8ef3e40

{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @reparse.h:203:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_d07aafa2007b7654" const_withoutSign_after7
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CBool
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after7@

    __defined at:__ @reparse.h:203:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_a2bb46ca9f36cc46" hs_bindgen_test_reparse_a2bb46ca9f36cc46
  :: IO (Ptr.FunPtr (A -> FC.CBool -> IO ()))

{-# NOINLINE const_withoutSign_after7_ptr #-}

const_withoutSign_after7_ptr :: Ptr.FunPtr (A -> FC.CBool -> IO ())
const_withoutSign_after7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_a2bb46ca9f36cc46

{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @reparse.h:204:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_5f3b8ee6bb9b0dfb" const_withoutSign_after8
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> FC.CSize
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_withoutSign_after8@

    __defined at:__ @reparse.h:204:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_4777111d7551332e" hs_bindgen_test_reparse_4777111d7551332e
  :: IO (Ptr.FunPtr (A -> FC.CSize -> IO ()))

{-# NOINLINE const_withoutSign_after8_ptr #-}

const_withoutSign_after8_ptr :: Ptr.FunPtr (A -> FC.CSize -> IO ())
const_withoutSign_after8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_4777111d7551332e

{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @reparse.h:208:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_a79f56af6e9ef2f7" const_pointers_args1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @const_pointers_args1@

    __defined at:__ @reparse.h:208:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_ad5aa005051441d6" hs_bindgen_test_reparse_ad5aa005051441d6
  :: IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args1_ptr #-}

const_pointers_args1_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_ad5aa005051441d6

{-| __C declaration:__ @const_pointers_args2@

    __defined at:__ @reparse.h:209:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_022f5aa1f6eb033a" const_pointers_args2
  :: A
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
foreign import ccall unsafe "hs_bindgen_test_reparse_205f50942ba9b45e" hs_bindgen_test_reparse_205f50942ba9b45e
  :: IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args2_ptr #-}

const_pointers_args2_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_205f50942ba9b45e

{-| __C declaration:__ @const_pointers_args3@

    __defined at:__ @reparse.h:210:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_39ad759f70b99e37" const_pointers_args3
  :: A
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
foreign import ccall unsafe "hs_bindgen_test_reparse_c2ddece5d773a650" hs_bindgen_test_reparse_c2ddece5d773a650
  :: IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args3_ptr #-}

const_pointers_args3_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_c2ddece5d773a650

{-| __C declaration:__ @const_pointers_args4@

    __defined at:__ @reparse.h:211:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_d4a5973dc0abcd97" const_pointers_args4
  :: A
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
foreign import ccall unsafe "hs_bindgen_test_reparse_ed9daaf3a67948dd" hs_bindgen_test_reparse_ed9daaf3a67948dd
  :: IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args4_ptr #-}

const_pointers_args4_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_ed9daaf3a67948dd

{-| __C declaration:__ @const_pointers_args5@

    __defined at:__ @reparse.h:212:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_1ed4abb42ab52f46" const_pointers_args5
  :: A
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
foreign import ccall unsafe "hs_bindgen_test_reparse_4b329c596dbf249e" hs_bindgen_test_reparse_4b329c596dbf249e
  :: IO (Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE const_pointers_args5_ptr #-}

const_pointers_args5_ptr :: Ptr.FunPtr (A -> (Ptr.Ptr FC.CInt) -> IO ())
const_pointers_args5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_4b329c596dbf249e

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @reparse.h:214:19@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_7d43de2d59af106e" const_pointers_ret1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret1@

    __defined at:__ @reparse.h:214:19@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_fb3f484345aeb2d2" hs_bindgen_test_reparse_fb3f484345aeb2d2
  :: IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret1_ptr #-}

const_pointers_ret1_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_fb3f484345aeb2d2

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @reparse.h:215:19@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_b7a5da1fec42008c" const_pointers_ret2
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret2@

    __defined at:__ @reparse.h:215:19@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_4242fece463aa185" hs_bindgen_test_reparse_4242fece463aa185
  :: IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret2_ptr #-}

const_pointers_ret2_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_4242fece463aa185

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @reparse.h:216:19@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_ea139108aca83634" const_pointers_ret3
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret3@

    __defined at:__ @reparse.h:216:19@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_d88b6e752a8081c3" hs_bindgen_test_reparse_d88b6e752a8081c3
  :: IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret3_ptr #-}

const_pointers_ret3_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_d88b6e752a8081c3

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @reparse.h:217:19@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_ab3c3cf6c30a91a4" const_pointers_ret4
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret4@

    __defined at:__ @reparse.h:217:19@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_ae606f6ad11597ce" hs_bindgen_test_reparse_ae606f6ad11597ce
  :: IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret4_ptr #-}

const_pointers_ret4_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_ae606f6ad11597ce

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @reparse.h:218:19@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_e32a2dec62135c7a" const_pointers_ret5
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.Ptr FC.CInt)

{-| __C declaration:__ @const_pointers_ret5@

    __defined at:__ @reparse.h:218:19@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_c86348709b464f9b" hs_bindgen_test_reparse_c86348709b464f9b
  :: IO (Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt)))

{-# NOINLINE const_pointers_ret5_ptr #-}

const_pointers_ret5_ptr :: Ptr.FunPtr (A -> IO (Ptr.Ptr FC.CInt))
const_pointers_ret5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_c86348709b464f9b

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

{-| __C declaration:__ @const_funptr1@

    __defined at:__ @reparse.h:238:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr1 = Const_funptr1
  { un_Const_funptr1 :: Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO A)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @const_funptr2@

    __defined at:__ @reparse.h:239:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr2 = Const_funptr2
  { un_Const_funptr2 :: Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO A)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @const_funptr3@

    __defined at:__ @reparse.h:240:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr3 = Const_funptr3
  { un_Const_funptr3 :: Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A))
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @const_funptr4@

    __defined at:__ @reparse.h:241:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr4 = Const_funptr4
  { un_Const_funptr4 :: Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A))
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @const_funptr5@

    __defined at:__ @reparse.h:242:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr5 = Const_funptr5
  { un_Const_funptr5 :: Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A))
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @const_funptr6@

    __defined at:__ @reparse.h:243:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr6 = Const_funptr6
  { un_Const_funptr6 :: Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A))
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @const_funptr7@

    __defined at:__ @reparse.h:244:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr7 = Const_funptr7
  { un_Const_funptr7 :: Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A))
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @reparse.h:246:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_02f253d2b51e801b" const_array_elem1_wrapper
  :: Ptr.Ptr A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

const_array_elem1 :: (HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ()
const_array_elem1 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    const_array_elem1_wrapper ptr1)

{-| __C declaration:__ @const_array_elem1@

    __defined at:__ @reparse.h:246:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_74accfb2c8dbad9b" hs_bindgen_test_reparse_74accfb2c8dbad9b
  :: IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ()))

{-# NOINLINE const_array_elem1_ptr #-}

const_array_elem1_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray A) -> IO ())
const_array_elem1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_74accfb2c8dbad9b

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @reparse.h:247:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_ae594e2b9ac5b1c3" const_array_elem2_wrapper
  :: Ptr.Ptr (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

const_array_elem2 :: (HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ()
const_array_elem2 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    const_array_elem2_wrapper ptr1)

{-| __C declaration:__ @const_array_elem2@

    __defined at:__ @reparse.h:247:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_e545f64064b17b40" hs_bindgen_test_reparse_e545f64064b17b40
  :: IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ()))

{-# NOINLINE const_array_elem2_ptr #-}

const_array_elem2_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ())
const_array_elem2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_e545f64064b17b40

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @reparse.h:248:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_2149e2b9eadf2ded" const_array_elem3_wrapper
  :: Ptr.Ptr (Ptr.Ptr A)
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO ()

const_array_elem3 :: (HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ()
const_array_elem3 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    const_array_elem3_wrapper ptr1)

{-| __C declaration:__ @const_array_elem3@

    __defined at:__ @reparse.h:248:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_9467829a46b5dfc3" hs_bindgen_test_reparse_9467829a46b5dfc3
  :: IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ()))

{-# NOINLINE const_array_elem3_ptr #-}

const_array_elem3_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)) -> IO ())
const_array_elem3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_9467829a46b5dfc3

foreign import ccall safe "hs_bindgen_test_reparse_14dbb45095c3c138" noParams1
  :: IO A

foreign import ccall unsafe "hs_bindgen_test_reparse_af69ae434bc46f7f" hs_bindgen_test_reparse_af69ae434bc46f7f
  :: IO (Ptr.FunPtr (IO A))

{-# NOINLINE noParams1_ptr #-}

noParams1_ptr :: Ptr.FunPtr (IO A)
noParams1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_af69ae434bc46f7f

{-| __C declaration:__ @noParams2@

    __defined at:__ @reparse.h:257:3@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_667f19a2bcb7ada6" noParams2
  :: IO A

{-| __C declaration:__ @noParams2@

    __defined at:__ @reparse.h:257:3@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_2f061e057ad050d2" hs_bindgen_test_reparse_2f061e057ad050d2
  :: IO (Ptr.FunPtr (IO A))

{-# NOINLINE noParams2_ptr #-}

noParams2_ptr :: Ptr.FunPtr (IO A)
noParams2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_2f061e057ad050d2

{-| __C declaration:__ @noParams3@

    __defined at:__ @reparse.h:258:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_34332de6ec849f2a" noParams3
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> Ptr.FunPtr (IO FC.CInt)
     {- ^ __C declaration:__ @arg2@
     -}
  -> IO ()

{-| __C declaration:__ @noParams3@

    __defined at:__ @reparse.h:258:6@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_a179b59c001d5529" hs_bindgen_test_reparse_a179b59c001d5529
  :: IO (Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ()))

{-# NOINLINE noParams3_ptr #-}

noParams3_ptr :: Ptr.FunPtr (A -> (Ptr.FunPtr (IO FC.CInt)) -> IO ())
noParams3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_a179b59c001d5529

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @reparse.h:262:8@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_7c42b6acbe3f8845" funptr_ret1
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (IO ()))

{-| __C declaration:__ @funptr_ret1@

    __defined at:__ @reparse.h:262:8@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_ae380911ef8bd65a" hs_bindgen_test_reparse_ae380911ef8bd65a
  :: IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO ()))))

{-# NOINLINE funptr_ret1_ptr #-}

funptr_ret1_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO ())))
funptr_ret1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_ae380911ef8bd65a

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @reparse.h:263:8@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_de4479feaa4c1b63" funptr_ret2
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (IO FC.CInt))

{-| __C declaration:__ @funptr_ret2@

    __defined at:__ @reparse.h:263:8@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_0e60d042fc021fa0" hs_bindgen_test_reparse_0e60d042fc021fa0
  :: IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO FC.CInt))))

{-# NOINLINE funptr_ret2_ptr #-}

funptr_ret2_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (IO FC.CInt)))
funptr_ret2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_0e60d042fc021fa0

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @reparse.h:264:8@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_8ca1c491149e7a82" funptr_ret3
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> IO ()))

{-| __C declaration:__ @funptr_ret3@

    __defined at:__ @reparse.h:264:8@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_ee8815fca40a6e75" hs_bindgen_test_reparse_ee8815fca40a6e75
  :: IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> IO ()))))

{-# NOINLINE funptr_ret3_ptr #-}

funptr_ret3_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> IO ())))
funptr_ret3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_ee8815fca40a6e75

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @reparse.h:265:8@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_28a86a376fb47aef" funptr_ret4
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar))

{-| __C declaration:__ @funptr_ret4@

    __defined at:__ @reparse.h:265:8@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_5d6841bf202a780e" hs_bindgen_test_reparse_5d6841bf202a780e
  :: IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar))))

{-# NOINLINE funptr_ret4_ptr #-}

funptr_ret4_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CChar)))
funptr_ret4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_5d6841bf202a780e

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @reparse.h:269:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_677dad7824f8f926" funptr_ret5
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret5@

    __defined at:__ @reparse.h:269:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_43e62fcd431c5993" hs_bindgen_test_reparse_43e62fcd431c5993
  :: IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret5_ptr #-}

funptr_ret5_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_43e62fcd431c5993

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @reparse.h:270:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_32eeedbdca340214" funptr_ret6
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret6@

    __defined at:__ @reparse.h:270:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_ae01ac1f8293cbcd" hs_bindgen_test_reparse_ae01ac1f8293cbcd
  :: IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret6_ptr #-}

funptr_ret6_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_ae01ac1f8293cbcd

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @reparse.h:271:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_6c9848f47db6c013" funptr_ret7
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret7@

    __defined at:__ @reparse.h:271:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_f6edab58ac911c39" hs_bindgen_test_reparse_f6edab58ac911c39
  :: IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret7_ptr #-}

funptr_ret7_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_f6edab58ac911c39

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @reparse.h:272:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_685ed1da05a496ec" funptr_ret8
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret8@

    __defined at:__ @reparse.h:272:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_e16f7f0071bb9009" hs_bindgen_test_reparse_e16f7f0071bb9009
  :: IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret8_ptr #-}

funptr_ret8_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_e16f7f0071bb9009

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @reparse.h:273:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_d838f03e8f8b0e9a" funptr_ret9
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret9@

    __defined at:__ @reparse.h:273:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_d7e96ea609f2a0d5" hs_bindgen_test_reparse_d7e96ea609f2a0d5
  :: IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret9_ptr #-}

funptr_ret9_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_d7e96ea609f2a0d5

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @reparse.h:274:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall safe "hs_bindgen_test_reparse_e44ba9f0163468d8" funptr_ret10
  :: A
     {- ^ __C declaration:__ @arg1@
     -}
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))

{-| __C declaration:__ @funptr_ret10@

    __defined at:__ @reparse.h:274:20@

    __exported by:__ @reparse.h@
-}
foreign import ccall unsafe "hs_bindgen_test_reparse_163513e7413372dd" hs_bindgen_test_reparse_163513e7413372dd
  :: IO (Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt)))))

{-# NOINLINE funptr_ret10_ptr #-}

funptr_ret10_ptr :: Ptr.FunPtr (A -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr FC.CInt))))
funptr_ret10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_reparse_163513e7413372dd

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
