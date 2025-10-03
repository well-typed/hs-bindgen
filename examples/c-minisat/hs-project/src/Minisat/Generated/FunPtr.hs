{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Minisat.Generated.FunPtr where

import Prelude (IO)

import Foreign.C qualified as FC
import GHC.IO.Unsafe qualified
import GHC.Ptr qualified as Ptr

import HsBindgen.Runtime.Prelude qualified

import Minisat.Generated

$(HsBindgen.Runtime.Prelude.addCSource "#include </home/bolt/Desktop/Bolt/UMinho/Profissional/Well-Typed/Projects/hs-bindgen/examples/c-minisat/minisat-c-bindings/minisat.h>\n/* get_minisat_new_ptr */ __attribute__ ((const)) minisat_solver *(*hs_bindgen_c7f7beadae89a91b (void)) (void) { return &minisat_new; } \n/* get_minisat_delete_ptr */ __attribute__ ((const)) void (*hs_bindgen_36f210a59d785d05 (void)) (minisat_solver *arg1) { return &minisat_delete; } \n/* get_minisat_newVar_ptr */ __attribute__ ((const)) minisat_Var (*hs_bindgen_c327a8ffd68f5bfc (void)) (minisat_solver *arg1) { return &minisat_newVar; } \n/* get_minisat_newLit_ptr */ __attribute__ ((const)) minisat_Lit (*hs_bindgen_4099199081576ac7 (void)) (minisat_solver *arg1) { return &minisat_newLit; } \n/* get_minisat_mkLit_ptr */ __attribute__ ((const)) minisat_Lit (*hs_bindgen_374c425b777d42b7 (void)) (minisat_Var arg1) { return &minisat_mkLit; } \n/* get_minisat_mkLit_args_ptr */ __attribute__ ((const)) minisat_Lit (*hs_bindgen_39feacf51f5fcad9 (void)) (minisat_Var arg1, signed int arg2) { return &minisat_mkLit_args; } \n/* get_minisat_negate_ptr */ __attribute__ ((const)) minisat_Lit (*hs_bindgen_7c2b093060501a5b (void)) (minisat_Lit arg1) { return &minisat_negate; } \n/* get_minisat_var_ptr */ __attribute__ ((const)) minisat_Var (*hs_bindgen_e7269b32df239a35 (void)) (minisat_Lit arg1) { return &minisat_var; } \n/* get_minisat_sign_ptr */ __attribute__ ((const)) minisat_bool (*hs_bindgen_d7898db10c8a5511 (void)) (minisat_Lit arg1) { return &minisat_sign; } \n/* get_minisat_addClause_ptr */ __attribute__ ((const)) minisat_bool (*hs_bindgen_a30c94f76a0d8f56 (void)) (minisat_solver *arg1, signed int arg2, minisat_Lit *arg3) { return &minisat_addClause; } \n/* get_minisat_addClause_begin_ptr */ __attribute__ ((const)) void (*hs_bindgen_772a868b2aa11eb4 (void)) (minisat_solver *arg1) { return &minisat_addClause_begin; } \n/* get_minisat_addClause_addLit_ptr */ __attribute__ ((const)) void (*hs_bindgen_a33be719c90f553c (void)) (minisat_solver *arg1, minisat_Lit arg2) { return &minisat_addClause_addLit; } \n/* get_minisat_addClause_commit_ptr */ __attribute__ ((const)) minisat_bool (*hs_bindgen_c0bddec9c322ce8d (void)) (minisat_solver *arg1) { return &minisat_addClause_commit; } \n/* get_minisat_simplify_ptr */ __attribute__ ((const)) minisat_bool (*hs_bindgen_dcec5d715495d1eb (void)) (minisat_solver *arg1) { return &minisat_simplify; } \n/* get_minisat_solve_ptr */ __attribute__ ((const)) minisat_bool (*hs_bindgen_5254c67d8df71eee (void)) (minisat_solver *arg1, signed int arg2, minisat_Lit *arg3) { return &minisat_solve; } \n/* get_minisat_limited_solve_ptr */ __attribute__ ((const)) minisat_lbool (*hs_bindgen_4cce3c2d31ee6799 (void)) (minisat_solver *arg1, signed int arg2, minisat_Lit *arg3) { return &minisat_limited_solve; } \n/* get_minisat_solve_begin_ptr */ __attribute__ ((const)) void (*hs_bindgen_64cceafe5e94910e (void)) (minisat_solver *arg1) { return &minisat_solve_begin; } \n/* get_minisat_solve_addLit_ptr */ __attribute__ ((const)) void (*hs_bindgen_5fee18416bc6b045 (void)) (minisat_solver *arg1, minisat_Lit arg2) { return &minisat_solve_addLit; } \n/* get_minisat_solve_commit_ptr */ __attribute__ ((const)) minisat_bool (*hs_bindgen_b066b8eb6d0a8e24 (void)) (minisat_solver *arg1) { return &minisat_solve_commit; } \n/* get_minisat_limited_solve_commit_ptr */ __attribute__ ((const)) minisat_lbool (*hs_bindgen_dbfaa5257475a75c (void)) (minisat_solver *arg1) { return &minisat_limited_solve_commit; } \n/* get_minisat_okay_ptr */ __attribute__ ((const)) minisat_bool (*hs_bindgen_6b8b28ab6e35428c (void)) (minisat_solver *arg1) { return &minisat_okay; } \n/* get_minisat_setPolarity_ptr */ __attribute__ ((const)) void (*hs_bindgen_7cd6dff7a1dad628 (void)) (minisat_solver *arg1, minisat_Var arg2, signed int arg3) { return &minisat_setPolarity; } \n/* get_minisat_setDecisionVar_ptr */ __attribute__ ((const)) void (*hs_bindgen_139816e5c127e320 (void)) (minisat_solver *arg1, minisat_Var arg2, signed int arg3) { return &minisat_setDecisionVar; } \n/* get_minisat_get_l_True_ptr */ __attribute__ ((const)) minisat_lbool (*hs_bindgen_2f0602019f602d63 (void)) (void) { return &minisat_get_l_True; } \n/* get_minisat_get_l_False_ptr */ __attribute__ ((const)) minisat_lbool (*hs_bindgen_dc0d38e352b90679 (void)) (void) { return &minisat_get_l_False; } \n/* get_minisat_get_l_Undef_ptr */ __attribute__ ((const)) minisat_lbool (*hs_bindgen_5551c93c61be9c0e (void)) (void) { return &minisat_get_l_Undef; } \n/* get_minisat_value_Var_ptr */ __attribute__ ((const)) minisat_lbool (*hs_bindgen_4450939cd0a4daff (void)) (minisat_solver *arg1, minisat_Var arg2) { return &minisat_value_Var; } \n/* get_minisat_value_Lit_ptr */ __attribute__ ((const)) minisat_lbool (*hs_bindgen_60e39511cc1939cc (void)) (minisat_solver *arg1, minisat_Lit arg2) { return &minisat_value_Lit; } \n/* get_minisat_modelValue_Var_ptr */ __attribute__ ((const)) minisat_lbool (*hs_bindgen_aaf498d9dbcb4171 (void)) (minisat_solver *arg1, minisat_Var arg2) { return &minisat_modelValue_Var; } \n/* get_minisat_modelValue_Lit_ptr */ __attribute__ ((const)) minisat_lbool (*hs_bindgen_4f94a8f34824f9cc (void)) (minisat_solver *arg1, minisat_Lit arg2) { return &minisat_modelValue_Lit; } \n/* get_minisat_num_assigns_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_178afcd464fac263 (void)) (minisat_solver *arg1) { return &minisat_num_assigns; } \n/* get_minisat_num_clauses_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_a1efb9447df69bac (void)) (minisat_solver *arg1) { return &minisat_num_clauses; } \n/* get_minisat_num_learnts_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_6cc40c40c0290405 (void)) (minisat_solver *arg1) { return &minisat_num_learnts; } \n/* get_minisat_num_vars_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_f6d1006d0adf0e81 (void)) (minisat_solver *arg1) { return &minisat_num_vars; } \n/* get_minisat_num_freeVars_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_d0310a430596b54a (void)) (minisat_solver *arg1) { return &minisat_num_freeVars; } \n/* get_minisat_conflict_len_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_777816c2da8702e3 (void)) (minisat_solver *arg1) { return &minisat_conflict_len; } \n/* get_minisat_conflict_nthLit_ptr */ __attribute__ ((const)) minisat_Lit (*hs_bindgen_8db1a92a10a7762c (void)) (minisat_solver *arg1, signed int arg2) { return &minisat_conflict_nthLit; } \n/* get_minisat_set_conf_budget_ptr */ __attribute__ ((const)) void (*hs_bindgen_4e9ecf7947b71c8d (void)) (minisat_solver *arg1, signed int arg2) { return &minisat_set_conf_budget; } \n/* get_minisat_set_prop_budget_ptr */ __attribute__ ((const)) void (*hs_bindgen_b3afdcd3d537c571 (void)) (minisat_solver *arg1, signed int arg2) { return &minisat_set_prop_budget; } \n/* get_minisat_no_budget_ptr */ __attribute__ ((const)) void (*hs_bindgen_d7966e5a6ae9d123 (void)) (minisat_solver *arg1) { return &minisat_no_budget; } \n/* get_minisat_interrupt_ptr */ __attribute__ ((const)) void (*hs_bindgen_a73e6daffe590639 (void)) (minisat_solver *arg1) { return &minisat_interrupt; } \n/* get_minisat_clearInterrupt_ptr */ __attribute__ ((const)) void (*hs_bindgen_70859c92773f01bf (void)) (minisat_solver *arg1) { return &minisat_clearInterrupt; } \n/* get_minisat_setFrozen_ptr */ __attribute__ ((const)) void (*hs_bindgen_60a54743957ad3ff (void)) (minisat_solver *arg1, minisat_Var arg2, minisat_bool arg3) { return &minisat_setFrozen; } \n/* get_minisat_isEliminated_ptr */ __attribute__ ((const)) minisat_bool (*hs_bindgen_dca8d0faa4ba6832 (void)) (minisat_solver *arg1, minisat_Var arg2) { return &minisat_isEliminated; } \n/* get_minisat_eliminate_ptr */ __attribute__ ((const)) minisat_bool (*hs_bindgen_658b466efae6659b (void)) (minisat_solver *arg1, minisat_bool arg2) { return &minisat_eliminate; } \n/* get_minisat_set_verbosity_ptr */ __attribute__ ((const)) void (*hs_bindgen_84a583a32d3cd2a0 (void)) (minisat_solver *arg1, signed int arg2) { return &minisat_set_verbosity; } \n/* get_minisat_num_conflicts_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_8ba7c25248f20592 (void)) (minisat_solver *arg1) { return &minisat_num_conflicts; } \n/* get_minisat_num_decisions_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_c171def294a2e7ac (void)) (minisat_solver *arg1) { return &minisat_num_decisions; } \n/* get_minisat_num_restarts_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_4a055a6d4906c317 (void)) (minisat_solver *arg1) { return &minisat_num_restarts; } \n/* get_minisat_num_propagations_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_173a53fea73317c5 (void)) (minisat_solver *arg1) { return &minisat_num_propagations; } \n")

foreign import ccall unsafe "hs_bindgen_c7f7beadae89a91b" hs_bindgen_c7f7beadae89a91b
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr Minisat_solver)))

{-# NOINLINE minisat_new_ptr #-}

{-| __C declaration:__ @minisat_new@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:46:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_new_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Minisat_solver))
minisat_new_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c7f7beadae89a91b

foreign import ccall unsafe "hs_bindgen_36f210a59d785d05" hs_bindgen_36f210a59d785d05
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO ()))

{-# NOINLINE minisat_delete_ptr #-}

{-| __C declaration:__ @minisat_delete@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:47:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_delete_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO ())
minisat_delete_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_36f210a59d785d05

foreign import ccall unsafe "hs_bindgen_c327a8ffd68f5bfc" hs_bindgen_c327a8ffd68f5bfc
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO Minisat_Var))

{-# NOINLINE minisat_newVar_ptr #-}

{-| __C declaration:__ @minisat_newVar@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:49:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_newVar_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO Minisat_Var)
minisat_newVar_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c327a8ffd68f5bfc

foreign import ccall unsafe "hs_bindgen_4099199081576ac7" hs_bindgen_4099199081576ac7
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO Minisat_Lit))

{-# NOINLINE minisat_newLit_ptr #-}

{-| __C declaration:__ @minisat_newLit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:50:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_newLit_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO Minisat_Lit)
minisat_newLit_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4099199081576ac7

foreign import ccall unsafe "hs_bindgen_374c425b777d42b7" hs_bindgen_374c425b777d42b7
  :: IO (Ptr.FunPtr (Minisat_Var -> IO Minisat_Lit))

{-# NOINLINE minisat_mkLit_ptr #-}

{-| __C declaration:__ @minisat_mkLit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:52:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_mkLit_ptr :: Ptr.FunPtr (Minisat_Var -> IO Minisat_Lit)
minisat_mkLit_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_374c425b777d42b7

foreign import ccall unsafe "hs_bindgen_39feacf51f5fcad9" hs_bindgen_39feacf51f5fcad9
  :: IO (Ptr.FunPtr (Minisat_Var -> FC.CInt -> IO Minisat_Lit))

{-# NOINLINE minisat_mkLit_args_ptr #-}

{-| __C declaration:__ @minisat_mkLit_args@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:53:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_mkLit_args_ptr :: Ptr.FunPtr (Minisat_Var -> FC.CInt -> IO Minisat_Lit)
minisat_mkLit_args_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_39feacf51f5fcad9

foreign import ccall unsafe "hs_bindgen_7c2b093060501a5b" hs_bindgen_7c2b093060501a5b
  :: IO (Ptr.FunPtr (Minisat_Lit -> IO Minisat_Lit))

{-# NOINLINE minisat_negate_ptr #-}

{-| __C declaration:__ @minisat_negate@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:54:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_negate_ptr :: Ptr.FunPtr (Minisat_Lit -> IO Minisat_Lit)
minisat_negate_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7c2b093060501a5b

foreign import ccall unsafe "hs_bindgen_e7269b32df239a35" hs_bindgen_e7269b32df239a35
  :: IO (Ptr.FunPtr (Minisat_Lit -> IO Minisat_Var))

{-# NOINLINE minisat_var_ptr #-}

{-| __C declaration:__ @minisat_var@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:56:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_var_ptr :: Ptr.FunPtr (Minisat_Lit -> IO Minisat_Var)
minisat_var_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e7269b32df239a35

foreign import ccall unsafe "hs_bindgen_d7898db10c8a5511" hs_bindgen_d7898db10c8a5511
  :: IO (Ptr.FunPtr (Minisat_Lit -> IO Minisat_bool))

{-# NOINLINE minisat_sign_ptr #-}

{-| __C declaration:__ @minisat_sign@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:57:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_sign_ptr :: Ptr.FunPtr (Minisat_Lit -> IO Minisat_bool)
minisat_sign_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d7898db10c8a5511

foreign import ccall unsafe "hs_bindgen_a30c94f76a0d8f56" hs_bindgen_a30c94f76a0d8f56
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> FC.CInt -> (Ptr.Ptr Minisat_Lit) -> IO Minisat_bool))

{-# NOINLINE minisat_addClause_ptr #-}

{-| __C declaration:__ @minisat_addClause@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:59:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_addClause_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> FC.CInt -> (Ptr.Ptr Minisat_Lit) -> IO Minisat_bool)
minisat_addClause_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a30c94f76a0d8f56

foreign import ccall unsafe "hs_bindgen_772a868b2aa11eb4" hs_bindgen_772a868b2aa11eb4
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO ()))

{-# NOINLINE minisat_addClause_begin_ptr #-}

{-| __C declaration:__ @minisat_addClause_begin@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:60:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_addClause_begin_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO ())
minisat_addClause_begin_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_772a868b2aa11eb4

foreign import ccall unsafe "hs_bindgen_a33be719c90f553c" hs_bindgen_a33be719c90f553c
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Lit -> IO ()))

{-# NOINLINE minisat_addClause_addLit_ptr #-}

{-| __C declaration:__ @minisat_addClause_addLit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:61:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_addClause_addLit_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Lit -> IO ())
minisat_addClause_addLit_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a33be719c90f553c

foreign import ccall unsafe "hs_bindgen_c0bddec9c322ce8d" hs_bindgen_c0bddec9c322ce8d
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO Minisat_bool))

{-# NOINLINE minisat_addClause_commit_ptr #-}

{-| __C declaration:__ @minisat_addClause_commit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:62:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_addClause_commit_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO Minisat_bool)
minisat_addClause_commit_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c0bddec9c322ce8d

foreign import ccall unsafe "hs_bindgen_dcec5d715495d1eb" hs_bindgen_dcec5d715495d1eb
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO Minisat_bool))

{-# NOINLINE minisat_simplify_ptr #-}

{-| __C declaration:__ @minisat_simplify@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:64:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_simplify_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO Minisat_bool)
minisat_simplify_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dcec5d715495d1eb

foreign import ccall unsafe "hs_bindgen_5254c67d8df71eee" hs_bindgen_5254c67d8df71eee
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> FC.CInt -> (Ptr.Ptr Minisat_Lit) -> IO Minisat_bool))

{-# NOINLINE minisat_solve_ptr #-}

{-| __C declaration:__ @minisat_solve@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:66:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_solve_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> FC.CInt -> (Ptr.Ptr Minisat_Lit) -> IO Minisat_bool)
minisat_solve_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5254c67d8df71eee

foreign import ccall unsafe "hs_bindgen_4cce3c2d31ee6799" hs_bindgen_4cce3c2d31ee6799
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> FC.CInt -> (Ptr.Ptr Minisat_Lit) -> IO Minisat_lbool))

{-# NOINLINE minisat_limited_solve_ptr #-}

{-| __C declaration:__ @minisat_limited_solve@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:67:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_limited_solve_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> FC.CInt -> (Ptr.Ptr Minisat_Lit) -> IO Minisat_lbool)
minisat_limited_solve_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4cce3c2d31ee6799

foreign import ccall unsafe "hs_bindgen_64cceafe5e94910e" hs_bindgen_64cceafe5e94910e
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO ()))

{-# NOINLINE minisat_solve_begin_ptr #-}

{-| __C declaration:__ @minisat_solve_begin@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:68:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_solve_begin_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO ())
minisat_solve_begin_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_64cceafe5e94910e

foreign import ccall unsafe "hs_bindgen_5fee18416bc6b045" hs_bindgen_5fee18416bc6b045
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Lit -> IO ()))

{-# NOINLINE minisat_solve_addLit_ptr #-}

{-| __C declaration:__ @minisat_solve_addLit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:69:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_solve_addLit_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Lit -> IO ())
minisat_solve_addLit_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5fee18416bc6b045

foreign import ccall unsafe "hs_bindgen_b066b8eb6d0a8e24" hs_bindgen_b066b8eb6d0a8e24
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO Minisat_bool))

{-# NOINLINE minisat_solve_commit_ptr #-}

{-| __C declaration:__ @minisat_solve_commit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:70:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_solve_commit_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO Minisat_bool)
minisat_solve_commit_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b066b8eb6d0a8e24

foreign import ccall unsafe "hs_bindgen_dbfaa5257475a75c" hs_bindgen_dbfaa5257475a75c
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO Minisat_lbool))

{-# NOINLINE minisat_limited_solve_commit_ptr #-}

{-| __C declaration:__ @minisat_limited_solve_commit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:71:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_limited_solve_commit_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO Minisat_lbool)
minisat_limited_solve_commit_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dbfaa5257475a75c

foreign import ccall unsafe "hs_bindgen_6b8b28ab6e35428c" hs_bindgen_6b8b28ab6e35428c
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO Minisat_bool))

{-# NOINLINE minisat_okay_ptr #-}

{-| __C declaration:__ @minisat_okay@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:74:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_okay_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO Minisat_bool)
minisat_okay_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6b8b28ab6e35428c

foreign import ccall unsafe "hs_bindgen_7cd6dff7a1dad628" hs_bindgen_7cd6dff7a1dad628
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Var -> FC.CInt -> IO ()))

{-# NOINLINE minisat_setPolarity_ptr #-}

{-| __C declaration:__ @minisat_setPolarity@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:76:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_setPolarity_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Var -> FC.CInt -> IO ())
minisat_setPolarity_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7cd6dff7a1dad628

foreign import ccall unsafe "hs_bindgen_139816e5c127e320" hs_bindgen_139816e5c127e320
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Var -> FC.CInt -> IO ()))

{-# NOINLINE minisat_setDecisionVar_ptr #-}

{-| __C declaration:__ @minisat_setDecisionVar@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:77:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_setDecisionVar_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Var -> FC.CInt -> IO ())
minisat_setDecisionVar_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_139816e5c127e320

foreign import ccall unsafe "hs_bindgen_2f0602019f602d63" hs_bindgen_2f0602019f602d63
  :: IO (Ptr.FunPtr (IO Minisat_lbool))

{-# NOINLINE minisat_get_l_True_ptr #-}

{-| __C declaration:__ @minisat_get_l_True@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:79:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_get_l_True_ptr :: Ptr.FunPtr (IO Minisat_lbool)
minisat_get_l_True_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2f0602019f602d63

foreign import ccall unsafe "hs_bindgen_dc0d38e352b90679" hs_bindgen_dc0d38e352b90679
  :: IO (Ptr.FunPtr (IO Minisat_lbool))

{-# NOINLINE minisat_get_l_False_ptr #-}

{-| __C declaration:__ @minisat_get_l_False@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:80:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_get_l_False_ptr :: Ptr.FunPtr (IO Minisat_lbool)
minisat_get_l_False_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dc0d38e352b90679

foreign import ccall unsafe "hs_bindgen_5551c93c61be9c0e" hs_bindgen_5551c93c61be9c0e
  :: IO (Ptr.FunPtr (IO Minisat_lbool))

{-# NOINLINE minisat_get_l_Undef_ptr #-}

{-| __C declaration:__ @minisat_get_l_Undef@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:81:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_get_l_Undef_ptr :: Ptr.FunPtr (IO Minisat_lbool)
minisat_get_l_Undef_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5551c93c61be9c0e

foreign import ccall unsafe "hs_bindgen_4450939cd0a4daff" hs_bindgen_4450939cd0a4daff
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Var -> IO Minisat_lbool))

{-# NOINLINE minisat_value_Var_ptr #-}

{-| __C declaration:__ @minisat_value_Var@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:83:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_value_Var_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Var -> IO Minisat_lbool)
minisat_value_Var_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4450939cd0a4daff

foreign import ccall unsafe "hs_bindgen_60e39511cc1939cc" hs_bindgen_60e39511cc1939cc
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Lit -> IO Minisat_lbool))

{-# NOINLINE minisat_value_Lit_ptr #-}

{-| __C declaration:__ @minisat_value_Lit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:84:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_value_Lit_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Lit -> IO Minisat_lbool)
minisat_value_Lit_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_60e39511cc1939cc

foreign import ccall unsafe "hs_bindgen_aaf498d9dbcb4171" hs_bindgen_aaf498d9dbcb4171
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Var -> IO Minisat_lbool))

{-# NOINLINE minisat_modelValue_Var_ptr #-}

{-| __C declaration:__ @minisat_modelValue_Var@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:85:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_modelValue_Var_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Var -> IO Minisat_lbool)
minisat_modelValue_Var_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_aaf498d9dbcb4171

foreign import ccall unsafe "hs_bindgen_4f94a8f34824f9cc" hs_bindgen_4f94a8f34824f9cc
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Lit -> IO Minisat_lbool))

{-# NOINLINE minisat_modelValue_Lit_ptr #-}

{-| __C declaration:__ @minisat_modelValue_Lit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:86:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_modelValue_Lit_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Lit -> IO Minisat_lbool)
minisat_modelValue_Lit_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4f94a8f34824f9cc

foreign import ccall unsafe "hs_bindgen_178afcd464fac263" hs_bindgen_178afcd464fac263
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt))

{-# NOINLINE minisat_num_assigns_ptr #-}

{-| __C declaration:__ @minisat_num_assigns@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:88:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_num_assigns_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt)
minisat_num_assigns_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_178afcd464fac263

foreign import ccall unsafe "hs_bindgen_a1efb9447df69bac" hs_bindgen_a1efb9447df69bac
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt))

{-# NOINLINE minisat_num_clauses_ptr #-}

{-| __C declaration:__ @minisat_num_clauses@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:89:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_num_clauses_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt)
minisat_num_clauses_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a1efb9447df69bac

foreign import ccall unsafe "hs_bindgen_6cc40c40c0290405" hs_bindgen_6cc40c40c0290405
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt))

{-# NOINLINE minisat_num_learnts_ptr #-}

{-| __C declaration:__ @minisat_num_learnts@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:90:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_num_learnts_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt)
minisat_num_learnts_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6cc40c40c0290405

foreign import ccall unsafe "hs_bindgen_f6d1006d0adf0e81" hs_bindgen_f6d1006d0adf0e81
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt))

{-# NOINLINE minisat_num_vars_ptr #-}

{-| __C declaration:__ @minisat_num_vars@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:91:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_num_vars_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt)
minisat_num_vars_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f6d1006d0adf0e81

foreign import ccall unsafe "hs_bindgen_d0310a430596b54a" hs_bindgen_d0310a430596b54a
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt))

{-# NOINLINE minisat_num_freeVars_ptr #-}

{-| __C declaration:__ @minisat_num_freeVars@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:92:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_num_freeVars_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt)
minisat_num_freeVars_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d0310a430596b54a

foreign import ccall unsafe "hs_bindgen_777816c2da8702e3" hs_bindgen_777816c2da8702e3
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt))

{-# NOINLINE minisat_conflict_len_ptr #-}

{-| __C declaration:__ @minisat_conflict_len@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:94:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_conflict_len_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt)
minisat_conflict_len_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_777816c2da8702e3

foreign import ccall unsafe "hs_bindgen_8db1a92a10a7762c" hs_bindgen_8db1a92a10a7762c
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> FC.CInt -> IO Minisat_Lit))

{-# NOINLINE minisat_conflict_nthLit_ptr #-}

{-| __C declaration:__ @minisat_conflict_nthLit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:95:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_conflict_nthLit_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> FC.CInt -> IO Minisat_Lit)
minisat_conflict_nthLit_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8db1a92a10a7762c

foreign import ccall unsafe "hs_bindgen_4e9ecf7947b71c8d" hs_bindgen_4e9ecf7947b71c8d
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> FC.CInt -> IO ()))

{-# NOINLINE minisat_set_conf_budget_ptr #-}

{-| __C declaration:__ @minisat_set_conf_budget@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:97:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_set_conf_budget_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> FC.CInt -> IO ())
minisat_set_conf_budget_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4e9ecf7947b71c8d

foreign import ccall unsafe "hs_bindgen_b3afdcd3d537c571" hs_bindgen_b3afdcd3d537c571
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> FC.CInt -> IO ()))

{-# NOINLINE minisat_set_prop_budget_ptr #-}

{-| __C declaration:__ @minisat_set_prop_budget@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:98:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_set_prop_budget_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> FC.CInt -> IO ())
minisat_set_prop_budget_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b3afdcd3d537c571

foreign import ccall unsafe "hs_bindgen_d7966e5a6ae9d123" hs_bindgen_d7966e5a6ae9d123
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO ()))

{-# NOINLINE minisat_no_budget_ptr #-}

{-| __C declaration:__ @minisat_no_budget@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:99:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_no_budget_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO ())
minisat_no_budget_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d7966e5a6ae9d123

foreign import ccall unsafe "hs_bindgen_a73e6daffe590639" hs_bindgen_a73e6daffe590639
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO ()))

{-# NOINLINE minisat_interrupt_ptr #-}

{-| __C declaration:__ @minisat_interrupt@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:102:6@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_interrupt_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO ())
minisat_interrupt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a73e6daffe590639

foreign import ccall unsafe "hs_bindgen_70859c92773f01bf" hs_bindgen_70859c92773f01bf
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO ()))

{-# NOINLINE minisat_clearInterrupt_ptr #-}

{-| __C declaration:__ @minisat_clearInterrupt@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:103:6@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_clearInterrupt_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO ())
minisat_clearInterrupt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_70859c92773f01bf

foreign import ccall unsafe "hs_bindgen_60a54743957ad3ff" hs_bindgen_60a54743957ad3ff
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Var -> Minisat_bool -> IO ()))

{-# NOINLINE minisat_setFrozen_ptr #-}

{-| __C declaration:__ @minisat_setFrozen@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:106:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_setFrozen_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Var -> Minisat_bool -> IO ())
minisat_setFrozen_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_60a54743957ad3ff

foreign import ccall unsafe "hs_bindgen_dca8d0faa4ba6832" hs_bindgen_dca8d0faa4ba6832
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Var -> IO Minisat_bool))

{-# NOINLINE minisat_isEliminated_ptr #-}

{-| __C declaration:__ @minisat_isEliminated@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:107:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_isEliminated_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_Var -> IO Minisat_bool)
minisat_isEliminated_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dca8d0faa4ba6832

foreign import ccall unsafe "hs_bindgen_658b466efae6659b" hs_bindgen_658b466efae6659b
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_bool -> IO Minisat_bool))

{-# NOINLINE minisat_eliminate_ptr #-}

{-| __C declaration:__ @minisat_eliminate@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:108:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_eliminate_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> Minisat_bool -> IO Minisat_bool)
minisat_eliminate_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_658b466efae6659b

foreign import ccall unsafe "hs_bindgen_84a583a32d3cd2a0" hs_bindgen_84a583a32d3cd2a0
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> FC.CInt -> IO ()))

{-# NOINLINE minisat_set_verbosity_ptr #-}

{-| __C declaration:__ @minisat_set_verbosity@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:112:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_set_verbosity_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> FC.CInt -> IO ())
minisat_set_verbosity_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_84a583a32d3cd2a0

foreign import ccall unsafe "hs_bindgen_8ba7c25248f20592" hs_bindgen_8ba7c25248f20592
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt))

{-# NOINLINE minisat_num_conflicts_ptr #-}

{-| __C declaration:__ @minisat_num_conflicts@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:116:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_num_conflicts_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt)
minisat_num_conflicts_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8ba7c25248f20592

foreign import ccall unsafe "hs_bindgen_c171def294a2e7ac" hs_bindgen_c171def294a2e7ac
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt))

{-# NOINLINE minisat_num_decisions_ptr #-}

{-| __C declaration:__ @minisat_num_decisions@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:117:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_num_decisions_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt)
minisat_num_decisions_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c171def294a2e7ac

foreign import ccall unsafe "hs_bindgen_4a055a6d4906c317" hs_bindgen_4a055a6d4906c317
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt))

{-# NOINLINE minisat_num_restarts_ptr #-}

{-| __C declaration:__ @minisat_num_restarts@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:118:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_num_restarts_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt)
minisat_num_restarts_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4a055a6d4906c317

foreign import ccall unsafe "hs_bindgen_173a53fea73317c5" hs_bindgen_173a53fea73317c5
  :: IO (Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt))

{-# NOINLINE minisat_num_propagations_ptr #-}

{-| __C declaration:__ @minisat_num_propagations@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:119:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
minisat_num_propagations_ptr :: Ptr.FunPtr ((Ptr.Ptr Minisat_solver) -> IO FC.CInt)
minisat_num_propagations_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_173a53fea73317c5
