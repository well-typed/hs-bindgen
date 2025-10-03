{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Minisat.Generated.Unsafe where

import Prelude (IO)

import Foreign.C qualified as FC
import GHC.Ptr qualified as Ptr

import HsBindgen.Runtime.Prelude qualified

import Minisat.Generated

$(HsBindgen.Runtime.Prelude.addCSource "#include </home/bolt/Desktop/Bolt/UMinho/Profissional/Well-Typed/Projects/hs-bindgen/examples/c-minisat/minisat-c-bindings/minisat.h>\nminisat_solver *hs_bindgen_d9598f1ade50f17a (void) { return minisat_new(); }\nvoid hs_bindgen_e38e3eed6052fcad (minisat_solver *arg1) { minisat_delete(arg1); }\nminisat_Var hs_bindgen_d08dcf6e4ac64013 (minisat_solver *arg1) { return minisat_newVar(arg1); }\nminisat_Lit hs_bindgen_70aa36ec6edab36e (minisat_solver *arg1) { return minisat_newLit(arg1); }\nminisat_Lit hs_bindgen_1e4b69255a200fba (minisat_Var arg1) { return minisat_mkLit(arg1); }\nminisat_Lit hs_bindgen_c122d48b276dc5fc (minisat_Var arg1, signed int arg2) { return minisat_mkLit_args(arg1, arg2); }\nminisat_Lit hs_bindgen_564e54985fc69722 (minisat_Lit arg1) { return minisat_negate(arg1); }\nminisat_Var hs_bindgen_fa779dcc5b5932bd (minisat_Lit arg1) { return minisat_var(arg1); }\nminisat_bool hs_bindgen_c54529ffe55babbb (minisat_Lit arg1) { return minisat_sign(arg1); }\nminisat_bool hs_bindgen_259b880c3092f824 (minisat_solver *arg1, signed int arg2, minisat_Lit *arg3) { return minisat_addClause(arg1, arg2, arg3); }\nvoid hs_bindgen_f7f8231105a8cff4 (minisat_solver *arg1) { minisat_addClause_begin(arg1); }\nvoid hs_bindgen_7a6da8baa35e5d5d (minisat_solver *arg1, minisat_Lit arg2) { minisat_addClause_addLit(arg1, arg2); }\nminisat_bool hs_bindgen_fc445843c7f80482 (minisat_solver *arg1) { return minisat_addClause_commit(arg1); }\nminisat_bool hs_bindgen_b82e40b1a33ba893 (minisat_solver *arg1) { return minisat_simplify(arg1); }\nminisat_bool hs_bindgen_b8f3764348f83f2e (minisat_solver *arg1, signed int arg2, minisat_Lit *arg3) { return minisat_solve(arg1, arg2, arg3); }\nminisat_lbool hs_bindgen_f2b0127e26913778 (minisat_solver *arg1, signed int arg2, minisat_Lit *arg3) { return minisat_limited_solve(arg1, arg2, arg3); }\nvoid hs_bindgen_a6dabf5cfec7232e (minisat_solver *arg1) { minisat_solve_begin(arg1); }\nvoid hs_bindgen_f4f2d37281866eae (minisat_solver *arg1, minisat_Lit arg2) { minisat_solve_addLit(arg1, arg2); }\nminisat_bool hs_bindgen_20693361e52bc9d6 (minisat_solver *arg1) { return minisat_solve_commit(arg1); }\nminisat_lbool hs_bindgen_b5bec2b07482e7f2 (minisat_solver *arg1) { return minisat_limited_solve_commit(arg1); }\nminisat_bool hs_bindgen_62c1dec217b672c6 (minisat_solver *arg1) { return minisat_okay(arg1); }\nvoid hs_bindgen_926d92230e1cb0f1 (minisat_solver *arg1, minisat_Var arg2, signed int arg3) { minisat_setPolarity(arg1, arg2, arg3); }\nvoid hs_bindgen_2fa492a4f7c3a2cf (minisat_solver *arg1, minisat_Var arg2, signed int arg3) { minisat_setDecisionVar(arg1, arg2, arg3); }\nminisat_lbool hs_bindgen_2868900700dc25a1 (void) { return minisat_get_l_True(); }\nminisat_lbool hs_bindgen_4889ca09c1fd170f (void) { return minisat_get_l_False(); }\nminisat_lbool hs_bindgen_aa12bba17770ff8f (void) { return minisat_get_l_Undef(); }\nminisat_lbool hs_bindgen_f4fa8000be0b801d (minisat_solver *arg1, minisat_Var arg2) { return minisat_value_Var(arg1, arg2); }\nminisat_lbool hs_bindgen_4c6639698694562e (minisat_solver *arg1, minisat_Lit arg2) { return minisat_value_Lit(arg1, arg2); }\nminisat_lbool hs_bindgen_60f65a1c16a84963 (minisat_solver *arg1, minisat_Var arg2) { return minisat_modelValue_Var(arg1, arg2); }\nminisat_lbool hs_bindgen_2ed92b6f1503de8e (minisat_solver *arg1, minisat_Lit arg2) { return minisat_modelValue_Lit(arg1, arg2); }\nsigned int hs_bindgen_01222d74d51d0c89 (minisat_solver *arg1) { return minisat_num_assigns(arg1); }\nsigned int hs_bindgen_c9f85406171043aa (minisat_solver *arg1) { return minisat_num_clauses(arg1); }\nsigned int hs_bindgen_27467416299b90f1 (minisat_solver *arg1) { return minisat_num_learnts(arg1); }\nsigned int hs_bindgen_7c0fc14d0cdbb4d0 (minisat_solver *arg1) { return minisat_num_vars(arg1); }\nsigned int hs_bindgen_eac6c70389dc81eb (minisat_solver *arg1) { return minisat_num_freeVars(arg1); }\nsigned int hs_bindgen_c7b45d262088f500 (minisat_solver *arg1) { return minisat_conflict_len(arg1); }\nminisat_Lit hs_bindgen_3c688a761b472956 (minisat_solver *arg1, signed int arg2) { return minisat_conflict_nthLit(arg1, arg2); }\nvoid hs_bindgen_943e06eb6f9a1a67 (minisat_solver *arg1, signed int arg2) { minisat_set_conf_budget(arg1, arg2); }\nvoid hs_bindgen_069ba879caabaaef (minisat_solver *arg1, signed int arg2) { minisat_set_prop_budget(arg1, arg2); }\nvoid hs_bindgen_580b7175713a9c1d (minisat_solver *arg1) { minisat_no_budget(arg1); }\nvoid hs_bindgen_f66072807a9bc80d (minisat_solver *arg1) { minisat_interrupt(arg1); }\nvoid hs_bindgen_c5381c21e634cc2b (minisat_solver *arg1) { minisat_clearInterrupt(arg1); }\nvoid hs_bindgen_ef86700a69c3a54d (minisat_solver *arg1, minisat_Var arg2, minisat_bool arg3) { minisat_setFrozen(arg1, arg2, arg3); }\nminisat_bool hs_bindgen_66515216e28cc304 (minisat_solver *arg1, minisat_Var arg2) { return minisat_isEliminated(arg1, arg2); }\nminisat_bool hs_bindgen_84223f9e3245172b (minisat_solver *arg1, minisat_bool arg2) { return minisat_eliminate(arg1, arg2); }\nvoid hs_bindgen_bc9fb24f599d513b (minisat_solver *arg1, signed int arg2) { minisat_set_verbosity(arg1, arg2); }\nsigned int hs_bindgen_673edbb1d2b5b2f4 (minisat_solver *arg1) { return minisat_num_conflicts(arg1); }\nsigned int hs_bindgen_efee69724d306178 (minisat_solver *arg1) { return minisat_num_decisions(arg1); }\nsigned int hs_bindgen_204fbdbe83570e78 (minisat_solver *arg1) { return minisat_num_restarts(arg1); }\nsigned int hs_bindgen_1586f5c7f7b66b32 (minisat_solver *arg1) { return minisat_num_propagations(arg1); }\n")

{-| __C declaration:__ @minisat_new@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:46:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_d9598f1ade50f17a" minisat_new
  :: IO (Ptr.Ptr Minisat_solver)

{-| __C declaration:__ @minisat_delete@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:47:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_e38e3eed6052fcad" minisat_delete
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_newVar@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:49:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_d08dcf6e4ac64013" minisat_newVar
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO Minisat_Var

{-| __C declaration:__ @minisat_newLit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:50:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_70aa36ec6edab36e" minisat_newLit
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO Minisat_Lit

{-| __C declaration:__ @minisat_mkLit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:52:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_1e4b69255a200fba" minisat_mkLit
  :: Minisat_Var
     {- ^ __C declaration:__ @x@
     -}
  -> IO Minisat_Lit

{-| __C declaration:__ @minisat_mkLit_args@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:53:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_c122d48b276dc5fc" minisat_mkLit_args
  :: Minisat_Var
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @sign@
     -}
  -> IO Minisat_Lit

{-| __C declaration:__ @minisat_negate@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:54:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_564e54985fc69722" minisat_negate
  :: Minisat_Lit
     {- ^ __C declaration:__ @p@
     -}
  -> IO Minisat_Lit

{-| __C declaration:__ @minisat_var@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:56:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_fa779dcc5b5932bd" minisat_var
  :: Minisat_Lit
     {- ^ __C declaration:__ @p@
     -}
  -> IO Minisat_Var

{-| __C declaration:__ @minisat_sign@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:57:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_c54529ffe55babbb" minisat_sign
  :: Minisat_Lit
     {- ^ __C declaration:__ @p@
     -}
  -> IO Minisat_bool

{-| __C declaration:__ @minisat_addClause@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:59:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_259b880c3092f824" minisat_addClause
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @len@
     -}
  -> Ptr.Ptr Minisat_Lit
     {- ^ __C declaration:__ @ps@
     -}
  -> IO Minisat_bool

{-| __C declaration:__ @minisat_addClause_begin@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:60:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_f7f8231105a8cff4" minisat_addClause_begin
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_addClause_addLit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:61:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_7a6da8baa35e5d5d" minisat_addClause_addLit
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> Minisat_Lit
     {- ^ __C declaration:__ @p@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_addClause_commit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:62:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_fc445843c7f80482" minisat_addClause_commit
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO Minisat_bool

{-| __C declaration:__ @minisat_simplify@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:64:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_b82e40b1a33ba893" minisat_simplify
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO Minisat_bool

{-| __C declaration:__ @minisat_solve@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:66:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_b8f3764348f83f2e" minisat_solve
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @len@
     -}
  -> Ptr.Ptr Minisat_Lit
     {- ^ __C declaration:__ @ps@
     -}
  -> IO Minisat_bool

{-| __C declaration:__ @minisat_limited_solve@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:67:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_f2b0127e26913778" minisat_limited_solve
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @len@
     -}
  -> Ptr.Ptr Minisat_Lit
     {- ^ __C declaration:__ @ps@
     -}
  -> IO Minisat_lbool

{-| __C declaration:__ @minisat_solve_begin@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:68:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_a6dabf5cfec7232e" minisat_solve_begin
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_solve_addLit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:69:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_f4f2d37281866eae" minisat_solve_addLit
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> Minisat_Lit
     {- ^ __C declaration:__ @p@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_solve_commit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:70:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_20693361e52bc9d6" minisat_solve_commit
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO Minisat_bool

{-| __C declaration:__ @minisat_limited_solve_commit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:71:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_b5bec2b07482e7f2" minisat_limited_solve_commit
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO Minisat_lbool

{-| __C declaration:__ @minisat_okay@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:74:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_62c1dec217b672c6" minisat_okay
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO Minisat_bool

{-| __C declaration:__ @minisat_setPolarity@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:76:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_926d92230e1cb0f1" minisat_setPolarity
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> Minisat_Var
     {- ^ __C declaration:__ @v@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @b@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_setDecisionVar@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:77:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_2fa492a4f7c3a2cf" minisat_setDecisionVar
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> Minisat_Var
     {- ^ __C declaration:__ @v@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @b@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_get_l_True@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:79:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_2868900700dc25a1" minisat_get_l_True
  :: IO Minisat_lbool

{-| __C declaration:__ @minisat_get_l_False@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:80:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_4889ca09c1fd170f" minisat_get_l_False
  :: IO Minisat_lbool

{-| __C declaration:__ @minisat_get_l_Undef@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:81:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_aa12bba17770ff8f" minisat_get_l_Undef
  :: IO Minisat_lbool

{-| __C declaration:__ @minisat_value_Var@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:83:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_f4fa8000be0b801d" minisat_value_Var
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> Minisat_Var
     {- ^ __C declaration:__ @x@
     -}
  -> IO Minisat_lbool

{-| __C declaration:__ @minisat_value_Lit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:84:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_4c6639698694562e" minisat_value_Lit
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> Minisat_Lit
     {- ^ __C declaration:__ @p@
     -}
  -> IO Minisat_lbool

{-| __C declaration:__ @minisat_modelValue_Var@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:85:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_60f65a1c16a84963" minisat_modelValue_Var
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> Minisat_Var
     {- ^ __C declaration:__ @x@
     -}
  -> IO Minisat_lbool

{-| __C declaration:__ @minisat_modelValue_Lit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:86:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_2ed92b6f1503de8e" minisat_modelValue_Lit
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> Minisat_Lit
     {- ^ __C declaration:__ @p@
     -}
  -> IO Minisat_lbool

{-| __C declaration:__ @minisat_num_assigns@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:88:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_01222d74d51d0c89" minisat_num_assigns
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @minisat_num_clauses@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:89:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_c9f85406171043aa" minisat_num_clauses
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @minisat_num_learnts@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:90:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_27467416299b90f1" minisat_num_learnts
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @minisat_num_vars@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:91:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_7c0fc14d0cdbb4d0" minisat_num_vars
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @minisat_num_freeVars@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:92:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_eac6c70389dc81eb" minisat_num_freeVars
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @minisat_conflict_len@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:94:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_c7b45d262088f500" minisat_conflict_len
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @minisat_conflict_nthLit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:95:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_3c688a761b472956" minisat_conflict_nthLit
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @i@
     -}
  -> IO Minisat_Lit

{-| __C declaration:__ @minisat_set_conf_budget@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:97:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_943e06eb6f9a1a67" minisat_set_conf_budget
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_set_prop_budget@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:98:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_069ba879caabaaef" minisat_set_prop_budget
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_no_budget@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:99:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_580b7175713a9c1d" minisat_no_budget
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_interrupt@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:102:6@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_f66072807a9bc80d" minisat_interrupt
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_clearInterrupt@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:103:6@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_c5381c21e634cc2b" minisat_clearInterrupt
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_setFrozen@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:106:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_ef86700a69c3a54d" minisat_setFrozen
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> Minisat_Var
     {- ^ __C declaration:__ @v@
     -}
  -> Minisat_bool
     {- ^ __C declaration:__ @b@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_isEliminated@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:107:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_66515216e28cc304" minisat_isEliminated
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> Minisat_Var
     {- ^ __C declaration:__ @v@
     -}
  -> IO Minisat_bool

{-| __C declaration:__ @minisat_eliminate@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:108:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_84223f9e3245172b" minisat_eliminate
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> Minisat_bool
     {- ^ __C declaration:__ @turn_off_elim@
     -}
  -> IO Minisat_bool

{-| __C declaration:__ @minisat_set_verbosity@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:112:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_bc9fb24f599d513b" minisat_set_verbosity
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @v@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_num_conflicts@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:116:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_673edbb1d2b5b2f4" minisat_num_conflicts
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @minisat_num_decisions@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:117:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_efee69724d306178" minisat_num_decisions
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @minisat_num_restarts@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:118:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_204fbdbe83570e78" minisat_num_restarts
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @minisat_num_propagations@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:119:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall unsafe "hs_bindgen_1586f5c7f7b66b32" minisat_num_propagations
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt
