{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Minisat.Generated.Safe where

import Prelude (IO)

import Foreign.C qualified as FC
import GHC.Ptr qualified as Ptr

import HsBindgen.Runtime.Prelude qualified

import Minisat.Generated

$(HsBindgen.Runtime.Prelude.addCSource "#include </home/bolt/Desktop/Bolt/UMinho/Profissional/Well-Typed/Projects/hs-bindgen/examples/c-minisat/minisat-c-bindings/minisat.h>\nminisat_solver *hs_bindgen_9a5547ce0a57fb8a (void) { return minisat_new(); }\nvoid hs_bindgen_2744d30ad95405d6 (minisat_solver *arg1) { minisat_delete(arg1); }\nminisat_Var hs_bindgen_010517bb169afb37 (minisat_solver *arg1) { return minisat_newVar(arg1); }\nminisat_Lit hs_bindgen_b59bdab42e53ab79 (minisat_solver *arg1) { return minisat_newLit(arg1); }\nminisat_Lit hs_bindgen_fd2ef71f0a3d4fe5 (minisat_Var arg1) { return minisat_mkLit(arg1); }\nminisat_Lit hs_bindgen_2b0b8c73a5d10a9f (minisat_Var arg1, signed int arg2) { return minisat_mkLit_args(arg1, arg2); }\nminisat_Lit hs_bindgen_5d1abd9af0a77f5a (minisat_Lit arg1) { return minisat_negate(arg1); }\nminisat_Var hs_bindgen_b51f9b92d9c7b45e (minisat_Lit arg1) { return minisat_var(arg1); }\nminisat_bool hs_bindgen_3b56deef4d8d34ac (minisat_Lit arg1) { return minisat_sign(arg1); }\nminisat_bool hs_bindgen_915733c72f129fdf (minisat_solver *arg1, signed int arg2, minisat_Lit *arg3) { return minisat_addClause(arg1, arg2, arg3); }\nvoid hs_bindgen_cacf08ceeaaa5475 (minisat_solver *arg1) { minisat_addClause_begin(arg1); }\nvoid hs_bindgen_0618846d301db6ba (minisat_solver *arg1, minisat_Lit arg2) { minisat_addClause_addLit(arg1, arg2); }\nminisat_bool hs_bindgen_57ce1f67b003c061 (minisat_solver *arg1) { return minisat_addClause_commit(arg1); }\nminisat_bool hs_bindgen_62fc51e9cffd2941 (minisat_solver *arg1) { return minisat_simplify(arg1); }\nminisat_bool hs_bindgen_ed93b55f94b2a915 (minisat_solver *arg1, signed int arg2, minisat_Lit *arg3) { return minisat_solve(arg1, arg2, arg3); }\nminisat_lbool hs_bindgen_6c2cac2d6db91888 (minisat_solver *arg1, signed int arg2, minisat_Lit *arg3) { return minisat_limited_solve(arg1, arg2, arg3); }\nvoid hs_bindgen_09423453af5937fd (minisat_solver *arg1) { minisat_solve_begin(arg1); }\nvoid hs_bindgen_63118857b07a987b (minisat_solver *arg1, minisat_Lit arg2) { minisat_solve_addLit(arg1, arg2); }\nminisat_bool hs_bindgen_e1b261aa407a6216 (minisat_solver *arg1) { return minisat_solve_commit(arg1); }\nminisat_lbool hs_bindgen_31f94efc14a2a5ce (minisat_solver *arg1) { return minisat_limited_solve_commit(arg1); }\nminisat_bool hs_bindgen_d00a4c9df418e634 (minisat_solver *arg1) { return minisat_okay(arg1); }\nvoid hs_bindgen_82848031c0ba9c43 (minisat_solver *arg1, minisat_Var arg2, signed int arg3) { minisat_setPolarity(arg1, arg2, arg3); }\nvoid hs_bindgen_9d45c5a203e55b4c (minisat_solver *arg1, minisat_Var arg2, signed int arg3) { minisat_setDecisionVar(arg1, arg2, arg3); }\nminisat_lbool hs_bindgen_972d41a37435a86e (void) { return minisat_get_l_True(); }\nminisat_lbool hs_bindgen_eb0bc1025f9d1ab4 (void) { return minisat_get_l_False(); }\nminisat_lbool hs_bindgen_4edb50c2a36ecfee (void) { return minisat_get_l_Undef(); }\nminisat_lbool hs_bindgen_72a30e48854d7d49 (minisat_solver *arg1, minisat_Var arg2) { return minisat_value_Var(arg1, arg2); }\nminisat_lbool hs_bindgen_24a1faadfc02dd9e (minisat_solver *arg1, minisat_Lit arg2) { return minisat_value_Lit(arg1, arg2); }\nminisat_lbool hs_bindgen_95c2a43874f7529d (minisat_solver *arg1, minisat_Var arg2) { return minisat_modelValue_Var(arg1, arg2); }\nminisat_lbool hs_bindgen_bf2ae9163b1761d0 (minisat_solver *arg1, minisat_Lit arg2) { return minisat_modelValue_Lit(arg1, arg2); }\nsigned int hs_bindgen_18bd7ba1c917426f (minisat_solver *arg1) { return minisat_num_assigns(arg1); }\nsigned int hs_bindgen_cc5376aed9c5b8bf (minisat_solver *arg1) { return minisat_num_clauses(arg1); }\nsigned int hs_bindgen_cddeecf886ce36b4 (minisat_solver *arg1) { return minisat_num_learnts(arg1); }\nsigned int hs_bindgen_c70c97a2abb2f695 (minisat_solver *arg1) { return minisat_num_vars(arg1); }\nsigned int hs_bindgen_33794eb8f5c34325 (minisat_solver *arg1) { return minisat_num_freeVars(arg1); }\nsigned int hs_bindgen_7395cbcdc5b2fcc8 (minisat_solver *arg1) { return minisat_conflict_len(arg1); }\nminisat_Lit hs_bindgen_a5c566b7df30814d (minisat_solver *arg1, signed int arg2) { return minisat_conflict_nthLit(arg1, arg2); }\nvoid hs_bindgen_c6f12fb7aeec2951 (minisat_solver *arg1, signed int arg2) { minisat_set_conf_budget(arg1, arg2); }\nvoid hs_bindgen_f1cfc0ac3f7927e3 (minisat_solver *arg1, signed int arg2) { minisat_set_prop_budget(arg1, arg2); }\nvoid hs_bindgen_66e1d1df61401adc (minisat_solver *arg1) { minisat_no_budget(arg1); }\nvoid hs_bindgen_a0a7432dda988e11 (minisat_solver *arg1) { minisat_interrupt(arg1); }\nvoid hs_bindgen_a82a274766f57e9a (minisat_solver *arg1) { minisat_clearInterrupt(arg1); }\nvoid hs_bindgen_7537f70ca0c755bd (minisat_solver *arg1, minisat_Var arg2, minisat_bool arg3) { minisat_setFrozen(arg1, arg2, arg3); }\nminisat_bool hs_bindgen_d8a4bac7ef6e9b37 (minisat_solver *arg1, minisat_Var arg2) { return minisat_isEliminated(arg1, arg2); }\nminisat_bool hs_bindgen_0b129371e1327a9c (minisat_solver *arg1, minisat_bool arg2) { return minisat_eliminate(arg1, arg2); }\nvoid hs_bindgen_1cd56a1f8a7366ac (minisat_solver *arg1, signed int arg2) { minisat_set_verbosity(arg1, arg2); }\nsigned int hs_bindgen_ad97627c14b0c288 (minisat_solver *arg1) { return minisat_num_conflicts(arg1); }\nsigned int hs_bindgen_3675fb30c95e5b31 (minisat_solver *arg1) { return minisat_num_decisions(arg1); }\nsigned int hs_bindgen_5c79999eea513cf5 (minisat_solver *arg1) { return minisat_num_restarts(arg1); }\nsigned int hs_bindgen_28edfd8354bc60c9 (minisat_solver *arg1) { return minisat_num_propagations(arg1); }\n")

{-| __C declaration:__ @minisat_new@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:46:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_9a5547ce0a57fb8a" minisat_new
  :: IO (Ptr.Ptr Minisat_solver)

{-| __C declaration:__ @minisat_delete@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:47:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_2744d30ad95405d6" minisat_delete
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_newVar@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:49:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_010517bb169afb37" minisat_newVar
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO Minisat_Var

{-| __C declaration:__ @minisat_newLit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:50:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_b59bdab42e53ab79" minisat_newLit
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO Minisat_Lit

{-| __C declaration:__ @minisat_mkLit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:52:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_fd2ef71f0a3d4fe5" minisat_mkLit
  :: Minisat_Var
     {- ^ __C declaration:__ @x@
     -}
  -> IO Minisat_Lit

{-| __C declaration:__ @minisat_mkLit_args@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:53:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_2b0b8c73a5d10a9f" minisat_mkLit_args
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
foreign import ccall safe "hs_bindgen_5d1abd9af0a77f5a" minisat_negate
  :: Minisat_Lit
     {- ^ __C declaration:__ @p@
     -}
  -> IO Minisat_Lit

{-| __C declaration:__ @minisat_var@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:56:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_b51f9b92d9c7b45e" minisat_var
  :: Minisat_Lit
     {- ^ __C declaration:__ @p@
     -}
  -> IO Minisat_Var

{-| __C declaration:__ @minisat_sign@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:57:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_3b56deef4d8d34ac" minisat_sign
  :: Minisat_Lit
     {- ^ __C declaration:__ @p@
     -}
  -> IO Minisat_bool

{-| __C declaration:__ @minisat_addClause@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:59:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_915733c72f129fdf" minisat_addClause
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
foreign import ccall safe "hs_bindgen_cacf08ceeaaa5475" minisat_addClause_begin
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_addClause_addLit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:61:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_0618846d301db6ba" minisat_addClause_addLit
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
foreign import ccall safe "hs_bindgen_57ce1f67b003c061" minisat_addClause_commit
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO Minisat_bool

{-| __C declaration:__ @minisat_simplify@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:64:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_62fc51e9cffd2941" minisat_simplify
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO Minisat_bool

{-| __C declaration:__ @minisat_solve@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:66:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_ed93b55f94b2a915" minisat_solve
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
foreign import ccall safe "hs_bindgen_6c2cac2d6db91888" minisat_limited_solve
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
foreign import ccall safe "hs_bindgen_09423453af5937fd" minisat_solve_begin
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_solve_addLit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:69:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_63118857b07a987b" minisat_solve_addLit
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
foreign import ccall safe "hs_bindgen_e1b261aa407a6216" minisat_solve_commit
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO Minisat_bool

{-| __C declaration:__ @minisat_limited_solve_commit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:71:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_31f94efc14a2a5ce" minisat_limited_solve_commit
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO Minisat_lbool

{-| __C declaration:__ @minisat_okay@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:74:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_d00a4c9df418e634" minisat_okay
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO Minisat_bool

{-| __C declaration:__ @minisat_setPolarity@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:76:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_82848031c0ba9c43" minisat_setPolarity
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
foreign import ccall safe "hs_bindgen_9d45c5a203e55b4c" minisat_setDecisionVar
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
foreign import ccall safe "hs_bindgen_972d41a37435a86e" minisat_get_l_True
  :: IO Minisat_lbool

{-| __C declaration:__ @minisat_get_l_False@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:80:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_eb0bc1025f9d1ab4" minisat_get_l_False
  :: IO Minisat_lbool

{-| __C declaration:__ @minisat_get_l_Undef@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:81:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_4edb50c2a36ecfee" minisat_get_l_Undef
  :: IO Minisat_lbool

{-| __C declaration:__ @minisat_value_Var@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:83:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_72a30e48854d7d49" minisat_value_Var
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
foreign import ccall safe "hs_bindgen_24a1faadfc02dd9e" minisat_value_Lit
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
foreign import ccall safe "hs_bindgen_95c2a43874f7529d" minisat_modelValue_Var
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
foreign import ccall safe "hs_bindgen_bf2ae9163b1761d0" minisat_modelValue_Lit
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
foreign import ccall safe "hs_bindgen_18bd7ba1c917426f" minisat_num_assigns
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @minisat_num_clauses@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:89:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_cc5376aed9c5b8bf" minisat_num_clauses
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @minisat_num_learnts@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:90:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_cddeecf886ce36b4" minisat_num_learnts
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @minisat_num_vars@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:91:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_c70c97a2abb2f695" minisat_num_vars
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @minisat_num_freeVars@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:92:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_33794eb8f5c34325" minisat_num_freeVars
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @minisat_conflict_len@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:94:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_7395cbcdc5b2fcc8" minisat_conflict_len
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @minisat_conflict_nthLit@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:95:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_a5c566b7df30814d" minisat_conflict_nthLit
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
foreign import ccall safe "hs_bindgen_c6f12fb7aeec2951" minisat_set_conf_budget
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
foreign import ccall safe "hs_bindgen_f1cfc0ac3f7927e3" minisat_set_prop_budget
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
foreign import ccall safe "hs_bindgen_66e1d1df61401adc" minisat_no_budget
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_interrupt@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:102:6@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_a0a7432dda988e11" minisat_interrupt
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_clearInterrupt@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:103:6@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_a82a274766f57e9a" minisat_clearInterrupt
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO ()

{-| __C declaration:__ @minisat_setFrozen@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:106:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_7537f70ca0c755bd" minisat_setFrozen
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
foreign import ccall safe "hs_bindgen_d8a4bac7ef6e9b37" minisat_isEliminated
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
foreign import ccall safe "hs_bindgen_0b129371e1327a9c" minisat_eliminate
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
foreign import ccall safe "hs_bindgen_1cd56a1f8a7366ac" minisat_set_verbosity
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
foreign import ccall safe "hs_bindgen_ad97627c14b0c288" minisat_num_conflicts
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @minisat_num_decisions@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:117:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_3675fb30c95e5b31" minisat_num_decisions
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @minisat_num_restarts@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:118:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_5c79999eea513cf5" minisat_num_restarts
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @minisat_num_propagations@

    __defined at:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h:119:17@

    __exported by:__ @\/home\/bolt\/Desktop\/Bolt\/UMinho\/Profissional\/Well-Typed\/Projects\/hs-bindgen\/examples\/c-minisat\/minisat-c-bindings\/minisat.h@
-}
foreign import ccall safe "hs_bindgen_28edfd8354bc60c9" minisat_num_propagations
  :: Ptr.Ptr Minisat_solver
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt
