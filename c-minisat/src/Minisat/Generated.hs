{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Minisat.Generated where

import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (Bounded, Enum, Eq, IO, Integral, Num, Ord, Read, Real, Show)

$(CAPI.addCSource "#include \"minisat.h\"\nminisat_solver *MinisatGenerated_minisat_new (void) { return minisat_new(); }\nvoid MinisatGenerated_minisat_delete (minisat_solver *arg1) { minisat_delete(arg1); }\nminisat_Var MinisatGenerated_minisat_newVar (minisat_solver *arg1) { return minisat_newVar(arg1); }\nminisat_Lit MinisatGenerated_minisat_newLit (minisat_solver *arg1) { return minisat_newLit(arg1); }\nminisat_Lit MinisatGenerated_minisat_mkLit (minisat_Var arg1) { return minisat_mkLit(arg1); }\nminisat_Lit MinisatGenerated_minisat_mkLit_args (minisat_Var arg1, signed int arg2) { return minisat_mkLit_args(arg1, arg2); }\nminisat_Lit MinisatGenerated_minisat_negate (minisat_Lit arg1) { return minisat_negate(arg1); }\nminisat_Var MinisatGenerated_minisat_var (minisat_Lit arg1) { return minisat_var(arg1); }\nminisat_bool MinisatGenerated_minisat_sign (minisat_Lit arg1) { return minisat_sign(arg1); }\nminisat_bool MinisatGenerated_minisat_addClause (minisat_solver *arg1, signed int arg2, minisat_Lit *arg3) { return minisat_addClause(arg1, arg2, arg3); }\nvoid MinisatGenerated_minisat_addClause_begin (minisat_solver *arg1) { minisat_addClause_begin(arg1); }\nvoid MinisatGenerated_minisat_addClause_addLit (minisat_solver *arg1, minisat_Lit arg2) { minisat_addClause_addLit(arg1, arg2); }\nminisat_bool MinisatGenerated_minisat_addClause_commit (minisat_solver *arg1) { return minisat_addClause_commit(arg1); }\nminisat_bool MinisatGenerated_minisat_simplify (minisat_solver *arg1) { return minisat_simplify(arg1); }\nminisat_bool MinisatGenerated_minisat_solve (minisat_solver *arg1, signed int arg2, minisat_Lit *arg3) { return minisat_solve(arg1, arg2, arg3); }\nminisat_lbool MinisatGenerated_minisat_limited_solve (minisat_solver *arg1, signed int arg2, minisat_Lit *arg3) { return minisat_limited_solve(arg1, arg2, arg3); }\nvoid MinisatGenerated_minisat_solve_begin (minisat_solver *arg1) { minisat_solve_begin(arg1); }\nvoid MinisatGenerated_minisat_solve_addLit (minisat_solver *arg1, minisat_Lit arg2) { minisat_solve_addLit(arg1, arg2); }\nminisat_bool MinisatGenerated_minisat_solve_commit (minisat_solver *arg1) { return minisat_solve_commit(arg1); }\nminisat_lbool MinisatGenerated_minisat_limited_solve_commit (minisat_solver *arg1) { return minisat_limited_solve_commit(arg1); }\nminisat_bool MinisatGenerated_minisat_okay (minisat_solver *arg1) { return minisat_okay(arg1); }\nvoid MinisatGenerated_minisat_setPolarity (minisat_solver *arg1, minisat_Var arg2, signed int arg3) { minisat_setPolarity(arg1, arg2, arg3); }\nvoid MinisatGenerated_minisat_setDecisionVar (minisat_solver *arg1, minisat_Var arg2, signed int arg3) { minisat_setDecisionVar(arg1, arg2, arg3); }\nminisat_lbool MinisatGenerated_minisat_get_l_True (void) { return minisat_get_l_True(); }\nminisat_lbool MinisatGenerated_minisat_get_l_False (void) { return minisat_get_l_False(); }\nminisat_lbool MinisatGenerated_minisat_get_l_Undef (void) { return minisat_get_l_Undef(); }\nminisat_lbool MinisatGenerated_minisat_value_Var (minisat_solver *arg1, minisat_Var arg2) { return minisat_value_Var(arg1, arg2); }\nminisat_lbool MinisatGenerated_minisat_value_Lit (minisat_solver *arg1, minisat_Lit arg2) { return minisat_value_Lit(arg1, arg2); }\nminisat_lbool MinisatGenerated_minisat_modelValue_Var (minisat_solver *arg1, minisat_Var arg2) { return minisat_modelValue_Var(arg1, arg2); }\nminisat_lbool MinisatGenerated_minisat_modelValue_Lit (minisat_solver *arg1, minisat_Lit arg2) { return minisat_modelValue_Lit(arg1, arg2); }\nsigned int MinisatGenerated_minisat_num_assigns (minisat_solver *arg1) { return minisat_num_assigns(arg1); }\nsigned int MinisatGenerated_minisat_num_clauses (minisat_solver *arg1) { return minisat_num_clauses(arg1); }\nsigned int MinisatGenerated_minisat_num_learnts (minisat_solver *arg1) { return minisat_num_learnts(arg1); }\nsigned int MinisatGenerated_minisat_num_vars (minisat_solver *arg1) { return minisat_num_vars(arg1); }\nsigned int MinisatGenerated_minisat_num_freeVars (minisat_solver *arg1) { return minisat_num_freeVars(arg1); }\nsigned int MinisatGenerated_minisat_conflict_len (minisat_solver *arg1) { return minisat_conflict_len(arg1); }\nminisat_Lit MinisatGenerated_minisat_conflict_nthLit (minisat_solver *arg1, signed int arg2) { return minisat_conflict_nthLit(arg1, arg2); }\nvoid MinisatGenerated_minisat_set_conf_budget (minisat_solver *arg1, signed int arg2) { minisat_set_conf_budget(arg1, arg2); }\nvoid MinisatGenerated_minisat_set_prop_budget (minisat_solver *arg1, signed int arg2) { minisat_set_prop_budget(arg1, arg2); }\nvoid MinisatGenerated_minisat_no_budget (minisat_solver *arg1) { minisat_no_budget(arg1); }\nvoid MinisatGenerated_minisat_interrupt (minisat_solver *arg1) { minisat_interrupt(arg1); }\nvoid MinisatGenerated_minisat_clearInterrupt (minisat_solver *arg1) { minisat_clearInterrupt(arg1); }\nvoid MinisatGenerated_minisat_setFrozen (minisat_solver *arg1, minisat_Var arg2, minisat_bool arg3) { minisat_setFrozen(arg1, arg2, arg3); }\nminisat_bool MinisatGenerated_minisat_isEliminated (minisat_solver *arg1, minisat_Var arg2) { return minisat_isEliminated(arg1, arg2); }\nminisat_bool MinisatGenerated_minisat_eliminate (minisat_solver *arg1, minisat_bool arg2) { return minisat_eliminate(arg1, arg2); }\nvoid MinisatGenerated_minisat_set_verbosity (minisat_solver *arg1, signed int arg2) { minisat_set_verbosity(arg1, arg2); }\nsigned int MinisatGenerated_minisat_num_conflicts (minisat_solver *arg1) { return minisat_num_conflicts(arg1); }\nsigned int MinisatGenerated_minisat_num_decisions (minisat_solver *arg1) { return minisat_num_decisions(arg1); }\nsigned int MinisatGenerated_minisat_num_restarts (minisat_solver *arg1) { return minisat_num_restarts(arg1); }\nsigned int MinisatGenerated_minisat_num_propagations (minisat_solver *arg1) { return minisat_num_propagations(arg1); }\n")

data Minisat_solver

opaque :: forall a0. a0 -> a0
opaque = \x0 -> x0

newtype Minisat_Var = Minisat_Var
  { un_Minisat_Var :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype Minisat_Lit = Minisat_Lit
  { un_Minisat_Lit :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype Minisat_lbool = Minisat_lbool
  { un_Minisat_lbool :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype Minisat_bool = Minisat_bool
  { un_Minisat_bool :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

foreign import ccall safe "MinisatGenerated_minisat_new" minisat_new :: IO (F.Ptr Minisat_solver)

foreign import ccall safe "MinisatGenerated_minisat_delete" minisat_delete :: (F.Ptr Minisat_solver) -> IO ()

foreign import ccall safe "MinisatGenerated_minisat_newVar" minisat_newVar :: (F.Ptr Minisat_solver) -> IO Minisat_Var

foreign import ccall safe "MinisatGenerated_minisat_newLit" minisat_newLit :: (F.Ptr Minisat_solver) -> IO Minisat_Lit

foreign import ccall safe "MinisatGenerated_minisat_mkLit" minisat_mkLit :: Minisat_Var -> IO Minisat_Lit

foreign import ccall safe "MinisatGenerated_minisat_mkLit_args" minisat_mkLit_args :: Minisat_Var -> FC.CInt -> IO Minisat_Lit

foreign import ccall safe "MinisatGenerated_minisat_negate" minisat_negate :: Minisat_Lit -> IO Minisat_Lit

foreign import ccall safe "MinisatGenerated_minisat_var" minisat_var :: Minisat_Lit -> IO Minisat_Var

foreign import ccall safe "MinisatGenerated_minisat_sign" minisat_sign :: Minisat_Lit -> IO Minisat_bool

foreign import ccall safe "MinisatGenerated_minisat_addClause" minisat_addClause :: (F.Ptr Minisat_solver) -> FC.CInt -> (F.Ptr Minisat_Lit) -> IO Minisat_bool

foreign import ccall safe "MinisatGenerated_minisat_addClause_begin" minisat_addClause_begin :: (F.Ptr Minisat_solver) -> IO ()

foreign import ccall safe "MinisatGenerated_minisat_addClause_addLit" minisat_addClause_addLit :: (F.Ptr Minisat_solver) -> Minisat_Lit -> IO ()

foreign import ccall safe "MinisatGenerated_minisat_addClause_commit" minisat_addClause_commit :: (F.Ptr Minisat_solver) -> IO Minisat_bool

foreign import ccall safe "MinisatGenerated_minisat_simplify" minisat_simplify :: (F.Ptr Minisat_solver) -> IO Minisat_bool

foreign import ccall safe "MinisatGenerated_minisat_solve" minisat_solve :: (F.Ptr Minisat_solver) -> FC.CInt -> (F.Ptr Minisat_Lit) -> IO Minisat_bool

foreign import ccall safe "MinisatGenerated_minisat_limited_solve" minisat_limited_solve :: (F.Ptr Minisat_solver) -> FC.CInt -> (F.Ptr Minisat_Lit) -> IO Minisat_lbool

foreign import ccall safe "MinisatGenerated_minisat_solve_begin" minisat_solve_begin :: (F.Ptr Minisat_solver) -> IO ()

foreign import ccall safe "MinisatGenerated_minisat_solve_addLit" minisat_solve_addLit :: (F.Ptr Minisat_solver) -> Minisat_Lit -> IO ()

foreign import ccall safe "MinisatGenerated_minisat_solve_commit" minisat_solve_commit :: (F.Ptr Minisat_solver) -> IO Minisat_bool

foreign import ccall safe "MinisatGenerated_minisat_limited_solve_commit" minisat_limited_solve_commit :: (F.Ptr Minisat_solver) -> IO Minisat_lbool

foreign import ccall safe "MinisatGenerated_minisat_okay" minisat_okay :: (F.Ptr Minisat_solver) -> IO Minisat_bool

foreign import ccall safe "MinisatGenerated_minisat_setPolarity" minisat_setPolarity :: (F.Ptr Minisat_solver) -> Minisat_Var -> FC.CInt -> IO ()

foreign import ccall safe "MinisatGenerated_minisat_setDecisionVar" minisat_setDecisionVar :: (F.Ptr Minisat_solver) -> Minisat_Var -> FC.CInt -> IO ()

foreign import ccall safe "MinisatGenerated_minisat_get_l_True" minisat_get_l_True :: IO Minisat_lbool

foreign import ccall safe "MinisatGenerated_minisat_get_l_False" minisat_get_l_False :: IO Minisat_lbool

foreign import ccall safe "MinisatGenerated_minisat_get_l_Undef" minisat_get_l_Undef :: IO Minisat_lbool

foreign import ccall safe "MinisatGenerated_minisat_value_Var" minisat_value_Var :: (F.Ptr Minisat_solver) -> Minisat_Var -> IO Minisat_lbool

foreign import ccall safe "MinisatGenerated_minisat_value_Lit" minisat_value_Lit :: (F.Ptr Minisat_solver) -> Minisat_Lit -> IO Minisat_lbool

foreign import ccall safe "MinisatGenerated_minisat_modelValue_Var" minisat_modelValue_Var :: (F.Ptr Minisat_solver) -> Minisat_Var -> IO Minisat_lbool

foreign import ccall safe "MinisatGenerated_minisat_modelValue_Lit" minisat_modelValue_Lit :: (F.Ptr Minisat_solver) -> Minisat_Lit -> IO Minisat_lbool

foreign import ccall safe "MinisatGenerated_minisat_num_assigns" minisat_num_assigns :: (F.Ptr Minisat_solver) -> IO FC.CInt

foreign import ccall safe "MinisatGenerated_minisat_num_clauses" minisat_num_clauses :: (F.Ptr Minisat_solver) -> IO FC.CInt

foreign import ccall safe "MinisatGenerated_minisat_num_learnts" minisat_num_learnts :: (F.Ptr Minisat_solver) -> IO FC.CInt

foreign import ccall safe "MinisatGenerated_minisat_num_vars" minisat_num_vars :: (F.Ptr Minisat_solver) -> IO FC.CInt

foreign import ccall safe "MinisatGenerated_minisat_num_freeVars" minisat_num_freeVars :: (F.Ptr Minisat_solver) -> IO FC.CInt

foreign import ccall safe "MinisatGenerated_minisat_conflict_len" minisat_conflict_len :: (F.Ptr Minisat_solver) -> IO FC.CInt

foreign import ccall safe "MinisatGenerated_minisat_conflict_nthLit" minisat_conflict_nthLit :: (F.Ptr Minisat_solver) -> FC.CInt -> IO Minisat_Lit

foreign import ccall safe "MinisatGenerated_minisat_set_conf_budget" minisat_set_conf_budget :: (F.Ptr Minisat_solver) -> FC.CInt -> IO ()

foreign import ccall safe "MinisatGenerated_minisat_set_prop_budget" minisat_set_prop_budget :: (F.Ptr Minisat_solver) -> FC.CInt -> IO ()

foreign import ccall safe "MinisatGenerated_minisat_no_budget" minisat_no_budget :: (F.Ptr Minisat_solver) -> IO ()

foreign import ccall safe "MinisatGenerated_minisat_interrupt" minisat_interrupt :: (F.Ptr Minisat_solver) -> IO ()

foreign import ccall safe "MinisatGenerated_minisat_clearInterrupt" minisat_clearInterrupt :: (F.Ptr Minisat_solver) -> IO ()

foreign import ccall safe "MinisatGenerated_minisat_setFrozen" minisat_setFrozen :: (F.Ptr Minisat_solver) -> Minisat_Var -> Minisat_bool -> IO ()

foreign import ccall safe "MinisatGenerated_minisat_isEliminated" minisat_isEliminated :: (F.Ptr Minisat_solver) -> Minisat_Var -> IO Minisat_bool

foreign import ccall safe "MinisatGenerated_minisat_eliminate" minisat_eliminate :: (F.Ptr Minisat_solver) -> Minisat_bool -> IO Minisat_bool

foreign import ccall safe "MinisatGenerated_minisat_set_verbosity" minisat_set_verbosity :: (F.Ptr Minisat_solver) -> FC.CInt -> IO ()

foreign import ccall safe "MinisatGenerated_minisat_num_conflicts" minisat_num_conflicts :: (F.Ptr Minisat_solver) -> IO FC.CInt

foreign import ccall safe "MinisatGenerated_minisat_num_decisions" minisat_num_decisions :: (F.Ptr Minisat_solver) -> IO FC.CInt

foreign import ccall safe "MinisatGenerated_minisat_num_restarts" minisat_num_restarts :: (F.Ptr Minisat_solver) -> IO FC.CInt

foreign import ccall safe "MinisatGenerated_minisat_num_propagations" minisat_num_propagations :: (F.Ptr Minisat_solver) -> IO FC.CInt
