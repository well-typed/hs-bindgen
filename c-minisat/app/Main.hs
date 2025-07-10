module Main where

import qualified Minisat.Generated as Minisat
import qualified Foreign.Ptr as F
import Data.Functor (void)

-- Adds a list of literals
--
-- addClause [a, b, c] == a v b v c
--
-- multiple calls to addClause, e.g.
--
-- addClause [a, b] >> addClause [c, d] == (a v b) ∧ (c v d)
--
addClause :: F.Ptr Minisat.Minisat_solver -> [Minisat.Minisat_Lit] -> IO ()
addClause s lits = do
  Minisat.minisat_addClause_begin s
  mapM_ (Minisat.minisat_addClause_addLit s) lits
  void $ Minisat.minisat_addClause_commit s

exampleSolver :: IO ()
exampleSolver = do
  minisatSolver <- Minisat.minisat_new

  x1 <- Minisat.minisat_newLit minisatSolver
  x2 <- Minisat.minisat_newLit minisatSolver
  x3 <- Minisat.minisat_newLit minisatSolver

  nx1 <- Minisat.minisat_negate x1
  nx2 <- Minisat.minisat_negate x1
  nx3 <- Minisat.minisat_negate x1

  Minisat.minisat_addClause_begin minisatSolver

  addClause minisatSolver [x1, x2]
  addClause minisatSolver [nx1, x3]
  addClause minisatSolver [nx2, nx3]

  sat <- Minisat.minisat_solve minisatSolver 0 F.nullPtr
  let satResult = Minisat.un_Minisat_bool sat /= 0
  putStrLn ("Satisfiable? " ++ show satResult)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

  exampleSolver
