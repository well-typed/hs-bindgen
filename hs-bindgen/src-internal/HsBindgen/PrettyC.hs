-- | A small subset of C which we can pretty-print.
--
-- Used for generating C wrappers in userland-capi approach.
-- It's cleaner to generate AST than glueing string-of-code together.
module HsBindgen.PrettyC (
    Decl (..),
    Args,
    withArgs,
    argsToIdx,
    Stmt (..),
    Expr (..),
    prettyDecl,
) where

import HsBindgen.Imports
import HsBindgen.C.AST.Type qualified as C

import DeBruijn (Idx, lookupEnv, sizeEnv, tabulateEnv, Env(..))
import Control.Monad.State.Strict (State, evalState, put, get)

type Name = String

data Decl where
    FunDefn :: Name -> C.Type -> Args ctx -> [Stmt ctx] -> Decl

deriving instance Show Decl

type Args ctx = Env ctx C.Type

-- Env, and thus Args, are SnocList.
-- when converting from ordinary list, we need to reverse first.
withArgs :: [C.Type] -> (forall ctx. Args ctx -> r) -> r
withArgs tys = withArgs' (reverse tys)

withArgs' :: [C.Type] -> (forall ctx. Args ctx -> r) -> r
withArgs' []       k = k EmptyEnv
withArgs' (x : xs) k = withArgs' xs $ \args -> k (args :> x)

argsToIdx :: Args ctx -> [Idx ctx]
argsToIdx args = toList (tabulateEnv (sizeEnv args) id)

data Stmt ctx
    = Return (Expr ctx)
    | Expr (Expr ctx)
  deriving Show

data Expr ctx
    = Call Name [Expr ctx]
    | Var (Idx ctx)
    -- | DeRef Expr
  deriving Show

prettyDecl :: Decl -> ShowS
prettyDecl (FunDefn n ty args stmts) = prettyFunDefn n ty args stmts

prettyFunDefn :: forall ctx. Name -> C.Type -> Args ctx -> [Stmt ctx] -> ShowS
prettyFunDefn fun res args stmts =
    C.showsFunctionType (showString fun) args' res .
    showString " { " . foldMapShowS (prettyStmt env) stmts . showString " }"
  where
    args0 :: State Int (Env ctx ((ShowS, C.Type), ShowS))
    args0 = forM args $ \ty -> do
        i <- get
        put $! i + 1
        let n = showString "arg" . shows i
        return ((n, ty), n)

    args1 = evalState args0 1
    args' = toList (fst <$> args1)
    env   = snd <$> args1

prettyStmt :: Env ctx ShowS -> Stmt ctx -> ShowS
prettyStmt env (Return e) = showString "return " . prettyExpr env e . showChar ';'
prettyStmt env (Expr e)   =                        prettyExpr env e . showChar ';'

prettyExpr :: Env ctx ShowS -> Expr ctx -> ShowS
prettyExpr env (Var s)     = lookupEnv s env
prettyExpr env (Call f xs) = showString f . showChar '(' . foldMapSepShowS (showString ", ") (prettyExpr env) xs . showChar ')'

foldMapShowS :: (a -> ShowS) -> [a] -> ShowS
foldMapShowS f = foldr (\a b -> f a . b) id

foldMapSepShowS :: ShowS -> (a -> ShowS) -> [a] -> ShowS
foldMapSepShowS _sep _f []     = id
foldMapSepShowS  sep  f (x:xs) = foldr1 (\a b -> a . sep . b) (fmap f (x :| xs))
