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
    LVal (..),
    Expr (..),
    prettyDecl,
) where

import Control.Monad.State.Strict (State, evalState, get, put)

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.PrettyPrinter qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Imports

import DeBruijn (Env (..), Idx, lookupEnv, sizeEnv, tabulateEnv)

type Name = String

data Decl where
    FunDefn :: Name -> C.Type Final -> C.FunctionPurity -> Args ctx -> [Stmt ctx] -> Decl

deriving instance Show Decl

type Args ctx = Env ctx (C.Type Final)

-- Env, and thus Args, are SnocList.
-- when converting from ordinary list, we need to reverse first.
withArgs :: [a] -> (forall ctx. Env ctx a -> r) -> r
withArgs tys = withArgs' (reverse tys)

withArgs' :: [a] -> (forall ctx. Env ctx a -> r) -> r
withArgs' []       k = k EmptyEnv
withArgs' (x : xs) k = withArgs' xs $ \args -> k (args :> x)

argsToIdx :: Env ctx a -> Env ctx (Idx ctx)
argsToIdx args = tabulateEnv (sizeEnv args) id

data Stmt ctx
    = Return (Expr ctx)
    | Expr (Expr ctx)
    | Assign (LVal ctx) (Expr ctx) -- technically an expression, but we treat it as a statement.
  deriving Show

data LVal ctx
    = LVar (Idx ctx)
    | LDeRef (LVal ctx)
  deriving Show

data Expr ctx
    = Call Name [Expr ctx]
    | Var (Idx ctx)
      -- | A named variable can be used to refer to variables that are free with
      -- respect to the enclosing 'Decl'\/'FunDefn'.
      --
      -- If a variable is bound by the function definition, use 'Var' instead.
      --
      -- For example, any global variable is a free variable with respect to a
      -- function definition:
      --
      -- > int i = 0;
      -- > void i_plus (int j) { i += j; }
      --
      -- With respect to the function definition, @i@ is a free variable, @j@ is
      -- a bound variable. A 'FunDefn' describing @i_plus@ should use 'NamedVar'
      -- for @i@, and 'Var' for @j@.
    | NamedVar Name
    | DeRef (Expr ctx)
      -- | The @&@ C-operator.
    | Address (Expr ctx)
  deriving Show

prettyDecl :: Decl -> ShowS
prettyDecl (FunDefn n ty attrs args stmts) = prettyFunDefn n ty attrs args stmts

prettyFunDefn :: forall ctx. Name -> C.Type Final -> C.FunctionPurity -> Args ctx -> [Stmt ctx] -> ShowS
prettyFunDefn fun res pur args stmts =
      C.showsFunctionType (showString fun) pur args' res
    . showString "\n{\n"
    . foldMapShowS (\stmt -> showString "  "
                           . prettyStmt env stmt
                           . showChar '\n'
                   ) stmts
    . showString "}"
  where
    args0 :: State Int (Env ctx ((ShowS, C.Type Final), ShowS))
    args0 = forM args $ \ty -> do
        i <- get
        put $! i + 1
        let n = showString "arg" . shows i
        return ((n, ty), n)

    args1 = evalState args0 1
    args' = toList (fst <$> args1)
    env   = snd <$> args1

prettyStmt :: Env ctx ShowS -> Stmt ctx -> ShowS
prettyStmt env (Return e)   = showString "return "                . prettyExpr env e . showChar ';'
prettyStmt env (Expr e)     =                                       prettyExpr env e . showChar ';'
prettyStmt env (Assign x e) = prettyLVal env x . showString " = " . prettyExpr env e . showChar ';'

prettyLVal :: Env ctx ShowS -> LVal ctx -> ShowS
prettyLVal env (LVar x)   = lookupEnv x env
prettyLVal env (LDeRef x) = showChar '*' . prettyLVal env x

prettyExpr :: Env ctx ShowS -> Expr ctx -> ShowS
prettyExpr env  (Var s)      = lookupEnv s env
prettyExpr _env (NamedVar n) = showString n
prettyExpr env  (DeRef e)    = showChar '*' . prettyExpr env e
prettyExpr env  (Address e)  = showChar '&' . prettyExpr env e
prettyExpr env  (Call f xs)  = showString f . showChar '(' . foldMapSepShowS (showString ", ") (prettyExpr env) xs . showChar ')'

foldMapShowS :: (a -> ShowS) -> [a] -> ShowS
foldMapShowS f = foldr (\a b -> f a . b) id

foldMapSepShowS :: ShowS -> (a -> ShowS) -> [a] -> ShowS
foldMapSepShowS _sep _f []     = id
foldMapSepShowS  sep  f (x:xs) = foldr1 (\a b -> a . sep . b) (fmap f (x :| xs))
