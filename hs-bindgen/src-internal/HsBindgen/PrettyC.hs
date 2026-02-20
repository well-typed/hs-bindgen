-- | A small subset of C which we can pretty-print.
--
-- Used for generating C wrappers in userland-capi approach.
-- It's cleaner to generate AST than glueing string-of-code together.
module HsBindgen.PrettyC (
    FunDefn (..),
    Args,
    withArgs,
    argsToIdx,
    -- * AST
    Statement (..),
    CompoundStatement (..),
    CSList (..),
    Declaration (..),
    Declarator (..),
    Initializer (..),
    LVal (..),
    Expr (..),
    -- * Pretty-printing
    prettyFunDefn,
) where

import Prelude hiding (lines, unlines)

import Control.Monad.State.Strict (State, evalState, get, put)
import DeBruijn (Env (..), Idx, lookupEnv, sizeEnv, sizeToInt, tabulateEnv)

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.PrettyPrinter qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Imports

type Name = String

data FunDefn where
    FunDefn :: Name -> C.Type Final -> C.FunctionPurity -> Args ctx -> CompoundStatement ctx -> FunDefn

deriving instance Show FunDefn

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

{-------------------------------------------------------------------------------
  AST
-------------------------------------------------------------------------------}

-- | Statement
--
-- <https://en.cppreference.com/w/c/language/statements.html>
data Statement ctx where
    CompoundStatement :: CompoundStatement ctx -> Statement ctx
    -- | Expression statement
    --
    -- <https://en.cppreference.com/w/c/language/statements.html#Expression_statements>
    ExpressionStatement :: Expr ctx -> Statement ctx
  deriving Show

-- | Compound statement
--
-- <https://en.cppreference.com/w/c/language/statements.html#Compound_statements>
data CompoundStatement ctx where
    CSList :: CSList ctx -> CompoundStatement ctx
  deriving Show

-- | Compound statement (continued)
--
-- <https://en.cppreference.com/w/c/language/statements.html#Compound_statements>
data CSList ctx where
    CSNil :: CSList ctx
    CSStatement :: Statement ctx -> CSList ctx -> CSList ctx
    CSDeclaration :: Declaration ctx ctx' -> CSList ctx' -> CSList ctx

deriving stock instance Show (CSList ctx)

-- | @declaration@
--
-- <https://en.cppreference.com/w/c/language/declarations.html>
data Declaration ctx ctx' where
    -- | A declaration: no initializer
    Declaration :: C.Type Final -> Declarator ctx ctx' -> Declaration ctx ctx'
    -- | A definition: a declaration with an initializer
    Definition :: C.Type Final -> Declarator ctx ctx' -> Initializer ctx -> Declaration ctx ctx'

deriving stock instance Show (Declaration ctx ctx')

-- | @declarator@
--
-- <https://en.cppreference.com/w/c/language/declarations.html#Declarators>
data Declarator ctx ctx' where
    -- | An identifier
    Identifier :: Declarator ctx (S ctx)

deriving stock instance Show (Declarator ctx ctx')

-- | @initializer@
--
-- <https://en.cppreference.com/w/c/language/initialization.html>
data Initializer ctx where
    -- | @expression@
    InitializerExpr :: Expr ctx -> Initializer ctx

deriving stock instance Show (Initializer ctx)

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
    | Return (Expr ctx)
    | Assign (LVal ctx) (Expr ctx)
  deriving Show

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

prettyFunDefn :: FunDefn -> ShowS
prettyFunDefn (FunDefn n ty attrs args stmts) = prettyFunDefn' n ty attrs args stmts

prettyFunDefn' ::
     forall ctx.
     Name
  -> C.Type Final
  -> C.FunctionPurity
  -> Args ctx
  -> CompoundStatement ctx
  -> ShowS
prettyFunDefn' fun res pur args stmts =
      C.showsFunctionType (showString fun) pur args' res
    . showString "\n"
    . unlines (prettyCompoundStatement stmts env)
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

prettyStatement :: Statement ctx -> Env ctx ShowS -> [Line]
prettyStatement (CompoundStatement stmts) env = prettyCompoundStatement stmts env
prettyStatement (ExpressionStatement expr) env = [Line $ prettyExpr env expr . showChar ';']

prettyCompoundStatement :: CompoundStatement ctx -> Env ctx ShowS -> [Line]
prettyCompoundStatement (CSList stmts) env = concat
    [ [Line $ showChar '{']
    , tabs (prettyCSList stmts env)
    , [Line $ showChar '}']
    ]

prettyCSList :: CSList ctx -> Env ctx ShowS -> [Line]
prettyCSList CSNil _env = []
prettyCSList (CSStatement stmt stmts) env = concat
    [ prettyStatement stmt env
    , prettyCSList stmts env
    ]
prettyCSList (CSDeclaration decl stmts) env =
    let (s, env') = prettyDeclaration decl env
    in  s : prettyCSList stmts env'

prettyDeclaration ::Declaration ctx ctx' ->  Env ctx ShowS -> (Line, Env ctx' ShowS)
prettyDeclaration (Declaration t d) env =
    let (name, env') = prettyDeclarator d env
    in  ( Line $ C.showsVariableType name t . showChar ';'
        , env'
        )
prettyDeclaration (Definition t d e) env =
    let (name, env') = prettyDeclarator d env
    in  ( Line $ C.showsVariableType name t . showString " = " . prettyInitializer e env . showChar ';'
        , env'
        )

prettyDeclarator ::
     forall ctx ctx'.
     Declarator ctx ctx'
  -> Env ctx ShowS
  -> (ShowS, Env ctx' ShowS)
prettyDeclarator Identifier env = (name, env')
  where
    env' = env :> name
    name = showChar 'x' . shows (sizeToInt (sizeEnv env) + 1)

prettyInitializer :: Initializer ctx -> Env ctx ShowS -> ShowS
prettyInitializer (InitializerExpr e) env = prettyExpr env e

prettyLVal :: Env ctx ShowS -> LVal ctx -> ShowS
prettyLVal env (LVar x)   = lookupEnv x env
prettyLVal env (LDeRef x) = showChar '*' . prettyLVal env x

prettyExpr :: Env ctx ShowS -> Expr ctx -> ShowS
prettyExpr env  (Var s)      = lookupEnv s env
prettyExpr _env (NamedVar n) = showString n
prettyExpr env  (DeRef e)    = showChar '*' . prettyExpr env e
prettyExpr env  (Address e)  = showChar '&' . prettyExpr env e
prettyExpr env  (Call f xs)  = showParen True (showString f) . showParen True (foldMapSepShowS (showString ", ") (prettyExpr env) xs)
prettyExpr env  (Return e)   = showString "return " . prettyExpr env e
prettyExpr env  (Assign x e) = prettyLVal env x . showString " = " . prettyExpr env e

{-------------------------------------------------------------------------------
  Foldable
-------------------------------------------------------------------------------}

foldMapShowS :: (a -> ShowS) -> [a] -> ShowS
foldMapShowS f = foldr (\a b -> f a . b) id

foldMapSepShowS :: ShowS -> (a -> ShowS) -> [a] -> ShowS
foldMapSepShowS _sep _f []     = id
foldMapSepShowS  sep  f (x:xs) = foldr1 (\a b -> a . sep . b) (fmap f (x :| xs))

{-------------------------------------------------------------------------------
  Lines
-------------------------------------------------------------------------------}

-- | A single line of text (to be pretty-printed)
newtype Line = Line ShowS

unlines :: [Line] -> ShowS
unlines = foldMapShowS $ \(Line s) ->
    s . showChar '\n'

tabs :: [Line] -> [Line]
tabs xs = fmap tab xs

tab :: Line -> Line
tab (Line s) = Line (showString "  " . s)
