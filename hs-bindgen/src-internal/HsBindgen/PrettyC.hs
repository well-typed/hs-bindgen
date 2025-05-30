-- | A small subset of C which we can pretty-print.
--
-- Used for generating C wrappers in userland-capi approach.
-- It's cleaner to generate AST than glueing string-of-code together.
module HsBindgen.PrettyC (
    Decl (..),
    Stmt (..),
    Expr (..),
    prettyDecl,
) where

import HsBindgen.Imports
import HsBindgen.C.AST.Type qualified as C

type Name = String

data Decl
    = FunDefn Name C.Type [(Name, C.Type)] [Stmt] -- ^ function declaration, signature as a string for now.
  deriving Show

data Stmt
    = Return Expr
    | Expr Expr
  deriving Show

data Expr
    = Call Name [Expr]
    | Var Name
    -- | DeRef Expr
  deriving Show

prettyDecl :: Decl -> ShowS
prettyDecl (FunDefn n ty args stmts) =
    C.showsFunctionType (showString n) (first showString <$> args) ty .
    showString " { " . foldMapShowS prettyStmt stmts . showString " }"

prettyStmt :: Stmt -> ShowS
prettyStmt (Return e) = showString "return " . prettyExpr e . showChar ';'
prettyStmt (Expr e)   =                        prettyExpr e . showChar ';'

prettyExpr :: Expr -> ShowS
prettyExpr (Var s) = showString s
prettyExpr (Call f xs) = showString f . showChar '(' . foldMapSepShowS (showString ", ") prettyExpr xs . showChar ')'

foldMapShowS :: (a -> ShowS) -> [a] -> ShowS
foldMapShowS f = foldr (\a b -> f a . b) id

foldMapSepShowS :: ShowS -> (Expr -> ShowS) -> [Expr] -> ShowS
foldMapSepShowS _sep _f []     = id
foldMapSepShowS  sep  f (x:xs) = foldr1 (\a b -> a . sep . b) (fmap f (x :| xs))
