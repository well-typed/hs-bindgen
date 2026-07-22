-- | Macro types specific to @c-expr-dsl@.
--
-- Intended for unqualified import.
module HsBindgen.Internal.Macro.CExpr.Type (
    CExpr
  ) where

import C.Expr.Syntax qualified as CExpr
import C.Expr.Typecheck qualified as CExpr

import HsBindgen.Macro.Type qualified as Macro

-- | Tag for the default C macro language, backed by @c-expr-dsl@.
data CExpr

instance Macro.HasTypes CExpr where
  type Parsed CExpr           = CExpr.Macro
  type TypecheckedType CExpr  = CExpr.TypecheckedMacroTypeExpr
  type TypecheckedValue CExpr = CExpr.TypecheckedMacroValueExpr
