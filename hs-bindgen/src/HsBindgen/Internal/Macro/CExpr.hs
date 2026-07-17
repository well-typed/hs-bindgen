-- | Macro types specific to @c-expr-dsl@.
--
-- Intended for qualified import
--
-- @
-- import HsBindgen.Internal.Macro.CExpr (CExpr)
-- import HsBindgen.Internal.Macro.CExpr qualified as Macro
-- @
module HsBindgen.Internal.Macro.CExpr (
    CExpr
  , Macro.Parsed(..)
  , Macro.TypecheckedType(..)
  , Macro.TypecheckedValue(..)
    -- Internal
  , convertTagKind
  ) where

import C.Expr.Syntax qualified as CExpr
import C.Expr.Typecheck qualified as CExpr

import HsBindgen.IR.C qualified as C
import HsBindgen.Macro.Type qualified as Macro

-- | Tag for the default C macro language, backed by @c-expr-dsl@.
data CExpr

instance Macro.HasTypes CExpr where
  newtype Parsed CExpr ann =
    ParsedCExpr{
        unwrap :: CExpr.Macro ann
      }
  newtype TypecheckedType CExpr var =
    TypecheckedTypeCExpr{
        unwrap :: CExpr.TypecheckedMacroTypeExpr var
      }
  newtype TypecheckedValue CExpr var =
    TypecheckedValueCExpr{
        unwrap :: CExpr.TypecheckedMacroValueExpr var
      }

deriving stock instance (Show ann) => Show (Macro.Parsed CExpr ann)
deriving stock instance (Eq   ann) => Eq   (Macro.Parsed CExpr ann)

deriving stock instance (Show var) => Show (Macro.TypecheckedType CExpr var)
deriving stock instance (Eq var)   => Eq   (Macro.TypecheckedType CExpr var)

deriving stock instance Functor     (Macro.TypecheckedType CExpr)
deriving stock instance Foldable    (Macro.TypecheckedType CExpr)
deriving stock instance Traversable (Macro.TypecheckedType CExpr)

deriving stock instance (Show var) => Show (Macro.TypecheckedValue CExpr var)
deriving stock instance (Eq var)   => Eq   (Macro.TypecheckedValue CExpr var)

deriving stock instance Functor     (Macro.TypecheckedValue CExpr)
deriving stock instance Foldable    (Macro.TypecheckedValue CExpr)
deriving stock instance Traversable (Macro.TypecheckedValue CExpr)

{-------------------------------------------------------------------------------
  Internal helpers
-------------------------------------------------------------------------------}

convertTagKind :: CExpr.TagKind -> C.TagKind
convertTagKind = \case
  CExpr.TagStruct -> C.TagKindStruct
  CExpr.TagUnion  -> C.TagKindUnion
  CExpr.TagEnum   -> C.TagKindEnum
