module HsBindgen.Macro.CExpr (
    CExpr
  , ParsedMacroBody(..)
  , TypecheckedMacroTypeBody(..)
  , TypecheckedMacroValueBody(..)

    -- Internal
  , convertTagKind
  ) where

import C.Expr.Syntax qualified as CExpr
import C.Expr.Typecheck qualified as CExpr

import HsBindgen.IR.C qualified as C
import HsBindgen.Macro.Type

-- | Tag for the default C macro language, backed by @c-expr-dsl@.
data CExpr

instance HasMacroTypes CExpr where
  newtype ParsedMacroBody CExpr =
    ParsedMacroBodyCExpr CExpr.Macro
  newtype TypecheckedMacroTypeBody CExpr var =
    TypecheckedMacroTypeBodyCExpr (CExpr.TypecheckedMacroTypeExpr var)
  newtype TypecheckedMacroValueBody CExpr var =
    TypecheckedMacroValueBodyCExpr (CExpr.TypecheckedMacroValueExpr var)

deriving stock instance Show (ParsedMacroBody CExpr)
deriving stock instance Eq   (ParsedMacroBody CExpr)

deriving stock instance (Show var) => Show (TypecheckedMacroTypeBody CExpr var)
deriving stock instance (Eq var)   => Eq   (TypecheckedMacroTypeBody CExpr var)

deriving stock instance Functor     (TypecheckedMacroTypeBody CExpr)
deriving stock instance Foldable    (TypecheckedMacroTypeBody CExpr)
deriving stock instance Traversable (TypecheckedMacroTypeBody CExpr)

deriving stock instance (Show var) => Show (TypecheckedMacroValueBody CExpr var)
deriving stock instance (Eq var)   => Eq   (TypecheckedMacroValueBody CExpr var)

deriving stock instance Functor     (TypecheckedMacroValueBody CExpr)
deriving stock instance Foldable    (TypecheckedMacroValueBody CExpr)
deriving stock instance Traversable (TypecheckedMacroValueBody CExpr)

{-------------------------------------------------------------------------------
  Internal helpers
-------------------------------------------------------------------------------}

convertTagKind :: CExpr.TagKind -> C.TagKind
convertTagKind = \case
  CExpr.TagStruct -> C.TagKindStruct
  CExpr.TagUnion  -> C.TagKindUnion
  CExpr.TagEnum   -> C.TagKindEnum
