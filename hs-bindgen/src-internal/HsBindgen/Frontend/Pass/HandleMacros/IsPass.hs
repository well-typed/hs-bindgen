module HsBindgen.Frontend.Pass.HandleMacros.IsPass (
    HandleMacros
  , HandleMacrosReparseMsg(..)
    -- * Checked macros
  , CheckedMacro(..)
  , CheckedMacroType(..)
  , CheckedMacroExpr(..)
  ) where

import C.Expr.Syntax qualified as CExpr.DSL
import C.Expr.Typecheck.Type qualified as CExpr.DSL

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.HandleMacros.Error
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type HandleMacros :: Pass
data HandleMacros a

-- We do not need the @ReparseInfo@ anymore, so we drop it from the annotations.
type family AnnHandleMacros (ix :: Symbol) :: Star where
  AnnHandleMacros "TranslationUnit" = DeclMeta
  AnnHandleMacros _                 = NoAnn

instance IsPass HandleMacros where
  type MacroBody  HandleMacros = CheckedMacro HandleMacros
  type ExtBinding HandleMacros = Void
  type Ann ix     HandleMacros = AnnHandleMacros ix
  type Msg        HandleMacros = HandleMacrosReparseMsg

{-------------------------------------------------------------------------------
  Checked macros
-------------------------------------------------------------------------------}

data CheckedMacro p =
    MacroType (CheckedMacroType p)
  | MacroExpr CheckedMacroExpr

data CheckedMacroType p = CheckedMacroType{
      typ :: C.Type p
    , ann :: Ann "CheckedMacroType" p -- for the name mangler
    }

-- | Checked expression (function) macro
--
-- TODO: This is wrong, it does not allow name mangling to do its job. To fix
-- that we'd have to change 'CExpr.DSL.MExpr'.
data CheckedMacroExpr = CheckedMacroExpr{
      args :: [CExpr.DSL.Name]
    , body :: CExpr.DSL.MExpr CExpr.DSL.Ps
    , typ  :: CExpr.DSL.Quant (CExpr.DSL.Type CExpr.DSL.Ty)
    }
  deriving stock (Show, Eq, Generic)

deriving stock instance IsPass p => Show (CheckedMacro     p)
deriving stock instance IsPass p => Show (CheckedMacroType p)

deriving stock instance IsPass p => Eq (CheckedMacro     p)
deriving stock instance IsPass p => Eq (CheckedMacroType p)

instance CoercePass CheckedMacroType p p'
      => CoercePass CheckedMacro p p' where
  coercePass (MacroType typ)  = MacroType (coercePass typ)
  coercePass (MacroExpr expr) = MacroExpr expr

instance (
      CoercePass C.Type p p'
    , Ann "CheckedMacroType" p ~ Ann "CheckedMacroType" p'
    ) => CoercePass CheckedMacroType p p' where
  coercePass macroType = CheckedMacroType{
        typ = coercePass macroType.typ
      , ann = macroType.ann
      }

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassId ConstructTranslationUnit HandleMacros
