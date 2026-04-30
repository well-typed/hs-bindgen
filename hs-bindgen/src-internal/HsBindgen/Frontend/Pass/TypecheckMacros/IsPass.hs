module HsBindgen.Frontend.Pass.TypecheckMacros.IsPass (
    TypecheckMacros
    -- * Checked macros
  , CheckedMacro(..)
  , CheckedMacroType(..)
  , CheckedMacroExpr(..)
    -- * Conversions
  , convertTagKind
  ) where

import C.Expr.Syntax qualified as CExpr
import C.Expr.Typecheck.Interface.Value qualified as V
import C.Expr.Typecheck.Type qualified as CExpr

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass (ReparseInfo)
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type TypecheckMacros :: Pass
data TypecheckMacros a

-- | Macros have been typechecked ('MacroBody' is 'CheckedMacro TypecheckMacros').
-- Other declarations still carry their 'ReparseInfo' annotations from
-- 'ConstructTranslationUnit'; these are consumed in the following
-- 'ReparseMacroExpansions' pass.
type family AnnTypecheckMacros (ix :: Symbol) :: Star where
  AnnTypecheckMacros "TranslationUnit" = DeclMeta
  AnnTypecheckMacros "StructField"     = ReparseInfo
  AnnTypecheckMacros "UnionField"      = ReparseInfo
  AnnTypecheckMacros "Typedef"         = ReparseInfo
  AnnTypecheckMacros "Function"        = ReparseInfo
  AnnTypecheckMacros "Global"          = ReparseInfo
  AnnTypecheckMacros _                 = NoAnn

instance IsPass TypecheckMacros where
  type MacroBody   TypecheckMacros = CheckedMacro TypecheckMacros
  type Ann ix      TypecheckMacros = AnnTypecheckMacros ix
  type Msg         TypecheckMacros = NoMsg Level
  type MacroId     TypecheckMacros = Id TypecheckMacros
  type CommentDecl TypecheckMacros = Maybe (C.Comment TypecheckMacros)
  macroIdId _ = id

{-------------------------------------------------------------------------------
  Checked macros
-------------------------------------------------------------------------------}

data CheckedMacro p =
    MacroType (CheckedMacroType p)
  | MacroExpr (CheckedMacroExpr p)

-- | Checked type macro
data CheckedMacroType p = CheckedMacroType{
      typ :: Type p
    , ann :: Ann "CheckedMacroType" p
    }

-- | Checked expression (function) macro
data CheckedMacroExpr p = CheckedMacroExpr{
      args :: [CExpr.Name]
    , body :: V.Expr (Id p)
    , typ  :: CExpr.Quant (CExpr.Type CExpr.Ty)
    }
  deriving stock (Generic)

deriving stock instance IsPass p => Show (CheckedMacro     p)
deriving stock instance IsPass p => Show (CheckedMacroType p)
deriving stock instance IsPass p => Show (CheckedMacroExpr p)

deriving stock instance IsPass p => Eq (CheckedMacro     p)
deriving stock instance IsPass p => Eq (CheckedMacroType p)
deriving stock instance IsPass p => Eq (CheckedMacroExpr p)

-- | Convert a @c-expr-dsl@ 'CExpr.TagKind' to an hs-bindgen 'CTagKind'
convertTagKind :: CExpr.TagKind -> CTagKind
convertTagKind = \case
    CExpr.TagStruct -> CTagKindStruct
    CExpr.TagUnion  -> CTagKindUnion
    CExpr.TagEnum   -> CTagKindEnum

{-------------------------------------------------------------------------------
  CoercePass for CheckedMacro (parametric; applies to any passes p, p')
-------------------------------------------------------------------------------}

instance (
      CoercePass CheckedMacroType p p'
    , CoercePass CheckedMacroExpr p p'
    ) => CoercePass CheckedMacro p p' where
  coercePass (MacroType typ)  = MacroType (coercePass typ)
  coercePass (MacroExpr expr) = MacroExpr (coercePass expr)

instance (
      CoercePass Type p p'
    , Ann "CheckedMacroType" p ~ Ann "CheckedMacroType" p'
    ) => CoercePass CheckedMacroType p p' where
  coercePass macroType = CheckedMacroType{
        typ = coercePass macroType.typ
      , ann = macroType.ann
      }

instance CoercePassId p p' => CoercePass CheckedMacroExpr p p' where
  coercePass e = CheckedMacroExpr{
        args = e.args
      , body = fmap (coercePassId (Proxy @'(p, p'))) e.body
      , typ  = e.typ
      }

{-------------------------------------------------------------------------------
  CoercePass: ConstructTranslationUnit → TypecheckMacros
-------------------------------------------------------------------------------}

instance CoercePassId               ConstructTranslationUnit TypecheckMacros
instance CoercePassMacroId          ConstructTranslationUnit TypecheckMacros where
    coercePassMacroId _ = absurd

instance CoercePassAnn "TypeFunArg"  ConstructTranslationUnit TypecheckMacros
instance CoercePassAnn "StructField" ConstructTranslationUnit TypecheckMacros
instance CoercePassAnn "UnionField"  ConstructTranslationUnit TypecheckMacros
instance CoercePassAnn "Typedef"     ConstructTranslationUnit TypecheckMacros
instance CoercePassAnn "Function"    ConstructTranslationUnit TypecheckMacros
instance CoercePassAnn "Global"      ConstructTranslationUnit TypecheckMacros
instance CoercePassCommentDecl       ConstructTranslationUnit TypecheckMacros where
  coercePassCommentDecl _ = fmap coercePass
