module HsBindgen.Frontend.Pass.TypecheckMacros.IsPass (
    TypecheckMacros
    -- * Checked macros
  , CheckedMacro(..)
  , CheckedMacroType(..)
  , CheckedMacroValue(..)
  , MacroTypeBodyVar(..)
    -- * Conversions
  , convertTagKind
  ) where

import C.Expr.Syntax qualified as CExpr
import C.Expr.Typecheck qualified as CExpr

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass (ReparseInfo, Tokens)
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
  AnnTypecheckMacros "StructField"     = ReparseInfo Tokens
  AnnTypecheckMacros "UnionField"      = ReparseInfo Tokens
  AnnTypecheckMacros "Typedef"         = ReparseInfo Tokens
  AnnTypecheckMacros "Function"        = ReparseInfo Tokens
  AnnTypecheckMacros "Global"          = ReparseInfo Tokens
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
    MacroType  (CheckedMacroType p)
  | MacroValue (CheckedMacroValue p)

-- | Checked type macro
data CheckedMacroType p = CheckedMacroType{
      typ :: CExpr.CheckedMacroTypeExpr (MacroTypeBodyVar p)
    , ann :: Ann "CheckedMacroType" p
    }

newtype CheckedMacroValue p = CheckedMacroValue {
      val :: CExpr.CheckedMacroValueExpr (Id p)
    }

deriving stock instance IsPass p => Show (CheckedMacro     p)
deriving stock instance IsPass p => Show (CheckedMacroType p)
deriving stock instance IsPass p => Show (CheckedMacroValue p)

deriving stock instance IsPass p => Eq (CheckedMacro     p)
deriving stock instance IsPass p => Eq (CheckedMacroType p)
deriving stock instance IsPass p => Eq (CheckedMacroValue p)

-- | We have to be specific, when a macro type refers to an external binding.
data MacroTypeBodyVar p =
    MacroTypeExtBinding (ExtBinding p)
  | MacroTypeBodyVar    (Id p)

deriving stock instance IsPass p => Eq   (MacroTypeBodyVar p)
deriving stock instance IsPass p => Ord  (MacroTypeBodyVar p)
deriving stock instance IsPass p => Show (MacroTypeBodyVar p)

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
    , CoercePassId p p'
    ) => CoercePass CheckedMacro p p' where
  coercePass = \case
    MacroType  typ -> MacroType  (coercePass typ)
    MacroValue val -> MacroValue (coercePass val)

instance (
      CoercePassId p p'
    , ExtBinding p ~ ExtBinding p'
    ) => CoercePass MacroTypeBodyVar p p' where
  coercePass = \case
    MacroTypeExtBinding x -> MacroTypeExtBinding x
    MacroTypeBodyVar    x -> MacroTypeBodyVar (coercePassId (Proxy @'(p, p')) x)

instance (
      CoercePassId p p'
    , ExtBinding p ~ ExtBinding p'
    , Ann "CheckedMacroType" p ~ Ann "CheckedMacroType" p'
    ) => CoercePass CheckedMacroType p p' where
  coercePass (CheckedMacroType (CExpr.CheckedMacroTypeExpr body typ) ann) =
    let body' = fmap coercePass body
    in  CheckedMacroType{
          typ = CExpr.CheckedMacroTypeExpr body' typ
        , ann = ann
        }

instance CoercePassId p p' => CoercePass CheckedMacroValue p p' where
  coercePass (CheckedMacroValue (CExpr.CheckedMacroValueExpr params body typ)) =
    let body' = fmap (coercePassId (Proxy @'(p, p'))) body
    in  CheckedMacroValue{
            val = CExpr.CheckedMacroValueExpr params body' typ
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
