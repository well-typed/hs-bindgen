module HsBindgen.Frontend.Pass.TypecheckMacros.IsPass (
    TypecheckMacros
    -- * Checked macros
  , TypecheckedMacro(..)
  , TypecheckedMacroType(..)
  , TypecheckedMacroValue(..)
  , MacroTypeBodyVar(..)
  ) where

import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass (ReparseInfo, Tokens)
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Type

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type TypecheckMacros :: Pass
data TypecheckMacros a

-- | Macros have been typechecked ('MacroBody' is 'TypecheckedMacro TypecheckMacros').
-- Other declarations still carry their 'ReparseInfo' annotations from
-- 'ConstructTranslationUnit'; these are consumed in the following
-- 'ReparseMacroExpansions' pass.
type family AnnTypecheckMacros (ix :: Symbol) :: Star where
  AnnTypecheckMacros "StructField"     = ReparseInfo Tokens
  AnnTypecheckMacros "UnionField"      = ReparseInfo Tokens
  AnnTypecheckMacros "Typedef"         = ReparseInfo Tokens
  AnnTypecheckMacros "Function"        = ReparseInfo Tokens
  AnnTypecheckMacros "Global"          = ReparseInfo Tokens
  AnnTypecheckMacros _                 = NoAnn

instance IsPass TypecheckMacros

instance PassId TypecheckMacros

instance PassScopedName TypecheckMacros

instance PassMacro TypecheckMacros where
  type MacroId   TypecheckMacros = Id TypecheckMacros
  type MacroBody TypecheckMacros = TypecheckedMacro TypecheckMacros

  macroIdId _ = id

instance PassExtBinding TypecheckMacros

instance PassCommentDecl TypecheckMacros where
  type CommentDecl TypecheckMacros = Maybe (C.Comment TypecheckMacros)

instance PassAnn TypecheckMacros where
  type Ann ix TypecheckMacros = AnnTypecheckMacros ix

instance PassMsg TypecheckMacros

{-------------------------------------------------------------------------------
  Checked macros
-------------------------------------------------------------------------------}

-- 'TypecheckedMacro' is used as 'MacroBody p l', so the order of type
-- parameters is fixed.
data TypecheckedMacro p l =
    MacroType  (TypecheckedMacroType  l p)
  | MacroValue (TypecheckedMacroValue l p)

-- | Checked type macro
data TypecheckedMacroType l p = HasMacroTypes l => TypecheckedMacroType{
      body :: TypecheckedMacroTypeBody l (MacroTypeBodyVar p)
    , ann  :: Ann "TypecheckedMacroType" p
    }

data TypecheckedMacroValue l p = HasMacroTypes l => TypecheckedMacroValue {
      body :: TypecheckedMacroValueBody l (Id p)
    }

deriving stock instance IsPass p => Show (TypecheckedMacro      p l)
deriving stock instance IsPass p => Show (TypecheckedMacroType  l p)
deriving stock instance IsPass p => Show (TypecheckedMacroValue l p)

deriving stock instance IsPass p => Eq (TypecheckedMacro      p l)
deriving stock instance IsPass p => Eq (TypecheckedMacroType  l p)
deriving stock instance IsPass p => Eq (TypecheckedMacroValue l p)

-- | We have to be specific, when a macro type refers to an external binding.
data MacroTypeBodyVar p =
    MacroTypeExtBinding (ExtBinding p)
  | MacroTypeBodyVar    (Id p)

deriving stock instance IsPass p => Eq   (MacroTypeBodyVar p)
deriving stock instance IsPass p => Ord  (MacroTypeBodyVar p)
deriving stock instance IsPass p => Show (MacroTypeBodyVar p)

{-------------------------------------------------------------------------------
  CoercePass for TypecheckedMacro (parametric; applies to any passes p, p')
-------------------------------------------------------------------------------}

instance (
      CoercePassId p p'
    , ExtBinding p ~ ExtBinding p'
    , Ann "TypecheckedMacroType" p ~ Ann "TypecheckedMacroType" p'
    ) => CoercePassParam TypecheckedMacro p p' where
  coercePassParam = \case
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
    , Ann "TypecheckedMacroType" p ~ Ann "TypecheckedMacroType" p'
    ) => CoercePass (TypecheckedMacroType l) p p' where
  coercePass (TypecheckedMacroType body ann) =
    TypecheckedMacroType{
        body = fmap coercePass body
      , ann  = ann
      }

instance CoercePassId p p' => CoercePass (TypecheckedMacroValue l) p p' where
  coercePass (TypecheckedMacroValue body) =
    TypecheckedMacroValue{
        body = fmap (coercePassId (Proxy @'(p, p'))) body
      }

{-------------------------------------------------------------------------------
  CoercePass: ConstructTranslationUnit → TypecheckMacros
-------------------------------------------------------------------------------}

instance CoercePassId               ConstructTranslationUnit TypecheckMacros
instance CoercePassMacroId          ConstructTranslationUnit TypecheckMacros where
    coercePassMacroId _ = absurd
instance CoercePassMacroUnderlying  ConstructTranslationUnit TypecheckMacros

instance CoercePassAnn "TypeFunArg"  ConstructTranslationUnit TypecheckMacros
instance CoercePassAnn "StructField" ConstructTranslationUnit TypecheckMacros
instance CoercePassAnn "UnionField"  ConstructTranslationUnit TypecheckMacros
instance CoercePassAnn "Typedef"     ConstructTranslationUnit TypecheckMacros
instance CoercePassAnn "Function"    ConstructTranslationUnit TypecheckMacros
instance CoercePassAnn "Global"      ConstructTranslationUnit TypecheckMacros
instance CoercePassCommentDecl       ConstructTranslationUnit TypecheckMacros where
  coercePassCommentDecl _ = fmap coercePass
