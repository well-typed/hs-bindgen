module HsBindgen.Frontend.Pass.TypecheckMacros.IsPass (
    TypecheckMacros
    -- * Checked macros
  , CheckedMacro(..)
  , TypecheckedMacro(..)
  , TypecheckedMacroType(..)
  , TypecheckedMacroValue(..)
  , MacroTypeBodyVar(..)
  ) where

import HsBindgen.Frontend.Analysis (Dependency)
import HsBindgen.Frontend.MacroFallback
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass (ReparseInfo, Tokens)
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.IR.Pass.Types (CoercePassAnonRef)
import HsBindgen.Macro.Type qualified as Macro

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type TypecheckMacros :: Pass
data TypecheckMacros a

-- | Macros have been typechecked, but the fallback for the macros the macro
-- language gave up on is not decided yet ('MacroBody' is
-- @'PendingMacro' ('CheckedMacro' 'TypecheckMacros')@). Other declarations
-- still carry their 'ReparseInfo' annotations from 'ConstructTranslationUnit';
-- these are consumed in the 'ReparseMacroExpansions' pass.
type family AnnTypecheckMacros (ix :: Symbol) :: Star where
  AnnTypecheckMacros "IndirectField" = ReparseInfo Tokens
  AnnTypecheckMacros "Function"      = ReparseInfo Tokens
  AnnTypecheckMacros "Global"        = ReparseInfo Tokens
  AnnTypecheckMacros "RegularField"  = ReparseInfo Tokens
  AnnTypecheckMacros "Typedef"       = ReparseInfo Tokens
  AnnTypecheckMacros _               = NoAnn

instance IsPass TypecheckMacros

instance PassId TypecheckMacros

instance PassScopedName TypecheckMacros

instance PassTypes TypecheckMacros

instance PassMacro TypecheckMacros where
  type MacroId   TypecheckMacros = Id TypecheckMacros
  type MacroBody TypecheckMacros = PendingMacro (CheckedMacro TypecheckMacros)

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

-- | A macro the macro language typechecked
--
-- Used as @'PendingMacro' ('CheckedMacro' p) l@, so the order of type
-- parameters is fixed.
data CheckedMacro p l =
    CheckedType  (TypecheckedMacroType  l p)
  | CheckedValue (TypecheckedMacroValue l p)

-- 'TypecheckedMacro' is used as 'MacroBody p l', so the order of type
-- parameters is fixed.
data TypecheckedMacro p l =
    MacroType  (TypecheckedMacroType  l p)
  | MacroValue (TypecheckedMacroValue l p)
    -- | A macro the macro language gave up on, exported as raw token spellings
    --
    -- Always a value, never a type, so reparsing declarations that use it
    -- proceeds exactly as if the macro had been dropped: only macro-defined
    -- types are kept unexpanded.
  | MacroRaw FallbackMacro

-- | Checked type macro
data TypecheckedMacroType l p = Macro.HasTypes l => TypecheckedMacroType{
      body :: Macro.TypecheckedType l (MacroTypeBodyVar p)
    , deps :: [(C.DeclId, Dependency)]
    , ann  :: Ann "TypecheckedMacroType" p
    }

data TypecheckedMacroValue l p = Macro.HasTypes l => TypecheckedMacroValue {
      body :: Macro.TypecheckedValue l (Id p)
    , deps :: [(C.DeclId, Dependency)]
    }

deriving stock instance IsPass p => Show (CheckedMacro          p l)
deriving stock instance IsPass p => Show (TypecheckedMacro      p l)
deriving stock instance IsPass p => Show (TypecheckedMacroType  l p)
deriving stock instance IsPass p => Show (TypecheckedMacroValue l p)

deriving stock instance IsPass p => Eq (CheckedMacro          p l)
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
    MacroRaw   raw -> MacroRaw   raw

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
  coercePass (TypecheckedMacroType body deps ann) =
    TypecheckedMacroType{
        body = fmap coercePass body
      , deps = deps
      , ann  = ann
      }

instance CoercePassId p p' => CoercePass (TypecheckedMacroValue l) p p' where
  coercePass (TypecheckedMacroValue body deps) =
    TypecheckedMacroValue{
        body = fmap (coercePassId (Proxy @'(p, p'))) body
      , deps = deps
      }

{-------------------------------------------------------------------------------
  CoercePass: ConstructTranslationUnit → TypecheckMacros
-------------------------------------------------------------------------------}

instance CoercePassAnonRef          ConstructTranslationUnit TypecheckMacros
instance CoercePassId               ConstructTranslationUnit TypecheckMacros
instance CoercePassTypes            ConstructTranslationUnit TypecheckMacros
instance CoercePassMacroId          ConstructTranslationUnit TypecheckMacros where
    coercePassMacroId _ = absurd
instance CoercePassMacroUnderlying  ConstructTranslationUnit TypecheckMacros

instance CoercePassAnn "Function"      ConstructTranslationUnit TypecheckMacros
instance CoercePassAnn "Global"        ConstructTranslationUnit TypecheckMacros
instance CoercePassAnn "IndirectField" ConstructTranslationUnit TypecheckMacros
instance CoercePassAnn "RegularField"  ConstructTranslationUnit TypecheckMacros
instance CoercePassAnn "TypeFunArg"    ConstructTranslationUnit TypecheckMacros
instance CoercePassAnn "Typedef"       ConstructTranslationUnit TypecheckMacros
instance CoercePassCommentDecl         ConstructTranslationUnit TypecheckMacros where
  coercePassCommentDecl _ = fmap coercePass
