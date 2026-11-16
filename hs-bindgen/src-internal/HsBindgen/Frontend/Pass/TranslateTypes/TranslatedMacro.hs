module HsBindgen.Frontend.Pass.TranslateTypes.TranslatedMacro (
    TranslatedMacro (..)
  , TranslatedMacroType (..)
  ) where

import HsBindgen.Frontend.MacroFallback
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Pass

{-------------------------------------------------------------------------------
  Translated macros
-------------------------------------------------------------------------------}

data TranslatedMacro p l =
    TMacroType  (TranslatedMacroType p)
  | TMacroValue (TypecheckedMacroValue l p)
  | TMacroRaw FallbackMacro

data TranslatedMacroType p = TranslatedMacroType {
      typ :: Hs.Type
    , ann :: Ann "TranslatedMacroType" p
    }

deriving stock instance IsPass p => Show (TranslatedMacro p l)
deriving stock instance IsPass p => Show (TranslatedMacroType p)

deriving stock instance IsPass p => Eq (TranslatedMacro p l)
deriving stock instance IsPass p => Eq (TranslatedMacroType p)

{-------------------------------------------------------------------------------
  CoercePass for TranslatedMacro (parametric; applies to any passes p, p')
-------------------------------------------------------------------------------}

instance (
      CoercePassId p p'
    , Ann "TranslatedMacroType" p ~ Ann "TranslatedMacroType" p'
    ) => CoercePassParam TranslatedMacro p p' where
  coercePassParam = \case
    TMacroType  typ -> TMacroType  (coercePass typ)
    TMacroValue val -> TMacroValue (coercePass val)
    TMacroRaw   raw -> TMacroRaw   raw

instance (
      Ann "TranslatedMacroType" p ~ Ann "TranslatedMacroType" p'
    ) => CoercePass TranslatedMacroType p p' where
  coercePass (TranslatedMacroType typ ann) =
    TranslatedMacroType{
        typ = typ
      , ann = ann
      }
