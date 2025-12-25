module HsBindgen.Frontend.Pass.HandleMacros.IsPass (
    HandleMacros
  , HandleMacrosReparseMsg(..)
  ) where

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal (CheckedMacro)
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
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassId ConstructTranslationUnit HandleMacros
