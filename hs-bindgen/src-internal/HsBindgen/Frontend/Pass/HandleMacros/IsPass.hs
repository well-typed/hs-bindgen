module HsBindgen.Frontend.Pass.HandleMacros.IsPass (
    HandleMacros
  , HandleMacrosReparseMsg(..)
  ) where

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.HandleMacros.Error
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type HandleMacros :: Pass
data HandleMacros a deriving anyclass ValidPass

-- We do not need the @ReparseInfo@ anymore, so we drop it from the annotations.
type family AnnHandleMacros (ix :: Symbol) :: Star where
  AnnHandleMacros "TranslationUnit" = DeclMeta
  AnnHandleMacros _                 = NoAnn

instance IsPass HandleMacros where
  type Id           HandleMacros = C.PrelimDeclId
  type FieldName    HandleMacros = C.ScopedName
  type ArgumentName HandleMacros = Maybe C.ScopedName
  type MacroBody    HandleMacros = CheckedMacro HandleMacros
  type ExtBinding   HandleMacros = Void
  type Ann ix       HandleMacros = AnnHandleMacros ix
  type Msg          HandleMacros = HandleMacrosReparseMsg

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassId ConstructTranslationUnit HandleMacros where
  coercePassId _ = id

instance CoercePassHaskellId ConstructTranslationUnit HandleMacros where
  coercePassHaskellId _ = id
