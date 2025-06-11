module HsBindgen.Frontend.Pass.HandleMacros.IsPass (
    HandleMacros
  ) where

import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Sort.IsPass (DeclMeta)
import HsBindgen.Imports
import HsBindgen.Language.C

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
  type Id         HandleMacros = DeclId
  type FieldName  HandleMacros = CName
  type TypedefRef HandleMacros = CName
  type MacroBody  HandleMacros = CheckedMacro HandleMacros
  type Ann ix     HandleMacros = AnnHandleMacros ix
