module HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass (
    ReparseMacroExpansions
  ) where

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type ReparseMacroExpansions :: Pass
data ReparseMacroExpansions a

-- We do not need the @ReparseInfo@ anymore, so we drop it from the annotations.
type family AnnReparseMacroExpansions (ix :: Symbol) :: Star where
  AnnReparseMacroExpansions "TranslationUnit" = DeclMeta
  AnnReparseMacroExpansions _                 = NoAnn

instance IsPass ReparseMacroExpansions where
  type MacroBody  ReparseMacroExpansions = CheckedMacro ReparseMacroExpansions
  type Ann ix     ReparseMacroExpansions = AnnReparseMacroExpansions ix
  type Msg        ReparseMacroExpansions = NoMsg Level
  type MacroId    ReparseMacroExpansions = Id ReparseMacroExpansions
  macroIdId _ = id

{-------------------------------------------------------------------------------
  CoercePass: TypecheckMacros → ReparseMacroExpansions
-------------------------------------------------------------------------------}

instance CoercePassId               TypecheckMacros ReparseMacroExpansions
instance CoercePassMacroId          TypecheckMacros ReparseMacroExpansions
instance CoercePassAnn "TypeFunArg" TypecheckMacros ReparseMacroExpansions
