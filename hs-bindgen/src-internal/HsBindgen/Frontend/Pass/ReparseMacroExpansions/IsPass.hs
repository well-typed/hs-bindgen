module HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass (
    ReparseMacroExpansions
  ) where

import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.Zip.IsPass (Zip)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type ReparseMacroExpansions :: Pass
data ReparseMacroExpansions a

-- We do not need the @ReparseInfo@ anymore, so we drop it from the annotations.
type family AnnReparseMacroExpansions (ix :: Symbol) :: Star where
  AnnReparseMacroExpansions _                 = NoAnn

instance IsPass ReparseMacroExpansions

instance PassId ReparseMacroExpansions

instance PassTypes ReparseMacroExpansions

instance PassScopedName ReparseMacroExpansions

instance PassMacro ReparseMacroExpansions where
  type MacroId         ReparseMacroExpansions = Id ReparseMacroExpansions
  type MacroBody       ReparseMacroExpansions = TypecheckedMacro ReparseMacroExpansions
  type MacroUnderlying ReparseMacroExpansions = C.Type ReparseMacroExpansions

  macroIdId _ = id

instance PassExtBinding ReparseMacroExpansions

instance PassCommentDecl ReparseMacroExpansions where
  type CommentDecl ReparseMacroExpansions = Maybe (C.Comment ReparseMacroExpansions)

instance PassAnn ReparseMacroExpansions where
  type Ann ix ReparseMacroExpansions = AnnReparseMacroExpansions ix

instance PassMsg ReparseMacroExpansions

{-------------------------------------------------------------------------------
  CoercePass: Zip → ReparseMacroExpansions
-------------------------------------------------------------------------------}

instance CoercePassId               Zip ReparseMacroExpansions
instance CoercePassTypes            Zip ReparseMacroExpansions
instance CoercePassMacroId          Zip ReparseMacroExpansions
instance CoercePassAnn "TypeFunArg" Zip ReparseMacroExpansions
instance CoercePassCommentDecl      Zip ReparseMacroExpansions where
  coercePassCommentDecl _ = fmap coercePass

instance CoercePassMacroBody Zip ReparseMacroExpansions where
  coercePassMacroBody _ = coercePassParam

instance CoercePassMacroUnderlying Zip  ReparseMacroExpansions where
  coercePassMacroUnderlying _ = coercePass
