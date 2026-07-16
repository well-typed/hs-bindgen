module HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass (
    ReparseMacroExpansions
  ) where

import HsBindgen.Frontend.Pass.PrepareReparse.IsPass (PrepareReparse)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.LanC (LanC)
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
  CoercePass: PrepareReparse → ReparseMacroExpansions
-------------------------------------------------------------------------------}

instance CoercePassId                  PrepareReparse ReparseMacroExpansions
instance CoercePassMacroId             PrepareReparse ReparseMacroExpansions
instance CoercePassAnn "TypeFunArg"    PrepareReparse ReparseMacroExpansions

instance CoercePassCommentDecl PrepareReparse ReparseMacroExpansions where
  coercePassCommentDecl _ = fmap coercePass

instance CoercePassMacroBody PrepareReparse ReparseMacroExpansions where
  coercePassMacroBody _ = coercePassParam

instance CoercePassMacroUnderlying PrepareReparse  ReparseMacroExpansions where
  coercePassMacroUnderlying _ = absurd

{-------------------------------------------------------------------------------
  CoercePass: LanC → ReparseMacroExpansions
-------------------------------------------------------------------------------}

instance CoercePassId               LanC ReparseMacroExpansions
instance CoercePassMacroId          LanC ReparseMacroExpansions
instance CoercePassAnn "TypeFunArg" LanC ReparseMacroExpansions
instance CoercePassCommentDecl      LanC ReparseMacroExpansions where
  coercePassCommentDecl _ = fmap coercePass

instance CoercePassMacroBody LanC ReparseMacroExpansions where
  coercePassMacroBody _ = coercePassParam
