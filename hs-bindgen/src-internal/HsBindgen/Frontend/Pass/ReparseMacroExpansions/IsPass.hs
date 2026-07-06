module HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass (
    ReparseMacroExpansions
  , BeforeReparse(..)
  ) where

import HsBindgen.Frontend.Pass.PrepareReparse.IsPass (PrepareReparse)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.LanC.IsPass
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
  AnnReparseMacroExpansions "StructField"     = BeforeReparse (C.StructField PrepareReparse)
  AnnReparseMacroExpansions "UnionField"      = BeforeReparse (C.UnionField  PrepareReparse)
  AnnReparseMacroExpansions "Typedef"         = BeforeReparse (C.Typedef     PrepareReparse)
  AnnReparseMacroExpansions "Function"        = BeforeReparse (C.Function    PrepareReparse)
  AnnReparseMacroExpansions "Global"          = BeforeReparse (C.Global      PrepareReparse)
  AnnReparseMacroExpansions _                 = NoAnn

instance IsPass ReparseMacroExpansions

instance PassId ReparseMacroExpansions

instance PassScopedName ReparseMacroExpansions

instance PassMacro ReparseMacroExpansions where
  type MacroId         ReparseMacroExpansions = Id ReparseMacroExpansions
  type MacroBody       ReparseMacroExpansions = TypecheckedMacro ReparseMacroExpansions
  type MacroUnderlying ReparseMacroExpansions = ()

  macroIdId _ = id

instance PassExtBinding ReparseMacroExpansions

instance PassCommentDecl ReparseMacroExpansions where
  type CommentDecl ReparseMacroExpansions = Maybe (C.Comment ReparseMacroExpansions)

instance PassAnn ReparseMacroExpansions where
  type Ann ix ReparseMacroExpansions = AnnReparseMacroExpansions ix

instance PassMsg ReparseMacroExpansions

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

instance CoercePassMacroUnderlying LanC  ReparseMacroExpansions where
  coercePassMacroUnderlying _ = id
