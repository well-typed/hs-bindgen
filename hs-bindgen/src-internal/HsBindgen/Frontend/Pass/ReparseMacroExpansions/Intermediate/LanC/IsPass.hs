module HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.LanC.IsPass (
    ReparseMacroExpansions
  , BeforeReparse(..)
  ) where

import HsBindgen.Frontend.Pass.PrepareReparse.IsPass (PrepareReparse)
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

-- TODO <https://github.com/well-typed/hs-bindgen/issues/2024>
--
-- Right now, we always store the "before reparse" information. Hence, we have
-- to align all declarations, even the ones we did not reparse successfully. We
-- could add a second constructor to 'BeforeReparse' that conveys: We used the
-- fallback here, no need to align.
newtype BeforeReparse a = BeforeReparse { unwrap :: a }
  deriving stock (Eq, Ord, Show)

{-------------------------------------------------------------------------------
  CoercePass: TypecheckMacros → ReparseMacroExpansions
-------------------------------------------------------------------------------}

instance CoercePassId               TypecheckMacros ReparseMacroExpansions
instance CoercePassMacroId          TypecheckMacros ReparseMacroExpansions
instance CoercePassAnn "TypeFunArg" TypecheckMacros ReparseMacroExpansions
instance CoercePassCommentDecl      TypecheckMacros ReparseMacroExpansions where
  coercePassCommentDecl _ = fmap coercePass

{-------------------------------------------------------------------------------
  CoercePass: PrepareReparse → ReparseMacroExpansions
-------------------------------------------------------------------------------}

instance CoercePassId               PrepareReparse ReparseMacroExpansions
instance CoercePassMacroId          PrepareReparse ReparseMacroExpansions
instance CoercePassAnn "TypeFunArg" PrepareReparse ReparseMacroExpansions
instance CoercePassCommentDecl      PrepareReparse ReparseMacroExpansions where
  coercePassCommentDecl _ = fmap coercePass

-- instance CoercePassMacroBody TypecheckMacros ReparseMacroExpansions where
--   coercePassMacroBody _ = coercePassParam

instance CoercePassMacroUnderlying TypecheckMacros ReparseMacroExpansions where
  coercePassMacroUnderlying _ = absurd
instance CoercePassMacroUnderlying PrepareReparse  ReparseMacroExpansions where
  coercePassMacroUnderlying _ = absurd
