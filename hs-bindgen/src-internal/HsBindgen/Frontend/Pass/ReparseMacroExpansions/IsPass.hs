module HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass (
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

instance C.CoercePassId               TypecheckMacros ReparseMacroExpansions
instance C.CoercePassMacroId          TypecheckMacros ReparseMacroExpansions
instance C.CoercePassAnn "TypeFunArg" TypecheckMacros ReparseMacroExpansions
instance C.CoercePassCommentDecl      TypecheckMacros ReparseMacroExpansions where
  coercePassCommentDecl _ = fmap C.coercePass

{-------------------------------------------------------------------------------
  CoercePass: PrepareReparse → ReparseMacroExpansions
-------------------------------------------------------------------------------}

instance C.CoercePassId               PrepareReparse ReparseMacroExpansions
instance C.CoercePassMacroId          PrepareReparse ReparseMacroExpansions
instance C.CoercePassAnn "TypeFunArg" PrepareReparse ReparseMacroExpansions
instance C.CoercePassCommentDecl      PrepareReparse ReparseMacroExpansions where
  coercePassCommentDecl _ = fmap C.coercePass

-- instance C.CoercePassMacroBody TypecheckMacros ReparseMacroExpansions where
--   coercePassMacroBody _ = C.coercePassParam

instance C.CoercePassMacroUnderlying TypecheckMacros ReparseMacroExpansions where
  coercePassMacroUnderlying _ = absurd
instance C.CoercePassMacroUnderlying PrepareReparse  ReparseMacroExpansions where
  coercePassMacroUnderlying _ = absurd
