module HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.LanC.IsPass (
    LanC
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

type LanC :: Pass
data LanC a

-- We do not need the @ReparseInfo@ anymore, so we drop it from the annotations.
type family AnnLanC (ix :: Symbol) :: Star where
  AnnLanC "Function"    = BeforeReparse (C.Function    PrepareReparse)
  AnnLanC "Global"      = BeforeReparse (C.Global      PrepareReparse)
  AnnLanC "StructField" = BeforeReparse (C.StructField PrepareReparse)
  AnnLanC "Typedef"     = BeforeReparse (C.Typedef     PrepareReparse)
  AnnLanC "UnionField"  = BeforeReparse (C.UnionField  PrepareReparse)
  AnnLanC _             = NoAnn

instance IsPass LanC

instance PassId LanC

instance PassTypes LanC

instance PassScopedName LanC

instance PassMacro LanC where
  type MacroId         LanC = Id LanC
  type MacroBody       LanC = TypecheckedMacro LanC
  type MacroUnderlying LanC = ()

  macroIdId _ = id

instance PassExtBinding LanC

instance PassCommentDecl LanC where
  type CommentDecl LanC = Maybe (C.Comment LanC)

instance PassAnn LanC where
  type Ann ix LanC = AnnLanC ix

instance PassMsg LanC

-- TODO <https://github.com/well-typed/hs-bindgen/issues/2024>
--
-- Right now, we always store the "before reparse" information. Hence, we have
-- to align all declarations, even the ones we did not reparse successfully. We
-- could add a second constructor to 'BeforeReparse' that conveys: We used the
-- fallback here, no need to align.
newtype BeforeReparse a = BeforeReparse { unwrap :: a }
  deriving stock (Eq, Ord, Show)

{-------------------------------------------------------------------------------
  CoercePass: PrepareReparse → LanC
-------------------------------------------------------------------------------}

instance CoercePassId               PrepareReparse LanC
instance CoercePassTypes            PrepareReparse LanC
instance CoercePassMacroId          PrepareReparse LanC
instance CoercePassAnn "TypeFunArg" PrepareReparse LanC
instance CoercePassCommentDecl      PrepareReparse LanC where
  coercePassCommentDecl _ = fmap coercePass

instance CoercePassMacroBody PrepareReparse LanC where
  coercePassMacroBody _ = coercePassParam

instance CoercePassMacroUnderlying PrepareReparse  LanC where
  coercePassMacroUnderlying _ = absurd
