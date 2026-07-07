module HsBindgen.Frontend.Pass.ReparseMacroExpansions.LanC (
    LanC
  ) where

import Data.Void (absurd)

import HsBindgen.Frontend.Pass.PrepareReparse.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type LanC :: Pass
data LanC a


instance IsPass LanC

instance PassId LanC

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
  type Ann ix LanC = NoAnn

instance PassMsg LanC

{-------------------------------------------------------------------------------
  CoercePass: PrepareReparse → LanC
-------------------------------------------------------------------------------}

instance CoercePassId               PrepareReparse LanC
instance CoercePassMacroId          PrepareReparse LanC
instance CoercePassAnn "TypeFunArg" PrepareReparse LanC
instance CoercePassCommentDecl      PrepareReparse LanC where
  coercePassCommentDecl _ = fmap coercePass

instance CoercePassMacroBody PrepareReparse LanC where
  coercePassMacroBody _ = coercePassParam

instance CoercePassMacroUnderlying PrepareReparse  LanC where
  coercePassMacroUnderlying _ = absurd
