module HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.Zip.IsPass (
    Zip
  ) where

import HsBindgen.Frontend.Pass.PrepareReparse.IsPass
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.LanC.IsPass (LanC)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Zip :: Pass
data Zip a

type family AnnZip (ix :: Symbol) :: Star where
  AnnZip _ = NoAnn

instance IsPass Zip

instance PassId Zip

instance PassScopedName Zip

instance PassTypes Zip

instance PassMacro Zip where
  type MacroId         Zip = Id Zip
  type MacroBody       Zip = TypecheckedMacro Zip
  type MacroUnderlying Zip = C.Type Zip

  macroIdId _ = id

instance PassExtBinding Zip

instance PassCommentDecl Zip where
  type CommentDecl Zip = Maybe (C.Comment Zip)

instance PassAnn Zip where
  type Ann ix Zip = AnnZip ix

instance PassMsg Zip

{-------------------------------------------------------------------------------
  Coerce
-------------------------------------------------------------------------------}

instance CoercePassId               PrepareReparse Zip
instance CoercePassMacroId          PrepareReparse Zip
instance CoercePassCommentDecl      PrepareReparse Zip where
  coercePassCommentDecl _ = fmap coercePass

instance CoercePassId               LanC Zip
instance CoercePassMacroId          LanC Zip
instance CoercePassAnn "TypeFunArg" LanC Zip
instance CoercePassCommentDecl      LanC Zip where
  coercePassCommentDecl _ = fmap coercePass

instance CoercePassMacroBody LanC Zip where
  coercePassMacroBody _ = coercePassParam
