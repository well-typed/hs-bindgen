module HsBindgen.Frontend.Pass.Zip.IsPass (
    Zip
  ) where

import HsBindgen.Frontend.Pass.PrepareReparse.IsPass
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass
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

instance C.CoercePassId               PrepareReparse Zip
instance C.CoercePassMacroId          PrepareReparse Zip
instance C.CoercePassCommentDecl      PrepareReparse Zip where
  coercePassCommentDecl _ = fmap C.coercePass

instance C.CoercePassId               ReparseMacroExpansions Zip
instance C.CoercePassMacroId          ReparseMacroExpansions Zip
instance C.CoercePassAnn "TypeFunArg" ReparseMacroExpansions Zip
instance C.CoercePassCommentDecl      ReparseMacroExpansions Zip where
  coercePassCommentDecl _ = fmap C.coercePass

instance C.CoercePassMacroBody ReparseMacroExpansions Zip where
  coercePassMacroBody _ = C.coercePassParam
