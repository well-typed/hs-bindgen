module HsBindgen.Frontend.Pass.Zip.IsPass (
    Zip
  ) where

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Zip :: Pass
data Zip a

type family AnnZip (ix :: Symbol) :: Star where
  AnnZip _ = NoAnn

instance IsPass Zip where
  type MacroBody       Zip = TypecheckedMacro Zip
  type Ann ix          Zip = AnnZip ix
  type Msg             Zip = NoMsg Level
  type MacroId         Zip = Id Zip
  type CommentDecl     Zip = Maybe (C.Comment Zip)
  type MacroUnderlying Zip = C.Type Zip
  macroIdId _ = id

{-------------------------------------------------------------------------------
  Coerce
-------------------------------------------------------------------------------}

instance CoercePassId               PrepareReparse Zip
instance CoercePassMacroId          PrepareReparse Zip
instance CoercePassCommentDecl      PrepareReparse Zip where
  coercePassCommentDecl _ = fmap coercePass

instance CoercePassId               ReparseMacroExpansions Zip
instance CoercePassMacroId          ReparseMacroExpansions Zip
instance CoercePassAnn "TypeFunArg" ReparseMacroExpansions Zip
instance CoercePassCommentDecl      ReparseMacroExpansions Zip where
  coercePassCommentDecl _ = fmap coercePass

instance CoercePassMacroBody ReparseMacroExpansions Zip where
  coercePassMacroBody _ = coercePassParam
