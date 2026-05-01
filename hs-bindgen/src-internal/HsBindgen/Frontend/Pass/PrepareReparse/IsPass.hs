-- | Definitions for the @PrepareReparse@ pass
--
-- This module is intended to be imported unqualified.
--
-- > import HsBindgen.Frontend.Pass.PrepareReparse.IsPass
module HsBindgen.Frontend.Pass.PrepareReparse.IsPass (
    PrepareReparse
  ) where

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass (DeclMeta)
import HsBindgen.Frontend.Pass.Parse.IsPass (ReparseInfo, Tokens)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type PrepareReparse :: Pass
data PrepareReparse a

type family AnnPrepareReparse (ix :: Symbol) :: Star where
  AnnPrepareReparse "TranslationUnit" = DeclMeta
  AnnPrepareReparse "StructField"     = ReparseInfo Tokens
  AnnPrepareReparse "UnionField"      = ReparseInfo Tokens
  AnnPrepareReparse "Typedef"         = ReparseInfo Tokens
  AnnPrepareReparse "Function"        = ReparseInfo Tokens
  AnnPrepareReparse "Global"          = ReparseInfo Tokens
  AnnPrepareReparse _                 = NoAnn

instance IsPass PrepareReparse where
  type MacroBody   PrepareReparse = CheckedMacro PrepareReparse
  type Ann ix      PrepareReparse = AnnPrepareReparse ix
  type Msg         PrepareReparse = NoMsg Level
  type MacroId     PrepareReparse = Id PrepareReparse
  type CommentDecl PrepareReparse = Maybe (C.Comment PrepareReparse)
  macroIdId _ = id

{-------------------------------------------------------------------------------
  CoercePass: TypecheckMacros → PrepareReparse
-------------------------------------------------------------------------------}

instance CoercePassId               TypecheckMacros PrepareReparse
instance CoercePassMacroId          TypecheckMacros PrepareReparse
instance CoercePassAnn "TypeFunArg" TypecheckMacros PrepareReparse

instance CoercePassCommentDecl      TypecheckMacros PrepareReparse where
  coercePassCommentDecl _ = fmap coercePass

instance CoercePassMacroBody        TypecheckMacros PrepareReparse where
  coercePassMacroBody _ = \case
      MacroType ty -> MacroType $ coercePass ty
      MacroExpr expr -> MacroExpr $ coercePass expr
