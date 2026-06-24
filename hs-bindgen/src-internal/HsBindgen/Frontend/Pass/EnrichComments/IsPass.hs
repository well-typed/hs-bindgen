module HsBindgen.Frontend.Pass.EnrichComments.IsPass (
    EnrichComments
  ) where

import HsBindgen.Frontend.Pass.FillUnnamedIds.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.IR.Pass.Types (CoercePassAnonRef)
import HsBindgen.Macro.Interface qualified as Macro

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type EnrichComments :: Pass
data EnrichComments a

-- We preserve the annotations from the @Parse@ pass (same as @FillUnnamedIds@)
type family AnnEnrichComments ix where
  AnnEnrichComments "Function"      = ReparseInfo Tokens
  AnnEnrichComments "Global"        = ReparseInfo Tokens
  AnnEnrichComments "IndirectField" = ReparseInfo Tokens
  AnnEnrichComments "RegularField"  = ReparseInfo Tokens
  AnnEnrichComments "Typedef"       = ReparseInfo Tokens
  AnnEnrichComments _               = NoAnn

instance IsPass EnrichComments

instance PassId EnrichComments

instance PassScopedName EnrichComments

instance PassTypes EnrichComments

instance PassMacro EnrichComments where
  type MacroBody EnrichComments = Macro.Unresolved

instance PassExtBinding EnrichComments

instance PassCommentDecl EnrichComments where
  type CommentDecl EnrichComments = Maybe (C.Comment EnrichComments)

instance PassAnn EnrichComments where
  type Ann ix EnrichComments = AnnEnrichComments ix

instance PassMsg EnrichComments

{-------------------------------------------------------------------------------
  CoercePass

  EnrichComments has the same associated types as FillUnnamedIds (same Id,
  ScopedName, MacroBody, ExtBinding, MacroId, Ann). The only difference is
  'C.CommentDecl', so all trivial helpers can use the default identity, and
  'C.CoercePassCommentDecl' needs a custom instance that sets the comment to
  'Nothing' (since @CommentDecl FillUnnamedIds = ()@ and
  @CommentDecl EnrichComments = Maybe (Comment EnrichComments)@).
-------------------------------------------------------------------------------}

instance CoercePassAnonRef             FillUnnamedIds EnrichComments
instance CoercePassId                  FillUnnamedIds EnrichComments
instance CoercePassMacroBody           FillUnnamedIds EnrichComments
instance CoercePassMacroId             FillUnnamedIds EnrichComments
instance CoercePassMacroUnderlying     FillUnnamedIds EnrichComments
instance CoercePassTypes               FillUnnamedIds EnrichComments
instance CoercePassAnn "IndirectField" FillUnnamedIds EnrichComments
instance CoercePassAnn "Global"        FillUnnamedIds EnrichComments
instance CoercePassAnn "TypeFunArg"    FillUnnamedIds EnrichComments

instance CoercePassCommentDecl FillUnnamedIds EnrichComments where
  coercePassCommentDecl _ () = Nothing
