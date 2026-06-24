module HsBindgen.Frontend.Pass.EnrichComments.IsPass (
    EnrichComments
  ) where

import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Type qualified as Macro

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type EnrichComments :: Pass
data EnrichComments a

-- We preserve the annotations from the @Parse@ pass (same as @AssignAnonIds@)
type family AnnEnrichComments ix where
  AnnEnrichComments "Function"    = ReparseInfo Tokens
  AnnEnrichComments "Global"      = ReparseInfo Tokens
  AnnEnrichComments "StructField" = ReparseInfo Tokens
  AnnEnrichComments "Typedef"     = ReparseInfo Tokens
  AnnEnrichComments "UnionField"  = ReparseInfo Tokens
  AnnEnrichComments _             = NoAnn

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

  EnrichComments has the same associated types as AssignAnonIds (same Id,
  ScopedName, MacroBody, ExtBinding, MacroId, Ann). The only difference is
  'C.CommentDecl', so all trivial helpers can use the default identity, and
  'C.CoercePassCommentDecl' needs a custom instance that sets the comment to
  'Nothing' (since @CommentDecl AssignAnonIds = ()@ and
  @CommentDecl EnrichComments = Maybe (Comment EnrichComments)@).
-------------------------------------------------------------------------------}

instance CoercePassId               AssignAnonIds EnrichComments
instance CoercePassTypes            AssignAnonIds EnrichComments
instance CoercePassMacroBody        AssignAnonIds EnrichComments
instance CoercePassMacroId          AssignAnonIds EnrichComments
instance CoercePassMacroUnderlying  AssignAnonIds EnrichComments
instance CoercePassAnn "TypeFunArg" AssignAnonIds EnrichComments
instance CoercePassAnn "Global"     AssignAnonIds EnrichComments

instance CoercePassCommentDecl AssignAnonIds EnrichComments where
  coercePassCommentDecl _ () = Nothing
