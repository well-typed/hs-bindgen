module HsBindgen.Frontend.Pass.EnrichComments.IsPass (
    EnrichComments
  ) where

import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Type

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type EnrichComments :: Pass
data EnrichComments a

-- We preserve the annotations from the @Parse@ pass (same as @AssignAnonIds@)
type family AnnEnrichComments ix where
  AnnEnrichComments "StructField" = ReparseInfo Tokens
  AnnEnrichComments "UnionField"  = ReparseInfo Tokens
  AnnEnrichComments "Typedef"     = ReparseInfo Tokens
  AnnEnrichComments "Function"    = ReparseInfo Tokens
  AnnEnrichComments "Global"      = ReparseInfo Tokens
  AnnEnrichComments _             = NoAnn

instance IsPass EnrichComments

instance PassId EnrichComments

instance PassScopedName EnrichComments

instance PassMacro EnrichComments where
  type MacroBody EnrichComments = ParsedMacroBody

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

instance C.CoercePassId               AssignAnonIds EnrichComments
instance C.CoercePassMacroBody        AssignAnonIds EnrichComments
instance C.CoercePassMacroId          AssignAnonIds EnrichComments
instance C.CoercePassMacroUnderlying  AssignAnonIds EnrichComments
instance C.CoercePassAnn "TypeFunArg" AssignAnonIds EnrichComments
instance C.CoercePassAnn "Global"     AssignAnonIds EnrichComments

instance C.CoercePassCommentDecl AssignAnonIds EnrichComments where
  coercePassCommentDecl _ () = Nothing
