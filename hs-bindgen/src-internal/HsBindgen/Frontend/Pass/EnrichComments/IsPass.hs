module HsBindgen.Frontend.Pass.EnrichComments.IsPass (
    EnrichComments
  ) where

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass (AssignAnonIds)
import HsBindgen.Frontend.Pass.Parse.IsPass (ParsedMacro, ReparseInfo, Tokens)
import HsBindgen.Imports
import HsBindgen.Util.Tracer

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

instance IsPass EnrichComments where
  type MacroBody   EnrichComments = ParsedMacro
  type ExtBinding  EnrichComments = Void
  type Ann ix      EnrichComments = AnnEnrichComments ix
  type Msg         EnrichComments = NoMsg Level
  type CommentDecl EnrichComments = Maybe (C.Comment EnrichComments)

{-------------------------------------------------------------------------------
  CoercePass

  EnrichComments has the same associated types as AssignAnonIds (same Id,
  ScopedName, MacroBody, ExtBinding, MacroId, Ann). The only difference is
  'CommentDecl', so all trivial helpers can use the default identity, and
  'CoercePassCommentDecl' needs a custom instance that sets the comment to
  'Nothing' (since @CommentDecl AssignAnonIds = ()@ and
  @CommentDecl EnrichComments = Maybe (Comment EnrichComments)@).
-------------------------------------------------------------------------------}

instance CoercePassId               AssignAnonIds EnrichComments
instance CoercePassMacroBody        AssignAnonIds EnrichComments
instance CoercePassMacroId          AssignAnonIds EnrichComments
instance CoercePassAnn "TypeFunArg" AssignAnonIds EnrichComments
instance CoercePassAnn "Global"     AssignAnonIds EnrichComments

instance CoercePassCommentDecl AssignAnonIds EnrichComments where
  coercePassCommentDecl _ () = Nothing
