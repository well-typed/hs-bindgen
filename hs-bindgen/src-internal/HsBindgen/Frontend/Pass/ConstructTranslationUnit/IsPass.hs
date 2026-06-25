module HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass (
    ConstructTranslationUnit
  ) where

import HsBindgen.Frontend.Pass.EnrichComments.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Type qualified as Macro

{-------------------------------------------------------------------------------
  Definition

  Inspect the parse results and construct the translation unit.
-------------------------------------------------------------------------------}

type ConstructTranslationUnit :: Pass
data ConstructTranslationUnit a

type family AnnConstructTranslationUnit (ix :: Symbol) :: Star where
  AnnConstructTranslationUnit "StructField"     = ReparseInfo Tokens
  AnnConstructTranslationUnit "UnionField"      = ReparseInfo Tokens
  AnnConstructTranslationUnit "Typedef"         = ReparseInfo Tokens
  AnnConstructTranslationUnit "Function"        = ReparseInfo Tokens
  AnnConstructTranslationUnit "Global"          = ReparseInfo Tokens
  AnnConstructTranslationUnit _                 = NoAnn

instance IsPass ConstructTranslationUnit

instance PassId ConstructTranslationUnit

instance PassScopedName ConstructTranslationUnit

instance PassMacro ConstructTranslationUnit where
  type MacroBody ConstructTranslationUnit = Macro.Resolved

instance PassExtBinding ConstructTranslationUnit

instance PassCommentDecl ConstructTranslationUnit where
  type CommentDecl ConstructTranslationUnit = Maybe (C.Comment ConstructTranslationUnit)

instance PassAnn ConstructTranslationUnit where
  type Ann ix ConstructTranslationUnit = AnnConstructTranslationUnit ix

instance PassMsg ConstructTranslationUnit

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassId                 EnrichComments ConstructTranslationUnit
instance CoercePassMacroId            EnrichComments ConstructTranslationUnit
instance CoercePassMacroUnderlying    EnrichComments ConstructTranslationUnit

instance CoercePassAnn "TypeFunArg"   EnrichComments ConstructTranslationUnit
instance CoercePassAnn "StructField"  EnrichComments ConstructTranslationUnit
instance CoercePassAnn "UnionField"   EnrichComments ConstructTranslationUnit
instance CoercePassAnn "Typedef"      EnrichComments ConstructTranslationUnit
instance CoercePassAnn "Function"     EnrichComments ConstructTranslationUnit
instance CoercePassAnn "Global"       EnrichComments ConstructTranslationUnit
instance CoercePassCommentDecl        EnrichComments ConstructTranslationUnit where
  coercePassCommentDecl _ = fmap coercePass
