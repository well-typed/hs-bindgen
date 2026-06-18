module HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass (
    ConstructTranslationUnit
  ) where

import HsBindgen.Frontend.Pass.EnrichComments.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Type

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
  type MacroBody ConstructTranslationUnit = ParsedMacroBody

instance PassExtBinding ConstructTranslationUnit

instance PassCommentDecl ConstructTranslationUnit where
  type CommentDecl ConstructTranslationUnit = Maybe (C.Comment ConstructTranslationUnit)

instance PassAnn ConstructTranslationUnit where
  type Ann ix ConstructTranslationUnit = AnnConstructTranslationUnit ix

instance PassMsg ConstructTranslationUnit

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance C.CoercePassMacroBody          EnrichComments ConstructTranslationUnit
instance C.CoercePassId                 EnrichComments ConstructTranslationUnit
instance C.CoercePassMacroId            EnrichComments ConstructTranslationUnit
instance C.CoercePassMacroUnderlying    EnrichComments ConstructTranslationUnit

instance C.CoercePassAnn "TypeFunArg"   EnrichComments ConstructTranslationUnit
instance C.CoercePassAnn "StructField"  EnrichComments ConstructTranslationUnit
instance C.CoercePassAnn "UnionField"   EnrichComments ConstructTranslationUnit
instance C.CoercePassAnn "Typedef"      EnrichComments ConstructTranslationUnit
instance C.CoercePassAnn "Function"     EnrichComments ConstructTranslationUnit
instance C.CoercePassAnn "Global"       EnrichComments ConstructTranslationUnit
instance C.CoercePassCommentDecl        EnrichComments ConstructTranslationUnit where
  coercePassCommentDecl _ = fmap C.coercePass
