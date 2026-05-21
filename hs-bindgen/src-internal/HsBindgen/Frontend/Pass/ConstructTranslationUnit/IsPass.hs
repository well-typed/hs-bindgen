module HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass (
    ConstructTranslationUnit
  ) where

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.EnrichComments.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition

  Inspect the parse results and construct the translation unit.
-------------------------------------------------------------------------------}

type ConstructTranslationUnit :: Pass
data ConstructTranslationUnit a

type family AnnConstructTranslationUnit (ix :: Symbol) :: Star where
  AnnConstructTranslationUnit "StructField"     = ReparseInfo
  AnnConstructTranslationUnit "UnionField"      = ReparseInfo
  AnnConstructTranslationUnit "Typedef"         = ReparseInfo
  AnnConstructTranslationUnit "Function"        = ReparseInfo
  AnnConstructTranslationUnit "Global"          = ReparseInfo
  AnnConstructTranslationUnit _                 = NoAnn

instance IsPass ConstructTranslationUnit where
  type MacroBody   ConstructTranslationUnit = ParsedMacro
  type ExtBinding  ConstructTranslationUnit = Void
  type Ann ix      ConstructTranslationUnit = AnnConstructTranslationUnit ix
  type Msg         ConstructTranslationUnit = NoMsg Level
  type CommentDecl ConstructTranslationUnit = Maybe (C.Comment ConstructTranslationUnit)

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassMacroBody          EnrichComments ConstructTranslationUnit
instance CoercePassId                 EnrichComments ConstructTranslationUnit
instance CoercePassMacroId            EnrichComments ConstructTranslationUnit

instance CoercePassAnn "TypeFunArg"   EnrichComments ConstructTranslationUnit
instance CoercePassAnn "StructField"  EnrichComments ConstructTranslationUnit
instance CoercePassAnn "UnionField"   EnrichComments ConstructTranslationUnit
instance CoercePassAnn "Typedef"      EnrichComments ConstructTranslationUnit
instance CoercePassAnn "Function"     EnrichComments ConstructTranslationUnit
instance CoercePassAnn "Global"       EnrichComments ConstructTranslationUnit
instance CoercePassCommentDecl        EnrichComments ConstructTranslationUnit where
  coercePassCommentDecl _ = fmap coercePass
