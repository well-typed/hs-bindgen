module HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass (
    ConstructTranslationUnit
  , DeclMeta(..)
  ) where

import HsBindgen.Frontend.Analysis.DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph.Definition (UseDeclGraph)
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
  AnnConstructTranslationUnit "TranslationUnit" = DeclMeta
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
  Information about the declarations
-------------------------------------------------------------------------------}

data DeclMeta = DeclMeta {
      declIndex    :: DeclIndex
    , useDeclGraph :: UseDeclGraph
    , declUseGraph :: DeclUseGraph
    }
  deriving stock (Show, Generic)

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
