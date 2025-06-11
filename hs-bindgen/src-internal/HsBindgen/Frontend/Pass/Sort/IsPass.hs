module HsBindgen.Frontend.Pass.Sort.IsPass (Sort) where

import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.AST.Internal (ValidPass)
import HsBindgen.Frontend.NonSelectedDecls (NonSelectedDecls)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse)
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition

  The only thing that changes in this pass is the annotation on the top-level
  'TranslationUnit'; for everything else we simply refer to the 'Parse' pass.
-------------------------------------------------------------------------------}

type Sort :: Pass
data Sort a deriving anyclass ValidPass

type family AnalyzeAnn (ix :: Symbol) :: Star where
  AnalyzeAnn "TranslationUnit" = (UseDeclGraph, NonSelectedDecls)
  AnalyzeAnn ix                = Ann ix Parse

instance IsPass Sort where
  type Id         Sort = Id         Parse
  type FieldName  Sort = FieldName  Parse
  type TypedefRef Sort = TypedefRef Parse
  type MacroBody  Sort = MacroBody  Parse
  type Ann ix     Sort = AnalyzeAnn ix
