module HsBindgen.Frontend.Pass.ConstructTranslationUnit (
    constructTranslationUnit
  ) where

import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.EnrichComments.IsPass
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Predicate (IsMainHeader)
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro
import HsBindgen.Macro.UniqueExpansion qualified as UniqueExpansion

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

type PreviousPass = EnrichComments

constructTranslationUnit ::
     forall l. Macro.HasTypes l
  => Macro.Lang l
  -> UniqueExpansion.Analysis
  -> IsMainHeader
  -> [ParseResult l PreviousPass]
  -> IncludeGraph
  -> C.TranslationUnit l ConstructTranslationUnit
constructTranslationUnit macroLang macroAnalysis isMainHeader parseResults includeGraph =
    C.TranslationUnit{
        includeGraph = includeGraph
      , db           =
          DeclDB.fromParseResults macroLang macroAnalysis isMainHeader parseResults
      }
