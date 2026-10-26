module HsBindgen.Frontend.Pass.ConstructTranslationUnit (
    constructTranslationUnit
  ) where

import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.DeclDB (DeclDB)
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.DeclMeta
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
constructTranslationUnit macroLang macroAnalysis isMainHeader parseResults includeGraph = C.TranslationUnit{
      decls        = DeclUseGraph.sortDecls
                       includeGraph
                       declMeta.declDB
                       declMeta.declUseGraph
                       (DeclDB.getDecls declMeta.declDB)
    , includeGraph = includeGraph
    , meta         = declMeta
    }
  where
    declMeta :: DeclMeta l
    declMeta =
      mkDeclMeta macroLang macroAnalysis isMainHeader parseResults

mkDeclMeta ::
     forall l. Macro.HasTypes l
  => Macro.Lang l
  -> UniqueExpansion.Analysis
  -> IsMainHeader
  -> [ParseResult l PreviousPass]
  -> DeclMeta l
mkDeclMeta macroLang macroAnalysis isMainHeader parseResults = declMeta
  where
    declDB :: DeclDB l ConstructTranslationUnit
    declDB =
      DeclDB.fromParseResults macroLang macroAnalysis isMainHeader parseResults

    declUseGraph :: DeclUseGraph
    declUseGraph = DeclUseGraph.construct declDB

    useDeclGraph :: UseDeclGraph
    useDeclGraph = UseDeclGraph.fromDeclUseGraph declUseGraph

    declMeta :: DeclMeta l
    declMeta = DeclMeta{
      declDB       = declDB
    , declUseGraph = declUseGraph
    , useDeclGraph = useDeclGraph
    }
