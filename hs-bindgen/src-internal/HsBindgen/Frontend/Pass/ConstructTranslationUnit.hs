module HsBindgen.Frontend.Pass.ConstructTranslationUnit (
    constructTranslationUnit
  ) where

import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.EnrichComments.IsPass
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

type PreviousPass = EnrichComments

constructTranslationUnit ::
     forall l. Macro.HasTypes l
  => Macro.Lang l
  -> [ParseResult l PreviousPass]
  -> IncludeGraph
  -> C.TranslationUnit l ConstructTranslationUnit
constructTranslationUnit macroLang parseResults includeGraph = C.TranslationUnit{
      decls        = DeclUseGraph.toDecls
                       declMeta.declIndex
                       declMeta.declUseGraph
    , includeGraph = includeGraph
    , meta         = declMeta
    }
  where
    declMeta :: DeclMeta l
    declMeta = mkDeclMeta macroLang parseResults includeGraph

mkDeclMeta ::
     forall l. Macro.HasTypes l
  => Macro.Lang l
  -> [ParseResult l PreviousPass]
  -> IncludeGraph
  -> DeclMeta l
mkDeclMeta macroLang parseResults includeGraph = declMeta
  where
    declIndex :: DeclIndex l
    declIndex = DeclIndex.fromParseResults macroLang parseResults

    declUseGraph :: DeclUseGraph
    declUseGraph = DeclUseGraph.construct includeGraph declIndex

    useDeclGraph :: UseDeclGraph
    useDeclGraph = UseDeclGraph.fromDeclUseGraph declUseGraph

    declMeta :: DeclMeta l
    declMeta = DeclMeta{
      declIndex    = declIndex
    , declUseGraph = declUseGraph
    , useDeclGraph = useDeclGraph
    }
