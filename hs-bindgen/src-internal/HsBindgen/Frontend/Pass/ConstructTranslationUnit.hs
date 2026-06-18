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
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro.Interface
import HsBindgen.Macro.Type

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

type PreviousPass = EnrichComments

constructTranslationUnit ::
     forall l. HasMacroTypes l
  => MacroLang l
  -> [ParseResult l PreviousPass]
  -> IncludeGraph
  -> C.TranslationUnit l ConstructTranslationUnit
constructTranslationUnit macroLang parseResults includeGraph = C.TranslationUnit{
      decls        = map C.coercePass $
                           DeclUseGraph.toDecls
                             declMeta.declIndex
                             declMeta.declUseGraph
    , includeGraph = includeGraph
    , meta         = declMeta
    }
  where
    declMeta :: DeclMeta l
    declMeta = mkDeclMeta macroLang parseResults includeGraph

mkDeclMeta ::
     forall l. HasMacroTypes l
  => MacroLang l
  -> [ParseResult l PreviousPass]
  -> IncludeGraph
  -> DeclMeta l
mkDeclMeta macroLang parseResults includeGraph = DeclMeta{
      declIndex    = declIndex
    , declUseGraph = declUseGraph
    , useDeclGraph = useDeclGraph
    }
  where
    declIndex :: DeclIndex l
    declIndex = DeclIndex.fromParseResults parseResults

    declUseGraph :: DeclUseGraph
    declUseGraph = DeclUseGraph.fromDecls macroLang includeGraph declIndex

    useDeclGraph :: UseDeclGraph
    useDeclGraph = UseDeclGraph.fromDeclUseGraph declUseGraph
