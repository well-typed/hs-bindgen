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
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.TranslationUnit qualified as C
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.EnrichComments.IsPass
import HsBindgen.Frontend.Pass.Parse.Result

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

type PreviousPass = EnrichComments

constructTranslationUnit ::
     [ParseResult PreviousPass]
  -> IncludeGraph
  -> C.TranslationUnit ConstructTranslationUnit
constructTranslationUnit parseResults includeGraph = C.TranslationUnit{
      decls        = map coercePass $
                           DeclUseGraph.toDecls
                             declMeta.declIndex
                             declMeta.declUseGraph
    , includeGraph = includeGraph
    , meta         = declMeta
    }
  where
    declMeta :: DeclMeta
    declMeta = mkDeclMeta parseResults includeGraph

mkDeclMeta ::
     [ParseResult PreviousPass]
  -> IncludeGraph
  -> DeclMeta
mkDeclMeta parseResults includeGraph = DeclMeta{
      declIndex    = declIndex
    , declUseGraph = declUseGraph
    , useDeclGraph = useDeclGraph
    }
  where
    declIndex :: DeclIndex
    declIndex = DeclIndex.fromParseResults parseResults

    declUseGraph :: DeclUseGraph
    declUseGraph = DeclUseGraph.fromDecls includeGraph declIndex

    useDeclGraph :: UseDeclGraph
    useDeclGraph = UseDeclGraph.fromDeclUseGraph declUseGraph
