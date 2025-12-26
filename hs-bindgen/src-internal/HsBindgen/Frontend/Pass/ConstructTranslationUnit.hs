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
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.Parse.Result

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

constructTranslationUnit ::
     [ParseResult AssignAnonIds]
  -> IncludeGraph
  -> C.TranslationUnit ConstructTranslationUnit
constructTranslationUnit parseResults includeGraph = C.TranslationUnit{
      decls        = map coercePass $
                           UseDeclGraph.toDecls
                             declMeta.declIndex
                             declMeta.useDeclGraph
    , includeGraph = includeGraph
    , ann          = declMeta
    }
  where
    declMeta :: DeclMeta
    declMeta = mkDeclMeta parseResults includeGraph

mkDeclMeta ::
     [ParseResult AssignAnonIds]
  -> IncludeGraph
  -> DeclMeta
mkDeclMeta parseResults includeGraph = DeclMeta{
      declIndex
    , declUseGraph
    , useDeclGraph
    }
  where
    declIndex :: DeclIndex
    declIndex = DeclIndex.fromParseResults parseResults

    useDeclGraph :: UseDeclGraph
    useDeclGraph =
      UseDeclGraph.fromDecls includeGraph $ DeclIndex.getDecls declIndex

    declUseGraph :: DeclUseGraph
    declUseGraph = DeclUseGraph.fromUseDecl useDeclGraph
