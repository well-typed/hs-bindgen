module HsBindgen.Frontend.Pass.ConstructTranslationUnit (
    constructTranslationUnit
  ) where

import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.Parse.Result

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

constructTranslationUnit ::
     [ParseResult AssignAnonIds]
  -> IncludeGraph
  -> (C.TranslationUnit ConstructTranslationUnit, [Msg ConstructTranslationUnit])
constructTranslationUnit parseResults includeGraph =
    let (declMeta, declIndexErrors) = mkDeclMeta parseResults includeGraph
    in ( C.TranslationUnit{
             unitDecls        = map coercePass $
                                  UseDeclGraph.toDecls
                                    (declIndex   declMeta)
                                    (declUseDecl declMeta)
           , unitIncludeGraph = includeGraph
           , unitAnn          = declMeta
           , ..
           }
       , declIndexErrors
       )

mkDeclMeta ::
     [ParseResult AssignAnonIds]
  -> IncludeGraph
  -> (DeclMeta, [Msg ConstructTranslationUnit])
mkDeclMeta parseResults includeGraph =
    let declIndex = DeclIndex.fromParseResults parseResults
        declUseDecl =
          UseDeclGraph.fromDecls includeGraph $ DeclIndex.getDecls declIndex
        declDeclUse = DeclUseGraph.fromUseDecl declUseDecl
    in (DeclMeta{..}, [])
