module HsBindgen.Frontend.Pass.ConstructTranslationUnit (
    constructTranslationUnit
  ) where

import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

constructTranslationUnit ::
     C.TranslationUnit Parse
  -> (C.TranslationUnit ConstructTranslationUnit, [Msg ConstructTranslationUnit])
constructTranslationUnit unit@C.TranslationUnit{..} =
    let (declMeta, declIndexErrors) = mkDeclMeta unit
    in ( C.TranslationUnit{
             unitAnn   = declMeta
           , unitDecls = map coercePass $
                           UseDeclGraph.toDecls
                             (declIndex   declMeta)
                             (declUseDecl declMeta)
           , ..
           }
       , declIndexErrors
       )

mkDeclMeta :: C.TranslationUnit Parse -> (DeclMeta, [Msg ConstructTranslationUnit])
mkDeclMeta unit =
    let (declIndex, declIndexErrors) = DeclIndex.fromDecls unitDecls
        declUseDecl = UseDeclGraph.fromDecls unitIncludeGraph unitDecls
        declDeclUse = DeclUseGraph.fromUseDecl declUseDecl
    in ( DeclMeta{..}
       , map ConstructTranslationUnitErrorDeclIndex declIndexErrors
       )
  where
    C.TranslationUnit{
        unitDecls
      , unitIncludeGraph
      } = unit
