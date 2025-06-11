module HsBindgen.Frontend.Pass.Sort (sortDecls) where

import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Sort.IsPass

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

sortDecls :: C.TranslationUnit Parse -> C.TranslationUnit Sort
sortDecls unit@C.TranslationUnit{..} = C.TranslationUnit{
      unitAnn   = declMeta
    , unitDecls = map coercePass $
                    UseDeclGraph.toDecls
                      (declIndex declMeta)
                      (declUsage declMeta)
    , ..
    }
  where
    declMeta :: DeclMeta
    declMeta = mkDeclMeta unit

{-------------------------------------------------------------------------------
  Information about declarations
-------------------------------------------------------------------------------}

mkDeclMeta :: C.TranslationUnit Parse -> DeclMeta
mkDeclMeta unit =
    DeclMeta{
        declIndex       = DeclIndex.fromDecls unitDecls
      , declUsage       = UseDeclGraph.fromDecls unitIncludeGraph unitDecls
      , declNonSelected = nonSelected
      }
  where
    C.TranslationUnit{
        unitDecls
      , unitIncludeGraph
      , unitAnn = nonSelected
      } = unit

