module HsBindgen.Frontend.Pass.Sort (
    sortDecls
  ) where

import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.NonParsedDecls (NonParsedDecls)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Sort.IsPass

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

sortDecls ::
     C.TranslationUnit Parse
  -> (C.TranslationUnit Sort, [Msg Sort])
sortDecls unit@C.TranslationUnit{..} =
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

mkDeclMeta :: C.TranslationUnit Parse -> (DeclMeta Sort, [Msg Sort])
mkDeclMeta unit =
    let (declIndex, declIndexErrors) = DeclIndex.fromDecls unitDecls
        declUseDecl = UseDeclGraph.fromDecls unitIncludeGraph unitDecls
        declDeclUse = DeclUseGraph.fromUseDecl declUseDecl
    in ( DeclMeta{..}
       , map SortErrorDeclIndex declIndexErrors
       )
  where
    C.TranslationUnit{
        unitDecls
      , unitIncludeGraph
      , unitAnn = parseUnitAnn
      } = unit

    declNonParsed :: NonParsedDecls
    declNonParsed = parseDeclNonParsed parseUnitAnn

    declParseMsgs :: ParseMsgs Sort
    declParseMsgs = coerceParseMsgs $ parseDeclParseMsg parseUnitAnn
