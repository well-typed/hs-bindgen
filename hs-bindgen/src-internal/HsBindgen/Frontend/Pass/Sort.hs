module HsBindgen.Frontend.Pass.Sort (sortDecls) where

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Graph.UseDecl (UseDeclGraph)
import HsBindgen.Frontend.Graph.UseDecl qualified as UseDeclGraph
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Sort.IsPass

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

sortDecls :: C.TranslationUnit Parse -> C.TranslationUnit Sort
sortDecls C.TranslationUnit{..} = C.TranslationUnit{
      unitDecls = map coercePass $ UseDeclGraph.toDecls useDeclGraph
    , unitAnn   = (useDeclGraph, unitAnn)
    , unitIncludeGraph
    }
  where
    useDeclGraph :: UseDeclGraph
    useDeclGraph = UseDeclGraph.fromDecls unitIncludeGraph unitDecls
