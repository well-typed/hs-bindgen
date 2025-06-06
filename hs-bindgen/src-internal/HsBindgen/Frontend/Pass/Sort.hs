module HsBindgen.Frontend.Pass.Sort (sortDecls) where

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Graph.UseDef (UseDefGraph)
import HsBindgen.Frontend.Graph.UseDef qualified as UseDefGraph
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Sort.IsPass

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

sortDecls :: C.TranslationUnit Parse -> C.TranslationUnit Sort
sortDecls C.TranslationUnit{..} = C.TranslationUnit{
      unitDecls = map coercePass $ UseDefGraph.toDecls useDefGraph
    , unitAnn   = (useDefGraph, unitAnn)
    , unitIncludeGraph
    }
  where
    useDefGraph :: UseDefGraph
    useDefGraph = UseDefGraph.fromDecls unitIncludeGraph unitDecls

