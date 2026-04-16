module HsBindgen.Frontend.Analysis.UseDeclGraph.Definition (
    UseDeclGraph(..)
  , toDynGraph
    -- * Debugging
  , dumpMermaid
  ) where

import Data.DynGraph.Labelled (DynGraph)
import Data.DynGraph.Labelled qualified as DynGraph

import HsBindgen.Frontend.AST.Type (ValOrRef)
import HsBindgen.Frontend.Naming

-- | ValOrRef-definition graph
--
-- Whenever declaration A uses (depends on) declaration B, there will be
-- an edge from A to B in this graph.
data UseDeclGraph = UseDeclGraph {
      graph :: DynGraph ValOrRef DeclId
    }
  deriving stock (Show, Eq)

toDynGraph :: UseDeclGraph -> DynGraph ValOrRef DeclId
toDynGraph useDeclGraph = useDeclGraph.graph

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

dumpMermaid :: UseDeclGraph -> String
dumpMermaid useDeclGraph =
    DynGraph.dumpMermaid
      DynGraph.MermaidOptions{
          reverseEdges = False
        , renderVertex = Just . show
        , renderEdge   = \e ->
            DynGraph.EdgeSpec DynGraph.Straight (Just $ show e)
        }
      useDeclGraph.graph
