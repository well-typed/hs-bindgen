module HsBindgen.Frontend.Analysis.DeclUseGraph.Definition (
    DeclUseGraph(..)
  , toDigraph
    -- * Visualization
  , renderMermaid
  ) where

import Data.Digraph (Digraph)
import Data.Digraph qualified as Digraph

import HsBindgen.Frontend.Analysis
import HsBindgen.IR.C qualified as C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Declaration definition-usage graph
--
-- This graph has edges from definition sites to use sites.
data DeclUseGraph = DeclUseGraph {
      graph :: Digraph Dependency C.DeclId
    }
  deriving stock (Eq, Show)

toDigraph :: DeclUseGraph -> Digraph Dependency C.DeclId
toDigraph declUseGraph = declUseGraph.graph

{-------------------------------------------------------------------------------
  Visualization
-------------------------------------------------------------------------------}

renderMermaid :: DeclUseGraph -> String
renderMermaid declUseGraph = Digraph.renderMermaid opts declUseGraph.graph
  where
    opts :: Digraph.VisOptions Dependency C.DeclId
    opts = Digraph.VisOptions{
        visVertex = \v -> Digraph.VisVertex{
            label = Just (show v)
          }
      , visEdge   = \e -> Digraph.VisEdge{
            label = Just (show e)
          , style = Digraph.Solid
          }
      , reverseEdges = False
      }
