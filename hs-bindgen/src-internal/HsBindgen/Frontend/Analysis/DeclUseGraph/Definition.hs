module HsBindgen.Frontend.Analysis.DeclUseGraph.Definition (
    -- * Definition
    DeclUseGraph(..)
  , toDigraph
    -- * Visualization
  , renderMermaid
  ) where

import Data.Digraph (Digraph)
import Data.Digraph qualified as Digraph

import HsBindgen.Frontend.AST.Type (ValOrRef)
import HsBindgen.Frontend.Naming

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Declaration definition-usage graph
--
-- This graph has edges from definition sites to use sites.
data DeclUseGraph = DeclUseGraph {
      graph :: Digraph ValOrRef DeclId
    }
  deriving stock (Show, Eq)

toDigraph :: DeclUseGraph -> Digraph ValOrRef DeclId
toDigraph declUseGraph = declUseGraph.graph

{-------------------------------------------------------------------------------
  Visualization
-------------------------------------------------------------------------------}

renderMermaid :: DeclUseGraph -> String
renderMermaid declUseGraph = Digraph.renderMermaid opts declUseGraph.graph
  where
    opts :: Digraph.VisOptions ValOrRef DeclId
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
