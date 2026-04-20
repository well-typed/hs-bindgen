-- | Usage-declaration graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
-- > import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
module HsBindgen.Frontend.Analysis.UseDeclGraph (
    -- * Definition
    UseDeclGraph -- opaque
    -- * Construction
  , fromDeclUseGraph
    -- * Query
  , getTransitiveDeps
  , getStrictTransitiveDeps
    -- * Visualization
  , renderMermaid
  ) where

import Data.Digraph (Digraph)
import Data.Digraph qualified as Digraph
import Data.Set qualified as Set

import HsBindgen.Frontend.Analysis.DeclUseGraph.Definition (DeclUseGraph)
import HsBindgen.Frontend.Analysis.DeclUseGraph.Definition qualified as DeclUseGraph
import HsBindgen.Frontend.AST.Type (ValOrRef)
import HsBindgen.Frontend.Naming
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Declaration usage-definition graph
--
-- Whenever declaration @A@ uses (depends on) declaration @B@, there is an edge
-- from @A@ to @B@ in this graph.
data UseDeclGraph = UseDeclGraph {
      graph :: Digraph ValOrRef DeclId
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromDeclUseGraph :: DeclUseGraph -> UseDeclGraph
fromDeclUseGraph declUseGraph = UseDeclGraph{
      graph = Digraph.transpose $ DeclUseGraph.toDigraph declUseGraph
    }

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

getTransitiveDeps :: UseDeclGraph -> Set DeclId -> Set DeclId
getTransitiveDeps useDeclGraph decls = Digraph.reaches decls useDeclGraph.graph

getStrictTransitiveDeps :: UseDeclGraph -> Set DeclId -> Set DeclId
getStrictTransitiveDeps graph decls = getTransitiveDeps graph decls Set.\\ decls

{-------------------------------------------------------------------------------
  Visualization
-------------------------------------------------------------------------------}

renderMermaid :: UseDeclGraph -> String
renderMermaid useDeclGraph = Digraph.renderMermaid opts useDeclGraph.graph
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
