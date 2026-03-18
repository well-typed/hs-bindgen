module Data.DynGraph (
    -- * Type
    DynGraph
    -- * Construction
  , empty
  , fromLabelled
    -- * Insertion
  , insertVertex
  , insertEdge
    -- * Query
  , vertices
  , reaches
  , topSort
  , dff
  , dfFindMember
    -- * Deletion
  , filterVerticesCombineEdges
    -- * Debugging
  , MermaidOptions(..)
  , dumpMermaid
  ) where

import Data.DynGraph.Labelled qualified as Labelled
import Data.Set (Set)
import Data.Tree (Tree)

{-------------------------------------------------------------------------------
  Type
-------------------------------------------------------------------------------}

type DynGraph = Labelled.DynGraph ()

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | The empty graph
empty :: DynGraph a
empty = Labelled.empty

fromLabelled :: Labelled.DynGraph l a -> DynGraph a
fromLabelled = Labelled.mapEdges $ const ()

{-------------------------------------------------------------------------------
  Insertion
-------------------------------------------------------------------------------}

-- | Insert a vertex
--
-- The graph is not changed if the vertex already exists.
insertVertex :: Ord a => a -> DynGraph a -> DynGraph a
insertVertex = Labelled.insertVertex

-- | Insert an edge
--
-- This function inserts vertices automatically.
--
-- The graph is not changed if the edge already exists.
insertEdge :: forall a. Ord a => a -> a -> DynGraph a -> DynGraph a
insertEdge = flip Labelled.insertEdge ()

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Gets the vertices in the graph
vertices :: DynGraph a -> [a]
vertices = Labelled.vertices

-- | Gets the set of vertices that are reachable from any of the specified
-- vertices
--
-- The specified vertices are included in the set (assuming that they are in
-- the graph).
reaches :: Ord a => DynGraph a -> [a] -> Set a
reaches = Labelled.reaches

-- | Gets a topological sort of the graph
topSort :: DynGraph a -> [a]
topSort = Labelled.topSort

-- | Gets the spanning forest of the graph obtained from a depth-first search of
-- the graph starting from each vertex in insertion order
dff :: DynGraph a -> [Tree a]
dff = Labelled.dff

-- | Find the first vertex in the specified set in a depth-first traversal of
-- the graph starting from the specified vertex
--
-- This function is specific to equality so that more can be done in the index
-- domain, for performance.
dfFindMember :: Ord a => Set a -> DynGraph a -> a -> Maybe a
dfFindMember = Labelled.dfFindMember

{-------------------------------------------------------------------------------
  Deletion
-------------------------------------------------------------------------------}

-- | Retain vertices that satisfy the predicate.
--
-- If possible, combine dangling (i.e., transitive) edges.
--
-- For example, assume
--
--   A-->B-->C
--       |
--       +-->D
--
-- Removal of vertex 'B' creates
--
--   A------>C
--   |
--   +------>D
filterVerticesCombineEdges :: Ord a => (a -> Bool) -> DynGraph a -> DynGraph a
filterVerticesCombineEdges p = Labelled.filterVerticesCombineEdges p (\_ _ -> ())

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

data MermaidOptions a = MermaidOptions{
      reverseEdges :: Bool
    , renderVertex :: a -> Maybe String
    }

-- | Render a Mermaid diagram
--
-- See https://mermaid.js.org/>
dumpMermaid :: MermaidOptions a -> DynGraph a -> String
dumpMermaid opts =
    Labelled.dumpMermaid Labelled.MermaidOptions{
        reverseEdges = opts.reverseEdges
      , renderVertex = opts.renderVertex
      , renderEdge   = \() -> Labelled.EdgeSpec Labelled.Straight Nothing
      }
