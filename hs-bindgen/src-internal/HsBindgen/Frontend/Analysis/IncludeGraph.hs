-- | Include graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
-- > import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
module HsBindgen.Frontend.Analysis.IncludeGraph (
    IncludeGraph(..)
    -- * Construction
  , empty
  , register
  , fromList
    -- * Query
  , reaches
  , toSortedList
  , getMainPath
    -- * Debugging
  , dumpMermaid
  ) where

import Data.List qualified as List
import Data.Set (Set)

import Clang.Paths
import Data.DynGraph (DynGraph)
import Data.DynGraph qualified as DynGraph
import HsBindgen.Frontend.RootHeader qualified as RootHeader

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Include graph
--
-- We create a DAG of C header paths with an edge for each @#include@.
-- The edges are /reversed/ to represent an \"included by\" relation.
newtype IncludeGraph = IncludeGraph (DynGraph SourcePath)
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: IncludeGraph
empty = IncludeGraph DynGraph.empty

register ::
     SourcePath -- ^ Path of header that includes the following header
  -> SourcePath -- ^ Path of the included header
  -> IncludeGraph
  -> IncludeGraph
register header incHeader (IncludeGraph graph) =
    IncludeGraph $ DynGraph.insertEdge incHeader header graph

fromList :: [(SourcePath, SourcePath)] -> IncludeGraph
fromList edges = List.foldl' add empty edges
  where
    add :: IncludeGraph -> (SourcePath, SourcePath) -> IncludeGraph
    add graph (fr, to) = register fr to graph

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

reaches :: IncludeGraph -> SourcePath -> Set SourcePath
reaches (IncludeGraph graph) = DynGraph.reaches graph

toSortedList :: IncludeGraph -> [SourcePath]
toSortedList (IncludeGraph graph) =
    List.delete RootHeader.name $ DynGraph.topSort graph

getMainPath ::
     Set SourcePath
  -> IncludeGraph
  -> SourcePath
  -> Maybe SourcePath
getMainPath mainPaths (IncludeGraph graph) =
    DynGraph.dfFindMember mainPaths graph

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

dumpMermaid :: IncludeGraph -> String
dumpMermaid (IncludeGraph graph) = DynGraph.dumpMermaid getSourcePath graph
