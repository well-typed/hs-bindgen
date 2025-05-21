-- | Include graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Graph.Includes (IncludeGraph)
-- > import HsBindgen.Frontend.Graph.Includes qualified as IncludeGraph
module HsBindgen.Frontend.Graph.Includes (
    IncludeGraph(..)
    -- * Construction
  , empty
  , register
    -- * Query
  , toSortedList
  ) where

import Data.List qualified as List

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
  deriving stock (Show)

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

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

toSortedList :: IncludeGraph -> [SourcePath]
toSortedList (IncludeGraph graph) =
    List.delete RootHeader.name $ DynGraph.topSort graph