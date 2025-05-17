-- | Include graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.C.Raw.Graph.Includes (IncludeGraph)
-- > import HsBindgen.C.Raw.Graph.Includes qualified as IncludeGraph
module HsBindgen.C.Raw.Graph.Includes (
    IncludeGraph
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
import HsBindgen.C.Raw.RootHeader qualified as RootHeader

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Include graph
--
-- We create a DAG of C header paths with an edge for each @#include@.
-- The edges are /reversed/ to represent an \"included by\" relation.
type IncludeGraph = DynGraph SourcePath

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: IncludeGraph
empty = DynGraph.empty

register ::
     SourcePath -- ^ Path of header that includes the following header
  -> SourcePath -- ^ Path of the included header
  -> IncludeGraph
  -> IncludeGraph
register header incHeader =
    DynGraph.insertEdge incHeader header

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

toSortedList :: IncludeGraph -> [SourcePath]
toSortedList = List.delete RootHeader.name . DynGraph.topSort