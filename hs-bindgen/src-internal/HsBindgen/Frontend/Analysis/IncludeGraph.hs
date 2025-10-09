-- | Include graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
-- > import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
module HsBindgen.Frontend.Analysis.IncludeGraph (
    IncludeGraph(..)
  , Include(..)
  , includeArg
    -- * Construction
  , empty
  , register
  , fromList
    -- * Query
  , reaches
  , toSortedList
  , getIncludes
    -- * Debugging
  , Predicate
  , dumpMermaid
  ) where

import Data.DynGraph.Labelled (DynGraph)
import Data.DynGraph.Labelled qualified as DynGraph
import Data.List qualified as List
import Data.Set (Set)

import Clang.Paths

import HsBindgen.Frontend.RootHeader (HashIncludeArg (getHashIncludeArg))
import HsBindgen.Frontend.RootHeader qualified as RootHeader

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Include graph
--
-- We create a DAG of C header paths with an edge for each @#include@.
-- The edges are /reversed/ to represent an \"included by\" relation.
newtype IncludeGraph = IncludeGraph (DynGraph Include SourcePath)
  deriving stock (Show, Eq)

-- | Include directive as written in the source
data Include =
    BracketInclude     HashIncludeArg  -- ^ @#include <...>@
  | QuoteInclude       HashIncludeArg  -- ^ @#include "..."@
  | BracketIncludeNext HashIncludeArg  -- ^ @#include_next <...>@
  | QuoteIncludeNext   HashIncludeArg  -- ^ @#include_next "..."@
  deriving stock (Show, Eq, Ord)

-- | Get the 'HashIncludeArg' for an 'Include'
includeArg :: Include -> HashIncludeArg
includeArg = \case
    BracketInclude     arg -> arg
    QuoteInclude       arg -> arg
    BracketIncludeNext arg -> arg
    QuoteIncludeNext   arg -> arg

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: IncludeGraph
empty = IncludeGraph DynGraph.empty

register ::
     SourcePath -- ^ Path of header that includes the following header
  -> Include
  -> SourcePath -- ^ Path of the included header
  -> IncludeGraph
  -> IncludeGraph
register header include incHeader (IncludeGraph graph) =
    IncludeGraph $ DynGraph.insertEdge incHeader include header graph

fromList :: [(SourcePath, Include, SourcePath)] -> IncludeGraph
fromList edges = List.foldl' add empty edges
  where
    add :: IncludeGraph -> (SourcePath, Include, SourcePath) -> IncludeGraph
    add graph (fr, inc, to) = register fr inc to graph

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

reaches :: IncludeGraph -> SourcePath -> Set SourcePath
reaches (IncludeGraph graph) = DynGraph.reaches graph . List.singleton

toSortedList :: IncludeGraph -> [SourcePath]
toSortedList (IncludeGraph graph) =
    List.delete RootHeader.name $ DynGraph.topSort graph

getIncludes ::
     IncludeGraph
  -> SourcePath
  -> DynGraph.FindEdgesResult Include
getIncludes (IncludeGraph graph) = DynGraph.findEdges graph

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

-- | Include graph predicate
--
-- An include graph predicate can be used to filter which indexes of an include
-- graph are shown.
type Predicate = SourcePath -> Bool

dumpMermaid :: Predicate -> IncludeGraph -> String
dumpMermaid p (IncludeGraph graph) =
    DynGraph.dumpMermaid True p (Just . renderInclude) getSourcePath graph
  where
    renderInclude :: Include -> String
    renderInclude = \case
      BracketInclude     i -> "#include <"       ++ getHashIncludeArg i ++ ">"
      QuoteInclude       i -> "#include \""      ++ getHashIncludeArg i ++ "\""
      BracketIncludeNext i -> "#include_next <"  ++ getHashIncludeArg i ++ ">"
      QuoteIncludeNext   i -> "#include_next \"" ++ getHashIncludeArg i ++ "\""
