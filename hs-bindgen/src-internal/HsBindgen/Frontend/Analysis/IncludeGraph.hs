-- | Include graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
-- > import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
module HsBindgen.Frontend.Analysis.IncludeGraph (
    IncludeGraph(..)
  , Include(..)
  , MacroArg
  , getIncludeArg
  , getIncludeMacroArg
    -- * Construction
  , empty
  , register
  , fromList
    -- * Query
  , reaches
  , toSortedList
  , toOrderMap
  , getIncludes
    -- * Debugging
  , Predicate
  , DumpOpts(..)
  , dumpMermaid
  ) where

import Data.DynGraph.Labelled (DynGraph, MermaidOptions (reverseEdges))
import Data.DynGraph.Labelled qualified as Graph
import Data.List qualified as List
import Data.Map.Strict qualified as Map

import Clang.Paths

import HsBindgen.Frontend.RootHeader (HashIncludeArg)
import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Graph = DynGraph

-- | Include graph
--
-- We create a DAG of C header paths with an edge for each @#include@.
-- The edges are /reversed/ to represent an \"included by\" relation.
data IncludeGraph = IncludeGraph{
      graph :: Graph Include SourcePath
    }
  deriving stock (Show, Eq)

-- | Include directive as written in the source
--
-- This is only used for documentation and debugging.
--
-- Include directives may be specified using macros.  For details, see all of
-- section 6.10 (Proprocessing directives) in the C99 specification.  In this
-- case, we simply specify the filename.  Example:
--
-- > #define SOME_HEADER <stdint.h>
-- > #include SOME_HEADER
--
-- In Haddock documentation, we display @stdint.h@.  In debugging output, we
-- display @#include (stdint.h)@.
data Include =
    BracketInclude     HashIncludeArg           -- ^ @#include <...>@
  | QuoteInclude       HashIncludeArg           -- ^ @#include "..."@
  | MacroInclude       HashIncludeArg MacroArg  -- ^ Macro @#include@ argument
  | BracketIncludeNext HashIncludeArg           -- ^ @#include_next <...>@
  | QuoteIncludeNext   HashIncludeArg           -- ^ @#include_next "..."@
  | MacroIncludeNext   HashIncludeArg MacroArg  -- ^ Macro @#include_next@ argument
  deriving stock (Show, Eq, Ord)

-- | Macro argument
--
-- This is the raw text of a macro argument to @#include@ or @#include_next@.
type MacroArg = Text

-- | Get the 'HashIncludeArg' for an 'Include'
getIncludeArg :: Include -> HashIncludeArg
getIncludeArg = \case
    BracketInclude     arg   -> arg
    QuoteInclude       arg   -> arg
    MacroInclude       arg _ -> arg
    BracketIncludeNext arg   -> arg
    QuoteIncludeNext   arg   -> arg
    MacroIncludeNext   arg _ -> arg

-- | Get the 'MacroArg' for an 'Include'
getIncludeMacroArg :: Include -> Maybe MacroArg
getIncludeMacroArg = \case
    BracketInclude{}                   -> Nothing
    QuoteInclude{}                     -> Nothing
    MacroInclude         _arg macroArg -> Just macroArg
    BracketIncludeNext{}               -> Nothing
    QuoteIncludeNext{}                 -> Nothing
    MacroIncludeNext     _arg macroArg -> Just macroArg

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: IncludeGraph
empty = IncludeGraph Graph.empty

register ::
     SourcePath -- ^ Path of header that includes the following header
  -> Include
  -> SourcePath -- ^ Path of the included header
  -> IncludeGraph
  -> IncludeGraph
register header include incHeader includeGraph =
    IncludeGraph $
      Graph.insertEdge incHeader include header $
        Graph.insertVertex incHeader $ Graph.insertVertex header $
          includeGraph.graph

fromList :: [(SourcePath, Include, SourcePath)] -> IncludeGraph
fromList edges = List.foldl' add empty edges
  where
    add :: IncludeGraph -> (SourcePath, Include, SourcePath) -> IncludeGraph
    add graph (fr, inc, to) = register fr inc to graph

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

reaches :: IncludeGraph -> SourcePath -> Set SourcePath
reaches includeGraph = Graph.reaches includeGraph.graph . List.singleton

toSortedList :: IncludeGraph -> [SourcePath]
toSortedList includeGraph =
    List.delete RootHeader.name $ Graph.topSort includeGraph.graph

toOrderMap :: IncludeGraph -> Map SourcePath Int
toOrderMap graph = Map.fromList (zip (toSortedList graph) [0..])

getIncludes ::
     IncludeGraph
  -> SourcePath
  -> Graph.FindEdgesResult Include
getIncludes includeGraph = Graph.findEdges includeGraph.graph

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

-- | Include graph predicate
type Predicate = SourcePath -> Bool

data DumpOpts = DumpOpts {
      -- | Only show vertices satisfying the predicate.
      --
      --   Combine dangling (i.e., transient) edges at removed vertices.
      --
      --   For example,
      --
      --     A-D->B-->C
      --         |
      --         +-->D
      --
      --   Removal of node 'B' creates
      --
      --     A-->C
      --     |
      --     --->D
      predicate :: Predicate
      -- | How should we show the include header?
      --
      -- - If 'True': Show paths of include header files
      --
      -- - If 'False':  Show the @#include@ argument, which usually shorter
    , showPaths :: Bool
    }

-- | Dump include graph
--
-- See 'DumpOpts'.
--
-- Render direct (transient) dependencies using straight (dotted) arrows,
-- respectively.
dumpMermaid :: DumpOpts -> IncludeGraph -> String
dumpMermaid o g =
    Graph.dumpMermaid opts $
      Graph.combineParallelEdges combineParallel $
        Graph.filterVerticesCombineEdges predicate combineSequential $
          Graph.mapEdges (const Direct) $
            Graph.mapVerticesOutgoingEdges Vertex g.graph
  where
    predicate :: Vertex -> Bool
    predicate v = o.predicate v.path

    renderVertex :: Vertex -> FilePath
    renderVertex v =
      if o.showPaths then
        getSourcePath v.path
      else
        getIncludePath v

    renderEdge :: Edge -> Graph.EdgeSpec
    renderEdge = \case
      Direct    -> Graph.EdgeSpec Graph.Straight Nothing
      Transient -> Graph.EdgeSpec Graph.Dotted   Nothing

    opts :: Graph.MermaidOptions Edge Vertex
    opts = Graph.MermaidOptions{
        reverseEdges = True
      , renderVertex = Just . renderVertex
      , renderEdge   = renderEdge
      }

    toPath :: Include -> FilePath
    toPath = (.path) . getIncludeArg

    getIncludePath :: Vertex -> FilePath
    getIncludePath v = safeHead $ List.sortOn length $  map toPath $ v.includes

    safeHead :: [String] -> String
    safeHead []    = getSourcePath $ RootHeader.name
    safeHead (x:_) = x

data Edge = Direct | Transient
 deriving stock (Show, Eq, Ord)

data Vertex = Vertex {
      includes :: [Include]
    , path     :: SourcePath
   }
 deriving stock (Show, Eq, Ord)

-- | Sequential combination of simple include edges.
--
-- @
-- A---D--->B---D--->C
--
-- A--------I------->C
-- @
combineSequential :: Edge -> Edge -> Edge
combineSequential _ _ = Transient

-- | Parallel combination of simple include edges.
--
-- @
-- A---D--->B---D--->C
-- |                 ^
-- |                 |
-- ---------D---------
--
-- A--------I------->C
-- |                 ^
-- |                 |
-- ---------D---------
-- @
combineParallel :: Edge -> Edge -> Edge
combineParallel (Direct   ) (_        ) = Direct
combineParallel (_        ) (Direct   ) = Direct
combineParallel (Transient) (Transient) = Transient
