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
    -- * Visualization
  , Predicate
  , VisOpts(..)
  , renderMermaid
  ) where

import Data.Digraph (Digraph)
import Data.Digraph qualified as Digraph
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.Paths

import HsBindgen.Frontend.RootHeader (HashIncludeArg)
import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Include graph
--
-- We create a DAG of C header paths with an edge for each @#include@.
-- The edges are /reversed/ to represent an \"included by\" relation.
data IncludeGraph = IncludeGraph{
      graph :: Digraph Include SourcePath
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
empty = IncludeGraph Digraph.empty

register ::
     SourcePath -- ^ Path of header that includes the following header
  -> Include
  -> SourcePath -- ^ Path of the included header
  -> IncludeGraph
  -> IncludeGraph
register header include incHeader includeGraph = IncludeGraph $
    Digraph.insertEdge incHeader include header includeGraph.graph

fromList :: [(SourcePath, Include, SourcePath)] -> IncludeGraph
fromList edges = List.foldl' add empty edges
  where
    add :: IncludeGraph -> (SourcePath, Include, SourcePath) -> IncludeGraph
    add graph (fr, inc, to) = register fr inc to graph

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

reaches :: IncludeGraph -> SourcePath -> Set SourcePath
reaches includeGraph path =
    Digraph.reaches (Set.singleton path) includeGraph.graph

toSortedList :: IncludeGraph -> [SourcePath]
toSortedList includeGraph =
    let (cycles, paths) = Digraph.topSort includeGraph.graph
    in  List.delete RootHeader.name paths ++ Set.elems cycles

toOrderMap :: IncludeGraph -> Map SourcePath Int
toOrderMap graph = Map.fromList (zip (toSortedList graph) [0..])

getIncludes ::
     IncludeGraph
  -> SourcePath
  -> Digraph.FindEdgesResult Include
getIncludes includeGraph path = Digraph.findEdges path includeGraph.graph

{-------------------------------------------------------------------------------
  Visualization
-------------------------------------------------------------------------------}

-- | Include graph predicate
type Predicate = SourcePath -> Bool

data VisOpts = VisOpts {
      -- | Only show vertices satisfying the predicate
      --
      -- Edges that traverse removed vertices are combined.
      --
      -- Example:
      --
      -- @
      --   A-->B-->C
      --       |
      --       +-->D
      -- @
      --
      -- Removal of vertex 'B' results in the following graph:
      --
      -- @
      --   A-->C
      --   |
      --   +-->D
      -- @
      --
      -- Combined edges are rendered using dotted lines instead of solid lines.
      predicate :: Predicate

      -- | How should we show the include header?
      --
      -- - If 'True': Show paths of include header files
      --
      -- - If 'False': Show the @#include@ argument, which usually shorter
    , showPaths :: Bool
    }

-- | Render a Mermaid diagram
--
-- See 'VisOpts'.
renderMermaid :: VisOpts -> IncludeGraph -> String
renderMermaid o g =
      Digraph.renderMermaid opts
    . Digraph.combineParallelEdges combineParallel
    . Digraph.filterVerticesCombineEdges predicate combineSequential
    . Digraph.mapEdges (const Direct)
    $ Digraph.mapVerticesOutgoingEdges Vertex g.graph
  where
    opts :: Digraph.VisOptions Edge Vertex
    opts = Digraph.VisOptions{
        visVertex = \v -> Digraph.VisVertex{
            label =
              if o.showPaths
                then Just (getSourcePath v.path)
                else Just (getIncludePath v)
          }
      , visEdge = \e -> Digraph.VisEdge{
            label = Nothing
          , style = case e of
              Direct    -> Digraph.Solid
              Transient -> Digraph.Dotted
          }
      , reverseEdges = True
      }

    predicate :: Vertex -> Bool
    predicate v = o.predicate v.path

data Vertex = Vertex {
      path     :: SourcePath
    , includes :: Set Include
    }
  deriving stock (Show, Eq, Ord)

data Edge = Direct | Transient
  deriving stock (Show, Eq, Ord)

getIncludePath :: Vertex -> FilePath
getIncludePath =
      safeHead
    . List.sortOn length
    . map ((.path) . getIncludeArg)
    . Set.elems
    . (.includes)
  where
    safeHead :: [FilePath] -> FilePath
    safeHead []    = getSourcePath $ RootHeader.name
    safeHead (x:_) = x

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
