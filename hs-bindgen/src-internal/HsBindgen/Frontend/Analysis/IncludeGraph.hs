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
  , insertVertex
  , fromList
    -- * Query
  , reaches
  , toSortedList
  , getIncludes
    -- * Include order
  , IncludeOrder -- opaque
  , IncludeOrderIx(..)
  , toIncludeOrder
  , lookupIncludeOrder
    -- * Visualization
  , Predicate
  , HeaderLabelStyle(..)
  , IncludeGraphFormat(..)
  , VisOpts(..)
  , renderMermaid
  , renderSortedList
  ) where

import Data.Digraph (Digraph)
import Data.Digraph qualified as Digraph
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.Paths

import HsBindgen.Imports
import HsBindgen.IR.C qualified as C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Include graph
--
-- We create a DAG of C header paths with an edge for each @#include@.
-- The edges are /reversed/ to represent an \"included by\" relation.
data IncludeGraph = IncludeGraph{
      graph :: Digraph Include RealPath
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
    BracketInclude     C.HashIncludeArg           -- ^ @#include <...>@
  | QuoteInclude       C.HashIncludeArg           -- ^ @#include "..."@
  | MacroInclude       C.HashIncludeArg MacroArg  -- ^ Macro @#include@ argument
  | BracketIncludeNext C.HashIncludeArg           -- ^ @#include_next <...>@
  | QuoteIncludeNext   C.HashIncludeArg           -- ^ @#include_next "..."@
  | MacroIncludeNext   C.HashIncludeArg MacroArg  -- ^ Macro @#include_next@ argument
  deriving stock (Show, Eq, Ord)

-- | Macro argument
--
-- This is the raw text of a macro argument to @#include@ or @#include_next@.
type MacroArg = Text

-- | Get the 'C.HashIncludeArg' for an 'Include'
getIncludeArg :: Include -> C.HashIncludeArg
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
     RealPath -- ^ Path of header that includes the following header
  -> Include
  -> RealPath -- ^ Path of the included header
  -> IncludeGraph
  -> IncludeGraph
register header include incHeader includeGraph = IncludeGraph $
    Digraph.insertEdge incHeader include header includeGraph.graph

insertVertex :: RealPath -> IncludeGraph -> IncludeGraph
insertVertex path includeGraph = IncludeGraph $
    Digraph.insertVertex path includeGraph.graph

fromList :: [(RealPath, Include, RealPath)] -> IncludeGraph
fromList edges = List.foldl' add empty edges
  where
    add :: IncludeGraph -> (RealPath, Include, RealPath) -> IncludeGraph
    add graph (fr, inc, to) = register fr inc to graph

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

reaches :: IncludeGraph -> RealPath -> Set RealPath
reaches includeGraph path =
    Digraph.reaches (Set.singleton path) includeGraph.graph

toSortedList :: IncludeGraph -> [RealPath]
toSortedList = Digraph.sort . (.graph)

getIncludes ::
     IncludeGraph
  -> RealPath
  -> Digraph.FindEdgesResult Include
getIncludes includeGraph path = Digraph.findEdges path includeGraph.graph

{-------------------------------------------------------------------------------
  Include order
-------------------------------------------------------------------------------}

-- | Position of a source in the include order
--
-- The constructor order /is/ the specification: an unknown path sorts last.
-- Do not reorder.
--
-- The root header is synthetic (virtual, in-memory) and has no 'RealPath', so
-- it cannot appear as a key in the include graph. Declarations are never
-- located in it, and include directives from it are identified by
-- @fromRealPath == Nothing@ in 'HsBindgen.Frontend.ProcessIncludes.IncDir'.
data IncludeOrderIx =
    -- | Position in the topologically sorted include graph
    InIncludeGraph Int
    -- | Path unknown to the include graph
    --
    -- Reaching this is a bug; see
    -- 'HsBindgen.Frontend.Pass.Select.IsPass.SelectSourceNotInIncludeGraph'.
  | NotInIncludeGraph
  deriving stock (Show, Eq, Ord)

-- | The include order of a t'IncludeGraph', for repeated lookup
newtype IncludeOrder = IncludeOrder (Map RealPath Int)

toIncludeOrder :: IncludeGraph -> IncludeOrder
toIncludeOrder graph = IncludeOrder $ Map.fromList (zip (toSortedList graph) [0..])

lookupIncludeOrder :: IncludeOrder -> RealPath -> IncludeOrderIx
lookupIncludeOrder (IncludeOrder order) path =
    maybe NotInIncludeGraph InIncludeGraph (Map.lookup path order)

{-------------------------------------------------------------------------------
  Visualization
-------------------------------------------------------------------------------}

-- | Include graph predicate
type Predicate = RealPath -> Bool

-- | How should we show the include header?
data HeaderLabelStyle =
    -- | Show the @#include@ argument, which is usually shorter
    ShowIncludeArgs
    -- | Show paths of include header files
  | ShowPaths
  deriving stock (Show, Eq)

-- | How should we render the include graph?
data IncludeGraphFormat =
    -- | Mermaid diagram
    Mermaid
    -- | Topologically sorted list of headers, one per line
  | SortedList
  deriving stock (Show, Eq)

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
    , labelStyle :: HeaderLabelStyle
    }

-- | Render a Mermaid diagram
--
-- See t'VisOpts'.
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
            label = Just (vertexLabel o v)
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

-- | Render the include graph as a topologically sorted list of headers
--
-- One header per line, in an order such that a header is listed only after all
-- the headers it @#include@s.  This is the linear form of 'toSortedList'; the
-- t'VisOpts' 'predicate' and 'labelStyle' fields filter and label exactly as for
-- 'renderMermaid'.
renderSortedList :: VisOpts -> IncludeGraph -> String
renderSortedList o g =
      unlines
    . map (vertexLabel o)
    . filter (o.predicate . (.path))
    $ Digraph.sort annotated
  where
    annotated :: Digraph Include Vertex
    annotated = Digraph.mapVerticesOutgoingEdges Vertex g.graph

data Vertex = Vertex {
      path     :: RealPath
    , includes :: Set Include
    }
  deriving stock (Show, Eq, Ord)

data Edge = Direct | Transient
  deriving stock (Show, Eq, Ord)

-- | Display label for a vertex: its resolved path, or the shortest @#include@
-- argument used to include it (see t'VisOpts' @labelStyle@).
vertexLabel :: VisOpts -> Vertex -> String
vertexLabel o v = case o.labelStyle of
    ShowPaths       -> getRealPath v.path
    ShowIncludeArgs -> getIncludePath v

getIncludePath :: Vertex -> FilePath
getIncludePath v =
      safeHead
    . List.sortOn length
    . map ((.path) . getIncludeArg)
    . Set.elems
    $ v.includes
  where
    safeHead :: [FilePath] -> FilePath
    safeHead []    = getRealPath v.path
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
