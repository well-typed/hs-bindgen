-- | Include graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
-- > import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
module HsBindgen.Frontend.Analysis.IncludeGraph (
    IncludeGraph(..)
  , SourceFile(..)
  , Header(..)
  , Include(..)
  , MacroArg
  , getIncludeArg
  , getIncludeMacroArg
    -- * Construction
  , fromEdges
    -- * Query
  , reachesNames
  , lookupHeader
  , headerNameOf
  , lookupPath
  , headerNameOfPath
  , mainHeaderNamesOf
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

import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Imports
import HsBindgen.IR.C (FileId, HeaderName)
import HsBindgen.IR.C qualified as C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Include graph
--
-- We create a DAG of C source files with an edge for each @#include@.
-- The edges are /reversed/ to represent an \"included by\" relation.
--
-- Vertices are file identities, not the paths @clang@ reported. One file is one
-- vertex however many spellings reached it, and the spellings survive on the
-- edges, which is where they belong: a spelling describes a directive, not a
-- file.
data IncludeGraph = IncludeGraph{
      graph :: Digraph Include SourceFile

      -- | What we know about each file on disk, beyond its identity
      --
      -- Every 'OnDisk' vertex has an entry.
    , headers :: Map FileId Header

      -- | Which file each path @clang@ reported actually names
      --
      -- @clang@ prints one file under several names: a directive is reported by
      -- the spelling that requested it, a source location by the name the file
      -- was first looked up under. So this is many to one, and every entry
      -- comes from a name @clang@ produced rather than from guessing at a path.
      --
      -- Needed because a declaration is not always close enough to its
      -- t'HeaderInfo' to ask directly. Entries for external, squashed and
      -- unusable declarations keep a location and nothing else.
    , paths :: Map SourcePath SourceFile

      -- | Names of the main headers, i.e. those the root header includes
      --
      -- Kept as names so that a binding specification generated from this run
      -- is keyed on something a later run can reproduce.
    , mainHeaders :: Set HeaderName
    }
  deriving stock (Show, Eq)

-- | Vertex of the include graph
data SourceFile =
    -- | A file on disk, identified by its real path
    OnDisk FileId

    -- | Input @clang@ has no real path for, identified by the name it gave
    --
    -- The synthetic root header is the case that arises in practice. It is not
    -- a header the user can name or select, so it carries no t'Header'.
  | InMemory SourcePath
  deriving stock (Show, Eq, Ord)

-- | What we know about a header, beyond its identity
data Header = Header{
      -- | The name to show the user, and to key a binding spec on
      name :: HeaderName

      -- | Other @#include@ arguments that reach this same file
      --
      -- Populated when a symlink lets one file be reached under more than one
      -- name. Collapsing those into one vertex is right, since it really is one
      -- file, but we should not silently rename what the user wrote.
    , aliases :: Set C.HashIncludeArg
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

-- | Build the graph from everything observed in one parse
--
-- Takes all four parts at once rather than letting a caller assemble them, so
-- there is no half-built graph whose 'headers' or 'paths' are still empty.
-- "HsBindgen.Frontend.ProcessIncludes" is the only place that has the
-- observations, and it has them all together.
fromEdges ::
     [(SourceFile, Include, SourceFile)]
       -- ^ One per @#include@: the file containing it, the directive, the file
       -- it reached
  -> Map FileId Header
       -- ^ What each file is called, and what else reaches it
  -> Map SourcePath SourceFile
       -- ^ Which file each path @clang@ reported names
  -> Set HeaderName
       -- ^ Names of the main headers
  -> IncludeGraph
fromEdges edges headers paths mainHeaders = IncludeGraph{
      graph       = List.foldl' insert Digraph.empty edges
    , headers     = headers
    , paths       = paths
    , mainHeaders = mainHeaders
    }
  where
    -- Reversed, so an edge runs from the included file to the one including it.
    insert ::
         Digraph Include SourceFile
      -> (SourceFile, Include, SourceFile)
      -> Digraph Include SourceFile
    insert g (from, include, to) = Digraph.insertEdge to include from g

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Every file that (transitively) includes the given one, and itself
reaches :: IncludeGraph -> SourceFile -> Set SourceFile
reaches includeGraph file =
    Digraph.reaches (Set.singleton file) includeGraph.graph

-- | The names of the headers that (transitively) include the given file
--
-- This is what a binding spec is keyed on, so the answer must not depend on
-- which spelling happened to reach the file.
reachesNames :: IncludeGraph -> SourceFile -> Set HeaderName
reachesNames includeGraph file = Set.fromList
    [ header.name
    | OnDisk fileId <- Set.toList (reaches includeGraph file)
    , Just header   <- [Map.lookup fileId includeGraph.headers]
    ]

lookupHeader :: IncludeGraph -> SourceFile -> Maybe Header
lookupHeader includeGraph = \case
    OnDisk fileId -> Map.lookup fileId includeGraph.headers
    InMemory{}    -> Nothing

headerNameOf :: IncludeGraph -> SourceFile -> Maybe HeaderName
headerNameOf includeGraph = fmap (.name) . lookupHeader includeGraph

-- | Which file a path @clang@ reported names
lookupPath :: IncludeGraph -> SourcePath -> Maybe SourceFile
lookupPath includeGraph path = Map.lookup path includeGraph.paths

-- | The name of the header a reported path names
headerNameOfPath :: IncludeGraph -> SourcePath -> Maybe HeaderName
headerNameOfPath includeGraph path =
    headerNameOf includeGraph =<< lookupPath includeGraph path

-- | Names of the main headers that (transitively) include the given file
--
-- This is what a generated binding specification is keyed on, and it is a
-- subset of what 'reachesNames' offers a consumer, so a specification written
-- here matches when it is read back.
mainHeaderNamesOf :: IncludeGraph -> SourceFile -> Set HeaderName
mainHeaderNamesOf includeGraph file =
    Set.intersection includeGraph.mainHeaders (reachesNames includeGraph file)

toSortedList :: IncludeGraph -> [SourceFile]
toSortedList includeGraph =
    filter (not . isRootHeader) (Digraph.sort includeGraph.graph)

isRootHeader :: SourceFile -> Bool
isRootHeader = \case
    InMemory path -> RootHeader.isRootHeaderPath path
    OnDisk{}      -> False

getIncludes ::
     IncludeGraph
  -> SourceFile
  -> Digraph.FindEdgesResult Include
getIncludes includeGraph file = Digraph.findEdges file includeGraph.graph

{-------------------------------------------------------------------------------
  Include order
-------------------------------------------------------------------------------}

-- | Position of a source in the include order
--
-- The constructor order /is/ the specification: the root header precedes every
-- real source, and an unknown file sorts last. Do not reorder.
data IncludeOrderIx =
    -- | The root header
    --
    -- The root header is synthetic and not a source file, so it is not part of
    -- the include order proper. Anything located in it comes from a root
    -- directive, i.e. directly from the user, and hence comes first.
    InRootHeader
    -- | Position in the topologically sorted include graph
  | InIncludeGraph Int
    -- | File unknown to the include graph
    --
    -- Reaching this is a bug; see
    -- 'HsBindgen.Frontend.Pass.Select.IsPass.SelectSourceNotInIncludeGraph'.
  | NotInIncludeGraph
  deriving stock (Show, Eq, Ord)

-- | The include order of a t'IncludeGraph', for repeated lookup
newtype IncludeOrder = IncludeOrder (Map SourceFile Int)

toIncludeOrder :: IncludeGraph -> IncludeOrder
toIncludeOrder graph = IncludeOrder $ Map.fromList (zip (toSortedList graph) [0..])

lookupIncludeOrder :: IncludeOrder -> SourceFile -> IncludeOrderIx
lookupIncludeOrder (IncludeOrder order) file
  | isRootHeader file = InRootHeader
  | otherwise = maybe NotInIncludeGraph InIncludeGraph (Map.lookup file order)

{-------------------------------------------------------------------------------
  Visualization
-------------------------------------------------------------------------------}

-- | Include graph predicate
type Predicate = SourceFile -> Bool

-- | How should we label a header?
data HeaderLabelStyle =
    -- | The header's name
    --
    -- One name per file, derived from the file rather than from whichever
    -- directive reached it. This is the string a selection predicate matches
    -- and a binding specification is keyed on, so a name read off the graph
    -- can be pasted into either.
    ShowHeaderNames

    -- | The real path of the file on disk
    --
    -- Names this machine, so it answers \"which file is this vertex\" and
    -- nothing that outlives the run.
  | ShowRealPaths
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

      -- | How should we label a header?
    , labelStyle :: HeaderLabelStyle
    }

-- | Render a Mermaid diagram
--
-- See t'VisOpts'.
renderMermaid :: VisOpts -> IncludeGraph -> String
renderMermaid o g =
      Digraph.renderMermaid opts
    . Digraph.combineParallelEdges combineParallel
    . Digraph.filterVerticesCombineEdges o.predicate combineSequential
    . Digraph.mapEdges (const Direct)
    $ g.graph
  where
    opts :: Digraph.VisOptions Edge SourceFile
    opts = Digraph.VisOptions{
        visVertex = \file -> Digraph.VisVertex{
            label = Just (vertexLabel o.labelStyle g file)
          }
      , visEdge = \e -> Digraph.VisEdge{
            label = Nothing
          , style = case e of
              Direct    -> Digraph.Solid
              Transient -> Digraph.Dotted
          }
      , reverseEdges = True
      }

-- | Render the include graph as a topologically sorted list of headers
--
-- One header per line, in an order such that a header is listed only after all
-- the headers it @#include@s.  This is the linear form of 'toSortedList'; the
-- t'VisOpts' 'predicate' and 'labelStyle' fields filter and label exactly as for
-- 'renderMermaid'.
renderSortedList :: VisOpts -> IncludeGraph -> String
renderSortedList o g =
      unlines
    . map (vertexLabel o.labelStyle g)
    . filter o.predicate
    $ Digraph.sort g.graph

data Edge = Direct | Transient
  deriving stock (Show, Eq, Ord)

-- | Display label for a vertex
--
-- Labelling by name and not by an incoming @#include@ argument is the point: a
-- file is reached by as many spellings as there are directives, and picking one
-- of them puts a second name for the file in front of the reader, which is what
-- the rest of this module exists to avoid.
vertexLabel :: HeaderLabelStyle -> IncludeGraph -> SourceFile -> String
vertexLabel style g file = case style of
    ShowHeaderNames -> maybe realPath ((.path) . C.headerNameArg) (headerNameOf g file)
    ShowRealPaths   -> realPath
  where
    -- A vertex with no name is one @clang@ gave no real path for, i.e. the
    -- synthetic root header, so its reported name is all either style has.
    realPath :: String
    realPath = case file of
      InMemory path -> getSourcePath path
      OnDisk fileId -> fileId.path

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
