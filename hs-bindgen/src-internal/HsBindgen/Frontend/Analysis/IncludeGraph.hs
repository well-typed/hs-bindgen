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
  , dumpMermaid
  ) where

import Data.DynGraph.Labelled (DynGraph)
import Data.DynGraph.Labelled qualified as DynGraph
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

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
      graph :: DynGraph Include SourcePath
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
empty = IncludeGraph DynGraph.empty

register ::
     SourcePath -- ^ Path of header that includes the following header
  -> Include
  -> SourcePath -- ^ Path of the included header
  -> IncludeGraph
  -> IncludeGraph
register header include incHeader includeGraph = IncludeGraph $
    DynGraph.insertEdge incHeader include header includeGraph.graph

fromList :: [(SourcePath, Include, SourcePath)] -> IncludeGraph
fromList edges = List.foldl' add empty edges
  where
    add :: IncludeGraph -> (SourcePath, Include, SourcePath) -> IncludeGraph
    add graph (fr, inc, to) = register fr inc to graph

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

reaches :: IncludeGraph -> SourcePath -> Set SourcePath
reaches includeGraph = DynGraph.reaches includeGraph.graph . List.singleton

toSortedList :: IncludeGraph -> [SourcePath]
toSortedList includeGraph =
    List.delete RootHeader.name $ DynGraph.topSort includeGraph.graph

toOrderMap :: IncludeGraph -> Map SourcePath Int
toOrderMap graph = Map.fromList (zip (toSortedList graph) [0..])

getIncludes ::
     IncludeGraph
  -> SourcePath
  -> DynGraph.FindEdgesResult Include
getIncludes includeGraph = DynGraph.findEdges includeGraph.graph

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

-- | Include graph predicate
--
-- An include graph predicate can be used to filter which indexes of an include
-- graph are shown.
type Predicate = SourcePath -> Bool

dumpMermaid :: Predicate -> IncludeGraph -> String
dumpMermaid p includeGraph =
    DynGraph.dumpMermaid
      DynGraph.MermaidOptions{
          reverseEdges = True
        , renderVertex = \path -> guard (p path) >> return (getSourcePath path)
        , renderEdge   = Just . renderInclude
        }
      includeGraph.graph
  where
    renderInclude :: Include -> String
    renderInclude = \case
      BracketInclude     i   -> "#include <"       ++ i.path ++ ">"
      QuoteInclude       i   -> "#include \""      ++ i.path ++ "\""
      MacroInclude       _ m -> "#include "        ++ Text.unpack m
      BracketIncludeNext i   -> "#include_next <"  ++ i.path ++ ">"
      QuoteIncludeNext   i   -> "#include_next \"" ++ i.path ++ "\""
      MacroIncludeNext   _ m -> "#include_next "   ++ Text.unpack m
