-- | Declaration usage-definition graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Graph.UseDef (UseDefGraph, Usage)
-- > import HsBindgen.Frontend.Graph.UseDef qualified as UseDefGraph
module HsBindgen.Frontend.Graph.UseDef (
    -- * Definition
    UseDefGraph(..)
  , Usage(..)
  , ValOrRef(..)
    -- * Construction
  , fromDecls
    -- * Query
  , toDecls
  , lookup
    -- * Debugging
  , dumpMermaid
  ) where

import Prelude hiding (lookup)

import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Ord (comparing)

import Clang.HighLevel.Types
import Clang.Paths
import Data.DynGraph.Labelled (DynGraph)
import Data.DynGraph.Labelled qualified as DynGraph
import HsBindgen.Errors
import HsBindgen.Frontend.AST
import HsBindgen.Frontend.AST.Deps
import HsBindgen.Frontend.Graph.Includes (IncludeGraph)
import HsBindgen.Frontend.Graph.Includes qualified as IncludeGraph
import HsBindgen.Frontend.Pass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Usage-definition graph
--
-- Whenever declaration A uses (depends on) declaration B, there will be
-- an edge from A to B in this graph.
data UseDefGraph p = UseDefGraph{
      useDefIndex :: Map (Id p) (Decl p)
    , useDefGraph :: DynGraph Usage (Id p)
    }
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: UseDefGraph p
empty = UseDefGraph{
      useDefIndex = Map.empty
    , useDefGraph = DynGraph.empty
    }

insert :: forall p.
     (Ord (Id p), Show (Id p), HasCallStack)
  => UseDefGraph p -> Decl p -> UseDefGraph p
insert UseDefGraph{useDefIndex, useDefGraph} decl =
    UseDefGraph{
        useDefIndex = Map.alter addDecl declId useDefIndex
        -- Map.insert declId decl useDefIndex
      , useDefGraph = foldr
                       (uncurry $ DynGraph.insertEdge declId)
                       (DynGraph.insertVertex declId useDefGraph)
                       (depsOfDecl declKind)
      }
  where
    Decl{declInfo = DeclInfo{declId}, declKind} = decl

    addDecl :: Maybe (Decl p) -> Maybe (Decl p)
    addDecl Nothing  = Just decl
    addDecl (Just _) = panicPure $ "duplicate declaration for " ++ show declId

fromDecls ::
     (Ord (Id p), Show (Id p), HasCallStack)
  => IncludeGraph -> [Decl p] -> UseDefGraph p
fromDecls includeGraph decls =
    Foldable.foldl' insert empty $
      -- It is important that we insert elements into the graph in source order
      -- (this affects the dff). While we /might/ be able to ensure that we
      -- get regular definitions from clang in source order, this is certainly
      -- not the case for macros.
      List.sortBy (comparing $ annSortKey sourceMap) decls
  where
    sourcePaths :: [SourcePath]
    sourcePaths = IncludeGraph.toSortedList includeGraph

    sourceMap :: Map SourcePath Int
    sourceMap = Map.fromList $ zip sourcePaths [0..]

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Construct ordered list of declarations
--
-- This follows source order whenever possible, but ensures that def sites will
-- come before use sites.
--
-- For each declaration we provide one example of how that declaration is used
-- (if one exists).
toDecls :: Ord (Id p) => UseDefGraph p -> [Decl p]
toDecls UseDefGraph{useDefIndex, useDefGraph} =
    -- TODO: Should this just be DynGraph.topSort?
    -- Not sure why that has an additional reverse.
    map (useDefIndex Map.!) . DynGraph.postorderForest $
      DynGraph.dff useDefGraph

lookup :: Ord (Id p) => Id p -> UseDefGraph p -> Maybe (Decl p)
lookup uid UseDefGraph{useDefIndex} = Map.lookup uid useDefIndex

{-------------------------------------------------------------------------------
  Construction auxiliary: sort key
-------------------------------------------------------------------------------}

data SortKey = SortKey{
      sortPathIx :: Int
    , sortLineNo :: Int
    , sortColNo  :: Int
    }
  deriving (Eq, Ord, Show)

annSortKey :: Map SourcePath Int -> Decl p -> SortKey
annSortKey sourceMap Decl{declInfo = DeclInfo{declLoc}} = SortKey{
      sortPathIx = sourceMap Map.! singleLocPath declLoc
    , sortLineNo = singleLocLine declLoc
    , sortColNo  = singleLocColumn declLoc
    }

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

dumpMermaid :: forall p.
     Ord (Id p)
  => (Id p -> String) -> UseDefGraph p -> String
dumpMermaid showId UseDefGraph{useDefIndex, useDefGraph} =
    DynGraph.dumpMermaid
      (Just . show)
      (\uid -> showDecl $ useDefIndex Map.! uid)
      useDefGraph
  where
    showDecl :: Decl p -> String
    showDecl Decl{declInfo = DeclInfo{declId}} = showId declId
