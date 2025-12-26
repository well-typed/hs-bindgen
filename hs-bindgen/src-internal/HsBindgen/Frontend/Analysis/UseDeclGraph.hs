{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE NoNamedFieldPuns  #-}
{-# LANGUAGE NoRecordWildCards #-}

-- | Declaration usage-definition graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph, ValOrRef)
-- > import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
module HsBindgen.Frontend.Analysis.UseDeclGraph (
    -- * Definition
    UseDeclGraph -- opaque
  , toDynGraph
    -- * Construction
  , fromDecls
  , fromSortedDecls
    -- * Query
  , toDecls
  , getTransitiveDeps
  , getStrictTransitiveDeps
    -- * Deletion
  , deleteDeps
    -- * Debugging
  , dumpMermaid
  ) where

import Data.DynGraph.Labelled (DynGraph)
import Data.DynGraph.Labelled qualified as DynGraph
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set

import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Deps qualified as Deps
import HsBindgen.Frontend.AST.Type (ValOrRef (..))
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Preliminaries
-------------------------------------------------------------------------------}

type Decl = C.Decl AssignAnonIds

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | ValOrRef-definition graph
--
-- Whenever declaration A uses (depends on) declaration B, there will be
-- an edge from A to B in this graph.
data UseDeclGraph = UseDeclGraph {
      graph :: DynGraph ValOrRef DeclId
    }
  deriving stock (Show, Eq)

toDynGraph :: UseDeclGraph -> DynGraph ValOrRef DeclId
toDynGraph useDeclGraph = useDeclGraph.graph

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromDecls :: IncludeGraph -> [Decl] -> UseDeclGraph
fromDecls includeGraph decls =
      fromSortedDecls $ List.sortOn (annSortKey orderMap) decls
  where
    orderMap :: Map SourcePath Int
    orderMap = IncludeGraph.toOrderMap includeGraph

fromSortedDecls :: [Decl] -> UseDeclGraph
fromSortedDecls decls = UseDeclGraph{
      graph = foldl' (flip addEdges) vertices decls
    }
  where
    -- We first insert all declarations, so that they are assigned vertices.
    -- Since we do this in source order, this ensures that we preserve source
    -- order as much as possible in 'toDecls' (modulo dependencies).
    vertices :: DynGraph ValOrRef DeclId
    vertices = foldl' (flip addVertex) DynGraph.empty decls

    addVertex, addEdges ::
         Decl
      -> DynGraph ValOrRef DeclId
      -> DynGraph ValOrRef DeclId
    addVertex d g = DynGraph.insertVertex d.info.id g
    addEdges  d g = foldl' (flip (addEdge d)) g (Deps.depsOfDecl d.kind)

    addEdge ::
         Decl
      -> (ValOrRef, DeclId)
      -> DynGraph ValOrRef DeclId
      -> DynGraph ValOrRef DeclId
    addEdge d (l, d') = DynGraph.insertEdge d.info.id l d'

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
toDecls :: DeclIndex -> UseDeclGraph -> [Decl]
toDecls index useDeclGraph =
    -- TODO: Should this just be DynGraph.topSort?
    -- Not sure why that has an additional reverse.
    -- NOTE: There might be dependencies in 'useDeclGraph' on declarations
    -- without a corresponding entry in 'useDeclIndex': for example, this can
    -- happen when we areusing external binding specifications.
    mapMaybe (`DeclIndex.lookup` index) . DynGraph.postorderForest $
      DynGraph.dff $ DynGraph.filterEdges usedByVal useDeclGraph.graph
  where
    usedByVal :: ValOrRef -> Bool
    usedByVal = (== ByValue)

getTransitiveDeps :: UseDeclGraph -> [DeclId] -> Set DeclId
getTransitiveDeps useDeclGraph = DynGraph.reaches useDeclGraph.graph

getStrictTransitiveDeps :: UseDeclGraph -> [DeclId] -> Set DeclId
getStrictTransitiveDeps graph xs =
    getTransitiveDeps graph xs Set.\\ (Set.fromList xs)

{-------------------------------------------------------------------------------
  Deletion
-------------------------------------------------------------------------------}

-- | Delete dependencies
deleteDeps ::
     [DeclId]
  -> UseDeclGraph
  -> UseDeclGraph
deleteDeps depIds useDeclGraph = UseDeclGraph{
      graph = DynGraph.deleteEdgesTo depIds useDeclGraph.graph
    }

{-------------------------------------------------------------------------------
  Construction auxiliary: sort key
-------------------------------------------------------------------------------}

data SortKey = SortKey{
      sortPathIx :: Int
    , sortLineNo :: Int
    , sortColNo  :: Int
    }
  deriving (Eq, Ord, Show)

annSortKey :: Map SourcePath Int -> C.Decl p -> SortKey
annSortKey sourceMap decl = SortKey{
      sortPathIx = sortPathIx
    , sortLineNo = singleLocLine   decl.info.loc
    , sortColNo  = singleLocColumn decl.info.loc
    }
  where
    key        = singleLocPath decl.info.loc
    sortPathIx = fromMaybe
      (panicPure $ "Source of declaration " <> show key <> " not in source map")
      (Map.lookup key sourceMap)

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

dumpMermaid :: UseDeclGraph -> String
dumpMermaid useDeclGraph =
    DynGraph.dumpMermaid
      DynGraph.MermaidOptions{
          reverseEdges = False
        , renderVertex = Just . show
        , renderEdge   = Just . show
        }
      useDeclGraph.graph
