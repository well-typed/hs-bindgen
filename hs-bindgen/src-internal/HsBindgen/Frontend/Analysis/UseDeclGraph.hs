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
import HsBindgen.Frontend.AST.Deps qualified as Deps
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.AST.Type (ValOrRef (..))
import HsBindgen.Frontend.Naming qualified as C
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
newtype UseDeclGraph = Wrap {
      unwrap :: DynGraph ValOrRef C.DeclId
    }
  deriving stock (Show, Eq)

toDynGraph :: UseDeclGraph -> DynGraph ValOrRef C.DeclId
toDynGraph = unwrap

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
fromSortedDecls decls = Wrap $
    -- We first insert all declarations, so that they are assigned vertices.
    -- Since we do this in source order, this ensures that we preserve source
    -- order as much as possible in 'toDecls' (modulo dependencies).
    let vertices :: DynGraph ValOrRef C.DeclId
        vertices = foldl' (flip addVertex) DynGraph.empty decls
    in foldl' (flip addEdges) vertices decls
  where
    addVertex, addEdges ::
         Decl
      -> DynGraph ValOrRef C.DeclId
      -> DynGraph ValOrRef C.DeclId
    addVertex d g = DynGraph.insertVertex d.declInfo.declId g
    addEdges  d g = foldl' (flip (addEdge d)) g (Deps.depsOfDecl $ C.declKind d)

    addEdge ::
         Decl
      -> (ValOrRef, C.DeclId)
      -> DynGraph ValOrRef C.DeclId
      -> DynGraph ValOrRef C.DeclId
    addEdge d (l, d') = DynGraph.insertEdge d.declInfo.declId l d'

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
toDecls index (Wrap graph) =
    -- TODO: Should this just be DynGraph.topSort?
    -- Not sure why that has an additional reverse.
    -- NOTE: There might be dependencies in 'useDeclGraph' on declarations
    -- without a corresponding entry in 'useDeclIndex': for example, this can
    -- happen when we areusing external binding specifications.
    mapMaybe (`DeclIndex.lookup` index) . DynGraph.postorderForest $
      DynGraph.dff $ DynGraph.filterEdges usedByVal graph
  where
    usedByVal :: ValOrRef -> Bool
    usedByVal = (== ByValue)

getTransitiveDeps :: UseDeclGraph -> [C.DeclId] -> Set C.DeclId
getTransitiveDeps = DynGraph.reaches . unwrap

getStrictTransitiveDeps :: UseDeclGraph -> [C.DeclId] -> Set C.DeclId
getStrictTransitiveDeps graph xs =
    getTransitiveDeps graph xs Set.\\ (Set.fromList xs)

{-------------------------------------------------------------------------------
  Deletion
-------------------------------------------------------------------------------}

-- | Delete dependencies
deleteDeps ::
     [C.DeclId]
  -> UseDeclGraph
  -> UseDeclGraph
deleteDeps depIds = Wrap . DynGraph.deleteEdgesTo depIds . unwrap

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
annSortKey sourceMap C.Decl{declInfo = C.DeclInfo{declLoc}} =
  let key        = singleLocPath declLoc
      sortPathIx = fromMaybe
        (panicPure $ "Source of declaration " <> show key <> " not in source map")
        (Map.lookup key sourceMap)
  in SortKey{
      sortPathIx
    , sortLineNo = singleLocLine declLoc
    , sortColNo  = singleLocColumn declLoc
    }

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

dumpMermaid :: UseDeclGraph -> String
dumpMermaid (Wrap graph) =
    DynGraph.dumpMermaid False (const True) (Just . show) show graph
