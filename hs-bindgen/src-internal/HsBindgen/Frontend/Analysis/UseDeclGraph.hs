-- | Declaration usage-definition graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph, Usage)
-- > import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
module HsBindgen.Frontend.Analysis.UseDeclGraph (
    -- * Definition
    UseDeclGraph -- opaque
  , toDynGraph
  , Usage(..)
  , ValOrRef(..)
    -- * Construction
  , fromDecls
  , fromSortedDecls
    -- * Query
  , toDecls
    -- * Debugging
  , dumpMermaid
  ) where

import Data.List qualified as List
import Data.Map qualified as Map

import Clang.HighLevel.Types
import Clang.Paths
import Data.DynGraph.Labelled (DynGraph)
import Data.DynGraph.Labelled qualified as DynGraph
import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.AST.Deps
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Usage-definition graph
--
-- Whenever declaration A uses (depends on) declaration B, there will be
-- an edge from A to B in this graph.
newtype UseDeclGraph = Wrap {
      unwrap :: DynGraph Usage QualDeclId
    }
  deriving stock (Show, Eq)

toDynGraph :: UseDeclGraph -> DynGraph Usage QualDeclId
toDynGraph = unwrap

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromDecls :: IncludeGraph -> [C.Decl Parse] -> UseDeclGraph
fromDecls includeGraph decls =
      fromSortedDecls $ List.sortOn (annSortKey sourceMap) decls
  where
    sourcePaths :: [SourcePath]
    sourcePaths = IncludeGraph.toSortedList includeGraph

    sourceMap :: Map SourcePath Int
    sourceMap = Map.fromList $ zip sourcePaths [0..]

fromSortedDecls :: [C.Decl Parse] -> UseDeclGraph
fromSortedDecls decls = Wrap $
    -- We first insert all declarations, so that they are assigned vertices.
    -- Since we do this in source order, this ensures that we preserve source
    -- order as much as possible in 'toDecls' (modulo dependencies).
    let vertices :: DynGraph Usage QualDeclId
        vertices = foldl' (flip addVertex) DynGraph.empty decls
    in foldl' (flip addEdges) vertices decls
  where
    addVertex, addEdges ::
         C.Decl Parse
      -> DynGraph Usage QualDeclId -> DynGraph Usage QualDeclId
    addVertex d g = DynGraph.insertVertex (declQualDeclId d) g
    addEdges  d g = foldl' (flip (addEdge d)) g (depsOfDecl $ C.declKind d)

    addEdge ::
         C.Decl Parse
      -> (Usage, QualDeclId)
      -> DynGraph Usage QualDeclId -> DynGraph Usage QualDeclId
    addEdge d (l, d') = DynGraph.insertEdge (declQualDeclId d) l d'

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
toDecls :: DeclIndex -> UseDeclGraph -> [C.Decl Parse]
toDecls index (Wrap graph) =
    -- TODO: Should this just be DynGraph.topSort?
    -- Not sure why that has an additional reverse.
    -- NOTE: There might be dependencies in 'useDeclGraph' on declarations
    -- without a corresponding entry in 'useDeclIndex': for example, this can
    -- happen when we areusing external binding specifications.
    mapMaybe (`DeclIndex.lookup` index) . DynGraph.postorderForest $
      DynGraph.dff graph

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

dumpMermaid :: DeclIndex -> UseDeclGraph -> String
dumpMermaid declIndex (Wrap graph) =
    DynGraph.dumpMermaid
      (Just . show)
      (\uid -> showDecl $ declIndex DeclIndex.! uid)
      graph
  where
    showDecl :: C.Decl Parse -> String
    showDecl C.Decl{declInfo = C.DeclInfo{declId}} = show declId
