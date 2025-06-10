-- | Declaration usage-definition graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Graph.UseDecl (UseDeclGraph, Usage)
-- > import HsBindgen.Frontend.Graph.UseDecl qualified as UseDeclGraph
module HsBindgen.Frontend.Graph.UseDecl (
    -- * Definition
    UseDeclGraph(..)
  , Usage(..)
  , ValOrRef(..)
    -- * Construction
  , fromDecls
    -- * Query
  , toDecls
  , lookup
  , (!)
    -- * Debugging
  , dumpMermaid
  ) where

import Prelude hiding (lookup)

import Data.List qualified as List
import Data.Map qualified as Map
import Data.Ord (comparing)

import Clang.HighLevel.Types
import Clang.Paths
import Data.DynGraph.Labelled (DynGraph)
import Data.DynGraph.Labelled qualified as DynGraph
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Deps
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Graph.Includes (IncludeGraph)
import HsBindgen.Frontend.Graph.Includes qualified as IncludeGraph
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Usage-definition graph
--
-- Whenever declaration A uses (depends on) declaration B, there will be
-- an edge from A to B in this graph.
data UseDeclGraph = UseDeclGraph{
      useDeclIndex :: Map (C.QualId Parse) (C.Decl Parse)
    , useDeclGraph :: DynGraph Usage (C.QualId Parse)
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromDecls ::
     HasCallStack
  => IncludeGraph -> [C.Decl Parse] -> UseDeclGraph
fromDecls includeGraph decls = UseDeclGraph{
      useDeclIndex = constructUseDeclIndex decls
    , useDeclGraph = constructUseDeclGraph $
                      List.sortBy (comparing $ annSortKey sourceMap) decls
    }
  where
    sourcePaths :: [SourcePath]
    sourcePaths = IncludeGraph.toSortedList includeGraph

    sourceMap :: Map SourcePath Int
    sourceMap = Map.fromList $ zip sourcePaths [0..]

constructUseDeclIndex ::
     HasCallStack
  => [C.Decl Parse]
  -> Map (C.QualId Parse) (C.Decl Parse)
constructUseDeclIndex = Map.fromListWith aux . map (\d -> (C.declQualId d, d))
  where
    -- Some declarations can be repeated, but if so, they must be essentially
    -- the same. For example, this is valid C, which declares the same struct
    -- "foo" twice:
    --
    -- > struct foo;
    -- > struct foo;
    aux :: C.Decl Parse -> C.Decl Parse -> C.Decl Parse
    aux new old
      | C.declKind old == C.declKind new
      = old

      | otherwise
      = panicPure $ "duplicate declaration for " ++ show (C.declQualId new)

constructUseDeclGraph :: [C.Decl Parse] -> DynGraph Usage (C.QualId Parse)
constructUseDeclGraph decls =
    -- We first insert all declarations, so that they are assigned vertices.
    -- Since we do this in source order, this ensures that we preserve source
    -- order as much as possible in 'toDecls' (modulo dependencies).
    let vertices :: DynGraph Usage (C.QualId Parse)
        vertices = foldl' (flip addVertex) DynGraph.empty decls
    in foldl' (flip addEdges) vertices decls
  where
    addVertex, addEdges ::
         C.Decl Parse
      -> DynGraph Usage (C.QualId Parse) -> DynGraph Usage (C.QualId Parse)
    addVertex d g = DynGraph.insertVertex (C.declQualId d) g
    addEdges  d g = foldl' (flip (addEdge d)) g (depsOfDecl $ C.declKind d)

    addEdge ::
         C.Decl Parse
      -> (Usage, C.QualId Parse)
      -> DynGraph Usage (C.QualId Parse) -> DynGraph Usage (C.QualId Parse)
    addEdge d (l, d') = DynGraph.insertEdge (C.declQualId d) l d'

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
toDecls :: UseDeclGraph -> [C.Decl Parse]
toDecls ud@UseDeclGraph{useDeclGraph} =
    -- TODO: Should this just be DynGraph.topSort?
    -- Not sure why that has an additional reverse.
    -- NOTE: There might be dependencies in 'useDeclGraph' on declarations
    -- without a corresponding entry in 'useDeclIndex': for example, this can
    -- happen when we areusing external binding specifications.
    mapMaybe (`lookup` ud) . DynGraph.postorderForest $
      DynGraph.dff useDeclGraph

lookup :: C.QualId Parse -> UseDeclGraph -> Maybe (C.Decl Parse)
lookup uid UseDeclGraph{useDeclIndex} = Map.lookup uid useDeclIndex

(!) :: HasCallStack => UseDeclGraph -> C.QualId Parse -> C.Decl Parse
(!) ud uid =
    fromMaybe (panicPure $ "Unknown key: " ++ show uid) $
       lookup uid ud

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
dumpMermaid ud@UseDeclGraph{useDeclGraph} =
    DynGraph.dumpMermaid
      (Just . show)
      (\uid -> showDecl $ ud ! uid)
      useDeclGraph
  where
    showDecl :: C.Decl Parse -> String
    showDecl C.Decl{declInfo = C.DeclInfo{declId}} = show declId
