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
  , (!)
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
data UseDefGraph = UseDefGraph{
      useDefIndex :: Map (C.QualId Parse) (C.Decl Parse)
    , useDefGraph :: DynGraph Usage (C.QualId Parse)
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: UseDefGraph
empty = UseDefGraph{
      useDefIndex = Map.empty
    , useDefGraph = DynGraph.empty
    }

insert ::
     HasCallStack
  => UseDefGraph -> C.Decl Parse -> UseDefGraph
insert UseDefGraph{useDefIndex, useDefGraph} new =
    UseDefGraph{
        useDefIndex = Map.alter addDecl qid useDefIndex
        -- Map.insert declId decl useDefIndex
      , useDefGraph = foldr
                       (uncurry $ DynGraph.insertEdge qid)
                       (DynGraph.insertVertex qid useDefGraph)
                       (depsOfDecl $ C.declKind new)
      }
  where
    qid :: C.QualId Parse
    qid = C.declQualId new

    -- Add the declaration into the map
    --
    -- NOTE: Some declarations can be repeated, but if so, they must be
    -- essentially the same. For example, this is valid C, which declares the
    -- same struct "foo" twice:
    --
    -- > struct foo;
    -- > struct foo;
    addDecl :: Maybe (C.Decl Parse) -> Maybe (C.Decl Parse)
    addDecl Nothing    = Just new
    addDecl (Just old)
      | C.declKind old == C.declKind new
      = Just old

      | otherwise
      = panicPure $ "duplicate declaration for " ++ show qid

fromDecls ::
     HasCallStack
  => IncludeGraph -> [C.Decl Parse] -> UseDefGraph
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
toDecls :: UseDefGraph -> [C.Decl Parse]
toDecls ud@UseDefGraph{useDefGraph} =
    -- TODO: Should this just be DynGraph.topSort?
    -- Not sure why that has an additional reverse.
    -- NOTE: There might be dependencies in 'useDefGraph' on declarations
    -- without a corresponding entry in 'useDefIndex': for example, this can
    -- happen when we areusing external binding specifications.
    mapMaybe (`lookup` ud) . DynGraph.postorderForest $
      DynGraph.dff useDefGraph

lookup :: C.QualId Parse -> UseDefGraph -> Maybe (C.Decl Parse)
lookup uid UseDefGraph{useDefIndex} = Map.lookup uid useDefIndex

(!) :: HasCallStack => UseDefGraph -> C.QualId Parse -> C.Decl Parse
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

dumpMermaid :: UseDefGraph -> String
dumpMermaid ud@UseDefGraph{useDefGraph} =
    DynGraph.dumpMermaid
      (Just . show)
      (\uid -> showDecl $ ud ! uid)
      useDefGraph
  where
    showDecl :: C.Decl Parse -> String
    showDecl C.Decl{declInfo = C.DeclInfo{declId}} = show declId
