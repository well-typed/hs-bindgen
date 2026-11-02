module HsBindgen.Frontend.Analysis.DeclUseGraph.Query (
    -- * Query
    outputOrder
    -- ** Transitive usage
  , getUseSitesTransitively
    -- ** Direct usage
  , getUseSites
  , getUseSitesNoSelfReferences
  ) where

import Data.Digraph qualified as Digraph
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.HighLevel.Types

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis
import HsBindgen.Frontend.Analysis.DeclUseGraph.Definition
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | The order of the generated bindings: dependency order, refined by
-- best-effort source order
outputOrder :: HasCallStack => IncludeGraph.IncludeGraph -> DeclDB.Order l p
outputOrder includeGraph =
    DeclDB.DependencyOrder DeclDB.ShapeEdges $ \decl ->
      annSortKey includeOrder decl.info.loc
  where
    includeOrder :: IncludeGraph.IncludeOrder
    includeOrder = IncludeGraph.toIncludeOrder includeGraph

data SortKey = SortKey{
      sortPathIx :: IncludeGraph.IncludeOrderIx
    , sortLineNo :: Int
    , sortColNo  :: Int
    }
  deriving (Eq, Ord, Show)

-- | Best-effort source order (see the orderings in "HsBindgen.Frontend.DeclDB")
annSortKey ::
     HasCallStack
  => IncludeGraph.IncludeOrder
  -> SingleLoc C.DeclPath
  -> SortKey
annSortKey order loc = SortKey{
      sortPathIx = sortPathIx
    , sortLineNo = singleLocLine   loc
    , sortColNo  = singleLocColumn loc
    }
  where
    path = singleLocPath loc
    -- Unlike a trace message, a declaration whose source is not in the include
    -- graph means the frontend is broken; there is nothing to salvage.
    sortPathIx = case IncludeGraph.lookupIncludeOrder order path of
      IncludeGraph.NotInIncludeGraph ->
        panicPure $ "Source of declaration " <> show path <> " not in include graph"
      ix -> ix

{-------------------------------------------------------------------------------
  Transitive usage
-------------------------------------------------------------------------------}

getUseSitesTransitively :: DeclUseGraph -> Set C.DeclId -> Set C.DeclId
getUseSitesTransitively declUseGraph declIds =
    Digraph.reaches declIds declUseGraph.graph

{-------------------------------------------------------------------------------
  Direct usage
-------------------------------------------------------------------------------}

getUseSites :: DeclUseGraph -> C.DeclId -> [(C.DeclId, Dependency)]
getUseSites declUseGraph declId =
    aux $ Digraph.neighbors declId declUseGraph.graph
  where
    aux :: Map C.DeclId (Set Dependency) -> [(C.DeclId, Dependency)]
    aux m = [
        (declId', edge)
      | (declId', edges) <- Map.toList m
      , edge <- Set.elems edges
      ]

getUseSitesNoSelfReferences ::
     DeclUseGraph
  -> C.DeclId
  -> [(C.DeclId, Dependency)]
getUseSitesNoSelfReferences graph declId =
  filter (not . isSelfReference) $ getUseSites graph declId
    where
      isSelfReference :: (C.DeclId, Dependency) -> Bool
      isSelfReference (declId', _usage) = declId == declId'
