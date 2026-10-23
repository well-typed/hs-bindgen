module HsBindgen.Frontend.Analysis.DeclUseGraph.Query (
    -- * Query
    toDecls
    -- ** Transitive usage
  , getUseSitesTransitively
    -- ** Direct usage
  , getUseSites
  , getUseSitesNoSelfReferences
  ) where

import Data.Digraph qualified as Digraph
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Set qualified as Set

import Clang.HighLevel.Types

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis
import HsBindgen.Frontend.Analysis.DeclUseGraph.Definition
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.DeclDB (DeclDB)
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Construct ordered list of declarations
--
-- Dependency order, refined by 'annSortKey': definition sites come before use
-- sites, and otherwise we follow source order as far as 'annSortKey' knows it.
toDecls ::
     HasCallStack
  => IncludeGraph
  -> DeclDB l ConstructTranslationUnit
  -> DeclUseGraph
  -> [C.Decl l ConstructTranslationUnit]
toDecls includeGraph declDB declUseGraph =
    -- 'DeclDB.lookup' skips the inactive declarations.
    mapMaybe (`DeclDB.lookup` declDB) . Digraph.sortBy (comparing key) $
      Digraph.filterEdges (== NeedsShape) declUseGraph.graph
  where
    order :: IncludeGraph.IncludeOrder
    order = IncludeGraph.toIncludeOrder includeGraph

    key :: C.DeclId -> DeclSortKey
    key declId = case DeclDB.lookup declId declDB of
      Just decl -> DeclSucceeded $ annSortKey order decl.info.loc
      Nothing   -> DeclFailed

-- | Failed declarations sort last; they have no source position we could use
data DeclSortKey = DeclSucceeded SortKey | DeclFailed
  deriving stock (Eq, Ord)

data SortKey = SortKey{
      sortPathIx :: IncludeGraph.IncludeOrderIx
    , sortLineNo :: Int
    , sortColNo  :: Int
    }
  deriving (Eq, Ord, Show)

-- | Best-effort source order (see the note on orderings in
-- "HsBindgen.Frontend.Pass.Parse")
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
