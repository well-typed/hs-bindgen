module HsBindgen.Frontend.Analysis.DeclUseGraph.Query (
    -- * Query
    sortDecls
  , declSortKey
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
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass (Id)

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Order declarations: dependency order, refined by 'declSortKey'
--
-- Definition sites come before use sites, and otherwise we follow source order
-- as far as 'annSortKey' knows it. The database supplies the sort keys of all
-- vertices, active or not; every given declaration must be a vertex.
sortDecls ::
     forall l p q.
     (HasCallStack, Id p ~ C.DeclId)
  => IncludeGraph
  -> DeclDB l q
  -> DeclUseGraph
  -> [C.Decl l p]
  -> [C.Decl l p]
sortDecls includeGraph declDB declUseGraph decls
    | Map.null notVertices = sorted
    | otherwise            = panicPure $
        "Declarations not in the graph: " <> show (Map.keys notVertices)
  where
    order :: IncludeGraph.IncludeOrder
    order = IncludeGraph.toIncludeOrder includeGraph

    byId :: Map C.DeclId (C.Decl l p)
    byId = Map.fromList [(decl.id, decl) | decl <- decls]

    sortedIds :: [C.DeclId]
    sortedIds =
      Digraph.sortBy (comparing key) $
        Digraph.filterEdges (== NeedsShape) declUseGraph.graph

    sorted :: [C.Decl l p]
    sorted = mapMaybe (`Map.lookup` byId) sortedIds

    notVertices :: Map C.DeclId (C.Decl l p)
    notVertices = Map.withoutKeys byId (Set.fromList sortedIds)

    key :: C.DeclId -> DeclSortKey
    key declId = case DeclDB.lookupEntry declId declDB of
      Just entry -> declSortKey order entry
      Nothing    -> panicPure $ "Vertex not in the database: " <> show declId

-- | Entries without a source position sort last
data DeclSortKey = DeclWithLoc SortKey | DeclNoLoc
  deriving stock (Eq, Ord)

-- | The key of an entry
--
-- Read from the entry's 'C.DeclInfo', so deactivating or squashing an entry
-- keeps its key; only 'DeclDB.externalize' changes it.
declSortKey ::
     HasCallStack
  => IncludeGraph.IncludeOrder
  -> DeclDB.Entry l p
  -> DeclSortKey
declSortKey order = \case
    DeclDB.Active   decl     -> withLoc decl.info
    DeclDB.Squashed squashed -> withLoc squashed.info
    DeclDB.External{}        -> DeclNoLoc
    DeclDB.Inactive inactive -> case inactive of
      DeclDB.InactiveDecl info  -> withLoc info.info
      DeclDB.InactiveConflict{} -> DeclNoLoc
  where
    withLoc :: C.DeclInfo -> DeclSortKey
    withLoc info = DeclWithLoc $ annSortKey order info.loc

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
