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
import Data.Set qualified as Set

import HsBindgen.Frontend.Analysis
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph.Definition
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Construct ordered list of declarations
--
-- This follows source order whenever possible, but ensures that definition
-- sites come before use sites.
--
-- For each declaration we provide one example of how that declaration is used
-- (if one exists).
toDecls :: DeclIndex l -> DeclUseGraph -> [C.Decl l ConstructTranslationUnit]
toDecls index declUseGraph =
    -- NOTE: There might be dependencies in the 'DeclUseGraph' on declarations
    -- without a corresponding entry in the 'DeclIndex'.  For example, this can
    -- happen when we are using external binding specifications.
    mapMaybe (`DeclIndex.lookup` index) . Digraph.sort $
      Digraph.filterEdges (== NeedsShape) declUseGraph.graph

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
