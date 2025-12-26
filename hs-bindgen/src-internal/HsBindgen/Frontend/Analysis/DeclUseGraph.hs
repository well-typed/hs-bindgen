{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE NoNamedFieldPuns  #-}
{-# LANGUAGE NoRecordWildCards #-}

-- | Declaration definition-usage graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
-- > import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
module HsBindgen.Frontend.Analysis.DeclUseGraph (
    -- * Definition
    DeclUseGraph -- opaque
    -- * Construction
  , fromUseDecl
    -- * Transitive usage
  , getUseSitesTransitively
    -- * Direct usage
  , getUseSites
  , getUseSitesNoSelfReferences
  ) where

import Data.DynGraph.Labelled (DynGraph)
import Data.DynGraph.Labelled qualified as DynGraph
import Data.Set qualified as Set

import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Type (ValOrRef)
import HsBindgen.Frontend.Naming
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Reverse of 'UseDeclGraph'
--
-- This graph has edges from def sites to use sites.
data DeclUseGraph = DeclUseGraph {
      graph :: DynGraph ValOrRef DeclId
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromUseDecl :: UseDeclGraph -> DeclUseGraph
fromUseDecl useDeclGraph = DeclUseGraph{
      graph = DynGraph.reverse $ UseDeclGraph.toDynGraph useDeclGraph
    }

{-------------------------------------------------------------------------------
  Transitive usage
-------------------------------------------------------------------------------}

getUseSitesTransitively :: DeclUseGraph -> [DeclId] -> Set DeclId
getUseSitesTransitively declUseGraph = DynGraph.reaches declUseGraph.graph

{-------------------------------------------------------------------------------
  Direct usage
-------------------------------------------------------------------------------}

getUseSites :: DeclUseGraph -> DeclId -> [(DeclId, ValOrRef)]
getUseSites declUseGraph = Set.toList . DynGraph.neighbors declUseGraph.graph

getUseSitesNoSelfReferences :: DeclUseGraph -> DeclId -> [(DeclId, ValOrRef)]
getUseSitesNoSelfReferences graph qualPrelimDeclId =
  filter (not . isSelfReference) $ getUseSites graph qualPrelimDeclId
    where
      isSelfReference :: (DeclId, ValOrRef) -> Bool
      isSelfReference (qualPrelimDeclId', _usage) =
        qualPrelimDeclId == qualPrelimDeclId'

