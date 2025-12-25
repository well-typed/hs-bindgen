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
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Reverse of 'UseDeclGraph'
--
-- This graph has edges from def sites to use sites.
newtype DeclUseGraph = Wrap {
      unwrap :: DynGraph ValOrRef C.DeclId
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromUseDecl :: UseDeclGraph -> DeclUseGraph
fromUseDecl = Wrap . DynGraph.reverse . UseDeclGraph.toDynGraph

{-------------------------------------------------------------------------------
  Transitive usage
-------------------------------------------------------------------------------}

getUseSitesTransitively :: DeclUseGraph -> [C.DeclId] -> Set C.DeclId
getUseSitesTransitively = DynGraph.reaches . unwrap

{-------------------------------------------------------------------------------
  Direct usage
-------------------------------------------------------------------------------}

getUseSites :: DeclUseGraph -> C.DeclId -> [(C.DeclId, ValOrRef)]
getUseSites (Wrap graph) = Set.toList . DynGraph.neighbors graph

getUseSitesNoSelfReferences :: DeclUseGraph -> C.DeclId -> [(C.DeclId, ValOrRef)]
getUseSitesNoSelfReferences graph qualPrelimDeclId =
  filter (not . isSelfReference) $ getUseSites graph qualPrelimDeclId
    where
      isSelfReference :: (C.DeclId, ValOrRef) -> Bool
      isSelfReference (qualPrelimDeclId', _usage) =
        qualPrelimDeclId == qualPrelimDeclId'

