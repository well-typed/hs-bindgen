module HsBindgen.Frontend.Analysis.DeclUseGraph.Query (
    -- * Query
    toDecls
    -- ** Transitive usage
  , getUseSitesTransitively
    -- ** Direct usage
  , getUseSites
  , getUseSitesNoSelfReferences
  ) where

import Data.Digraph (Digraph)
import Data.Digraph qualified as Digraph
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph.Definition
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type (ValOrRef (..))
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass
import HsBindgen.Imports

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
toDecls :: DeclIndex -> DeclUseGraph -> [C.Decl AssignAnonIds]
toDecls index declUseGraph =
    -- NOTE: There might be dependencies in 'declUseGraph' on declarations
    -- without a corresponding entry in 'index'.  For example, this can happen
    -- when we are using external binding specifications.
    mapMaybe (`DeclIndex.lookup` index) . topSort $
      Digraph.filterEdges usedByVal declUseGraph.graph
  where
    topSort :: Digraph ValOrRef DeclId -> [DeclId]
    topSort graph =
      let (cycles, decls) = Digraph.topSort graph
      in  decls ++ Set.elems cycles

    usedByVal :: ValOrRef -> Bool
    usedByVal = (== ByValue)

{-------------------------------------------------------------------------------
  Transitive usage
-------------------------------------------------------------------------------}

getUseSitesTransitively :: DeclUseGraph -> Set DeclId -> Set DeclId
getUseSitesTransitively declUseGraph declIds =
    Digraph.reaches declIds declUseGraph.graph

{-------------------------------------------------------------------------------
  Direct usage
-------------------------------------------------------------------------------}

getUseSites :: DeclUseGraph -> DeclId -> [(DeclId, ValOrRef)]
getUseSites declUseGraph declId =
    aux $ Digraph.neighbors declId declUseGraph.graph
  where
    aux :: Map DeclId (Set ValOrRef) -> [(DeclId, ValOrRef)]
    aux m = [
        (declId', edge)
      | (declId', edges) <- Map.toList m
      , edge <- Set.elems edges
      ]

getUseSitesNoSelfReferences :: DeclUseGraph -> DeclId -> [(DeclId, ValOrRef)]
getUseSitesNoSelfReferences graph qualPrelimDeclId =
  filter (not . isSelfReference) $ getUseSites graph qualPrelimDeclId
    where
      isSelfReference :: (DeclId, ValOrRef) -> Bool
      isSelfReference (qualPrelimDeclId', _usage) =
        qualPrelimDeclId == qualPrelimDeclId'
