-- | Declaration-usage graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
-- > import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
module HsBindgen.Frontend.Analysis.DeclUseGraph (
    -- * Definition
    DeclUseGraph -- opaque
  , toDigraph
    -- * Query
  , declSortKey
    -- ** Transitive usage
  , getUseSitesTransitively
    -- ** Direct usage
  , getUseSites
  , getUseSitesNoSelfReferences
    -- * Visualization
  , renderMermaid
  ) where

-- NOTE This module is split up in order to avoid import cycles. The
-- construction and the edge edits are used through "HsBindgen.Frontend.DeclDB",
-- which keeps the vertices equal to its keys.
import HsBindgen.Frontend.Analysis.DeclUseGraph.Definition
import HsBindgen.Frontend.Analysis.DeclUseGraph.Query
