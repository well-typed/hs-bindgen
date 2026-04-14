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
    -- * Construction
  , fromDecls
  , insertDepsOfDecl
    -- * Deletion
  , deleteDeps
  , deleteRevDeps
    -- * Query
  , toDecls
    -- ** Transitive usage
  , getUseSitesTransitively
    -- ** Direct usage
  , getUseSites
  , getUseSitesNoSelfReferences
    -- * Visualization
  , renderMermaid
  ) where

-- NOTE This module is split up in order to avoid import cycles.
import HsBindgen.Frontend.Analysis.DeclUseGraph.Construction
import HsBindgen.Frontend.Analysis.DeclUseGraph.Definition
import HsBindgen.Frontend.Analysis.DeclUseGraph.Query
