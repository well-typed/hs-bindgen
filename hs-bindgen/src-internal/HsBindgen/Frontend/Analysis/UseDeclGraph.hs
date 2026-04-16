-- | Usage-declaration graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph, ValOrRef)
-- > import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
module HsBindgen.Frontend.Analysis.UseDeclGraph (
    -- * Definition
    UseDeclGraph -- opaque
  , toDynGraph
    -- * Construction
  , fromDecls
  , insertDepsOfDecl
    -- * Query
  , toDecls
  , getTransitiveDeps
  , getStrictTransitiveDeps
    -- * Deletion
  , deleteDeps
  , deleteRevDeps
    -- * Debugging
  , dumpMermaid
  ) where

import HsBindgen.Frontend.Analysis.UseDeclGraph.Construction
import HsBindgen.Frontend.Analysis.UseDeclGraph.Definition
import HsBindgen.Frontend.Analysis.UseDeclGraph.Query

