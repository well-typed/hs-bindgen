module HsBindgen.Frontend.Analysis.UseDeclGraph.Query (
    -- * Query
    toDecls
  , getTransitiveDeps
  , getStrictTransitiveDeps
  ) where

import Data.DynGraph.Labelled qualified as DynGraph
import Data.Set qualified as Set

import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.UseDeclGraph.Definition
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
-- This follows source order whenever possible, but ensures that def sites will
-- come before use sites.
--
-- For each declaration we provide one example of how that declaration is used
-- (if one exists).
toDecls :: DeclIndex -> UseDeclGraph -> [C.Decl AssignAnonIds]
toDecls index useDeclGraph =
    -- NOTE: There might be dependencies in 'UseDeclGraph' on declarations
    -- without a corresponding entry in 'DeclIndex': for example, this can
    -- happen when we are using external binding specifications.
    --
    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1887>
    --
    -- Parent issue: <https://github.com/well-typed/hs-bindgen/issues/1580>.
    --
    -- Direct use of 'topSort' here leads to the wrong order because the edges
    -- of the 'UseDeclGraph' are reversed (i.e., they point from the declaration
    -- to their dependencies, but we require dependencies before the
    -- declarations that use them). We could also perform a topological sort on
    -- the 'DeclUseGraph', but somehow the insertion order is preserved only in
    -- the 'UseDeclGraph'.
    mapMaybe (`DeclIndex.lookup` index) $
      DynGraph.postorderForest $
        DynGraph.dff $
          DynGraph.filterEdges usedByVal useDeclGraph.graph
  where
    usedByVal :: ValOrRef -> Bool
    usedByVal = (== ByValue)

getTransitiveDeps :: UseDeclGraph -> [DeclId] -> Set DeclId
getTransitiveDeps useDeclGraph = DynGraph.reaches useDeclGraph.graph

getStrictTransitiveDeps :: UseDeclGraph -> [DeclId] -> Set DeclId
getStrictTransitiveDeps graph xs =
    getTransitiveDeps graph xs Set.\\ (Set.fromList xs)
