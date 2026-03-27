module HsBindgen.Frontend.Analysis.UseDeclGraph.Construction (
    -- * Construction
    fromDecls
  , insertDepsOfDecl
    -- * Deletion
  , deleteDeps
  , deleteRevDeps
  ) where

import Data.DynGraph.Labelled (DynGraph)
import Data.DynGraph.Labelled qualified as DynGraph
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set

import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph.Definition
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Deps
import HsBindgen.Frontend.AST.Type (ValOrRef (..))
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.EnrichComments.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromDecls :: HasCallStack => IncludeGraph -> DeclIndex -> UseDeclGraph
fromDecls includeGraph declIndex =
    foldl'
      (flip insertDepsOfDeclParsedMacro)
      verticesGraph
      sortedSuccessfulDecls
  where
    -- We cannot use plain 'depsOfDecl' here because we have not typechecked
    -- macros yet. Instead, we must provide a "resolver" for bare names (see
    -- below).
    insertDepsOfDeclParsedMacro :: C.Decl EnrichComments -> UseDeclGraph -> UseDeclGraph
    insertDepsOfDeclParsedMacro d =
      insertDeps d.info.id (depsOfDeclParsedMacro resolver d.kind)

    resolver :: MacroNameResolver
    resolver nm
      | macroId   `Set.member` allDeclIds = Just CNameKindMacro
      | typedefId `Set.member` allDeclIds = Just CNameKindOrdinary
      | otherwise                         = Nothing
      where
        macroId, typedefId :: DeclId
        macroId   = DeclId (CDeclName nm CNameKindMacro)    False
        typedefId = DeclId (CDeclName nm CNameKindOrdinary) False

    -- We first insert all vertices. For successfully parsed declarations, we do
    -- this in source order, this ensures that we preserve source order as much
    -- as possible in 'toDecls' (modulo dependencies).
    verticesGraph :: UseDeclGraph
    verticesGraph =
      UseDeclGraph $
        foldl' insertVertex DynGraph.empty $
          map (.info.id) sortedSuccessfulDecls ++ failedDeclIds

    successfulDecls :: [C.Decl EnrichComments]
    successfulDecls = DeclIndex.getDecls declIndex

    successfulDeclIds :: Set DeclId
    successfulDeclIds = Set.fromList $ map (.info.id) successfulDecls

    allDeclIds :: Set DeclId
    allDeclIds = DeclIndex.keysSet declIndex

    failedDeclIds :: [DeclId]
    failedDeclIds = Set.toList $ allDeclIds Set.\\ successfulDeclIds

    sortedSuccessfulDecls :: [C.Decl EnrichComments]
    sortedSuccessfulDecls = List.sortOn (annSortKey orderMap) successfulDecls

    orderMap :: Map SourcePath Int
    orderMap = IncludeGraph.toOrderMap includeGraph

    insertVertex ::
         DynGraph ValOrRef DeclId
      -> DeclId
      -> DynGraph ValOrRef DeclId
    insertVertex g d = DynGraph.insertVertex d g

-- | Inserts dependency edges of provided declaration.
insertDepsOfDecl ::
     (IsPass p, Id p ~ DeclId, DepsOfDecl p, HasCallStack)
  => C.Decl p
  -> UseDeclGraph
  -> UseDeclGraph
insertDepsOfDecl d = insertDeps (d.info.id) (depsOfDecl d.kind)

insertDeps ::
     (HasCallStack)
  => DeclId
  -> [(ValOrRef, DeclId)]
  -> UseDeclGraph
  -> UseDeclGraph
insertDeps source targets g =
    UseDeclGraph $ foldl' (insertEdge source) (g.graph) targets

-- | Insert an edge
--
-- Assume that vertices are in the graph.
--
-- Panic if vertices are not in the graph.
--
-- The graph is not changed if the edge already exists.
insertEdge ::
     HasCallStack
  => DeclId
  -> DynGraph ValOrRef DeclId
  -> (ValOrRef, DeclId)
  -> DynGraph ValOrRef DeclId
insertEdge source g (l, target) = DynGraph.insertEdge source l target g

{-------------------------------------------------------------------------------
  Deletion
-------------------------------------------------------------------------------}

-- | Delete edges from the specified vertices, representing dependencies
--
-- This function is used when a type is opaqued, in which case the type no
-- longer has dependencies.
deleteDeps ::
     [DeclId]
  -> UseDeclGraph
  -> UseDeclGraph
deleteDeps depIds useDeclGraph = UseDeclGraph{
      graph = DynGraph.deleteEdgesFrom depIds useDeclGraph.graph
    }

-- | Delete edges to the specified vertices, representing reverse dependencies
--
-- This function is used when a type is replace with an external reference, in
-- which case all uses of the type no longer directly depend on the type.
deleteRevDeps ::
     [DeclId]
  -> UseDeclGraph
  -> UseDeclGraph
deleteRevDeps depIds useDeclGraph = UseDeclGraph{
      graph = DynGraph.deleteEdgesTo depIds useDeclGraph.graph
    }

{-------------------------------------------------------------------------------
  Construction auxiliary: sort key
-------------------------------------------------------------------------------}

data SortKey = SortKey{
      sortPathIx :: Int
    , sortLineNo :: Int
    , sortColNo  :: Int
    }
  deriving (Eq, Ord, Show)

annSortKey :: Map SourcePath Int -> C.Decl p -> SortKey
annSortKey sourceMap decl = SortKey{
      sortPathIx = sortPathIx
    , sortLineNo = singleLocLine   decl.info.loc
    , sortColNo  = singleLocColumn decl.info.loc
    }
  where
    key        = singleLocPath decl.info.loc
    sortPathIx = fromMaybe
      (panicPure $ "Source of declaration " <> show key <> " not in source map")
      (Map.lookup key sourceMap)
