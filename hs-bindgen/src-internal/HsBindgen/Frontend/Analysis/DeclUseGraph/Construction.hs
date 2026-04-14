module HsBindgen.Frontend.Analysis.DeclUseGraph.Construction (
    -- * Construction
    fromDecls
  , insertDepsOfDecl
    -- * Deletion
  , deleteDeps
  , deleteRevDeps
  ) where

import Data.Digraph (Digraph)
import Data.Digraph qualified as Digraph
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph.Definition
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Deps
import HsBindgen.Frontend.AST.Type (ValOrRef)
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromDecls :: HasCallStack => IncludeGraph -> DeclIndex -> DeclUseGraph
fromDecls includeGraph declIndex = DeclUseGraph $
    foldl'
      (flip insertDepsOfDeclParsedMacro)
      verticesGraph
      sortedSuccessfulDecls
  where
    successfulDecls, sortedSuccessfulDecls :: [C.Decl AssignAnonIds]
    successfulDecls       = DeclIndex.getDecls declIndex
    sortedSuccessfulDecls = List.sortOn (annSortKey orderMap) successfulDecls

    orderMap :: Map SourcePath Int
    orderMap = IncludeGraph.toOrderMap includeGraph

    allDeclIds, successfulDeclIds, failedDeclIds :: Set DeclId
    allDeclIds        = DeclIndex.keysSet declIndex
    successfulDeclIds = Set.fromList $ map (.info.id) successfulDecls
    failedDeclIds     = allDeclIds Set.\\ successfulDeclIds

    -- We first insert all declarations, so that they are assigned vertices.
    -- For successfully parsed declarations, we do this in source order.  This
    -- ensures that we preserve source order as much as possible in 'toDecls'
    -- (modulo dependencies).
    verticesGraph :: Digraph ValOrRef DeclId
    verticesGraph = foldl' (flip Digraph.insertVertex) Digraph.empty $
      map (.info.id) sortedSuccessfulDecls ++ Set.toList failedDeclIds

    -- We cannot use plain 'depsOfDecl' here because we have not typechecked
    -- macros yet.  Instead, we must provide a "resolver" for bare names (see
    -- below).
    insertDepsOfDeclParsedMacro ::
         C.Decl AssignAnonIds
      -> Digraph ValOrRef DeclId
      -> Digraph ValOrRef DeclId
    insertDepsOfDeclParsedMacro decl =
      insertDeps decl.info.id (depsOfDeclParsedMacro resolver decl.kind)

    resolver :: MacroNameResolver
    resolver nm
      | macroId   `Set.member` allDeclIds = Just CNameKindMacro
      | typedefId `Set.member` allDeclIds = Just CNameKindOrdinary
      | otherwise                         = Nothing
      where
        macroId, typedefId :: DeclId
        macroId   = DeclId (CDeclName nm CNameKindMacro)    False
        typedefId = DeclId (CDeclName nm CNameKindOrdinary) False

insertDeps ::
     HasCallStack
  => DeclId
  -> [(ValOrRef, DeclId)]
  -> Digraph ValOrRef DeclId
  -> Digraph ValOrRef DeclId
insertDeps source = flip (foldl' aux)
  where
    aux ::
         Digraph ValOrRef DeclId
      -> (ValOrRef, DeclId)
      -> Digraph ValOrRef DeclId
    aux graph (edge, target) =
      case Digraph.insertEdgeIfVerticesExist target edge source graph of
        Digraph.InsertEdgeSuccess graph' -> graph'
        Digraph.InsertEdgeSourceVertexNotFound declId ->
          panicPure $ "source declaration ID not in graph: " ++ show declId
        Digraph.InsertEdgeTargetVertexNotFound declId ->
          panicPure $ "target declaration ID not in graph: " ++ show declId

-- | Insert dependency edges of the provided declaration
insertDepsOfDecl ::
     (IsPass p, Id p ~ DeclId, DepsOfDecl p, HasCallStack)
  => C.Decl p
  -> DeclUseGraph
  -> DeclUseGraph
insertDepsOfDecl decl declUseGraph = DeclUseGraph $
    insertDeps (decl.info.id) (depsOfDecl decl.kind) declUseGraph.graph

{-------------------------------------------------------------------------------
  Deletion
-------------------------------------------------------------------------------}

-- | Delete edges to the specified vertices, representing dependencies
--
-- This function is used when a type is opaqued, in which case the type no
-- longer has dependencies.
deleteDeps :: Set DeclId -> DeclUseGraph -> DeclUseGraph
deleteDeps depIds declUseGraph = DeclUseGraph{
      graph = Digraph.deleteEdgesTo depIds declUseGraph.graph
    }

-- | Delete edges from the specified vertices, representing uses
--
-- This function is used when a type is replaced with an external reference, in
-- which case all uses of the type no longer directly depend on the type.
deleteRevDeps :: Set DeclId -> DeclUseGraph -> DeclUseGraph
deleteRevDeps depIds declUseGraph = DeclUseGraph{
      graph = Digraph.deleteEdgesFrom depIds declUseGraph.graph
    }

{-------------------------------------------------------------------------------
  Auxiliary
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
