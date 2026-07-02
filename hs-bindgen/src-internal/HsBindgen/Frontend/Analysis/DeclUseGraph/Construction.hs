module HsBindgen.Frontend.Analysis.DeclUseGraph.Construction (
    -- * Construction
    construct
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
import HsBindgen.Frontend.Analysis.Deps
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.Zip.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

construct ::
     forall l. HasCallStack
  => Macro.Lang l
  -> IncludeGraph
  -> DeclIndex l
  -> DeclUseGraph
construct macroLang includeGraph declIndex = declUseGraph
  where
    -- The include graph informs us about the order of declarations which is
    -- key.
    orderMap :: Map SourcePath Int
    orderMap = IncludeGraph.toOrderMap includeGraph
    -- The declaration index contains declarations that we cannot use. Macros
    -- have already been resolved while constructing the declaration index.
    successfulDecls, sortedSuccessfulDecls :: [C.Decl l ConstructTranslationUnit]
    successfulDecls       = DeclIndex.getDecls declIndex
    sortedSuccessfulDecls = List.sortOn (annSortKey orderMap) successfulDecls

    allDeclIds, successfulDeclIds, failedDeclIds :: Set C.DeclId
    allDeclIds        = DeclIndex.keysSet declIndex
    successfulDeclIds = Set.fromList $ map (.info.id) sortedSuccessfulDecls
    failedDeclIds     = allDeclIds Set.\\ successfulDeclIds

    declUseGraph :: DeclUseGraph
    declUseGraph = DeclUseGraph $
      foldl'
        (flip insertDepsOfDeclParsedMacro)
        verticesGraph
        sortedSuccessfulDecls

    -- We first insert all declarations, so that they are assigned vertices. For
    -- successfully parsed declarations, we do this in source order. This
    -- ensures that we preserve source order as much as possible in 'toDecls'
    -- (modulo dependencies).
    verticesGraph :: Digraph C.ValOrRef C.DeclId
    verticesGraph = foldl' (flip Digraph.insertVertex) Digraph.empty $
      map (.info.id) sortedSuccessfulDecls ++ Set.toList failedDeclIds

    -- We cannot use plain 'depsOfDecl' here because, at this pass, macros have
    -- been resolved (in the declaration index) but not yet typechecked. We
    -- therefore use 'depsOfDeclParsedMacro', which derives the dependencies
    -- from the resolved (but not yet typechecked) macro body.
    insertDepsOfDeclParsedMacro ::
         C.Decl l ConstructTranslationUnit
      -> Digraph C.ValOrRef C.DeclId
      -> Digraph C.ValOrRef C.DeclId
    insertDepsOfDeclParsedMacro decl =
      insertDeps decl.info.id (depsOfDeclParsedMacro macroLang decl.kind)

insertDeps ::
     (HasCallStack)
  => C.DeclId
  -> [(C.ValOrRef, C.DeclId)]
  -> Digraph C.ValOrRef C.DeclId
  -> Digraph C.ValOrRef C.DeclId
insertDeps source = flip (foldl' aux)
  where
    aux ::
         Digraph C.ValOrRef C.DeclId
      -> (C.ValOrRef, C.DeclId)
      -> Digraph C.ValOrRef C.DeclId
    aux graph (edge, target) =
      case Digraph.insertEdgeIfVerticesExist target edge source graph of
        Digraph.InsertEdgeSuccess graph' -> graph'
        Digraph.InsertEdgeSourceVertexNotFound declId ->
          panicPure $ "source declaration ID not in graph: " ++ show declId
        Digraph.InsertEdgeTargetVertexNotFound declId ->
          panicPure $ "target declaration ID not in graph: " ++ show declId

-- | Inserts dependency edges of provided declaration.
insertDepsOfDecl ::
     (Macro.HasTypes l, HasCallStack)
  => Macro.Lang l
  -> C.Decl l Zip
  -> DeclUseGraph
  -> DeclUseGraph
insertDepsOfDecl macroLang decl declUseGraph = DeclUseGraph $
    insertDeps (decl.info.id) (depsOfDecl macroLang decl.kind) declUseGraph.graph

{-------------------------------------------------------------------------------
  Deletion
-------------------------------------------------------------------------------}

-- | Delete edges to the specified vertices, representing dependencies
--
-- This function is used when a type is opaqued, in which case the type no
-- longer has dependencies.
deleteDeps :: Set C.DeclId -> DeclUseGraph -> DeclUseGraph
deleteDeps depIds declUseGraph = DeclUseGraph{
      graph = Digraph.deleteEdgesTo depIds declUseGraph.graph
    }

-- | Delete edges from the specified vertices, representing uses
--
-- This function is used when a type is replaced with an external reference, in
-- which case all uses of the type no longer directly depend on the type.
deleteRevDeps :: Set C.DeclId -> DeclUseGraph -> DeclUseGraph
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

annSortKey :: Map SourcePath Int -> C.Decl l p -> SortKey
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
