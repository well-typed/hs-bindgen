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
import Data.Set qualified as Set


import HsBindgen.Errors
import HsBindgen.Frontend.Analysis
import HsBindgen.Frontend.Analysis.DeclUseGraph.Definition
import HsBindgen.Frontend.Analysis.Deps
import HsBindgen.Frontend.DeclDB (DeclDB)
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

construct ::
     forall l. HasCallStack
  => DeclDB l ConstructTranslationUnit
  -> DeclUseGraph
construct declDB = declUseGraph
  where
    -- The database contains declarations that we cannot use. Macros have
    -- already been resolved while constructing the database.
    successfulDecls :: [C.Decl l ConstructTranslationUnit]
    successfulDecls = DeclDB.getDecls declDB

    declUseGraph :: DeclUseGraph
    declUseGraph = DeclUseGraph $
      foldl'
        (flip insertDepsOfDeclParsedMacro)
        verticesGraph
        successfulDecls

    -- We first insert all declarations, so that they are assigned vertices.
    verticesGraph :: Digraph Dependency C.DeclId
    verticesGraph = foldl' (flip Digraph.insertVertex) Digraph.empty $
      Set.toList (DeclDB.keysSet declDB)

    -- We cannot use plain 'depsOfDecl' here because, at this pass, macros have
    -- been resolved (in the database) but not yet typechecked. We
    -- therefore use 'depsOfDeclParsedMacro', which derives the dependencies
    -- from the resolved (but not yet typechecked) macro body.
    insertDepsOfDeclParsedMacro ::
         C.Decl l ConstructTranslationUnit
      -> Digraph Dependency C.DeclId
      -> Digraph Dependency C.DeclId
    insertDepsOfDeclParsedMacro decl =
      insertDeps decl.id (depsOfDeclParsedMacro decl.kind)

insertDeps ::
     (HasCallStack)
  => C.DeclId
  -> [(C.DeclId, Dependency)]
  -> Digraph Dependency C.DeclId
  -> Digraph Dependency C.DeclId
insertDeps source = flip (foldl' aux)
  where
    aux ::
         Digraph Dependency C.DeclId
      -> (C.DeclId, Dependency)
      -> Digraph Dependency C.DeclId
    aux graph (target, edge) =
      case Digraph.insertEdgeIfVerticesExist target edge source graph of
        Digraph.InsertEdgeSuccess graph' -> graph'
        Digraph.InsertEdgeSourceVertexNotFound declId ->
          panicPure $ "source declaration ID not in graph: " ++ show declId
        Digraph.InsertEdgeTargetVertexNotFound declId ->
          panicPure $ "target declaration ID not in graph: " ++ show declId

-- | Inserts dependency edges of provided declaration.
insertDepsOfDecl ::
     HasCallStack
  => C.Decl l ReparseMacroExpansions
  -> DeclUseGraph
  -> DeclUseGraph
insertDepsOfDecl decl declUseGraph = DeclUseGraph $
    insertDeps (decl.id) (depsOfDecl decl.kind) declUseGraph.graph

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
