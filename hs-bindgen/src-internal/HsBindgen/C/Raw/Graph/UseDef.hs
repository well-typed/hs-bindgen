-- | Declaration usage-definition graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.C.Raw.Graph.UseDef (UseDefGraph, Usage)
-- > import HsBindgen.C.Raw.Graph.UseDef qualified as UseDefGraph
module HsBindgen.C.Raw.Graph.UseDef (
    -- * Definition
    UseDefGraph(..)
  , Usage(..)
  , ValOrRef(..)
    -- * Construction
  , fromDecls
    -- * Query
  , toDecls
    -- * Debugging
  , dumpMermaid
  ) where

import Data.Bifunctor
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.Ord
import Data.Text (Text)
import Data.Text qualified as Text

import Clang.HighLevel.Types
import Clang.Paths
import Data.DynGraph.Labelled (DynGraph)
import Data.DynGraph.Labelled qualified as DynGraph
import HsBindgen.C.Raw.AST
import HsBindgen.C.Raw.Graph.Includes (IncludeGraph)
import HsBindgen.C.Raw.Graph.Includes qualified as IncludeGraph
import HsBindgen.C.Raw.Pass.Parse

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Usage-definition graph
--
-- Whenever declaration A uses (depends on) declaration B, there will be
-- an edge from A to B in this graph.
data UseDefGraph = UseDefGraph{
      useDefIndex :: Map DeclId (Decl Parsed)
    , useDefGraph :: DynGraph Usage DeclId
    }
  deriving stock (Show)

data Usage =
    UsedInTypedef ValOrRef
  | UsedInField ValOrRef Text
  deriving stock (Show, Eq, Ord)

data ValOrRef = ByValue | ByRef
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: UseDefGraph
empty = UseDefGraph{
      useDefIndex = Map.empty
    , useDefGraph = DynGraph.empty
    }

insert :: UseDefGraph -> Decl Parsed -> UseDefGraph
insert UseDefGraph{useDefIndex, useDefGraph} decl@(Decl info kind) =
    UseDefGraph{
        useDefIndex = Map.insert (declId info) decl useDefIndex
      , useDefGraph = foldr
                       (uncurry $ DynGraph.insertEdge (declId info))
                       (DynGraph.insertVertex (declId info) useDefGraph)
                       (depsOfDecl kind)
      }

fromDecls :: IncludeGraph -> [Decl Parsed] -> UseDefGraph
fromDecls includeGraph decls =
    Foldable.foldl' insert empty $
      -- It is important that we insert elements into the graph in source order
      -- (this affects the dff). While we /might/ be able to ensure that we
      -- get regular definitions from clang in source order, this is certainly
      -- not the case for macros.
      List.sortBy (comparing $ annSortKey sourceMap) decls
  where
    sourcePaths :: [SourcePath]
    sourcePaths = IncludeGraph.toSortedList includeGraph

    sourceMap :: Map SourcePath Int
    sourceMap = Map.fromList $ zip sourcePaths [0..]

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
toDecls :: UseDefGraph -> [Decl Parsed]
toDecls UseDefGraph{useDefIndex, useDefGraph} =
    -- TODO: Should this just be DynGraph.topSort?
    -- Not sure why that has an additional reverse.
    map (useDefIndex Map.!) . DynGraph.postorderForest $
      DynGraph.dff useDefGraph

{-------------------------------------------------------------------------------
  Construction auxiliary: sort key
-------------------------------------------------------------------------------}

data SortKey = SortKey{
      sortPathIx :: Int
    , sortLineNo :: Int
    , sortColNo  :: Int
    }
  deriving (Eq, Ord, Show)

annSortKey :: Map SourcePath Int -> Decl Parsed -> SortKey
annSortKey sourceMap (Decl info _) = SortKey{
      sortPathIx = sourceMap Map.! singleLocPath sloc
    , sortLineNo = singleLocLine sloc
    , sortColNo  = singleLocColumn sloc
    }
  where
    sloc = declLoc info

{-------------------------------------------------------------------------------
  Construction: auxiliary find dependencies
-------------------------------------------------------------------------------}

depsOfDecl :: DeclKind Parsed -> [(Usage, DeclId)]
depsOfDecl (DeclStruct fs) =
    concatMap depsOfField fs
depsOfDecl (DeclTypedef ty) =
    map (uncurry aux) $ maybeToList (depsOfType ty)
  where
    aux :: ValOrRef -> DeclId -> (Usage, DeclId)
    aux isPtr uid = (UsedInTypedef isPtr, uid)

depsOfField :: Field Parsed -> [(Usage, DeclId)]
depsOfField Field{fieldName, fieldType} =
    map (uncurry aux) $ maybeToList (depsOfType fieldType)
  where
    aux :: ValOrRef -> DeclId -> (Usage, DeclId)
    aux isPtr uid = (UsedInField isPtr fieldName, uid)

-- | The declarations this type depends on
--
-- We also report whether this dependence is through a pointer or not.
--
-- NOTE: We are only interested in /direct/ dependencies here; transitive
-- dependencies will materialize when we build the graph. That's why this
-- returns at most /one/ dependency.
depsOfType :: Type Parsed -> Maybe (ValOrRef, DeclId)
depsOfType (TypePrim _)      = Nothing
depsOfType (TypeStruct  uid) = Just (ByValue, uid)
depsOfType (TypeTypedef uid) = Just (ByValue, uid)
depsOfType (TypePointer ty)  = first (const ByRef) <$> depsOfType ty

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

dumpMermaid :: UseDefGraph -> String
dumpMermaid UseDefGraph{useDefIndex, useDefGraph} =
    DynGraph.dumpMermaid
      (Just . show)
      (\uid -> showDecl $ useDefIndex Map.! uid)
      useDefGraph
  where
    showDecl :: Decl Parsed -> String
    showDecl (Decl DeclInfo{declId} _kind) =
        case declId of
          DeclNamed name -> Text.unpack name
          DeclAnon  loc  -> show loc

