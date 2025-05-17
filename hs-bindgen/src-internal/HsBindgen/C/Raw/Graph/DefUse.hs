-- | Declaration definition-usage graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.C.Raw.Graph.DefUse (DefUseGraph)
-- > import HsBindgen.C.Raw.Graph.DefUse qualified as DefUseGraph
module HsBindgen.C.Raw.Graph.DefUse (
    -- * Definition
    DefUseGraph(..)
    -- * Construction
  , fromUseDef
    -- * Query
  , UseOfAnon(..)
  , findUseOfAnon
    -- * Debugging
  , dumpMermaid
  ) where

import Control.Monad.State
import Data.Map qualified as Map

import Data.DynGraph.Labelled qualified as DynGraph
import HsBindgen.C.Raw.AST
import HsBindgen.C.Raw.Graph.UseDef (UseDefGraph(..), Usage(..))
import HsBindgen.C.Raw.Graph.UseDef qualified as UseDefGraph
import HsBindgen.C.Raw.Pass.Parse
import HsBindgen.Errors
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Reverse of 'UseDefGraph'
--
-- This graph has edges from def sites to use sites.
newtype DefUseGraph = DefUseGraph UseDefGraph

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromUseDef :: UseDefGraph -> DefUseGraph
fromUseDef UseDefGraph{useDefIndex, useDefGraph} = DefUseGraph UseDefGraph{
      useDefIndex
    , useDefGraph = DynGraph.reverse useDefGraph
    }

{-------------------------------------------------------------------------------
  Query: usage of anon declarations
-------------------------------------------------------------------------------}

data UseOfAnon =
    UsedByNamed Usage Text
  | UsedByAnon Usage UseOfAnon
  deriving stock (Show)

-- | Find use site for anonymous declaration, if it exists
--
-- Unused anonymous declarations can be removed.
findUseOfAnon :: DefUseGraph -> AnonId -> Maybe UseOfAnon
findUseOfAnon (DefUseGraph UseDefGraph{useDefIndex, useDefGraph}) anonId =
    flip evalState id $
      DynGraph.findTrailFrom
        useDefGraph
        (aux . map (second (useDefIndex Map.!)))
        (DeclAnon anonId)
  where
    aux ::
         [(Usage, Decl Parsed)] -- ^ Direct use sites
      -> State
           (UseOfAnon -> UseOfAnon)
           (Either DeclId (Maybe UseOfAnon))
    aux [(u, d)] = do
        case declId of
          DeclNamed name -> do
            f <- get
            return $ Right . Just $ f (UsedByNamed u name)
          DeclAnon _anonId -> do
            modify (. UsedByAnon u)
            return $ Left declId
      where
        Decl DeclInfo{declId} _kind = d
    aux [] =
        return $ Right Nothing
    aux (_:_:_) =
        panicPure "findUseOfAnon: impossible multiple use of anon decl"

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

dumpMermaid :: DefUseGraph -> String
dumpMermaid (DefUseGraph graph) = UseDefGraph.dumpMermaid graph