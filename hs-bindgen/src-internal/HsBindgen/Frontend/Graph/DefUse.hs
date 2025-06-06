-- | Declaration definition-usage graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Graph.DefUse (DefUseGraph)
-- > import HsBindgen.Frontend.Graph.DefUse qualified as DefUseGraph
module HsBindgen.Frontend.Graph.DefUse (
    -- * Definition
    DefUseGraph(..)
    -- * Construction
  , fromUseDef
    -- * Query
  , UseOfDecl(..)
  , findNamedUseOf
    -- * Debugging
  , dumpMermaid
  ) where

import Control.Monad.State

import Data.DynGraph.Labelled qualified as DynGraph
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Graph.UseDef (UseDefGraph(..), Usage(..))
import HsBindgen.Frontend.Graph.UseDef qualified as UseDefGraph
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Reverse of 'UseDefGraph'
--
-- This graph has edges from def sites to use sites.
newtype DefUseGraph = DefUseGraph UseDefGraph
  deriving stock (Show, Eq)

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

data UseOfDecl =
    UsedByNamed Usage (CName, Namespace)
  | UsedByAnon Usage UseOfDecl
  deriving stock (Show)

-- | Find direct or indirect use by a named declaration, if it exists
findNamedUseOf :: DefUseGraph -> C.QualId Parse -> Maybe UseOfDecl
findNamedUseOf (DefUseGraph ud@UseDefGraph{useDefGraph}) =
      flip evalState id
    . DynGraph.findTrailFrom
        useDefGraph
        (aux . map (first (ud UseDefGraph.!)))
  where
    aux ::
         [(C.Decl Parse, Usage)] -- ^ Direct use sites
      -> State
           (UseOfDecl -> UseOfDecl)
           (Either (C.QualId Parse) (Maybe UseOfDecl))
    aux [(d, u)] = do
        case uid of
          DeclNamed name -> do
            f <- get
            return $ Right . Just $ f (UsedByNamed u (name, ns))
          DeclAnon _anonId -> do
            modify (. UsedByAnon u)
            return $ Left qid
      where
        qid@(C.QualId uid ns) = C.declQualId d
    aux [] =
        return $ Right Nothing
    aux (_:_:_) =
        panicPure "findUseOfAnon: impossible multiple use of anon decl"

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

dumpMermaid :: DefUseGraph -> String
dumpMermaid (DefUseGraph graph) = UseDefGraph.dumpMermaid graph
