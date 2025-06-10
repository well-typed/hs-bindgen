-- | Declaration definition-usage graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Graph.DeclUse (DeclUseGraph)
-- > import HsBindgen.Frontend.Graph.DeclUse qualified as DeclUseGraph
module HsBindgen.Frontend.Graph.DeclUse (
    -- * Definition
    DeclUseGraph(..)
    -- * Construction
  , fromUseDecl
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
import HsBindgen.Frontend.Graph.UseDecl (Usage (..), UseDeclGraph (..))
import HsBindgen.Frontend.Graph.UseDecl qualified as UseDeclGraph
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Reverse of 'UseDeclGraph'
--
-- This graph has edges from def sites to use sites.
newtype DeclUseGraph = DeclUseGraph UseDeclGraph
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromUseDecl :: UseDeclGraph -> DeclUseGraph
fromUseDecl UseDeclGraph{useDeclIndex, useDeclGraph} = DeclUseGraph UseDeclGraph{
      useDeclIndex
    , useDeclGraph = DynGraph.reverse useDeclGraph
    }

{-------------------------------------------------------------------------------
  Query: usage of anon declarations
-------------------------------------------------------------------------------}

data UseOfDecl =
    UsedByNamed Usage (CName, Namespace)
  | UsedByAnon Usage UseOfDecl
  deriving stock (Show)

-- | Find direct or indirect use by a named declaration, if it exists
findNamedUseOf :: DeclUseGraph -> C.QualId Parse -> Maybe UseOfDecl
findNamedUseOf (DeclUseGraph ud@UseDeclGraph{useDeclGraph}) =
      flip evalState id
    . DynGraph.findTrailFrom
        useDeclGraph
        (aux . map (first (ud UseDeclGraph.!)))
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

dumpMermaid :: DeclUseGraph -> String
dumpMermaid (DeclUseGraph graph) = UseDeclGraph.dumpMermaid graph
