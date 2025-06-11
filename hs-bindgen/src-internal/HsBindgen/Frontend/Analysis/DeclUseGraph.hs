-- | Declaration definition-usage graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
-- > import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
module HsBindgen.Frontend.Analysis.DeclUseGraph (
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
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph, Usage (..))
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Reverse of 'UseDeclGraph'
--
-- This graph has edges from def sites to use sites.
--
-- TODO: This is wrong. The types suggest that the nested graph is the use-decl
-- graph, but it's not, it's the reverse.
newtype DeclUseGraph = Wrap {
      unwrap :: UseDeclGraph
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromUseDecl :: UseDeclGraph -> DeclUseGraph
fromUseDecl (UseDeclGraph.Wrap graph) = Wrap . UseDeclGraph.Wrap $
    DynGraph.reverse graph

{-------------------------------------------------------------------------------
  Query: usage of anon declarations
-------------------------------------------------------------------------------}

data UseOfDecl =
    UsedByNamed Usage (CName, Namespace)
  | UsedByAnon Usage UseOfDecl
  deriving stock (Show)

-- | Find direct or indirect use by a named declaration, if it exists
findNamedUseOf :: DeclIndex -> DeclUseGraph -> C.QualId Parse -> Maybe UseOfDecl
findNamedUseOf declIndex (Wrap (UseDeclGraph.Wrap useDeclGraph)) =
      flip evalState id
    . DynGraph.findTrailFrom
        useDeclGraph
        (aux . map (first (declIndex DeclIndex.!)))
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

dumpMermaid :: DeclIndex -> DeclUseGraph -> String
dumpMermaid declIndex (Wrap graph) = UseDeclGraph.dumpMermaid declIndex graph
