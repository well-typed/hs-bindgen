-- | Declaration definition-usage graph
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
-- > import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
module HsBindgen.Frontend.Analysis.DeclUseGraph (
    -- * Definition
    DeclUseGraph -- opaque
    -- * Construction
  , fromUseDecl
    -- * Query
  , UseOfDecl(..)
  , findNamedUseOf
  , findAliasesOf
  ) where

import Control.Monad.State
import Data.Set qualified as Set

import Data.DynGraph.Labelled (DynGraph)
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
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Reverse of 'UseDeclGraph'
--
-- This graph has edges from def sites to use sites.
newtype DeclUseGraph = Wrap {
      unwrap :: DynGraph Usage QualDeclId
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromUseDecl :: UseDeclGraph -> DeclUseGraph
fromUseDecl = Wrap . DynGraph.reverse . UseDeclGraph.toDynGraph

{-------------------------------------------------------------------------------
  Query: usage of anon declarations
-------------------------------------------------------------------------------}

data UseOfDecl =
    UsedByNamed Usage C.QualName
  | UsedByAnon Usage UseOfDecl
  deriving stock (Show)

-- | Find direct or indirect use by a named declaration, if it exists
findNamedUseOf :: DeclIndex -> DeclUseGraph -> QualDeclId -> Maybe UseOfDecl
findNamedUseOf declIndex (Wrap graph) =
      flip evalState id
    . DynGraph.findTrailFrom
        graph
        (aux . map (first (declIndex DeclIndex.!)))
  where
    aux ::
         [(C.Decl Parse, Usage)] -- ^ Direct use sites
      -> State
           (UseOfDecl -> UseOfDecl)
           (Either QualDeclId (Maybe UseOfDecl))
    aux [(d, u)] = do
        case uid of
          DeclNamed name -> do
            f <- get
            return $ Right . Just $ f (UsedByNamed u (C.QualName name nk))
          DeclAnon _anonId -> do
            modify (. UsedByAnon u)
            return $ Left qid
      where
        qid@(QualDeclId uid nk) = declQualDeclId d
    aux [] =
        return $ Right Nothing
    aux (_:_:_) =
        panicPure "findUseOfAnon: impossible multiple use of anon decl"

{-------------------------------------------------------------------------------
  Query: aliases of declarations
-------------------------------------------------------------------------------}

findAliasesOf :: DeclUseGraph -> QualDeclId -> [CName]
findAliasesOf (Wrap graph) =
    mapMaybe (uncurry aux) . Set.toList . DynGraph.neighbors graph
  where
    aux :: QualDeclId -> Usage -> Maybe CName
    aux (QualDeclId (DeclNamed cname) _) (UsedInTypedef UseDeclGraph.ByValue) =
      Just cname
    aux _ _ = Nothing
