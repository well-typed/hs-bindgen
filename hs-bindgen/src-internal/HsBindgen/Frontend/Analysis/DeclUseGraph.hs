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
    -- ** Transitive usage
  , findNamedUseOf
    -- ** Direct usage
  , getUseSites
  , findAliasesOf
  ) where

import Control.Monad.State
import Data.Set qualified as Set

import Data.DynGraph.Labelled (DynGraph)
import Data.DynGraph.Labelled qualified as DynGraph
import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Deps
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Type.DeclId
import HsBindgen.Imports
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
  | UsedByFieldOfAnon ValOrRef (FieldName Parse) UseOfDecl
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
        let mName =
              case uid of
                DeclNamed   name -> Just name
                DeclAnon    _    -> Nothing
                DeclBuiltin name -> Just name
        case mName of
          Just name -> do
            f <- get
            return $ Right . Just $ f (UsedByNamed u (C.QualName name nk))
          Nothing -> do
            modify (. usedByAnon d u)
            return $ Left qid
      where
        qid@(QualDeclId uid nk) = declQualDeclId d
    aux [] =
        return $ Right Nothing
    aux (_:_:_) =
        panicPure "findUseOfAnon: impossible multiple use of anon decl"

    usedByAnon :: C.Decl Parse -> Usage -> UseOfDecl -> UseOfDecl
    usedByAnon _ (UsedInField valOrRef name) =
        UsedByFieldOfAnon valOrRef name
    usedByAnon d _otherwise =
        -- Anonymous functions or typedefs do not exist
        panicPure $ "Unexpected anonymous " ++ show d

{-------------------------------------------------------------------------------
  Simple queries
-------------------------------------------------------------------------------}

getUseSites :: DeclUseGraph -> QualDeclId -> [(QualDeclId, Usage)]
getUseSites (Wrap graph) = Set.toList . DynGraph.neighbors graph

findAliasesOf :: DeclUseGraph -> QualDeclId -> [C.Name]
findAliasesOf graph =
    mapMaybe (uncurry aux) . getUseSites graph
  where
    aux :: QualDeclId -> Usage -> Maybe C.Name
    aux (QualDeclId (DeclNamed cname) _) (UsedInTypedef UseDeclGraph.ByValue) =
      Just cname
    aux _ _ = Nothing
