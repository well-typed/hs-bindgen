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
import Data.DynGraph.Labelled (DynGraph)
import Data.DynGraph.Labelled qualified as DynGraph
import Data.Set qualified as Set

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Deps
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Reverse of 'UseDeclGraph'
--
-- This graph has edges from def sites to use sites.
newtype DeclUseGraph = Wrap {
      unwrap :: DynGraph Usage C.QualPrelimDeclId
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
    UsedByNamed Usage C.Name
  | UsedByFieldOfAnon ValOrRef (FieldName Parse) UseOfDecl
  deriving stock (Show)

-- | Find direct or indirect use by a named declaration, if it exists
findNamedUseOf ::
     DeclIndex
  -> DeclUseGraph
  -> C.QualPrelimDeclId
  -> Maybe UseOfDecl
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
           (Either C.QualPrelimDeclId (Maybe UseOfDecl))
    aux [(d, u)] = do
        let uid = C.declId (C.declInfo d)
            mName =
              case uid of
                C.PrelimDeclIdNamed   name -> Just name
                C.PrelimDeclIdAnon    _    -> Nothing
                C.PrelimDeclIdBuiltin name -> Just name
        case mName of
          Just name -> do
            f <- get
            return $ Right . Just $ f (UsedByNamed u name)
          Nothing -> do
            modify (. usedByAnon d u)
            return . Left . C.qualPrelimDeclId uid $
              (C.declKindNameKind (C.declKind d))
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

getUseSites :: DeclUseGraph -> C.QualPrelimDeclId -> [(C.QualPrelimDeclId, Usage)]
getUseSites (Wrap graph) = Set.toList . DynGraph.neighbors graph

findAliasesOf :: DeclUseGraph -> C.QualPrelimDeclId -> [C.Name]
findAliasesOf graph =
    mapMaybe (uncurry aux) . getUseSites graph
  where
    aux :: C.QualPrelimDeclId -> Usage -> Maybe C.Name
    aux (C.QualPrelimDeclIdNamed cname _) (UsedInTypedef UseDeclGraph.ByValue) =
      Just cname
    aux _ _ = Nothing
