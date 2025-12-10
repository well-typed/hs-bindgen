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
    -- * Transitive usage
  , getUseSitesTransitively
    -- ** Anonymous declarations
  , UseOfDecl(..)
  , findNamedUseOf
    -- * Direct usage
  , getUseSites
  , getUseSitesNoSelfReferences
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
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Reverse of 'UseDeclGraph'
--
-- This graph has edges from def sites to use sites.
newtype DeclUseGraph = Wrap {
      unwrap :: DynGraph Usage C.PrelimDeclId
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromUseDecl :: UseDeclGraph -> DeclUseGraph
fromUseDecl = Wrap . DynGraph.reverse . UseDeclGraph.toDynGraph

{-------------------------------------------------------------------------------
  Transitive usage
-------------------------------------------------------------------------------}

getUseSitesTransitively :: DeclUseGraph -> [C.PrelimDeclId] -> Set C.PrelimDeclId
getUseSitesTransitively = DynGraph.reaches . unwrap

{-------------------------------------------------------------------------------
  Transitive usage: Anonymous declarations
-------------------------------------------------------------------------------}

data UseOfDecl =
    UsedByNamed Usage C.DeclName
  | UsedByFieldOfAnon ValOrRef (FieldName Parse) UseOfDecl
  deriving stock (Show)

-- | Find direct or indirect use of an anon decl by a named decl, if any
findNamedUseOf ::
     HasCallStack
  => DeclIndex
  -> DeclUseGraph
  -> C.PrelimDeclId
  -> Maybe UseOfDecl
findNamedUseOf declIndex (Wrap graph) = \anonDecl ->
    flip evalState id $
      DynGraph.findTrailFrom
        graph
        (aux . map (first (declIndex DeclIndex.!)))
        anonDecl
  where
    aux ::
         [(C.Decl Parse, Usage)] -- ^ Direct use sites
      -> State
           (UseOfDecl -> UseOfDecl)
           (Either C.PrelimDeclId (Maybe UseOfDecl))
    aux [(d, u)] = do
        let uid = C.declId (C.declInfo d)
        case C.prelimDeclIdName uid of
          Just name -> do
            f <- get
            return $ Right . Just $ f (UsedByNamed u name)
          Nothing -> do
            modify (. usedByAnon d u)
            return $ Left uid
    aux [] =
        return $ Right Nothing
    aux (_:_:_) =
        panicPure "findNamedUseOf: impossible multiple use of anon decl"

    usedByAnon :: C.Decl Parse -> Usage -> UseOfDecl -> UseOfDecl
    usedByAnon _ (UsedInField valOrRef name) =
        UsedByFieldOfAnon valOrRef name
    usedByAnon d _otherwise =
        -- Anonymous functions or typedefs do not exist
        panicPure $ "unexpected anonymous " ++ show d

{-------------------------------------------------------------------------------
  Direct usage
-------------------------------------------------------------------------------}

getUseSites :: DeclUseGraph -> C.PrelimDeclId -> [(C.PrelimDeclId, Usage)]
getUseSites (Wrap graph) = Set.toList . DynGraph.neighbors graph

getUseSitesNoSelfReferences :: DeclUseGraph -> C.PrelimDeclId -> [(C.PrelimDeclId, Usage)]
getUseSitesNoSelfReferences graph qualPrelimDeclId =
  filter (not . isSelfReference) $ getUseSites graph qualPrelimDeclId
    where
      isSelfReference :: (C.PrelimDeclId, Usage) -> Bool
      isSelfReference (qualPrelimDeclId', _usage) =
        qualPrelimDeclId == qualPrelimDeclId'

