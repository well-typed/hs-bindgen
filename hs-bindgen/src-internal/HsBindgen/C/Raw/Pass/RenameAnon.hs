module HsBindgen.C.Raw.Pass.RenameAnon (
    Renamed
  , CName(..)
  , renameAnon
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Set qualified as Set

import HsBindgen.C.Raw.AST
import HsBindgen.C.Raw.Graph.DefUse (DefUseGraph, UseOfAnon(..))
import HsBindgen.C.Raw.Graph.DefUse qualified as DefUseGraph
import HsBindgen.C.Raw.Graph.UseDef (Usage(..), ValOrRef(..))
import HsBindgen.C.Raw.Pass.Parse
import HsBindgen.C.Raw.Pass.RenameAnon.IsPass
import HsBindgen.C.Raw.Pass.RenameAnon.ProduceCName
import HsBindgen.Errors
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Rename anonymous declarations
--
-- Precondition: input must be ordered so that def sites come before use sites.
-- This is required so that we can drop typedefs around anonymous declarations.
renameAnon :: DefUseGraph -> [Decl Parsed] -> [Decl Renamed]
renameAnon defUseGraph = catMaybes . runM defUseGraph . mapM renameOrDrop

{-------------------------------------------------------------------------------
  Internal: monad used for renaming

  NOTE: Binding specifications are used to override the /Haskell/ name that we
  produce. The names we generate for anonymous types are an /input/ to this
  process. Therefore the process of choosing these anonymous names is
  deterministic, and we do not need to keep a running mapping between 'UniqueId'
  and 'CName'.
-------------------------------------------------------------------------------}

newtype M a = WrapM {
      unwrapM :: ReaderT DefUseGraph (State DroppedTypedefs) a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadReader DefUseGraph
    , MonadState DroppedTypedefs
    )

runM :: DefUseGraph -> M a -> a
runM defUseGraph =
      flip evalState Set.empty
    . flip runReaderT defUseGraph
    . unwrapM

-- | We drop typedefs in two cases:
--
-- * When we have a typedef around an anonymous struct
-- * When we have a typedef around a struct with the same name
type DroppedTypedefs = Set Text

{-------------------------------------------------------------------------------
  Renaming logic proper
-------------------------------------------------------------------------------}

class Rename a where
  rename :: a Parsed -> M (a Renamed)

renameOrDrop :: Decl Parsed -> M (Maybe (Decl Renamed))
renameOrDrop decl@(Decl info kind) = do
    shouldDrop <- dropThisTypedef decl
    if shouldDrop then
      return Nothing
    else fmap Just $ do
      dropLaterTypedefs (declId info)
      Decl <$> rename info <*> rename kind

instance Rename DeclInfo where
  rename DeclInfo{declLoc, declId} =
      DeclInfo declLoc <$> renameDeclId declId

instance Rename DeclKind where
  rename (DeclStruct  fs) = DeclStruct  <$> mapM rename fs
  rename (DeclTypedef ty) = DeclTypedef <$> rename ty

instance Rename Field where
  rename Field{fieldName, fieldType} =
      Field fieldName <$> rename fieldType

instance Rename Type where
  rename (TypePrim    p)   = return $ TypePrim p
  rename (TypeStruct  uid) = TypeStruct  <$> renameDeclId uid
  rename (TypeTypedef uid) = TypeTypedef <$> renameDeclId uid
  rename (TypePointer ty)  = TypePointer <$> rename ty

renameDeclId :: DeclId -> M CName
renameDeclId declId = do
    defUseGraph <- ask
    case declId of
      DeclNamed name   -> return $ CName name
      DeclAnon  anonId ->
        case DefUseGraph.findUseOfAnon defUseGraph anonId of
          Nothing ->
            -- clang will actually warn about this, but we should probably
            -- return an error instead.
            panicPure $ "Anonymous type without use site: " ++ show anonId
          Just useOfAnon ->
            return $ nameForAnon useOfAnon

{-------------------------------------------------------------------------------
  Drop typedefs

  TODO: Docs
-------------------------------------------------------------------------------}

dropThisTypedef :: Decl Parsed -> M Bool
dropThisTypedef (Decl info kind) = do
    droppedTypedefs <- get
    return $
      if | DeclNamed name <- declId info
         , DeclTypedef _ <- kind
         -> Set.member name droppedTypedefs

         | otherwise
         -> False

dropLaterTypedefs :: DeclId -> M ()
dropLaterTypedefs declId = do
    mName <- aux <$> ask
    case mName of
      Nothing   -> return ()
      Just name -> modify $ Set.insert name
  where
    aux :: DefUseGraph -> Maybe Text
    aux defUseGraph = do
        anonId    <- isAnonDecl declId
        useOfAnon <- DefUseGraph.findUseOfAnon defUseGraph anonId
        case useOfAnon of
          UsedByNamed (UsedInTypedef ByValue) name -> Just name
          _otherwise                               -> Nothing

