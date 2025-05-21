module HsBindgen.Frontend.Pass.RenameAnon (
    module HsBindgen.Frontend.Pass.RenameAnon.IsPass
  , renameAnon
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Set qualified as Set

import HsBindgen.Errors
import HsBindgen.Frontend.AST
import HsBindgen.Frontend.Graph.DefUse (DefUseGraph, UseOfAnon(..))
import HsBindgen.Frontend.Graph.DefUse qualified as DefUseGraph
import HsBindgen.Frontend.Graph.UseDef (Usage(..), ValOrRef(..))
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleMacros
import HsBindgen.Frontend.Pass.Parse
import HsBindgen.Frontend.Pass.RenameAnon.IsPass
import HsBindgen.Frontend.Pass.RenameAnon.ProduceCName
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Rename anonymous declarations
--
-- Precondition: input must be ordered so that def sites come before use sites.
-- This is required so that we can drop typedefs around anonymous declarations.
renameAnon :: TranslationUnit HandleMacros -> TranslationUnit RenameAnon
renameAnon TranslationUnit{unitDecls, unitIncludeGraph, unitAnn} =
    reassemble $ catMaybes . runM defUseGraph $ mapM renameOrDrop unitDecls
  where
    defUseGraph :: DefUseGraph
    defUseGraph = DefUseGraph.fromUseDef unitAnn

    reassemble :: [Decl RenameAnon] -> TranslationUnit RenameAnon
    reassemble decls' = TranslationUnit{
          unitDecls = decls'
        , unitIncludeGraph
        , unitAnn
        }

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
  rename :: a HandleMacros -> M (a RenameAnon)

renameOrDrop :: Decl HandleMacros -> M (Maybe (Decl RenameAnon))
renameOrDrop decl@Decl{declInfo, declKind} = do
    shouldDrop <- dropThisTypedef decl
    if shouldDrop then
      return Nothing
    else fmap Just $ do
      dropLaterTypedefs (declId declInfo)
      let mkDecl ::
               DeclInfo RenameAnon
            -> DeclKind RenameAnon
            -> Decl RenameAnon
          mkDecl info' kind' = Decl{
              declInfo = info'
            , declKind = kind'
            , declAnn  = NoAnn
            }
      mkDecl <$> rename declInfo <*> rename declKind

instance Rename DeclInfo where
  rename DeclInfo{declLoc, declId} = DeclInfo declLoc <$> renameDeclId declId

instance Rename DeclKind where
  rename (DeclStruct  fs) = DeclStruct  <$> mapM rename fs
  rename DeclStructOpaque = pure $ DeclStructOpaque
  rename (DeclTypedef ty) = DeclTypedef <$> rename ty
  rename (DeclMacro   ts) = pure $ DeclMacro ts

instance Rename Field where
  rename Field{fieldName, fieldType, fieldOffset, fieldAnn} =
      pure Field
        <*> pure fieldName
        <*> rename fieldType
        <*> pure fieldOffset
        <*> pure fieldAnn

instance Rename Typedef where
  rename Typedef{typedefType, typedefAnn} =
      pure Typedef
        <*> rename typedefType
        <*> pure typedefAnn

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

  When we have a typedef around an anonymous declaration:

  > typedef struct {
  >   int x;
  >   int y;
  > } foo;

  then we use the name of the typedef for the name of the struct, and then
  omit the typedef altogether from the AST.
-------------------------------------------------------------------------------}

dropThisTypedef :: Decl HandleMacros -> M Bool
dropThisTypedef Decl{declInfo, declKind} = do
    droppedTypedefs <- get
    return $
      if | DeclNamed name <- declId declInfo
         , DeclTypedef _ <- declKind
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

