module HsBindgen.Frontend.Pass.MangleNames (
    mangleNames
  ) where

import Control.Applicative (asum)
import Control.Monad.Except (ExceptT (..), MonadError (..), liftEither,
                             runExcept, runExceptT)
import Control.Monad.Reader
import Control.Monad.State
import Data.Either (partitionEithers)
import Data.Foldable qualified as Foldable
import Data.Function
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Proxy
import Data.Set qualified as Set

import Clang.HighLevel.Types

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config.MangleCandidate (MangleCandidate (..))
import HsBindgen.Config.MangleCandidate qualified as MangleCandidate
import HsBindgen.Config.Prelims (FieldNamingStrategy (..))
import HsBindgen.Frontend.Analysis.DeclIndex (Squashed (..))
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.Typedefs (TypedefAnalysis)
import HsBindgen.Frontend.Analysis.Typedefs qualified as TypedefAnalysis
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.MangleNames.Error
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer (WithCallStack, withCallStack)

import Doxygen.Parser.Types qualified as Doxy

{-------------------------------------------------------------------------------
  Top-level

  We proceed in two passes: first we go over all def sites (declarations), and
  choose names; then in a second pass we patch up all references (in both def
  and use sites). This is necessary because although we will encounter
  declarations before they are "used", a reference through a pointer field does
  not count as a "use".
-------------------------------------------------------------------------------}

mangleNames ::
     HasCallStack
  => FieldNamingStrategy
  -> C.TranslationUnit ResolveBindingSpecs
  -> (C.TranslationUnit MangleNames, [Msg MangleNames])
mangleNames fieldNaming unit = (
         C.TranslationUnit{
           decls        = decls2
         , includeGraph = unit.includeGraph
         , ann          = updateDeclMeta
                            nameMap
                            (failures1 ++ failures2)
                            squashes
                            unit.ann
        }
    , msgs1 ++ msgs2
    )
  where
    typedefAnalysis :: TypedefAnalysis
    typedefAnalysis = TypedefAnalysis.fromDecls unit.ann.declUseGraph unit.decls

    mangleCandidateConfig :: MangleCandidate Maybe
    mangleCandidateConfig = MangleCandidate.mangleCandidateDefault

    -- Pass 1.
    nameMap   :: NameMap
    failures1 :: [MangleNamesFailure]
    msgs1     :: [Msg MangleNames]
    (decls1, nameMap, failures1, msgs1) =
      chooseNames typedefAnalysis mangleCandidateConfig unit.decls

    -- Pass 2.
    env :: Env
    env = Env{
          typedefAnalysis     = typedefAnalysis
        , mangleCandidate     = mangleCandidateConfig
        , nameMap             = nameMap
        , fieldNamingStrategy = fieldNaming
        }

    mangleDeclResults   :: [MangleDeclResult]
    msgs2 :: [Msg MangleNames]
    (mangleDeclResults, msgs2) = runM env $ mapM mangleDecl decls1

    failures2 :: [MangleNamesFailure]
    squashes  :: [MangleNamesSquash]
    (failures2, squashes, decls2) = partitionResults mangleDeclResults

    partitionResults ::
         [MangleDeclResult]
      -> ( [MangleNamesFailure]
         , [MangleNamesSquash]
         , [C.Decl MangleNames] )
    partitionResults = rev . Foldable.foldl' aux ([], [], [])
      where
        aux (fs, ss, ds) x = case x of
          MdrFailure  f -> (f:fs, ss,   ds)
          MdrSquashed s -> (fs,   s:ss, ds)
          MdrMangled  d -> (fs,   ss,   d:ds)

        rev (fs, ss, ds) = (reverse fs, reverse ss, reverse ds)

updateDeclMeta ::
      NameMap
   -> [MangleNamesFailure]
   -> [MangleNamesSquash]
   -> DeclMeta
   -> DeclMeta
updateDeclMeta nameMap failures squashes declMeta = declMeta{
      declIndex =
        DeclIndex.registerMangleNamesFailure failuresMap $
        DeclIndex.registerSquashedDeclarations squashesMap $
          declMeta.declIndex
    }
  where
    failuresMap :: Map DeclId (SingleLoc, MangleNamesError)
    failuresMap = Map.fromList $ map (\f -> (f.id, (f.loc, f.err))) failures

    squashesMap ::  Map DeclId Squashed
    squashesMap = Map.fromList $ map (\s -> (s.id, toSquashed s.squash)) squashes

    toSquashed :: TypedefAnalysis.Squash -> Squashed
    toSquashed s = Squashed {
        typedefLoc   = s.typedefLoc
      , targetNameC  = s.targetId
      , targetNameHs = lookupType s.targetId nameMap
      }

{-------------------------------------------------------------------------------
  Pass 1: Choose names
-------------------------------------------------------------------------------}

chooseNames ::
     HasCallStack
  => TypedefAnalysis
  -> MangleCandidate Maybe
  -> [C.Decl ResolveBindingSpecs]
  -> ([C.Decl ResolveBindingSpecs], NameMap, [MangleNamesFailure], [Msg MangleNames])
chooseNames td mc decls =
    let specifiedNames :: Map DeclId (Hs.Name Hs.NsTypeConstr)
        specifiedNames = Map.fromList $ mapMaybe getSpecifiedName decls

        nameInfos :: [NameInfo]
        failures  :: [MangleNamesFailure]
        messages  :: [Msg MangleNames]
        ((failures, nameInfos), messages) =
          bimap partitionEithers mconcat $ unzip $
            map (nameForDecl td mc specifiedNames) decls

        nameMap :: NameMap
        nameMap = fromDeclIdPairs $
          map (\n -> DeclIdPair n.cName n.hsName) nameInfos

        -- When detecting collisions, we only use original (i.e., non-squashed)
        -- declarations.
        nameInfosOriginal :: [NameInfo]
        nameInfosOriginal = filter (not . (.squashed)) nameInfos

        collisions :: Map Hs.SomeName [(DeclId, SingleLoc)]
        collisions = getDuplicates $ Map.fromList $
          map (\n -> ((n.cName, n.loc), n.hsName)) nameInfosOriginal

        collisionFailures :: [MangleNamesFailure]
        collisionFailures = concatMap getCollisionFailures $ Map.toList collisions

        collidingDeclIds :: Set DeclId
        collidingDeclIds = Set.fromList $ map (.id) collisionFailures

        okDecls :: [C.Decl ResolveBindingSpecs]
        okDecls = filter (\x -> Set.notMember x.info.id collidingDeclIds) decls

    in  (okDecls, nameMap, failures ++ collisionFailures, messages)
  where
    getSpecifiedName ::
      C.Decl ResolveBindingSpecs -> Maybe (DeclId, Hs.Name Hs.NsTypeConstr)
    getSpecifiedName decl = (decl.info.id,) <$> ((.hsName) =<< decl.ann.cSpec)

getCollisionFailures :: (Hs.SomeName, [(DeclId, SingleLoc)]) -> [MangleNamesFailure]
getCollisionFailures (i, xs) =
    [ MangleNamesFailure d l $ MangleNamesCollision i idsWithLocs | (d, l) <- xs ]
  where
    idsWithLocs :: [WithLocationInfo DeclId]
    idsWithLocs = map (\(d, l) -> WithLocationInfo (declIdLocationInfo d [l]) d) xs

-- | Internal.
data NameInfo = NameInfo {
    cName    :: DeclId
  , hsName   :: Hs.SomeName
    -- | We need the location to obtain 'HsBindgen.Frontend.LocationInfo.WithLocationInfo'
  , loc      :: SingleLoc
    -- | We expect name collisions for squashed declarations
  , squashed :: Bool
  }
  deriving stock (Eq, Show)

nameForDecl ::
     HasCallStack
  => TypedefAnalysis
  -> MangleCandidate Maybe
  -> Map DeclId (Hs.Name Hs.NsTypeConstr)
  -> C.Decl ResolveBindingSpecs
  -> (Either MangleNamesFailure NameInfo, [Msg MangleNames])
nameForDecl td mc specifiedNames decl =
    withDeclNamespace decl.kind $ \(nsProxy :: Proxy ns) ->
      let mangleCandidate' :: Text -> Either MangleNamesError (Hs.Name ns)
          mangleCandidate' d = runExcept $ mangleCandidate mc nsProxy d
      in case Map.lookup declId specifiedNames of
        Just hsNm ->
          let -- Binding spec specified a name for this declaration. In this
              -- case, this overrides any naming decisions we might make here.
              --
              -- TODO <https://github.com/well-typed/hs-bindgen/issues/1436>
              -- When squashing becomes configurable, we need to update the
              -- logic for `isSquashed` here.
              isSquashed :: Bool
              isSquashed = case Map.lookup declId td.map of
                Just TypedefAnalysis.Squash{} -> True
                _otherwise                    -> False
          in  ( Right $ NameInfo declId (Hs.demoteNs hsNm) loc isSquashed
              , []
              )
        Nothing ->
          case Map.lookup declId td.map of
            Nothing ->
              mangleCandidate' declId.name.text & \case
                Left err -> failWith err
                Right hsNm ->
                  ( Right $ NameInfo declId (Hs.demoteNs hsNm) loc False
                  , [] )
            Just (TypedefAnalysis.AddSuffix suffix) ->
              mangleCandidate' (declId.name.text <> suffix) & \case
                Left err -> failWith err
                Right hsNm ->
                  ( Right $ NameInfo declId (Hs.demoteNs hsNm) loc False
                  , (toMs [MangleNamesAssignedName (Hs.demoteNs hsNm)])
                  )
            Just (TypedefAnalysis.UseNameOf declId') ->
              case Map.lookup declId' specifiedNames of
                Just hsNm ->
                  ( Right $ NameInfo declId (Hs.demoteNs hsNm) loc False
                  , [] )
                Nothing ->
                  mangleCandidate' declId'.name.text & \case
                    Left err -> failWith err
                    Right hsNm ->
                      ( Right $ NameInfo declId (Hs.demoteNs hsNm) loc False
                      , toMs [ MangleNamesAssignedName (Hs.demoteNs hsNm)
                             | declId.name.text /= declId'.name.text ] )
            Just (TypedefAnalysis.Squash s) ->
              case Map.lookup s.targetId specifiedNames of
                Just hsNm ->
                  ( Right $ NameInfo declId (Hs.demoteNs hsNm) loc True
                  , [] )
                Nothing ->
                  mangleCandidate' declId.name.text & \case
                    Left err -> failWith err
                    Right hsNm ->
                      ( Right $ NameInfo declId (Hs.demoteNs hsNm) loc True
                      , [] )
  where
    declId :: DeclId
    declId = decl.info.id

    info :: C.DeclInfo ResolveBindingSpecs
    info = decl.info

    loc :: SingleLoc
    loc = decl.info.loc

    failWith :: MangleNamesError -> (Either MangleNamesFailure a, [Msg MangleNames])
    failWith err = (Left $ toFailure info err, [])

    toMs :: HasCallStack => [a] -> [WithCallStack (WithLocationInfo a)]
    toMs = map (withCallStack . withDeclLoc decl.info)

{-------------------------------------------------------------------------------
  Internal: working with 'MangleCandidate'
-------------------------------------------------------------------------------}

mangleCandidate :: forall ns.
     Hs.SingNamespace ns
  => MangleCandidate Maybe
  -> Proxy ns
  -> Text
  -> E Identity (Hs.Name ns)
mangleCandidate mc _ cName =
    case MangleCandidate.mangleCandidate mc cName of
      Just hsName -> pure hsName
      Nothing     -> throwError $ MangleNamesCouldNotMangle cName

{-------------------------------------------------------------------------------
  Internal: monad for pass 2, applying the name map
-------------------------------------------------------------------------------}

type E m a = ExceptT MangleNamesError m a

newtype M a = WrapM (
      StateT [Msg MangleNames] (Reader Env) a
    )
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadReader Env
    )

data MangleNamesFailure = MangleNamesFailure {
      id  :: DeclId
    , loc :: SingleLoc
    , err :: MangleNamesError
    }
  deriving (Show, Eq, Ord, Generic)

toFailure :: C.DeclInfo ResolveBindingSpecs -> MangleNamesError -> MangleNamesFailure
toFailure i e = MangleNamesFailure i.id i.loc e

data MangleNamesSquash = MangleNamesSquash {
      id     :: DeclId
    , squash :: TypedefAnalysis.Squash
}
  deriving (Show, Eq, Generic)

data Env = Env{
      typedefAnalysis     :: TypedefAnalysis
    , nameMap             :: NameMap
    , mangleCandidate     :: MangleCandidate Maybe
    , fieldNamingStrategy :: FieldNamingStrategy
    }
  deriving stock (Generic)

runM :: Env -> M a -> (a, [Msg MangleNames])
runM env (WrapM ma) = second reverse . flip runReader env $ runStateT ma mempty

checkTypedefAnalysis :: DeclId -> M (Maybe TypedefAnalysis.Conclusion)
checkTypedefAnalysis declId = do
    td <- asks (.typedefAnalysis)
    return $ Map.lookup declId td.map

{-------------------------------------------------------------------------------
  Name map
-------------------------------------------------------------------------------}

data NameMap = NameMap {
      typeConstrs :: Map DeclId (Hs.Name Hs.NsTypeConstr)
    , dataConstrs :: Map DeclId (Hs.Name Hs.NsConstr)
    , vars        :: Map DeclId (Hs.Name Hs.NsVar)
    }
  deriving stock (Show, Eq, Generic)

emptyNameMap :: NameMap
emptyNameMap = NameMap Map.empty Map.empty Map.empty

fromDeclIdPairs :: [DeclIdPair] -> NameMap
fromDeclIdPairs = Foldable.foldl' aux emptyNameMap
  where
    aux :: NameMap -> DeclIdPair -> NameMap
    aux nameMap pair = case pair.hsName.ns of
      Hs.NsTypeConstr ->
        nameMap & #typeConstrs %~
          Map.insert pair.cName (Hs.assertNs (Proxy @Hs.NsTypeConstr) pair.hsName)
      Hs.NsConstr ->
        nameMap & #dataConstrs %~
          Map.insert pair.cName (Hs.assertNs (Proxy @Hs.NsConstr) pair.hsName)
      Hs.NsVar ->
        nameMap & #vars %~
          Map.insert pair.cName (Hs.assertNs (Proxy @Hs.NsVar) pair.hsName)

lookupType :: DeclId -> NameMap -> Maybe (Hs.Name Hs.NsTypeConstr)
lookupType declId nameMap = Map.lookup declId nameMap.typeConstrs

lookupTypeE :: DeclId -> E M (Hs.Name Hs.NsTypeConstr)
lookupTypeE declId = do
    nameMap <- asks (.nameMap)
    case lookupType declId nameMap of
      Nothing ->
        throwError $
          MangleNamesUnderlyingDeclNotMangled declId (NonEmpty.singleton Hs.NsTypeConstr)
      Just hsNm ->
        pure hsNm

lookupTypePair :: DeclId -> E M DeclIdPair
lookupTypePair declId = lookupTypeE declId >>= \hsName -> pure $ DeclIdPair{
        cName  = declId
      , hsName = Hs.demoteNs hsName
    }

lookupData :: DeclId -> NameMap -> Maybe (Hs.Name Hs.NsConstr)
lookupData declId nameMap = Map.lookup declId nameMap.dataConstrs

lookupDataE :: DeclId -> E M (Hs.Name Hs.NsConstr)
lookupDataE declId = do
    nameMap <- asks (.nameMap)
    case lookupData declId nameMap of
      Nothing ->
        throwError $
          MangleNamesUnderlyingDeclNotMangled declId (NonEmpty.singleton Hs.NsConstr)
      Just hsNm ->
        pure hsNm

lookupVar :: DeclId -> NameMap -> Maybe (Hs.Name Hs.NsVar)
lookupVar declId nameMap = Map.lookup declId nameMap.vars

lookupVarE :: DeclId -> E M (Hs.Name Hs.NsVar)
lookupVarE declId = do
    nameMap <- asks (.nameMap)
    case lookupVar declId nameMap of
      Nothing ->
        throwError $
          MangleNamesUnderlyingDeclNotMangled declId (NonEmpty.singleton Hs.NsVar)
      Just hsNm ->
        pure hsNm

lookupVarPair :: DeclId -> E M DeclIdPair
lookupVarPair declId = lookupVarE declId >>= \hsName -> pure $ DeclIdPair{
        cName  = declId
      , hsName = Hs.demoteNs hsName
    }

{-------------------------------------------------------------------------------
  Pass 2: Apply NameMap
-------------------------------------------------------------------------------}

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1432>
--
-- Names created in this second pass are not checked for duplicates! For
-- example, the name of the auxiliary @typedef@ for @struct@s containing a FLAM
-- could clash with another type constructor name.

class Mangle a where
  mangle ::
       a ResolveBindingSpecs
    -> E M (a MangleNames)

class MangleWithDeclName a where
  mangleWithDeclName :: Text -> a ResolveBindingSpecs -> E M (a MangleNames)

data MangleDeclResult =
      MdrFailure  MangleNamesFailure
    | MdrSquashed MangleNamesSquash
    | MdrMangled (C.Decl MangleNames)

mangleDecl :: C.Decl ResolveBindingSpecs -> M MangleDeclResult
mangleDecl decl = do
    mConclusion <- checkTypedefAnalysis decl.info.id
    case mConclusion of
      Just (TypedefAnalysis.Squash s) ->
        -- We issue delayed squashed messages for selected declarations in the
        -- `Select` pass.
        pure $ MdrSquashed $ MangleNamesSquash decl.info.id s
      _otherwise -> withDeclNamespace decl.kind $ \(nsProxy :: Proxy ns) -> do
        eDecl' <- runExceptT $ do
          (hsName, info') <- mangleDeclInfo nsProxy decl.info
          let hsId :: Text
              hsId = hsName.text
          kind' <- case decl.kind of
            C.DeclStruct   x         -> C.DeclStruct           <$> mangleWithDeclName hsId x
            C.DeclUnion    x         -> C.DeclUnion            <$> mangleWithDeclName hsId x
            C.DeclEnum     x         -> C.DeclEnum             <$> mangleWithDeclName hsId x
            C.DeclAnonEnumConstant x -> C.DeclAnonEnumConstant <$> mangle x
            C.DeclTypedef  x         -> C.DeclTypedef          <$> mangleWithDeclName hsId x
            C.DeclFunction x         -> C.DeclFunction         <$> mangle x
            C.DeclMacro    x         -> C.DeclMacro            <$> mangleWithDeclName hsId x
            C.DeclGlobal   x         -> C.DeclGlobal           <$> mangle x
            C.DeclOpaque             -> pure C.DeclOpaque
          pure $ C.Decl{
              info = info'
            , kind = kind'
            , ann  = decl.ann
            }
        pure $ case eDecl' of
          Left err    -> MdrFailure $ toFailure decl.info err
          Right decl' -> MdrMangled decl'

{-------------------------------------------------------------------------------
  Scoped names
-------------------------------------------------------------------------------}

-- | Apply Haskell naming rules
mkIdentifier ::
     Hs.SingNamespace ns
  => Proxy ns -> Text -> E M (Hs.Name ns)
mkIdentifier ns candidate = do
    mc <- asks (.mangleCandidate)
    liftEither . runExcept $ mangleCandidate mc ns candidate

mangleFieldName ::
     Text
  -> CScopedName
  -> E M ScopedNamePair
mangleFieldName hsName fieldCName = do
    strategy <- asks (.fieldNamingStrategy)
    let candidate :: Text
        candidate = case strategy of
          AddFieldPrefixes ->
            hsName <> "_" <> fieldCName.text
          OmitFieldPrefixes ->
            fieldCName.text
    name <- mkIdentifier (Proxy @Hs.NsVar) candidate
    return ScopedNamePair{
        cName  = fieldCName
      , hsName = Hs.demoteNs name
      }

mangleAccessorName ::
     Text
  -> CScopedName
  -> E M (Hs.Name Hs.NsVar)
mangleAccessorName hsName fieldCName =
    mkIdentifier (Proxy @Hs.NsVar) candidate
  where
    candidate :: Text
    candidate =
      hsName <> "_" <> fieldCName.text

-- | Mangle enum constant name
--
-- Since these live in the global namespace, we do not prepend the name of
-- the enclosing enum.
mangleEnumConstant ::
     CScopedName
  -> E M ScopedNamePair
mangleEnumConstant cName = do
    name <- mkIdentifier (Proxy @Hs.NsConstr) cName.text
    return ScopedNamePair{
        cName  = cName
      , hsName = Hs.demoteNs name
      }

-- | Mangle function argument name
--
-- Function argument names are not really used when generating Haskell code.
-- They are more relevant for documentation purposes so we don't do any
-- mangling.
mangleArgumentName ::
     CScopedName
  -> E M ScopedNamePair
mangleArgumentName argName = do
    name <- mkIdentifier (Proxy @Hs.NsVar) argName.text
    return ScopedNamePair{
        cName  = argName
      , hsName = Hs.demoteNs name
      }

{-------------------------------------------------------------------------------
  Additional name mangling functionality
-------------------------------------------------------------------------------}

mkStructNames :: Maybe a -> Text -> E M StructNames
mkStructNames mFlam name = do
    constrName <- mkIdentifier (Proxy @Hs.NsConstr) name
    let auxName = name <> "_Aux"
    mFlamAux <- case mFlam of
      Nothing -> pure Nothing
      Just _  -> Just <$> mkIdentifier (Proxy @Hs.NsTypeConstr) auxName
    pure StructNames{
        constr  = constrName
      , flamAux = mFlamAux
      }

mkFieldName ::
  FieldNamingStrategy -> Text -> E M (Hs.Name Hs.NsVar)
mkFieldName strategy name = mkIdentifier (Proxy @Hs.NsVar) $ case strategy of
    AddFieldPrefixes  -> "unwrap" <> name
    OmitFieldPrefixes -> "unwrap"

-- | Generic construction of newtype names, given only the type name
mkNewtypeNames ::
  FieldNamingStrategy -> Text -> E M NewtypeNames
mkNewtypeNames strategy name = do
    constrName <- mkIdentifier (Proxy @Hs.NsConstr) name
    fieldName  <- mkFieldName strategy name
    pure $ NewtypeNames{
      dataConstr = constrName
    , field      = fieldName
    }

-- | Union names
--
-- A union is represented by a newtype around the raw bytes.
mkUnionNames ::
  FieldNamingStrategy -> Text -> E M NewtypeNames
mkUnionNames = mkNewtypeNames

-- | Enum names
--
-- An enum is represented by a newtype around an integral value.
mkEnumNames ::
  FieldNamingStrategy -> Text -> E M NewtypeNames
mkEnumNames = mkNewtypeNames

-- | Typedef
--
-- Typedefs are represented by newtypes
mkTypedefNames ::
  Maybe a -> FieldNamingStrategy -> Text -> E M TypedefNames
mkTypedefNames mFunPtr strategy name = do
    orig <- mkNewtypeNames strategy name
    aux  <- case mFunPtr of
      Nothing -> pure Nothing
      Just _  -> do
        auxName  <- mkIdentifier (Proxy @Hs.NsTypeConstr) $ name <> "_Aux"
        auxNames <- mkNewtypeNames strategy auxName.text
        pure $ Just (auxName, auxNames)
    pure TypedefNames{
        orig = orig
      , aux  = aux
      }

-- | Macro types
--
-- These behave like typedefs.
mkMacroTypeNames ::
  FieldNamingStrategy -> Text -> E M NewtypeNames
mkMacroTypeNames = mkNewtypeNames

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

class Hs.SingNamespace ns => HasName ns where
  getName :: DeclId -> E M (Hs.Name ns)

instance HasName Hs.NsTypeConstr where
  getName = lookupTypeE

instance HasName Hs.NsConstr where
  getName = lookupDataE

instance HasName Hs.NsVar where
  getName = lookupVarE

mangleDeclInfo ::
     forall ns. HasName ns
  => Proxy ns
  -> C.DeclInfo ResolveBindingSpecs
  -> E M (Hs.Name ns, C.DeclInfo MangleNames)
mangleDeclInfo _ info = do
    hsName     <- getName info.id
    comment'   <- traverse mangle info.comment
    enclosing' <- mapM mangleEnclosingRef info.enclosing
    let info' = C.DeclInfo{
            loc          = info.loc
          , id           = DeclIdPair{
              cName  = info.id
            , hsName = Hs.demoteNs hsName
            }
          , seqNr        = info.seqNr
          , headerInfo   = info.headerInfo
          , availability = info.availability
          , comment      = comment'
          , enclosing    = enclosing'
          }
    pure (hsName, info')

mangleEnclosingRef ::
     C.EnclosingRef ResolveBindingSpecs
  -> E M (C.EnclosingRef MangleNames)
mangleEnclosingRef = \case
    C.EnclosingRef e -> do
      -- We must not propagate potential name mangler failures of the enclosing
      -- declaration, which is a backwards reference. Instead, we mark the
      -- reference "unusable".
      r <- tryError $ lookupTypePair e
      case r of
        Left  _ -> pure $ C.UnusableEnclosingRef e
        Right m -> pure $ C.EnclosingRef m
    C.UnusableEnclosingRef e ->
      pure $ C.UnusableEnclosingRef e

instance MangleWithDeclName C.DeclKind where
  mangleWithDeclName hsName = \case
      C.DeclStruct   x         -> C.DeclStruct           <$> mangleWithDeclName hsName x
      C.DeclUnion    x         -> C.DeclUnion            <$> mangleWithDeclName hsName x
      C.DeclEnum     x         -> C.DeclEnum             <$> mangleWithDeclName hsName x
      C.DeclAnonEnumConstant x -> C.DeclAnonEnumConstant <$> mangle x
      C.DeclTypedef  x         -> C.DeclTypedef          <$> mangleWithDeclName hsName x
      C.DeclFunction x         -> C.DeclFunction         <$> mangle x
      C.DeclMacro    x         -> C.DeclMacro            <$> mangleWithDeclName hsName x
      C.DeclGlobal   x         -> C.DeclGlobal           <$> mangle x
      C.DeclOpaque             -> pure C.DeclOpaque

instance MangleWithDeclName C.Struct where
  mangleWithDeclName hsName struct = do
      fields <- mapM (mangleWithDeclName hsName) struct.fields
      flam   <- mapM (mangleWithDeclName hsName) struct.flam
      names  <- mkStructNames struct.flam hsName
      pure $ reconstruct fields flam names
    where
      reconstruct ::
           [C.StructField MangleNames]
        -> Maybe (C.StructField MangleNames)
        -> StructNames
        -> C.Struct MangleNames
      reconstruct structFields' structFlam' structNames = C.Struct{
            fields    = structFields'
          , flam      = structFlam'
          , ann       = structNames
          , sizeof    = struct.sizeof
          , alignment = struct.alignment
          }

instance MangleWithDeclName C.StructField where
  mangleWithDeclName hsName field = do
      reconstruct
         <$> mangleFieldName hsName field.info.name
         <*> mangle field.typ
         <*> mapM mangle field.info.comment
    where
      reconstruct ::
           ScopedNamePair
        -> C.Type MangleNames
        -> Maybe (C.Comment MangleNames)
        -> C.StructField MangleNames
      reconstruct structFieldName' structFieldType' structFieldComment' =
          C.StructField {
              info   = C.FieldInfo {
                           loc     = field.info.loc
                         , name    = structFieldName'
                         , comment = structFieldComment'
                         }
            , typ    = structFieldType'
            , offset = field.offset
            , width  = field.width
            , ann    = field.ann
            }

instance MangleWithDeclName C.Union where
  mangleWithDeclName hsName union = do
      strategy <- asks (.fieldNamingStrategy)
      unionNames <- mkUnionNames strategy hsName
      reconstruct unionNames <$> mapM (mangleWithDeclName hsName) union.fields
    where
      reconstruct :: NewtypeNames -> [C.UnionField MangleNames] -> C.Union MangleNames
      reconstruct unionNames unionFields' = C.Union{
            fields    = unionFields'
          , ann       = unionNames
          , sizeof    = union.sizeof
          , alignment = union.alignment
          }

instance MangleWithDeclName C.UnionField where
  mangleWithDeclName hsName field = do
      fieldName    <- mangleFieldName hsName field.info.name
      fieldType    <- mangle field.typ
      fieldComment <- mapM mangle field.info.comment
      accessorName <- mangleAccessorName hsName field.info.name
      getterName   <- mkIdentifier (Proxy @Hs.NsVar) $ "get_" <> accessorName.text
      setterName   <- mkIdentifier (Proxy @Hs.NsVar) $ "set_" <> accessorName.text
      pure $
        C.UnionField {
            info = C.FieldInfo {
                       loc     = field.info.loc
                     , name    = fieldName
                     , comment = fieldComment
                     }
          , typ  = fieldType
          , ann  = UnionFieldNames {
              getter = getterName
            , setter = setterName
            }
          }

instance MangleWithDeclName C.Enum where
  mangleWithDeclName hsName enum = do
      strategy <- asks (.fieldNamingStrategy)
      enumNames <- mkEnumNames strategy hsName
      reconstruct enumNames
        <$> mangle enum.typ
        <*> mapM mangle enum.constants
    where
      reconstruct ::
           NewtypeNames
        -> C.Type MangleNames
        -> [C.EnumConstant MangleNames]
        -> C.Enum MangleNames
      reconstruct enumNames enumType' enumConstants' = C.Enum{
            typ       = enumType'
          , constants = enumConstants'
          , ann       = enumNames
          , sizeof    = enum.sizeof
          , alignment = enum.alignment
          }

instance Mangle C.AnonEnumConstant where
  mangle (C.AnonEnumConstant primTyp constant') = do
      reconstruct <$> mangle constant'
    where
      reconstruct :: C.EnumConstant MangleNames -> C.AnonEnumConstant MangleNames
      reconstruct enumConstants' = C.AnonEnumConstant{
            typ      = primTyp
          , constant = enumConstants'
          }

instance Mangle C.EnumConstant where
  mangle constant = do
      reconstruct
        <$> mangleEnumConstant constant.info.name
        <*> mapM mangle constant.info.comment
    where
      reconstruct ::
            ScopedNamePair
         -> Maybe (C.Comment MangleNames)
         -> C.EnumConstant MangleNames
      reconstruct enumConstantName' enumConstantComment' = C.EnumConstant{
            info  = C.FieldInfo {
                        loc     = constant.info.loc
                      , name    = enumConstantName'
                      , comment = enumConstantComment'
                      }
          , value = constant.value
          }

instance Mangle C.Comment where
  mangle (C.Comment comment) = C.Comment <$> mapM mangle comment


instance Mangle C.CommentRef where
    mangle = \case
      (C.CommentRef name Nothing mKind) -> do
        mTarget <- lift $ searchNameMap name mKind
        pure $ C.CommentRef name mTarget mKind
      (C.CommentRef name (Just declId) mKind) ->
        (\pair -> C.CommentRef name (Just pair) mKind) <$> lookupAnyPair declId
      where
        -- | Dispatch on the Doxygen @kindref@ attribute to choose which
        -- 'CNameKind' values to try. Per-kind lookup is handled by
        -- 'lookupByKind', which narrows sub-map searches:
        --
        -- * Compound → only tagged kinds, only 'typeConstrs' (3 lookups)
        -- * Member  → ordinary + macro kinds, 'typeConstrs' + 'vars' (4 lookups)
        -- * Nothing → all kinds, per-kind dispatch (7 lookups, fallback)
        searchNameMap :: Text -> Maybe Doxy.RefKind -> M (Maybe DeclIdPair)
        searchNameMap name = \case
            Just Doxy.RefCompound ->
              searchKinds
                [ CNameKindTagged CTagKindStruct
                , CNameKindTagged CTagKindUnion
                , CNameKindTagged CTagKindEnum ]
                name
            Just Doxy.RefMember ->
              searchKinds [CNameKindOrdinary, CNameKindMacro] name
            Nothing ->
              searchKinds [minBound .. maxBound] name

        -- | For each 'CNameKind', construct a 'DeclId' and try the
        -- appropriate sub-maps via 'lookupByKind'. Return the first match.
        searchKinds :: [CNameKind] -> Text -> M (Maybe DeclIdPair)
        searchKinds kinds name = do
            nameMap <- asks (.nameMap)
            pure $ asum
              [ lookupByKind kind (DeclId (CDeclName name kind) False) nameMap
              | kind <- kinds
              ]

        -- | Dispatch to the appropriate sub-map(s) based on 'CNameKind'.
        --
        -- Tagged names can only be type constructors; ordinary and macro
        -- names can be type constructors or variables.
        lookupByKind :: CNameKind -> DeclId -> NameMap -> Maybe DeclIdPair
        lookupByKind = \case
            CNameKindTagged{} -> lookupTypeNs
            _                 -> lookupTypeOrVarNs

        lookupTypeNs :: DeclId -> NameMap -> Maybe DeclIdPair
        lookupTypeNs declId nameMap =
            DeclIdPair declId . Hs.demoteNs <$> lookupType declId nameMap

        lookupTypeOrVarNs :: DeclId -> NameMap -> Maybe DeclIdPair
        lookupTypeOrVarNs declId nameMap =
            DeclIdPair declId <$> asum
              [ Hs.demoteNs <$> lookupType declId nameMap
              , Hs.demoteNs <$> lookupVar  declId nameMap
              ]

        lookupAnyPair :: DeclId -> E M DeclIdPair
        lookupAnyPair declId = do
            nameMap <- asks (.nameMap)
            case lookupByKind declId.name.kind declId nameMap of
              Nothing ->
                throwError $
                  MangleNamesUnderlyingDeclNotMangled declId $ case declId.name.kind of
                    CNameKindTagged{} -> NonEmpty.singleton Hs.NsTypeConstr
                    _                 -> NonEmpty.fromList [Hs.NsTypeConstr, Hs.NsVar]
              Just pair ->
                pure pair

instance MangleWithDeclName C.Typedef where
  mangleWithDeclName hsName typedef = do
      strategy <- asks (.fieldNamingStrategy)
      names    <- mkTypedefNames mFunPtr strategy hsName
      reconstruct names <$> mangle typedef.typ
    where
      reconstruct :: TypedefNames -> C.Type MangleNames -> C.Typedef MangleNames
      reconstruct names typ = C.Typedef{
            typ = typ
          , ann = names
          }

      -- TODO https://github.com/well-typed/hs-bindgen/issues/1925
      --
      -- Tie generation of names to the generation of the associated code. This
      -- is especially ugly.
      mFunPtr :: Maybe (C.Typedef ResolveBindingSpecs)
      mFunPtr = case typedef.typ of
        (C.TypePointers _ (C.TypeFun _ _)) -> Just typedef
        _                                  -> Nothing

instance Mangle C.Function where
  mangle function = do
      reconstruct
        <$> mapM mangleFunctionArg function.args
        <*> mangle function.res
    where
      mangleFunctionArg ::
           C.FunctionArg ResolveBindingSpecs
        -> E M (C.FunctionArg MangleNames)
      mangleFunctionArg functionArg = do
          name'   <- traverse mangleArgumentName functionArg.name
          argTyp' <- mangle functionArg.argTyp
          pure C.FunctionArg {
              name = name'
            , argTyp = argTyp'
            }

      reconstruct ::
           [C.FunctionArg MangleNames]
        -> C.Type MangleNames
        -> C.Function MangleNames
      reconstruct functionArgs' functionRes' = C.Function{
            args  = functionArgs'
          , res   = functionRes'
          , attrs = function.attrs
          , ann   = function.ann
          }

instance Mangle C.Global where
  mangle global = do
      typ' <- mangle global.typ
      pure C.Global{
          typ = typ'
        , ann = global.ann
        }

instance MangleWithDeclName CheckedMacro where
  mangleWithDeclName hsName = \case
      MacroType typ  -> MacroType <$> mangleWithDeclName hsName typ
      MacroExpr expr -> MacroExpr <$> mangle expr

instance MangleWithDeclName CheckedMacroType where
  mangleWithDeclName hsName macroType = do
      strategy <- asks (.fieldNamingStrategy)
      macroTypeNames <- mkMacroTypeNames strategy hsName
      reconstruct macroTypeNames <$> mangle macroType.typ
    where
      reconstruct :: NewtypeNames -> C.Type MangleNames -> CheckedMacroType MangleNames
      reconstruct macroTypeNames typ' = CheckedMacroType{
            typ = typ'
          , ann = macroTypeNames
          }

instance Mangle CheckedMacroExpr where
  mangle (CheckedMacroExpr params body typ) = do
      body' <- traverse lookupVarPair body
      pure CheckedMacroExpr{
            params = params
          , body   = body'
          , typ    = typ
          }

instance Mangle C.Type where
  mangle = \case
      -- Interesting cases
      C.TypeRef declId  -> fmap C.TypeRef $
        lookupTypePair declId
      C.TypeEnum ref -> fmap C.TypeEnum $
        C.Ref <$> lookupTypePair ref.name <*> mangle ref.underlying
      C.TypeMacro ref -> fmap C.TypeMacro $
        C.Ref <$>  lookupTypePair ref.name <*> mangle ref.underlying
      C.TypeTypedef ref -> fmap C.TypeTypedef $
        C.Ref <$>  lookupTypePair ref.name <*> mangle ref.underlying

      -- Recursive cases
      C.TypePointers n typ             -> C.TypePointers n <$> mangle typ
      C.TypeFun args res               -> C.TypeFun <$> mapM mangle args <*> mangle res
      C.TypeConstArray n typ           -> C.TypeConstArray n <$> mangle typ
      C.TypeIncompleteArray typ        -> C.TypeIncompleteArray <$> mangle typ
      C.TypeBlock typ                  -> C.TypeBlock <$> mangle typ
      C.TypeQual qual typ              -> C.TypeQual qual <$> mangle typ
      C.TypeExtBinding (C.Ref ext uTy) ->
        -- The underlying type may reference the external binding itself (e.g.
        -- the typedef name that was replaced). We extend the 'NameMap' with the
        -- external binding so that such references can be resolved.
        fmap C.TypeExtBinding $ C.Ref ext <$>
          local
            ( #nameMap % #typeConstrs %~ Map.insert ext.cName ext.hsName.name )
            ( mangle uTy )

      -- The other entries do not need any name mangling
      C.TypePrim prim                  -> return $ C.TypePrim prim
      C.TypeVoid                       -> return $ C.TypeVoid
      C.TypeComplex prim               -> return $ C.TypeComplex prim

instance Mangle C.TypeFunArg where
  mangle arg = C.TypeFunArgF <$> mangle arg.typ <*> pure arg.ann

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

withDeclNamespace ::
     C.DeclKind ResolveBindingSpecs
  -> (forall ns. HasName ns => Proxy ns -> r)
  -> r
withDeclNamespace kind k =
    case kind of
      C.DeclStruct{}           -> k (Proxy @Hs.NsTypeConstr)
      C.DeclUnion{}            -> k (Proxy @Hs.NsTypeConstr)
      C.DeclTypedef{}          -> k (Proxy @Hs.NsTypeConstr)
      C.DeclEnum{}             -> k (Proxy @Hs.NsTypeConstr)
      C.DeclAnonEnumConstant{} -> k (Proxy @Hs.NsConstr)
      C.DeclOpaque{}           -> k (Proxy @Hs.NsTypeConstr)
      C.DeclFunction{}         -> k (Proxy @Hs.NsVar)
      C.DeclGlobal{}           -> k (Proxy @Hs.NsVar)

      C.DeclMacro macro ->
        case macro of
          MacroType{} -> k (Proxy @Hs.NsTypeConstr)
          MacroExpr{} -> k (Proxy @Hs.NsVar)

withDeclLoc :: forall p a.
     IsPass p
  => C.DeclInfo p -> a -> WithLocationInfo a
withDeclLoc info msg = WithLocationInfo{
      loc = idLocationInfo (Proxy @p) info.id [info.loc]
    , msg = msg
    }

invert :: Ord v => Map k v -> Map v [k]
invert = Map.foldlWithKey' aux Map.empty
  where
    aux :: Ord a => Map a [b] -> b -> a -> Map a [b]
    aux acc k v = Map.alter (insertOrPrepend k) v acc

    insertOrPrepend :: k -> Maybe [k] -> Maybe [k]
    insertOrPrepend k Nothing   = Just [k]
    insertOrPrepend k (Just ks) = Just $ k : ks

getDuplicates :: forall k v. (Ord v) => Map k v -> Map v [k]
getDuplicates = Map.filter isDup . invert
  where
    isDup :: [a] -> Bool
    isDup (_:_:_) = True
    isDup _       = False

-- The function 'tryError' is only available in `mtl` versions 2.3 and newer.
tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)
