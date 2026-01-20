module HsBindgen.Frontend.Pass.MangleNames (
    mangleNames
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bitraversable (bimapM)
import Data.Foldable qualified as Foldable
import Data.Function
import Data.Map qualified as Map
import Data.Proxy
import Data.Set qualified as Set

import Clang.HighLevel.Types

import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config.FixCandidate (FixCandidate (..))
import HsBindgen.Config.FixCandidate qualified as FixCandidate
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
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.MangleNames.Error
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Top-level

  We proceed in two passes: first we go over all def sites (declarations), and
  choose names; then in a second pass we patch up all references (in both def
  and use sites). This is necessary because although we will encounter
  declarations before they are "used", a reference through a pointer field does
  not count as a "use".
-------------------------------------------------------------------------------}

mangleNames ::
     C.TranslationUnit ResolveBindingSpecs
  -> (C.TranslationUnit MangleNames, [Msg MangleNames])
mangleNames unit = (
         C.TranslationUnit{
           decls        = catMaybes decls2
         , includeGraph = unit.includeGraph
         , ann          = updateDeclMeta td nm fs unit.ann
        }
    , msgs
    )
  where
    td :: TypedefAnalysis
    td = TypedefAnalysis.fromDecls unit.ann.declUseGraph unit.decls

    fc :: FixCandidate Maybe
    fc = FixCandidate.fixCandidateDefault

    -- Pass 1.
    nm   :: NameMap
    r1   :: Report
    (decls1, nm, r1) = chooseNames td fc unit.decls

    -- Pass 2.
    env :: Env
    env = Env{
          typedefAnalysis = td
        , fixCandidate    = fc
        , nameMap         = nm
        }

    decls2 :: [Maybe (C.Decl MangleNames)]
    r2     :: Report
    (decls2, r2) = runM env $ mapM mangleDecl decls1

    fs :: [Failure]
    fs = r1.failures ++ r2.failures

    msgs :: [Msg MangleNames]
    msgs = r1.messages ++ r2.messages

updateDeclMeta :: TypedefAnalysis -> NameMap -> [Failure] -> DeclMeta -> DeclMeta
updateDeclMeta td nm fs declMeta = declMeta{
      declIndex =
        DeclIndex.registerMangleNamesFailures failuresMap $
        DeclIndex.registerSquashedDeclarations squashedMap declMeta.declIndex
    }
  where
    failuresMap :: Map DeclId (SingleLoc, MangleNamesFailure)
    failuresMap = Map.fromList fs

    squashedMap :: Map DeclId Squashed
    squashedMap = Map.fromList $ [
        (cDeclId,) squashed
      | (cDeclId, TypedefAnalysis.Squash s) <-
           Map.toList td.map
      , let squashed :: Squashed
            squashed = Squashed {
                typedefLoc   = s.typedefLoc
              , targetNameC  = s.targetId
              , targetNameHs = Map.lookup s.targetId nm
              }
      ]

{-------------------------------------------------------------------------------
  Pass 1: Choose names

  When this fails, we construct a placeholder name; this allows us to proceed
  even if there are errors.
-------------------------------------------------------------------------------}

type NameMap = Map DeclId Hs.Identifier

chooseNames ::
     TypedefAnalysis
  -> FixCandidate Maybe
  -> [C.Decl ResolveBindingSpecs]
  -> ([C.Decl ResolveBindingSpecs], NameMap, Report)
chooseNames td fc decls =
    let specifiedNames :: NameMap
        specifiedNames = Map.fromList $ mapMaybe getSpecifiedName decls

        nameInfos :: [NameInfo]
        report    :: Report
        (nameInfos, report) =
          second mconcat . unzip $
            map (nameForDecl td fc specifiedNames) decls

        nameMap :: NameMap
        nameMap = Map.fromList $ map (\n -> (n.nameC, n.nameHs)) nameInfos

        -- When detecting collisions, we only use original (i.e., non-squashed)
        -- declarations.
        nameInfosOriginal :: [NameInfo]
        nameInfosOriginal = filter (not . (.squashed)) nameInfos

        collisions :: Map Hs.Identifier [(DeclId, SingleLoc)]
        collisions = getDuplicates $ Map.fromList $
          map (\n -> ((n.nameC, n.loc), n.nameHs)) nameInfosOriginal

        collisionFailures :: [Failure]
        collisionFailures = concatMap getCollisionFailures $ Map.toList collisions

        collidingDeclIds :: Set DeclId
        collidingDeclIds = Set.fromList $ map fst collisionFailures

        okDecls :: [C.Decl ResolveBindingSpecs]
        okDecls = filter (\x -> Set.notMember x.info.id collidingDeclIds) decls

    in  (okDecls, nameMap, report <> Report collisionFailures [])
  where
    getSpecifiedName :: C.Decl ResolveBindingSpecs -> Maybe (DeclId, Hs.Identifier)
    getSpecifiedName decl = (decl.info.id,) <$> ((.hsIdent) =<< decl.ann.cSpec)

getCollisionFailures :: (Hs.Identifier, [(DeclId, SingleLoc)]) -> [Failure]
getCollisionFailures (i, xs) =
    [ (d, (l, MangleNamesCollision i idsWithLocs)) | (d, l) <- xs]
  where
    idsWithLocs :: [WithLocationInfo DeclId]
    idsWithLocs = map (\(d, l) -> WithLocationInfo (declIdLocationInfo d [l]) d) xs

-- | Internal.
data NameInfo = NameInfo {
    nameC    :: DeclId
    -- | We need the location to obtain 'WithLocationInfo'
  , loc      :: SingleLoc
  , nameHs   :: Hs.Identifier
    -- | We expect name collisions for squashed declarations
  , squashed :: Bool
  }
  deriving stock (Eq, Show)

nameForDecl ::
     TypedefAnalysis
  -> FixCandidate Maybe
  -> NameMap
  -> C.Decl ResolveBindingSpecs
  -> (NameInfo, Report)
nameForDecl td fc specifiedNames decl =
    case Map.lookup declId specifiedNames of
      Just hsName ->
        -- Binding spec specified a name for this declaration.
        -- In this case, this overrides any naming decisions we might make here.
        --
        -- TODO: <https://github.com/well-typed/hs-bindgen/issues/1436> When
        -- squashing becomes configurable, we need to update the logic for
        -- `isSquashed` here.
        let isSquashed = case Map.lookup declId td.map of
              Just TypedefAnalysis.Squash{} -> True
              _otherwise                    -> False
        in  (NameInfo declId loc hsName isSquashed, mempty)
      Nothing ->
        withDeclNamespace decl.kind $ \ns ->
          case Map.lookup declId td.map of
            Nothing ->
              fromDeclId fc ns declId & \(hsName, fs) -> (
                  NameInfo declId loc hsName False
                , Report (toFs fs) []
                )
            Just (TypedefAnalysis.Rename (TypedefAnalysis.AddSuffix suffix)) ->
              fromDeclId fc ns declId & \(hsName, fs) ->
                let newName = hsName <> suffix in (
                  NameInfo declId loc newName False
                , Report (toFs fs) (toMs [MangleNamesRenamed newName])
                )
            Just (TypedefAnalysis.Rename (TypedefAnalysis.UseNameOf declId')) ->
              case Map.lookup declId' specifiedNames of
                Just hsName -> (NameInfo declId loc hsName False, mempty)
                Nothing ->
                  fromDeclId fc ns declId' & \(hsName, fs) -> (
                      NameInfo declId loc hsName False
                    , Report
                        (toFs fs)
                        (toMs [ MangleNamesRenamed hsName
                              | declId.name.text /= declId'.name.text
                              ])
                    )
            Just (TypedefAnalysis.Squash s) ->
              case Map.lookup s.targetId specifiedNames of
                Just hsName -> (NameInfo declId loc hsName True, mempty)
                Nothing ->
                  fromDeclId fc ns declId & \(hsName, fs) -> (
                      NameInfo declId loc hsName True
                    , Report (toFs fs) []
                    )
  where
    declId :: DeclId
    declId = decl.info.id

    loc :: SingleLoc
    loc = decl.info.loc

    toFs :: [MangleNamesFailure] -> [Failure]
    toFs = map (\f -> (decl.info.id, (loc, f)))

    toMs :: [a] -> [WithLocationInfo a]
    toMs = map (withDeclLoc decl.info)

{-------------------------------------------------------------------------------
  Internal: working with 'FixCandidate'
-------------------------------------------------------------------------------}

fixCandidate :: forall ns.
     Hs.SingNamespace ns
  => FixCandidate Maybe
  -> Proxy ns
  -> Text
  -> (Hs.Identifier, [MangleNamesFailure])
fixCandidate fc _ cName =
    case FixCandidate.fixCandidate fc cName :: Maybe (Hs.ExportedName ns) of
      Just hsName -> (Hs.Identifier hsName.text, [])
      Nothing -> (Hs.Identifier "", [MangleNamesCouldNotMangle cName])

fromDeclId :: forall ns.
     Hs.SingNamespace ns
  => FixCandidate Maybe
  -> Proxy ns
  -> DeclId
  -> (Hs.Identifier, [MangleNamesFailure])
fromDeclId fc ns declId = fixCandidate fc ns declId.name.text

{-------------------------------------------------------------------------------
  Internal: monad for pass 2, applying the namemap
-------------------------------------------------------------------------------}

newtype M a = WrapM (
      StateT Report (Reader Env) a
    )
  deriving newtype (
      Functor
    , Applicative
    , Monad
    )

type Failure = (DeclId, (SingleLoc, MangleNamesFailure))

data Report = Report{
      failures :: [Failure]
    , messages :: [Msg MangleNames]
    }
    deriving stock (Show, Generic)

instance Semigroup Report where
  l <> r = Report (l.failures ++ r.failures) (l.messages ++ r.messages)

instance Monoid Report where
  mempty = Report [] []

data Env = Env{
      typedefAnalysis :: TypedefAnalysis
    , nameMap         :: NameMap
    , fixCandidate    :: FixCandidate Maybe
    }

runM :: Env -> M a -> (a, Report)
runM env (WrapM ma) = second reverseR . flip runReader env $ runStateT ma mempty
  where
    reverseR :: Report -> Report
    reverseR x = Report (reverse x.failures) (reverse x.messages)

checkTypedefAnalysis :: DeclId -> M (Maybe TypedefAnalysis.Conclusion)
checkTypedefAnalysis declId = WrapM $ do
    td <- asks (.typedefAnalysis)
    return $ Map.lookup declId td.map

traceMsg :: Msg MangleNames -> M ()
traceMsg msg = WrapM $ modify (over #messages (msg :))

addFailure :: C.DeclInfo MangleNames -> MangleNamesFailure -> M ()
addFailure info failure =
    WrapM $ modify (over #failures (failureWithInfo :))
  where
    failureWithInfo :: Failure
    failureWithInfo = (info.id.cName, (info.loc, failure))

mangleDeclId :: DeclId -> M DeclIdPair
mangleDeclId declId = WrapM $ do
    nm <- asks (.nameMap)
    let assignedId = maybe (noAssignedIdentifier UnderlyingTypeNotMangled)
                      assignedIdentifier
                      (Map.lookup declId nm)
    pure $ DeclIdPair declId assignedId

-- | Search the 'NameMap', when we don't know the name kind
searchNameMap :: Text -> M (Maybe DeclIdPair)
searchNameMap name = WrapM $ do
     nm <- asks (.nameMap)
     return $ Foldable.asum [
         case may of
            Nothing -> Nothing
            Just _ -> Just $ DeclIdPair declId (mkAssignedId may)
       | kind <- [minBound .. maxBound]
       , let declId = DeclId{name = C.DeclName name kind, isAnon = False}
       , let may = Map.lookup declId nm
       ]
  where
    mkAssignedId may = maybe (noAssignedIdentifier UnderlyingTypeNotMangled)
                          assignedIdentifier
                          may

{-------------------------------------------------------------------------------
  Pass 2: apply NameMap
-------------------------------------------------------------------------------}

class Mangle a where
  mangle :: a ResolveBindingSpecs -> M (a MangleNames)

class MangleInDecl a where
  mangleInDecl :: C.DeclInfo MangleNames -> a ResolveBindingSpecs -> M (a MangleNames)

mangleDecl :: C.Decl ResolveBindingSpecs -> M (Maybe (C.Decl MangleNames))
mangleDecl decl = do
    mConclusion <- checkTypedefAnalysis decl.info.id
    case mConclusion of
      Just (TypedefAnalysis.Squash s) -> do
        traceMsg $ (withDeclLoc decl.info $ MangleNamesSquashed s)
        return Nothing
      _otherwise -> do
        declId'      <- mangleDeclId decl.info.id
        declComment' <- mapM mangle decl.info.comment

        let info :: C.DeclInfo MangleNames
            info = C.DeclInfo{
                 id           = declId'
               , comment      = declComment'
               , loc          = decl.info.loc
               , headerInfo   = decl.info.headerInfo
               , availability = decl.info.availability
               }

            reconstruct :: C.DeclKind MangleNames -> C.Decl MangleNames
            reconstruct declKind' = C.Decl{
                  info = info
                , kind = declKind'
                , ann  = decl.ann
                }

        Just . reconstruct <$> mangleInDecl info decl.kind

{-------------------------------------------------------------------------------
  Scoped names
-------------------------------------------------------------------------------}

-- | Apply Haskell naming rules
mkIdentifier ::
     Hs.SingNamespace ns
  => C.DeclInfo MangleNames  -- ^ Relevant decl (used only for location info)
  -> Proxy ns -> Text -> M Hs.Identifier
mkIdentifier info ns candidate = do
    fc <- WrapM $ asks (.fixCandidate)
    let (fieldHsName, mError) = fixCandidate fc ns candidate
    forM_ mError $ addFailure info
    return fieldHsName

mangleFieldName ::
     C.DeclInfo MangleNames
  -> C.ScopedName
  -> M ScopedNamePair
mangleFieldName info fieldCName =
    ScopedNamePair fieldCName <$>
      mkIdentifier info (Proxy @Hs.NsVar) candidate
  where
    candidate :: Text
    candidate = info.id.unsafeHsName.text <> "_" <> fieldCName.text

-- | Mangle enum constant name
--
-- Since these live in the global namespace, we do not prepend the name of
-- the enclosing enum.
mangleEnumConstant ::
     C.DeclInfo MangleNames
  -> C.ScopedName
  -> M ScopedNamePair
mangleEnumConstant info cName =
    ScopedNamePair cName <$>
      mkIdentifier info (Proxy @Hs.NsConstr) cName.text

-- | Mangle function argument name
--
-- Function argument names are not really used when generating Haskell code.
-- They are more relevant for documentation purposes so we don't do any
-- mangling.
mangleArgumentName ::
     C.DeclInfo MangleNames
  -> C.ScopedName
  -> M ScopedNamePair
mangleArgumentName info argName =
    ScopedNamePair argName <$>
      mkIdentifier info (Proxy @Hs.NsVar) argName.text

{-------------------------------------------------------------------------------
  Additional name mangling functionality
-------------------------------------------------------------------------------}

-- | Struct names
--
-- Right now we reuse the name of the type also for the constructor.
mkStructNames :: C.DeclInfo MangleNames -> RecordNames
mkStructNames info = RecordNames{
      constr = Hs.unsafeHsIdHsName info.id.unsafeHsName
    }

-- | Generic construction of newtype names, given only the type name
mkNewtypeNames :: C.DeclInfo MangleNames -> NewtypeNames
mkNewtypeNames info = NewtypeNames{
      constr = Hs.unsafeHsIdHsName $          info.id.unsafeHsName
    , field  = Hs.unsafeHsIdHsName $ "un_" <> info.id.unsafeHsName
    }

-- | Union names
--
-- A union is represented by a newtype around the raw bytes.
mkUnionNames :: C.DeclInfo MangleNames -> NewtypeNames
mkUnionNames = mkNewtypeNames

-- | Enum names
--
-- An enum is represented by a newtype around an integral value.
mkEnumNames :: C.DeclInfo MangleNames -> NewtypeNames
mkEnumNames = mkNewtypeNames

-- | Typedef
--
-- Typedefs are represented by newtypes
mkTypedefNames :: C.DeclInfo MangleNames -> NewtypeNames
mkTypedefNames = mkNewtypeNames

-- | Macro types
--
-- These behave like typedefs.
mkMacroTypeNames :: C.DeclInfo MangleNames -> NewtypeNames
mkMacroTypeNames = mkNewtypeNames

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance MangleInDecl C.DeclKind where
  mangleInDecl info = \case
      C.DeclStruct   x         -> C.DeclStruct           <$> mangleInDecl info x
      C.DeclUnion    x         -> C.DeclUnion            <$> mangleInDecl info x
      C.DeclTypedef  x         -> C.DeclTypedef          <$> mangleInDecl info x
      C.DeclEnum     x         -> C.DeclEnum             <$> mangleInDecl info x
      C.DeclAnonEnumConstant x -> C.DeclAnonEnumConstant <$> mangleInDecl info x
      C.DeclFunction x         -> C.DeclFunction         <$> mangleInDecl info x
      C.DeclMacro    x         -> C.DeclMacro            <$> mangleInDecl info x
      C.DeclGlobal   x         -> C.DeclGlobal           <$> mangle            x
      C.DeclOpaque             -> return C.DeclOpaque

instance MangleInDecl C.Struct where
  mangleInDecl info struct =
      reconstruct
        <$> mapM (mangleInDecl info) struct.fields
        <*> mapM (mangleInDecl info) struct.flam
    where
      reconstruct ::
           [C.StructField MangleNames]
        -> Maybe (C.StructField MangleNames)
        -> C.Struct MangleNames
      reconstruct structFields' structFlam' = C.Struct{
            fields    = structFields'
          , flam      = structFlam'
          , ann       = mkStructNames info
          , sizeof    = struct.sizeof
          , alignment = struct.alignment
          }

instance MangleInDecl C.StructField where
  mangleInDecl info field = do
      reconstruct
         <$> mangleFieldName info field.info.name
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

instance MangleInDecl C.Union where
  mangleInDecl info union = do
      reconstruct <$> mapM (mangleInDecl info) union.fields
    where
      reconstruct :: [C.UnionField MangleNames] -> C.Union MangleNames
      reconstruct unionFields' = C.Union{
            fields    = unionFields'
          , ann       = mkUnionNames info
          , sizeof    = union.sizeof
          , alignment = union.alignment
          }

instance MangleInDecl C.UnionField where
  mangleInDecl info field = do
      reconstruct
        <$> mangleFieldName info field.info.name
        <*> mangle field.typ
        <*> mapM mangle field.info.comment
    where
      reconstruct ::
           ScopedNamePair
        -> C.Type MangleNames
        -> Maybe (C.Comment MangleNames)
        -> C.UnionField MangleNames
      reconstruct unionFieldName' unionFieldType' unionFieldComment' =
        C.UnionField {
            info = C.FieldInfo {
                       loc     = field.info.loc
                     , name    = unionFieldName'
                     , comment = unionFieldComment'
                     }
          , typ  = unionFieldType'
          , ann  = field.ann
          }

instance MangleInDecl C.Enum where
  mangleInDecl info enum = do
      reconstruct
        <$> mangle enum.typ
        <*> mapM (mangleInDecl info) enum.constants
    where
      reconstruct ::
           C.Type MangleNames
        -> [C.EnumConstant MangleNames]
        -> C.Enum MangleNames
      reconstruct enumType' enumConstants' = C.Enum{
            typ       = enumType'
          , constants = enumConstants'
          , ann       = mkEnumNames info
          , sizeof    = enum.sizeof
          , alignment = enum.alignment
          }

instance MangleInDecl C.AnonEnumConstant where
  mangleInDecl info (C.AnonEnumConstant primTyp constant') = do
      reconstruct <$> mangleInDecl info constant'
    where
      reconstruct :: C.EnumConstant MangleNames -> C.AnonEnumConstant MangleNames
      reconstruct enumConstants' = C.AnonEnumConstant{
            typ      = primTyp
          , constant = enumConstants'
          }

instance MangleInDecl C.EnumConstant where
  mangleInDecl info constant = do
      reconstruct
        <$> mangleEnumConstant info constant.info.name
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
  mangle (C.CommentRef name Nothing) =
      -- NB: If this fails it means that we tried all possible name kinds and
      -- still didn't find any result. This might be because of a typo on the
      -- docs, or a missing reference.
      C.CommentRef name <$> searchNameMap name
  mangle (C.CommentRef name (Just declId)) =
      C.CommentRef name . Just <$> mangleDeclId declId

instance MangleInDecl C.Typedef where
  mangleInDecl info typedef = do
      reconstruct <$> mangle typedef.typ
    where
      reconstruct :: C.Type MangleNames -> C.Typedef MangleNames
      reconstruct typedefType' = C.Typedef{
            typ = typedefType'
          , ann = mkTypedefNames info
          }

instance MangleInDecl C.Function where
  mangleInDecl info function = do
      reconstruct
        <$> mapM
              (bimapM (mapM $ mangleArgumentName info) mangle)
              function.args
        <*> mangle function.res
    where
      reconstruct ::
           [(Maybe ScopedNamePair, C.Type MangleNames)]
        -> C.Type MangleNames
        -> C.Function MangleNames
      reconstruct functionArgs' functionRes' = C.Function{
            args  = functionArgs'
          , res   = functionRes'
          , attrs = function.attrs
          , ann   = function.ann
          }

instance MangleInDecl CheckedMacro where
  mangleInDecl info = \case
      MacroType typ  -> MacroType <$> mangleInDecl info typ
      MacroExpr expr -> return $ MacroExpr expr

instance MangleInDecl CheckedMacroType where
  mangleInDecl info macroType = do
      reconstruct <$> mangle macroType.typ
    where
      reconstruct :: C.Type MangleNames -> CheckedMacroType MangleNames
      reconstruct typ' = CheckedMacroType{
            typ = typ'
          , ann = mkMacroTypeNames info
          }

instance Mangle C.Type where
  mangle = \case
      -- Interesting cases
      C.TypeRef declId  ->
        fmap C.TypeRef $
        mangleDeclId declId
      C.TypeEnum ref ->
        fmap C.TypeEnum $
        C.Ref <$> mangleDeclId ref.name <*> mangle ref.underlying
      C.TypeMacro ref ->
        fmap C.TypeMacro $
        C.Ref <$>  mangleDeclId ref.name <*> mangle ref.underlying
      C.TypeTypedef ref ->
        fmap C.TypeTypedef $
        C.Ref <$>  mangleDeclId ref.name <*> mangle ref.underlying

      -- Recursive cases
      C.TypePointers n typ             -> C.TypePointers n <$> mangle typ
      C.TypeFun args res               -> C.TypeFun <$> mapM mangle args <*> mangle res
      C.TypeConstArray n typ           -> C.TypeConstArray n <$> mangle typ
      C.TypeIncompleteArray typ        -> C.TypeIncompleteArray <$> mangle typ
      C.TypeBlock typ                  -> C.TypeBlock <$> mangle typ
      C.TypeQual qual typ              -> C.TypeQual qual <$> mangle typ
      C.TypeExtBinding (C.Ref ext uTy) -> fmap C.TypeExtBinding $
        C.Ref ext <$> mangle uTy

      -- The other entries do not need any name mangling
      C.TypePrim prim                  -> return $ C.TypePrim prim
      C.TypeVoid                       -> return $ C.TypeVoid
      C.TypeComplex prim               -> return $ C.TypeComplex prim

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

withDeclNamespace ::
     C.DeclKind ResolveBindingSpecs
  -> (forall ns. Hs.SingNamespace ns => Proxy ns -> r)
  -> r
withDeclNamespace kind k =
    case kind of
      C.DeclStruct{}           -> k (Proxy @Hs.NsTypeConstr)
      C.DeclUnion{}            -> k (Proxy @Hs.NsTypeConstr)
      C.DeclTypedef{}          -> k (Proxy @Hs.NsTypeConstr)
      C.DeclEnum{}             -> k (Proxy @Hs.NsTypeConstr)
      C.DeclAnonEnumConstant{} -> k (Proxy @Hs.NsTypeConstr)
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
