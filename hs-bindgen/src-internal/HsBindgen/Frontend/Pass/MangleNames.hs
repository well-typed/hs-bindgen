module HsBindgen.Frontend.Pass.MangleNames (
    mangleNames
  ) where

import Control.Applicative (asum)
import Control.Monad.Except (ExceptT, MonadError (..), liftEither, runExcept,
                             runExceptT)
import Control.Monad.Reader (Reader, ask, asks, local, runReader)
import Data.Either (partitionEithers)
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.Proxy
import Data.Set qualified as Set

import Clang.HighLevel.Types

import HsBindgen.BindingSpec
import HsBindgen.Config.MangleCandidate (MangleCandidate (..))
import HsBindgen.Config.MangleCandidate qualified as MangleCandidate
import HsBindgen.Config.Prelims (FieldNamingStrategy (..))
import HsBindgen.Frontend.Analysis.DeclIndex (Squashed (..))
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.Typedefs (TypedefAnalysis)
import HsBindgen.Frontend.Analysis.Typedefs qualified as TypedefAnalysis
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.Pass.MangleNames.Error
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.MangleNames.Names
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.IR.Translation
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Macro.Flip
import HsBindgen.Macro.Type qualified as Macro
import HsBindgen.Util.Tracer (WithCallStack, withCallStack)

import Doxygen.Parser.Types qualified as Doxy

{-------------------------------------------------------------------------------
  Top-level

  Name mangling proceeds in three traversals:

  1. 'createNames' chooses top-level names and mints all within-declaration
     names (annotations and 'ScopedNamePair' slots), producing a 'CreateNames'
     AST whose 'Id' references are still unresolved. It also produces the
     'NameMap' (resolution index) and the squash records.

  2. 'detectClashes' assembles a single 'NameRegistry' from the 'NameMap' (for
     top-level names) and the 'CreateNames' AST (for derived names) and reports
     all collisions in one sweep.

  3. 'resolveNames' rewrites every 'C.DeclId' reference into a 'DeclIdPair' using
     the 'NameMap', copying annotations and scoped names across unchanged, and
     dropping the declarations flagged by 'detectClashes'. This produces the
     final 'MangleNames' AST.
-------------------------------------------------------------------------------}

mangleNames ::
     forall l. (HasCallStack, Macro.HasTypes l)
  => FieldNamingStrategy
  -> C.TranslationUnit l ResolveBindingSpecs
  -> (C.TranslationUnit l MangleNames, [AnnMsg MangleNames])
mangleNames fieldNaming unit = (
         C.TranslationUnit{
           decls        = decls3
         , includeGraph = unit.includeGraph
         , meta         = updateDeclMeta
                            nameMap
                            (failures1 ++ failures2 ++ failures3)
                            squashes
                            unit.meta
        }
    , msgs1 ++ [
          debugMsg $ MangleNamesNameMap nameMap
        , debugMsg $ MangleNamesNameRegistry registry
        ]
    )
  where
    typedefAnalysis :: TypedefAnalysis
    typedefAnalysis = TypedefAnalysis.fromDecls unit.meta.declUseGraph unit.decls

    mangleCandidateConfig :: MangleCandidate Maybe
    mangleCandidateConfig = MangleCandidate.mangleCandidateDefault

    -- Traversal 1: choose top-level names and mint within-declaration names.
    declsC    :: [C.Decl l CreateNames]
    nameMap   :: NameMap
    squashes  :: [MangleNamesSquash]
    failures1 :: [MangleNamesFailure]
    msgs1     :: [AnnMsg MangleNames]
    (declsC, nameMap, squashes, failures1, msgs1) =
      createNames typedefAnalysis mangleCandidateConfig fieldNaming unit.decls

    -- Traversal 2: detect clashes among all minted names (single sweep).
    registry  :: NameRegistry
    failures2 :: [MangleNamesFailure]
    (registry, failures2) = detectClashes fieldNaming nameMap declsC

    dropped :: Set C.DeclId
    dropped = Set.fromList $ map (.id) failures2

    -- Traversal 3: resolve all references.
    decls3    :: [C.Decl l MangleNames]
    failures3 :: [MangleNamesFailure]
    (decls3, failures3) = resolveNames nameMap dropped declsC

updateDeclMeta ::
      NameMap
   -> [MangleNamesFailure]
   -> [MangleNamesSquash]
   -> DeclMeta l
   -> DeclMeta l
updateDeclMeta nameMap failures squashes declMeta = declMeta{
      declIndex =
        DeclIndex.registerMangleNamesFailure failuresMap $
        DeclIndex.registerSquashedDeclarations squashesMap $
          declMeta.declIndex
    }
  where
    failuresMap :: Map C.DeclId (SingleLoc, MangleNamesError)
    failuresMap = Map.fromList $ map (\f -> (f.id, (f.loc, f.err))) failures

    squashesMap ::  Map C.DeclId Squashed
    squashesMap = Map.fromList $ map (\s -> (s.id, toSquashed s.squash)) squashes

    toSquashed :: TypedefAnalysis.Squash -> Squashed
    toSquashed s = Squashed {
        typedefLoc   = s.typedefLoc
      , targetNameC  = s.targetId
      , targetNameHs = lookupType s.targetId nameMap
      }

{-------------------------------------------------------------------------------
  Failures and squashes
-------------------------------------------------------------------------------}

data MangleNamesFailure = MangleNamesFailure {
      id  :: C.DeclId
    , loc :: SingleLoc
    , err :: MangleNamesError
    }
  deriving (Show, Eq, Ord, Generic)

toFailure :: (Id p ~ C.DeclId) => C.DeclInfo p -> MangleNamesError -> MangleNamesFailure
toFailure i e = MangleNamesFailure i.id i.loc e

data MangleNamesSquash = MangleNamesSquash {
      id     :: C.DeclId
    , squash :: TypedefAnalysis.Squash
}
  deriving (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Internal: working with 'MangleCandidate'
-------------------------------------------------------------------------------}

type E m a = ExceptT MangleNamesError m a

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
  Traversal 1: Create names

  1a chooses top-level names (and builds the 'NameMap'); 1b mints all
  within-declaration names, building the 'CreateNames' AST.
-------------------------------------------------------------------------------}

createNames ::
     forall l. (HasCallStack, Macro.HasTypes l)
  => TypedefAnalysis
  -> MangleCandidate Maybe
  -> FieldNamingStrategy
  -> [C.Decl l ResolveBindingSpecs]
  -> ( [C.Decl l CreateNames]
     , NameMap
     , [MangleNamesSquash]
     , [MangleNamesFailure]
     , [AnnMsg MangleNames]
     )
createNames td mc strategy decls = (
      createdDecls
    , nameMap
    , squashes
    , nameFailures ++ createFailures
    , messages
    )
  where
    specifiedNames :: Map C.DeclId (Hs.Name Hs.NsTypeConstr)
    specifiedNames = Map.fromList $ mapMaybe getSpecifiedName decls

    getSpecifiedName ::
      C.Decl l ResolveBindingSpecs -> Maybe (C.DeclId, Hs.Name Hs.NsTypeConstr)
    getSpecifiedName decl = (decl.info.id,) <$> ((.hsName) =<< decl.ann.cSpec)

    -- 1a: choose top-level names.
    nameResults :: [(Either MangleNamesFailure NameInfo, [AnnMsg MangleNames])]
    nameResults = map (nameForDecl td mc specifiedNames) decls

    nameFailures :: [MangleNamesFailure]
    nameInfos    :: [NameInfo]
    (nameFailures, nameInfos) = partitionEithers (map fst nameResults)

    messages :: [AnnMsg MangleNames]
    messages = concatMap snd nameResults

    nameMap :: NameMap
    nameMap = fromDeclIdPairs $
      map (\n -> DeclIdPair n.cName n.hsName) nameInfos

    -- The chosen top-level Haskell name (as text), per successfully-named
    -- declaration. Used to derive within-declaration names in 1b.
    nameTexts :: Map C.DeclId Text
    nameTexts = Map.fromList $ map (\n -> (n.cName, n.hsName.text)) nameInfos

    failedNamingIds :: Set C.DeclId
    failedNamingIds = Set.fromList $ map (.id) nameFailures

    -- 1b: mint within-declaration names.
    env :: CreateEnv
    env = CreateEnv{
        mangleCandidate     = mc
      , fieldNamingStrategy = strategy
      }

    createResults :: [CreateDeclResult l]
    createResults =
      mapMaybe (createDecl env td nameTexts failedNamingIds) decls

    createFailures :: [MangleNamesFailure]
    squashes       :: [MangleNamesSquash]
    createdDecls   :: [C.Decl l CreateNames]
    (createFailures, squashes, createdDecls) =
      partitionCreateResults createResults

-- | Internal.
data NameInfo = NameInfo {
    cName    :: C.DeclId
  , hsName   :: Hs.SomeName
    -- | We need the location to obtain 'C.WithLocationInfo'
  , loc      :: SingleLoc
  }
  deriving stock (Eq, Show)

nameForDecl ::
     HasCallStack
  => TypedefAnalysis
  -> MangleCandidate Maybe
  -> Map C.DeclId (Hs.Name Hs.NsTypeConstr)
  -> C.Decl l ResolveBindingSpecs
  -> (Either MangleNamesFailure NameInfo, [AnnMsg MangleNames])
nameForDecl td mc specifiedNames decl =
    second toMs $
    withDeclNamespace decl.kind $ \(nsProxy :: Proxy ns) ->
      let mangleCandidate' :: Text -> Either MangleNamesError (Hs.Name ns)
          mangleCandidate' d = runExcept $ mangleCandidate mc nsProxy d
      in case Map.lookup declId specifiedNames of
        Just hsNm ->
          -- Binding spec specified a name for this declaration. In this case,
          -- this overrides any naming decisions we might make here.
          --
          -- TODO <https://github.com/well-typed/hs-bindgen/issues/1436>
          -- When squashing becomes configurable, revisit how a specified name
          -- interacts with squashing here.
          ( Right $ NameInfo declId (Hs.demoteNs hsNm) loc
          , []
          )
        Nothing ->
          case Map.lookup declId td.map of
            Nothing ->
              mangleCandidate' declId.name.text & \case
                Left err -> failWith err
                Right hsNm ->
                  ( Right $ NameInfo declId (Hs.demoteNs hsNm) loc
                  , [] )
            Just (TypedefAnalysis.AddSuffix suffix) ->
              mangleCandidate' (declId.name.text <> suffix) & \case
                Left err -> failWith err
                Right hsNm ->
                  ( Right $ NameInfo declId (Hs.demoteNs hsNm) loc
                  , [MangleNamesAssignedName (Hs.demoteNs hsNm)] )
            Just (TypedefAnalysis.UseNameOf declId') ->
              case Map.lookup declId' specifiedNames of
                Just hsNm ->
                  ( Right $ NameInfo declId (Hs.demoteNs hsNm) loc
                  , [ MangleNamesReusedAssignedName (Hs.demoteNs hsNm) ] )
                Nothing ->
                  mangleCandidate' declId'.name.text & \case
                    Left err -> failWith err
                    Right hsNm ->
                      ( Right $ NameInfo declId (Hs.demoteNs hsNm) loc
                      , [ MangleNamesAssignedName (Hs.demoteNs hsNm)
                        | declId.name.text /= declId'.name.text ] )
            Just (TypedefAnalysis.Squash s) ->
              case Map.lookup s.targetId specifiedNames of
                Just hsNm ->
                  ( Right $ NameInfo declId (Hs.demoteNs hsNm) loc
                  , [] )
                Nothing ->
                  mangleCandidate' declId.name.text & \case
                    Left err -> failWith err
                    Right hsNm ->
                      ( Right $ NameInfo declId (Hs.demoteNs hsNm) loc
                      , [] )
  where
    declId :: C.DeclId
    declId = decl.info.id

    info :: C.DeclInfo ResolveBindingSpecs
    info = decl.info

    loc :: SingleLoc
    loc = decl.info.loc

    failWith :: MangleNamesError -> (Either MangleNamesFailure a, [MangleNamesMsg])
    failWith err = (Left $ toFailure info err, [])

    toMs :: HasCallStack => [a] -> [WithCallStack (C.WithLocationInfo a)]
    toMs = map (withCallStack . withDeclLoc decl.info)

{-------------------------------------------------------------------------------
  Traversal 1b: minting within-declaration names

  All names minted here are simply created (via 'mkName'); they are /not/
  recorded for collision detection, which is the job of 'detectClashes'.
-------------------------------------------------------------------------------}

data CreateEnv = CreateEnv{
      mangleCandidate     :: MangleCandidate Maybe
    , fieldNamingStrategy :: FieldNamingStrategy
    }
  deriving stock (Generic)

type CreateM   = Reader CreateEnv
type CreateE a = ExceptT MangleNamesError CreateM a

runCreate :: CreateEnv -> CreateE a -> Either MangleNamesError a
runCreate env action = runReader (runExceptT action) env

-- | Apply Haskell naming rules to a candidate name
mkName ::
     Hs.SingNamespace ns
  => Proxy ns -> Text -> CreateE (Hs.Name ns)
mkName ns candidate = do
    mc <- asks (.mangleCandidate)
    liftEither . runExcept $ mangleCandidate mc ns candidate

data CreateDeclResult l =
      CdrFailure  MangleNamesFailure
    | CdrSquashed MangleNamesSquash
    | CdrCreated (C.Decl l CreateNames)

partitionCreateResults ::
     [CreateDeclResult l]
  -> ( [MangleNamesFailure]
     , [MangleNamesSquash]
     , [C.Decl l CreateNames] )
partitionCreateResults = rev . Foldable.foldl' aux ([], [], [])
  where
    aux (fs, ss, ds) = \case
      CdrFailure  f -> (f:fs, ss,   ds)
      CdrSquashed s -> (fs,   s:ss, ds)
      CdrCreated  d -> (fs,   ss,   d:ds)

    rev (fs, ss, ds) = (reverse fs, reverse ss, reverse ds)

createDecl ::
     Macro.HasTypes l
  => CreateEnv
  -> TypedefAnalysis
  -> Map C.DeclId Text
  -> Set C.DeclId
  -> C.Decl l ResolveBindingSpecs
  -> Maybe (CreateDeclResult l)
createDecl env td nameTexts failedNamingIds decl
    -- Squashed declarations are not emitted; we issue delayed squashed messages
    -- for selected declarations in the `Select` pass.
    | Just (TypedefAnalysis.Squash s) <- Map.lookup declId td.map
    = Just $ CdrSquashed $ MangleNamesSquash declId s
    -- The declaration's top-level name could not be mangled in 1a; the failure
    -- is already recorded, so we drop it here without contributing anything.
    | Set.member declId failedNamingIds
    = Nothing
    | otherwise
    = case Map.lookup declId nameTexts of
        Nothing     -> Nothing
        Just hsName -> Just $
          case runCreate env (createDeclKind hsName decl.kind) of
            Left err   -> CdrFailure $ toFailure decl.info err
            Right kind -> CdrCreated C.Decl{
                info = coercePass decl.info
              , kind = kind
              , ann  = decl.ann
              }
  where
    declId :: C.DeclId
    declId = decl.info.id

createDeclKind ::
     Macro.HasTypes l
  => Text
  -> C.DeclKind l ResolveBindingSpecs
  -> CreateE (C.DeclKind l CreateNames)
createDeclKind hsName = \case
    C.DeclStruct           x -> C.DeclStruct           <$> createStruct hsName x
    C.DeclUnion            x -> C.DeclUnion            <$> createUnion hsName x
    C.DeclEnum             x -> C.DeclEnum             <$> createEnum hsName x
    C.DeclAnonEnumConstant x -> C.DeclAnonEnumConstant <$> createAnonEnumConstant x
    C.DeclTypedef          x -> C.DeclTypedef          <$> createTypedef hsName x
    C.DeclFunction         x -> C.DeclFunction         <$> createFunction x
    C.DeclMacro            x -> C.DeclMacro            <$> createMacro hsName x
    C.DeclGlobal           x -> C.DeclGlobal           <$> createGlobal x
    C.DeclOpaque mSize       -> pure (C.DeclOpaque mSize)

{-------------------------------------------------------------------------------
  Traversal 1b: name-bundle constructors
-------------------------------------------------------------------------------}

createStructNames :: Text -> CreateE StructNames
createStructNames name = do
    constr <- mkName (Proxy @Hs.NsConstr) name
    pure StructNames{
        constr = constr
      }

-- | Mint names for the flexible array member (FLAM), if present
--
-- The auxiliary type-constructor name is minted exactly when there is a FLAM
-- and carried inside the 'C.Flam' constructor alongside the field, so that name
-- creation cannot diverge from code generation
-- (<https://github.com/well-typed/hs-bindgen/issues/1925>).
createFlam :: Text -> C.Flam ResolveBindingSpecs -> CreateE (C.Flam CreateNames)
createFlam _      C.NoFlam        = pure C.NoFlam
createFlam hsName (C.Flam field _) = do
    field'  <- createStructField hsName field
    auxName <- mkName (Proxy @Hs.NsTypeConstr) (hsName <> "_Aux")
    pure $ C.Flam field' FlamNames{ aux = auxName }

-- | Generic construction of newtype names, given only the type name
createNewtypeNames :: FieldNamingStrategy -> Text -> CreateE NewtypeNames
createNewtypeNames strategy name = do
    dataConstr <- mkName (Proxy @Hs.NsConstr) name
    field      <- mkName (Proxy @Hs.NsVar) $ case strategy of
                    AddFieldPrefixes  -> "unwrap" <> name
                    OmitFieldPrefixes -> "unwrap"
    pure NewtypeNames{
        dataConstr = dataConstr
      , field      = field
      }

createTypedefNames ::
  Maybe a -> FieldNamingStrategy -> Text -> CreateE TypedefNames
createTypedefNames mFunPtr strategy name = do
    orig <- createNewtypeNames strategy name
    aux  <- case mFunPtr of
      Nothing -> pure Nothing
      Just _  -> do
        auxName  <- mkName (Proxy @Hs.NsTypeConstr) $ name <> "_Aux"
        auxNames <- createNewtypeNames strategy auxName.text
        pure $ Just (auxName, auxNames)
    pure TypedefNames{
        orig = orig
      , aux  = aux
      }

{-------------------------------------------------------------------------------
  Traversal 1b: scoped names
-------------------------------------------------------------------------------}

createFieldName :: Text -> C.ScopedName -> CreateE ScopedNamePair
createFieldName hsName fieldCName = do
    strategy <- asks (.fieldNamingStrategy)
    let candidate :: Text
        candidate = case strategy of
          AddFieldPrefixes  -> hsName <> "_" <> fieldCName.text
          OmitFieldPrefixes -> fieldCName.text
    name <- mkName (Proxy @Hs.NsVar) candidate
    pure ScopedNamePair{
        cName  = fieldCName
      , hsName = Hs.demoteNs name
      }

-- | Mangle an enum constant name
--
-- Since these live in the global namespace, we do not prepend the name of the
-- enclosing enum.
createEnumConstantName :: C.ScopedName -> CreateE ScopedNamePair
createEnumConstantName cName = do
    name <- mkName (Proxy @Hs.NsConstr) cName.text
    pure ScopedNamePair{
        cName  = cName
      , hsName = Hs.demoteNs name
      }

-- | Mangle function argument name
--
-- Function argument names are not really used when generating Haskell code.
-- They are more relevant for documentation purposes so we don't do any
-- mangling.
createArgumentName :: C.ScopedName -> CreateE ScopedNamePair
createArgumentName argName = do
    name <- mkName (Proxy @Hs.NsVar) argName.text
    pure ScopedNamePair{
        cName  = argName
      , hsName = Hs.demoteNs name
      }

{-------------------------------------------------------------------------------
  Traversal 1b: per-construct creators
-------------------------------------------------------------------------------}

createStruct ::
     Text -> C.Struct ResolveBindingSpecs -> CreateE (C.Struct CreateNames)
createStruct hsName struct = do
    fields <- mapM (createStructField hsName) struct.fields
    flam   <- createFlam hsName struct.flam
    names  <- createStructNames hsName
    pure C.Struct{
        fields    = fields
      , flam      = flam
      , ann       = names
      , sizeof    = struct.sizeof
      , alignment = struct.alignment
      }

createStructField ::
     Text
  -> C.StructField ResolveBindingSpecs
  -> CreateE (C.StructField CreateNames)
createStructField hsName field = do
    name' <- createFieldName hsName field.info.name
    pure C.StructField{
        info   = C.FieldInfo{
                     loc     = field.info.loc
                   , name    = name'
                   , comment = fmap coercePass field.info.comment
                   }
      , typ    = coercePass field.typ
      , offset = field.offset
      , width  = field.width
      , ann    = field.ann
      }

createUnion ::
     Text -> C.Union ResolveBindingSpecs -> CreateE (C.Union CreateNames)
createUnion hsName union = do
    strategy <- asks (.fieldNamingStrategy)
    names    <- createNewtypeNames strategy hsName
    fields   <- mapM (createUnionField hsName) union.fields
    pure C.Union{
        fields    = fields
      , ann       = names
      , sizeof    = union.sizeof
      , alignment = union.alignment
      }

createUnionField ::
     Text
  -> C.UnionField ResolveBindingSpecs
  -> CreateE (C.UnionField CreateNames)
createUnionField hsName field = do
    name'    <- createFieldName hsName field.info.name
    pure C.UnionField{
        info = C.FieldInfo{
                   loc     = field.info.loc
                 , name    = name'
                 , comment = fmap coercePass field.info.comment
                 }
      , typ  = coercePass field.typ
      , ann  = NoAnn
      }

createEnum :: Text -> C.Enum ResolveBindingSpecs -> CreateE (C.Enum CreateNames)
createEnum hsName enum = do
    strategy  <- asks (.fieldNamingStrategy)
    names     <- createNewtypeNames strategy hsName
    constants <- mapM createEnumConstant enum.constants
    pure C.Enum{
        typ       = coercePass enum.typ
      , constants = constants
      , ann       = names
      , sizeof    = enum.sizeof
      , alignment = enum.alignment
      }

createEnumConstant ::
     C.EnumConstant ResolveBindingSpecs
  -> CreateE (C.EnumConstant CreateNames)
createEnumConstant constant = do
    name' <- createEnumConstantName constant.info.name
    pure C.EnumConstant{
        info  = C.FieldInfo{
                    loc     = constant.info.loc
                  , name    = name'
                  , comment = fmap coercePass constant.info.comment
                  }
      , value = constant.value
      }

createAnonEnumConstant ::
     C.AnonEnumConstant ResolveBindingSpecs
  -> CreateE (C.AnonEnumConstant CreateNames)
createAnonEnumConstant (C.AnonEnumConstant primTyp constant) = do
    constant' <- createEnumConstant constant
    pure C.AnonEnumConstant{
        typ      = primTyp
      , constant = constant'
      }

createTypedef ::
     Text -> C.Typedef ResolveBindingSpecs -> CreateE (C.Typedef CreateNames)
createTypedef hsName typedef = do
    strategy <- asks (.fieldNamingStrategy)
    names    <- createTypedefNames mFunPtr strategy hsName
    pure C.Typedef{
        typ = coercePass typedef.typ
      , ann = names
      }
  where
    -- TODO https://github.com/well-typed/hs-bindgen/issues/1925
    --
    -- Tie generation of names to the generation of the associated code. This
    -- is especially ugly.
    mFunPtr :: Maybe (C.Typedef ResolveBindingSpecs)
    mFunPtr = typedef <$ C.getFirstFunTypeIndirection typedef.typ

createFunction ::
     C.Function ResolveBindingSpecs -> CreateE (C.Function CreateNames)
createFunction function = do
    args <- mapM createFunctionArg function.args
    pure C.Function{
        args  = args
      , res   = coercePass function.res
      , attrs = function.attrs
      , ann   = function.ann
      }
  where
    createFunctionArg ::
         C.FunctionArg ResolveBindingSpecs
      -> CreateE (C.FunctionArg CreateNames)
    createFunctionArg arg = do
      name' <- traverse createArgumentName arg.name
      pure C.FunctionArg{
          name   = name'
        , argTyp = coercePass arg.argTyp
        }

createGlobal ::
     C.Global ResolveBindingSpecs -> CreateE (C.Global CreateNames)
createGlobal global = pure C.Global{
      typ = coercePass global.typ
    , ann = global.ann
    }

createMacro ::
     Macro.HasTypes l
  => Text
  -> TypecheckedMacro ResolveBindingSpecs l
  -> CreateE (TypecheckedMacro CreateNames l)
createMacro hsName = \case
    MacroType  typ -> MacroType <$> createMacroType hsName typ
    MacroValue val -> pure $ MacroValue (coercePass val)

createMacroType ::
     Macro.HasTypes l
  => Text
  -> TypecheckedMacroType l ResolveBindingSpecs
  -> CreateE (TypecheckedMacroType l CreateNames)
createMacroType hsName macroType = do
    strategy <- asks (.fieldNamingStrategy)
    names    <- createNewtypeNames strategy hsName
    pure TypecheckedMacroType{
        body = fmap coercePass macroType.body
      , ann  = names
      }

{-------------------------------------------------------------------------------
  Traversal 2: Detect clashes

  Build a single 'NameRegistry' from the 'NameMap' (top-level names) and the
  'CreateNames' AST (derived names), then report all collisions in one sweep.
  Over-removal is explicitly accepted: a single declaration may participate in
  several collisions, and every participating owner is dropped.
-------------------------------------------------------------------------------}

detectClashes ::
     forall l.
     FieldNamingStrategy
  -> NameMap
  -> [C.Decl l CreateNames]
  -> (NameRegistry, [MangleNamesFailure])
detectClashes strategy nameMap decls =
    (registry, collisionFailures (collisions registry))
  where
    registry :: NameRegistry
    registry = Foldable.foldl' registerDecl emptyNameRegistry decls

    registerDecl :: NameRegistry -> C.Decl l CreateNames -> NameRegistry
    registerDecl reg decl =
        Foldable.foldl'
          (\r (role, scope, loc', sname) -> registerSomeName role scope owner loc' sname r)
          reg0
          (derivedNames strategy owner loc decl.kind)
      where
        owner :: C.DeclId
        owner = decl.info.id

        loc :: SingleLoc
        loc = decl.info.loc

        -- The declaration's own top-level name. Squashed declarations do not
        -- appear in 'decls', so they are never registered here.
        reg0 :: NameRegistry
        reg0 = case lookupSomeName owner nameMap of
          Just sname -> registerSomeName Declaration GlobalScope owner loc sname reg
          Nothing    -> reg

-- | All derived (non-top-level) names of a declaration, with the role, scope,
-- and location under which they must be unique.
--
-- Most derived names are blamed on (and located at) the enclosing declaration.
-- Record field selectors are the exception: they carry the field's /own/
-- location, so that a within-record duplicate-field collision points at the
-- offending fields.
derivedNames :: forall l.
     FieldNamingStrategy
  -> C.DeclId
  -> SingleLoc
  -> C.DeclKind l CreateNames
  -> [(NameRole, Scope, SingleLoc, Hs.SomeName)]
derivedNames strategy owner loc = \case
    C.DeclStruct struct ->
         (Constructor, GlobalScope, loc, Hs.demoteNs struct.ann.constr)
       : [ (AuxType, GlobalScope, loc, Hs.demoteNs flamNames.aux)
         | C.Flam _ flamNames <- [struct.flam] ]
      ++ concatMap structFieldNames
           (struct.fields ++ maybeToList (C.flamStructField struct.flam))
    C.DeclUnion union ->
         newtypeNames union.ann
      ++ concatMap unionFieldNames union.fields
    C.DeclEnum enum ->
         newtypeNames enum.ann
      ++ map enumConstantName enum.constants
    -- An anonymous enum constant is a top-level declaration in its own right;
    -- its name is registered as a 'Declaration'. We must /not/ also register
    -- the inner enum-constant scoped name, or it would collide with itself.
    C.DeclAnonEnumConstant{} -> []
    C.DeclTypedef typedef    -> typedefNames typedef.ann
    C.DeclMacro (MacroType macroType) -> newtypeNames macroType.ann
    C.DeclMacro MacroValue{} -> []
    C.DeclFunction{}         -> []
    C.DeclGlobal{}           -> []
    C.DeclOpaque{}           -> []
  where
    -- Field selectors minted under 'OmitFieldPrefixes' enable
    -- @DuplicateRecordFields@, so they need only be unique within a record.
    fieldScope :: Scope
    fieldScope = case strategy of
      AddFieldPrefixes  -> GlobalScope
      OmitFieldPrefixes -> DeclScope owner

    structFieldNames :: C.StructField CreateNames -> [(NameRole, Scope, SingleLoc, Hs.SomeName)]
    structFieldNames field = [ (Field, fieldScope, field.info.loc, field.info.name.hsName) ]

    unionFieldNames :: C.UnionField CreateNames -> [(NameRole, Scope, SingleLoc, Hs.SomeName)]
    unionFieldNames field =
        [ (Field, fieldScope, field.info.loc, field.info.name.hsName) ]

    enumConstantName :: C.EnumConstant CreateNames -> (NameRole, Scope, SingleLoc, Hs.SomeName)
    enumConstantName constant = (Constructor, GlobalScope, loc, constant.info.name.hsName)

    -- The newtype "unwrap" field is recorded only under 'AddFieldPrefixes',
    -- where it is the globally-unique @unwrap<TypeName>@. Under
    -- 'OmitFieldPrefixes' it is simply @unwrap@, lives alone in its own record,
    -- and so can never collide.
    newtypeNames :: NewtypeNames -> [(NameRole, Scope, SingleLoc, Hs.SomeName)]
    newtypeNames n =
        (Constructor, GlobalScope, loc, Hs.demoteNs n.dataConstr)
      : [ (Field, GlobalScope, loc, Hs.demoteNs n.field)
        | AddFieldPrefixes <- [strategy] ]

    typedefNames :: TypedefNames -> [(NameRole, Scope, SingleLoc, Hs.SomeName)]
    typedefNames t =
         newtypeNames t.orig
      ++ concat [ (AuxType, GlobalScope, loc, Hs.demoteNs auxName) : newtypeNames auxNames
                | (auxName, auxNames) <- maybeToList t.aux ]

-- | Turn registry collisions into per-declaration failures
--
-- Each 'Collision' produced by 'collisions' lives in a single 'Scope'. A
-- collision within a 'DeclScope' is a set of record field selectors of one
-- declaration that mangle to the same Haskell name (only possible under
-- 'OmitFieldPrefixes'); we report it as a more specific
-- 'MangleNamesDuplicateFieldName'. All other collisions are reported as
-- 'MangleNamesCollision', one failure per participating declaration.
collisionFailures :: [Collision] -> [MangleNamesFailure]
collisionFailures = concatMap getCollisionFailures
  where
    getCollisionFailures :: Collision -> [MangleNamesFailure]
    getCollisionFailures c = case c.origins of
        o:_ | DeclScope{} <- o.scope ->
          -- Within-record duplicate field selector. All origins share the same
          -- owner (the enclosing record), so this yields a single failure.
          [ MangleNamesFailure o.owner o.loc $
              MangleNamesDuplicateFieldName c.name (map (.loc) c.origins)
          ]
        _otherwise ->
          [ MangleNamesFailure o.owner o.loc (MangleNamesCollision c.name idsWithLocs)
          | o <- origins
          ]
      where
        -- One failure per distinct declaration.
        origins :: [NameOrigin]
        origins = Map.elems $ Map.fromList $ map (\o -> (o.owner, o)) c.origins

        idsWithLocs :: [C.WithLocationInfo C.DeclId]
        idsWithLocs =
          [ C.WithLocationInfo (C.declIdLocationInfo o.owner [o.loc]) o.owner
          | o <- origins
          ]

{-------------------------------------------------------------------------------
  Traversal 3: Resolve names

  Rewrite every 'C.DeclId' reference into a 'DeclIdPair' using the 'NameMap',
  copying annotations and scoped names across unchanged. Declarations flagged by
  'detectClashes' are dropped (their failure is already recorded); a reference
  to an unmangleable declaration drops the referring declaration with a
  'MangleNamesUnderlyingDeclNotMangled' failure.
-------------------------------------------------------------------------------}

type ResolveM   = Reader NameMap
type ResolveE a = ExceptT MangleNamesError ResolveM a

resolveNames ::
     forall l. Macro.HasTypes l
  => NameMap
  -> Set C.DeclId
  -> [C.Decl l CreateNames]
  -> ([C.Decl l MangleNames], [MangleNamesFailure])
resolveNames nameMap dropped decls = (oks, failures)
  where
    failures :: [MangleNamesFailure]
    oks      :: [C.Decl l MangleNames]
    (failures, oks) = partitionEithers $ mapMaybe resolveOne decls

    resolveOne ::
         C.Decl l CreateNames
      -> Maybe (Either MangleNamesFailure (C.Decl l MangleNames))
    resolveOne decl
      | Set.member decl.info.id dropped = Nothing
      | otherwise = Just $
          case runReader (runExceptT (resolveDecl decl)) nameMap of
            Left err    -> Left  $ toFailure decl.info err
            Right decl' -> Right decl'

resolveDecl ::
     Macro.HasTypes l
  => C.Decl l CreateNames
  -> ResolveE (C.Decl l MangleNames)
resolveDecl decl =
    withDeclNamespace decl.kind $ \nsProxy -> do
      info' <- resolveDeclInfo nsProxy decl.info
      kind' <- resolveDeclKind decl.kind
      pure C.Decl{
          info = info'
        , kind = kind'
        , ann  = decl.ann
        }

resolveDeclInfo ::
     forall ns. Hs.SingNamespace ns
  => Proxy ns
  -> C.DeclInfo CreateNames
  -> ResolveE (C.DeclInfo MangleNames)
resolveDeclInfo nsProxy info = do
    hsName     <- resolveDeclName nsProxy info.id
    comment'   <- traverse resolve info.comment
    enclosing' <- mapM resolveEnclosingRef info.enclosing
    pure C.DeclInfo{
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

resolveDeclName ::
     forall ns. Hs.SingNamespace ns
  => Proxy ns -> C.DeclId -> ResolveE (Hs.Name ns)
resolveDeclName _ declId = case Hs.singNamespace @ns of
    Hs.SNsTypeConstr -> lookupTypeR declId
    Hs.SNsConstr     -> lookupDataR declId
    Hs.SNsVar        -> lookupVarR  declId

resolveEnclosingRef ::
     C.EnclosingRef CreateNames
  -> ResolveE (C.EnclosingRef MangleNames)
resolveEnclosingRef = \case
    C.EnclosingRef e -> do
      -- We must not propagate potential name mangler failures of the enclosing
      -- declaration, which is a backwards reference. Instead, we mark the
      -- reference "unusable".
      r <- tryError $ lookupTypePairR e
      pure $ either (const (C.UnusableEnclosingRef e)) C.EnclosingRef r
    C.UnusableEnclosingRef e ->
      pure $ C.UnusableEnclosingRef e

resolveDeclKind ::
     Macro.HasTypes l
  => C.DeclKind l CreateNames
  -> ResolveE (C.DeclKind l MangleNames)
resolveDeclKind = \case
    C.DeclStruct           x -> C.DeclStruct           <$> resolve x
    C.DeclUnion            x -> C.DeclUnion            <$> resolve x
    C.DeclEnum             x -> C.DeclEnum             <$> resolve x
    C.DeclAnonEnumConstant x -> C.DeclAnonEnumConstant <$> resolve x
    C.DeclTypedef          x -> C.DeclTypedef          <$> resolve x
    C.DeclFunction         x -> C.DeclFunction         <$> resolve x
    C.DeclMacro            x -> C.DeclMacro            <$> resolveMacro x
    C.DeclGlobal           x -> C.DeclGlobal           <$> resolve x
    C.DeclOpaque mSize       -> pure (C.DeclOpaque mSize)

{-------------------------------------------------------------------------------
  Traversal 3: name-map lookups
-------------------------------------------------------------------------------}

lookupTypeR :: C.DeclId -> ResolveE (Hs.Name Hs.NsTypeConstr)
lookupTypeR declId = do
    nameMap <- ask
    case lookupType declId nameMap of
      Nothing ->
        throwError $
          MangleNamesUnderlyingDeclNotMangled declId (NonEmpty.singleton Hs.NsTypeConstr)
      Just hsNm -> pure hsNm

lookupDataR :: C.DeclId -> ResolveE (Hs.Name Hs.NsConstr)
lookupDataR declId = do
    nameMap <- ask
    case lookupData declId nameMap of
      Nothing ->
        throwError $
          MangleNamesUnderlyingDeclNotMangled declId (NonEmpty.singleton Hs.NsConstr)
      Just hsNm -> pure hsNm

lookupVarR :: C.DeclId -> ResolveE (Hs.Name Hs.NsVar)
lookupVarR declId = do
    nameMap <- ask
    case lookupVar declId nameMap of
      Nothing ->
        throwError $
          MangleNamesUnderlyingDeclNotMangled declId (NonEmpty.singleton Hs.NsVar)
      Just hsNm -> pure hsNm

lookupTypePairR :: C.DeclId -> ResolveE DeclIdPair
lookupTypePairR declId =
    (\hsName -> DeclIdPair declId (Hs.demoteNs hsName)) <$> lookupTypeR declId

lookupVarPairR :: C.DeclId -> ResolveE DeclIdPair
lookupVarPairR declId =
    (\hsName -> DeclIdPair declId (Hs.demoteNs hsName)) <$> lookupVarR declId

{-------------------------------------------------------------------------------
  Traversal 3: Resolve instances
-------------------------------------------------------------------------------}

class Resolve a where
  resolve :: a CreateNames -> ResolveE (a MangleNames)

instance Resolve C.Struct where
  resolve struct = do
    fields <- mapM resolve struct.fields
    flam   <- C.traverseFlamField resolve struct.flam
    pure C.Struct{
        fields    = fields
      , flam      = flam
      , ann       = struct.ann
      , sizeof    = struct.sizeof
      , alignment = struct.alignment
      }

instance Resolve C.StructField where
  resolve field = do
    typ'     <- resolve field.typ
    comment' <- traverse resolve field.info.comment
    pure C.StructField{
        info   = C.FieldInfo{
                     loc     = field.info.loc
                   , name    = field.info.name
                   , comment = comment'
                   }
      , typ    = typ'
      , offset = field.offset
      , width  = field.width
      , ann    = field.ann
      }

instance Resolve C.Union where
  resolve union = do
    fields <- mapM resolve union.fields
    pure C.Union{
        fields    = fields
      , ann       = union.ann
      , sizeof    = union.sizeof
      , alignment = union.alignment
      }

instance Resolve C.UnionField where
  resolve field = do
    typ'     <- resolve field.typ
    comment' <- traverse resolve field.info.comment
    pure C.UnionField{
        info = C.FieldInfo{
                   loc     = field.info.loc
                 , name    = field.info.name
                 , comment = comment'
                 }
      , typ  = typ'
      , ann  = field.ann
      }

instance Resolve C.Enum where
  resolve enum = do
    typ'       <- resolve enum.typ
    constants' <- mapM resolve enum.constants
    pure C.Enum{
        typ       = typ'
      , constants = constants'
      , ann       = enum.ann
      , sizeof    = enum.sizeof
      , alignment = enum.alignment
      }

instance Resolve C.EnumConstant where
  resolve constant = do
    comment' <- traverse resolve constant.info.comment
    pure C.EnumConstant{
        info  = C.FieldInfo{
                    loc     = constant.info.loc
                  , name    = constant.info.name
                  , comment = comment'
                  }
      , value = constant.value
      }

instance Resolve C.AnonEnumConstant where
  resolve (C.AnonEnumConstant primTyp constant) = do
    constant' <- resolve constant
    pure C.AnonEnumConstant{
        typ      = primTyp
      , constant = constant'
      }

instance Resolve C.Typedef where
  resolve typedef = do
    typ' <- resolve typedef.typ
    pure C.Typedef{
        typ = typ'
      , ann = typedef.ann
      }

instance Resolve C.Function where
  resolve function = do
    args <- mapM resolveArg function.args
    res' <- resolve function.res
    pure C.Function{
        args  = args
      , res   = res'
      , attrs = function.attrs
      , ann   = function.ann
      }
    where
      resolveArg ::
           C.FunctionArg CreateNames -> ResolveE (C.FunctionArg MangleNames)
      resolveArg arg = do
        argTyp' <- resolve arg.argTyp
        pure C.FunctionArg{
            name   = arg.name
          , argTyp = argTyp'
          }

instance Resolve C.Global where
  resolve global = do
    typ' <- resolve global.typ
    pure C.Global{
        typ = typ'
      , ann = global.ann
      }

resolveMacro ::
     Macro.HasTypes l
  => TypecheckedMacro CreateNames l
  -> ResolveE (TypecheckedMacro MangleNames l)
resolveMacro = \case
    MacroType  typ -> MacroType  <$> resolveMacroType typ
    MacroValue val -> MacroValue <$> resolveMacroValue val

resolveMacroType ::
     Macro.HasTypes l
  => TypecheckedMacroType l CreateNames
  -> ResolveE (TypecheckedMacroType l MangleNames)
resolveMacroType macroType = do
    body' <- traverse resolveMacroTypeVar macroType.body
    pure TypecheckedMacroType{
        body = body'
      , ann  = macroType.ann
      }
  where
    resolveMacroTypeVar ::
         MacroTypeBodyVar CreateNames
      -> ResolveE (MacroTypeBodyVar MangleNames)
    resolveMacroTypeVar = \case
      MacroTypeExtBinding ext -> pure $ MacroTypeExtBinding ext
      MacroTypeBodyVar declId -> MacroTypeBodyVar <$> lookupTypePairR declId

resolveMacroValue ::
     Macro.HasTypes l
  => TypecheckedMacroValue l CreateNames
  -> ResolveE (TypecheckedMacroValue l MangleNames)
resolveMacroValue macroValue = do
    body' <- traverse lookupVarPairR macroValue.body
    pure $ TypecheckedMacroValue body'

instance Resolve C.Comment where
  resolve (C.Comment comment) = C.Comment <$> mapM resolve comment

instance Resolve C.CommentRef where
  resolve = \case
    C.CommentRef name Nothing mKind -> do
      nameMap <- ask
      pure $ C.CommentRef name (searchNameMap nameMap name mKind) mKind
    C.CommentRef name (Just declId) mKind ->
      (\pair -> C.CommentRef name (Just pair) mKind) <$> lookupAnyPair declId

instance Resolve C.Type where
  resolve = \case
      -- Interesting cases
      C.TypeRef declId  -> fmap C.TypeRef $
        lookupTypePairR declId
      C.TypeEnum ref -> fmap C.TypeEnum $
        C.Ref <$> lookupTypePairR ref.name <*> resolve ref.underlying
      C.TypeMacro ref -> fmap C.TypeMacro $
        C.MacroRef <$> lookupTypePairR ref.name <*> resolve ref.underlying
      C.TypeTypedef ref -> fmap C.TypeTypedef $
        C.Ref <$> lookupTypePairR ref.name <*> resolve ref.underlying

      -- Recursive cases
      C.TypePointers n typ             -> C.TypePointers n <$> resolve typ
      C.TypeFun args res               -> C.TypeFun <$> mapM resolve args <*> resolve res
      C.TypeConstArray n typ           -> C.TypeConstArray n <$> resolve typ
      C.TypeIncompleteArray typ        -> C.TypeIncompleteArray <$> resolve typ
      C.TypeBlock typ                  -> C.TypeBlock <$> resolve typ
      C.TypeQual qual typ              -> C.TypeQual qual <$> resolve typ
      C.TypeExtBinding (C.Ref ext uTy) ->
        -- The underlying type may reference the external binding itself (e.g.
        -- the typedef name that was replaced). We extend the 'NameMap' with the
        -- external binding so that such references can be resolved.
        fmap C.TypeExtBinding $ C.Ref ext <$>
          local
            ( #typeConstrs %~ Map.insert ext.cName ext.hsName.name )
            ( resolve uTy )

      -- The other entries do not need any name mangling
      C.TypePrim prim                  -> pure $ C.TypePrim prim
      C.TypeVoid                       -> pure C.TypeVoid
      C.TypeComplex prim               -> pure $ C.TypeComplex prim

instance Resolve C.TypeFunArg where
  resolve arg = C.TypeFunArgF <$> resolve arg.typ <*> pure arg.ann

{-------------------------------------------------------------------------------
  Resolving comment references
-------------------------------------------------------------------------------}

-- | Dispatch on the Doxygen @kindref@ attribute to choose which 'C.NameKind'
-- values to try. Per-kind lookup is handled by 'lookupByKind', which narrows
-- sub-map searches:
--
-- * Compound → only tagged kinds, only 'typeConstrs' (3 lookups)
-- * Member  → ordinary + macro kinds, 'typeConstrs' + 'vars' (4 lookups)
-- * Nothing → all kinds, per-kind dispatch (7 lookups, fallback)
searchNameMap :: NameMap -> Text -> Maybe Doxy.RefKind -> Maybe DeclIdPair
searchNameMap nameMap name = \case
    Just Doxy.RefCompound ->
      searchKinds
        [ C.NameKindTagged C.TagKindStruct
        , C.NameKindTagged C.TagKindUnion
        , C.NameKindTagged C.TagKindEnum ]
    Just Doxy.RefMember ->
      searchKinds [C.NameKindOrdinary, C.NameKindMacro]
    Nothing ->
      searchKinds [minBound .. maxBound]
  where
    -- | For each 'C.NameKind', construct a 'C.DeclId' and try the appropriate
    -- sub-maps via 'lookupByKind'. Return the first match.
    searchKinds :: [C.NameKind] -> Maybe DeclIdPair
    searchKinds kinds = asum
        [ lookupByKind kind (C.DeclId (C.DeclName name kind) False) nameMap
        | kind <- kinds
        ]

-- | Dispatch to the appropriate sub-map(s) based on 'C.NameKind'.
--
-- Tagged names can only be type constructors; ordinary and macro names can be
-- type constructors or variables.
lookupByKind :: C.NameKind -> C.DeclId -> NameMap -> Maybe DeclIdPair
lookupByKind = \case
    C.NameKindTagged{} -> lookupTypeNs
    _                 -> lookupTypeOrVarNs
  where
    lookupTypeNs :: C.DeclId -> NameMap -> Maybe DeclIdPair
    lookupTypeNs declId nameMap =
        DeclIdPair declId . Hs.demoteNs <$> lookupType declId nameMap

    lookupTypeOrVarNs :: C.DeclId -> NameMap -> Maybe DeclIdPair
    lookupTypeOrVarNs declId nameMap =
        DeclIdPair declId <$> asum
          [ Hs.demoteNs <$> lookupType declId nameMap
          , Hs.demoteNs <$> lookupVar  declId nameMap
          ]

lookupAnyPair :: C.DeclId -> ResolveE DeclIdPair
lookupAnyPair declId = do
    nameMap <- ask
    case lookupByKind declId.name.kind declId nameMap of
      Nothing ->
        throwError $
          MangleNamesUnderlyingDeclNotMangled declId $ case declId.name.kind of
            C.NameKindTagged{} -> NonEmpty.singleton Hs.NsTypeConstr
            _                 -> NonEmpty.fromList [Hs.NsTypeConstr, Hs.NsVar]
      Just pair ->
        pure pair

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

withDeclNamespace ::
     (MacroBody p ~ TypecheckedMacro p)
  => C.DeclKind l p
  -> (forall ns. Hs.SingNamespace ns => Proxy ns -> r)
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
          MacroType{}  -> k (Proxy @Hs.NsTypeConstr)
          MacroValue{} -> k (Proxy @Hs.NsVar)

withDeclLoc :: forall p a.
     IsPass p
  => C.DeclInfo p -> a -> C.WithLocationInfo a
withDeclLoc info msg = C.WithLocationInfo{
      loc = idLocationInfo (Proxy @p) info.id [info.loc]
    , msg = msg
    }

-- The function 'tryError' is only available in `mtl` versions 2.3 and newer.
tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)

debugMsg :: MangleNamesMsg -> AnnMsg MangleNames
debugMsg = withCallStack . C.WithLocationInfo C.LocationUnavailable
