module HsBindgen.Frontend.Pass.MangleNames.CreateNames (
    -- * Pass definition
    CreateNames
    -- * Traversal 1: create names
  , createNames
    -- * Shared types (needed by traversals 2 and 3)
  , MangleNamesFailure(..)
  , toFailure
    -- * Shared utility (needed by traversal 3)
  , withDeclNamespace
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Except (ExceptT, MonadError (..), liftEither, runExcept,
                             runExceptT)
import Control.Monad.Reader (Reader, asks, runReader)
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Proxy

import Clang.HighLevel.Types

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config.MangleCandidate (MangleCandidate (..))
import HsBindgen.Config.MangleCandidate qualified as MangleCandidate
import HsBindgen.Config.Prelims (FieldNamingStrategy (..))
import HsBindgen.Frontend.Analysis.Typedefs (TypedefAnalysis)
import HsBindgen.Frontend.Analysis.Typedefs qualified as TypedefAnalysis
import HsBindgen.Frontend.Pass.MangleNames.Error
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.MangleNames.Names
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.IR.Translation
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Macro.Type qualified as Macro
import HsBindgen.Util.Tracer (WithCallStack, withCallStack)

{-------------------------------------------------------------------------------
  Intermediate pass: CreateNames

  This pass is local to name mangling: it is /not/ exposed as a frontend
  artefact and not selectable via the CLI. It is the result of the first of the
  three name-mangling traversals ("create names"): it has all within-declaration
  names minted (as annotations and 'ScopedNamePair' slots), but all references
  (the 'Id' slots) are still unresolved 'DeclId's. The final 'resolveNames'
  traversal rewrites these into 'DeclIdPair's, producing 'MangleNames'.

  Note that @'Ann' ix 'CreateNames'@ and @'Ann' ix 'MangleNames'@ reduce to the
  same bundle types, and 'ScopedName' is 'ScopedNamePair' in both. The
  annotations and scoped names can therefore be carried across directly during
  'resolveNames'.
-------------------------------------------------------------------------------}

-- | Create names pass (local to name mangling)
type CreateNames :: Pass
data CreateNames a

type family AnnCreateNames ix where
  AnnCreateNames "Decl"                 = PrescriptiveDeclSpec
  AnnCreateNames "Struct"               = StructNames
  AnnCreateNames "Flam"                 = FlamNames
  AnnCreateNames "Union"                = NewtypeNames
  AnnCreateNames "Enum"                 = NewtypeNames
  AnnCreateNames "Typedef"              = TypedefNames
  AnnCreateNames "TypecheckedMacroType" = NewtypeNames
  AnnCreateNames _                      = NoAnn

instance IsPass CreateNames

instance PassId CreateNames where
  type Id CreateNames = C.DeclId

instance PassScopedName CreateNames where
  type ScopedName CreateNames = ScopedNamePair

instance PassMacro CreateNames where
  type MacroId         CreateNames = C.DeclId
  type MacroBody       CreateNames = TypecheckedMacro CreateNames
  type MacroUnderlying CreateNames = C.Type CreateNames

  macroIdId _ = id

instance PassExtBinding CreateNames where
  type ExtBinding CreateNames = BindingSpec.ResolvedExtBinding

  extBindingId _ = (.cName)

instance PassCommentDecl CreateNames where
  type CommentDecl CreateNames = Maybe (C.Comment CreateNames)

instance PassAnn CreateNames where
  type Ann ix CreateNames = AnnCreateNames ix

instance PassMsg CreateNames where
  type Msg CreateNames = C.WithLocationInfo MangleNamesMsg

{-------------------------------------------------------------------------------
  CoercePass: ResolveBindingSpecs → CreateNames

  Used by the "create names" traversal to reindex the parts of a declaration
  that do not change representation (types, comments, enclosing references). The
  'Id' (and hence 'MacroId') and 'ExtBinding' representations agree between the
  two passes, so these coercions are pure reindexing.
-------------------------------------------------------------------------------}

instance CoercePassId               ResolveBindingSpecs CreateNames
instance CoercePassMacroId          ResolveBindingSpecs CreateNames
instance CoercePassAnn "TypeFunArg" ResolveBindingSpecs CreateNames

instance CoercePassMacroUnderlying ResolveBindingSpecs CreateNames where
  coercePassMacroUnderlying _ = coercePass

instance CoercePassCommentDecl ResolveBindingSpecs CreateNames where
  coercePassCommentDecl _ = fmap coercePass

{-------------------------------------------------------------------------------
  Shared types
-------------------------------------------------------------------------------}

data MangleNamesFailure = MangleNamesFailure {
      id  :: C.DeclId
    , loc :: SingleLoc
    , err :: MangleNamesError
    }
  deriving (Show, Eq, Ord, Generic)

toFailure :: (Id p ~ C.DeclId) => C.DeclInfo p -> MangleNamesError -> MangleNamesFailure
toFailure i e = MangleNamesFailure i.id i.loc e

{-------------------------------------------------------------------------------
  Shared utility
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

{-------------------------------------------------------------------------------
  Internal: working with 'MangleCandidate'
-------------------------------------------------------------------------------}

type E m a = ExceptT MangleNamesCreationError m a

mangleCandidate :: forall ns.
     Hs.SingNamespace ns
  => MangleCandidate Maybe
  -> Proxy ns
  -> Text
  -> E Identity (Hs.Name ns)
mangleCandidate mc _ cName =
    case MangleCandidate.mangleCandidate mc cName of
      Just hsName -> pure hsName
      Nothing     -> throwError $ CreateNamesCouldNotMangle cName

{-------------------------------------------------------------------------------
  Traversal 1: Create names

  For each declaration we first choose its top-level name (step 1a,
  'nameForDecl') and, when that succeeds, immediately mint all its
  within-declaration names (step 1b, 'createDecl').

  The top-level name feeds the 'NameMap' regardless of what happens in 1b: a
  squashed or otherwise un-emitted declaration must still be resolvable by name.
-------------------------------------------------------------------------------}

createNames ::
     forall l. (HasCallStack, Macro.HasTypes l)
  => TypedefAnalysis
  -> MangleCandidate Maybe
  -> FieldNamingStrategy
  -> [C.Decl l ResolveBindingSpecs]
  -> ( [C.Decl l CreateNames]
     , [(C.DeclId, Hs.Name Hs.NsTypeConstr, TypedefAnalysis.Squash)]
     , NameMap
     , [MangleNamesFailure]
     , [AnnMsg MangleNames]
     )
createNames td mc strategy decls = (
      map snd successes
    , squashes
    , nameMap
    , failures
    , messages
    )
  where
    specifiedNames :: Map C.DeclId (Hs.Name Hs.NsTypeConstr)
    specifiedNames = Map.fromList $ mapMaybe getSpecifiedName decls

    getSpecifiedName ::
      C.Decl l ResolveBindingSpecs -> Maybe (C.DeclId, Hs.Name Hs.NsTypeConstr)
    getSpecifiedName decl = (decl.info.id,) <$> ((.hsName) =<< decl.ann.cSpec)

    env :: CreateEnv
    env = CreateEnv{
        mangleCandidate     = mc
      , fieldNamingStrategy = strategy
      }

    results :: [CreateNamesResult (C.Decl l CreateNames)]
    messages :: [AnnMsg MangleNames]
    (results, messages) = second concat $ unzip $ map perDecl decls

    perDecl ::
         C.Decl l ResolveBindingSpecs
      -> (CreateNamesResult (C.Decl l CreateNames), [AnnMsg MangleNames])
    perDecl decl = (,msgs) $ case nameResult of
        CnFailure err      -> CnFailure err
        CnSquashed nC nH s -> CnSquashed nC nH s
        CnMangled name _   -> createNamesWithin env name decl
      where
        nameResult :: CreateNamesResult ()
        msgs       :: [AnnMsg MangleNames]
        (nameResult, msgs) = nameForDecl td mc specifiedNames decl

    failures  :: [MangleNamesFailure]
    squashes  :: [(C.DeclId, Hs.Name Hs.NsTypeConstr, TypedefAnalysis.Squash)]
    successes :: [(DeclIdPair, C.Decl l CreateNames)]
    (failures, squashes, successes) = partitionCreateResults results

    nameMap :: NameMap
    nameMap = fromDeclIdPairs $
         map fst successes
      ++ map (\(cN, hN, _) -> DeclIdPair cN (Hs.demoteNs hN)) squashes

nameForDecl ::
     HasCallStack
  => TypedefAnalysis
  -> MangleCandidate Maybe
  -> Map C.DeclId (Hs.Name Hs.NsTypeConstr)
  -> C.Decl l ResolveBindingSpecs
  -> (CreateNamesResult (), [AnnMsg MangleNames])
nameForDecl td mc specifiedNames decl =
    second toMs $
    withDeclNamespace decl.kind $ \(nsProxy :: Proxy ns) ->
      let mangleNs :: Text -> Either MangleNamesCreationError (Hs.Name ns)
          mangleNs d = runExcept $ mangleCandidate mc nsProxy d
      in case Map.lookup declId td.map of
        -- Squashing always affects two declarations: the surrounding typedef
        -- (squashed) and the inner declaration, which "uses the name of" the
        -- typedef (see 'UseNameOf' below). We generate no binding for the
        -- typedef, but we still need _two_ name-map entries pointing at the same
        -- Haskell name: one for the inner declaration and one for the typedef.
        --
        -- Crucially, this squash decision is independent of whether a name was
        -- specified: a squashed typedef must never be emitted as an ordinary
        -- binding, or it would collide with the inner declaration it shares a
        -- name with. A specified name (on the typedef itself, or failing that on
        -- its squash target) is honoured for the name-map entry; otherwise we
        -- mangle the typedef's own name.
        --
        -- TODO <https://github.com/well-typed/hs-bindgen/issues/1436>
        -- When squashing becomes configurable, revisit how a specified name
        -- interacts with squashing here.
        Just (TypedefAnalysis.Squash s) ->
          case Hs.singNamespace @ns of
            Hs.SNsTypeConstr ->
              case Map.lookup declId specifiedNames
                     <|> Map.lookup s.targetId specifiedNames of
                Just hsNm -> (CnSquashed declId hsNm s, [])
                Nothing   ->
                  mangleType declId.name.text & \case
                    Left err   -> failWith err
                    Right hsNm -> (CnSquashed declId hsNm s, [])
            _otherwise ->
              failWith $ CreateNamesSquashMustBeTypeConstr
                (Hs.namespaceOf $ Hs.singNamespace @ns)

        mConclusion ->
          case Map.lookup declId specifiedNames of
            Just hsNm ->
              -- Binding spec specified a name for this declaration. This
              -- overrides any naming decision we might make here.
              (succeedWith hsNm, [])
            Nothing ->
              case mConclusion of
                Nothing ->
                  mangleNs declId.name.text & \case
                    Left err   -> failWith err
                    Right hsNm -> (succeedWith hsNm, [])
                Just (TypedefAnalysis.AddSuffix suffix) ->
                  mangleNs (declId.name.text <> suffix) & \case
                    Left err ->
                      failWith err
                    Right hsNm ->
                      ( succeedWith hsNm
                      , [MangleNamesAssignedName (Hs.demoteNs hsNm)] )
                Just (TypedefAnalysis.UseNameOf declId') ->
                  case Map.lookup declId' specifiedNames of
                    Just hsNm ->
                      ( succeedWith hsNm
                      , [ MangleNamesReusedAssignedName (Hs.demoteNs hsNm) ] )
                    Nothing ->
                      mangleNs declId'.name.text & \case
                        Left err ->
                          failWith err
                        Right hsNm ->
                          ( succeedWith hsNm
                          , [ MangleNamesAssignedName (Hs.demoteNs hsNm)
                            | declId.name.text /= declId'.name.text ] )
  where
    declId :: C.DeclId
    declId = decl.info.id

    info :: C.DeclInfo ResolveBindingSpecs
    info = decl.info

    failWith :: MangleNamesCreationError -> (CreateNamesResult a, [MangleNamesMsg])
    failWith err = (CnFailure $ toFailure info (MangleNamesCreationError err), [])

    getPair :: Hs.SingNamespace ns => Hs.Name ns -> DeclIdPair
    getPair = DeclIdPair declId . Hs.demoteNs

    succeedWith :: Hs.SingNamespace ns => Hs.Name ns -> CreateNamesResult ()
    succeedWith n = CnMangled (getPair n) ()

    toMs :: HasCallStack => [a] -> [WithCallStack (C.WithLocationInfo a)]
    toMs = map (withCallStack . withDeclLoc decl.info)

    mangleType :: Text -> Either MangleNamesCreationError (Hs.Name Hs.NsTypeConstr)
    mangleType d =
      runExcept $
        mangleCandidate mc (Proxy :: Proxy Hs.NsTypeConstr) d

{-------------------------------------------------------------------------------
  Traversal 1b: within-declaration names
-------------------------------------------------------------------------------}

data CreateEnv = CreateEnv{
      mangleCandidate     :: MangleCandidate Maybe
    , fieldNamingStrategy :: FieldNamingStrategy
    }
  deriving stock (Generic)

type CreateM   = Reader CreateEnv
type CreateE a = ExceptT MangleNamesCreationError CreateM a

runCreate :: CreateEnv -> CreateE a -> Either MangleNamesCreationError a
runCreate env action = runReader (runExceptT action) env

-- | Apply Haskell naming rules to a candidate name
mkName ::
     Hs.SingNamespace ns
  => Proxy ns -> Text -> CreateE (Hs.Name ns)
mkName ns candidate = do
    mc <- asks (.mangleCandidate)
    liftEither . runExcept $ mangleCandidate mc ns candidate

data CreateNamesResult a =
      CnFailure   MangleNamesFailure
    | CnSquashed  C.DeclId (Hs.Name Hs.NsTypeConstr) TypedefAnalysis.Squash
    | CnMangled   DeclIdPair a

partitionCreateResults ::
     [CreateNamesResult a]
  -> ( [MangleNamesFailure]
     , [(C.DeclId, Hs.Name Hs.NsTypeConstr, TypedefAnalysis.Squash)]
     , [(DeclIdPair, a)] )
partitionCreateResults = rev . Foldable.foldl' aux ([], [], [])
  where
    aux (fs, ss, ds) = \case
      CnFailure  f       -> (f:fs, ss,           ds      )
      CnSquashed cN hN s -> (fs,   (cN,hN,s):ss, ds      )
      CnMangled  n d     -> (fs,   ss,           (n,d):ds)

    rev (fs, ss, ds) = (reverse fs, reverse ss, reverse ds)

-- | Mint the within-declaration names of a declaration whose top-level name
-- ('hsName') has already been chosen in step 1a.
createNamesWithin ::
     Macro.HasTypes l
  => CreateEnv
  -> DeclIdPair
  -> C.Decl l ResolveBindingSpecs
  -> CreateNamesResult (C.Decl l CreateNames)
createNamesWithin env declIdPair decl =
    case runCreate env (createDeclKind declIdPair.hsName.text decl.kind) of
      Left err   -> CnFailure $ toFailure decl.info (MangleNamesCreationError err)
      Right kind -> CnMangled declIdPair C.Decl{
          info = coercePass decl.info
        , kind = kind
        , ann  = decl.ann
        }

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

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1925>
--
-- Whether we need the auxiliary name or not should be reflected in the AST, so
-- we can use an annotation just like we do for FLAMs.
createTypedefNames ::
  Bool -> FieldNamingStrategy -> Text -> CreateE TypedefNames
createTypedefNames isFunPtr strategy name = do
    orig <- createNewtypeNames strategy name
    aux  <-
      if isFunPtr then do
        auxName  <- mkName (Proxy @Hs.NsTypeConstr) $ name <> "_Aux"
        auxNames <- createNewtypeNames strategy auxName.text
        pure $ Just (auxName, auxNames)
      else
        pure Nothing
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
    names    <- createTypedefNames isFunPtr strategy hsName
    pure C.Typedef{
        typ = coercePass typedef.typ
      , ann = names
      }
  where
    -- TODO https://github.com/well-typed/hs-bindgen/issues/1925
    --
    -- Tie generation of names to the generation of the associated code. This
    -- is especially ugly.
    isFunPtr :: Bool
    isFunPtr = isJust $ typedef <$ C.getFirstFunTypeIndirection typedef.typ

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
      , deps = macroType.deps
      , ann  = names
      }

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

withDeclLoc :: forall p a.
     IsPass p
  => C.DeclInfo p -> a -> C.WithLocationInfo a
withDeclLoc info msg = C.WithLocationInfo{
      loc = idLocationInfo (Proxy @p) info.id [info.loc]
    , msg = msg
    }
