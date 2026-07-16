module HsBindgen.Frontend.Pass.MangleNames.ResolveNames (
    resolveNames
  ) where

import Control.Applicative (asum)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Data.Either (partitionEithers)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Proxy
import Data.Set qualified as Set

import HsBindgen.BindingSpec
import HsBindgen.Frontend.Pass.MangleNames.CreateNames
import HsBindgen.Frontend.Pass.MangleNames.Error
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.MangleNames.Names
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Translation
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Macro.Type qualified as Macro

import Doxygen.Parser.Types qualified as Doxy

{-------------------------------------------------------------------------------
  Traversal 3: Resolve names

  Rewrite every 'C.DeclId' reference into a 'DeclIdPair' and every
  'C.ScopedName' reference into a 'ScopedNamePair' using the 'NameMap', copying
  annotations across unchanged. Declarations flagged by 'detectClashes' are
  dropped (their failure is already recorded); a reference to an unmangleable
  declaration drops the referring declaration with a
  'ResolveNamesUnderlyingDeclNotMangled' failure. A reference to an unmangleable
  scoped name drops the referring declaration with a
  'ResolveNamesScopedNameNotMangled' failure.
-------------------------------------------------------------------------------}

type ResolveM   = Reader NameMap
type ResolveE a = ExceptT MangleNamesResolutionError ResolveM a

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
            Left err    -> Left  $ toFailure decl.info (MangleNamesResolutionError err)
            Right decl' -> Right decl'

resolveDecl ::
     Macro.HasTypes l
  => C.Decl l CreateNames
  -> ResolveE (C.Decl l MangleNames)
resolveDecl decl =
    withDeclNamespace decl.kind $ \nsProxy -> do
      info' <- resolveDeclInfo nsProxy decl.info
      kind' <- resolveDeclKind decl.info decl.kind
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
    comment'   <- traverse (resolve info) info.comment
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

resolveFieldInfo ::
     C.DeclInfo CreateNames
  -> C.FieldInfo CreateNames
  -> ResolveE (C.FieldInfo MangleNames)
resolveFieldInfo info fieldInfo = do
    name'    <- lookupScopedNamePairR info.id fieldInfo.name
    comment' <- traverse (resolve info) fieldInfo.comment
    pure C.FieldInfo{
        loc   = fieldInfo.loc
      , name  = name'
      , comment = comment'
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
  => C.DeclInfo CreateNames
  -> C.DeclKind l CreateNames
  -> ResolveE (C.DeclKind l MangleNames)
resolveDeclKind info = \case
    C.DeclStruct           x -> C.DeclStruct           <$> resolve info x
    C.DeclUnion            x -> C.DeclUnion            <$> resolve info x
    C.DeclEnum             x -> C.DeclEnum             <$> resolve info x
    C.DeclAnonEnumConstant x -> C.DeclAnonEnumConstant <$> resolve info x
    C.DeclTypedef          x -> C.DeclTypedef          <$> resolve info x
    C.DeclFunction         x -> C.DeclFunction         <$> resolve info x
    C.DeclMacro            x -> C.DeclMacro            <$> resolveMacro x
    C.DeclGlobal           x -> C.DeclGlobal           <$> resolve info x
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
          ResolveNamesUnderlyingDeclNotMangled declId (NonEmpty.singleton Hs.NsTypeConstr)
      Just hsNm -> pure hsNm

lookupDataR :: C.DeclId -> ResolveE (Hs.Name Hs.NsConstr)
lookupDataR declId = do
    nameMap <- ask
    case lookupData declId nameMap of
      Nothing ->
        throwError $
          ResolveNamesUnderlyingDeclNotMangled declId (NonEmpty.singleton Hs.NsConstr)
      Just hsNm -> pure hsNm

lookupVarR :: C.DeclId -> ResolveE (Hs.Name Hs.NsVar)
lookupVarR declId = do
    nameMap <- ask
    case lookupVar declId nameMap of
      Nothing ->
        throwError $
          ResolveNamesUnderlyingDeclNotMangled declId (NonEmpty.singleton Hs.NsVar)
      Just hsNm -> pure hsNm

lookupScopedNameR :: C.DeclId -> C.ScopedName -> ResolveE Hs.SomeName
lookupScopedNameR declId scopedName = do
    nameMap <- ask
    case lookupScopedName declId scopedName nameMap of
      Nothing ->
        throwError $
          ResolveNamesScopedNameNotMangled declId scopedName
      Just hsNm -> pure hsNm

lookupTypePairR :: C.DeclId -> ResolveE DeclIdPair
lookupTypePairR declId =
    (\hsName -> DeclIdPair declId (Hs.demoteNs hsName)) <$> lookupTypeR declId

lookupVarPairR :: C.DeclId -> ResolveE DeclIdPair
lookupVarPairR declId =
    (\hsName -> DeclIdPair declId (Hs.demoteNs hsName)) <$> lookupVarR declId

lookupScopedNamePairR :: C.DeclId -> C.ScopedName -> ResolveE ScopedNamePair
lookupScopedNamePairR declId scopedName =
    ScopedNamePair scopedName <$> lookupScopedNameR declId scopedName

{-------------------------------------------------------------------------------
  Traversal 3: Resolve instances
-------------------------------------------------------------------------------}

class Resolve a where
  resolve :: C.DeclInfo CreateNames -> a CreateNames -> ResolveE (a MangleNames)

instance Resolve C.Struct where
  resolve info struct = do
    fields <- mapM (resolve info) struct.fields
    flam   <- C.traverseFlamField (resolve info) struct.flam
    pure C.Struct{
        fields    = fields
      , flam      = flam
      , ann       = struct.ann
      , sizeof    = struct.sizeof
      , alignment = struct.alignment
      }

instance Resolve C.Union where
  resolve info union = do
    fields <- mapM (resolve info) union.fields
    pure C.Union{
        fields    = fields
      , ann       = union.ann
      , sizeof    = union.sizeof
      , alignment = union.alignment
      }

instance Resolve C.Field where
  resolve info = \case
      C.FieldExplicit field -> C.FieldExplicit <$> (resolve info) field
      C.FieldImplicit field -> C.FieldImplicit <$> (resolve info) field

instance Resolve C.ExplicitField where
  resolve info field = do
    fieldInfo' <- resolveFieldInfo info field.info
    typ'       <- (resolve info) field.typ
    pure C.ExplicitField{
        info   = fieldInfo'
      , typ    = typ'
      , offset = field.offset
      , width  = field.width
      , ann    = field.ann
      }

instance Resolve C.ImplicitField where
  resolve info field = do
    fieldInfo' <- resolveFieldInfo info field.info
    typRef'    <- resolve info field.typRef
    pure C.ImplicitField{
        info   = fieldInfo'
      , typRef    = typRef'
      , offset = field.offset
      , ann    = field.ann
      }

instance Resolve C.AnonRef where
  resolve info = \case
      C.AnonRef ref -> C.AnonRef <$> lookupTypePairR ref
      C.AnonExtBinding ext -> C.AnonExtBinding <$> resolveExtBindingRef info ext

instance Resolve C.Enum where
  resolve info enum = do
    typ'       <- (resolve info) enum.typ
    constants' <- mapM (resolve info) enum.constants
    pure C.Enum{
        typ       = typ'
      , constants = constants'
      , ann       = enum.ann
      , sizeof    = enum.sizeof
      , alignment = enum.alignment
      }

instance Resolve C.EnumConstant where
  resolve info constant = do
    fieldInfo' <- resolveFieldInfo info constant.info
    pure C.EnumConstant{
        info  = fieldInfo'
      , value = constant.value
      }

instance Resolve C.AnonEnumConstant where
  resolve info (C.AnonEnumConstant primTyp constant) = do
    constant' <- (resolve info) constant
    pure C.AnonEnumConstant{
        typ      = primTyp
      , constant = constant'
      }

instance Resolve C.Typedef where
  resolve info typedef = do
    typ' <- (resolve info) typedef.typ
    pure C.Typedef{
        typ = typ'
      , ann = typedef.ann
      }

instance Resolve C.Function where
  resolve info function = do
    args <- mapM resolveArg function.args
    res' <- (resolve info) function.res
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
        name' <- mapM (lookupScopedNamePairR info.id) arg.name
        argTyp' <- (resolve info) arg.argTyp
        pure C.FunctionArg{
            name   = name'
          , argTyp = argTyp'
          }

instance Resolve C.Global where
  resolve info global = do
    typ' <- resolve info global.typ
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
      , deps = macroType.deps
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
    pure $ TypecheckedMacroValue body' macroValue.deps

instance Resolve C.Comment where
  resolve info (C.Comment comment) = C.Comment <$> mapM (resolve info) comment

instance Resolve C.CommentRef where
  resolve _info = \case
    C.CommentRef name Nothing mKind -> do
      nameMap <- ask
      pure $ C.CommentRef name (searchNameMap nameMap name mKind) mKind
    C.CommentRef name (Just declId) mKind ->
      (\pair -> C.CommentRef name (Just pair) mKind) <$> lookupAnyPair declId

instance Resolve C.Type where
  resolve info = \case
      -- Interesting cases
      C.TypeRef declId  -> fmap C.TypeRef $
        lookupTypePairR declId
      C.TypeEnum ref -> fmap C.TypeEnum $
        C.Ref <$> lookupTypePairR ref.name <*> resolve info ref.underlying
      C.TypeMacro ref -> fmap C.TypeMacro $
        C.MacroRef <$> lookupTypePairR ref.name <*> resolve info ref.underlying
      C.TypeTypedef ref -> fmap C.TypeTypedef $
        C.Ref <$> lookupTypePairR ref.name <*> resolve info ref.underlying

      -- Recursive cases
      C.TypePointers n typ             -> C.TypePointers n <$> resolve info typ
      C.TypeFun args res               -> C.TypeFun <$> mapM (resolve info) args <*> resolve info res
      C.TypeConstArray n typ           -> C.TypeConstArray n <$> resolve info typ
      C.TypeIncompleteArray typ        -> C.TypeIncompleteArray <$> resolve info typ
      C.TypeBlock typ                  -> C.TypeBlock <$> resolve info typ
      C.TypeQual qual typ              -> C.TypeQual qual <$> resolve info typ
      C.TypeExtBinding ref             -> C.TypeExtBinding <$> resolveExtBindingRef info ref

      -- The other entries do not need any name mangling
      C.TypePrim prim                  -> pure $ C.TypePrim prim
      C.TypeVoid                       -> pure C.TypeVoid
      C.TypeComplex prim               -> pure $ C.TypeComplex prim

instance Resolve C.TypeFunArg where
  resolve info arg = C.TypeFunArgF <$> resolve info arg.typ <*> pure arg.ann

resolveExtBindingRef ::
     C.DeclInfo CreateNames
  -> C.ExtBindingRef CreateNames
  -> ResolveE (C.ExtBindingRef MangleNames)
resolveExtBindingRef info (C.Ref ext uTy) =
    -- The underlying type may reference the external binding itself (e.g.
    -- the typedef name that was replaced). We extend the 'NameMap' with the
    -- external binding so that such references can be resolved.
    C.Ref ext <$>
      local
        ( #typeConstrs %~ Map.insert ext.cName ext.hsName.name )
        ( resolve info uTy )

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
          ResolveNamesUnderlyingDeclNotMangled declId $ case declId.name.kind of
            C.NameKindTagged{} -> NonEmpty.singleton Hs.NsTypeConstr
            _                 -> NonEmpty.fromList [Hs.NsTypeConstr, Hs.NsVar]
      Just pair ->
        pure pair

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- The function 'tryError' is only available in `mtl` versions 2.3 and newer.
tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)
