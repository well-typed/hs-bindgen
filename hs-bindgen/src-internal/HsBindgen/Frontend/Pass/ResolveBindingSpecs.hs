module HsBindgen.Frontend.Pass.ResolveBindingSpecs (
    resolveBindingSpecs
  ) where

import Control.Monad ((<=<))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Reader qualified as Reader
import Control.Monad.State (MonadState, State, runState)
import Control.Monad.State qualified as State
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.BindingSpec (MergedBindingSpecs, PrescriptiveBindingSpec)
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.Pass.Zip.IsPass
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Macro.Flip
import HsBindgen.Macro.Type qualified as Macro
import HsBindgen.Util.Monad (mapMaybeM)
import HsBindgen.Util.Tracer (withCallStack)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

type PreviousPass = Zip

resolveBindingSpecs ::
     forall l. HasCallStack
  => Hs.ModuleName
  -> MergedBindingSpecs
  -> PrescriptiveBindingSpec
  -> C.TranslationUnit l PreviousPass
  -> (C.TranslationUnit l ResolveBindingSpecs, [AnnMsg ResolveBindingSpecs])
resolveBindingSpecs hsModuleName extSpecs pSpec unit =
    let pSpecModule = BindingSpec.moduleName pSpec
        (pSpecErrs, pSpec')
          | pSpecModule == hsModuleName = ([], pSpec)
          | otherwise =
              ( [withCallStack (ResolveBindingSpecsModuleMismatch hsModuleName pSpecModule)]
              , BindingSpec.empty hsModuleName
              )
        (decls, state) =
          runM
            extSpecs
            pSpec'
            unit.includeGraph
            unit.meta.declIndex
            (resolveDecls unit.decls)
        declUseGraph =
            DeclUseGraph.deleteRevDeps (Map.keysSet state.extTypes)
          . DeclUseGraph.deleteDeps state.opqTypes
          $ unit.meta.declUseGraph
        notUsedErrs = map (withCallStack . ResolveBindingSpecsTypeNotUsed) $ Map.keys state.noPTypes
    in  ( reconstruct decls declUseGraph state
        , pSpecErrs ++ reverse state.traces ++ notUsedErrs
        )
  where
    reconstruct ::
         [C.Decl l ResolveBindingSpecs]
      -> DeclUseGraph
      -> MState
      -> C.TranslationUnit l ResolveBindingSpecs
    reconstruct decls' declUseGraph state =
      let externalIds :: Set C.DeclId
          externalIds = Map.keysSet state.extTypes

          index' :: DeclIndex l
          index' =
                DeclIndex.registerExternalDeclarations externalIds
              . DeclIndex.registerOmittedDeclarations state.omitTypes
              $ unit.meta.declIndex

          unitMeta' :: DeclMeta l
          unitMeta' = DeclMeta {
                declIndex    = index'
              , useDeclGraph = UseDeclGraph.fromDeclUseGraph declUseGraph
              , declUseGraph = declUseGraph
              }

      in C.TranslationUnit{
             decls        = decls'
           , includeGraph = unit.includeGraph
           , meta         = unitMeta'
           }

{-------------------------------------------------------------------------------
  Internal: monad
-------------------------------------------------------------------------------}

newtype M l a = WrapM (ReaderT (MEnv l) (State MState) a)
  deriving newtype (
      Applicative
    , Functor
    , Monad
    , MonadReader (MEnv l)
    , MonadState MState
    )

runM ::
     MergedBindingSpecs
  -> PrescriptiveBindingSpec
  -> IncludeGraph
  -> DeclIndex l
  -> M l a
  -> (a, MState)
runM extSpecs pSpec includeGraph declIndex (WrapM m) =
    let env    = MEnv extSpecs pSpec includeGraph declIndex
        state0 = initMState pSpec
    in  runState (runReaderT m env) state0

{-------------------------------------------------------------------------------
  Internal: monad reader
-------------------------------------------------------------------------------}

data MEnv l = MEnv {
      extSpecs     :: MergedBindingSpecs
    , pSpec        :: PrescriptiveBindingSpec
    , includeGraph :: IncludeGraph
    , declIndex    :: DeclIndex l
    }

deriving stock instance Macro.HasTypes l => Show (MEnv l)

{-------------------------------------------------------------------------------
  Internal: monad state
-------------------------------------------------------------------------------}

data MState = MState {
      traces    :: [AnnMsg ResolveBindingSpecs] -- ^ reverse order
    , extTypes  :: Map C.DeclId (ExtBinding ResolveBindingSpecs)
    , noPTypes  :: Map C.DeclId [Set SourcePath]
    , omitTypes :: Map C.DeclId SingleLoc
    , opqTypes  :: Set C.DeclId -- ^ opaqued types
    }
  deriving (Show, Generic)

initMState :: PrescriptiveBindingSpec -> MState
initMState pSpec = MState {
      traces    = []
    , extTypes  = Map.empty
    , noPTypes  = BindingSpec.getCTypes pSpec
    , omitTypes = Map.empty
    , opqTypes  = Set.empty
    }

insertTrace :: AnnMsg ResolveBindingSpecs -> MState -> MState
insertTrace msg = #traces %~ (msg :)

insertExtType :: C.DeclId -> ExtBinding ResolveBindingSpecs -> MState -> MState
insertExtType cDeclId typ = #extTypes %~ Map.insert cDeclId typ

insertOpaquedType :: C.DeclId -> MState -> MState
insertOpaquedType cDeclId = #opqTypes %~ Set.insert cDeclId

deleteNoPType :: C.DeclId -> SourcePath -> MState -> MState
deleteNoPType cDeclId path = #noPTypes %~ Map.update (aux []) cDeclId
  where
    aux :: [Set SourcePath] -> [Set SourcePath] -> Maybe [Set SourcePath]
    aux acc = \case
      s : ss
        | Set.member path s ->
            case ss ++ acc of
              []  -> Nothing
              ss' -> Just ss'
        | otherwise -> aux (s : acc) ss
      [] -> Just acc

insertOmittedType :: C.DeclId -> SingleLoc -> MState -> MState
insertOmittedType cDeclId sloc = #omitTypes %~ Map.insert cDeclId sloc

{-------------------------------------------------------------------------------
  Internal: implementation
-------------------------------------------------------------------------------}

-- Resolve declarations, in two passes
resolveDecls ::
     HasCallStack
  => [C.Decl l PreviousPass]
  -> M l [C.Decl l ResolveBindingSpecs]
resolveDecls = mapM (uncurry resolveDeep) <=< mapMaybeM resolveTop

-- Pass one: top-level
--
-- If a declaration has an external binding, then the declaration is dropped and
-- the external binding is recorded.
--
-- If a declaration is omitted, then the declaration is dropped and the omission
-- is recorded.
--
-- Otherwise, the declaration is kept and is associated with a type
-- specification when applicable.
resolveTop ::
     HasCallStack
  => C.Decl l PreviousPass
  -> M l
       ( Maybe
           ( C.Decl l PreviousPass
           , (Maybe BindingSpec.CTypeSpec, Maybe BindingSpec.HsTypeSpec)
           )
       )
resolveTop decl = Reader.ask >>= \env -> do
    let sourcePath = singleLocPath decl.info.loc
        declPaths  = IncludeGraph.reaches env.includeGraph sourcePath
        mMsg       = Just $ withCallStack $ ResolveBindingSpecsOmittedType decl.info.id
    isExt <- isJust <$> resolveExtBinding decl.info.id declPaths mMsg
    if isExt
      then do
        State.modify' $ insertTrace (withCallStack $ ResolveBindingSpecsExtDecl decl.info.id)
        return Nothing
      else case BindingSpec.lookupCTypeSpec decl.info.id declPaths env.pSpec of
        Just (_hsModuleName, BindingSpec.Require cTypeSpec) -> do
          State.modify' $
              insertTrace (withCallStack $ ResolveBindingSpecsPreRequire decl.info.id)
            . deleteNoPType decl.info.id sourcePath
          let mHsTypeSpec = do
                hsIdentifier <- cTypeSpec.hsName
                BindingSpec.lookupHsTypeSpec hsIdentifier env.pSpec
          applyPrescriptive decl cTypeSpec mHsTypeSpec
        Just (_hsModuleName, BindingSpec.Omit) -> do
          State.modify' $
              insertTrace (withCallStack $ ResolveBindingSpecsPreOmit decl.info.id)
            . deleteNoPType decl.info.id sourcePath
            . insertOmittedType decl.info.id decl.info.loc
          return Nothing
        Nothing -> return $ Just (decl, (Nothing, Nothing))

-- | Apply prescriptive binding specifications
--
-- A prescriptive binding specification can change a declaration or even drop
-- it.
--
-- Type specifications that do not match declarations may themselves be mutated.
applyPrescriptive ::
     forall l. HasCallStack
  => C.Decl l PreviousPass
  -> BindingSpec.CTypeSpec
  -> Maybe BindingSpec.HsTypeSpec
  -> M l
       ( Maybe
           ( C.Decl l PreviousPass
           , (Maybe BindingSpec.CTypeSpec, Maybe BindingSpec.HsTypeSpec)
           )
       )
applyPrescriptive decl cTypeSpec = \case
    Nothing         -> return $ Just (decl, (Just cTypeSpec, Nothing))
    Just hsTypeSpec -> do
      -- TODO <https://github.com/well-typed/hs-bindgen/issues/1447>
      -- We should validate instances only set for supported kinds
      -- (instances themselves are to be resolved in a separate pass)
      (decl', hsRep) <- case hsTypeSpec.hsRep of
        Nothing    -> return (decl, Nothing)
        Just hsRep -> case hsRep of
          BindingSpec.HsTypeRepRecord    recordRep  -> auxRecord recordRep
          BindingSpec.HsTypeRepNewtype   newtypeRep -> auxNewtype newtypeRep
          BindingSpec.HsTypeRepEmptyData            -> auxEmptyData
          BindingSpec.HsTypeRepTypeAlias            -> auxTypeAlias
      let hsTypeSpec' = hsTypeSpec{ BindingSpec.hsRep = hsRep }
      return $ Just (decl', (Just cTypeSpec, Just hsTypeSpec'))
  where
    auxRecord ::
         BindingSpec.HsRecordRep
      -> M l (C.Decl l PreviousPass, Maybe BindingSpec.HsTypeRep)
    auxRecord recordRep =
      -- TODO <https://github.com/well-typed/hs-bindgen/issues/1447>
      -- We should validate the record type and number of fields.
      return (decl, Just (BindingSpec.HsTypeRepRecord recordRep))

    auxNewtype ::
         BindingSpec.HsNewtypeRep
      -> M l (C.Decl l PreviousPass, Maybe BindingSpec.HsTypeRep)
    auxNewtype newtypeRep =
      -- TODO <https://github.com/well-typed/hs-bindgen/issues/1447>
      -- We should validate enum, typedef, or macro type
      return (decl, Just (BindingSpec.HsTypeRepNewtype newtypeRep))

    auxEmptyData :: M l (C.Decl l PreviousPass, Maybe BindingSpec.HsTypeRep)
    auxEmptyData = do
      declIndex <- Reader.asks (.declIndex)
      -- A complete C type keeps its size and alignment, so that a 'StaticSize'
      -- instance can be generated for the empty data type.  Structs, unions, and
      -- enums carry the layout directly; a typedef is followed to its underlying
      -- type.  Types that are genuinely opaque in C, primitives and pointers
      -- (whose size is not available at this pass), and macro types keep
      -- 'Nothing'.
      let (isValid, mSize) = case decl.kind of
            C.DeclStruct s    -> (True, Just (C.OpaqueSize s.sizeof s.alignment))
            C.DeclUnion  u    -> (True, Just (C.OpaqueSize u.sizeof u.alignment))
            C.DeclEnum   e    -> (True, Just (C.OpaqueSize e.sizeof e.alignment))
            C.DeclTypedef td  -> (True, underlyingOpaqueSize declIndex td.typ)
            C.DeclOpaque m    -> (True, m)
            C.DeclMacro macro -> case macro of
              MacroType{}  -> (True,  Nothing)
              MacroValue{} -> (False, Nothing)
            _otherwise        -> (False, Nothing)
      if isValid
        then do
          State.modify' $
              insertTrace (withCallStack $ ResolveBindingSpecsPreEmptyData decl.info.id)
            . insertOpaquedType decl.info.id
          -- Cannot use record update because 'C.kind' is ambiguous
          let decl' = C.Decl{
                  C.info = decl.info
                , C.kind = C.DeclOpaque mSize
                , C.ann  = decl.ann
                }
          return (decl', Just BindingSpec.HsTypeRepEmptyData)
        else do
          State.modify' $
            insertTrace (withCallStack $ ResolveBindingSpecsPreEmptyDataInvalid decl.info.id)
          return (decl, Nothing)

    auxTypeAlias :: M l (C.Decl l PreviousPass, Maybe BindingSpec.HsTypeRep)
    auxTypeAlias =
      -- TODO <https://github.com/well-typed/hs-bindgen/issues/1447>
      -- We should validate types.
      -- Return different decl?
      return (decl, Just BindingSpec.HsTypeRepTypeAlias)

-- | Size and alignment of an @emptydata@ type's underlying C type, when it can
-- be determined at this pass.
--
-- 'C.getCanonicalType' resolves typedef and macro-type references and strips
-- qualifiers, so a typedef to a struct or union surfaces as a 'C.TypeRef' to its
-- declaration, whose layout we recover via the declaration index.  Primitives,
-- pointers, and enums canonicalise to a type with no such reference and yield
-- 'Nothing', since their sizes are not available in this frontend pass.
underlyingOpaqueSize ::
     DeclIndex l
  -> C.Type PreviousPass
  -> Maybe C.OpaqueSize
underlyingOpaqueSize declIndex ty =
    case C.getCanonicalType ty of
      C.TypeRef declId -> do
        decl <- DeclIndex.lookup declId declIndex
        case decl.kind of
          C.DeclStruct s -> pure (C.OpaqueSize s.sizeof s.alignment)
          C.DeclUnion  u -> pure (C.OpaqueSize u.sizeof u.alignment)
          _otherwise     -> Nothing
      _otherwise       -> Nothing

-- Pass two: deep
--
-- Types within the declaration are resolved, and it is reconstructed for the
-- current pass.
resolveDeep ::
     C.Decl l PreviousPass
  -> (Maybe BindingSpec.CTypeSpec, Maybe BindingSpec.HsTypeSpec)
  -> M l (C.Decl l ResolveBindingSpecs)
resolveDeep decl (cSpec, hsSpec) = do
    declKind' <- resolve decl.info.id decl.kind
    return C.Decl {
        info = coercePass decl.info
      , kind = declKind'
      , ann  = PrescriptiveDeclSpec{cSpec = cSpec, hsSpec = hsSpec}
      }

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

-- | Resolve references to external declarations
--
-- This is part of the second pass ('resolveDeep').
--
-- We do not need to handle references to omitted declarations here since
-- declarations depending on unavailable declarations will be removed in the
-- @Select@ pass.
class Resolve a l where
  resolve ::
       HasCallStack
    => C.DeclId -- context declaration
    -> a PreviousPass
    -> M l (a ResolveBindingSpecs)

resolveFlip ::
     Resolve (Flip f n) l
  => C.DeclId
  -> f PreviousPass n
  -> M l (f ResolveBindingSpecs n)
resolveFlip declId = flipM (resolve declId)

instance Resolve (C.DeclKind l) l where
  resolve ctx = \case
      C.DeclStruct struct                  -> C.DeclStruct           <$> resolve ctx struct
      C.DeclUnion union                    -> C.DeclUnion            <$> resolve ctx union
      C.DeclTypedef typedef                -> C.DeclTypedef          <$> resolve ctx typedef
      C.DeclEnum enum                      -> C.DeclEnum             <$> resolve ctx enum
      C.DeclAnonEnumConstant anonEnumConst -> pure $ C.DeclAnonEnumConstant (coercePass anonEnumConst)
      C.DeclOpaque mSize                   -> return (C.DeclOpaque mSize)
      C.DeclMacro macro                    -> C.DeclMacro            <$> resolveFlip ctx macro
      C.DeclFunction fun                   -> C.DeclFunction         <$> resolve ctx fun
      C.DeclGlobal ty                      -> C.DeclGlobal           <$> resolve ctx ty

instance Resolve C.Struct l where
  resolve ctx struct =
      reconstruct
        <$> mapM (resolve ctx) struct.fields
        <*> C.traverseFlamField (resolve ctx) struct.flam
    where
      reconstruct ::
           [C.StructField ResolveBindingSpecs]
        -> C.Flam ResolveBindingSpecs
        -> C.Struct ResolveBindingSpecs
      reconstruct structFields' structFlam' = C.Struct {
            fields    = structFields'
          , flam      = structFlam'
          , sizeof    = struct.sizeof
          , alignment = struct.alignment
          , ann       = struct.ann
          }

instance Resolve C.StructField l where
  resolve ctx field =
      reconstruct <$> resolve ctx field.typ
    where
      reconstruct ::
           C.Type ResolveBindingSpecs
        -> C.StructField ResolveBindingSpecs
      reconstruct structFieldType' = C.StructField {
          typ    = structFieldType'
        , info   = coercePass field.info
        , offset = field.offset
        , width  = field.width
        , ann    = field.ann
        }

instance Resolve C.Union l where
  resolve ctx union =
      reconstruct <$> mapM (resolve ctx) union.fields
    where
      reconstruct ::
           [C.UnionField ResolveBindingSpecs]
        -> C.Union ResolveBindingSpecs
      reconstruct unionFields' = C.Union {
          fields    = unionFields'
        , sizeof    = union.sizeof
        , alignment = union.alignment
        , ann       = union.ann
        }

instance Resolve C.UnionField l where
  resolve ctx field =
      reconstruct <$> resolve ctx field.typ
    where
      reconstruct :: C.Type ResolveBindingSpecs
                 -> C.UnionField ResolveBindingSpecs
      reconstruct unionFieldType' = C.UnionField {
          typ  = unionFieldType'
        , info = coercePass field.info
        , ann  = field.ann
        }

instance Resolve C.Enum l where
  resolve ctx enum =
      reconstruct <$> resolve ctx enum.typ
    where
      reconstruct :: C.Type ResolveBindingSpecs -> C.Enum ResolveBindingSpecs
      reconstruct enumType' = C.Enum {
          typ       = enumType'
        , constants = map coercePass enum.constants
        , sizeof    = enum.sizeof
        , alignment = enum.alignment
        , ann       = enum.ann
        }

instance Resolve C.Typedef l where
  resolve ctx typedef =
      reconstruct <$> resolve ctx typedef.typ
    where
      reconstruct :: C.Type ResolveBindingSpecs -> C.Typedef ResolveBindingSpecs
      reconstruct typedefType' = C.Typedef {
            typ = typedefType'
          , ann = typedef.ann
          }

instance Resolve C.Function l where
  resolve ctx function =
    reconstruct
      <$> mapM (resolve ctx) function.args
      <*> resolve ctx function.res
    where
      reconstruct ::
           [C.FunctionArg ResolveBindingSpecs]
        -> C.Type ResolveBindingSpecs
        -> C.Function ResolveBindingSpecs
      reconstruct functionArgs' functionRes' = C.Function {
          args  = functionArgs'
        , res   = functionRes'
        , attrs = function.attrs
        , ann   = function.ann
        }

instance Resolve C.Global l where
  resolve ctx global =
      reconstruct <$> resolve ctx global.typ
    where
      reconstruct :: C.Type ResolveBindingSpecs -> C.Global ResolveBindingSpecs
      reconstruct globalType' = C.Global{
            typ = globalType'
          , ann = global.ann
          }

instance Resolve C.FunctionArg l where
  resolve ctx functionArg =
    reconstruct
      <$> pure functionArg.name
      <*> resolve ctx functionArg.argTyp
    where
      reconstruct ::
           Maybe (ScopedName ResolveBindingSpecs)
        -> C.TypeFunArg ResolveBindingSpecs
        -> C.FunctionArg ResolveBindingSpecs
      reconstruct name' argTyp' = C.FunctionArg {
            name = name'
          , argTyp = argTyp'
          }

instance Resolve (Flip TypecheckedMacro l) l where
  resolve ctx (Flip m) = Flip <$> case m of
    MacroType  typ -> MacroType  <$> resolve ctx typ
    MacroValue val -> MacroValue <$> pure (coercePass val)

instance Resolve (TypecheckedMacroType l) l where
  resolve ctx = \case
      (TypecheckedMacroType body ann) -> do
        body' <- traverse resolveVar body
        pure TypecheckedMacroType{
            body = body'
          , ann  = ann
          }
    where
      -- TODO <https://github.com/well-typed/hs-bindgen/issues/1969>
      --
      -- Usage of 'MacroTypeBodyVar' hints at possible drawbacks of our design
      -- of 'C.DeclId'. Maybe we can directly refer to external bindings from
      -- 'C.DeclId'?
      resolveVar ::
           MacroTypeBodyVar Zip
        -> M l (MacroTypeBodyVar ResolveBindingSpecs)
      resolveVar (MacroTypeExtBinding   x) = absurd x
      resolveVar (MacroTypeBodyVar declId) = do
          mExt <- auxExt ctx declId
          pure $ case mExt of
            Just ext -> MacroTypeExtBinding ext
            Nothing  -> MacroTypeBodyVar declId

instance Resolve C.Type l where
  resolve ctx = \case
      C.TypeRef uid -> do
        mResolved <- aux uid
        let ref' = C.TypeRef uid
        case mResolved of
          Just r  -> return $ r ref'
          Nothing -> return ref'
      C.TypeEnum ref -> do
        mResolved <- aux ref.name
        underlying' <- resolve ctx ref.underlying
        let enum' = C.TypeEnum (C.Ref ref.name underlying')
        case mResolved of
          Just r  -> return $ r enum'
          Nothing -> return enum'
      C.TypeMacro ref -> do
        mResolved <- aux ref.name
        underlying' <- resolve ctx ref.underlying
        let macro' = C.TypeMacro (C.MacroRef ref.name underlying')
        case mResolved of
          Just r  -> return $ r macro'
          Nothing -> return macro'
      C.TypeTypedef ref -> do
        mResolved <- aux ref.name
        underlying' <- resolve ctx ref.underlying
        let typedef' = C.TypeTypedef (C.Ref ref.name underlying')
        case mResolved of
          Just r  -> return $ r typedef'
          Nothing -> return typedef'

      -- Recursive cases
      C.TypePointers n t      -> C.TypePointers n <$> resolve ctx t
      C.TypeFun args res      ->
        C.TypeFun <$> mapM (resolve ctx) args <*> resolve ctx res
      C.TypeConstArray n t    -> C.TypeConstArray n <$> resolve ctx t
      C.TypeIncompleteArray t -> C.TypeIncompleteArray <$> resolve ctx t
      C.TypeBlock t           -> C.TypeBlock <$> resolve ctx t
      C.TypeQual qual t       -> C.TypeQual qual <$> resolve ctx t

      -- Simple cases
      C.TypePrim t         -> return (C.TypePrim t)
      C.TypeVoid           -> return (C.TypeVoid)
      C.TypeComplex t      -> return (C.TypeComplex t)
    where
      aux ::
           HasCallStack
        => C.DeclId
        -> M l (Maybe (C.Type ResolveBindingSpecs -> C.Type ResolveBindingSpecs))
      aux cDeclId = fmap reconstruct <$> auxExt ctx cDeclId
        where
          reconstruct ty uTy = C.TypeExtBinding $ C.Ref ty uTy

instance Resolve C.TypeFunArg l where
  resolve ctx arg = do
      typ' <- resolve ctx arg.typ
      pure C.TypeFunArgF {
          typ = typ'
        , ann = arg.ann
        }

auxExt ::
     HasCallStack
  => C.DeclId
  -> C.DeclId
  -> M l (Maybe BindingSpec.ResolvedExtBinding)
auxExt ctx cDeclId = Reader.ask >>= \env -> State.get >>= \state ->
    case Map.lookup cDeclId state.extTypes of
      Just ty -> do
        State.modify' $ insertTrace (withCallStack $ ResolveBindingSpecsExtType ctx cDeclId)
        pure (Just ty)
      Nothing ->
        case DeclIndex.lookupUnusableLoc cDeclId env.declIndex of
          []   -> pure Nothing
          locs -> do
            let declPaths =
                  foldMap
                    (IncludeGraph.reaches env.includeGraph . singleLocPath)
                    locs
            mTy <- resolveExtBinding cDeclId declPaths Nothing
            case mTy of
              Just ty -> do
                State.modify' $
                    insertTrace (withCallStack $ ResolveBindingSpecsExtType ctx cDeclId)
                  . insertExtType cDeclId ty
                pure (Just ty)
              Nothing -> pure Nothing

{-------------------------------------------------------------------------------
  Internal: auxiliary functions
-------------------------------------------------------------------------------}

-- | Lookup qualified name in the 'HsBindgen.BindingSpec.Private.V1.ResolvedBindingSpec'
resolveExtBinding ::
     HasCallStack
  => C.DeclId
  -> Set SourcePath
     -- | Message to emit for omitted types.
  -> Maybe (AnnMsg ResolveBindingSpecs)
  -> M l (Maybe BindingSpec.ResolvedExtBinding)
resolveExtBinding cDeclId declPaths mMsg = do
    env <- Reader.ask
    case BindingSpec.lookupMergedBindingSpecs cDeclId declPaths env.extSpecs of
      Just (hsModuleName, BindingSpec.Require cTypeSpec, mHsTypeSpec) ->
        case (cTypeSpec.hsName, mHsTypeSpec) of
          (Just hsName, Just hsTypeSpec) -> do
            let resolved = BindingSpec.ResolvedExtBinding {
                    cName  = cDeclId
                  , hsName = Hs.ExtRef hsModuleName hsName
                  , cSpec  = cTypeSpec
                  , hsSpec = hsTypeSpec
                  }
            State.modify' $ insertExtType cDeclId resolved
            return (Just resolved)
          (Nothing, _) -> do
            State.modify' $
              insertTrace (withCallStack $ ResolveBindingSpecsExtHsRefNoIdentifier cDeclId)
            return Nothing
          (_, Nothing) -> do
            State.modify' $
              insertTrace (withCallStack $ ResolveBindingSpecsNoHsTypeSpec cDeclId)
            return Nothing
      Just (_hsModuleName, BindingSpec.Omit, _mHsTypeSpec) -> do
        forM_ mMsg $ \msg -> State.modify' $ insertTrace msg
        return Nothing
      Nothing ->
        return Nothing
