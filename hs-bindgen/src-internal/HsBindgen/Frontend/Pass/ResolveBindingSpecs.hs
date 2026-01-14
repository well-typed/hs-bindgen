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
import HsBindgen.Config.ClangArgs qualified as ClangArgs
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Monad (mapMaybeM)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

resolveBindingSpecs ::
     ClangArgs.Target
  -> Hs.ModuleName
  -> MergedBindingSpecs
  -> PrescriptiveBindingSpec
  -> C.TranslationUnit HandleMacros
  -> (C.TranslationUnit ResolveBindingSpecs, [Msg ResolveBindingSpecs])
resolveBindingSpecs target hsModuleName extSpecs pSpec unit =
    let pSpecModule = BindingSpec.moduleName pSpec
        (pSpecErrs, pSpec')
          | pSpecModule == hsModuleName = ([], pSpec)
          | otherwise =
              ( [ResolveBindingSpecsModuleMismatch hsModuleName pSpecModule]
              , BindingSpec.empty target hsModuleName
              )
        (decls, state) =
          runM
            extSpecs
            pSpec'
            unit.includeGraph
            unit.ann.declIndex
            (resolveDecls unit.decls)
        useDeclGraph =
          UseDeclGraph.deleteDeps (Map.keys state.extTypes) unit.ann.useDeclGraph
        notUsedErrs = ResolveBindingSpecsTypeNotUsed <$> Map.keys state.noPTypes
    in  ( reconstruct decls useDeclGraph state
        , pSpecErrs ++ reverse state.traces ++ notUsedErrs
        )
  where
    reconstruct ::
         [C.Decl ResolveBindingSpecs]
      -> UseDeclGraph
      -> MState
      -> C.TranslationUnit ResolveBindingSpecs
    reconstruct decls' useDeclGraph state =
      let externalIds :: Set DeclId
          externalIds = Map.keysSet state.extTypes

          index' :: DeclIndex
          index' =
                DeclIndex.registerExternalDeclarations externalIds
              . DeclIndex.registerOmittedDeclarations state.omitTypes
              $ unit.ann.declIndex

          unitAnn' :: DeclMeta
          unitAnn' = DeclMeta {
                declIndex    = index'
              , useDeclGraph = useDeclGraph
              , declUseGraph = DeclUseGraph.fromUseDecl useDeclGraph
              }

      in C.TranslationUnit{
             decls        = decls'
           , includeGraph = unit.includeGraph
           , ann          = unitAnn'
           }

{-------------------------------------------------------------------------------
  Internal: monad
-------------------------------------------------------------------------------}

newtype M a = WrapM (ReaderT MEnv (State MState) a)
  deriving newtype (
      Applicative
    , Functor
    , Monad
    , MonadReader MEnv
    , MonadState MState
    )

runM ::
     MergedBindingSpecs
  -> PrescriptiveBindingSpec
  -> IncludeGraph
  -> DeclIndex
  -> M a
  -> (a, MState)
runM extSpecs pSpec includeGraph declIndex (WrapM m) =
    let env    = MEnv extSpecs pSpec includeGraph declIndex
        state0 = initMState pSpec
    in  runState (runReaderT m env) state0

{-------------------------------------------------------------------------------
  Internal: monad reader
-------------------------------------------------------------------------------}

data MEnv = MEnv {
      extSpecs     :: MergedBindingSpecs
    , pSpec        :: PrescriptiveBindingSpec
    , includeGraph :: IncludeGraph
    , declIndex    :: DeclIndex
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Internal: monad state
-------------------------------------------------------------------------------}

data MState = MState {
      traces    :: [Msg ResolveBindingSpecs] -- ^ reverse order
    , extTypes  :: Map DeclId (ExtBinding ResolveBindingSpecs)
    , noPTypes  :: Map DeclId [Set SourcePath]
    , omitTypes :: Map DeclId SingleLoc
    }
  deriving (Show, Generic)

initMState :: PrescriptiveBindingSpec -> MState
initMState pSpec = MState {
      traces    = []
    , extTypes  = Map.empty
    , noPTypes  = BindingSpec.getCTypes pSpec
    , omitTypes = Map.empty
    }

insertTrace :: Msg ResolveBindingSpecs -> MState -> MState
insertTrace msg = #traces %~ (msg :)

insertExtType :: DeclId -> ExtBinding ResolveBindingSpecs -> MState -> MState
insertExtType cDeclId typ = #extTypes %~ Map.insert cDeclId typ

deleteNoPType :: DeclId -> SourcePath -> MState -> MState
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

insertOmittedType :: DeclId -> SingleLoc -> MState -> MState
insertOmittedType cDeclId sloc = #omitTypes %~ Map.insert cDeclId sloc

{-------------------------------------------------------------------------------
  Internal: implementation
-------------------------------------------------------------------------------}

-- Resolve declarations, in two passes
resolveDecls :: [C.Decl HandleMacros] -> M [C.Decl ResolveBindingSpecs]
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
     C.Decl HandleMacros
  -> M
       ( Maybe
           ( C.Decl HandleMacros
           , (Maybe BindingSpec.CTypeSpec, Maybe BindingSpec.HsTypeSpec)
           )
       )
resolveTop decl = Reader.ask >>= \env -> do
    let sourcePath = singleLocPath decl.info.loc
        declPaths  = IncludeGraph.reaches env.includeGraph sourcePath
        mMsg       = Just $ ResolveBindingSpecsOmittedType decl.info.id
    isExt <- isJust <$> resolveExtBinding decl.info.id declPaths mMsg
    if isExt
      then do
        State.modify' $ insertTrace (ResolveBindingSpecsExtDecl decl.info.id)
        return Nothing
      else case BindingSpec.lookupCTypeSpec decl.info.id declPaths env.pSpec of
        Just (_hsModuleName, BindingSpec.Require cTypeSpec) -> do
          State.modify' $
              insertTrace (ResolveBindingSpecsPrescriptiveRequire decl.info.id)
            . deleteNoPType decl.info.id sourcePath
          let mHsTypeSpec = do
                hsIdentifier <- cTypeSpec.hsIdent
                BindingSpec.lookupHsTypeSpec hsIdentifier env.pSpec
          return $ Just (decl, (Just cTypeSpec, mHsTypeSpec))
        Just (_hsModuleName, BindingSpec.Omit) -> do
          State.modify' $
              insertTrace (ResolveBindingSpecsPrescriptiveOmit decl.info.id)
            . deleteNoPType decl.info.id sourcePath
            . insertOmittedType decl.info.id decl.info.loc
          return Nothing
        Nothing -> return $ Just (decl, (Nothing, Nothing))

-- Pass two: deep
--
-- Types within the declaration are resolved, and it is reconstructed for the
-- current pass.
resolveDeep ::
     C.Decl HandleMacros
  -> (Maybe BindingSpec.CTypeSpec, Maybe BindingSpec.HsTypeSpec)
  -> M (C.Decl ResolveBindingSpecs)
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

class Resolve a where
  resolve ::
       DeclId -- context declaration
    -> a HandleMacros
    -> M (a ResolveBindingSpecs)

instance Resolve C.DeclKind where
  resolve ctx = \case
      C.DeclStruct struct    -> C.DeclStruct   <$> resolve ctx struct
      C.DeclUnion union      -> C.DeclUnion    <$> resolve ctx union
      C.DeclTypedef typedef  -> C.DeclTypedef  <$> resolve ctx typedef
      C.DeclEnum enum        -> C.DeclEnum     <$> resolve ctx enum
      C.DeclOpaque           -> return C.DeclOpaque
      C.DeclMacro macro      -> C.DeclMacro    <$> resolve ctx macro
      C.DeclFunction fun     -> C.DeclFunction <$> resolve ctx fun
      C.DeclGlobal ty        -> C.DeclGlobal   <$> resolve ctx ty

instance Resolve C.Struct where
  resolve ctx struct =
      reconstruct
        <$> mapM (resolve ctx) struct.fields
        <*> mapM (resolve ctx) struct.flam
    where
      reconstruct ::
           [C.StructField ResolveBindingSpecs]
        -> Maybe (C.StructField ResolveBindingSpecs)
        -> C.Struct ResolveBindingSpecs
      reconstruct structFields' structFlam' = C.Struct {
            fields    = structFields'
          , flam      = structFlam'
          , sizeof    = struct.sizeof
          , alignment = struct.alignment
          , ann       = struct.ann
          }

instance Resolve C.StructField where
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

instance Resolve C.Union where
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

instance Resolve C.UnionField where
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

instance Resolve C.Enum where
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

instance Resolve C.Typedef where
  resolve ctx typedef =
      reconstruct <$> resolve ctx typedef.typ
    where
      reconstruct :: C.Type ResolveBindingSpecs -> C.Typedef ResolveBindingSpecs
      reconstruct typedefType' = C.Typedef {
            typ = typedefType'
          , ann = typedef.ann
          }

instance Resolve C.Function where
  resolve ctx function =
    reconstruct
      <$> mapM (\(mbName, ty) -> (mbName,) <$> resolve ctx ty) function.args
      <*> resolve ctx function.res
    where
      reconstruct ::
           [(Maybe C.ScopedName, C.Type ResolveBindingSpecs)]
        -> C.Type ResolveBindingSpecs
        -> C.Function ResolveBindingSpecs
      reconstruct functionArgs' functionRes' = C.Function {
          args  = functionArgs'
        , res   = functionRes'
        , attrs = function.attrs
        , ann   = function.ann
        }

instance Resolve CheckedMacro where
  resolve ctx = \case
    MacroType typ  -> MacroType <$> resolve ctx typ
    MacroExpr expr -> return (MacroExpr expr)

instance Resolve CheckedMacroType where
  resolve ctx macroType = reconstruct <$> resolve ctx macroType.typ
    where
      reconstruct ::
           C.Type ResolveBindingSpecs
        -> CheckedMacroType ResolveBindingSpecs
      reconstruct typ' = CheckedMacroType {
          typ = typ'
        , ann = macroType.ann
        }

instance Resolve C.Type where
  resolve ctx = \case
      C.TypeRef uid -> do
        mResolved <- aux uid
        let ref' = C.TypeRef uid
        case mResolved of
          Just r  -> return $ r ref'
          Nothing -> return ref'
      C.TypeMacro ref -> do
        mResolved <- aux ref.name
        underlying' <- resolve ctx ref.underlying
        let macro' = C.TypeMacro (C.Ref ref.name underlying')
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
      aux :: DeclId -> M (Maybe (C.Type ResolveBindingSpecs -> C.Type ResolveBindingSpecs))
      aux cDeclId = Reader.ask >>= \env -> State.get >>= \state ->
        -- Check for selected external binding
        case Map.lookup cDeclId state.extTypes of
          Just ty -> do
            State.modify' $ insertTrace (ResolveBindingSpecsExtType ctx cDeclId)
            pure $ Just $ \uTy -> C.TypeExtBinding $ C.Ref ty uTy
          Nothing -> do
            -- Check for external binding of type that is unusable.
            case DeclIndex.lookupUnusableLoc cDeclId env.declIndex of
              []   -> return Nothing
              locs -> do
                let declPaths =
                      foldMap
                        (IncludeGraph.reaches env.includeGraph . singleLocPath)
                        locs
                mTy <- resolveExtBinding cDeclId declPaths Nothing
                case mTy of
                  Just ty -> do
                    State.modify' $
                        insertTrace (ResolveBindingSpecsExtType ctx cDeclId)
                      . insertExtType cDeclId ty
                    pure $ Just $ \uTy -> C.TypeExtBinding $ C.Ref ty uTy
                  Nothing -> return Nothing

{-------------------------------------------------------------------------------
  Internal: auxiliary functions
-------------------------------------------------------------------------------}

-- | Lookup qualified name in the 'ExternalResolvedBindingSpec'
resolveExtBinding ::
     DeclId
  -> Set SourcePath
     -- | Message to emit for omitted types.
  -> Maybe ResolveBindingSpecsMsg
  -> M (Maybe ResolvedExtBinding)
resolveExtBinding cDeclId declPaths mMsg = do
    env <- Reader.ask
    case BindingSpec.lookupMergedBindingSpecs cDeclId declPaths env.extSpecs of
      Just (hsModuleName, BindingSpec.Require cTypeSpec, mHsTypeSpec) ->
        case (cTypeSpec.hsIdent, mHsTypeSpec) of
          (Just hsIdentifier, Just hsTypeSpec) -> do
            let resolved = ResolvedExtBinding {
                    cName  = cDeclId
                  , hsName = Hs.ExtRef hsModuleName hsIdentifier
                  , cSpec  = cTypeSpec
                  , hsSpec = hsTypeSpec
                  }
            case hsTypeSpec.hsRep of
              Just _hsTypeSpecRep -> do
                State.modify' $
                  insertExtType
                    cDeclId
                    resolved
                return (Just resolved)
              Nothing -> do
                State.modify' $
                  insertTrace (ResolveBindingSpecsNoHsTypeRep cDeclId)
                return Nothing
          (Nothing, _) -> do
            State.modify' $
              insertTrace (ResolveBindingSpecsExtHsRefNoIdentifier cDeclId)
            return Nothing
          (_, Nothing) -> do
            State.modify' $
              insertTrace (ResolveBindingSpecsNoHsTypeSpec cDeclId)
            return Nothing
      Just (_hsModuleName, BindingSpec.Omit, _mHsTypeSpec) -> do
        forM_ mMsg $ \msg -> State.modify' $ insertTrace msg
        return Nothing
      Nothing ->
        return Nothing
