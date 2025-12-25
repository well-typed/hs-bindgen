{-# LANGUAGE OverloadedLabels #-}

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
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming qualified as C
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
resolveBindingSpecs
  target
  hsModuleName
  extSpecs
  pSpec
  C.TranslationUnit{unitDecls, unitIncludeGraph, unitAnn} =
    let pSpecModule = BindingSpec.moduleName pSpec
        (pSpecErrs, pSpec')
          | pSpecModule == hsModuleName = ([], pSpec)
          | otherwise =
              ( [ResolveBindingSpecsModuleMismatch hsModuleName pSpecModule]
              , BindingSpec.empty target hsModuleName
              )
        (decls, state@MState{..}) =
          runM
            extSpecs
            pSpec'
            unitIncludeGraph
            unitAnn.declIndex
            (resolveDecls unitDecls)
        useDeclGraph =
          UseDeclGraph.deleteDeps (Map.keys stateExtTypes) unitAnn.declUseDecl
        notUsedErrs = ResolveBindingSpecsTypeNotUsed <$> Map.keys stateNoPTypes
    in  ( reconstruct decls useDeclGraph state
        , pSpecErrs ++ reverse stateTraces ++ notUsedErrs
        )
  where
    reconstruct ::
         [C.Decl ResolveBindingSpecs]
      -> UseDeclGraph
      -> MState
      -> C.TranslationUnit ResolveBindingSpecs
    reconstruct decls' useDeclGraph MState{..} =
      let externalIds :: Set C.DeclId
          externalIds = Map.keysSet stateExtTypes

          index' :: DeclIndex
          index' =
              DeclIndex.registerExternalDeclarations externalIds
            . DeclIndex.registerOmittedDeclarations stateOmitTypes
            $ unitAnn.declIndex

          unitAnn' :: DeclMeta
          unitAnn' =
            unitAnn {
              declIndex   = index'
            , declUseDecl = useDeclGraph
            , declDeclUse = DeclUseGraph.fromUseDecl useDeclGraph
            }
      in  C.TranslationUnit{
        unitDecls = decls'
      , unitIncludeGraph
      , unitAnn = unitAnn'
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
      envExtSpecs     :: MergedBindingSpecs
    , envPSpec        :: PrescriptiveBindingSpec
    , envIncludeGraph :: IncludeGraph
    , envDeclIndex    :: DeclIndex
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Internal: monad state
-------------------------------------------------------------------------------}

data MState = MState {
      stateTraces    :: [Msg ResolveBindingSpecs] -- ^ reverse order
    , stateExtTypes  :: Map C.DeclId (C.Type ResolveBindingSpecs)
    , stateNoPTypes  :: Map C.DeclId [Set SourcePath]
    , stateOmitTypes :: Map C.DeclId SingleLoc
    }
  deriving (Show)

initMState :: PrescriptiveBindingSpec -> MState
initMState pSpec = MState {
      stateTraces    = []
    , stateExtTypes  = Map.empty
    , stateNoPTypes  = BindingSpec.getCTypes pSpec
    , stateOmitTypes = Map.empty
    }

insertTrace :: Msg ResolveBindingSpecs -> MState -> MState
insertTrace msg st = st {
      stateTraces = msg : stateTraces st
    }

insertExtType :: C.DeclId -> C.Type ResolveBindingSpecs -> MState -> MState
insertExtType cDeclId typ st = st {
      stateExtTypes = Map.insert cDeclId typ (stateExtTypes st)
    }

deleteNoPType :: C.DeclId -> SourcePath -> MState -> MState
deleteNoPType cDeclId path st = st {
      stateNoPTypes = Map.update (aux []) cDeclId (stateNoPTypes st)
    }
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

insertOmittedType ::
     C.DeclId
  -> SingleLoc
  -> MState
  -> MState
insertOmittedType cDeclId sloc st = st {
      stateOmitTypes = Map.insert cDeclId sloc (stateOmitTypes st)
    }

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
resolveTop decl = Reader.ask >>= \MEnv{..} -> do
    let sourcePath = singleLocPath $ C.declLoc (C.declInfo decl)
        declPaths  = IncludeGraph.reaches envIncludeGraph sourcePath
        mMsg       = Just $ ResolveBindingSpecsOmittedType cDeclId
    isExt <- isJust <$> resolveExtBinding cDeclId declPaths mMsg
    if isExt
      then do
        State.modify' $ insertTrace (ResolveBindingSpecsExtDecl cDeclId)
        return Nothing
      else case BindingSpec.lookupCTypeSpec cDeclId declPaths envPSpec of
        Just (_hsModuleName, BindingSpec.Require cTypeSpec) -> do
          State.modify' $
              insertTrace (ResolveBindingSpecsPrescriptiveRequire cDeclId)
            . deleteNoPType cDeclId sourcePath
          let mHsTypeSpec = do
                hsIdentifier <- BindingSpec.cTypeSpecIdentifier cTypeSpec
                BindingSpec.lookupHsTypeSpec hsIdentifier envPSpec
          return $ Just (decl, (Just cTypeSpec, mHsTypeSpec))
        Just (_hsModuleName, BindingSpec.Omit) -> do
          State.modify' $
              insertTrace (ResolveBindingSpecsPrescriptiveOmit cDeclId)
            . deleteNoPType cDeclId sourcePath
            . insertOmittedType cDeclId decl.declInfo.declLoc
          return Nothing
        Nothing -> return $ Just (decl, (Nothing, Nothing))
  where
    cDeclId :: C.DeclId
    cDeclId = decl.declInfo.declId

-- Pass two: deep
--
-- Types within the declaration are resolved, and it is reconstructed for the
-- current pass.
resolveDeep ::
     C.Decl HandleMacros
  -> (Maybe BindingSpec.CTypeSpec, Maybe BindingSpec.HsTypeSpec)
  -> M (C.Decl ResolveBindingSpecs)
resolveDeep decl mTypeSpecs = do
    declKind' <- resolve decl.declInfo.declId decl.declKind
    return C.Decl {
        declInfo = coercePass decl.declInfo
      , declKind = declKind'
      , declAnn  = mTypeSpecs
      }

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

class Resolve a where
  resolve ::
       C.DeclId -- context declaration
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
  resolve ctx C.Struct{..} = reconstruct <$> mapM (resolve ctx) structFields
    where
      reconstruct ::
           [C.StructField ResolveBindingSpecs]
        -> C.Struct ResolveBindingSpecs
      reconstruct structFields' = C.Struct {
          structFields = structFields'
        , ..
        }

instance Resolve C.StructField where
  resolve ctx C.StructField{..} = reconstruct <$> resolve ctx structFieldType
    where
      reconstruct ::
           C.Type ResolveBindingSpecs
        -> C.StructField ResolveBindingSpecs
      reconstruct structFieldType' = C.StructField {
          structFieldInfo = coercePass structFieldInfo
        , structFieldType = structFieldType'
        , ..
        }

instance Resolve C.Union where
  resolve ctx C.Union{..} = reconstruct <$> mapM (resolve ctx) unionFields
    where
      reconstruct ::
           [C.UnionField ResolveBindingSpecs]
        -> C.Union ResolveBindingSpecs
      reconstruct unionFields' = C.Union {
          unionFields = unionFields'
        , ..
        }

instance Resolve C.UnionField where
  resolve ctx C.UnionField{..} = reconstruct <$> resolve ctx unionFieldType
    where
      reconstruct :: C.Type ResolveBindingSpecs
                 -> C.UnionField ResolveBindingSpecs
      reconstruct unionFieldType' = C.UnionField {
          unionFieldInfo = coercePass unionFieldInfo
        , unionFieldType = unionFieldType'
        , ..
        }

instance Resolve C.Enum where
  resolve ctx C.Enum{..} = reconstruct <$> resolve ctx enumType
    where
      reconstruct :: C.Type ResolveBindingSpecs -> C.Enum ResolveBindingSpecs
      reconstruct enumType' = C.Enum {
          enumType      = enumType'
        , enumConstants = map coercePass enumConstants
        , ..
        }

instance Resolve C.Typedef where
  resolve ctx C.Typedef{..} = reconstruct <$> resolve ctx typedefType
    where
      reconstruct :: C.Type ResolveBindingSpecs -> C.Typedef ResolveBindingSpecs
      reconstruct typedefType' = C.Typedef {
            typedefType = typedefType'
          , ..
          }

instance Resolve C.Function where
  resolve ctx C.Function{..} =
    reconstruct
      <$> mapM (\(mbName, ty) -> (mbName,) <$> resolve ctx ty) functionArgs
      <*> resolve ctx functionRes
    where
      reconstruct ::
           [(Maybe C.ScopedName, C.Type ResolveBindingSpecs)]
        -> C.Type ResolveBindingSpecs
        -> C.Function ResolveBindingSpecs
      reconstruct functionArgs' functionRes' = C.Function {
          functionArgs = functionArgs'
        , functionRes  = functionRes'
        , ..
        }

instance Resolve C.CheckedMacro where
  resolve ctx = \case
    C.MacroType typ  -> C.MacroType <$> resolve ctx typ
    C.MacroExpr expr -> return (C.MacroExpr expr)

instance Resolve C.CheckedMacroType where
  resolve ctx C.CheckedMacroType{..} = reconstruct <$> resolve ctx macroType
    where
      reconstruct ::
           C.Type ResolveBindingSpecs
        -> C.CheckedMacroType ResolveBindingSpecs
      reconstruct macroType' = C.CheckedMacroType {
          macroType = macroType'
        , ..
        }

instance Resolve C.Type where
  resolve ctx = \case
      C.TypeRef uid -> do
        mResolved <- aux uid
        case mResolved of
          Just r  -> return r
          Nothing -> return $ C.TypeRef uid
      C.TypeTypedef (C.TypedefRef uid uTy) -> do
        mResolved <- aux uid
        case mResolved of
          Just r  -> return r
          Nothing -> C.TypeTypedef . C.TypedefRef uid <$> resolve ctx uTy

      -- Recursive cases
      C.TypePointers n t      -> C.TypePointers n <$> resolve ctx t
      C.TypeFun args res      ->
        C.TypeFun <$> mapM (resolve ctx) args <*> resolve ctx res
      C.TypeConstArray n t    -> C.TypeConstArray n <$> resolve ctx t
      C.TypeIncompleteArray t -> C.TypeIncompleteArray <$> resolve ctx t
      C.TypeBlock t           -> C.TypeBlock <$> resolve ctx t
      C.TypeQualified qual t  -> C.TypeQualified qual <$> resolve ctx t

      -- Simple cases
      C.TypePrim t         -> return (C.TypePrim t)
      C.TypeVoid           -> return (C.TypeVoid)
      C.TypeExtBinding ext -> absurd ext
      C.TypeComplex t      -> return (C.TypeComplex t)
    where
      aux :: C.DeclId -> M (Maybe (C.Type ResolveBindingSpecs))
      aux cDeclId = Reader.ask >>= \MEnv{..} -> State.get >>= \MState{..} ->
        -- Check for selected external binding
        case Map.lookup cDeclId stateExtTypes of
          Just ty -> do
            State.modify' $ insertTrace (ResolveBindingSpecsExtType ctx cDeclId)
            return $ Just ty
          Nothing -> do
            -- Check for external binding of type that is unusable.
            case DeclIndex.lookupUnusableLoc cDeclId envDeclIndex of
              []   -> return Nothing
              locs -> do
                let declPaths =
                      foldMap
                        (IncludeGraph.reaches envIncludeGraph . singleLocPath)
                        locs
                mTy <- fmap C.TypeExtBinding <$>
                  resolveExtBinding cDeclId declPaths Nothing
                case mTy of
                  Just ty -> do
                    State.modify' $
                        insertTrace (ResolveBindingSpecsExtType ctx cDeclId)
                      . insertExtType cDeclId ty
                    return (Just ty)
                  Nothing -> return Nothing

{-------------------------------------------------------------------------------
  Internal: auxiliary functions
-------------------------------------------------------------------------------}

-- | Lookup qualified name in the 'ExternalResolvedBindingSpec'
resolveExtBinding ::
     C.DeclId
  -> Set SourcePath
     -- | Message to emit for omitted types.
  -> Maybe ResolveBindingSpecsMsg
  -> M (Maybe ResolvedExtBinding)
resolveExtBinding cDeclId declPaths mMsg = do
    MEnv{envExtSpecs} <- Reader.ask
    case BindingSpec.lookupMergedBindingSpecs cDeclId declPaths envExtSpecs of
      Just (hsModuleName, BindingSpec.Require cTypeSpec, mHsTypeSpec) ->
        case (BindingSpec.cTypeSpecIdentifier cTypeSpec, mHsTypeSpec) of
          (Just hsIdentifier, Just hsTypeSpec) -> do
            let resolved = ResolvedExtBinding {
                    extCDeclId = cDeclId
                  , extHsRef   = Hs.ExtRef hsModuleName hsIdentifier
                  , extCSpec   = cTypeSpec
                  , extHsSpec  = hsTypeSpec
                  }
            case BindingSpec.hsTypeSpecRep hsTypeSpec of
              Just _hsTypeSpecRep -> do
                State.modify' $
                  insertExtType
                    cDeclId
                    (C.TypeExtBinding resolved)
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
