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
import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.NameAnon.IsPass
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
  -> C.TranslationUnit NameAnon
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
    in  ( reassemble decls useDeclGraph state
        , pSpecErrs ++ reverse stateTraces ++ notUsedErrs
        )
  where
    reassemble ::
         [C.Decl ResolveBindingSpecs]
      -> UseDeclGraph
      -> MState
      -> C.TranslationUnit ResolveBindingSpecs
    reassemble decls' useDeclGraph MState{..} =
      let externalIds :: Set C.PrelimDeclId
          externalIds = Map.keysSet stateExtTypes

          index' :: DeclIndex
          index' =
            DeclIndex.registerExternalDeclarations externalIds    $
            DeclIndex.registerOmittedDeclarations  stateOmitTypes $
              unitAnn.declIndex

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
    , stateExtTypes  :: Map C.PrelimDeclId (C.Type ResolveBindingSpecs)
    , stateNoPTypes  :: Map C.DeclName [Set SourcePath]
    , stateOmitTypes :: Map C.PrelimDeclId (C.DeclName, SourcePath)
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

insertExtType ::
     C.PrelimDeclId
  -> C.Type ResolveBindingSpecs
  -> MState
  -> MState
insertExtType cQualPrelimDeclId typ st = st {
      stateExtTypes =
        Map.insert cQualPrelimDeclId typ (stateExtTypes st)
    }

deleteNoPType :: C.DeclName -> SourcePath -> MState -> MState
deleteNoPType cDeclName path st = st {
      stateNoPTypes = Map.update (aux []) cDeclName (stateNoPTypes st)
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

insertOmittedType :: C.PrelimDeclId -> C.DeclName -> SourcePath -> MState -> MState
insertOmittedType cQualPrelimDeclId cDeclName path st = st {
      stateOmitTypes = Map.insert cQualPrelimDeclId (cDeclName, path) (stateOmitTypes st)
    }

{-------------------------------------------------------------------------------
  Internal: implementation
-------------------------------------------------------------------------------}

-- Resolve declarations, in two passes
resolveDecls :: [C.Decl NameAnon] -> M [C.Decl ResolveBindingSpecs]
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
     C.Decl NameAnon
  -> M
       ( Maybe
           ( C.Decl NameAnon
           , (Maybe BindingSpec.CTypeSpec, Maybe BindingSpec.HsTypeSpec)
           )
       )
resolveTop decl = Reader.ask >>= \MEnv{..} ->
    case decl.declInfo.declId.origDeclId of
      C.OrigDeclId cOrigDeclId -> do
        let cDeclName   = C.declName decl
            sourcePath  = singleLocPath $ C.declLoc (C.declInfo decl)
            declPaths   = IncludeGraph.reaches envIncludeGraph sourcePath
            mMsg        = Just $ ResolveBindingSpecsOmittedType cDeclName
        isExt <- isJust <$>
          resolveExtBinding cDeclName cOrigDeclId declPaths mMsg
        if isExt
          then do
            State.modify' $ insertTrace (ResolveBindingSpecsExtDecl cDeclName)
            return Nothing
          else case BindingSpec.lookupCTypeSpec cDeclName declPaths envPSpec of
            Just (_hsModuleName, BindingSpec.Require cTypeSpec) -> do
              State.modify' $
                  insertTrace (ResolveBindingSpecsPrescriptiveRequire cDeclName)
                . deleteNoPType cDeclName sourcePath
              let mHsTypeSpec = do
                    hsIdentifier <- BindingSpec.cTypeSpecIdentifier cTypeSpec
                    BindingSpec.lookupHsTypeSpec hsIdentifier envPSpec
              return $ Just (decl, (Just cTypeSpec, mHsTypeSpec))
            Just (_hsModuleName, BindingSpec.Omit) -> do
              State.modify' $
                  insertTrace (ResolveBindingSpecsPrescriptiveOmit cDeclName)
                . deleteNoPType cDeclName sourcePath
                . insertOmittedType cOrigDeclId cDeclName sourcePath
              return Nothing
            Nothing -> return $ Just (decl, (Nothing, Nothing))
      C.AuxForDecl _parent ->
        -- TODO <https://github.com/well-typed/hs-bindgen/issues/1379>
        -- Auxiliary declarations are not introduced until @HandleTypedefs@
        panicPure "Unexpected aux decl"

-- Pass two: deep
--
-- Types within the declaration are resolved, and it is reassembled for the
-- current pass.
resolveDeep ::
     C.Decl NameAnon
  -> (Maybe BindingSpec.CTypeSpec, Maybe BindingSpec.HsTypeSpec)
  -> M (C.Decl ResolveBindingSpecs)
resolveDeep decl@C.Decl{..} mTypeSpecs = do
    declKind' <- resolve (C.declName decl) declKind
    return C.Decl {
        declInfo = coercePass declInfo
      , declKind = declKind'
      , declAnn  = mTypeSpecs
      }

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

class Resolve a where
  resolve ::
       C.DeclName -- context declaration
    -> a NameAnon
    -> M (a ResolveBindingSpecs)

instance Resolve C.DeclKind where
  resolve ctx = \case
      C.DeclStruct struct    -> C.DeclStruct   <$> resolve ctx struct
      C.DeclUnion union      -> C.DeclUnion    <$> resolve ctx union
      C.DeclTypedef typedef  -> C.DeclTypedef  <$> resolve ctx typedef
      C.DeclEnum enum        -> C.DeclEnum     <$> resolve ctx enum
      C.DeclOpaque cNameKind -> return (C.DeclOpaque cNameKind)
      C.DeclMacro macro      -> C.DeclMacro    <$> resolve ctx macro
      C.DeclFunction fun     -> C.DeclFunction <$> resolve ctx fun
      C.DeclGlobal ty        -> C.DeclGlobal   <$> resolve ctx ty

instance Resolve C.Struct where
  resolve ctx C.Struct{..} = reassemble <$> mapM (resolve ctx) structFields
    where
      reassemble ::
           [C.StructField ResolveBindingSpecs]
        -> C.Struct ResolveBindingSpecs
      reassemble structFields' = C.Struct {
          structFields = structFields'
        , ..
        }

instance Resolve C.StructField where
  resolve ctx C.StructField{..} = reassemble <$> resolve ctx structFieldType
    where
      C.FieldInfo{..} = structFieldInfo
      reassemble ::
           C.Type ResolveBindingSpecs
        -> C.StructField ResolveBindingSpecs
      reassemble structFieldType' = C.StructField {
          structFieldInfo =
            C.FieldInfo {
              fieldComment = fmap resolveCommentReference fieldComment
            , ..
            }
        , structFieldType = structFieldType'
        , ..
        }

instance Resolve C.Union where
  resolve ctx C.Union{..} = reassemble <$> mapM (resolve ctx) unionFields
    where
      reassemble ::
           [C.UnionField ResolveBindingSpecs]
        -> C.Union ResolveBindingSpecs
      reassemble unionFields' = C.Union {
          unionFields = unionFields'
        , ..
        }

instance Resolve C.UnionField where
  resolve ctx C.UnionField{..} = reassemble <$> resolve ctx unionFieldType
    where
      C.FieldInfo{..} = unionFieldInfo
      reassemble :: C.Type ResolveBindingSpecs
                 -> C.UnionField ResolveBindingSpecs
      reassemble unionFieldType' = C.UnionField {
          unionFieldInfo =
            C.FieldInfo {
              fieldComment = fmap resolveCommentReference fieldComment
            , ..
            }
        , unionFieldType = unionFieldType'
        , ..
        }

instance Resolve C.Enum where
  resolve ctx C.Enum{..} = reassemble <$> resolve ctx enumType
    where
      reassemble :: C.Type ResolveBindingSpecs -> C.Enum ResolveBindingSpecs
      reassemble enumType' = C.Enum {
          enumType      = enumType'
        , enumConstants = map coercePass enumConstants
        , ..
        }

instance Resolve C.Typedef where
  resolve ctx C.Typedef{..} = reassemble <$> resolve ctx typedefType
    where
      reassemble :: C.Type ResolveBindingSpecs -> C.Typedef ResolveBindingSpecs
      reassemble typedefType' = C.Typedef {
            typedefType = typedefType'
          , ..
          }

instance Resolve C.Function where
  resolve ctx C.Function{..} =
    reassemble
      <$> mapM (\(mbName, ty) -> (mbName,) <$> resolve ctx ty) functionArgs
      <*> resolve ctx functionRes
    where
      reassemble ::
           [(ArgumentName ResolveBindingSpecs, C.Type ResolveBindingSpecs)]
        -> C.Type ResolveBindingSpecs
        -> C.Function ResolveBindingSpecs
      reassemble functionArgs' functionRes' = C.Function {
          functionArgs = functionArgs'
        , functionRes  = functionRes'
        , ..
        }

instance Resolve C.CheckedMacro where
  resolve ctx = \case
    C.MacroType typ  -> C.MacroType <$> resolve ctx typ
    C.MacroExpr expr -> return (C.MacroExpr expr)

instance Resolve C.CheckedMacroType where
  resolve ctx C.CheckedMacroType{..} = reassemble <$> resolve ctx macroType
    where
      reassemble ::
           C.Type ResolveBindingSpecs
        -> C.CheckedMacroType ResolveBindingSpecs
      reassemble macroType' = C.CheckedMacroType {
          macroType = macroType'
        , ..
        }

instance Resolve C.Type where
  resolve ctx = \case
      C.TypeRef uid -> do
        mResolved <- aux uid
        case mResolved of
          Just r  -> return r
          Nothing -> return $ C.TypeRef (coercePass uid)
      C.TypeTypedef uid uTy -> do
        mResolved <- aux uid
        case mResolved of
          Just r  -> return r
          Nothing -> C.TypeTypedef (coercePass uid) <$> resolve ctx uTy

      -- Recursive cases
      C.TypePointer t         -> C.TypePointer <$> resolve ctx t
      C.TypeFun args res      ->
        C.TypeFun <$> mapM (resolve ctx) args <*> resolve ctx res
      C.TypeConstArray n t    -> C.TypeConstArray n <$> resolve ctx t
      C.TypeIncompleteArray t -> C.TypeIncompleteArray <$> resolve ctx t
      C.TypeBlock t           -> C.TypeBlock <$> resolve ctx t
      C.TypeConst t           -> C.TypeConst <$> resolve ctx t

      -- Simple cases
      C.TypePrim t         -> return (C.TypePrim t)
      C.TypeVoid           -> return (C.TypeVoid)
      C.TypeExtBinding ext -> absurd ext
      C.TypeComplex t      -> return (C.TypeComplex t)
    where
      aux :: C.DeclId NameAnon -> M (Maybe (C.Type ResolveBindingSpecs))
      aux cDeclId =
         case cDeclId.origDeclId of
           C.OrigDeclId orig ->
             auxOrig cDeclId orig
           C.AuxForDecl _parent ->
             -- TODO <https://github.com/well-typed/hs-bindgen/issues/1379>
             -- Auxiliary declarations are not introduced until @HandleTypedefs@
             panicPure "Unexpected aux decl"

      auxOrig ::
           C.DeclId NameAnon
        -> C.PrelimDeclId
        -> M (Maybe (C.Type ResolveBindingSpecs))
      auxOrig cDeclId cOrigDeclId =
        Reader.ask >>= \MEnv{..} -> State.get >>= \MState{..} -> do
          -- Check for selected external binding
          case Map.lookup cOrigDeclId stateExtTypes of
            Just ty -> do
              State.modify' $
                insertTrace (ResolveBindingSpecsExtType ctx cDeclName)
              return $ Just ty
            Nothing -> do
              -- Check for external binding of type that is unusable.
              case DeclIndex.lookupUnusableLoc cOrigDeclId envDeclIndex of
                []   -> return Nothing
                locs -> do
                  let declPaths =
                        foldMap
                          (IncludeGraph.reaches envIncludeGraph . singleLocPath)
                          locs
                  mTy <- fmap C.TypeExtBinding <$>
                    resolveExtBinding cDeclName cOrigDeclId declPaths Nothing
                  case mTy of
                    Just ty -> do
                      State.modify' $
                          insertTrace (ResolveBindingSpecsExtType ctx cDeclName)
                        . insertExtType cOrigDeclId ty
                      return (Just ty)
                    Nothing -> return Nothing
        where
          cDeclName = cDeclId.name

{-------------------------------------------------------------------------------
  Internal: auxiliary functions
-------------------------------------------------------------------------------}

resolveCommentReference ::
     C.Comment NameAnon
  -> C.Comment ResolveBindingSpecs
resolveCommentReference (C.Comment comment) =
    C.Comment (fmap coercePass comment)

-- | Lookup qualified name in the 'ExternalResolvedBindingSpec'
resolveExtBinding ::
     C.DeclName
  -> C.PrelimDeclId
  -> Set SourcePath
     -- | Message to emit for omitted types.
  -> Maybe ResolveBindingSpecsMsg
  -> M (Maybe ResolvedExtBinding)
resolveExtBinding cDeclName cQualPrelimDeclId declPaths mMsg = do
    MEnv{envExtSpecs} <- Reader.ask
    case BindingSpec.lookupMergedBindingSpecs cDeclName declPaths envExtSpecs of
      Just (hsModuleName, BindingSpec.Require cTypeSpec, mHsTypeSpec) ->
        case BindingSpec.cTypeSpecIdentifier cTypeSpec of
          Just hsIdentifier -> do
            let resolved = ResolvedExtBinding {
                    extCName  = cDeclName
                  , extHsRef  = Hs.ExtRef hsModuleName hsIdentifier
                  , extCSpec  = cTypeSpec
                  , extHsSpec = mHsTypeSpec
                  }
            State.modify' $
              insertExtType
                cQualPrelimDeclId
                (C.TypeExtBinding resolved)
            return (Just resolved)
          Nothing -> do
            State.modify' $
              insertTrace (ResolveBindingSpecsExtHsRefNoIdentifier cDeclName)
            return Nothing
      Just (_hsModuleName, BindingSpec.Omit, _mHsTypeSpec) -> do
        forM_ mMsg $ \msg -> State.modify' $ insertTrace msg
        return Nothing
      Nothing ->
        return Nothing
