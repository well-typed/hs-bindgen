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
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex (..))
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
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Imports
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
      let index :: DeclIndex
          index = unitAnn.declIndex

          omitted :: Map C.QualPrelimDeclId (C.QualName, SourcePath)
          omitted = stateOmitTypes

          external :: Set C.QualPrelimDeclId
          external = Map.keysSet stateExtTypes

          omittedIds, externalIds, handledIds :: Set C.QualPrelimDeclId
          omittedIds = Map.keysSet omitted
          externalIds = Map.keysSet stateExtTypes
          handledIds = omittedIds `Set.union` externalIds

          index' :: DeclIndex
          index' = DeclIndex {
              succeeded    = Map.withoutKeys index.succeeded    handledIds
            , notAttempted = Map.withoutKeys index.notAttempted handledIds
            , failed       = Map.withoutKeys index.failed       handledIds
            -- TODO https://github.com/well-typed/hs-bindgen/issues/1280: We do
            -- not support external binding specifications for macros yet, but
            -- if we do, we need to handle those here.
            , failedMacros = index.failedMacros
            , omitted
            , external
            }

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
    , stateExtTypes  :: Map C.QualPrelimDeclId (C.Type ResolveBindingSpecs)
    , stateNoPTypes  :: Map C.QualName [Set SourcePath]
    , stateOmitTypes :: Map C.QualPrelimDeclId (C.QualName, SourcePath)
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
     C.QualPrelimDeclId
  -> C.Type ResolveBindingSpecs
  -> MState
  -> MState
insertExtType cQualPrelimDeclId typ st = st {
      stateExtTypes =
        Map.insert cQualPrelimDeclId typ (stateExtTypes st)
    }

deleteNoPType :: C.QualName -> SourcePath -> MState -> MState
deleteNoPType cQualName path st = st {
      stateNoPTypes = Map.update (aux []) cQualName (stateNoPTypes st)
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

insertOmittedType :: C.QualPrelimDeclId -> C.QualName -> SourcePath -> MState -> MState
insertOmittedType cQualPrelimDeclId cQualName path st = st {
      stateOmitTypes = Map.insert cQualPrelimDeclId (cQualName, path) (stateOmitTypes st)
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
resolveTop decl = Reader.ask >>= \MEnv{..} -> do
    let cQualName  = C.declQualName decl
        cQualPrelimDeclId = C.declOrigQualPrelimDeclId decl
        sourcePath = singleLocPath $ C.declLoc (C.declInfo decl)
        declPaths  = IncludeGraph.reaches envIncludeGraph sourcePath
    isExt <- isJust <$> resolveExtBinding cQualName cQualPrelimDeclId declPaths
    if isExt
      then do
        State.modify' $ insertTrace (ResolveBindingSpecsExtDecl cQualName)
        return Nothing
      else case BindingSpec.lookupCTypeSpec cQualName declPaths envPSpec of
        Just (_hsModuleName, BindingSpec.Require cTypeSpec) -> do
          State.modify' $
              insertTrace (ResolveBindingSpecsPrescriptiveRequire cQualName)
            . deleteNoPType cQualName sourcePath
          let mHsTypeSpec = do
                hsIdentifier <- BindingSpec.cTypeSpecIdentifier cTypeSpec
                BindingSpec.lookupHsTypeSpec hsIdentifier envPSpec
          return $ Just (decl, (Just cTypeSpec, mHsTypeSpec))
        Just (_hsModuleName, BindingSpec.Omit) -> do
          State.modify' $
              insertTrace (ResolveBindingSpecsPrescriptiveOmit cQualName)
            . deleteNoPType cQualName sourcePath
            . insertOmittedType cQualPrelimDeclId cQualName sourcePath
          return Nothing
        Nothing -> return $ Just (decl, (Nothing, Nothing))

-- Pass two: deep
--
-- Types within the declaration are resolved, and it is reassembled for the
-- current pass.
resolveDeep ::
     C.Decl NameAnon
  -> (Maybe BindingSpec.CTypeSpec, Maybe BindingSpec.HsTypeSpec)
  -> M (C.Decl ResolveBindingSpecs)
resolveDeep decl@C.Decl{..} mTypeSpecs = do
    declKind' <- resolve (C.declQualName decl) declKind
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
       C.QualName -- context declaration
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
      C.TypeStruct       uid ->
        auxU C.TypeStruct       uid (C.NameKindTagged C.TagKindStruct)
      C.TypeUnion        uid ->
        auxU C.TypeUnion        uid (C.NameKindTagged C.TagKindUnion)
      C.TypeEnum         uid ->
        auxU C.TypeEnum         uid (C.NameKindTagged C.TagKindEnum)
      C.TypeMacroTypedef uid ->
        auxU C.TypeMacroTypedef uid C.NameKindOrdinary
      C.TypeTypedef (OrigTypedefRef nm uTy) -> do
        uTy' <- resolve ctx uTy
        let mkTypeTypedef = C.TypeTypedef . flip OrigTypedefRef uTy'
        auxN mkTypeTypedef nm C.NameKindOrdinary

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
      auxU ::
           (Id ResolveBindingSpecs -> C.Type ResolveBindingSpecs)
        -> Id NameAnon
        -> C.NameKind
        -> M (C.Type ResolveBindingSpecs)
      auxU mk uid = aux (const (mk uid')) . C.declIdToQualDeclId uid
        where
          uid' :: Id ResolveBindingSpecs
          uid' = coercePassId (Proxy @'(NameAnon, ResolveBindingSpecs)) uid

      auxN ::
           (C.Name -> C.Type ResolveBindingSpecs)
        -> C.Name
        -> C.NameKind
        -> M (C.Type ResolveBindingSpecs)
      auxN mk cName cNameKind = aux mk C.QualDeclId {
          qualDeclId     = C.DeclIdNamed cName C.NameOriginInSource
        , qualDeclIdKind = cNameKind
        }

      aux ::
           (C.Name -> C.Type ResolveBindingSpecs)
        -> C.QualDeclId NameAnon
        -> M (C.Type ResolveBindingSpecs)
      aux mk cQualDeclId@C.QualDeclId{..} =
        Reader.ask >>= \MEnv{..} -> State.get >>= \MState{..} -> do
          let cQualName = C.QualName qualDeclIdName qualDeclIdKind
              cQualPrelimDeclId = C.qualDeclIdToQualPrelimDeclId cQualDeclId
          -- Check for type omitted by binding specification
          when (Map.member cQualPrelimDeclId stateOmitTypes) $
            State.modify' $
              insertTrace (ResolveBindingSpecsOmittedTypeUse cQualName)
          -- Check for selected external binding
          case Map.lookup cQualPrelimDeclId stateExtTypes of
            Just ty -> do
              State.modify' $
                insertTrace (ResolveBindingSpecsExtType ctx cQualName)
              return ty
            Nothing -> do
              -- Check for external binding of type that we omitted or failed to
              -- parse.
              case lookupMissing cQualPrelimDeclId envDeclIndex of
                [] -> return (mk qualDeclIdName)
                locs -> do
                  let declPaths =
                        foldMap
                          (IncludeGraph.reaches envIncludeGraph . singleLocPath)
                          locs
                  mTy <- fmap C.TypeExtBinding <$>
                    resolveExtBinding cQualName cQualPrelimDeclId declPaths
                  case mTy of
                    Just ty -> do
                      State.modify' $
                          insertTrace (ResolveBindingSpecsExtType ctx cQualName)
                        . insertExtType cQualPrelimDeclId ty
                      return ty
                    Nothing -> return (mk qualDeclIdName)
        where
          qualDeclIdName = C.qualDeclIdName cQualDeclId

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
     C.QualName
  -> C.QualPrelimDeclId
  -> Set SourcePath
  -> M (Maybe ResolvedExtBinding)
resolveExtBinding cQualName cQualPrelimDeclId declPaths  = do
    MEnv{envExtSpecs} <- Reader.ask
    case BindingSpec.lookupMergedBindingSpecs cQualName declPaths envExtSpecs of
      Just (hsModuleName, BindingSpec.Require cTypeSpec, mHsTypeSpec) ->
        case BindingSpec.cTypeSpecIdentifier cTypeSpec of
          Just hsIdentifier -> do
            let resolved = ResolvedExtBinding {
                    extCName  = cQualName
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
              insertTrace (ResolveBindingSpecsExtHsRefNoIdentifier cQualName)
            return Nothing
      Just (_hsModuleName, BindingSpec.Omit, _mHsTypeSpec) -> do
        State.modify' $
          insertTrace (ResolveBindingSpecsOmittedTypeUse cQualName)
        return Nothing
      Nothing ->
        return Nothing

-- For a given declaration ID, look up the source locations of "not attempted"
-- or "failed" parses.
lookupMissing :: C.QualPrelimDeclId -> DeclIndex -> [SingleLoc]
lookupMissing qualPrelimDeclId index = catMaybes $ [
    loc . unParseNotAttempted <$> Map.lookup qualPrelimDeclId index.notAttempted
  , loc . unParseFailure      <$> Map.lookup qualPrelimDeclId index.failed
  ]
