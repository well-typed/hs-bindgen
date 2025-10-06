module HsBindgen.Frontend.Pass.ResolveBindingSpecs (
    resolveBindingSpecs
  ) where

import Control.Monad ((<=<))
import Control.Monad.RWS (MonadReader, MonadState, RWS)
import Control.Monad.RWS qualified as RWS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.BindingSpec (ExternalBindingSpec, PrescriptiveBindingSpec)
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.NonParsedDecls (NonParsedDecls)
import HsBindgen.Frontend.NonParsedDecls qualified as NonParsedDecls
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.NameAnon.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.Sort.IsPass
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Monad (mapMaybeM)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

resolveBindingSpecs ::
     ExternalBindingSpec
  -> PrescriptiveBindingSpec
  -> C.TranslationUnit NameAnon
  -> (C.TranslationUnit ResolveBindingSpecs, [Msg ResolveBindingSpecs])
resolveBindingSpecs
  extSpec
  pSpec
  C.TranslationUnit{unitDecls, unitIncludeGraph, unitAnn} =
    let (decls, MState{..}) =
          runM
            extSpec
            pSpec
            unitIncludeGraph
            (declUseDecl unitAnn)
            (declNonParsed unitAnn)
            (resolveDecls unitDecls)
        notUsedErrs = ResolveBindingSpecsTypeNotUsed <$> Map.keys stateNoPTypes
    in  (reassemble decls stateUseDecl, reverse stateErrors ++ notUsedErrs)
  where
    reassemble ::
         [C.Decl ResolveBindingSpecs]
      -> UseDeclGraph
      -> C.TranslationUnit ResolveBindingSpecs
    reassemble decls' useDeclGraph = C.TranslationUnit{
        unitDecls = decls'
      , unitIncludeGraph
      , unitAnn = coerceDeclMeta $ unitAnn { declUseDecl = useDeclGraph }
      }

{-------------------------------------------------------------------------------
  Internal: monad
-------------------------------------------------------------------------------}

newtype M a = WrapM (RWS MEnv () MState a)
  deriving newtype (
      Applicative
    , Functor
    , Monad
    , MonadReader MEnv
    , MonadState MState
    )

runM ::
     ExternalBindingSpec
  -> PrescriptiveBindingSpec
  -> IncludeGraph
  -> UseDeclGraph
  -> NonParsedDecls
  -> M a
  -> (a, MState)
runM extSpec pSpec includeGraph useDeclGraph nonParsedDecls (WrapM m) =
    let env        = MEnv extSpec pSpec includeGraph nonParsedDecls
        state0     = initMState pSpec useDeclGraph
        (x, s, ()) = RWS.runRWS m env state0
    in  (x, s)

{-------------------------------------------------------------------------------
  Internal: monad reader
-------------------------------------------------------------------------------}

data MEnv = MEnv {
      envExtSpec        :: ExternalBindingSpec
    , envPSpec          :: PrescriptiveBindingSpec
    , envIncludeGraph   :: IncludeGraph
    , envNonParsedDecls :: NonParsedDecls
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Internal: monad state
-------------------------------------------------------------------------------}

data MState = MState {
      stateErrors    :: [Msg ResolveBindingSpecs] -- ^ Stored in reverse order
    , stateExtTypes  :: Map C.QualName (C.Type ResolveBindingSpecs)
    , stateNoPTypes  :: Map C.QualName [Set SourcePath]
    , stateOmitTypes :: Set C.QualName
    , stateUseDecl   :: UseDeclGraph
    }
  deriving (Show)

initMState :: PrescriptiveBindingSpec -> UseDeclGraph -> MState
initMState pSpec useDeclGraph = MState {
      stateErrors    = []
    , stateExtTypes  = Map.empty
    , stateNoPTypes  = BindingSpec.getTypes pSpec
    , stateOmitTypes = Set.empty
    , stateUseDecl   = useDeclGraph
    }

insertError :: Msg ResolveBindingSpecs -> MState -> MState
insertError e st = st {
      stateErrors = e : stateErrors st
    }

insertExtType ::
     C.QualName
  -> C.Type ResolveBindingSpecs
  -> MState
  -> MState
insertExtType cQualName typ st = st {
      stateExtTypes = Map.insert cQualName typ (stateExtTypes st)
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

insertOmittedType :: C.QualName -> MState -> MState
insertOmittedType cQualName st = st {
      stateOmitTypes = Set.insert cQualName (stateOmitTypes st)
    }

deleteDeps :: C.NsPrelimDeclId -> [C.NsPrelimDeclId] -> MState -> MState
deleteDeps declId depIds st = st {
      stateUseDecl = UseDeclGraph.deleteDeps declId depIds (stateUseDecl st)
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
  -> M (Maybe (C.Decl NameAnon, Maybe BindingSpec.TypeSpec))
resolveTop decl = RWS.ask >>= \MEnv{..} -> do
    let cQualName  = C.declQualName decl
        sourcePath = singleLocPath $ C.declLoc (C.declInfo decl)
        declPaths  = IncludeGraph.reaches envIncludeGraph sourcePath
    isExt <- isJust <$> resolveExtBinding cQualName declPaths
    if isExt
      then return Nothing
      else case BindingSpec.lookupTypeSpec cQualName declPaths envPSpec of
        Just (BindingSpec.Require typeSpec) -> do
          RWS.modify' $ deleteNoPType cQualName sourcePath
          return $ Just (decl, Just typeSpec)
        Just BindingSpec.Omit -> do
          RWS.modify' $
              deleteNoPType cQualName sourcePath
            . insertOmittedType cQualName
          return Nothing
        Nothing -> return $ Just (decl, Nothing)

-- Pass two: deep
--
-- Types within the declaration are resolved, and it is reassembled for the
-- current pass.
resolveDeep ::
     C.Decl NameAnon
  -> Maybe BindingSpec.TypeSpec
  -> M (C.Decl ResolveBindingSpecs)
resolveDeep decl@C.Decl{..} mTypeSpec = do
    (depIds, decl') <- fmap reassemble <$> resolve declKind
    unless (Set.null depIds) . RWS.modify' $
      deleteDeps (C.declOrigNsPrelimDeclId decl) (Set.toList depIds)
    return decl'
  where
    reassemble :: C.DeclKind ResolveBindingSpecs -> C.Decl ResolveBindingSpecs
    reassemble declKind' = C.Decl {
        declInfo = coercePass declInfo
      , declKind = declKind'
      , declAnn  = fromMaybe def mTypeSpec
      }

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

class Resolve a where
  resolve :: a NameAnon -> M (Set C.NsPrelimDeclId, a ResolveBindingSpecs)

instance Resolve C.DeclKind where
  resolve = \case
      C.DeclStruct struct    -> fmap C.DeclStruct   <$> resolve struct
      C.DeclUnion union      -> fmap C.DeclUnion    <$> resolve union
      C.DeclTypedef typedef  -> fmap C.DeclTypedef  <$> resolve typedef
      C.DeclEnum enum        -> fmap C.DeclEnum     <$> resolve enum
      C.DeclOpaque cNameKind -> return (Set.empty, C.DeclOpaque cNameKind)
      C.DeclMacro macro      -> fmap C.DeclMacro    <$> resolve macro
      C.DeclFunction fun     -> fmap C.DeclFunction <$> resolve fun
      C.DeclGlobal ty        -> fmap C.DeclGlobal   <$> resolve ty

instance Resolve C.Struct where
  resolve C.Struct{..} =
      bimap Set.unions reassemble . unzip <$> mapM resolve structFields
    where
      reassemble ::
           [C.StructField ResolveBindingSpecs]
        -> C.Struct ResolveBindingSpecs
      reassemble structFields' = C.Struct {
          structFields = structFields'
        , ..
        }

instance Resolve C.StructField where
  resolve C.StructField{..} = do
      fmap reassemble <$> resolve structFieldType
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
  resolve C.Union{..} =
      bimap Set.unions reassemble . unzip <$> mapM resolve unionFields
    where
      reassemble ::
           [C.UnionField ResolveBindingSpecs]
        -> C.Union ResolveBindingSpecs
      reassemble unionFields' = C.Union {
          unionFields = unionFields'
        , ..
        }

instance Resolve C.UnionField where
  resolve C.UnionField{..} = do
      fmap reassemble <$> resolve unionFieldType
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
  resolve C.Enum{..} =
      fmap reassemble <$> resolve enumType
    where
      reassemble :: C.Type ResolveBindingSpecs -> C.Enum ResolveBindingSpecs
      reassemble enumType' = C.Enum {
          enumType      = enumType'
        , enumConstants = map coercePass enumConstants
        , ..
        }

instance Resolve C.Typedef where
  resolve C.Typedef{..} =
      fmap reassemble <$> resolve typedefType
    where
      reassemble :: C.Type ResolveBindingSpecs -> C.Typedef ResolveBindingSpecs
      reassemble typedefType' = C.Typedef {
            typedefType = typedefType'
          , ..
          }

instance Resolve C.Function where
  resolve C.Function{..} = do
      (argsDepIds, functionArgs') <-
        first Set.unions . unzip
          <$> mapM (\(mbName, ty) -> do
                      (x, y) <- resolve ty
                      pure (x, (mbName, y))
                   )
                   functionArgs
      (resDepIds, functionRes') <- resolve functionRes
      return
        (Set.union argsDepIds resDepIds, reassemble functionArgs' functionRes')
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
  resolve = \case
    C.MacroType typ  -> fmap C.MacroType <$> resolve typ
    C.MacroExpr expr -> return (Set.empty, C.MacroExpr expr)

instance Resolve C.CheckedMacroType where
  resolve C.CheckedMacroType{..} =
      fmap reassemble <$> resolve macroType
    where
      reassemble ::
           C.Type ResolveBindingSpecs
        -> C.CheckedMacroType ResolveBindingSpecs
      reassemble macroType' = C.CheckedMacroType {
          macroType = macroType'
        , ..
        }

instance Resolve C.Type where
  resolve = \case
      C.TypeStruct       uid ->
        auxU C.TypeStruct       uid (C.NameKindTagged C.TagKindStruct)
      C.TypeUnion        uid ->
        auxU C.TypeUnion        uid (C.NameKindTagged C.TagKindUnion)
      C.TypeEnum         uid ->
        auxU C.TypeEnum         uid (C.NameKindTagged C.TagKindEnum)
      C.TypeMacroTypedef uid ->
        auxU C.TypeMacroTypedef uid C.NameKindOrdinary
      C.TypeTypedef      nm  ->
        auxN C.TypeTypedef      nm  C.NameKindOrdinary

      -- Recursive cases
      C.TypePointer t         -> fmap C.TypePointer <$> resolve t
      C.TypeFun args res      -> do
        (argsDepIds, args') <- first Set.unions . unzip <$> mapM resolve args
        (resDepIds,  res')  <- resolve res
        return (Set.union argsDepIds resDepIds, C.TypeFun args' res')
      C.TypeConstArray n t    -> fmap (C.TypeConstArray n) <$> resolve t
      C.TypeIncompleteArray t -> fmap C.TypeIncompleteArray <$> resolve t
      C.TypeBlock t           -> fmap C.TypeBlock <$> resolve t
      C.TypeConst t           -> fmap C.TypeConst <$> resolve t

      -- Simple cases
      C.TypePrim t         -> return (Set.empty, C.TypePrim t)
      C.TypeVoid           -> return (Set.empty, C.TypeVoid)
      C.TypeExtBinding ext -> absurd ext
      C.TypeComplex t      -> return (Set.empty, C.TypeComplex t)
    where
      auxU ::
           (Id ResolveBindingSpecs -> C.Type ResolveBindingSpecs)
        -> Id NameAnon
        -> C.NameKind
        -> M (Set C.NsPrelimDeclId, C.Type ResolveBindingSpecs)
      auxU mk uid = aux (const (mk uid)) . C.qualDeclId uid

      auxN ::
           (C.Name -> C.Type ResolveBindingSpecs)
        -> C.Name
        -> C.NameKind
        -> M (Set C.NsPrelimDeclId, C.Type ResolveBindingSpecs)
      auxN mk cName cNameKind = aux mk C.QualDeclId {
          qualDeclIdName   = cName
        , qualDeclIdOrigin = C.NameOriginInSource
        , qualDeclIdKind   = cNameKind
        }

      aux ::
           (C.Name -> C.Type ResolveBindingSpecs)
        -> C.QualDeclId
        -> M (Set C.NsPrelimDeclId, C.Type ResolveBindingSpecs)
      aux mk cQualDeclId@C.QualDeclId{..} =
        RWS.ask >>= \MEnv{..} -> RWS.get >>= \MState{..} -> do
          let cQualName = C.QualName qualDeclIdName qualDeclIdKind
              nsid = C.qualDeclIdNsPrelimDeclId cQualDeclId
          -- check for type omitted by binding specification
          when (Set.member cQualName stateOmitTypes) $
            RWS.modify' $
              insertError (ResolveBindingSpecsOmittedTypeUse cQualName)
          -- check for selected external binding
          case Map.lookup cQualName stateExtTypes of
            Just ty -> return (Set.singleton nsid, ty)
            Nothing -> do
              -- check for external binding of non-selected type
              case NonParsedDecls.lookup cQualName envNonParsedDecls of
                Nothing -> return (Set.empty, mk qualDeclIdName)
                Just sourcePath -> do
                  let declPaths =
                        IncludeGraph.reaches envIncludeGraph sourcePath
                  resolveExtBinding cQualName declPaths >>= \case
                    Just resolved -> do
                      let ty = C.TypeExtBinding resolved
                      RWS.modify' $ insertExtType cQualName ty
                      return (Set.singleton nsid, ty)
                    Nothing -> return (Set.empty, mk qualDeclIdName)

{-------------------------------------------------------------------------------
  Internal: auxiliary functions
-------------------------------------------------------------------------------}

resolveCommentReference :: C.Comment NameAnon
                        -> C.Comment ResolveBindingSpecs
resolveCommentReference (C.Comment comment) =
  C.Comment (fmap (\(C.ById i) -> C.ById i) comment)

-- | Lookup qualified name in the 'ExternalResolvedBindingSpec'
resolveExtBinding ::
     C.QualName
  -> Set SourcePath
  -> M (Maybe ResolvedExtBinding)
resolveExtBinding cQualName declPaths  = do
    MEnv{envExtSpec} <- RWS.ask
    case BindingSpec.lookupTypeSpec cQualName declPaths envExtSpec of
      Just (BindingSpec.Require typeSpec) ->
        case getHsExtRef cQualName typeSpec of
          Right ref -> do
            let resolved = ResolvedExtBinding {
                    extCName  = cQualName
                  , extHsRef  = ref
                  , extHsSpec = typeSpec
                  }
            RWS.modify' $ insertExtType cQualName (C.TypeExtBinding resolved)
            return (Just resolved)
          Left e -> do
            RWS.modify' $ insertError e
            return Nothing
      Just BindingSpec.Omit -> do
        RWS.modify' $
          insertError (ResolveBindingSpecsOmittedTypeUse cQualName)
        return Nothing
      Nothing ->
        return Nothing

getHsExtRef ::
     C.QualName
  -> BindingSpec.TypeSpec
  -> Either (Msg ResolveBindingSpecs) Hs.ExtRef
getHsExtRef cQualName typeSpec = do
    extRefModule <-
      maybe (Left (ResolveBindingSpecsExtHsRefNoModule cQualName)) Right $
        BindingSpec.typeSpecModule typeSpec
    extRefIdentifier <-
      maybe (Left (ResolveBindingSpecsExtHsRefNoIdentifier cQualName)) Right $
        BindingSpec.typeSpecIdentifier typeSpec
    return Hs.ExtRef{extRefModule, extRefIdentifier}
