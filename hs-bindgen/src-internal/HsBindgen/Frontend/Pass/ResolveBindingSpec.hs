module HsBindgen.Frontend.Pass.ResolveBindingSpec (
    resolveBindingSpec
  , BindingSpecError(..)
  ) where

import Control.Exception (Exception(..))
import Control.Monad ((<=<))
import Control.Monad.RWS (MonadReader, MonadState, RWS)
import Control.Monad.RWS qualified as RWS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.CNameSpelling
import Clang.HighLevel.Types
import HsBindgen.BindingSpec (ResolvedBindingSpec)
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Graph.Includes (IncludeGraph)
import HsBindgen.Frontend.Graph.Includes qualified as IncludeGraph
import HsBindgen.Frontend.NonSelectedDecls (NonSelectedDecls)
import HsBindgen.Frontend.NonSelectedDecls qualified as NonSelectedDecls
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.RenameAnon.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C (CName(..))
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

resolveBindingSpec ::
     ResolvedBindingSpec -- ^ Configuration binding specification
  -> ResolvedBindingSpec -- ^ External binding specification
  -> C.TranslationUnit RenameAnon
  -> (C.TranslationUnit ResolveBindingSpec, [BindingSpecError])
resolveBindingSpec
  confSpec
  extSpec
  C.TranslationUnit{unitDecls, unitIncludeGraph, unitAnn} =
    let (decls, MState{..}) =
          runM confSpec extSpec unitIncludeGraph (snd unitAnn) $
            resolveDecls unitDecls
        notUsedErrs = BindingSpecTypeNotUsed <$> Set.toAscList stateNoConfTypes
    in  (reassemble decls, reverse stateErrors ++ notUsedErrs)
  where
    reassemble ::
         [C.Decl ResolveBindingSpec]
      -> C.TranslationUnit ResolveBindingSpec
    reassemble decls' = C.TranslationUnit{
        unitDecls = decls'
      , unitIncludeGraph
      , unitAnn = fst unitAnn
      }

data BindingSpecError =
    BindingSpecExtHsRefNoModule CNameSpelling
  | BindingSpecExtHsRefNoIdentifier CNameSpelling
  | BindingSpecOmittedTypeUse CNameSpelling
  | BindingSpecTypeNotUsed CNameSpelling
  deriving stock (Show)

instance Exception BindingSpecError where
  toException = hsBindgenExceptionToException
  fromException = hsBindgenExceptionFromException
  displayException = \case
    BindingSpecExtHsRefNoModule cname ->
      "no Haskell module specified in " ++ show cname
        ++ " external binding specification"
    BindingSpecExtHsRefNoIdentifier cname ->
      "no Haskell identifier specified in " ++ show cname
        ++ " external binding specification"
    BindingSpecOmittedTypeUse cname ->
      "omitted type " ++ show cname ++ " used"
    BindingSpecTypeNotUsed cname ->
      "binding specification for type " ++ show cname ++ " not used"

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
     ResolvedBindingSpec -- ^ Configuration binding specification
  -> ResolvedBindingSpec -- ^ External binding specification
  -> IncludeGraph
  -> NonSelectedDecls
  -> M a
  -> (a, MState)
runM confSpec extSpec includeGraph nonSelectedDecls (WrapM m) =
    let env        = MEnv confSpec extSpec includeGraph nonSelectedDecls
        state0     = initMState confSpec
        (x, s, ()) = RWS.runRWS m env state0
    in  (x, s)

{-------------------------------------------------------------------------------
  Internal: monad reader
-------------------------------------------------------------------------------}

data MEnv = MEnv {
      envConfSpec         :: ResolvedBindingSpec
    , envExtSpec          :: ResolvedBindingSpec
    , envIncludeGraph     :: IncludeGraph
    , envNonSelectedDecls :: NonSelectedDecls
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Internal: monad state
-------------------------------------------------------------------------------}

data MState = MState {
      stateErrors      :: [BindingSpecError] -- ^ Stored in reverse order
    , stateExtTypes    :: Map (C.QualId RenameAnon) (C.Type ResolveBindingSpec)
    , stateNoConfTypes :: Set CNameSpelling
    , stateOmitTypes   :: Set (C.QualId RenameAnon)
    }
  deriving (Show)

initMState :: ResolvedBindingSpec -> MState
initMState confSpec = MState {
      stateErrors      = []
    , stateExtTypes    = Map.empty
    , stateNoConfTypes = Map.keysSet $ BindingSpec.bindingSpecTypes confSpec
    , stateOmitTypes   = Set.empty
    }

insertError :: BindingSpecError -> MState -> MState
insertError e st = st {
      stateErrors = e : stateErrors st
    }

insertExtType ::
     C.QualId RenameAnon
  -> C.Type ResolveBindingSpec
  -> MState
  -> MState
insertExtType qualId typ st = st {
      stateExtTypes = Map.insert qualId typ (stateExtTypes st)
    }

deleteNoConfType :: CNameSpelling -> MState -> MState
deleteNoConfType cname st = st {
      stateNoConfTypes = Set.delete cname (stateNoConfTypes st)
    }

insertOmittedType :: C.QualId RenameAnon -> MState -> MState
insertOmittedType qualId st = st {
      stateOmitTypes = Set.insert qualId (stateOmitTypes st)
    }

{-------------------------------------------------------------------------------
  Internal: implementation
-------------------------------------------------------------------------------}

-- Resolve declarations, in two passes
resolveDecls :: [C.Decl RenameAnon] -> M [C.Decl ResolveBindingSpec]
resolveDecls = mapM (uncurry resolveDeep) <=< mapMaybeM resolveTop

-- Pass one: top-level
--
-- If a declaration has an external binding, then the declaration is dropped and
-- the external binding is recorded.
--
-- If a declaration is omitted, then the declaration is dropped and the omission
-- is recorded.
--
-- Otherwise, the declaration is kept and is associated with an type
-- specification when applicable.
resolveTop ::
     C.Decl RenameAnon
  -> M (Maybe (C.Decl RenameAnon, Maybe BindingSpec.TypeSpec))
resolveTop decl = RWS.ask >>= \MEnv{..} -> do
    let qualId     = C.declQualId decl
        cname      = qualIdCNameSpelling qualId
        sourcePath = singleLocPath $ C.declLoc (C.declInfo decl)
        declPaths  = IncludeGraph.reaches envIncludeGraph sourcePath
    isExt <- case BindingSpec.lookupTypeSpec cname declPaths envExtSpec of
      Just (BindingSpec.Require typeSpec) ->
        case getExtHsRef cname typeSpec of
          Right extHsRef -> do
            let ty = C.TypeExtBinding cname extHsRef typeSpec
            RWS.modify' $ insertExtType qualId ty
            return True
          Left e -> do
            RWS.modify' $ insertError e
            return False
      Just BindingSpec.Omit -> do
        RWS.modify' $ insertError (BindingSpecOmittedTypeUse cname)
        return False
      Nothing -> return False
    if isExt
      then return Nothing
      else case BindingSpec.lookupTypeSpec cname declPaths envConfSpec of
        Just (BindingSpec.Require typeSpec) -> do
          RWS.modify' $ deleteNoConfType cname
          return $ Just (decl, Just typeSpec)
        Just BindingSpec.Omit -> do
          RWS.modify' $ deleteNoConfType cname . insertOmittedType qualId
          return Nothing
        Nothing -> return $ Just (decl, Nothing)

-- Pass two: deep
--
-- Types within the declaration are resolved, and it is reassembled for the
-- current pass.
resolveDeep ::
     C.Decl RenameAnon
  -> Maybe BindingSpec.TypeSpec
  -> M (C.Decl ResolveBindingSpec)
resolveDeep C.Decl{..} mTypeSpec =
    reassemble <$> resolve declKind
  where
    reassemble :: C.DeclKind ResolveBindingSpec -> C.Decl ResolveBindingSpec
    reassemble declKind' = C.Decl {
        declInfo = mkDeclInfo declInfo
      , declKind = declKind'
      , declAnn  = fromMaybe BindingSpec.defaultTypeSpec mTypeSpec
      }

    mkDeclInfo :: C.DeclInfo RenameAnon -> C.DeclInfo ResolveBindingSpec
    mkDeclInfo C.DeclInfo{..} = C.DeclInfo{..}

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

class Resolve a where
  resolve :: a RenameAnon -> M (a ResolveBindingSpec)

instance Resolve C.DeclKind where
  resolve = \case
      C.DeclStruct struct   -> C.DeclStruct <$> resolve struct
      C.DeclStructOpaque    -> return C.DeclStructOpaque
      C.DeclUnion union     -> C.DeclUnion <$> resolve union
      C.DeclUnionOpaque     -> return C.DeclUnionOpaque
      C.DeclTypedef typedef -> C.DeclTypedef <$> resolve typedef
      C.DeclEnum enum       -> C.DeclEnum <$> resolve enum
      C.DeclEnumOpaque      -> return C.DeclEnumOpaque
      C.DeclMacro macro     -> C.DeclMacro <$> resolve macro
      C.DeclFunction fun    -> C.DeclFunction <$> resolve fun

instance Resolve C.Struct where
  resolve C.Struct{..} =
      reassemble <$> mapM resolve structFields
    where
      reassemble ::
           [C.StructField ResolveBindingSpec]
        -> C.Struct ResolveBindingSpec
      reassemble structFields' = C.Struct {
            structFields = structFields'
          , ..
          }

instance Resolve C.StructField where
  resolve C.StructField{..} =
      reassemble <$> resolve structFieldType
    where
      reassemble ::
           C.Type ResolveBindingSpec
        -> C.StructField ResolveBindingSpec
      reassemble structFieldType' = C.StructField {
            structFieldType = structFieldType'
          , ..
          }

instance Resolve C.Union where
  resolve C.Union{..} =
      reassemble <$> mapM resolve unionFields
    where
      reassemble ::
           [C.UnionField ResolveBindingSpec]
        -> C.Union ResolveBindingSpec
      reassemble unionFields' = C.Union {
            unionFields = unionFields'
          , ..
          }

instance Resolve C.UnionField where
  resolve C.UnionField{..} =
      reassemble <$> resolve unionFieldType
    where
      reassemble :: C.Type ResolveBindingSpec -> C.UnionField ResolveBindingSpec
      reassemble unionFieldType' = C.UnionField {
            unionFieldType = unionFieldType'
          , ..
          }

instance Resolve C.Enum where
  resolve C.Enum{..} =
      reassemble
        <$> resolve enumType
        <*> mapM resolve enumConstants
    where
     reassemble ::
          C.Type ResolveBindingSpec
       -> [C.EnumConstant ResolveBindingSpec]
       -> C.Enum ResolveBindingSpec
     reassemble enumType' enumConstants' = C.Enum{
           enumType      = enumType'
         , enumConstants = enumConstants'
         , ..
         }

instance Resolve C.EnumConstant where
  resolve C.EnumConstant{..} = return C.EnumConstant{..}

instance Resolve C.Typedef where
  resolve C.Typedef{..} =
      reassemble <$> resolve typedefType
    where
      reassemble :: C.Type ResolveBindingSpec -> C.Typedef ResolveBindingSpec
      reassemble typedefType' = C.Typedef {
            typedefType = typedefType'
          , ..
          }

instance Resolve C.Function where
  resolve C.Function{..} = do
      functionArgs' <- mapM resolve functionArgs
      functionRes'  <- resolve functionRes
      return C.Function {
          functionArgs = functionArgs'
        , functionRes  = functionRes'
        , ..
        }

instance Resolve C.CheckedMacro where
  resolve (C.MacroType typ)  = C.MacroType <$> resolve typ
  resolve (C.MacroExpr expr) = return $ C.MacroExpr expr

instance Resolve C.CheckedMacroType where
  resolve C.CheckedMacroType{..} = do
      macroType' <- resolve macroType
      return C.CheckedMacroType{
          macroType = macroType'
        , ..
        }

instance Resolve C.Type where
  resolve = \case
      C.TypePrim t            -> return (C.TypePrim t)
      C.TypeStruct uid        -> aux C.TypeStruct uid C.NamespaceStruct
      C.TypeUnion uid         -> aux C.TypeUnion uid C.NamespaceUnion
      C.TypeEnum uid          -> aux C.TypeEnum uid C.NamespaceEnum
      C.TypeMacroTypedef uid  -> aux C.TypeMacroTypedef uid C.NamespaceMacro
      C.TypePointer t         -> C.TypePointer <$> resolve t
      C.TypeFun args res      -> C.TypeFun <$> mapM resolve args <*> resolve res
      C.TypeVoid              -> return C.TypeVoid
      C.TypeConstArray n t    -> C.TypeConstArray n <$> resolve t
      C.TypeIncompleteArray t -> C.TypeIncompleteArray <$> resolve t

      C.TypeTypedef typedef ->
        case typedef of
          TypedefRegular uid ->
            aux (C.TypeTypedef . TypedefRegular) uid C.NamespaceTypedef
          TypedefSquashed cname ty ->
            C.TypeTypedef . TypedefSquashed cname <$> resolve ty

      C.TypeExtBinding cSpelling extHsRef typeSpec ->
        return (C.TypeExtBinding cSpelling extHsRef typeSpec)
    where
      aux ::
           (Id ResolveBindingSpec -> C.Type ResolveBindingSpec)
        -> Id RenameAnon
        -> C.Namespace
        -> M (C.Type ResolveBindingSpec)
      aux mk uid namespace =
        RWS.ask >>= \MEnv{..} -> RWS.get >>= \MState{..} -> do
          let qualId = C.QualId uid namespace
              cname  = qualIdCNameSpelling qualId
          -- check for type omitted by binding specification
          when (Set.member qualId stateOmitTypes) $
            RWS.modify' $ insertError (BindingSpecOmittedTypeUse cname)
          -- check for selected external binding
          case Map.lookup qualId stateExtTypes of
            Just ty -> return ty
            Nothing -> do
              -- check for external binding of non-selected type
              let k = (uid, namespace)
              case NonSelectedDecls.lookup k envNonSelectedDecls of
                Nothing -> return (mk uid)
                Just sourcePath -> do
                  let declPaths =
                        IncludeGraph.reaches envIncludeGraph sourcePath
                  case BindingSpec.lookupTypeSpec cname declPaths envExtSpec of
                    Just (BindingSpec.Require typeSpec) ->
                      case getExtHsRef cname typeSpec of
                        Right extHsRef -> do
                          let ty = C.TypeExtBinding cname extHsRef typeSpec
                          RWS.modify' $ insertExtType qualId ty
                          return ty
                        Left e -> do
                          RWS.modify' $ insertError e
                          return (mk uid)
                    Just BindingSpec.Omit -> do
                      RWS.modify' $
                        insertError (BindingSpecOmittedTypeUse cname)
                      return (mk uid)
                    Nothing -> return (mk uid)

{-------------------------------------------------------------------------------
  Internal: auxiliary functions
-------------------------------------------------------------------------------}

qualIdCNameSpelling :: C.QualId RenameAnon -> CNameSpelling
qualIdCNameSpelling (C.QualId (CName cname) namespace) =
    let prefix = case namespace of
          C.NamespaceTypedef  -> ""
          C.NamespaceStruct   -> "struct "
          C.NamespaceUnion    -> "union "
          C.NamespaceEnum     -> "enum "
          C.NamespaceMacro    -> ""
          C.NamespaceFunction -> ""
    in  CNameSpelling $ prefix <> cname

getExtHsRef ::
     CNameSpelling
  -> BindingSpec.TypeSpec
  -> Either BindingSpecError ExtHsRef
getExtHsRef cname typeSpec = do
    extHsRefModule <-
      maybe (Left (BindingSpecExtHsRefNoModule cname)) Right $
        BindingSpec.typeSpecModule typeSpec
    extHsRefIdentifier <-
      maybe (Left (BindingSpecExtHsRefNoIdentifier cname)) Right $
        BindingSpec.typeSpecIdentifier typeSpec
    return ExtHsRef{extHsRefModule, extHsRefIdentifier}

mapMaybeM :: forall a b m. Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = foldr aux (pure [])
  where
    aux :: a -> m [b] -> m [b]
    aux x doRest = f x >>= \case
      Just y  -> (y :) <$> doRest
      Nothing -> doRest
