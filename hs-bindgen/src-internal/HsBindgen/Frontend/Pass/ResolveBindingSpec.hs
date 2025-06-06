module HsBindgen.Frontend.Pass.ResolveBindingSpec (
    resolveBindingSpec
  , BindingSpecError(..)
  ) where

import Control.Exception (Exception(..))
import Control.Monad.RWS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.CNameSpelling
import Clang.HighLevel.Types
import Clang.Paths
import HsBindgen.BindingSpec (ResolvedBindingSpec)
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Graph.Includes (IncludeGraph)
import HsBindgen.Frontend.Graph.Includes qualified as IncludeGraph
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.RenameAnon.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass
import HsBindgen.Frontend.SourceMap (SourceMap(..))
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
    let extMap = mkExtMap extSpec unitIncludeGraph (snd unitAnn)
        (decls, ctx) = runM confSpec extMap $
          resolveDecls confSpec unitIncludeGraph unitDecls
        notUsedErrs =
          BindingSpecTypeNotUsed <$> Set.toAscList (ctxNoConfTypes ctx)
    in  (reassemble decls, reverse (ctxErrors ctx) ++ notUsedErrs)
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

newtype M a = WrapM (RWS ExtMap () Ctx a)
  deriving newtype (Applicative, Functor, Monad, MonadReader ExtMap, MonadState Ctx)

runM :: ResolvedBindingSpec -> ExtMap -> M a -> (a, Ctx)
runM confSpec extMap (WrapM m) =
    let (x, s, ()) = runRWS m extMap (initCtx confSpec)
    in  (x, s)

{-------------------------------------------------------------------------------
  Internal: external binding map
-------------------------------------------------------------------------------}

type ExtMap =
  Map (C.QualId RenameAnon) (BindingSpec.Omittable BindingSpec.TypeSpec)

mkExtMap ::
     ResolvedBindingSpec -- ^ External binding specification
  -> IncludeGraph
  -> SourceMap RenameAnon
  -> Map (C.QualId RenameAnon) (BindingSpec.Omittable BindingSpec.TypeSpec)
mkExtMap extSpec includeGraph (SourceMap sourceMap) =
    flip Map.mapMaybeWithKey sourceMap $ \qualId sourcePath ->
      let cname     = qualIdCNameSpelling qualId
          declPaths = IncludeGraph.reaches includeGraph sourcePath
      in  BindingSpec.lookupTypeSpec cname declPaths extSpec

{-------------------------------------------------------------------------------
  Internal: state context
-------------------------------------------------------------------------------}

data Ctx = Ctx {
      ctxErrors       :: [BindingSpecError] -- ^ Stored in reverse order
    , ctxNoConfTypes  :: Set CNameSpelling
    , ctxOmittedTypes :: Set (C.QualId RenameAnon)
    }
  deriving (Show)

initCtx :: ResolvedBindingSpec -> Ctx
initCtx confSpec = Ctx {
      ctxErrors       = []
    , ctxNoConfTypes  = Map.keysSet $ BindingSpec.bindingSpecTypes confSpec
    , ctxOmittedTypes = Set.empty
    }

insertError :: BindingSpecError -> Ctx -> Ctx
insertError e ctx = ctx {
      ctxErrors = e : ctxErrors ctx
    }

deleteNoConfType :: CNameSpelling -> Ctx -> Ctx
deleteNoConfType cname ctx = ctx {
      ctxNoConfTypes = Set.delete cname (ctxNoConfTypes ctx)
    }

insertOmittedType :: C.QualId RenameAnon -> Ctx -> Ctx
insertOmittedType qualId ctx = ctx {
      ctxOmittedTypes = Set.insert qualId (ctxOmittedTypes ctx)
    }

{-------------------------------------------------------------------------------
  Internal: implementation
-------------------------------------------------------------------------------}

resolveDecls ::
     ResolvedBindingSpec -- ^ Configuration binding specification
  -> IncludeGraph
  -> [C.Decl RenameAnon]
  -> M [C.Decl ResolveBindingSpec]
resolveDecls confSpec includeGraph = mapMaybeM aux
  where
    aux :: C.Decl RenameAnon -> M (Maybe (C.Decl ResolveBindingSpec))
    aux decl = auxExt =<< ask
      where
        qualId :: C.QualId RenameAnon
        qualId = C.declQualId decl

        cname :: CNameSpelling
        cname = qualIdCNameSpelling qualId

        declPaths :: Set SourcePath
        declPaths = IncludeGraph.reaches includeGraph $
          singleLocPath (C.declLoc (C.declInfo decl))

        auxExt :: ExtMap -> M (Maybe (C.Decl ResolveBindingSpec))
        auxExt extMap = case Map.lookup qualId extMap of
          Just (BindingSpec.Require typeSpec) ->
            case getExtHsRef cname typeSpec of
              Right _extHsRef -> return Nothing
              Left e -> do
                modify' $ insertError e
                auxConf
          Just BindingSpec.Omit -> do
            modify' $ insertError (BindingSpecOmittedTypeUse cname)
            auxConf
          Nothing -> auxConf

        auxConf :: M (Maybe (C.Decl ResolveBindingSpec))
        auxConf = case BindingSpec.lookupTypeSpec cname declPaths confSpec of
          Just (BindingSpec.Require typeSpec) -> do
            modify' $ deleteNoConfType cname
            Just <$> resolveDecl decl (Just typeSpec)
          Just BindingSpec.Omit -> do
            modify' $ deleteNoConfType cname . insertOmittedType qualId
            return Nothing
          Nothing -> Just <$> resolveDecl decl Nothing

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

resolveDecl ::
     C.Decl RenameAnon
  -> Maybe BindingSpec.TypeSpec
  -> M (C.Decl ResolveBindingSpec)
resolveDecl C.Decl{..} mTypeSpec =
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
      aux mk uid namespace = do
          let qualId = C.QualId uid namespace
              cname  = qualIdCNameSpelling qualId
          isOmitted <- gets $ Set.member qualId . ctxOmittedTypes
          when isOmitted $
            modify' $ insertError (BindingSpecOmittedTypeUse cname)
          extMap <- ask
          case Map.lookup qualId extMap of
            Just (BindingSpec.Require typeSpec) ->
              case getExtHsRef cname typeSpec of
                Right extHsRef ->
                  return $ C.TypeExtBinding cname extHsRef typeSpec
                Left e -> do
                  modify' $ insertError e
                  return $ mk uid
            Just BindingSpec.Omit -> do
              modify' $ insertError (BindingSpecOmittedTypeUse cname)
              return $ mk uid
            Nothing ->
              return $ mk uid

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

mapMaybeM :: forall a b m. Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = foldr aux (pure [])
  where
    aux :: a -> m [b] -> m [b]
    aux x doRest = f x >>= \case
      Just y  -> (y :) <$> doRest
      Nothing -> doRest
