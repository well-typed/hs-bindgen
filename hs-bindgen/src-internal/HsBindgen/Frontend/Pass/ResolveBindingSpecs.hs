module HsBindgen.Frontend.Pass.ResolveBindingSpecs (
    module HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
  , resolveBindingSpecs
  , BindingSpecsError(..)
  ) where

import Control.Exception (Exception(..))
import Control.Monad.State
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.CNameSpelling
import Clang.HighLevel.Types
import Clang.Paths
import HsBindgen.BindingSpecs (ResolvedBindingSpecs)
import HsBindgen.BindingSpecs qualified as BindingSpecs
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Graph.Includes (IncludeGraph)
import HsBindgen.Frontend.Graph.Includes qualified as IncludeGraph
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.RenameAnon
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C (CName(..))
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

resolveBindingSpecs ::
     ResolvedBindingSpecs -- ^ Configuration binding specifications
  -> ResolvedBindingSpecs -- ^ External binding specifications
  -> C.TranslationUnit RenameAnon
  -> (C.TranslationUnit ResolveBindingSpecs, [BindingSpecsError])
resolveBindingSpecs
  confSpecs
  extSpecs
  C.TranslationUnit{unitDecls, unitIncludeGraph, unitAnn} =
    let (decls, ctx) = runM confSpecs $
          resolveDecls confSpecs extSpecs unitIncludeGraph unitDecls
        notUsedErrs =
          BindingSpecsTypeNotUsed <$> Set.toAscList (ctxNoConfTypes ctx)
    in  (reassemble decls, reverse (ctxErrors ctx) ++ notUsedErrs)
  where
    reassemble ::
         [C.Decl ResolveBindingSpecs]
      -> C.TranslationUnit ResolveBindingSpecs
    reassemble decls' = C.TranslationUnit{
        unitDecls = decls'
      , unitIncludeGraph
      , unitAnn
      }

data BindingSpecsError =
    BindingSpecsExtHsRefNoModule CNameSpelling
  | BindingSpecsExtHsRefNoIdentifier CNameSpelling
  | BindingSpecsOmittedTypeUse CNameSpelling
  | BindingSpecsTypeNotUsed CNameSpelling
  deriving stock (Show)

instance Exception BindingSpecsError where
  toException = hsBindgenExceptionToException
  fromException = hsBindgenExceptionFromException
  displayException = \case
    BindingSpecsExtHsRefNoModule cname ->
      "no Haskell module specified in " ++ show cname
        ++ " external binding specification"
    BindingSpecsExtHsRefNoIdentifier cname ->
      "no Haskell identifier specified in " ++ show cname
        ++ " external binding specification"
    BindingSpecsOmittedTypeUse cname ->
      "omitted type " ++ show cname ++ " used"
    BindingSpecsTypeNotUsed cname ->
      "binding specifications for type " ++ show cname ++ " not used"

{-------------------------------------------------------------------------------
  Internal: monad
-------------------------------------------------------------------------------}

newtype M a = WrapM {
      unwrapM :: State Ctx a
    }
  deriving newtype (Applicative, Functor, Monad, MonadState Ctx)

runM :: ResolvedBindingSpecs -> M a -> (a, Ctx)
runM confSpecs = flip runState (initCtx confSpecs) . unwrapM

{-------------------------------------------------------------------------------
  Internal: state context
-------------------------------------------------------------------------------}

data Ctx = Ctx {
      ctxErrors       :: [BindingSpecsError] -- ^ Stored in reverse order
    , ctxExtTypes     :: Map (C.QualId RenameAnon) (C.Type ResolveBindingSpecs)
    , ctxNoConfTypes  :: Set CNameSpelling
    , ctxOmittedTypes :: Set (C.QualId RenameAnon)
    }

initCtx :: ResolvedBindingSpecs -> Ctx
initCtx confSpecs = Ctx {
      ctxErrors       = []
    , ctxExtTypes     = Map.empty
    , ctxNoConfTypes  = Map.keysSet $ BindingSpecs.bindingSpecsTypes confSpecs
    , ctxOmittedTypes = Set.empty
    }

insertError :: BindingSpecsError -> Ctx -> Ctx
insertError e ctx = ctx {
      ctxErrors = e : ctxErrors ctx
    }

insertExtType :: C.QualId RenameAnon -> C.Type ResolveBindingSpecs -> Ctx -> Ctx
insertExtType qualId typ ctx = ctx {
      ctxExtTypes = Map.insert qualId typ (ctxExtTypes ctx)
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
     ResolvedBindingSpecs -- ^ Configuration binding specifications
  -> ResolvedBindingSpecs -- ^ External binding specifications
  -> IncludeGraph
  -> [C.Decl RenameAnon]
  -> M [C.Decl ResolveBindingSpecs]
resolveDecls confSpecs extSpecs includeGraph = mapMaybeM aux
  where
    aux :: C.Decl RenameAnon -> M (Maybe (C.Decl ResolveBindingSpecs))
    aux decl = auxExt
      where
        qualId :: C.QualId RenameAnon
        qualId = C.declQualId decl

        cname :: CNameSpelling
        cname = qualIdCNameSpelling qualId

        declPaths :: Set SourcePath
        declPaths = IncludeGraph.reaches includeGraph $
          singleLocPath (C.declLoc (C.declInfo decl))

        auxExt :: M (Maybe (C.Decl ResolveBindingSpecs))
        auxExt = case BindingSpecs.lookupType cname declPaths extSpecs of
          Just (BindingSpecs.Require typeSpec) ->
            case getExtHsRef cname typeSpec of
              Right extHsRef -> do
                let t = C.TypeExtBinding extHsRef typeSpec
                modify' $ insertExtType qualId t
                return Nothing
              Left e -> do
                modify' $ insertError e
                auxConf
          Just BindingSpecs.Omit -> do
            modify' $ insertError (BindingSpecsOmittedTypeUse cname)
            auxConf
          Nothing -> auxConf

        auxConf :: M (Maybe (C.Decl ResolveBindingSpecs))
        auxConf = case BindingSpecs.lookupType cname declPaths confSpecs of
          Just (BindingSpecs.Require typeSpec) -> do
            modify' $ deleteNoConfType cname
            Just <$> mkDecl decl (Just typeSpec)
          Just BindingSpecs.Omit -> do
            modify' $ deleteNoConfType cname . insertOmittedType qualId
            return Nothing
          Nothing -> Just <$> mkDecl decl Nothing

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
  -> BindingSpecs.Type
  -> Either BindingSpecsError ExtHsRef
getExtHsRef cname typ = do
    extHsRefModule <-
      maybe (Left (BindingSpecsExtHsRefNoModule cname)) Right $
        BindingSpecs.typeModule typ
    extHsRefIdentifier <-
      maybe (Left (BindingSpecsExtHsRefNoIdentifier cname)) Right $
        BindingSpecs.typeIdentifier typ
    return ExtHsRef{extHsRefModule, extHsRefIdentifier}

mkDecl ::
     C.Decl RenameAnon
  -> Maybe BindingSpecs.Type
  -> M (C.Decl ResolveBindingSpecs)
mkDecl C.Decl{..} mTypeSpec =
    reassemble <$> mkDeclKind declKind
  where
    reassemble :: C.DeclKind ResolveBindingSpecs -> C.Decl ResolveBindingSpecs
    reassemble declKind' = C.Decl {
        declInfo = mkDeclInfo declInfo
      , declKind = declKind'
      , declAnn  = fromMaybe BindingSpecs.defaultTypeSpec mTypeSpec
      }

mkDeclInfo :: C.DeclInfo RenameAnon -> C.DeclInfo ResolveBindingSpecs
mkDeclInfo C.DeclInfo{..} = C.DeclInfo{..}

mkDeclKind :: C.DeclKind RenameAnon -> M (C.DeclKind ResolveBindingSpecs)
mkDeclKind = \case
    C.DeclStruct struct   -> C.DeclStruct <$> mkStruct struct
    C.DeclStructOpaque    -> return C.DeclStructOpaque
    C.DeclUnion union     -> C.DeclUnion <$> mkUnion union
    C.DeclTypedef typedef -> C.DeclTypedef <$> mkTypedef typedef
    C.DeclEnum enum       -> C.DeclEnum <$> mkEnum enum
    C.DeclEnumOpaque      -> return C.DeclEnumOpaque
    C.DeclMacro macro     -> C.DeclMacro <$> mkMacro macro
    C.DeclFunction fun    -> C.DeclFunction <$> mkFunction fun

mkStruct :: C.Struct RenameAnon -> M (C.Struct ResolveBindingSpecs)
mkStruct C.Struct{..} =
    reassemble <$> mapM mkStructField structFields
  where
    reassemble ::
         [C.StructField ResolveBindingSpecs]
      -> C.Struct ResolveBindingSpecs
    reassemble structFields' = C.Struct {
          structFields = structFields'
        , ..
        }

mkStructField ::
     C.StructField RenameAnon
  -> M (C.StructField ResolveBindingSpecs)
mkStructField C.StructField{..} =
    reassemble <$> mkType structFieldType
  where
    reassemble ::
         C.Type ResolveBindingSpecs
      -> C.StructField ResolveBindingSpecs
    reassemble structFieldType' = C.StructField {
          structFieldType = structFieldType'
        , ..
        }

mkUnion :: C.Union RenameAnon -> M (C.Union ResolveBindingSpecs)
mkUnion C.Union{..} =
    reassemble <$> mapM mkUnionField unionFields
  where
    reassemble ::
         [C.UnionField ResolveBindingSpecs]
      -> C.Union ResolveBindingSpecs
    reassemble unionFields' = C.Union {
          unionFields = unionFields'
        , ..
        }

mkUnionField :: C.UnionField RenameAnon -> M (C.UnionField ResolveBindingSpecs)
mkUnionField C.UnionField{..} =
    reassemble <$> mkType unionFieldType
  where
    reassemble :: C.Type ResolveBindingSpecs -> C.UnionField ResolveBindingSpecs
    reassemble unionFieldType' = C.UnionField {
          unionFieldType = unionFieldType'
        , ..
        }

mkEnum :: C.Enum RenameAnon -> M (C.Enum ResolveBindingSpecs)
mkEnum C.Enum{..} =
    reassemble
      <$> mkType enumType
      <*> mapM mkEnumConstant enumConstants
  where
   reassemble ::
        C.Type ResolveBindingSpecs
     -> [C.EnumConstant ResolveBindingSpecs]
     -> C.Enum ResolveBindingSpecs
   reassemble enumType' enumConstants' = C.Enum{
         enumType      = enumType'
       , enumConstants = enumConstants'
       , ..
       }

mkEnumConstant ::
     C.EnumConstant RenameAnon
  -> M (C.EnumConstant ResolveBindingSpecs)
mkEnumConstant C.EnumConstant{..} = return C.EnumConstant{..}

mkTypedef :: C.Typedef RenameAnon -> M (C.Typedef ResolveBindingSpecs)
mkTypedef C.Typedef{..} =
    reassemble <$> mkType typedefType
  where
    reassemble :: C.Type ResolveBindingSpecs -> C.Typedef ResolveBindingSpecs
    reassemble typedefType' = C.Typedef {
          typedefType = typedefType'
        , ..
        }

mkFunction :: C.Function RenameAnon -> M (C.Function ResolveBindingSpecs)
mkFunction C.Function{..} = do
    functionArgs' <- mapM mkType functionArgs
    functionRes'  <- mkType functionRes
    return C.Function {
        functionArgs = functionArgs'
      , functionRes  = functionRes'
      , ..
      }

mkMacro :: C.CheckedMacro RenameAnon -> M (C.CheckedMacro ResolveBindingSpecs)
mkMacro (C.MacroType typ)  = C.MacroType <$> mkMacroType typ
mkMacro (C.MacroExpr expr) = return $ C.MacroExpr expr

mkMacroType ::
     C.CheckedMacroType RenameAnon
  -> M (C.CheckedMacroType ResolveBindingSpecs)
mkMacroType C.CheckedMacroType{..} = do
    macroType' <- mkType macroType
    return C.CheckedMacroType{
        macroType = macroType'
      , ..
      }

mkType :: C.Type RenameAnon -> M (C.Type ResolveBindingSpecs)
mkType = \case
    C.TypePrim t            -> return (C.TypePrim t)
    C.TypeStruct uid        -> aux C.TypeStruct uid C.NamespaceStruct
    C.TypeUnion uid         -> aux C.TypeUnion uid C.NamespaceUnion
    C.TypeEnum uid          -> aux C.TypeEnum uid C.NamespaceEnum
    C.TypeTypedef uid ann   -> aux (`C.TypeTypedef` ann) uid C.NamespaceTypedef
    C.TypePointer t         -> C.TypePointer <$> mkType t
    C.TypeFun args res      -> C.TypeFun <$> mapM mkType args <*> mkType res
    C.TypeVoid              -> return C.TypeVoid
    C.TypeConstArray n t    -> C.TypeConstArray n <$> mkType t
    C.TypeIncompleteArray t -> C.TypeIncompleteArray <$> mkType t

    C.TypeExtBinding extHsRef typeSpec ->
      return (C.TypeExtBinding extHsRef typeSpec)
  where
    aux ::
         (Id ResolveBindingSpecs -> C.Type ResolveBindingSpecs)
      -> Id RenameAnon
      -> C.Namespace
      -> M (C.Type ResolveBindingSpecs)
    aux mk uid namespace = do
      let qualId = C.QualId uid namespace
          cname  = qualIdCNameSpelling qualId
      isOmitted <- gets $ Set.member qualId . ctxOmittedTypes
      when isOmitted . modify' $ insertError (BindingSpecsOmittedTypeUse cname)
      gets $ fromMaybe (mk uid) . Map.lookup qualId . ctxExtTypes

{-------------------------------------------------------------------------------
  Internal: auxiliary functions
-------------------------------------------------------------------------------}

mapMaybeM :: forall a b m. Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = foldr aux (pure [])
  where
    aux :: a -> m [b] -> m [b]
    aux x doRest = f x >>= \case
      Just y  -> (y :) <$> doRest
      Nothing -> doRest
