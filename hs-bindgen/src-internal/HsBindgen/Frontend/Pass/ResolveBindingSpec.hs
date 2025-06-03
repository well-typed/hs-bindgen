module HsBindgen.Frontend.Pass.ResolveBindingSpec (
    module HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass
  , resolveBindingSpec
  , BindingSpecError(..)
  ) where

import Control.Exception (Exception(..))
import Control.Monad.State
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
import HsBindgen.Frontend.Pass.RenameAnon
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
    let (decls, ctx) = runM confSpec $
          resolveDecls confSpec extSpec unitIncludeGraph unitDecls
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
      , unitAnn
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

newtype M a = WrapM {
      unwrapM :: State Ctx a
    }
  deriving newtype (Applicative, Functor, Monad, MonadState Ctx)

runM :: ResolvedBindingSpec -> M a -> (a, Ctx)
runM confSpec = flip runState (initCtx confSpec) . unwrapM

{-------------------------------------------------------------------------------
  Internal: state context
-------------------------------------------------------------------------------}

data Ctx = Ctx {
      ctxErrors       :: [BindingSpecError] -- ^ Stored in reverse order
    , ctxExtTypes     :: Map (C.QualId RenameAnon) (C.Type ResolveBindingSpec)
    , ctxNoConfTypes  :: Set CNameSpelling
    , ctxOmittedTypes :: Set (C.QualId RenameAnon)
    }

initCtx :: ResolvedBindingSpec -> Ctx
initCtx confSpec = Ctx {
      ctxErrors       = []
    , ctxExtTypes     = Map.empty
    , ctxNoConfTypes  = Map.keysSet $ BindingSpec.bindingSpecTypes confSpec
    , ctxOmittedTypes = Set.empty
    }

insertError :: BindingSpecError -> Ctx -> Ctx
insertError e ctx = ctx {
      ctxErrors = e : ctxErrors ctx
    }

insertExtType :: C.QualId RenameAnon -> C.Type ResolveBindingSpec -> Ctx -> Ctx
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
     ResolvedBindingSpec -- ^ Configuration binding specification
  -> ResolvedBindingSpec -- ^ External binding specification
  -> IncludeGraph
  -> [C.Decl RenameAnon]
  -> M [C.Decl ResolveBindingSpec]
resolveDecls confSpec extSpec includeGraph = mapMaybeM aux
  where
    aux :: C.Decl RenameAnon -> M (Maybe (C.Decl ResolveBindingSpec))
    aux decl = auxExt
      where
        qualId :: C.QualId RenameAnon
        qualId = C.declQualId decl

        cname :: CNameSpelling
        cname = qualIdCNameSpelling qualId

        declPaths :: Set SourcePath
        declPaths = IncludeGraph.reaches includeGraph $
          singleLocPath (C.declLoc (C.declInfo decl))

        auxExt :: M (Maybe (C.Decl ResolveBindingSpec))
        auxExt = case BindingSpec.lookupTypeSpec cname declPaths extSpec of
          Just (BindingSpec.Require typeSpec) ->
            case getExtHsRef cname typeSpec of
              Right extHsRef -> do
                let t = C.TypeExtBinding extHsRef typeSpec
                modify' $ insertExtType qualId t
                return Nothing
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
            Just <$> mkDecl decl (Just typeSpec)
          Just BindingSpec.Omit -> do
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

mkDecl ::
     C.Decl RenameAnon
  -> Maybe BindingSpec.TypeSpec
  -> M (C.Decl ResolveBindingSpec)
mkDecl C.Decl{..} mTypeSpec =
    reassemble <$> mkDeclKind declKind
  where
    reassemble :: C.DeclKind ResolveBindingSpec -> C.Decl ResolveBindingSpec
    reassemble declKind' = C.Decl {
        declInfo = mkDeclInfo declInfo
      , declKind = declKind'
      , declAnn  = fromMaybe BindingSpec.defaultTypeSpec mTypeSpec
      }

mkDeclInfo :: C.DeclInfo RenameAnon -> C.DeclInfo ResolveBindingSpec
mkDeclInfo C.DeclInfo{..} = C.DeclInfo{..}

mkDeclKind :: C.DeclKind RenameAnon -> M (C.DeclKind ResolveBindingSpec)
mkDeclKind = \case
    C.DeclStruct struct   -> C.DeclStruct <$> mkStruct struct
    C.DeclStructOpaque    -> return C.DeclStructOpaque
    C.DeclUnion union     -> C.DeclUnion <$> mkUnion union
    C.DeclTypedef typedef -> C.DeclTypedef <$> mkTypedef typedef
    C.DeclEnum enum       -> C.DeclEnum <$> mkEnum enum
    C.DeclEnumOpaque      -> return C.DeclEnumOpaque
    C.DeclMacro macro     -> C.DeclMacro <$> mkMacro macro
    C.DeclFunction fun    -> C.DeclFunction <$> mkFunction fun

mkStruct :: C.Struct RenameAnon -> M (C.Struct ResolveBindingSpec)
mkStruct C.Struct{..} =
    reassemble <$> mapM mkStructField structFields
  where
    reassemble ::
         [C.StructField ResolveBindingSpec]
      -> C.Struct ResolveBindingSpec
    reassemble structFields' = C.Struct {
          structFields = structFields'
        , ..
        }

mkStructField ::
     C.StructField RenameAnon
  -> M (C.StructField ResolveBindingSpec)
mkStructField C.StructField{..} =
    reassemble <$> mkType structFieldType
  where
    reassemble ::
         C.Type ResolveBindingSpec
      -> C.StructField ResolveBindingSpec
    reassemble structFieldType' = C.StructField {
          structFieldType = structFieldType'
        , ..
        }

mkUnion :: C.Union RenameAnon -> M (C.Union ResolveBindingSpec)
mkUnion C.Union{..} =
    reassemble <$> mapM mkUnionField unionFields
  where
    reassemble ::
         [C.UnionField ResolveBindingSpec]
      -> C.Union ResolveBindingSpec
    reassemble unionFields' = C.Union {
          unionFields = unionFields'
        , ..
        }

mkUnionField :: C.UnionField RenameAnon -> M (C.UnionField ResolveBindingSpec)
mkUnionField C.UnionField{..} =
    reassemble <$> mkType unionFieldType
  where
    reassemble :: C.Type ResolveBindingSpec -> C.UnionField ResolveBindingSpec
    reassemble unionFieldType' = C.UnionField {
          unionFieldType = unionFieldType'
        , ..
        }

mkEnum :: C.Enum RenameAnon -> M (C.Enum ResolveBindingSpec)
mkEnum C.Enum{..} =
    reassemble
      <$> mkType enumType
      <*> mapM mkEnumConstant enumConstants
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

mkEnumConstant ::
     C.EnumConstant RenameAnon
  -> M (C.EnumConstant ResolveBindingSpec)
mkEnumConstant C.EnumConstant{..} = return C.EnumConstant{..}

mkTypedef :: C.Typedef RenameAnon -> M (C.Typedef ResolveBindingSpec)
mkTypedef C.Typedef{..} =
    reassemble <$> mkType typedefType
  where
    reassemble :: C.Type ResolveBindingSpec -> C.Typedef ResolveBindingSpec
    reassemble typedefType' = C.Typedef {
          typedefType = typedefType'
        , ..
        }

mkFunction :: C.Function RenameAnon -> M (C.Function ResolveBindingSpec)
mkFunction C.Function{..} = do
    functionArgs' <- mapM mkType functionArgs
    functionRes'  <- mkType functionRes
    return C.Function {
        functionArgs = functionArgs'
      , functionRes  = functionRes'
      , ..
      }

mkMacro :: C.CheckedMacro RenameAnon -> M (C.CheckedMacro ResolveBindingSpec)
mkMacro (C.MacroType typ)  = C.MacroType <$> mkMacroType typ
mkMacro (C.MacroExpr expr) = return $ C.MacroExpr expr

mkMacroType ::
     C.CheckedMacroType RenameAnon
  -> M (C.CheckedMacroType ResolveBindingSpec)
mkMacroType C.CheckedMacroType{..} = do
    macroType' <- mkType macroType
    return C.CheckedMacroType{
        macroType = macroType'
      , ..
      }

mkType :: C.Type RenameAnon -> M (C.Type ResolveBindingSpec)
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
         (Id ResolveBindingSpec -> C.Type ResolveBindingSpec)
      -> Id RenameAnon
      -> C.Namespace
      -> M (C.Type ResolveBindingSpec)
    aux mk uid namespace = do
      let qualId = C.QualId uid namespace
          cname  = qualIdCNameSpelling qualId
      isOmitted <- gets $ Set.member qualId . ctxOmittedTypes
      when isOmitted . modify' $ insertError (BindingSpecOmittedTypeUse cname)
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
