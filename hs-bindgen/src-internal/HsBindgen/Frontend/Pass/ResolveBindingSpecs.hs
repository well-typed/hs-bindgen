module HsBindgen.Frontend.Pass.ResolveBindingSpecs (
    module HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
  , resolveBindingSpecs
  , BindingSpecsError(..)
  ) where

import Control.Monad.State
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.CNameSpelling
import Clang.HighLevel.Types
import Clang.Paths
import HsBindgen.BindingSpecs (ResolvedBindingSpecs)
import HsBindgen.BindingSpecs qualified as BindingSpecs
import HsBindgen.Frontend.AST
import HsBindgen.Frontend.Graph.Includes (IncludeGraph)
import HsBindgen.Frontend.Graph.Includes qualified as IncludeGraph
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.RenameAnon
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

resolveBindingSpecs ::
     ResolvedBindingSpecs -- ^ Configuration binding specifications
  -> ResolvedBindingSpecs -- ^ External binding specifications
  -> TranslationUnit RenameAnon
  -> (TranslationUnit ResolveBindingSpecs, [BindingSpecsError])
resolveBindingSpecs
  confSpecs
  extSpecs
  TranslationUnit{unitDecls, unitIncludeGraph, unitAnn} =
    first reassemble . runM $
      resolveDecls confSpecs extSpecs unitIncludeGraph unitDecls
  where
    reassemble ::
         [Decl ResolveBindingSpecs]
      -> TranslationUnit ResolveBindingSpecs
    reassemble decls' = TranslationUnit{
        unitDecls = decls'
      , unitIncludeGraph
      , unitAnn
      }

data BindingSpecsError =
    BindingSpecsOmittedTypeUse BindingSpecs.OmittedTypeUseException
  | BindingSpecsInvalidExtRef  BindingSpecs.GetExtHsRefException
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Internal: monad
-------------------------------------------------------------------------------}

newtype M a = WrapM {
      unwrapM :: State Ctx a
    }
  deriving newtype (Applicative, Functor, Monad, MonadState Ctx)

runM :: M a -> (a, [BindingSpecsError])
runM = fmap (reverse . ctxErrors) . flip runState initCtx . unwrapM

{-------------------------------------------------------------------------------
  Internal: state context
-------------------------------------------------------------------------------}

data Ctx = Ctx {
      ctxErrors       :: [BindingSpecsError] -- ^ Stored in reverse order
    , ctxExtTypes     :: Map (QualId RenameAnon) (Type ResolveBindingSpecs)
    , ctxOmittedTypes :: Set (QualId RenameAnon)
    }

initCtx :: Ctx
initCtx = Ctx {
      ctxErrors       = []
    , ctxExtTypes     = Map.empty
    , ctxOmittedTypes = Set.empty
    }

insertError :: BindingSpecsError -> Ctx -> Ctx
insertError e ctx = ctx {
      ctxErrors = e : ctxErrors ctx
    }

insertExtType :: QualId RenameAnon -> Type ResolveBindingSpecs -> Ctx -> Ctx
insertExtType qualId typ ctx = ctx {
      ctxExtTypes = Map.insert qualId typ (ctxExtTypes ctx)
    }

insertOmittedType :: QualId RenameAnon -> Ctx -> Ctx
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
  -> [Decl RenameAnon]
  -> M [Decl ResolveBindingSpecs]
resolveDecls confSpecs extSpecs includeGraph = mapMaybeM aux
  where
    aux :: Decl RenameAnon -> M (Maybe (Decl ResolveBindingSpecs))
    aux decl = auxExt
      where
        qualId :: QualId RenameAnon
        qualId = declQualId decl

        cname :: CNameSpelling
        cname = qualIdCNameSpelling qualId

        declPaths :: Set SourcePath
        declPaths = IncludeGraph.reaches includeGraph $
          singleLocPath (declLoc (declInfo decl))

        auxExt :: M (Maybe (Decl ResolveBindingSpecs))
        auxExt = case BindingSpecs.lookupType cname declPaths extSpecs of
          Just (BindingSpecs.Require typeSpec) ->
            case BindingSpecs.getExtHsRef cname typeSpec of
              Right extHsRef -> do
                let t = TypeExtBinding extHsRef typeSpec
                modify' $ insertExtType qualId t
                return Nothing
              Left e -> do
                modify' $ insertError (BindingSpecsInvalidExtRef e)
                auxConf
          Just BindingSpecs.Omit -> do
            let e = BindingSpecs.OmittedTypeUse cname
            modify' $ insertError (BindingSpecsOmittedTypeUse e)
            auxConf
          Nothing -> auxConf

        auxConf :: M (Maybe (Decl ResolveBindingSpecs))
        auxConf = case BindingSpecs.lookupType cname declPaths confSpecs of
          Just (BindingSpecs.Require typeSpec) ->
            Just <$> mkDecl decl (Just typeSpec)
          Just BindingSpecs.Omit -> do
            modify' $ insertOmittedType qualId
            return Nothing
          Nothing -> Just <$> mkDecl decl Nothing

qualIdCNameSpelling :: QualId RenameAnon -> CNameSpelling
qualIdCNameSpelling (QualId (CName cname) namespace) =
    let prefix = case namespace of
          NamespaceTypedef  -> ""
          NamespaceStruct   -> "struct "
          NamespaceUnion    -> "union "
          NamespaceEnum     -> "enum "
          NamespaceMacro    -> ""
          NamespaceFunction -> ""
    in  CNameSpelling $ prefix <> cname

mkDecl ::
     Decl RenameAnon
  -> Maybe BindingSpecs.Type
  -> M (Decl ResolveBindingSpecs)
mkDecl decl mTypeSpec = reassemble <$> mkDeclKind (declKind decl)
  where
    reassemble :: DeclKind ResolveBindingSpecs -> Decl ResolveBindingSpecs
    reassemble declKind = Decl {
        declInfo = mkDeclInfo (declInfo decl)
      , declKind
      , declAnn = mTypeSpec
      }

mkDeclInfo :: DeclInfo RenameAnon -> DeclInfo ResolveBindingSpecs
mkDeclInfo DeclInfo{declLoc, declId} = DeclInfo{declLoc, declId}

mkDeclKind :: DeclKind RenameAnon -> M (DeclKind ResolveBindingSpecs)
mkDeclKind = \case
    DeclStruct struct   -> DeclStruct <$> mkStruct struct
    DeclStructOpaque    -> return DeclStructOpaque
    DeclUnion union     -> DeclUnion <$> mkUnion union
    DeclUnionOpaque     -> return DeclUnionOpaque
    DeclTypedef typedef -> DeclTypedef <$> mkTypedef typedef
    DeclEnum enumConsts -> return (DeclEnum enumConsts)
    DeclEnumOpaque      -> return DeclEnumOpaque
    DeclMacro macro     -> return (DeclMacro macro)
    DeclFunction fun    -> DeclFunction <$> mkFunction fun

mkStruct :: Struct RenameAnon -> M (Struct ResolveBindingSpecs)
mkStruct struct = reassemble <$> mapM mkStructField (structFields struct)
  where
    reassemble ::
         [StructField ResolveBindingSpecs]
      -> Struct ResolveBindingSpecs
    reassemble structFields = Struct {
        structSizeof = structSizeof struct
      , structAlignment = structAlignment struct
      , structFields
      }

mkStructField :: StructField RenameAnon -> M (StructField ResolveBindingSpecs)
mkStructField field = reassemble <$> mkType (structFieldType field)
  where
    reassemble :: Type ResolveBindingSpecs -> StructField ResolveBindingSpecs
    reassemble structFieldType = StructField {
        structFieldName = structFieldName field
      , structFieldType
      , structFieldOffset = structFieldOffset field
      , structFieldAnn = structFieldAnn field
      }

mkUnion :: Union RenameAnon -> M (Union ResolveBindingSpecs)
mkUnion union = reassemble <$> mapM mkUnionField (unionFields union)
  where
    reassemble :: [UnionField ResolveBindingSpecs] -> Union ResolveBindingSpecs
    reassemble unionFields = Union {
        unionSizeof = unionSizeof union
      , unionAlignment = unionAlignment union
      , unionFields
      }

mkUnionField :: UnionField RenameAnon -> M (UnionField ResolveBindingSpecs)
mkUnionField field = reassemble <$> mkType (unionFieldType field)
  where
    reassemble :: Type ResolveBindingSpecs -> UnionField ResolveBindingSpecs
    reassemble unionFieldType = UnionField {
        unionFieldName = unionFieldName field
      , unionFieldType
      , unionFieldAnn = unionFieldAnn field
      }

mkTypedef :: Typedef RenameAnon -> M (Typedef ResolveBindingSpecs)
mkTypedef typedef = reassemble <$> mkType (typedefType typedef)
  where
    reassemble :: Type ResolveBindingSpecs -> Typedef ResolveBindingSpecs
    reassemble typedefType = Typedef {
        typedefType
      , typedefAnn = typedefAnn typedef
      }

mkFunction :: Function RenameAnon -> M (Function ResolveBindingSpecs)
mkFunction fun = do
    functionArgs <- mapM mkType (functionArgs fun)
    functionRes <- mkType (functionRes fun)
    return Function {
        functionName = functionName fun
      , functionArgs
      , functionRes
      , functionAnn = functionAnn fun
      }

mkType :: Type RenameAnon -> M (Type ResolveBindingSpecs)
mkType = \case
    TypePrim t -> return (TypePrim t)
    TypeStruct uid -> aux TypeStruct uid NamespaceStruct
    TypeUnion uid -> aux TypeUnion uid NamespaceUnion
    TypeEnum uid -> aux TypeEnum uid NamespaceEnum
    TypeTypedef uid ann -> aux (`TypeTypedef` ann) uid NamespaceTypedef
    TypePointer t -> TypePointer <$> mkType t
    TypeFunction args res -> TypeFunction <$> mapM mkType args <*> mkType res
    TypeVoid -> return TypeVoid
    TypeExtBinding extHsRef typeSpec ->
      return (TypeExtBinding extHsRef typeSpec)
  where
    aux ::
         (Id ResolveBindingSpecs -> Type ResolveBindingSpecs)
      -> Id RenameAnon
      -> Namespace
      -> M (Type ResolveBindingSpecs)
    aux mk uid namespace = do
      let qualId = QualId uid namespace
          cname  = qualIdCNameSpelling qualId
      isOmitted <- gets $ Set.member qualId . ctxOmittedTypes
      when isOmitted $
        let e = BindingSpecs.OmittedTypeUse cname
        in  modify' $ insertError (BindingSpecsOmittedTypeUse e)
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
