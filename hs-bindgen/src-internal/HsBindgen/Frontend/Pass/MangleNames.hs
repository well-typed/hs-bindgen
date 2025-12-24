module HsBindgen.Frontend.Pass.MangleNames (
    mangleNames
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bitraversable (bimapM)
import Data.Foldable qualified as Foldable
import Data.Function
import Data.Map qualified as Map
import Data.Proxy

import Clang.HighLevel.Types

import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config.FixCandidate (FixCandidate (..))
import HsBindgen.Config.FixCandidate qualified as FixCandidate
import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.Typedefs (TypedefAnalysis)
import HsBindgen.Frontend.Analysis.Typedefs qualified as TypedefAnalysis
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Top-level

  We proceed in two passes: first we go over all def sites (declarations), and
  choose names; then in a second pass we patch up all references (in both def
  and use sites). This is necessary because although we will encounter
  declarations before they are "used", a reference through a pointer field does
  not count as a "use".
-------------------------------------------------------------------------------}

mangleNames ::
     C.TranslationUnit Select
  -> (C.TranslationUnit MangleNames, [Msg MangleNames])
mangleNames unit = (
         C.TranslationUnit{
           unitDecls        = catMaybes decls'
         , unitIncludeGraph = unit.unitIncludeGraph
         , unitAnn          = updateDeclMeta td nm unit.unitAnn
        }
    , msgs1 ++ msgs2
    )
  where
    td :: TypedefAnalysis
    td = TypedefAnalysis.fromDecls unit.unitAnn.declDeclUse unit.unitDecls

    fc :: FixCandidate Maybe
    fc = FixCandidate.fixCandidateDefault

    nm      :: NameMap
    msgs1 :: [Msg MangleNames]
    (nm, msgs1) = chooseNames td fc (C.unitDecls unit)

    env :: Env
    env = Env{
          envTypedefAnalysis = td
        , envFixCandidate    = fc
        , envNameMap         = nm
        }

    decls' :: [Maybe (C.Decl MangleNames)]
    msgs2  :: [MangleNamesMsg]
    (decls', msgs2) = runM env $ mapM mangleDecl unit.unitDecls

updateDeclMeta :: TypedefAnalysis -> NameMap -> DeclMeta -> DeclMeta
updateDeclMeta td nm declMeta = declMeta{
      declIndex =
        DeclIndex.registerSquashedDeclarations squashedMap declMeta.declIndex
    }
  where
    squashedMap :: Map C.DeclId (SingleLoc, Hs.Identifier)
    squashedMap = Map.fromList $ catMaybes [
        (cDeclId,) . (sloc,) <$> Map.lookup wrappedId nm
      | (cDeclId, TypedefAnalysis.Squash sloc wrappedId) <-
          Map.toList td.analysis
      ]

{-------------------------------------------------------------------------------
  Pass 1: Choose names

  When this fails, we construct a placeholder name; this allows us to proceed
  even if there are errors.
-------------------------------------------------------------------------------}

type NameMap = Map C.DeclId Hs.Identifier

chooseNames ::
     TypedefAnalysis
  -> FixCandidate Maybe
  -> [C.Decl Select]
  -> (NameMap, [Msg MangleNames])
chooseNames td fc decls =
    let specifiedNames = Map.fromList $ mapMaybe getSpecifiedName decls
    in  bimap Map.fromList concat . unzip $
          map (nameForDecl td fc specifiedNames) decls
  where
    getSpecifiedName :: C.Decl Select -> Maybe (C.DeclId, Hs.Identifier)
    getSpecifiedName decl =
      (decl.declInfo.declId,)
        <$> (BindingSpec.cTypeSpecIdentifier =<< fst (decl.declAnn))

nameForDecl ::
     TypedefAnalysis
  -> FixCandidate Maybe
  -> NameMap
  -> C.Decl Select
  -> ((C.DeclId, Hs.Identifier), [Msg MangleNames])
nameForDecl td fc specifiedNames decl =
    case Map.lookup declId specifiedNames of
      Just hsName ->
        -- Binding spec specified a name for this declaration.
        -- In this case, this overrides any naming decisions we might make here.
        --
        -- TODO: <https://github.com/well-typed/hs-bindgen/issues/1436>
        -- If we have a binding specification for a type that is squashed, it is
        -- currently silenty ignored, because that declaration has already been
        -- removed from the list of declarations in @Select@.
        ((declId, hsName), [])
      Nothing -> withDeclNamespace decl.declKind $ \ns ->
        case Map.lookup declId td.analysis of
          Nothing ->
            fromDeclId fc ns declId & \(hsName, msgs) -> (
                (declId, hsName)
              , msgs
              )
          Just (TypedefAnalysis.Rename (TypedefAnalysis.AddSuffix suffix)) ->
            fromDeclId fc ns declId & \(hsName, msgs) ->
              let newName = hsName <> suffix in (
                (declId, newName)
              , MangleNamesRenamed decl.declInfo newName : msgs
              )
          Just (TypedefAnalysis.Rename (TypedefAnalysis.UseNameOf declId')) ->
            case Map.lookup declId' specifiedNames of
              Just hsName -> ((declId, hsName), [])
              Nothing ->
                fromDeclId fc ns declId' & \(hsName, msgs) -> (
                    (declId, hsName)
                  , if declId.name.text /= declId'.name.text
                      then MangleNamesRenamed decl.declInfo hsName : msgs
                      else msgs
                  )
          Just (TypedefAnalysis.Squash _ declId') ->
            case Map.lookup declId' specifiedNames of
              Just hsName -> ((declId, hsName), [])
              Nothing ->
                fromDeclId fc ns declId & \(hsName, msgs) -> (
                    (declId, hsName)
                  , msgs
                  )
  where
    declId :: C.DeclId
    declId = decl.declInfo.declId

{-------------------------------------------------------------------------------
  Internal: working with 'FixCandidate'
-------------------------------------------------------------------------------}

fixCandidate :: forall ns.
     Hs.SingNamespace ns
  => FixCandidate Maybe
  -> Proxy ns
  -> Text
  -> (Hs.Identifier, [MangleNamesMsg])
fixCandidate fc _ cName =
    case FixCandidate.fixCandidate fc cName :: Maybe (Hs.ExportedName ns) of
      Just hsName -> (Hs.Identifier hsName.text, [])
      Nothing -> (Hs.Identifier "", [MangleNamesCouldNotMangle cName])

fromDeclId :: forall ns.
     Hs.SingNamespace ns
  => FixCandidate Maybe
  -> Proxy ns
  -> C.DeclId
  -> (Hs.Identifier, [MangleNamesMsg])
fromDeclId fc ns declId = fixCandidate fc ns declId.name.text

{-------------------------------------------------------------------------------
  Internal: monad for pass 2, applying the namemap
-------------------------------------------------------------------------------}

newtype M a = WrapM {
      unwrapM :: StateT [Msg MangleNames] (Reader Env) a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    )

data Env = Env{
      envTypedefAnalysis :: TypedefAnalysis
    , envNameMap         :: NameMap
    , envFixCandidate    :: FixCandidate Maybe
    }

runM :: Env -> M a -> (a, [Msg MangleNames])
runM env = second reverse . flip runReader env . flip runStateT [] . unwrapM

checkTypedefAnalysis :: C.DeclId -> M (Maybe TypedefAnalysis.Conclusion)
checkTypedefAnalysis declId = WrapM $ do
    td <- asks envTypedefAnalysis
    return $ Map.lookup declId td.analysis

traceMsg :: MangleNamesMsg -> M ()
traceMsg msg = WrapM $ modify (msg :)

mangleDeclId :: C.DeclId -> M C.DeclIdPair
mangleDeclId declId = WrapM $ do
    nm <- asks envNameMap
    case Map.lookup declId nm of
      Just hsName -> return $ C.DeclIdPair declId hsName
      Nothing     -> panicPure $ "Missing declaration: " <> show declId

-- | Apply Haskell naming rules
mkIdentifier :: Hs.SingNamespace ns => Proxy ns -> Text -> M Hs.Identifier
mkIdentifier ns candidate = do
    fc <- WrapM $ asks envFixCandidate
    let (fieldHsName, mError) = fixCandidate fc ns candidate
    forM_ mError traceMsg
    return fieldHsName

-- | Search the 'NameMap', when we don't know the name kind
searchNameMap :: Text -> M (Maybe C.DeclIdPair)
searchNameMap name = WrapM $ do
     nm <- asks envNameMap
     return $ Foldable.asum [
         C.DeclIdPair declId <$> Map.lookup declId nm
       | kind <- [minBound .. maxBound]
       , let declId = C.DeclId{name = C.DeclName name kind, isAnon = False}
       ]

{-------------------------------------------------------------------------------
  Pass 2: apply NameMap
-------------------------------------------------------------------------------}

class Mangle a where
  mangle :: a Select -> M (a MangleNames)

class MangleInDecl a where
  mangleInDecl :: C.DeclInfo MangleNames -> a Select -> M (a MangleNames)

mangleDecl :: C.Decl Select -> M (Maybe (C.Decl MangleNames))
mangleDecl decl = do
     mConclusion <- checkTypedefAnalysis decl.declInfo.declId
     case mConclusion of
       Just TypedefAnalysis.Squash{} -> do
         traceMsg $ MangleNamesSquashed decl.declInfo
         return Nothing
       _otherwise -> do
         declId'      <- mangleDeclId decl.declInfo.declId
         declComment' <- traverse mangle decl.declInfo.declComment

         let info :: C.DeclInfo MangleNames
             info = C.DeclInfo{
                  declId           = declId'
                , declComment      = declComment'
                , declLoc          = decl.declInfo.declLoc
                , declHeaderInfo   = decl.declInfo.declHeaderInfo
                , declAvailability = decl.declInfo.declAvailability
                }

             mk :: C.DeclKind MangleNames -> C.Decl MangleNames
             mk declKind' = C.Decl{
                   declInfo = info
                 , declKind = declKind'
                 , declAnn  = decl.declAnn
                 }

         Just . mk <$> mangleInDecl info decl.declKind

{-------------------------------------------------------------------------------
  Additional name mangling functionality
-------------------------------------------------------------------------------}

mangleFieldName :: C.DeclInfo MangleNames -> C.ScopedName -> M C.NamePair
mangleFieldName info fieldCName =
    C.NamePair fieldCName <$> mkIdentifier (Proxy @Hs.NsVar) candidate
  where
    candidate :: Text
    candidate = info.declId.hsName.text <> "_" <> fieldCName.text

-- | Mangle enum constant name
--
-- Since these live in the global namespace, we do not prepend the name of
-- the enclosing enum.
mangleEnumConstant :: C.DeclInfo MangleNames -> C.ScopedName -> M C.NamePair
mangleEnumConstant _info cName =
    C.NamePair cName <$> mkIdentifier (Proxy @Hs.NsConstr) cName.text

-- | Struct names
--
-- Right now we reuse the name of the type also for the constructor.
mkStructNames :: C.DeclInfo MangleNames -> C.RecordNames
mkStructNames info = C.RecordNames{
      recordConstr = Hs.unsafeHsIdHsName info.declId.hsName
    }

-- | Generic construction of newtype names, given only the type name
mkNewtypeNames :: C.DeclInfo MangleNames -> C.NewtypeNames
mkNewtypeNames info = C.NewtypeNames{
      newtypeConstr = Hs.unsafeHsIdHsName $          info.declId.hsName
    , newtypeField  = Hs.unsafeHsIdHsName $ "un_" <> info.declId.hsName
    }

-- | Union names
--
-- A union is represented by a newtype around the raw bytes.
mkUnionNames :: C.DeclInfo MangleNames -> C.NewtypeNames
mkUnionNames = mkNewtypeNames

-- | Enum names
--
-- An enum is represented by a newtype around an integral value.
mkEnumNames :: C.DeclInfo MangleNames -> C.NewtypeNames
mkEnumNames = mkNewtypeNames

-- | Typedef
--
-- Typedefs are represented by newtypes
mkTypedefNames :: C.DeclInfo MangleNames -> C.NewtypeNames
mkTypedefNames = mkNewtypeNames

-- | Macro types
--
-- These behave like typedefs.
mkMacroTypeNames :: C.DeclInfo MangleNames -> C.NewtypeNames
mkMacroTypeNames = mkNewtypeNames

-- | Mangle function argument name
--
-- Function argument names are not really used when generating Haskell code.
-- They are more relevant for documentation purposes so we don't do any
-- mangling.
mangleArgumentName :: C.ScopedName -> M C.NamePair
mangleArgumentName argName =
    C.NamePair argName <$> mkIdentifier (Proxy @Hs.NsVar) argName.text

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance MangleInDecl C.DeclKind where
  mangleInDecl info = \case
      C.DeclStruct   x -> C.DeclStruct   <$> mangleInDecl info x
      C.DeclUnion    x -> C.DeclUnion    <$> mangleInDecl info x
      C.DeclTypedef  x -> C.DeclTypedef  <$> mangleInDecl info x
      C.DeclEnum     x -> C.DeclEnum     <$> mangleInDecl info x
      C.DeclFunction x -> C.DeclFunction <$> mangleInDecl info x
      C.DeclMacro    x -> C.DeclMacro    <$> mangleInDecl info x
      C.DeclGlobal   x -> C.DeclGlobal   <$> mangle            x
      C.DeclOpaque     -> return C.DeclOpaque

instance MangleInDecl C.Struct where
  mangleInDecl info C.Struct{..} = do
      let mk :: [C.StructField MangleNames] -> C.Struct MangleNames
          mk structFields' = C.Struct{
                structFields = structFields'
              , structAnn    = mkStructNames info
              , ..
              }
      mk <$> mapM (mangleInDecl info) structFields

instance MangleInDecl C.StructField where
  mangleInDecl info C.StructField{..} = do
      let mk ::
               FieldName MangleNames
            -> C.Type MangleNames
            -> Maybe (C.Comment MangleNames)
            -> C.StructField MangleNames
          mk   structFieldName' structFieldType' structFieldComment' =
            C.StructField {
                structFieldInfo =
                  C.FieldInfo {
                    fieldLoc     = C.fieldLoc structFieldInfo
                  , fieldName    = structFieldName'
                  , fieldComment = structFieldComment'
                  }
              , structFieldType = structFieldType'
              , ..
              }
      mk <$> mangleFieldName info (C.fieldName structFieldInfo)
         <*> mangle structFieldType
         <*> traverse mangle (C.fieldComment structFieldInfo)

instance MangleInDecl C.Union where
  mangleInDecl info C.Union{..} = do
      let mk :: [C.UnionField MangleNames] -> C.Union MangleNames
          mk unionFields' = C.Union{
                unionFields = unionFields'
              , unionAnn    = mkUnionNames info
              , ..
              }
      mk <$> mapM (mangleInDecl info) unionFields

instance MangleInDecl C.UnionField where
  mangleInDecl info C.UnionField{..} = do
      let mk ::
               FieldName MangleNames
            -> C.Type MangleNames
            -> Maybe (C.Comment MangleNames)
            -> C.UnionField MangleNames
          mk unionFieldName' unionFieldType' unionFieldComment' =
            C.UnionField {
                unionFieldInfo =
                  C.FieldInfo {
                    fieldLoc     = C.fieldLoc unionFieldInfo
                  , fieldName    = unionFieldName'
                  , fieldComment = unionFieldComment'
                  }
              , unionFieldType = unionFieldType'
              , ..
              }
      mk <$> mangleFieldName info (C.fieldName unionFieldInfo)
         <*> mangle unionFieldType
         <*> traverse mangle (C.fieldComment unionFieldInfo)

instance MangleInDecl C.Enum where
  mangleInDecl info C.Enum{..} = do
      let mk ::
               C.Type MangleNames
            -> [C.EnumConstant MangleNames]
            -> C.Enum MangleNames
          mk enumType' enumConstants' = C.Enum{
                enumType      = enumType'
              , enumConstants = enumConstants'
              , enumAnn       = mkEnumNames info
              , ..
              }
      mk <$> mangle enumType
         <*> mapM (mangleInDecl info) enumConstants

instance MangleInDecl C.EnumConstant where
  mangleInDecl info C.EnumConstant{..} = do
      let mk :: C.NamePair
             -> Maybe (C.Comment MangleNames)
             -> C.EnumConstant MangleNames
          mk enumConstantName' enumConstantComment' = C.EnumConstant{
                enumConstantInfo =
                  C.FieldInfo {
                    fieldLoc     = C.fieldLoc enumConstantInfo
                  , fieldName    = enumConstantName'
                  , fieldComment = enumConstantComment'
                  }
              , ..
              }
      mk <$> mangleEnumConstant info (C.fieldName enumConstantInfo)
         <*> traverse mangle (C.fieldComment enumConstantInfo)

instance Mangle C.Comment where
  mangle (C.Comment comment) = C.Comment <$> traverse mangle comment

instance Mangle C.CommentRef where
  mangle (C.CommentRef name Nothing) =
      -- NB: If this fails it means that we tried all possible name kinds and
      -- still didn't find any result. This might be because of a typo on the
      -- docs, or a missing reference.
      C.CommentRef name <$> searchNameMap name
  mangle (C.CommentRef name (Just declId)) =
      C.CommentRef name . Just <$> mangleDeclId declId

instance MangleInDecl C.Typedef where
  mangleInDecl info C.Typedef{..} = do
      let mk :: C.Type MangleNames -> C.Typedef MangleNames
          mk typedefType' = C.Typedef{
                typedefType = typedefType'
              , typedefAnn  = mkTypedefNames info
              , ..
              }
      mk <$> mangle typedefType

instance MangleInDecl C.Function where
  mangleInDecl _info C.Function{..} = do
      let mk ::
               [(Maybe C.NamePair, C.Type MangleNames)]
            -> C.Type MangleNames
            -> C.Function MangleNames
          mk functionArgs' functionRes' = C.Function{
                functionArgs = functionArgs'
              , functionRes  = functionRes'
              , ..
              }
      mk <$> mapM (bimapM (traverse mangleArgumentName) mangle) functionArgs
         <*> mangle functionRes

instance MangleInDecl C.CheckedMacro where
  mangleInDecl info = \case
      C.MacroType typ  -> C.MacroType <$> mangleInDecl info typ
      C.MacroExpr expr -> return $ C.MacroExpr expr

instance MangleInDecl C.CheckedMacroType where
  mangleInDecl info C.CheckedMacroType{..} = do
    let mk :: C.Type MangleNames -> C.CheckedMacroType MangleNames
        mk macroType' = C.CheckedMacroType{
               macroType    = macroType'
             , macroTypeAnn = mkMacroTypeNames info
             , ..
             }
    mk <$> mangle macroType

instance Mangle C.Type where
  mangle = \case
      -- Interesting cases
      C.TypeRef     declId     -> C.TypeRef <$> mangleDeclId declId
      C.TypeTypedef declId uTy -> C.TypeTypedef
                                    <$> mangleDeclId declId
                                    <*> mangle uTy

      -- Recursive cases
      C.TypePointers n typ      -> C.TypePointers n <$> mangle typ
      C.TypeFun args res        -> C.TypeFun <$> mapM mangle args <*> mangle res
      C.TypeConstArray n typ    -> C.TypeConstArray n <$> mangle typ
      C.TypeIncompleteArray typ -> C.TypeIncompleteArray <$> mangle typ
      C.TypeBlock typ           -> C.TypeBlock <$> mangle typ
      C.TypeConst typ           -> C.TypeConst <$> mangle typ

      -- The other entries do not need any name mangling
      C.TypePrim prim      -> return $ C.TypePrim prim
      C.TypeVoid           -> return $ C.TypeVoid
      C.TypeExtBinding ext -> return $ C.TypeExtBinding ext
      C.TypeComplex prim   -> return $ C.TypeComplex prim

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

withDeclNamespace ::
     C.DeclKind Select
  -> (forall ns. Hs.SingNamespace ns => Proxy ns -> r)
  -> r
withDeclNamespace kind k =
    case kind of
      C.DeclStruct{}   -> k (Proxy @Hs.NsTypeConstr)
      C.DeclUnion{}    -> k (Proxy @Hs.NsTypeConstr)
      C.DeclTypedef{}  -> k (Proxy @Hs.NsTypeConstr)
      C.DeclEnum{}     -> k (Proxy @Hs.NsTypeConstr)
      C.DeclOpaque{}   -> k (Proxy @Hs.NsTypeConstr)
      C.DeclFunction{} -> k (Proxy @Hs.NsVar)
      C.DeclGlobal{}   -> k (Proxy @Hs.NsVar)

      C.DeclMacro macro ->
        case macro of
          C.MacroType{} -> k (Proxy @Hs.NsTypeConstr)
          C.MacroExpr{} -> k (Proxy @Hs.NsVar)
