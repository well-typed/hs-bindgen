module HsBindgen.Frontend.Pass.MangleNames (
    mangleNames
  , MangleError(..)
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Map qualified as Map
import Data.Proxy

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config.FixCandidate (FixCandidate (..))
import HsBindgen.Config.FixCandidate qualified as FixCandidate
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.Rename.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C (CName (..))
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell

{-------------------------------------------------------------------------------
  Top-level

  We proceed in two passes: first we go over all def sites (declarations), and
  choose names; then in a second pass we patch up all references (in both def
  and use sites). This is necessary because although we will encounter
  declarations before they are "used", a reference through a pointer field does
  not count as a "use".
-------------------------------------------------------------------------------}

mangleNames ::
     C.TranslationUnit ResolveBindingSpec
  -> (C.TranslationUnit NameMangler, [MangleError])
mangleNames unit =
    (unit', errors1 ++ errors2)
  where
    -- TODO: This should be configurable
    fc :: FixCandidate Maybe
    fc = FixCandidate.fixCandidateDefault

    nameMap :: NameMap
    errors1 :: [MangleError]
    (nameMap, errors1) = chooseNames fc (C.unitDecls unit)

    unit'   :: C.TranslationUnit NameMangler
    errors2 :: [MangleError]
    (unit', errors2) = runM fc nameMap $ mangle unit

{-------------------------------------------------------------------------------
  Pass 1: Choose names

  When this fails, we construct a placeholder name; this allows us to proceed
  even if there are errors.
-------------------------------------------------------------------------------}

type NameMap = Map (C.QualId ResolveBindingSpec) HsIdentifier

data MangleError =
    CouldNotMangle Text
  deriving stock (Show, Eq)

chooseNames ::
     FixCandidate Maybe
  -> [C.Decl ResolveBindingSpec] -> (NameMap, [MangleError])
chooseNames fc decls =
    bimap Map.fromList catMaybes $
      unzip $ map (nameForDecl fc) decls

nameForDecl ::
     FixCandidate Maybe
  -> C.Decl ResolveBindingSpec
  -> ((C.QualId ResolveBindingSpec, HsIdentifier), Maybe MangleError)
nameForDecl fc decl =
    case typeSpecIdentifier of
      Just hsName -> (choose hsName, Nothing)
      Nothing     -> withDeclNamespace declKind $ \ns ->
                       first choose $ fromCName fc ns cName
  where
    C.Decl{declInfo = C.DeclInfo{declId = cName}, declKind, declAnn} = decl
    BindingSpec.TypeSpec{typeSpecIdentifier} = declAnn

    choose :: HsIdentifier -> (C.QualId ResolveBindingSpec, HsIdentifier)
    choose hsName = (C.declQualId decl, hsName)

fromCName :: forall ns.
     SingNamespace ns
  => FixCandidate Maybe
  -> Proxy ns
  -> CName
  -> (HsIdentifier, Maybe MangleError)
fromCName fc _ (CName cName) =
    case mFixed of
      Just (HsName hsName) -> (HsIdentifier hsName, Nothing)
      Nothing              -> (HsIdentifier "", Just $ CouldNotMangle cName)
  where
    mFixed :: Maybe (HsName ns)
    mFixed = FixCandidate.fixCandidate fc cName

{-------------------------------------------------------------------------------
  Internal: monad for pass 2, applying the namemap
-------------------------------------------------------------------------------}

newtype M a = WrapM {
      unwrapM :: StateT [MangleError] (Reader Env) a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadState [MangleError]
    , MonadReader Env
    )

data Env = Env{
      envNameMap      :: NameMap
    , envFixCandidate :: FixCandidate Maybe
    }

runM :: FixCandidate Maybe -> NameMap -> M a -> (a, [MangleError])
runM fc nm = flip runReader env . flip runStateT [] . unwrapM
  where
    env :: Env
    env = Env{
          envFixCandidate = fc
        , envNameMap      = nm
        }

{-------------------------------------------------------------------------------
  Pass 2: apply NameMap
-------------------------------------------------------------------------------}

class Mangle a where
  mangle :: a ResolveBindingSpec -> M (a NameMangler)

class MangleDecl a where
  mangleDecl ::
       C.DeclInfo NameMangler
    -> a ResolveBindingSpec -> M (a NameMangler)

mangleQualId :: HasCallStack => C.QualId ResolveBindingSpec -> M NamePair
mangleQualId qualId@(C.QualId cName _namespace) = do
    nm <- asks envNameMap
    return $
      case Map.lookup qualId nm of
        Just hsName -> NamePair cName hsName
        Nothing     -> panicPure $ concat [
            "Name mangler bug: no name for "
          , show qualId
          , " in "
          , show nm
          ]

{-------------------------------------------------------------------------------
  Additional name mangling functionality

  TODO: Perhaps some (or all) of this should be configurable.
-------------------------------------------------------------------------------}

mangleFieldName :: C.DeclInfo NameMangler -> CName -> M NamePair
mangleFieldName info fieldCName = do
    fc <- asks envFixCandidate
    let candidate = declCName <> "_" <> fieldCName
    let (fieldHsName, mError) = fromCName fc (Proxy @NsVar) candidate
    forM_ mError $ modify . (:)
    return $ NamePair fieldCName fieldHsName
  where
    C.DeclInfo{declId = NamePair declCName _declHsName} = info

-- | Mangle enum constant name
--
-- Since these live in the global namespace, we do not prepend the name of
-- the enclosing enum.
mangleEnumConstant :: C.DeclInfo NameMangler -> CName -> M NamePair
mangleEnumConstant _info cName = do
    fc <- asks envFixCandidate
    let (hsName, mError) = fromCName fc (Proxy @NsConstr) cName
    forM_ mError $ modify . (:)
    return $ NamePair cName hsName

-- | Struct names
--
-- Right now we reuse the name of the type also for the constructor.
mkStructNames :: C.DeclInfo NameMangler -> RecordNames
mkStructNames info = RecordNames{
      recordConstr = nameHs declId
    }
  where
    C.DeclInfo{declId} = info

-- | Generic construction of newtype names, given only the type name
mkNewtypeNames :: C.DeclInfo NameMangler -> NewtypeNames
mkNewtypeNames info = NewtypeNames{
      newtypeConstr = nameHs declId
    , newtypeField  = "un_" <> nameHs declId
    }
  where
    C.DeclInfo{declId} = info

-- | Union names
--
-- A union is represented by a newtype around the raw bytes.
mkUnionNames :: C.DeclInfo NameMangler -> NewtypeNames
mkUnionNames = mkNewtypeNames

-- | Enum names
--
-- An enum is represented by a newtype around an integral value.
mkEnumNames :: C.DeclInfo NameMangler -> NewtypeNames
mkEnumNames = mkNewtypeNames

-- | Typedef
--
-- Typedefs are represented by newtypes
mkTypedefNames :: C.DeclInfo NameMangler -> NewtypeNames
mkTypedefNames = mkNewtypeNames

-- | Macro types
--
-- These behave like typedefs.
mkMacroTypeNames :: C.DeclInfo NameMangler -> NewtypeNames
mkMacroTypeNames = mkNewtypeNames

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance Mangle C.TranslationUnit where
  mangle C.TranslationUnit{..} = do
      let mk :: [C.Decl NameMangler] -> C.TranslationUnit NameMangler
          mk decls = C.TranslationUnit{unitDecls = decls, ..}
      mk <$> mapM mangle unitDecls

instance Mangle C.Decl where
  mangle decl = do
      declId' <- mangleQualId (C.declQualId decl)

      let info :: C.DeclInfo NameMangler
          info = C.DeclInfo{declId = declId', ..}

          mk :: C.DeclKind NameMangler -> C.Decl NameMangler
          mk declKind' = C.Decl{
                declInfo = info
              , declKind = declKind'
              , declAnn  = DeclSpec declAnn
              }

      mk <$> mangleDecl info declKind
    where
      C.Decl{declInfo = C.DeclInfo{..}, declKind, declAnn} = decl

instance MangleDecl C.DeclKind where
  mangleDecl info (C.DeclStruct struct) =
      C.DeclStruct <$> mangleDecl info struct
  mangleDecl _ C.DeclStructOpaque =
      return $ C.DeclStructOpaque
  mangleDecl info (C.DeclUnion union) =
      C.DeclUnion <$> mangleDecl info union
  mangleDecl _ C.DeclUnionOpaque =
      return $ C.DeclUnionOpaque
  mangleDecl info (C.DeclTypedef typedef) =
      C.DeclTypedef <$> mangleDecl info typedef
  mangleDecl info (C.DeclEnum enum) =
      C.DeclEnum <$> mangleDecl info enum
  mangleDecl _ C.DeclEnumOpaque =
      return $ C.DeclEnumOpaque
  mangleDecl info (C.DeclFunction fun) =
      C.DeclFunction <$> mangleDecl info fun
  mangleDecl info (C.DeclMacro macro) =
      C.DeclMacro <$> mangleDecl info macro

instance MangleDecl C.Struct where
  mangleDecl info C.Struct{..} = do
      let mk :: [C.StructField NameMangler] -> C.Struct NameMangler
          mk structFields' = C.Struct{
                structFields = structFields'
              , structAnn    = mkStructNames info
              , ..
              }
      mk <$> mapM (mangleDecl info) structFields

instance MangleDecl C.StructField where
  mangleDecl info C.StructField{..} = do
      let mk ::
               FieldName NameMangler
            -> C.Type NameMangler
            -> C.StructField NameMangler
          mk   structFieldName' structFieldType' = C.StructField{
                structFieldName = structFieldName'
              , structFieldType = structFieldType'
              , ..
              }
      mk <$> mangleFieldName info structFieldName
         <*> mangle structFieldType

instance MangleDecl C.Union where
  mangleDecl info C.Union{..} = do
      let mk :: [C.UnionField NameMangler] -> C.Union NameMangler
          mk unionFields' = C.Union{
                unionFields = unionFields'
              , unionAnn    = mkUnionNames info
              , ..
              }
      mk <$> mapM (mangleDecl info) unionFields

instance MangleDecl C.UnionField where
  mangleDecl info C.UnionField{..} = do
      let mk ::
               FieldName NameMangler
            -> C.Type NameMangler
            -> C.UnionField NameMangler
          mk unionFieldName' unionFieldType' = C.UnionField{
                unionFieldName = unionFieldName'
              , unionFieldType = unionFieldType'
              , ..
              }
      mk <$> mangleFieldName info unionFieldName
         <*> mangle unionFieldType

instance MangleDecl C.Enum where
  mangleDecl info C.Enum{..} = do
      let mk ::
               C.Type NameMangler
            -> [C.EnumConstant NameMangler]
            -> C.Enum NameMangler
          mk enumType' enumConstants' = C.Enum{
                enumType      = enumType'
              , enumConstants = enumConstants'
              , enumAnn       = mkEnumNames info
              , ..
              }
      mk <$> mangle enumType
         <*> mapM (mangleDecl info) enumConstants

instance MangleDecl C.EnumConstant where
  mangleDecl info C.EnumConstant{..} = do
      let mk :: NamePair -> C.EnumConstant NameMangler
          mk enumConstantName' = C.EnumConstant{
                enumConstantName = enumConstantName'
              , ..
              }
      mk <$> mangleEnumConstant info enumConstantName

instance MangleDecl C.Typedef where
  mangleDecl info C.Typedef{..} = do
      let mk :: C.Type NameMangler -> C.Typedef NameMangler
          mk typedefType' = C.Typedef{
                typedefType = typedefType'
              , typedefAnn  = mkTypedefNames info
              }
      mk <$> mangle typedefType

instance MangleDecl C.Function where
  mangleDecl _info C.Function{..} = do
      let mk ::
               [C.Type NameMangler]
            -> C.Type NameMangler
            -> C.Function NameMangler
          mk functionArgs' functionRes' = C.Function{
                functionArgs = functionArgs'
              , functionRes  = functionRes'
              , ..
              }
      mk <$> mapM mangle functionArgs <*> mangle functionRes

instance MangleDecl C.CheckedMacro where
  mangleDecl info = \case
      C.MacroType typ  -> C.MacroType <$> mangleDecl info typ
      C.MacroExpr expr -> return $ C.MacroExpr expr

instance MangleDecl C.CheckedMacroType where
  mangleDecl info C.CheckedMacroType{..} = do
    let mk :: C.Type NameMangler -> C.CheckedMacroType NameMangler
        mk macroType' = C.CheckedMacroType{
               macroType    = macroType'
             , macroTypeAnn = mkMacroTypeNames info
             }
    mk <$> mangle macroType

instance Mangle C.Type where
  mangle (C.TypeStruct name) =
      C.TypeStruct <$> mangleQualId (C.QualId name C.NamespaceStruct)
  mangle (C.TypeUnion name) =
      C.TypeUnion <$> mangleQualId (C.QualId name C.NamespaceUnion)
  mangle (C.TypeEnum name) =
      C.TypeEnum <$> mangleQualId (C.QualId name C.NamespaceEnum)
  mangle (C.TypeTypedef ref) =
      C.TypeTypedef <$> mangle ref
  mangle (C.TypeMacroTypedef name) =
      C.TypeMacroTypedef
        <$> mangleQualId (C.QualId name C.NamespaceMacro)
  mangle (C.TypePointer typ) =
      C.TypePointer <$> mangle typ
  mangle (C.TypeFun args res) =
      C.TypeFun <$> mapM mangle args <*> mangle res
  mangle (C.TypeConstArray n typ) =
      C.TypeConstArray n <$> mangle typ
  mangle (C.TypeIncompleteArray typ) =
      C.TypeIncompleteArray <$> mangle typ

  -- The other entries do not need any name mangling
  mangle (C.TypePrim prim)            = return $ C.TypePrim prim
  mangle  C.TypeVoid                  = return $ C.TypeVoid
  mangle (C.TypeExtBinding c ref typ) = return $ C.TypeExtBinding c ref typ

instance Mangle RenamedTypedefRef where
  mangle (TypedefRegular name) =
      TypedefRegular <$> mangleQualId (C.QualId name C.NamespaceTypedef)
  mangle (TypedefSquashed cName ty) =
      TypedefSquashed cName <$> mangle ty

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

withDeclNamespace ::
     C.DeclKind ResolveBindingSpec
  -> (forall ns. SingNamespace ns => Proxy ns -> r)
  -> r
withDeclNamespace kind k =
    case kind of
      C.DeclStruct{}       -> k (Proxy @NsTypeConstr)
      C.DeclStructOpaque{} -> k (Proxy @NsTypeConstr)
      C.DeclUnion{}        -> k (Proxy @NsTypeConstr)
      C.DeclUnionOpaque{}  -> k (Proxy @NsTypeConstr)
      C.DeclTypedef{}      -> k (Proxy @NsTypeConstr)
      C.DeclEnum{}         -> k (Proxy @NsTypeConstr)
      C.DeclEnumOpaque{}   -> k (Proxy @NsTypeConstr)
      C.DeclFunction{}     -> k (Proxy @NsVar)

      C.DeclMacro macro ->
        case macro of
          C.MacroType{} -> k (Proxy @NsTypeConstr)
          C.MacroExpr{} -> k (Proxy @NsVar)

