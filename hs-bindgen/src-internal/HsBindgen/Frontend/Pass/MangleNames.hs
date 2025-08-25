module HsBindgen.Frontend.Pass.MangleNames (
    mangleNames
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Map qualified as Map
import Data.Proxy

import Clang.HighLevel.Documentation qualified as C

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config.FixCandidate (FixCandidate (..))
import HsBindgen.Config.FixCandidate qualified as FixCandidate
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleTypedefs.IsPass
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Imports
import HsBindgen.Language.Haskell
import Data.Bitraversable (bimapM)

{-------------------------------------------------------------------------------
  Top-level

  We proceed in two passes: first we go over all def sites (declarations), and
  choose names; then in a second pass we patch up all references (in both def
  and use sites). This is necessary because although we will encounter
  declarations before they are "used", a reference through a pointer field does
  not count as a "use".
-------------------------------------------------------------------------------}

mangleNames ::
     C.TranslationUnit HandleTypedefs
  -> (C.TranslationUnit MangleNames, [Msg MangleNames])
mangleNames unit =
    (unit', errors1 ++ errors2)
  where
    -- TODO: This should be configurable
    fc :: FixCandidate Maybe
    fc = FixCandidate.fixCandidateDefault

    nameMap :: NameMap
    errors1 :: [Msg MangleNames]
    (nameMap, errors1) = chooseNames fc (C.unitDecls unit)

    unit'   :: C.TranslationUnit MangleNames
    errors2 :: [Msg MangleNames]
    (unit', errors2) = runM fc nameMap $ mangle unit

{-------------------------------------------------------------------------------
  Pass 1: Choose names

  When this fails, we construct a placeholder name; this allows us to proceed
  even if there are errors.
-------------------------------------------------------------------------------}

type NameMap = Map C.QualName  HsIdentifier

chooseNames ::
     FixCandidate Maybe
  -> [C.Decl HandleTypedefs] -> (NameMap, [Msg MangleNames])
chooseNames fc decls =
    bimap Map.fromList catMaybes $
      unzip $ map (nameForDecl fc) decls

nameForDecl ::
     FixCandidate Maybe
  -> C.Decl HandleTypedefs
  -> ((C.QualName, HsIdentifier), Maybe (Msg MangleNames))
nameForDecl fc decl =
    case typeSpecIdentifier of
      Just hsName -> (choose hsName, Nothing)
      Nothing     -> withDeclNamespace declKind $ \ns ->
                       first choose $ fromCName fc ns cName
  where
    C.Decl{
        declInfo = C.DeclInfo{declId = C.DeclId{declIdName = cName}}
      , declKind
      , declAnn
      } = decl
    BindingSpec.TypeSpec{typeSpecIdentifier} = declAnn

    choose :: HsIdentifier -> (C.QualName, HsIdentifier)
    choose hsName = (C.declQualName decl, hsName)

fromCName :: forall ns.
     SingNamespace ns
  => FixCandidate Maybe
  -> Proxy ns
  -> C.Name
  -> (HsIdentifier, Maybe (Msg MangleNames))
fromCName fc _ (C.Name cName) =
    case mFixed of
      Just (HsName hsName) -> (HsIdentifier hsName, Nothing)
      Nothing -> (HsIdentifier "", Just $ MangleNamesCouldNotMangle cName)
  where
    mFixed :: Maybe (HsName ns)
    mFixed = FixCandidate.fixCandidate fc cName

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
    , MonadState [MangleNamesMsg]
    , MonadReader Env
    )

data Env = Env{
      envNameMap      :: NameMap
    , envFixCandidate :: FixCandidate Maybe
    }

runM :: FixCandidate Maybe -> NameMap -> M a -> (a, [Msg MangleNames])
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
  mangle :: a HandleTypedefs -> M (a MangleNames)

class MangleDecl a where
  mangleDecl ::
       C.DeclInfo MangleNames
    -> a HandleTypedefs -> M (a MangleNames)

mangleQualName :: C.QualName -> C.NameOrigin -> M (NamePair, C.NameOrigin)
mangleQualName cQualName@(C.QualName cName _namespace) nameOrigin = do
    nm <- asks envNameMap
    case Map.lookup cQualName nm of
      Just hsName -> return (NamePair cName hsName, nameOrigin)
      Nothing     -> do
        -- NB: We did not register any declaration with the given ID. This is
        -- most likely because the user did not select the declaration. If the
        -- declaration was completely missing, Clang would have complained
        -- already.
        modify (MangleNamesMissingDeclaration cQualName :)
        -- Use a fake Haskell ID.
        return (NamePair cName (HsIdentifier "MissingDeclaration"), nameOrigin)

{-------------------------------------------------------------------------------
  Additional name mangling functionality

  TODO: Perhaps some (or all) of this should be configurable.
-------------------------------------------------------------------------------}

mangleFieldName :: C.DeclInfo MangleNames -> C.Name -> M NamePair
mangleFieldName info fieldCName = do
    fc <- asks envFixCandidate
    let candidate = declCName <> "_" <> fieldCName
    let (fieldHsName, mError) = fromCName fc (Proxy @NsVar) candidate
    forM_ mError $ modify . (:)
    return $ NamePair fieldCName fieldHsName
  where
    C.DeclInfo{declId = (NamePair declCName _declHsName, _origin)} = info

-- | Mangle enum constant name
--
-- Since these live in the global namespace, we do not prepend the name of
-- the enclosing enum.
mangleEnumConstant :: C.DeclInfo MangleNames -> C.Name -> M NamePair
mangleEnumConstant _info cName = do
    fc <- asks envFixCandidate
    let (hsName, mError) = fromCName fc (Proxy @NsConstr) cName
    forM_ mError $ modify . (:)
    return $ NamePair cName hsName

-- | Struct names
--
-- Right now we reuse the name of the type also for the constructor.
mkStructNames :: C.DeclInfo MangleNames -> RecordNames
mkStructNames info = RecordNames{
      recordConstr = nameHs namePair
    }
  where
    C.DeclInfo{declId = (namePair, _origin)} = info

-- | Generic construction of newtype names, given only the type name
mkNewtypeNames :: C.DeclInfo MangleNames -> NewtypeNames
mkNewtypeNames info = NewtypeNames{
      newtypeConstr = nameHs namePair
    , newtypeField  = "un_" <> nameHs namePair
    }
  where
    C.DeclInfo{declId = (namePair, _origin)} = info

-- | Union names
--
-- A union is represented by a newtype around the raw bytes.
mkUnionNames :: C.DeclInfo MangleNames -> NewtypeNames
mkUnionNames = mkNewtypeNames

-- | Enum names
--
-- An enum is represented by a newtype around an integral value.
mkEnumNames :: C.DeclInfo MangleNames -> NewtypeNames
mkEnumNames = mkNewtypeNames

-- | Typedef
--
-- Typedefs are represented by newtypes
mkTypedefNames :: C.DeclInfo MangleNames -> NewtypeNames
mkTypedefNames = mkNewtypeNames

-- | Macro types
--
-- These behave like typedefs.
mkMacroTypeNames :: C.DeclInfo MangleNames -> NewtypeNames
mkMacroTypeNames = mkNewtypeNames

-- | Mangle function argument name
--
-- Function argument names are not really used when generating Haskell code.
-- They are more relevant for documentation purposes so we don't do any
-- mangling.
mangleArgumentName :: C.Name -> M NamePair
mangleArgumentName argName = do
    fc <- asks envFixCandidate
    let (hsName, mError) = fromCName fc (Proxy @NsVar) argName
    forM_ mError $ modify . (:)
    return $ NamePair argName hsName

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance Mangle C.TranslationUnit where
  mangle C.TranslationUnit{..} = do
      let mk :: [C.Decl MangleNames] -> C.TranslationUnit MangleNames
          mk decls = C.TranslationUnit{unitDecls = decls, ..}
      mk <$> mapM mangle unitDecls

instance Mangle C.Decl where
  mangle decl = do
      declId' <- mangleQualName (C.declQualName decl) (C.declIdOrigin declId)
      declComment' <- traverse mangle declComment

      let info :: C.DeclInfo MangleNames
          info = C.DeclInfo{ declId = declId'
                           , declComment = declComment'
                           , ..
                           }

          mk :: C.DeclKind MangleNames -> C.Decl MangleNames
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
  mangleDecl _ (C.DeclGlobal ty) =
      C.DeclGlobal <$> mangle ty
  mangleDecl _ (C.DeclConst ty) =
      C.DeclConst <$> mangle ty

instance Mangle C.Reference where
  mangle (C.ById C.DeclId{..}) = do
    nm <- asks envNameMap
    let lookupResults =
          catMaybes [ Map.lookup (C.QualName declIdName nameKind) nm
                    | nameKind <- [minBound .. maxBound]
                    ]
    case lookupResults of
      (hsName:_) -> return $ C.ById (NamePair declIdName hsName, declIdOrigin)
      []         -> do
        -- NB: If we hit this case it means that we tried all possible name
        -- kinds and still didn't find any result. This might be because of a
        -- typo on the docs or a miss reference.
        --
        modify (MangleNamesMissingDeclaration (C.QualName declIdName C.NameKindOrdinary) :)
        --
        -- Use the fake Haskell ID.
        return $ C.ById $ (NamePair declIdName (HsIdentifier "Missing declaration"), declIdOrigin)

instance Mangle C.CommentReference where
  mangle (C.CommentReference C.Comment{..}) =
        C.CommentReference
    .   C.Comment commentCName
    <$> traverse mangleCommentBlockContent commentChildren

    where
      mangleCommentInlineContent = \case
        C.TextContent{..}        -> pure $ C.TextContent {..}
        C.InlineCommand n r args -> do
          args' <- traverse ( (\case
                                Left t -> pure (Left t)
                                Right t -> fmap Right t
                              )
                            . fmap mangle) args
          pure (C.InlineCommand n r args')
        C.HtmlStartTag{..}       -> pure $ C.HtmlStartTag{..}
        C.HtmlEndTag{..}         -> pure $ C.HtmlEndTag{..}

      mangleCommentBlockContent = \case
        C.Paragraph p              -> C.Paragraph <$> traverse mangleCommentInlineContent p
        C.BlockCommand n args p    -> C.BlockCommand n args <$> (traverse mangleCommentInlineContent p)
        C.ParamCommand n i d e p   -> C.ParamCommand n i d e <$> (traverse mangleCommentBlockContent p)
        C.TParamCommand n p c      -> C.TParamCommand n p <$> (traverse mangleCommentBlockContent c)
        C.VerbatimBlockCommand{..} -> pure C.VerbatimBlockCommand{..}
        C.VerbatimLine{..}         -> pure C.VerbatimLine{..}

instance MangleDecl C.Struct where
  mangleDecl info C.Struct{..} = do
      let mk :: [C.StructField MangleNames] -> C.Struct MangleNames
          mk structFields' = C.Struct{
                structFields = structFields'
              , structAnn    = mkStructNames info
              , ..
              }
      mk <$> mapM (mangleDecl info) structFields

instance MangleDecl C.StructField where
  mangleDecl info C.StructField{..} = do
      let mk ::
               FieldName MangleNames
            -> C.Type MangleNames
            -> Maybe (C.CommentReference MangleNames)
            -> C.StructField MangleNames
          mk   structFieldName' structFieldType' structFieldComment' =
            C.StructField {
                structFieldName = structFieldName'
              , structFieldType = structFieldType'
              , structFieldComment = structFieldComment'
              , ..
              }
      mk <$> mangleFieldName info structFieldName
         <*> mangle structFieldType
         <*> traverse mangle structFieldComment

instance MangleDecl C.Union where
  mangleDecl info C.Union{..} = do
      let mk :: [C.UnionField MangleNames] -> C.Union MangleNames
          mk unionFields' = C.Union{
                unionFields = unionFields'
              , unionAnn    = mkUnionNames info
              , ..
              }
      mk <$> mapM (mangleDecl info) unionFields

instance MangleDecl C.UnionField where
  mangleDecl info C.UnionField{..} = do
      let mk ::
               FieldName MangleNames
            -> C.Type MangleNames
            -> Maybe (C.CommentReference MangleNames)
            -> C.UnionField MangleNames
          mk unionFieldName' unionFieldType' unionFieldComment' =
            C.UnionField {
                unionFieldName = unionFieldName'
              , unionFieldType = unionFieldType'
              , unionFieldComment = unionFieldComment'
              , ..
              }
      mk <$> mangleFieldName info unionFieldName
         <*> mangle unionFieldType
         <*> traverse mangle unionFieldComment

instance MangleDecl C.Enum where
  mangleDecl info C.Enum{..} = do
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
         <*> mapM (mangleDecl info) enumConstants

instance MangleDecl C.EnumConstant where
  mangleDecl info C.EnumConstant{..} = do
      let mk :: NamePair
             -> Maybe (C.CommentReference MangleNames)
             -> C.EnumConstant MangleNames
          mk enumConstantName' enumConstantComment' = C.EnumConstant{
                enumConstantName = enumConstantName'
              , enumConstantComment = enumConstantComment'
              , ..
              }
      mk <$> mangleEnumConstant info enumConstantName
         <*> traverse mangle enumConstantComment

instance MangleDecl C.Typedef where
  mangleDecl info C.Typedef{..} = do
      let mk :: C.Type MangleNames -> C.Typedef MangleNames
          mk typedefType' = C.Typedef{
                typedefType = typedefType'
              , typedefAnn  = mkTypedefNames info
              , ..
              }
      mk <$> mangle typedefType

instance MangleDecl C.Function where
  mangleDecl _info C.Function{..} = do
      let mk ::
               [(Maybe NamePair, C.Type MangleNames)]
            -> C.Type MangleNames
            -> C.Function MangleNames
          mk functionArgs' functionRes' = C.Function{
                functionArgs = functionArgs'
              , functionRes  = functionRes'
              , ..
              }
      mk <$> mapM (bimapM (traverse mangleArgumentName) mangle) functionArgs
         <*> mangle functionRes

instance MangleDecl C.CheckedMacro where
  mangleDecl info = \case
      C.MacroType typ  -> C.MacroType <$> mangleDecl info typ
      C.MacroExpr expr -> return $ C.MacroExpr expr

instance MangleDecl C.CheckedMacroType where
  mangleDecl info C.CheckedMacroType{..} = do
    let mk :: C.Type MangleNames -> C.CheckedMacroType MangleNames
        mk macroType' = C.CheckedMacroType{
               macroType    = macroType'
             , macroTypeAnn = mkMacroTypeNames info
             , ..
             }
    mk <$> mangle macroType

instance Mangle C.Type where
  mangle = \case
      C.TypeStruct C.DeclId{..} -> C.TypeStruct <$>
        mangleQualName
          (C.QualName declIdName (C.NameKindTagged C.TagKindStruct))
          declIdOrigin
      C.TypeUnion C.DeclId{..} -> C.TypeUnion <$>
        mangleQualName
          (C.QualName declIdName (C.NameKindTagged C.TagKindUnion))
          declIdOrigin
      C.TypeEnum C.DeclId{..} -> C.TypeEnum <$>
        mangleQualName
          (C.QualName declIdName (C.NameKindTagged C.TagKindEnum))
          declIdOrigin
      C.TypeMacroTypedef C.DeclId{..} -> C.TypeMacroTypedef <$>
        mangleQualName (C.QualName declIdName C.NameKindOrdinary) declIdOrigin

      -- Recursive cases
      C.TypeTypedef ref         -> C.TypeTypedef <$> mangle ref
      C.TypePointer typ         -> C.TypePointer <$> mangle typ
      C.TypeFun args res        -> C.TypeFun <$> mapM mangle args <*> mangle res
      C.TypeConstArray n typ    -> C.TypeConstArray n <$> mangle typ
      C.TypeIncompleteArray typ -> C.TypeIncompleteArray <$> mangle typ
      C.TypeBlock typ           -> C.TypeBlock <$> mangle typ

      -- The other entries do not need any name mangling
      C.TypePrim prim      -> return $ C.TypePrim prim
      C.TypeVoid           -> return $ C.TypeVoid
      C.TypeExtBinding ext -> return $ C.TypeExtBinding ext

instance Mangle RenamedTypedefRef where
  mangle (TypedefRegular C.DeclId{..}) = TypedefRegular <$>
    mangleQualName (C.QualName declIdName C.NameKindOrdinary) declIdOrigin
  mangle (TypedefSquashed cName ty) =
    TypedefSquashed cName <$> mangle ty

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

withDeclNamespace ::
     C.DeclKind HandleTypedefs
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
      C.DeclGlobal{}       -> k (Proxy @NsVar)
      C.DeclConst{}        -> k (Proxy @NsVar)

      C.DeclMacro macro ->
        case macro of
          C.MacroType{} -> k (Proxy @NsTypeConstr)
          C.MacroExpr{} -> k (Proxy @NsVar)
