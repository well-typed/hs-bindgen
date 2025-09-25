{-# LANGUAGE RecordWildCards #-}

module HsBindgen.Backend.HsModule.Translation (
    -- * GhcPragma
    GhcPragma (..)
    -- * ImportListItem
  , ImportListItem(..)
    -- * HsModule
  , HsModule(..)
    -- * Translation
  , HsModuleOpts(..)
  , translateModuleMultiple
  , translateModuleSingle
  , mergeDecls
  ) where

import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

import HsBindgen.Backend.Extensions
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type qualified as Hs
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.HsModule.Capi (capiImport)
import HsBindgen.Backend.HsModule.Names
import HsBindgen.Backend.SHs.AST
import HsBindgen.Imports
import HsBindgen.Language.Haskell

{-------------------------------------------------------------------------------
  GhcPragma
-------------------------------------------------------------------------------}

-- | GHC Pragma
--
-- Example: @LANGUAGE NoImplicitPrelude@
newtype GhcPragma = GhcPragma { unGhcPragma :: String }
  deriving newtype (Eq, Ord, IsString)

{-------------------------------------------------------------------------------
  ImportListItem
-------------------------------------------------------------------------------}

-- | Import list item
data ImportListItem =
    QualifiedImportListItem   HsImportModule
    -- An empty import list (@Just []@) means "import instances only", and
    -- differs greatly from having no import list (@Nothing@), which is an open,
    -- unqualified import.
  | UnqualifiedImportListItem HsImportModule (Maybe [ResolvedName])
  deriving stock (Eq, Ord)

{-------------------------------------------------------------------------------
  HsModule
-------------------------------------------------------------------------------}

-- | Haskell module
data HsModule = HsModule {
      hsModulePragmas              :: [GhcPragma]
    , hsModuleName                 :: String
    , hsModuleImports              :: [ImportListItem]
    , hsModuleUserlandCapiWrappers :: [UserlandCapiWrapper]
    , hsModuleDecls                :: [SDecl]
    }

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

data HsModuleOpts = HsModuleOpts {
      hsModuleOptsBaseName  :: HsModuleName
    }
  deriving stock (Show, Eq, Generic)

instance Default HsModuleOpts where
  def = HsModuleOpts {
      hsModuleOptsBaseName  = "Generated"
    }

translateModuleMultiple ::
  HsModuleName -> ByCategory ([UserlandCapiWrapper], [SDecl]) -> ByCategory HsModule
translateModuleMultiple moduleBaseName declsByCat =
  mapByCategory go declsByCat
  where
    go :: BindingCategory -> ([UserlandCapiWrapper], [SDecl]) -> HsModule
    go cat (wrappers, decls) =
      translateModule' (Just cat) moduleBaseName wrappers decls

translateModuleSingle ::
     Safety -> HsModuleName -> ByCategory ([UserlandCapiWrapper], [SDecl])
  -> HsModule
translateModuleSingle safety name declsByCat =
  translateModule' Nothing name wrappers decls
  where
    wrappers :: [UserlandCapiWrapper]
    decls :: [SDecl]
    (wrappers, decls) = mergeDecls safety declsByCat

mergeDecls ::
  Safety
  -> ByCategory ([UserlandCapiWrapper], [SDecl])
  -> ([UserlandCapiWrapper], [SDecl])
mergeDecls safety declsByCat =
    Foldable.fold $ ByCategory $ removeSafetyCategory $ unByCategory declsByCat
  where
    safetyToRemove = case safety of
      Safe   -> BUnsafe
      Unsafe -> BSafe
    removeSafetyCategory = Map.filterWithKey (\k _ -> k /= safetyToRemove)

translateModule' ::
  Maybe BindingCategory -> HsModuleName -> [UserlandCapiWrapper] -> [SDecl] -> HsModule
translateModule' mcat moduleBaseName hsModuleUserlandCapiWrappers hsModuleDecls =
    let hsModulePragmas =
          resolvePragmas hsModuleUserlandCapiWrappers hsModuleDecls
        hsModuleImports =
          resolveImports moduleBaseName mcat hsModuleUserlandCapiWrappers hsModuleDecls
        addSubModule = case mcat of
          Nothing       -> id
          Just BType    -> id
          Just otherCat -> (<> ('.' : displayBindingCategory otherCat))
        hsModuleName = addSubModule $ Text.unpack $ getHsModuleName moduleBaseName
    in  HsModule{..}

{-------------------------------------------------------------------------------
  Auxiliary: Pragma resolution
-------------------------------------------------------------------------------}

resolvePragmas :: [UserlandCapiWrapper] -> [SDecl] -> [GhcPragma]
resolvePragmas wrappers ds =
    Set.toAscList . mconcat $ haddockPrunePragmas : userlandCapiPragmas : constPragmas : map resolveDeclPragmas ds
  where
    constPragmas :: Set GhcPragma
    constPragmas = Set.singleton "LANGUAGE NoImplicitPrelude"

    userlandCapiPragmas :: Set GhcPragma
    userlandCapiPragmas = case wrappers of
      []  -> Set.empty
      _xs -> Set.singleton "LANGUAGE TemplateHaskell"

    haddockPrunePragmas :: Set GhcPragma
    haddockPrunePragmas = case wrappers of
      []  -> Set.empty
      _xs -> Set.singleton "OPTIONS_HADDOCK prune"

resolveDeclPragmas :: SDecl -> Set GhcPragma
resolveDeclPragmas decl =
    Set.map f (requiredExtensions decl)
  where
    f ext = GhcPragma ("LANGUAGE " ++ show ext)

{-------------------------------------------------------------------------------
  Auxiliary: Import resolution
-------------------------------------------------------------------------------}

-- | Resolve imports in a list of declarations
resolveImports ::
  HsModuleName -> Maybe BindingCategory -> [UserlandCapiWrapper] -> [SDecl] -> [ImportListItem]
resolveImports baseModule cat wrappers ds =
    let ImportAcc requiresTypeModule qs us = mconcat $ map resolveDeclImports ds
    in  Set.toAscList . mconcat $
            bindingCatImport requiresTypeModule
          : Set.map QualifiedImportListItem (userlandCapiImport <> qs)
          : map (Set.singleton . uncurry mkUImportListItem) (Map.toList us)
  where
    mkUImportListItem :: HsImportModule -> Set ResolvedName -> ImportListItem
    mkUImportListItem imp xs = UnqualifiedImportListItem imp (Just $ Set.toAscList xs)

    bindingCatImport :: Bool -> Set ImportListItem
    bindingCatImport False = mempty
    bindingCatImport True = case cat of
      Nothing      -> mempty
      (Just BType) -> mempty
      _otherCat ->
        let base = HsImportModule (Text.unpack $ getHsModuleName baseModule) Nothing
        in  Set.singleton $ UnqualifiedImportListItem base Nothing
    userlandCapiImport :: Set HsImportModule
    userlandCapiImport = case wrappers of
      []  -> mempty
      _xs -> Set.singleton capiImport

-- | Accumulator for resolving imports
--
-- Both qualified imports and unqualified imports are accumulated.
data ImportAcc = ImportAcc {
      _importAccRequireTypeModule :: Bool
    , _importAccQualified         :: Set HsImportModule
    , _importAccUnqualified       :: Map HsImportModule (Set ResolvedName)
    }

instance Semigroup ImportAcc where
  ImportAcc tL qL uL <> ImportAcc tR qR uR =
    ImportAcc (tL || tR) (qL <> qR) (Map.unionWith (<>) uL uR)

instance Monoid ImportAcc where
  mempty = ImportAcc False mempty mempty

-- | Resolve imports in a declaration
resolveDeclImports :: SDecl -> ImportAcc
resolveDeclImports = \case
    DVar Var {..} ->
      resolveTypeImports varType <> resolveExprImports varExpr
    DInst Instance{..} -> mconcat $
         [resolveGlobalImports instanceClass]
      ++ map resolveTypeImports instanceArgs
      ++ map (resolveGlobalImports . fst) instanceDecs
      ++ map (resolveExprImports . snd) instanceDecs
    DRecord Record{..} -> mconcat [
        mconcat $ map (resolveTypeImports . fieldType) dataFields
      , resolveNestedDeriv dataDeriv
      ]
    DEmptyData _name -> mempty
    DNewtype Newtype{..} -> mconcat [
        resolveTypeImports $ fieldType newtypeField
      , resolveNestedDeriv newtypeDeriv
      ]
    DDerivingInstance DerivingInstance {..} -> resolveStrategyImports derivingInstanceStrategy
                                            <> resolveTypeImports derivingInstanceType
    DForeignImport ForeignImport {..} ->
         foldMap (resolveTypeImports . functionParameterType)
                 foreignImportParameters
      <> resolveTypeImports (Hs.extractResultType foreignImportResultType)
    DPatternSynonym PatternSynonym {..} ->
        resolveTypeImports patSynType <>
        resolvePatExprImports patSynRHS
    DPragma {} -> mempty

-- | Resolve nested deriving clauses (part of a datatype declaration)
resolveNestedDeriv :: [(Hs.Strategy ClosedType, [Global])] -> ImportAcc
resolveNestedDeriv = mconcat . map aux
  where
    aux :: (Hs.Strategy ClosedType, [Global]) -> ImportAcc
    aux (strategy, cls) = mconcat $
          resolveStrategyImports strategy
        : map resolveGlobalImports cls

-- | Resolve global imports
resolveGlobalImports :: Global -> ImportAcc
resolveGlobalImports g = case resolveGlobal g of
    n@ResolvedName{..} -> case resolvedNameImport of
      Nothing -> ImportAcc False mempty mempty
      Just (QualifiedHsImport hsImportModule) ->
        ImportAcc False (Set.singleton hsImportModule) mempty
      Just (UnqualifiedHsImport hsImportModule) ->
        ImportAcc False mempty (Map.singleton hsImportModule (Set.singleton n))

-- | Resolve imports in an expression
resolveExprImports :: SExpr ctx -> ImportAcc
resolveExprImports = \case
    EGlobal g -> resolveGlobalImports g
    EBound _x -> mempty
    EFree {} -> mempty
    ECon _n -> mempty
    EIntegral _ t -> maybe mempty resolvePrimTypeImports t
    EChar {} -> mconcat $ map resolveGlobalImports
                  [ CharValue_tycon
                  , CharValue_constructor
                  , CharValue_fromAddr
                  ]
    EString {} -> mempty
    ECString {} -> resolvePrimTypeImports Hs.HsPrimCStringLen
                <> resolveGlobalImports Ptr_constructor
    EFloat _ t -> resolvePrimTypeImports t
    EDouble _ t -> resolvePrimTypeImports t
    EApp f x -> resolveExprImports f <> resolveExprImports x
    EInfix op x y ->
      resolveGlobalImports op <> resolveExprImports x <> resolveExprImports y
    ELam _mPat body -> resolveExprImports body
    EUnusedLam body -> resolveExprImports body
    ECase x alts -> mconcat $
        resolveExprImports x
      : [ resolveExprImports body
        | SAlt _con _add _hints body <- alts
        ]
    ETup xs -> foldMap resolveExprImports xs
    EList xs -> foldMap resolveExprImports xs

-- | Resolve imports in a pattern|expression
resolvePatExprImports :: PatExpr -> ImportAcc
resolvePatExprImports = \case
    PEApps _n xs -> foldMap resolvePatExprImports xs
    PELit _      -> mempty

-- | Resolve imports in a type
resolveTypeImports :: SType ctx -> ImportAcc
resolveTypeImports = \case
    TGlobal g -> resolveGlobalImports g
    TCon _n -> ImportAcc True mempty mempty
    TLit _n -> mempty
    TExt ref _typeSpec -> resolveExtHsRefImports ref
    TApp c x -> resolveTypeImports c <> resolveTypeImports x
    TFun a b -> resolveTypeImports a <> resolveTypeImports b
    TBound {} -> mempty
    TForall _hints _qtvs ctxt body ->
      foldMap resolveTypeImports (body:ctxt)

resolvePrimTypeImports :: Hs.HsPrimType -> ImportAcc
resolvePrimTypeImports = resolveGlobalImports . PrimType

resolveStrategyImports :: Hs.Strategy ClosedType -> ImportAcc
resolveStrategyImports = \case
    Hs.DeriveNewtype -> mempty
    Hs.DeriveStock -> mempty
    Hs.DeriveVia ty -> resolveTypeImports ty

resolveExtHsRefImports :: ExtHsRef -> ImportAcc
resolveExtHsRefImports ExtHsRef{..} =
    let hsImportModule = HsImportModule {
            hsImportModuleName  = Text.unpack $ getHsModuleName extHsRefModule
          , hsImportModuleAlias = Nothing
          }
    in  ImportAcc False (Set.singleton hsImportModule) mempty
