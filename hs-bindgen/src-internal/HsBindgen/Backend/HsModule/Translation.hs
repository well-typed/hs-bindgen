{-# LANGUAGE RecordWildCards #-}

module HsBindgen.Backend.HsModule.Translation (
    -- * GhcPragma
    GhcPragma (..)
    -- * ImportListItem
  , ImportListItem(..)
    -- * HsModule
  , HsModule(..)
    -- * Translation
  , translateModuleMultiple
  , translateModuleSingle
  ) where

import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import HsBindgen.Backend.Category
import HsBindgen.Backend.Extensions
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type qualified as Hs
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.HsModule.Capi (capiImport)
import HsBindgen.Backend.HsModule.Names
import HsBindgen.Backend.SHs.AST
import HsBindgen.Config.Prelims
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

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
      hsModulePragmas   :: [GhcPragma]
    , hsModuleName      ::  Hs.ModuleName
    , hsModuleImports   :: [ImportListItem]
    , hsModuleCWrappers :: [CWrapper]
    , hsModuleDecls     :: [SDecl]
    }

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

translateModuleMultiple ::
     BaseModuleName
  -> ByCategory_ ([CWrapper], [SDecl])
  -> ByCategory_ (Maybe HsModule)
translateModuleMultiple moduleBaseName declsByCat =
    mapWithCategory_ go declsByCat
  where
    go :: Category -> ([CWrapper], [SDecl]) -> Maybe HsModule
    go _ ([], []) = Nothing
    go cat xs     = Just $ translateModule' (Just cat) moduleBaseName xs

translateModuleSingle ::
     BaseModuleName
  -> ByCategory_ ([CWrapper], [SDecl])
  -> HsModule
translateModuleSingle name declsByCat =
    translateModule' Nothing name $ Foldable.fold declsByCat

translateModule' ::
     Maybe Category
  -> BaseModuleName
  -> ([CWrapper], [SDecl])
  -> HsModule
translateModule' mcat moduleBaseName (hsModuleCWrappers, hsModuleDecls) =
    let hsModulePragmas =
          resolvePragmas hsModuleCWrappers hsModuleDecls
        hsModuleImports =
          resolveImports moduleBaseName mcat hsModuleCWrappers hsModuleDecls
        hsModuleName = fromBaseModuleName moduleBaseName mcat
    in  HsModule{..}

{-------------------------------------------------------------------------------
  Auxiliary: Pragma resolution
-------------------------------------------------------------------------------}

resolvePragmas :: [CWrapper] -> [SDecl] -> [GhcPragma]
resolvePragmas wrappers ds =
    Set.toAscList . mconcat $
      haddockPrunePragmas : userlandCapiPragmas : constPragmas : map resolveDeclPragmas ds
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
     BaseModuleName
  -> Maybe Category
  -> [CWrapper]
  -> [SDecl]
  -> [ImportListItem]
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
      Nothing    -> mempty
      Just CType -> mempty
      _otherCat  ->
        let base = HsImportModule{
                hsImportModuleName  = fromBaseModuleName baseModule (Just CType)
              , hsImportModuleAlias = Nothing
              }
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
    DInst Instance{..} -> mconcat $
         [resolveGlobalImports instanceClass]
      ++ map resolveTypeImports instanceArgs
      ++ concat [
             resolveGlobalImports c : map resolveTypeImports ts
           | (c, ts) <- instanceSuperClasses
           ]
      ++ concat [
             resolveGlobalImports t : resolveTypeImports r : map resolveTypeImports as
           | (t, as, r) <- instanceTypes
           ]
      ++ concat [
            [resolveGlobalImports f, resolveExprImports e]
          | (f, e) <- instanceDecs
          ]
    DRecord Record{..} -> mconcat [
        mconcat $ map (resolveTypeImports . fieldType) dataFields
      , resolveNestedDeriv dataDeriv
      ]
    DEmptyData _name -> mempty
    DNewtype Newtype{..} -> mconcat [
        resolveTypeImports $ fieldType newtypeField
      , resolveNestedDeriv newtypeDeriv
      ]
    DDerivingInstance DerivingInstance{..} -> resolveStrategyImports derivingInstanceStrategy
                                            <> resolveTypeImports derivingInstanceType
    DForeignImport ForeignImport{..} ->
         foldMap (resolveTypeImports . (.typ)) foreignImportParameters
      <> resolveTypeImports foreignImportResult.typ
    DBinding Binding{..} ->
         foldMap (resolveTypeImports . (.typ)) parameters
      <> resolveTypeImports result.typ
      <> resolveExprImports body
    DPatternSynonym PatternSynonym {..} ->
        resolveTypeImports patSynType <>
        resolvePatExprImports patSynRHS

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
    EUnboxedIntegral _ -> mempty
    EChar {} -> mconcat $ map resolveGlobalImports
                  [ CharValue_tycon
                  , CharValue_constructor
                  , CharValue_fromAddr
                  , Maybe_just
                  , Maybe_nothing
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
      : [ case alt of
            SAlt _con _add _hints body -> resolveExprImports body
            SAltNoConstr _hints body -> resolveExprImports body
            SAltUnboxedTuple _add _hints body -> resolveExprImports body
        | alt <- alts
        ]
    ETup xs -> foldMap resolveExprImports xs
    EUnboxedTup xs -> foldMap resolveExprImports xs
    EList xs -> foldMap resolveExprImports xs
    ETypeApp f t -> resolveExprImports f <> resolveTypeImports t

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
    TFree _ -> mempty
    TLit _n -> mempty
    TStrLit _s -> mempty
    TExt ref _cTypeSpec _hsTypeSpec -> resolveExtHsRefImports ref
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

resolveExtHsRefImports :: Hs.ExtRef -> ImportAcc
resolveExtHsRefImports Hs.ExtRef{..} =
    let hsImportModule = HsImportModule {
            hsImportModuleName  = extRefModule
          , hsImportModuleAlias = Nothing
          }
    in  ImportAcc False (Set.singleton hsImportModule) mempty
