{-# LANGUAGE RecordWildCards #-}

module HsBindgen.Backend.Artefact.HsModule.Translation (
    -- * GhcPragma
    GhcPragma (..)
    -- * ImportListItem
  , ImportListItem(..)
    -- * HsModule
  , HsModule(..)
    -- * Translation
  , HsModuleOpts(..)
  , HsModuleStructure(..)
  , translateModule
  ) where

import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

import HsBindgen.Backend.Artefact.HsModule.Names
import HsBindgen.Backend.Extensions
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type qualified as Hs
import HsBindgen.Backend.Hs.CallConv
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
    -- TODO_PR: We need to import the Type category unqualified and open.
    UnqualifiedImportListItem HsImportModule (Maybe [ResolvedName])
  | QualifiedImportListItem   HsImportModule
  deriving stock (Eq)

instance Ord ImportListItem where
  compare :: ImportListItem -> ImportListItem -> Ordering
  compare l r = case l of
    UnqualifiedImportListItem iL nsL -> case r of
      UnqualifiedImportListItem iR nsR -> case compare iL iR of
        EQ -> compare nsL nsR
        o  -> o
      QualifiedImportListItem iR -> case compare iL iR of
        EQ -> LT
        o  -> o
    QualifiedImportListItem iL -> case r of
      UnqualifiedImportListItem iR _nsR -> case compare iL iR of
        EQ -> GT
        o  -> o
      QualifiedImportListItem iR -> compare iL iR

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

data HsModuleStructure = ModulesByBindingCategory | SingleModule Safety
  deriving stock (Show, Eq, Generic)

instance Default HsModuleStructure where
  def = ModulesByBindingCategory

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

data HsModuleOpts = HsModuleOpts {
      hsModuleOptsBaseName  :: HsModuleName
    , hsModuleOptsStructure :: HsModuleStructure
    }
  deriving stock (Show, Eq, Generic)

instance Default HsModuleOpts where
  def = HsModuleOpts {
      hsModuleOptsBaseName  = "Generated"
    , hsModuleOptsStructure = def
    }

translateModule ::
  HsModuleOpts -> ByCategory ([UserlandCapiWrapper], [SDecl]) -> ByCategory HsModule
translateModule opts declsByCategory = case hsModuleOptsStructure opts of
  ModulesByBindingCategory -> mapByCategory go declsByCategory
  SingleModule safety      ->
    let safetyToRemove = case safety of
          Safe   -> BUnsafe
          Unsafe -> BSafe
        removeSafetyCategory = Map.filterWithKey (\k _ -> k /= safetyToRemove)
        decls = ByCategory $ removeSafetyCategory $ unByCategory declsByCategory

        foldedDecls :: ([UserlandCapiWrapper], [SDecl])
        foldedDecls = Foldable.fold decls

        -- TODO_PR: HACK, we are using `BType` as "single module binding
        -- category".
        mdl = go BType foldedDecls
    in  ByCategory $ Map.singleton BType mdl
  where
    go :: BindingCategory -> ([UserlandCapiWrapper], [SDecl]) -> HsModule
    go cat (hsModuleUserlandCapiWrappers, hsModuleDecls) =
      let hsModulePragmas =
            resolvePragmas hsModuleUserlandCapiWrappers hsModuleDecls
          hsModuleImports =
            resolveImports moduleBaseName cat hsModuleUserlandCapiWrappers hsModuleDecls
          addSubModule = case cat of
            BType   -> id
            BSafe   -> (<> ".Safe")
            BUnsafe -> (<> ".Unsafe")
            BFunPtr -> (<> ".FunPtr")
            BGlobal -> (<> ".Global")
          hsModuleName = addSubModule $ Text.unpack $ getHsModuleName moduleBaseName
      in  HsModule{..}

    moduleBaseName :: HsModuleName
    moduleBaseName = hsModuleOptsBaseName opts

{-------------------------------------------------------------------------------
  Auxiliary: Pragma resolution
-------------------------------------------------------------------------------}

resolvePragmas :: [UserlandCapiWrapper] -> [SDecl] -> [GhcPragma]
resolvePragmas wrappers ds =
    Set.toAscList . mconcat $ userlandCapiPragmas : constPragmas : map resolveDeclPragmas ds
  where
    constPragmas :: Set GhcPragma
    constPragmas = Set.singleton "LANGUAGE NoImplicitPrelude"

    userlandCapiPragmas :: Set GhcPragma
    userlandCapiPragmas = case wrappers of
      []  -> Set.empty
      _xs -> Set.singleton "LANGUAGE TemplateHaskell"

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
  HsModuleName -> BindingCategory -> [UserlandCapiWrapper] -> [SDecl] -> [ImportListItem]
resolveImports baseModule cat wrappers ds =
    let (qs, us) = unImportAcc . mconcat $ map resolveDeclImports ds
    in  Set.toAscList . mconcat $
            bindingCatImport
          : Set.map QualifiedImportListItem (userlandCapiImport <> qs)
          : map (Set.singleton . uncurry mkUImportListItem) (Map.toList us)
  where
    mkUImportListItem :: HsImportModule -> Set ResolvedName -> ImportListItem
    mkUImportListItem imp xs = UnqualifiedImportListItem imp (Just $ Set.toAscList xs)

    bindingCatImport :: Set ImportListItem
    bindingCatImport = case cat of
      BType     -> mempty
      _otherCat ->
        let mdl = HsImportModule (Text.unpack $ getHsModuleName baseModule) Nothing
        in  Set.singleton $ UnqualifiedImportListItem mdl Nothing

    userlandCapiImport :: Set HsImportModule
    userlandCapiImport = case wrappers of
      []  -> mempty
      _xs -> let mdl = HsImportModule "HsBindgen.Runtime.CAPI" (Just "CAPI")
             in  Set.singleton mdl

-- | Accumulator for resolving imports
--
-- Both qualified imports and unqualified imports are accumulated.
newtype ImportAcc = ImportAcc {
      unImportAcc :: (Set HsImportModule, Map HsImportModule (Set ResolvedName))
    }

instance Semigroup ImportAcc where
  ImportAcc (qL, uL) <> ImportAcc (qR, uR) =
    ImportAcc (qL <> qR, Map.unionWith (<>) uL uR)

instance Monoid ImportAcc where
  mempty = ImportAcc (mempty, mempty)

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
resolveGlobalImports g = ImportAcc $ case resolveGlobal g of
    n@ResolvedName{..} -> case resolvedNameImport of
      Nothing -> (mempty, mempty)
      Just (QualifiedHsImport hsImportModule) ->
        (Set.singleton hsImportModule, mempty)
      Just (UnqualifiedHsImport hsImportModule) ->
        (mempty, Map.singleton hsImportModule (Set.singleton n))

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
    TCon _n -> mempty
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
    in  ImportAcc (Set.singleton hsImportModule, mempty)
