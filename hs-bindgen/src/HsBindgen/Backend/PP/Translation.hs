{-# LANGUAGE RecordWildCards #-}

module HsBindgen.Backend.PP.Translation (
    -- * ImportListItem
    ImportListItem(..)
    -- * HsModule
  , HsModule(..)
    -- * Translation
  , HsModuleOpts(..)
  , translate
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import HsBindgen.SHs.AST
import HsBindgen.SHs.Translation qualified as SHs
import HsBindgen.Backend.PP
import HsBindgen.C.AST qualified as C
import HsBindgen.Hs.Translation

{-------------------------------------------------------------------------------
  ImportListItem
-------------------------------------------------------------------------------}

-- | Import list item
data ImportListItem =
    UnqualifiedImportListItem HsImport [ResolvedName]
  | QualifiedImportListItem   HsImport
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
      hsModuleName    :: String
    , hsModuleImports :: [ImportListItem]
    , hsModuleDecls   :: [SDecl]
    }

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

newtype HsModuleOpts = HsModuleOpts {
      hsModuleOptsName :: String
    }
  deriving stock (Show)

translate :: HsModuleOpts -> C.Header -> HsModule
translate HsModuleOpts{..} header =
    let hsModuleName = hsModuleOptsName
        hsModuleDecls = map SHs.translateDecl (generateDeclarations header)
        hsModuleImports = resolveImports hsModuleDecls
    in  HsModule{..}

{-------------------------------------------------------------------------------
  Auxiliary: Import resolution
-------------------------------------------------------------------------------}

-- | Resolve imports in a list of declarations
resolveImports :: [SDecl] -> [ImportListItem]
resolveImports ds =
    let (qs, us) = unImportAcc . mconcat $ map resolveDeclImports ds
    in  Set.toAscList . mconcat $
            Set.map QualifiedImportListItem qs
          : map (Set.singleton . uncurry mkUImportListItem) (Map.toList us)
  where
    mkUImportListItem :: HsImport -> Set ResolvedName -> ImportListItem
    mkUImportListItem imp = UnqualifiedImportListItem imp . Set.toAscList

-- | Accumulator for resolving imports
--
-- Both qualified imports and unqualified imports are accumulated.
newtype ImportAcc = ImportAcc {
      unImportAcc :: (Set HsImport, Map HsImport (Set ResolvedName))
    }

instance Semigroup ImportAcc where
  ImportAcc (qL, uL) <> ImportAcc (qR, uR) =
    ImportAcc (qL <> qR, Map.unionWith (<>) uL uR)

instance Monoid ImportAcc where
  mempty = ImportAcc (mempty, mempty)

-- | Resolve imports in a declaration
resolveDeclImports :: SDecl -> ImportAcc
resolveDeclImports = \case
    DVar _name mbTy e ->
      maybe mempty resolveTypeImports mbTy <> resolveExprImports e
    DInst Instance{..} -> mconcat $
        resolveGlobalImports instanceClass
      : map (resolveGlobalImports . fst) instanceDecs
      ++ map (resolveExprImports . snd) instanceDecs
    DRecord Record{..} -> mconcat $
      map (resolveTypeImports . snd) dataFields
    DEmptyData _name -> mempty
    DNewtype Newtype{..} -> resolveTypeImports newtypeType
    DDerivingNewtypeInstance ty -> resolveTypeImports ty
    DForeignImport ForeignImport {..} -> resolveTypeImports foreignImportType
    DPatternSynonym PatternSynonym {..} ->
        resolveTypeImports patSynType <>
        resolveExprImports patSynRHS

-- | Resolve global imports
resolveGlobalImports :: Global -> ImportAcc
resolveGlobalImports g = ImportAcc $ case resolveGlobal g of
    n@ResolvedName{..}
      | resolvedNameQualify -> (Set.singleton resolvedNameImport, mempty)
      | otherwise ->
          (mempty, Map.singleton resolvedNameImport (Set.singleton n))

-- | Resolve imports in an expression
resolveExprImports :: SExpr ctx -> ImportAcc
resolveExprImports = \case
    EGlobal g -> resolveGlobalImports g
    EBound _x -> mempty
    EFree {} -> mempty
    ECon _n -> mempty
    EIntegral {} -> mempty
    EFloat    {} -> mempty
    EDouble   {} -> mempty
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

-- | Resolve imports in a type
resolveTypeImports :: SType ctx -> ImportAcc
resolveTypeImports = \case
    TGlobal g -> resolveGlobalImports g
    TCon _n -> mempty
    TLit _n -> mempty
    TApp c x -> resolveTypeImports c <> resolveTypeImports x
    TFun a b -> resolveTypeImports a <> resolveTypeImports b
    TBound {} -> mempty
    TForall _hints _qtvs ctxt body ->
      foldMap resolveTypeImports (body:ctxt)
