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

import HsBindgen.Backend.Common
import HsBindgen.Backend.Common.Translation
import HsBindgen.Backend.PP
import HsBindgen.C.AST qualified as C
import HsBindgen.Translation.LowLevel

{-------------------------------------------------------------------------------
  ImportListItem
-------------------------------------------------------------------------------}

-- | Import list item
data ImportListItem =
    UnqualifiedImportListItem HsImport [ResolvedName]
  | QualifiedImportListItem   HsImport
  -- NOTE constructor order affects order of imports in generated code
  deriving (Eq, Ord)

{-------------------------------------------------------------------------------
  HsModule
-------------------------------------------------------------------------------}

-- | Haskell module
data HsModule = HsModule {
      hsModuleName    :: String
    , hsModuleImports :: [ImportListItem]
    , hsModuleDecls   :: [SDecl BE]
    }

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

newtype HsModuleOpts = HsModuleOpts {
      hsModuleOptsName :: String
    }
  deriving (Show)

translate :: HsModuleOpts -> C.Header -> HsModule
translate HsModuleOpts{..} header =
    let hsModuleName = hsModuleOptsName
        (hsModuleDecls, _) = runM $ mapM (toBE BE) (generateDeclarations header)
        hsModuleImports = resolveImports hsModuleDecls
    in  HsModule{..}

{-------------------------------------------------------------------------------
  Auxiliary: Import resolution
-------------------------------------------------------------------------------}

-- | Resolve imports in a list of declarations
resolveImports :: [SDecl BE] -> [ImportListItem]
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
resolveDeclImports :: SDecl BE -> ImportAcc
resolveDeclImports = \case
    DVar _name e -> resolveExprImports e
    DInst Instance{..} -> mconcat $
        resolveGlobalImports instanceClass
      : map (resolveGlobalImports . fst) instanceDecs
      ++ map (resolveExprImports . snd) instanceDecs
    DRecord Record{..} -> mconcat $
      map (resolveTypeImports . snd) dataFields
    DNewtype Newtype{..} -> resolveTypeImports newtypeType

-- | Resolve global imports
resolveGlobalImports :: Global -> ImportAcc
resolveGlobalImports g = ImportAcc $ case resolveGlobal g of
    n@ResolvedName{..}
      | resolvedNameQualify -> (Set.singleton resolvedNameImport, mempty)
      | otherwise ->
          (mempty, Map.singleton resolvedNameImport (Set.singleton n))

-- | Resolve imports in an expression
resolveExprImports :: SExpr BE -> ImportAcc
resolveExprImports = \case
    EGlobal g -> resolveGlobalImports g
    EVar _x -> mempty
    ECon _n -> mempty
    EInt _i -> mempty
    EApp f x -> resolveExprImports f <> resolveExprImports x
    EInfix op x y ->
      resolveGlobalImports op <> resolveExprImports x <> resolveExprImports y
    ELam _mPat body -> resolveExprImports body
    ECase x ms -> mconcat $
        resolveExprImports x
      : map (\(_cnst, _params, body) -> resolveExprImports body) ms
    EInj x -> resolveExprImports x

-- | Resolve imports in a type
resolveTypeImports :: SType BE -> ImportAcc
resolveTypeImports = \case
    TGlobal g -> resolveGlobalImports g
    TCon _n -> mempty
    TApp c x -> resolveTypeImports c <> resolveTypeImports x
