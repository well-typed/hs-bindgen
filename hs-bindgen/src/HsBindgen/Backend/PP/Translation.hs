{-# LANGUAGE RecordWildCards #-}

module HsBindgen.Backend.PP.Translation (
    -- * GhcPragma
    GhcPragma
    -- * ImportListItem
  , ImportListItem(..)
    -- * HsModule
  , HsModule(..)
    -- * Translation
  , HsModuleOpts(..)
  , translateModule
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import HsBindgen.Backend.PP.Names
import HsBindgen.Hs.AST.Type qualified as Hs
import HsBindgen.Imports
import HsBindgen.SHs.AST

{-------------------------------------------------------------------------------
  GhcPragma
-------------------------------------------------------------------------------}

-- | GHC Pragma
--
-- Example: @LANGUAGE NoImplicitPrelude@
type GhcPragma = String

{-------------------------------------------------------------------------------
  ImportListItem
-------------------------------------------------------------------------------}

-- | Import list item
data ImportListItem =
    UnqualifiedImportListItem HsImportModule [ResolvedName]
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
      hsModulePragmas :: [GhcPragma]
    , hsModuleName    :: String
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

translateModule :: HsModuleOpts -> [SDecl] -> HsModule
translateModule HsModuleOpts{..} hsModuleDecls =
    let hsModulePragmas = resolvePragmas hsModuleDecls
        hsModuleImports = resolveImports hsModuleDecls
        hsModuleName    = hsModuleOptsName
    in  HsModule{..}


{-------------------------------------------------------------------------------
  Auxiliary: Pragma resolution
-------------------------------------------------------------------------------}

resolvePragmas :: [SDecl] -> [GhcPragma]
resolvePragmas ds =
    Set.toAscList . mconcat $ constPragmas : map resolveDeclPragmas ds
  where
    constPragmas :: Set GhcPragma
    constPragmas = Set.singleton "LANGUAGE NoImplicitPrelude"

resolveDeclPragmas :: SDecl -> Set GhcPragma
resolveDeclPragmas = \case
    DVar{} -> Set.empty
    DInst{} -> Set.empty
    DRecord{} -> Set.empty
    DNewtype{} -> Set.empty
    DEmptyData{} -> Set.empty
    DDerivingInstance{} -> Set.fromList
      [ "LANGUAGE DerivingStrategies"
      , "LANGUAGE GeneralizedNewtypeDeriving"
      , "LANGUAGE StandaloneDeriving"
      ]
    DForeignImport{} -> Set.singleton "LANGUAGE CApiFFI"
    DPatternSynonym{} -> Set.singleton "LANGUAGE PatternSynonyms"

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
    mkUImportListItem :: HsImportModule -> Set ResolvedName -> ImportListItem
    mkUImportListItem imp = UnqualifiedImportListItem imp . Set.toAscList

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
    DVar _name mbTy e ->
      maybe mempty resolveTypeImports mbTy <> resolveExprImports e
    DInst Instance{..} -> mconcat $
        resolveGlobalImports instanceClass
      : map (resolveGlobalImports . fst) instanceDecs
      ++ map (resolveExprImports . snd) instanceDecs
    DRecord Record{..} -> mconcat $
      map (resolveTypeImports . fieldType) dataFields
    DEmptyData _name -> mempty
    DNewtype Newtype{..} -> resolveTypeImports $ fieldType newtypeField
    DDerivingInstance _s ty -> resolveTypeImports ty
    DForeignImport ForeignImport {..} -> resolveTypeImports foreignImportType
    DPatternSynonym PatternSynonym {..} ->
        resolveTypeImports patSynType <>
        resolveExprImports patSynRHS

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

resolvePrimTypeImports :: Hs.HsPrimType -> ImportAcc
resolvePrimTypeImports = resolveTypeImports . TGlobal . PrimType
