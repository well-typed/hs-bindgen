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
import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.HsModule.CAPI (capiModule)
import HsBindgen.Backend.HsModule.Names
import HsBindgen.Backend.SHs.AST
import HsBindgen.Config.Prelims
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
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
    QualifiedImportListItem   Hs.ModuleName (Maybe String)
    -- An empty import list (@Just []@) means "import instances only", and
    -- differs greatly from having no import list (@Nothing@), which is an open,
    -- unqualified import.
  | UnqualifiedImportListItem Hs.ModuleName (Maybe [ResolvedName])
  deriving stock (Eq, Ord)

{-------------------------------------------------------------------------------
  HsModule
-------------------------------------------------------------------------------}

-- | Haskell module
data HsModule = HsModule {
      pragmas   :: [GhcPragma]
    , name      ::  Hs.ModuleName
    , imports   :: [ImportListItem]
    , cWrappers :: [CWrapper]
    , decls     :: [SDecl]
    }

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

translateModuleMultiple ::
     FieldNamingStrategy
  -> BaseModuleName
  -> ByCategory_ ([CWrapper], [SDecl])
  -> ByCategory_ (Maybe HsModule)
translateModuleMultiple fieldNaming moduleBaseName declsByCat =
    mapWithCategory_ go declsByCat
  where
    go :: Category -> ([CWrapper], [SDecl]) -> Maybe HsModule
    go _ ([], []) = Nothing
    go cat xs     = Just $ translateModule' fieldNaming (Just cat) moduleBaseName xs

translateModuleSingle ::
     FieldNamingStrategy
  -> BaseModuleName
  -> ByCategory_ ([CWrapper], [SDecl])
  -> HsModule
translateModuleSingle fieldNaming name declsByCat =
    translateModule' fieldNaming Nothing name $ Foldable.fold declsByCat

translateModule' ::
     FieldNamingStrategy
  -> Maybe Category
  -> BaseModuleName
  -> ([CWrapper], [SDecl])
  -> HsModule
translateModule' fieldNaming mcat moduleBaseName (cWrappers, decs) = HsModule{
      pragmas   = resolvePragmas fieldNaming cWrappers decs
    , imports   = resolveImports moduleBaseName mcat cWrappers decs
    , name      = fromBaseModuleName moduleBaseName mcat
    , cWrappers = cWrappers
    , decls     = decs
    }

{-------------------------------------------------------------------------------
  Auxiliary: Pragma resolution
-------------------------------------------------------------------------------}

resolvePragmas :: FieldNamingStrategy -> [CWrapper] -> [SDecl] -> [GhcPragma]
resolvePragmas fieldNaming wrappers ds =
    Set.toAscList . mconcat $
      haddockPrunePragmas : userlandCapiPragmas
        : map (resolveDeclPragmas fieldNaming) ds
  where
    userlandCapiPragmas :: Set GhcPragma
    userlandCapiPragmas = case wrappers of
      []  -> Set.empty
      _xs -> Set.singleton "LANGUAGE TemplateHaskell"

    haddockPrunePragmas :: Set GhcPragma
    haddockPrunePragmas = case wrappers of
      []  -> Set.empty
      _xs -> Set.singleton "OPTIONS_HADDOCK prune"

resolveDeclPragmas :: FieldNamingStrategy -> SDecl -> Set GhcPragma
resolveDeclPragmas fieldNaming decl =
    Set.map f (requiredExtensions fieldNaming decl)
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
    let acc = mconcat $ map resolveDeclImports ds
    in  Set.toAscList . mconcat $
            bindingCatImport acc.requireTypes
          : Set.map (uncurry QualifiedImportListItem) (userlandCapiImport <> acc.qualified)
          : map (Set.singleton . uncurry mkUImportListItem) (Map.toList acc.unqualified)
  where
    mkUImportListItem :: Hs.ModuleName -> Set ResolvedName -> ImportListItem
    mkUImportListItem m xs = UnqualifiedImportListItem m (Just $ Set.toAscList xs)

    bindingCatImport :: Bool -> Set ImportListItem
    bindingCatImport requireTypes = case requireTypes of
      False -> mempty
      True  -> case cat of
        Nothing    -> mempty
        Just CType -> mempty
        _otherCat  ->
          let moduleName = fromBaseModuleName baseModule (Just CType)
          in  Set.singleton $ UnqualifiedImportListItem moduleName Nothing

    userlandCapiImport :: Set (Hs.ModuleName, Maybe String)
    userlandCapiImport = case wrappers of
      []  -> mempty
      _xs -> Set.singleton (capiModule, Nothing)

-- | Accumulator for resolving imports
--
-- Both qualified imports and unqualified imports are accumulated.
data ImportAcc = ImportAcc {
      requireTypes :: Bool
    , qualified    :: Set (Hs.ModuleName, Maybe String)
    , unqualified  :: Map Hs.ModuleName (Set ResolvedName)
    }

instance Semigroup ImportAcc where
  a <> b = ImportAcc{
        requireTypes = combineWith (||)                 (.requireTypes)
      , qualified    = combineWith (<>)                 (.qualified)
      , unqualified  = combineWith (Map.unionWith (<>)) (.unqualified)
      }
    where
      combineWith :: (a -> a -> a) -> (ImportAcc -> a) -> a
      combineWith op f = f a `op` f b

instance Monoid ImportAcc where
  mempty = ImportAcc{
        requireTypes = False
      , qualified    = mempty
      , unqualified  = mempty
      }

-- | Resolve imports in a declaration
resolveDeclImports :: SDecl -> ImportAcc
resolveDeclImports = \case
    DTypSyn typSyn -> mconcat [
        resolveTypeImports typSyn.typ
      ]
    DInst inst -> mconcat $ concat [
        [resolveTypeClassImports inst.clss]
      , map resolveTypeImports inst.args
      , [ resolveTypeImports t | t <- inst.super ]
      , concat [
           resolveGlobalImports t : resolveTypeImports r : map resolveTypeImports as
         | (t, as, r) <- inst.types
         ]
      , concat [
            [resolveGlobalImports f, resolveExprImports e]
          | (f, e) <- inst.decs
          ]
      ]
    DRecord record -> mconcat [
        mconcat $ map (resolveTypeImports . (.typ)) record.fields
      , resolveNestedDeriv record.deriv
      ]
    DEmptyData _name ->
      mempty
    DNewtype newtyp -> mconcat [
        resolveTypeImports newtyp.field.typ
      , resolveNestedDeriv newtyp.deriv
      ]
    DDerivingInstance deriv -> mconcat [
        resolveStrategyImports deriv.strategy
      , resolveTypeClassImports deriv.cls
      ]
    DForeignImport foreignImport -> mconcat [
        foldMap (resolveTypeImports . (.typ)) foreignImport.parameters
      , resolveTypeImports foreignImport.result.typ
      ]
    DBinding binding -> mconcat [
        foldMap (resolveTypeImports . (.typ)) binding.parameters
      , resolveTypeImports binding.result.typ
      , resolveExprImports binding.body
      ]
    DPatternSynonym patSyn -> mconcat [
        resolveTypeImports patSyn.typ
      , resolvePatExprImports patSyn.rhs
      ]

-- | Resolve nested deriving clauses (part of a datatype declaration)
resolveNestedDeriv :: [(Hs.Strategy ClosedType, [Inst.TypeClass])] -> ImportAcc
resolveNestedDeriv = mconcat . map aux
  where
    aux :: (Hs.Strategy ClosedType, [Inst.TypeClass]) -> ImportAcc
    aux (strategy, cls) = mconcat $
          resolveStrategyImports strategy
        : map resolveTypeClassImports cls

resolveTypeClassImports :: Inst.TypeClass -> ImportAcc
resolveTypeClassImports = resolveGlobalImports . typeClassGlobal

-- | Resolve global imports
resolveGlobalImports :: Global c -> ImportAcc
resolveGlobalImports global =
    case resolved.hsImport of
      Hs.ImplicitPrelude -> ImportAcc{
          requireTypes = False
        , qualified    = mempty
        , unqualified  = mempty
        }
      Hs.QualifiedImport m as -> ImportAcc{
          requireTypes = False
        , qualified    = Set.singleton (m, as)
        , unqualified  = mempty
        }
      Hs.UnqualifiedImport m -> ImportAcc{
          requireTypes = False
        , qualified    = mempty
        , unqualified  = Map.singleton m (Set.singleton resolved)
        }
  where
    resolved :: ResolvedName
    resolved = resolveGlobal global

-- | Resolve imports in an expression
resolveExprImports :: SExpr ctx -> ImportAcc
resolveExprImports = \case
    EGlobal g -> resolveGlobalImports g
    EBound _x -> mempty
    EFree {} -> mempty
    ECon _n -> mempty
    EIntegral _ t -> maybe mempty resolveGlobalImports t
    EUnboxedIntegral _ -> mempty
    EChar {} -> mconcat $
      map (resolveGlobalImports . cExprGlobalType) [
          CharValue_type
        ] ++
      map (resolveGlobalImports . cExprGlobalExpr) [
          CharValue_constructor
        , CharValue_fromAddr
        ] ++
      map (resolveGlobalImports . bindgenGlobalExpr) [
          Maybe_just
        , Maybe_nothing
        ]
    EString {} -> mempty
    ECString {} -> resolveGlobalImports (bindgenGlobalType CStringLen_type)
                <> resolveGlobalImports (bindgenGlobalExpr Foreign_Ptr_constructor)
    EFloat _ t -> resolveGlobalImports t
    EDouble _ t -> resolveGlobalImports t
    EApp f x -> resolveExprImports f <> resolveExprImports x
    EInfix op x y ->
      resolveGlobalImports (infixOpGlobal op) <> resolveExprImports x <> resolveExprImports y
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
    -- TODO https://github.com/well-typed/hs-bindgen/issues/1714: Tuples should
    -- probably not use 'Solo'/'MkSolo'. Then we do not need this import.
    EBoxedOpenTup n | n<= 1 ->
      ImportAcc{
          requireTypes = False
        , qualified = Set.singleton ("HsBindgen.Runtime.Internal.Prelude", Just "RIP")
        , unqualified = mempty
        }
    EBoxedOpenTup{} -> mempty
    EBoxedClosedTup{} -> mempty
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
    TCon _n -> ImportAcc {
        requireTypes = True
      , qualified    = mempty
      , unqualified  = mempty
      }
    TFree _ -> mempty
    TLit _n -> mempty
    TStrLit _s -> mempty
    TExt ref _cTypeSpec _hsTypeSpec -> resolveExtHsRefImports ref
    TApp c x -> resolveTypeImports c <> resolveTypeImports x
    TFun a b -> resolveTypeImports a <> resolveTypeImports b
    TBound{} -> mempty
    -- TODO https://github.com/well-typed/hs-bindgen/issues/1714: Tuples should
    -- probably not use 'Solo'/'MkSolo'. Then we do not need this import.
    TBoxedOpenTup n | n <= 1 ->
      ImportAcc{
          requireTypes = False
        , qualified = Set.singleton ("HsBindgen.Runtime.Internal.Prelude", Just "RIP")
        , unqualified = mempty
        }
    TBoxedOpenTup{} -> mempty
    TEq{} -> mempty
    TForall _hints _qtvs ctxt body ->
      foldMap resolveTypeImports (body:ctxt)

resolveStrategyImports :: Hs.Strategy ClosedType -> ImportAcc
resolveStrategyImports = \case
    Hs.DeriveNewtype -> mempty
    Hs.DeriveStock -> mempty
    Hs.DeriveVia ty -> resolveTypeImports ty

resolveExtHsRefImports :: Hs.ExtRef -> ImportAcc
resolveExtHsRefImports extRef = ImportAcc{
      requireTypes = False
    , qualified    = Set.singleton (extRef.moduleName, Nothing)
    , unqualified  = mempty
    }
