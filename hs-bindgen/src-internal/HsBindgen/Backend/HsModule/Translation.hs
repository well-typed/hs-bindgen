module HsBindgen.Backend.HsModule.Translation (
    -- * GhcPragma
    GhcPragma (..)
    -- * ImportListItem
  , ImportListItem(..)
    -- * Export list
  , ExportEntry(..)
  , ExportItem(..)
  , resolveDeclExports
    -- * HsModule
  , HsModule(..)
    -- * HsBoot
  , HsBoot(..)
  , hasHsBoot
    -- * Translation
  , translateModuleMultiple
  , translateModuleSingle
  ) where

import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Language.Haskell.TH qualified as TH

import HsBindgen.Backend.Category
import HsBindgen.Backend.Extensions
import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.HsModule.Names
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.SHs.AST.Expr (FBind (FBind))
import HsBindgen.Config.Prelims
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.IR.C qualified as C
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
    -- | @import {-# SOURCE #-} M@: an open, unqualified import of the boot file
    -- of @M@
  | SourceImportListItem      Hs.ModuleName
  deriving stock (Eq, Ord)

{-------------------------------------------------------------------------------
  Export list
-------------------------------------------------------------------------------}

-- | An entry in the module export list.
--
-- Sections nest naturally: an 'ExportSection' carries its own children, so
-- depth is implicit in the tree structure rather than tracked as an integer
-- on each header.  The pretty-printer derives Haddock @*@ count from the
-- recursion depth.
--
-- Example: a top-level @api_version_t@ followed by a @Core Data Types@
-- group containing a record and a derived pattern synonym is rendered as:
--
-- > [ ExportEntry (ExportTypeAll "Api_version_t")
-- > , ExportSection [TextContent "Core Data Types"]
-- >     [ ExportEntry (ExportTypeAll "Config_t")
-- >     , ExportEntry (ExportPattern "COLOR_RED")
-- >     ]
-- > ]
data ExportEntry =
    ExportEntry ExportItem
    -- | A Haddock section header carrying its title (as inline content,
    -- so it can mix bold\/italic\/monospace markup) and its nested children.
  | ExportSection [HsDoc.CommentInlineContent] [ExportEntry]

-- | An item in the module export list
data ExportItem =
    -- | Export a type with all its constructors and fields: @TypeName(..)@
    ExportTypeAll Text
    -- | Export a plain name (type without constructors, or term-level binding)
  | ExportName Text
    -- | Export a pattern synonym: @pattern PatName@
  | ExportPattern Text

{-------------------------------------------------------------------------------
  HsModule
-------------------------------------------------------------------------------}

-- | Haskell module
data HsModule = HsModule {
      pragmas        :: [GhcPragma]
    , name           ::  Hs.ModuleName
    , exports        :: [ExportEntry]
    , imports        :: [ImportListItem]
    , qualifiedStyle :: QualifiedStyle
      -- | Root directives, prepended to the CAPI wrapper source
    , rootDirectives :: [C.RootDirective C.HashIncludeArg]
    , cWrappers      :: [CWrapper]
    , decls          :: [SDecl]
    }

{-------------------------------------------------------------------------------
  HsBoot
-------------------------------------------------------------------------------}

-- | Boot file of a Haskell module
--
-- A boot file has no export list: it exports what it declares.
data HsBoot = HsBoot {
      pragmas        :: [GhcPragma]
    , name           :: Hs.ModuleName
    , imports        :: [ImportListItem]
    , qualifiedStyle :: QualifiedStyle
    , decls          :: [HsBootDecl]
    }

-- | Does the module of this category have a boot file?
hasHsBoot :: Category -> Bool
hasHsBoot = isJust . hsBootDeclsFor

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

translateModuleMultiple ::
     FieldNamingStrategy
  -> ModuleRenderConfig
  -> [C.RootDirective C.HashIncludeArg]
  -> BaseModuleName
  -> ([SDecl] -> [ExportEntry])
  -> ByCategory_ ([CWrapper], [SDecl])
  -> ByCategory_ (Maybe (HsModule, Maybe HsBoot))
translateModuleMultiple fns mrc dirs moduleBaseName resolveExports declsByCat =
    mapWithCategory_ go declsByCat
  where
    go :: Category -> ([CWrapper], [SDecl]) -> Maybe (HsModule, Maybe HsBoot)
    go _   ([], [])       = Nothing
    go cat xs@(_, decs) = Just (
        translateModule'
          fns
          mrc
          dirs
          (Just cat)
          moduleBaseName
          (sourceImports cat)
          resolveExports
          xs
      , translateHsBoot fns mrc moduleBaseName cat <$> bootDecls cat decs
      )

    -- | Is there an FFI module?
    --
    -- The types module names what the FFI module defines (today, the functions
    -- of its @ToFunPtr@ and @FromFunPtr@ instances), so it imports the FFI
    -- module whenever there is one. See the golden fixture
    -- @types/boot/ffi_module@.
    hasFFIModule :: Bool
    hasFFIModule = case view (lensForCategory (CTerm CFFI)) declsByCat of
      ([], []) -> False
      _ffi     -> True

    -- | Modules that this module imports through their boot files
    --
    -- The FFI module imports the types module, and GHC refuses two modules
    -- that import each other directly, so the types module imports the FFI
    -- module through its boot file. See the golden fixture
    -- @types/boot/ffi_module@.
    sourceImports :: Category -> [Hs.ModuleName]
    sourceImports = \case
      CType | hasFFIModule -> [fromBaseModuleName moduleBaseName (Just (CTerm CFFI))]
      _otherCategory       -> []

    -- For now, due to the nature of generated callback code, a types module
    -- gets a boot file only when there is an FFI module. The two modules
    -- need each other: the types module names the functions of the FFI
    -- module (in its @ToFunPtr@ and @FromFunPtr@ instances), and the FFI module
    -- uses the types of the types module. GHC cannot compile two modules that
    -- import each other directly, so the types module imports the FFI module
    -- through the boot file of the FFI module. That boot file names types of
    -- the types module too, and GHC compiles it before the types module, so
    -- it imports the boot file of the types module.
    --
    -- See the golden fixture @types/boot/ffi_module@.
    bootDecls :: Category -> [SDecl] -> Maybe [HsBootDecl]
    bootDecls cat decs
      | CType <- cat
      , not hasFFIModule = Nothing
      | otherwise = (\f -> concatMap f decs) <$> hsBootDeclsFor cat

translateModuleSingle ::
     FieldNamingStrategy
  -> ModuleRenderConfig
  -> [C.RootDirective C.HashIncludeArg]
  -> BaseModuleName
  -> ([SDecl] -> [ExportEntry])
  -> ByCategory_ ([CWrapper], [SDecl])
  -> HsModule
translateModuleSingle fns mrc dirs name resolveExports declsByCat =
    translateModule' fns mrc dirs Nothing name [] resolveExports $
      Foldable.fold declsByCat

translateModule' ::
     FieldNamingStrategy
  -> ModuleRenderConfig
  -> [C.RootDirective C.HashIncludeArg]
  -> Maybe Category
  -> BaseModuleName
  -> [Hs.ModuleName] -- ^ Modules imported through their boot files
  -> ([SDecl] -> [ExportEntry])
  -> ([CWrapper], [SDecl])
  -> HsModule
translateModule' fns mrc dirs mcat moduleBaseName sourceImports resolveExports (cWrappers, decs) =
    HsModule{
        pragmas        = resolvePragmas fns mrc.qualifiedStyle cWrappers $
                           foldMap (requiredExtensions fns) decs
      , exports        = resolveExports decs
      , imports        = resolveImports moduleBaseName mcat cWrappers sourceImports decs
      , name           = fromBaseModuleName moduleBaseName mcat
      , qualifiedStyle = mrc.qualifiedStyle
      , rootDirectives = dirs
      , cWrappers      = cWrappers
      , decls          = decs
      }

{-------------------------------------------------------------------------------
  Boot files
-------------------------------------------------------------------------------}

translateHsBoot ::
     FieldNamingStrategy
  -> ModuleRenderConfig
  -> BaseModuleName
  -> Category
  -> [HsBootDecl]
  -> HsBoot
translateHsBoot fns mrc moduleBaseName cat decs =
    HsBoot{
        pragmas        = resolvePragmas fns mrc.qualifiedStyle [] $
                           foldMap requiredHsBootExtensions decs
      , name           = fromBaseModuleName moduleBaseName (Just cat)
      , imports        = resolveHsBootImports moduleBaseName cat decs
      , qualifiedStyle = mrc.qualifiedStyle
      , decls          = decs
      }

-- | What the boot file of the module of a category declares, if it has one
hsBootDeclsFor :: Category -> Maybe (SDecl -> [HsBootDecl])
hsBootDeclsFor = \case
    CType     -> Just typesHsBootDecls
    CTerm cat -> case cat of
      CFFI    -> Just ffiHsBootDecls
      CSafe   -> Nothing
      CUnsafe -> Nothing
      CFunPtr -> Nothing
      CGlobal -> Nothing

-- | The boot file of a types module declares its types
--
-- Data types and newtypes are declared abstract (@data T@). Type synonyms are
-- given in full: GHC rejects an abstract @data T@ in a boot file when the
-- module defines @T@ as a synonym, such as @type T = WithFlam CInt T_Aux@.
--
-- See the golden fixture @types/boot/ffi_module@.
typesHsBootDecls :: SDecl -> [HsBootDecl]
typesHsBootDecls = \case
    DTypSyn typSyn      -> [HsBootTypSyn typSyn.name typSyn.typ]
    DRecord record      -> [HsBootData record.typ]
    DNewtype newtyp     -> [HsBootData newtyp.name]
    DEmptyData empty    -> [HsBootData empty.name]
    DInst _             -> []
    DDerivingInstance _ -> []
    DForeignImport _    -> []
    DBinding _          -> []
    DPatternSynonym _   -> []
    DCompletePragma _   -> []

-- | The boot file of an FFI module declares the type of each of its functions
--
-- The types module imports the FFI module through this boot file, so the boot
-- file declares every function that the types module can name. A signature is
-- all that the types module needs. The foreign imports are left out: only the
-- functions around them are named.
--
-- See the golden fixture @types/boot/ffi_module@.
ffiHsBootDecls :: SDecl -> [HsBootDecl]
ffiHsBootDecls = \case
    DBinding binding    -> [
        HsBootSignature
          binding.name
          (map (.typ) binding.parameters)
          binding.result.typ
      ]
    DTypSyn _           -> []
    DInst _             -> []
    DRecord _           -> []
    DEmptyData _        -> []
    DDerivingInstance _ -> []
    DNewtype _          -> []
    DForeignImport _    -> []
    DPatternSynonym _   -> []
    DCompletePragma _   -> []

{-------------------------------------------------------------------------------
  Auxiliary: Pragma resolution
-------------------------------------------------------------------------------}

resolvePragmas ::
     FieldNamingStrategy
  -> QualifiedStyle
  -> [CWrapper]
  -> Set TH.Extension
  -> [GhcPragma]
resolvePragmas fieldNaming qualStyle wrappers exts =
    Set.toAscList . mconcat $ [
        noImplicitPreludePragma
      , omitFieldPrefixesPragmas
      , haddockPrunePragmas
      , userlandCapiPragmas
      , qualifiedPostPragma
      , Set.map extensionPragma exts
      ]
  where
    -- Generated modules import every name they use; see 'resolveImports'.
    noImplicitPreludePragma :: Set GhcPragma
    noImplicitPreludePragma = Set.singleton "LANGUAGE NoImplicitPrelude"

    -- 'NoFieldSelectors' is the negation of the default-on 'FieldSelectors' and
    -- so cannot be carried in the positive 'TH.Extension' set (see
    -- 'HsBindgen.Backend.Extensions.omitFieldPrefixesExtensions'); we emit it as
    -- a module pragma directly. Without top-level field selectors, an unprefixed
    -- field name cannot clash with a non-field declaration of the same name.
    omitFieldPrefixesPragmas :: Set GhcPragma
    omitFieldPrefixesPragmas = case fieldNaming of
      AddFieldPrefixes  -> Set.empty
      OmitFieldPrefixes -> Set.singleton "LANGUAGE NoFieldSelectors"
    userlandCapiPragmas :: Set GhcPragma
    userlandCapiPragmas = case wrappers of
      []  -> Set.empty
      _xs -> Set.singleton "LANGUAGE TemplateHaskell"

    haddockPrunePragmas :: Set GhcPragma
    haddockPrunePragmas = case wrappers of
      []  -> Set.empty
      _xs -> Set.singleton "OPTIONS_HADDOCK prune"

    qualifiedPostPragma :: Set GhcPragma
    qualifiedPostPragma = case qualStyle of
      PreQualified  -> Set.empty
      PostQualified -> Set.singleton "LANGUAGE ImportQualifiedPost"

extensionPragma :: TH.Extension -> GhcPragma
extensionPragma ext = GhcPragma ("LANGUAGE " ++ show ext)

{-------------------------------------------------------------------------------
  Auxiliary: Import resolution
-------------------------------------------------------------------------------}

-- | Resolve imports in a list of declarations
resolveImports ::
     BaseModuleName
  -> Maybe Category
  -> [CWrapper]
  -> [Hs.ModuleName] -- ^ Modules imported through their boot files
  -> [SDecl]
  -> [ImportListItem]
resolveImports baseModule cat wrappers sourceImports ds =
    let acc = mconcat $ map resolveDeclImports ds
    in  Set.toAscList . mconcat $ [
            bindingCatImport acc.requireTypes
          , Set.fromList $ map SourceImportListItem sourceImports
          , Set.map (uncurry QualifiedImportListItem) userlandCapiImport
          , importListItems acc
          ]
  where
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

-- | Resolve the imports of a boot file
--
-- As for a module, except for the types module. The boot file of the FFI module
-- names types of the types module, but cannot import it: the types module
-- imports the FFI module through this boot file, so GHC compiles the boot file
-- before the types module. The boot file imports the boot file of the types
-- module instead, with @{-# SOURCE #-}@.
--
-- See the golden fixture @types/boot/ffi_module@.
resolveHsBootImports ::
     BaseModuleName
  -> Category
  -> [HsBootDecl]
  -> [ImportListItem]
resolveHsBootImports baseModule cat ds =
    let acc = mconcat $ map resolveHsBootDeclImports ds
    in  Set.toAscList $ typesImport acc.requireTypes <> importListItems acc
  where
    typesImport :: Bool -> Set ImportListItem
    typesImport requireTypes = case cat of
      CType -> mempty
      CTerm _
        | requireTypes ->
            Set.singleton . SourceImportListItem $
              fromBaseModuleName baseModule (Just CType)
        | otherwise ->
            mempty

-- | The import list items for the names in an accumulator
importListItems :: ImportAcc -> Set ImportListItem
importListItems acc = mconcat [
      Set.map (uncurry QualifiedImportListItem) acc.qualified
    , Set.fromList $ map (uncurry mkUImportListItem) (Map.toList acc.unqualified)
    ]
  where
    mkUImportListItem :: Hs.ModuleName -> Set ResolvedName -> ImportListItem
    mkUImportListItem m xs = UnqualifiedImportListItem m (Just $ Set.toAscList xs)

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
      , resolveTypeImports deriv.typ
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
    DCompletePragma _completePragma ->
      mempty

-- | Resolve imports in a declaration of a boot file
resolveHsBootDeclImports :: HsBootDecl -> ImportAcc
resolveHsBootDeclImports = \case
    HsBootData _name ->
      mempty
    HsBootTypSyn _name typ ->
      resolveTypeImports typ
    HsBootSignature _name args res ->
      foldMap resolveTypeImports (res : args)

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
    EIntegral _ t -> maybe mempty resolveTypeImports t
    EUnboxedIntegral _ -> mempty
    ECChar {} -> mempty
    EString {} -> mempty
    ECString {} -> resolveGlobalImports (bindgenGlobalTerm ByteString_pack)
    EFloat _ t -> resolveTypeImports t
    EDouble _ t -> resolveTypeImports t
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
    EUnit -> mempty
    EBoxedTup{} -> mempty
    EUnboxedTup{} -> mempty
    EList xs -> foldMap resolveExprImports xs
    ETypeApp f t -> resolveExprImports f <> resolveTypeImports t
    ERecCon _con fbinds -> foldMap resolveFBindImports fbinds

resolveFBindImports :: FBind ctx -> ImportAcc
resolveFBindImports (FBind _label expr) = resolveExprImports expr

-- | Resolve imports in a pattern|expression
resolvePatExprImports :: PatExpr -> ImportAcc
resolvePatExprImports = \case
    PEApps _n xs -> foldMap resolvePatExprImports xs
    PELit _      -> mempty

-- | Resolve imports in a type
resolveTypeImports :: SType ctx -> ImportAcc
resolveTypeImports = \case
    TGlobal g -> resolveGlobalImports g
    TClass cls -> resolveTypeClassImports cls
    TCon _n -> ImportAcc {
        requireTypes = True
      , qualified    = mempty
      , unqualified  = mempty
      }
    TFree _ -> mempty
    TLit _n -> mempty
    TStrLit _s -> mempty
    TExt ref -> resolveExtHsRefImports ref
    TApp c x -> resolveTypeImports c <> resolveTypeImports x
    TFun a b -> resolveTypeImports a <> resolveTypeImports b
    TBound{} -> mempty
    TUnit -> mempty
    TBoxedTup{} -> mempty
    TEq -> resolveGlobalImports (bindgenGlobalType TypeEquality_type)
    TForall _hints _qtvs ctxt body ->
      foldMap resolveTypeImports (body:ctxt)
    TList t -> resolveTypeImports t

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

{-------------------------------------------------------------------------------
  Auxiliary: Export resolution
-------------------------------------------------------------------------------}

-- | Resolve the export items contributed by a single declaration.
--
-- Returns one or more 'ExportItem' values for declarations with exported
-- (user-facing) names; internal names and instance declarations produce
-- the empty list (instances are auto-exported by GHC).
resolveDeclExports :: SDecl -> [ExportItem]
resolveDeclExports = \case
    DTypSyn typSyn               -> exportTypeConstr typSyn.name ExportName
    DRecord record               -> exportTypeConstr record.typ ExportTypeAll
    DNewtype newtyp              -> exportTypeConstr newtyp.name ExportTypeAll
    DEmptyData empty             -> exportTypeConstr empty.name ExportName
    DForeignImport foreignImport -> exportVar foreignImport.name
    DBinding binding             -> exportVar binding.name
    DPatternSynonym patSyn       -> exportPattern patSyn.name
    DCompletePragma _            -> []
    -- Instances are automatically exported by GHC
    DInst _                      -> []
    DDerivingInstance _          -> []
  where
    exportTypeConstr :: Hs.Name Hs.NsTypeConstr -> (Text -> ExportItem) -> [ExportItem]
    exportTypeConstr name mkItem = [mkItem name.text]

    exportVar :: Hs.TermName -> [ExportItem]
    exportVar name = case name of
      Hs.ExportedName n -> [ExportName n.text]
      Hs.InternalName _ -> []

    exportPattern :: Hs.Name Hs.NsConstr -> [ExportItem]
    exportPattern name = [ExportPattern name.text]
