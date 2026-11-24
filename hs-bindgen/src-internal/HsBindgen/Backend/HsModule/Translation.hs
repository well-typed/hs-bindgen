module HsBindgen.Backend.HsModule.Translation (
    -- * GhcPragma
    GhcPragma (..)
    -- * ImportListItem
  , ImportSource(..)
  , ImportListItem(..)
    -- * Export list
  , ExportEntry(..)
  , ExportItem(..)
  , resolveDeclExports
    -- * HsModule
  , HsModule(..)
  , withModuleDoc
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

-- | Whether an import reads a module or its boot file
data ImportSource =
    -- | @import M@
    NormalImport
    -- | @import {-# SOURCE #-} M@: the boot file of @M@, which breaks an import
    -- cycle
  | SourceImport
  deriving stock (Eq, Ord)

-- | Import list item
--
-- 'resolveImports' sorts imports with this 'Ord' instance. The 'ImportSource'
-- is the first field, and 'NormalImport' sorts before 'SourceImport', so the
-- @{-# SOURCE #-}@ imports of each kind come after its normal imports.
data ImportListItem =
    QualifiedImportListItem   ImportSource Hs.ModuleName (Maybe String)
    -- An empty import list (@Just []@) means "import instances only", and
    -- differs greatly from having no import list (@Nothing@), which is an open,
    -- unqualified import.
  | UnqualifiedImportListItem ImportSource Hs.ModuleName (Maybe [ResolvedName])
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
      -- | The Haddock of the module itself, above the module header
    , moduleDoc      :: Maybe HsDoc.Comment
    , name           ::  Hs.ModuleName
    , exports        :: [ExportEntry]
    , imports        :: [ImportListItem]
    , qualifiedStyle :: QualifiedStyle
      -- | Root directives, prepended to the CAPI wrapper source
    , rootDirectives :: [C.RootDirective C.HashIncludeArg]
    , cWrappers      :: [CWrapper]
    , decls          :: [SDecl]
    }

-- | Give a module its own Haddock
--
-- Library mode documents its vendor module this way.
withModuleDoc :: HsDoc.Comment -> HsModule -> HsModule
withModuleDoc doc m = m{moduleDoc = Just doc}

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
  -> HsBootPolicy
  -> [C.RootDirective C.HashIncludeArg]
  -> BaseModuleName
  -> [ImportListItem]
     -- ^ Library-mode @{-# SOURCE #-}@ imports of the types module (see
     -- @crossImportsFor@). Empty outside library mode.
  -> ([SDecl] -> [ExportEntry])
  -> ByCategory_ ([CWrapper], [SDecl])
  -> ByCategory_ (Maybe (HsModule, Maybe HsBoot))
translateModuleMultiple fns mrc hsBootPolicy dirs moduleBaseName crossModuleImports resolveExports declsByCat =
    mapWithCategory_ go declsByCat
  where
    go :: Category -> ([CWrapper], [SDecl]) -> Maybe (HsModule, Maybe HsBoot)
    go _   ([], [])       = Nothing
    go cat xs@(_, decs) = Just (
        addOrphanPragma cat $
          translateModule'
            fns
            mrc
            dirs
            (Just cat)
            moduleBaseName
            (crossImportsFor cat)
            resolveExports
            xs
      , translateHsBoot mrc moduleBaseName cat <$> bootDecls cat decs
      )

    addOrphanPragma :: Category -> HsModule -> HsModule
    addOrphanPragma (CTerm CCallback) m = HsModule{
          pragmas        = "OPTIONS_GHC -Wno-orphans" : m.pragmas
        , moduleDoc      = m.moduleDoc
        , name           = m.name
        , exports        = m.exports
        , imports        = m.imports
        , qualifiedStyle = m.qualifiedStyle
        , rootDirectives = m.rootDirectives
        , cWrappers      = m.cWrappers
        , decls          = m.decls
        }
    addOrphanPragma _otherCategory m = m

    -- Only a types module gets these: a @{-# SOURCE #-}@ import of each peer in
    -- its import cycle that its types refer to. Term modules (Safe, Unsafe,
    -- FunPtr, Global, Callback) can't be in a cycle, so they get none.
    --
    -- NOTE: This assumes the bindings are compiled as a whole package with
    -- @ghc --make@, as cabal and stack do. Compiling bindings with import
    -- cycles one module at a time, with @ghc -c@, is not supported: a term
    -- module can then fail with "Couldn't match type FFIType ...".
    crossImportsFor :: Category -> [ImportListItem]
    crossImportsFor CType     = crossModuleImports
    crossImportsFor (CTerm _) = []

    bootDecls :: Category -> [SDecl] -> Maybe [HsBootDecl]
    bootDecls cat decs = case hsBootPolicy of
      GenerateHsBoot      -> (\f -> concatMap f decs) <$> hsBootDeclsFor cat
      DoNotGenerateHsBoot -> Nothing

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
  -> [ImportListItem] -- ^ Library-mode cross-module imports, already adjusted
                     --   per category by @crossImportsFor@
  -> ([SDecl] -> [ExportEntry])
  -> ([CWrapper], [SDecl])
  -> HsModule
translateModule' fns mrc dirs mcat moduleBaseName crossModuleImports resolveExports (cWrappers, decs) =
    HsModule{
        pragmas        = resolvePragmas mrc.qualifiedStyle cWrappers $
                              fieldNamingPragmas fns
                           <> Set.map extensionPragma
                                (foldMap (requiredExtensions fns) decs)
      , moduleDoc      = Nothing
      , exports        = resolveExports decs
      , imports        = resolveImports moduleBaseName mcat cWrappers crossModuleImports decs
      , name           = fromBaseModuleName moduleBaseName mcat
      , qualifiedStyle = mrc.qualifiedStyle
      , rootDirectives = dirs
      , cWrappers      = cWrappers
      , decls          = decs
      }

{-------------------------------------------------------------------------------
  Boot files
-------------------------------------------------------------------------------}

-- | Translate the declarations of a boot file
--
-- A boot file has no fields, so the field naming strategy does not matter.
translateHsBoot ::
     ModuleRenderConfig
  -> BaseModuleName
  -> Category
  -> [HsBootDecl]
  -> HsBoot
translateHsBoot mrc moduleBaseName cat decs =
  let currentModule = fromBaseModuleName moduleBaseName (Just cat)
   in HsBoot{
        pragmas        = resolvePragmas mrc.qualifiedStyle [] $
                           Set.map extensionPragma
                             (foldMap requiredHsBootExtensions decs)
      , name           = fromBaseModuleName moduleBaseName (Just cat)
      , imports        = resolveHsBootImports currentModule decs
      , qualifiedStyle = mrc.qualifiedStyle
      , decls          = decs
      }

-- | What the boot file of the module of a category declares, if it has one
--
-- Only types modules have boot files, so that types modules whose headers use
-- each other can import each other through their boot files.
--
-- See the golden fixture @types\/boot\/internal_module@.
hsBootDeclsFor :: Category -> Maybe (SDecl -> [HsBootDecl])
hsBootDeclsFor = \case
    CType    -> Just typesHsBootDecls
    CTerm _  -> Nothing

-- | What the boot file of a types module declares
--
-- A module that imports the types module with @{-# SOURCE #-}@ sees only the
-- boot file, and needs three things from it:
--
-- * the name of a type, to write a type such as @Ptr A@;
-- * that an instance exists, to write its own instances: the @ReadRaw@
--   instance of a struct with a field of type @B@ needs @ReadRaw B@;
-- * the type of a value, to use a macro of another header in its cycle:
--   @#define B_LIMIT (A_MAGIC + 1)@ needs the type of @a_MAGIC@.
--
typesHsBootDecls :: SDecl -> [HsBootDecl]
typesHsBootDecls = \case
    DTypSyn typSyn ->
      [HsBootTypSyn typSyn.name typSyn.typ]
    DRecord record ->
      HsBootData record.typ : derived record.typ record.deriv
    DNewtype newtyp ->
      HsBootData newtyp.name : derived newtyp.name newtyp.deriv
    DEmptyData empty ->
      [HsBootData empty.name]
    DInst inst
      | [TCon name] <- inst.args, null inst.super ->
          [HsBootInstance inst.clss name]
      | otherwise ->
          []
    DDerivingInstance deriv
      | TApp (TClass clss) (TCon name) <- deriv.typ ->
          [HsBootInstance clss name]
      | otherwise ->
          []
    DForeignImport _ ->
      []
    DBinding binding -> case binding.name of
      Hs.ExportedName _ ->
        [HsBootSig binding.name binding.parameters binding.result]
      Hs.InternalName _ ->
        []
    DPatternSynonym _ ->
      []
    DCompletePragma _ ->
      []
  where
    derived ::
         Hs.Name Hs.NsTypeConstr
      -> [(Hs.Strategy ClosedType, [Inst.TypeClass])]
      -> [HsBootDecl]
    derived name deriv = [
          HsBootInstance clss name
        | (_strategy, classes) <- deriv
        , clss <- classes
        ]

{-------------------------------------------------------------------------------
  Auxiliary: Pragma resolution
-------------------------------------------------------------------------------}

-- | The pragmas of a module or boot file, given those its declarations need
resolvePragmas ::
     QualifiedStyle
  -> [CWrapper]
  -> Set GhcPragma
  -> [GhcPragma]
resolvePragmas qualStyle wrappers declPragmas =
    Set.toAscList . mconcat $ [
        noImplicitPreludePragma
      , haddockPrunePragmas
      , userlandCapiPragmas
      , qualifiedPostPragma
      , declPragmas
      ]
  where
    -- Generated modules import every name they use; see 'resolveImports'.
    noImplicitPreludePragma :: Set GhcPragma
    noImplicitPreludePragma = Set.singleton "LANGUAGE NoImplicitPrelude"

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

-- | @NoFieldSelectors@ is the negation of the default-on @FieldSelectors@, so
-- it cannot be carried in the positive 'TH.Extension' set (see
-- 'HsBindgen.Backend.Extensions.omitFieldPrefixesExtensions'). We emit it as a
-- module pragma directly. Without top-level field selectors, an unprefixed
-- field name cannot clash with a non-field declaration of the same name.
fieldNamingPragmas :: FieldNamingStrategy -> Set GhcPragma
fieldNamingPragmas = \case
    AddFieldPrefixes  -> Set.empty
    OmitFieldPrefixes -> Set.singleton "LANGUAGE NoFieldSelectors"

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
  -> [ImportListItem] -- ^ Library-mode cross-module imports: the SOURCE
                     --   imports of a types module, none for a term module
  -> [SDecl]
  -> [ImportListItem]
resolveImports baseModule cat wrappers crossModuleImports ds =
    let acc = mconcat $ map resolveDeclImports ds
        currentModule = fromBaseModuleName baseModule cat

        -- The modules this module imports with {-# SOURCE #-}: the other
        -- types modules of its import cycle. A reference to one of their
        -- names (TExt or EExt) would add a normal qualified import of the
        -- same module and close the cycle, so 'importListItems' drops it. A
        -- term module has no SOURCE imports, so it imports every module
        -- normally.
        sccPeerModules = Set.fromList
          [ m | QualifiedImportListItem SourceImport m _ <- crossModuleImports ]

    in  Set.toAscList . mconcat $ [
            bindingCatImport acc.requireTypes
          , Set.fromList crossModuleImports
          , Set.map (uncurry (QualifiedImportListItem NormalImport)) userlandCapiImport
          , importListItems currentModule sccPeerModules acc
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
          in  Set.singleton $
                UnqualifiedImportListItem NormalImport moduleName Nothing

    userlandCapiImport :: Set (Hs.ModuleName, Maybe String)
    userlandCapiImport = case wrappers of
      []  -> mempty
      _xs -> Set.singleton (capiModule, Nothing)

-- | Resolve the imports of a boot file
--
-- Only types modules have boot files. Their imports are the qualified modules
-- that the boot file's declarations refer to, as for a normal module. Unlike a
-- normal module, a boot file imports the other modules of its import cycle
-- normally, not through their boot files. A type synonym that names a peer's
-- type therefore closes the cycle: the flexible array member of
-- @struct A { int n; struct B *items[]; }@ makes the boot file of @Lib.A@
-- import @Lib.B@, which imports @Lib.A@ with @{-# SOURCE #-}@.
resolveHsBootImports :: Hs.ModuleName -> [HsBootDecl] -> [ImportListItem]
resolveHsBootImports currentModule ds =
    let acc = mconcat $ map resolveHsBootDeclImports ds
    in  Set.toAscList $ importListItems currentModule Set.empty acc

-- | The import list items for the names in an accumulator, without the
-- current module and without the given import-cycle peers, which already have
-- a @{-# SOURCE #-}@ import.
importListItems ::
     Hs.ModuleName -> Set Hs.ModuleName -> ImportAcc -> Set ImportListItem
importListItems currentModule sccPeerModules acc = mconcat [
      Set.map (uncurry (QualifiedImportListItem NormalImport)) $
        Set.filter (\(m, _) -> not (suppress m)) acc.qualified
    , Set.fromList $ map (uncurry mkUImportListItem) $
        filter (\(m, _) -> not (suppress m)) (Map.toList acc.unqualified)
    ]
  where
    suppress :: Hs.ModuleName -> Bool
    suppress m = m == currentModule || Set.member m sccPeerModules

    mkUImportListItem :: Hs.ModuleName -> Set ResolvedName -> ImportListItem
    mkUImportListItem m xs =
      UnqualifiedImportListItem NormalImport m (Just $ Set.toAscList xs)

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
    HsBootInstance clss _name ->
      resolveTypeClassImports clss
    HsBootSig _name params result -> mconcat [
        foldMap (resolveTypeImports . (.typ)) params
      , resolveTypeImports result.typ
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
    EExt ref -> resolveIdentifierImports ref
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
    TExt ref -> resolveIdentifierImports ref
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

resolveIdentifierImports :: Hs.Identifier -> ImportAcc
resolveIdentifierImports identifier = ImportAcc{
      requireTypes = False
    , qualified    = Set.singleton (identifier.moduleName, Nothing)
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
