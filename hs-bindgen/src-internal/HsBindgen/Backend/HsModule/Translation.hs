module HsBindgen.Backend.HsModule.Translation (
    -- * GhcPragma
    GhcPragma (..)
    -- * ImportListItem
  , ImportListItem(..)
    -- * Export list
  , ExportEntry(..)
  , ExportItem(..)
    -- * GroupSections
  , GroupSections
    -- * HsModule
  , HsModule(..)
    -- * Translation
  , translateModuleMultiple
  , translateModuleSingle
  ) where

import Data.Foldable qualified as Foldable
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import HsBindgen.Backend.Category
import HsBindgen.Backend.Extensions
import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.HsModule.Names
import HsBindgen.Backend.SHs.AST
import HsBindgen.Config.Prelims
import HsBindgen.Frontend.AST.Decl (DeclInfo (..))
import HsBindgen.Frontend.Naming (CDeclName (..), DeclId (..), DeclIdPair (..))
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
  ExportItem
-------------------------------------------------------------------------------}

-- | An entry in the module export list
--
-- Separates section headers from actual export items so the pretty-printer
-- can handle them differently (section headers have no commas).
--
-- Example: given two grouped declarations and one ungrouped, the export list
-- might be:
--
-- > [ ExportEntry (ExportName "api_version")     -- ungrouped, hoisted
-- > , ExportSectionHeader 1 "Core Data Types"    -- section transition
-- > , ExportEntry (ExportTypeAll "Config_t")      -- grouped
-- > , ExportEntry (ExportPattern "COLOR_RED")     -- derived from grouped
-- > ]
data ExportEntry =
    ExportEntry ExportItem
    -- | A Haddock section header at the given depth.  Depth 1 means @-- *@,
    -- depth 2 means @-- **@, etc.
  | ExportSectionHeader Natural Text

-- | An item in the module export list
data ExportItem =
    -- | Export a type with all its constructors and fields: @TypeName(..)@
    ExportTypeAll Text
    -- | Export a plain name (type without constructors, or term-level binding)
  | ExportName Text
    -- | Export a pattern synonym: @pattern PatName@
  | ExportPattern Text

{-------------------------------------------------------------------------------
  GroupSections
-------------------------------------------------------------------------------}

-- | Mapping from declaration names to their Doxygen group title path.
--
-- Built by 'computeGroupSections' in "HsBindgen" from Doxygen XML data and
-- the final C declarations.  Keyed by both C names and Haskell names because
-- the two consumer-side lookup functions cover different 'SDecl' constructors:
--
--  * __Haskell-name keys__ are needed for 'DForeignImport', 'DBinding', and
--    'DPatternSynonym', which have no 'Origin.Decl' and therefore no
--    extractable C name (see 'sdeclOriginCName').  For these constructors,
--    'sdeclExportedName' is the only lookup path.
--
--  * __C-name keys__ are needed for backend-generated declarations whose
--    Haskell name differs from the originating C name (e.g. @_Aux@ newtypes).
--    Their Haskell name is not in the map, but 'sdeclOriginCName' extracts
--    the C name, which is.
--
-- Values are group title paths from root to leaf, e.g.:
--
-- > fromList
-- >   [ ("config_t",  ["Core Data Types"])
-- >   , ("Config_t",  ["Core Data Types"])
-- >   , ("inner_typ", ["Outer Group", "Inner A"])
-- >   , ("Inner_typ", ["Outer Group", "Inner A"])
-- >   ]
--
-- An empty map means no group information is available, in which case
-- no section headers are emitted.
type GroupSections = Map Text [Text]

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
    , cWrappers      :: [CWrapper]
    , decls          :: [SDecl]
    }

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

translateModuleMultiple ::
     FieldNamingStrategy
  -> ModuleRenderConfig
  -> BaseModuleName
  -> GroupSections
  -> ByCategory_ ([CWrapper], [SDecl])
  -> ByCategory_ (Maybe HsModule)
translateModuleMultiple fns mrc moduleBaseName groups declsByCat =
    mapWithCategory_ go declsByCat
  where
    go :: Category -> ([CWrapper], [SDecl]) -> Maybe HsModule
    go _ ([], []) = Nothing
    go cat xs     = Just $ translateModule' fns mrc (Just cat) moduleBaseName groups xs

translateModuleSingle ::
     FieldNamingStrategy
  -> ModuleRenderConfig
  -> BaseModuleName
  -> GroupSections
  -> ByCategory_ ([CWrapper], [SDecl])
  -> HsModule
translateModuleSingle fns mrc name groups declsByCat =
    translateModule' fns mrc Nothing name groups $ Foldable.fold declsByCat

translateModule' ::
     FieldNamingStrategy
  -> ModuleRenderConfig
  -> Maybe Category
  -> BaseModuleName
  -> GroupSections
  -> ([CWrapper], [SDecl])
  -> HsModule
translateModule' fns mrc mcat moduleBaseName groups (cWrappers, decs) = HsModule{
      pragmas        = resolvePragmas fns mrc.qualifiedStyle cWrappers decs
    , exports        = resolveExports groups decs
    , imports        = resolveImports moduleBaseName mcat cWrappers decs
    , name           = fromBaseModuleName moduleBaseName mcat
    , qualifiedStyle = mrc.qualifiedStyle
    , cWrappers      = cWrappers
    , decls          = decs
    }

{-------------------------------------------------------------------------------
  Auxiliary: Pragma resolution
-------------------------------------------------------------------------------}

resolvePragmas :: FieldNamingStrategy -> QualifiedStyle -> [CWrapper] -> [SDecl] -> [GhcPragma]
resolvePragmas fieldNaming qualStyle wrappers ds =
    Set.toAscList . mconcat $
      haddockPrunePragmas
        : userlandCapiPragmas
        : qualifiedPostPragma
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

    qualifiedPostPragma :: Set GhcPragma
    qualifiedPostPragma = case qualStyle of
      PreQualified  -> Set.empty
      PostQualified -> Set.singleton "LANGUAGE ImportQualifiedPost"

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
    EIntegral _ t -> maybe mempty resolveTypeImports t
    EUnboxedIntegral _ -> mempty
    EChar {} -> mconcat $
      map (resolveGlobalImports . cExprGlobalType) [
          CharValue_type
        ] ++
      map (resolveGlobalImports . cExprGlobalTerm) [
          CharValue_constructor
        , CharValue_fromAddr
        ] ++
      map (resolveGlobalImports . bindgenGlobalTerm) [
          Maybe_just
        , Maybe_nothing
        ]
    EString {} -> mempty
    ECString {} -> resolveGlobalImports (bindgenGlobalType CStringLen_type)
                <> resolveGlobalImports (bindgenGlobalTerm Foreign_Ptr_constructor)
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
    TExt ref _cTypeSpec _hsTypeSpec -> resolveExtHsRefImports ref
    TApp c x -> resolveTypeImports c <> resolveTypeImports x
    TFun a b -> resolveTypeImports a <> resolveTypeImports b
    TBound{} -> mempty
    TUnit -> mempty
    TBoxedTup{} -> mempty
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

{-------------------------------------------------------------------------------
  Auxiliary: Export resolution
-------------------------------------------------------------------------------}

-- | How a declaration relates to doxygen group sections
--
-- Tagging is a two-step lookup (see 'taggedExports'):
--
--  1. Try the Haskell export name in 'GroupSections'.
--  2. If that misses, try the C origin name ('sdeclOriginCName').
--  3. If both miss, distinguish top-level C declarations ('ExportUngrouped')
--     from anonymous\/nested\/derived ones ('ExportDerived').
--
-- Example: given @\@defgroup core "Core"@ containing @config_t@ and its
-- anonymous inner union, plus an ungrouped @api_version_t@:
--
-- > Config_t        → ExportGrouped ["Core"]    (step 1: Haskell name hit)
-- > Config_t_union  → ExportGrouped ["Core"]    (step 2: C name "config_t" hit)
-- > Api_version_t   → ExportUngrouped           (step 3: top-level, no group)
-- > pattern COLOR_RED → ExportDerived            (step 3: not top-level)
data ExportGroupTag =
    -- | Top-level C declaration not in any doxygen group.
    -- Hoisted before all section headers by 'resolveExports'.
    ExportUngrouped
    -- | Backend-derived declaration (e.g. pattern synonyms, anonymous inner
    -- types) without its own group membership.  Inherits the enclosing
    -- section from the preceding grouped declaration in 'insertSections'.
  | ExportDerived
    -- | Member of a doxygen group.  The path lists section titles from
    -- root to leaf (e.g. @[\"Outer Group\", \"Inner A\"]@).
  | ExportGrouped [Text]

-- | Resolve exports from a list of declarations
--
-- Only declarations with exported (user-facing) names are included.
-- Internal names (e.g. @hs_bindgen_...@ helper bindings) are excluded.
-- Instances and deriving instances are never exported explicitly (GHC exports
-- them automatically).
--
-- When 'GroupSections' is non-empty, the export list is built in two phases:
--
--  1. __Tag__: each declaration is classified as 'ExportUngrouped',
--     'ExportDerived', or 'ExportGrouped' (see 'ExportGroupTag').
--  2. __Partition & assemble__: ungrouped items are hoisted to the front
--     (before any section headers), then grouped\/derived items follow with
--     Haddock section headers inserted at group transitions.
--
-- Example: for a C header with an ungrouped @api_version_t@ and a
-- @\@defgroup core "Core"@ containing @config_t@ and @enum color@:
--
-- > resolveExports groups decls
-- > ==> [ ExportEntry (ExportTypeAll "Api_version_t")  -- ungrouped, hoisted
-- >      , ExportSectionHeader 1 "Core"                 -- section transition
-- >      , ExportEntry (ExportTypeAll "Config_t")       -- grouped
-- >      , ExportEntry (ExportTypeAll "Color")           -- grouped
-- >      , ExportEntry (ExportPattern "COLOR_RED")       -- derived, inherits
-- >      ]
--
-- When the map is empty (no doxygen data), the declarations are returned
-- in their original order without any section headers.
resolveExports :: GroupSections -> [SDecl] -> [ExportEntry]
resolveExports groups decls
  | Map.null groups = map ExportEntry $ concatMap resolveDeclExports decls
  | otherwise =
    let tagged    = concatMap taggedExports decls
        (ungrouped, rest) = partition isUngrouped tagged
    in  map (ExportEntry . snd) ungrouped
     ++ insertSections [] rest
  where
    -- | Classify a declaration and pair each of its export items with a tag.
    -- A single 'SDecl' can produce multiple export items (e.g. a record
    -- type exports both @TypeName(..)@ and its pattern synonyms).
    taggedExports :: SDecl -> [(ExportGroupTag, ExportItem)]
    taggedExports decl =
      let lookupGroup n = Map.lookup n groups
          tag = case sdeclExportedName decl >>= lookupGroup of
            Just path -> ExportGrouped path
            Nothing   -> case sdeclOriginCName decl >>= lookupGroup of
              Just path -> ExportGrouped path
              Nothing
                | sdeclIsTopLevel decl -> ExportUngrouped
                | otherwise            -> ExportDerived
      in [(tag, item) | item <- resolveDeclExports decl]

    isUngrouped :: (ExportGroupTag, a) -> Bool
    isUngrouped (ExportUngrouped, _) = True
    isUngrouped _                    = False

    -- | Walk the grouped and derived items, emitting section headers at
    -- group transitions.  @prev@ tracks the current group path so that
    -- 'sectionTransition' can compute the minimal set of new headers.
    --
    -- 'ExportDerived' items inherit the enclosing section — they are emitted
    -- without updating @prev@, keeping pattern synonyms and anonymous inner
    -- types together with their parent declaration.
    insertSections :: [Text] -> [(ExportGroupTag, ExportItem)] -> [ExportEntry]
    insertSections _ [] = []
    insertSections prev ((tag, item) : rest) = case tag of
      ExportGrouped g ->
        let sections = sectionTransition prev g
        in sections ++ [ExportEntry item] ++ insertSections g rest
      _otherwise -> ExportEntry item : insertSections prev rest

-- | Compute section headers needed when transitioning between group paths.
--
-- Compares the previous and current group title paths, finds their common
-- prefix, and emits headers only for the new segments.  The header depth
-- starts at @commonPrefixLength + 1@.
--
-- Examples:
--
-- > sectionTransition [] ["Core"]
-- >   ==> [ExportSectionHeader 1 "Core"]
-- >
-- > sectionTransition ["Core"] ["Core"]
-- >   ==> []                                       -- same group, no header
-- >
-- > sectionTransition ["Core"] ["Advanced"]
-- >   ==> [ExportSectionHeader 1 "Advanced"]       -- sibling group
-- >
-- > sectionTransition ["Outer"] ["Outer", "Inner"]
-- >   ==> [ExportSectionHeader 2 "Inner"]           -- child group
-- >
-- > sectionTransition ["Outer", "Inner A"] ["Outer", "Inner B"]
-- >   ==> [ExportSectionHeader 2 "Inner B"]         -- sibling under same parent
sectionTransition :: [Text] -> [Text] -> [ExportEntry]
sectionTransition old new =
    let common = commonPrefixLength old new
        newSegments = drop common new
    in  zipWith ExportSectionHeader [fromIntegral (common + 1) ..] newSegments
  where
    commonPrefixLength :: Eq a => [a] -> [a] -> Int
    commonPrefixLength xs ys = length $ takeWhile (uncurry (==)) $ zip xs ys

-- | Is this a non-nested C declaration?
--
-- 'True' when @declEnclosing = Nothing@ — i.e. the declaration is not
-- inside another struct\/union.  Only 'Origin.Decl'-carrying constructors
-- can be top-level; the rest ('DForeignImport', 'DBinding', etc.) return
-- 'False'.
sdeclIsTopLevel :: SDecl -> Bool
sdeclIsTopLevel = \case
    DTypSyn typSyn      -> isTopLevel typSyn.origin
    DRecord record      -> isTopLevel record.origin
    DNewtype newtyp     -> isTopLevel newtyp.origin
    DEmptyData empty    -> isTopLevel empty.origin
    DForeignImport _    -> False
    DBinding _          -> False
    DPatternSynonym _   -> False
    DInst _             -> False
    DDerivingInstance _  -> False
  where
    isTopLevel :: Origin.Decl a -> Bool
    isTopLevel (Origin.Decl DeclInfo{declEnclosing = enc} _ _) =
      isNothing enc

-- | Extract the C name from the declaration's 'Origin.Decl', if present.
--
-- Available for 'DTypSyn', 'DRecord', 'DNewtype', 'DEmptyData'.  Returns
-- 'Nothing' for the rest ('DForeignImport', 'DBinding', 'DPatternSynonym',
-- etc.) because they carry different origin types without a 'C.DeclInfo'.
--
-- Used as the fallback in 'taggedExports' when the Haskell export name
-- misses in 'GroupSections' — e.g. @Event_callback_t_Aux@ is not a key,
-- but its C origin name @event_callback_t@ is.
sdeclOriginCName :: SDecl -> Maybe Text
sdeclOriginCName = \case
    DTypSyn typSyn               -> Just $ originCName typSyn.origin
    DRecord record               -> Just $ originCName record.origin
    DNewtype newtyp              -> Just $ originCName newtyp.origin
    DEmptyData empty             -> Just $ originCName empty.origin
    DForeignImport _             -> Nothing
    DBinding _                   -> Nothing
    DPatternSynonym _            -> Nothing
    DInst _                      -> Nothing
    DDerivingInstance _          -> Nothing
  where
    originCName :: Origin.Decl a -> Text
    originCName (Origin.Decl DeclInfo{id = declIdPair} _ _) =
      let DeclIdPair{cName = DeclId{name = CDeclName{text = t}}} = declIdPair
      in t

-- | Extract the user-facing Haskell name from a declaration.
--
-- Returns 'Nothing' for internal names, instances, and deriving instances.
--
-- This is the primary lookup key in 'taggedExports'.  For constructors
-- where 'sdeclOriginCName' returns 'Nothing' ('DForeignImport',
-- 'DBinding', 'DPatternSynonym'), this is the /only/ path into
-- 'GroupSections' — which is why the map is keyed by Haskell names too.
sdeclExportedName :: SDecl -> Maybe Text
sdeclExportedName = \case
    DTypSyn typSyn               -> exportedName typSyn.name
    DRecord record               -> exportedName record.typ
    DNewtype newtyp              -> exportedName newtyp.name
    DEmptyData empty             -> exportedName empty.name
    DForeignImport foreignImport -> exportedName foreignImport.name
    DBinding binding             -> exportedName binding.name
    DPatternSynonym patSyn       -> exportedName patSyn.name
    DInst _                      -> Nothing
    DDerivingInstance _          -> Nothing
  where
    exportedName :: Hs.Name ns -> Maybe Text
    exportedName n = case n of
      Hs.ExportedName _ -> Just (Hs.getName n)
      Hs.InternalName _ -> Nothing

-- | Resolve exports from a single declaration
resolveDeclExports :: SDecl -> [ExportItem]
resolveDeclExports = \case
    DTypSyn typSyn               -> exportTypeConstr typSyn.name ExportName
    DRecord record               -> exportTypeConstr record.typ ExportTypeAll
    DNewtype newtyp              -> exportTypeConstr newtyp.name ExportTypeAll
    DEmptyData empty             -> exportTypeConstr empty.name ExportName
    DForeignImport foreignImport -> exportVar foreignImport.name
    DBinding binding             -> exportVar binding.name
    DPatternSynonym patSyn       -> exportPattern patSyn.name
    -- Instances are automatically exported by GHC
    DInst _                      -> []
    DDerivingInstance _          -> []
  where
    exportTypeConstr :: Hs.Name Hs.NsTypeConstr -> (Text -> ExportItem) -> [ExportItem]
    exportTypeConstr name mkItem = case name of
      Hs.ExportedName _ -> [mkItem (Hs.getName name)]
      Hs.InternalName _ -> []

    exportVar :: Hs.Name Hs.NsVar -> [ExportItem]
    exportVar name = case name of
      Hs.ExportedName _ -> [ExportName (Hs.getName name)]
      Hs.InternalName _ -> []

    exportPattern :: Hs.Name Hs.NsConstr -> [ExportItem]
    exportPattern name = case name of
      Hs.ExportedName _ -> [ExportPattern (Hs.getName name)]
      Hs.InternalName _ -> []
