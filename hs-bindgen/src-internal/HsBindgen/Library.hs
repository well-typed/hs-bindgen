-- | Plan a library: place declarations in header units, then plan the output
--
-- Library mode turns each C header under a @--library@ directory into a
-- /header unit/: everything the run generates for that header. Only headers
-- that the run parses count, that is, the root headers and the headers they
-- include. A unit has one module name, derived from the header path
-- ('moduleNameFromPath'). It can write
--
-- * one module per binding category: the types module, such as
--   @Lib.Geo.Point@, and its sub-modules @Lib.Geo.Point.Safe@, @.Unsafe@,
--   @.FunPtr@, @.Global@ and @.Callback@;
-- * a boot file for the types module;
-- * a binding specification file.
--
-- The /vendor declarations/ form one unit, with the module name
-- @Lib.Vendor@. They are the declarations of headers outside every library
-- directory, and the declarations without a header, such as the macros of
-- @--hash-define@. The unit holds those that the selection keeps, usually
-- because program slicing pulls in what the library uses.
--
-- One more module can be generated: the global callback module, such as
-- @Lib.Callback@. It holds the @ToFunPtr@ and @FromFunPtr@ instances of the raw
-- function types that mention a type the run generates, such as
-- @Ptr S -> IO ()@ for a field @void (*f)(struct s *)@ (see
-- "HsBindgen.Frontend.Pass.FunPtrInstances"). Two headers can use the same raw
-- function type, and one
-- shared module defines each instance only once. Function types that mention
-- no such type get no instance here.
-- @hs-bindgen-runtime@ provides instances for the common ones.
--
-- Library mode plans in two steps.
--
-- 1. 'placeDecls' runs before name mangling. It decides the unit of each
--    declaration, and derives the module name of each unit. Name mangling
--    needs nothing else.
-- 2. 'planLibrary' runs on the translation unit after the final selection. It
--    decides which declarations each unit gets, which units form import
--    cycles, which get boot files, and which files the run writes. It then
--    rejects the plan if two units have the same module name, or two files the
--    same path. The backend has not run yet, so a rejected run writes nothing.
--
-- Intended for qualified import.
module HsBindgen.Library (
    -- * Placement
    Placement(..)
  , UnitHeader(..)
  , unitModuleName
  , unitHeaders
  , lookupModule
  , placeDecls
    -- * Plan
  , PlanConfig(..)
  , Plan(..)
  , HeaderUnit(..)
  , planLibrary
    -- * Output files
  , planOutputs
  , globalCallbackOutput
  , OutputFile(..)
  , OutputRoot(..)
  , OutputOrigin(..)
  , outputFilePath
  , bindingSpecFileName
  , headerIncludeArgs
    -- * Errors
  , PlanError(..)
    -- * Module name derivation
  , moduleNameFromPath
  ) where

import Data.Char qualified as Char
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Optics.Core (view)
import System.FilePath (dropExtension, isRelative, makeRelative,
                        splitDirectories, (</>))
import System.FilePath.Posix qualified as Posix
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types (singleLocPath)
import Clang.Paths (RealPath, getRealPath)

import HsBindgen.Backend.Category (ByCategory (..), Category (..), Choice (..),
                                   HsBootPolicy (..), TermCategory (..),
                                   lensForTermCategory)
import HsBindgen.Backend.Hs.Translation (callbackCategory, declCategories)
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config.Prelims (BaseModuleName (..), fromBaseModuleName)
import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.DeclDB (DeclDB)
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.Pass.Final (Final)
import HsBindgen.Frontend.Predicate (findLibraryDir)
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.FunPtrInstance
import HsBindgen.IR.Pass (Id)
import HsBindgen.IR.Translation (DeclIdPair (..))
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer (PrettyForTrace (..))

{-------------------------------------------------------------------------------
  Placement
-------------------------------------------------------------------------------}

-- | The unit and module of every declaration that 'placeDecls' places
--
-- 'placeDecls' places every declaration that is still active after the
-- @ResolveBindingSpecs@ pass. It leaves out the unusable declarations, such as
-- a struct that failed to parse. It also leaves out the declarations that a
-- prescriptive binding specification omits, or that an external one replaces.
-- A library declaration has its header under a
-- @--library@ directory, and goes to the unit of that header. Every other
-- declaration is a vendor declaration, and goes to the unit of the vendor
-- declarations, whose module is @\<base\>.Vendor@.
--
-- Two headers can get the same module name, such as @_foo.h@ and @foo.h@
-- (see 'moduleNameFromPath'). The placement keeps both. Whether that is an
-- error depends on which header units end up writing files, so 'planLibrary'
-- decides (see 'checkModuleNames'). The vendor module takes part too: a header
-- such as @vendor.h@ directly under a library directory also gets the module
-- name @Lib.Vendor@.
--
-- Until then, name mangling uses that module name for the declarations of both
-- headers: in their Haskell identifiers, and as the scope in which two names
-- clash. So a name in one header can clash with a name in the other, even when
-- the other header ends up writing nothing. With
--
-- > // lib/_foo.h
-- > struct x { int a; };
-- >
-- > // lib/foo.h
-- > typedef int x;
-- > int f(x v);
--
-- both @struct x@ and the typedef get the Haskell name @X@ in @Lib.Foo@. Name
-- mangling reports the clash and drops both, and @f@ with them, even when the
-- selection keeps only @foo.h@. Single-header mode reports the same clash for
-- these two headers.
--
-- A library header such as @vendor.h@ shares its module name with the vendor
-- declarations in the same way. Name mangling runs before the selection, so
-- it scopes the names of @vendor.h@ together with every vendor declaration,
-- also those that the selection drops later.
--
-- This is a known limitation. Avoiding it would need name mangling to scope
-- clashes by header rather than by module name. However, to hit this limitation
-- three things need to happen: two units with the same module name, a selection
-- that leaves one of them with nothing to write, and a tag in one unit with the
-- same name as an ordinary identifier, such as a typedef, in the other.
--
data Placement = Placement {
      -- | The unit of every placed declaration
      unitOf       :: Map C.DeclId UnitHeader
      -- | The module name of the vendor declarations, @\<base\>.Vendor@
    , vendorModule :: Hs.ModuleName
    }

-- | The headers whose declarations a header unit holds
--
-- 'LibraryHeader' carries the module name of the unit.
-- 'VendorHeaders' is nullary; its module name lives in
-- 'Placement'.'vendorModule'.
data UnitHeader =
    -- | One header under a @--library@ directory
    LibraryHeader RealPath Hs.ModuleName
    -- | The vendor declarations: those of the headers outside every
    -- @--library@ directory, and those without a header
  | VendorHeaders
  deriving stock (Show, Eq, Ord)

-- | The module name of a unit
--
-- The first argument is the vendor module name from the 'Placement'.
unitModuleName :: Hs.ModuleName -> UnitHeader -> Hs.ModuleName
unitModuleName _            (LibraryHeader _ m) = m
unitModuleName vendorModule VendorHeaders       = vendorModule

-- | The distinct units in a placement
unitHeaders :: Placement -> Set UnitHeader
unitHeaders placement = Set.fromList (Map.elems placement.unitOf)

-- | The unit of a placed declaration
--
-- Panics if 'placeDecls' never placed the declaration.
unitOf :: Placement -> C.DeclId -> UnitHeader
unitOf placement declId =
    fromMaybe (notPlaced declId) (Map.lookup declId placement.unitOf)

-- | The module of a declaration, if 'placeDecls' placed it
--
-- Name mangling needs this lookup. Besides the placed declarations, it meets
-- the declarations that a typedef refers to, and some of those are not
-- active: unusable ones, such as a struct that failed to parse, and ones that
-- a prescriptive binding specification omits. 'placeDecls' never places such
-- a declaration, and it generates nothing.
lookupModule :: Placement -> C.DeclId -> Maybe Hs.ModuleName
lookupModule placement declId =
    unitModuleName placement.vendorModule <$> Map.lookup declId placement.unitOf

notPlaced :: C.DeclId -> a
notPlaced declId =
    panicPure $ "library mode never placed the declaration " ++ show declId

-- | Place each declaration in its unit
--
-- Takes the active declarations after the @ResolveBindingSpecs@ pass, before
-- name mangling. That pass applies both kinds of binding specification.
--
-- Fails if no declaration has a header under a library directory.
placeDecls ::
     forall l p.
     (Id p ~ C.DeclId)
  => [FilePath]    -- ^ Library directories (canonicalized)
  -> Hs.ModuleName -- ^ Base module name prefix (e.g. @\"Lib\"@)
  -> [C.Decl l p]
  -> Either PlanError Placement
placeDecls libraryDirs baseModule decls
    -- Report a run without library declarations rather than silently
    -- generating nothing.
    | not (any isLibraryUnit (Map.elems declUnits)) =
        Left (NoHeadersFound libraryDirs)
    | otherwise =
        Right Placement {
            unitOf       = declUnits
          , vendorModule = vendorMod
          }
  where
    declUnits :: Map C.DeclId UnitHeader
    declUnits = Map.fromList [
        (decl.id, placeDecl decl)
      | decl <- decls
      ]

    placeDecl :: C.Decl l p -> UnitHeader
    placeDecl decl = case libraryHeader decl of
      Just (header, libDir) ->
        LibraryHeader header (moduleNameFromPath baseModule libDir header)
      Nothing ->
        VendorHeaders

    isLibraryUnit :: UnitHeader -> Bool
    isLibraryUnit (LibraryHeader _ _) = True
    isLibraryUnit VendorHeaders       = False

    vendorMod :: Hs.ModuleName
    vendorMod = Hs.ModuleName $ baseModule.text <> ".Vendor"

    -- The header of a library declaration, with the library directory that
    -- contains it
    libraryHeader :: C.Decl l p -> Maybe (RealPath, FilePath)
    libraryHeader decl = do
        header <- C.declPathRealPath (singleLocPath decl.info.loc)
        libDir <- findLibraryDir libraryDirs header
        pure (header, libDir)

{-------------------------------------------------------------------------------
  Plan
-------------------------------------------------------------------------------}

-- | What the plan needs from the configuration of the run
data PlanConfig = PlanConfig {
      baseModule        :: Hs.ModuleName
    , categoryChoice    :: ByCategory Choice
      -- | The format of the binding specification files, if the run writes
      -- them
    , bindingSpecFormat :: Maybe BindingSpec.Format
    }

-- | The header units of a library
--
data Plan l = Plan {
      -- | The header units that write at least one file: the units of library
      -- headers in header path order, then the unit of the vendor
      -- declarations
      units          :: [HeaderUnit l]
      -- | The global callback module, if the run writes it, with the
      -- instances for raw function types ('HeadFunType') that it holds, in
      -- output order
    , globalCallback :: Maybe (Hs.ModuleName, NonEmpty FunPtrInstance)
    }

-- | The plan for one header unit (see the module header)
data HeaderUnit l = HeaderUnit {
      -- | The name of the unit's types module, and the prefix of its
      -- sub-modules
      moduleName   :: Hs.ModuleName
    , header       :: UnitHeader
      -- | The active declarations of the unit, in output order
    , decls        :: [C.Decl l Final]
      -- | The @ToFunPtr@ and @FromFunPtr@ instances for the newtypes of the
      -- unit ('HeadNewtype'), in output order
    , instances    :: [FunPtrInstance]
      -- | The typedefs squashed into a declaration of this unit, for the unit's
      -- binding specification
    , squashedIn   :: Map C.DeclId (RealPath, Hs.Name Hs.NsTypeConstr)
      -- | The declarations of the header that a prescriptive binding
      -- specification omits, each with the header that declares it, for the
      -- unit's binding specification
      --
      -- The map also holds each typedef squashed into one of those omitted
      -- declarations (see 'DeclDB.getOmitted'), with its own header. If the
      -- prescriptive specification omits @struct S@ from @b.h@, and @a.h@ has
      -- @typedef struct S S;@, both are in the map of @b.h@'s unit: @struct S@
      -- with @b.h@, and the typedef with @a.h@. The unit's binding
      -- specification has an @omit@ entry for each.
      --
      -- Empty for the unit of the vendor declarations. The placement knows only
      -- the library headers with active declarations, so the plan cannot tell
      -- the header of an omitted vendor declaration from a library header
      -- without them.
    , omitted      :: Map C.DeclId RealPath
      -- | The module names of the other units in this unit's import cycle that
      -- its types refer to directly. Empty outside a cycle.
      --
      -- The types module imports the boot file of each of them with a
      -- @{-\# SOURCE \#-}@ import.
    , sourcePeers  :: Set Hs.ModuleName
      -- | 'GenerateHsBoot' in an import cycle, 'DoNotGenerateHsBoot' outside
    , hsBootPolicy :: HsBootPolicy
      -- | The binding categories that get a module, after the user's category
      -- choice
    , categories   :: Set Category
      -- | Every file the unit writes (see 'unitOutputs')
    , outputs      :: NonEmpty OutputFile
    }

-- | Plan the header units and output files of a library
--
-- Takes the translation unit after the final selection. Each declaration that
-- is active there goes to the unit of its header, or to the unit of the vendor
-- declarations. The user's category choice decides which of its bindings are
-- written.
--
-- Fails if two units have the same module name (see 'checkModuleNames'), or
-- two output files the same path (see 'checkOutputPaths').
--
-- For example, with @--library lib --module Lib --enable-program-slicing@:
--
-- > // lib/a.h
-- > struct a { struct b *b; };
-- > int a_size(struct a *p);
-- >
-- > // lib/b.h
-- > struct b { struct a *a; };
-- >
-- > // lib/c.h, with vendor/v.h outside every library directory
-- > #include "v.h"
-- > struct c { struct v *v; };
--
-- * Program slicing selects the vendor declaration @struct v@, because
--   @struct c@ uses it. It goes to the vendor module @Lib.Vendor@, which
--   @Lib.C@ imports.
-- * The types of @Lib.A@ and @Lib.B@ refer to each other. Each module imports
--   the other with a @SOURCE@ import, so both get a boot file.
-- * The run writes @Lib\/A.hs@, @Lib\/A.hs-boot@, @Lib\/A\/Safe.hs@,
--   @Lib\/A\/Unsafe.hs@, @Lib\/A\/FunPtr.hs@, @Lib\/B.hs@,
--   @Lib\/B.hs-boot@, @Lib\/C.hs@ and @Lib\/Vendor.hs@.
planLibrary ::
     forall l.
     PlanConfig
  -> Placement
  -> C.TranslationUnit l Final
  -> [FunPtrInstance] -- ^ In output order
  -> Either PlanError (Plan l)
planLibrary config placement tu insts = do
    -- 1. The active declarations, in output order
    --
    -- A vendor declaration is active when the selection keeps it and no
    -- external binding specification covers it.
    let activeDecls :: [C.Decl l Final]
        activeDecls =
          DeclDB.active (DeclUseGraph.outputOrder tu.includeGraph) tu.db

    -- 2. The SOURCE imports between the types modules of each import cycle
    let sourcePeers :: Map Hs.ModuleName (Set Hs.ModuleName)
        sourcePeers = findSourcePeers tu.db
                    [ ( unitModuleName placement.vendorModule
                          (unitOf placement decl.id.cName)
                      , decl
                      )
                    | decl <- activeDecls
                    ]

    -- 3. One unit per library header and one for the vendor declarations,
    -- without the units that write no file
    let declsByUnit :: Map UnitHeader [C.Decl l Final]
        declsByUnit = Map.map reverse $ Map.fromListWith (++)
                    [ (unitOf placement decl.id.cName, [decl])
                    | decl <- activeDecls
                    ]

        -- 'DeclDB.getOmitted' gives a typedef squashed into an omitted
        -- declaration the header of that declaration, which decides the unit.
        -- The entry keeps the header that declares the typedef, unless the
        -- typedef is in no header.
        omittedByHeader :: Map RealPath (Map C.DeclId RealPath)
        omittedByHeader = Map.fromListWith Map.union [
            (header, Map.singleton declId ownHeader)
          | (declId, header) <- Map.toList (DeclDB.getOmitted tu.db)
          , let ownHeader = fromMaybe header (declaringHeader declId)
          ]

        declaringHeader :: C.DeclId -> Maybe RealPath
        declaringHeader declId = do
          locs <- DeclDB.lookupLoc declId tu.db
          C.declPathRealPath (singleLocPath (C.declLocsMin locs))

        planUnit :: UnitHeader -> Maybe (HeaderUnit l)
        planUnit unitHeader = do
            outputs <- NonEmpty.nonEmpty $
              unitOutputs
                config.bindingSpecFormat
                modName
                unitHeader
                unitBootPolicy
                categories
                squashedIn
            pure HeaderUnit {
                moduleName   = modName
              , header       = unitHeader
              , decls        = unitDecls
              , instances    = unitInstances
              , squashedIn   = squashedIn
              , omitted      = unitOmitted
              , sourcePeers  = unitSourcePeers
              , hsBootPolicy = unitBootPolicy
              , categories   = categories
              , outputs      = outputs
              }
          where
            modName :: Hs.ModuleName
            modName = unitModuleName placement.vendorModule unitHeader

            unitDecls :: [C.Decl l Final]
            unitDecls = Map.findWithDefault [] unitHeader declsByUnit

            -- A newtype is defined in the module that its identifier names
            unitInstances :: [FunPtrInstance]
            unitInstances = [
                inst
              | inst@FunPtrInstance{head = HeadNewtype ident _} <- insts
              , ident.moduleName == modName
              ]

            squashedIn :: Map C.DeclId (RealPath, Hs.Name Hs.NsTypeConstr)
            squashedIn =
              DeclDB.getSquashed tu.db $
                Set.fromList (map (.id.cName) unitDecls)

            unitOmitted :: Map C.DeclId RealPath
            unitOmitted = case unitHeader of
              LibraryHeader header _ ->
                Map.findWithDefault Map.empty header omittedByHeader
              VendorHeaders ->
                Map.empty

            unitSourcePeers :: Set Hs.ModuleName
            unitSourcePeers = Map.findWithDefault Set.empty modName sourcePeers

            unitBootPolicy :: HsBootPolicy
            unitBootPolicy
              | Set.null unitSourcePeers = DoNotGenerateHsBoot
              | otherwise                = GenerateHsBoot

            categories :: Set Category
            categories = Set.filter (categoryIncluded config.categoryChoice)
                       $ foldMap declCategories unitDecls
                      <> Set.fromList [
                             callbackCategory unitBootPolicy
                           | not (null unitInstances)
                           ]

        units :: [HeaderUnit l]
        units = mapMaybe planUnit (Set.toList (unitHeaders placement))

    -- 4. The global callback module, written when a raw function type needs
    -- callback instances
    let globalCallback :: Maybe (Hs.ModuleName, NonEmpty FunPtrInstance)
        globalCallback = do
          funTypeInsts <- NonEmpty.nonEmpty [
              inst
            | inst@FunPtrInstance{head = HeadFunType} <- insts
            ]
          pure ( fromBaseModuleName
                  (BaseModuleName config.baseModule.text)
                  (Just (CTerm CCallback))
               , funTypeInsts
               )

        plan :: Plan l
        plan = Plan {
            units          = units
          , globalCallback = globalCallback
          }

    -- 5. No two units with the same module name, and no two files with the
    -- same path
    checkModuleNames units
    checkOutputPaths (planOutputs plan)
    pure plan

-- | For each types module in an import cycle, the other modules of the cycle
-- that it imports
--
-- These imports are @{-\# SOURCE \#-}@ imports. Every module in a cycle
-- gets a boot file. Modules outside every cycle are absent from the result.
--
-- Only types modules form cycles: a term module (such as @Safe@ or @Global@)
-- imports types modules, but no types module imports a term module. A types
-- module imports the modules of the types its own types refer to directly. A
-- typedef squashed into a type has no binding of its own, so a reference to
-- the typedef leads to the module of the type. For example:
--
-- > // lib/a.h
-- > typedef struct S S;          // squashed into struct S
-- >
-- > // lib/b.h
-- > struct S { struct C *c; };
-- >
-- > // lib/c.h
-- > #include "a.h"
-- > struct C { S *s; };          // refers to the typedef from a.h
--
-- @Lib.B@ imports @Lib.C@, and @Lib.C@ imports @Lib.B@ rather than @Lib.A@.
-- So @Lib.B@ and @Lib.C@ form a cycle, and each imports the other with a
-- @SOURCE@ import. Nothing is left for @a.h@.
findSourcePeers ::
     forall l.
     DeclDB l Final
  -> [(Hs.ModuleName, C.Decl l Final)] -- ^ Active declarations with their modules
  -> Map Hs.ModuleName (Set Hs.ModuleName)
findSourcePeers db decls = Map.fromList
    [ ( modName
      , Map.findWithDefault Set.empty modName imports
          `Set.intersection`
        members
      )
    | CyclicSCC cycleMembers <- stronglyConnComp [
          (modName, modName, Set.toList deps)
        | (modName, deps) <- Map.toList imports
        ]
    , let members = Set.fromList cycleMembers
    , modName <- cycleMembers
    ]
  where
    useDeclGraph :: UseDeclGraph
    useDeclGraph = UseDeclGraph.fromDeclUseGraph db.declUseGraph

    -- The other types modules that each types module imports
    imports :: Map Hs.ModuleName (Set Hs.ModuleName)
    imports = Map.fromListWith Set.union [
        (modName, Set.fromList depModules)
      | (declId, modName) <- Map.toList ctypesModuleOf
      , let depModules = [
                depModule
              | dep <- Set.toList
                     $ UseDeclGraph.getDirectDeps useDeclGraph (Set.singleton declId)
              , Just depModule <- [Map.lookup (squashTarget dep) ctypesModuleOf]
              , depModule /= modName
              ]
      ]

    -- The (C)types module of each declaration that has bindings there
    ctypesModuleOf :: Map C.DeclId Hs.ModuleName
    ctypesModuleOf = Map.fromList [
        (decl.id.cName, modName)
      | (modName, decl) <- decls
      , CType `Set.member` declCategories decl
      ]

    squashTarget :: C.DeclId -> C.DeclId
    squashTarget declId =
      case DeclDB.lookupEntry declId db of
        Just (DeclDB.Squashed _ target) -> target.c
        _otherwise                      -> declId


-- | The files a header unit writes
--
-- One module per binding category. A types module gets a boot file under
-- 'GenerateHsBoot'. With @--gen-binding-spec-dir@, a unit that has a types
-- module or a squashed typedef also gets a binding specification file.
--
unitOutputs ::
     Maybe BindingSpec.Format
  -> Hs.ModuleName
  -> UnitHeader
  -> HsBootPolicy
  -> Set Category -- ^ Categories that get a module
  -> Map C.DeclId (RealPath, Hs.Name Hs.NsTypeConstr)
     -- ^ Typedefs squashed into the unit's types
  -> [OutputFile]
unitOutputs mFormat modName header hsBootPolicy categories squashedIn =
    concat [
        [ OutputFile {
              root   = HsOutputDir
            , path   = Hs.moduleNamePath $
                fromBaseModuleName (BaseModuleName modName.text) (Just cat)
            , origin = HeaderModule header cat
            }
        | cat <- Set.toList categories
        ]
      , [ OutputFile {
              root   = HsOutputDir
            , path   = Hs.moduleNameBootPath modName
            , origin = HeaderBootFile header
            }
        | hasTypesModule
        , hsBootPolicy == GenerateHsBoot
        ]
      , [ OutputFile {
              root   = BindingSpecDir
            , path   = bindingSpecFileName format modName
            , origin = BindingSpecFile header
            }
        | hasTypesModule || not (Map.null squashedIn)
        , Just format <- [mFormat]
        ]
      ]
  where
    hasTypesModule :: Bool
    hasTypesModule = CType `Set.member` categories

-- | Does the user's category choice keep the module of a category?
categoryIncluded :: ByCategory Choice -> Category -> Bool
categoryIncluded choice = \case
    CType     -> included choice.cType
    CTerm cat -> included (view (lensForTermCategory cat) choice)
  where
    included :: Choice lvl -> Bool
    included = \case
      ExcludeCategory       -> False
      IncludeTypeCategory   -> True
      IncludeTermCategory _ -> True

-- | Reject two header units with the same module name
--
-- Different headers can get the same module name (see 'moduleNameFromPath').
-- The check only looks at units in the plan, which write at least one file. A
-- header with nothing left to write takes no part: with @_foo.h@ and @foo.h@,
-- both @Lib.Foo@, a run that selects only @foo.h@ succeeds. That is why the
-- check runs here and not in 'placeDecls': only the plan knows which headers
-- write files. The unit of the vendor declarations takes part like any other:
-- a library header such as @vendor.h@ with something to write collides with
-- it.
--
-- Two units can still be rejected even when they write no file in common:
--
-- > // lib/_foo.h
-- > struct point { int x, y; };
-- >
-- > // lib/foo.h
-- > int area(int w, int h);
--
-- @_foo.h@ writes @Lib\/Foo.hs@, and @foo.h@ writes @Lib\/Foo\/Safe.hs@,
-- @Lib\/Foo\/Unsafe.hs@ and @Lib\/Foo\/FunPtr.hs@. No path appears twice,
-- so 'checkOutputPaths' finds nothing. But the result would mislead: @Lib.Foo@
-- and @Lib.Foo.Safe@ look like the bindings of one header, yet they belong to
-- two different units.
--
-- Module names that differ only in case, such as @Lib.Point@ and @Lib.POINT@,
-- are different modules. 'checkOutputPaths' rejects them if they write the
-- same file on a case-insensitive file system.
checkModuleNames :: forall l. [HeaderUnit l] -> Either PlanError ()
checkModuleNames units =
    case [ (u1, u2) | u1 : u2 : _ <- Map.elems byModuleName ] of
      []           -> Right ()
      (u1, u2) : _ ->
        Left $ ModuleNameCollision u1.moduleName
          (u1.header, u1.outputs)
          (u2.header, u2.outputs)
  where
    byModuleName :: Map Hs.ModuleName [HeaderUnit l]
    byModuleName = Map.map reverse
                 $ Map.fromListWith (++)
                 [ (unit.moduleName, [unit])
                 | unit <- units
                 ]

{-------------------------------------------------------------------------------
  Output files
-------------------------------------------------------------------------------}

-- | Every file the run writes
--
-- The collision check, the dry-run listing and the comparison with the files
-- actually written all use this list.
planOutputs :: Plan l -> [OutputFile]
planOutputs plan =
    concatMap (NonEmpty.toList . (.outputs)) plan.units
    ++ maybeToList (globalCallbackOutput plan)

-- | The 'OutputFile' for the global callback module, if the plan has one
globalCallbackOutput :: Plan l -> Maybe OutputFile
globalCallbackOutput plan = do
    (modName, _insts) <- plan.globalCallback
    pure OutputFile {
        root   = HsOutputDir
      , path   = Hs.moduleNamePath modName
      , origin = GlobalCallbackModule
      }

-- | The directory an output file is relative to
data OutputRoot =
    -- | @--hs-output-dir@
    HsOutputDir
    -- | @--gen-binding-spec-dir@
  | BindingSpecDir
  deriving stock (Show, Eq, Ord)

-- | Why the run writes a file
data OutputOrigin =
    -- | The module of a binding category of a unit
    HeaderModule UnitHeader Category
    -- | The boot file of the types module of a unit
  | HeaderBootFile UnitHeader
    -- | The module of the callbacks of raw function types, shared by every
    -- unit
  | GlobalCallbackModule
    -- | The binding specification of the module of a unit
  | BindingSpecFile UnitHeader
  deriving stock (Show, Eq, Ord)

-- | A file the run writes
data OutputFile = OutputFile {
      root   :: OutputRoot
      -- | Relative to the root
    , path   :: FilePath
    , origin :: OutputOrigin
    }
  deriving stock (Show)

-- | Where the run writes a file, given the Haskell output directory and the
-- binding specification directory
--
-- Panics on a binding specification file without a binding specification
-- directory: the plan has none then.
outputFilePath :: FilePath -> Maybe FilePath -> OutputFile -> FilePath
outputFilePath hsOutputDir mBindingSpecDir file =
    case file.root of
      HsOutputDir    -> hsOutputDir </> file.path
      BindingSpecDir -> case mBindingSpecDir of
        Just dir -> dir </> file.path
        Nothing  -> panicPure $
          "binding specification file without a directory: " ++ file.path

-- | The name of the binding specification file of a module, such as
-- @Lib.Geo.Point.bindingspec.yaml@
bindingSpecFileName :: BindingSpec.Format -> Hs.ModuleName -> FilePath
bindingSpecFileName format modName =
    Hs.moduleNameToString modName ++ case format of
      BindingSpec.FormatJSON -> ".bindingspec.json"
      BindingSpec.FormatYAML -> ".bindingspec.yaml"

-- | The names of a header relative to each include directory that contains
-- it, such as @rpm\/argv.h@ for the header @\/usr\/include\/rpm\/argv.h@ and
-- the include directory @\/usr\/include@
--
-- A binding specification names the header of each entry this way, so that a
-- run with the same include directories can read the specification back.
headerIncludeArgs :: [FilePath] -> RealPath -> [C.HashIncludeArg]
headerIncludeArgs includeDirs header = [
      C.HashIncludeArg (Posix.joinPath (splitDirectories relPath))
    | dir <- includeDirs
    , let relPath = makeRelative dir (getRealPath header)
    , isRelative relPath
    ]

-- | Reject two output files with the same path
--
-- Paths are compared within each root, ignoring case. On a case-insensitive
-- file system (the default on macOS and Windows), paths that differ only in
-- case name the same file. The check ignores case on every system, so a run
-- that succeeds on Linux also succeeds on macOS.
--
-- For example, @Point.h@ and @POINT.h@ get the module names @Lib.Point@ and
-- @Lib.POINT@. If both have types, they write @Lib\/Point.hs@ and
-- @Lib\/POINT.hs@, one file on macOS, and the run is rejected. If one has only
-- types and the other only functions, they write @Lib\/Point.hs@ and
-- @Lib\/POINT\/Safe.hs@ (and so on), and the run succeeds.
--
-- Two files can have the same path without their units sharing a module name:
-- a header @a\/safe.h@ has the types module @Lib.A.Safe@, which is also the
-- name of the @Safe@ module of @a.h@.
checkOutputPaths :: [OutputFile] -> Either PlanError ()
checkOutputPaths files =
    case [ (f1, f2) | f1 : f2 : _ <- Map.elems byPath ] of
      []           -> Right ()
      (f1, f2) : _ -> Left (OutputPathCollision f1 f2)
  where
    byPath :: Map (OutputRoot, FilePath) [OutputFile]
    byPath = Map.map reverse
           $ Map.fromListWith (++)
           [ ((file.root, map Char.toLower file.path), [file])
           | file <- files
           ]

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Planning errors that prevent the library from being generated.
data PlanError =
    -- | Two header units have the same module name: the module name, then the
    -- headers and the files of each unit (see 'checkModuleNames')
    ModuleNameCollision
      Hs.ModuleName
      (UnitHeader, NonEmpty OutputFile)
      (UnitHeader, NonEmpty OutputFile)
    -- | No library declaration: no active declaration after the
    -- @ResolveBindingSpecs@ pass is in a header under the library directories
    -- (see 'placeDecls')
    --
    -- The run does not scan the directories. A header under them counts only
    -- if the run parses it, as a root header or through an @#include@, and it
    -- declares something.
  | NoHeadersFound [FilePath]
    -- | Two output files have the same path within their root, ignoring case
    -- (see 'checkOutputPaths')
  | OutputPathCollision OutputFile OutputFile
  deriving stock (Show)

instance PrettyForTrace PlanError where
  prettyForTrace = \case
    ModuleNameCollision m (h1, files1) (h2, files2) ->
      PP.vcat [
          PP.hsep [
              "Module name collision:"
            , prettyUnitHeader h1
            , "and"
            , prettyUnitHeader h2
            , "both get the module name"
            , prettyForTrace m
            ]
        , PP.nest 2 $ PP.vcat [
              PP.hsep [
                  prettyUnitHeader h
                , writes h
                , PP.string $ List.intercalate ", " $
                    List.sort (map (.path) (NonEmpty.toList files))
                ]
            | (h, files) <- [(h1, files1), (h2, files2)]
            ]
        ]
    NoHeadersFound dirs ->
      PP.hsep [
          "No C headers found under library"
        , if length dirs == 1 then "directory:" else "directories:"
        , PP.string (List.intercalate ", " dirs)
        ]
    OutputPathCollision f1 f2 ->
      PP.hsep [
          "Output path collision:"
        , PP.string f1.path
        , PP.parens (prettyOutputOrigin f1.origin)
        , "vs"
        , PP.string f2.path
        , PP.parens (prettyOutputOrigin f2.origin)
        ]
    where
      prettyOutputOrigin :: OutputOrigin -> PP.CtxDoc
      prettyOutputOrigin = \case
        HeaderModule header cat ->
          PP.hsep [categoryModule cat, "of", prettyUnitHeader header]
        HeaderBootFile header ->
          PP.hsep ["boot file of", prettyUnitHeader header]
        GlobalCallbackModule ->
          "global callback module"
        BindingSpecFile header ->
          PP.hsep ["binding specification of", prettyUnitHeader header]

      prettyUnitHeader :: UnitHeader -> PP.CtxDoc
      prettyUnitHeader = \case
        LibraryHeader header _ -> PP.string (getRealPath header)
        VendorHeaders          -> "the vendor declarations"

      -- The verb after 'prettyUnitHeader', in agreement with it
      writes :: UnitHeader -> PP.CtxDoc
      writes = \case
        LibraryHeader _ _ -> "writes"
        VendorHeaders     -> "write"

      categoryModule :: Category -> PP.CtxDoc
      categoryModule = \case
        CType           -> "Types module"
        CTerm CCallback -> "Callback module"
        CTerm CSafe     -> "Safe module"
        CTerm CUnsafe   -> "Unsafe module"
        CTerm CFunPtr   -> "FunPtr module"
        CTerm CGlobal   -> "Global module"

{-------------------------------------------------------------------------------
  Module name derivation
-------------------------------------------------------------------------------}

-- | Derive a Haskell module name from a header's path.
--
-- Strips the library directory prefix and file extension, then converts
-- each path component into a valid Haskell module component: drop dots and
-- leading non-alphanumeric characters, replace other non-alphanumeric
-- characters with @_@, upper-case the first letter, and prefix with @C@ if the
-- component starts with a digit.
--
-- > moduleNameFromPath "Lib" "/inc" (RealPath "/inc/geo/point.h")
-- >   == ModuleName "Lib.Geo.Point"
--
-- Different headers can get the same name. Leading non-alphanumeric
-- characters are dropped (@_foo.h@ and @foo.h@), other ones become @_@
-- (@foo-bar.h@ and @foo_bar.h@), and the first letter is upper-cased (@Foo.h@
-- and @foo.h@). 'planLibrary' accepts such a pair only when one of the two
-- headers writes no file (see 'checkModuleNames'). Names that differ in case
-- past the first letter, such as @Lib.Point@ and @Lib.POINT@, are different
-- modules. 'planLibrary' rejects them only if they write the same file on a
-- case-insensitive file system (see 'checkOutputPaths').
moduleNameFromPath ::
     Hs.ModuleName -- ^ Base module name prefix
  -> FilePath      -- ^ Library directory
  -> RealPath      -- ^ Header path (absolute, canonical)
  -> Hs.ModuleName
moduleNameFromPath (Hs.ModuleName base) libDir header =
    Hs.ModuleName $ base <> "." <> Text.intercalate "." components
  where
    relPath = makeRelative libDir (getRealPath header)
    components =
      map (sanitizeComponent . Text.pack) $
        splitDirectories (dropExtension relPath)

-- | Sanitize a single path component into a valid Haskell module component.
sanitizeComponent :: Text.Text -> Text.Text
sanitizeComponent t = case Text.uncons stripped of
    Nothing       -> "C"
    Just (c, rest)
      | Char.isDigit c -> "C" <> stripped
      | otherwise      -> Text.cons (Char.toUpper c) rest
  where
    sanitized = Text.map (\c -> if Char.isAlphaNum c then c else '_')
              $ Text.filter (/= '.') t
    stripped  = Text.dropWhile (not . Char.isAlphaNum) sanitized
