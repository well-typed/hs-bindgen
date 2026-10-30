-- | Plan a library: assign declarations to modules, compute dependencies
--
-- Given the parsed translation unit from a whole-library frontend run,
-- determine which header owns which declaration, derive Haskell module names,
-- and compute the inter-module dependency structure (SCCs for boot files,
-- transitive reaches for instance-only imports).
--
-- Intended for qualified import.
module HsBindgen.Library (
    -- * Plan
    LibraryPlan(..)
  , LibraryModule(..)
  , Placement
  , PlanError(..)
  , planLibrary
    -- * Module name derivation
  , moduleNameFromPath
  ) where

import Data.Char qualified as Char
import Data.Graph (SCC (..), stronglyConnComp)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import System.FilePath (dropExtension, isRelative, makeRelative,
                        splitDirectories)

import Clang.HighLevel.Types (singleLocPath)
import Clang.Paths (RealPath, getRealPath)

import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass (Id)
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Maps each library declaration to the Haskell module it belongs to.
--
-- Contains exactly the keys of this map contain only declarations whose header
-- falls under a @--library@ directory. Vendor declarations (from third-party
-- headers that the library includes but that are not under a library directory)
-- are absent.
type Placement = Map C.DeclId Hs.ModuleName

-- | The result of planning a library.
data LibraryPlan = LibraryPlan {
      modules   :: [LibraryModule]
    , placement :: Placement
    }

-- | A single library module, corresponding to one C header.
data LibraryModule = LibraryModule {
      moduleName :: Hs.ModuleName
    , headerPath :: RealPath
      -- | All declarations placed in this module (from 'assignHeaders').
    , placed     :: Set C.DeclId
      -- | Other library modules in the same strongly connected component.
      -- The library runner uses this to emit @{-\# SOURCE \#-}@ imports
      -- between their boot files. Empty when this module is not part of
      -- a cycle.
    , sccPeers   :: Set Hs.ModuleName
      -- | All library modules this module transitively depends on,
      -- excluding itself. Refers only to library modules; vendor
      -- declarations that appear in the 'UseDeclGraph' but are absent
      -- from the 'Placement' do not contribute. The library runner uses
      -- this to generate instance-only imports (@import M.X ()@) in
      -- term modules.
    , reaches    :: Set Hs.ModuleName
    }

{-------------------------------------------------------------------------------
  Planning
-------------------------------------------------------------------------------}

-- | Plan a library from the parsed translation unit.
--
-- The inputs should come from the translation unit after resolving the
-- user's external binding specs and stdlib, but before selection.
planLibrary ::
     (Id p ~ C.DeclId)
  => [FilePath]    -- ^ Library directories (canonicalized)
  -> Hs.ModuleName -- ^ Base module name prefix (e.g. @\"Lib\"@)
  -> UseDeclGraph
  -> [C.Decl l p]
  -> Either PlanError LibraryPlan
planLibrary libraryDirs baseModule useDeclGraph decls = do
    -- Group declarations by their header path and derive a module name
    -- for each header. Declarations from non-library headers are filtered
    -- out.
    let headerMap = assignHeaders libraryDirs baseModule decls

    -- Validate if two headers would produce module names that collide.
    validateModuleNames headerMap

    let -- Make the per-header grouping into a per-declaration one.
        placement  = buildPlacement headerMap

        -- Lift declaration-level dependencies to module-level, i.e. module A
        -- depends on module B when any of A's declarations transitively
        -- reference a declaration placed in B.
        moduleDeps = computeModuleDeps useDeclGraph placement headerMap

        sccs       = computeSCCs moduleDeps
        sccMap     = sccPeerMap sccs

        -- This is needed because modules need instance-only imports for every
        -- library types module they transitively reach, not just direct
        -- neighbors.
        reachesMap = computeReaches moduleDeps

        libraryModules =
          [ LibraryModule {
                moduleName = modName
              , headerPath = header
              , placed     = declIds
              , sccPeers   = Map.findWithDefault Set.empty modName sccMap
              , reaches    = Map.findWithDefault Set.empty modName reachesMap
              }
          | (header, (modName, declIds)) <- Map.toList headerMap
          ]
    pure LibraryPlan {
        modules   = libraryModules
      , placement = placement
      }

-- | Two headers produced module names that differ only in case, which
-- would collide on case-insensitive file systems (macOS HFS+, Windows NTFS).
data PlanError =
    ModuleNameCollision Hs.ModuleName Hs.ModuleName RealPath RealPath
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Step 1: Assign declarations to headers

  For each declaration whose source header falls under a library directory,
  record the header's module name and the declaration's id. Declarations
  from non-library headers are silently skipped.
-------------------------------------------------------------------------------}

type HeaderMap = Map RealPath (Hs.ModuleName, Set C.DeclId)

assignHeaders ::
     (Id p ~ C.DeclId)
  => [FilePath]
  -> Hs.ModuleName
  -> [C.Decl l p]
  -> HeaderMap
assignHeaders libraryDirs baseModule decls =
    Map.fromListWith merge
      [ (header, (moduleNameFromPath baseModule libDir header, Set.singleton decl.id))
      | decl <- decls
      , Just header <- [C.declPathRealPath (singleLocPath decl.info.loc)]
      , Just libDir <- [findLibraryDir libraryDirs header]
      ]
  where
    merge (m, ids1) (_m, ids2) = (m, Set.union ids1 ids2)

-- | Find which library directory (if any) contains the given header.
-- 'makeRelative' returns a relative path only when the directory is a
-- genuine prefix; otherwise it returns the original absolute path.
findLibraryDir :: [FilePath] -> RealPath -> Maybe FilePath
findLibraryDir dirs header =
    case [ d
         | d <- dirs
         , isRelative (makeRelative d (getRealPath header))
         ] of
      []    -> Nothing
      (d:_) -> Just d

{-------------------------------------------------------------------------------
  Step 2: Build placement map

  Invert the header map into a flat declaration-to-module mapping.
-------------------------------------------------------------------------------}

buildPlacement :: HeaderMap -> Placement
buildPlacement headerMap = Map.fromList
    [ (declId, modName)
    | (modName, declIds) <- Map.elems headerMap
    , declId <- Set.toList declIds
    ]

{-------------------------------------------------------------------------------
  Step 3: Module dependency graph

  For each module, compute which other library modules it depends on.
  The transitive closure of a module's declarations (via UseDeclGraph)
  may include vendor declarations that are absent from the placement;
  those are dropped here, because we query the placement map.
-------------------------------------------------------------------------------}

type ModuleDeps = Map Hs.ModuleName (Set Hs.ModuleName)

computeModuleDeps ::
     UseDeclGraph
  -> Placement
  -> HeaderMap
  -> ModuleDeps
computeModuleDeps useDeclGraph placement headerMap =
    Map.fromList
      [ (modName, deps)
      | (modName, declIds) <- Map.elems headerMap
            -- Here there might be other third-party declarations.
      , let transitiveDeps = UseDeclGraph.getTransitiveDeps useDeclGraph declIds
            -- Collect the other library modules that own at least one
            -- declaration in the transitive closure. Unplaced declarations
            -- (non-library headers) have no entry in placement and are
            -- skipped by the pattern match.
            deps = Set.fromList
              [ depMod
              | depId <- Set.toList transitiveDeps
                -- By querying placement we don't get any third-party declId's
                -- modules.
              , Just depMod <- [Map.lookup depId placement]
              , depMod /= modName
              ]
      ]

{-------------------------------------------------------------------------------
  Step 4: SCCs

  Strongly connected components of the module dependency graph. Modules
  in a cyclic SCC need boot files and {-# SOURCE #-} imports.
-------------------------------------------------------------------------------}

computeSCCs :: ModuleDeps -> [SCC Hs.ModuleName]
computeSCCs moduleDeps =
    stronglyConnComp
      [ (modName, modName, Set.toList deps)
      | (modName, deps) <- Map.toList moduleDeps
      ]

-- | For each module in a cyclic SCC, record the other members.
-- Acyclic modules (singletons) are absent from the result.
sccPeerMap :: [SCC Hs.ModuleName] -> Map Hs.ModuleName (Set Hs.ModuleName)
sccPeerMap sccs = Map.fromList
    [ (m, Set.delete m members)
    | CyclicSCC ms <- sccs
    , let members = Set.fromList ms
    , m <- ms
    ]

{-------------------------------------------------------------------------------
  Step 5: Transitive reaches

  For each module, the full set of other library modules it transitively
  depends on. When module A's types module references a type from B, and
  B references a type from C, then A reaches both B and C. A's term modules
  (Safe, Unsafe, etc.) need instance-only imports for every module A reaches,
  because B's @HasFFIType@ instances may refer to C's types.

  Example: given direct deps  A -> {B},  B -> {C},  C -> {}
  the transitive reaches are  A -> {B, C},  B -> {C},  C -> {}
-------------------------------------------------------------------------------}

type TransitiveReaches = Map Hs.ModuleName (Set Hs.ModuleName)

computeReaches :: ModuleDeps -> TransitiveReaches
computeReaches deps = Map.mapWithKey (\m _ -> reachable m) deps
  where
    -- BFS from 'start', following the direct-dependency edges, collecting
    -- every module visited (excluding 'start' itself, since a module does
    -- not "reach" itself).
    reachable :: Hs.ModuleName -> Set Hs.ModuleName
    reachable start = Set.delete start $ go Set.empty (Set.singleton start)

    go :: Set Hs.ModuleName -> Set Hs.ModuleName -> Set Hs.ModuleName
    go visited queue
      | Set.null toVisit = visited
      | otherwise        = go (Set.union visited toVisit) toVisit
      where
        -- Expand each queued module to its direct dependencies, then
        -- drop anything already visited to guarantee termination.
        toVisit = Set.unions
                  [ Map.findWithDefault Set.empty m deps
                  | m <- Set.toList queue
                  ]
                  Set.\\ visited

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

-- | Reject headers whose module names collide case-insensitively.
--
-- GHC maps module names to file paths, and on case-insensitive file systems
-- (macOS HFS+, Windows NTFS) two modules differing only in case would
-- resolve to the same file. For example, @Point.h@ and @POINT.h@ under
-- the same directory would produce @Lib.Point@ and @Lib.POINT@, which
-- both map to @Lib/Point.hs@ on macOS.
--
-- This check is conservative: it rejects collisions that would only matter
-- on case-insensitive systems, even when running on Linux.
validateModuleNames :: HeaderMap -> Either PlanError ()
validateModuleNames headerMap = do
    -- Group all (moduleName, header) pairs by their lower-cased module name.
    -- A group with two entries means either two headers produce the exact
    -- same module name (e.g. Foo.h and foo.h both becoming Lib.Collide.Foo)
    -- or two headers produce names that differ only in case (e.g. Point.h
    -- and POINT.h becoming Lib.Point and Lib.POINT), which would collide on
    -- a case-insensitive file system.
    let caseInsensitiveMap = Map.fromListWith (++)
          [ (Text.toLower modName.text, [(modName, header)])
          | (header, (modName, _)) <- Map.toList headerMap
          ]
    case [ (m1, m2, h1, h2)
         | group <- Map.elems caseInsensitiveMap
         , (m1, h1):(m2, h2):_ <- [group]
         ] of
      []              -> Right ()
      (m1,m2,h1,h2):_ -> Left $ ModuleNameCollision m1 m2 h1 h2

{-------------------------------------------------------------------------------
  Module name derivation
-------------------------------------------------------------------------------}

-- | Derive a Haskell module name from a header's path.
--
-- Strips the library directory prefix and file extension, then converts
-- each path component into a valid Haskell module component: upper-case
-- the first letter, replace non-alphanumeric characters with @_@, and
-- prefix with @C@ if the component starts with a digit.
--
-- > moduleNameFromPath "Lib" "/inc" (RealPath "/inc/geo/point.h")
-- >   == ModuleName "Lib.Geo.Point"
moduleNameFromPath ::
     Hs.ModuleName    -- ^ Base module name prefix
  -> FilePath          -- ^ Library directory
  -> RealPath          -- ^ Header path (absolute, canonical)
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
sanitizeComponent t = case Text.uncons sanitized of
    Nothing       -> "C"
    Just (c, rest)
      | Char.isDigit c -> "C" <> sanitized
      | otherwise      -> Text.cons (Char.toUpper c) rest
  where
    sanitized = Text.map (\c -> if Char.isAlphaNum c then c else '_')
              $ Text.filter (/= '.') t
