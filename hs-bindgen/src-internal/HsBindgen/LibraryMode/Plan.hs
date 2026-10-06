-- | The plan for library mode: which headers get a Haskell module, in which
-- order the modules are generated, and what they are called.
module HsBindgen.LibraryMode.Plan (
    -- * Plan
    LibraryPlan(..)
  , LibraryUnit(..)
  , planLibrary
    -- * Module naming
  , deriveModuleName
    -- * Collision detection
  , Collision(..)
  , directCollisions
  , categoryOverlaps
  , formatCollision
  ) where

import Data.Digraph (Digraph)
import Data.Digraph qualified as Digraph
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Data.Text qualified as Text
import System.FilePath (dropExtension, isRelative, makeRelative,
                        splitDirectories)

import Clang.HighLevel.Types (singleLocPath)
import Clang.Paths

import HsBindgen.Backend.Category (Category (..), TermCategory)
import HsBindgen.Backend.Hs.Translation (declCategories)
import HsBindgen.Config.MangleCandidate (MangleCandidate (..), mangleCandidate,
                                         mangleCandidateDefault)
import HsBindgen.Config.Prelims (BaseModuleName (..), baseModuleNameToString,
                                 fromBaseModuleName, termCategorySuffix)
import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.Pass.Final (Final)
import HsBindgen.Frontend.Predicate (Regex, matchTest)
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Translation (DeclIdPair (..))
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Macro.Type qualified as Macro

{-------------------------------------------------------------------------------
  Plan
-------------------------------------------------------------------------------}

-- | What library mode generates
data LibraryPlan = LibraryPlan {
      -- | One per module, in processing order: each unit comes after the units
      -- whose declarations it uses
      units           :: [LibraryUnit]
      -- | Headers under a library directory that @--except-library@ leaves out
    , excluded        :: [RealPath]
      -- | Headers under a library directory, not excluded, in which nothing
      -- is generated
    , withoutBindings :: [RealPath]
    }
  deriving stock (Show, Eq)

-- | Headers that share one Haskell module
--
-- A unit is usually a single header. Headers whose declarations use each
-- other, directly or through other headers, form one unit: no processing order
-- puts each of them after every header it depends on, so they are processed in
-- a single step.
data LibraryUnit = LibraryUnit {
      -- | Sorted by path
      headers    :: NonEmpty RealPath
    , moduleName :: BaseModuleName
      -- | The modules the unit writes: the base module for its types, and
      -- one submodule per term category (see 'unitCategories')
      --
      -- Strict, so that the plan does not hold on to the declarations it was
      -- computed from.
    , categories :: !(Set Category)
    }
  deriving stock (Show, Eq)

-- | Plan the modules of a library
--
-- A header gets a module when its normalised path is under a library
-- directory, it matches no exclusion pattern, and something in it is
-- generated. These headers are grouped and ordered by how their declarations
-- use each other (see 'sortByDeclarationUse'), and each group is named (see
-- 'mkLibraryUnit').
planLibrary ::
     Macro.HasTypes l
  => [FilePath]
     -- ^ Normalised library directories (@--library@)
  -> [Regex]
     -- ^ Exclusion patterns (@--except-library@)
  -> BaseModuleName
  -> IncludeGraph
  -> UseDeclGraph
  -> [C.Decl l Final]
     -- ^ The declarations the frontend hands to the backend
  -> LibraryPlan
planLibrary roots exceptPatterns base includeGraph useDeclGraph decls =
    LibraryPlan {
        units           = map mkUnit components
      , excluded        = excludedHeaders
      , withoutBindings = emptyHeaders
      }
  where
    located :: Map C.DeclId RealPath
    located = declLocations decls

    components :: [NonEmpty RealPath]
    components = sortByDeclarationUse useDeclGraph located includedHeaders

    mkUnit :: NonEmpty RealPath -> LibraryUnit
    mkUnit component =
      mkLibraryUnit roots base includeGraph
        (unitCategories useDeclGraph located categoryOf withModule component)
        component

    generating, withModule :: Set RealPath
    generating = Set.fromList (Map.elems located)
    withModule = Set.fromList includedHeaders

    categoryOf :: Map C.DeclId (Set Category)
    categoryOf = Map.fromList [
          (decl.info.id.cName, declCategories decl.kind)
        | decl <- decls
        ]

    isUnderRoot, isExcluded :: RealPath -> Bool
    isUnderRoot header = any (getRealPath header `isUnderDir`) roots
    isExcluded  header =
      any (`matchTest` getRealPathText header) exceptPatterns

    -- The headers stay in include order, which breaks ties in
    -- 'sortByDeclarationUse'
    excludedHeaders, keptHeaders, includedHeaders, emptyHeaders :: [RealPath]
    (excludedHeaders, keptHeaders)  =
      List.partition isExcluded $
        filter isUnderRoot (IncludeGraph.toSortedList includeGraph)
    (includedHeaders, emptyHeaders) =
      List.partition (`Set.member` generating) keptHeaders

{-------------------------------------------------------------------------------
  Declaration order
-------------------------------------------------------------------------------}

-- | The header each generated declaration is located in
--
-- Takes the declarations the frontend hands to the backend. Declarations that
-- fail to parse, are omitted, are bound by an external binding specification
-- or are not selected are missing, and so are include guards: an umbrella
-- header that only includes other headers generates nothing. Macros defined on
-- the command line or in the root header are in no header, and are left out.
declLocations :: [C.Decl l Final] -> Map C.DeclId RealPath
declLocations decls = Map.fromList [
      (decl.info.id.cName, header)
    | decl <- decls
    , Just header <- [declHeader decl]
    ]

declHeader :: C.Decl l Final -> Maybe RealPath
declHeader decl = C.declPathRealPath (singleLocPath decl.info.loc)

-- | The modules a unit writes
--
-- A module keeps its types in the base module and its terms in one submodule
-- per category, and only writes the ones it has something for.
--
-- What a unit has is the declarations in its headers. It can also end up with
-- a declaration from a header that gets no module, since the first unit that
-- needs such a declaration generates it. Which unit comes first is not decided
-- here, so every unit that reaches the declaration counts as writing it. That
-- errs towards reporting a collision.
unitCategories ::
     UseDeclGraph
  -> Map C.DeclId RealPath
     -- ^ Generated declarations and their headers
  -> Map C.DeclId (Set Category)
     -- ^ The categories of each generated declaration
  -> Set RealPath
     -- ^ The headers that get a module
  -> NonEmpty RealPath
     -- ^ The headers of one unit
  -> Set Category
unitCategories useDeclGraph located categoryOf moduleHeaders component =
    foldMap categoriesOf (Set.union own hosted)
  where
    headers :: Set RealPath
    headers = Set.fromList (toList component)

    own, hosted :: Set C.DeclId
    own    = Map.keysSet $ Map.filter (`Set.member` headers) located
    hosted = Set.filter inNoModule $
               UseDeclGraph.getStrictTransitiveDeps useDeclGraph own

    inNoModule :: C.DeclId -> Bool
    inNoModule declId = case Map.lookup declId located of
        Just header -> header `Set.notMember` moduleHeaders
        Nothing     -> False

    categoriesOf :: C.DeclId -> Set Category
    categoriesOf declId = Map.findWithDefault Set.empty declId categoryOf

-- | Group headers whose declarations use each other, dependencies first
--
-- Header @x@ depends on header @y@ when a declaration in @x@ uses one in @y@,
-- directly or through declarations in other headers. A struct counts where it
-- is defined, so a header that only declares it forward and points to it
-- depends on the header with the definition. Headers that depend on each other
-- form one group, since no order puts each of them after the others. Each
-- group comes after the groups it depends on; the order of the given headers
-- breaks ties.
sortByDeclarationUse ::
     UseDeclGraph
  -> Map C.DeclId RealPath
     -- ^ Generated declarations and their headers (see 'declLocations')
  -> [RealPath]
     -- ^ Headers to group, in include order
  -> [NonEmpty RealPath]
sortByDeclarationUse useDeclGraph located headers =
    Digraph.sortComponents $ List.foldl' addUse vertices uses
  where
    -- Inserting the vertices first makes their indices follow the given order
    vertices :: Digraph () RealPath
    vertices = List.foldl' (flip Digraph.insertVertex) Digraph.empty headers

    declsIn :: Map RealPath (Set C.DeclId)
    declsIn = Map.fromListWith (<>) [
          (header, Set.singleton declId)
        | (declId, header) <- Map.toList located
        ]

    -- Each given header with every other given header its declarations reach.
    -- Headers outside the given ones get no vertex; their declarations still
    -- link the others.
    uses :: [(RealPath, RealPath)]
    uses = [
          (dep, user)
        | user     <- headers
        , declId   <- Set.toList $
                        UseDeclGraph.getStrictTransitiveDeps useDeclGraph $
                          Map.findWithDefault Set.empty user declsIn
        , Just dep <- [Map.lookup declId located]
        , dep /= user
        , Digraph.hasVertex dep vertices
        ]

    -- Edges run from the used header to the user, as in the include graph, so
    -- that 'Digraph.sortComponents' puts dependencies first
    addUse :: Digraph () RealPath -> (RealPath, RealPath) -> Digraph () RealPath
    addUse graph (dep, user) = Digraph.insertEdge dep () user graph

{-------------------------------------------------------------------------------
  Module naming
-------------------------------------------------------------------------------}

-- | Build the unit for a group of headers from 'sortByDeclarationUse'.
--
-- The module is named like the group's outermost header: the one that
-- includes, directly or through other headers, the most of the group's other
-- headers. A C program usually includes that header to get at the rest, and
-- the name stays as short as a single header's however large the group gets.
-- Among equally outermost headers, the first by path wins.
--
-- @
-- [argv.h, rpmtag.h, rpmtd.h, rpmtypes.h] -> RPM.Rpmtd
-- @
--
-- since @rpmtd.h@ includes the other three.
mkLibraryUnit ::
     [FilePath]
     -- ^ Library directories (@--library@)
  -> BaseModuleName
  -> IncludeGraph
  -> Set Category
     -- ^ See 'unitCategories'
  -> NonEmpty RealPath
     -- ^ Headers in one declaration loop
  -> LibraryUnit
mkLibraryUnit roots base includeGraph categories component =
    LibraryUnit {
        headers    = sorted
      , moduleName = deriveModuleName roots base outermost
      , categories = categories
      }
  where
    sorted :: NonEmpty RealPath
    sorted = NonEmpty.sort component

    -- The header that includes the most of the others ranks highest; on a
    -- tie, the first by path
    outermost :: RealPath
    outermost = getDown . snd . maximum $ fmap rank sorted

    rank :: RealPath -> (Int, Down RealPath)
    rank header = (length (filter (header `Set.member`) includers), Down header)

    -- For each header, the headers that include it, directly or through other
    -- headers (itself among them)
    includers :: [Set RealPath]
    includers = map (IncludeGraph.reaches includeGraph) (toList sorted)

-- | The module name of a header
--
-- The name follows the path of the header below its library directory:
--
-- 1. Take the path relative to the library directory, without the file
--    extension: @rpm\/rpmio.h@ gives @rpm\/rpmio@.
-- 2. Split it at the directories: @rpm@ and @rpmio@.
-- 3. Turn each part into a module name component (see
--    'moduleNameComponent'): @Rpm@ and @Rpmio@.
-- 4. Join the components with dots, after the base module name:
--    @RPM.Rpm.Rpmio@.
--
-- A header under several library directories, one inside the other, is named
-- relative to the shortest. The name then does not depend on the order of the
-- flags, and a header in a subdirectory keeps the subdirectory in its name,
-- which keeps it apart from a header of the same name further up.
deriveModuleName ::
     [FilePath]
     -- ^ Library directories (@--library@), one of which contains the header
  -> BaseModuleName
  -> RealPath
  -> BaseModuleName
deriveModuleName roots (BaseModuleName base) header =
    BaseModuleName $ Text.intercalate "." (base : components)
  where
    path :: FilePath
    path = getRealPath header

    relative :: FilePath
    relative = case List.sortOn (length . splitDirectories) containing of
        root : _ -> makeRelative root path
        []       -> panicPure $ "header under no library directory: " ++ path

    containing :: [FilePath]
    containing = [ root | root <- roots, path `isUnderDir` root ]

    components :: [Text]
    components =
        mapMaybe (moduleNameComponent . Text.pack)
      . splitDirectories
      $ dropExtension relative

-- | Turn a directory or file name into one component of a module name
--
-- This is the name mangler that turns C names into Haskell type names: a
-- module name component follows the same rules as a type name. A name that
-- those rules allow only gets its first letter uppercased. For the others:
--
-- * a character that is not a letter, a digit, an underscore or a single
--   quote becomes an underscore;
-- * a name that does not start with a letter gets a @C@ in front, and its
--   first letter is uppercased.
--
-- > rpmio        becomes  Rpmio
-- > glib-object  becomes  Glib_object
-- > foo.bar      becomes  Foo_bar   (a dot left after dropping the extension)
-- > 3d           becomes  C3D
--
-- Only the empty name has no component.
moduleNameComponent :: Text -> Maybe Text
moduleNameComponent =
    fmap (.text) . mangleCandidate @Hs.NsTypeConstr rules
  where
    rules :: MangleCandidate Maybe
    rules = mangleCandidateDefault {
          onInvalidChar = const (Just "_")
          -- Names that are reserved for types are fine for modules
        , reservedNames = mempty
        }

-- | Check whether a file path is contained under a directory.
isUnderDir :: FilePath -> FilePath -> Bool
isUnderDir path dir = isRelative (makeRelative dir path)

{-------------------------------------------------------------------------------
  Collision detection
-------------------------------------------------------------------------------}

-- | Units whose output files would overwrite each other
--
-- A unit is given by its headers.
data Collision =
    -- | Several units get the same module name
    DirectCollision
      BaseModuleName
      [NonEmpty RealPath]  -- ^ Two or more units
    -- | A category submodule of one unit is the base module of another
  | CategoryOverlap
      Hs.ModuleName        -- ^ The module both write
      (NonEmpty RealPath)  -- ^ The unit that keeps terms in it
      TermCategory         -- ^ The category of those terms
      (NonEmpty RealPath)  -- ^ The unit that keeps its types in it
  deriving stock (Show, Eq)

-- | Units that get the same module name
--
-- Different header names can give the same module name:
--
-- * the first character is uppercased, so @foo.h@ and @Foo.h@ both give
--   @Foo@;
-- * a character that a module name cannot contain becomes an underscore, so
--   @foo-bar.h@, @foo.bar.h@ and @foo_bar.h@ all give @Foo_bar@;
-- * a name that does not start with a letter gets a @C@ in front, so @3d.h@
--   and @C3D.h@ both give @C3D@.
--
-- The headers of one declaration loop share a unit, so they do not collide
-- with each other.
directCollisions :: [LibraryUnit] -> [Collision]
directCollisions units = [
      DirectCollision (BaseModuleName name) colliding
    | (name, colliding@(_ : _ : _)) <- Map.toList byName
    ]
  where
    byName :: Map Text [NonEmpty RealPath]
    byName = Map.fromListWith (flip (++)) [
          (unit.moduleName.text, [unit.headers])
        | unit <- units
        ]

-- | Category submodules of one unit that are the base module of another
--
-- A module keeps its terms in category submodules, so @foo.h@ (module
-- @M.Foo@) and @foo\/safe.h@ (module @M.Foo.Safe@) can both write
-- @M\/Foo\/Safe.hs@: the first its safe foreign imports, the second its types.
-- A unit only writes the modules it has something for (see 'unitCategories'),
-- so this needs @foo.h@ to have a function and @foo\/safe.h@ to have a type.
--
-- This only applies when every category goes to a file of its own.
categoryOverlaps :: [LibraryUnit] -> [Collision]
categoryOverlaps units = [
      CategoryOverlap submodule unit.headers category other.headers
    | unit           <- units
    , CTerm category <- Set.toList unit.categories
    , let submodule =
            fromBaseModuleName unit.moduleName (Just (CTerm category))
    , other          <- Map.findWithDefault [] submodule.text withTypes
    ]
  where
    -- The units that write their base module, by its name
    withTypes :: Map Text [LibraryUnit]
    withTypes = Map.fromListWith (flip (++)) [
          (unit.moduleName.text, [unit])
        | unit <- units
        , CType `Set.member` unit.categories
        ]

formatCollision :: Collision -> String
formatCollision = \case
    DirectCollision name units -> unlines $ [
        "  Module name collision: " ++ baseModuleNameToString name
      , "  Headers mapping to the same module:"
      ] ++ [
        "    " ++ formatUnitHeaders headers
      | headers <- units
      ]
    CategoryOverlap submodule termHeaders category typeHeaders -> unlines [
        "  Category overlap on: " ++ Hs.moduleNameToString submodule
      , concat [
            "    ", formatUnitHeaders termHeaders
          , " (", Text.unpack (termCategorySuffix category), ")"
          ]
      , "    " ++ formatUnitHeaders typeHeaders ++ " (Types)"
      ]

-- | The headers of one unit, comma-separated
formatUnitHeaders :: NonEmpty RealPath -> String
formatUnitHeaders =
    List.intercalate ", " . map (Text.unpack . getRealPathText) . toList
