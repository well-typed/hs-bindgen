-- | The plan for library mode: which headers get a Haskell module, in which
-- order the modules are generated, and what they are called.
module HsBindgen.LibraryMode.Plan (
    -- * Plan
    LibraryPlan(..)
  , LibraryUnit(..)
  , planLibrary
    -- * Module naming
  , deriveModuleName
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

import HsBindgen.Config.MangleCandidate (MangleCandidate (..), mangleCandidate,
                                         mangleCandidateDefault)
import HsBindgen.Config.Prelims (BaseModuleName (..))
import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.Pass.Final (Final)
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Translation (DeclIdPair (..))
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Plan
-------------------------------------------------------------------------------}

-- | What library mode generates
data LibraryPlan = LibraryPlan {
      -- | One per module, in processing order: each unit comes after the units
      -- whose declarations it uses
      units :: [LibraryUnit]
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
    }
  deriving stock (Show, Eq)

-- | Plan the modules of a library
--
-- A header gets a module when its normalised path is under a library
-- directory and something in it is generated. These headers are grouped and
-- ordered by how their declarations use each other (see
-- 'sortByDeclarationUse'), and each group is named (see 'mkLibraryUnit').
planLibrary ::
     [FilePath]
     -- ^ Normalised library directories (@--library@)
  -> BaseModuleName
  -> IncludeGraph
  -> UseDeclGraph
  -> [C.Decl l Final]
     -- ^ The declarations the frontend hands to the backend
  -> LibraryPlan
planLibrary roots base includeGraph useDeclGraph decls =
    LibraryPlan {
        units = map (mkLibraryUnit roots base includeGraph) components
      }
  where
    located :: Map C.DeclId RealPath
    located = declLocations decls

    components :: [NonEmpty RealPath]
    components = sortByDeclarationUse useDeclGraph located includedHeaders

    generating :: Set RealPath
    generating = Set.fromList (Map.elems located)

    isUnderRoot :: RealPath -> Bool
    isUnderRoot header = any (getRealPath header `isUnderDir`) roots

    -- The headers stay in include order, which breaks ties in
    -- 'sortByDeclarationUse'
    includedHeaders :: [RealPath]
    includedHeaders =
      filter (\header -> isUnderRoot header && header `Set.member` generating) $
        IncludeGraph.toSortedList includeGraph

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
    , Just header <- [C.declPathRealPath (singleLocPath decl.info.loc)]
    ]

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
  -> NonEmpty RealPath
     -- ^ Headers in one declaration loop
  -> LibraryUnit
mkLibraryUnit roots base includeGraph component =
    LibraryUnit {
        headers    = sorted
      , moduleName = deriveModuleName roots base outermost
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
