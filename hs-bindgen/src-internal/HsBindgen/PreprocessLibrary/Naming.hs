-- | Module naming and library filtering for library mode.
--
-- Intended for qualified import.
--
-- > import HsBindgen.PreprocessLibrary.Naming qualified as Naming
module HsBindgen.PreprocessLibrary.Naming (
    -- * Library units
    LibraryUnit(..)
  , mkLibraryUnit
    -- * Module naming
  , deriveModuleName
  , moduleToPath
  , isUnderDir
    -- * Library-root filtering
  , LibraryHeaderResult(..)
  , filterByLibraryRoot
    -- * Collision detection
  , Collision(..)
  , detectCollisions
  , formatCollision
  ) where

import Data.Char qualified as Char
import Data.Foldable (toList)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath (dropExtension, isRelative, joinPath, makeRelative,
                        splitDirectories)

import Clang.Paths

import HsBindgen.Config.Prelims (BaseModuleName (..), termCategorySuffix)
import HsBindgen.Frontend.Predicate (Regex, matchTest)

{-------------------------------------------------------------------------------
  Library units
-------------------------------------------------------------------------------}

-- | Headers that share one Haskell module
--
-- A unit is usually a single header. Headers that include each other,
-- directly or through other headers, form one unit: no processing order puts
-- each of them after every header it depends on, so they are processed in a
-- single step.
data LibraryUnit = LibraryUnit {
      -- | Sorted by path
      headers    :: NonEmpty RealPath
    , moduleName :: BaseModuleName
    }
  deriving stock (Show, Eq)

-- | Build the unit for a group of headers from the include graph.
--
-- A single header gets its 'deriveModuleName' name. A group is named after
-- all of its headers, sorted by path so the name does not depend on include
-- order: the directories they share appear once, then each header's
-- remaining path components, concatenated, joined with @_@.
--
-- @
-- [a.h, b.h]                   -> Base.A_B
-- [widget\/core.h, widget\/util.h] -> Base.Widget.Core_Util
-- [widget\/core.h, util\/log.h]    -> Base.UtilLog_WidgetCore
-- @
mkLibraryUnit :: [FilePath]        -- ^ Library directories (@--library@)
              -> BaseModuleName
              -> NonEmpty RealPath -- ^ Headers in one include cycle
              -> LibraryUnit
mkLibraryUnit roots base component = LibraryUnit {
      headers    = sorted
    , moduleName = case sorted of
        hdr :| [] -> deriveModuleName roots base hdr
        _         -> joinModuleName base (dirs ++ [cycleName])
    }
  where
    sorted = NonEmpty.sort component

    perHeader :: NonEmpty [Text]
    perHeader = fmap (moduleComponents roots) sorted

    -- Directories shared by every header; never includes a file name
    dirs :: [Text]
    dirs = case fmap dropLast perHeader of
      c :| cs -> List.foldl' commonPrefix c cs

    cycleName :: Text
    cycleName = Text.intercalate "_"
      [ Text.concat (drop (length dirs) c)
      | c <- toList perHeader
      ]

    dropLast :: [a] -> [a]
    dropLast xs = take (length xs - 1) xs

    commonPrefix :: Eq a => [a] -> [a] -> [a]
    commonPrefix xs ys = map fst . takeWhile (uncurry (==)) $ zip xs ys

-- | Derive a Haskell module name from a header's normalised path.
--
-- Finds the library directory the header falls under, computes the relative path,
-- drops the file extension, capitalizes each path component, and joins them
-- with dots under the base module name.
--
-- @
-- deriveModuleName ["\/usr\/include"] (BaseModuleName "Widget") (RealPath "\/usr\/include\/widget\/core.h")
--   == BaseModuleName "Widget.Widget.Core"
-- @
deriveModuleName :: [FilePath]      -- ^ Library directories (@--library@)
                 -> BaseModuleName
                 -> RealPath
                 -> BaseModuleName
deriveModuleName roots base rp =
    joinModuleName base (moduleComponents roots rp)

-- | Module name components of a header, relative to the library directories
moduleComponents :: [FilePath] -> RealPath -> [Text]
moduleComponents roots rp = components
  where
    path = getRealPath rp

    rel = case [ makeRelative r path
               | r <- roots
               , path `isUnderDir` r
               ] of
            (r : _) -> r
            []      -> path

    components =
        map (Text.pack . capitalize)
      . splitDirectories
      $ dropExtension rel

    capitalize [] = []
    capitalize (c : cs) = Char.toUpper c : cs

joinModuleName :: BaseModuleName -> [Text] -> BaseModuleName
joinModuleName (BaseModuleName base) components =
    BaseModuleName $ base <> "." <> Text.intercalate "." components

-- | Convert a dotted module name to a file path (e.g. @"A.B.C"@ to @"A\/B\/C"@).
moduleToPath :: BaseModuleName -> FilePath
moduleToPath (BaseModuleName m) =
    joinPath . map Text.unpack $ Text.splitOn "." m

-- | Check whether a file path is contained under a directory.
isUnderDir :: FilePath -> FilePath -> Bool
isUnderDir path dir = isRelative (makeRelative dir path)

{-------------------------------------------------------------------------------
  Library-root filtering

  Module-generation scope: which headers in the include graph get their
  own Haskell module. This is determined by --library (directory)
  and --except-library (PCRE exclusion), and is independent of
  selection predicates, which control which declarations get bindings.
-------------------------------------------------------------------------------}

-- | Result of filtering the include graph by library directories and exclusions.
data LibraryHeaderResult = LibraryHeaderResult {
      -- | Headers that get a module, grouped by include cycle
      included     :: [NonEmpty RealPath]
    , outsideRoots :: [RealPath]
    , excluded     :: [RealPath]
    }

-- | Filter headers to those under a library directory and not excluded.
--
-- A header gets a Haskell module when its normalised path falls under at least
-- one library directory AND does not match any exclusion pattern. Filtering
-- keeps the include-cycle groups; a group left with no headers is dropped.
--
filterByLibraryRoot ::
     [FilePath]
     -- ^ Normalised library directories (@--library@)
  -> [Regex]
     -- ^ Exclusion patterns (@--except-library@)
  -> [NonEmpty RealPath]
     -- ^ All headers from the include graph, grouped by include cycle
     -- (topologically sorted)
  -> LibraryHeaderResult
filterByLibraryRoot roots excludePatterns components =
    LibraryHeaderResult {
        included     = mapMaybe (NonEmpty.nonEmpty . filter isIncluded . toList)
                         components
      , outsideRoots = outsideHdrs
      , excluded     = excludedHdrs
      }
  where
    isUnderRoot rp = any (getRealPath rp `isUnderDir`) roots
    isExcluded  rp = any (\re -> matchTest re (getRealPathText rp)) excludePatterns
    isIncluded  rp = isUnderRoot rp && not (isExcluded rp)

    (underRoots, outsideHdrs) =
      List.partition isUnderRoot (concatMap toList components)
    excludedHdrs = filter isExcluded underRoots

{-------------------------------------------------------------------------------
  Collision detection

  Three collision classes exist:

  1. First-character case folding: capitalize only uppercases the first
     character, so foo.h and Foo.h both produce Foo.

  2. Dot-slash equivalence: dropExtension strips only the last
     extension, so Widget.Core.h retains a dot in the stem
     (Widget.Core). That dot becomes a module separator in the derived
     name, producing the same module as widget/core.h from two
     separate path components.

  3. Category overlap (FilePerModule only): a header's base module name
     coincides with another header's category submodule. For example,
     headers foo.h (module M.Foo) and foo/safe.h (module M.Foo.Safe)
     collide because M.Foo's Safe category file and M.Foo.Safe's Types
     file both resolve to M/Foo/Safe.hs.

  Classes 1 and 2 are caught by the direct collision check (same
  BaseModuleName from different units; the headers of one include cycle
  share a unit, so they never collide with each other). Class 3 requires
  expanding each base name with the category suffixes Safe, Unsafe,
  FunPtr, and Global (matching HsBindgen.Config.Prelims.fromBaseModuleName).
-------------------------------------------------------------------------------}

-- | Colliding units are given by their headers
data Collision
  = DirectCollision Text [NonEmpty RealPath]
  | CategoryOverlap Text (NonEmpty RealPath) Text (NonEmpty RealPath) Text
  deriving stock (Show, Eq)

-- | Detect module name collisions that would cause output files to
-- overwrite each other.
--
-- When @checkCategories@ is @True@ (FilePerModule mode), each base module
-- name is checked against other base module names with a category suffix
-- appended. For example, base module @M.Foo@ and base module @M.Foo.Safe@
-- collide because @M.Foo@'s Safe category file overwrites @M.Foo.Safe@'s
-- Types file.
detectCollisions ::
     Bool
     -- ^ Check category overlaps (True for FilePerModule)
  -> [LibraryUnit]
  -> [Collision]
detectCollisions checkCategories units =
    directCollisions ++ categoryOverlaps
  where
    modules :: [(NonEmpty RealPath, BaseModuleName)]
    modules = [ (unit.headers, unit.moduleName) | unit <- units ]

    byBase :: Map.Map Text [NonEmpty RealPath]
    byBase = Map.fromListWith (++)
      [ (base, [hdrs])
      | (hdrs, BaseModuleName base) <- modules
      ]

    directCollisions =
      [ DirectCollision m hdrss
      | (m, hdrss) <- Map.toList byBase
      , length hdrss > 1
      ]

    categorySuffixes :: [Text]
    categorySuffixes = map termCategorySuffix [minBound .. maxBound]

    categoryOverlaps
      | not checkCategories = []
      | otherwise =
          [ CategoryOverlap catModule hdr1 suffix hdr2 "Types"
          | suffix <- categorySuffixes
          , (hdr1, BaseModuleName base1) <- modules
          , let catModule = base1 <> "." <> suffix
          , (hdr2, BaseModuleName base2) <- modules
          , base2 == catModule
          ]

formatCollision :: Collision -> String
formatCollision (DirectCollision m hdrss) = unlines $
    [ "  Module name collision: " ++ Text.unpack m
    , "  Headers mapping to the same module:"
    ] ++
    [ "    " ++ formatUnitHeaders hdrs
    | hdrs <- hdrss
    ]
formatCollision (CategoryOverlap m h1 r1 h2 r2) = unlines
    [ "  Category overlap on: " ++ Text.unpack m
    , "    " ++ formatUnitHeaders h1 ++ " (" ++ Text.unpack r1 ++ ")"
    , "    " ++ formatUnitHeaders h2 ++ " (" ++ Text.unpack r2 ++ ")"
    ]

-- | The headers of one unit, comma-separated
formatUnitHeaders :: NonEmpty RealPath -> String
formatUnitHeaders =
    List.intercalate ", " . map (Text.unpack . getRealPathText) . toList
