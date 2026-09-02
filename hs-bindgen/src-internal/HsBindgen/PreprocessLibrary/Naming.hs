-- | Module naming and library-root filtering for @preprocess-library@.
--
-- Intended for qualified import.
--
-- > import HsBindgen.PreprocessLibrary.Naming qualified as Naming
module HsBindgen.PreprocessLibrary.Naming (
    -- * Module naming
    deriveModuleName
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
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath (dropExtension, isRelative, makeRelative,
                        splitDirectories)

import Clang.Paths

import HsBindgen.Config.Prelims (BaseModuleName (..))
import HsBindgen.Frontend.Predicate (Regex, matchTest)

-- | Derive a Haskell module name from a header's canonical path.
--
-- Finds the library root the header falls under, computes the relative path,
-- drops the file extension, capitalizes each path component, and joins them
-- with dots under the base module name.
--
-- @
-- deriveModuleName ["\/usr\/include"] (BaseModuleName "Widget") (RealPath "\/usr\/include\/widget\/core.h")
--   == BaseModuleName "Widget.Widget.Core"
-- @
deriveModuleName :: [FilePath] -> BaseModuleName -> RealPath -> BaseModuleName
deriveModuleName roots (BaseModuleName base) rp =
    BaseModuleName $ base <> "." <> Text.intercalate "." components
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

-- | Convert a dotted module name to a file path (e.g. @"A.B.C"@ to @"A\/B\/C"@).
moduleToPath :: BaseModuleName -> FilePath
moduleToPath (BaseModuleName m) =
    Text.unpack $ Text.replace "." "/" m

-- | Check whether a file path is contained under a directory.
isUnderDir :: FilePath -> FilePath -> Bool
isUnderDir path dir = isRelative (makeRelative dir path)

{-------------------------------------------------------------------------------
  Library-root filtering

  Module-generation scope: which headers in the include graph get their
  own Haskell module. This is determined by --library-root (directory)
  and --except-library-root (PCRE exclusion), and is independent of
  selection predicates, which control which declarations get bindings.
-------------------------------------------------------------------------------}

-- | Result of filtering the include graph by library roots and exclusions.
data LibraryHeaderResult = LibraryHeaderResult {
      included     :: [RealPath]
    , outsideRoots :: [RealPath]
    , excluded     :: [RealPath]
    }

-- | Filter headers to those under a library root and not excluded.
--
-- A header gets its own Haskell module when its canonical path falls under
-- at least one library root AND does not match any exclusion pattern.
-- This determines module-generation scope, independent of selection
-- predicates (which control declaration-level filtering).
filterByLibraryRoot ::
     [FilePath]
     -- ^ Canonicalized library roots (@--library-root@)
  -> [Regex]
     -- ^ Exclusion patterns (@--except-library-root@)
  -> [RealPath]
     -- ^ All headers from the include graph (topologically sorted)
  -> LibraryHeaderResult
filterByLibraryRoot roots excludePatterns sorted =
    LibraryHeaderResult {
        included     = includedHdrs
      , outsideRoots = outsideHdrs
      , excluded     = excludedHdrs
      }
  where
    isUnderRoot rp = any (getRealPath rp `isUnderDir`) roots
    isExcluded  rp = any (\re -> matchTest re (getRealPathText rp)) excludePatterns

    (underRoots, outsideHdrs) = List.partition isUnderRoot sorted
    (excludedHdrs, includedHdrs) = List.partition isExcluded underRoots

{-------------------------------------------------------------------------------
  Collision detection

  Three collision classes exist:

  1. First-character case folding: capitalize only uppercases the first
     character, so foo.h and Foo.h both produce Foo. Note that foo.h
     and FOO.h do NOT collide (Foo vs FOO).

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
  BaseModuleName from different headers). Class 3 requires expanding
  each base name with the category suffixes Safe, Unsafe, FunPtr, and
  Global (matching HsBindgen.Config.Prelims.fromBaseModuleName).
-------------------------------------------------------------------------------}

data Collision
  = DirectCollision Text [RealPath]
  | CategoryOverlap Text RealPath Text RealPath Text
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
  -> [(RealPath, BaseModuleName)]
  -> [Collision]
detectCollisions checkCategories modules =
    directCollisions ++ categoryOverlaps
  where
    byBase :: Map.Map Text [RealPath]
    byBase = Map.fromListWith (++)
      [ (base, [hdr])
      | (hdr, BaseModuleName base) <- modules
      ]

    directCollisions =
      [ DirectCollision m hdrs
      | (m, hdrs) <- Map.toList byBase
      , length hdrs > 1
      ]

    categorySuffixes :: [Text]
    categorySuffixes = ["Safe", "Unsafe", "FunPtr", "Global"]

    categoryOverlaps
      | not checkCategories = []
      | otherwise =
          [ CategoryOverlap catModule hdr1 suffix hdr2 "Types"
          | (hdr1, BaseModuleName base1) <- modules
          , suffix <- categorySuffixes
          , let catModule = base1 <> "." <> suffix
          , (hdr2, BaseModuleName base2) <- modules
          , base2 == catModule
          ]

formatCollision :: Collision -> String
formatCollision (DirectCollision m hdrs) = unlines $
    [ "  Module name collision: " ++ Text.unpack m
    , "  Headers mapping to the same module:"
    ] ++
    [ "    " ++ Text.unpack (getRealPathText h)
    | h <- hdrs
    ]
formatCollision (CategoryOverlap m h1 r1 h2 r2) = unlines
    [ "  Category overlap on: " ++ Text.unpack m
    , "    " ++ Text.unpack (getRealPathText h1)
         ++ " (" ++ Text.unpack r1 ++ ")"
    , "    " ++ Text.unpack (getRealPathText h2)
         ++ " (" ++ Text.unpack r2 ++ ")"
    ]
