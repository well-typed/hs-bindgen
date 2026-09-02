-- | Plan computation for the @preprocess-library@ subcommand.
--
-- Determines which headers get their own Haskell module, derives module
-- names from header paths, and assembles the processing steps in
-- topological order.
--
-- Intended for qualified import.
--
-- > import HsBindgen.PreprocessLibrary.Plan qualified as Plan
module HsBindgen.PreprocessLibrary.Plan (
    -- * Plan types
    PlanStep(..)
  , Plan(..)
    -- * Plan computation
  , computePlan
    -- * Helpers
  , deriveModuleName
  , moduleToPath
  , isUnderDir
  ) where

import Data.Char qualified as Char
import Data.List qualified as List
import Data.Text qualified as Text
import System.FilePath (dropExtension, isRelative, makeRelative,
                        splitDirectories)

import Clang.Paths

import HsBindgen.Config.Prelims (BaseModuleName (..))
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Predicate (Regex, matchTest)

{-------------------------------------------------------------------------------
  Plan types
-------------------------------------------------------------------------------}

-- | One step in the processing plan, corresponding to one @hsBindgen@ run
-- that produces one Haskell module.
data PlanStep = PlanStep {
      -- | The header whose declarations this step generates bindings for.
      stepHeader :: SourcePath

      -- | The Haskell module name derived from the header's relative path
      -- under the library root, prefixed by the user's @--module@ value.
    , stepModule :: BaseModuleName
    }

-- | A complete processing plan produced by 'computePlan'.
data Plan = Plan {
      -- | Steps to execute, in topological (dependency-first) order.
      steps          :: [PlanStep]

      -- | Headers from the include graph that fell outside every
      -- @--library-root@ and were therefore skipped.
    , excludedByRoot :: [SourcePath]

      -- | Headers excluded by @--exclude-header@.
    , excludedByUser :: [SourcePath]
    }

{-------------------------------------------------------------------------------
  Plan computation

  1. Topologically sort the include graph (leaves first).
  2. Partition into headers under a library root and headers outside.
  3. Partition the under-root headers into excluded (--exclude-header)
     and included.
  4. Derive a Haskell module name from each included header's path
     relative to its library root.
  5. Assemble PlanSteps in topological order.

  Excluded headers are simply skipped. Types they define are still
  available to other modules through program slicing (which pulls in
  transitive type dependencies) and shared via binding spec chaining.
-------------------------------------------------------------------------------}

computePlan ::
     IncludeGraph.IncludeGraph
  -> [FilePath]
  -> [Regex]
  -> BaseModuleName
  -> Plan
computePlan includeGraph roots excludeRegexes baseModule =
    Plan {
        steps          = planSteps
      , excludedByRoot = outsideRoots
      , excludedByUser = excludedHdrs
      }
  where
    sorted = IncludeGraph.toSortedList includeGraph

    isUnderRoot sp =
        any (getSourcePath sp `isUnderDir`) roots

    isExcluded sp =
        any (\re -> matchTest re (Text.pack $ getSourcePath sp)) excludeRegexes

    (underRoots, outsideRoots) = List.partition isUnderRoot sorted
    (excludedHdrs, includedHdrs) = List.partition isExcluded underRoots

    planSteps :: [PlanStep]
    planSteps =
        [ PlanStep {
              stepHeader = h
            , stepModule = deriveModuleName roots baseModule h
            }
        | h <- includedHdrs
        ]

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Derive a Haskell module name from a header's path.
--
-- Finds the library root the header falls under, computes the relative path,
-- drops the file extension, capitalizes each path component, and joins them
-- with dots under the base module name.
--
-- @
-- deriveModuleName ["\/usr\/include"] (BaseModuleName "Widget") "\/usr\/include\/widget\/core.h"
--   == BaseModuleName "Widget.Widget.Core"
-- @
deriveModuleName :: [FilePath] -> BaseModuleName -> SourcePath -> BaseModuleName
deriveModuleName roots (BaseModuleName base) sp =
    BaseModuleName $ base <> "." <> Text.intercalate "." components
  where
    path = getSourcePath sp

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
--
-- Uses 'makeRelative': when the path can be expressed relative to the
-- directory, the result is a relative path ('isRelative' returns @True@).
-- When it cannot, 'makeRelative' returns the original absolute path unchanged.
isUnderDir :: FilePath -> FilePath -> Bool
isUnderDir path dir = isRelative (makeRelative dir path)
