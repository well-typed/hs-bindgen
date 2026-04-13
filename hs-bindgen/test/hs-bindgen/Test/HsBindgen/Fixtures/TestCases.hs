{-# LANGUAGE OverloadedRecordDot #-}

-- | Shared fixture compilation status and skip logic
--
-- This module provides the common 'FixtureStatus' type and shared skip
-- reasons used by both 'Test.HsBindgen.THFixtures.TestCases' and
-- 'Test.HsBindgen.PPFixtures.TestCases'.
--
-- To add a new skip reason that applies to /both/ TH and PP fixture
-- compilation, add it to 'commonFixtureStatus'. For mode-specific skips,
-- add them in the respective @determineTHStatus@ or @determinePPStatus@.
--
module Test.HsBindgen.Fixtures.TestCases (
    FixtureStatus(..)
  , isCompile
  , commonFixtureStatus
    -- * Shared skip lists
  , commonKnownEmpty
    -- * Tasty options
  , HaddockMode(..)
  , haddockOutputDir
    -- * Tasty test helpers
  , SimpleTest(..)
  , mkFixtureTest
  , passedResult
  , skippedResult
    -- * Helpers
  , isSuccess
  , isVariantTest
  ) where

import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.Options (IsOption (..), OptionDescription (Option),
                           flagCLParser, safeReadBool)
import Test.Tasty.Providers (IsTest (..), singleTest, testFailed)
import Test.Tasty.Providers.ConsoleFormat (noResultDetails)
import Test.Tasty.Runners (Result (..))
import Test.Tasty.Runners qualified as Tasty

import Test.HsBindgen.Fixtures.Utils (sanitizeLibName)
import Test.HsBindgen.Golden.Infra.TestCase

{-------------------------------------------------------------------------------
  Fixture status
-------------------------------------------------------------------------------}

-- | Compilation status for a fixture
data FixtureStatus
    = FixtureCompile
      -- ^ Compile this fixture (with @-optc-Werror@ where applicable)
    | FixtureCompileNoOptcWerror
      -- ^ Compile without @-optc-Werror@ (PP only; TH treats as 'FixtureCompile')
    | FixtureSkip String
      -- ^ Skip with reason
  deriving stock (Eq, Show)

-- | Does this status indicate compilation?
isCompile :: FixtureStatus -> Bool
isCompile FixtureCompile             = True
isCompile FixtureCompileNoOptcWerror = True
isCompile FixtureSkip{}              = False

{-------------------------------------------------------------------------------
  Common skip logic
-------------------------------------------------------------------------------}

-- | Determine fixture status based on skip reasons common to both TH and PP
--
-- Returns @Just status@ if a common rule applies, @Nothing@ if mode-specific
-- logic should decide.
--
commonFixtureStatus :: TestCase -> Maybe FixtureStatus
commonFixtureStatus tc
  -- Expected failure tests produce no valid output
  | not (isSuccess tc.outcome)
      = Just $ FixtureSkip "Expected failure test"
  -- Tests with external binding specs not yet supported
  | "binding-specs/fun_arg/" `isPrefixOf` tc.name
      = Just $ FixtureSkip "External binding specs not yet supported (issue #1495)"
  -- Apple block extension requires clang
  | tc.name == "edge-cases/iterator"
      = Just $ FixtureSkip "Apple block extension requires clang (issue #913)"
  -- Unusable struct
  | tc.name == "functions/decls_in_signature"
      = Just $ FixtureSkip "Unusable struct (issue #1128)"
  -- Issue #1490
  | tc.name `elem` issue1490
      = Just $ FixtureSkip "Issue #1490"
  -- Issue #1679
  | tc.name `elem` issue1679
      = Just $ FixtureSkip "Issue #1679"
  -- @typedef int bool@ / @#define bool int@ is invalid in C23
  | tc.name == "types/primitives/bool_typedef_override"
 || tc.name == "types/primitives/bool_macro_override"
      = Just $ FixtureSkip "bool override is invalid in C23"
  -- Empty output fixtures (shared between TH and PP)
  | tc.name `elem` commonKnownEmpty
      = Just $ FixtureSkip "Empty fixture (no bindings generated)"
  | otherwise
      = Nothing
  where
    issue1490 :: [String]
    issue1490 = [
        "functions/heap_types/struct_const_member"
      , "functions/heap_types/struct_const_typedef"
      , "functions/heap_types/struct_const"
      , "functions/heap_types/union_const_member"
      , "functions/heap_types/union_const_typedef"
      , "functions/heap_types/union_const"
      ]

    issue1679 :: [String]
    issue1679 = [
        "program-analysis/program-slicing/macro_selected"
      , "program-analysis/program-slicing/macro_unselected"
      ]

{-------------------------------------------------------------------------------
  Shared skip lists
-------------------------------------------------------------------------------}

-- | Empty output fixtures shared between TH and PP
--
commonKnownEmpty :: [String]
commonKnownEmpty = [
    "binding-specs/macro_trans_dep_missing"
  , "declarations/declaration_unselected_b"
  , "declarations/name_collision"
  , "declarations/redeclaration_different"
  , "edge-cases/clang_generated_collision"
  , "edge-cases/duplicate"
  , "edge-cases/headers"
  , "edge-cases/select_no_match"
  , "edge-cases/thread_local"
  , "edge-cases/unsupported_builtin"
  , "functions/not_visible_decl"
  , "macros/macro_type_void"
  , "program-analysis/delay_traces"
  , "program-analysis/selection_foo"
  , "program-analysis/selection_merge_traces"
  , "program-analysis/selection_omit_prescriptive"
  , "program-analysis/selection_squash_typedef"
  , "types/anonymous/edge-cases/empty_anon"
  , "types/anonymous/edge-cases/unnamed_bitfield"
  , "types/special/long_double"
  , "types/structs/unnamed_struct"
  , "types/typedefs/typenames"
  ]

{-------------------------------------------------------------------------------
  Tasty options
-------------------------------------------------------------------------------}

-- | Whether to generate Haddock documentation for fixtures
data HaddockMode = NoHaddock | WithHaddock
  deriving stock (Eq, Ord)

instance IsOption HaddockMode where
    defaultValue = NoHaddock
    parseValue s = case safeReadBool s of
        Just True  -> Just WithHaddock
        Just False -> Just NoHaddock
        Nothing    -> Nothing
    optionName = return "haddock"
    optionHelp = return "Generate Haddock documentation for fixtures"
    optionCLParser = flagCLParser Nothing WithHaddock

-- | Convert 'HaddockMode' to an optional output directory
haddockOutputDir :: HaddockMode -> FilePath -> Maybe FilePath
haddockOutputDir NoHaddock   _        = Nothing
haddockOutputDir WithHaddock buildDir = Just $ buildDir </> "haddock-fixtures"

{-------------------------------------------------------------------------------
  Tasty test helpers
-------------------------------------------------------------------------------}

-- | A simple test that runs an IO action returning a 'Result'
newtype SimpleTest = SimpleTest (IO Result)

instance IsTest SimpleTest where
    run _ (SimpleTest action) _ = action
    testOptions = return [Option (Proxy :: Proxy HaddockMode)]

-- | Create a test tree entry for a single fixture
--
-- Skipped fixtures show their reason; compiled fixtures look up their
-- result in the shared batch-build results map.
mkFixtureTest ::
     String                              -- ^ Library name prefix (e.g. @\"fixture-\"@)
  -> String                              -- ^ Label (e.g. @\"PP compilation\"@)
  -> (TestCase -> FixtureStatus)         -- ^ Status function
  -> IO (Map.Map String (Either String ()))  -- ^ Batch build results
  -> TestCase
  -> TestTree
mkFixtureTest prefix label determineStatus getResults tc =
    case determineStatus tc of
      FixtureSkip reason ->
          singleTest tc.name $
            SimpleTest $ return $ skippedResult reason
      _ ->
          singleTest tc.name $ SimpleTest $ do
              results <- getResults
              let libName = prefix ++ sanitizeLibName tc.name
              case Map.lookup libName results of
                  Just (Right ()) -> return $ passedResult $
                      label ++ " succeeded"
                  Just (Left err) -> return $ testFailed $
                      label ++ " failed:\n" ++ err
                  Nothing -> return $ testFailed
                      "Library not found in batch build results"

passedResult :: String -> Result
passedResult msg = Result Tasty.Success msg "OK" 0 noResultDetails

skippedResult :: String -> Result
skippedResult reason = Result Tasty.Success ("Skipped: " ++ reason) ("Skipped: " ++ reason) 0 noResultDetails

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Check if an outcome is 'Success'
isSuccess :: Outcome -> Bool
isSuccess Success = True
isSuccess _       = False

-- | Check if a test name is a variant test (e.g., "foo.1.bar")
isVariantTest :: String -> Bool
isVariantTest name = any isVariantSuffix (suffixes name)
  where
    suffixes :: String -> [String]
    suffixes []     = []
    suffixes (_:xs) = xs : suffixes xs

    isVariantSuffix :: String -> Bool
    isVariantSuffix ('.':c:_) = c `elem` ['0'..'9']
    isVariantSuffix _         = False
