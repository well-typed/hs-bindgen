{-# LANGUAGE CPP                #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Test case definitions for TH fixture compilation
--
-- This module determines which golden test cases can be compiled as TH
-- fixtures by importing the test case list from "Test.HsBindgen.Golden".
--
module Test.HsBindgen.THFixtures.TestCases (
    THStatus(..)
  , determineTHStatus
  ) where

import Data.List (isPrefixOf)

import Clang.Version

import Test.HsBindgen.Golden.TestCase

{-------------------------------------------------------------------------------
  TH compilation status
-------------------------------------------------------------------------------}

-- | TH compilation status
--
data THStatus
    = THCompile     -- ^ Compile this fixture
    | THSkip String -- ^ Skip with reason
  deriving stock (Eq, Show)

-- | Determine if a test case should be compiled as TH
--
-- NOTE: The skip logic here should be kept in sync with
-- @scripts/ci/compile-fixtures.sh@ KNOWN_FAILURES.
--
determineTHStatus :: TestCase -> THStatus
determineTHStatus tc
  -- Check clangVersion requirement first
  | Just versionPred <- tc.clangVersion
  , case clangVersion of
      ClangVersion version  -> not (versionPred version)
      ClangVersionUnknown _ -> True
      = THSkip "Requires newer clang version"
  -- First check basic outcome
  | not (isSuccess tc.outcome)
      = THSkip "Expected failure test"
  -- Tests with external binding specs not yet supported in TH
  | "binding-specs/fun_arg/" `isPrefixOf` tc.name
      = THSkip "External binding specs not yet supported (issue #1495)"
  -- Apple block extension requires clang
  | tc.name == "edge-cases/iterator"
      = THSkip "Apple block extension requires clang (issue #913)"
  -- Unusable struct
  | tc.name == "functions/decls_in_signature"
      = THSkip "Unusable struct (issue #1128)"
  -- Issue #1490
  | tc.name `elem` issue1490
      = THSkip "Issue #1490"
  -- Issue #1679
  | tc.name `elem` issue1679
      = THSkip "Issue #1679"
  -- Complex configuration (program slicing, disabled stdlib)
  | tc.name `elem` complexConfig
      = THSkip "Complex configuration (program slicing, disabled stdlib)"
  -- Empty output fixtures
  --
  -- NOTE: This list should be kept in sync with
  -- @scripts/ci/compile-fixtures.sh@ KNOWN_EMPTY.
  | tc.name `elem` emptyOutputFixtures
      = THSkip "Empty fixture (no bindings generated)"
  -- Windows-specific failures
  | tc.name `elem` windowsSpecificFailures
      = THSkip "Windows-specific failure"
  -- Variant tests (name contains a period followed by a number)
  | isVariantTest tc.name
      = THSkip "Variant test (only base test compiled)"
  | otherwise
      = THCompile
  where
    -- Non-NFC Unicode cannot be encoded in temp file paths on Windows
    windowsSpecificFailures :: [String]
#ifdef mingw32_HOST_OS
    windowsSpecificFailures = [
        "edge-cases/adios"
      ]
#else
    windowsSpecificFailures = []
#endif

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

    complexConfig :: [String]
    complexConfig = [
        "globals/globals"
      , "program-analysis/program_slicing_simple"
      , "program-analysis/selection_omit_external_a"
      , "program-analysis/selection_omit_external_b"
      ]

    emptyOutputFixtures :: [String]
    emptyOutputFixtures = [
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
      , "macros/macro_type_void"
      , "program-analysis/delay_traces"
      , "program-analysis/selection_foo"
      , "program-analysis/selection_merge_traces"
      , "program-analysis/selection_omit_prescriptive"
      , "program-analysis/selection_squash_typedef"
      , "types/special/long_double"
      , "types/structs/implicit_fields_struct"
      , "types/structs/unnamed-struct"
      , "types/unions/implicit_fields_union"
      , "types/typedefs/typenames"
      ]

-- | Check if an outcome is 'Success'
isSuccess :: Outcome -> Bool
isSuccess Success = True
isSuccess _       = False

-- | Check if a test name is a variant test (e.g., "foo.1.bar")
--
isVariantTest :: String -> Bool
isVariantTest name = any isVariantSuffix (suffixes name)
  where
    suffixes :: String -> [String]
    suffixes []     = []
    suffixes (_:xs) = xs : suffixes xs

    isVariantSuffix :: String -> Bool
    isVariantSuffix ('.':c:_) = c `elem` ['0'..'9']
    isVariantSuffix _         = False
