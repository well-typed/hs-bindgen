{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Test case definitions for TH fixture compilation
--
-- This module provides the list of fixtures to compile by importing from
-- the shared test case specifications in test-common.
--
module Test.THFixtures.TestCases (
    TestCaseInfo(..)
  , TestCaseSpec(..)
  , THStatus(..)
  , allTestCases
  , specName
  ) where

import Data.List (isPrefixOf)

import Clang.Version

import Test.Common.HsBindgen.TestCase.All (allTestCaseSpecs)
import Test.Common.HsBindgen.TestCase.Spec

{-------------------------------------------------------------------------------
  Platform-specific skips
-------------------------------------------------------------------------------}

-- | Tests that fail on Windows due to platform-specific issues
--
-- Each entry pairs a test name with its specific skip reason.
--
windowsSpecificFailures :: [(String, String)]
windowsSpecificFailures =
#ifdef mingw32_HOST_OS
    [ ("types/structs/bitfields"
      , "Windows: 33-bit bitfield invalid (long is 32-bit)")
    , ("edge-cases/adios"
      , "Windows: non-NFC Unicode cannot be encoded in temp file paths")
    , ("types/complex/complex_non_float_test"
      , "Windows: clang parses C99 'complex' keyword differently")
    , ("types/complex/hsb_complex_test"
      , "Windows: clang parses C99 'complex' keyword differently")
    , ("program-analysis/selection_bad_size_t"
      , "Windows: size_t typedef conflicts with system headers")
    , ("program-analysis/selection_bad"
      , "Windows: size_t typedef conflicts with system headers")
    , ("types/stdlib/stdlib_insts"
      , "Windows: stdlib headers contain _Static_assert not yet supported")
    ]
#else
    []
#endif

-- | Tests that fail on macOS due to platform-specific issues
--
-- Each entry pairs a test name with its specific skip reason.
--
macosSpecificFailures :: [(String, String)]
macosSpecificFailures =
#ifdef darwin_HOST_OS
    [ ("types/stdlib/stdlib_insts"
      , "macOS: uchar.h header not found in CI environment")
    ]
#else
    []
#endif

{-------------------------------------------------------------------------------
  Test case info
-------------------------------------------------------------------------------}

-- | Information about a test case for TH compilation
--
-- This wraps the TestCaseSpec with a TH-specific status.
--
data TestCaseInfo = TestCaseInfo {
      -- | The shared test case specification
      spec   :: TestCaseSpec
      -- | Whether this test should be compiled or skipped
    , status :: THStatus
    }

-- | Get the name from a TestCaseSpec
--
specName :: TestCaseSpec -> String
specName s = s.name

{-------------------------------------------------------------------------------
  Build test cases from shared specs
-------------------------------------------------------------------------------}

-- | All test cases for TH compilation
--
-- This imports the shared specs and determines which ones can be compiled.
--
allTestCases :: [TestCaseInfo]
allTestCases = map buildTestCaseInfo allTestCaseSpecs

-- | Build a TestCaseInfo from a TestCaseSpec
buildTestCaseInfo :: TestCaseSpec -> TestCaseInfo
buildTestCaseInfo s = TestCaseInfo {
    spec   = s
  , status = determineTHStatus s
  }

-- | Determine if a test case should be compiled as TH
--
-- This function extends the basic 'thStatus' with additional skip conditions
-- specific to TH compilation.
--
determineTHStatus :: TestCaseSpec -> THStatus
determineTHStatus s
  -- Platform-specific failures
  | Just reason <- lookup s.name windowsSpecificFailures
      = THSkip reason
  | Just reason <- lookup s.name macosSpecificFailures
      = THSkip reason
  -- Check clangVersion requirement first
  | Just versionPred <- s.clangVersion
  , case clangVersion of
      ClangVersion version  -> not (versionPred version)
      ClangVersionUnknown _ -> True
      = THSkip "Requires newer clang version"
  -- First check basic outcome
  | s.outcome /= Success
      = THSkip "Expected failure test"
  -- Tests with external binding specs not yet supported in TH
  | "binding-specs/fun_arg/" `isPrefixOf` s.name
      = THSkip "External binding specs not yet supported (issue #1495)"
  -- Apple block extension requires clang
  | s.name == "edge-cases/iterator"
      = THSkip "Apple block extension requires clang (issue #913)"
  -- Unusable struct
  | s.name == "functions/decls_in_signature"
      = THSkip "Unusable struct (issue #1128)"
  -- Issue #1490
  | s.name `elem` [
        "functions/heap_types/struct_const_member"
      , "functions/heap_types/struct_const_typedef"
      , "functions/heap_types/struct_const"
      , "functions/heap_types/union_const_member"
      , "functions/heap_types/union_const_typedef"
      , "functions/heap_types/union_const"
      ]
      = THSkip "Issue #1490"
  -- Complex configuration (program slicing, disabled stdlib)
  | s.name `elem` [
        "globals/globals"
      , "program-analysis/program_slicing_simple"
      , "program-analysis/selection_omit_external_a"
      , "program-analysis/selection_omit_external_b"
      ]
      = THSkip "Complex configuration (program slicing, disabled stdlib)"
  -- Empty output fixtures
  | s.name `elem` emptyOutputFixtures
      = THSkip "Empty fixture (no bindings generated)"
  -- Variant tests (name contains a period followed by a number)
  | isVariantTest s.name
      = THSkip "Variant test (only base test compiled)"
  | otherwise
      = THCompile

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

-- | Fixtures that produce no output (empty bindings)
--
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
