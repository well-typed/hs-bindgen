{-# LANGUAGE CPP                #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Test case definitions for TH fixture compilation
--
-- This module determines which golden test cases can be compiled as TH
-- fixtures by importing the test case list from "Test.HsBindgen.Golden".
--
module Test.HsBindgen.THFixtures.TestCases (
    determineTHStatus
  ) where

import Clang.Version

import Test.HsBindgen.Fixtures.TestCases
import Test.HsBindgen.Golden.Infra.TestCase

{-------------------------------------------------------------------------------
  TH compilation status
-------------------------------------------------------------------------------}

-- | Determine if a test case should be compiled as TH
--
-- Uses 'commonFixtureStatus' for shared skip reasons, then applies
-- TH-specific checks.
--
determineTHStatus :: TestCase -> FixtureStatus
determineTHStatus tc
  -- Common skip reasons (shared with PP)
  | Just status <- commonFixtureStatus tc
      = status
  -- Check clangVersion requirement
  | Just versionPred <- tc.clangVersion
  , case clangVersion of
      ClangVersion version  -> not (versionPred version)
      ClangVersionUnknown _ -> True
      = FixtureSkip "Requires newer clang version"
  -- Complex configuration (program slicing, disabled stdlib)
  --
  -- NOTE: These are TH-specific; PP fixture compilation handles
  -- multi-module fixtures natively via @--category@ flags.
  | tc.name `elem` complexConfig
      = FixtureSkip "Complex configuration (program slicing, disabled stdlib)"
  -- Windows-specific failures
  | tc.name `elem` windowsSpecificFailures
      = FixtureSkip "Windows-specific failure"
  -- Variant tests (name contains a period followed by a number)
  | isVariantTest tc.name
      = FixtureSkip "Variant test (only base test compiled)"
  | otherwise
      = FixtureCompile
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

    complexConfig :: [String]
    complexConfig = [
        "globals/globals"
      , "program-analysis/program_slicing_simple"
      , "program-analysis/selection_omit_external_a"
      , "program-analysis/selection_omit_external_b"
      ]
