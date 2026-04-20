{-# LANGUAGE OverloadedRecordDot #-}

-- | Test case definitions for PP fixture compilation
--
-- This module determines which golden test cases can be compiled as PP
-- fixtures. Common skip logic lives in "Test.HsBindgen.Fixtures.TestCases";
-- this module adds PP-specific checks.
--
module Test.HsBindgen.PPFixtures.TestCases (
    determinePPStatus
  ) where

import Test.HsBindgen.Fixtures.TestCases
import Test.HsBindgen.Golden.Infra.TestCase

{-------------------------------------------------------------------------------
  PP compilation status
-------------------------------------------------------------------------------}

-- | Determine if a test case should be compiled as a PP fixture
--
-- Uses 'commonFixtureStatus' for shared skip reasons, then applies
-- PP-specific checks.
--
determinePPStatus :: TestCase -> FixtureStatus
determinePPStatus tc
  -- Common skip reasons (shared with TH)
  | Just status <- commonFixtureStatus tc
      = status
  -- PP-specific empty fixtures (e.g. variant tests that are empty)
  | tc.name `elem` ppExtraEmpty
      = FixtureSkip "Empty fixture (no bindings generated)"
  -- Known -Werror unclean fixtures
  --
  | tc.name `elem` knownWerrorUnclean
      = FixtureCompileNoOptcWerror
  | otherwise
      = FixtureCompile
  where
    -- Empty fixtures not covered by the common list
    ppExtraEmpty :: [String]
    ppExtraEmpty = [
        "program-analysis/selection_matches_c_names.2.negative_case"
      ]

    knownWerrorUnclean :: [String]
    knownWerrorUnclean = [
        "arrays/array"
      , "edge-cases/adios"
      , "attributes/visibility_attributes"
      , "attributes/visibility/functions"
      , "attributes/visibility/variables"
      , "declarations/tentative_definitions"
      ]
