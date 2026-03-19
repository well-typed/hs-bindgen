-- | Golden tests: arrays
module Test.HsBindgen.Golden.Arrays (testCases) where

import HsBindgen.Frontend.Naming
import HsBindgen.Imports
import HsBindgen.TraceMsg

import Test.Common.HsBindgen.Trace.Patterns
import Test.Common.HsBindgen.Trace.Predicate
import Test.HsBindgen.Golden.Infra.TestCase

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

testCases :: [TestCase]
testCases = [
      test_arrays_array
    , test_arrays_const_qualifier
    , test_arrays_multi_dim
    , test_arrays_failing_array_res_1
    , test_arrays_failing_array_res_2
    , test_arrays_failing_array_res_3
    , test_arrays_failing_array_res_4
    , test_arrays_failing_array_res_5
    , test_arrays_failing_array_res_6
    , test_arrays_failing_array_res_7
    , test_arrays_failing_array_res_8
    ]

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

test_arrays_array :: TestCase
test_arrays_array =
    defaultTest "arrays/array"
      & #clangVersion   .~ Just (>= (19, 0, 0))
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchDelayed name ParsePotentialDuplicateSymbol{} ->
              Just $ Expected name
            MatchDiagnosticOption "-Wno-extern-initializer" ->
              Just Tolerated
            MatchDiagnosticOption "-Wno-tentative-definition-array" ->
              Just Tolerated
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = [
          -- Duplicate symbols
          "arr0"
        , "arr3"
        , "arr1"
        , "arr6"
        ]

-- | @const@ qualifiers applied to arrays (through typedefs) should be
-- represented in the bindings.
test_arrays_const_qualifier :: TestCase
test_arrays_const_qualifier = defaultTest "arrays/const_qualifier"

test_arrays_multi_dim :: TestCase
test_arrays_multi_dim = defaultTest "arrays/multi_dim"

test_arrays_failing_array_res_1 :: TestCase
test_arrays_failing_array_res_1 =
    failingTestLibclangSimple "arrays/failing/array_res_1" $ \case
      (matchDiagnosticSpelling "function cannot return array type" -> Just _diag) ->
        Just $ Expected ()
      TraceFrontend (FrontendClang _) ->
        Just $ Tolerated
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

test_arrays_failing_array_res_2 :: TestCase
test_arrays_failing_array_res_2 =
    failingTestLibclangSimple "arrays/failing/array_res_2" $ \case
      (matchDiagnosticSpelling "function cannot return array type" -> Just _diag) ->
        Just $ Expected ()
      TraceFrontend (FrontendClang _) ->
        Just $ Tolerated
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

test_arrays_failing_array_res_3 :: TestCase
test_arrays_failing_array_res_3 =
    failingTestLibclangSimple "arrays/failing/array_res_3" $ \case
      (matchDiagnosticSpelling "function cannot return array type" -> Just _diag) ->
        Just $ Expected ()
      TraceFrontend (FrontendClang _) ->
        Just $ Tolerated
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

test_arrays_failing_array_res_4 :: TestCase
test_arrays_failing_array_res_4 =
    failingTestLibclangSimple "arrays/failing/array_res_4" $ \case
      (matchDiagnosticSpelling "function cannot return array type" -> Just _diag) ->
        Just $ Expected ()
      TraceFrontend (FrontendClang _) ->
        Just $ Tolerated
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

test_arrays_failing_array_res_5 :: TestCase
test_arrays_failing_array_res_5 =
    failingTestLibclangSimple "arrays/failing/array_res_5" $ \case
      (matchDiagnosticSpelling "function cannot return array type" -> Just _diag) ->
        Just $ Expected ()
      TraceFrontend (FrontendClang _) ->
        Just $ Tolerated
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

test_arrays_failing_array_res_6 :: TestCase
test_arrays_failing_array_res_6 =
    failingTestLibclangSimple "arrays/failing/array_res_6" $ \case
      (matchDiagnosticSpelling "function cannot return array type" -> Just _diag) ->
        Just $ Expected ()
      TraceFrontend (FrontendClang _) ->
        Just $ Tolerated
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

test_arrays_failing_array_res_7 :: TestCase
test_arrays_failing_array_res_7 =
    failingTestLibclangSimple "arrays/failing/array_res_7" $ \case
      (matchDiagnosticSpelling "function cannot return array type" -> Just _diag) ->
        Just $ Expected ()
      TraceFrontend (FrontendClang _) ->
        Just $ Tolerated
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

test_arrays_failing_array_res_8 :: TestCase
test_arrays_failing_array_res_8 =
    failingTestLibclangSimple "arrays/failing/array_res_8" $ \case
      (matchDiagnosticSpelling "function cannot return array type" -> Just _diag) ->
        Just $ Expected ()
      TraceFrontend (FrontendClang _) ->
        Just $ Tolerated
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing
