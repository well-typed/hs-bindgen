-- | Golden tests: declarations
module Test.HsBindgen.Golden.Declarations (testCases) where

import HsBindgen.Config.Internal
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.MangleNames.Error
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.TraceMsg

import Test.Common.HsBindgen.Trace.Patterns
import Test.Common.HsBindgen.Trace.Predicate
import Test.HsBindgen.Golden.Infra.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

testCases :: [TestCase]
testCases = [
      -- Default tests
      defaultTest "declarations/declarations_required_for_scoping"
    , defaultTest "declarations/forward_declaration"
    , defaultTest "declarations/opaque_declaration"
      -- Bespoke tests
    , test_declarations_declaration_unselected_b
    , test_declarations_definitions
    , test_declarations_duplicate_field_name_omit
    , test_declarations_failing_tentative_definitions_linkage
    , test_declarations_name_collision
    , test_declarations_name_collision_aux
    , test_declarations_redeclaration
    , test_declarations_redeclaration_different
    , test_declarations_redeclaration_identical
    , test_declarations_select_scoping
    , test_declarations_tentative_definitions
    ]

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

test_declarations_declaration_unselected_b :: TestCase
test_declarations_declaration_unselected_b =
    testTraceMulti "declarations/declaration_unselected_b" declsWithMsgs $ \case
      MatchSelect name (MatchTransMissing [MatchTransNotSelected]) ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["f"]

test_declarations_definitions :: TestCase
test_declarations_definitions =
    testTraceMulti "declarations/definitions" declsWithMsgs $ \case
      MatchDelayed name ParsePotentialDuplicateSymbol{} ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["foo", "n"]

test_declarations_duplicate_field_name_omit :: TestCase
test_declarations_duplicate_field_name_omit =
    defaultTest "declarations/duplicate_field_name_omit"
      & #onFrontend      .~ (\cfg -> cfg
            & #fieldNamingStrategy .~ OmitFieldPrefixes
          )
      & #tracePredicate  .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name (SelectMangleNamesFailure MangleNamesDuplicateFieldName{}) ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["struct S"]

test_declarations_failing_tentative_definitions_linkage :: TestCase
test_declarations_failing_tentative_definitions_linkage =
    failingTestLibclangMulti "declarations/failing/tentative_definitions_linkage" [(), ()] $ \case
      (matchDiagnosticSpelling "non-static declaration of" -> Just _diag) ->
        Just $ Expected ()
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

-- This tests https://github.com/well-typed/hs-bindgen/issues/1373.
--
-- It covers both a cross-declaration collision (@union y@ vs @union Y@) and an
-- intra-declaration collision (@enum Color { Color }@, whose tag and enumerator
-- both produce the Haskell name @Color@).
test_declarations_name_collision :: TestCase
test_declarations_name_collision =
    testTraceMulti "declarations/name_collision" declsWithMsgs $ \case
      MatchSelect name (SelectMangleNamesFailure MangleNamesCollision{}) ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["union y", "union Y", "enum Color", "struct S", "S_x"]

-- This tests sweep-2 collision detection for names derived in pass 2:
--
-- * A struct with a flexible array member generates @Foo_Aux@ (the FLAM
--   auxiliary type), which clashes with a top-level @foo_Aux@ declaration.
--
-- * A function-pointer typedef generates @Bar_Aux@ (the function-pointer
--   auxiliary type), which clashes with a top-level @bar_Aux@ declaration.
test_declarations_name_collision_aux :: TestCase
test_declarations_name_collision_aux =
    testTraceMulti "declarations/name_collision_aux" declsWithMsgs $ \case
      MatchSelect name (SelectMangleNamesFailure MangleNamesCollision{}) ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["struct foo", "foo_Aux", "bar", "bar_Aux"]

test_declarations_redeclaration :: TestCase
test_declarations_redeclaration =
    defaultTest "declarations/redeclaration"
      & #cStandard      .~ c11
      & #tracePredicate .~ multiTracePredicate expected trace
  where
    expected :: [CDeclName]
    expected = ["x"]

    trace :: TraceMsg -> Maybe (TraceExpectation CDeclName)
    trace = \case
      MatchDelayed name ParsePotentialDuplicateSymbol{} ->
        Just $ Expected name
      _otherwise ->
        Nothing

test_declarations_redeclaration_different :: TestCase
test_declarations_redeclaration_different =
    testTraceSimple "declarations/redeclaration_different" $ \case
      MatchSelect _name SelectConflict{} ->
        Just $ Expected ()
      (matchDiagnosticSpelling "macro redefined" -> Just _diag) ->
        Just $ Tolerated
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

test_declarations_redeclaration_identical :: TestCase
test_declarations_redeclaration_identical =
    defaultTest "declarations/redeclaration_identical"
      & #cStandard      .~ c11
      & #tracePredicate .~ multiTracePredicate expected trace
  where
    expected :: [CDeclName]
    expected = ["macro A"]

    trace :: TraceMsg -> Maybe (TraceExpectation CDeclName)
    trace = \case
      MatchSelect name@"macro A" SelectConflict{} ->
        Just $ Expected name
      _otherwise ->
        Nothing

test_declarations_select_scoping :: TestCase
test_declarations_select_scoping =
    defaultTest "declarations/select_scoping"
      & #onFrontend .~ (\cfg -> cfg
          & #selectionPredicate .~ BIf (SelectHeader FromMainHeaders)
          )
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name (MatchTransMissing [MatchTransNotSelected]) ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = [
          "ParsedAndSelected2"
        , "ParsedAndSelected3"
        ]

test_declarations_tentative_definitions :: TestCase
test_declarations_tentative_definitions =
    testTraceMulti "declarations/tentative_definitions" declsWithMsgs $ \case
      MatchDelayed name ParsePotentialDuplicateSymbol{} ->
        Just $ Expected name
      MatchDiagnosticOption "-Wno-extern-initializer" ->
        Just $ Tolerated
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["i1", "i2", "i3"]
