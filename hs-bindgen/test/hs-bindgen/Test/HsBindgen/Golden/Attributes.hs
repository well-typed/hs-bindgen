-- | Golden tests: attributes
module Test.HsBindgen.Golden.Attributes (testCases) where

import HsBindgen.Config.Internal
import HsBindgen.Frontend.Naming
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
      test_attributes_asm
    , test_attributes_attributes
    , test_attributes_deprecated
    , test_attributes_type_attributes
    , test_attributes_visibility_functions
    , test_attributes_visibility_variables
    ]

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

test_attributes_asm :: TestCase
test_attributes_asm =
    defaultTest "attributes/asm"
      & #clangVersion .~ Just (>= (18, 0, 0))
      & #cStandard    .~ gnu23

test_attributes_attributes :: TestCase
test_attributes_attributes =
    testDiagnostic "attributes/attributes" $ \diag ->
      diagnosticCategoryText diag == "Nullability Issue"

test_attributes_deprecated :: TestCase
test_attributes_deprecated =
  defaultTest "attributes/deprecated"
    & #onFrontend .~ ( #selectPredicate .~
          BAnd
            (BIf (SelectHeader FromMainHeaders))
            (BNot (BIf (SelectDecl DeclDeprecated)))
        )
    & #tracePredicate .~ singleTracePredicate (\case
            MatchNoDeclarations ->
              Just $ Expected ()
            _otherwise ->
              Nothing
          )

test_attributes_type_attributes :: TestCase
test_attributes_type_attributes =
    testTraceSimple "attributes/type_attributes" $ \case
      MatchSelect _name SelectDeprecated{} ->
        Just $ Expected ()
      _otherwise ->
        Nothing

test_attributes_visibility_functions :: TestCase
test_attributes_visibility_functions =
    defaultTest "attributes/visibility/functions"
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchDelayed name ParsePotentialDuplicateSymbol{} ->
              Just $ Expected name
            MatchDelayed name ParseNonPublicVisibility{} ->
              Just $ Expected name
            MatchDiagnosticOption "-Wno-extern-initializer" ->
              Just Tolerated
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = [
          -- Problematic non-public visibility
          "f2" , "f3" , "f4"
        , "f12", "f13", "f14"

          -- Duplicate symbols
        , "f5" , "f6" , "f7" , "f8" , "f9"
        , "f15", "f16", "f17", "f18", "f19"
        ]

test_attributes_visibility_variables :: TestCase
test_attributes_visibility_variables =
    defaultTest "attributes/visibility/variables"
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchDelayed name ParsePotentialDuplicateSymbol{} ->
              Just $ Expected name
            MatchDelayed name ParseNonPublicVisibility{} ->
              Just $ Expected name
            MatchDiagnosticOption "-Wno-extern-initializer" ->
              Just Tolerated
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = [
          -- Problematic non-public visibility
          "i12", "i13", "i14"
          -- Duplicate symbols
        , "i0" , "i1" , "i2" , "i3" , "i4"
        , "i5" , "i6" , "i7" , "i8" , "i9"
        , "i15", "i16", "i17", "i18", "i19"
        ]
