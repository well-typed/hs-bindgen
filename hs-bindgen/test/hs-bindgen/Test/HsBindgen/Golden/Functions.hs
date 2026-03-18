-- | Golden tests: functions
module Test.HsBindgen.Golden.Functions (testCases) where

import HsBindgen.Backend.Category
import HsBindgen.Config.Internal
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId qualified as PrelimDeclId
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.Select.IsPass
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
      defaultTest "functions/callbacks"
    , defaultTest "functions/circular_dependency_fun"
    , defaultTest "functions/heap_types/struct_const_member"
    , defaultTest "functions/heap_types/struct_const_typedef"
    , defaultTest "functions/heap_types/struct_const"
    , defaultTest "functions/heap_types/struct"
    , defaultTest "functions/heap_types/union_const_member"
    , defaultTest "functions/heap_types/union_const_typedef"
    , defaultTest "functions/heap_types/union_const"
    , defaultTest "functions/heap_types/union"
    , defaultTest "functions/typedef_funptr"
      -- Bespoke tests
    , test_functions_decls_in_signature
    , test_functions_fun_attributes
    , test_functions_fun_attributes_conflict
    , test_functions_simple_func
    , test_functions_simple_func_rename
    , test_functions_varargs
    ]

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

test_functions_decls_in_signature :: TestCase
test_functions_decls_in_signature =
    testTraceMulti "functions/decls_in_signature" declsWithMsgs $ \case
      MatchDelayed name ParseUnsupportedAnonInSignature{} ->
        Just $ Expected name
      MatchDiagnosticOption _diag ->
        Just $ Tolerated
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["f3", "f4", "f5"]

test_functions_fun_attributes :: TestCase
test_functions_fun_attributes =
    defaultTest "functions/fun_attributes"
      & #clangVersion   .~ Just (>= (15, 0, 0))
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchDelayed name ParseUnsupportedVariadicFunction ->
              Just $ Expected name
            MatchDelayed name ParseNonPublicVisibility ->
              Just $ Expected name
            MatchDelayed name ParsePotentialDuplicateSymbol{} ->
              Just $ Expected name
            MatchSelect name SelectDeprecated{} ->
              Just $ Expected name
            MatchSelect name (SelectParseNotAttempted DeclarationUnavailable) ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = [
          "my_printf"
        , "i"
        , "f3"
        , "old_fn_deprecated"
        , "old_fn_unavailable"
        ]

test_functions_fun_attributes_conflict :: TestCase
test_functions_fun_attributes_conflict =
    testTraceMulti "functions/fun_attributes_conflict" declsWithMsgs $ \case
      MatchDiagnosticOption "-Wno-ignored-attributes" ->
        Just Tolerated
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = []

test_functions_simple_func :: TestCase
test_functions_simple_func =
    defaultTest "functions/simple_func"
      & #cStandard .~ c99

test_functions_simple_func_rename :: TestCase
test_functions_simple_func_rename =
    testVariant "functions/simple_func" "1.rename"
      & #cStandard .~ c99
      & #onBackend .~ ( #categoryChoice .~ ByCategory {
            cType = IncludeTypeCategory
          , cSafe = ExcludeCategory
          , cUnsafe = ExcludeCategory
          , cFunPtr = IncludeTermCategory $ RenameTerm $ \t -> t <> "_random_user_specified_suffix"
          , cGlobal = ExcludeCategory
          })

test_functions_varargs :: TestCase
test_functions_varargs =
    testTraceMulti "functions/varargs" declsWithMsgs $ \case
      MatchDelayed name ParseUnsupportedVariadicFunction ->
        Just $ Expected name
      MatchDelayed name (
        ParseUnderlyingTypeFailed
          (PrelimDeclId.Named (CDeclName "va_list" CNameKindOrdinary))
          (ParseUnsupportedBuiltin "__builtin_va_list")
        ) ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["f", "g"]
