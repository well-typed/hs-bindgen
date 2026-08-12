-- | Golden tests: functions
module Test.HsBindgen.Golden.Functions (testCases) where

import HsBindgen.Backend.Category
import HsBindgen.Config.Internal
import HsBindgen.Frontend.Analysis.DeclIndex (UnusableReason (..))
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate (Boolean (..))
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
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
    , test_decls_in_signature
    , test_fun_attributes
    , test_fun_attributes_conflict
    , test_hash_defines
    , test_hash_defines_select_all
    , test_not_visible_decl
    , test_simple_func
    , test_simple_func_rename
    , test_varargs
    ]

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

test_decls_in_signature :: TestCase
test_decls_in_signature =
    testTraceMulti "functions/decls_in_signature" declsWithMsgs $ \case
      MatchDelayed name ParseDeclarationNotVisible{} ->
        Just $ Expected name
      MatchDelayed name ParseUnsupportedUnnamedInSignature{} ->
        Just $ Expected name
      MatchDiagnosticOption _diag ->
        Just $ Tolerated
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["f1", "f2", "f3", "f4", "f5"]

test_fun_attributes :: TestCase
test_fun_attributes =
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
            MatchUnusable name UnusableUnavailable ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = [
          "my_printf"
        , "i"
        , "f3"
        , "old_fn_deprecated"
        , "old_fn_unavailable"
        ]

test_fun_attributes_conflict :: TestCase
test_fun_attributes_conflict =
    testTraceMulti "functions/fun_attributes_conflict" declsWithMsgs $ \case
      MatchDiagnosticOption "-Wno-ignored-attributes" ->
        Just Tolerated
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = []

-- | @#define@ root directives reach the generation /and/ compilation stage
--
-- The header does not preprocess without @MY_SIZE@, so PP\/TH fixture
-- compilation only succeeds if the directives are forwarded to the wrapper
-- source.
test_hash_defines :: TestCase
test_hash_defines =
    defaultTest "functions/hash_defines"
      & #hashDefines .~ hashDefines

-- | Root-header @#define@s produce no bindings, not even under @--select-all@
--
-- They live in the synthetic root header, which is not a main header of
-- anything, so they are not attempted at all — no macro bindings, and no
-- traces.
test_hash_defines_select_all :: TestCase
test_hash_defines_select_all =
    testVariant "functions/hash_defines" (Just 1) "select_all"
      & #hashDefines .~ hashDefines
      & #onFrontend  .~ ( #selectionPredicate .~ BTrue)

hashDefines :: [C.HashDefine]
hashDefines = [
      C.HashDefine "MY_FEATURE" "1"
    , C.HashDefine "MY_SIZE"    "8"
    , C.HashDefine "MY_EMPTY"   ""
    ]

test_not_visible_decl :: TestCase
test_not_visible_decl =
    testTraceMulti "functions/not_visible_decl" declsWithMsgs $ \case
      MatchDelayed name ParseDeclarationNotVisible{} ->
        Just $ Expected name
      MatchDelayed name ParseUnsupportedUnnamedInSignature{} ->
        Just $ Expected name
      MatchDiagnosticOption _diag ->
        Just $ Tolerated
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["f", "g", "h"]

test_simple_func :: TestCase
test_simple_func =
    defaultTest "functions/simple_func"
      & #cStandard .~ c99

test_simple_func_rename :: TestCase
test_simple_func_rename =
    testVariant "functions/simple_func" (Just 1) "rename"
      & #cStandard .~ c99
      & #onBackend .~ ( #categoryChoice .~ ByCategory {
            cType = IncludeTypeCategory
          , cSafe = ExcludeCategory
          , cUnsafe = ExcludeCategory
          , cFunPtr = IncludeTermCategory $ RenameTerm $ \t -> t <> "_random_user_specified_suffix"
          , cGlobal = ExcludeCategory
          })

test_varargs :: TestCase
test_varargs =
    testTraceMulti "functions/varargs" declsWithMsgs $ \case
      MatchDelayed name ParseUnsupportedVariadicFunction ->
        Just $ Expected name
      MatchDelayed name (
        ParseUnderlyingTypeFailed
          (C.PrelimDeclIdNamed (C.DeclName "va_list" C.NameKindOrdinary))
          (ParseUnsupportedBuiltin "__builtin_va_list")
        ) ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["f", "g"]
