-- | Golden tests: edge cases
module Test.HsBindgen.Golden.EdgeCases (testCases) where

import HsBindgen.Config.ClangArgs
import HsBindgen.Config.Internal
import HsBindgen.Frontend.Analysis.DeclIndex (UnusableEntry (..),
                                              UnusableReason (..))
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
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
      defaultTest "edge-cases/aux_funptr_newtypes"
    , defaultTest "edge-cases/distilled_lib_1"
    , defaultTest "edge-cases/enum_as_array_size"
    , defaultTest "edge-cases/flam_functions"
    , defaultTest "edge-cases/flam"
    , defaultTest "edge-cases/mangle_fun_param_names"
    , defaultTest "edge-cases/names"
    , defaultTest "edge-cases/spec_examples"
    , defaultTest "edge-cases/typedef_bitfield"
    , defaultTest "edge-cases/typedef_void"
    , defaultTest "edge-cases/unnamed_type_multiple_fields"
    , defaultTest "edge-cases/unnamed_type_multiple_typedefs"
    , defaultTest "edge-cases/uses_utf8"
      -- Bespoke tests
    , test_adios
    , test_clang_generated_collision
    , test_duplicate
    , test_duplicate_record_field
    , test_headers
    , test_include_macro
    , test_iterator
    , test_ordinary_unnamed_decl
    , test_select_no_match
    , test_thread_local
    , test_unsupported_builtin
    ]

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

test_adios :: TestCase
test_adios =
    defaultTest "edge-cases/adios"
      & #cStandard .~ c11

test_clang_generated_collision :: TestCase
test_clang_generated_collision =
    defaultTest "edge-cases/clang_generated_collision"
      & #clangVersion   .~ Just (>= (16, 0, 0))
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name SelectConflict{} ->
              Just $ Expected name
            MatchSelect _ (MatchTransMissing [MatchTransUnusable (UnusableConflict{})]) ->
              Just Tolerated
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["struct foo"]

test_duplicate :: TestCase
test_duplicate =
    defaultTest "edge-cases/duplicate"
      & #onFrontend .~ (\cfg -> cfg
          & #selectionPredicate .~ BOr
              (BIf $ SelectDecl (DeclNameMatches "function"))
              (BIf $ SelectDecl (DeclNameMatches "duplicate"))
          & #programSlicing .~ EnableProgramSlicing
          )
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name SelectConflict{} ->
              Just $ Expected (name, "conflict")
            MatchSelect name (MatchTransMissing [MatchTransUnusable UnusableConflict{}]) ->
              Just $ Expected (name, "transitive conflict")
            MatchUnusable name UnusableMangleNamesFailure{} ->
              Just $ Expected (name, "mangle")
            _otherwise ->
               Nothing
          )
  where
    declsWithMsgs :: [(C.DeclName, String)]
    declsWithMsgs = [
          ("macro duplicate", "conflict")
        , ("duplicate",       "conflict")
        , ("function",        "transitive conflict")
        , ("function",        "mangle")
        ]

test_duplicate_record_field :: TestCase
test_duplicate_record_field =
    defaultTest "edge-cases/duplicate_record_field"
      & #onFrontend .~ (\cfg -> cfg
            & #fieldNamingStrategy .~ OmitFieldPrefixes
          )

test_headers :: TestCase
test_headers =
    testTraceSimple "edge-cases/headers" $ \case
      MatchNoDeclarations ->
        Just $ Expected ()
      _otherwise ->
        Nothing

test_include_macro :: TestCase
test_include_macro =
    defaultTest "edge-cases/include_macro_parent"
      & #onFrontend .~ ( #selectionPredicate .~ BTrue )

test_iterator :: TestCase
test_iterator =
    defaultTest "edge-cases/iterator"
      & #clangVersion .~ Just (>= (15, 0, 0))
      & #cStandard    .~ c23
      & #onBoot       .~ ( #clangArgs % #enableBlocks .~ True )

test_ordinary_unnamed_decl :: TestCase
test_ordinary_unnamed_decl =
    defaultTest "edge-cases/ordinary_unnamed_decl_parent"
      & #onFrontend .~ ( #selectionPredicate .~ BTrue )

test_select_no_match :: TestCase
test_select_no_match =
    defaultTest "edge-cases/select_no_match"
      & #onFrontend .~ ( #selectionPredicate .~
            BIf (SelectDecl (DeclNameMatches "this_pattern_will_never_match"))
          )
      & #tracePredicate .~ singleTracePredicate (\case
            MatchNoDeclarations ->
              Just $ Expected ()
            _otherwise ->
              Nothing
          )

test_thread_local :: TestCase
test_thread_local =
    defaultTest "edge-cases/thread_local"
      & #clangVersion   .~ Just (>= (16, 0, 0))
      & #cStandard    .~ c23
      & #tracePredicate .~ singleTracePredicate (\case
            MatchDelayed _name ParseUnsupportedTLS ->
              Just $ Expected ()
            _otherwise ->
              Nothing
          )

test_unsupported_builtin :: TestCase
test_unsupported_builtin =
    testTraceMulti "edge-cases/unsupported_builtin" declsWithMsgs $ \case
      MatchDelayed name (ParseUnsupportedBuiltin "__builtin_va_list") ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["va_list"]
