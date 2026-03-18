-- | Golden tests: edge cases
module Test.HsBindgen.Golden.EdgeCases (testCases) where

import HsBindgen.Config.ClangArgs
import HsBindgen.Config.Internal
import HsBindgen.Frontend.Analysis.DeclIndex (Unusable (..))
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.TraceMsg

import Test.Common.HsBindgen.Trace.Patterns
import Test.Common.HsBindgen.Trace.Predicate
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

testCases :: [TestCase]
testCases = [
      -- Default tests
      defaultTest "edge-cases/anon_multiple_fields"
    , defaultTest "edge-cases/anon_multiple_typedefs"
    , defaultTest "edge-cases/aux_funptr_newtypes"
    , defaultTest "edge-cases/distilled_lib_1"
    , defaultTest "edge-cases/enum_as_array_size"
    , defaultTest "edge-cases/flam_functions"
    , defaultTest "edge-cases/flam"
    , defaultTest "edge-cases/names"
    , defaultTest "edge-cases/spec_examples"
    , defaultTest "edge-cases/typedef_bitfield"
    , defaultTest "edge-cases/typedef_void"
    , defaultTest "edge-cases/uses_utf8"
      -- Bespoke tests
    , test_edgeCases_adios
    , test_edgeCases_clang_generated_collision
    , test_edgeCases_duplicate
    , test_edgeCases_duplicate_record_field
    , test_edgeCases_headers
    , test_edgeCases_include_macro
    , test_edgeCases_iterator
    , test_edgeCases_ordinary_anon
    , test_edgeCases_select_no_match
    , test_edgeCases_thread_local
    , test_edgeCases_unsupported_builtin
    ]

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

test_edgeCases_adios :: TestCase
test_edgeCases_adios =
    defaultTest "edge-cases/adios"
      & #cStandard .~ c11

test_edgeCases_clang_generated_collision :: TestCase
test_edgeCases_clang_generated_collision =
    defaultTest "edge-cases/clang_generated_collision"
      & #clangVersion   .~ Just (>= (16, 0, 0))
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name SelectConflict{} ->
              Just $ Expected name
            MatchSelect _ (MatchTransMissing [MatchTransUnusable UnusableConflict{}]) ->
              Just Tolerated
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["struct foo"]

test_edgeCases_duplicate :: TestCase
test_edgeCases_duplicate =
    defaultTest "edge-cases/duplicate"
      & #onFrontend .~ (\cfg -> cfg
          & #selectPredicate .~ BOr
              (BIf $ SelectDecl (DeclNameMatches "function"))
              (BIf $ SelectDecl (DeclNameMatches "duplicate"))
          )
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name SelectConflict{} ->
              Just $ Expected (name, "conflict")
            MatchSelect name (MatchTransMissing [MatchTransUnusable UnusableConflict{}]) ->
              Just $ Expected (name, "transitive conflict")
            _otherwise ->
               Nothing
          )
  where
    declsWithMsgs :: [(CDeclName, String)]
    declsWithMsgs = [
          ("duplicate", "conflict")
        , ("function", "transitive conflict")
        ]

test_edgeCases_duplicate_record_field :: TestCase
test_edgeCases_duplicate_record_field =
    defaultTest "edge-cases/duplicate_record_field"
      & #onFrontend .~ (\cfg -> cfg
            & #fieldNamingStrategy .~ OmitFieldPrefixes
          )

test_edgeCases_headers :: TestCase
test_edgeCases_headers =
    testTraceSimple "edge-cases/headers" $ \case
      MatchNoDeclarations ->
        Just $ Expected ()
      _otherwise ->
        Nothing

test_edgeCases_include_macro :: TestCase
test_edgeCases_include_macro =
    defaultTest "edge-cases/include_macro_parent"
      & #onFrontend .~ ( #selectPredicate .~ BTrue )

test_edgeCases_iterator :: TestCase
test_edgeCases_iterator =
    defaultTest "edge-cases/iterator"
      & #clangVersion .~ Just (>= (15, 0, 0))
      & #cStandard    .~ c23
      & #onBoot       .~ ( #clangArgs % #enableBlocks .~ True )

test_edgeCases_ordinary_anon :: TestCase
test_edgeCases_ordinary_anon =
    defaultTest "edge-cases/ordinary_anon_parent"
      & #onFrontend .~ ( #selectPredicate .~ BTrue )

test_edgeCases_select_no_match :: TestCase
test_edgeCases_select_no_match =
    defaultTest "edge-cases/select_no_match"
      & #onFrontend .~ ( #selectPredicate .~
            BIf (SelectDecl (DeclNameMatches "this_pattern_will_never_match"))
          )
      & #tracePredicate .~ singleTracePredicate (\case
            MatchNoDeclarations ->
              Just $ Expected ()
            _otherwise ->
              Nothing
          )

test_edgeCases_thread_local :: TestCase
test_edgeCases_thread_local =
    defaultTest "edge-cases/thread_local"
      & #clangVersion   .~ Just (>= (16, 0, 0))
      & #cStandard    .~ c23
      & #tracePredicate .~ singleTracePredicate (\case
            MatchDelayed _name ParseUnsupportedTLS ->
              Just $ Expected ()
            _otherwise ->
              Nothing
          )

test_edgeCases_unsupported_builtin :: TestCase
test_edgeCases_unsupported_builtin =
    testTraceMulti "edge-cases/unsupported_builtin" declsWithMsgs $ \case
      MatchDelayed name (ParseUnsupportedBuiltin "__builtin_va_list") ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["va_list"]
