-- | Golden tests: program analysis
module Test.HsBindgen.Golden.ProgramAnalysis (testCases) where

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config.Internal
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.TraceMsg

import Test.Common.HsBindgen.Trace.Patterns
import Test.Common.HsBindgen.Trace.Predicate
import Test.HsBindgen.Golden.Infra.TestCase

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

testCases :: [TestCase]
testCases = [
      test_programAnalysis_delay_traces
      -- * Program slicing
    , test_programAnalysis_program_slicing_macro_selected
    , test_programAnalysis_program_slicing_macro_unselected
    , test_programAnalysis_program_slicing_typedef_selected
    , test_programAnalysis_program_slicing_typedef_unselected
    , test_programAnalysis_program_slicing_selection
    , test_programAnalysis_program_slicing_simple
      -- * Selection
    , test_programAnalysis_selection_bad
    , test_programAnalysis_selection_fail
    , test_programAnalysis_selection_fail_variant_1
    , test_programAnalysis_selection_fail_variant_2
    , test_programAnalysis_selection_fail_variant_3
    , test_programAnalysis_selection_foo
    , test_programAnalysis_selection_matches_c_names_1
    , test_programAnalysis_selection_matches_c_names_2
    , test_programAnalysis_selection_merge_traces
    , test_programAnalysis_selection_omit_external_a
    , test_programAnalysis_selection_omit_external_b
    , test_programAnalysis_selection_omit_prescriptive
    , test_programAnalysis_selection_squash
      -- * Typedef analysis
    , test_programAnalysis_typedef_analysis
    ]

{-------------------------------------------------------------------------------
  Delay traces
-------------------------------------------------------------------------------}

test_programAnalysis_delay_traces :: TestCase
test_programAnalysis_delay_traces =
    defaultTest "program-analysis/delay_traces"
      & #onFrontend .~ ( #selectPredicate .~
            -- NOTE: Matching for name kind is not good practice, but we want to
            -- check if nested, but deselected declarations are correctly
            -- assigned name kinds.
            BOr
              (BIf $ SelectDecl (DeclNameMatches "_function"))
              (BOr
                (BIf $ SelectDecl (DeclNameMatches "struct"))
                (BIf $ SelectDecl (DeclNameMatches "union"))
              )
            )
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchDelayed name ParseUnsupportedLongDouble ->
              Just $ Expected name
            MatchDelayed name ParseUnsupportedVariadicFunction ->
              Just $ Expected name
            MatchDelayed name ParseNestedDeclsFailed ->
              Just $ Expected name
            _otherwise ->
               Nothing
          )
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = [
          "long_double_function"
        , "var_arg_function"
        , "struct long_double_s"
        , "struct nested_long_double_s"
        , "struct nested_long_double_u"
          -- TODO https://github.com/well-typed/hs-bindgen/issues/1851
          -- Why are the anonymous nested structs not reported? Because
          -- they are skipped by the AssignAnonIds step?
        ]

{-------------------------------------------------------------------------------
  Program slicing
-------------------------------------------------------------------------------}

test_programAnalysis_program_slicing_macro_selected :: TestCase
test_programAnalysis_program_slicing_macro_selected =
    defaultTest "program-analysis/program-slicing/macro_selected"
      & #onFrontend .~ (\cfg -> cfg
            & #programSlicing .~ EnableProgramSlicing
          )

test_programAnalysis_program_slicing_macro_unselected :: TestCase
test_programAnalysis_program_slicing_macro_unselected =
    defaultTest "program-analysis/program-slicing/macro_unselected"
      & #onFrontend .~ (\cfg -> cfg
            & #programSlicing .~ DisableProgramSlicing
          )
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name TransitiveDependenciesMissing{} ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
  where
    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1679>
    -- We should get a message about @foo@ missing.
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = []

test_programAnalysis_program_slicing_typedef_selected :: TestCase
test_programAnalysis_program_slicing_typedef_selected =
    defaultTest "program-analysis/program-slicing/typedef_selected"
      & #onFrontend .~ (\cfg -> cfg
            & #programSlicing .~ EnableProgramSlicing
          )

test_programAnalysis_program_slicing_typedef_unselected :: TestCase
test_programAnalysis_program_slicing_typedef_unselected =
    defaultTest "program-analysis/program-slicing/typedef_unselected"
      & #onFrontend .~ (\cfg -> cfg
            & #programSlicing .~ DisableProgramSlicing
          )
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name TransitiveDependenciesMissing{} ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["foo"]

test_programAnalysis_program_slicing_selection :: TestCase
test_programAnalysis_program_slicing_selection =
    defaultTest "program-analysis/program_slicing_selection"
      & #onFrontend .~ (\cfg -> cfg
          & #selectPredicate .~ BOr
              (BIf . SelectDecl $ DeclNameMatches "FileOperationRecord")
              (BIf . SelectDecl $ DeclNameMatches "read_file_chunk")
          & #programSlicing .~ EnableProgramSlicing
          )
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name (SelectStatusInfo (Selected SelectionRoot)) ->
              Just $ Expected (name, "root")
            MatchSelect name (SelectStatusInfo (Selected TransitiveDependency)) ->
              Just $ Expected (name, "dependency")
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [(CDeclName, String)]
    declsWithMsgs = [
          ("struct FileOperationRecord" , "root")
        , ("read_file_chunk"            , "root")
        , ("enum FileOperationStatus"   , "dependency")
        ]

-- Check that program slicing generates bindings for uint32_t and uint64_t if we
-- only provide external binding specifications for uint64_t.
test_programAnalysis_program_slicing_simple :: TestCase
test_programAnalysis_program_slicing_simple =
    defaultTest "program-analysis/program_slicing_simple"
      & #onFrontend .~ (\cfg -> cfg
          & #selectPredicate .~ BIf (SelectHeader FromMainHeaders)
          & #programSlicing  .~ EnableProgramSlicing
          )
      & #specStdlib .~ BindingSpec.DisableStdlibBindingSpec
      & #specExternal .~ [ "examples/golden/program-analysis/program_slicing_simple.yaml" ]
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name (SelectStatusInfo (Selected SelectionRoot)) ->
              Just $ Expected (name, "root")
            MatchSelect name (SelectStatusInfo (Selected TransitiveDependency)) ->
              Just $ Expected (name, "dependency")
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [(CDeclName, String)]
    declsWithMsgs = [
          ("struct foo" , "root")
        , ("bar"        , "root")
        , ("uint32_t"   , "dependency")
        ]

{-------------------------------------------------------------------------------
  Selection
-------------------------------------------------------------------------------}

test_programAnalysis_selection_bad :: TestCase
test_programAnalysis_selection_bad =
    testTraceMulti "program-analysis/selection_bad" declsWithMsgs $ \case
      MatchSelect name MatchTransMissing{} ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    -- @f@ depends on user-defined @size_t@, which is not selected
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["f"]

test_programAnalysis_selection_fail :: TestCase
test_programAnalysis_selection_fail =
    testTraceMulti "program-analysis/selection_fail" declsWithMsgs $ \case
      MatchSelect name SelectParseFailure{} ->
        Just $ Expected name
      MatchSelect name (MatchTransMissing [MatchTransUnusable _unusable]) ->
        Just $ Expected name
      MatchSelect name (SelectStatusInfo (Selected SelectionRoot)) ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = [
          "struct Fail"
        , "struct Fail"
        , "struct DependOnFailByValue"
        , "struct DependOnFailByReference"
        , "struct OkBefore"
        , "struct OkAfter"
        ]

test_programAnalysis_selection_fail_variant_1 :: TestCase
test_programAnalysis_selection_fail_variant_1 =
    testVariant "program-analysis/selection_fail" "1.deselect_failed"
      & #onFrontend .~ ( #selectPredicate .~  BAnd
            (       BIf $ SelectHeader   FromMainHeaders)
            (BNot $ BIf $ SelectDecl   $ DeclNameMatches "struct Fail")
          )
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name (MatchTransMissing [MatchTransNotSelected]) ->
              Just $ Expected name
            MatchSelect name (SelectStatusInfo (Selected SelectionRoot)) ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = [
          "struct DependOnFailByValue"
        , "struct DependOnFailByReference"
        , "struct OkBefore"
        , "struct OkAfter"
        ]

test_programAnalysis_selection_fail_variant_2 :: TestCase
test_programAnalysis_selection_fail_variant_2 =
    testVariant "program-analysis/selection_fail" "2.program_slicing"
      & #onFrontend .~ (\cfg -> cfg
          & #selectPredicate .~ BAnd
              (       BIf $ SelectHeader   FromMainHeaders)
              (BNot $ BIf $ SelectDecl   $ DeclNameMatches "struct Fail")
          & #programSlicing .~ EnableProgramSlicing
          )
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
           MatchSelect name (MatchTransMissing [MatchTransUnusable _unusable]) ->
             Just $ Expected name
           MatchSelect name (SelectStatusInfo (Selected SelectionRoot)) ->
             Just $ Expected name
           _otherwise ->
             Nothing
         )
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = [
          "struct DependOnFailByValue"
        , "struct DependOnFailByReference"
        , "struct OkBefore"
        , "struct OkAfter"
        ]

test_programAnalysis_selection_fail_variant_3 :: TestCase
test_programAnalysis_selection_fail_variant_3 =
    testVariant "program-analysis/selection_fail" "3.select_ok"
      & #onFrontend .~ (\cfg -> cfg
          & #selectPredicate .~ BAnd
              (       BIf $ SelectDecl $ DeclNameMatches "struct OkBefore")
              (BNot $ BIf $ SelectDecl $ DeclNameMatches "struct Fail")
          & #programSlicing .~ EnableProgramSlicing
          )
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name (SelectStatusInfo (Selected SelectionRoot)) ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["struct OkBefore"]

test_programAnalysis_selection_foo :: TestCase
test_programAnalysis_selection_foo =
    testTraceMulti "program-analysis/selection_foo" declsWithMsgs $ \case
      MatchSelect name SelectParseFailure{} ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["f"]

test_programAnalysis_selection_matches_c_names_1 :: TestCase
test_programAnalysis_selection_matches_c_names_1 =
    testVariant "program-analysis/selection_matches_c_names" "1.positive_case"
      & #specPrescriptive .~ Just "examples/golden/program-analysis/selection_matches_c_names.yaml"
      & #onFrontend .~ ( #selectPredicate .~ predicate )
  where
    predicate :: Boolean SelectPredicate
    predicate =
            BOr
              (BIf (SelectDecl $ DeclNameMatches "FunctionWithAssignedHaskellNameByNameMangler"))
              (BIf (SelectDecl $ DeclNameMatches "struct StructWithAssignedHaskellNameByPrescriptiveBindingSpecs"))

test_programAnalysis_selection_matches_c_names_2 :: TestCase
test_programAnalysis_selection_matches_c_names_2 =
    testVariant "program-analysis/selection_matches_c_names" "2.negative_case"
      & #specPrescriptive .~ Just "examples/golden/program-analysis/selection_matches_c_names.yaml"
      & #onFrontend .~ ( #selectPredicate .~ predicate )
      & #tracePredicate .~ singleTracePredicate (\case
            MatchNoDeclarations ->
              Just $ Expected ()
            _otherwise ->
              Nothing
          )
  where
    predicate :: Boolean SelectPredicate
    predicate =
            BOr
              (BIf (SelectDecl $ DeclNameMatches "functionWithAssignedHaskellNameByNameMangler"))
              (BIf (SelectDecl $ DeclNameMatches "NewName"))

test_programAnalysis_selection_merge_traces :: TestCase
test_programAnalysis_selection_merge_traces =
    defaultTest "program-analysis/selection_merge_traces"
      & #onFrontend .~ ( #selectPredicate .~ predicate )
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name (MatchTransMissing [_, _]) -> Just $ Expected name
            _otherwise ->
              Nothing
          )
  where
    predicate :: Boolean SelectPredicate
    predicate =
            BAnd
              (BIf (SelectHeader FromMainHeaders)) $
            BAnd
              (BNot (BIf (SelectDecl $ DeclNameMatches "struct X")))
              (BNot (BIf (SelectDecl $ DeclNameMatches "struct Y")))

    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["dependsOnXAndY"]

test_programAnalysis_selection_omit_external_a :: TestCase
test_programAnalysis_selection_omit_external_a =
    defaultTest "program-analysis/selection_omit_external_a"
      & #specExternal .~ ["examples/golden/program-analysis/selection_omit_external.yaml"]
      & #onFrontend .~ ( #programSlicing .~ EnableProgramSlicing )
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchResolveBindingSpecs (ResolveBindingSpecsOmittedType declId) ->
              Just $ Expected declId.name
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["struct Omitted"]

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1361>
-- We should warn when we create bindings for a declaration omitted by an
-- /external/ binding specification.
test_programAnalysis_selection_omit_external_b :: TestCase
test_programAnalysis_selection_omit_external_b =
    defaultTest "program-analysis/selection_omit_external_b"
      & #specExternal .~ ["examples/golden/program-analysis/selection_omit_external.yaml"]
      & #onFrontend   .~ ( #programSlicing .~  EnableProgramSlicing )

test_programAnalysis_selection_omit_prescriptive :: TestCase
test_programAnalysis_selection_omit_prescriptive =
    defaultTest "program-analysis/selection_omit_prescriptive"
      & #specPrescriptive .~ Just "examples/golden/program-analysis/selection_omit_prescriptive.yaml"
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name (MatchTransMissing [MatchTransUnusable _unusable]) ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = [
          "struct DirectlyDependsOnOmitted"
        , "struct IndirectlyDependsOnOmitted"
        ]

test_programAnalysis_selection_squash :: TestCase
test_programAnalysis_selection_squash =
    defaultTest "program-analysis/selection_squash_typedef"
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name (MatchTransMissing [MatchTransNotSelected]) ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["typedef_to_struct_a"]

{-------------------------------------------------------------------------------
  Typedef analysis
-------------------------------------------------------------------------------}

test_programAnalysis_typedef_analysis :: TestCase
test_programAnalysis_typedef_analysis =
    testTraceMulti "program-analysis/typedef_analysis" declsWithMsgs $ \case
      MatchSelect name SelectMangleNamesSquashed{} ->
        Just $ Expected (name, Nothing)
      MatchMangle name (MangleNamesAssignedName new) ->
        Just $ Expected (name, Just new)
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [(CDeclName, Maybe Hs.Identifier)]
    declsWithMsgs = [
          ("struct struct1"  , Just "Struct1_t")
        , ("struct1_t"       , Nothing)
        , ("struct struct2"  , Just "Struct2_t")
        , ("struct2_t"       , Nothing)
        , ("struct struct3"  , Just "Struct3_t")
        , ("struct3_t"       , Nothing)
        , ("struct struct4"  , Just "Struct4_t")
        , ("struct4_t"       , Nothing)
        , ("struct struct6"  , Just "Struct6_Aux")
        , ("struct8"         , Nothing)
        , ("struct9"         , Nothing)
        , ("struct struct10" , Just "Struct10_t")
        , ("struct10_t"      , Nothing)
        , ("struct struct11" , Just "Struct11_t")
        , ("struct11_t"      , Nothing)
        , ("struct struct12" , Just "Struct12_t")
        , ("struct12_t"      , Nothing)
        ]
