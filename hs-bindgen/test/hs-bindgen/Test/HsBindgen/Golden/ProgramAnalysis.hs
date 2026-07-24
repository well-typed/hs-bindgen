-- | Golden tests: program analysis
module Test.HsBindgen.Golden.ProgramAnalysis (testCases) where

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config.ClangArgs
import HsBindgen.Config.Internal
import HsBindgen.Frontend.Analysis.DeclIndex (UnusableReason (..))
import HsBindgen.Frontend.Pass.MangleNames.Error
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
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
      -- * Circular programs
    , test_programAnalysis_circular_includes
    , test_programAnalysis_circular_macro
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
    , test_programAnalysis_typedef_block
    , test_programAnalysis_typedef_name_clash
    , test_programAnalysis_typedef_suffix_clash
    ]

{-------------------------------------------------------------------------------
  Delay traces
-------------------------------------------------------------------------------}

test_programAnalysis_delay_traces :: TestCase
test_programAnalysis_delay_traces =
    defaultTest "program-analysis/delay_traces"
      & #onFrontend .~ ( #selectionPredicate .~
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
    declsWithMsgs :: [C.DeclName]
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
  Circularities
-------------------------------------------------------------------------------}

test_programAnalysis_circular_includes :: TestCase
test_programAnalysis_circular_includes =
    defaultTest "program-analysis/circular_includes"

test_programAnalysis_circular_macro :: TestCase
test_programAnalysis_circular_macro =
    defaultTest "program-analysis/circular_macro"
      & #tracePredicate .~
          multiTracePredicateCustomLogLevel
            -- We want to check macro traces.
            (getCustomLogLevel [EnableMacroWarnings]) declsWithMsgs (\case
                MatchUnusable name UnusableMacroTypecheckFailure{} ->
                  Just $ Expected (name, "typecheck")
                MatchSelect name TransitiveDependenciesMissing{} ->
                  Just $ Expected (name, "transitive")
                _otherwise ->
                  Nothing
              )
  where
    declsWithMsgs :: [(C.DeclName, String)]
    declsWithMsgs = [
        ("macro A", "typecheck")
      , ("macro A", "transitive")
      , ("macro B", "typecheck")
      , ("macro B", "transitive")
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
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["foo"]

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
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["foo"]

test_programAnalysis_program_slicing_selection :: TestCase
test_programAnalysis_program_slicing_selection =
    defaultTest "program-analysis/program_slicing_selection"
      & #onFrontend .~ (\cfg -> cfg
          & #selectionPredicate .~ BOr
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
    declsWithMsgs :: [(C.DeclName, String)]
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
          & #selectionPredicate .~ BIf (SelectHeader FromMainHeaders)
          & #programSlicing  .~ EnableProgramSlicing
          )
      & #specStdlib .~ BindingSpec.DisableStdlibBindingSpec
      & #specExternal .~ [ "test-artefacts/headers/golden/program-analysis/program_slicing_simple.yaml" ]
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name (SelectStatusInfo (Selected SelectionRoot)) ->
              Just $ Expected (name, "root")
            MatchSelect name (SelectStatusInfo (Selected TransitiveDependency)) ->
              Just $ Expected (name, "dependency")
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [(C.DeclName, String)]
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
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["f"]

test_programAnalysis_selection_fail :: TestCase
test_programAnalysis_selection_fail =
    testTraceMulti "program-analysis/selection_fail" declsWithMsgs $ \case
      MatchUnusable name UnusableParseFailure{} ->
        Just $ Expected name
      MatchSelect name (MatchTransMissing [MatchTransUnusable _unusable]) ->
        Just $ Expected name
      MatchSelect name (SelectStatusInfo (Selected SelectionRoot)) ->
        Just $ Expected name
      MatchUnusable name (UnusableMangleNamesFailure{}) ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = [
          "struct Fail"
        , "struct Fail"
        , "struct DependOnFailByValue"
        , "struct DependOnFailByValue"
        , "struct DependOnFailByReference"
        , "struct DependOnFailByReference"
        , "struct OkBefore"
        , "struct OkAfter"
        ]

test_programAnalysis_selection_fail_variant_1 :: TestCase
test_programAnalysis_selection_fail_variant_1 =
    testVariant "program-analysis/selection_fail" (Just 1) "deselect_failed"
      & #onFrontend .~ ( #selectionPredicate .~  BAnd
            (       BIf $ SelectHeader   FromMainHeaders)
            (BNot $ BIf $ SelectDecl   $ DeclNameMatches "struct Fail")
          )
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name (MatchTransMissing [MatchTransNotSelected]) ->
              Just $ Expected name
            MatchSelect name (SelectStatusInfo (Selected SelectionRoot)) ->
              Just $ Expected name
            MatchUnusable name (UnusableMangleNamesFailure{}) ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = [
          "struct DependOnFailByValue"
        , "struct DependOnFailByValue"
        , "struct DependOnFailByReference"
        , "struct DependOnFailByReference"
        , "struct OkBefore"
        , "struct OkAfter"
        ]

test_programAnalysis_selection_fail_variant_2 :: TestCase
test_programAnalysis_selection_fail_variant_2 =
    testVariant "program-analysis/selection_fail" (Just 2) "program_slicing"
      & #onFrontend .~ (\cfg -> cfg
          & #selectionPredicate .~ BAnd
              (       BIf $ SelectHeader   FromMainHeaders)
              (BNot $ BIf $ SelectDecl   $ DeclNameMatches "struct Fail")
          & #programSlicing .~ EnableProgramSlicing
          )
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
           MatchSelect name (MatchTransMissing [MatchTransUnusable _unusable]) ->
             Just $ Expected name
           MatchSelect name (SelectStatusInfo (Selected SelectionRoot)) ->
             Just $ Expected name
           MatchUnusable name (UnusableMangleNamesFailure{}) ->
             Just $ Expected name
           _otherwise ->
             Nothing
         )
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = [
          "struct DependOnFailByValue"
        , "struct DependOnFailByValue"
        , "struct DependOnFailByReference"
        , "struct DependOnFailByReference"
        , "struct OkBefore"
        , "struct OkAfter"
        ]

test_programAnalysis_selection_fail_variant_3 :: TestCase
test_programAnalysis_selection_fail_variant_3 =
    testVariant "program-analysis/selection_fail" (Just 3) "select_ok"
      & #onFrontend .~ (\cfg -> cfg
          & #selectionPredicate .~ BAnd
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
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["struct OkBefore"]

test_programAnalysis_selection_foo :: TestCase
test_programAnalysis_selection_foo =
    testTraceMulti "program-analysis/selection_foo" declsWithMsgs $ \case
      MatchSelect _name (MatchTransMissing [_]) ->
        Just $ Expected ("macro A", "foo_t not selected")

      -- We get a transient name mangler failure here because @foo_t@ is
      -- unsupported, and so, it is not mangled.
      MatchUnusable _name UnusableMangleNamesFailure{} ->
        Just $ Expected ("macro A", "foo_t not mangled")
      MatchUnusable _name UnusableParseFailure{} ->
        Just $ Expected ("f", "unsupported underlying type")
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [(C.DeclName, String)]
    declsWithMsgs = [
        ("macro A", "foo_t not selected")
      , ("macro A", "foo_t not mangled")
      , ("f", "unsupported underlying type")
      ]

test_programAnalysis_selection_matches_c_names_1 :: TestCase
test_programAnalysis_selection_matches_c_names_1 =
    testVariant "program-analysis/selection_matches_c_names" (Just 1) "positive_case"
      & #specPrescriptive .~ Just "test-artefacts/headers/golden/program-analysis/selection_matches_c_names.yaml"
      & #onFrontend .~ ( #selectionPredicate .~ predicate )
  where
    predicate :: Boolean SelectionPredicate
    predicate =
            BOr
              (BIf (SelectDecl $ DeclNameMatches "FunctionWithAssignedHaskellNameByNameMangler"))
              (BIf (SelectDecl $ DeclNameMatches "struct StructWithAssignedHaskellNameByPrescriptiveBindingSpecs"))

test_programAnalysis_selection_matches_c_names_2 :: TestCase
test_programAnalysis_selection_matches_c_names_2 =
    testVariant "program-analysis/selection_matches_c_names" (Just 2) "negative_case"
      & #specPrescriptive .~ Just "test-artefacts/headers/golden/program-analysis/selection_matches_c_names.yaml"
      & #onFrontend .~ ( #selectionPredicate .~ predicate )
      & #tracePredicate .~ singleTracePredicate (\case
            MatchNoDeclarations ->
              Just $ Expected ()
            _otherwise ->
              Nothing
          )
  where
    predicate :: Boolean SelectionPredicate
    predicate =
            BOr
              (BIf (SelectDecl $ DeclNameMatches "functionWithAssignedHaskellNameByNameMangler"))
              (BIf (SelectDecl $ DeclNameMatches "NewName"))

test_programAnalysis_selection_merge_traces :: TestCase
test_programAnalysis_selection_merge_traces =
    defaultTest "program-analysis/selection_merge_traces"
      & #onFrontend .~ ( #selectionPredicate .~ predicate )
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name (MatchTransMissing [_, _]) -> Just $ Expected name
            _otherwise ->
              Nothing
          )
  where
    predicate :: Boolean SelectionPredicate
    predicate =
            BAnd
              (BIf (SelectHeader FromMainHeaders)) $
            BAnd
              (BNot (BIf (SelectDecl $ DeclNameMatches "struct X")))
              (BNot (BIf (SelectDecl $ DeclNameMatches "struct Y")))

    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["dependsOnXAndY"]

test_programAnalysis_selection_omit_external_a :: TestCase
test_programAnalysis_selection_omit_external_a =
    defaultTest "program-analysis/selection_omit_external_a"
      & #specExternal .~ ["test-artefacts/headers/golden/program-analysis/selection_omit_external.yaml"]
      & #onFrontend .~ ( #programSlicing .~ EnableProgramSlicing )
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchResolveBindingSpecs (ResolveBindingSpecsOmittedType declId) ->
              Just $ Expected declId.name
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["struct Omitted"]

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1361>
-- We should warn when we create bindings for a declaration omitted by an
-- /external/ binding specification.
test_programAnalysis_selection_omit_external_b :: TestCase
test_programAnalysis_selection_omit_external_b =
    defaultTest "program-analysis/selection_omit_external_b"
      & #specExternal .~ ["test-artefacts/headers/golden/program-analysis/selection_omit_external.yaml"]
      & #onFrontend   .~ ( #programSlicing .~  EnableProgramSlicing )

test_programAnalysis_selection_omit_prescriptive :: TestCase
test_programAnalysis_selection_omit_prescriptive =
    defaultTest "program-analysis/selection_omit_prescriptive"
      & #specPrescriptive .~ Just "test-artefacts/headers/golden/program-analysis/selection_omit_prescriptive.yaml"
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name (MatchTransMissing{}) ->
              Just $ Expected (name, "select")
            MatchUnusable name (UnusableMangleNamesFailure (MangleNamesResolutionError ResolveNamesUnderlyingDeclNotMangled{})) ->
              Just $ Expected (name, "mangle")
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [(C.DeclName, String)]
    declsWithMsgs = [
          ("struct DirectlyDependsOnOmitted",   "mangle")
        , ("struct DirectlyDependsOnOmitted",   "select")
        , ("struct IndirectlyDependsOnOmitted", "select")
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
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["typedef_to_struct_a"]

{-------------------------------------------------------------------------------
  Typedef analysis
-------------------------------------------------------------------------------}

-- | Locally resolvable typedef/tag clashes: squashing and suffixing.
--
-- See also 'test_programAnalysis_typedef_block' (the @-fblocks@ path) and
-- 'test_programAnalysis_typedef_name_clash' (clashes that are /not/ locally
-- resolvable and are reported as failures).
test_programAnalysis_typedef_analysis :: TestCase
test_programAnalysis_typedef_analysis =
    testTraceMulti "program-analysis/typedef_analysis" declsWithMsgs $ \case
      MatchSelect name SelectMangleNamesSquashed{} ->
        Just $ Expected (name, Nothing)
      MatchMangle name (MangleNamesAssignedName new) ->
        Just $ Expected (name, Just new.text)
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [(C.DeclName, Maybe Text)]
    declsWithMsgs = [
          ("struct struct1"  , Just "Struct1_t")
        , ("struct1_t"       , Nothing)
        , ("struct struct2"  , Just "Struct2_t")
        , ("struct2_t"       , Nothing)
        , ("struct struct3"  , Just "Struct3_t")
        , ("struct3_t"       , Nothing)
        , ("struct struct4"  , Just "Struct4_t")
        , ("struct4_t"       , Nothing)
        , ("struct struct6a" , Just "Struct6a_struct")
        , ("struct struct6b" , Just "Struct6b_struct")
        , ("struct8"         , Nothing)
        , ("struct9"         , Nothing)
        , ("struct struct10" , Just "Struct10_t")
        , ("struct10_t"      , Nothing)
        , ("struct struct11" , Just "Struct11_t")
        , ("struct11_t"      , Nothing)
        , ("struct struct12" , Just "Struct12_t")
        , ("struct12_t"      , Nothing)
          -- Tagged types referenced (and clashing by name) inside a
          -- function-pointer typedef: suffixed via the return type ('foo') and
          -- via an argument type ('bar').
        , ("struct foo"      , Just "Foo_struct")
        , ("struct bar"      , Just "Bar_struct")
          -- A 'const' qualifier is transparent: the tagged type underneath
          -- keeps its directness. Example 15 is a same-name direct alias and is
          -- squashed (like example 8); in example 16 the qualifier wraps a
          -- pointer, so the eponymous tag is reached through indirection and
          -- suffixed (like example 6a).
        , ("struct15"        , Nothing)
        , ("struct struct16" , Just "Struct16_struct")
        ]

-- | Exercise the 'C.TypeBlock' path in 'taggedPayloads'.
--
-- Kept separate from 'test_programAnalysis_typedef_analysis' because this
-- header requires @-fblocks@.
test_programAnalysis_typedef_block :: TestCase
test_programAnalysis_typedef_block =
    testTraceMulti "program-analysis/typedef_block" declsWithMsgs (\case
          MatchMangle name (MangleNamesAssignedName new) ->
            Just $ Expected (name, new.text)
          _otherwise ->
            Nothing
        )
      & #onBoot .~ ( #clangArgs % #enableBlocks .~ True )
  where
    declsWithMsgs :: [(C.DeclName, Text)]
    declsWithMsgs = [
          ("struct blk"    , "Blk_struct")
        , ("struct blkarg" , "Blkarg_struct")
        ]

-- | Clash cases that the typedef analysis deliberately does NOT resolve
-- locally; both colliding declarations are deselected via 'DetectClashesCollision'.
--
-- Complements 'test_programAnalysis_typedef_analysis', which covers the cases
-- that /are/ locally resolvable.
test_programAnalysis_typedef_name_clash :: TestCase
test_programAnalysis_typedef_name_clash =
    testTraceMulti "program-analysis/typedef_name_clash" declsWithMsgs $ \case
      MatchUnusable name (UnusableMangleNamesFailure (MangleNamesCollisionError DetectClashesCollision{})) ->
        Just $ Expected (name, "collision")
      MatchSelect name (TransitiveDependenciesMissing{}) ->
        Just $ Expected (name, "transitivity")
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [(C.DeclName, String)]
    declsWithMsgs = [
          ("struct a_unrelated" , "collision")
        , ("a_unrelated"        , "collision")
        , ("struct a_fun"       , "collision")
        , ("a_fun"              , "collision")
        , ("struct b_clash"     , "collision")
        , ("struct b_other"     , "collision")
        , ("b_clash"            , "transitivity")
        ]

-- | The deterministic disambiguation suffix (@_struct@\/@_union@\/@_enum@) is
-- never counter-disambiguated, so it can itself collide with an
-- independently-named declaration. When it does, the name-mangler's collision
-- check deselects /both/ colliding declarations (it does not invent a
-- @_struct2@); the eponymous typedef then loses its dropped dependency.
--
-- Complements 'test_programAnalysis_typedef_analysis' (the suffix in isolation)
-- and 'test_programAnalysis_typedef_name_clash' (clashes with no suffix
-- involved).
test_programAnalysis_typedef_suffix_clash :: TestCase
test_programAnalysis_typedef_suffix_clash =
    testTraceMulti "program-analysis/typedef_suffix_clash" declsWithMsgs $ \case
      MatchUnusable name (UnusableMangleNamesFailure (MangleNamesCollisionError DetectClashesCollision{})) ->
        Just $ Expected (name, "collision")
      MatchSelect name (TransitiveDependenciesMissing{}) ->
        Just $ Expected (name, "transitivity")
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [(C.DeclName, String)]
    declsWithMsgs = [
          -- Suffixed 'struct sfx' (Sfx_struct) vs real 'struct sfx_struct'.
          ("struct sfx"        , "collision")
        , ("struct sfx_struct" , "collision")
        , ("sfx"               , "transitivity")
          -- Suffixed 'union u' (U_union) vs real 'struct u_union'.
        , ("union u"           , "collision")
        , ("struct u_union"    , "collision")
        , ("u"                 , "transitivity")
        ]
