-- | Golden tests
module Test.HsBindgen.Golden (tests) where

import Data.List qualified as List
import Data.Map qualified as Map
import Data.Text qualified as Text
import Test.Tasty

import Clang.Version
import HsBindgen.BindingSpec
import HsBindgen.BindingSpec.Internal qualified as BindingSpec
import HsBindgen.C.Predicate (Predicate (..))
import HsBindgen.Config
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass.Slice.IsPass (ProgramSlicing (..))
import HsBindgen.Language.C.Name
import HsBindgen.Pipeline qualified as Pipeline
import HsBindgen.TraceMsg

import Test.Common.HsBindgen.TracePredicate
import Test.HsBindgen.Resources
import Test.HsBindgen.Golden.TestCase

import Test.HsBindgen.Golden.Check.BindingSpec  qualified as BindingSpec
import Test.HsBindgen.Golden.Check.C            qualified as C
import Test.HsBindgen.Golden.Check.Exts         qualified as Exts
import Test.HsBindgen.Golden.Check.FailingTrace qualified as FailingTrace
import Test.HsBindgen.Golden.Check.Hs           qualified as Hs
import Test.HsBindgen.Golden.Check.PP           qualified as PP
import Test.HsBindgen.Golden.Check.Rust         qualified as Rust
import Test.HsBindgen.Golden.Check.TH           qualified as TH

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests testResources =
    testGroup "Test.HsBindgen.Golden" $
      map (testTreeFor testResources) testCases

testTreeFor :: IO TestResources -> TestCase -> TestTree
testTreeFor testResources test@TestCase{testHasOutput, testClangVersion}
  | Just versionPred <- testClangVersion
  , case clangVersion of
      ClangVersion version  -> not (versionPred version)
      ClangVersionUnknown _ -> True
  = testGroup (testName test) []

  | not testHasOutput
  = FailingTrace.check testResources test

  | otherwise
  = testGroup (testName test) [
        C.check           testResources test
      , Hs.check          testResources test
      , Exts.check        testResources test
      , TH.check          testResources test
      , PP.check          testResources test
      , BindingSpec.check testResources test
      , Rust.check        testResources test
      ]

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

testCases :: [TestCase]
testCases = [
      --
      -- Standard test cases
      --

      defaultTest "adios"
    , defaultTest "anonymous"
    , defaultTest "bitfields"
    , defaultTest "bool"
    , defaultTest "enums"
    , defaultTest "fixedarray"
    , defaultTest "fixedwidth"
    , defaultTest "flam"
    , defaultTest "forward_declaration"
    , defaultTest "headers"
    , defaultTest "macro_functions"
    , defaultTest "macro_in_fundecl_vs_typedef"
    , defaultTest "macro_in_fundecl"
    , defaultTest "macro_typedef_scope"
    , defaultTest "macro_typedef_struct"
    , defaultTest "macro_types"
    , defaultTest "macros"
    , defaultTest "names"
    , defaultTest "nested_enums"
    , defaultTest "nested_types"
    , defaultTest "nested_unions"
    , defaultTest "opaque_declaration"
    , defaultTest "primitive_types"
    , defaultTest "recursive_struct"
    , defaultTest "redeclaration_identical"
    , defaultTest "simple_func"
    , defaultTest "simple_structs"
    , defaultTest "spec_examples"
    , defaultTest "struct_arg"
    , defaultTest "fixedarray_arg"
    , defaultTest "type_naturals"
    , defaultTest "typedef_vs_macro"
    , defaultTest "typedefs"
    , defaultTest "typenames"
    , defaultTest "unions"
    , defaultTest "vector"
    , defaultTest "uses_utf8"

      --
      -- Clang diagnostics
      --

    , testDiagnostic "attributes" $ \diag ->
        diagnosticCategoryText diag == "Nullability Issue"
    , testDiagnostic "unnamed-struct" $ \diag ->
        diagnosticCategoryText diag == "Semantic Issue"

      --
      -- Tets that require a trace predicate
      --

    , testTraceCustom "decls_in_signature" ["f1", "f2", "f3", "f4", "f5"] $ \case
        TraceFrontend (FrontendParse (UnexpectedAnonInSignature info)) ->
          Just . Expected $ C.declId info
        TraceClang (ClangDiagnostic _diag) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , testTraceCustom "distilled_lib_1" (replicate 2 ()) $ \case
        TraceFrontend (FrontendHandleMacros (MacroErrorTc (TcErrors _))) ->
          Just $ Expected ()
        _otherwise ->
          Nothing
    , testTraceCustom "skip_over_long_double" ["fun1", "struct1"] $ \case
        TraceFrontend (FrontendParse (UnsupportedType info UnsupportedLongDouble)) ->
          Just $ Expected $ C.declId info
        _otherwise ->
          Nothing
    , testTraceSimple "varargs" $ \case
        TraceFrontend (FrontendParse (UnsupportedType _ UnsupportedVariadicFunction)) ->
          Just $ Expected ()
        _otherwise ->
          Nothing

      --
      -- Failing tests
      --

    , failingTestSimple "long_double" $ \case
        TraceFrontend (FrontendParse (UnsupportedType _ UnsupportedLongDouble)) ->
          Just $ Expected ()
        _otherwise ->
          Nothing
    , failingTestSimple "implicit_fields_struct" $ \case
        TraceFrontend (FrontendParse (UnsupportedImplicitFields {})) ->
          Just $ Expected ()
        _otherwise ->
          Nothing
    , failingTestSimple "declaration_unselected_b" $ \case
        TraceFrontend (FrontendMangleNames (MissingDeclaration {})) ->
          Just $ Expected ()
        _otherwise ->
          Nothing
    , failingTestSimple "redeclaration_different" $ \case
        TraceFrontend (FrontendSort (SortErrorDeclIndex (Redeclaration {}))) ->
          Just (Expected ())
        TraceClang (ClangDiagnostic x) ->
          if "macro redefined" `Text.isInfixOf` diagnosticSpelling x
            then Just Tolerated
            else Nothing
        _otherwise ->
          Nothing
    , failingTestSimple "fixedarray_res_a" $ \case
         TraceClang (ClangDiagnostic x) ->
           if "brackets are not allowed here" `Text.isInfixOf` diagnosticSpelling x
             then Just (Expected ())
             else Just Tolerated
         _otherwise ->
           Nothing
    , failingTestSimple "fixedarray_res_b" $ \case
        TraceClang (ClangDiagnostic x) ->
          if "function cannot return array type" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        TraceClang _ ->
          Just Tolerated
        _otherwise ->
          Nothing

      --
      -- Miscellaneous other tests that require special treatment
      --

      -- Tests for which rust-bindgen fails (but we don't)
    , (defaultTest "macro_strings")   {testRustBindgenFails = True}
    , (defaultTest "type_attributes") {testRustBindgenFails = True}

      -- We can only properly detect /all/ anonymous structs with clang 19
    , (defaultTest "named_vs_anon"){
          testClangVersion   = Just $ (>= (19, 1, 0))
        , testTracePredicate = customTracePredicate [] $ \case
            TraceFrontend (FrontendHandleMacros _) ->
              Just Tolerated
            _otherwise ->
              Nothing
        }

      -- Check that program slicing generates bindings for uint32_t if we
      -- remove it from the standard external binding specification
    , (defaultTest "program_slicing"){
          testOnConfig = \cfg -> cfg{
              configPredicate      = SelectAll
            , configProgramSlicing = EnableProgramSlicing
            }
        , testOnExtSpec = \extSpec ->
            let uInt32T = QualName {
                    qualNameName = "uint32_t"
                  , qualNameKind = NameKindOrdinary
                  }
            in Pipeline.BindingSpec {
                bindingSpecUnresolved =
                    BindingSpec.BindingSpec
                  . Map.delete uInt32T
                  . BindingSpec.bindingSpecTypes
                  . Pipeline.bindingSpecUnresolved
                  $ extSpec
              , bindingSpecResolved =
                    BindingSpec.BindingSpec
                  . Map.delete uInt32T
                  . BindingSpec.bindingSpecTypes
                  . Pipeline.bindingSpecResolved
                  $ extSpec
              }
        , testTracePredicate = singleTracePredicate $ \case
            TraceFrontend (FrontendHandleMacros (MacroErrorReparse err)) ->
              if "Unexpected primitive type" `List.isInfixOf` reparseError err
                then Just $ Expected ()
                else Nothing
            _otherwise ->
              Nothing
        }
    ]
