-- | Golden tests
module Test.HsBindgen.Golden (tests) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Test.Tasty

import Clang.Args
import Clang.Enum.Simple
import Clang.LowLevel.Core
import Clang.Version
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate (DeclPredicate (..),
                                     HeaderPathPredicate (..), Predicate (..))
import HsBindgen.TraceMsg

import Test.Common.HsBindgen.TracePredicate
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources

import Test.HsBindgen.Golden.Check.BindingSpec qualified as BindingSpec
import Test.HsBindgen.Golden.Check.C qualified as C
import Test.HsBindgen.Golden.Check.Exts qualified as Exts
import Test.HsBindgen.Golden.Check.FailingTrace qualified as FailingTrace
import Test.HsBindgen.Golden.Check.Hs qualified as Hs
import Test.HsBindgen.Golden.Check.PP qualified as PP
import Test.HsBindgen.Golden.Check.Rust qualified as Rust
import Test.HsBindgen.Golden.Check.TH qualified as TH

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

  -- If there's unnamed structures libclang is going to parse the 'commentCName'
  -- as something like "struct unnamed at /absolute/path/to/file:line" this is
  -- troublesome because the fixture tests are not going to be run in the same
  -- machine that generated them. So we disable the Hs and PP tests for this
  -- header file.
  --
  -- See issue: #966
  --
  | testName test == "doxygen_docs"
  = testGroup (testName test) [
        C.check           testResources test
      -- , Hs.check          testResources test
      , Exts.check        testResources test
      , TH.check          testResources test
      -- , PP.check          testResources test
      , BindingSpec.check testResources test
      , Rust.check        testResources test
      ]

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
    , defaultTest "distilled_lib_1"
    , defaultTest "enums"
    , defaultTest "enum_cpp_syntax"
    , defaultTest "fixedwidth"
    , defaultTest "flam"
    , defaultTest "forward_declaration"
    , defaultTest "headers"
    , defaultTest "macro_functions"
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
    , defaultTest "type_naturals"
    , defaultTest "typedef_vs_macro"
    , defaultTest "typedefs"
    , defaultTest "typenames"
    , defaultTest "unions"
    , defaultTest "uses_utf8"
    , defaultTest "vector"

      --
      -- Clang diagnostics
      --

    , testDiagnostic "attributes" $ \diag ->
        diagnosticCategoryText diag == "Nullability Issue"
    , testDiagnostic "unnamed-struct" $ \diag ->
        diagnosticCategoryText diag == "Semantic Issue"

      --
      -- Tests that require a trace predicate
      --

    , testTraceCustom "decls_in_signature" ["f3", "f4", "f5"] $ \case
        TraceFrontend (FrontendParse (ParseUnexpectedAnonInSignature info)) ->
          Just . Expected $ C.declId info
        TraceFrontend (FrontendClang (ClangDiagnostic _diag)) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , testTraceCustom "definitions" ["foo", "n"] $ \case
        TraceFrontend (FrontendParse (ParsePotentialDuplicateSymbol info _isPublic)) ->
          Just $ Expected (C.declId info)
        _otherwise ->
          Nothing
    , let declsWithMsgs = [
              -- Duplicate symbols
              "bar1", "bar2", "bar3", "bar4"
            , "baz1", "baz2", "baz3"
            , "foo1", "foo2", "foo3"
            , "quux"
            , "wam"
            ]
      in testTraceCustom "macro_in_fundecl" declsWithMsgs $ \case
        TraceFrontend (FrontendParse (ParsePotentialDuplicateSymbol info _isPublic)) ->
          Just $ Expected (C.declId info)
        _otherwise ->
          Nothing
    , testTraceCustom "macro_in_fundecl_vs_typedef" ["quux1", "quux2", "wam1", "wam2"] $ \case
        TraceFrontend (FrontendParse (ParsePotentialDuplicateSymbol info _isPublic)) ->
          Just $ Expected (C.declId info)
        _otherwise ->
          Nothing
    , testTraceCustom "redeclaration" ["x", "n"] $ \case
        TraceFrontend (FrontendParse (ParsePotentialDuplicateSymbol info _isPublic)) ->
          Just $ Expected (C.declId info)
        TraceFrontend (FrontendParse (ParseUnknownStorageClass info (unsafeFromSimpleEnum -> CX_SC_Static))) ->
          Just $ Expected (C.declId info)
        _otherwise ->
          Nothing
    , testTraceCustom "skip_over_long_double" ["fun1", "struct1"] $ \case
        TraceFrontend (FrontendParse (ParseUnsupportedType info UnsupportedLongDouble)) ->
          Just $ Expected $ C.declId info
        _otherwise ->
          Nothing
    , testTraceCustom "tentative_definitions" ["i1", "i2", "i3", "i3"] $ \case
        TraceFrontend (FrontendParse (ParsePotentialDuplicateSymbol info _isPublic)) ->
          Just $ Expected (C.declId info)
        TraceFrontend (FrontendParse (ParseUnknownStorageClass info (unsafeFromSimpleEnum -> CX_SC_Static))) ->
          Just $ Expected (C.declId info)
        TraceFrontend (FrontendClang (ClangDiagnostic Diagnostic {diagnosticOption = Just "-Wno-extern-initializer"})) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , let declsWithMsgs :: [Labelled C.Name]
          declsWithMsgs = [
                Labelled "Renamed"  "struct1"
              , Labelled "Squashed" "struct1_t"
              , Labelled "Renamed"  "struct2"
              , Labelled "Squashed" "struct2_t"
              , Labelled "Renamed"  "struct3"
              , Labelled "Squashed" "struct3_t"
              , Labelled "Renamed"  "struct4"
              , Labelled "Squashed" "struct4_t"
              , Labelled "Renamed"  "struct6"
              , Labelled "Squashed" "struct8"
              , Labelled "Squashed" "struct9"
              , Labelled "Renamed"  "struct10"
              , Labelled "Squashed" "struct10_t"
              , Labelled "Renamed"  "struct11"
              , Labelled "Squashed" "struct11_t"
              , Labelled "Renamed"  "struct12"
              , Labelled "Squashed" "struct12_t"
              ]
      in testTraceCustom "typedef_analysis" declsWithMsgs $ \case
        TraceFrontend (FrontendHandleTypedefs (HandleTypedefsSquashed info)) ->
          Just $ Expected $ Labelled "Squashed" $ C.declIdName (C.declId info)
        TraceFrontend (FrontendHandleTypedefs (HandleTypedefsRenamedTagged info _to)) ->
          Just $ Expected $ Labelled "Renamed"  $ C.declIdName (C.declId info)
        _otherwise ->
          Nothing
    , testTraceCustom "varargs" ["f", "g"] $ \case
        TraceFrontend (FrontendParse (ParseUnsupportedType info UnsupportedVariadicFunction)) ->
          Just $ Expected $ C.declId info
        _otherwise ->
          Nothing
    , let declsWithWarnings = [
              -- *** Functions ***
              -- Problematic non-public visibility
              "f2" , "f3" , "f4"
            , "f12", "f13", "f14"

              -- Duplicate symbols
            , "f5" , "f6" , "f7" , "f8" , "f9"
            , "f15", "f16", "f17", "f18", "f19"

              -- *** Global variables ***
              -- Problematic non-public visibility
            , "i12", "i13", "i14"
              -- Duplicate symbols
            , "i0" , "i1" , "i2" , "i3" , "i4"
            , "i5" , "i6" , "i7" , "i8" , "i9"
            , "i15", "i16", "i17", "i18", "i19"
              -- Unsupported storage class
            , "i20", "i21", "i22", "i23", "i24"
            , "i25", "i26", "i27", "i28", "i29"
            ]
      in (defaultTest "visibility_attributes") {
          testTracePredicate = customTracePredicate' declsWithWarnings $ \case
            TraceFrontend (FrontendParse (ParsePotentialDuplicateSymbol info _isPublic)) ->
              Just $ Expected (C.declId info)
            TraceFrontend (FrontendParse (ParseNonPublicVisibility info)) ->
              Just $ Expected (C.declId info)
            TraceFrontend (FrontendParse (ParseUnknownStorageClass info (unsafeFromSimpleEnum -> CX_SC_Static))) ->
              Just $ Expected (C.declId info)
            TraceFrontend (FrontendClang (ClangDiagnostic Diagnostic {diagnosticOption = Just "-Wno-extern-initializer"})) ->
              Just Tolerated
            _otherwise ->
              Nothing
        }

      --
      -- Failing tests
      --

    , failingTestSimple "long_double" $ \case
        TraceFrontend (FrontendParse (ParseUnsupportedType _ UnsupportedLongDouble)) ->
          Just $ Expected ()
        _otherwise ->
          Nothing
    , failingTestSimple "implicit_fields_struct" $ \case
        TraceFrontend (FrontendParse (ParseUnsupportedImplicitFields {})) ->
          Just $ Expected ()
        _otherwise ->
          Nothing
    , failingTestSimple "declaration_unselected_b" $ \case
        TraceFrontend (FrontendMangleNames (MangleNamesMissingDeclaration {})) ->
          Just $ Expected ()
        _otherwise ->
          Nothing
    , failingTestSimple "redeclaration_different" $ \case
        TraceFrontend (FrontendSort (SortErrorDeclIndex (Redeclaration {}))) ->
          Just (Expected ())
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "macro redefined" `Text.isInfixOf` diagnosticSpelling x
            then Just Tolerated
            else Nothing
        _otherwise ->
          Nothing
    , failingTestCustom "tentative_definitions_linkage" [(), ()] $ \case
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "non-static declaration of" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        _otherwise ->
          Nothing
    , failingTestSimple "unsupported_builtin" $ \case
        TraceFrontend (FrontendParse (ParseUnsupportedType _info (UnsupportedBuiltin "__builtin_va_list"))) ->
          Just $ Expected ()
        _otherwise ->
          Nothing

      -- Arrays
    , failingTestSimple "array_res_1" $ \case
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "function cannot return array type" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        TraceFrontend (FrontendClang _) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , failingTestSimple "array_res_2" $ \case
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "function cannot return array type" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        TraceFrontend (FrontendClang _) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , failingTestSimple "array_res_3" $ \case
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "function cannot return array type" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        TraceFrontend (FrontendClang _) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , failingTestSimple "array_res_4" $ \case
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "function cannot return array type" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        TraceFrontend (FrontendClang _) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , failingTestSimple "array_res_5" $ \case
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "function cannot return array type" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        TraceFrontend (FrontendClang _) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , failingTestSimple "array_res_6" $ \case
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "function cannot return array type" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        TraceFrontend (FrontendClang _) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , failingTestSimple "array_res_7" $ \case
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "function cannot return array type" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        TraceFrontend (FrontendClang _) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , failingTestSimple "array_res_8" $ \case
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "function cannot return array type" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        TraceFrontend (FrontendClang _) ->
          Just Tolerated
        _otherwise ->
          Nothing

      --
      -- Miscellaneous other tests that require special treatment
      --

    , let declsWithWarnings = [
              -- Duplicate symbols
              "arr0"
            , "arr3"
            , "arr1"
            , "arr6"
              -- Unkown storage class: static non-const
            , "arr4"
            , "arr5"
            , "arr8"
            ]
      in (defaultTest "array") {
          testClangVersion = Just (>= (19, 0, 0))
        , testTracePredicate = customTracePredicate' declsWithWarnings $ \case
            TraceFrontend (FrontendParse (ParsePotentialDuplicateSymbol info _isPublic)) ->
               Just $ Expected (C.declId info)
            TraceFrontend (FrontendClang (ClangDiagnostic Diagnostic {diagnosticOption = Just "-Wno-extern-initializer"})) ->
               Just Tolerated
            TraceFrontend (FrontendClang (ClangDiagnostic Diagnostic {diagnosticOption = Just "-Wno-tentative-definition-array"})) ->
               Just Tolerated
            TraceFrontend (FrontendParse (ParseUnknownStorageClass info (unsafeFromSimpleEnum -> CX_SC_Static))) ->
               Just $ Expected (C.declId info)
            _otherwise ->
               Nothing
        }
    , (defaultTest "asm") {
          testOnFrontendConfig = \cfg -> cfg {
              frontendClangArgs = (frontendClangArgs cfg) {
                  clangEnableGnu = True
                }
            }
        }
    , (defaultTest "doxygen_docs") {
          testClangVersion = Just (>= (19, 0, 0))
        }
    , (defaultTest "bool_c23") {
          testClangVersion = Just (>= (15, 0, 0))
        }
    , (defaultTest "fun_attributes") {
          testClangVersion = Just (>= (15, 0, 0))
        , testTracePredicate = customTracePredicate' ["my_printf", "i", "f3"] $ \case
             TraceFrontend (FrontendParse (ParseUnsupportedType info UnsupportedVariadicFunction)) ->
               Just $ Expected (C.declId info)
             TraceFrontend (FrontendParse (ParseNonPublicVisibility info)) ->
               Just $ Expected (C.declId info)
             TraceFrontend (FrontendParse (ParsePotentialDuplicateSymbol info _isPublic)) ->
               Just $ Expected (C.declId info)
             _otherwise ->
               Nothing
        , testRustBindgen = RustBindgenFail
        }
    , (defaultTest "fun_attributes_conflict") {
          testTracePredicate = customTracePredicate [] $ \case
             TraceFrontend (FrontendClang (ClangDiagnostic Diagnostic {diagnosticOption = Just "-Wno-ignored-attributes"})) ->
               Just Tolerated
             _otherwise ->
               Nothing
        }
    , let declsWithWarnings :: [C.PrelimDeclId]
          declsWithWarnings = [
                -- non-extern non-static globals
                "nesInteger"
              , "nesFloating"
              , "nesString1"
              , "nesString2"
              , "nesCharacter"
              , "nesParen"
              , "nesUnary"
              , "nesBinary"
              , "nesConditional"
              , "nesCast"
              , "nesCompound"
              , "nesInitList"
              , "nesBool"
              , "streamBinary"
              , "streamBinary_len"
              , "some_global_struct"
                -- Other warnings
              , "unusableAnon"
                -- Duplicate symbols
              , "classless"
              ]
      in (defaultTest "globals") {
          -- Getting different output from (the same version of) rust-bindgen
          -- for this test on CI than locally. Unsure why, compiled against
          -- different llvm version? For now we just disable it.
          testRustBindgen    = RustBindgenIgnore
        , testTracePredicate = customTracePredicate' declsWithWarnings $ \case
            TraceFrontend (FrontendParse (ParsePotentialDuplicateSymbol info _isPublic)) ->
              Just $ Expected (C.declId info)
            TraceFrontend (FrontendParse (ParseUnexpectedAnonInExtern info)) ->
              Just $ Expected (C.declId info)
            _otherwise ->
              Nothing
        }
    , (defaultTest "iterator") {
          testClangVersion     = Just (>= (15, 0, 0))
        , testOnFrontendConfig = \cfg -> cfg{
              frontendClangArgs = (frontendClangArgs cfg) {
                  clangEnableBlocks = True
                }
            }
        }
    , (defaultTest "macro_strings") {
          testRustBindgen = RustBindgenFail
        }
    , (defaultTest "named_vs_anon"){
          testClangVersion = Just (>= (19, 1, 0))
        }
    , (defaultTest "program_slicing_simple"){
          -- Check that program slicing generates bindings for uint32_t and
          -- uint64_t if we only provide external binding specifications for
          -- uint64_t.
          testOnFrontendConfig = \cfg -> cfg{
              frontendParsePredicate  = PTrue
            , frontendSelectPredicate = PIf (Left FromMainHeaders)
            , frontendProgramSlicing  = EnableProgramSlicing
            }
        , testStdlibSpec = BindingSpec.DisableStdlibBindingSpec
        , testExtBindingSpecs = [ "examples/golden/program_slicing_simple.yaml" ]
        , testTracePredicate = customTracePredicate [
              "selected foo"
            , "selected uint32_t"
            ] $ \case
            TraceFrontend (FrontendSelect (SelectSelected info)) ->
              expectSelected info $ Set.fromList [
                  "foo"
                , "uint32_t"
                ]
            TraceFrontend (FrontendSelect (SelectExcluded _)) -> Just Tolerated
            _otherwise ->
              Nothing
        }
    , (defaultTest "program_slicing_selection"){
          testOnFrontendConfig = \cfg -> cfg{
              frontendParsePredicate  = PTrue
            , frontendSelectPredicate = POr
                (PIf . Right $ DeclNameMatches "FileOperationRecord")
                (PIf . Right $ DeclNameMatches "read_file_chunk")
            , frontendProgramSlicing  = EnableProgramSlicing
            }
        , testTracePredicate = customTracePredicate [
              "selected FileOperationRecord"
            , "selected FileOperationStatus"
            , "selected read_file_chunk"
              -- Macro redefines a global variable
            , "NsPrelimDeclIdNamed \"stdin\" TypeNamespaceOrdinary"
            , "NsPrelimDeclIdNamed \"stdout\" TypeNamespaceOrdinary"
            , "NsPrelimDeclIdNamed \"stderr\" TypeNamespaceOrdinary"
            ] $ \case
            TraceFrontend (FrontendParse msg) -> case msg of
              -- TODO: Ideally, we do not see this warnings because they affect
              -- skipped declarations. See
              -- https://github.com/well-typed/hs-bindgen/issues/905.
              ParseUnsupportedType _ UnsupportedLongDouble       -> Just Tolerated
              ParseUnsupportedType _ UnsupportedVariadicFunction -> Just Tolerated
              ParseUnsupportedType _ (UnsupportedBuiltin _)      -> Just Tolerated
              _other                                             -> Nothing
            TraceFrontend (FrontendSelect (SelectSelected info)) ->
              expectSelected info $ Set.fromList [
                  "FileOperationRecord"
                , "FileOperationStatus"
                , "read_file_chunk"
                ]
            TraceFrontend (FrontendSelect (SelectExcluded _)) -> Just Tolerated
            TraceFrontend (FrontendSort (SortErrorDeclIndex (Redeclaration {redeclarationId = x}))) ->
              Just $ Expected (show x)
            _otherwise ->
              Nothing
          -- TODO: Also, we may want to specify an allow list; see
          -- https://github.com/well-typed/hs-bindgen/issues/907.
        , testRustBindgen = RustBindgenRun
        }
    , (defaultFailingTest "thread_local"){
          testClangVersion   = Just (>= (16, 0, 0))
        , testTracePredicate = singleTracePredicate $ \case
            TraceFrontend (FrontendParse (ParseUnsupportedTLS{})) ->
              Just $ Expected ()
            _otherwise ->
              Nothing
        }
    , (defaultTest "type_attributes") {
          testRustBindgen = RustBindgenFail
        }
    ]
  where
    expectSelected ::
         C.DeclInfo Select
      -> Set C.Name
      -> Maybe (TraceExpectation String)
    expectSelected info expectedNames = case C.declIdName (C.declId info) of
      name
        | Set.member name expectedNames ->
            Just . Expected $ "selected " ++ Text.unpack (C.getName name)
        | otherwise -> Just Unexpected
