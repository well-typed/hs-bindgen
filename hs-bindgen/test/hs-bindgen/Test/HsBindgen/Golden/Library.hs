-- | Golden tests: library mode (@--library@)
--
-- The headers are in @test-artefacts/headers/library@ and the golden
-- directories in @test-artefacts/fixtures/library@, one directory per topic.
module Test.HsBindgen.Golden.Library (testCases) where

import Data.Text qualified as Text
import System.FilePath ((</>))

import HsBindgen.Config.Internal
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Library (PlanError (..), UnitHeader (..))
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer (Level)

import Test.Common.HsBindgen.Trace.Patterns
import Test.Common.HsBindgen.Trace.Predicate
import Test.HsBindgen.Golden.Infra.LibraryTest
import Test.HsBindgen.Golden.Infra.TestCase

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

testCases :: [LibraryTest]
testCases = [
      -- * Walkthrough
      test_walkthrough_follows_includes
    , test_walkthrough_module_name_collision
    , test_walkthrough_uses_without_including
    , test_walkthrough_import_cycle
      -- * Layout
    , test_layout_all_output_kinds
    , test_layout_module_names
      -- * Import cycles
    , test_cycles_boot_files
    , test_cycles_include_cycle
    , test_cycles_vendor_cycle
      -- * Typedefs
    , test_typedefs_squashed_across_headers
    , test_typedefs_squashed_into_unusable
    , test_typedefs_clashes_with_unusable
      -- * Callbacks
    , test_callbacks_typedef_and_raw_callbacks
    , test_callbacks_typedef_and_raw_callbacks_hs_boot
      -- * Selection
    , test_selection_vendor_dependencies
    , test_selection_vendor_dependencies_slicing
    , test_selection_vendor_dependencies_select_header
    , test_selection_vendor_dependencies_select_vendor_header
      -- * Binding specifications
    , test_binding_specs_generated_specs
    , test_binding_specs_external_specs
    , test_binding_specs_external_callbacks
      -- * Documentation
    , test_documentation_doxygen_in_library_headers
      -- * Plan errors
    , test_errors_module_name_collision
    , test_errors_output_path_collision
    , test_errors_vendor_module_collision
    , test_errors_no_headers_found
    ]

{-------------------------------------------------------------------------------
  Walkthrough
-------------------------------------------------------------------------------}

-- | A step of the library that the library mode documentation builds up
--
-- Every step reads the same library directory, @walkthrough/geometry@, with
-- the base module @Geometry@. The root header of a step decides which headers
-- of the library the run reaches.
walkthroughStep :: String -> LibraryTest
walkthroughStep name =
    libraryTest "walkthrough" name
      & #libraryDirs         .~ [headersDir </> "geometry"]
      & #test % #onBoot      .~ ( #baseModule .~ geometry )
  where
    headersDir :: FilePath
    headersDir = "test-artefacts" </> "headers" </> "library" </> "walkthrough"

    geometry :: BaseModuleName
    geometry = BaseModuleName (Text.pack "Geometry")

-- | Every header includes the headers whose declarations it uses
test_walkthrough_follows_includes :: LibraryTest
test_walkthrough_follows_includes =
    walkthroughStep "follows_includes"
      & #bindingSpecDir         .~ Just "bindingspecs"
      & #test % #tracePredicate .~ expectLibraryTraces []

-- | @shape.h@ and @_shape.h@ get the same module name
test_walkthrough_module_name_collision :: LibraryTest
test_walkthrough_module_name_collision =
    walkthroughStep "module_name_collision"
      & #outcome .~ RejectedByPlan (\case
            ModuleNameCollision{} -> True
            _otherwise            -> False
          )

-- | @canvas.h@ uses @struct shape@ without including @shape.h@
test_walkthrough_uses_without_including :: LibraryTest
test_walkthrough_uses_without_including =
    walkthroughStep "uses_without_including"
      & #test % #tracePredicate .~ expectLibraryTraces []

-- | The types of @layer.h@ and @scene.h@ refer to each other
test_walkthrough_import_cycle :: LibraryTest
test_walkthrough_import_cycle =
    walkthroughStep "import_cycle"
      & #test % #tracePredicate .~ expectLibraryTraces [bootFiles]

{-------------------------------------------------------------------------------
  Layout
-------------------------------------------------------------------------------}

-- | Every kind of file a library run writes
test_layout_all_output_kinds :: LibraryTest
test_layout_all_output_kinds =
    libraryTest "layout" "all_output_kinds"
      & #bindingSpecDir      .~ Just "bindingspecs"
      & #test % #tracePredicate .~ expectLibraryTraces [bootFiles]

-- | Module names from header paths, and one Haskell name in two modules
test_layout_module_names :: LibraryTest
test_layout_module_names =
    libraryTest "layout" "module_names"
      & #test % #tracePredicate .~ expectLibraryTraces []

{-------------------------------------------------------------------------------
  Import cycles
-------------------------------------------------------------------------------}

-- | Boot files and @SOURCE@ imports, with macros and an @FFIType@ that reduces
-- through the other member of the cycle
test_cycles_boot_files :: LibraryTest
test_cycles_boot_files =
    libraryTest "cycles" "boot_files"
      & #test % #tracePredicate .~ expectLibraryTraces [bootFiles]

-- | Headers that include each other without an import cycle
test_cycles_include_cycle :: LibraryTest
test_cycles_include_cycle =
    libraryTest "cycles" "include_cycle"
      & #test % #tracePredicate .~ expectLibraryTraces []

-- | The vendor module and the module of a library header refer to each other
--
-- The vendor header @vendor/host.h@ declares @struct lib_ctx@ forward, and the
-- library header @ctx.h@ defines it. The vendor module gets a boot file, and a
-- binding specification like any other unit.
test_cycles_vendor_cycle :: LibraryTest
test_cycles_vendor_cycle =
    libraryTest "cycles" "vendor_cycle"
      & #bindingSpecDir         .~ Just "bindingspecs"
      & #test % #onFrontend     .~ ( #programSlicing .~ EnableProgramSlicing )
      & #test % #tracePredicate .~ expectLibraryTraces [
            bootFiles
          , vendorModule "VendorCycle.Vendor" 1
          ]

{-------------------------------------------------------------------------------
  Typedefs
-------------------------------------------------------------------------------}

-- | Typedefs squashed into structs of other headers
test_typedefs_squashed_across_headers :: LibraryTest
test_typedefs_squashed_across_headers =
    libraryTest "typedefs" "squashed_across_headers"
      & #test % #tracePredicate .~ expectLibraryTraces [
            bootFiles
          , emptyUnit "SquashedAcrossHeaders.Fwd"
          ]

-- | A typedef squashed into a struct that fails to parse
--
-- The library plan never places the struct, but name mangling still squashes
-- the typedef into it.
test_typedefs_squashed_into_unusable :: LibraryTest
test_typedefs_squashed_into_unusable =
    libraryTest "typedefs" "squashed_into_unusable"
      & #test % #tracePredicate .~ expectLibraryTraces [
            longDouble "struct ld"
          , notSelected "ld_t"
          ]

-- | A typedef with the name of a struct that fails to parse, and points at it
--
-- The library plan never places the struct, but name mangling still asks
-- whether the two share a module.
test_typedefs_clashes_with_unusable :: LibraryTest
test_typedefs_clashes_with_unusable =
    libraryTest "typedefs" "clashes_with_unusable"
      & #test % #tracePredicate .~ expectLibraryTraces [
            longDouble "struct foo"
          , notSelected "foo"
          ]

{-------------------------------------------------------------------------------
  Callbacks
-------------------------------------------------------------------------------}

-- | Where the instances of typedef and raw callbacks go
test_callbacks_typedef_and_raw_callbacks :: LibraryTest
test_callbacks_typedef_and_raw_callbacks =
    libraryTest "callbacks" "typedef_and_raw_callbacks"
      & #test % #tracePredicate .~ expectLibraryTraces []

-- | With @--generate-hs-boot@: a boot file for every types module, and the
-- typedef callbacks in per-header Callback modules
test_callbacks_typedef_and_raw_callbacks_hs_boot :: LibraryTest
test_callbacks_typedef_and_raw_callbacks_hs_boot =
    test_callbacks_typedef_and_raw_callbacks
      & libraryVariant 1 "generate_hs_boot"
      & #test % #onBackend      .~ ( #hsBootPolicy .~ GenerateHsBoot )
      & #test % #tracePredicate .~ expectLibraryTraces [hsBootFlag]

{-------------------------------------------------------------------------------
  Selection
-------------------------------------------------------------------------------}

-- | Library declarations that use declarations of a vendor header
--
-- The selection drops them: their vendor dependencies are not selected.
test_selection_vendor_dependencies :: LibraryTest
test_selection_vendor_dependencies =
    libraryTest "selection" "vendor_dependencies"
      & #test % #tracePredicate .~ expectLibraryTraces (
            map notSelected [
                "struct uses_vendor"
              , "uses_vendor_take"
              , "uses_vendor_age"
              , "struct uses_vendor_indirectly"
              , "uses_vendor_indirectly_apply"
              ]
          ++ vendorEmptyUnits
          )

-- | With program slicing, the vendor declarations are selected, and they go
-- to the vendor module
test_selection_vendor_dependencies_slicing :: LibraryTest
test_selection_vendor_dependencies_slicing =
    test_selection_vendor_dependencies
      & libraryVariant 1 "program_slicing"
      & #test % #onFrontend .~ ( #programSlicing .~ EnableProgramSlicing )
      & #test % #tracePredicate .~ expectLibraryTraces [
            vendorModule "VendorDependencies.Vendor" 2
          ]

-- | A selection predicate with program slicing: the selected header pulls in
-- a declaration of another header, which lands in the module of that header
test_selection_vendor_dependencies_select_header :: LibraryTest
test_selection_vendor_dependencies_select_header =
    test_selection_vendor_dependencies
      & libraryVariant 2 "select_by_header_path"
      & #test % #onFrontend .~ (
              ( #programSlicing     .~ EnableProgramSlicing )
            . ( #selectionPredicate .~
                 BIf (SelectHeader (HeaderPathMatches "selected\\.h$")) )
          )
      & #test % #tracePredicate .~ expectLibraryTraces vendorEmptyUnits

-- | A selection predicate that selects the vendor header, without program
-- slicing
--
-- The vendor declarations go to the vendor module, as with program slicing.
test_selection_vendor_dependencies_select_vendor_header :: LibraryTest
test_selection_vendor_dependencies_select_vendor_header =
    test_selection_vendor_dependencies
      & libraryVariant 3 "select_vendor_header"
      & #test % #onFrontend .~ ( #selectionPredicate .~
              BOr (BIf (SelectHeader FromMainHeaders))
                  (BIf (SelectHeader (HeaderPathMatches "/vendor/vendor\\.h$")))
          )
      & #test % #tracePredicate .~ expectLibraryTraces [
            vendorModule "VendorDependencies.Vendor" 2
          ]

-- | The headers whose declarations all depend on vendor declarations
vendorEmptyUnits :: [Text]
vendorEmptyUnits = [
      emptyUnit "VendorDependencies.Uses_vendor"
    , emptyUnit "VendorDependencies.Uses_vendor_indirectly"
    ]

{-------------------------------------------------------------------------------
  Binding specifications
-------------------------------------------------------------------------------}

-- | Binding specifications with squashed and omitted entries
test_binding_specs_generated_specs :: LibraryTest
test_binding_specs_generated_specs =
    libraryTest "binding-specs" "generated_specs"
      & #bindingSpecDir .~ Just "bindingspecs"
      & #test % #specPrescriptive .~ Just
          "test-artefacts/headers/library/binding-specs/generated_specs.yaml"
      & #test % #tracePredicate .~ expectLibraryTraces [
            emptyUnit "GeneratedSpecs.Fwd"
          ]

-- | A library that uses the types of 'test_binding_specs_generated_specs'
-- through the binding specifications that test writes
test_binding_specs_external_specs :: LibraryTest
test_binding_specs_external_specs =
    libraryTest "binding-specs" "external_specs"
      & #dependencies .~ [test_binding_specs_generated_specs]
      & #test % #specExternal .~ [
            generatedSpec test_binding_specs_generated_specs $
              Hs.ModuleName "GeneratedSpecs.Same_header"
          , generatedSpec test_binding_specs_generated_specs $
              Hs.ModuleName "GeneratedSpecs.Other_header"
          ]
      & #test % #tracePredicate .~ expectLibraryTraces []

-- | Two callbacks with the same raw function type, which mentions a type of
-- 'test_binding_specs_generated_specs'
--
-- Comparing the two types needs the module of that type, which only its
-- binding specification knows.
test_binding_specs_external_callbacks :: LibraryTest
test_binding_specs_external_callbacks =
    libraryTest "binding-specs" "external_callbacks"
      & #dependencies .~ [test_binding_specs_generated_specs]
      & #test % #specExternal .~ [
            generatedSpec test_binding_specs_generated_specs $
              Hs.ModuleName "GeneratedSpecs.Same_header"
          ]
      & #test % #tracePredicate .~ expectLibraryTraces []

{-------------------------------------------------------------------------------
  Documentation
-------------------------------------------------------------------------------}

-- | Doxygen comments of library headers that only the input header includes
--
-- Like the other golden tests, this test fails when @doxygen@ is not on the
-- @PATH@: the missing-@doxygen@ warning is unexpected.
test_documentation_doxygen_in_library_headers :: LibraryTest
test_documentation_doxygen_in_library_headers =
    libraryTest "documentation" "doxygen_in_library_headers"
      & #test % #tracePredicate .~ expectLibraryTraces []

{-------------------------------------------------------------------------------
  Plan errors
-------------------------------------------------------------------------------}

test_errors_module_name_collision :: LibraryTest
test_errors_module_name_collision =
    libraryTest "errors" "module_name_collision"
      & #outcome .~ RejectedByPlan (\case
            ModuleNameCollision{} -> True
            _otherwise            -> False
          )

test_errors_output_path_collision :: LibraryTest
test_errors_output_path_collision =
    libraryTest "errors" "output_path_collision"
      & #outcome .~ RejectedByPlan (\case
            OutputPathCollision{} -> True
            _otherwise            -> False
          )

-- | A library header @vendor.h@ gets the module name of the vendor module
test_errors_vendor_module_collision :: LibraryTest
test_errors_vendor_module_collision =
    libraryTest "errors" "vendor_module_collision"
      & #test % #onFrontend .~ ( #programSlicing .~ EnableProgramSlicing )
      & #outcome .~ RejectedByPlan (\case
            ModuleNameCollision _ (LibraryHeader _, _) (VendorHeaders, _) ->
              True
            _otherwise ->
              False
          )

-- | The library directory has a header, but no root header includes it
test_errors_no_headers_found :: LibraryTest
test_errors_no_headers_found =
    libraryTest "errors" "no_headers_found"
      & #outcome .~ RejectedByPlan (\case
            NoHeadersFound{} -> True
            _otherwise       -> False
          )

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

-- | Expect each of these library and selection traces once
--
-- Any other warning or error is unexpected, as with 'defaultTracePredicate'.
expectLibraryTraces :: [Text] -> TracePredicate Level TraceMsg
expectLibraryTraces expected = multiTracePredicate expected $ \case
    TraceLibrary msg -> case msg of
      LibraryEmptyUnit moduleName _path ->
        Just $ Expected $ emptyUnit moduleName.text
      LibraryAutoBootFiles ->
        Just $ Expected bootFiles
      LibraryHsBootFlagNotRecommended ->
        Just $ Expected hsBootFlag
      LibraryVendorModule moduleName count ->
        Just $ Expected $ vendorModule moduleName.text count
      LibraryVendorDecls{} ->
        Nothing
      LibraryOutputMismatch{} ->
        Just Unexpected
    MatchSelect name (MatchTransMissing _) ->
      Just $ Expected $ notSelected (C.renderDeclName name)
    MatchDelayed name (ParseUnsupportedFloatType UnsupportedLongDouble) ->
      Just $ Expected $ longDouble (C.renderDeclName name)
    _otherwise ->
      Nothing

bootFiles :: Text
bootFiles = "boot files for an import cycle"

hsBootFlag :: Text
hsBootFlag = "--generate-hs-boot not recommended"

emptyUnit :: Text -> Text
emptyUnit moduleName = "no files for " <> moduleName

-- | The label of the trace that counts the declarations of the vendor module
vendorModule :: Text -> Int -> Text
vendorModule moduleName count =
    "vendor module " <> moduleName <> ", declarations: " <> Text.pack (show count)

notSelected :: Text -> Text
notSelected name = "dependency not selected: " <> name

longDouble :: Text -> Text
longDouble name = "unsupported long double: " <> name
