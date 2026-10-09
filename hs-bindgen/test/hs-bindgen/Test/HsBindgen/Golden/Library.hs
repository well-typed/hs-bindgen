-- | Golden tests: a library generated as one module per header
--
-- A case reads one header and generates a module for each library header that
-- it reaches.  There is a golden file for the list of modules, and one for
-- each file of each module.  The files of a case are in one directory, laid
-- out as @preprocess --library@ writes them, so that @pp-fixtures@ can compile
-- them together.
module Test.HsBindgen.Golden.Library (
    tests
  , fixtureCases
  ) where

import Data.ByteString.UTF8 qualified as UTF8
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (listToMaybe)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, takeFileName, (<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

import HsBindgen
import HsBindgen.ArtefactM (DelayedIO (..), DirPolicy (..), FileContent (..),
                            FileDescription (..), FileLocation (..),
                            FilePolicy (..), executeDelayedIOActions)
import HsBindgen.Backend.Category
import HsBindgen.BindingSpec (Prescriptive (..))
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Boot (ClangArtefacts (..), getClangArtefacts)
import HsBindgen.Config.ClangArgs (ClangArgsConfig (..))
import HsBindgen.Config.Internal
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Library (Library (..), LibraryError, LibraryPath,
                          renderLibraryPath)
import HsBindgen.Library.Plan (Module (..))
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer (Level, nullTracer, prettyForTrace)

import Test.Common.HsBindgen.Trace.Predicate
import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Golden.Infra.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Cases
-------------------------------------------------------------------------------}

-- | A library and what is generated for it
data LibraryCase = LibraryCase {
      -- | Name of the test, and of its directory under @fixtures\/library@
      name :: String

      -- | The header to read, as it is included with @test-artefacts\/headers@
      -- on the include path
    , root :: String

      -- | The library directories, below @test-artefacts\/headers@
    , library :: NonEmpty FilePath

      -- | More directories for the include path, below
      -- @test-artefacts\/headers@
    , includes :: [FilePath]

      -- | The declarations that are selected, among those of the library
      -- headers
    , predicate :: Boolean SelectionPredicate

      -- | The base module
    , baseModule :: BaseModuleName

      -- | The modules that are expected, in any order
      --
      -- Each has a golden test for every file that a module can have.  A
      -- module that is generated and not listed here shows in the list of
      -- modules, which has a golden test of its own.
    , generated :: [BaseModuleName]

      -- | The modules whose binding specification has a golden test, each
      -- with its golden file
      --
      -- Such a specification is also written to a file and read back.
    , specs :: [(BaseModuleName, FilePath)]

      -- | What the run that reads the header has beyond the above: the
      -- binding specifications that it is given, and the warnings that it is
      -- expected to print
    , onTest :: TestCase -> TestCase
    }

cases :: [LibraryCase]
cases = [
      -- b.h includes a.h and defines the struct that a.h declares forward
      LibraryCase{
          name       = "forward_decl"
        , root       = "forward_decl/b.h"
        , library    = "forward_decl" :| []
        , includes   = []
        , predicate  = BTrue
        , baseModule = "M"
        , generated  = ["M.B", "M.A"]
        , specs      = []
        , onTest     = id
        }
      -- Headers in a subdirectory, one of them used by nobody
    , LibraryCase{
          name       = "mylib"
        , root       = "mylib.h"
        , library    = "." :| []
        , includes   = []
        , predicate  = BTrue
        , baseModule = "MyLib"
        , generated  = [
              "MyLib.Mylib.Types"
            , "MyLib.Mylib.Internal"
            , "MyLib.Mylib.Ops"
            , "MyLib.Mylib"
            ]
          -- Next to the modules, as with @--gen-binding-spec-dir@ and the
          -- directory that the modules go to
        , specs      = [
              (name, fixtures </> "mylib" </> baseModuleNameToString name <.> "yaml")
            | name <- [
                  "MyLib.Mylib.Types"
                , "MyLib.Mylib.Internal"
                , "MyLib.Mylib.Ops"
                , "MyLib.Mylib"
                ]
            ]
        , onTest     = id
        }
      -- types.h has the typedefs for the structs that foo.h and bar.h define,
      -- and alias.h a typedef for a struct from outside the library
    , LibraryCase{
          name       = "forward_typedef"
        , root       = "forward_typedef/root.h"
        , library    = "forward_typedef/lib" :| []
        , includes   = ["forward_typedef"]
        , predicate  = BTrue
        , baseModule = "M"
        , generated  = ["M.Types", "M.Bar", "M.Alias", "M.Foo", "M.Api"]
        , specs      = []
        , onTest     = id
        }
      -- user.h uses a struct from outside the library by its tag, and comes
      -- before alias.h, which has the typedef for it
    , LibraryCase{
          name       = "vendor_by_tag"
        , root       = "vendor_by_tag/root.h"
        , library    = "vendor_by_tag/lib" :| []
        , includes   = ["vendor_by_tag"]
        , predicate  = BTrue
        , baseModule = "M"
        , generated  = ["M.Alias", "M.User"]
        , specs      = []
        , onTest     = id
        }
      -- Each of the two headers uses a declaration of the other, so they
      -- share a module.  elaborate.h includes the other one.
    , LibraryCase{
          name       = "elaborate"
        , root       = "golden/macros/parse/elaborate.h"
        , library    = "golden/macros/parse" :| []
        , includes   = []
        , predicate  = BTrue
        , baseModule = "M"
        , generated  = ["M.Elaborate", "M.Elaborate_inner"]
        , specs      = []
        , onTest     = id
        }
      -- The two headers include each other, and no declaration of one uses a
      -- declaration of the other
    , LibraryCase{
          name       = "circular_includes"
        , root       = "golden/program-analysis/circular_includes.h"
        , library    = "golden/program-analysis" :| []
        , includes   = []
        , predicate  = BTrue
        , baseModule = "M"
        , generated  = ["M.Circular_includes", "M.Circular_includes_inner"]
        , specs      = []
        , onTest     = id
        }
      -- The header that is read has no declaration of its own
    , LibraryCase{
          name       = "macro_typedef_scope_multiple"
        , root       = "golden/macros/parse/macro_typedef_scope_multiple.h"
        , library    = "golden/macros/parse" :| []
        , includes   = []
        , predicate  = BTrue
        , baseModule = "M"
        , generated  = [
              "M.Macro_typedef_scope_multiple"
            , "M.Macro_typedef_scope_multiple_inner1"
            , "M.Macro_typedef_scope_multiple_inner2"
            ]
        , specs      = []
        , onTest     = id
        }
      -- types.h has a typedef for a pointer to a struct of td.h, which has a
      -- field of a type from types.h.  td.h includes types.h.
    , LibraryCase{
          name       = "rpm_pair"
        , root       = "rpm_pair/td.h"
        , library    = "rpm_pair" :| []
        , includes   = []
        , predicate  = BTrue
        , baseModule = "M"
        , generated  = ["M.Td", "M.Types"]
        , specs      = []
        , onTest     = id
        }
      -- A function of each header has the struct of the other in its type,
      -- and neither header includes the other
    , LibraryCase{
          name       = "window_widget"
        , root       = "window_widget/root.h"
        , library    = "window_widget" :| []
        , includes   = []
        , predicate  = BTrue
        , baseModule = "M"
        , generated  = ["M.Window", "M.Widget", "M.Root"]
        , specs      = []
        , onTest     = id
        }
      -- mylib without the header that nobody uses, as with
      -- @--select-except-by-header-path 'internal\.h$'@.  The pattern is
      -- matched against the whole path, which has other separators on
      -- Windows, so it has none.
    , LibraryCase{
          name       = "mylib_except"
        , root       = "mylib.h"
        , library    = "." :| []
        , includes   = []
        , predicate  = BNot (BIf (SelectHeader (HeaderPathMatches "internal\\.h$")))
        , baseModule = "MyLib"
        , generated  = [
              "MyLib.Mylib.Types"
            , "MyLib.Mylib.Internal"
            , "MyLib.Mylib.Ops"
            , "MyLib.Mylib"
            ]
        , specs      = []
        , onTest     = id
        }
      -- The declarations of detail.h are not selected.  It defines a struct
      -- that the other two headers use, and the struct goes to the module of
      -- the first.
    , LibraryCase{
          name       = "excepted_type"
        , root       = "excepted_type/root.h"
        , library    = "excepted_type" :| []
        , includes   = []
        , predicate  = BNot (BIf (SelectHeader (HeaderPathMatches "detail\\.h$")))
        , baseModule = "M"
        , generated  = ["M.Reader", "M.Writer", "M.Detail"]
        , specs      = []
        , onTest     = id
        }
      -- mylib without the declarations for rectangles, as with
      -- @--select-except-by-decl-name rect_@.  The name that is matched has
      -- the tag, so the struct is @struct rect_t@.
    , LibraryCase{
          name       = "mylib_except_decl"
        , root       = "mylib.h"
        , library    = "." :| []
        , includes   = []
        , predicate  = BNot (BIf (SelectDecl (DeclNameMatches "rect_")))
        , baseModule = "MyLib"
        , generated  = [
              "MyLib.Mylib.Types"
            , "MyLib.Mylib.Internal"
            , "MyLib.Mylib.Ops"
            , "MyLib.Mylib"
            ]
        , specs      = []
        , onTest     = id
        }
      -- mylib with the declarations of the given header only, as with
      -- @--select-from-main-headers@.  The header has a function, and the
      -- headers that it includes own nothing, so the struct in the type of
      -- the function is generated with it.
    , LibraryCase{
          name       = "mylib_main_headers"
        , root       = "mylib.h"
        , library    = "." :| []
        , includes   = []
        , predicate  = BIf (SelectHeader FromMainHeaders)
        , baseModule = "MyLib"
        , generated  = ["MyLib.Mylib"]
        , specs      = []
        , onTest     = id
        }
      -- forward_decl with an external binding specification for the struct
      -- of b.h.  Nobody owns a declaration that is bound, so b.h has nothing
      -- and a.h does not wait for it.
    , LibraryCase{
          name       = "forward_decl_external"
        , root       = "forward_decl/b.h"
        , library    = "forward_decl" :| []
        , includes   = []
        , predicate  = BTrue
        , baseModule = "M"
        , generated  = ["M.B", "M.A"]
        , specs      = []
        , onTest     = #specExternal .~ [headersDir </> "forward_decl/shape.yaml"]
        }
      -- forward_typedef with a prescriptive binding specification that makes
      -- the struct of foo.h opaque.  Without its fields the struct uses
      -- nothing, so foo.h does not wait for bar.h and types.h.
    , LibraryCase{
          name       = "forward_typedef_emptydata"
        , root       = "forward_typedef/root.h"
        , library    = "forward_typedef/lib" :| []
        , includes   = ["forward_typedef"]
        , predicate  = BTrue
        , baseModule = "M"
        , generated  = ["M.Types", "M.Foo", "M.Bar", "M.Alias", "M.Api"]
        , specs      = []
        , onTest     = \test -> test
            & #specPrescriptive .~
                Just (headersDir </> "forward_typedef/foo_p.yaml")
            & #tracePredicate .~ unusedIn ["M.Bar", "M.Alias", "M.Api"]
        }
      -- mylib with a prescriptive binding specification that names no
      -- module, and so is for every module of the library.  It gives the
      -- struct of types.h another name, which the modules that use the
      -- struct use too.
    , LibraryCase{
          name       = "mylib_rename"
        , root       = "mylib.h"
        , library    = "." :| []
        , includes   = []
        , predicate  = BTrue
        , baseModule = "MyLib"
        , generated  = [
              "MyLib.Mylib.Types"
            , "MyLib.Mylib.Internal"
            , "MyLib.Mylib.Ops"
            , "MyLib.Mylib"
            ]
        , specs      = []
        , onTest     = \test -> test
            & #specPrescriptive .~ Just (headersDir </> "mylib_p.yaml")
            & #tracePredicate .~ unusedIn [
                "MyLib.Mylib.Internal"
              , "MyLib.Mylib.Ops"
              , "MyLib.Mylib"
              ]
        }
      -- A prescriptive binding specification for every module omits the
      -- struct of stopwatch.h, and every module records the omission.  The
      -- binding specifications of the first two modules are the ones that
      -- the golden test for merging two omissions reads
      -- ("Test.HsBindgen.Golden.BindingSpecs").
    , LibraryCase{
          name       = "omit_twice"
        , root       = "binding-specs/merge/omit_twice/report.h"
        , library    = "golden/binding-specs/merge/omit_twice" :| []
        , includes   = ["golden"]
        , predicate  = BTrue
        , baseModule = "Lib"
        , generated  = ["Lib.Stopwatch", "Lib.Counter", "Lib.Report"]
        , specs      = [
              ("Lib.Stopwatch", omitTwice </> "stopwatch.yaml")
            , ("Lib.Counter",   omitTwice </> "counter.yaml")
            ]
        , onTest     = #specPrescriptive .~ Just (omitTwice </> "report_p.yaml")
        }
      -- register.h has a function that takes a callback over the struct of
      -- foo.h.  Instances for the type of the callback are generated with a
      -- type that it mentions, and here that type is in another module: no
      -- module has the instances.
    , LibraryCase{
          name       = "callback"
        , root       = "callback/register.h"
        , library    = "callback" :| []
        , includes   = []
        , predicate  = BTrue
        , baseModule = "M"
        , generated  = ["M.Foo", "M.Register"]
        , specs      = []
        , onTest     = id
        }
    ]
  where
    omitTwice :: FilePath
    omitTwice = headersDir </> "golden/binding-specs/merge/omit_twice"

-- | Expect these modules to report an entry of the prescriptive binding
-- specification as unused
--
-- A module after the one that defines a type has a binding for the type, so
-- an entry for that type has nothing to apply to there.
unusedIn :: [BaseModuleName] -> TracePredicate Level TraceMsg
unusedIn names =
    multiTracePredicate (map baseModuleNameToString names) $ \case
      TraceModule (InModule name (TraceFrontend (FrontendResolveBindingSpecs
        ResolveBindingSpecsTypeNotUsed{}))) ->
          Just $ Expected (baseModuleNameToString name)
      _otherwise ->
        Nothing

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests getTestResources =
    testGroup "Test.HsBindgen.Golden.Library" $
      map (testCaseFor getTestResources) cases

-- | What is generated for one module
data ModuleFiles = ModuleFiles {
      -- | The module of each binding category
      bindings :: ByCategory_ (Maybe String)

      -- | The binding specification, as the modules after this one get it
    , spec :: BindingSpec.BindingSpec

      -- | The file that @--gen-binding-spec-dir@ writes for the module
    , specFile :: FileDescription
    }

-- | The modules of a library, as they are generated
type Generated = [(BaseModuleName, Module LibraryPath C.DeclId, ModuleFiles)]

testCaseFor :: IO TestResources -> LibraryCase -> TestTree
testCaseFor getTestResources libraryCase =
    withResource run (\_ -> pure ()) $ \getResult ->
      let filesOf :: BaseModuleName -> IO (Maybe ModuleFiles)
          filesOf name = do
              result <- successful =<< getResult
              pure $ listToMaybe [
                  files
                | (name', _, files) <- result
                , name' == name
                ]
      in  testGroup libraryCase.name $ concat [
              [ goldenAnsiDiff "modules" (fixtureDir </> "modules.txt") $
                  \_report -> ActualValue . listing <$> (successful =<< getResult)
              ]
            , [ goldenAnsiDiff (localPath name category) file $ \_report ->
                  filesOf name >>= \case
                    Just files
                      | Just content <-
                          view (lensForCategory category) files.bindings -> do
                        createDirectoryIfMissing True (takeDirectory file)
                        pure $ ActualValue content
                    _otherwise ->
                      pure ActualNoOutput
              | name     <- libraryCase.generated
              , category <- NonEmpty.toList allCategories
              , let file = fixtureDir </> localPath name category
              ]
            , concat [
                  [ goldenAnsiDiff (takeFileName file) file $ \_report ->
                      maybe ActualNoOutput (ActualValue . written . (.specFile))
                        <$> filesOf name
                  , testCase (takeFileName file ++ " read back") $
                      filesOf name >>= \case
                        Just files -> readsBack files
                        Nothing    -> assertFailure "The module was not generated"
                  ]
                | (name, file) <- libraryCase.specs
                ]
            ]
  where
    fixtureDir :: FilePath
    fixtureDir = fixtureDirOf libraryCase

    run :: IO (Either BindgenError (Either LibraryError Generated))
    run = do
        testResources <- getTestResources
        let headers = testResources.packageRoot </> headersDir
            library = Library{
                dirs = (headers </>) <$> libraryCase.library
              }
        createDirectoryIfMissing True fixtureDir
        runTestHsBindgen (const $ pure ()) getTestResources (toTestCase libraryCase) $
          forEachModule library libraryCase.predicate $
            ModuleFiles
              <$> getBindingsMultiple ModuleRenderConfig{qualifiedStyle = def}
              <*> generatedBindingSpec
              <*> getBindingSpecFileIn
                    DoNotOverwriteFiles
                    DoNotCreateOutputDirs
                    fixtureDir

    successful ::
         Either BindgenError (Either LibraryError Generated)
      -> IO Generated
    successful = \case
      Left err           -> assertFailure $
        "Expected 'hs-bindgen' to succeed, but received an error: " ++ show err
      Right (Left err)   -> assertFailure $
        "Expected a library, but: " ++ show (prettyForTrace err)
      Right (Right mods) -> pure mods

    written :: FileDescription -> String
    written file = case file.content of
      StringContent     str   -> str
      ByteStringContent bytes -> UTF8.toString bytes

    -- The file of a specification loads as the specification.  So what a
    -- module is given for the modules before it is what the files say.
    readsBack :: ModuleFiles -> Assertion
    readsBack files =
        withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
          let path = tmpDir </> "bindingspec" <.> "yaml"
          executeDelayedIOActions nullTracer [
              WriteToFile FileDescription{
                  description = files.specFile.description
                , location    = UserSpecified path
                , filePolicy  = files.specFile.filePolicy
                , dirPolicy   = files.specFile.dirPolicy
                , content     = files.specFile.content
                }
            ]
          bootConfig <-
            getTestBootConfig (toTestCase libraryCase) <$> getTestResources
          clang      <- getClangArtefacts nullTracer bootConfig.clangArgs
          loaded     <-
            BindingSpec.loadPrescriptiveBindingSpec
              nullTracer
              clang.clangArgs
              (BindingSpec.moduleName files.spec)
              BindingSpec.BindingSpecStrict
              (Just path)
          loaded @?= Prescriptive{
              declared = Just (BindingSpec.moduleName files.spec)
            , spec     = files.spec
            }

-- | The modules in the order of generation, each with its headers and files
listing :: Generated -> String
listing mods = unlines $ concat [
      (baseModuleNameToString name ++ ": " ++ headers m)
    : case filesOf name files.bindings of
        []    -> ["  (nothing generated)"]
        paths -> map ("  " ++) paths
    | (name, m, files) <- mods
    ]
  where
    headers :: Module LibraryPath C.DeclId -> String
    headers m =
      List.intercalate ", " $ map renderLibraryPath (NonEmpty.toList m.headers)

    filesOf :: BaseModuleName -> ByCategory_ (Maybe String) -> [String]
    filesOf name files = [
          map slash (localPath name category)
        | category <- NonEmpty.toList allCategories
        , isJust (view (lensForCategory category) files)
        ]

    -- The listing is the same on every platform
    slash :: Char -> Char
    slash c = if c == '\\' then '/' else c

-- | Where the module of a category is, below the output directory
localPath :: BaseModuleName -> Category -> FilePath
localPath name category =
    Hs.moduleNamePath (fromBaseModuleName name (Just category))

{-------------------------------------------------------------------------------
  Fixtures
-------------------------------------------------------------------------------}

headersDir :: FilePath
headersDir = "test-artefacts" </> "headers"

-- | The directory with one directory of fixtures for each case
fixtures :: FilePath
fixtures = "test-artefacts" </> "fixtures" </> "library"

fixtureDirOf :: LibraryCase -> FilePath
fixtureDirOf libraryCase = fixtures </> libraryCase.name

-- | The run that reads the header of the case
--
-- The name and the output directory are what @pp-fixtures@ knows the fixture
-- by.
toTestCase :: LibraryCase -> TestCase
toTestCase libraryCase =
    libraryCase.onTest $ defaultTest libraryCase.name
      & #name        .~ "library" </> libraryCase.name
      & #inputHeader .~ libraryCase.root
      & #inputDir    .~ headersDir
      & #outputDir   .~ fixtureDirOf libraryCase
      & #onBoot      .~ (\boot -> boot
          & #baseModule .~ libraryCase.baseModule
          & #clangArgs % #extraIncludeDirs %~ (++ includeDirsOf libraryCase)
          )

-- | The directories that the case adds to the include path
--
-- They are relative to the package, which is where the tests run.
includeDirsOf :: LibraryCase -> [FilePath]
includeDirsOf libraryCase = map (headersDir </>) libraryCase.includes

-- | The fixtures for @pp-fixtures@ to compile, each with the directories that
-- it needs on the include path
--
-- A case with an external binding specification is left out: its modules
-- import one that is not among the fixtures.
fixtureCases :: [(TestCase, [FilePath])]
fixtureCases = [
      (test, includeDirsOf libraryCase)
    | libraryCase <- cases
    , let test = toTestCase libraryCase
    , null test.specExternal
    ]
