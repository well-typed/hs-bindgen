-- | Golden tests of @--library@: one module per header
--
-- Each test runs the library once and checks the list of its modules, and the
-- files of each module against the fixtures in
-- @test-artefacts\/fixtures\/library@. In 'Full' mode it also compiles those
-- fixtures, with @ghc --make@ and one module at a time.
module Test.HsBindgen.Library (tests) where

import Data.List.NonEmpty qualified as NonEmpty
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import HsBindgen
import HsBindgen.Backend.Category
import HsBindgen.Backend.HsModule.Translation (hasHsBoot)
import HsBindgen.Config.Internal
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Fixtures.Utils
import Test.HsBindgen.Golden.Infra.TestCase
import Test.HsBindgen.PPFixtures.Compile (detectModules)
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

-- | A library to generate
data LibraryTest = LibraryTest {
      -- | Its main header, include directory, fixture directory, base module
      -- and selection
      test    :: TestCase
      -- | The library directory, relative to the package root
    , library :: FilePath
      -- | The modules, in include order of their headers
    , modules :: [BaseModuleName]
    }
  deriving stock (Generic)

-- | A library in @test-artefacts\/headers\/library\/NAME@, with base module @M@
--
-- Without @--select@ flags, @--library@ selects the declarations in every
-- header, which the library restricts to its own headers.
libraryTest ::
     String           -- ^ Name, also of the directories of headers and fixtures
  -> String           -- ^ Main header
  -> [BaseModuleName] -- ^ Modules
  -> LibraryTest
libraryTest name header modules = LibraryTest{
      test    = defaultTest name
        & #inputHeader .~ header
        & #inputDir    .~ headers
        & #outputDir   .~ "test-artefacts" </> "fixtures" </> "library" </> name
        & #onBoot      .~ ( #baseModule .~ "M")
        & #onFrontend  .~ ( #selectionPredicate .~ BIf (SelectHeader FromAllHeaders))
    , library = headers
    , modules = modules
    }
  where
    headers :: FilePath
    headers = "test-artefacts" </> "headers" </> "library" </> name

withBaseModule :: BaseModuleName -> LibraryTest -> LibraryTest
withBaseModule baseModule = #test % #onBoot .~ ( #baseModule .~ baseModule)

testCases :: [LibraryTest]
testCases = [
      -- A forward declaration does not move a type out of its header: no M.A
      -- types module.
      libraryTest "forward_decl" "b.h" ["M.A", "M.B"]
      -- A squashed typedef stays with its struct, also when the struct is
      -- outside the library (window_t).
    , libraryTest "forward_typedef" "root.h"
        ["M.Types", "M.Foo", "M.Bar", "M.Api", "M.Alias"]
        & #library %~ (</> "lib")
      -- Module names below a directory
    , withBaseModule "MyLib" $
        libraryTest "mylib" "mylib.h"
          [ "MyLib.Mylib.Types", "MyLib.Mylib.Ops", "MyLib.Mylib.Internal"
          , "MyLib.Mylib"
          ]
      -- A deselected header owns nothing.
    , withBaseModule "MyLib" $
        libraryTest "mylib" "mylib.h"
          ["MyLib.Mylib.Types", "MyLib.Mylib.Ops", "MyLib.Mylib"]
        & #test % #name .~ "mylib_without_internal"
        & #test % #outputDir %~ (++ "_without_internal")
        & #test % #onFrontend .~ ( #selectionPredicate .~ selectExcept "internal\\.h$")
      -- Headers that include each other but use nothing of each other
    , libraryTest "circular_includes" "circular_includes.h"
        ["M.Circular_includes", "M.Circular_includes_inner"]
        & #test % #inputDir .~ programAnalysis
        & #library .~ programAnalysis
      -- Two types modules that use each other: SOURCE imports both ways.
    , libraryTest "list_node" "node.h" ["M.List", "M.Node"]
      -- A loop between the headers only through functions: the types modules
      -- do not import each other.
    , libraryTest "window_widget" "root.h" ["M.Window", "M.Widget", "M.Root"]
      -- A loop with a callback: the Callback module, its SOURCE imports, and
      -- its import of Lib.Point for the FFIType equation of Status.
    , withBaseModule "Lib" $
        libraryTest "point_shape" "shape.h" ["Lib.Point", "Lib.Shape"]
      -- Callbacks that mention each other's types
    , libraryTest "callback_loop" "root.h" ["M.A", "M.B", "M.Root"]
      -- A typedef of another header's typedef, and of that one again: the term
      -- modules of draw.h import the types modules of the other two for their
      -- FFIType equations.
    , libraryTest "typedef_chain" "draw.h" ["M.Point", "M.Shape", "M.Draw"]
      -- A type that two headers need while its own header is deselected goes
      -- to the first module that needs it.
    , libraryTest "reader_writer" "root.h" ["M.Reader", "M.Writer", "M.Root"]
        & #test % #onFrontend .~ ( #selectionPredicate .~ selectExcept "detail\\.h$")
      -- Per-module names: M.A.Foo and M.B.Foo, both Bars and both bar_xs
    , libraryTest "split" "b.h" ["M.A", "M.B"]
      -- The running example: two Points
    , withBaseModule "Lib" $
        libraryTest "points" "root.h"
          ["Lib.Geo.Point", "Lib.Geo.Shape", "Lib.Ui.Point", "Lib.Root"]
      -- A squashed typedef takes its struct from outside the library, also
      -- when an earlier header uses the struct directly.
    , libraryTest "squash_outside" "root.h" ["M.Early", "M.Alias"]
        & #library %~ (</> "lib")
    ]
  where
    programAnalysis :: FilePath
    programAnalysis = "test-artefacts" </> "headers" </> "golden" </> "program-analysis"

    selectExcept :: String -> Boolean SelectionPredicate
    selectExcept regex = mergeBooleans
      [BIf $ SelectHeader $ HeaderPathMatches $ fromString regex]
      [BIf $ SelectHeader FromAllHeaders]

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests getTestResources =
    askOption $ \runMode ->
    testGroup "Test.HsBindgen.Library" [
        testLibrary getTestResources runMode libraryTest'
      | libraryTest' <- testCases
      ]

-- | Run the library once, and share the result across its tests
testLibrary :: IO TestResources -> RunMode -> LibraryTest -> TestTree
testLibrary getTestResources runMode libraryTest' =
    withResource run (\_ -> pure ()) $ \getResult ->
    testGroup libraryTest'.test.name $ concat [
        [ testCase "modules" $ do
            result <- getResult
            map fst result @?= libraryTest'.modules
        ]
      , [ goldenAnsiDiff (testName name category file) path $ \_report -> do
            result <- getResult
            case lookup name result of
              Nothing     -> pure $ ActualFailed $ "No module " ++ show name
              Just output ->
                case view (lensForCategory category) output >>= select file of
                  Nothing -> pure ActualNoOutput
                  Just x  -> do
                    -- For accepting the output, the first time
                    createDirectoryIfMissing True (takeDirectory path)
                    pure $ ActualValue x
        | name     <- libraryTest'.modules
        , category <- categories
        , file     <- ModuleFile : [BootFile | hasHsBoot category]
        , let path = fixture name category file
        ]
      , case runMode of
          Fast -> []
          Full -> [
              testCase "compile with --make" $ compileFixtures False
            , testCase "compile one module at a time" $ compileFixtures True
            ]
      ]
  where
    test :: TestCase
    test = libraryTest'.test

    run :: IO [(BaseModuleName, ByCategory_ (Maybe (String, Maybe String)))]
    run = do
      let mrc = ModuleRenderConfig{qualifiedStyle = test.qualifiedStyle}
      result <-
        runTestHsBindgenLibrary
          (\_ -> pure ())
          getTestResources
          test
          libraryTest'.library
          ((,) <$> ModuleBaseName <*> getBindingsMultiple mrc)
      case result of
        Left  err -> assertFailure $ "hs-bindgen failed: " ++ show err
        Right r   -> pure r

    categories :: [Category]
    categories = case runMode of
      Fast -> filter (/= CTerm CUnsafe) (NonEmpty.toList allCategories)
      Full -> NonEmpty.toList allCategories

    select :: File -> (String, Maybe String) -> Maybe String
    select ModuleFile = Just . fst
    select BootFile   = snd

    testName :: BaseModuleName -> Category -> File -> String
    testName name category file = unwords $
        baseModuleNameToString name
      : show category
      : ["boot" | BootFile <- [file]]

    fixture :: BaseModuleName -> Category -> File -> FilePath
    fixture name category file = test.outputDir </> path moduleName
      where
        moduleName :: Hs.ModuleName
        moduleName = fromBaseModuleName name (Just category)

        path :: Hs.ModuleName -> FilePath
        path = case file of
          ModuleFile -> Hs.moduleNamePath
          BootFile   -> Hs.moduleNameBootPath

    -- The fixtures are compiled as written, so the goldens must be up to date.
    -- Under @--make@, GHC warns that a SOURCE import outside a loop is not
    -- needed; @--library@ writes them everywhere until it detects loops.
    compileFixtures :: Bool -> Assertion
    compileFixtures oneAtATime = do
      testResources <- getTestResources
      let repoRoot   = takeDirectory testResources.packageRoot
          fixtureDir = testResources.packageRoot </> test.outputDir
          ghcOpts    = sharedGhcOptions ++ ["-Werror", "-Wwarn=unused-imports"]
      (includeDirs, envFile) <- setupFixtureEnv repoRoot
      let includeDirs' = (testResources.packageRoot </> test.inputDir) : includeDirs
      result <- withSystemTempDirectory "hs-bindgen-library" $ \tmpDir ->
        if oneAtATime then
          runOneShotGhcBuild tmpDir envFile ghcOpts includeDirs' fixtureDir
        else do
          modules <- detectModules fixtureDir
          results <-
            runDirectGhcBuild tmpDir envFile ghcOpts includeDirs' Nothing
              [(test.name, fixtureDir, modules, [])]
          pure $ sequence_ results
      either assertFailure pure result

-- | The files written for the module of a category
data File =
    -- | The module itself
    ModuleFile
    -- | Its boot file
  | BootFile
