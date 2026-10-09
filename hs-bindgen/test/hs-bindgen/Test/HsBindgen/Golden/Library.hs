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

import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import Test.Tasty
import Test.Tasty.HUnit (assertFailure)

import HsBindgen
import HsBindgen.Backend.Category
import HsBindgen.Config.ClangArgs (ClangArgsConfig (..))
import HsBindgen.Config.Internal
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Library (Library (..), LibraryError, LibraryPath,
                          renderLibraryPath)
import HsBindgen.Library.Plan (Module (..))
import HsBindgen.Util.Tracer (prettyForTrace)

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

      -- | The base module
    , baseModule :: BaseModuleName

      -- | The modules that are expected, in any order
      --
      -- Each has a golden test for every file that a module can have.  A
      -- module that is generated and not listed here shows in the list of
      -- modules, which has a golden test of its own.
    , generated :: [BaseModuleName]
    }

cases :: [LibraryCase]
cases = [
      -- b.h includes a.h and defines the struct that a.h declares forward
      LibraryCase{
          name       = "forward_decl"
        , root       = "forward_decl/b.h"
        , library    = "forward_decl" :| []
        , includes   = []
        , baseModule = "M"
        , generated  = ["M.B", "M.A"]
        }
      -- Headers in a subdirectory, one of them used by nobody
    , LibraryCase{
          name       = "mylib"
        , root       = "mylib.h"
        , library    = "." :| []
        , includes   = []
        , baseModule = "MyLib"
        , generated  = [
              "MyLib.Mylib.Types"
            , "MyLib.Mylib.Internal"
            , "MyLib.Mylib.Ops"
            , "MyLib.Mylib"
            ]
        }
      -- types.h has the typedefs for the structs that foo.h and bar.h define,
      -- and alias.h a typedef for a struct from outside the library
    , LibraryCase{
          name       = "forward_typedef"
        , root       = "forward_typedef/root.h"
        , library    = "forward_typedef/lib" :| []
        , includes   = ["forward_typedef"]
        , baseModule = "M"
        , generated  = ["M.Types", "M.Bar", "M.Alias", "M.Foo", "M.Api"]
        }
      -- user.h uses a struct from outside the library by its tag, and comes
      -- before alias.h, which has the typedef for it
    , LibraryCase{
          name       = "vendor_by_tag"
        , root       = "vendor_by_tag/root.h"
        , library    = "vendor_by_tag/lib" :| []
        , includes   = ["vendor_by_tag"]
        , baseModule = "M"
        , generated  = ["M.Alias", "M.User"]
        }
    ]

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests getTestResources =
    testGroup "Test.HsBindgen.Golden.Library" $
      map (testCaseFor getTestResources) cases

-- | The modules of a library, as they are generated
type Generated = [(BaseModuleName, Module LibraryPath C.DeclId, ByCategory_ (Maybe String))]

testCaseFor :: IO TestResources -> LibraryCase -> TestTree
testCaseFor getTestResources libraryCase =
    withResource run (\_ -> pure ()) $ \getResult ->
      testGroup libraryCase.name $
          goldenAnsiDiff "modules" (fixtureDir </> "modules.txt") (\_report ->
            ActualValue . listing <$> (successful =<< getResult))
        : [ goldenAnsiDiff (localPath name category) file $ \_report -> do
              result <- successful =<< getResult
              case [ files | (name', _, files) <- result, name' == name ] of
                [files] | Just content <- view (lensForCategory category) files -> do
                  createDirectoryIfMissing True (takeDirectory file)
                  pure $ ActualValue content
                _otherwise ->
                  pure ActualNoOutput
          | name     <- libraryCase.generated
          , category <- NonEmpty.toList allCategories
          , let file = fixtureDir </> localPath name category
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
          forEachModule library BTrue $
            getBindingsMultiple ModuleRenderConfig{qualifiedStyle = def}

    successful ::
         Either BindgenError (Either LibraryError Generated)
      -> IO Generated
    successful = \case
      Left err           -> assertFailure $
        "Expected 'hs-bindgen' to succeed, but received an error: " ++ show err
      Right (Left err)   -> assertFailure $
        "Expected a library, but: " ++ show (prettyForTrace err)
      Right (Right mods) -> pure mods

-- | The modules in the order of generation, each with its headers and files
listing :: Generated -> String
listing mods = unlines $ concat [
      (baseModuleNameToString name ++ ": " ++ headers m)
    : case filesOf name files of
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

fixtureDirOf :: LibraryCase -> FilePath
fixtureDirOf libraryCase =
    "test-artefacts" </> "fixtures" </> "library" </> libraryCase.name

-- | The run that reads the header of the case
--
-- The name and the output directory are what @pp-fixtures@ knows the fixture
-- by.
toTestCase :: LibraryCase -> TestCase
toTestCase libraryCase =
    defaultTest libraryCase.name
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
fixtureCases :: [(TestCase, [FilePath])]
fixtureCases = [
      (toTestCase libraryCase, includeDirsOf libraryCase)
    | libraryCase <- cases
    ]
