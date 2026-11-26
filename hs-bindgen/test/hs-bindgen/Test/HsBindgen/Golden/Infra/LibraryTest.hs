-- | Configuring library-mode test cases
--
-- A library test runs @hs-bindgen@ in library mode (@--library@) over the
-- headers of one fixture. The headers of the topic @cycles@ lie in
-- @test-artefacts/headers/library/cycles@, which is the include directory of
-- the run. For the fixture @boot_files@, the root header is @boot_files.h@ and
-- the library directory is @boot_files/@ next to it, so the root header gets no
-- module of its own.
--
-- Intended for unqualified import.
module Test.HsBindgen.Golden.Infra.LibraryTest (
    -- * Definition
    LibraryTest(..)
  , LibraryOutcome(..)
    -- * Construction
  , libraryTest
  , libraryVariant
    -- * Derived
  , generatedSpec
  ) where

import Data.Char qualified as Char
import Data.Text qualified as Text
import System.FilePath

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config.Internal
import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Library (PlanError, bindingSpecFileName)

import Test.HsBindgen.Golden.Infra.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data LibraryTest = LibraryTest {
      -- | The configuration of the run
      --
      -- The test case gives the name of the test, the root header, the include
      -- directory, the golden directory, the trace predicate and the
      -- configuration hooks.
      test           :: TestCase

      -- | The @--library@ directories, relative to the package root
    , libraryDirs    :: [FilePath]

      -- | The @--gen-binding-spec-dir@ of the run, relative to the golden
      -- directory, if the run writes binding specifications
    , bindingSpecDir :: Maybe FilePath

      -- | The library tests whose generated modules this library imports
      --
      -- Fixture compilation puts their golden directories on the search path.
    , dependencies   :: [LibraryTest]

    , outcome        :: LibraryOutcome
    }
  deriving stock (Generic)

data LibraryOutcome =
    -- | The run writes exactly the files of the golden directory
    WritesGolden
    -- | The library plan rejects the run with an error that satisfies the
    -- predicate, and nothing is written
  | RejectedByPlan (PlanError -> Bool)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | A library test that writes its golden directory
--
-- The test is named @library/\<topic\>/\<name\>@, and so is its golden
-- directory under @test-artefacts/fixtures@. The base module is the name in
-- camel case, such as @BootFiles@ for @boot_files@.
libraryTest ::
     FilePath -- ^ Topic, such as @cycles@
  -> String   -- ^ Name of the root header and of the library directory
  -> LibraryTest
libraryTest topic name = LibraryTest{
      -- Test names use @/@ on every platform, as the other golden tests do.
      test           = defaultTest ("library/" ++ topic ++ "/" ++ name)
                         & #inputHeader .~ name <.> "h"
                         & #inputDir    .~ headersDir
                         & #cStandard   .~ c11
                         & #onBoot      .~ ( #baseModule .~ baseModule )
                         & #onFrontend  .~
                             ( #selectionPredicate .~
                                 BIf (SelectHeader FromLibraryHeaders) )
    , libraryDirs    = [headersDir </> name]
    , bindingSpecDir = Nothing
    , dependencies   = []
    , outcome        = WritesGolden
    }
  where
    headersDir :: FilePath
    headersDir = "test-artefacts" </> "headers" </> "library" </> topic

    baseModule :: BaseModuleName
    baseModule = BaseModuleName $ Text.pack $ concatMap capitalise $
      words (map (\c -> if c == '_' then ' ' else c) name)

    capitalise :: String -> String
    capitalise = \case
      c : cs -> Char.toUpper c : cs
      []     -> []

-- | A variant of a library test, with its own name and golden directory
--
-- See 'mkTestVariant'.
libraryVariant :: Natural -> String -> LibraryTest -> LibraryTest
libraryVariant index suffix = #test %~ mkTestVariant (Just index) suffix

{-------------------------------------------------------------------------------
  Derived
-------------------------------------------------------------------------------}

-- | The binding specification that a library test writes for a module,
-- relative to the package root
--
-- Panics if the test writes no binding specifications: that is a mistake in
-- the definition of the test.
generatedSpec :: LibraryTest -> Hs.ModuleName -> FilePath
generatedSpec libraryTest' moduleName =
    case libraryTest'.bindingSpecDir of
      Just dir ->
            libraryTest'.test.outputDir
        </> dir
        </> bindingSpecFileName BindingSpec.FormatYAML moduleName
      Nothing ->
        panicPure $ libraryTest'.test.name ++ " writes no binding specifications"
