-- | One translation unit generated as several modules
--
-- The header @forward_decl/b.h@ includes @a.h@.  The function of @a.h@ takes a
-- struct that @b.h@ defines.  The tests generate a module for each of the two
-- headers from one run, @M.B@ first and then @M.A@, which uses its type.
module Test.HsBindgen.Unit.ModuleScope (tests) where

import Data.List (isInfixOf)
import Data.Set qualified as Set
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import HsBindgen
import HsBindgen.ArtefactM (DirPolicy (..), FilePolicy (..))
import HsBindgen.Backend.Category
import HsBindgen.BindingSpec (ExternalBindingSpec)
import HsBindgen.Config.Internal
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.Macro qualified as Macro
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer (Level)

import Test.Common.HsBindgen.Trace.Patterns
import Test.Common.HsBindgen.Trace.Predicate
import Test.HsBindgen.Golden.Infra.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests getTestResources = testGroup "Test.HsBindgen.Unit.ModuleScope" [
      testRunOfItsOwn  getTestResources
    , testPrescriptive getTestResources
    , testMessages     getTestResources
    ]

-- | A module is the module that a run of its own generates
--
-- The run of its own has the name of the module as its module name, selects
-- its declarations with program slicing, and reads the binding specification
-- of the module before it from the file that is written for it.
testRunOfItsOwn :: IO TestResources -> TestTree
testRunOfItsOwn getTestResources =
  testCase "a module is what a run of its own generates" $
    withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
      let specFileB :: FilePath
          specFileB = tmpDir </> "M.B" <.> "yaml"
      (moduleB, moduleA) <- run forwardDecl $ do
        (specB, moduleB) <- WithModule scopeB $
              (,) <$> generatedBindingSpec <*> modules
          <*  writeBindingSpec DoNotOverwriteFiles DoNotCreateOutputDirs specFileB
        moduleA <- WithModule (scopeA specB) modules
        pure (moduleB, moduleA)

      ownB <- run (runOfItsOwn "M.B" "^struct shape$" []         ) modules
      ownA <- run (runOfItsOwn "M.A" "^shape_draw$"   [specFileB]) modules
      moduleB @?= ownB
      moduleA @?= ownA

      -- The comparison says nothing if both sides are empty.
      assertBool "M.B defines the struct" $
        maybe False ("data Shape" `isInfixOf`) (typesOf moduleB)
      typesOf moduleA @?= Nothing
      assertBool "M.A uses the struct of M.B" $
        maybe False ("M.B.Shape" `isInfixOf`) (safeOf moduleA)
  where
    run ::
         TestCase
      -> (forall l. Macro.HasTypes l => Artefact l a)
      -> IO a
    run = runTestHsBindgenSuccess (const $ pure ()) getTestResources

    safeOf :: ByCategory_ (Maybe String) -> Maybe String
    safeOf = view (lensForCategory (CTerm CSafe))

-- | A prescriptive binding specification that names no module is the module's
--
-- The run as a whole is for another module than the one that is generated.
-- The specification is loaded once, for the run, and still configures the
-- module, without a warning that it is meant for another one.
testPrescriptive :: IO TestResources -> TestTree
testPrescriptive getTestResources =
  testCase "a specification without hsmodule is the module's" $
    withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
      let specFile :: FilePath
          specFile = tmpDir </> "rename" <.> "yaml"

          renamed :: TestCase -> TestCase
          renamed = #specPrescriptive .~ Just specFile
      writeFile specFile $ unlines [
          "version:"
        , "  hs_bindgen: 1.0.0.0"
        , "  binding_specification: '1.0'"
        , "ctypes:"
        , "- headers: b.h"
        , "  cname: struct shape"
        , "  hsname: Polygon"
        ]
      moduleB <- run (renamed forwardDecl) $ WithModule scopeB modules
      ownB    <- run (renamed $ runOfItsOwn "M.B" "^struct shape$" []) modules
      moduleB @?= ownB
      assertBool "M.B defines the struct under its new name" $
        maybe False ("data Polygon" `isInfixOf`) (typesOf moduleB)
  where
    run ::
         TestCase
      -> (forall l. Macro.HasTypes l => Artefact l a)
      -> IO a
    run = runTestHsBindgenSuccess (const $ pure ()) getTestResources

-- | A message from generating a module names the module
testMessages :: IO TestResources -> TestTree
testMessages getTestResources =
  testCase "a message names its module" $
    void $ runTestHsBindgenSuccess
      (const $ pure ())
      getTestResources
      (forwardDecl & #tracePredicate .~ selectedIn)
      (do specB <- WithModule scopeB generatedBindingSpec
          WithModule (scopeA specB) modules)
  where
    selectedIn :: TracePredicate Level TraceMsg
    selectedIn =
      multiTracePredicate [("M.B", "struct shape"), ("M.A", "shape_draw")] $
        \case
          TraceModule (InModule name (MatchSelect decl SelectedAsRoot)) ->
            Just $ Expected (baseModuleNameToString name, decl)
          MatchSelect _ SelectedAsRoot ->
            Just Unexpected
          _otherwise ->
            Nothing

pattern SelectedAsRoot :: SelectMsg
pattern SelectedAsRoot <- SelectStatusInfo (Selected SelectionRoot)

{-------------------------------------------------------------------------------
  The header and its two modules
-------------------------------------------------------------------------------}

forwardDecl :: TestCase
forwardDecl =
    defaultTest "b"
      & #name     .~ "forward_decl"
      & #inputDir .~ "test-artefacts" </> "headers" </> "forward_decl"

scopeB :: ModuleScope
scopeB = ModuleScope{
      baseModule = "M.B"
    , imported   = []
    , roots      = Set.singleton "struct shape"
    }

scopeA :: ExternalBindingSpec -> ModuleScope
scopeA specB = ModuleScope{
      baseModule = "M.A"
    , imported   = [specB]
    , roots      = Set.singleton "shape_draw"
    }

-- | The run that generates one module and nothing else
runOfItsOwn ::
     BaseModuleName
  -> Regex       -- ^ The declarations of the module
  -> [FilePath]  -- ^ External binding specifications
  -> TestCase
runOfItsOwn baseModule decls specs =
    forwardDecl
      & #onBoot       .~ ( #baseModule .~ baseModule )
      & #specExternal .~ specs
      & #onFrontend   .~ (\cfg -> cfg
          & #selectionPredicate .~ BIf (SelectDecl (DeclNameMatches decls))
          & #programSlicing     .~ EnableProgramSlicing
          )

-- | The modules that are generated, one for each binding category
modules :: Artefact l (ByCategory_ (Maybe String))
modules = getBindingsMultiple ModuleRenderConfig{qualifiedStyle = def}

-- | The module that holds the types
typesOf :: ByCategory_ (Maybe String) -> Maybe String
typesOf = view (lensForCategory CType)
