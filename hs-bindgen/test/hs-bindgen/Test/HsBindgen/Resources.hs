-- | Test resources (for integration with tasty)
--
-- Intended for unqualified import.
module Test.HsBindgen.Resources (
    TestResources -- opaque
  , withTestResources
    -- * Use the resources
  , getTestPackageRoot
  , getTestDefaultClangArgsConfig
  , getTestDefaultBackendConfig
    -- ** rust-bindgen
  , RustBindgenResult(..)
  , callRustBindgen
  ) where

import Data.Default (Default (..))
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Common.Util.Cabal
import Test.HsBindgen.Resources.Rust
import Test.Tasty

import Clang.Args
import Clang.Paths

import HsBindgen.Backend.Artefact.HsModule.Translation
import HsBindgen.Backend.Hs.Haddock.Config
import HsBindgen.Backend.Hs.Translation
import HsBindgen.Backend.SHs.AST (Safety (..))
import HsBindgen.Backend.UniqueId
import HsBindgen.Config
import HsBindgen.Config.ClangArgs

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data TestResources = TestResources {
      -- | Package root
      testPackageRoot :: FilePath

      -- | Clang arguments configuration we use when running the tests
      --
      -- NOTE: Individual tests will need to add their required include dirs.
    , testClangArgsConfig :: ClangArgsConfig

      -- | Path to @rust-bindgen@, if available
    , testRustBindgen :: RustBindgen
    }

{-------------------------------------------------------------------------------
  Acquisition and release
-------------------------------------------------------------------------------}

withTestResources :: (IO TestResources -> TestTree) -> TestTree
withTestResources = withResource initTestResources freeTestResources

initTestResources :: IO TestResources
initTestResources = do
    testPackageRoot <- findPackageDirectory "hs-bindgen"
    let testClangArgsConfig = mkTestClangArgsConfig testPackageRoot
    testRustBindgen <- initRustBindgen
    return TestResources{..}

freeTestResources :: TestResources -> IO ()
freeTestResources TestResources{..} =
    freeRustBindgen testRustBindgen

{-------------------------------------------------------------------------------
  Package root
-------------------------------------------------------------------------------}

getTestPackageRoot :: IO TestResources -> IO FilePath
getTestPackageRoot = fmap testPackageRoot

{-------------------------------------------------------------------------------
  Clang arguments
-------------------------------------------------------------------------------}

mkTestClangArgsConfig :: FilePath -> ClangArgsConfig
mkTestClangArgsConfig packageRoot = def {
      clangTarget = Just $
        (Target_Linux_X86_64, TargetEnvOverride "gnu")
    , clangCStandard = Just $
        C23
    , clangExtraIncludeDirs = [
          CIncludeDir (packageRoot </> "musl-include/x86_64")
        ]
    }

getTestDefaultClangArgsConfig ::
     IO TestResources
  -> [FilePath]
  -> IO ClangArgsConfig
getTestDefaultClangArgsConfig testResources extraIncludeDirs =
    aux <$> testResources
  where
    aux :: TestResources -> ClangArgsConfig
    aux TestResources{..} = testClangArgsConfig{
          clangExtraIncludeDirs =
               -- NOTE: The include search path is traversed from left to right.
               -- That is, earlier flags overrule later flags, and so, the
               -- test-specific include directories must come before the default
               -- include directories.
               map (CIncludeDir . (</>) testPackageRoot) extraIncludeDirs
            <> clangExtraIncludeDirs testClangArgsConfig
        }

{-------------------------------------------------------------------------------
  Test configuration
-------------------------------------------------------------------------------}

getTestDefaultBackendConfig :: TestName -> PathStyle -> BackendConfig
getTestDefaultBackendConfig testName pathStyle = def{
      backendTranslationOpts = def {
        translationUniqueId = UniqueId $ "test." ++ testName
      }
    , backendHsModuleOpts = HsModuleOpts{
        hsModuleOptsBaseName  = "Example"
        -- TODO https://github.com/well-typed/hs-bindgen/issues/1089: Tests with
        -- multiple modules.
      , hsModuleOptsModuleOrg = Single Safe
      }
    , backendHaddockConfig = HaddockConfig pathStyle
    }

{-------------------------------------------------------------------------------
  rust-bindgen
-------------------------------------------------------------------------------}

data RustBindgenResult =
    RustBindgenSuccess String          -- ^ stdout on success
  | RustBindgenFailed ExitCode String  -- ^ stderr on failure
  | RustBindgenNotCalled

callRustBindgen ::
     IO TestResources
  -> ClangArgsConfig
     -- ^ Clang arguments configuration
     --
     -- We take this as an explicit argument rather than calling
     -- 'getTestDefaultClangArgsConfig' here, because individual tests may
     -- override the default configuration.
  -> FilePath
  -> IO RustBindgenResult
callRustBindgen testResources clangArgsConfig input = do
    TestResources{..} <- testResources
    case testRustBindgen of
      RustBindgenInPath     path -> go path
      RustBindgenDownloaded path -> go path
      RustBindgenUnavailable     -> return RustBindgenNotCalled
  where
    go :: FilePath -> IO RustBindgenResult
    go path = do
        (exitCode, stdout, stderr) <- runRustBindgen clangArgsConfig path input
        case exitCode of
          ExitSuccess -> return $ RustBindgenSuccess stdout
          _otherwise  -> return $ RustBindgenFailed exitCode stderr
