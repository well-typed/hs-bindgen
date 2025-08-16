-- | Test resources (for integration with tasty)
--
-- Intended for unqualified import.
module Test.HsBindgen.Resources (
    TestResources -- opaque
  , withTestResources
    -- * Use the resources
  , getTestPackageRoot
  , getTestDefaultClangArgs
  , getTestDefaultFrontendConfig
  , getTestDefaultBackendConfig
    -- ** rust-bindgen
  , RustBindgenResult(..)
  , callRustBindgen
  ) where

import System.Exit (ExitCode (..))
import Test.Tasty

import Clang.Args
import HsBindgen.Lib

import Test.Common.Util.Cabal
import Test.HsBindgen.Resources.Rust

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data TestResources = TestResources {
      -- | Package root
      testPackageRoot :: FilePath

      -- | Clang arguments we use when running the tests
      --
      -- NOTE: Individual tests will need to add their required include dirs.
    , testClangArgs :: ClangArgs

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
    let testClangArgs = mkTestClangArgs testPackageRoot
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

mkTestClangArgs :: FilePath -> ClangArgs
mkTestClangArgs packageRoot = def {
      clangTarget = Just $
        (Target_Linux_X86_64, TargetEnvOverride "gnu")
    , clangCStandard = Just $
        C23
    , clangExtraIncludeDirs = [
          CIncludeDir (packageRoot </> "musl-include/x86_64")
        ]
    }

getTestDefaultClangArgs :: IO TestResources -> [FilePath] -> IO ClangArgs
getTestDefaultClangArgs testResources extraIncludeDirs =
    aux <$> testResources
  where
    aux :: TestResources -> ClangArgs
    aux TestResources{testPackageRoot, testClangArgs} = testClangArgs{
          clangExtraIncludeDirs =
               -- NOTE: The include search path is traversed from left to right.
               -- That is, earlier flags overrule later flags, and so, the
               -- test-specific include directories must come before the default
               -- include directories.
               map (CIncludeDir . (</>) testPackageRoot) extraIncludeDirs
            <> clangExtraIncludeDirs testClangArgs
        }

{-------------------------------------------------------------------------------
  Test configuration
-------------------------------------------------------------------------------}

getTestDefaultFrontendConfig ::
  IO TestResources -> [FilePath] -> IO FrontendConfig
getTestDefaultFrontendConfig testResources extraIncludeDirs = do
    aux <$> getTestDefaultClangArgs testResources extraIncludeDirs
  where
    aux :: ClangArgs -> FrontendConfig
    aux clangArgs = def{
          frontendClangArgs    = clangArgs
        }

getTestDefaultBackendConfig :: TestName -> BackendConfig
getTestDefaultBackendConfig testName = def{
      backendTranslationOpts = def {
        translationUniqueId = UniqueId $ "test." ++ testName
      }
    , backendHsModuleOpts = HsModuleOpts{hsModuleOptsName = "Example"}
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
  -> ClangArgs
     -- ^ Clang arguments
     --
     -- We take this as an explicit argument rather than calling
     -- 'getTestDefaultClangArgs' here, because individual tests may override
     -- those default arguments.
  -> FilePath
  -> IO RustBindgenResult
callRustBindgen testResources clangArgs input = do
    TestResources{..} <- testResources
    case testRustBindgen of
      RustBindgenInPath     path -> go path
      RustBindgenDownloaded path -> go path
      RustBindgenUnavailable     -> return RustBindgenNotCalled
  where
    go :: FilePath -> IO RustBindgenResult
    go path = do
        (exitCode, stdout, stderr) <- runRustBindgen clangArgs path input
        case exitCode of
          ExitSuccess -> return $ RustBindgenSuccess stdout
          _otherwise  -> return $ RustBindgenFailed exitCode stderr
