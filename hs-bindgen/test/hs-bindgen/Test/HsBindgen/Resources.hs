-- | Test resources (for integration with tasty)
--
-- Intended for unqualified import.
module Test.HsBindgen.Resources (
    TestResources -- opaque
  , withTestResources
    -- * Use the resources
  , getTestPackageRoot
  , getTestDefaultClangArgs
  , getTestDefaultConfig
  , getTestDefaultExtSpec
    -- ** rust-bindgen
  , RustBindgenResult(..)
  , callRustBindgen
  ) where

import System.Exit (ExitCode (..))
import Test.Tasty

import Clang.Args
import HsBindgen.Lib

import Test.Common.HsBindgen.TracePredicate
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

      -- | Default external binding specification
    , testExtSpec :: BindingSpec

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
    testExtSpec     <- initExtBindingSpec testClangArgs
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
    , clangSystemIncludePathDirs = [
          CIncludePathDir (packageRoot </> "musl-include/x86_64")
        ]
    }

getTestDefaultClangArgs :: IO TestResources -> [FilePath] -> IO ClangArgs
getTestDefaultClangArgs testResources extraIncludeDirs =
    aux <$> testResources
  where
    aux :: TestResources -> ClangArgs
    aux TestResources{testPackageRoot, testClangArgs} = testClangArgs{
          clangQuoteIncludePathDirs =
            map (CIncludePathDir . (</>) testPackageRoot) extraIncludeDirs
        }

{-------------------------------------------------------------------------------
  Test configuration
-------------------------------------------------------------------------------}

getTestDefaultConfig :: IO TestResources -> [FilePath] -> IO Config
getTestDefaultConfig testResources extraIncludeDirs = do
    aux <$> getTestDefaultClangArgs testResources extraIncludeDirs
  where
    aux :: ClangArgs -> Config
    aux clangArgs = def{
          configClangArgs    = clangArgs
        , configHsModuleOpts = HsModuleOpts{hsModuleOptsName = "Example"}
        }

{-------------------------------------------------------------------------------
  Binding specifications
-------------------------------------------------------------------------------}

initExtBindingSpec :: ClangArgs -> IO BindingSpec
initExtBindingSpec clangArgs =
    -- This trace predicate is used only during resolution of the default
    -- binding specifications.
    withTracePredicate defaultTracePredicate $ \tracer ->
      loadExtBindingSpecs tracer clangArgs EnableStdlibBindingSpec []

getTestDefaultExtSpec :: IO TestResources -> IO BindingSpec
getTestDefaultExtSpec = fmap testExtSpec

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
