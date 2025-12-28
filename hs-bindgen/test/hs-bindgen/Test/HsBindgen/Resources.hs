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
  ) where

import System.FilePath ((</>))
import Test.Tasty

import Clang.Args

import HsBindgen.Backend.Hs.Haddock.Config
import HsBindgen.Backend.Hs.Translation.Config
import HsBindgen.Config.ClangArgs
import HsBindgen.Config.Internal
import HsBindgen.Imports

import Test.Common.Util.Cabal

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data TestResources = TestResources {
      -- | Package root
      packageRoot :: FilePath

      -- | Clang arguments configuration we use when running the tests
      --
      -- NOTE: Individual tests will need to add their required include dirs.
    , clangArgs :: ClangArgsConfig FilePath
    }

{-------------------------------------------------------------------------------
  Acquisition and release
-------------------------------------------------------------------------------}

withTestResources :: (IO TestResources -> TestTree) -> TestTree
withTestResources = withResource initTestResources freeTestResources

initTestResources :: IO TestResources
initTestResources = do
    testPackageRoot <- findPackageDirectory "hs-bindgen"
    return TestResources{
        packageRoot = testPackageRoot
      , clangArgs   = mkTestClangArgsConfig testPackageRoot
      }

freeTestResources :: TestResources -> IO ()
freeTestResources _ = return ()

{-------------------------------------------------------------------------------
  Package root
-------------------------------------------------------------------------------}

getTestPackageRoot :: IO TestResources -> IO FilePath
getTestPackageRoot = fmap (.packageRoot)

{-------------------------------------------------------------------------------
  Clang arguments
-------------------------------------------------------------------------------}

mkTestClangArgsConfig :: FilePath -> ClangArgsConfig FilePath
mkTestClangArgsConfig packageRoot = def {
      target = Just Target_Linux_Musl_X86_64
    , cStandard = C23
    , extraIncludeDirs = [
          packageRoot </> "musl-include/x86_64"
        ]
    }

getTestDefaultClangArgsConfig ::
     IO TestResources
  -> [FilePath]
  -> IO (ClangArgsConfig FilePath)
getTestDefaultClangArgsConfig testResources extraIncludeDirs' =
    aux <$> testResources
  where
    -- NOTE: The include search path is traversed from left to right. That is,
    -- earlier flags overrule later flags, and so, the test-specific include
    -- directories must come before the default include directories.
    aux :: TestResources -> ClangArgsConfig FilePath
    aux resources = resources.clangArgs
        & #extraIncludeDirs .~
               map ((</>) resources.packageRoot) extraIncludeDirs'
            <> resources.clangArgs.extraIncludeDirs

{-------------------------------------------------------------------------------
  Test configuration
-------------------------------------------------------------------------------}

getTestDefaultBackendConfig :: TestName -> PathStyle -> BackendConfig
getTestDefaultBackendConfig testName pathStyle = def{
      -- Honor 'maxUniqueIdLength'.
      translation = def & #uniqueId .~ UniqueId (take 35 $ "test." <> testName)
    , haddock = HaddockConfig pathStyle
    }

