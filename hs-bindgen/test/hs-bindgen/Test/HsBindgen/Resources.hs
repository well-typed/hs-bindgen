-- | Test resources (for integration with tasty)
--
-- Intended for unqualified import.
module Test.HsBindgen.Resources (
    -- * C standards
    CStandard
  , c89
  , gnu89
  , c95
  , c99
  , gnu99
  , c11
  , gnu11
  , c17
  , gnu17
  , c23
  , gnu23
    -- * Test resources
  , TestResources(..)
  , withTestResources
  , getTestClangArgsConfig
    -- * Backend configuration
  , getTestDefaultBackendConfig
  , getTestThBackendConfig
  , applyTestThCategoryChoice
  , testThCategoryChoice
  ) where

import System.FilePath ((</>))
import Test.Tasty

import HsBindgen.Backend.Category (ByCategory (..), Choice (..),
                                   RenameTerm (..))
import HsBindgen.Backend.Hs.Haddock.Config
import HsBindgen.Config.ClangArgs
import HsBindgen.Config.Internal
import HsBindgen.Imports

import Test.Common.Util.Cabal

{-------------------------------------------------------------------------------
  CStandard
-------------------------------------------------------------------------------}

-- | C standard string, as accepted by the Clang @-std@ option
--
-- To see what C standards are supported by a specific version of Clang, run
-- @clang -x c -std=mediocrity -@ to get a list.  Note that deprecated C
-- standard strings are not listed even though they are still supported.  For
-- example, when using Clang 21, @c2x@ is /not/ listed because that version
-- supports @c23@.  It still accepts @c2x@, however, treating it the same as
-- @c23@.  Specifying @c2x@ in test configuration is a convenient way to make
-- the test work across versions of Clang where some versions support @c2x@ but
-- not @c23@.
type CStandard = String

c89, gnu89, c95 :: CStandard
c89   = "c89"
gnu89 = "gnu89"
c95   = "iso9899:199409"

c99, gnu99 :: CStandard
c99   = "c99"
gnu99 = "gnu99"

c11, gnu11 :: CStandard
c11   = "c11"
gnu11 = "gnu11"

c17, gnu17 :: CStandard
c17   = "c17"
gnu17 = "gnu17"

c23, gnu23 :: CStandard
c23   = "c2x"
gnu23 = "gnu2x"

{-------------------------------------------------------------------------------
  TestResources
-------------------------------------------------------------------------------}

data TestResources = TestResources {
      -- | Package root
      packageRoot :: FilePath

      -- | Common Clang arguments
      --
      -- This configures the Clang arguments that are common to all tests.
    , clangArgsConfig :: ClangArgsConfig FilePath
    }

withTestResources :: (IO TestResources -> TestTree) -> TestTree
withTestResources = withResource initTestResources freeTestResources
  where
    initTestResources :: IO TestResources
    initTestResources = do
      packageRoot <- findPackageDirectory "hs-bindgen"
      return TestResources{
          packageRoot     = packageRoot
        , clangArgsConfig = def{
              builtinIncDir    = BuiltinIncDirDisable
            , extraIncludeDirs = [packageRoot </> "musl-include/x86_64"]
            , argsAfter        = ["-target", "x86_64-pc-linux-musl"]
            }
        }

    freeTestResources :: TestResources -> IO ()
    freeTestResources _ = return ()

getTestClangArgsConfig ::
     CStandard
  -> [FilePath]  -- ^ Include directories, relative to the package root
  -> TestResources
  -> ClangArgsConfig FilePath
getTestClangArgsConfig cStandard includeDirs testResources =
    testResources.clangArgsConfig{
        -- NOTE: The C include search path is searched from left to right:
        -- earlier flags overrule later flags.  We therefore configure the
        -- test-specific include directories before the common include
        -- directories.
        extraIncludeDirs =
             map ((</>) testResources.packageRoot) includeDirs
          <> testResources.clangArgsConfig.extraIncludeDirs
      , argsAfter =
             testResources.clangArgsConfig.argsAfter
          <> ["-std=" ++ cStandard]
      }

{-------------------------------------------------------------------------------
  Backend configuration
-------------------------------------------------------------------------------}

getTestDefaultBackendConfig :: TestName -> PathStyle -> BackendConfig
getTestDefaultBackendConfig testName pathStyle = def{
      -- Honor 'maxUniqueIdLength'.
      uniqueId = UniqueId (take 35 $ "test." <> testName)
    , haddock  = HaddockConfig pathStyle
    }

getTestThBackendConfig :: TestName -> PathStyle -> BackendConfig
getTestThBackendConfig testName pathStyle =
    (getTestDefaultBackendConfig testName pathStyle) {
        categoryChoice = testThCategoryChoice
      }

-- | Apply TH-specific category choice to an existing 'BackendConfig',
-- preserving all other settings (e.g. 'fieldNamingStrategy').
applyTestThCategoryChoice :: BackendConfig -> BackendConfig
applyTestThCategoryChoice cfg = cfg { categoryChoice = testThCategoryChoice }

testThCategoryChoice :: ByCategory Choice
testThCategoryChoice = ByCategory {
      cType   = IncludeTypeCategory
    , cSafe   = IncludeTermCategory $ RenameTerm (<> "_safe")
    , cUnsafe = IncludeTermCategory $ RenameTerm (<> "_unsafe")
    , cFunPtr = IncludeTermCategory $ RenameTerm (<> "_funptr")
    , cGlobal = IncludeTermCategory def
    }
