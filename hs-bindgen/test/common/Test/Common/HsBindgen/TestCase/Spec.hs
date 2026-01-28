-- | Shared test case specification types
--
-- This module provides types that define the configuration needed for
-- test case execution. These types are shared between golden tests and
-- TH fixture compilation tests.
--
module Test.Common.HsBindgen.TestCase.Spec (
    -- * Core types
    Outcome(..)
  , TestCaseSpec(..)
    -- * TH compilation status
  , THStatus(..)
  , thStatus
    -- * Default construction
  , defaultSpec
  ) where

import GHC.Generics (Generic)
import System.FilePath ((<.>), (</>))

import HsBindgen.BindingSpec
import HsBindgen.Config.Internal

{-------------------------------------------------------------------------------
  Core types
-------------------------------------------------------------------------------}

-- | Test outcome
--
data Outcome =
      -- | We expect the test to succeed (with or without output).
      Success
      -- | We expect the test to fail with 'hsBindgenE' returning a 'BindgenError'.
    | FailureBindgen
      -- | We expect the test to fail right after invoking `libclang`.
    | FailureLibclang
  deriving stock (Eq, Show)

-- | Shared test case specification
--
-- This type captures the configuration needed for binding generation that is
-- shared between golden tests and TH fixture compilation tests.
--
data TestCaseSpec = TestCaseSpec {
      -- | Name of the test (in the tasty test tree) and the input header
      name :: String

      -- | Name of the header file, e.g., "foo.h"
    , inputHeader :: String

      -- | Directory that the input header is in, relative to the package root
    , inputDir :: FilePath

      -- | External binding spec files (relative to package root)
    , specExternal :: [FilePath]

      -- | Prescriptive binding spec file (relative to package root)
    , specPrescriptive :: Maybe FilePath

      -- | Configure if the @stdlib@ binding specification should be used
    , specStdlib :: EnableStdlibBindingSpec

      -- | Modify the default boot test configuration
    , onBoot :: BootConfig -> BootConfig

      -- | Modify the default frontend test configuration
    , onFrontend :: FrontendConfig -> FrontendConfig

      -- | Does this test have output, or does it fail?
    , outcome :: Outcome

      -- | Tests that require a specific @libclang@ version
      --
      -- If the predicate does not match, the test is skipped entirely.
    , clangVersion :: Maybe ((Int, Int, Int) -> Bool)
    }
  deriving stock (Generic)

{-------------------------------------------------------------------------------
  TH compilation status
-------------------------------------------------------------------------------}

-- | TH compilation status
--
data THStatus
    = THCompile     -- ^ Compile this fixture
    | THSkip String -- ^ Skip with reason
  deriving stock (Eq, Show)

-- | Determine if a test case should be compiled as TH
--
-- This function checks the outcome and other properties of the test case
-- to determine if it can be compiled using Template Haskell.
--
thStatus :: TestCaseSpec -> THStatus
thStatus spec
  | Success <- spec.outcome
      = THCompile
  | otherwise
      = THSkip "Expected failure test"

{-------------------------------------------------------------------------------
  Default construction
-------------------------------------------------------------------------------}

-- | Create a default test case specification from a file path
--
-- The file path should be relative to the golden examples directory,
-- without the @.h@ extension.
--
defaultSpec :: String -> TestCaseSpec
defaultSpec fp = TestCaseSpec {
      name             = fp
    , inputHeader      = fp <.> "h"
    , inputDir         = "examples" </> "golden"
    , specExternal     = []
    , specPrescriptive = Nothing
    , specStdlib       = EnableStdlibBindingSpec
    , onBoot           = id
    , onFrontend       = id
    , outcome          = Success
    , clangVersion     = Nothing
    }
