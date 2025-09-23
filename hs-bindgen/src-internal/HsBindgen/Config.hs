-- | Configuration of @hs-bindgen@.

-- NOTE: This is stable public API.
module HsBindgen.Config (
    Config(..)

    -- * Preprocessor
  , ConfigPP(..)

    -- * Template Haskell
  , ConfigTH(..)
  )
where

import HsBindgen.Backend.Hs.Haddock.Config
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.UniqueId
import HsBindgen.BindingSpec
import HsBindgen.Config.ClangArgs
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.Language.Haskell

{-------------------------------------------------------------------------------
  Common
-------------------------------------------------------------------------------}

-- | Configuration shared between preprocessor and Template-Haskell modes.
--
-- Stable public API.
data Config path = Config {
    -- * Boot
    configClangArgsConfig         :: ClangArgsConfig path
  , configStdlibSpec              :: EnableStdlibBindingSpec
  , configCompatibility           :: BindingSpecCompatibility
  , configExtBindingSpecs         :: [path]
  , configPrescriptiveBindingSpec :: Maybe path

    -- * Frontend
  , configParsePredicate  :: ParsePredicate
  , configSelectPredicate :: SelectPredicate
  , configProgramSlicing  :: ProgramSlicing

    -- * Backend
    -- | Path style used in Haddock comments.
  , configHaddockPathStyle :: PathStyle
  }
  deriving stock (Show, Eq, Generic)
  deriving stock (Functor, Foldable, Traversable)
  deriving anyclass (Default)

{-------------------------------------------------------------------------------
  Preprocessor
-------------------------------------------------------------------------------}

-- | Configuration specific to preprocessor mode.
data ConfigPP = ConfigPP {
    configPPUniqueId   :: Maybe UniqueId
  , configPPModuleName :: HsModuleName
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Default)

{-------------------------------------------------------------------------------
  Template Haskell
-------------------------------------------------------------------------------}

-- | Configuration specific to Template-Haskell mode.
data ConfigTH = ConfigTH {
    -- | Foreign import safety.
    configTHSafety :: Safety
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Default)
