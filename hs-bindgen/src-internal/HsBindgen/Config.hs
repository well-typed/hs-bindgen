-- | Configuration of @hs-bindgen@.

-- NOTE: This is stable public API.
module HsBindgen.Config (
    Config_(..)

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
data Config_ path = Config {
    -- * Boot
    clang       :: ClangArgsConfig path
  , bindingSpec :: BindingSpecConfig

    -- * Frontend
  , parsePredicate  :: ParsePredicate
  , selectPredicate :: SelectPredicate
  , programSlicing  :: ProgramSlicing

    -- * Backend
    -- | Path style used in Haddock comments.
  , haddockPathStyle :: PathStyle
  }
  deriving stock (Show, Eq, Generic)
  deriving stock (Functor, Foldable, Traversable)
  deriving anyclass (Default)

{-------------------------------------------------------------------------------
  Preprocessor
-------------------------------------------------------------------------------}

-- | Configuration specific to preprocessor mode.
data ConfigPP = ConfigPP {
    uniqueId   :: Maybe UniqueId
  , moduleName :: HsModuleName
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Default)

{-------------------------------------------------------------------------------
  Template Haskell
-------------------------------------------------------------------------------}

-- | Configuration specific to Template-Haskell mode.
data ConfigTH = ConfigTH {
    -- | Foreign import safety.
    safety :: Safety
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Default)
