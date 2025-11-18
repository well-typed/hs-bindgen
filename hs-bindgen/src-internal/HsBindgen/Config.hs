{-# LANGUAGE OverloadedLabels #-}

-- | Configuration of @hs-bindgen@.
module HsBindgen.Config (
    Config_(..)
  , toBindgenConfig

    -- * Preprocessor
  , ConfigPP(..)
  , toBindgenConfigPP

    -- * Template Haskell
  , ConfigTH(..)
  )
where

import Optics.Core ((%), (&), (.~))

import HsBindgen.Backend.Hs.Haddock.Config
import HsBindgen.Backend.Hs.Translation
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.UniqueId
import HsBindgen.BindingSpec
import HsBindgen.Config.ClangArgs
import HsBindgen.Config.Internal
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Common
-------------------------------------------------------------------------------}

-- NOTE: Stable public API.

-- | Configuration shared between preprocessor (e.g., the @hs-bindgen@ client:
-- @hs-bindgen-cli@) and Template-Haskell modes
data Config_ path = Config {
    -- * Boot
    clang       :: ClangArgsConfig path
  , bindingSpec :: BindingSpecConfig

    -- * Frontend
  , parsePredicate  :: Boolean ParsePredicate
  , selectPredicate :: Boolean SelectPredicate
  , programSlicing  :: ProgramSlicing

    -- * Backend
  , haddockPathStyle :: PathStyle
  }
  deriving stock (Show, Eq, Generic)
  deriving stock (Functor, Foldable, Traversable)
  deriving anyclass (Default)

toBindgenConfig :: Config_ FilePath -> BindgenConfig
toBindgenConfig Config{..} = BindgenConfig bootConfig frontendConfig backendConfig
  where
    bootConfig = BootConfig {
        bootClangArgsConfig   = clang
      , bootBindingSpecConfig = bindingSpec
      }
    frontendConfig = FrontendConfig {
          frontendParsePredicate  = parsePredicate
        , frontendSelectPredicate = selectPredicate
        , frontendProgramSlicing  = programSlicing
      }
    backendConfig :: BackendConfig
    backendConfig = def {
        backendHaddockConfig = HaddockConfig {
            pathStyle = haddockPathStyle
          }
      }

{-------------------------------------------------------------------------------
  Preprocessor
-------------------------------------------------------------------------------}

-- NOTE: Stable public API.

-- | Configuration specific to preprocessor mode
data ConfigPP = ConfigPP {
    uniqueId :: Maybe UniqueId
  }
  deriving stock (Show, Eq, Generic)

instance Default ConfigPP where
  def = ConfigPP {
      uniqueId   = def
    }

toBindgenConfigPP :: Config_ FilePath -> ConfigPP -> BindgenConfig
toBindgenConfigPP config ConfigPP{..} = bindgenConfig & setUniqueId
  where
    bindgenConfig = toBindgenConfig config
    setUniqueId =
      #bindgenBackendConfig % #backendTranslationOpts % #translationUniqueId
        .~ (fromMaybe def uniqueId)

{-------------------------------------------------------------------------------
  Template Haskell
-------------------------------------------------------------------------------}

-- NOTE: Stable public API.

-- | Configuration specific to Template-Haskell mode
data ConfigTH = ConfigTH {
    -- | Foreign import safety
    --
    -- The generated identifiers of @safe@ and @unsafe@ foreign imports are
    -- identical, so we have to choose one.
    --
    -- Default:
    --
    -- >>> def :: Safety
    -- Safe
    safety :: Safety

    -- | Show trace messages of the provided 'Level' or higher.
    --
    -- Default:
    --
    -- >>> def :: Verbosity
    -- Notice
  , verbosity :: Verbosity

    -- | Custom log level settings
    --
    -- For example, use 'EnableMacroWarnings' to warn on macro parse/reparse
    -- errors.
  , customLogLevelSettings :: [CustomLogLevelSetting]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Default
