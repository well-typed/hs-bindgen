{-# LANGUAGE OverloadedLabels #-}

-- | Configuration of @hs-bindgen@.
module HsBindgen.Config (
    Config_(..)
  , UniqueId(..)
  , BaseModuleName(..)
  , toBindgenConfig
  , OutputDirPolicy (..)
  , FileOverwritePolicy (..)
    -- * Template Haskell
  , ConfigTH(..)
  )
where

import HsBindgen.Backend.Hs.Haddock.Config
import HsBindgen.Backend.Hs.Translation.Config
import HsBindgen.Backend.SHs.AST
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

-- | User-provided configuration shared between client commands and
--   Template-Haskell mode
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

toBindgenConfig :: Config_ FilePath -> OutputDirPolicy -> FileOverwritePolicy -> UniqueId -> BaseModuleName -> BindgenConfig
toBindgenConfig Config{..} outputDirPolicy fop uniqueId baseModuleName =
    BindgenConfig bootConfig frontendConfig backendConfig
  where
    bootConfig = BootConfig {
        bootClangArgsConfig   = clang
      , bootBaseModuleName    = baseModuleName
      , bootBindingSpecConfig = bindingSpec
      }
    frontendConfig = FrontendConfig {
          frontendParsePredicate  = parsePredicate
        , frontendSelectPredicate = selectPredicate
        , frontendProgramSlicing  = programSlicing
      }
    backendConfig :: BackendConfig
    backendConfig = BackendConfig {
        backendTranslationConfig = def {
            translationUniqueId = uniqueId
          }
      , backendHaddockConfig = HaddockConfig {
            pathStyle = haddockPathStyle
          }
      , backendOutputDirPolicy = outputDirPolicy
      , backendFileOverwrite = fop
      }

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
