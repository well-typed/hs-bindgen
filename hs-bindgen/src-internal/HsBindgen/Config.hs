{-# LANGUAGE OverloadedLabels #-}

-- | Configuration of @hs-bindgen@.
module HsBindgen.Config (
    Config_(..)
  , toBindgenConfig

    -- * Client
  , OutputDirPolicy(..)

    -- * Template Haskell
  , ConfigTH(..)
  )
where

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
import HsBindgen.Language.Haskell qualified as Hs
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

    -- * Binding specifications
  , outputBindingSpec :: Maybe path
  }
  deriving stock (Show, Eq, Generic)
  deriving stock (Functor, Foldable, Traversable)
  deriving anyclass (Default)

toBindgenConfig :: Config_ FilePath -> UniqueId -> Hs.ModuleName -> BindgenConfig
toBindgenConfig Config{..} uniqueId hsModuleName =
    BindgenConfig bootConfig frontendConfig backendConfig
  where
    bootConfig = BootConfig {
        bootClangArgsConfig   = clang
      , bootHsModuleName      = hsModuleName
      , bootBindingSpecConfig = bindingSpec
      }
    frontendConfig = FrontendConfig {
          frontendParsePredicate  = parsePredicate
        , frontendSelectPredicate = selectPredicate
        , frontendProgramSlicing  = programSlicing
      }
    backendConfig :: BackendConfig
    backendConfig = BackendConfig {
        backendTranslationOpts = def {
            translationUniqueId = uniqueId
          }
      , backendHaddockConfig = HaddockConfig {
            pathStyle = haddockPathStyle
          }
      }

{-------------------------------------------------------------------------------
  Client
-------------------------------------------------------------------------------}

-- NOTE: Stable public API.

data OutputDirPolicy
  = CreateDirStructure
  | DoNotCreateDirStructure
  deriving (Show, Eq)

instance Default OutputDirPolicy where
  def = DoNotCreateDirStructure

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
