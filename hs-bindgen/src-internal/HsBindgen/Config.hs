{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE NoNamedFieldPuns  #-}
{-# LANGUAGE NoRecordWildCards #-}

-- | Configuration of @hs-bindgen@.
module HsBindgen.Config (
    Config_(..)
  , UniqueId(..)
  , BaseModuleName(..)
  , BackendConfig(..)
  , toBindgenConfig
    -- * Template Haskell
  , ConfigTH(..)
  )
where

import HsBindgen.Backend.Category
import HsBindgen.Backend.Hs.Haddock.Config
import HsBindgen.Backend.Hs.Translation.Config
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
  deriving stock (Eq, Show, Generic)
  deriving stock (Functor, Foldable, Traversable)
  deriving anyclass (Default)

toBindgenConfig ::
     Config_ FilePath
  -> UniqueId
  -> BaseModuleName
  -> ByCategory Choice
  -> BindgenConfig
toBindgenConfig config uniqueId baseModuleName choice = BindgenConfig{
      boot = BootConfig {
         clangArgs   = config.clang
       , baseModule  = baseModuleName
       , bindingSpec = config.bindingSpec
       }
    , frontend = FrontendConfig {
         parsePredicate  = config.parsePredicate
       , selectPredicate = config.selectPredicate
       , programSlicing  = config.programSlicing
       }
    , backend = BackendConfig {
         translation = def {
             translationUniqueId = uniqueId
           }
       , haddock = HaddockConfig {
             pathStyle = config.haddockPathStyle
           }
       , categoryChoice = choice
       }
    }

{-------------------------------------------------------------------------------
  Template Haskell
-------------------------------------------------------------------------------}

-- NOTE: Stable public API.

-- | Configuration specific to Template-Haskell mode
data ConfigTH = ConfigTH {
    -- | Some identifiers (e.g., identifiers of @safe@ and @unsafe@ foreign
    -- imports) are identical, so we have to choose which ones to generate
    -- bindings for.
    --
    -- We can also include all declarations, carefully renaming identifiers to
    -- avoid name clashes.
    --
    -- Default: 'Category.useSafe'.
    categoryChoice :: ByCategory Choice

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
  , customLogLevels :: [CustomLogLevelSetting]
  }
  deriving stock (Generic)

instance Default ConfigTH where
  def = ConfigTH{
        categoryChoice  = useSafeCategory
      , verbosity       = def
      , customLogLevels = []
      }
