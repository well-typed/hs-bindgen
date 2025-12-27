{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE NoNamedFieldPuns  #-}
{-# LANGUAGE NoRecordWildCards #-}

module HsBindgen.Config.Internal (
    -- * Bindgen
    BindgenConfig (..)
    -- * Boot
  , BootConfig (..)
    -- * Frontend
  , FrontendConfig (..)
    -- * Backend
  , BackendConfig (..)
  , BackendConfigMsg (..)
  , checkBackendConfig
    -- * Re-exports
  , module HsBindgen.Config.Prelims
  ) where

import HsBindgen.Backend.Category
import HsBindgen.Backend.Hs.Haddock.Config
import HsBindgen.Backend.Hs.Translation.Config
import HsBindgen.BindingSpec
import HsBindgen.Config.ClangArgs
import HsBindgen.Config.Prelims
import HsBindgen.Frontend.Pass.Select.IsPass (ProgramSlicing)
import HsBindgen.Frontend.Predicate (Boolean, ParsePredicate, SelectPredicate)
import HsBindgen.Imports
import HsBindgen.Util.Tracer

-- | Configuration of @hs-bindgen@.
--
-- 'BindgenConfig' combines all configurable settings of @hs-bindgen@.
--
-- NOTE: Configuration types determine the "how", not the "what". For example,
-- it should state how we process a header file, but not state which headers we
-- want to process.
--
-- NOTE: Configuration types should contain user-provided data, not
-- @hs-bindgen@-provided data. @hs-bindgen@ provides data in the form of
-- artefacts.
data BindgenConfig = BindgenConfig {
      boot     :: BootConfig
    , frontend :: FrontendConfig
    , backend  :: BackendConfig
    }
  deriving stock (Show, Generic)
  deriving anyclass (Default)

{-------------------------------------------------------------------------------
  Boot configuration
-------------------------------------------------------------------------------}

data BootConfig = BootConfig {
      clangArgs   :: ClangArgsConfig FilePath
    , baseModule  :: BaseModuleName
    , bindingSpec :: BindingSpecConfig
    }
  deriving stock (Show, Eq, Generic)

instance Default BootConfig where
  def = BootConfig {
        clangArgs   = def
      , baseModule  = def
      , bindingSpec = def
      }

{-------------------------------------------------------------------------------
  Frontend configuration
-------------------------------------------------------------------------------}

-- | Configuration of frontend of @hs-bindgen@.
--
-- The frontend parses the C code and reifies the C declarations.
data FrontendConfig = FrontendConfig {
      parsePredicate  :: Boolean ParsePredicate
    , selectPredicate :: Boolean SelectPredicate
    , programSlicing  :: ProgramSlicing
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Default

{-------------------------------------------------------------------------------
  Backend configuration
-------------------------------------------------------------------------------}

-- | Configuration of backend of @hs-bindgen@.
--
-- The backend translates the reified C declarations to Haskell declarations.
--
-- See also the notes at 'FrontendConfig'.
data BackendConfig = BackendConfig {
      translation    :: TranslationConfig
    , haddock        :: HaddockConfig
    , categoryChoice :: ByCategory Choice
    }
  deriving stock (Show, Generic)
  deriving anyclass Default

checkBackendConfig :: Tracer BackendConfigMsg -> BackendConfig -> IO ()
checkBackendConfig tracer backendConfig =
    checkUniqueId (contramap BackendConfigUniqueId tracer) uniqueId
  where
    uniqueId :: UniqueId
    uniqueId = translationUniqueId backendConfig.translation

data BackendConfigMsg = BackendConfigUniqueId UniqueIdMsg
  deriving stock (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)
