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
import HsBindgen.BindingSpec
import HsBindgen.Config.ClangArgs
import HsBindgen.Config.Prelims
import HsBindgen.Frontend.Pass.Select.IsPass (ProgramSlicing)
import HsBindgen.Frontend.Predicate (Boolean, SelectPredicate)
import HsBindgen.Imports
import HsBindgen.Util.Tracer

import Doxygen.Parser qualified as Doxygen

-- | Configuration of @hs-bindgen@.
--
-- t'BindgenConfig' combines all configurable settings of @hs-bindgen@ that are
-- necessary in all modes (CLI and Template Haskell).
--
-- Configuration types determine the "how", not the "what". For example,
-- it should state how we process a header file, but not state which headers we
-- want to process.
--
-- Configuration types should contain user-provided data, not
-- @hs-bindgen@-provided data. @hs-bindgen@ provides data in the form of
-- artefacts.
--
-- These t'BindgenConfig' options are provided /once/ (i.e., the function
-- 'HsBindgen.hsBindgen' runs once). This is, for example, the C standard. In
-- contrast, configuration of external artifacts that may change even for the
-- same @hsBindgen@ run, should be directly provided to these external
-- artifacts. This is, for example, the output file path of the generated
-- bindings.
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
      selectPredicate     :: Boolean SelectPredicate
    , programSlicing      :: ProgramSlicing
    , fieldNamingStrategy :: FieldNamingStrategy
    , doxygenConfig       :: Doxygen.Config
    }
  deriving stock (Show, Eq, Generic)

instance Default FrontendConfig where
  def = FrontendConfig {
      selectPredicate     = def
    , programSlicing      = def
    , fieldNamingStrategy = def
    , doxygenConfig       = Doxygen.defaultConfig
    }

{-------------------------------------------------------------------------------
  Backend configuration
-------------------------------------------------------------------------------}

-- | Configuration of backend of @hs-bindgen@.
--
-- The backend translates the reified C declarations to Haskell declarations.
data BackendConfig = BackendConfig {
      uniqueId            :: UniqueId
    , haddock             :: HaddockConfig
    , categoryChoice      :: ByCategory Choice
    }
  deriving stock (Show, Generic)
  deriving anyclass Default

checkBackendConfig :: Tracer BackendConfigMsg -> BackendConfig -> IO ()
checkBackendConfig tracer backendConfig =
    checkUniqueId
      (contramap BackendConfigUniqueId tracer)
      backendConfig.uniqueId

data BackendConfigMsg = BackendConfigUniqueId UniqueIdMsg
  deriving stock (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)
