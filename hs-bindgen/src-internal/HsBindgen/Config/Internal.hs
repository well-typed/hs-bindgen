module HsBindgen.Config.Internal
  ( -- * Bindgen
    BindgenConfig (..)
    -- * Boot
  , BootConfig (..)
    -- * Frontend
  , FrontendConfig (..)
    -- * Backend
  , BackendConfig (..)
  , BackendConfigMsg (..)
  , checkBackendConfig
    -- * File overwrite policy
  , FileOverwritePolicy (..)
    -- * Re-exports
  , module HsBindgen.Config.Prelims
  ) where

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
      bindgenBootConfig     :: BootConfig
    , bindgenFrontendConfig :: FrontendConfig
    , bindgenBackendConfig  :: BackendConfig
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Default

{-------------------------------------------------------------------------------
  Boot configuration
-------------------------------------------------------------------------------}

data BootConfig = BootConfig {
      bootClangArgsConfig     :: ClangArgsConfig FilePath
    , bootBaseModuleName      :: BaseModuleName
    , bootBindingSpecConfig   :: BindingSpecConfig
    }
  deriving stock (Show, Eq, Generic)

instance Default BootConfig where
  def = BootConfig {
        bootClangArgsConfig   = def
      , bootBaseModuleName    = def
      , bootBindingSpecConfig = def
      }

{-------------------------------------------------------------------------------
  Frontend configuration
-------------------------------------------------------------------------------}

-- | Configuration of frontend of @hs-bindgen@.
--
-- The frontend parses the C code and reifies the C declarations.
data FrontendConfig = FrontendConfig {
      frontendParsePredicate  :: Boolean ParsePredicate
    , frontendSelectPredicate :: Boolean SelectPredicate
    , frontendProgramSlicing  :: ProgramSlicing
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
      backendTranslationConfig :: TranslationConfig
    , backendHaddockConfig     :: HaddockConfig
    , backendFileOverwrite     :: FileOverwritePolicy
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Default

checkBackendConfig :: Tracer BackendConfigMsg -> BackendConfig -> IO ()
checkBackendConfig tracer backendConfig =
    checkUniqueId (contramap BackendConfigUniqueId tracer) uniqueId
  where
    uniqueId :: UniqueId
    uniqueId = translationUniqueId $ backendTranslationConfig backendConfig

data BackendConfigMsg = BackendConfigUniqueId UniqueIdMsg
  deriving stock (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)

{-------------------------------------------------------------------------------
  File overwrite policy
-------------------------------------------------------------------------------}

data FileOverwritePolicy
  = AllowFileOverwrite
  | ProtectExistingFiles
  deriving (Show, Eq)

instance Default FileOverwritePolicy where
  def = ProtectExistingFiles
