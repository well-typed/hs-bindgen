module HsBindgen.Config
  ( -- * Bindgen
    BindgenConfig (..)
    -- * Frontend
  , FrontendConfig (..)
    -- * Backend
  , BackendConfig (..)
  , BackendConfigMsg (..)
  , checkBackendConfig
  ) where

import Data.Default (Default)
import GHC.Generics (Generic)

import Clang.Args
import HsBindgen.Backend.Artefact.HsModule.Translation
import HsBindgen.Backend.Hs.Translation
import HsBindgen.Backend.UniqueId
import HsBindgen.BindingSpec
import HsBindgen.Frontend.Pass.Select.IsPass (ProgramSlicing)
import HsBindgen.Frontend.Predicate (ParsePredicate, SelectPredicate)
import HsBindgen.Util.Tracer

-- | Configuration of @hs-bindgen@.
--
-- 'BindgenConfig' combines all configurable settings of @hs-bindgen@.
data BindgenConfig = BindgenConfig {
      bindgenBindingSpecConfig :: BindingSpecConfig
    , bindgenFrontendConfig    :: FrontendConfig
    , bindgenBackendConfig     :: BackendConfig
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Default

-- | Configuration of frontend of @hs-bindgen@.
--
-- The frontend parses the C code and reifies the C declarations.
--
-- NOTE: 'FrontendConfig' determines the "how", not the "what". For example, it
-- should state how we process a header file, but not state which headers we
-- want to process.
--
-- NOTE: 'FrontendConfig' should contain user-provided data, not
-- @hs-bindgen@-provided data.
data FrontendConfig = FrontendConfig {
      frontendClangArgs       :: ClangArgs
    , frontendParsePredicate  :: ParsePredicate
    , frontendSelectPredicate :: SelectPredicate
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
      backendTranslationOpts :: TranslationOpts
    , backendHsModuleOpts    :: HsModuleOpts
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Default

checkBackendConfig :: Tracer IO BackendConfigMsg -> BackendConfig -> IO ()
checkBackendConfig tracer backendConfig =
    checkUniqueId (contramap BackendConfigUniqueId tracer) uniqueId
  where
    uniqueId :: UniqueId
    uniqueId = translationUniqueId $ backendTranslationOpts backendConfig

data BackendConfigMsg = BackendConfigUniqueId UniqueIdMsg
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (PrettyForTrace, HasDefaultLogLevel, HasSource)
