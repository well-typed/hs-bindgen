module HsBindgen.Config
  ( -- * Frontend
    FrontendConfig (..)
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
import HsBindgen.Frontend.Pass.Select.IsPass (ProgramSlicing)
import HsBindgen.Frontend.Predicate (ParsePredicate, SelectPredicate)
import HsBindgen.Util.Tracer

-- TODO_PR: BindgenConfig

-- | Configuration of @hs-bindgen@.
--
-- 'FrontendConfig' determines the "how", not the "what". For example, it should state how
-- we process a header file, but not state which headers we want to process.
--
-- 'FrontendConfig' should contain user-provided data, not @hs-bindgen@-provided data.
data FrontendConfig = FrontendConfig {
      frontendConfigClangArgs       :: ClangArgs
    , frontendConfigParsePredicate  :: ParsePredicate
    , frontendConfigSelectPredicate :: SelectPredicate
    , frontendConfigProgramSlicing  :: ProgramSlicing
    }
  deriving stock (Show, Generic)

instance Default FrontendConfig

{-------------------------------------------------------------------------------
  Backend configuration
-------------------------------------------------------------------------------}

data BackendConfig = BackendConfig {
      backendConfigTranslationOpts :: TranslationOpts
    , backendConfigHsModuleOpts    :: HsModuleOpts
    }
  deriving stock (Show, Generic)

instance Default BackendConfig

checkBackendConfig :: Tracer IO BackendConfigMsg -> BackendConfig -> IO ()
checkBackendConfig tracer backendConfig =
    checkUniqueId (contramap BackendConfigUniqueId tracer) uniqueId
  where
    uniqueId :: UniqueId
    uniqueId = translationUniqueId $ backendConfigTranslationOpts backendConfig

data BackendConfigMsg = BackendConfigUniqueId UniqueIdMsg
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (PrettyForTrace, HasDefaultLogLevel, HasSource)
