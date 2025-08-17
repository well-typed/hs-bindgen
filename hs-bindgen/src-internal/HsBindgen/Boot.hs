module HsBindgen.Boot
  ( boot
  , BootArtefact (..)
  , BootMsg (..)
  ) where

import Clang.Args
import HsBindgen.BindingSpec
import HsBindgen.Config
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Util.Tracer

-- | Boot phase.
--
-- Basic setup and checks.
--
-- - Check arguments to @#include@.
--
-- - Load external binding specifications.
--
-- - Load prescriptive binding specifications.
boot ::
     Tracer IO BootMsg
  -> BindgenConfig
  -> [UncheckedHashIncludeArg]
  -> IO BootArtefact
boot
  tracer
  BindgenConfig{..}
  uncheckedHashIncludeArgs = do
    let tracerBackendConfig :: Tracer IO BackendConfigMsg
        tracerBackendConfig = contramap BootBackendConfig tracer
    checkBackendConfig tracerBackendConfig bindgenBackendConfig
    let tracerHashInclude :: Tracer IO HashIncludeArgMsg
        tracerHashInclude = contramap BootHashIncludeArg tracer
    hashIncludeArgs <-
      mapM (hashIncludeArgWithTrace tracerHashInclude) uncheckedHashIncludeArgs
    let tracerBindingSpec :: Tracer IO BindingSpecMsg
        tracerBindingSpec = contramap BootBindingSpec tracer
    (extSpec, pSpec) <-
      loadBindingSpecs tracerBindingSpec clangArgs bindingSpecConfig
    pure BootArtefact {
          bootHashIncludeArgs         = hashIncludeArgs
        , bootExternalBindingSpec     = extSpec
        , bootPrescriptiveBindingSpec = pSpec
        }
  where
    clangArgs :: ClangArgs
    clangArgs = frontendClangArgs bindgenFrontendConfig

    bindingSpecConfig :: BindingSpecConfig
    bindingSpecConfig = bootBindingSpecConfig bindgenBootConfig

{-------------------------------------------------------------------------------
  Artefact
-------------------------------------------------------------------------------}

data BootArtefact = BootArtefact {
    bootHashIncludeArgs         :: [HashIncludeArg]
  , bootExternalBindingSpec     :: ExternalBindingSpec
  , bootPrescriptiveBindingSpec :: PrescriptiveBindingSpec
  }

{-------------------------------------------------------------------------------
  Trace
-------------------------------------------------------------------------------}

-- | Boot trace messages
data BootMsg =
    BootBackendConfig  BackendConfigMsg
  | BootHashIncludeArg HashIncludeArgMsg
  | BootBindingSpec    BindingSpecMsg
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)
