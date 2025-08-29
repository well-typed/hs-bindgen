module HsBindgen.Boot
  ( boot
  , BootArtefact (..)
  , BootMsg (..)
  ) where

import Clang.Args
import HsBindgen.BindingSpec
import HsBindgen.Clang.BuiltinIncDir
import HsBindgen.Config
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Util.Tracer

import Text.SimplePrettyPrint qualified as PP

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
boot tracer bindgenConfig@BindgenConfig{..} uncheckedHashIncludeArgs = do
    let tracerBootStatus = contramap BootStatus tracer
    traceWith tracerBootStatus $ BootStart bindgenConfig
    let tracerBackendConfig :: Tracer IO BackendConfigMsg
        tracerBackendConfig = contramap BootBackendConfig tracer
    checkBackendConfig tracerBackendConfig bindgenBackendConfig
    let tracerHashInclude :: Tracer IO HashIncludeArgMsg
        tracerHashInclude = contramap BootHashIncludeArg tracer
    hashIncludeArgs <-
      mapM (hashIncludeArgWithTrace tracerHashInclude) uncheckedHashIncludeArgs
    let tracerBuiltinIncDir :: Tracer IO BuiltinIncDirMsg
        tracerBuiltinIncDir = contramap BootBuiltinIncDir tracer
    clangArgs <-
      applyBuiltinIncDir (frontendClangArgs bindgenFrontendConfig) <$>
        getBuiltinIncDir tracerBuiltinIncDir builtinIncDirConfig
    let tracerBindingSpec :: Tracer IO BindingSpecMsg
        tracerBindingSpec = contramap BootBindingSpec tracer
    (extSpec, pSpec) <-
      loadBindingSpecs tracerBindingSpec clangArgs bindingSpecConfig
    let bootArtefact = BootArtefact {
          bootClangArgs               = clangArgs
        , bootHashIncludeArgs         = hashIncludeArgs
        , bootExternalBindingSpec     = extSpec
        , bootPrescriptiveBindingSpec = pSpec
        }
    traceWith tracerBootStatus $ BootEnd bootArtefact
    pure bootArtefact
  where
    builtinIncDirConfig :: BuiltinIncDirConfig
    builtinIncDirConfig = bootBuiltinIncDirConfig bindgenBootConfig

    bindingSpecConfig :: BindingSpecConfig
    bindingSpecConfig = bootBindingSpecConfig bindgenBootConfig

{-------------------------------------------------------------------------------
  Artefact
-------------------------------------------------------------------------------}

data BootArtefact = BootArtefact {
    bootClangArgs               :: ClangArgs
  , bootHashIncludeArgs         :: [HashIncludeArg]
  , bootExternalBindingSpec     :: ExternalBindingSpec
  , bootPrescriptiveBindingSpec :: PrescriptiveBindingSpec
  }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Trace
-------------------------------------------------------------------------------}

data BootStatusMsg =
    BootStart BindgenConfig
  | BootEnd BootArtefact
  deriving stock (Show, Eq, Generic)

instance PrettyForTrace BootStatusMsg where
  prettyForTrace = \case
    BootStart x -> PP.hang "Booting with configuration" 2 $
                     PP.showToCtxDoc x
    BootEnd   x -> PP.hang "Booted  up; returning boot artefact" 2 $
                     PP.showToCtxDoc x

instance IsTrace Level BootStatusMsg where
  getDefaultLogLevel = const Debug
  getSource          = const HsBindgen
  getTraceId         = const "boot-status"

-- | Boot trace messages
data BootMsg =
    BootBackendConfig  BackendConfigMsg
  | BootBindingSpec    BindingSpecMsg
  | BootBuiltinIncDir  BuiltinIncDirMsg
  | BootHashIncludeArg HashIncludeArgMsg
  | BootStatus         BootStatusMsg
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)
