module HsBindgen.Boot
  ( boot
  , getClangArgs
  , BootArtefact (..)
  , BootMsg (..)
  ) where

import Clang.Args
import HsBindgen.BindingSpec
import HsBindgen.Clang.BuiltinIncDir
import HsBindgen.Clang.ExtraClangArgs
import HsBindgen.Config
import HsBindgen.Config.ClangArgs (ClangArgsConfig)
import HsBindgen.Config.ClangArgs qualified as ClangArgs
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Util.Tracer

import Text.SimplePrettyPrint qualified as PP

-- | Boot phase.
--
-- Basic setup and checks.
--
-- - Check arguments to @#include@.
-- - Determine Clang arguments.
-- - Load external and prescriptive binding specifications.
boot ::
     Tracer IO BootMsg
  -> BindgenConfig
  -> [UncheckedHashIncludeArg]
  -> IO BootArtefact
boot tracer bindgenConfig@BindgenConfig{..} uncheckedHashIncludeArgs = do
    traceWith tracerBootStatus $ BootStart bindgenConfig

    checkBackendConfig (contramap BootBackendConfig tracer) bindgenBackendConfig

    hashIncludeArgs <-
      let tracer' = contramap BootHashIncludeArg tracer
      in  mapM (hashIncludeArgWithTrace tracer') uncheckedHashIncludeArgs

    clangArgs <- getClangArgs tracer $ bootClangArgsConfig bindgenBootConfig

    (extSpec, pSpec) <-
      loadBindingSpecs
        (contramap BootBindingSpec tracer)
        clangArgs
        (bootBindingSpecConfig bindgenBootConfig)

    let bootArtefact = BootArtefact {
          bootClangArgs               = clangArgs
        , bootHashIncludeArgs         = hashIncludeArgs
        , bootExternalBindingSpec     = extSpec
        , bootPrescriptiveBindingSpec = pSpec
        }
    traceWith tracerBootStatus $ BootEnd bootArtefact
    return bootArtefact
  where
    tracerBootStatus :: Tracer IO BootStatusMsg
    tracerBootStatus = contramap BootStatus tracer

-- | Determine Clang arguments
getClangArgs :: Tracer IO BootMsg -> ClangArgsConfig -> IO ClangArgs
getClangArgs tracer config = do
    extraClangArgs <- getExtraClangArgs (contramap BootExtraClangArgs tracer) $
      fst <$> ClangArgs.clangTarget config
    -- NOTE getBuiltinIncDir may need extraClangArgs
    let configExtra = applyExtraClangArgs extraClangArgs config
    argsExtra <- either throwIO return $ ClangArgs.getClangArgs configExtra
    mBuiltinIncDir <-
      getBuiltinIncDir
        (contramap BootBuiltinIncDir tracer)
        argsExtra
        (ClangArgs.clangBuiltinIncDir config)
    let configBuiltin = applyBuiltinIncDir mBuiltinIncDir configExtra
    either throwIO return $ ClangArgs.getClangArgs configBuiltin

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
  | BootExtraClangArgs ExtraClangArgsMsg
  | BootHashIncludeArg HashIncludeArgMsg
  | BootStatus         BootStatusMsg
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)
