module HsBindgen.Boot
  ( boot
  , BootArtefact (..)
  , BootMsg (..)
  ) where

import GHC.Generics (Generic)

import Clang.Args
import HsBindgen.BindingSpec
import HsBindgen.Config
import HsBindgen.Frontend.RootHeader
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
  -> Config
  -> BindingSpecConfig
  -> [UncheckedHashIncludeArg]
  -> IO BootArtefact
boot tracer config bindingSpecConfig uncheckedHashIncludeArgs = do
    let tracerConfig :: Tracer IO ConfigMsg
        tracerConfig = contramap BootConfig tracer
    checkConfig tracerConfig config
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
    clangArgs = configClangArgs config

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
    BootConfig         ConfigMsg
  | BootHashIncludeArg HashIncludeArgMsg
  | BootBindingSpec    BindingSpecMsg
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyForTrace, HasDefaultLogLevel, HasSource)
