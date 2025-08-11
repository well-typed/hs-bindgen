module HsBindgen.Boot
  ( boot
  , BootArtefact (..)
  , BootMsg (..)
  ) where

import GHC.Generics (Generic)

import Clang.Args
import HsBindgen.BindingSpec
import HsBindgen.Frontend.RootHeader
import HsBindgen.Util.Tracer

boot ::
     Tracer IO BootMsg
  -> ClangArgs
  -> BindingSpecConfig
  -> [UncheckedHashIncludeArg]
  -> IO BootArtefact
boot tracer clangArgs bindingSpecConfig uncheckedHashIncludeArgs = do
    let tracerHashInclude :: Tracer IO HashIncludeArgMsg
        tracerHashInclude = contramap BootHashIncludeArg tracer
    hashIncludeArgs <-
      mapM (hashIncludeArgWithTrace tracerHashInclude) uncheckedHashIncludeArgs
    let tracerBindingSpec :: Tracer IO BindingSpecMsg
        tracerBindingSpec = contramap BootBindingSpec tracer
    (extSpec, pSpec) <-
      loadBindingSpecs tracerBindingSpec clangArgs bindingSpecConfig
    pure BootArtefact {
        bootHashIncludeArgs = hashIncludeArgs
        , bootExternalBindingSpec = extSpec
        , bootPrescriptiveBindingSpec = pSpec
        }

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

data BootMsg =
    BootHashIncludeArg HashIncludeArgMsg
  | BootBindingSpec BindingSpecMsg
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyForTrace, HasDefaultLogLevel, HasSource)
