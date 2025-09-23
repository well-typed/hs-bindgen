module HsBindgen.Boot
  ( boot
  , getClangArgs
  , BootArtefact (..)
  , BootMsg (..)
  ) where

import Text.SimplePrettyPrint (CtxDoc, (><))
import Text.SimplePrettyPrint qualified as PP

import Clang.Args

import HsBindgen.BindingSpec
import HsBindgen.Cache
import HsBindgen.Clang.BuiltinIncDir
import HsBindgen.Clang.ExtraClangArgs
import HsBindgen.Config
import HsBindgen.Config.ClangArgs (ClangArgsConfig)
import HsBindgen.Config.ClangArgs qualified as ClangArgs
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Util.Tracer

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
    traceStatus $ BootStatusStart bindgenConfig

    checkBackendConfig (contramap BootBackendConfig tracer) bindgenBackendConfig

    getHashIncludeArgs <- cache "hashIncludeArgs" $ do
      let tracer' = contramap BootHashIncludeArg tracer
      withTrace BootStatusHashIncludeArgs $
        mapM (hashIncludeArgWithTrace tracer') uncheckedHashIncludeArgs

    getClangArgs' <- cache "clangArgs" $ do
      withTrace BootStatusClangArgs $
        getClangArgs tracer $ bootClangArgsConfig bindgenBootConfig

    getBindingSpecs <- cache "loadBindingSpecs" $ do
      clangArgs <- getClangArgs'
      loadBindingSpecs
        (contramap BootBindingSpec tracer)
        clangArgs
        (bootBindingSpecConfig bindgenBootConfig)

    getExternalBindingSpec <- cache "getExternalBindingSpec" $ do
      withTrace BootStatusExternalBindingSpec $ fmap fst getBindingSpecs

    getPrescriptiveBindingSpec <- cache "getPrescriptiveBindingSpec" $ do
      withTrace BootStatusExternalBindingSpec $ fmap snd getBindingSpecs

    pure BootArtefact {
          bootClangArgs               = getClangArgs'
        , bootHashIncludeArgs         = getHashIncludeArgs
        , bootExternalBindingSpec     = getExternalBindingSpec
        , bootPrescriptiveBindingSpec = getPrescriptiveBindingSpec
        }
  where
    tracerBootStatus :: Tracer IO BootStatusMsg
    tracerBootStatus = contramap BootStatus tracer

    traceStatus :: BootStatusMsg -> IO ()
    traceStatus = traceWith tracerBootStatus

    withTrace :: (a -> BootStatusMsg) -> IO a -> IO a
    withTrace c a = do
      x <- a
      traceStatus $ c x
      pure x

    cache :: String -> IO a -> IO (IO a)
    cache = cacheWith (contramap BootCache tracer) . Just

-- | Determine Clang arguments
getClangArgs :: Tracer IO BootMsg -> ClangArgsConfig -> IO ClangArgs
getClangArgs tracer config = do
    extraClangArgs <- getExtraClangArgs (contramap BootExtraClangArgs tracer) $
      fst <$> ClangArgs.clangTarget config
    mBuiltinIncDir <- getBuiltinIncDir (contramap BootBuiltinIncDir tracer) $
      ClangArgs.clangBuiltinIncDir config
    either throwIO return
      . ClangArgs.getClangArgs
      . applyExtraClangArgs extraClangArgs
      . applyBuiltinIncDir  mBuiltinIncDir
      $ config

{-------------------------------------------------------------------------------
  Artefact
-------------------------------------------------------------------------------}

data BootArtefact = BootArtefact {
    bootClangArgs               :: IO ClangArgs
  , bootHashIncludeArgs         :: IO [HashIncludeArg]
  , bootExternalBindingSpec     :: IO ExternalBindingSpec
  , bootPrescriptiveBindingSpec :: IO PrescriptiveBindingSpec
  }

{-------------------------------------------------------------------------------
  Trace
-------------------------------------------------------------------------------}

data BootStatusMsg =
    BootStatusStart                   BindgenConfig
  | BootStatusClangArgs               ClangArgs
  | BootStatusHashIncludeArgs         [HashIncludeArg]
  | BootStatusExternalBindingSpec     ExternalBindingSpec
  | BootStatusPrescriptiveBindingSpec PrescriptiveBindingSpec
  deriving stock (Show, Generic)

bootStatus :: Show a => String -> a -> CtxDoc
bootStatus nm x =
  PP.hang ("Boot status (" >< PP.string nm >< "):") 2 $ PP.showToCtxDoc x

instance PrettyForTrace BootStatusMsg where
  prettyForTrace = \case
    BootStatusStart                   x -> bootStatus "BindgenConfig"           x
    BootStatusClangArgs               x -> bootStatus "ClangArgs"               x
    BootStatusHashIncludeArgs         x -> bootStatus "HashIncludeArgs"         x
    BootStatusExternalBindingSpec     x -> bootStatus "ExternalBindingSpec"     x
    BootStatusPrescriptiveBindingSpec x -> bootStatus "PrescriptiveBindingSpec" x

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
  | BootCache          CacheMsg
  deriving stock (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)
