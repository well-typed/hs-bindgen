module HsBindgen.Boot (
    runBoot
  , getClangArgs
  , BootArtefact (..)
  , BootMsg (..)
  ) where

import Text.SimplePrettyPrint (CtxDoc, (><))
import Text.SimplePrettyPrint qualified as PP

import Clang.Args

import HsBindgen.Backend.Category (Category (..))
import HsBindgen.BindingSpec
import HsBindgen.Cache
import HsBindgen.Clang
import HsBindgen.Clang.BuiltinIncDir
import HsBindgen.Clang.CompareVersions
import HsBindgen.Clang.ExtraClangArgs
import HsBindgen.Config.ClangArgs (ClangArgsConfig)
import HsBindgen.Config.ClangArgs qualified as ClangArgs
import HsBindgen.Config.Internal
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
runBoot ::
     Tracer BootMsg
  -> BindgenConfig
  -> [UncheckedHashIncludeArg]
  -> IO BootArtefact
runBoot tracer config uncheckedHashIncludeArgs = do
    traceStatus $ BootStatusStart config

    checkBackendConfig (contramap BootBackendConfig tracer) config.backend

    getHashIncludeArgs <- cache "hashIncludeArgs" $ Cached $ do
      let tracer' = contramap BootHashIncludeArg tracer
      withTrace BootStatusHashIncludeArgs $
        mapM (hashIncludeArgWithTrace tracer') uncheckedHashIncludeArgs

    getClangArgs' <- cache "clangArgs" $ Cached $
      getClangArgs tracer config.boot.clangArgs

    getBindingSpecs <- cache "loadBindingSpecs" $ do
      clangArgs <- getClangArgs'
      liftIO $ loadBindingSpecs
        (contramap BootBindingSpec tracer)
        clangArgs
        (fromBaseModuleName config.boot.baseModule (Just CType))
        config.boot.bindingSpec

    getExternalBindingSpecs <- cache "getExternalBindingSpecs" $
      withTrace BootStatusExternalBindingSpecs $
        fmap fst $ getBindingSpecs

    getPrescriptiveBindingSpec <- cache "getPrescriptiveBindingSpec" $
      withTrace BootStatusPrescriptiveBindingSpec $
        fmap snd $ getBindingSpecs

    pure BootArtefact {
          baseModule              = config.boot.baseModule
        , cStandard               = config.boot.clangArgs.cStandard
        , clangArgs               = getClangArgs'
        , hashIncludeArgs         = getHashIncludeArgs
        , externalBindingSpecs    = getExternalBindingSpecs
        , prescriptiveBindingSpec = getPrescriptiveBindingSpec
        }
  where
    tracerBootStatus :: Tracer BootStatusMsg
    tracerBootStatus = contramap BootStatus tracer

    traceStatus :: MonadIO m => BootStatusMsg -> m ()
    traceStatus = traceWith tracerBootStatus

    withTrace :: MonadIO m => (a -> BootStatusMsg) -> m a -> m a
    withTrace c a = do
      x <- a
      traceStatus $ c x
      pure x

    cache :: String -> Cached a -> IO (Cached a)
    cache = cacheWith (contramap (BootCache . SafeTrace) tracer) . Just

-- | Determine Clang arguments
getClangArgs :: Tracer BootMsg -> ClangArgsConfig FilePath -> IO ClangArgs
getClangArgs tracer config0 = do
    compareClangVersions (contramap BootCompareClangVersions tracer)
    -- Apply extra Clang arguments and builtin include directory to the config
    extraClangArgs <- getExtraClangArgs tracerExtraClangArgs
    mBuiltinIncDir <- getBuiltinIncDir tracerBuiltinIncDir config0.builtinIncDir
    let config =
            applyExtraClangArgs extraClangArgs
          . applyBuiltinIncDir  mBuiltinIncDir
          $ config0
    -- Determine Clang arguments for the config
    either throwIO return $ ClangArgs.clangArgsConfigToClangArgs config
  where
    tracerBuiltinIncDir :: Tracer BuiltinIncDirMsg
    tracerBuiltinIncDir = contramap BootBuiltinIncDir tracer

    tracerExtraClangArgs :: Tracer ExtraClangArgsMsg
    tracerExtraClangArgs = contramap BootExtraClangArgs tracer

{-------------------------------------------------------------------------------
  Artefact
-------------------------------------------------------------------------------}

data BootArtefact = BootArtefact {
      baseModule              :: BaseModuleName
    , cStandard               :: CStandard
    , clangArgs               :: Cached ClangArgs
    , hashIncludeArgs         :: Cached [HashIncludeArg]
    , externalBindingSpecs    :: Cached MergedBindingSpecs
    , prescriptiveBindingSpec :: Cached PrescriptiveBindingSpec
    }

{-------------------------------------------------------------------------------
  Trace
-------------------------------------------------------------------------------}

data BootStatusMsg =
    BootStatusStart                   BindgenConfig
  | BootStatusClangArgs               ClangArgs
  | BootStatusHashIncludeArgs         [HashIncludeArg]
  | BootStatusExternalBindingSpecs    MergedBindingSpecs
  | BootStatusPrescriptiveBindingSpec PrescriptiveBindingSpec
  deriving stock (Show, Generic)

bootStatus :: Show a => String -> a -> CtxDoc
bootStatus nm x =
  PP.hang ("Boot status (" >< PP.string nm >< "):") 2 $ PP.show x

instance PrettyForTrace BootStatusMsg where
  prettyForTrace = \case
    BootStatusStart                   x -> bootStatus "BindgenConfig"           x
    BootStatusClangArgs               x -> bootStatus "ClangArgs"               x
    BootStatusHashIncludeArgs         x -> bootStatus "HashIncludeArgs"         x
    BootStatusExternalBindingSpecs    x -> bootStatus "ExternalBindingSpecs"    x
    BootStatusPrescriptiveBindingSpec x -> bootStatus "PrescriptiveBindingSpec" x

instance IsTrace Level BootStatusMsg where
  getDefaultLogLevel = const Debug
  getSource          = const HsBindgen
  getTraceId         = const "boot-status"

-- | Boot trace messages
data BootMsg =
    BootBackendConfig        BackendConfigMsg
  | BootBindingSpec          BindingSpecMsg
  | BootBuiltinIncDir        BuiltinIncDirMsg
  | BootClang                ClangMsg
  | BootExtraClangArgs       ExtraClangArgsMsg
  | BootHashIncludeArg       HashIncludeArgMsg
  | BootCompareClangVersions CompareVersionsMsg
  | BootStatus               BootStatusMsg
  | BootCache                (SafeTrace CacheMsg)
  deriving stock (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)
