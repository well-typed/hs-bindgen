module HsBindgen.Boot (
    runBoot
  , ClangArtefacts (..)
  , getClangArtefacts
  , BootArtefact (..)
  , BootMsg (..)
  ) where

import Control.Exception (displayException)
import Text.SimplePrettyPrint (CtxDoc, (><))
import Text.SimplePrettyPrint qualified as PP

import Clang.Args

import HsBindgen.Backend.Category (Category (..))
import HsBindgen.BindingSpec
import HsBindgen.Cache
import HsBindgen.Clang
import HsBindgen.Clang.BuiltinIncDir
import HsBindgen.Clang.CompareVersions (CompareVersionsMsg,
                                        compareClangVersions)
import HsBindgen.Clang.CStandard
import HsBindgen.Clang.ExtraClangArgs
import HsBindgen.Clang.Sizeof (getSizeofs)
import HsBindgen.Config.ClangArgs (ClangArgsConfig)
import HsBindgen.Config.ClangArgs qualified as ClangArgs
import HsBindgen.Config.Internal
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Language.C (Sizeofs)
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

    getClangArtefacts' <- cache "clangArtefacts" $ Cached $
      getClangArtefacts tracer config.boot.clangArgs

    getCStandard <- cache "cStandard" $ withTrace BootStatusCStandard $
      (.cStandard) <$> getClangArtefacts'

    getClangArgs <- cache "clangArgs" $ withTrace BootStatusClangArgs $
      (.clangArgs) <$> getClangArtefacts'

    getBindingSpecs <- cache "loadBindingSpecs" $ do
      clangArgs <- getClangArgs
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

    sizeofs <- cache "sizeofs" $ do
      clangArgs <- getClangArgs
      liftIO $ getSizeofs (contramap BootSizeofs tracer) clangArgs

    pure BootArtefact {
          baseModule              = config.boot.baseModule
        , cStandard               = getCStandard
        , clangArgs               = getClangArgs
        , hashIncludeArgs         = getHashIncludeArgs
        , externalBindingSpecs    = getExternalBindingSpecs
        , prescriptiveBindingSpec = getPrescriptiveBindingSpec
        , sizeofs                 = sizeofs
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

data ClangArtefacts = ClangArtefacts {
    cStandard :: ClangCStandard
  , clangArgs :: ClangArgs
  }

-- | Determine Clang artefacts
getClangArtefacts ::
     Tracer BootMsg
  -> ClangArgsConfig FilePath
  -> IO ClangArtefacts
getClangArtefacts tracer config0 = do
    compareClangVersions (contramap BootCompareClangVersions tracer)
    extraClangArgs <- getExtraClangArgs tracerExtraClangArgs
    mBuiltinIncDir <- getBuiltinIncDir tracerBuiltinIncDir config0.builtinIncDir
    let config =
            applyExtraClangArgs extraClangArgs
          . applyBuiltinIncDir  mBuiltinIncDir
          $ config0
        clangArgs' = ClangArgs.clangArgsConfigToClangArgs config
    cStandard' <- getClangCStandard' tracer clangArgs'
    return ClangArtefacts{
        cStandard = cStandard'
      , clangArgs = clangArgs'
      }
  where
    tracerBuiltinIncDir :: Tracer BuiltinIncDirMsg
    tracerBuiltinIncDir = contramap BootBuiltinIncDir tracer

    tracerExtraClangArgs :: Tracer ExtraClangArgsMsg
    tracerExtraClangArgs = contramap BootExtraClangArgs tracer

-- | Fatal exceptions
data FatalException =
    UnableToDetermineCStandardException
  deriving Show

instance Exception FatalException where
  displayException = \case
    UnableToDetermineCStandardException -> "Unable to determine C standard"

-- | Determine the C standard using @libclang@
--
-- This function throws a 'UnableToDetermineCStandardException' if the call to
-- @libclang@ fails or we are unable to determine the C standard.
getClangCStandard' :: Tracer BootMsg -> ClangArgs -> IO ClangCStandard
getClangCStandard' tracer clangArgs =
    getClangCStandard clangArgs >>= \case
      Just clangCStandard -> do
        traceWith tracerCStandard $ BootCStandardClang clangCStandard
        return clangCStandard
      Nothing -> do
        traceWith tracerCStandard BootCStandardFail
        throwIO UnableToDetermineCStandardException
  where
    tracerCStandard :: Tracer BootCStandardMsg
    tracerCStandard = contramap BootCStandard tracer

{-------------------------------------------------------------------------------
  Artefact
-------------------------------------------------------------------------------}

data BootArtefact = BootArtefact {
      baseModule              :: BaseModuleName
    , cStandard               :: Cached ClangCStandard
    , clangArgs               :: Cached ClangArgs
    , hashIncludeArgs         :: Cached [HashIncludeArg]
    , externalBindingSpecs    :: Cached MergedBindingSpecs
    , prescriptiveBindingSpec :: Cached PrescriptiveBindingSpec
    , sizeofs                 :: Cached Sizeofs
    }

{-------------------------------------------------------------------------------
  Trace
-------------------------------------------------------------------------------}

data BootStatusMsg =
    BootStatusStart                   BindgenConfig
  | BootStatusCStandard               ClangCStandard
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
    BootStatusCStandard               x -> bootStatus "ClangCStandard"          x
    BootStatusClangArgs               x -> bootStatus "ClangArgs"               x
    BootStatusHashIncludeArgs         x -> bootStatus "HashIncludeArgs"         x
    BootStatusExternalBindingSpecs    x -> bootStatus "ExternalBindingSpecs"    x
    BootStatusPrescriptiveBindingSpec x -> bootStatus "PrescriptiveBindingSpec" x

instance IsTrace Level BootStatusMsg where
  getDefaultLogLevel = const Debug
  getSource          = const HsBindgen
  getTraceId         = const "boot-status"

data BootCStandardMsg =
    BootCStandardClang ClangCStandard
  | BootCStandardFail
  deriving stock (Show, Generic)

instance PrettyForTrace BootCStandardMsg where
  prettyForTrace = \case
    BootCStandardClang std ->
      "C standard determined by libclang: " >< PP.show std
    BootCStandardFail ->
      "Unable to determine C standard"

instance IsTrace Level BootCStandardMsg where
  getDefaultLogLevel = \case
    BootCStandardClang{} -> Info
    BootCStandardFail    -> Error

  getSource  = const HsBindgen
  getTraceId = const "boot-c-standard"

-- | Boot trace messages
data BootMsg =
    BootBackendConfig        BackendConfigMsg
  | BootBindingSpec          BindingSpecMsg
  | BootBuiltinIncDir        BuiltinIncDirMsg
  | BootClang                ClangMsg
  | BootCStandard            BootCStandardMsg
  | BootExtraClangArgs       ExtraClangArgsMsg
  | BootHashIncludeArg       HashIncludeArgMsg
  | BootCompareClangVersions CompareVersionsMsg
  | BootStatus               BootStatusMsg
  | BootCache                (SafeTrace CacheMsg)
  | BootSizeofs              ClangMsg
  deriving stock (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)
