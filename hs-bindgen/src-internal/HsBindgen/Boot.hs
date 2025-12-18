module HsBindgen.Boot
  ( boot
  , ClangArtefacts(..)
  , getClangArtefacts
  , BootArtefact (..)
  , BootTargetMsg (..)
  , BootMsg (..)
  ) where

import Control.Exception (Exception (..))
import Control.Monad ((<=<))
import Data.Text qualified as Text
import Text.SimplePrettyPrint (CtxDoc, (><))
import Text.SimplePrettyPrint qualified as PP

import Clang.Args
import Clang.LowLevel.Core

import HsBindgen.Backend.Category (Category (..))
import HsBindgen.BindingSpec
import HsBindgen.Cache
import HsBindgen.Clang
import HsBindgen.Clang.BuiltinIncDir
import HsBindgen.Clang.CompareVersions (CompareVersionsMsg,
                                        compareClangVersions)
import HsBindgen.Clang.CStandard
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
boot ::
     Tracer BootMsg
  -> BindgenConfig
  -> [UncheckedHashIncludeArg]
  -> IO BootArtefact
boot
  tracer
  bindgenConfig@BindgenConfig{..}
  uncheckedHashIncludeArgs = do
    traceStatus $ BootStatusStart bindgenConfig

    checkBackendConfig (contramap BootBackendConfig tracer) bindgenBackendConfig

    getHashIncludeArgs <- cache "hashIncludeArgs" $ Cached $ do
      let tracer' = contramap BootHashIncludeArg tracer
      withTrace BootStatusHashIncludeArgs $
        mapM (hashIncludeArgWithTrace tracer') uncheckedHashIncludeArgs

    getClangArtefacts' <- cache "clangArtefacts" $ Cached $
      getClangArtefacts tracer clangArgsConfig

    getCStandard <- cache "cStandard" $ withTrace BootStatusCStandard $
      clangArtefactsCStandard <$> getClangArtefacts'

    getTarget <- cache "target" $ withTrace BootStatusTarget $
      clangArtefactsTarget <$> getClangArtefacts'

    getClangArgs <- cache "clangArgs" $ withTrace BootStatusClangArgs $
      clangArtefactsArgs <$> getClangArtefacts'

    getBindingSpecs <- cache "loadBindingSpecs" $ do
      clangArgs <- getClangArgs
      target <- getTarget
      liftIO $ loadBindingSpecs
        (contramap BootBindingSpec tracer)
        clangArgs
        target
        (fromBaseModuleName baseModuleName (Just CType))
        (bootBindingSpecConfig bindgenBootConfig)

    getExternalBindingSpecs <- cache "getExternalBindingSpecs" $
      withTrace BootStatusExternalBindingSpecs $
        fmap fst $ getBindingSpecs

    getPrescriptiveBindingSpec <- cache "getPrescriptiveBindingSpec" $
      withTrace BootStatusPrescriptiveBindingSpec $
        fmap snd $ getBindingSpecs

    pure BootArtefact {
          bootBaseModule              = baseModuleName
        , bootCStandard               = getCStandard
        , bootTarget                  = getTarget
        , bootClangArgs               = getClangArgs
        , bootHashIncludeArgs         = getHashIncludeArgs
        , bootExternalBindingSpecs    = getExternalBindingSpecs
        , bootPrescriptiveBindingSpec = getPrescriptiveBindingSpec
        }
  where
    clangArgsConfig :: ClangArgsConfig FilePath
    clangArgsConfig = bindgenBootConfig.bootClangArgsConfig

    baseModuleName :: BaseModuleName
    baseModuleName = bindgenBootConfig.bootBaseModuleName

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
    clangArtefactsCStandard :: ClangCStandard
  , clangArtefactsTarget    :: ClangArgs.Target
  , clangArtefactsArgs      :: ClangArgs
  }

-- | Determine Clang artefacts
getClangArtefacts ::
     Tracer BootMsg
  -> ClangArgsConfig FilePath
  -> IO ClangArtefacts
getClangArtefacts tracer config0 = do
    compareClangVersions (contramap BootCompareClangVersions tracer)
    -- Apply extra Clang arguments and builtin include directory to the config
    extraClangArgs <- getExtraClangArgs tracerExtraClangArgs
    mBuiltinIncDir <- getBuiltinIncDir tracerBuiltinIncDir $
      ClangArgs.builtinIncDir config0
    let config =
            applyExtraClangArgs extraClangArgs
          . applyBuiltinIncDir  mBuiltinIncDir
          $ config0
    -- Determine Clang arguments for the config
    clangArgs <- either throwIO return $ ClangArgs.getClangArgs config
    -- Determine C standard using @libclang@
    clangArtefactsCStandard <- getClangCStandard' tracer clangArgs
    -- Use specified target or determine using @libclang@
    clangArtefactsTarget <-
      maybe (getClangTarget tracer clangArgs) return $
        ClangArgs.target config
    -- Reclaculate Clang arguments if necessary
    clangArtefactsArgs <-
      if isJust config.target
        then return clangArgs
        else either throwIO return $ ClangArgs.getClangArgs config {
            ClangArgs.target    = Just clangArtefactsTarget
          }
    return ClangArtefacts{..}
  where
    tracerBuiltinIncDir :: Tracer BuiltinIncDirMsg
    tracerBuiltinIncDir = contramap BootBuiltinIncDir tracer

    tracerExtraClangArgs :: Tracer ExtraClangArgsMsg
    tracerExtraClangArgs = contramap BootExtraClangArgs tracer

-- | Fatal exceptions
data FatalException =
    UnableToDetermineCStandardException
  | UnableToDetermineTargetException
  deriving Show

instance Exception FatalException where
  displayException = \case
    UnableToDetermineCStandardException -> "Unable to determine C standard"
    UnableToDetermineTargetException    -> "Unable to determine target"

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

-- | Determine the target using @libclang@
--
-- This function throws 'UnableToDetermineTargetException' if the call to
-- @libclang@ fails or the target triple returned by @libclang@ does not
-- translate to a supported target.
getClangTarget :: Tracer BootMsg -> ClangArgs -> IO ClangArgs.Target
getClangTarget tracer clangArgs = do
    tt <- maybe (throwIO UnableToDetermineTargetException) return
      <=< withClang (contramap BootClang tracer) setup $ \unit -> Just <$>
        bracket
          (clang_getTranslationUnitTargetInfo unit)
          clang_TargetInfo_dispose
          clang_TargetInfo_getTriple
    case ClangArgs.parseTargetTripleLenient (Text.unpack tt) of
      Just target -> do
        traceWith tracerTarget $ BootTargetClang tt target
        return target
      Nothing -> do
        traceWith tracerTarget $ BootTargetFail tt
        throwIO UnableToDetermineTargetException
  where
    tracerTarget :: Tracer BootTargetMsg
    tracerTarget = contramap BootTarget tracer

    setup :: ClangSetup
    setup = defaultClangSetup clangArgs $
      ClangInputMemory "hs-bindgen-boot-target.h" ""

{-------------------------------------------------------------------------------
  Artefact
-------------------------------------------------------------------------------}

data BootArtefact = BootArtefact {
    bootBaseModule              :: BaseModuleName
  , bootCStandard               :: Cached ClangCStandard
  , bootTarget                  :: Cached ClangArgs.Target
  , bootClangArgs               :: Cached ClangArgs
  , bootHashIncludeArgs         :: Cached [HashIncludeArg]
  , bootExternalBindingSpecs    :: Cached MergedBindingSpecs
  , bootPrescriptiveBindingSpec :: Cached PrescriptiveBindingSpec
  }

{-------------------------------------------------------------------------------
  Trace
-------------------------------------------------------------------------------}

data BootStatusMsg =
    BootStatusStart                   BindgenConfig
  | BootStatusCStandard               ClangCStandard
  | BootStatusTarget                  ClangArgs.Target
  | BootStatusClangArgs               ClangArgs
  | BootStatusHashIncludeArgs         [HashIncludeArg]
  | BootStatusExternalBindingSpecs    MergedBindingSpecs
  | BootStatusPrescriptiveBindingSpec PrescriptiveBindingSpec
  deriving stock (Show, Generic)

bootStatus :: Show a => String -> a -> CtxDoc
bootStatus nm x =
  PP.hang ("Boot status (" >< PP.string nm >< "):") 2 $ PP.showToCtxDoc x

instance PrettyForTrace BootStatusMsg where
  prettyForTrace = \case
    BootStatusStart                   x -> bootStatus "BindgenConfig"           x
    BootStatusCStandard               x -> bootStatus "ClangCStandard"          x
    BootStatusTarget                  x -> bootStatus "Target"                  x
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
      "C standard determined by libclang: " >< PP.showToCtxDoc std
    BootCStandardFail ->
      "Unable to determine C standard"

instance IsTrace Level BootCStandardMsg where
  getDefaultLogLevel = \case
    BootCStandardClang{} -> Info
    BootCStandardFail    -> Error

  getSource  = const HsBindgen
  getTraceId = const "boot-c-standard"

data BootTargetMsg =
    BootTargetClang Text ClangArgs.Target
  | BootTargetFail  Text
  deriving stock (Show, Generic)

instance PrettyForTrace BootTargetMsg where
  prettyForTrace = \case
    BootTargetClang tt t ->
      "Target determined by libclang: " >< PP.showToCtxDoc t
        >< " (translated from " >< PP.showToCtxDoc tt >< ")"
    BootTargetFail tt ->
      "Unable to translate libclang target triple " >< PP.showToCtxDoc tt
        >< " to supported target"
        PP.$$ "  Configure a supported target to resolve this error."

instance IsTrace Level BootTargetMsg where
  getDefaultLogLevel = \case
    BootTargetClang{} -> Info
    BootTargetFail{}  -> Error

  getSource  = const HsBindgen
  getTraceId = const "boot-target"

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
  | BootTarget               BootTargetMsg
  | BootCache                (SafeTrace CacheMsg)
  deriving stock (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)
