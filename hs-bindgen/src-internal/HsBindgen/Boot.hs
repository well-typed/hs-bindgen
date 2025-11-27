module HsBindgen.Boot
  ( boot
  , getClangArgsAndTarget
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

import HsBindgen.BindingSpec
import HsBindgen.Cache
import HsBindgen.Clang
import HsBindgen.Clang.BuiltinIncDir
import HsBindgen.Clang.CompareVersions (CompareVersionsMsg,
                                        compareClangVersions)
import HsBindgen.Clang.ExtraClangArgs
import HsBindgen.Config.ClangArgs (ClangArgsConfig)
import HsBindgen.Config.ClangArgs qualified as ClangArgs
import HsBindgen.Config.Internal
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
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

    getHashIncludeArgs <- cache "hashIncludeArgs" $ do
      let tracer' = contramap BootHashIncludeArg tracer
      withTrace BootStatusHashIncludeArgs $
        mapM (hashIncludeArgWithTrace tracer') uncheckedHashIncludeArgs

    getClangArgsAndTarget' <- cache "clangArgsAndTarget" $
      getClangArgsAndTarget tracer clangArgsConfig

    getClangArgs <- cache "clangArgs" . withTrace BootStatusClangArgs $
      fst <$> getClangArgsAndTarget'

    getTarget <- cache "target" . withTrace BootStatusTarget $
      snd <$> getClangArgsAndTarget'

    getBindingSpecs <- cache "loadBindingSpecs" $ do
      clangArgs <- getClangArgs
      target <- getTarget
      loadBindingSpecs
        (contramap BootBindingSpec tracer)
        clangArgs
        target
        hsModuleName
        (bootBindingSpecConfig bindgenBootConfig)

    getExternalBindingSpecs <- cache "getExternalBindingSpecs" $ do
      withTrace BootStatusExternalBindingSpecs $ fmap fst getBindingSpecs

    getPrescriptiveBindingSpec <- cache "getPrescriptiveBindingSpec" $ do
      withTrace BootStatusPrescriptiveBindingSpec $ fmap snd getBindingSpecs

    pure BootArtefact {
          bootModule                  = hsModuleName
        , bootCStandard               = clangArgsConfig.cStandard
        , bootClangArgs               = getClangArgs
        , bootTarget                  = getTarget
        , bootHashIncludeArgs         = getHashIncludeArgs
        , bootExternalBindingSpecs    = getExternalBindingSpecs
        , bootPrescriptiveBindingSpec = getPrescriptiveBindingSpec
        }
  where
    clangArgsConfig :: ClangArgsConfig FilePath
    clangArgsConfig = bindgenBootConfig.bootClangArgsConfig

    hsModuleName :: Hs.ModuleName
    hsModuleName = bindgenBootConfig.bootHsModuleName

    tracerBootStatus :: Tracer BootStatusMsg
    tracerBootStatus = contramap BootStatus tracer

    traceStatus :: BootStatusMsg -> IO ()
    traceStatus = traceWith tracerBootStatus

    withTrace :: (a -> BootStatusMsg) -> IO a -> IO a
    withTrace c a = do
      x <- a
      traceStatus $ c x
      pure x

    cache :: String -> IO a -> IO (IO a)
    cache = cacheWith (contramap (BootCache . SafeTrace) tracer) . Just

-- | Determine Clang arguments and target
getClangArgsAndTarget ::
     Tracer BootMsg
  -> ClangArgsConfig FilePath
  -> IO (ClangArgs, ClangArgs.Target)
getClangArgsAndTarget tracer config0 = do
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
    case ClangArgs.target config of
      -- If a target is specified, return Clang arguments and target
      Just target -> return (clangArgs, target)
      -- If a target is not specified:
      Nothing -> do
        -- Determine the target using @libclang@ with the Clang arguments
        target <- getClangTarget tracer clangArgs
        -- Recalculate the Clang arguments with the determined target
        either throwIO (return . (, target)) $
          ClangArgs.getClangArgs config { ClangArgs.target = Just target }
  where
    tracerBuiltinIncDir :: Tracer BuiltinIncDirMsg
    tracerBuiltinIncDir = contramap BootBuiltinIncDir tracer

    tracerExtraClangArgs :: Tracer ExtraClangArgsMsg
    tracerExtraClangArgs = contramap BootExtraClangArgs tracer

-- | Fatal error: unable to determine target
data UnableToDetermineTargetException = UnableToDetermineTargetException
  deriving Show

instance Exception UnableToDetermineTargetException where
  displayException UnableToDetermineTargetException =
    "Unable to determine target"

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
    bootModule                  :: Hs.ModuleName
  , bootCStandard               :: CStandard
  , bootClangArgs               :: IO ClangArgs
  , bootTarget                  :: IO ClangArgs.Target
  , bootHashIncludeArgs         :: IO [HashIncludeArg]
  , bootExternalBindingSpecs    :: IO MergedBindingSpecs
  , bootPrescriptiveBindingSpec :: IO PrescriptiveBindingSpec
  }

{-------------------------------------------------------------------------------
  Trace
-------------------------------------------------------------------------------}

data BootStatusMsg =
    BootStatusStart                   BindgenConfig
  | BootStatusClangArgs               ClangArgs
  | BootStatusTarget                  ClangArgs.Target
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
    BootStatusClangArgs               x -> bootStatus "ClangArgs"               x
    BootStatusTarget                  x -> bootStatus "Target"                  x
    BootStatusHashIncludeArgs         x -> bootStatus "HashIncludeArgs"         x
    BootStatusExternalBindingSpecs    x -> bootStatus "ExternalBindingSpecs"    x
    BootStatusPrescriptiveBindingSpec x -> bootStatus "PrescriptiveBindingSpec" x

instance IsTrace Level BootStatusMsg where
  getDefaultLogLevel = const Debug
  getSource          = const HsBindgen
  getTraceId         = const "boot-status"

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
  | BootExtraClangArgs       ExtraClangArgsMsg
  | BootHashIncludeArg       HashIncludeArgMsg
  | BootCompareClangVersions CompareVersionsMsg
  | BootStatus               BootStatusMsg
  | BootTarget               BootTargetMsg
  | BootCache                (SafeTrace CacheMsg)
  deriving stock (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)
