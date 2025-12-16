{-# LANGUAGE UndecidableSuperClasses #-}

module HsBindgen
  ( hsBindgen

    -- * Artefacts
  , Artefact(..)
  , writeIncludeGraph
  , writeUseDeclGraph
  , getBindings
  , getBindingsMultiple
  , writeBindings
  , writeBindingsMultiple
  , writeBindingSpec
  , writeTests

    -- * Errors
  , BindgenError(..)

    -- * Test infrastructure
  , hsBindgenE
  ) where

import Control.Monad.Except (MonadError (..), withExceptT)
import Control.Monad.Trans.Except (runExceptT)
import Optics.Core (view)
import System.Exit (ExitCode (..), exitWith)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Artefact
import HsBindgen.Backend
import HsBindgen.Backend.Category
import HsBindgen.Backend.HsModule.Render
import HsBindgen.Backend.HsModule.Translation
import HsBindgen.BindingSpec.Gen
import HsBindgen.Boot
import HsBindgen.Config.Internal
import HsBindgen.DelayedIO
import HsBindgen.Errors (throwPure_TODO)
import HsBindgen.Frontend
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.RootHeader (UncheckedHashIncludeArg)
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

-- | Main entry point to run @hs-bindgen@.
--
-- For a list of build artefacts, see the description and constructors of
-- 'Artefact'.
hsBindgen ::
     TracerConfig Level TraceMsg
  -> BindgenConfig
  -> [UncheckedHashIncludeArg]
  -> Artefact a
  -> IO a
hsBindgen t b i a = do
    eRes <- hsBindgenE t b i a
    case eRes of
      Left err -> do
        putStrLn $ PP.renderCtxDoc PP.defaultContext $ prettyForTrace err
        exitWith (ExitFailure 2)
      Right r  -> pure r

-- | Like 'hsBindgen' but does not exit with failure when an error has occurred.
hsBindgenE ::
     TracerConfig Level TraceMsg
  -> BindgenConfig
  -> [UncheckedHashIncludeArg]
  -> Artefact a
  -> IO (Either BindgenError a)
hsBindgenE
  tracerConfig
  bindgenConfig@BindgenConfig{..}
  uncheckedHashIncludeArgs
  artefacts = do
    eRes <- withTracer tracerConfig $ \tracer -> do
      -- Boot and frontend require unsafe tracer and `libclang`.
      let tracerFrontend :: Tracer FrontendMsg
          tracerFrontend = contramap TraceFrontend tracer
          tracerBoot :: Tracer BootMsg
          tracerBoot = contramap TraceBoot tracer
      -- 1. Boot.
      bootArtefact <-
        boot tracerBoot bindgenConfig uncheckedHashIncludeArgs
      -- 2. Frontend.
      frontendArtefact <-
        frontend tracerFrontend bindgenFrontendConfig bootArtefact
      -- 3. Backend.
      backendArtefact <- withTracerSafe tracerConfigSafe $ \tracerSafe -> do
        backend tracerSafe bindgenBackendConfig bootArtefact frontendArtefact
      -- 4. Artefacts.
      runArtefacts bootArtefact frontendArtefact backendArtefact artefacts

    runExceptT $ withTracerSafe tracerConfigSafe $ \tracer ->
        case eRes of
          Left er       -> throwError $ BindgenErrorReported er
          Right (r, as) -> do
            -- Before creating directories or writing output files, we verify
            -- adherence to the provided policies.
            withExceptT (BindgenDelayedIOError) $ mapM_ checkPolicy as
            liftIO $ executeFileSystemActions tracer as
            pure r

  where
    tracerConfigSafe :: TracerConfig SafeLevel a
    tracerConfigSafe = TracerConfig {
        tVerbosity      = tVerbosity tracerConfig
      , tOutputConfig   = def
      , tCustomLogLevel = mempty
      , tShowTimeStamp  = tShowTimeStamp tracerConfig
      , tShowCallStack  = tShowCallStack tracerConfig
      }

{-------------------------------------------------------------------------------
  Custom build artefacts
-------------------------------------------------------------------------------}

-- | Write the include graph to `STDOUT` or a file.
writeIncludeGraph :: FileOverwritePolicy -> Maybe FilePath -> Artefact ()
writeIncludeGraph pol mPath = do
    (predicate, includeGraph) <- IncludeGraph
    let rendered = IncludeGraph.dumpMermaid predicate includeGraph
    case mPath of
      Nothing   -> Lift $ delay $ WriteToStdOut $ TextContent rendered
      Just path -> Lift $ write pol "include graph" (UserSpecified path) rendered

-- | Write @use-decl@ graph to file.
writeUseDeclGraph :: FileOverwritePolicy -> Maybe FilePath -> Artefact ()
writeUseDeclGraph pol mPath = do
    useDeclGraph <- UseDeclGraph
    let rendered = UseDeclGraph.dumpMermaid useDeclGraph
    case mPath of
      Nothing   -> Lift $ delay $ WriteToStdOut $ TextContent rendered
      Just path -> Lift $ write pol "use-decl graph" (UserSpecified path) rendered

-- TODO_PR: Be careful when there are no declarations at all.

-- | Get bindings (single module).
getBindings :: Artefact String
getBindings = do
    name  <- FinalModuleBaseName
    decls <- FinalDecls
    pure $ render $ translateModuleSingle name decls

-- TODO_PR: Be careful when there are no declarations at all.

-- | Write bindings to file.
writeBindings :: FileOverwritePolicy -> FilePath -> Artefact ()
writeBindings fileOverwritePolicy path = do
    bindings <- getBindings
    Lift $ write fileOverwritePolicy "bindings" (UserSpecified path) bindings

-- TODO_PR: Be careful when there are no declarations at all.

-- | Get bindings (one module per binding category).
getBindingsMultiple :: Artefact (ByCategory_ (Maybe String))
getBindingsMultiple = do
  name  <- FinalModuleBaseName
  decls <- FinalDecls
  pure $ fmap render <$> translateModuleMultiple name decls

-- TODO_PR: Be careful when there are no declarations at all.

-- | Write bindings to files in provided output directory.
--
-- Each file contains a different binding category.
--
-- If no file is given, print to standard output.
writeBindingsMultiple ::
     FileOverwritePolicy
  -> OutputDirPolicy
  -> FilePath
  -> Artefact ()
writeBindingsMultiple fileOverwritePolicy outputDirPolicy hsOutputDir = do
    moduleBaseName     <- FinalModuleBaseName
    bindingsByCategory <- getBindingsMultiple
    writeByCategory
      fileOverwritePolicy
      outputDirPolicy
      "Bindings"
      hsOutputDir
      moduleBaseName
      bindingsByCategory

-- | Write binding specifications to file.
writeBindingSpec :: FileOverwritePolicy -> FilePath -> Artefact ()
writeBindingSpec fileOverwritePolicy path = do
    target         <- Target
    moduleBaseName <- FinalModuleBaseName
    getMainHeaders <- GetMainHeaders
    omitTypes      <- OmitTypes
    hsDecls        <- HsDecls
    -- Binding specifications only specify types.
    let bindingSpec =
          genBindingSpec
            target
            (fromBaseModuleName moduleBaseName (Just CType))
            getMainHeaders
            omitTypes
            (view (lensForCategory CType) hsDecls)
        fileDescription =
          FileDescription {
              description = "Binding specifications"
            , location    = UserSpecified path
            , fileOverwritePolicy
            , content     = BindingSpecContent  bindingSpec
            }
    Lift $ delay $ WriteToFile fileDescription

-- | Create test suite in directory.
writeTests :: FilePath -> Artefact ()
writeTests _testDir = do
    -- moduleBaseName  <- FinalModuleBaseName
    -- hashIncludeArgs <- HashIncludeArgs
    -- hsDecls         <- HsDecls
    -- liftIO $
    --   genTests
    --     hashIncludeArgs
    --     hsDecls
    --     moduleBaseName
    --     testDir
    throwPure_TODO 22 "Test generation integrated into the artefact API"

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

write :: FileOverwritePolicy -> String -> FileLocation -> String -> DelayedIOM ()
write pol what loc str =
    delay $ WriteToFile $ FileDescription what loc pol (TextContent str)

writeByCategory ::
     FileOverwritePolicy
  -> OutputDirPolicy
  -> String
  -> FilePath
  -> BaseModuleName
  -> ByCategory_ (Maybe String)
  -> Artefact ()
writeByCategory
  fileOverwritePolicy outputDirPolicy what outputDir moduleBaseName =
    sequence_ . mapWithCategory_ writeCategory
  where
    writeCategory :: Category -> Maybe String -> Artefact ()
    writeCategory _    Nothing   = pure ()
    writeCategory cat (Just str) =
        Lift $ write fileOverwritePolicy whatWithCategory location str
      where
        localPath :: FilePath
        localPath = Hs.moduleNamePath $ fromBaseModuleName moduleBaseName (Just cat)

        whatWithCategory :: String
        whatWithCategory = what ++ " (" ++ show cat ++ ")"

        location :: FileLocation
        location =
          RelativeFileLocation $
            RelativeToOutputDir {outputDir, localPath, outputDirPolicy}

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data BindgenError =
      BindgenErrorReported  AnErrorHappened
    | BindgenDelayedIOError DelayedIOError
    deriving stock (Show)

instance PrettyForTrace BindgenError where
  prettyForTrace = \case
    BindgenErrorReported  e -> prettyForTrace e
    BindgenDelayedIOError e -> prettyForTrace e
