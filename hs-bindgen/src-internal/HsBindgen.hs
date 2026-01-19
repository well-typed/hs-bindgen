module HsBindgen (
    hsBindgen

    -- * Artefacts
  , Artefact(..)
  , writeIncludeGraph
  , writeUseDeclGraph
  , getBindings
  , getBindingsMultiple
  , writeBindings
  , writeBindingsSingleToDir
  , writeBindingsMultiple
  , writeBindingsToDir
  , writeBindingSpec
  , writeTests

    -- * Errors
  , BindgenError(..)

    -- * Traces
  , SafeTraceMsg(..)

    -- * Test infrastructure
  , hsBindgenE
  ) where

import Control.Monad.Except (MonadError (..), withExceptT)
import Control.Monad.Trans.Except (runExceptT)
import System.Exit (ExitCode (..), exitWith)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Artefact
import HsBindgen.Backend
import HsBindgen.Backend.Category
import HsBindgen.Backend.HsModule.Render
import HsBindgen.Backend.HsModule.Translation
import HsBindgen.BindingSpec qualified as BindingSpec
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
     TracerConfig Level     TraceMsg
  -> TracerConfig SafeLevel SafeTraceMsg
  -> BindgenConfig
  -> [UncheckedHashIncludeArg]
  -> Artefact a
  -> IO a
hsBindgen tu ts b i a = do
    eRes <- hsBindgenE tu ts b i a
    case eRes of
      Left err -> do
        putStrLn $ PP.renderCtxDoc PP.defaultContext $ prettyForTrace err
        exitWith (ExitFailure 2)
      Right r  -> pure r

-- | Like 'hsBindgen' but does not exit with failure when an error has occurred.
hsBindgenE ::
     TracerConfig Level     TraceMsg
  -> TracerConfig SafeLevel SafeTraceMsg
  -> BindgenConfig
  -> [UncheckedHashIncludeArg]
  -> Artefact a
  -> IO (Either BindgenError a)
hsBindgenE
  tracerConfigUnsafe
  tracerConfigSafe
  config
  uncheckedHashIncludeArgs
  artefacts = do
    eRes <- withTracer tracerConfigUnsafe $ \tracerUnsafe -> do
      -- 1. Boot.
      let tracerBoot :: Tracer BootMsg
          tracerBoot = contramap TraceBoot tracerUnsafe
      bootArtefact <-
        runBoot tracerBoot config uncheckedHashIncludeArgs
      -- 2. Frontend.
      let tracerFrontend :: Tracer FrontendMsg
          tracerFrontend = contramap TraceFrontend tracerUnsafe
      frontendArtefact <-
        runFrontend tracerFrontend config.frontend bootArtefact
      -- 3. Backend.
      let tracerConfigBackend :: TracerConfig SafeLevel BackendMsg
          tracerConfigBackend = contramap SafeBackendMsg tracerConfigSafe
      backendArtefact <-
        withTracerSafe tracerConfigBackend  $ \tracerSafe ->
          runBackend tracerSafe config.backend bootArtefact frontendArtefact
      -- 4. Artefacts.
      let tracerConfigArtefact :: TracerConfig SafeLevel ArtefactMsg
          tracerConfigArtefact = contramap SafeArtefactMsg tracerConfigSafe
      withTracerSafe tracerConfigArtefact $ \tracerSafe ->
        runArtefacts
          tracerSafe
          bootArtefact
          frontendArtefact
          backendArtefact
          artefacts

    let tracerConfigDelayedIO :: TracerConfig SafeLevel DelayedIOMsg
        tracerConfigDelayedIO = contramap SafeDelayedIOMsg tracerConfigSafe
    runExceptT $ withTracerSafe tracerConfigDelayedIO $ \tracerSafe ->
      case eRes of
        Left er       -> throwError $ BindgenErrorReported er
        Right (r, as) -> do
          -- Before creating directories or writing output files, we verify
          -- adherence to the provided policies.
          withExceptT BindgenDelayedIOError $ mapM_ checkPolicy as
          liftIO $ executeDelayedIOActions tracerSafe as
          pure r

{-------------------------------------------------------------------------------
  Custom build artefacts
-------------------------------------------------------------------------------}

-- | Write the include graph to `STDOUT` or a file.
writeIncludeGraph :: FileOverwritePolicy -> Maybe FilePath -> Artefact ()
writeIncludeGraph pol mPath = do
    (predicate, includeGraph) <- IncludeGraph
    let rendered = IncludeGraph.dumpMermaid predicate includeGraph
    case mPath of
      Nothing   -> Lift $ delay $ WriteToStdOut $ StringContent rendered
      Just path -> write pol "include graph" (UserSpecified path) rendered

-- | Write @use-decl@ graph to file.
writeUseDeclGraph :: FileOverwritePolicy -> Maybe FilePath -> Artefact ()
writeUseDeclGraph pol mPath = do
    useDeclGraph <- UseDeclGraph
    let rendered = UseDeclGraph.dumpMermaid useDeclGraph
    case mPath of
      Nothing   -> Lift $ delay $ WriteToStdOut $ StringContent rendered
      Just path -> write pol "use-decl graph" (UserSpecified path) rendered

-- | Get bindings (single module).
getBindings :: Artefact String
getBindings = do
    name  <- FinalModuleBaseName
    decls <- FinalDecls
    when (all nullDecls decls) $
      EmitTrace $ NoBindingsSingleModule name
    pure $ render $ translateModuleSingle name decls

-- | Write bindings to file.
writeBindings :: FileOverwritePolicy -> FilePath -> Artefact ()
writeBindings fileOverwritePolicy path = do
    bindings <- getBindings
    write fileOverwritePolicy "bindings" (UserSpecified path) bindings

-- | Write bindings to a directory (single module combining all categories).
--
-- Unlike 'writeBindings', this writes to a directory and automatically
-- constructs the file path from the module name, similar to
-- 'writeBindingsMultiple' but generating only one file.
writeBindingsSingleToDir ::
     FileOverwritePolicy
  -> OutputDirPolicy
  -> FilePath
  -> Artefact ()
writeBindingsSingleToDir fileOverwritePolicy outputDirPolicy hsOutputDir = do
    moduleBaseName <- FinalModuleBaseName
    bindings       <- getBindings
    let localPath :: FilePath
        localPath = Hs.moduleNamePath $
            fromBaseModuleName moduleBaseName Nothing

        location :: FileLocation
        location = RelativeFileLocation RelativeToOutputDir{
              outputDir       = hsOutputDir
            , localPath       = localPath
            , outputDirPolicy = outputDirPolicy
            }

    write fileOverwritePolicy "bindings" location bindings

-- | Write bindings to a directory, choosing between single and multi-module modes.
--
-- - If categories were explicitly selected: single-module mode (one file with
-- all selected categories)
-- - If no categories were selected: multi-module mode (one file per category)
writeBindingsToDir ::
     FileOverwritePolicy
  -> OutputDirPolicy
  -> FilePath
  -> Bool  -- ^ True if categories were explicitly selected
  -> Artefact ()
writeBindingsToDir filePolicy dirPolicy hsOutputDir categoriesSelected =
    if categoriesSelected
      then writeBindingsSingleToDir filePolicy dirPolicy hsOutputDir
      else writeBindingsMultiple filePolicy dirPolicy hsOutputDir

-- | Get bindings (one module per binding category).
getBindingsMultiple :: Artefact (ByCategory_ (Maybe String))
getBindingsMultiple = do
    name  <- FinalModuleBaseName
    decls <- FinalDecls
    when (all nullDecls decls) $
      EmitTrace $ NoBindingsMultipleModules name
    pure $ fmap render <$> translateModuleMultiple name decls

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
    moduleBaseName <- FinalModuleBaseName
    includeGraph   <- snd <$> IncludeGraph
    declIndex      <- DeclIndex
    getMainHeaders <- GetMainHeaders
    omitTypes      <- OmitTypes
    squashedTypes  <- SquashedTypes
    hsDecls        <- HsDecls
    -- Binding specifications only specify types.
    let bs =
          genBindingSpec
            (BindingSpec.getFormat path)
            (fromBaseModuleName moduleBaseName (Just CType))
            includeGraph
            declIndex
            getMainHeaders
            omitTypes
            squashedTypes
            (view (lensForCategory CType) hsDecls)
        fileDescription = FileDescription {
              description     = "Binding specifications"
            , location        = UserSpecified path
            , overwritePolicy = fileOverwritePolicy
            , content         = ByteStringContent bs
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

write :: FileOverwritePolicy -> String -> FileLocation -> String -> Artefact ()
write pol what loc str
  | null str =
    EmitTrace $ SkipWriteToFileNoBindings loc
  | otherwise =
    Lift $ delay $ WriteToFile $ FileDescription what loc pol (StringContent str)

writeByCategory ::
     FileOverwritePolicy
  -> OutputDirPolicy
  -> String
  -> FilePath
  -> BaseModuleName
  -> ByCategory_ (Maybe String)
  -> Artefact ()
writeByCategory filePolicy dirPolicy what dir moduleBaseName =
    sequence_ . mapWithCategory_ writeCategory
  where
    writeCategory :: Category -> Maybe String -> Artefact ()
    writeCategory _    Nothing   = pure ()
    writeCategory cat (Just str) =
        write filePolicy whatWithCategory location str
      where
        localPath :: FilePath
        localPath = Hs.moduleNamePath $
            fromBaseModuleName moduleBaseName (Just cat)

        whatWithCategory :: String
        whatWithCategory = what ++ " (" ++ show cat ++ ")"

        location :: FileLocation
        location = RelativeFileLocation RelativeToOutputDir{
              outputDir       = dir
            , localPath       = localPath
            , outputDirPolicy = dirPolicy
            }

nullDecls :: ([a], [b]) -> Bool
nullDecls (xs, ys) = null xs && null ys

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

{-------------------------------------------------------------------------------
  Traces
-------------------------------------------------------------------------------}

data SafeTraceMsg =
      SafeBackendMsg   BackendMsg
    | SafeArtefactMsg  ArtefactMsg
    | SafeDelayedIOMsg DelayedIOMsg
  deriving (Show, Generic, PrettyForTrace, IsTrace SafeLevel)
