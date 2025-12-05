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

import Control.Monad.Except (MonadError (..))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Map qualified as Map
import System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                         doesFileExist)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeDirectory, (</>))
import Text.SimplePrettyPrint ((<+>))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Artefact
import HsBindgen.Backend
import HsBindgen.Backend.HsModule.Render
import HsBindgen.Backend.SHs.AST
import HsBindgen.BindingSpec.Gen
import HsBindgen.BindingSpec.Private.V1 qualified as BindingSpec
import HsBindgen.Boot
import HsBindgen.Config.Internal
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
            mapM_ checkPolicy as
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

    checkPolicy :: DelayedIO -> ExceptT BindgenError IO ()
    checkPolicy (PutStrLn   _) = pure ()
    checkPolicy (WriteFile fd) = case fd.location of
      UserSpecified path -> do
        let baseDir = takeDirectory path
        dirExists  <- liftIO $ doesDirectoryExist baseDir
        fileExists <- liftIO $ doesFileExist path
        unless dirExists $
          throwError $ BindgenFileSystemError $ DirectoryDoesNotExist baseDir
        when (fileExists && fd.fileOverwritePolicy == DoNotOverwriteFiles) $
          throwError $ BindgenFileSystemError $ FileAlreadyExists path
      RelativeFileLocation RelativeToOutputDir{..} -> do
        let path = outputDir </> localPath
        dirExists  <- liftIO $ doesDirectoryExist outputDir
        fileExists <- liftIO $ doesFileExist path
        unless (dirExists || outputDirPolicy == CreateOutputDirs ) $
          throwError $ BindgenFileSystemError $ DirectoryDoesNotExist outputDir
        when (fileExists && fd.fileOverwritePolicy == DoNotOverwriteFiles) $
          throwError $ BindgenFileSystemError $ FileAlreadyExists path

    executeFileSystemActions :: Tracer BindgenMsg -> [DelayedIO] -> IO ()
    executeFileSystemActions tracer as =
      forM_ as $ \case
        PutStrLn   x -> putStrLn x
        WriteFile fd -> do
          let path = fileLocationToPath fd.location
          traceWith tracer $ BindgenWriteFile path fd.description
          -- Creating the directory is justified by checking the policy first.
          createDirectoryIfMissing True (takeDirectory path)
          case fd.content of
            TextContent str        -> writeFile path str
            BindingSpecContent ubs -> BindingSpec.writeFile path ubs


{-------------------------------------------------------------------------------
  Custom build artefacts
-------------------------------------------------------------------------------}

-- | Write the include graph to `STDOUT` or a file.
writeIncludeGraph :: FileOverwritePolicy -> Maybe FilePath -> Artefact ()
writeIncludeGraph pol mPath = do
    (predicate, includeGraph) <- IncludeGraph
    let rendered = IncludeGraph.dumpMermaid predicate includeGraph
    case mPath of
      Nothing   -> Lift $ delay $ PutStrLn rendered
      Just path -> Lift $ write pol "include graph" (UserSpecified path) rendered

-- | Write @use-decl@ graph to file.
writeUseDeclGraph :: FileOverwritePolicy -> Maybe FilePath -> Artefact ()
writeUseDeclGraph pol mPath = do
    index <- DeclIndex
    useDeclGraph <- UseDeclGraph
    let rendered = UseDeclGraph.dumpMermaid index useDeclGraph
    case mPath of
      Nothing   -> Lift $ delay $ PutStrLn rendered
      Just path -> Lift $ write pol "use-decl graph" (UserSpecified path) rendered

-- | Get bindings (single module).
getBindings :: Safety -> Artefact String
getBindings safety = do
    finalModule <- finalModuleArtefact
    Lift $ pure . render $ finalModule
  where finalModuleArtefact = case safety of
          Safe   -> FinalModuleSafe
          Unsafe -> FinalModuleUnsafe

-- | Write bindings to file.
writeBindings :: FileOverwritePolicy -> Safety -> FilePath -> Artefact ()
writeBindings fileOverwritePolicy safety path = do
    bindings <- getBindings safety
    Lift $ write fileOverwritePolicy "bindings" (UserSpecified path) bindings

-- | Get bindings (one module per binding category).
getBindingsMultiple :: Artefact (ByCategory String)
getBindingsMultiple = fmap render <$> FinalModules

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
            (fromBaseModuleName moduleBaseName (Just BType))
            getMainHeaders
            omitTypes
            (fromMaybe [] (Map.lookup BType $ unByCategory hsDecls))
        fileDescription =
          FileDescription {
              description = "Binding specifications"
            , location    = UserSpecified path
            , fileOverwritePolicy
            , content     = BindingSpecContent  bindingSpec
            }
    Lift $ delay $ WriteFile fileDescription

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
    throwPure_TODO 22 "Test generation  integrated into the artefact API"

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

write :: FileOverwritePolicy -> String -> FileLocation -> String -> ArtefactM ()
write pol what loc str =
    delay $ WriteFile $ FileDescription what loc pol (TextContent str)

writeByCategory ::
     FileOverwritePolicy
  -> OutputDirPolicy
  -> String
  -> FilePath
  -> BaseModuleName
  -> ByCategory String
  -> Artefact ()
writeByCategory
  fileOverwritePolicy outputDirPolicy what outputDir moduleBaseName =
    mapM_ (uncurry writeCategory) . Map.toList . unByCategory
  where
    writeCategory :: BindingCategory -> String -> Artefact ()
    writeCategory cat str =
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
      BindgenErrorReported   AnErrorHappened
    | BindgenFileSystemError DelayedIOError
    deriving stock (Show)

instance PrettyForTrace BindgenError where
  prettyForTrace = \case
    BindgenErrorReported   e -> prettyForTrace e
    BindgenFileSystemError e -> prettyForTrace e

{-------------------------------------------------------------------------------
  Traces
-------------------------------------------------------------------------------}

data BindgenMsg =
    BindgenCreateDir FilePath
  | BindgenWriteFile FilePath String
  deriving stock (Show, Generic)

instance PrettyForTrace BindgenMsg where
  prettyForTrace = \case
    BindgenCreateDir path ->
      "Creating directory" <+> PP.showToCtxDoc path
    BindgenWriteFile path what ->
      "Writing" <+> PP.showToCtxDoc what <+> "to file" <+> PP.showToCtxDoc path

instance IsTrace SafeLevel BindgenMsg where
  getDefaultLogLevel = \case
    BindgenCreateDir{} -> SafeInfo
    BindgenWriteFile{} -> SafeInfo
  getSource = const HsBindgen
  getTraceId = \case
    BindgenCreateDir{} -> "bindgen-create-dir"
    BindgenWriteFile{} -> "bindgen-write-file"
