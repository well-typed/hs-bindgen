module HsBindgen (
    hsBindgen

    -- * Artefacts
  , Artefact(..)

    -- ** High-level artefacts
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

    -- ** Low-level artefacts
  , getConfig
  , getIncludeGraph
  , getDeclIndex
  , getUseDeclGraph
  , getDeclUseGraph
  , getOmittedTypes
  , getReifiedC
  , getSquashedTypes
  , getDependencies
  , getGetMainHeaders

    -- * Errors
  , BindgenError(..)

    -- * Traces
  , SafeTraceMsg(..)

    -- * Test infrastructure
  , hsBindgenE
  ) where

import Control.Exception (Exception (..), catch)
import Control.Monad.Except (MonadError (..), withExceptT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Map qualified as Map
import Data.Set qualified as Set
import System.Exit (ExitCode (..), exitWith)
import Text.SimplePrettyPrint qualified as PP

import Clang.Paths

import HsBindgen.Artefact
import HsBindgen.ArtefactM
import HsBindgen.Backend
import HsBindgen.Backend.Category
import HsBindgen.Backend.HsModule.Render
import HsBindgen.Backend.HsModule.Translation
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.BindingSpec.Gen
import HsBindgen.Boot
import HsBindgen.Clang
import HsBindgen.Config.Internal
import HsBindgen.Errors (throwPure_TODO)
import HsBindgen.Frontend
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.ProcessIncludes qualified as ProcessIncludes
import HsBindgen.Frontend.RootHeader (UncheckedHashIncludeArg)
import HsBindgen.Frontend.RootHeader qualified as RootHeader
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
    eRes <- hsBindgenE tu ts b i a `catch` \e -> case fromException e of
      Just (LibclangException msg) -> do
        putStrLn $ PP.renderCtxDoc PP.defaultContext (PP.string msg)
        -- We specifically use exit code 2 here; it means that the call to
        -- `libclang` has failed.
        exitWith (ExitFailure 2)
      _ -> throwIO e
    case eRes of
      Left err -> do
        putStrLn $ PP.renderCtxDoc PP.defaultContext $ prettyForTrace err
        -- We specifically use exit code 3 here; it means that `hs-bindgen` ran
        -- to completion, but an error has occurred.
        exitWith (ExitFailure 3)
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
          runBackend tracerSafe config bootArtefact frontendArtefact
      -- 4. Artefacts.
      let tracerConfigArtefact :: TracerConfig SafeLevel ArtefactMsg
          tracerConfigArtefact = contramap SafeArtefactMsg tracerConfigSafe
      withTracerSafe tracerConfigArtefact $ \tracerSafe ->
        runArtefacts
          tracerSafe
          config
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
  High-level artefacts
-------------------------------------------------------------------------------}

-- | Write the include graph to `STDOUT` or a file.
writeIncludeGraph :: FileOverwritePolicy -> Maybe FilePath -> Artefact ()
writeIncludeGraph pol mPath = do
    includeGraph <- getIncludeGraph
    let predicate = (/= RootHeader.name)
        rendered = IncludeGraph.dumpMermaid predicate includeGraph
    case mPath of
      Nothing   -> Lift $ delay $ WriteToStdOut $ StringContent rendered
      Just path -> write pol def "include graph" (UserSpecified path) rendered

-- | Write @use-decl@ graph to file.
writeUseDeclGraph :: FileOverwritePolicy -> Maybe FilePath -> Artefact ()
writeUseDeclGraph pol mPath = do
    useDeclGraph <- getUseDeclGraph
    let rendered = UseDeclGraph.dumpMermaid useDeclGraph
    case mPath of
      Nothing   -> Lift $ delay $ WriteToStdOut $ StringContent rendered
      Just path -> write pol def "use-decl graph" (UserSpecified path) rendered

-- | Get bindings (single module).
getBindings :: ModuleRenderConfig -> Artefact String
getBindings mrc = do
    name  <- ModuleBaseName
    decls <- FinalDecls
    when (all nullDecls decls) $ EmitTrace $ NoBindingsSingleModule name
    config <- getConfig
    let fns = config.frontend.fieldNamingStrategy
    pure $ render $
      translateModuleSingle fns mrc name decls

-- | Write bindings to file.
writeBindings :: ModuleRenderConfig -> FileOverwritePolicy -> FilePath -> Artefact ()
writeBindings mrc fileOverwritePolicy path = do
    bindings <- getBindings mrc
    write fileOverwritePolicy def "bindings" (UserSpecified path) bindings

-- | Write bindings to a directory (single module combining all categories).
--
-- Unlike 'writeBindings', this writes to a directory and automatically
-- constructs the file path from the module name, similar to
-- 'writeBindingsMultiple' but generating only one file.
writeBindingsSingleToDir ::
     ModuleRenderConfig
  -> FileOverwritePolicy
  -> OutputDirPolicy
  -> FilePath
  -> Artefact ()
writeBindingsSingleToDir mrc fileOverwritePolicy outputDirPolicy hsOutputDir = do
    moduleBaseName <- ModuleBaseName
    bindings       <- getBindings mrc
    let localPath :: FilePath
        localPath = Hs.moduleNamePath $
            fromBaseModuleName moduleBaseName Nothing

        location :: FileLocation
        location = RelativeFileLocation RelativeToOutputDir{
              outputDir = hsOutputDir
            , localPath = localPath
            }

    write fileOverwritePolicy outputDirPolicy "bindings" location bindings

-- | Write bindings to a directory, choosing between single and multi-module modes.
--
-- - If categories were explicitly selected: single-module mode (one file with
-- all selected categories)
-- - If no categories were selected: multi-module mode (one file per category)
writeBindingsToDir ::
     ModuleRenderConfig
  -> FileOverwritePolicy
  -> OutputDirPolicy
  -> FilePath
  -> Bool  -- ^ True if categories were explicitly selected
  -> Artefact ()
writeBindingsToDir mrc filePolicy dirPolicy hsOutputDir categoriesSelected =
    if categoriesSelected
      then writeBindingsSingleToDir mrc filePolicy dirPolicy hsOutputDir
      else writeBindingsMultiple mrc filePolicy dirPolicy hsOutputDir

-- | Get bindings (one module per binding category).
getBindingsMultiple :: ModuleRenderConfig -> Artefact (ByCategory_ (Maybe String))
getBindingsMultiple mrc = do
    name  <- ModuleBaseName
    decls <- FinalDecls
    when (all nullDecls decls) $
      EmitTrace $ NoBindingsMultipleModules name
    config <- getConfig
    let fns = config.frontend.fieldNamingStrategy
    pure $ fmap render <$> translateModuleMultiple fns mrc name decls

-- | Write bindings to files in provided output directory.
--
-- Each file contains a different binding category.
--
-- If no file is given, print to standard output.
writeBindingsMultiple ::
     ModuleRenderConfig
  -> FileOverwritePolicy
  -> OutputDirPolicy
  -> FilePath
  -> Artefact ()
writeBindingsMultiple mrc fileOverwritePolicy outputDirPolicy hsOutputDir = do
    moduleBaseName     <- ModuleBaseName
    bindingsByCategory <- getBindingsMultiple mrc
    writeByCategory
      fileOverwritePolicy
      outputDirPolicy
      "Bindings"
      hsOutputDir
      moduleBaseName
      bindingsByCategory

-- | Write binding specifications to file.
writeBindingSpec ::
     FileOverwritePolicy
  -> OutputDirPolicy
  -> FilePath
  -> Artefact ()
writeBindingSpec fileOverwritePolicy outputDirPolicy path = do
    moduleBaseName <- ModuleBaseName
    includeGraph   <- getIncludeGraph
    declIndex      <- getDeclIndex
    getMainHeaders <- getGetMainHeaders
    omittedTypes   <- getOmittedTypes
    squashedTypes  <- getSquashedTypes
    hsDecls        <- HsDecls
    -- Binding specifications only specify types.
    let bs =
          genBindingSpec
            (BindingSpec.getFormat path)
            (fromBaseModuleName moduleBaseName (Just CType))
            includeGraph
            declIndex
            getMainHeaders
            omittedTypes
            squashedTypes
            (view (lensForCategory CType) hsDecls)
        fileDescription = FileDescription {
              description     = "Binding specifications"
            , location        = UserSpecified path
            , overwritePolicy = fileOverwritePolicy
            , outputDirPolicy = outputDirPolicy
            , content         = ByteStringContent bs
            }
    Lift $ delay $ WriteToFile fileDescription

-- | Create test suite in directory.
writeTests :: FilePath -> Artefact ()
writeTests _testDir = do
    -- moduleBaseName  <- ModuleBaseName
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
  Low-level artefacts
-------------------------------------------------------------------------------}

getConfig :: Artefact BindgenConfig
getConfig = Lift askConfig

getGetMainHeaders :: Artefact ProcessIncludes.GetMainHeaders
getGetMainHeaders = (.getMainHeaders) <$> ParseInfoA

getIncludeGraph :: Artefact IncludeGraph
getIncludeGraph = (.includeGraph) <$> ParseInfoA

getDeclIndex :: Artefact DeclIndex
getDeclIndex = (.ann.declIndex) <$> FrontendPassA FinalPass

getUseDeclGraph :: Artefact UseDeclGraph
getUseDeclGraph = (.ann.useDeclGraph) <$> FrontendPassA ConstructTranslationUnitPass

getDeclUseGraph :: Artefact DeclUseGraph
getDeclUseGraph = (.ann.declUseGraph) <$> FrontendPassA ConstructTranslationUnitPass

getOmittedTypes :: Artefact [(DeclId, SourcePath)]
getOmittedTypes =
    Map.toList . DeclIndex.getOmitted <$> getDeclIndex

getReifiedC :: Artefact [C.Decl Final]
getReifiedC = (.decls) <$> FrontendPassA FinalPass

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1549>
-- When we properly record aliases, we may not need this anymore.
getSquashedTypes :: Artefact [(DeclId, (SourcePath, Hs.Identifier))]
getSquashedTypes = do
  decls <- getReifiedC
  let translatedDeclIds = Set.fromList $ map (.info.id.cName) decls
  declIndex <- getDeclIndex
  pure $ Map.toList $ DeclIndex.getSquashed declIndex translatedDeclIds

getDependencies :: Artefact [SourcePath]
getDependencies = IncludeGraph.toSortedList <$> getIncludeGraph

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

write :: FileOverwritePolicy -> OutputDirPolicy -> String -> FileLocation -> String -> Artefact ()
write pol dirPol what loc str
  | null str =
    EmitTrace $ SkipWriteToFileNoBindings loc
  | otherwise =
    Lift $ delay $ WriteToFile $ FileDescription what loc pol dirPol (StringContent str)

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
        write filePolicy dirPolicy whatWithCategory location str
      where
        localPath :: FilePath
        localPath = Hs.moduleNamePath $
            fromBaseModuleName moduleBaseName (Just cat)

        whatWithCategory :: String
        whatWithCategory = what ++ " (" ++ show cat ++ ")"

        location :: FileLocation
        location = RelativeFileLocation RelativeToOutputDir{
              outputDir = dir
            , localPath = localPath
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
