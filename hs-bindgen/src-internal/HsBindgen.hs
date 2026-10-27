module HsBindgen (
    hsBindgenMacroLang
  , hsBindgenLibraryMacroLang
  , ExecutionMode(..)

    -- * Artefacts
  , Artefact(..)

    -- ** High-level artefacts
  , writeIncludeGraph
  , writeUseDeclGraph
  , writeDoxygen
  , getBindings
  , getBindingsMultiple
  , writeBindings
  , writeBindingsSingle
  , writeBindingsMultiple
  , writeBindingSpec
  , writeTests
  , ModuleHeaders(..)
  , moduleHeaders
  , getModuleHeaders

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
  , hsBindgenEMacroLang
  , hsBindgenLibraryEMacroLang
  ) where

import Control.Exception (Exception (..), catch)
import Control.Monad (join)
import Control.Monad.Except (MonadError (..), liftEither, withExceptT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Set qualified as Set
import System.Exit (ExitCode (..), exitWith)
import Text.SimplePrettyPrint qualified as PP

import Clang.CStandard
import Clang.HighLevel.Types (singleLocPath)
import Clang.Paths

import HsBindgen.Artefact
import HsBindgen.ArtefactM
import HsBindgen.Backend
import HsBindgen.Backend.Category
import HsBindgen.Backend.HsModule.Render
import HsBindgen.Backend.HsModule.Translation
import HsBindgen.Backend.HsModule.Translation.Doxygen (ExportTags,
                                                       computeExportTags,
                                                       resolveExports)
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.BindingSpec.Gen
import HsBindgen.Boot
import HsBindgen.Cache (getCached)
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
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.Select.IsPass (ProgramSlicing (..))
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.ProcessIncludes qualified as ProcessIncludes
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Translation
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Library (LibraryModule (..), LibraryPlan (..))
import HsBindgen.Library qualified as Library
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

-- | Main entry point to run @hs-bindgen@.
--
-- For a list of build artefacts, see the description and constructors of
-- 'Artefact'.
hsBindgenMacroLang ::
     Macro.HasTypes l
  => (ClangCStandard -> IO (Macro.Lang l))
  -> TracerConfig Level     TraceMsg
  -> TracerConfig SafeLevel SafeTraceMsg
  -> BindgenConfig
  -> ExecutionMode
  -> [C.UncheckedRootDirective]
  -> Artefact l a
  -> IO a
hsBindgenMacroLang mkMacroLang tu ts b m i a =
    exitOnError $ hsBindgenEMacroLang mkMacroLang tu ts b m i a

-- | Like 'hsBindgen' but does not exit with failure when an error has occurred.
hsBindgenEMacroLang ::
     forall a l. Macro.HasTypes l
  => (ClangCStandard -> IO (Macro.Lang l))
  -> TracerConfig Level     TraceMsg
  -> TracerConfig SafeLevel SafeTraceMsg
  -> BindgenConfig
  -> ExecutionMode
  -> [C.UncheckedRootDirective]
  -> Artefact l a
  -> IO (Either BindgenError a)
hsBindgenEMacroLang
  mkMacroLang
  tracerConfigUnsafe
  tracerConfigSafe
  config
  executionMode
  uncheckedRootDirectives
  artefacts = do
    eRes <- withTracer tracerConfigUnsafe $ \tracerUnsafe -> do
      -- 1. Boot.
      let tracerBoot :: Tracer BootMsg
          tracerBoot = contramap TraceBoot tracerUnsafe
      bootArtefact <-
        runBoot tracerBoot mkMacroLang config uncheckedRootDirectives
      -- 2. Frontend.
      let tracerFrontend :: Tracer FrontendMsg
          tracerFrontend = contramap TraceFrontend tracerUnsafe
      frontendArtefact <-
        runFrontend tracerFrontend config.frontend bootArtefact
      -- 3. Backend and 4. Artefacts.
      runBackendAndArtefacts
        tracerConfigSafe
        config
        bootArtefact
        frontendArtefact
        artefacts

    runExceptT $ do
      (r, as) <- liftEither $ first BindgenErrorReported eRes
      writeOutput tracerConfigSafe executionMode as
      pure r

-- | Run @hs-bindgen@ for a library, one module per header (@--library@)
--
-- Every module is a run of its own, and the artefact runs once for each. The
-- results come in include order of the headers of the modules. See
-- "HsBindgen.Library" for which headers get a module and what goes in each.
--
-- With @--library@, program slicing is on and every types module gets a boot
-- file, whatever the configuration says. A prescriptive binding specification
-- in the configuration is not used: there is none per module yet.
hsBindgenLibraryMacroLang ::
     Macro.HasTypes l
  => (ClangCStandard -> IO (Macro.Lang l))
  -> TracerConfig Level     TraceMsg
  -> TracerConfig SafeLevel SafeTraceMsg
  -> BindgenConfig
  -> ExecutionMode
  -> Library.LibraryDirs
  -> [C.UncheckedRootDirective]
  -> Artefact l a
  -> IO [a]
hsBindgenLibraryMacroLang mkMacroLang tu ts b m d i a =
    exitOnError $ hsBindgenLibraryEMacroLang mkMacroLang tu ts b m d i a

-- | Like 'hsBindgenLibraryMacroLang' but does not exit with failure when an
-- error has occurred.
hsBindgenLibraryEMacroLang ::
     forall a l. Macro.HasTypes l
  => (ClangCStandard -> IO (Macro.Lang l))
  -> TracerConfig Level     TraceMsg
  -> TracerConfig SafeLevel SafeTraceMsg
  -> BindgenConfig
  -> ExecutionMode
  -> Library.LibraryDirs
  -> [C.UncheckedRootDirective]
  -> Artefact l a
  -> IO (Either BindgenError [a])
hsBindgenLibraryEMacroLang
  mkMacroLang
  tracerConfigUnsafe
  tracerConfigSafe
  config0
  executionMode
  dirs
  uncheckedRootDirectives
  artefact = do
    eRes <- withTracer tracerConfigUnsafe $ \tracerUnsafe -> runExceptT $ do
      let tracerBoot :: Tracer BootMsg
          tracerBoot = contramap TraceBoot tracerUnsafe

          tracerFrontend :: Tracer FrontendMsg
          tracerFrontend = contramap TraceFrontend tracerUnsafe

          tracerBindingSpec :: Tracer BindingSpec.BindingSpecMsg
          tracerBindingSpec = contramap BootBindingSpec tracerBoot

      -- 1. Parse the library.
      boot   <- withoutPrescriptiveBindingSpec <$>
        liftIO (runBoot tracerBoot mkMacroLang config uncheckedRootDirectives)
      parsed <- liftIO $ parseHeaders tracerFrontend config.frontend boot
      info   <- liftIO $ getCached parsed.parseMeta

      -- 2. Apply the user's binding specifications, and 3. plan the modules.
      bound <- liftIO $ applyBindingSpecs tracerFrontend boot parsed
      unit  <- liftIO $ getCached bound.resolveBindingSpecs
      plan  <- withExceptT BindgenLibraryError $ liftEither $
        Library.planLibrary
          dirs
          boot.baseModule
          info.isMainHeader
          info.isInMainHeaderDir
          predicate
          unit

      -- 4. Translate the whole library. It spells the names of types with
      -- their types modules, so it is never rendered: only the binding
      -- specifications of its modules leave it.
      whole <- liftIO $
        prepareBindings
          tracerFrontend
          (selecting $ Library.librarySelection predicate dirs)
          (ByTypesModule QualifiedTypeNames plan.placement)
          bound
      (wholeSpec, _noOutput) <- liftIO $
        runBackendAndArtefacts tracerConfigSafe config boot whole getBindingSpec

      -- 5. Make the binding specifications of the modules.
      specs <- liftIO $
        traverse
          (BindingSpec.resolveWith
             tracerBindingSpec
             (IncludeGraph.mainHeaders info.includeGraph))
          (splitLibraryBindingSpec wholeSpec)

      -- 6. Generate each module, with the binding specifications of the others.
      let typesModules :: [Hs.ModuleName]
          typesModules = map Library.typesModule plan.modules
      outputs <- forM plan.modules $ \libraryModule -> do
        others <- liftIO $
          BindingSpec.mergeExtBindingSpecs tracerBindingSpec $
            Map.elems $ Map.delete (Library.typesModule libraryModule) specs
        let bootModule :: BootArtefact l
            bootModule = withoutPrescriptiveBindingSpec boot{
                baseModule              = libraryModule.name
              , externalBindingSpecs    = (<> others) <$> boot.externalBindingSpecs
              , libraryImports          = LibraryImports{
                    typesModules    = Set.fromList typesModules
                    -- Those that exist: the ones with a binding specification
                  , instanceModules =
                      Set.intersection libraryModule.reaches (Map.keysSet specs)
                  }
              }
        boundModule <- liftIO $
          applyBindingSpecs tracerFrontend bootModule parsed
        frontendModule <- liftIO $
          prepareBindings
            tracerFrontend
            (selecting $ Library.moduleSelection predicate libraryModule)
            (ByTypesModule PlainTypeNames plan.placement)
            boundModule
        liftIO $
          runBackendAndArtefacts
            tracerConfigSafe
            config
            bootModule
            frontendModule
            artefact

      -- 7. Check the files of all modules, before anything is written.
      let files :: [(BaseModuleName, [FilePath])]
          files = [
              (libraryModule.name, filePaths as)
            | (libraryModule, (_, as)) <- zip plan.modules outputs
            ]
      forM_ (NonEmpty.nonEmpty $ Library.fileClashes files) $
        throwError . BindgenLibraryError . Library.FileClash
      pure (map fst outputs, concatMap snd outputs)

    runExceptT $ do
      (rs, as) <- liftEither $ join $ first BindgenErrorReported eRes
      writeOutput tracerConfigSafe executionMode as
      pure rs
  where
    config :: BindgenConfig
    config =
        config0
          & #frontend % #programSlicing .~ EnableProgramSlicing
          & #backend  % #hsBootPolicy   .~ GenerateHsBoot

    predicate :: Boolean SelectionPredicate
    predicate = config.frontend.selectionPredicate

    selecting :: Boolean SelectionPredicate -> FrontendConfig
    selecting selection = config.frontend & #selectionPredicate .~ selection

    -- The empty prescriptive binding specification of the run's types module
    withoutPrescriptiveBindingSpec :: BootArtefact l -> BootArtefact l
    withoutPrescriptiveBindingSpec boot = boot{
          prescriptiveBindingSpec = pure $
            BindingSpec.empty (fromBaseModuleName boot.baseModule (Just CType))
        }

    filePaths :: [DelayedIO] -> [FilePath]
    filePaths as = [
          fileLocationToPath fileDescription.location
        | WriteToFile fileDescription <- as
        ]

-- | Exit when an error has occurred
--
-- Exit code 3 means that the call to @libclang@ has failed, and exit code 4
-- that @hs-bindgen@ ran to completion, but an error has occurred.
exitOnError :: IO (Either BindgenError a) -> IO a
exitOnError run = do
    eRes <- run `catch` \e ->
      case fromException e of
        Just (LibclangException msg) -> do
          print $ PP.string msg
          -- We specifically use exit code 3 here; it means that the call to
          -- `libclang` has failed.
          exitWith (ExitFailure 3)
        _ -> throwIO e
    case eRes of
      Left err -> do
        print $ prettyForTrace err
        -- We specifically use exit code 4 here; it means that `hs-bindgen` ran
        -- to completion, but an error has occurred.
        exitWith (ExitFailure 4)
      Right r -> pure r

-- | Run the backend, and then the artefact
runBackendAndArtefacts ::
     Macro.HasTypes l
  => TracerConfig SafeLevel SafeTraceMsg
  -> BindgenConfig
  -> BootArtefact l
  -> FrontendArtefact l
  -> Artefact l a
  -> IO (a, [DelayedIO])
runBackendAndArtefacts tracerConfigSafe config boot frontend artefact = do
    let tracerConfigBackend :: TracerConfig SafeLevel BackendMsg
        tracerConfigBackend = contramap SafeBackendMsg tracerConfigSafe
    backend <-
      withTracerSafe tracerConfigBackend $ \tracerSafe ->
        runBackend tracerSafe config boot frontend
    let tracerConfigArtefact :: TracerConfig SafeLevel ArtefactMsg
        tracerConfigArtefact = contramap SafeArtefactMsg tracerConfigSafe
    withTracerSafe tracerConfigArtefact $ \tracerSafe ->
      runArtefacts tracerSafe config boot frontend backend artefact

-- | Check the delayed IO actions against their policies, then run them
writeOutput ::
     TracerConfig SafeLevel SafeTraceMsg
  -> ExecutionMode
  -> [DelayedIO]
  -> ExceptT BindgenError IO ()
writeOutput tracerConfigSafe executionMode as = do
    let tracerConfigDelayedIO :: TracerConfig SafeLevel DelayedIOMsg
        tracerConfigDelayedIO = contramap SafeDelayedIOMsg tracerConfigSafe
    withTracerSafe tracerConfigDelayedIO $ \tracerSafe -> do
      -- Before creating directories or writing output files, we verify
      -- adherence to the provided policies.
      withExceptT BindgenDelayedIOError $ mapM_ checkPolicy as
      liftIO $ executeDelayedIOActions tracerSafe executionMode as

{-------------------------------------------------------------------------------
  High-level artefacts
-------------------------------------------------------------------------------}

-- | Write the include graph to @STDOUT@ or a file.
writeIncludeGraph ::
     Boolean Regex
  -> IncludeGraph.HeaderLabelStyle
  -> IncludeGraph.IncludeGraphFormat
  -> FilePolicy
  -> DirPolicy
  -> Maybe FilePath
  -> Artefact l ()
writeIncludeGraph regex labelStyle format filePolicy dirPolicy mPath = do
    includeGraph <- getIncludeGraph
    let predicateUser :: RealPath -> Bool
        predicateUser rp = eval (\r -> matchTest r (getRealPathText rp)) regex
        opts = IncludeGraph.VisOpts{
            predicate  = predicateUser
          , labelStyle = labelStyle
          }
        rendered = case format of
          IncludeGraph.SortedList -> IncludeGraph.renderSortedList opts includeGraph
          IncludeGraph.Mermaid    -> IncludeGraph.renderMermaid    opts includeGraph
    case mPath of
      Nothing   ->
        Lift $ delay $ WriteToStdOut $ StringContent rendered
      Just path ->
        write filePolicy dirPolicy "include graph" (UserSpecified path) rendered

-- | Write @use-decl@ graph to file.
writeUseDeclGraph :: FilePolicy -> DirPolicy -> Maybe FilePath -> Artefact l ()
writeUseDeclGraph filePolicy dirPolicy mPath = do
    useDeclGraph <- getUseDeclGraph
    let rendered = UseDeclGraph.renderMermaid useDeclGraph
    case mPath of
      Nothing   ->
        Lift $ delay $ WriteToStdOut $ StringContent rendered
      Just path ->
        write filePolicy dirPolicy "use-decl graph" (UserSpecified path) rendered

-- | Write the parsed doxygen state to @STDOUT@ or a file.
writeDoxygen :: FilePolicy -> DirPolicy -> Maybe FilePath -> Artefact l ()
writeDoxygen filePolicy dirPolicy mPath = do
    doxy <- DoxygenA
    let rendered = show doxy
    case mPath of
      Nothing   ->
        Lift $ delay $ WriteToStdOut $ StringContent rendered
      Just path ->
        write filePolicy dirPolicy "doxygen" (UserSpecified path) rendered

-- | Get bindings (single module).
getBindings :: ModuleRenderConfig -> Artefact l String
getBindings mrc = do
    name   <- ModuleBaseName
    dirs   <- RootDirectives
    decls  <- FinalDecls
    tags   <- getExportTags
    when (all nullDecls decls) $ EmitTrace $ NoBindingsSingleModule name
    config <- getConfig
    let fns = config.frontend.fieldNamingStrategy
    pure $ render $
      translateModuleSingle fns mrc dirs name (resolveExports tags) decls

-- | Write bindings to file.
writeBindings ::
     ModuleRenderConfig
  -> FilePolicy
  -> DirPolicy
  -> FilePath
  -> Artefact l ()
writeBindings mrc filePolicy dirPolicy path = do
    bindings <- getBindings mrc
    write filePolicy dirPolicy "bindings" (UserSpecified path) bindings

-- | Write bindings to a directory (single module combining all categories).
--
-- Unlike 'writeBindings', this writes to a directory and automatically
-- constructs the file path from the module name, similar to
-- 'writeBindingsMultiple' but generating only one file.
writeBindingsSingle ::
     ModuleRenderConfig
  -> FilePolicy
  -> DirPolicy
  -> FilePath
  -> Artefact l ()
writeBindingsSingle mrc filePolicy dirPolicy hsOutputDir = do
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

    write filePolicy dirPolicy "bindings" location bindings

-- | Get bindings (one module per binding category), each with its boot file, if
-- it has one.
getBindingsMultiple :: ModuleRenderConfig -> Artefact l (ByCategory_ (Maybe (String, Maybe String)))
getBindingsMultiple mrc = do
    name    <- ModuleBaseName
    library <- LibraryImportsA
    dirs    <- RootDirectives
    decls   <- FinalDecls
    tags    <- getExportTags
    when (all nullDecls decls) $
      EmitTrace $ NoBindingsMultipleModules name
    config <- getConfig
    let fns = config.frontend.fieldNamingStrategy
    pure $ fmap (bimap render (fmap renderHsBoot)) <$>
      translateModuleMultiple
        fns
        mrc
        config.backend.hsBootPolicy
        library
        dirs
        name
        (resolveExports tags)
        decls

-- | Write bindings to files in provided output directory.
--
-- Each file contains a different binding category.
--
-- If no file is given, print to standard output.
writeBindingsMultiple ::
     ModuleRenderConfig
  -> FilePolicy
  -> DirPolicy
  -> FilePath
  -> Artefact l ()
writeBindingsMultiple mrc filePolicy dirPolicy hsOutputDir = do
    moduleBaseName     <- ModuleBaseName
    bindingsByCategory <- getBindingsMultiple mrc
    writeByCategory
      filePolicy
      dirPolicy
      "Bindings"
      hsOutputDir
      moduleBaseName
      bindingsByCategory

-- | Write binding specifications to file.
writeBindingSpec ::
     FilePolicy
  -> DirPolicy
  -> FilePath
  -> Artefact l ()
writeBindingSpec filePolicy dirPolicy path = do
    includeGraph <- getIncludeGraph
    declIndex    <- getDeclIndex
    spec         <- getBindingSpec
    let bs =
          encodeBindingSpec
            (BindingSpec.getFormat path)
            includeGraph
            declIndex
            spec
        fileDescription = FileDescription {
              description = "Binding specifications"
            , location    = UserSpecified path
            , filePolicy  = filePolicy
            , dirPolicy   = dirPolicy
            , content     = ByteStringContent bs
            }
    Lift $ delay $ WriteToFile fileDescription

-- | Create test suite in directory.
writeTests :: FilePath -> Artefact l ()
writeTests _testDir = do
    -- moduleBaseName  <- ModuleBaseName
    -- rootDirectives  <- RootDirectives
    -- hsDecls         <- HsDecls
    -- liftIO $
    --   genTests
    --     rootDirectives
    --     hsDecls
    --     moduleBaseName
    --     testDir
    throwPure_TODO 22 "Test generation integrated into the artefact API"

-- | A module and the headers its bindings come from
--
-- @preprocess --dry-run@ prints it as @M: a.h, b.h@, so that you can see which
-- headers a module covers without generating it.
data ModuleHeaders = ModuleHeaders {
      moduleName :: BaseModuleName
      -- | Each header as it was @#include@d, in include order
    , headers    :: [C.HashIncludeArg]
    }
  deriving stock (Show, Eq)

-- | The headers of a module, from the declarations it has bindings for
--
-- The caller decides which declarations make up the module. Each header that
-- contains one of them is listed once, spelled as it was @#include@d, in include
-- order: a header comes after the headers it includes. Macros defined by root
-- directives or @-D@ options are in no header, so the list can be empty.
moduleHeaders ::
     IncludeGraph.IncludeOrder
  -> BaseModuleName
  -> [C.Decl l Final]
  -> ModuleHeaders
moduleHeaders order name decls = ModuleHeaders{
      moduleName = name
    , headers    = map snd $ inIncludeOrder $ Map.toList headers
    }
  where
    -- All declarations in a header have the same include argument, so it does
    -- not matter which one 'Map.fromList' keeps.
    headers :: Map RealPath C.HashIncludeArg
    headers = Map.fromList [
        (path, info.includeArg)
      | decl <- decls
      , C.InHeader path   <- [singleLocPath decl.info.loc]
      , C.FromHeader info <- [decl.info.origin]
      ]

    inIncludeOrder :: [(RealPath, a)] -> [(RealPath, a)]
    inIncludeOrder =
      List.sortOn (IncludeGraph.lookupIncludeOrder order . C.InHeader . fst)

-- | The module of this run and its headers
--
-- Passes every reified declaration to 'moduleHeaders', since each one has a
-- binding in the module: the CLI always generates at least one category of
-- binding for every kind of declaration. 'Nothing' if there is nothing to bind.
getModuleHeaders :: Artefact l (Maybe ModuleHeaders)
getModuleHeaders = do
    name         <- ModuleBaseName
    decls        <- getReifiedC
    includeGraph <- getIncludeGraph
    pure $
      if null decls
        then Nothing
        else Just $
          moduleHeaders (IncludeGraph.toIncludeOrder includeGraph) name decls

{-------------------------------------------------------------------------------
  Low-level artefacts
-------------------------------------------------------------------------------}

getConfig :: Artefact l BindgenConfig
getConfig = Lift askConfig

getGetMainHeaders :: Artefact l ProcessIncludes.GetMainHeaders
getGetMainHeaders = (.getMainHeaders) <$> ParseInfoA

getIncludeGraph :: Artefact l IncludeGraph
getIncludeGraph = (.includeGraph) <$> ParseInfoA

getDeclIndex :: Artefact l (DeclIndex l)
getDeclIndex = (.meta.declIndex) <$> FrontendPassA FinalPass

getUseDeclGraph :: Artefact l UseDeclGraph
getUseDeclGraph = (.meta.useDeclGraph) <$> FrontendPassA FinalPass

getDeclUseGraph :: Artefact l DeclUseGraph
getDeclUseGraph = (.meta.declUseGraph) <$> FrontendPassA FinalPass

getOmittedTypes :: Artefact l [(C.DeclId, RealPath)]
getOmittedTypes =
    Map.toList . DeclIndex.getOmitted <$> getDeclIndex

getReifiedC :: Artefact l [C.Decl l Final]
getReifiedC = (.decls) <$> FrontendPassA FinalPass

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1549>
-- When we properly record aliases, we may not need this anymore.
getSquashedTypes :: Artefact l [(C.DeclId, (RealPath, Hs.Name Hs.NsTypeConstr))]
getSquashedTypes = do
  decls <- getReifiedC
  let translatedDeclIds = Set.fromList $ map (.info.id.cName) decls
  declIndex <- getDeclIndex
  pure $ Map.toList $ DeclIndex.getSquashed declIndex translatedDeclIds

getDependencies :: Artefact l [RealPath]
getDependencies = IncludeGraph.toSortedList <$> getIncludeGraph

-- | The binding specification of the types of the types module
getBindingSpec :: Artefact l BindingSpec.UnresolvedBindingSpec
getBindingSpec = do
    moduleBaseName <- ModuleBaseName
    getMainHeaders <- getGetMainHeaders
    omittedTypes   <- getOmittedTypes
    squashedTypes  <- getSquashedTypes
    hsDecls        <- HsDecls
    -- Binding specifications only specify types.
    pure $
      genUnresolvedBindingSpec
        (fromBaseModuleName moduleBaseName (Just CType))
        getMainHeaders
        omittedTypes
        squashedTypes
        (view (lensForCategory CType) hsDecls)

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

write :: FilePolicy -> DirPolicy -> String -> FileLocation -> String -> Artefact l ()
write filePolicy dirPolicy what loc str
  | null str =
    EmitTrace $ SkipWriteToFileNoBindings loc
  | otherwise =
    Lift $ delay $
      WriteToFile $ FileDescription what loc filePolicy dirPolicy (StringContent str)

writeByCategory ::
     FilePolicy
  -> DirPolicy
  -> String
  -> FilePath
  -> BaseModuleName
  -> ByCategory_ (Maybe (String, Maybe String))
  -> Artefact l ()
writeByCategory filePolicy dirPolicy what dir moduleBaseName =
    sequence_ . mapWithCategory_ writeCategory
  where
    writeCategory :: Category -> Maybe (String, Maybe String) -> Artefact l ()
    writeCategory _    Nothing                 = pure ()
    writeCategory cat (Just (str, mBootStr)) = do
        write filePolicy dirPolicy whatWithCategory (location Hs.moduleNamePath) str
        forM_ mBootStr $
          write filePolicy dirPolicy whatBootWithCategory (location Hs.moduleNameBootPath)
      where
        moduleName :: Hs.ModuleName
        moduleName = fromBaseModuleName moduleBaseName (Just cat)

        whatWithCategory, whatBootWithCategory :: String
        whatWithCategory     = what ++ " (" ++ show cat ++ ")"
        whatBootWithCategory = what ++ " boot file (" ++ show cat ++ ")"

        location :: (Hs.ModuleName -> FilePath) -> FileLocation
        location path = RelativeFileLocation RelativeToOutputDir{
              outputDir = dir
            , localPath = path moduleName
            }

nullDecls :: ([a], [b]) -> Bool
nullDecls (xs, ys) = null xs && null ys

{-------------------------------------------------------------------------------
  Export tags
-------------------------------------------------------------------------------}

-- | Fetch doxygen data and final C declarations, then precompute export tags.
getExportTags :: Artefact l ExportTags
getExportTags = do
    doxy  <- DoxygenA
    final <- FrontendPassA FinalPass
    pure $ computeExportTags doxy final.decls

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data BindgenError =
      BindgenErrorReported  AnErrorHappened
    | BindgenDelayedIOError DelayedIOError
    | BindgenLibraryError   Library.LibraryError
    deriving stock (Show)

instance PrettyForTrace BindgenError where
  prettyForTrace = \case
    BindgenErrorReported  e -> prettyForTrace e
    BindgenDelayedIOError e -> prettyForTrace e
    BindgenLibraryError   e -> prettyForTrace e

{-------------------------------------------------------------------------------
  Traces
-------------------------------------------------------------------------------}

data SafeTraceMsg =
      SafeBackendMsg   BackendMsg
    | SafeArtefactMsg  ArtefactMsg
    | SafeDelayedIOMsg DelayedIOMsg
  deriving (Show, Generic, PrettyForTrace, IsTrace SafeLevel)
