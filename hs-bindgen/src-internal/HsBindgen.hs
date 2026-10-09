module HsBindgen (
    hsBindgenMacroLang

    -- * Artefacts
  , Artefact(..)
  , ModuleScope(..)

    -- ** High-level artefacts
  , writeIncludeGraph
  , writeUseDeclGraph
  , writeDoxygen
  , getBindings
  , getBindingsMultiple
  , writeBindings
  , writeBindingsSingle
  , getBindingsFileSingle
  , writeBindingsMultiple
  , getBindingsFilesMultiple
  , generatedBindingSpec
  , writeBindingSpec
  , getBindingSpecFileIn
  , writeTests

    -- ** A library as one module per header
  , forEachModule
  , Written(..)
  , writeLibrary

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
  ) where

import Control.Exception (Exception (..), catch)
import Control.Monad.Except (MonadError (..), withExceptT)
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Set qualified as Set
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((<.>))
import Text.SimplePrettyPrint qualified as PP

import Clang.CStandard
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
import HsBindgen.Cache (Cached (..))
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
import HsBindgen.Frontend.Pass.Select.IsPass (ProgramSlicing (..),
                                              SelectConfig (..),
                                              SelectionRoots (..))
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.ProcessIncludes qualified as ProcessIncludes
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Translation
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Library (Library, LibraryError, LibraryPath)
import HsBindgen.Library qualified as Library
import HsBindgen.Library.Plan (Module (..))
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
  -> [C.UncheckedRootDirective]
  -> Artefact l a
  -> IO a
hsBindgenMacroLang mkMacroLang tu ts b i a = do
    eRes <- hsBindgenEMacroLang mkMacroLang tu ts b i a `catch` \e ->
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

-- | Like 'hsBindgen' but does not exit with failure when an error has occurred.
hsBindgenEMacroLang ::
     forall a l. Macro.HasTypes l
  => (ClangCStandard -> IO (Macro.Lang l))
  -> TracerConfig Level     TraceMsg
  -> TracerConfig SafeLevel SafeTraceMsg
  -> BindgenConfig
  -> [C.UncheckedRootDirective]
  -> Artefact l a
  -> IO (Either BindgenError a)
hsBindgenEMacroLang
  mkMacroLang
  tracerConfigUnsafe
  tracerConfigSafe
  config
  uncheckedRootDirectives
  artefacts = do
    let tracerConfigBackend :: TracerConfig SafeLevel BackendMsg
        tracerConfigBackend = contramap SafeBackendMsg tracerConfigSafe

        tracerConfigArtefact :: TracerConfig SafeLevel ArtefactMsg
        tracerConfigArtefact = contramap SafeArtefactMsg tracerConfigSafe
    eRes <- withTracer tracerConfigUnsafe $ \tracerUnsafe ->
      withTracerSafe tracerConfigBackend  $ \tracerBackend  ->
      withTracerSafe tracerConfigArtefact $ \tracerArtefact -> do
        -- 1. Boot.
        bootArtefact <-
          runBoot
            (contramap TraceBoot tracerUnsafe)
            mkMacroLang
            config
            uncheckedRootDirectives
        -- 2. Frontend.
        --
        -- The passes that do not depend on the module are shared by every
        -- module that is generated from the translation unit.
        sharedFrontend <-
          runFrontendShared
            (contramap TraceFrontend tracerUnsafe)
            config.frontend
            bootArtefact
        let -- The stages after the shared part of the frontend
            stagesFrom ::
                 Tracer TraceMsg
              -> SelectConfig
              -> BootArtefact l
              -> IO (Stages l)
            stagesFrom tracer selectConfig boot = do
              frontend <-
                runFrontendModule
                  (contramap TraceFrontend tracer)
                  config.frontend.fieldNamingStrategy
                  selectConfig
                  boot
                  sharedFrontend
              -- 3. Backend.
              backend <- runBackend tracerBackend config boot frontend
              pure Stages{
                  boot     = boot
                , frontend = frontend
                , backend  = backend
                }

            -- One module of several: its own name, binding specifications
            -- and selection, and every message under its name
            stagesOfModule :: ModuleScope -> Cached (Stages l)
            stagesOfModule scope = liftIO $ do
              let tracer :: Tracer TraceMsg
                  tracer =
                    contramap (TraceModule . InModule scope.baseModule)
                      tracerUnsafe
              boot <-
                bootModule (contramap TraceBoot tracer) scope bootArtefact
              stagesFrom
                tracer
                SelectConfig{
                    programSlicing = EnableProgramSlicing
                  , roots          = RootsByName scope.roots
                  }
                boot
        stages <-
          stagesFrom
            tracerUnsafe
            (configuredSelection config.frontend)
            bootArtefact
        -- 4. Artefacts.
        runArtefacts tracerArtefact config stagesOfModule stages artefacts

    let tracerConfigDelayedIO :: TracerConfig SafeLevel DelayedIOMsg
        tracerConfigDelayedIO = contramap SafeDelayedIOMsg tracerConfigSafe
    runExceptT $ withTracerSafe tracerConfigDelayedIO $ \tracerSafe ->
      case eRes of
        Left er       -> throwError $ BindgenErrorReported er
        Right (r, as) -> do
          -- Before creating directories or writing output files, we verify
          -- adherence to the provided policies.
          withExceptT BindgenDelayedIOError $ checkPolicies as
          liftIO $ executeDelayedIOActions tracerSafe as
          pure r

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
    write filePolicy dirPolicy "bindings"
      (moduleLocation hsOutputDir moduleBaseName Nothing)
      bindings

-- | The file that 'writeBindingsSingle' writes, if it writes one
--
-- Nothing is registered for writing.
getBindingsFileSingle ::
     ModuleRenderConfig
  -> FilePolicy
  -> DirPolicy
  -> FilePath
  -> Artefact l [FileDescription]
getBindingsFileSingle mrc filePolicy dirPolicy hsOutputDir = do
    moduleBaseName <- ModuleBaseName
    bindings       <- getBindings mrc
    pure [
        FileDescription{
            description = "bindings"
          , location    = moduleLocation hsOutputDir moduleBaseName Nothing
          , filePolicy  = filePolicy
          , dirPolicy   = dirPolicy
          , content     = StringContent bindings
          }
      | not (null bindings)
      ]

-- | Get bindings (one module per binding category).
getBindingsMultiple :: ModuleRenderConfig -> Artefact l (ByCategory_ (Maybe String))
getBindingsMultiple mrc = do
    name   <- ModuleBaseName
    dirs   <- RootDirectives
    decls  <- FinalDecls
    tags   <- getExportTags
    when (all nullDecls decls) $
      EmitTrace $ NoBindingsMultipleModules name
    config <- getConfig
    let fns = config.frontend.fieldNamingStrategy
    pure $ fmap render <$>
      translateModuleMultiple fns mrc dirs name (resolveExports tags) decls

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

-- | The files that 'writeBindingsMultiple' writes
--
-- Nothing is registered for writing.
getBindingsFilesMultiple ::
     ModuleRenderConfig
  -> FilePolicy
  -> DirPolicy
  -> FilePath
  -> Artefact l [FileDescription]
getBindingsFilesMultiple mrc filePolicy dirPolicy hsOutputDir = do
    moduleBaseName     <- ModuleBaseName
    bindingsByCategory <- getBindingsMultiple mrc
    pure $ catMaybes $ toList $
      mapWithCategory_ (fileOf moduleBaseName) bindingsByCategory
  where
    fileOf :: BaseModuleName -> Category -> Maybe String -> Maybe FileDescription
    fileOf moduleBaseName cat mBindings = do
        bindings <- mBindings
        pure FileDescription{
            description = categoryDescription "Bindings" cat
          , location    = moduleLocation hsOutputDir moduleBaseName (Just cat)
          , filePolicy  = filePolicy
          , dirPolicy   = dirPolicy
          , content     = StringContent bindings
          }

-- | The binding specification of the generated module
--
-- 'writeBindingSpec' writes this value.  It can also be used as it is, as an
-- external binding specification for another module of the same translation
-- unit, without a file in between.
generatedBindingSpec :: Artefact l BindingSpec.BindingSpec
generatedBindingSpec = do
    moduleBaseName <- ModuleBaseName
    includeGraph   <- getIncludeGraph
    getMainHeaders <- getGetMainHeaders
    omittedTypes   <- getOmittedTypes
    squashedTypes  <- getSquashedTypes
    hsDecls        <- HsDecls
    -- Binding specifications only specify types.
    pure $
      genBindingSpec
        (fromBaseModuleName moduleBaseName (Just CType))
        includeGraph
        getMainHeaders
        omittedTypes
        squashedTypes
        (view (lensForCategory CType) hsDecls)

-- | Write binding specifications to file.
writeBindingSpec ::
     FilePolicy
  -> DirPolicy
  -> FilePath
  -> Artefact l ()
writeBindingSpec filePolicy dirPolicy path = do
    spec <- generatedBindingSpec
    file <- bindingSpecFile filePolicy dirPolicy (UserSpecified path) spec
    Lift $ delay $ WriteToFile file

-- | The file for the binding specification of the generated module, in the
-- given directory
--
-- The file has the name of the module, as in @Lib.Types.yaml@.  Nothing is
-- registered for writing.
getBindingSpecFileIn ::
     FilePolicy
  -> DirPolicy
  -> FilePath
  -> Artefact l FileDescription
getBindingSpecFileIn filePolicy dirPolicy dir = do
    spec <- generatedBindingSpec
    let location = RelativeFileLocation RelativeToOutputDir{
            outputDir = dir
          , localPath =
              Hs.moduleNameToString (BindingSpec.moduleName spec) <.> "yaml"
          }
    bindingSpecFile filePolicy dirPolicy location spec

-- | A binding specification of the generated module as a file
--
-- The extension of the file says how the specification is written.
bindingSpecFile ::
     FilePolicy
  -> DirPolicy
  -> FileLocation
  -> BindingSpec.BindingSpec
  -> Artefact l FileDescription
bindingSpecFile filePolicy dirPolicy location spec = do
    includeGraph <- getIncludeGraph
    declIndex    <- getDeclIndex
    pure FileDescription {
        description = "Binding specifications"
      , location    = location
      , filePolicy  = filePolicy
      , dirPolicy   = dirPolicy
      , content     = ByteStringContent $
          encodeBindingSpec
            (BindingSpec.getFormat (fileLocationToPath location))
            includeGraph
            declIndex
            spec
      }

{-------------------------------------------------------------------------------
  A library as one module per header
-------------------------------------------------------------------------------}

-- | Compute an artefact for each module of a library
--
-- A run has two parts.  The headers are parsed once, as one translation unit,
-- and the modules are planned from what the frontend knows about it: which
-- library header owns what, which headers share a module, and in which order
-- the modules go ('Library.modules').  Then the artefact is computed for one
-- module after the other ('WithModule').  Only the passes that depend on the
-- module run again for each of them; the parse is shared.
--
-- A module is given the binding specifications of all the modules before it,
-- in memory.  So a type is generated by the first module that has it, and the
-- modules after it import it from there: the module of @draw.h@ finds
-- @struct shape@ in the specification of the module of @shape.h@, which comes
-- first.
--
-- The result has the modules in the order in which they were generated.
forEachModule :: forall l a.
     Library
  -> Boolean SelectionPredicate
     -- ^ The declarations that bindings are generated for
  -> Artefact l a
  -> Artefact l
       (Either LibraryError [(BaseModuleName, Module LibraryPath C.DeclId, a)])
forEachModule library predicate perModule = do
    base         <- ModuleBaseName
    includeGraph <- getIncludeGraph
    unit         <- WithModule (planning base) $ FrontendPassA MangleNamesPass
    paths        <- Lift $ runCached $ Cached $
                      Library.libraryPaths library $
                        IncludeGraph.toSortedList includeGraph
    case Library.modules base paths includeGraph predicate unit.meta of
      Left err   -> pure $ Left err
      Right mods -> Right <$> generate [] mods
  where
    -- The modules are planned in a scope of their own, which has the name of
    -- the base module and generates nothing.  No module of the library has
    -- that name.  So the plan is made with the binding specifications that
    -- are for every module, and not with a prescriptive one that names a
    -- module of the library.
    planning :: BaseModuleName -> ModuleScope
    planning base = ModuleScope{
          baseModule = base
        , imported   = []
        , roots      = Set.empty
        }

    -- The first argument has the specifications of the modules so far
    generate ::
         [BindingSpec.ExternalBindingSpec]
      -> [(BaseModuleName, Module LibraryPath C.DeclId)]
      -> Artefact l [(BaseModuleName, Module LibraryPath C.DeclId, a)]
    generate _     []                 = pure []
    generate specs ((name, m) : rest) = do
        (spec, x) <- WithModule (scope name m specs) $
                       (,) <$> generatedBindingSpec <*> perModule
        ((name, m, x) :) <$> generate (specs ++ [spec]) rest

    scope ::
         BaseModuleName
      -> Module LibraryPath C.DeclId
      -> [BindingSpec.ExternalBindingSpec]
      -> ModuleScope
    scope name m specs = ModuleScope{
          baseModule = name
        , imported   = specs
          -- An unnamed declaration is selected with the declaration that
          -- uses it, and the name that it was given is not a name to select.
        , roots      = Set.fromList [
              declId.name
            | declId <- Set.toList m.decls
            , not declId.isUnnamed
            ]
        }

-- | A module that was written for a library
data Written = Written {
      -- | The name of the module
      name :: BaseModuleName

      -- | The headers that the module was generated for
    , headers :: NonEmpty LibraryPath
    }
  deriving stock (Show, Eq)

-- | Write a library as one module per header
--
-- The artefact gives the files of a module, and is computed for each module of
-- the library ('forEachModule').  A module without files is not written.
--
-- No file is registered for writing before every module has been generated.
-- So when a module cannot be generated, nothing is written at all.  The same
-- holds when two of the modules to write have one name
-- ('Library.distinctNames').
writeLibrary :: forall l.
     Library
  -> Boolean SelectionPredicate
  -> Artefact l [FileDescription]
  -> Artefact l (Either LibraryError [Written])
writeLibrary library predicate filesOfModule =
    forEachModule library predicate rendered >>= \case
      Left err   -> pure $ Left err
      Right mods -> do
        let written = filter (\(_name, _m, files) -> not (null files)) mods
        case Library.distinctNames (map named written) of
          Left err -> pure $ Left err
          Right () -> do
            forM_ written $ \(_name, _m, files) ->
              mapM_ (Lift . delay . WriteToFile) files
            pure $ Right [
                Written{name = name, headers = m.headers}
              | (name, m, _files) <- written
              ]
  where
    -- A module with the header that it is named after
    named ::
         (BaseModuleName, Module LibraryPath C.DeclId, [FileDescription])
      -> (BaseModuleName, LibraryPath)
    named (name, m, _files) = (name, NonEmpty.head m.headers)

    -- A file that has not been rendered yet keeps everything alive that it is
    -- rendered from.  So the files of a module are rendered before the next
    -- module is generated.
    rendered :: Artefact l [FileDescription]
    rendered = do
        files <- filesOfModule
        sum (map (size . (.content)) files) `seq` pure files

    size :: FileContent -> Int
    size = \case
      StringContent     str -> length str
      ByteStringContent bs  -> BS.length bs

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
  -> ByCategory_ (Maybe String)
  -> Artefact l ()
writeByCategory filePolicy dirPolicy what dir moduleBaseName =
    sequence_ . mapWithCategory_ writeCategory
  where
    writeCategory :: Category -> Maybe String -> Artefact l ()
    writeCategory _    Nothing   = pure ()
    writeCategory cat (Just str) =
        write filePolicy dirPolicy
          (categoryDescription what cat)
          (moduleLocation dir moduleBaseName (Just cat))
          str

-- | What the output for a binding category is called
categoryDescription :: String -> Category -> String
categoryDescription what cat = what ++ " (" ++ show cat ++ ")"

-- | Where a module goes below the output directory
--
-- Without a category, it is the module that holds every category.
moduleLocation :: FilePath -> BaseModuleName -> Maybe Category -> FileLocation
moduleLocation dir moduleBaseName category =
    RelativeFileLocation RelativeToOutputDir{
        outputDir = dir
      , localPath = Hs.moduleNamePath $
          fromBaseModuleName moduleBaseName category
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
