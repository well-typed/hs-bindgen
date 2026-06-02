module HsBindgen (
    hsBindgen

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
import HsBindgen.Backend.HsModule.Translation.Doxygen (ExportGroupTag (..),
                                                       ExportTags,
                                                       resolveExports)
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
import HsBindgen.Frontend.AST.TranslationUnit qualified as C
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.MangleNames.IsPass (StructNames (..),
                                                   TypedefNames (..))
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.ProcessIncludes qualified as ProcessIncludes
import HsBindgen.Frontend.RootHeader (UncheckedHashIncludeArg)
import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

import Doxygen.Parser (Doxygen, lookupGroupInfo, lookupGroupMembership)

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
        print $ PP.string msg
        -- We specifically use exit code 2 here; it means that the call to
        -- `libclang` has failed.
        exitWith (ExitFailure 2)
      _ -> throwIO e
    case eRes of
      Left err -> do
        print $ prettyForTrace err
        -- We specifically use exit code 3 here; it means that `hs-bindgen` ran
        -- to completion, but an error has occurred.
        exitWith (ExitFailure 3)
      Right r -> pure r

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

-- | Write the include graph to @STDOUT@ or a file.
writeIncludeGraph ::
     Boolean Regex
  -> Bool
  -> FilePolicy
  -> DirPolicy
  -> Maybe FilePath
  -> Artefact ()
writeIncludeGraph regex showPaths filePolicy dirPolicy mPath = do
    includeGraph <- getIncludeGraph
    let predicateUser, predicateRoot :: SourcePath -> Bool
        predicateUser (SourcePath p) = eval (\r -> matchTest r p) regex
        predicateRoot                = (/= RootHeader.name)
        opts = IncludeGraph.VisOpts{
            predicate = \p -> predicateUser p && predicateRoot p
          , showPaths = showPaths
          }
        rendered = IncludeGraph.renderMermaid opts includeGraph
    case mPath of
      Nothing   ->
        Lift $ delay $ WriteToStdOut $ StringContent rendered
      Just path ->
        write filePolicy dirPolicy "include graph" (UserSpecified path) rendered

-- | Write @use-decl@ graph to file.
writeUseDeclGraph :: FilePolicy -> DirPolicy -> Maybe FilePath -> Artefact ()
writeUseDeclGraph filePolicy dirPolicy mPath = do
    useDeclGraph <- getUseDeclGraph
    let rendered = UseDeclGraph.renderMermaid useDeclGraph
    case mPath of
      Nothing   ->
        Lift $ delay $ WriteToStdOut $ StringContent rendered
      Just path ->
        write filePolicy dirPolicy "use-decl graph" (UserSpecified path) rendered

-- | Write the parsed doxygen state to @STDOUT@ or a file.
writeDoxygen :: FilePolicy -> DirPolicy -> Maybe FilePath -> Artefact ()
writeDoxygen filePolicy dirPolicy mPath = do
    doxy <- DoxygenA
    let rendered = show doxy
    case mPath of
      Nothing   ->
        Lift $ delay $ WriteToStdOut $ StringContent rendered
      Just path ->
        write filePolicy dirPolicy "doxygen" (UserSpecified path) rendered

-- | Get bindings (single module).
getBindings :: ModuleRenderConfig -> Artefact String
getBindings mrc = do
    name   <- ModuleBaseName
    decls  <- FinalDecls
    tags   <- getExportTags
    when (all nullDecls decls) $ EmitTrace $ NoBindingsSingleModule name
    config <- getConfig
    let fns = config.frontend.fieldNamingStrategy
    pure $ render $
      translateModuleSingle fns mrc name (resolveExports tags) decls

-- | Write bindings to file.
writeBindings ::
     ModuleRenderConfig
  -> FilePolicy
  -> DirPolicy
  -> FilePath
  -> Artefact ()
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
  -> Artefact ()
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

-- | Get bindings (one module per binding category).
getBindingsMultiple :: ModuleRenderConfig -> Artefact (ByCategory_ (Maybe String))
getBindingsMultiple mrc = do
    name   <- ModuleBaseName
    decls  <- FinalDecls
    tags   <- getExportTags
    when (all nullDecls decls) $
      EmitTrace $ NoBindingsMultipleModules name
    config <- getConfig
    let fns = config.frontend.fieldNamingStrategy
    pure $ fmap render <$>
      translateModuleMultiple fns mrc name (resolveExports tags) decls

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
  -> Artefact ()
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
  -> Artefact ()
writeBindingSpec filePolicy dirPolicy path = do
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
              description = "Binding specifications"
            , location    = UserSpecified path
            , filePolicy  = filePolicy
            , dirPolicy   = dirPolicy
            , content     = ByteStringContent bs
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
getDeclIndex = (.meta.declIndex) <$> FrontendPassA FinalPass

getUseDeclGraph :: Artefact UseDeclGraph
getUseDeclGraph = (.meta.useDeclGraph) <$> FrontendPassA FinalPass

getDeclUseGraph :: Artefact DeclUseGraph
getDeclUseGraph = (.meta.declUseGraph) <$> FrontendPassA FinalPass

getOmittedTypes :: Artefact [(DeclId, SourcePath)]
getOmittedTypes =
    Map.toList . DeclIndex.getOmitted <$> getDeclIndex

getReifiedC :: Artefact [C.Decl Final]
getReifiedC = (.decls) <$> FrontendPassA FinalPass

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1549>
-- When we properly record aliases, we may not need this anymore.
getSquashedTypes :: Artefact [(DeclId, (SourcePath, Hs.Name Hs.NsTypeConstr))]
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

write :: FilePolicy -> DirPolicy -> String -> FileLocation -> String -> Artefact ()
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
  Export tags
-------------------------------------------------------------------------------}

-- | Fetch doxygen data and final C declarations, then precompute export tags.
getExportTags :: Artefact ExportTags
getExportTags = do
    doxy  <- DoxygenA
    final <- FrontendPassA FinalPass
    pure $ computeExportTags doxy final.decls

-- | Build an 'ExportTags' map from Doxygen metadata and the final C
-- declarations.
--
-- The resulting map is keyed by Haskell name. 'resolveExports' performs a single
-- lookup with no fallbacks.
--
-- For each 'C.Decl Final':
--
--  * If the originating C name belongs to a @\@defgroup@, the tag is
--    @'Grouped' path@ (root-to-leaf section titles).
--  * Otherwise, if the declaration is top-level (@null info.enclosing@), the
--    tag is 'Ungrouped' and the declaration is explicitly hoisted before any
--    section headers in the export list.
--  * Otherwise (nested, no group), the declaration is omitted from the map.
--    Missing keys are interpreted as 'Derived' by 'resolveExports', so the
--    declaration inherits the preceding 'Grouped' section in source order.
--
-- Backend-synthesised companion declarations are inserted under their own
-- Haskell name with the same tag as their parent:
--
--  * Typedef function pointers contribute an auxiliary newtype @F_Aux@ whose
--    name comes from @typedef.names.aux@; the @_Aux@ form is delayed during
--    Hs translation and therefore not adjacent to its parent in source
--    order, so an entry under its own Hs name is necessary to keep it in
--    its parent's section.
--  * Structs with flexible array members contribute an auxiliary type whose
--    name comes from @struct.names.flamAux@; same reasoning.
--
-- Example: given a C header with
--
-- > /** @defgroup core "Core Data Types" @{ */
-- > typedef struct { ... } config_t;
-- > /** @} */
--
-- and the Haskell name @Config_t@, this produces:
--
-- > fromList [("Config_t", Grouped ["Core Data Types"])]
computeExportTags :: Doxygen -> [C.Decl Final] -> ExportTags
computeExportTags doxy decls =
    Map.fromList $ concatMap declTagEntries decls
  where
    declTagEntries :: C.Decl Final -> [(Text, ExportGroupTag)]
    declTagEntries decl =
        case declTag decl of
          Nothing  -> []
          Just tag ->
            (decl.info.id.hsName.text, tag) : auxEntries decl.kind tag

    -- | Compute the tag for a top-level or grouped declaration.  Returns
    -- 'Nothing' for nested declarations without a group (those flow through
    -- as 'Derived' by lookup miss in 'resolveExports').
    declTag :: C.Decl Final -> Maybe ExportGroupTag
    declTag decl =
      case groupPath decl.info.id.cName.name.text of
        Just path -> Just (Grouped path)
        Nothing
          | null decl.info.enclosing -> Just Ungrouped
          | otherwise                -> Nothing

    -- | Extra entries for backend-synthesised companion decls that share
    -- their parent's group membership.
    auxEntries :: C.DeclKind Final -> ExportGroupTag -> [(Text, ExportGroupTag)]
    auxEntries kind tag = case kind of
      C.DeclTypedef typedef
        | Just (auxName, _) <- typedef.names.aux ->
            [(auxName.text, tag)]
      C.DeclStruct struct
        | Just auxName <- struct.names.flamAux ->
            [(auxName.text, tag)]
      _ -> []

    -- | Resolve the full group title path (root to leaf) for a C name.
    --
    -- Walks the Doxygen group hierarchy upward from the declaration's
    -- immediate group to the root, collecting titles along the way.
    --
    -- > groupPath "config_t"
    -- >   -- lookupGroupMembership → Just "core_types"
    -- >   -- lookupGroupInfo "core_types" → Just ("Core Data Types", Nothing)
    -- >   ==> Just ["Core Data Types"]
    -- >
    -- > groupPath "inner_typ"
    -- >   -- lookupGroupMembership → Just "inner_a"
    -- >   -- lookupGroupInfo "inner_a" → Just ("Inner A", Just "outer")
    -- >   -- lookupGroupInfo "outer"   → Just ("Outer Group", Nothing)
    -- >   ==> Just ["Outer Group", "Inner A"]
    groupPath :: Text -> Maybe [Text]
    groupPath declName = do
      groupName <- lookupGroupMembership declName doxy
      (title, mParent) <- lookupGroupInfo groupName doxy
      pure $ buildPath [title] mParent

    -- | Accumulate group titles from leaf to root, prepending each parent.
    buildPath :: [Text] -> Maybe Text -> [Text]
    buildPath acc Nothing = acc
    buildPath acc (Just parentName) =
      case lookupGroupInfo parentName doxy of
        Just (parentTitle, grandparent) ->
          buildPath (parentTitle : acc) grandparent
        Nothing -> acc

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
