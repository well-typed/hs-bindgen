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
  ) where

import Data.Map qualified as Map
import System.Directory
    ( createDirectoryIfMissing,
      createDirectoryIfMissing,
      doesFileExist )
import System.FilePath
    ( takeDirectory, (</>), takeDirectory, (</>) )
import Data.Foldable (foldrM)
import Data.Foldable qualified as Foldable
import HsBindgen.Artefact
import HsBindgen.Backend
import HsBindgen.Backend.HsModule.Render
import HsBindgen.Backend.SHs.AST
import HsBindgen.BindingSpec.Gen
import HsBindgen.BindingSpec.Private.V1 qualified as BindingSpec
import HsBindgen.Boot
import HsBindgen.Config.Internal
import HsBindgen.Frontend
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.RootHeader (UncheckedHashIncludeArg)
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Test (genTests)
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
hsBindgen
  tracerConfig
  bindgenConfig@BindgenConfig{..}
  uncheckedHashIncludeArgs
  artefacts = do
    (result, fsActions) <- fmap (either ((, []) . Left) id) $ withTracer tracerConfig $ \tracer tracerUnsafeRef -> do
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
      withTracerSafe tracerConfigSafe $ \tracerSafe -> do
        runArtefacts
          tracerSafe
          tracerUnsafeRef
          bootArtefact
          frontendArtefact
          backendArtefact
          artefacts

    -- Execute file system actions based on FileOverwritePolicy
    value <- either throwIO pure result
    executeFileSystemActions (backendFileOverwrite bindgenBackendConfig) fsActions
    return value
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
  Execute file system actions
-------------------------------------------------------------------------------}

-- | Execute collected file system actions based on FileOverwritePolicy
executeFileSystemActions :: FileOverwritePolicy -> [FileSystemAction] -> IO ()
executeFileSystemActions fop actions = do
  -- Get the first file path that exists if any
  mbPath <-
    foldrM (\f mbp -> do
             case f of
               WriteFile _ path _
                 | Just _ <- mbp -> pure mbp
                 | otherwise -> do
                   fileExists <- doesFileExist path
                   pure $ if fileExists
                             then Just path
                             else mbp
           ) Nothing actions
  case fop of
    ProtectExistingFiles
      | Just path <- mbPath -> throwIO (FileAlreadyExistsException path)
    _ -> forM_ actions $ \case
          WriteFile _ path content -> do
            createDirectoryIfMissing True (takeDirectory path)
            case content of
              TextContent str -> writeFile path str
              BindingSpecContent ubs -> BindingSpec.writeFile path ubs

{-------------------------------------------------------------------------------
  Custom build artefacts
-------------------------------------------------------------------------------}

-- | Write the include graph to `STDOUT` or a file.
writeIncludeGraph :: Maybe FilePath -> Artefact ()
writeIncludeGraph mPath = do
    (p, includeGraph) <- IncludeGraph
    write "include graph" mPath $
      IncludeGraph.dumpMermaid p includeGraph

-- | Write @use-decl@ graph to file.
writeUseDeclGraph :: Maybe FilePath -> Artefact ()
writeUseDeclGraph mPath = do
    index <- DeclIndex
    useDeclGraph <- UseDeclGraph
    write "use-decl graph" mPath $
      UseDeclGraph.dumpMermaid index useDeclGraph

-- | Get bindings (single module).
getBindings :: Safety -> Artefact String
getBindings safety = do
    finalModule <- finalModuleArtefact
    Lift $ pure . render $ finalModule
  where finalModuleArtefact = case safety of
          Safe   -> FinalModuleSafe
          Unsafe -> FinalModuleUnsafe

-- | Write bindings to file.
--
-- If no file is given, print to standard output.
writeBindings :: Safety -> Maybe FilePath -> Artefact ()
writeBindings safety mPath = do
    bindings <- getBindings safety
    write "bindings" mPath bindings

-- | Get bindings (one module per binding category).
getBindingsMultiple :: Artefact (ByCategory String)
getBindingsMultiple = fmap render <$> FinalModules

-- | Write bindings to files in provided output directory.
--
-- Each file contains a different binding category.
--
-- If no file is given, print to standard output.
writeBindingsMultiple :: FilePath -> Artefact ()
writeBindingsMultiple hsOutputDir = do
    moduleBaseName     <- FinalModuleBaseName
    bindingsByCategory <- getBindingsMultiple
    writeByCategory "bindings" hsOutputDir moduleBaseName bindingsByCategory

-- | Write binding specifications to file.
writeBindingSpec :: FilePath -> Artefact ()
writeBindingSpec path = do
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
  FileWrite "binding specifications" path (BindingSpecContent bindingSpec)

-- | Create test suite in directory.
writeTests :: FilePath -> Artefact ()
writeTests testDir = do
    moduleBaseName  <- FinalModuleBaseName
    hashIncludeArgs <- HashIncludeArgs
    hsDecls         <- HsDecls
    liftIO $
      genTests
        hashIncludeArgs
        hsDecls
        moduleBaseName
        testDir

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

write :: String -> Maybe FilePath -> String -> Artefact ()
write _    Nothing     str = liftIO $ putStrLn str
write what (Just path) str = FileWrite what path (TextContent str)

writeByCategory ::
     String
  -> FilePath
  -> BaseModuleName
  -> ByCategory String
  -> Artefact ()
writeByCategory what hsOutputDir moduleBaseName =
    Foldable.foldl' (>>) (pure ()) . map (uncurry writeCategory) . Map.toList . unByCategory
  where
    writeCategory :: BindingCategory -> String -> Artefact ()
    writeCategory cat str = do
        write whatWithCategory (Just path) str
      where
        moduleName :: Hs.ModuleName
        moduleName = fromBaseModuleName moduleBaseName (Just cat)

        path :: FilePath
        path = hsOutputDir </> Hs.moduleNamePath moduleName

        whatWithCategory :: String
        whatWithCategory = what ++ " (" ++ show cat ++ ")"

