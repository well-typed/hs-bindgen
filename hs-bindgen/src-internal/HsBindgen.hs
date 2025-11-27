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

import Control.Monad (join)
import Control.Monad.Trans.Reader (ask)
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (<.>), (</>))

import HsBindgen.Artefact
import HsBindgen.Backend
import HsBindgen.Backend.HsModule.Render
import HsBindgen.Backend.SHs.AST
import HsBindgen.BindingSpec.Gen
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
    result <- fmap join $ withTracer tracerConfig $ \tracer tracerUnsafeRef -> do
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

    either throwIO pure result
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
writeIncludeGraph :: Maybe FilePath -> Artefact ()
writeIncludeGraph mPath = do
    (p, includeGraph) <- IncludeGraph
    Lift $ write "include graph" mPath
         $ IncludeGraph.dumpMermaid p includeGraph

-- | Write @use-decl@ graph to file.
writeUseDeclGraph :: Maybe FilePath -> Artefact ()
writeUseDeclGraph mPath = do
    index <- DeclIndex
    useDeclGraph <- UseDeclGraph
    Lift $ write "use-decl graph" mPath
         $ UseDeclGraph.dumpMermaid index useDeclGraph

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
    Lift $ write "bindings" mPath bindings

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
    Lift $ writeByCategory "bindings" hsOutputDir moduleBaseName bindingsByCategory

-- | Write binding specifications to file.
writeBindingSpec :: FilePath -> Artefact ()
writeBindingSpec path = do
  target         <- Target
  moduleBaseName <- FinalModuleBaseName
  getMainHeaders <- GetMainHeaders
  omitTypes      <- OmitTypes
  hsDecls        <- HsDecls
  tracer         <- Lift $ artefactTracer <$> ask
  traceWith tracer $ RunArtefactWriteFile "binding specifications" path
  -- Binding specifications only specify types.
  liftIO $ genBindingSpec target moduleBaseName path getMainHeaders omitTypes $
    fromMaybe [] (Map.lookup BType $ unByCategory hsDecls)

-- | Create test suite in directory.
writeTests :: FilePath -> Artefact ()
writeTests testDir = do
    moduleBaseName  <- FinalModuleBaseName
    hashIncludeArgs <- HashIncludeArgs
    hsDecls         <- HsDecls
    liftIO $ genTests
      hashIncludeArgs
      hsDecls
      moduleBaseName
      testDir

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

write :: String -> Maybe FilePath -> String -> ArtefactM ()
write _    Nothing     str = liftIO $ putStrLn str
write what (Just path) str = do
      tracer <- artefactTracer <$> ask
      traceWith tracer $ RunArtefactWriteFile what path
      liftIO $ do
        createDirectoryIfMissing True $ takeDirectory path
        writeFile path str

writeByCategory ::
     String
  -> FilePath
  -> Hs.ModuleName
  -> ByCategory String
  -> ArtefactM ()
writeByCategory what hsOutputDir moduleBaseName =
    mapM_ (uncurry writeCategory) . Map.toList . unByCategory
  where
    writeCategory :: BindingCategory -> String -> ArtefactM ()
    writeCategory cat str = do
      let addSubModule = case cat of
            BType    -> id
            otherCat -> (</> displayBindingCategory otherCat)
          path = addSubModule baseFilePath <.> "hs"
          whatWithCategory = what ++ " (" ++ show cat ++ ")"
      write whatWithCategory (Just path) str

    baseFilePath :: FilePath
    baseFilePath = Foldable.foldl' (</>) "" $
      hsOutputDir : map T.unpack (T.splitOn "." (Hs.getModuleName moduleBaseName))
