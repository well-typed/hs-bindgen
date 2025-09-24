{-# LANGUAGE UndecidableSuperClasses #-}

module HsBindgen
  ( hsBindgen

    -- * Artefacts
  , Artefact (..)
  , sequenceArtefacts
  , Artefacts
  , writeIncludeGraph
  , writeUseDeclGraph
  , getBindings
  , writeBindings
  , writeBindingsMultiple
  , writeBindingSpec
  , writeTests

    -- * Re-exports
  , I (..)
  , NP (..)
  ) where

import Control.Monad.Trans.Reader (ask)
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (<.>), (</>))

import Generics.SOP (I (..), NP (..))

import HsBindgen.Artefact
import HsBindgen.Backend
import HsBindgen.Backend.HsModule.Render
import HsBindgen.Backend.SHs.AST
import HsBindgen.BindingSpec.Gen
import HsBindgen.Boot
import HsBindgen.Config
import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.RootHeader (UncheckedHashIncludeArg)
import HsBindgen.Imports
import HsBindgen.Language.Haskell (HsModuleName (getHsModuleName))
import HsBindgen.Test (genTests)
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

-- | Main entry point to run @hs-bindgen@.
--
-- For a list of build artefacts, see the description and constructors of
-- 'Artefact'.
hsBindgen ::
     TracerConfig IO Level TraceMsg
  -> BindgenConfig
  -> [UncheckedHashIncludeArg]
  -> Artefacts as
  -> IO (NP I as)
hsBindgen
  tracerConfig
  bindgenConfig@BindgenConfig{..}
  uncheckedHashIncludeArgs
  artefacts = do
    -- Boot and frontend require unsafe tracer and `libclang`.
    eArtefact <- withTracer tracerConfig $ \tracer -> do
      let tracerFrontend :: Tracer IO FrontendMsg
          tracerFrontend = contramap TraceFrontend tracer
          tracerBoot :: Tracer IO BootMsg
          tracerBoot = contramap TraceBoot tracer
      -- 1. Boot.
      bootArtefact <-
        boot tracerBoot bindgenConfig uncheckedHashIncludeArgs
      -- 2. Frontend.
      frontendArtefact <-
        frontend tracerFrontend bindgenFrontendConfig bootArtefact
      pure (bootArtefact, frontendArtefact)
    (bootArtefact, frontendArtefact) <- either throwIO pure eArtefact
    -- 3. Backend.
    backendArtefact <- withTracerSafe tracerConfigSafe $ \tracer -> do
      backend tracer bindgenBackendConfig frontendArtefact
    -- 4. Artefacts.
    withTracerSafe tracerConfigSafe $ \tracer -> do
      runArtefacts tracer bootArtefact frontendArtefact backendArtefact artefacts
  where
    tracerConfigSafe :: TracerConfig IO SafeLevel a
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

-- | Courtesy of Edsko :-).
--
-- Another implementation for `sequenceArtefacts` which has the drawback of
-- creating deeply nested @(Lift .. (Lift .. ( .. )))@ structures.
--
-- @
-- import Data.Semigroup (Semigroup (..))
-- import Generics.SOP (unI)
--
-- instance Semigroup a => Semigroup (Artefact a) where
--   l <> r = Lift (l :* r :* Nil) (\(r1 :* r2 :* Nil) -> pure (unI r1 <> unI r2))
--
-- instance Monoid a => Monoid (Artefact a) where
--   mempty = Lift Nil (\_result -> return mempty)
--
-- sequenceArtefacts' :: [Artefact ()] -> Artefact ()
-- sequenceArtefacts' = mconcat
-- @
sequenceArtefacts :: [Artefact ()] -> Artefact ()
sequenceArtefacts = go Nil . reverse
  where
    go :: Artefacts as -> [Artefact ()] -> Artefact ()
    go acc []     = Lift acc $ \_results -> pure ()
    go acc (a:as) = go (a :* acc) as

-- | Write the include graph to `STDOUT` or a file.
writeIncludeGraph :: Maybe FilePath -> Artefact ()
writeIncludeGraph mPath = Lift (IncludeGraph :* Nil) $
    \(I (p, includeGraph) :* Nil) ->
      write "include graph" mPath $ IncludeGraph.dumpMermaid p includeGraph

-- | Write @use-decl@ graph to file.
writeUseDeclGraph :: FilePath -> Artefact ()
writeUseDeclGraph path = Lift (DeclIndex :* UseDeclGraph :* Nil) $
    \(I index :* I useDeclGraph :* Nil) ->
      write "use-decl graph" (Just path) (UseDeclGraph.dumpMermaid index useDeclGraph)

-- | Get bindings (single module).
getBindings :: Safety -> Artefact String
getBindings safety = Lift (finalModuleArtefact :* Nil) $
    \(I finalModule :* Nil) ->
      pure . render $ finalModule
  where finalModuleArtefact = case safety of
          Safe   -> FinalModuleSafe
          Unsafe -> FinalModuleUnsafe

-- | Write bindings to file.
--
-- If no file is given, print to standard output.
writeBindings :: Safety -> Maybe FilePath -> Artefact ()
writeBindings safety mPath = Lift (getBindings safety :* Nil) $
    \(I bindings :* Nil) ->
      write "bindings" mPath bindings

-- | Get bindings (one module per binding category).
getBindingsMultiple :: Artefact (ByCategory String)
getBindingsMultiple = Lift (FinalModules :* Nil) $
    \(I finalModule :* Nil) ->
      pure . (fmap render) $ finalModule

-- | Write bindings to files in provided output directory.
--
-- Each file contains a different binding category.
--
-- If no file is given, print to standard output.
writeBindingsMultiple :: FilePath -> Artefact ()
writeBindingsMultiple hsOutputDir = Lift (FinalModuleBaseName :* getBindingsMultiple :* Nil) $
    \(I moduleBaseName :* I bindingsByCategory :* Nil) ->
      writeByCategory "bindings" hsOutputDir moduleBaseName bindingsByCategory

-- | Write binding specifications to file.
writeBindingSpec :: FilePath -> Artefact ()
writeBindingSpec path =
  Lift (FinalModuleBaseName :* HashIncludeArgs :* HsDecls :* Nil) $
    \(I moduleBaseName :* I hashIncludeArgs :* I hsDecls :* Nil) -> do
      tracer <- artefactTracer <$> ask
      liftIO $ do
        traceWith tracer $ RunArtefactWriteFile "binding specifications" path
        -- Generate binding specification only for declarations of binding
        -- category 'BType'. If, in the future, we generate binding
        -- specifications for other binding categories, we need to take care of
        -- the final module names (e.g., @Generated.Safe@).
        genBindingSpec moduleBaseName hashIncludeArgs path $
          fromMaybe (panicPure "binding category BType is missing")
            (Map.lookup BType $ unByCategory hsDecls)

-- | Create test suite in directory.
writeTests :: FilePath -> Artefact ()
writeTests testDir =
  Lift (FinalModuleBaseName :* HashIncludeArgs :* HsDecls :* Nil) $
    \(I moduleBaseName :* I hashIncludeArgs :* I hsDecls :* Nil) ->
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
      liftIO $ do
        traceWith tracer $ RunArtefactWriteFile what path
        createDirectoryIfMissing True $ takeDirectory path
        writeFile path str

writeByCategory ::
     String
  -> FilePath
  -> HsModuleName
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
      hsOutputDir : map T.unpack (T.splitOn "." (getHsModuleName moduleBaseName))
