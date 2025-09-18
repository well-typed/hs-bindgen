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

import Data.Map qualified as Map
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (<.>), (</>))

import Clang.Paths

import Generics.SOP (I (..), NP (..))

import HsBindgen.Backend
import HsBindgen.Backend.Artefact.HsModule.Render
import HsBindgen.Backend.Artefact.HsModule.Translation
import HsBindgen.Backend.Artefact.Test (genTests)
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv (UserlandCapiWrapper)
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.BindingSpec.Gen
import HsBindgen.Boot
import HsBindgen.Config
import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.RootHeader (HashIncludeArg, UncheckedHashIncludeArg)
import HsBindgen.Imports
import HsBindgen.Language.Haskell (HsModuleName (getHsModuleName))
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
    let backendArtefact = backend bindgenBackendConfig frontendArtefact
    -- 4. Artefacts.
    runArtefacts bootArtefact frontendArtefact backendArtefact artefacts

{-------------------------------------------------------------------------------
  Build artefacts
-------------------------------------------------------------------------------}

-- | Build artefact.
data Artefact (a :: Star) where
  -- * Boot
  HashIncludeArgs     :: Artefact [HashIncludeArg]
  -- * Frontend
  IncludeGraph        :: Artefact (IncludeGraph.Predicate, IncludeGraph.IncludeGraph)
  DeclIndex           :: Artefact DeclIndex.DeclIndex
  UseDeclGraph        :: Artefact UseDeclGraph.UseDeclGraph
  DeclUseGraph        :: Artefact DeclUseGraph.DeclUseGraph
  ReifiedC            :: Artefact [C.Decl]
  Dependencies        :: Artefact [SourcePath]
  -- * Backend
  HsDecls             :: Artefact (ByCategory [Hs.Decl])
  FinalDecls          :: Artefact (ByCategory ([UserlandCapiWrapper], [SHs.SDecl]))
  FinalModuleBaseName :: Artefact HsModuleName
  FinalModule         :: Safety -> Artefact HsModule
  FinalModules        :: Artefact (ByCategory HsModule)
  -- * Lift and sequence
  Lift                :: Artefacts as -> (NP I as -> IO b) -> Artefact b

instance Functor Artefact where
  fmap f x = Lift (x :* Nil) (\(I r :* Nil) -> pure (f r))

-- | A list of 'Artefact's.
type Artefacts as = NP Artefact as

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
    go acc []     = Lift acc $ \_results -> return ()
    go acc (a:as) = go (a :* acc) as

-- | Write the include graph to `STDOUT` or a file.
writeIncludeGraph :: FilePath -> Artefact ()
writeIncludeGraph mPath = Lift (IncludeGraph :* Nil) $
    \(I (p, includeGraph) :* Nil) ->
      write mPath $ IncludeGraph.dumpMermaid p includeGraph

-- | Write @use-decl@ graph to file.
writeUseDeclGraph :: FilePath -> Artefact ()
writeUseDeclGraph file = Lift (DeclIndex :* UseDeclGraph :* Nil) $
    \(I index :* I useDeclGraph :* Nil) ->
      writeFile file (UseDeclGraph.dumpMermaid index useDeclGraph)

-- | Get bindings (single module).
getBindings :: Safety -> Artefact String
getBindings safety = Lift (FinalModule safety :* Nil) $
    \(I finalModule :* Nil) ->
      pure . render $ finalModule

-- | Write bindings to file.
--
-- If no file is given, print to standard output.
writeBindings :: Safety -> Maybe FilePath -> Artefact ()
writeBindings safety path = Lift (getBindings safety :* Nil) $
    \(I bindings :* Nil) ->
      write path bindings

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
      writeByCategory hsOutputDir moduleBaseName bindingsByCategory

-- | Write binding specifications to file.
writeBindingSpec :: FilePath -> Artefact ()
writeBindingSpec file =
  Lift (FinalModuleBaseName :* HashIncludeArgs :* HsDecls :* Nil) $
    \(I moduleBaseName :* I hashIncludeArgs :* I hsDecls :* Nil) ->
      -- Generate binding specification only for declarations of binding
      -- category 'BType'. If, in the future, we generate binding specifications
      -- for other binding categories, we need to take care of the final module
      -- names (e.g., @Generated.Safe@).
      genBindingSpec moduleBaseName hashIncludeArgs file $
        fromMaybe (panicPure "binding category BType is missing")
          (Map.lookup BType $ unByCategory hsDecls)

-- | Create test suite in directory.
writeTests :: FilePath -> Artefact ()
writeTests testDir =
  Lift (FinalModuleBaseName :* HashIncludeArgs :* HsDecls :* Nil) $
    \(I moduleBaseName :* I hashIncludeArgs :* I hsDecls :* Nil) ->
      genTests
        hashIncludeArgs
        hsDecls
        moduleBaseName
        testDir

{-------------------------------------------------------------------------------
  Artefacts
-------------------------------------------------------------------------------}

-- | Compute the results of a list of artefacts.
runArtefacts ::
     BootArtefact
  -> FrontendArtefact
  -> BackendArtefact
  -> Artefacts as
  -> IO (NP I as)
runArtefacts
  BootArtefact{..}
  FrontendArtefact{..}
  BackendArtefact{..} = go
  where
    go :: Artefacts as -> IO (NP I as)
    go Nil       = pure Nil
    go (a :* as) = (:*) . I <$> runArtefact a <*> go as

    runArtefact :: Artefact a -> IO a
    runArtefact = \case
      --Boot.
      HashIncludeArgs     -> pure bootHashIncludeArgs
      -- Frontend.
      IncludeGraph        -> pure frontendIncludeGraph
      DeclIndex           -> pure frontendIndex
      UseDeclGraph        -> pure frontendUseDeclGraph
      DeclUseGraph        -> pure frontendDeclUseGraph
      ReifiedC            -> pure frontendCDecls
      Dependencies        -> pure frontendDependencies
      -- Backend.
      HsDecls             -> pure backendHsDecls
      FinalDecls          -> pure backendFinalDecls
      FinalModuleBaseName -> pure backendFinalModuleBaseName
      FinalModule safety  -> pure $ translateModuleSingle safety backendFinalModuleBaseName backendFinalDecls
      FinalModules        -> pure $ translateModuleMultiple backendFinalModuleBaseName backendFinalDecls
      -- Lift and sequence.
      (Lift as' f)        -> go as' >>= f

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

write :: Maybe FilePath -> String -> IO ()
write Nothing     str = putStrLn str
write (Just path) str = do
      createDirectoryIfMissing True $ takeDirectory path
      writeFile path str

writeByCategory :: FilePath -> HsModuleName -> ByCategory String -> IO ()
writeByCategory hsOutputDir moduleBaseName =
    mapM_ (uncurry writeCategory) . Map.toList . unByCategory
  where
    writeCategory :: BindingCategory -> String -> IO ()
    writeCategory cat str = do
      let addSubModule = case cat of
            BType    -> id
            otherCat -> (</> displayBindingCategory otherCat)
          path = addSubModule baseFilePath <.> "hs"
      createDirectoryIfMissing True $ takeDirectory path
      writeFile path str

    baseFilePath :: FilePath
    baseFilePath = hsOutputDir </> T.unpack (getHsModuleName moduleBaseName)
