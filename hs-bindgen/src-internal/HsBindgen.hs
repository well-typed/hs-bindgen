{-# LANGUAGE UndecidableSuperClasses #-}

module HsBindgen
  ( hsBindgen
  , hsBindgenQ

    -- * Artefacts
  , Artefact (..)
  , Artefacts
  , writeIncludeGraph
  , writeUseDeclGraph
  , getBindings
  , writeBindings
  , writeBindingSpec
  , writeTests

    -- * Re-exports
  , I (..)
  , NP (..)

    -- * Internal
  , hsBindgen'
  ) where

import Generics.SOP (I (..), NP (..))
import Language.Haskell.TH (Q, runQ)

import Clang.Paths
import HsBindgen.Backend
import HsBindgen.Backend.Artefact.HsModule.Render
import HsBindgen.Backend.Artefact.HsModule.Translation
import HsBindgen.Backend.Artefact.Test (genTests)
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.BindingSpec.Gen
import HsBindgen.Boot
import HsBindgen.Config
import HsBindgen.Frontend
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Language.Haskell (HsModuleName)
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
  -> Artefacts IO as
  -> IO (NP I as)
hsBindgen = hsBindgen' id

-- TODO: Can we get rid of 'hsBindgenQ' and the need to use 'runQ'?
-- https://github.com/well-typed/hs-bindgen/issues/865.

-- | Main entry point to run @hs-bindgen@ with Template Haskell.
hsBindgenQ ::
     TracerConfig Q Level TraceMsg
  -> BindgenConfig
  -> [UncheckedHashIncludeArg]
  -> Artefacts Q as
  -> Q (NP I as)
hsBindgenQ = hsBindgen' runQ

hsBindgen' ::
     MonadIO m
  => (forall b. m b -> IO b)
  -> TracerConfig m Level TraceMsg
  -> BindgenConfig
  -> [UncheckedHashIncludeArg]
  -> Artefacts m as
  -> m (NP I as)
hsBindgen'
  unliftIO
  tracerConfig
  bindgenConfig@BindgenConfig{..}
  uncheckedHashIncludeArgs
  artefacts = do
    -- Boot and frontend require unsafe tracer and `libclang`.
    eArtefact <- withTracer tracerConfig $ \tracerM -> do
      let tracer :: Tracer IO TraceMsg
          tracer = natTracer unliftIO tracerM
          tracerFrontend :: Tracer IO FrontendMsg
          tracerFrontend = contramap TraceFrontend tracer
          tracerBoot :: Tracer IO BootMsg
          tracerBoot = contramap TraceBoot tracer
      -- 1. Boot.
      bootArtefact <-
        liftIO $ boot tracerBoot bindgenConfig uncheckedHashIncludeArgs
      -- 2. Frontend.
      frontendArtefact <- liftIO $
        frontend tracerFrontend bindgenFrontendConfig bootArtefact
      pure (bootArtefact, frontendArtefact)
    (bootArtefact, frontendArtefact) <- either (liftIO . throwIO) pure eArtefact
    -- 3. Backend.
    let backendArtefact = backend bindgenBackendConfig frontendArtefact
    -- 4. Artefacts.
    runArtefacts bootArtefact frontendArtefact backendArtefact artefacts

{-------------------------------------------------------------------------------
  Build artefacts
-------------------------------------------------------------------------------}

-- | Build artefact.
data Artefact m (a :: Star) where
  -- * Boot
  HashIncludeArgs :: Artefact m [HashIncludeArg]
  -- * Frontend
  IncludeGraph    :: Artefact m IncludeGraph.IncludeGraph
  DeclIndex       :: Artefact m DeclIndex.DeclIndex
  UseDeclGraph    :: Artefact m UseDeclGraph.UseDeclGraph
  DeclUseGraph    :: Artefact m DeclUseGraph.DeclUseGraph
  ReifiedC        :: Artefact m [C.Decl]
  Dependencies    :: Artefact m [SourcePath]
  -- * Backend
  HsDecls         :: Artefact m [Hs.Decl]
  FinalDecls      :: Artefact m [SHs.SDecl]
  FinalModuleName :: Artefact m HsModuleName
  FinalModule     :: Artefact m HsModule
  -- * Lift
  Lift            :: Artefacts m as -> (NP I as -> m b) -> Artefact m b

instance Applicative m => Functor (Artefact m) where
  fmap f x = Lift (x :* Nil) (\(I r :* Nil) -> pure (f r))

-- | A list of 'Artefact's.
type Artefacts m as = NP (Artefact m) as

-- | Write the include graph to file.
writeIncludeGraph :: MonadIO m => FilePath -> Artefact m ()
writeIncludeGraph file =
  Lift
    (IncludeGraph :* Nil)
    (\(I includeGraph :* Nil) ->
       liftIO $ writeFile file (IncludeGraph.dumpMermaid includeGraph))

-- | Write @use-decl@ graph to file.
writeUseDeclGraph :: MonadIO m => FilePath -> Artefact m ()
writeUseDeclGraph file =
  Lift
    (DeclIndex :* UseDeclGraph :* Nil)
    (\(I index :* I useDeclGraph :* Nil) ->
       liftIO $ writeFile file (UseDeclGraph.dumpMermaid index useDeclGraph)
    )

-- | Get bindings.
getBindings :: Applicative m => Artefact m String
getBindings =
  Lift
    (FinalModule :* Nil)
    (\(I finalModule :* Nil) ->
       pure . render $ finalModule
    )

-- | Write bindings to file. If no file is given, print to standard output.
writeBindings :: MonadIO m => Maybe FilePath -> Artefact m ()
writeBindings mfile =
  Lift
    (getBindings :* Nil)
    (\(I bindings :* Nil) -> liftIO $ write mfile bindings)

-- | Write binding specifications to file.
writeBindingSpec :: MonadIO m => FilePath -> Artefact m ()
writeBindingSpec file =
  Lift
    (FinalModuleName :* HashIncludeArgs :* HsDecls :* Nil)
    (\(I moduleName :* I hashIncludeArgs :* I hsDecls :* Nil) ->
       liftIO $ genBindingSpec moduleName hashIncludeArgs file hsDecls
    )

-- | Create test suite in directory.
writeTests :: MonadIO m => FilePath -> Artefact m ()
writeTests testDir =
  Lift
    (FinalModuleName :* HashIncludeArgs :* HsDecls :* Nil)
    (\(I moduleName :* I hashIncludeArgs :* I hsDecls :* Nil) ->
       liftIO $ genTests
         hashIncludeArgs
         hsDecls
         moduleName
         testDir
    )

{-------------------------------------------------------------------------------
  Artefacts
-------------------------------------------------------------------------------}

-- | Compute the results of a list of artefacts.
runArtefacts :: Monad m
  => BootArtefact
  -> FrontendArtefact
  -> BackendArtefact
  -> Artefacts m as
  -> m (NP I as)
runArtefacts
  BootArtefact{..}
  FrontendArtefact{..}
  BackendArtefact{..} = go
  where
    go :: Monad m => Artefacts m as -> m (NP I as)
    go Nil       = pure Nil
    go (a :* as) = (:*) . I <$> runArtefact a <*> go as

    runArtefact :: Monad m => Artefact m a -> m a
    runArtefact = \case
      --Boot.
      HashIncludeArgs -> pure bootHashIncludeArgs
      -- Frontend.
      IncludeGraph    -> pure frontendIncludeGraph
      DeclIndex       -> pure frontendIndex
      UseDeclGraph    -> pure frontendUseDeclGraph
      DeclUseGraph    -> pure frontendDeclUseGraph
      ReifiedC        -> pure frontendCDecls
      Dependencies    -> pure frontendDependencies
      -- Backend.
      HsDecls         -> pure backendHsDecls
      FinalDecls      -> pure backendFinalDecls
      FinalModuleName -> pure backendFinalModuleName
      FinalModule     -> pure backendFinalModule
      -- Lift.
      (Lift as' f)    -> go as' >>= f

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

write :: Maybe FilePath -> String -> IO ()
write mfile str =
    let out = case mfile of
          Nothing -> putStr
          Just file -> writeFile file
    in  out str
