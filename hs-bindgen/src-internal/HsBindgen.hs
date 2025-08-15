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
import HsBindgen.BindingSpec
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
  -> BindingSpecConfig
  -> FrontendConfig
  -> BackendConfig
  -> [UncheckedHashIncludeArg]
  -> Artefacts as
  -> IO (NP I as)
hsBindgen = hsBindgen' id

-- TODO: Can we get rid of 'hsBindgenQ' and the need to use 'runQ'?
-- https://github.com/well-typed/hs-bindgen/issues/865.

-- | Main entry point to run @hs-bindgen@ with Template Haskell.
hsBindgenQ ::
     TracerConfig Q Level TraceMsg
  -> BindingSpecConfig
  -> FrontendConfig
  -> BackendConfig
  -> [UncheckedHashIncludeArg]
  -> Artefacts as
  -> Q (NP I as)
hsBindgenQ = hsBindgen' runQ

hsBindgen' ::
     MonadIO m
  => (forall b. m b -> IO b)
  -> TracerConfig m Level TraceMsg
  -> BindingSpecConfig
  -> FrontendConfig
  -> BackendConfig
  -> [UncheckedHashIncludeArg]
  -> Artefacts as
  -> m (NP I as)
hsBindgen'
  unliftIO
  tracerConfig
  bindingSpecConfig
  frontendConfig
  backendConfig
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
        liftIO $ boot tracerBoot bindingSpecConfig frontendConfig backendConfig uncheckedHashIncludeArgs
      -- 2. Frontend.
      frontendArtefact <- liftIO $
        frontend tracerFrontend frontendConfig bootArtefact
      pure (bootArtefact, frontendArtefact)
    (bootArtefact, frontendArtefact) <- either (liftIO . throwIO) pure eArtefact
    -- 3. Backend.
    let backendArtefact = backend backendConfig frontendArtefact
    -- 4. Artefacts.
    runArtefacts bootArtefact frontendArtefact backendArtefact artefacts

{-------------------------------------------------------------------------------
  Build artefacts
-------------------------------------------------------------------------------}

-- | Build artefact.
data Artefact (a :: Star) where
  -- * Boot
  HashIncludeArgs :: Artefact [HashIncludeArg]
  -- * Frontend
  IncludeGraph    :: Artefact IncludeGraph.IncludeGraph
  DeclIndex       :: Artefact DeclIndex.DeclIndex
  UseDeclGraph    :: Artefact UseDeclGraph.UseDeclGraph
  DeclUseGraph    :: Artefact DeclUseGraph.DeclUseGraph
  ReifiedC        :: Artefact [C.Decl]
  Dependencies    :: Artefact [SourcePath]
  -- * Backend
  HsDecls         :: Artefact [Hs.Decl]
  FinalDecls      :: Artefact [SHs.SDecl]
  FinalModuleName :: Artefact HsModuleName
  FinalModule     :: Artefact HsModule
  -- * Lift
  Lift            :: Artefacts as -> (NP I as -> IO b) -> Artefact b

instance Functor Artefact where
  fmap f x = Lift (x :* Nil) (\(I r :* Nil) -> pure (f r))

-- | A list of 'Artefact's.
type Artefacts as = NP Artefact as

-- | Write the include graph to file.
writeIncludeGraph :: FilePath -> Artefact ()
writeIncludeGraph file =
  Lift
    (IncludeGraph :* Nil)
    (\(I includeGraph :* Nil) -> writeFile file (IncludeGraph.dumpMermaid includeGraph))

-- | Write @use-decl@ graph to file.
writeUseDeclGraph :: FilePath -> Artefact ()
writeUseDeclGraph file =
  Lift
    (DeclIndex :* UseDeclGraph :* Nil)
    (\(I index :* I useDeclGraph :* Nil) ->
       writeFile file (UseDeclGraph.dumpMermaid index useDeclGraph)
    )

-- | Get bindings.
getBindings :: Artefact String
getBindings =
  Lift
    (FinalModule :* Nil)
    (\(I finalModule :* Nil) ->
       pure . render $ finalModule
    )

-- | Write bindings to file. If no file is given, print to standard output.
writeBindings :: Maybe FilePath -> Artefact ()
writeBindings mfile =
  Lift
    (getBindings :* Nil)
    (\(I bindings :* Nil) -> write mfile bindings)

-- | Write binding specifications to file.
writeBindingSpec :: FilePath -> Artefact ()
writeBindingSpec file =
  Lift
    (FinalModuleName :* HashIncludeArgs :* HsDecls :* Nil)
    (\(I moduleName :* I hashIncludeArgs :* I hsDecls :* Nil) ->
       genBindingSpec moduleName hashIncludeArgs file hsDecls
    )

-- | Create test suite in directory.
writeTests :: FilePath -> Artefact ()
writeTests testDir =
  Lift
    (FinalModuleName :* HashIncludeArgs :* HsDecls :* Nil)
    (\(I moduleName :* I hashIncludeArgs :* I hsDecls :* Nil) ->
       genTests
         hashIncludeArgs
         hsDecls
         moduleName
         testDir
    )

{-------------------------------------------------------------------------------
  Artefacts
-------------------------------------------------------------------------------}

-- | Compute the results of a list of artefacts.
runArtefacts ::
     MonadIO m
  => BootArtefact
  -> FrontendArtefact
  -> BackendArtefact
  -> Artefacts as
  -> m (NP I as)
runArtefacts
  BootArtefact{..}
  FrontendArtefact{..}
  BackendArtefact{..} = go
  where
    go :: MonadIO m => Artefacts as -> m (NP I as)
    go Nil       = return Nil
    go (a :* as) = (:*) . I <$> runArtefact a <*> go as

    runArtefact :: MonadIO m => Artefact a -> m a
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
      (Lift as' f)    -> go as' >>= liftIO . f

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

write :: Maybe FilePath -> String -> IO ()
write mfile str =
    let out = case mfile of
          Nothing -> putStr
          Just file -> writeFile file
    in  out str
