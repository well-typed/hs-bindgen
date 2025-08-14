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

    -- * Exported for tests
  , hsBindgen'
  ) where

import Generics.SOP (I (..), NP (..))
import Language.Haskell.TH (Q, runQ)

import Clang.Paths
import HsBindgen.Backend
import HsBindgen.Backend.Artefact.PP.Render qualified as PP
import HsBindgen.Backend.Artefact.PP.Translation qualified as PP
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
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

-- | Main entry point to run @hs-bindgen@.
--
-- For a list of build artefacts, see the description and constructors of
-- 'Artefact'.
hsBindgen ::
     TracerConfig IO Level TraceMsg
  -> Config
  -> BindingSpecConfig
  -> [UncheckedHashIncludeArg]
  -> Artefacts as
  -> IO (NP I as)
hsBindgen tracerConfig = hsBindgen' id (withTracer tracerConfig)

-- TODO: Can we get rid of 'hsBindgenQ' and the need to use 'runQ'?
-- https://github.com/well-typed/hs-bindgen/issues/865.

-- | Main entry point to run @hs-bindgen@ with Template Haskell.
hsBindgenQ ::
     TracerConfig Q Level TraceMsg
  -> Config
  -> BindingSpecConfig
  -> [UncheckedHashIncludeArg]
  -> Artefacts as
  -> Q (NP I as)
hsBindgenQ tracerConfig = hsBindgen' runQ (withTracer tracerConfig)

hsBindgen' ::
     MonadIO m
  => (forall b. m b -> IO b)
  -> (forall c. (Tracer m TraceMsg -> m c) -> m (Maybe c))
  -> Config
  -> BindingSpecConfig
  -> [UncheckedHashIncludeArg]
  -> Artefacts as
  -> m (NP I as)
hsBindgen'
  unliftIO
  withTracerCustom
  config
  bindingSpecConfig
  uncheckedHashIncludeArgs
  artefacts = do
    -- Boot and frontend require unsafe tracer and `libclang`.
    mArtefact <- withTracerCustom $ \tracerM -> do
      let tracer :: Tracer IO TraceMsg
          tracer = natTracer unliftIO tracerM
          tracerFrontend :: Tracer IO FrontendMsg
          tracerFrontend = contramap TraceFrontend tracer
          tracerBoot :: Tracer IO BootMsg
          tracerBoot = contramap TraceBoot tracer
      -- 1. Boot.
      bootArtefact <- liftIO $
        boot tracerBoot config bindingSpecConfig uncheckedHashIncludeArgs
      -- 2. Frontend.
      frontendArtefact <- liftIO $
        frontend tracerFrontend config bootArtefact
      pure (bootArtefact, frontendArtefact)
    (bootArtefact, frontendArtefact) <- maybe fatalError pure mArtefact
    -- 3. Backend.
    let backendArtefact = backend config frontendArtefact
    -- 4. Artefacts.
    runArtefacts bootArtefact frontendArtefact backendArtefact artefacts

{-------------------------------------------------------------------------------
  Build artefacts
-------------------------------------------------------------------------------}

-- | Build artefact.
data Artefact (a :: Star) where
  -- * Boot
  HashIncludArgs :: Artefact [HashIncludeArg]
  -- * Frontend
  IncludeGraph   :: Artefact IncludeGraph.IncludeGraph
  DeclIndex      :: Artefact DeclIndex.DeclIndex
  UseDeclGraph   :: Artefact UseDeclGraph.UseDeclGraph
  DeclUseGraph   :: Artefact DeclUseGraph.DeclUseGraph
  ReifiedC       :: Artefact [C.Decl]
  Dependencies   :: Artefact [SourcePath]
  -- * Backend
  HsDecls        :: Artefact [Hs.Decl]
  FinalDecls     :: Artefact [SHs.SDecl]
  -- * Lift
  Lift :: Artefacts as -> (NP I as -> IO b) -> Artefact b

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
getBindings :: Config -> Artefact String
getBindings Config{..} =
  Lift
    (FinalDecls :* Nil)
    (\(I finalDecls :* Nil) ->
      let translate :: [SHs.SDecl] -> PP.HsModule
          translate = PP.translateModule configHsModuleOpts

          render :: PP.HsModule -> String
          render = PP.render configHsRenderOpts
      in pure . render $ translate finalDecls
    )

-- | Write bindings to file. If no file is given, print to standard output.
writeBindings :: Config -> Maybe FilePath -> Artefact ()
writeBindings config mfile =
  Lift
    (getBindings config :* Nil)
    (\(I bindings :* Nil) -> write mfile bindings)

-- | Write binding specifications to file.
writeBindingSpec :: Config -> FilePath -> Artefact ()
writeBindingSpec config file =
  Lift
    (HashIncludArgs :* HsDecls :* Nil)
    (\(I hashIncludeArgs :* I hsDecls :* Nil) ->
       genBindingSpec config hashIncludeArgs file hsDecls
    )

-- | Create test suite in directory.
writeTests :: Config -> FilePath -> Artefact ()
writeTests Config{..} testDir =
  Lift
    (HashIncludArgs :* HsDecls :* Nil)
    (\(I hashIncludeArgs :* I hsDecls :* Nil) ->
       genTests
         hashIncludeArgs
         hsDecls
         (PP.hsModuleOptsName configHsModuleOpts)
         (PP.hsLineLength configHsRenderOpts)
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
      HashIncludArgs -> pure bootHashIncludeArgs
      -- Frontend.
      IncludeGraph -> pure frontendIncludeGraph
      DeclIndex    -> pure frontendIndex
      UseDeclGraph -> pure frontendUseDeclGraph
      DeclUseGraph -> pure frontendDeclUseGraph
      ReifiedC     -> pure frontendCDecls
      Dependencies -> pure frontendDependencies
      -- Backend.
      HsDecls      -> pure backendHsDecls
      FinalDecls   -> pure backendFinalDecls
      -- Lift.
      (Lift as' f) -> go as' >>= liftIO . f

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

write :: Maybe FilePath -> String -> IO ()
write mfile str =
    let out = case mfile of
          Nothing -> putStr
          Just file -> writeFile file
    in  out str
