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
  , runArtefacts
  ) where

import Data.Functor ((<&>))
import Generics.SOP (I (..), NP (..))
import Language.Haskell.TH (Q, runQ)

import Clang.Args
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
import HsBindgen.ModuleUnique
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

-- TODO: TracerConfig into Config?
-- TODO: BindingSpecConfig into Config?
-- TODO: ModuleUnique into Config?

hsBindgen ::
     TracerConfig IO TraceMsg Level
  -> ModuleUnique
  -> Config
  -> BindingSpecConfig
  -> [UncheckedHashIncludeArg]
  -> Artefacts as
  -> IO (NP I as)
hsBindgen = hsBindgen' id

hsBindgenQ ::
     TracerConfig Q TraceMsg Level
  -> ModuleUnique
  -> Config
  -> BindingSpecConfig
  -> [UncheckedHashIncludeArg]
  -> Artefacts as
  -> Q (NP I as)
hsBindgenQ = hsBindgen' runQ

hsBindgen' ::
     MonadIO m
  => (forall b. m b -> IO b)
  -> TracerConfig m TraceMsg Level
  -> ModuleUnique
  -> Config
  -> BindingSpecConfig
  -> [UncheckedHashIncludeArg]
  -> Artefacts as
  -> m (NP I as)
hsBindgen'
  unliftIO
  tracerConfig
  moduleUnique
  config
  bindingSpecConfig
  uncheckedHashIncludeArgs
  artefacts = do
    -- Boot and frontend (requires unsafe tracer and `libclang`).
    mArtefact <- withTracer tracerConfig $ \tracerM -> do
      let tracer :: Tracer IO TraceMsg
          tracer = natTracer unliftIO tracerM
          tracerFrontend :: Tracer IO FrontendMsg
          tracerFrontend = contramap TraceFrontend tracer
          tracerBoot :: Tracer IO BootMsg
          tracerBoot = contramap TraceBoot tracer
      bootArtefact <- liftIO $
        boot tracerBoot clangArgs bindingSpecConfig uncheckedHashIncludeArgs
      frontendArtefact <- liftIO $
        frontend tracerFrontend config bootArtefact
      pure (bootArtefact, frontendArtefact)
    (bootArtefact, frontendArtefact) <- maybe fatalError pure mArtefact
    -- Backend.
    let backendArtefact = backend moduleUnique config frontendArtefact
    runArtefacts config bootArtefact frontendArtefact backendArtefact artefacts
  where
    clangArgs :: ClangArgs
    clangArgs = configClangArgs config

{-------------------------------------------------------------------------------
  Build artefacts
-------------------------------------------------------------------------------}

-- TODO: Hide internal API.
--
-- -- | Haskell declarations, translated from a C header
-- newtype HsDecls = WrapHsDecls {
--       unwrapHsDecls :: [Hs.Decl]
--     }

-- TODO: The "get" and "write" functions have access to 'Config' for now because
-- it needs access to pretty printer configurations. I think we have two
-- possibilities here: Either we collect all configuration into a single
-- 'Config' object and let getters/writers access this object. Or we separate
-- pretty-printer related configuration and directly apply the configuration
-- when defining the getters or writers such as 'getBindings' or
-- 'writeBindings'.

data Artefact (a :: Star) where
  -- Boot.
  HashIncludArgs :: Artefact [HashIncludeArg]
  -- Frontend.
  IncludeGraph   :: Artefact IncludeGraph.IncludeGraph
  DeclIndex      :: Artefact DeclIndex.DeclIndex
  UseDeclGraph   :: Artefact UseDeclGraph.UseDeclGraph
  DeclUseGraph   :: Artefact DeclUseGraph.DeclUseGraph
  ReifiedC       :: Artefact [C.Decl]
  Dependencies   :: Artefact [SourcePath]
  -- Backend.
  Hs             :: Artefact [Hs.Decl]
  SHs            :: Artefact [SHs.SDecl]
  -- Getter.
  Get :: Artefacts as -> (Config -> NP I as -> b) -> Artefact b
  -- Writer.
  Write :: Artefacts as -> (Config -> NP I as -> IO b) -> Artefact b

type Artefacts as = NP Artefact as

-- Writers.
writeIncludeGraph :: Maybe FilePath -> Artefact ()
writeIncludeGraph mfile =
  Write
    (IncludeGraph :* Nil)
    (\_ (I includeGraph :* Nil) -> write mfile (IncludeGraph.dumpMermaid includeGraph))

writeUseDeclGraph :: Maybe FilePath -> Artefact ()
writeUseDeclGraph mfile =
  Write
    (DeclIndex :* UseDeclGraph :* Nil)
    (\_ (I index :* I useDeclGraph :* Nil) ->
       write mfile (UseDeclGraph.dumpMermaid index useDeclGraph)
    )

getBindings :: Artefact String
getBindings =
  Get
    (SHs :* Nil)
    (\Config{..} (I sHsDecls :* Nil) ->
      let translate :: [SHs.SDecl] -> PP.HsModule
          translate = PP.translateModule configHsModuleOpts

          render :: PP.HsModule -> String
          render = PP.render configHsRenderOpts
      in render $ translate sHsDecls
    )

writeBindings :: Maybe FilePath -> Artefact ()
writeBindings mfile =
  Write
    (getBindings :* Nil)
    (\_ (I bindings :* Nil) -> write mfile bindings)

writeBindingSpec :: FilePath -> Artefact ()
writeBindingSpec file =
  Write
    (HashIncludArgs :* Hs :* Nil)
    (\config (I hashIncludeArgs :* I hsDecls :* Nil) ->
       genBindingSpec config hashIncludeArgs file hsDecls
    )

writeTests :: FilePath -> Artefact ()
writeTests testDir =
  Write
    (HashIncludArgs :* Hs :* Nil)
    (\Config{..} (I hashIncludeArgs :* I hsDecls :* Nil) ->
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

runArtefacts ::
     MonadIO m
  => Config
  -> BootArtefact
  -> FrontendArtefact
  -> BackendArtefact
  -> Artefacts as
  -> m (NP I as)
runArtefacts
  config
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
      Hs     -> pure backendHsDecls
      SHs    -> pure backendSHsDecls
      -- Getter.
      (Get as' f)   -> go as' <&> f config
      -- Writer.
      (Write as' f) -> go as' >>= liftIO . f config

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

write :: Maybe FilePath -> String -> IO ()
write mfile str =
    let out = case mfile of
          Nothing -> putStr
          Just file -> writeFile file
    in  out str
