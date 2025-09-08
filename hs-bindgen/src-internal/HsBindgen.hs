{-# LANGUAGE UndecidableSuperClasses #-}

module HsBindgen
  ( hsBindgen

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

import Data.Map qualified as Map
import Generics.SOP (I (..), NP (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (splitExtension, takeDirectory, (<.>), (</>))

import Clang.Paths
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
  IncludeGraph        :: Artefact IncludeGraph.IncludeGraph
  DeclIndex           :: Artefact DeclIndex.DeclIndex
  UseDeclGraph        :: Artefact UseDeclGraph.UseDeclGraph
  DeclUseGraph        :: Artefact DeclUseGraph.DeclUseGraph
  ReifiedC            :: Artefact [C.Decl]
  Dependencies        :: Artefact [SourcePath]
  -- * Backend
  HsDecls             :: Artefact (ByCategory [Hs.Decl])
  FinalDecls          :: Artefact (ByCategory ([UserlandCapiWrapper], [SHs.SDecl]))
  FinalModuleBaseName :: Artefact HsModuleName
  FinalModule         :: Artefact (ByCategory HsModule)
  -- * Lift
  Lift                :: Artefacts as -> (NP I as -> IO b) -> Artefact b

instance Functor Artefact where
  fmap f x = Lift (x :* Nil) (\(I r :* Nil) -> pure (f r))

-- | A list of 'Artefact's.
type Artefacts as = NP Artefact as

-- | Write the include graph to file.
writeIncludeGraph :: FilePath -> Artefact ()
writeIncludeGraph file = Lift (IncludeGraph :* Nil) $
    \(I includeGraph :* Nil) ->
      writeFile file (IncludeGraph.dumpMermaid includeGraph)

-- | Write @use-decl@ graph to file.
writeUseDeclGraph :: FilePath -> Artefact ()
writeUseDeclGraph file = Lift (DeclIndex :* UseDeclGraph :* Nil) $
    \(I index :* I useDeclGraph :* Nil) ->
      writeFile file (UseDeclGraph.dumpMermaid index useDeclGraph)

-- | Get bindings.
getBindings :: Artefact (ByCategory String)
getBindings = Lift (FinalModule :* Nil) $
    \(I finalModule :* Nil) ->
      pure . fmap render $ finalModule

-- | Write bindings to files.
--
-- Each file contains a different binding category.
--
-- If no file is given, print to standard output.
writeBindings :: Maybe FilePath -> Artefact ()
writeBindings mBasePath = Lift (getBindings :* Nil) $
    \(I bindings :* Nil) -> writeByCategory mBasePath bindings

-- | Write binding specifications to file.
writeBindingSpec :: FilePath -> Artefact ()
writeBindingSpec file =
  Lift (FinalModuleBaseName :* HashIncludeArgs :* HsDecls :* Nil) $
    \(I moduleBaseName :* I hashIncludeArgs :* I hsDecls :* Nil) ->
      -- TODO_PR @Travis: Do we have to amend the module names according to
      -- BindingCategory?
      genBindingSpec moduleBaseName hashIncludeArgs file $ concat hsDecls

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
      FinalModule         -> pure backendFinalModule
      -- Lift.
      (Lift as' f)        -> go as' >>= f

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

writeByCategory :: Maybe FilePath -> ByCategory String -> IO ()
writeByCategory Nothing =
    mapM_ putStrLn . unByCategory
-- TODO_PR: Think about how we treat extensions. At the moment, the base path
-- includes the extension. In my opinion, that should not be the case, because
-- only `.hs` is making sense here (to my knowledge).
writeByCategory (Just basePath) =
    mapM_ (uncurry writeCategory) . Map.toList . unByCategory
  where
    writeCategory :: BindingCategory -> String -> IO ()
    writeCategory cat str = do
      let addSubModule = case cat of
            BType   -> id
            BSafe   -> (</> "Safe")
            BUnsafe -> (</> "Unsafe")
            BFunPtr -> (</> "FunPtr")
            BGlobal -> (</> "Global")
          path = addSubModule basePathNoExt <.> ext
      createDirectoryIfMissing True $ takeDirectory path
      writeFile path str

    (basePathNoExt, ext) = splitExtension basePath
