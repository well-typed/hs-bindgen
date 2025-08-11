{-# LANGUAGE UndecidableSuperClasses #-}

module HsBindgen
  ( hsBindgen

    -- * Artefacts
  , Artefact (..)
  , Artefacts
  , writeIncludeGraph
  , writeUseDeclGraph
  , writeBindings
  , writeBindingSpec
  , writeTests

    -- * Re-exports
  , I (..)
  , NP (..)
  ) where

import System.Exit (exitFailure)

import Clang.Args
import Clang.Enum.Bitfield
import Clang.LowLevel.Core
import Clang.Paths
import Generics.SOP (I (..), NP (..))
import HsBindgen.Backend.Artefact.PP.Render qualified as PP
import HsBindgen.Backend.Artefact.PP.Translation qualified as PP
import HsBindgen.Backend.Artefact.Test (genTests)
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Translation qualified as Hs
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.SHs.Simplify qualified as SHs
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.BindingSpec
import HsBindgen.BindingSpec.Gen
import HsBindgen.Clang
import HsBindgen.Config
import HsBindgen.Frontend hiding (frontend)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.AST.Finalize
import HsBindgen.Frontend.AST.Internal hiding (Type)
import HsBindgen.Frontend.Pass.HandleMacros
import HsBindgen.Frontend.Pass.HandleTypedefs
import HsBindgen.Frontend.Pass.MangleNames
import HsBindgen.Frontend.Pass.NameAnon
import HsBindgen.Frontend.Pass.Parse
import HsBindgen.Frontend.Pass.ResolveBindingSpec
import HsBindgen.Frontend.Pass.Select
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Pass.Sort
import HsBindgen.Frontend.Pass.Sort.IsPass
import HsBindgen.Frontend.ProcessIncludes
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
hsBindgen
  tracerConfig
  moduleUnique
  config
  bindingSpecConfig
  uncheckedHashIncludeArgs
  artefacts = do
    -- Boot and frontend (requires unsafe tracer and `libclang`).
    mArtefact <- withTracer tracerConfig $ \tracer -> do
      let clangArgs :: ClangArgs
          clangArgs = configClangArgs config
      bootArtefact <-
        boot tracer clangArgs bindingSpecConfig uncheckedHashIncludeArgs
      let tracerFrontend :: Tracer IO FrontendMsg
          tracerFrontend = contramap TraceFrontend tracer
      frontendArtefact <- frontend tracerFrontend config bootArtefact
      pure (bootArtefact, frontendArtefact)
    (bootArtefact, frontendArtefact) <- maybe fatalError pure mArtefact
    -- Backend.
    let backendArtefact = backend moduleUnique config frontendArtefact
    runArtefacts config bootArtefact frontendArtefact backendArtefact artefacts

{-------------------------------------------------------------------------------
  Build artefacts
-------------------------------------------------------------------------------}

data Artefact (a :: Star) where
  -- Boot.
  HashIncludArgs :: Artefact [HashIncludeArg]
  -- Frontend.
  IncludeGraph   :: Artefact IncludeGraph.IncludeGraph
  DeclIndex      :: Artefact DeclIndex.DeclIndex
  UseDeclGraph   :: Artefact UseDeclGraph.UseDeclGraph
  DeclUseGraph   :: Artefact DeclUseGraph.DeclUseGraph
  ReifiedC       :: Artefact [C.Decl]
  -- Backend.
  Hs             :: Artefact [Hs.Decl]
  SHs            :: Artefact [SHs.SDecl]
  -- Writers.
  --
  -- TODO: The "write" function has access to 'Config' for now because it needs
  -- access to pretty printer configurations. I think we have two possibilities
  -- here: Either we collect all configuration into a single 'Config' object and
  -- let writers access this object. Or we separate pretty-printer related
  -- configuration and directly apply the configuration when defining the
  -- writers such as 'writeBindings'.
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

writeBindings :: Maybe FilePath -> Artefact ()
writeBindings mfile =
  Write
    (SHs :* Nil)
    (\Config{..} (I sHsDecls :* Nil) ->
      let translate :: [SHs.SDecl] -> PP.HsModule
          translate = PP.translateModule configHsModuleOpts

          render :: PP.HsModule -> String
          render = PP.render configHsRenderOpts
      in write mfile (render $ translate sHsDecls)
    )

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
  Boot
-------------------------------------------------------------------------------}

data BootArtefact = BootArtefact {
    bootHashIncludeArgs         :: [HashIncludeArg]
  , bootExternalBindingSpec     :: ExternalBindingSpec
  , bootPrescriptiveBindingSpec :: PrescriptiveBindingSpec
  }

boot ::
     Tracer IO TraceMsg
  -> ClangArgs
  -> BindingSpecConfig
  -> [UncheckedHashIncludeArg]
  -> IO BootArtefact
boot tracer clangArgs bindingSpecConfig uncheckedHashIncludeArgs = do
    let tracerHashInclude :: Tracer IO HashIncludeArgMsg
        tracerHashInclude = contramap TraceHashIncludeArg tracer
    hashIncludeArgs <-
      mapM (hashIncludeArgWithTrace tracerHashInclude) uncheckedHashIncludeArgs
    let tracerBindingSpec :: Tracer IO BindingSpecMsg
        tracerBindingSpec = contramap TraceBindingSpec tracer
    (extSpec, pSpec) <-
      loadBindingSpecs tracerBindingSpec clangArgs bindingSpecConfig
    pure BootArtefact {
        bootHashIncludeArgs = hashIncludeArgs
        , bootExternalBindingSpec = extSpec
        , bootPrescriptiveBindingSpec = pSpec
        }


{-------------------------------------------------------------------------------
  Frontend
-------------------------------------------------------------------------------}


data FrontendArtefact = FrontendArtefact {
    frontendIncludeGraph    :: IncludeGraph.IncludeGraph
  , frontendIndex           :: DeclIndex.DeclIndex
  , frontendUseDeclGraph    :: UseDeclGraph.UseDeclGraph
  , frontendDeclUseGraph    :: DeclUseGraph.DeclUseGraph
  , frontendCDecls          :: [C.Decl]
  }

frontend ::
     Tracer IO FrontendMsg
  -> Config
  -> BootArtefact
  -> IO FrontendArtefact
frontend tracer Config{..} BootArtefact{..} = do
    -- Frontend: Impure parse pass
    mParseResult <-
      withClang (contramap FrontendClang tracer) setup $ \unit -> Just <$> do
        (includeGraph, isMainHeader, isInMainHeaderDir, getMainHeader) <-
          processIncludes rootHeader unit
        reifiedUnit <- parseDecls
          (contramap FrontendParse tracer)
          rootHeader
          configParsePredicate
          includeGraph
          isMainHeader
          isInMainHeaderDir
          getMainHeader
          unit
        pure (reifiedUnit, isMainHeader, isInMainHeaderDir)

    (afterParse, isMainHeader, isInMainHeaderDir) <-
      maybe clangParseError pure mParseResult

    -- Frontend: Pure passes.
    let (afterSort, msgsSort) =
          sortDecls afterParse
        (afterHandleMacros, msgsHandleMacros) =
          handleMacros afterSort
        (afterNameAnon, msgsNameAnon) =
          nameAnon afterHandleMacros
        (afterResolveBindingSpec, msgsResolveBindingSpecs) =
          resolveBindingSpec bootExternalBindingSpec bootPrescriptiveBindingSpec afterNameAnon
        (afterSelect, msgsSelect) =
          selectDecls isMainHeader isInMainHeaderDir selectConfig afterResolveBindingSpec
        (afterHandleTypedefs, msgsHandleTypedefs) =
          handleTypedefs afterSelect
        (afterMangleNames, msgsMangleNames) =
          mangleNames afterHandleTypedefs

    -- TODO https://github.com/well-typed/hs-bindgen/issues/967: By emitting
    -- all traces in one place, we lose the callstack and timestamp
    -- information of the individual traces.

    -- TODO: Emitting traces forces all passes.
    forM_ msgsSort                $ traceWith tracer . FrontendSort
    forM_ msgsHandleMacros        $ traceWith tracer . FrontendHandleMacros
    forM_ msgsNameAnon            $ traceWith tracer . FrontendNameAnon
    forM_ msgsResolveBindingSpecs $ traceWith tracer . FrontendResolveBindingSpecs
    forM_ msgsSelect              $ traceWith tracer . FrontendSelect
    forM_ msgsHandleTypedefs      $ traceWith tracer . FrontendHandleTypedefs
    forM_ msgsMangleNames         $ traceWith tracer . FrontendMangleNames

    let -- Graphs.
        frontendIncludeGraph :: IncludeGraph.IncludeGraph
        frontendIncludeGraph = unitIncludeGraph afterParse
        frontendIndex        :: DeclIndex.DeclIndex
        frontendIndex        = declIndex $ unitAnn afterSort
        frontendUseDeclGraph :: UseDeclGraph.UseDeclGraph
        frontendUseDeclGraph = declUseDecl $ unitAnn afterSort
        frontendDeclUseGraph :: DeclUseGraph.DeclUseGraph
        frontendDeclUseGraph = declDeclUse $ unitAnn afterSort
        -- Declarations.
        frontendCDecls :: [C.Decl]
        frontendCDecls = C.unitDecls $ finalize afterMangleNames

    pure FrontendArtefact{..}
  where
    rootHeader :: RootHeader
    rootHeader = fromMainFiles bootHashIncludeArgs

    setup :: ClangSetup
    setup = (defaultClangSetup configClangArgs $ ClangInputMemory hFilePath hContent) {
          clangFlags = bitfieldEnum [
              CXTranslationUnit_SkipFunctionBodies
            , CXTranslationUnit_DetailedPreprocessingRecord
            , CXTranslationUnit_IncludeAttributedTypes
            , CXTranslationUnit_VisitImplicitAttributes
            ]
        }

    hFilePath :: FilePath
    hFilePath = getSourcePath name

    hContent :: String
    hContent = content rootHeader

    selectConfig :: SelectConfig
    selectConfig = SelectConfig configProgramSlicing configSelectPredicate

    clangParseError :: IO a
    clangParseError = do
      putStrLn "An unknown error happened while parsing headers with `libclang`"
      exitFailure

{-------------------------------------------------------------------------------
  Backend
-------------------------------------------------------------------------------}

data BackendArtefact = BackendArtefact {
    backendHsDecls  ::  [Hs.Decl]
  , backendSHsDecls :: [SHs.SDecl]
  }

backend :: ModuleUnique -> Config -> FrontendArtefact -> BackendArtefact
backend moduleUnique Config{..} FrontendArtefact{..} =
  BackendArtefact {
    backendHsDecls  = hsDecls
  , backendSHsDecls = sHsDecls
  }
  where
    hsDecls :: [Hs.Decl]
    hsDecls = Hs.generateDeclarations configTranslation moduleUnique frontendCDecls

    sHsDecls :: [SHs.SDecl]
    sHsDecls = SHs.simplifySHs $ SHs.translateDecls hsDecls

{-------------------------------------------------------------------------------
  Artefacts
-------------------------------------------------------------------------------}

runArtefacts ::
     Config
  -> BootArtefact
  -> FrontendArtefact
  -> BackendArtefact
  -> Artefacts as
  -> IO (NP I as)
runArtefacts
  config
  BootArtefact{..}
  FrontendArtefact{..}
  BackendArtefact{..} = go
  where
    go :: Artefacts as -> IO (NP I as)
    go Nil       = return Nil
    go (a :* as) = (:*) . I <$> runArtefact a <*> go as

    runArtefact :: Artefact a -> IO a
    runArtefact = \case
      --Boot.
      HashIncludArgs -> pure bootHashIncludeArgs
      -- Frontend.
      IncludeGraph -> pure frontendIncludeGraph
      DeclIndex    -> pure frontendIndex
      UseDeclGraph -> pure frontendUseDeclGraph
      DeclUseGraph -> pure frontendDeclUseGraph
      ReifiedC     -> pure frontendCDecls
      -- Backend.
      Hs     -> pure backendHsDecls
      SHs    -> pure backendSHsDecls
      -- Writer.
      (Write as' f) -> go as' >>= f config

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

write :: Maybe FilePath -> String -> IO ()
write mfile str =
    let out = case mfile of
          Nothing -> putStr
          Just file -> writeFile file
    in  out str
