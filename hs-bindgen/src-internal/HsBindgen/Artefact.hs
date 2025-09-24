module HsBindgen.Artefact (
    Artefact(..)
  , Artefacts
  , runArtefacts
  , RunArtefactMsg(..)
  )
where

import Text.SimplePrettyPrint ((<+>))
import Text.SimplePrettyPrint qualified as PP

import Clang.Paths

import Generics.SOP (I (..), NP (..))

import HsBindgen.Backend
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv (UserlandCapiWrapper)
import HsBindgen.Backend.HsModule.Translation
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Boot
import HsBindgen.Frontend
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.RootHeader (HashIncludeArg)
import HsBindgen.Imports
import HsBindgen.Language.Haskell
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Artefact
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
  FinalModuleSafe     :: Artefact HsModule
  FinalModuleUnsafe   :: Artefact HsModule
  FinalModules        :: Artefact (ByCategory HsModule)
  -- * Lift and sequence
  -- TODO_PR.
  Lift                :: Artefacts as -> (NP I as -> ArtefactM b) -> Artefact b

instance Functor Artefact where
  fmap f x = Lift (x :* Nil) (\_ (I r :* Nil) -> pure (f r))

-- | A list of 'Artefact's.
type Artefacts as = NP Artefact as

{-------------------------------------------------------------------------------
  Run artefacts
-------------------------------------------------------------------------------}

-- | Compute the results of a list of artefacts.
--
-- All top-level artefacts will be cached (this is not true for computed
-- artefacts, using, for example, the 'Functor' interface, or 'Lift').
runArtefacts ::
     Tracer IO RunArtefactMsg
  -> BootArtefact
  -> FrontendArtefact
  -> BackendArtefact
  -> Artefacts as
  -> IO (NP I as)
runArtefacts
  tracer
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
      HashIncludeArgs     -> bootHashIncludeArgs
      -- Frontend.
      IncludeGraph        -> frontendIncludeGraph
      DeclIndex           -> frontendIndex
      UseDeclGraph        -> frontendUseDeclGraph
      DeclUseGraph        -> frontendDeclUseGraph
      ReifiedC            -> frontendCDecls
      Dependencies        -> frontendDependencies
      -- Backend.
      HsDecls             -> backendHsDecls
      FinalDecls          -> backendFinalDecls
      FinalModuleBaseName -> pure backendFinalModuleBaseName
      FinalModuleSafe     -> backendFinalModuleSafe
      FinalModuleUnsafe   -> backendFinalModuleUnsafe
      FinalModules        -> backendFinalModules
      -- Lift and sequence.
      (Lift as' f)        -> go as' >>= f tracer

{-------------------------------------------------------------------------------
  Traces
-------------------------------------------------------------------------------}

data RunArtefactMsg = RunArtefactWriteFile String FilePath
  deriving stock (Show, Generic)

instance PrettyForTrace RunArtefactMsg where
  prettyForTrace = \case
    RunArtefactWriteFile what path ->
      "Writing" <+> PP.showToCtxDoc what <+> "to file" <+> PP.showToCtxDoc path

instance IsTrace SafeLevel RunArtefactMsg where
  getDefaultLogLevel = \case
    RunArtefactWriteFile _ _ -> SafeInfo
  getSource = const HsBindgen
  getTraceId = \case
    RunArtefactWriteFile _ _ -> "run-artefact-write-file"
