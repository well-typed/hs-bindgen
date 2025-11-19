module HsBindgen.Artefact (
    Artefact(..)
  , Artefacts
  , ArtefactM
  , ArtefactEnv(..)
  , runArtefacts
  , ArtefactMsg(..)

    -- * Re-exports
  , I (..)
  , NP (..)
  )
where

import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.IORef (IORef)
import Generics.SOP (I (..), NP (..))
import Text.SimplePrettyPrint ((<+>))
import Text.SimplePrettyPrint qualified as PP

import Clang.Paths

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
import HsBindgen.Frontend.ProcessIncludes qualified as ProcessIncludes
import HsBindgen.Frontend.RootHeader (HashIncludeArg)
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
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
  GetMainHeaders      :: Artefact ProcessIncludes.GetMainHeaders
  DeclIndex           :: Artefact DeclIndex.DeclIndex
  UseDeclGraph        :: Artefact UseDeclGraph.UseDeclGraph
  DeclUseGraph        :: Artefact DeclUseGraph.DeclUseGraph
  OmitTypes           :: Artefact [(C.QualName, SourcePath)]
  ReifiedC            :: Artefact [C.Decl]
  Dependencies        :: Artefact [SourcePath]
  -- * Backend
  HsDecls             :: Artefact (ByCategory [Hs.Decl])
  FinalDecls          :: Artefact (ByCategory ([UserlandCapiWrapper], [SHs.SDecl]))
  FinalModuleBaseName :: Artefact Hs.ModuleName
  FinalModuleSafe     :: Artefact HsModule
  FinalModuleUnsafe   :: Artefact HsModule
  FinalModules        :: Artefact (ByCategory HsModule)
  -- * Sequence artefacts
  Lift                :: Artefacts as -> (NP I as -> ArtefactM b) -> Artefact b
  Bind                :: Artefact b   -> (b -> Artefact c)        -> Artefact c

instance Functor Artefact where
  fmap :: (a -> b) -> Artefact a -> Artefact b
  fmap f x = Lift (x :* Nil) (\(I r :* Nil) -> pure (f r))

instance Applicative Artefact where
  pure :: a -> Artefact a
  pure x = Lift Nil (\Nil -> pure x)

  liftA2 :: (a -> b -> c) -> Artefact a -> Artefact b -> Artefact c
  liftA2 f x y = Lift (x :* y :* Nil) (\(I l :* I r :* Nil) -> pure (f l r))

instance Monad Artefact where
  (>>=) :: Artefact a -> (a -> Artefact b) -> Artefact b
  (>>=) = Bind

-- | A list of 'Artefact's.
type Artefacts as = NP Artefact as

{-------------------------------------------------------------------------------
  Artefact monad
-------------------------------------------------------------------------------}

type ArtefactM = ReaderT ArtefactEnv IO

data ArtefactEnv = ArtefactEnv {
      artefactTracer :: Tracer IO ArtefactMsg
    }

{-------------------------------------------------------------------------------
  Run artefacts
-------------------------------------------------------------------------------}

-- | Compute the results of a list of artefacts.
--
-- All top-level artefacts will be cached (this is not true for computed
-- artefacts, using, for example, the 'Functor' interface, or 'Lift').
runArtefacts :: forall e as.
     Tracer IO ArtefactMsg
  -> IORef (TracerState e)
  -> BootArtefact
  -> FrontendArtefact
  -> BackendArtefact
  -> Artefacts as
  -> IO (Either (TraceException e) (NP I as))
runArtefacts
  tracer
  tracerStateRef
  BootArtefact{..}
  FrontendArtefact{..}
  BackendArtefact{..}
  artefacts = runReaderT (runExceptT (go artefacts)) env
  where
    env :: ArtefactEnv
    env = ArtefactEnv tracer

    go :: Artefacts a -> ExceptT (TraceException e) ArtefactM (NP I a)
    go Nil       = pure Nil
    go (a :* as) = do
      artefactResult <- runArtefact a
      mbError <- checkTracerState tracerStateRef
      case mbError of
        Just err -> throwError err
        Nothing  -> (I artefactResult :*) <$> (go as)

    runArtefact :: Artefact a -> ExceptT (TraceException e) ArtefactM a
    runArtefact = \case
      --Boot.
      HashIncludeArgs     -> liftIO bootHashIncludeArgs
      -- Frontend.
      IncludeGraph        -> liftIO frontendIncludeGraph
      GetMainHeaders      -> liftIO frontendGetMainHeaders
      DeclIndex           -> liftIO frontendIndex
      UseDeclGraph        -> liftIO frontendUseDeclGraph
      DeclUseGraph        -> liftIO frontendDeclUseGraph
      OmitTypes           -> liftIO frontendOmitTypes
      ReifiedC            -> liftIO frontendCDecls
      Dependencies        -> liftIO frontendDependencies
      -- Backend.
      HsDecls             -> liftIO backendHsDecls
      FinalDecls          -> liftIO backendFinalDecls
      FinalModuleBaseName -> pure backendFinalModuleBaseName
      FinalModuleSafe     -> liftIO backendFinalModuleSafe
      FinalModuleUnsafe   -> liftIO backendFinalModuleUnsafe
      FinalModules        -> liftIO backendFinalModules
      -- Lift and sequence.
      (Lift as' f)        -> go as'        >>= lift . f
      (Bind x   f)        -> runArtefact x >>= runArtefact . f

{-------------------------------------------------------------------------------
  Traces
-------------------------------------------------------------------------------}

data ArtefactMsg = RunArtefactWriteFile String FilePath
  deriving stock (Show, Generic)

instance PrettyForTrace ArtefactMsg where
  prettyForTrace = \case
    RunArtefactWriteFile what path ->
      "Writing" <+> PP.showToCtxDoc what <+> "to file" <+> PP.showToCtxDoc path

instance IsTrace SafeLevel ArtefactMsg where
  getDefaultLogLevel = \case
    RunArtefactWriteFile _ _ -> SafeInfo
  getSource = const HsBindgen
  getTraceId = \case
    RunArtefactWriteFile _ _ -> "run-artefact-write-file"
