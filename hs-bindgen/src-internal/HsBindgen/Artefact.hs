module HsBindgen.Artefact (
    Artefact(..)
  , ArtefactM
  , ArtefactEnv(..)
  , FileSystemAction(..)
  , runArtefacts
  , ArtefactMsg(..)
  , FileContent(..)
  )
where

import Control.Monad (liftM)
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS.CPS (RWST, runRWST, tell)
import Data.IORef (IORef)
import Text.SimplePrettyPrint ((<+>))
import Text.SimplePrettyPrint qualified as PP

import Clang.Paths

import HsBindgen.Backend
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv (UserlandCapiWrapper)
import HsBindgen.Backend.HsModule.Translation
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.BindingSpec.Private.V1 (UnresolvedBindingSpec)
import HsBindgen.Boot
import HsBindgen.Config
import HsBindgen.Config.ClangArgs qualified as ClangArgs
import HsBindgen.Frontend
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.ProcessIncludes qualified as ProcessIncludes
import HsBindgen.Frontend.RootHeader (HashIncludeArg)
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Artefact
-------------------------------------------------------------------------------}

-- | Build artefact.
data Artefact (a :: Star) where
  -- * Boot
  Target              :: Artefact ClangArgs.Target
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
  FinalModuleBaseName :: Artefact BaseModuleName
  FinalModuleSafe     :: Artefact HsModule
  FinalModuleUnsafe   :: Artefact HsModule
  FinalModules        :: Artefact (ByCategory HsModule)
  -- * File writes
  FileWrite           :: String -> FilePath -> FileContent -> Artefact ()
  -- * Lift and sequence artefacts
  Lift                :: ArtefactM a -> Artefact a
  Bind                :: Artefact b  -> (b -> Artefact c ) -> Artefact c

instance Functor Artefact where
  fmap :: (a -> b) -> Artefact a -> Artefact b
  fmap = liftM

instance Applicative Artefact where
  pure :: a -> Artefact a
  pure = Lift . pure

  (<*>) :: Artefact (a -> b) -> Artefact a -> Artefact b
  (<*>) = ap

instance Monad Artefact where
  (>>=) :: Artefact a -> (a -> Artefact b) -> Artefact b
  (>>=) = Bind

instance MonadIO Artefact where
  liftIO :: IO a -> Artefact a
  liftIO = Lift . liftIO

{-------------------------------------------------------------------------------
  Artefact monad
-------------------------------------------------------------------------------}

type ArtefactM = RWST ArtefactEnv [FileSystemAction] () IO

data ArtefactEnv = ArtefactEnv {
      artefactTracer :: Tracer ArtefactMsg
    }

-- | A file system action to be executed
data FileSystemAction =
  WriteFile String FilePath FileContent

-- | Content to be written to a file
--
-- If it's TextContent then, depending on the FileOverwritePolicy, then this
-- code is going to run:
--
-- > createDirectoryIfMissing True $ takeDirectory path
-- > writeFile path str
--
-- If it's BindingSpecContent then, depending on the FileOverwritePolicy, then
-- this code is going to run:
--
-- > BindingSpec.writeFile path ubs
--
data FileContent
    = TextContent String
    | BindingSpecContent UnresolvedBindingSpec
    deriving Show

{-------------------------------------------------------------------------------
  Run artefacts
-------------------------------------------------------------------------------}

-- | Compute the results of a list of artefacts.
--
-- All top-level artefacts will be cached (this is not true for computed
-- artefacts, using, for example, the 'Functor' interface, or 'Lift').
--
runArtefacts :: forall e a.
     Tracer ArtefactMsg
  -> IORef (TracerState e)
  -> BootArtefact
  -> FrontendArtefact
  -> BackendArtefact
  -> Artefact a
  -> IO (Either (TraceException e) a, [FileSystemAction])
runArtefacts
  tracer
  tracerStateRef
  BootArtefact{..}
  FrontendArtefact{..}
  BackendArtefact{..}
  artefact = do
    (result, _, actions) <- runRWST (runExceptT (runArtefact artefact)) env ()
    return (result, actions)
  where
    env :: ArtefactEnv
    env = ArtefactEnv tracer

    runArtefact :: forall x. Artefact x -> ExceptT (TraceException e) ArtefactM x
    runArtefact = \case
      --Boot.
      Target              -> liftIO bootTarget
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
      (Lift f)            -> lift f
      (Bind x   f)        -> do
        r <- runArtefact x
        -- Check tracer state for 'Error' traces
        mbError <- checkTracerState tracerStateRef
        case mbError of
          Just err -> throwError err
          Nothing  -> runArtefact $ f r
      -- File writes.
      FileWrite what path content -> lift $ tell [WriteFile what path content]

{-------------------------------------------------------------------------------
  Traces
-------------------------------------------------------------------------------}

data ArtefactMsg = RunArtefactWriteFile FilePath FileContent
  deriving stock (Show, Generic)

instance PrettyForTrace ArtefactMsg where
  prettyForTrace = \case
    RunArtefactWriteFile path what ->
      "Writing" <+> PP.showToCtxDoc what <+> "to file" <+> PP.showToCtxDoc path

instance IsTrace SafeLevel ArtefactMsg where
  getDefaultLogLevel = \case
    RunArtefactWriteFile _ _ -> SafeInfo
  getSource = const HsBindgen
  getTraceId = \case
    RunArtefactWriteFile _ _ -> "run-artefact-write-file"
