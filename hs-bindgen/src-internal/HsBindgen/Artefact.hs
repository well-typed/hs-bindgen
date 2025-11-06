module HsBindgen.Artefact (
    Artefact(..)
  , Artefacts
  , ArtefactM
  , ArtefactEnv(..)
  , runArtefacts
  , sequenceArtefacts
  , ArtefactMsg(..)

    -- * Re-exports
  , I (..)
  , NP (..)
  )
where

import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.IORef (IORef, readIORef)
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
  OmitTypes           :: Artefact (Map C.QualName SourcePath)
  ReifiedC            :: Artefact [C.Decl]
  Dependencies        :: Artefact [SourcePath]
  -- * Backend
  HsDecls             :: Artefact (ByCategory [Hs.Decl])
  FinalDecls          :: Artefact (ByCategory ([UserlandCapiWrapper], [SHs.SDecl]))
  FinalModuleBaseName :: Artefact Hs.ModuleName
  FinalModuleSafe     :: Artefact HsModule
  FinalModuleUnsafe   :: Artefact HsModule
  FinalModules        :: Artefact (ByCategory HsModule)
  -- * Lift and sequence
  Lift                :: Artefacts as -> (NP I as -> ArtefactM b) -> Artefact b

instance Functor Artefact where
  fmap f x = Lift (x :* Nil) (\(I r :* Nil) -> pure (f r))

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
runArtefacts :: forall msg as.
     Tracer IO ArtefactMsg
  -> IORef (TracerState msg)
  -> BootArtefact
  -> FrontendArtefact
  -> BackendArtefact
  -> Artefacts as
  -> IO (Either (TraceException msg) (NP I as))
runArtefacts
  tracer
  tracerStateRef
  BootArtefact{..}
  FrontendArtefact{..}
  BackendArtefact{..}
  artefacts = runReaderT (go artefacts) env
  where
    env :: ArtefactEnv
    env = ArtefactEnv tracer

    go :: Artefacts x -> ArtefactM (Either (TraceException msg) (NP I x))
    go Nil       = pure (Right Nil)
    go (a :* as) = do
      artefactResult <- runArtefact a
      case artefactResult of
        Left errors -> pure (Left errors)
        Right artefact -> do
          tracerState <- liftIO $ readIORef tracerStateRef
          case tracerState of
            TracerState Error errors -> pure (Left (TraceException errors))
            _ -> do
              results <- go as
              case results of
                Left err -> pure (Left err)
                Right rs -> pure (Right (I artefact :* rs))

    runArtefact :: Artefact a -> ArtefactM (Either (TraceException msg) a)
    runArtefact = \case
      --Boot.
      HashIncludeArgs     -> Right <$> liftIO bootHashIncludeArgs
      -- Frontend.
      IncludeGraph        -> Right <$> liftIO frontendIncludeGraph
      GetMainHeaders      -> Right <$> liftIO frontendGetMainHeaders
      DeclIndex           -> Right <$> liftIO frontendIndex
      UseDeclGraph        -> Right <$> liftIO frontendUseDeclGraph
      DeclUseGraph        -> Right <$> liftIO frontendDeclUseGraph
      OmitTypes           -> Right <$> liftIO frontendOmitTypes
      ReifiedC            -> Right <$> liftIO frontendCDecls
      Dependencies        -> Right <$> liftIO frontendDependencies
      -- Backend.
      HsDecls             -> Right <$> liftIO backendHsDecls
      FinalDecls          -> Right <$> liftIO backendFinalDecls
      FinalModuleBaseName -> pure (Right backendFinalModuleBaseName)
      FinalModuleSafe     -> Right <$> liftIO backendFinalModuleSafe
      FinalModuleUnsafe   -> Right <$> liftIO backendFinalModuleUnsafe
      FinalModules        -> Right <$> liftIO backendFinalModules
      -- Lift and sequence.
      (Lift as' f)        -> go as' >>= \x -> mapM f x

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
    go acc []     = Lift acc $ \_results -> pure ()
    go acc (a:as) = go (a :* acc) as

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
