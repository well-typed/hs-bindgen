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
  DeclIndex           :: Artefact DeclIndex.DeclIndex
  UseDeclGraph        :: Artefact UseDeclGraph.UseDeclGraph
  DeclUseGraph        :: Artefact DeclUseGraph.DeclUseGraph
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
runArtefacts ::
     Tracer IO ArtefactMsg
  -> BootArtefact
  -> FrontendArtefact
  -> BackendArtefact
  -> Artefacts as
  -> IO (NP I as)
runArtefacts
  tracer
  BootArtefact{..}
  FrontendArtefact{..}
  BackendArtefact{..}
  artefacts = runReaderT (go artefacts) env
  where
    env :: ArtefactEnv
    env = ArtefactEnv tracer

    go :: Artefacts as -> ArtefactM (NP I as)
    go Nil       = pure Nil
    go (a :* as) = (:*) . I <$> runArtefact a <*> go as

    runArtefact :: Artefact a -> ArtefactM a
    runArtefact = \case
      --Boot.
      HashIncludeArgs     -> liftIO bootHashIncludeArgs
      -- Frontend.
      IncludeGraph        -> liftIO frontendIncludeGraph
      DeclIndex           -> liftIO frontendIndex
      UseDeclGraph        -> liftIO frontendUseDeclGraph
      DeclUseGraph        -> liftIO frontendDeclUseGraph
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
      (Lift as' f)        -> go as' >>= f

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
