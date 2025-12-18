module HsBindgen.Artefact (
    -- * Artefacts
    Artefact(..)
  , runArtefacts
  , ArtefactMsg(..)
  )
where

import Control.Monad (liftM)
import Text.SimplePrettyPrint ((<+>), (><))
import Text.SimplePrettyPrint qualified as PP

import Clang.Paths

import HsBindgen.Backend
import HsBindgen.Backend.Category
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv (CWrapper)
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Boot
import HsBindgen.Config
import HsBindgen.Config.ClangArgs qualified as ClangArgs
import HsBindgen.DelayedIO
import HsBindgen.Frontend
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.ProcessIncludes qualified as ProcessIncludes
import HsBindgen.Frontend.RootHeader (HashIncludeArg)
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
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
  OmitTypes           :: Artefact [(C.DeclName, SourcePath)]
  ReifiedC            :: Artefact [C.Decl]
  Dependencies        :: Artefact [SourcePath]
  -- * Backend
  HsDecls             :: Artefact (ByCategory_ [Hs.Decl])
  FinalDecls          :: Artefact (ByCategory_ ([CWrapper], [SHs.SDecl]))
  FinalModuleBaseName :: Artefact BaseModuleName
  -- * Control flow
  EmitTrace           :: ArtefactMsg -> Artefact ()
  Lift                :: DelayedIOM a -> Artefact a
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

{-------------------------------------------------------------------------------
  Run artefacts
-------------------------------------------------------------------------------}

-- | Compute the results of a list of artefacts.
--
-- All top-level artefacts will be cached (this is not true for computed
-- artefacts, using, for example, the 'Functor' interface, or 'Lift').
runArtefacts :: forall a.
     Tracer ArtefactMsg
  -> BootArtefact
  -> FrontendArtefact
  -> BackendArtefact
  -> Artefact a
  -> IO (a, [DelayedIO])
runArtefacts
  tracer
  BootArtefact{..}
  FrontendArtefact{..}
  BackendArtefact{..}
  artefact = second reverse <$> (runDelayedIOM $ runArtefact artefact)
  where
    runArtefact :: forall x. Artefact x -> DelayedIOM x
    runArtefact = \case
        --Boot.
        Target              -> runCached bootTarget
        HashIncludeArgs     -> runCached bootHashIncludeArgs
        -- Frontend.
        IncludeGraph        -> runCached frontendIncludeGraph
        GetMainHeaders      -> runCached frontendGetMainHeaders
        DeclIndex           -> runCached frontendIndex
        UseDeclGraph        -> runCached frontendUseDeclGraph
        DeclUseGraph        -> runCached frontendDeclUseGraph
        OmitTypes           -> runCached frontendOmitTypes
        ReifiedC            -> runCached frontendCDecls
        Dependencies        -> runCached frontendDependencies
        -- Backend.
        HsDecls             -> runCached backendHsDecls
        FinalDecls          -> runCached backendFinalDecls
        FinalModuleBaseName -> pure backendFinalModuleBaseName
        -- Control flow
        (EmitTrace x)       -> emitTrace tracer x
        (Lift   f)          -> f
        (Bind x f)          -> runArtefact x >>= runArtefact . f

{-------------------------------------------------------------------------------
  Traces
-------------------------------------------------------------------------------}

data ArtefactMsg =
      NoBindingsSingleModule BaseModuleName
    | NoBindingsMultipleModules BaseModuleName
    | SkipWriteToFileNoBindings FileLocation
  deriving stock (Show, Generic)

instance PrettyForTrace ArtefactMsg where
  prettyForTrace = \case
    NoBindingsSingleModule md ->
      "Module" <+> PP.showToCtxDoc md <+> "does not contain any bindings"
    NoBindingsMultipleModules md ->
      "All binding categories with base module name" <+> PP.showToCtxDoc md <+> "are empty"
    SkipWriteToFileNoBindings fp ->
      "Skipping 'write file' operation (" >< PP.showToCtxDoc fp >< "): file is empty"

instance IsTrace SafeLevel ArtefactMsg where
  getDefaultLogLevel = \case
    NoBindingsSingleModule{}    -> SafeNotice
    NoBindingsMultipleModules{} -> SafeNotice
    SkipWriteToFileNoBindings{} -> SafeNotice
  getSource = const HsBindgen
  getTraceId = \case
    NoBindingsSingleModule{}    -> "artefact-no-bindings-single"
    NoBindingsMultipleModules{} -> "artefact-no-bindings-multiple"
    SkipWriteToFileNoBindings{} -> "artefact-skip-write-file"
