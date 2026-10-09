module HsBindgen.Artefact (
    -- * Frontend passes
    FrontendPass(..)
    -- * Artefacts
  , Artefact(..)
  , Stages(..)
  , runArtefacts
  , ArtefactMsg(..)
  )
where

import Control.Monad (liftM)
import Text.SimplePrettyPrint ((<+>), (><))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.ArtefactM
import HsBindgen.Backend
import HsBindgen.Backend.Category
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv (CWrapper)
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Boot
import HsBindgen.Cache (Cached)
import HsBindgen.Config
import HsBindgen.Config.Internal
import HsBindgen.Frontend
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass (AdjustTypes)
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.EnrichComments.IsPass (EnrichComments)
import HsBindgen.Frontend.Pass.FillUnnamedIds.IsPass (FillUnnamedIds)
import HsBindgen.Frontend.Pass.Final (Final)
import HsBindgen.Frontend.Pass.MangleNames.IsPass (MangleNames)
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse)
import HsBindgen.Frontend.Pass.Parse.Result (ParseResult)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass (ReparseMacroExpansions)
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass (ResolveBindingSpecs)
import HsBindgen.Frontend.Pass.Select.IsPass (Select)
import HsBindgen.Frontend.Pass.SimplifyAST.IsPass (SimplifyAST)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass (TypecheckMacros)
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Util.Tracer

import Doxygen.Parser (Doxygen)

{-------------------------------------------------------------------------------
  Frontend passes
-------------------------------------------------------------------------------}

-- | Frontend passes
--
-- Each constructor corresponds to a frontend pass carrying the result type of
-- that pass. See "HsBindgen.Frontend" for the pass ordering and descriptions.
data FrontendPass (l :: Star) (result :: Star) where
  ParsePass
    :: FrontendPass l [ParseResult       l Parse]
  SimplifyASTPass
    :: FrontendPass l [ParseResult       l SimplifyAST]
  FillUnnamedIdsPass
    :: FrontendPass l [ParseResult       l FillUnnamedIds]
  EnrichCommentsPass
    :: FrontendPass l [ParseResult       l EnrichComments]
  ConstructTranslationUnitPass
    :: FrontendPass l (C.TranslationUnit l ConstructTranslationUnit)
  TypecheckMacrosPass
    :: FrontendPass l (C.TranslationUnit l TypecheckMacros)
  ReparseMacroExpansionsPass
    :: FrontendPass l (C.TranslationUnit l ReparseMacroExpansions)
  ResolveBindingSpecsPass
    :: FrontendPass l (C.TranslationUnit l ResolveBindingSpecs)
  MangleNamesPass
    :: FrontendPass l (C.TranslationUnit l MangleNames)
  AdjustTypesPass
    :: FrontendPass l (C.TranslationUnit l AdjustTypes)
  SelectPass
    :: FrontendPass l (C.TranslationUnit l Select)
  FinalPass
    :: FrontendPass l (C.TranslationUnit l Final)

{-------------------------------------------------------------------------------
  Artefact
-------------------------------------------------------------------------------}

-- | Build artefact.
data Artefact l (a :: Star) where
  -- * Boot
  RootDirectives  :: Artefact l [C.RootDirective C.HashIncludeArg]
  ModuleBaseName  :: Artefact l BaseModuleName
  -- * Frontend
  ParseInfoA      :: Artefact l ParseInfo
  DoxygenA        :: Artefact l Doxygen
  FrontendPassA   :: FrontendPass l result -> Artefact l result
  -- * Backend
  HsDecls         :: Artefact l (ByCategory_ [Hs.Decl l])
  FinalDecls      :: Artefact l (ByCategory_ ([CWrapper], [SHs.SDecl]))
  -- * Control flow
  EmitTrace       :: ArtefactMsg -> Artefact l ()
  -- | Report what a frontend pass would report
  --
  -- This is for an artefact that does the work of a pass outside the
  -- frontend, as the selection of a library is done.  The message goes where
  -- the messages of the frontend go, so it can be a warning, and it has the
  -- name of the module inside 'WithModule'.
  EmitFrontendTrace :: FrontendMsg -> Artefact l ()
  Lift            :: ArtefactM a -> Artefact l a
  Bind            :: Artefact l b -> (b -> Artefact l c) -> Artefact l c
  -- * One module of several
  -- | Compute an artefact for one module of the translation unit
  --
  -- Inside, every artefact is that of the module: the name is the module's,
  -- and so are the frontend passes from @ResolveBindingSpecs@ on and the
  -- backend.  What comes before those passes is computed once and shared.
  WithModule      :: ModuleScope -> Artefact l a -> Artefact l a

instance Functor (Artefact l) where
  fmap :: (a -> b) -> Artefact l a -> Artefact l b
  fmap = liftM

instance Applicative (Artefact l) where
  pure :: a -> Artefact l a
  pure = Lift . pure

  (<*>) :: Artefact l (a -> b) -> Artefact l a -> Artefact l b
  (<*>) = ap

instance Monad (Artefact l) where
  (>>=) :: Artefact l a -> (a -> Artefact l b) -> Artefact l b
  (>>=) = Bind

{-------------------------------------------------------------------------------
  Run artefacts
-------------------------------------------------------------------------------}

-- | What artefacts are computed from
data Stages l = Stages {
      boot     :: BootArtefact l
    , frontend :: FrontendArtefact l
    , backend  :: BackendArtefact l
      -- | Where the frontend of these stages reports
    , tracer   :: Tracer FrontendMsg
    }

-- | Compute the results of a list of artefacts.
--
-- All top-level artefacts will be cached (this is not true for computed
-- artefacts, using, for example, the 'Functor' interface, or 'Lift').
--
-- A 'WithModule' artefact is computed from stages of its own, which hold the
-- caches of that module.  Entering the same module twice computes it twice.
runArtefacts :: forall a l.
     Tracer ArtefactMsg
  -> BindgenConfig
  -> (ModuleScope -> Cached (Stages l))
     -- ^ The stages of one module of the translation unit
  -> Stages l
  -> Artefact l a
  -> IO (a, [DelayedIO])
runArtefacts tracer config stagesOfModule stages0 artefact =
    second reverse <$> (runArtefactM (runArtefact stages0 artefact) config)
  where
    runArtefact :: forall x. Stages l -> Artefact l x -> ArtefactM x
    runArtefact stages = \case
        --Boot.
        RootDirectives  -> runCached stages.boot.rootDirectives
        ModuleBaseName  -> pure stages.boot.baseModule
        -- Frontend.
        ParseInfoA      -> runCached stages.frontend.parseMeta
        DoxygenA        -> runCached stages.frontend.doxygen
        FrontendPassA p -> runFrontendPass stages.frontend p
        -- Backend.
        HsDecls         -> runCached stages.backend.hsDecls
        FinalDecls      -> runCached stages.backend.finalDecls
        -- Control flow
        (EmitTrace x)   -> emitTrace tracer x
        (EmitFrontendTrace x) -> emitTrace stages.tracer x
        (Lift   f)      -> f
        (Bind x f)      -> runArtefact stages x >>= runArtefact stages . f
        -- One module of several
        (WithModule scope x) -> do
          stagesModule <- runCached (stagesOfModule scope)
          runArtefact stagesModule x

    runFrontendPass ::
         FrontendArtefact l
      -> FrontendPass l result
      -> ArtefactM result
    runFrontendPass frontend = \case
        ParsePass                    -> runCached frontend.parse
        SimplifyASTPass              -> runCached frontend.simplifyAST
        FillUnnamedIdsPass           -> runCached frontend.fillUnnamedIds
        EnrichCommentsPass           -> runCached frontend.enrichComments
        ConstructTranslationUnitPass -> runCached frontend.constructTranslationUnit
        TypecheckMacrosPass          -> runCached frontend.typecheckMacros
        ReparseMacroExpansionsPass   -> runCached frontend.reparseMacroExpansions
        ResolveBindingSpecsPass      -> runCached frontend.resolveBindingSpecs
        MangleNamesPass              -> runCached frontend.mangleNames
        AdjustTypesPass              -> runCached frontend.adjustTypes
        SelectPass                   -> runCached frontend.select
        FinalPass                    -> runCached frontend.final

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
      "No output generated (module " <+> PP.show md <+> " is empty)"
    NoBindingsMultipleModules md ->
      "No output generated (all binding categories with base module name" <+> PP.show md <+> "are empty)"
    SkipWriteToFileNoBindings fp ->
      "Skipping 'write file' operation (" >< PP.show fp >< "): file is empty"

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
