module HsBindgen.Artefact (
    -- * Frontend passes
    FrontendPass(..)
    -- * Artefacts
  , Artefact(..)
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
import HsBindgen.Config
import HsBindgen.Config.Internal
import HsBindgen.Frontend
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass (AdjustTypes)
import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass (AssignAnonIds)
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.EnrichComments.IsPass (EnrichComments)
import HsBindgen.Frontend.Pass.Final (Final)
import HsBindgen.Frontend.Pass.MangleNames.IsPass (MangleNames)
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse)
import HsBindgen.Frontend.Pass.Parse.Result (ParseResult)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass (ReparseMacroExpansions)
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass (ResolveBindingSpecs)
import HsBindgen.Frontend.Pass.Select.IsPass (Select)
import HsBindgen.Frontend.Pass.SimplifyAST.IsPass (SimplifyAST)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass (TypecheckMacros)
import HsBindgen.Frontend.RootHeader (HashIncludeArg)
import HsBindgen.Imports
import HsBindgen.Util.Tracer

import Doxygen.Parser (Doxygen)

{-------------------------------------------------------------------------------
  Frontend passes
-------------------------------------------------------------------------------}

-- | Frontend passes
--
-- Each constructor corresponds to a frontend pass carrying the result type of
-- that pass. See "HsBindgen.Frontend" for the pass ordering and descriptions.
data FrontendPass (result :: Star) where
  ParsePass
    :: FrontendPass [ParseResult Parse]
  SimplifyASTPass
    :: FrontendPass [ParseResult SimplifyAST]
  AssignAnonIdsPass
    :: FrontendPass [ParseResult AssignAnonIds]
  EnrichCommentsPass
    :: FrontendPass [ParseResult EnrichComments]
  ConstructTranslationUnitPass
    :: FrontendPass (C.TranslationUnit ConstructTranslationUnit)
  TypecheckMacrosPass
    :: FrontendPass (C.TranslationUnit TypecheckMacros)
  ReparseMacroExpansionsPass
    :: FrontendPass (C.TranslationUnit ReparseMacroExpansions)
  ResolveBindingSpecsPass
    :: FrontendPass (C.TranslationUnit ResolveBindingSpecs)
  MangleNamesPass
    :: FrontendPass (C.TranslationUnit MangleNames)
  AdjustTypesPass
    :: FrontendPass (C.TranslationUnit AdjustTypes)
  SelectPass
    :: FrontendPass (C.TranslationUnit Select)
  FinalPass
    :: FrontendPass (C.TranslationUnit Final)

{-------------------------------------------------------------------------------
  Artefact
-------------------------------------------------------------------------------}

-- | Build artefact.
data Artefact (a :: Star) where
  -- * Boot
  HashIncludeArgs :: Artefact [HashIncludeArg]
  ModuleBaseName  :: Artefact BaseModuleName
  -- * Frontend
  ParseInfoA      :: Artefact ParseInfo
  DoxygenA        :: Artefact Doxygen
  FrontendPassA   :: FrontendPass result -> Artefact result
  -- * Backend
  HsDecls         :: Artefact (ByCategory_ [Hs.Decl])
  FinalDecls      :: Artefact (ByCategory_ ([CWrapper], [SHs.SDecl]))
  -- * Control flow
  EmitTrace       :: ArtefactMsg -> Artefact ()
  Lift            :: ArtefactM a -> Artefact a
  Bind            :: Artefact b  -> (b -> Artefact c ) -> Artefact c

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
  -> BindgenConfig
  -> BootArtefact
  -> FrontendArtefact
  -> BackendArtefact
  -> Artefact a
  -> IO (a, [DelayedIO])
runArtefacts tracer config boot frontend backend artefact =
    second reverse <$> (runArtefactM (runArtefact artefact) config)
  where
    runArtefact :: forall x. Artefact x -> ArtefactM x
    runArtefact = \case
        --Boot.
        HashIncludeArgs -> runCached boot.hashIncludeArgs
        ModuleBaseName  -> pure boot.baseModule
        -- Frontend.
        ParseInfoA      -> runCached frontend.parseMeta
        DoxygenA        -> runCached frontend.doxygen
        FrontendPassA p -> runFrontendPass p
        -- Backend.
        HsDecls         -> runCached backend.hsDecls
        FinalDecls      -> runCached backend.finalDecls
        -- Control flow
        (EmitTrace x)   -> emitTrace tracer x
        (Lift   f)      -> f
        (Bind x f)      -> runArtefact x >>= runArtefact . f

    runFrontendPass :: FrontendPass result -> ArtefactM result
    runFrontendPass = \case
        ParsePass                    -> runCached frontend.parse
        SimplifyASTPass              -> runCached frontend.simplifyAST
        AssignAnonIdsPass            -> runCached frontend.assignAnonIds
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
