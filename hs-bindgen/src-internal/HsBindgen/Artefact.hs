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

import HsBindgen.Backend
import HsBindgen.Backend.Category
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv (CWrapper)
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Boot
import HsBindgen.Config
import HsBindgen.DelayedIO
import HsBindgen.Frontend
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass (AdjustTypes)
import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass (AssignAnonIds)
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.Final (Final)
import HsBindgen.Frontend.Pass.HandleMacros.IsPass (HandleMacros)
import HsBindgen.Frontend.Pass.MangleNames.IsPass (MangleNames)
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse)
import HsBindgen.Frontend.Pass.Parse.Result (ParseResult)
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass (ResolveBindingSpecs)
import HsBindgen.Frontend.Pass.Select.IsPass (Select)
import HsBindgen.Frontend.Pass.SimplifyAST.IsPass (SimplifyAST)
import HsBindgen.Frontend.RootHeader (HashIncludeArg)
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Frontend passes
-------------------------------------------------------------------------------}

-- | Frontend passes
--
-- Each constructor corresponds to a frontend pass carrying the result type of
-- that pass. See "HsBindgen.Frontend" for the pass ordering and descriptions.
data FrontendPass (result :: Star) where
-- TODO-D: Rename to ParsePass etc.
  DumpParse
    :: FrontendPass [ParseResult Parse]
  DumpSimplifyAST
    :: FrontendPass [ParseResult SimplifyAST]
  DumpAssignAnonIds
    :: FrontendPass [ParseResult AssignAnonIds]
  DumpConstructTranslationUnit
    :: FrontendPass (C.TranslationUnit ConstructTranslationUnit)
  DumpHandleMacros
    :: FrontendPass (C.TranslationUnit HandleMacros)
  DumpResolveBindingSpecs
    :: FrontendPass (C.TranslationUnit ResolveBindingSpecs)
  DumpMangleNames
    :: FrontendPass (C.TranslationUnit MangleNames)
  DumpAdjustTypes
    :: FrontendPass (C.TranslationUnit AdjustTypes)
  DumpSelect
    :: FrontendPass (C.TranslationUnit Select)
  DumpFinal
    :: FrontendPass (C.TranslationUnit Final)

{-------------------------------------------------------------------------------
  Artefact
-------------------------------------------------------------------------------}

-- | Build artefact.
data Artefact (a :: Star) where
  -- * Boot
  HashIncludeArgs     :: Artefact [HashIncludeArg]
  -- * Frontend passes
  ParseMetaA          :: Artefact ParseMeta
  FrontendPassA       :: FrontendPass result -> Artefact result
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
runArtefacts tracer boot frontend backend artefact =
    second reverse <$> (runDelayedIOM $ runArtefact artefact)
  where
    runArtefact :: forall x. Artefact x -> DelayedIOM x
    runArtefact = \case
        --Boot.
        HashIncludeArgs     -> runCached boot.hashIncludeArgs

        -- Frontend.
        ParseMetaA          -> runCached frontend.parseMeta
        FrontendPassA p     -> runFrontendPass frontend p
        -- Backend.
        HsDecls             -> runCached backend.hsDecls
        FinalDecls          -> runCached backend.finalDecls
        FinalModuleBaseName -> pure backend.finalModuleBaseName
        -- Control flow
        (EmitTrace x)       -> emitTrace tracer x
        (Lift   f)          -> f
        (Bind x f)          -> runArtefact x >>= runArtefact . f

    runFrontendPass :: FrontendArtefact -> FrontendPass result -> DelayedIOM result
    runFrontendPass fe = \case
        DumpParse                    -> runCached fe.parse
        DumpSimplifyAST              -> runCached fe.simplifyAST
        DumpAssignAnonIds            -> runCached fe.assignAnonIds
        DumpConstructTranslationUnit -> runCached fe.constructTranslationUnit
        DumpHandleMacros             -> runCached fe.handleMacros
        DumpResolveBindingSpecs      -> runCached fe.resolveBindingSpecs
        DumpMangleNames              -> runCached fe.mangleNames
        DumpAdjustTypes              -> runCached fe.adjustTypes
        DumpSelect                   -> runCached fe.select
        DumpFinal                    -> runCached fe.final

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
      "No output generated (all binding categories with base module name " <+> PP.show md <+> " are empty)"
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
