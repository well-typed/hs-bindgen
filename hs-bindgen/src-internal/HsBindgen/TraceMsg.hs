-- | Top-level sum-type that captures all trace messages we might emit
--
-- Intended for unqualified import.
module HsBindgen.TraceMsg (
    TraceMsg(..)
    -- * Messages from individual passes
  , BindingSpecMsg(..)
  , ClangMsg(..)
  , DeclIndexError(..)
  , Diagnostic(..)
  , FrontendMsg(..)
  , ParseMsg(..)
  , SortMsg(..)
  , SliceMsg(..)
  , HandleMacrosMsg(..)
  , NameAnonMsg(..)
  , ResolveBindingSpecMsg(..)
  , HandleTypedefsMsg(..)
  , MangleNamesMsg(..)
  , ParseTypeException(..)
  , ReparseError(..)
  , ResolveHeaderMsg(..)
  , TcMacroError(..)
  -- * Log level customization
  , customLogLevelFrom
  , CustomLogLevelSetting (..)
  ) where

import Clang.HighLevel.Types (Diagnostic (..))
import HsBindgen.BindingSpec (BindingSpecMsg (..))
import HsBindgen.C.Reparse.Infra (ReparseError (..))
import HsBindgen.C.Tc.Macro (TcMacroError (..))
import HsBindgen.Clang (ClangMsg (..))
import HsBindgen.Frontend (FrontendMsg (..))
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndexError (..))
import HsBindgen.Frontend.Pass.HandleMacros.IsPass (HandleMacrosMsg (..))
import HsBindgen.Frontend.Pass.HandleTypedefs.IsPass (HandleTypedefsMsg (..))
import HsBindgen.Frontend.Pass.MangleNames.IsPass (MangleNamesMsg (..))
import HsBindgen.Frontend.Pass.NameAnon.IsPass (NameAnonMsg (..))
import HsBindgen.Frontend.Pass.Parse.IsPass (ParseMsg (..))
import HsBindgen.Frontend.Pass.Parse.Type.Monad (ParseTypeException (..))
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass (ResolveBindingSpecMsg (..))
import HsBindgen.Frontend.Pass.Slice.IsPass (SliceMsg (..))
import HsBindgen.Frontend.Pass.Sort.IsPass (SortMsg (..))
import HsBindgen.Resolve (ResolveHeaderMsg (..))
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  HsBindgen traces
-------------------------------------------------------------------------------}

-- | Traces supported by @hs-bindgen@.
--
-- Lazy on purpose to avoid evaluation when traces are not reported.
data TraceMsg =
    TraceBindingSpec BindingSpecMsg
  | TraceClang ClangMsg
  | TraceFrontend FrontendMsg
  | TraceResolveHeader ResolveHeaderMsg
  deriving stock (Show, Eq)

instance PrettyForTrace TraceMsg where
  prettyForTrace = \case
    TraceBindingSpec   x -> prettyForTrace x
    TraceClang         x -> prettyForTrace x
    TraceFrontend      x -> prettyForTrace x
    TraceResolveHeader x -> prettyForTrace x

instance HasDefaultLogLevel TraceMsg where
  getDefaultLogLevel = \case
    TraceBindingSpec   x -> getDefaultLogLevel x
    TraceClang         x -> getDefaultLogLevel x
    TraceFrontend      x -> getDefaultLogLevel x
    TraceResolveHeader x -> getDefaultLogLevel x

instance HasSource TraceMsg where
  getSource = \case
    TraceBindingSpec   x -> getSource x
    TraceClang         x -> getSource x
    TraceFrontend      x -> getSource x
    TraceResolveHeader x -> getSource x

{-------------------------------------------------------------------------------
  Log level customization
-------------------------------------------------------------------------------}

-- | Get a custom log level function from a list of available settings.
--
-- NOTE: The order of settings matters. The first setting specifying a custom
-- log level for the emitted trace overrules later settings.
customLogLevelFrom :: [CustomLogLevelSetting] -> CustomLogLevel TraceMsg
customLogLevelFrom = mconcat . map fromCustomLogLevelSetting

-- | List of predefined log level customization settings.
data CustomLogLevelSetting =
    -- | Change macro-related parsing traces to 'Warning's. By default, traces
    -- emitted while parsing macros are 'Info' messages.
    MacroTracesAreWarnings
  deriving stock (Eq, Show)

fromCustomLogLevelSetting :: CustomLogLevelSetting -> CustomLogLevel TraceMsg
fromCustomLogLevelSetting MacroTracesAreWarnings = macroTracesAreWarnings
  where
    macroTracesAreWarnings :: CustomLogLevel TraceMsg
    macroTracesAreWarnings = CustomLogLevel $ \case
        TraceFrontend (FrontendHandleMacros (HandleMacrosErrorReparse{})) -> Just Warning
        TraceFrontend (FrontendHandleMacros (HandleMacrosErrorTc{}))      -> Just Warning
        _otherTrace -> Nothing
