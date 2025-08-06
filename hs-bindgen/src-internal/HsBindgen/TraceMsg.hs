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
  , HandleMacrosMsg(..)
  , HandleTypedefsMsg(..)
  , MangleNamesMsg(..)
  , NameAnonMsg(..)
  , ParseMsg(..)
  , ParseTypeException(..)
  , ReparseError(..)
  , ResolveBindingSpecMsg(..)
  , ResolveHeaderMsg(..)
  , SelectMsg(..)
  , SortMsg(..)
  , TcMacroError(..)
  -- * Log level customization
  , customLogLevelFrom
  , CustomLogLevelSetting (..)
  ) where

import GHC.Generics (Generic)

import Clang.HighLevel.Types (Diagnostic (..))
import HsBindgen.BindingSpec (BindingSpecMsg (..))
import HsBindgen.Clang (ClangMsg (..))
import HsBindgen.Frontend (FrontendMsg (..))
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndexError (..))
import HsBindgen.Frontend.Macro.Reparse.Infra (ReparseError (..))
import HsBindgen.Frontend.Macro.Tc (TcMacroError (..))
import HsBindgen.Frontend.Pass.HandleMacros.IsPass (HandleMacrosMsg (..))
import HsBindgen.Frontend.Pass.HandleTypedefs.IsPass (HandleTypedefsMsg (..))
import HsBindgen.Frontend.Pass.MangleNames.IsPass (MangleNamesMsg (..))
import HsBindgen.Frontend.Pass.NameAnon.IsPass (NameAnonMsg (..))
import HsBindgen.Frontend.Pass.Parse.IsPass (ParseMsg (..))
import HsBindgen.Frontend.Pass.Parse.Type.Monad (ParseTypeException (..))
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass (ResolveBindingSpecMsg (..))
import HsBindgen.Frontend.Pass.Select.IsPass (SelectMsg (..))
import HsBindgen.Frontend.Pass.Sort.IsPass (SortMsg (..))
import HsBindgen.Frontend.RootHeader (HashIncludeArgMsg, getHashIncludeArg)
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
  | TraceFrontend FrontendMsg
  | TraceResolveHeader ResolveHeaderMsg
  | TraceHashIncludeArg HashIncludeArgMsg
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (PrettyForTrace, HasDefaultLogLevel, HasSource)

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
    -- | The header `uchar.h` is not available on MacOS.
  | UCharHeaderResolutionTraceIsInfo
  deriving stock (Eq, Show)

fromCustomLogLevelSetting :: CustomLogLevelSetting -> CustomLogLevel TraceMsg
fromCustomLogLevelSetting = \case
  MacroTracesAreWarnings     -> macroTracesAreWarnings
  UCharHeaderResolutionTraceIsInfo -> uCharResolutionTraceIsInfo
  where
    macroTracesAreWarnings :: CustomLogLevel TraceMsg
    macroTracesAreWarnings = CustomLogLevel $ \case
        TraceFrontend (FrontendHandleMacros (HandleMacrosErrorReparse{})) -> Just Warning
        TraceFrontend (FrontendHandleMacros (HandleMacrosErrorTc{}))      -> Just Warning
        _otherTrace -> Nothing
    uCharResolutionTraceIsInfo :: CustomLogLevel TraceMsg
    uCharResolutionTraceIsInfo = CustomLogLevel $ \case
        TraceResolveHeader (ResolveHeaderNotFound h)
          | getHashIncludeArg h == "uchar.h" -> Just Info
        _otherTrace -> Nothing
