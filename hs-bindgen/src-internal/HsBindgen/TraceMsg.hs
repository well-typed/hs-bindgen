-- | Top-level sum-type that captures all trace messages we might emit
--
-- Intended for unqualified import.
module HsBindgen.TraceMsg (
    TraceMsg(..)
    -- * Messages from individual passes
  , BindingSpecMsg(..)
  , BootMsg(..)
  , ClangMsg(..)
  , DeclIndexError(..)
  , Diagnostic(..)
  , FrontendMsg(..)
  , HandleMacrosMsg(..)
  , HandleTypedefsMsg(..)
  , HashIncludeArgMsg(..)
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
  , CustomLogLevelSetting (..)
  , getCustomLogLevel
  ) where

import Clang.HighLevel.Types (Diagnostic (..))
import HsBindgen.BindingSpec (BindingSpecMsg (..))
import HsBindgen.Boot
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
import HsBindgen.Frontend.RootHeader (HashIncludeArgMsg (..), getHashIncludeArg)
import HsBindgen.Imports
import HsBindgen.Resolve (ResolveHeaderMsg (..))
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  HsBindgen traces
-------------------------------------------------------------------------------}

-- | Traces supported by @hs-bindgen@.
--
-- Lazy on purpose to avoid evaluation when traces are not reported.
--
-- Does not include backend messages because, unlike 'TraceMsg', backend
-- messages cannot include 'Error's, or 'Warning's.
data TraceMsg =
    TraceBoot          BootMsg
  | TraceFrontend      FrontendMsg
  | TraceResolveHeader ResolveHeaderMsg
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)

{-------------------------------------------------------------------------------
  Log level customization
-------------------------------------------------------------------------------}

-- | List of predefined log level customization settings.
data CustomLogLevelSetting =
    -- * Generic setters
    MakeTrace Level TraceId

    -- * Specific setters
    -- | The header `uchar.h` is not available on MacOS. Set the log level to
    -- 'Info'.
  | MakeUCharResolutionTraceInfo
    -- | Set the log level of macro-related parsing traces to 'Warning'. By
    -- default, traces emitted while parsing macros have log level 'Info'.
  | MakeMacroTracesWarnings

    -- * Generic modifiers
    -- | Modify traces with log level 'Warning' to be fatal 'Error's.
  | MakeWarningsErrors
  deriving stock (Eq, Show, Ord)

-- | Get a custom log level function from a set of available settings.
--
-- NOTE: The order of custom log level settings is important because different
-- custom log level settings may affect the same trace. For example, assume a
-- trace @Trace@ has default log level 'Info', and two custom log level settings
-- @SetTraceWarning@ and @SetTraceError@ set the default log level of @T@ to
-- 'Warning' and 'Error', respectively. Then, if we apply @SetTraceWarning@
-- before @SetTraceError@, the the final log level of @T@ will be 'Error'.
getCustomLogLevel :: [CustomLogLevelSetting] -> CustomLogLevel Level TraceMsg
getCustomLogLevel = mconcat . map fromSetting

-- Internal.
fromSetting ::
     CustomLogLevelSetting
  -> CustomLogLevel Level TraceMsg
fromSetting = \case
    -- Generic setters.
    MakeTrace level traceId      -> makeTrace level traceId
    -- Specific setters.
    MakeMacroTracesWarnings      -> makeMacroTracesWarnings
    MakeUCharResolutionTraceInfo -> makeUCharResolutionTraceInfo
    -- Generic modifiers.
    MakeWarningsErrors           -> makeWarningsErrors
  where
    makeMacroTracesWarnings :: CustomLogLevel Level TraceMsg
    makeMacroTracesWarnings = CustomLogLevel $ \case
        TraceFrontend (FrontendHandleMacros (HandleMacrosErrorReparse{}))
          -> const Warning
        TraceFrontend (FrontendHandleMacros (HandleMacrosErrorTc{}))
          -> const Warning
        _otherTrace
          -> id
    makeUCharResolutionTraceInfo :: CustomLogLevel Level TraceMsg
    makeUCharResolutionTraceInfo = CustomLogLevel $ \case
        TraceResolveHeader (ResolveHeaderNotFound h)
          | getHashIncludeArg h == "uchar.h" -> const Info
        _otherTrace                          -> id
    makeTrace :: Level -> TraceId -> CustomLogLevel Level TraceMsg
    makeTrace desiredLevel traceId = CustomLogLevel $ \trace actualLevel ->
      if getTraceId trace == traceId
      then desiredLevel
      else actualLevel
    makeWarningsErrors :: CustomLogLevel Level TraceMsg
    makeWarningsErrors = CustomLogLevel $ \_ lvl ->
      if lvl == Warning
      then Error
      else lvl
