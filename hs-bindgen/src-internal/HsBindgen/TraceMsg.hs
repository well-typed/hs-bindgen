-- | Top-level sum-type that captures all trace messages we might emit
--
-- Intended for unqualified import.
module HsBindgen.TraceMsg (
    TraceMsg(..)
    -- * Messages from individual passes
  , BindingSpecMsg(..)
  , BootMsg(..)
  , ClangMsg(..)
  , Diagnostic(..)
  , FrontendMsg(..)
  , HashIncludeArgMsg(..)
  , MangleNamesMsg(..)
  , ImmediateAssignAnonIdsMsg (..)
  , ImmediateParseMsg(..)
  , DelayedParseMsg(..)
  , CExpr.DSL.MacroParseError(..)
  , CExpr.DSL.MacroTcError(..)
  , ResolveBindingSpecsMsg(..)
  , ResolveHeaderMsg(..)
  , SelectMsg(..)
  -- * Log level customization
  , CustomLogLevelSetting (..)
  , getCustomLogLevel
  ) where

import Data.List qualified as List

import C.Expr.Parse qualified as CExpr.DSL
import C.Expr.Typecheck qualified as CExpr.DSL

import Clang.HighLevel.Types (Diagnostic (..))

import HsBindgen.BindingSpec (BindingSpecMsg (..))
import HsBindgen.Boot
import HsBindgen.Clang (ClangMsg (..))
import HsBindgen.Clang.BuiltinIncDir (BuiltinIncDirMsg (..))
import HsBindgen.Frontend (FrontendMsg (..))
import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass (ImmediateAssignAnonIdsMsg (..))
import HsBindgen.Frontend.Pass.MangleNames.IsPass (MangleNamesMsg (..))
import HsBindgen.Frontend.Pass.Parse.Msg (DelayedParseMsg (..),
                                          ImmediateParseMsg (..))
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass (ResolveBindingSpecsMsg (..))
import HsBindgen.Frontend.Pass.Select.IsPass (SelectMsg (..))
import HsBindgen.Frontend.RootHeader (HashIncludeArgMsg (..))
import HsBindgen.Imports
import HsBindgen.Resolve (ResolveHeaderMsg (..))
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  HsBindgen boot/frontend traces
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
  | TraceBuiltinIncDir BuiltinIncDirMsg
  | TraceResolveHeader ResolveHeaderMsg
  deriving stock    (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)

{-------------------------------------------------------------------------------
  Log level customization
-------------------------------------------------------------------------------}

-- | List of predefined log level customization settings.
data CustomLogLevelSetting =
    -- * Generic setters
    MakeTrace Level TraceId

    -- * Generic modifiers
    -- | Modify traces with log level 'Warning' to be fatal 'Error's.
  | MakeWarningsErrors

    -- | Modify traces with log level Bug' to be fatal 'Error's.
  | MakeBugsErrors

    -- * Specific setters
    -- | Set the log level of macro-related parsing traces to 'Warning'.
    --
    -- Reparse and typechecking errors may indicate that something went
    -- wrong, or they may be caused by macro syntax that we do not yet
    -- support.
    --
    -- By default, traces emitted while parsing macros have log level 'Info'.
    -- because there are many unsupported macros in standard library
    -- implementations. Using this custom log level setting, users make them
    -- 'Warning' instead.
  | EnableMacroWarnings
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
    MakeTrace level traceId -> makeTrace level traceId
    -- Generic modifiers.
    MakeWarningsErrors      -> makeLevelErrors Warning
    -- Generic modifiers.
    MakeBugsErrors          -> makeLevelErrors Bug
    -- Specific setters.
    EnableMacroWarnings     -> enableMacroWarnings
  where
    makeTrace :: Level -> TraceId -> CustomLogLevel Level TraceMsg
    makeTrace desiredLevel traceId = CustomLogLevel $ \trace actualLevel ->
      if getTraceId trace == traceId
      then desiredLevel
      else actualLevel

    enableMacroWarnings :: CustomLogLevel Level TraceMsg
    enableMacroWarnings = CustomLogLevel $ \case
        -- Numerous locations may emit macro-related trace messages (e.g.,
        -- 'Parse', 'TypecheckMacros', 'ReparseMacroExpansions', or 'Select'
        -- passes), so we fall back to string search.
        t | "macro" `List.isInfixOf` (getTraceId t).id
          -> const Warning
        -- Macro parsing requires declarations required for scoping.
        TraceFrontend (FrontendParse WithLocationInfo{msg = ParseOfDeclarationRequiredForScopingFailed{}})
          -> const Warning
        _otherTrace
          -> id

    makeLevelErrors :: Level -> CustomLogLevel Level TraceMsg
    makeLevelErrors lvlError = CustomLogLevel $ \_ lvl ->
      if lvl >= lvlError
      then Error
      else lvl
