module HsBindgen.Macro.Error (
    MacroParseError(..)
  , MacroResolutionError(..)
  , MacroTypecheckError(..)
  ) where

import GHC.Generics
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Util.Tracer

-- | An opaque parse error from the macro-language backend.
newtype MacroParseError = MacroParseError { macroParseError :: String }
  deriving stock (Eq, Show, Generic)

newtype MacroResolutionError = MacroResolutionError {
      macroResolutionError :: String
    }
  deriving stock (Eq, Show, Generic)

instance PrettyForTrace MacroResolutionError where
  prettyForTrace (MacroResolutionError msg) =
    PP.hang "Could not resolve variables in macro:" 2 (PP.string msg)

instance IsTrace Level MacroResolutionError where
  -- See https://github.com/well-typed/hs-bindgen/issues/2147.
  --
  -- Macro resolution failures can be noisy. Users who want to see them, can
  -- change log-level or increase log-level using
  -- 'HsBindgen.TraceMsg.EnableMacroWarnings'.
  getDefaultLogLevel = const Info
  getSource          = const HsBindgen
  getTraceId         = const "macro-resolution"

-- | An opaque typecheck error from the macro-language backend.
newtype MacroTypecheckError = MacroTypecheckError {
      macroTypecheckError :: String
    }
  deriving stock (Eq, Show, Generic)

instance PrettyForTrace MacroTypecheckError where
  prettyForTrace = \case
      MacroTypecheckError err -> PP.hsep [
          "Failed to typecheck macro:"
        , PP.string err
        ]

instance IsTrace Level MacroTypecheckError where
  getDefaultLogLevel = \case
    MacroTypecheckError{} -> Info
  getSource          = const HsBindgen
  getTraceId         = const "macro-typecheck"
