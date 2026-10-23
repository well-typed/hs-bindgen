module HsBindgen.Macro.Error (
    MacroParseError(..)
  , prettyMacroParseErrorWithHeading
  , MacroResolutionError(..)
  , MacroTypecheckError(..)
  , MacroLangFailure(..)
  ) where

import GHC.Generics
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Util.Tracer

-- | A macro parse error.
data MacroParseError = MacroParseError {
      -- | Human-readable description of the failure.
      macroParseError       :: String
    }
  deriving stock (Eq, Show, Generic)

instance PrettyForTrace MacroParseError where
  prettyForTrace = prettyMacroParseErrorWithHeading "Could not parse macro:"

-- | Render a macro parse error below the given heading
prettyMacroParseErrorWithHeading :: PP.CtxDoc -> MacroParseError -> PP.CtxDoc
prettyMacroParseErrorWithHeading heading err = PP.vcat [
      heading
    , PP.nest 2 $
        PP.renderedLines $ \_maxWidth -> lines err.macroParseError
    ]

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

-- | The macro-language stage that gave up on a macro, with its error
data MacroLangFailure =
    FailedParse     MacroParseError
  | FailedResolve   MacroResolutionError
  | FailedTypecheck MacroTypecheckError
  deriving stock (Eq, Show, Generic)

instance PrettyForTrace MacroLangFailure where
  prettyForTrace = \case
      FailedParse     err -> prettyForTrace err
      FailedResolve   err -> prettyForTrace err
      FailedTypecheck err -> prettyForTrace err

instance IsTrace Level MacroLangFailure where
  getDefaultLogLevel = \case
      FailedParse     _   -> Info
      FailedResolve   err -> getDefaultLogLevel err
      FailedTypecheck err -> getDefaultLogLevel err
  getSource  = const HsBindgen
  getTraceId = \case
      FailedParse     _   -> "parse-macro"
      FailedResolve   err -> getTraceId err
      FailedTypecheck err -> getTraceId err
