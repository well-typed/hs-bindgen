module HsBindgen.Util.Trace (
    Trace (..)
  ) where

import Control.Exception (Exception (displayException))

import Clang.HighLevel.Types (Diagnostic, diagnosticIsError)
import HsBindgen.Clang.Args (ExtraClangArgsLog)
import HsBindgen.Frontend.Pass.Parse.Monad (ParseLog)
import HsBindgen.Resolve (ResolveHeaderException)
import HsBindgen.Util.Tracer (HasDefaultLogLevel (getDefaultLogLevel),
                              HasSource (getSource), Level (Error, Warning),
                              PrettyTrace (prettyTrace), Source (Libclang))

{-------------------------------------------------------------------------------
  HsBindgen traces
-------------------------------------------------------------------------------}

-- | Traces supported by @hs-bindgen@.
--
-- Lazy on purpose to avoid evaluation when traces are not reported.
data Trace = TraceDiagnostic Diagnostic
           | TraceExtraClangArgs ExtraClangArgsLog
           | TraceParse ParseLog
           | TraceResolveHeader ResolveHeaderException

instance PrettyTrace Trace where
  prettyTrace = \case
    TraceDiagnostic x     -> show x
    TraceExtraClangArgs x -> prettyTrace x
    TraceParse x          -> prettyTrace x
    TraceResolveHeader x  -> displayException x

instance HasDefaultLogLevel Trace where
  getDefaultLogLevel = \case
    -- We must evluate the diagnostic here to determine if it is an error.
    TraceDiagnostic x | diagnosticIsError x -> Error
    TraceDiagnostic _                       -> Warning
    TraceExtraClangArgs x                   -> getDefaultLogLevel x
    TraceParse x                            -> getDefaultLogLevel x
    TraceResolveHeader x                    -> getDefaultLogLevel x

instance HasSource Trace where
  getSource = \case
    TraceDiagnostic _     -> Libclang
    TraceExtraClangArgs x -> getSource x
    TraceParse x          -> getSource x
    TraceResolveHeader x  -> getSource x
