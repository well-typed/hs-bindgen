module HsBindgen.Util.Trace (
    Trace (..)
  ) where

import Clang.HighLevel.Types (Diagnostic, diagnosticIsError)

import Control.Exception (Exception (displayException))
import HsBindgen.C.Fold.Common (Skipped)
import HsBindgen.Resolve (ResolveHeaderException)
import HsBindgen.Util.Tracer (HasLogLevel (getLogLevel), HasSource (getSource),
                              Level (..), PrettyTrace (prettyTrace))

-- | Traces supported by @hs-bindgen@.
--
-- Lazy on purpose to avoid evaluation when traces are not reported.
data Trace = TraceDiagnostic Diagnostic
           | TraceResolveHeader ResolveHeaderException
           | TraceSkipped Skipped

instance PrettyTrace Trace where
  prettyTrace = \case
    TraceDiagnostic x    -> show x
    TraceResolveHeader x -> displayException x
    TraceSkipped x       -> prettyTrace x

instance HasLogLevel Trace where
  getLogLevel = \case
    -- We must evluate the diagnostic here to determine if it is an error.
    TraceDiagnostic x | diagnosticIsError x -> Error
    TraceDiagnostic _                       -> Warning
    TraceResolveHeader x                    -> getLogLevel x
    TraceSkipped x                          -> getLogLevel x

instance HasSource Trace where
  getSource = \case
    TraceDiagnostic _    -> "libclang"
    TraceResolveHeader x -> getSource x
    TraceSkipped x       -> getSource x
