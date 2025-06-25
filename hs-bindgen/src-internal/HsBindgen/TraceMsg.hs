-- | Top-level sum-type that captures all trace messages we might emit
--
-- Intended for unqualified import.
module HsBindgen.TraceMsg (
    TraceMsg(..)
  ) where

import Control.Exception (Exception (displayException))
import Data.Text (unpack)

import Clang.HighLevel.Types
import HsBindgen.Clang.Args (ExtraClangArgsMsg(..))
import HsBindgen.Frontend (FrontendMsg(..))
import HsBindgen.Resolve (ResolveHeaderException)
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  HsBindgen traces
-------------------------------------------------------------------------------}

-- | Traces supported by @hs-bindgen@.
--
-- Lazy on purpose to avoid evaluation when traces are not reported.
data TraceMsg =
    TraceDiagnostic Diagnostic
  | TraceExtraClangArgs ExtraClangArgsMsg
  | TraceFrontend FrontendMsg
  | TraceResolveHeader ResolveHeaderException
  deriving stock (Show, Eq)

instance PrettyForTrace TraceMsg where
  prettyTrace = \case
    TraceDiagnostic x     -> unpack $
      diagnosticCategoryText x <> ": " <> diagnosticFormatted x
    TraceExtraClangArgs x -> prettyTrace x
    TraceFrontend x       -> prettyTrace x
    TraceResolveHeader x  -> displayException x

instance HasDefaultLogLevel TraceMsg where
  getDefaultLogLevel = \case
    -- We must evluate the diagnostic here to determine if it is an error.
    TraceDiagnostic x | diagnosticIsError x -> Error
    TraceDiagnostic _                       -> Warning
    TraceExtraClangArgs x                   -> getDefaultLogLevel x
    TraceFrontend x                         -> getDefaultLogLevel x
    TraceResolveHeader x                    -> getDefaultLogLevel x

instance HasSource TraceMsg where
  getSource = \case
    TraceDiagnostic _     -> Libclang
    TraceExtraClangArgs x -> getSource x
    TraceFrontend x       -> getSource x
    TraceResolveHeader x  -> getSource x
