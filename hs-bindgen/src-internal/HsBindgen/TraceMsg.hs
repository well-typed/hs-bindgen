-- | Top-level sum-type that captures all trace messages we might emit
--
-- Intended for unqualified import.
module HsBindgen.TraceMsg (
    TraceMsg(..)
  ) where

import HsBindgen.BindingSpec (BindingSpecMsg)
import HsBindgen.Clang (ClangMsg)
import HsBindgen.Frontend (FrontendMsg)
import HsBindgen.Resolve (ResolveHeaderMsg)
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
  prettyTrace = \case
    TraceBindingSpec   x -> prettyTrace x
    TraceClang         x -> prettyTrace x
    TraceFrontend      x -> prettyTrace x
    TraceResolveHeader x -> prettyTrace x

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
