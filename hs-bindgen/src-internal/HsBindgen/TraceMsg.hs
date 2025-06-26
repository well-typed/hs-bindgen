-- | Top-level sum-type that captures all trace messages we might emit
--
-- Intended for unqualified import.
module HsBindgen.TraceMsg (
    TraceMsg(..)
  ) where

import HsBindgen.Clang (ClangMsg(..))
import HsBindgen.Frontend (FrontendMsg(..))
import HsBindgen.Resolve (ResolveHeaderMsg(..))
import HsBindgen.Util.Tracer
import HsBindgen.BindingSpec (ResolveBindingSpecMsg)

{-------------------------------------------------------------------------------
  HsBindgen traces
-------------------------------------------------------------------------------}

-- | Traces supported by @hs-bindgen@.
--
-- Lazy on purpose to avoid evaluation when traces are not reported.
data TraceMsg =
    TraceClang ClangMsg
  | TraceResolveBindingSpec ResolveBindingSpecMsg
  | TraceFrontend FrontendMsg
  | TraceResolveHeader ResolveHeaderMsg
  deriving stock (Show, Eq)

instance PrettyForTrace TraceMsg where
  prettyTrace = \case
    TraceClang              x -> prettyTrace x
    TraceResolveBindingSpec x -> prettyTrace x
    TraceFrontend           x -> prettyTrace x
    TraceResolveHeader      x -> prettyTrace x

instance HasDefaultLogLevel TraceMsg where
  getDefaultLogLevel = \case
    TraceClang              x -> getDefaultLogLevel x
    TraceResolveBindingSpec x -> getDefaultLogLevel x
    TraceFrontend           x -> getDefaultLogLevel x
    TraceResolveHeader      x -> getDefaultLogLevel x

instance HasSource TraceMsg where
  getSource = \case
    TraceClang              x -> getSource x
    TraceResolveBindingSpec x -> getSource x
    TraceFrontend           x -> getSource x
    TraceResolveHeader      x -> getSource x
