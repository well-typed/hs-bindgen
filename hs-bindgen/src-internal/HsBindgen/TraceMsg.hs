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
  , ParseMsg(..)
  , SortMsg(..)
  , SliceMsg(..)
  , HandleMacrosMsg(..)
  , NameAnonMsg(..)
  , ResolveBindingSpecMsg(..)
  , HandleTypedefsMsg(..)
  , MangleNamesMsg(..)
  , ParseTypeException(..)
  , ReparseError(..)
  , ResolveHeaderMsg(..)
  , TcMacroError(..)
  ) where

import Clang.HighLevel.Types (Diagnostic (..))
import HsBindgen.BindingSpec (BindingSpecMsg (..))
import HsBindgen.C.Reparse.Infra (ReparseError (..))
import HsBindgen.C.Tc.Macro (TcMacroError (..))
import HsBindgen.Clang (ClangMsg (..))
import HsBindgen.Frontend (FrontendMsg (..))
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndexError (..))
import HsBindgen.Frontend.Pass.HandleMacros.IsPass (HandleMacrosMsg(..))
import HsBindgen.Frontend.Pass.HandleTypedefs.IsPass (HandleTypedefsMsg(..))
import HsBindgen.Frontend.Pass.MangleNames.IsPass (MangleNamesMsg(..))
import HsBindgen.Frontend.Pass.NameAnon.IsPass (NameAnonMsg(..))
import HsBindgen.Frontend.Pass.Parse.IsPass (ParseMsg(..))
import HsBindgen.Frontend.Pass.Parse.Type.Monad (ParseTypeException (..))
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass
  (ResolveBindingSpecMsg(..))
import HsBindgen.Frontend.Pass.Slice.IsPass (SliceMsg(..))
import HsBindgen.Frontend.Pass.Sort.IsPass (SortMsg(..))
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
  | TraceClang ClangMsg
  | TraceFrontend FrontendMsg
  | TraceResolveHeader ResolveHeaderMsg
  deriving stock (Show, Eq)

instance PrettyForTrace TraceMsg where
  prettyForTrace = \case
    TraceBindingSpec   x -> prettyForTrace x
    TraceClang         x -> prettyForTrace x
    TraceFrontend      x -> prettyForTrace x
    TraceResolveHeader x -> prettyForTrace x

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
