module HsBindgen.Common (
    -- * Configuration

    -- ** Boot
    Config.BootConfig(..)
    -- *** Binding specifications
  , BindingSpec.BindingSpecConfig(..)
  , BindingSpec.EnableStdlibBindingSpec(..)

    -- ** Frontend
  , Config.FrontendConfig(..)
    -- *** Clang arguments
  , Args.ClangArgs(..)
  , Args.Target(..)
  , Args.TargetEnv(..)
  , Args.targetTriple
  , Args.CStandard(..)
    -- *** Predicates
  , Predicate.Predicate(..)
  , Predicate.HeaderPathPredicate(..)
  , Predicate.DeclPredicate (..)
  , Predicate.ParsePredicate
  , Predicate.SelectPredicate
  , Predicate.Regex -- opaque
  , Predicate.mergePredicates
    -- *** Program slicing
  , Select.ProgramSlicing(..)

    -- ** Backend
  , Config.BackendConfig(..)
    -- *** Translation options
  , Hs.UniqueId(..)
  , Hs.TranslationOpts(..)
  , Hs.Strategy(..)
  , Hs.HsTypeClass(..)
  , HsModule.HsModuleOpts(..)

    -- * Paths
  , RootHeader.HashIncludeArg(..)
  , RootHeader.hashIncludeArg
  , RootHeader.UncheckedHashIncludeArg
  , RootHeader.hashIncludeArgWithTrace
  , Paths.CIncludeDir(..)
  , (FilePath.</>)
  , FilePath.joinPath

    -- * Logging
  , TraceMsg.TraceMsg(..)
  , TraceMsg.BindingSpecMsg(..)
  , TraceMsg.BootMsg(..)
  , TraceMsg.ClangMsg(..)
  , TraceMsg.DeclIndexError(..)
  , TraceMsg.Diagnostic(..)
  , TraceMsg.FrontendMsg(..)
  , TraceMsg.HandleMacrosMsg(..)
  , TraceMsg.HandleTypedefsMsg(..)
  , TraceMsg.HashIncludeArgMsg(..)
  , TraceMsg.MangleNamesMsg(..)
  , TraceMsg.NameAnonMsg(..)
  , TraceMsg.ParseMsg(..)
  , TraceMsg.ParseTypeException(..)
  , TraceMsg.ReparseError(..)
  , TraceMsg.ResolveBindingSpecMsg(..)
  , TraceMsg.ResolveHeaderMsg(..)
  , TraceMsg.SelectMsg(..)
  , TraceMsg.SortMsg(..)
  , TraceMsg.TcMacroError(..)
    -- ** Tracer definition and main API
  , Tracer.Tracer -- opaque
  , Tracer.Contravariant(..)
  , Tracer.traceWith
  , Tracer.simpleTracer
    -- ** Data types and typeclasses useful for tracing
  , Tracer.PrettyForTrace(..)
  , Tracer.Level(..)
  , Tracer.SafeLevel(..)
  , Tracer.Source(..)
  , Tracer.TraceId (..)
  , Tracer.IsTrace(..)
  , Tracer.Verbosity(..)
    -- ** Tracer configuration
  , Tracer.ShowTimeStamp(..)
  , Tracer.ShowCallStack(..)
  , Tracer.TracerConfig(..)
    -- *** Custom output
  , Tracer.AnsiColor(..)
  , Tracer.Report
  , Tracer.OutputConfig(..)
    -- *** Custom log levels
  , Tracer.CustomLogLevel(..)
  , TraceMsg.CustomLogLevelSetting(..)
  , TraceMsg.getCustomLogLevel
    -- ** Tracers
  , Tracer.withTracer

    -- * Re-exports
  , Default(..)
  ) where

import System.FilePath qualified as FilePath

import Clang.Args qualified as Args
import Clang.Paths qualified as Paths

import HsBindgen.Backend.Artefact.HsModule.Translation qualified as HsModule
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Translation qualified as Hs
import HsBindgen.Backend.UniqueId qualified as Hs
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config qualified as Config
import HsBindgen.Frontend.Pass.Select.IsPass qualified as Select
import HsBindgen.Frontend.Predicate qualified as Predicate
import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.TraceMsg qualified as TraceMsg
import HsBindgen.Util.Tracer qualified as Tracer

import HsBindgen.Imports (Default (..))
