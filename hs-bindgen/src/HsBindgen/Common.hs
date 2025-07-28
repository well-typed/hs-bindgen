module HsBindgen.Common (

    -- * Options
    Config.Config(..)

    -- * Binding specifications
  , BindingSpec.BindingSpec -- opaque
  , BindingSpec.EnableStdlibBindingSpec(..)
  , BindingSpec.emptyBindingSpec

    -- ** Clang arguments
  , Args.ClangArgs(..)
  , Args.Target(..)
  , Args.TargetEnv(..)
  , Args.targetTriple
  , Args.CStandard(..)

    -- ** Translation options
  , Hs.TranslationOpts(..)
  , Hs.Strategy(..)
  , Hs.HsTypeClass(..)

    -- ** Selection predicates
  , Predicate.Predicate(..)
  , Predicate.Regex -- opaque
  , Predicate.mergePredicates

    -- ** Program slicing
  , Slice.ProgramSlicing(..)

    -- * Paths
  , Paths.CIncludePathDir(..)
  , (FilePath.</>)
  , FilePath.joinPath

    -- * Logging
  , TraceMsg.TraceMsg(..)
  , TraceMsg.BindingSpecMsg(..)
  , TraceMsg.ClangMsg(..)
  , TraceMsg.DeclIndexError(..)
  , TraceMsg.Diagnostic(..)
  , TraceMsg.FrontendMsg(..)
  , TraceMsg.ParseMsg(..)
  , TraceMsg.SortMsg(..)
  , TraceMsg.SliceMsg(..)
  , TraceMsg.HandleMacrosMsg(..)
  , TraceMsg.NameAnonMsg(..)
  , TraceMsg.ResolveBindingSpecMsg(..)
  , TraceMsg.HandleTypedefsMsg(..)
  , TraceMsg.MangleNamesMsg(..)
  , TraceMsg.ParseTypeException(..)
  , TraceMsg.ReparseError(..)
  , TraceMsg.ResolveHeaderMsg(..)
  , TraceMsg.TcMacroError(..)
    -- ** Tracer definition and main API
  , Tracer.Tracer -- opaque
  , Tracer.Contravariant(..)
  , Tracer.traceWith
  , Tracer.simpleTracer
    -- ** Data types and typeclasses useful for tracing
  , Tracer.Level(..)
  , Tracer.PrettyForTrace(..)
  , Tracer.HasDefaultLogLevel(..)
  , Tracer.Source(..)
  , Tracer.HasSource(..)
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
  , TraceMsg.customLogLevelFrom
  , TraceMsg.CustomLogLevelSetting(..)
    -- ** Tracers
  , Tracer.withTracerCustom

    -- * Re-exports
  , Default(..)
  ) where

import System.FilePath qualified as FilePath

import Clang.Args qualified as Args
import Clang.Paths qualified as Paths

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Config qualified as Config
import HsBindgen.Frontend.Pass.Slice.IsPass qualified as Slice
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.Translation qualified as Hs
import HsBindgen.TraceMsg qualified as TraceMsg
import HsBindgen.Util.Tracer qualified as Tracer

import HsBindgen.Imports (Default (..))
