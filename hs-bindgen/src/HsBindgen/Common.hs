module HsBindgen.Common (

    -- * Options
    Pipeline.Opts(..)

    -- * Binding specifications
  , Pipeline.BindingSpec -- opaque
  , Pipeline.StdlibBindingSpecConf(..)
  , Pipeline.emptyBindingSpec

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
  , Resolve.ResolveHeaderMsg(..)
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
  , Tracer.AnsiColor(..)
  , Tracer.ShowTimeStamp(..)
  , Tracer.ShowCallStack(..)
  , Tracer.TracerConf(..)
  , Tracer.CustomLogLevel(..)
    -- ** Tracers
  , Tracer.withTracerStdOut
  , Tracer.withTracerCustom

    -- * Re-exports
  , Default(..)
  ) where

import System.FilePath qualified as FilePath

import Clang.Args qualified as Args
import Clang.Paths qualified as Paths

import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Frontend.Pass.Slice qualified as Slice
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.Translation qualified as Hs
import HsBindgen.Pipeline qualified as Pipeline
import HsBindgen.Resolve qualified as Resolve
import HsBindgen.TraceMsg qualified as TraceMsg
import HsBindgen.Util.Tracer qualified as Tracer

import HsBindgen.Imports (Default (..))
