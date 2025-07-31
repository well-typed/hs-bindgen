{-# LANGUAGE CPP #-}

-- | Main entry point for using @hs-bindgen@ in TH mode.
--
-- Intended for unqualified import.

-- NOTE: Client code should /NOT/ have to import from @clang@.
module HsBindgen.TH (
    -- * Template Haskell API
    Pipeline.withHsBindgen
  , Pipeline.hashInclude

    -- * Options
  , Common.Config(..)
  , Pipeline.IncludeDir(..)
  , Pipeline.HashIncludeOpts(..)

    -- ** Clang arguments
  , Common.ClangArgs(..)
  , Common.Target(..)
  , Common.TargetEnv(..)
  , Common.targetTriple
  , Common.CStandard(..)

    -- ** Binding specifications
  , Common.BindingSpec -- opaque
  , Common.emptyBindingSpec
  , Common.EnableStdlibBindingSpec(..)
  , Common.BindingSpecConfig(..)

    -- ** Translation options
  , Common.TranslationOpts(..)
  , Common.Strategy(..)
  , Common.HsTypeClass(..)

    -- ** Selection predicates
  , Common.Predicate(..)
  , Common.HeaderPathPredicate (..)
  , Common.DeclPredicate (..)
  , Common.ParsePredicate
  , Common.SelectPredicate
  , Common.Regex -- opaque
  , Common.mergePredicates

    -- ** Program slicing
  , Common.ProgramSlicing(..)

    -- * Paths
  , Common.HashIncludeArg(..)
  -- TODO: https://github.com/well-typed/hs-bindgen/issues/958. Will be removed
  -- (instead we emit a warning trace).
  , Common.parseHashIncludeArg
  , Common.CIncludeDir(..)
  , (Common.</>)
  , Common.joinPath

    -- * Logging
  , Common.TraceMsg(..)
  , Common.BindingSpecMsg(..)
  , Common.ClangMsg(..)
  , Common.DeclIndexError(..)
  , Common.Diagnostic(..)
  , Common.FrontendMsg(..)
  , Common.ParseMsg(..)
  , Common.HandleMacrosMsg(..)
  , Common.NameAnonMsg(..)
  , Common.ResolveBindingSpecMsg(..)
  , Common.SelectMsg(..)
  , Common.HandleTypedefsMsg(..)
  , Common.MangleNamesMsg(..)
  , Common.ParseTypeException(..)
  , Common.ReparseError(..)
  , Common.ResolveHeaderMsg(..)
  , Common.TcMacroError(..)
    -- ** Tracer definition and main API
  , Common.Tracer -- opaque
  , Common.Contravariant(..)
  , Common.traceWith
  , Common.simpleTracer
    -- ** Data types and typeclasses useful for tracing
  , Common.Level(..)
  , Common.PrettyForTrace(..)
  , Common.HasDefaultLogLevel(..)
  , Common.Source(..)
  , Common.HasSource(..)
  , Common.Verbosity(..)
    -- ** Tracer configuration
  , Common.ShowTimeStamp(..)
  , Common.ShowCallStack(..)
  , Common.TracerConfig(..)
    -- *** Custom output
  , Common.AnsiColor(..)
  , Common.Report
  , Common.OutputConfig(..)
  , Tracer.outputConfigQ
    -- *** Custom log levels
  , Common.CustomLogLevel(..)
  , Common.customLogLevelFrom
  , Common.CustomLogLevelSetting(..)
    -- ** Tracers
  , Common.withTracerCustom

   -- * Re-exports
  , Common.Default(..)
  ) where

import HsBindgen.Common qualified as Common

import HsBindgen.Pipeline qualified as Pipeline
import HsBindgen.Util.Tracer qualified as Tracer
