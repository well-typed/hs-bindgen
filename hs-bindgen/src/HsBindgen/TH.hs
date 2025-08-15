{-# LANGUAGE CPP #-}

-- | Main entry point for using @hs-bindgen@ in TH mode.
--
-- Intended for unqualified import.

-- NOTE: Client code should /NOT/ have to import from @clang@.
module HsBindgen.TH (
    -- * Template Haskell API
    TH.withHsBindgen
  , TH.hashInclude

    -- * Options
  , Common.Config(..)
  , TH.IncludeDir(..)
  , TH.BindgenOpts(..)
  , TH.tracerConfigDefQ

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
  , Common.UniqueId(..)
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
  , Common.hashIncludeArg
  , Common.UncheckedHashIncludeArg
  , Common.hashIncludeArgWithTrace
  , Common.CIncludeDir(..)
  , (Common.</>)
  , Common.joinPath

    -- * Logging
  , Common.TraceMsg(..)
  , Common.BindingSpecMsg(..)
  , Common.BootMsg(..)
  , Common.ClangMsg(..)
  , Common.DeclIndexError(..)
  , Common.Diagnostic(..)
  , Common.FrontendMsg(..)
  , Common.HandleMacrosMsg(..)
  , Common.HandleTypedefsMsg(..)
  , Common.HashIncludeArgMsg(..)
  , Common.MangleNamesMsg(..)
  , Common.NameAnonMsg(..)
  , Common.ParseMsg(..)
  , Common.ParseTypeException(..)
  , Common.ReparseError(..)
  , Common.ResolveBindingSpecMsg(..)
  , Common.ResolveHeaderMsg(..)
  , Common.SelectMsg(..)
  , Common.SortMsg(..)
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
  , Common.withTracer

   -- * Re-exports
  , Common.Default(..)
  ) where

import HsBindgen.Common qualified as Common

import HsBindgen.TH.Internal qualified as TH

import HsBindgen.Util.Tracer qualified as Tracer
