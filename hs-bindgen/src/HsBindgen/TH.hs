{-# LANGUAGE CPP #-}

-- | Main entry point for using @hs-bindgen@ in TH mode.
--
-- Intended for unqualified import.

-- NOTE: Client code should /NOT/ have to import from @clang@.

-- TODO https://github.com/well-typed/hs-bindgen/issues/1045: Expand interface
-- to meet needs; delete other comments; check haddocks.

module HsBindgen.TH (
    -- * Template Haskell API
    TH.withHsBindgen
  , TH.hashInclude

    -- * Configuration
  , TH.IncludeDir(..)
  , TH.BindgenOpts(..)
  -- , TH.tracerConfigDefTH

  --   -- ** Boot
  -- , Common.BootConfig(..)
  --   -- *** Builtin include directory
  -- , Common.BuiltinIncDirConfig(..)
  --   -- *** Binding specifications
  -- , Common.BindingSpecConfig(..)
  -- , Common.EnableStdlibBindingSpec(..)

  --   -- ** Frontend
  -- , Common.FrontendConfig(..)
  --   -- *** Clang arguments
  -- , Common.ClangArgs(..)
  -- , Common.Target(..)
  -- , Common.TargetEnv(..)
  -- , Common.targetTriple
  -- , Common.CStandard(..)
  --   -- *** Predicates
  -- , Common.Predicate(..)
  -- , Common.HeaderPathPredicate (..)
  -- , Common.DeclPredicate (..)
  -- , Common.ParsePredicate
  -- , Common.SelectPredicate
  -- , Common.Regex -- opaque
  -- , Common.mergePredicates
  --   -- *** Program slicing
  -- , Common.ProgramSlicing(..)

  --   -- ** Backend
  -- , Common.BackendConfig(..)
  --   -- *** Translation options
  -- , Common.UniqueId(..)
  -- , Common.TranslationOpts(..)
  -- , Common.Strategy(..)
  -- , Common.HsTypeClass(..)
  -- , Common.HsModuleOpts(..)

  --   -- * Paths
  -- , Common.HashIncludeArg(..)
  -- , Common.hashIncludeArg
  -- , Common.UncheckedHashIncludeArg
  -- , Common.hashIncludeArgWithTrace
  -- , Common.CIncludeDir(..)
  -- , (Common.</>)
  -- , Common.joinPath

  --   -- * Logging
  -- , Common.TraceMsg(..)
  -- , Common.BindingSpecMsg(..)
  -- , Common.BootMsg(..)
  -- , Common.ClangMsg(..)
  -- , Common.DeclIndexError(..)
  -- , Common.Diagnostic(..)
  -- , Common.FrontendMsg(..)
  -- , Common.HandleMacrosMsg(..)
  -- , Common.HandleTypedefsMsg(..)
  -- , Common.HashIncludeArgMsg(..)
  -- , Common.MangleNamesMsg(..)
  -- , Common.NameAnonMsg(..)
  -- , Common.ParseMsg(..)
  -- , Common.ParseTypeException(..)
  -- , Common.ReparseError(..)
  -- , Common.ResolveBindingSpecMsg(..)
  -- , Common.ResolveHeaderMsg(..)
  -- , Common.SelectMsg(..)
  -- , Common.SortMsg(..)
  -- , Common.TcMacroError(..)
  --   -- ** Tracer definition and main API
  -- , Common.Tracer -- opaque
  -- , Common.traceWith
  -- , Common.simpleTracer
  --   -- ** Data types and typeclasses useful for tracing
  -- , Common.PrettyForTrace(..)
  -- , Common.Level(..)
  -- , Common.SafeLevel(..)
  -- , Common.Source(..)
  -- , Common.TraceId(..)
  -- , Common.IsTrace(..)
  -- , Common.Verbosity(..)
  --   -- ** Tracer configuration
  -- , Common.ShowTimeStamp(..)
  -- , Common.ShowCallStack(..)
  -- , Common.TracerConfig(..)
  --   -- *** Custom output
  -- , Common.AnsiColor(..)
  -- , Common.Report
  -- , Common.OutputConfig(..)
  --   -- *** Custom log levels
  -- , Common.CustomLogLevel(..)
  -- , Common.CustomLogLevelSetting(..)
  -- , Common.getCustomLogLevel
  --   -- ** Tracers
  -- , Common.withTracer

   -- * Re-exports
  -- , Common.Contravariant(..)
  , Common.Default(..)
  ) where

import HsBindgen.Common qualified as Common

import HsBindgen.TH.Internal qualified as TH
