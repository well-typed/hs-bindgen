{-# LANGUAGE CPP #-}

-- | Main entry point for using @hs-bindgen@ in TH mode.
--
-- Intended for unqualified import.

-- NOTE: Client code should /NOT/ have to import from @clang@.
module HsBindgen.TH (
    -- * Template Haskell API
    Pipeline.hashInclude'
  , Pipeline.hashInclude
  , Pipeline.hashIncludeWith

    -- * Options
  , Common.Config(..)
  , Pipeline.QuoteIncludePathDir(..)
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
  , loadExtBindingSpecs

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
  , Common.CIncludePathDir(..)
  , (Common.</>)
  , Common.joinPath
  , THSyntax.getPackageRoot

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

import Language.Haskell.TH qualified as TH

import HsBindgen.Common qualified as Common

import Clang.Args qualified as Args
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Pipeline qualified as Pipeline
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer
import HsBindgen.Util.Tracer qualified as Tracer

#ifdef MIN_VERSION_th_compat
import Language.Haskell.TH.Syntax.Compat qualified as THSyntax
#else
import Language.Haskell.TH.Syntax qualified as THSyntax
#endif

{-------------------------------------------------------------------------------
  Binding specifications
-------------------------------------------------------------------------------}

-- | Load external binding specifications
--
-- The format is determined by filename extension.  The following formats are
-- supported:
--
-- * YAML (@.yaml@ extension)
-- * JSON (@.json@ extension)
loadExtBindingSpecs ::
     Tracer TH.Q TraceMsg
  -> Args.ClangArgs
  -> Common.EnableStdlibBindingSpec
  -> [FilePath]
  -> TH.Q Common.BindingSpec
loadExtBindingSpecs tracer args stdlibConf =
    TH.runIO . BindingSpec.loadExtBindingSpecs tracer' args stdlibConf
  where
    tracer' :: Tracer IO BindingSpec.BindingSpecMsg
    tracer' = natTracer TH.runQ $ contramap TraceBindingSpec tracer
