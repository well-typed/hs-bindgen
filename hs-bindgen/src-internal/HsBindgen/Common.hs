-- | Common API used by @hs-bindgen@ the library, and Template Haskell mode.
--
-- This module is intended to be distributed as "other-module" with the public
-- API for @hs-bindgen@ when used as a library.
module HsBindgen.Common (
    -- * Configuration

    -- ** Boot
    Config.BootConfig(..)
    -- *** Clang arguments
  , ClangArgs.ClangArgsConfig(..)
  , ClangArgs.CStandard(..)
  , ClangArgs.Gnu(..)
  , ClangArgs.getStdClangArg
  , ClangArgs.Target(..)
  , ClangArgs.TargetEnv(..)
  , ClangArgs.targetTriple
  , ClangArgs.BuiltinIncDirConfig(..)
    -- *** Binding specifications
  , BindingSpec.BindingSpecConfig(..)
  , BindingSpec.EnableStdlibBindingSpec(..)

    -- ** Frontend
  , Config.FrontendConfig(..)
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
  , TraceMsg.MacroParseError(..)
  , TraceMsg.MacroTcError(..)
  , TraceMsg.MangleNamesMsg(..)
  , TraceMsg.NameAnonMsg(..)
  , TraceMsg.ParseMsg(..)
  , TraceMsg.ParseTypeException(..)
  , TraceMsg.ResolveBindingSpecMsg(..)
  , TraceMsg.ResolveHeaderMsg(..)
  , TraceMsg.SelectMsg(..)
  , TraceMsg.SortMsg(..)
    -- ** Tracer definition and main API
  , Tracer.Tracer -- opaque
  , Tracer.traceWith
  , Tracer.simpleTracer
  , Tracer.natTracer
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
  , Tracer.Contravariant(..)
  ) where

import System.FilePath qualified as FilePath

import Clang.Paths qualified as Paths

import HsBindgen.Backend.Artefact.HsModule.Translation qualified as HsModule
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Translation qualified as Hs
import HsBindgen.Backend.UniqueId qualified as Hs
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config qualified as Config
import HsBindgen.Config.ClangArgs qualified as ClangArgs
import HsBindgen.Frontend.Pass.Select.IsPass qualified as Select
import HsBindgen.Frontend.Predicate qualified as Predicate
import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.TraceMsg qualified as TraceMsg
import HsBindgen.Util.Tracer qualified as Tracer

import HsBindgen.Imports (Default (..))
