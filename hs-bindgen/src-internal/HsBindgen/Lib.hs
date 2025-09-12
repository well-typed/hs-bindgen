-- | Main entry point for @hs-bindgen@ the library.
--
-- Intended for unqualified import.
--
-- This module is intended to be the public API for @hs-bindgen@ when used as a
-- library.

-- NOTE: Client code should /NOT/ have to import from @clang@.
module HsBindgen.Lib (
    -- * Run @hs-bindgen@
    HsBindgen.hsBindgen
  , HsBindgen.Artefact(..)
  , HsBindgen.Artefacts
  , resolveHeaders

    -- ** Predefined artefacts
  , HsBindgen.writeBindings
  , HsBindgen.writeBindingSpec
  , HsBindgen.writeTests

    -- * Configuration
  , Config.BindgenConfig(..)

    -- ** Boot
  , Common.BootConfig(..)
    -- *** Clang arguments
  , Common.ClangArgsConfig(..)
  , Common.CStandard(..)
  , Common.Gnu(..)
  , Common.getStdClangArg
  , Common.Target(..)
  , Common.TargetEnv(..)
  , Common.targetTriple
  , Common.BuiltinIncDirConfig(..)
    -- *** Binding specifications
  , Common.BindingSpecConfig(..)
  , Common.EnableStdlibBindingSpec(..)
  , BindingSpec.ExternalBindingSpec
  , BindingSpec.PrescriptiveBindingSpec
  , BindingSpec.BindingSpec -- opaque
  , BindingSpec.emptyBindingSpec
  , BindingSpec.loadExtBindingSpecs
  , BindingSpec.loadPrescriptiveBindingSpec
  , BindingSpec.loadBindingSpecs
  , BindingSpec.getStdlibBindingSpec
  , BindingSpec.encodeBindingSpecJson
  , BindingSpec.encodeBindingSpecYaml

    -- ** Frontend
  , Common.FrontendConfig(..)
    -- *** Predicates
  , Common.Predicate(..)
  , Common.HeaderPathPredicate (..)
  , Common.DeclPredicate (..)
  , Common.ParsePredicate
  , Common.SelectPredicate
  , Common.Regex -- opaque
  , Common.mergePredicates
    -- *** Program slicing
  , Common.ProgramSlicing(..)

    -- ** Backend
  , Common.BackendConfig(..)
    -- *** Translation options
  , Common.UniqueId(..)
  , Common.TranslationOpts(..)
  , Common.Strategy(..)
  , Common.HsTypeClass(..)
  , Common.HsModuleOpts(..)

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
  , Common.MacroParseError(..)
  , Common.MacroTcError(..)
  , Common.MangleNamesMsg(..)
  , Common.NameAnonMsg(..)
  , Common.ParseMsg(..)
  , Common.ParseTypeException(..)
  , Common.ResolveBindingSpecMsg(..)
  , Common.ResolveHeaderMsg(..)
  , Common.SelectMsg(..)
  , Common.SortMsg(..)
    -- ** Tracer definition and main API
  , Common.Tracer -- opaque
  , Common.traceWith
  , Common.simpleTracer
    -- ** Data types and typeclasses useful for tracing
  -- , Common.PrettyForTrace(..)
  , Common.Level(..)
  , Common.SafeLevel(..)
  , Common.Source(..)
  , Common.TraceId(..)
  , Common.IsTrace(..)
  , Common.Verbosity(..)
    -- ** Tracer configuration
  , Common.ShowTimeStamp(..)
  , Common.ShowCallStack(..)
  , Common.TracerConfig(..)
    -- *** Custom output
  , Common.AnsiColor(..)
  , Common.Report
  , Common.OutputConfig(..)
    -- *** Custom log levels
  , Common.CustomLogLevel(..)
  , Common.CustomLogLevelSetting(..)
  , Common.getCustomLogLevel
    -- ** Tracers
  , Common.withTracer

    -- * Re-exports
  , Common.Contravariant(..)
  , Common.Default (..)
  , HsBindgen.I (..)
  , HsBindgen.NP (..)
  ) where

import Data.Map.Strict (Map)
import Data.Set (Set)

import HsBindgen.Common qualified as Common

import Clang.Args qualified as Args
import Clang.Paths qualified as Paths
import HsBindgen qualified
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config qualified as Config
import HsBindgen.Resolve qualified as Resolve
import HsBindgen.Util.Tracer qualified as Tracer

-- | Resolve headers, used for debugging
resolveHeaders ::
     Tracer.Tracer IO Common.ResolveHeaderMsg
  -> Args.ClangArgs
  -> Set Common.HashIncludeArg
  -> IO (Map Common.HashIncludeArg FilePath)
resolveHeaders tracer args headers =
    fmap Paths.getSourcePath <$>
      Resolve.resolveHeaders tracer args headers
