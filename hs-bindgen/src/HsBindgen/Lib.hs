-- | Main entry point for using @hs-bindgen@ as a library.
--
-- Intended for unqualified import.

-- NOTE: Client code should /NOT/ have to import from @clang@.
module HsBindgen.Lib (
    -- * Run @hs-bindgen@
    HsBindgen.hsBindgen
  , resolveHeaders
  , HsBindgen.Artefact(..)
  , HsBindgen.Artefacts

    -- ** Predefined artefacts
  , HsBindgen.writeBindings
  , HsBindgen.writeBindingSpec
  , HsBindgen.writeTests

    -- * Configuration
  , Config.BindgenConfig(..)

    -- ** Boot
  , Common.BootConfig(..)
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
    -- *** Clang arguments
  , Common.ClangArgs(..)
  , Common.Target(..)
  , Common.TargetEnv(..)
  , Common.targetTriple
  , Common.CStandard(..)
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
  , Common.PrettyForTrace(..)
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
  , Common.Default (..)
  , HsBindgen.I (..)
  , HsBindgen.NP (..)
  ) where

import Data.Map.Strict (Map)
import Data.Set (Set)

import HsBindgen.Common qualified as Common

import HsBindgen qualified
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config qualified as Config
import HsBindgen.Util.Tracer qualified as Tracer

import Clang.Paths qualified as Paths
import HsBindgen.Resolve qualified as Resolve

-- | Resolve headers, used for debugging
resolveHeaders ::
     Tracer.Tracer IO Common.ResolveHeaderMsg
  -> Common.ClangArgs
  -> Set Common.HashIncludeArg
  -> IO (Map Common.HashIncludeArg FilePath)
resolveHeaders tracer args headers =
    fmap Paths.getSourcePath <$>
      Resolve.resolveHeaders tracer args headers
