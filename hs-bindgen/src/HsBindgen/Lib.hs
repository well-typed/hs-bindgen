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

    -- * Options
  , ModuleUnique.ModuleUnique(..)
  , Common.Config(..)

    -- ** Clang arguments
  , Common.ClangArgs(..)
  , Common.Target(..)
  , Common.TargetEnv(..)
  , Common.targetTriple
  , Common.CStandard(..)

    -- ** Binding specifications
  , Common.BindingSpec -- opaque
  , Common.emptyBindingSpec
  , BindingSpec.ExternalBindingSpec
  , BindingSpec.PrescriptiveBindingSpec
  , Common.EnableStdlibBindingSpec(..)
  , Common.BindingSpecConfig(..)
  , BindingSpec.loadExtBindingSpecs
  , BindingSpec.loadPrescriptiveBindingSpec
  , BindingSpec.loadBindingSpecs
  , BindingSpec.getStdlibBindingSpec
  , BindingSpec.encodeBindingSpecJson
  , BindingSpec.encodeBindingSpecYaml

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

    -- ** Preprocessor
  , Backend.PP.HsModuleOpts(..)
  , Backend.PP.HsRenderOpts(..)

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
    -- *** Custom log levels
  , Common.CustomLogLevel(..)
  , Common.customLogLevelFrom
  , Common.CustomLogLevelSetting(..)
    -- ** Tracers
  , Common.withTracer
  , Tracer.fatalError

    -- * Re-exports
  , Common.Default (..)
  , HsBindgen.I (..)
  , HsBindgen.NP (..)
  ) where

import Data.Map.Strict (Map)
import Data.Set (Set)

import HsBindgen.Common qualified as Common

import HsBindgen qualified
import HsBindgen.Backend.Artefact.PP.Render qualified as Backend.PP
import HsBindgen.Backend.Artefact.PP.Translation qualified as Backend.PP
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.ModuleUnique qualified as ModuleUnique
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
