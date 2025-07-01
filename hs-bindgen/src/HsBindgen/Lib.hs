-- | Main entry point for using @hs-bindgen@ as a library.
--
-- Intended for unqualified import.

-- NOTE: Client code should /NOT/ have to import from @clang@.
module HsBindgen.Lib (
    -- * Parsing and translating
    HsDecls -- opaque
  , translateCHeaders

    -- * Preprocessor
  , preprocessPure
  , preprocessIO

    -- * Binding specification generation
  , genBindingSpec

    -- * Test generation
  , genTests

    -- * Debugging
    -- ** Header resolution
  , resolveHeader

    -- * Options
  , ModuleUnique(..)
  , Common.Opts(..)

    -- ** Clang arguments
  , Common.ClangArgs(..)
  , Common.Target(..)
  , Common.TargetEnv(..)
  , Common.targetTriple
  , Common.CStandard(..)

    -- ** Binding specifications
  , Common.BindingSpec -- opaque
  , Common.emptyBindingSpec
  , Common.StdlibBindingSpecConf(..)
  , Pipeline.loadExtBindingSpecs
  , Pipeline.loadPrescriptiveBindingSpec
  , Pipeline.getStdlibBindingSpec
  , Pipeline.encodeBindingSpecJson
  , Pipeline.encodeBindingSpecYaml

    -- ** Translation options
  , Common.TranslationOpts(..)
  , Common.Strategy(..)
  , Common.HsTypeClass(..)

    -- ** Selection predicates
  , Common.Predicate(..)
  , Common.Regex -- opaque

    -- ** Program slicing
  , Common.ProgramSlicing(..)

    -- ** Preprocessor
  , Pipeline.PPOpts(..)
  , Backend.PP.HsModuleOpts(..)
  , Backend.PP.HsRenderOpts(..)

    -- * Paths
  , Paths.CHeaderIncludePath -- opaque
  , Paths.parseCHeaderIncludePath
  , Common.CIncludePathDir(..)
  , (Common.</>)
  , Common.joinPath

    -- * Logging
  , Common.TraceMsg(..)
  , Common.ResolveHeaderMsg(..)
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
  , Common.ErrorTraceException(..)
    -- ** Tracer configuration
  , Common.AnsiColor(..)
  , Common.ShowTimeStamp(..)
  , Common.ShowCallStack(..)
  , Common.TracerConf(..)
  , Common.CustomLogLevel(..)
    -- ** Tracers
  , Common.withTracerStdOut
  , Common.withTracerCustom
  , Common.withTracerCustom'

    -- * Re-exports
  , Common.Default (..)
  ) where

import HsBindgen.Common qualified as Common

import Clang.Args qualified as Args
import Clang.Paths qualified as Paths
import HsBindgen.Backend.PP.Render qualified as Backend.PP
import HsBindgen.Backend.PP.Translation qualified as Backend.PP
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Pipeline qualified as Pipeline
import HsBindgen.Resolve qualified as Resolve

import HsBindgen.ModuleUnique
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Parsing and translating

  An opaque newtype is used because the C and Haskell ASTs are /not/ part of the
  public API.
-------------------------------------------------------------------------------}

-- | Haskell declarations, translated from a C header
newtype HsDecls = WrapHsDecls {
      unwrapHsDecls :: [Hs.Decl]
    }

-- | Translate C headers to Haskell declarations
translateCHeaders ::
     ModuleUnique -> Pipeline.Opts -> [Paths.CHeaderIncludePath] -> IO HsDecls
translateCHeaders mu opts =
    fmap WrapHsDecls . Pipeline.translateCHeaders mu opts

{-------------------------------------------------------------------------------
  Preprocessor
-------------------------------------------------------------------------------}

-- | Generate bindings for the given C header
preprocessPure :: Pipeline.PPOpts -> HsDecls -> String
preprocessPure ppOpts = Pipeline.preprocessPure ppOpts . unwrapHsDecls

-- | Generate bindings for the given C header
preprocessIO ::
     Pipeline.PPOpts
  -> Maybe FilePath -- ^ Output file or 'Nothing' for @STDOUT@
  -> HsDecls
  -> IO ()
preprocessIO ppOpts fp = Pipeline.preprocessIO ppOpts fp . unwrapHsDecls

{-------------------------------------------------------------------------------
  Binding specification generation
-------------------------------------------------------------------------------}

genBindingSpec ::
     Tracer IO Common.TraceMsg
  -> Pipeline.PPOpts
  -> [Paths.CHeaderIncludePath]
  -> FilePath
  -> HsDecls
  -> IO ()
genBindingSpec tracer ppOpts headerIncludePaths fp =
    Pipeline.genBindingSpec tracer ppOpts headerIncludePaths fp . unwrapHsDecls

{-------------------------------------------------------------------------------
  Test generation
-------------------------------------------------------------------------------}

genTests ::
     Pipeline.PPOpts
  -> [Paths.CHeaderIncludePath]
  -> FilePath -- ^ Test suite directory path
  -> HsDecls
  -> IO ()
genTests ppOpts headerIncludePaths testDir =
    Pipeline.genTests ppOpts headerIncludePaths testDir . unwrapHsDecls

{-------------------------------------------------------------------------------
  Paths
-------------------------------------------------------------------------------}

-- | Resolve a header, used for debugging
resolveHeader ::
     Tracer IO Resolve.ResolveHeaderMsg
  -> Args.ClangArgs
  -> Paths.CHeaderIncludePath -- ^ The header we want to resolve
  -> IO (Maybe FilePath)
resolveHeader tracer args path =
    fmap Paths.getSourcePath <$>
      Resolve.resolveHeader tracer args path
