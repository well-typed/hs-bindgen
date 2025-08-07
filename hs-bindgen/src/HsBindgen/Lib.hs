-- | Main entry point for using @hs-bindgen@ as a library.
--
-- Intended for unqualified import.

-- NOTE: Client code should /NOT/ have to import from @clang@.
module HsBindgen.Lib (
    -- * Frontend
    HsDecls -- opaque
  , translateCHeaders

    -- * Backend
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
  , Common.hashIncludeArgWithTrace
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
    -- *** Custom log levels
  , Common.CustomLogLevel(..)
  , Common.customLogLevelFrom
  , Common.CustomLogLevelSetting(..)
    -- ** Tracers
  , Tracer.withTracerStdOut
  , Common.withTracerCustom
  , Tracer.fatalError

    -- * Re-exports
  , Common.Default (..)
  ) where

import HsBindgen.Common qualified as Common

import Clang.Args qualified as Args
import Clang.Paths qualified as Paths
import HsBindgen.Backend.Artefact.PP.Render qualified as Backend.PP
import HsBindgen.Backend.Artefact.PP.Translation qualified as Backend.PP
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.BindingSpec.Gen qualified as BindingSpec
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Resolve qualified as Resolve
import HsBindgen.Util.Tracer qualified as Tracer

import HsBindgen.Pipeline.Lib qualified as Pipeline

import HsBindgen.BindingSpec
import HsBindgen.Frontend.RootHeader
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
      ModuleUnique
   -> Tracer IO Common.FrontendMsg
   -> Common.Config
   -> ExternalBindingSpec
   -> PrescriptiveBindingSpec
   -> [HashIncludeArg]
   -> IO HsDecls
translateCHeaders mu tracer config extSpec pSpec headers =
  WrapHsDecls <$> Pipeline.translateCHeaders mu tracer config extSpec pSpec headers

{-------------------------------------------------------------------------------
  Preprocessor
-------------------------------------------------------------------------------}

-- | Generate bindings for the given C header
preprocessPure :: Common.Config -> HsDecls -> String
preprocessPure config = Pipeline.preprocessPure config . unwrapHsDecls

-- | Generate bindings for the given C header
preprocessIO ::
     Common.Config
  -> Maybe FilePath -- ^ Output file or 'Nothing' for @STDOUT@
  -> HsDecls
  -> IO ()
preprocessIO config fp = Pipeline.preprocessIO config fp . unwrapHsDecls

{-------------------------------------------------------------------------------
  Binding specification generation
-------------------------------------------------------------------------------}

genBindingSpec ::
     Common.Config
  -> [HashIncludeArg]
  -> FilePath
  -> HsDecls
  -> IO ()
genBindingSpec config hashIncludeArgs path =
      BindingSpec.genBindingSpec config hashIncludeArgs path
    . unwrapHsDecls

{-------------------------------------------------------------------------------
  Test generation
-------------------------------------------------------------------------------}

genTests ::
     Common.Config
  -> [HashIncludeArg]
  -> FilePath -- ^ Test suite directory path
  -> HsDecls
  -> IO ()
genTests config hashIncludeArgs testDir =
    Pipeline.genTests config hashIncludeArgs testDir . unwrapHsDecls

{-------------------------------------------------------------------------------
  Paths
-------------------------------------------------------------------------------}

-- | Resolve a header, used for debugging
resolveHeader ::
     Tracer IO Resolve.ResolveHeaderMsg
  -> Args.ClangArgs
  -> HashIncludeArg -- ^ The header we want to resolve
  -> IO (Maybe FilePath)
resolveHeader tracer args path =
    fmap Paths.getSourcePath <$>
      Resolve.resolveHeader tracer args path
