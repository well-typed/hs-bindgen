-- | Main entry point for using @hs-bindgen@-as-a-library
--
-- Intended for unqualified import.
--
-- NOTE: Client code should /NOT/ have to import from @clang@.
module HsBindgen.Lib (
    -- * Parsing and translating
    HsDecls -- opaque
  , translateCHeader

    -- * Preprocessor
  , preprocessPure
  , preprocessIO

    -- * External bindings generation
  , genExtBindings

    -- * Test generation
  , genTests

    -- * Options
  , ModuleUnique(..)
  , Pipeline.Opts(..)
  , Pipeline.defaultOpts

    -- ** Clang arguments
  , Args.ClangArgs(..)
  , Args.defaultClangArgs
  , Args.Target(..)
  , Args.TargetEnv(..)
  , Args.targetTriple
  , Args.CStandard(..)

    -- ** External bindings
  , ExtBindings.ExtBindings
  , ExtBindings.emptyExtBindings
  , ExtBindings.loadExtBindings

    -- ** Translation options
  , Hs.TranslationOpts(..)
  , Hs.defaultTranslationOpts
  , Hs.Strategy(..)
  , Hs.HsTypeClass(..)

    -- ** Predicates
  , Predicate.Predicate(..)
  , Predicate.Regex -- opaque

    -- ** Logging
  , Tracer.Tracer
  , Tracer.Level(..)
  , Tracer.nullTracer
  , Tracer.mkTracerIO
  , Tracer.mkTracerQ
  , Tracer.mkTracer
  , Tracer.contramap
  , Tracer.traceWith

    -- ** Preprocessor
  , Pipeline.PPOpts(..)
  , Pipeline.defaultPPOpts
  , Backend.PP.HsModuleOpts(..)
  , Backend.PP.HsRenderOpts(..)

    -- * Paths
  , Paths.CHeaderIncludePath -- opaque
  , Paths.parseCHeaderIncludePath
  , Paths.CIncludePathDir(..)
  , (FilePath.</>)
  , FilePath.joinPath
  ) where

import System.FilePath qualified as FilePath

import Clang.Args qualified as Args
import Clang.Paths qualified as Paths
import HsBindgen.Backend.PP.Render qualified as Backend.PP
import HsBindgen.Backend.PP.Translation qualified as Backend.PP
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.ExtBindings qualified as ExtBindings
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.Translation qualified as Hs
import HsBindgen.Pipeline qualified as Pipeline
import HsBindgen.Util.Tracer qualified as Tracer
import HsBindgen.ModuleUnique

{-------------------------------------------------------------------------------
  Parsing and translating

  An opaque newtype is used because the C and Haskell ASTs are /not/ part of the
  public API.
-------------------------------------------------------------------------------}

-- | Haskell declarations, translated from a C header
newtype HsDecls = WrapHsDecls {
      unwrapHsDecls :: [Hs.Decl]
    }

translateCHeader :: ModuleUnique -> Pipeline.Opts -> Paths.CHeaderIncludePath -> IO HsDecls
translateCHeader mu opts = fmap WrapHsDecls . Pipeline.translateCHeader mu opts

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
  External bindings generation
-------------------------------------------------------------------------------}

genExtBindings ::
     Pipeline.PPOpts
  -> Paths.CHeaderIncludePath
  -> FilePath
  -> HsDecls
  -> IO ()
genExtBindings ppOpts headerIncludePath fp =
    Pipeline.genExtBindings ppOpts headerIncludePath fp . unwrapHsDecls

{-------------------------------------------------------------------------------
  Test generation
-------------------------------------------------------------------------------}

genTests ::
     Pipeline.PPOpts
  -> Paths.CHeaderIncludePath
  -> FilePath -- ^ Test suite directory path
  -> HsDecls
  -> IO ()
genTests ppOpts headerIncludePath testDir =
    Pipeline.genTests ppOpts headerIncludePath testDir . unwrapHsDecls
