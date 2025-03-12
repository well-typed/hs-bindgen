-- | Main entry point for using @hs-bindgen@-as-a-library
--
-- Intended for unqualified import.
--
-- NOTE: Client code should /NOT/ have to import from @hs-bindgen-libclang@;
-- that library should be considered internal to @hs-bindgen@.
module HsBindgen.Lib (
    -- * Header resolution
    Paths.CHeaderIncludePath -- opaque
  , Paths.parseCHeaderIncludePath
  , Resolve.resolveHeader

    -- * Parsing and translating
  , HsDecls -- opaque
  , translateCHeader

    -- * Preprocessor
  , preprocessPure
  , preprocessIO

    -- * Test generation
  , genTests

    -- * Options
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
  , Hs.TypeClass(..)

    -- ** Predicates
  , Predicate.Predicate(..)
  , Predicate.Regex -- opaque

    -- ** Logging
  , Tracer.Tracer
  , Tracer.nullTracer
  , Tracer.mkTracerIO
  , Tracer.mkTracerQ
  , Tracer.mkTracer
  , Tracer.contramap

    -- ** Preprocessor
  , Pipeline.PPOpts(..)
  , Pipeline.defaultPPOpts
  , Backend.PP.HsModuleOpts(..)
  , Backend.PP.HsRenderOpts(..)

    -- * Paths
  , Paths.SourcePath(..)
  , Paths.CIncludePathDir(..)
  , (FilePath.</>)
  , FilePath.joinPath
  ) where

import System.FilePath qualified as FilePath

import HsBindgen.Backend.PP.Render qualified as Backend.PP
import HsBindgen.Backend.PP.Translation qualified as Backend.PP
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Clang.Args qualified as Args
import HsBindgen.Clang.Paths qualified as Paths
import HsBindgen.ExtBindings qualified as ExtBindings
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Resolve qualified as Resolve
import HsBindgen.Hs.Translation qualified as Hs
import HsBindgen.Pipeline qualified as Pipeline
import HsBindgen.Util.Tracer qualified as Tracer

{-------------------------------------------------------------------------------
  Parsing and translating

  An opaque newtype is used because the C and Haskell ASTs are /not/ part of the
  public API.
-------------------------------------------------------------------------------}

-- | Haskell declarations, translated from a C header
newtype HsDecls = WrapHsDecls {
      unwrapHsDecls :: [Hs.Decl]
    }

translateCHeader :: Pipeline.Opts -> Paths.CHeaderIncludePath -> IO HsDecls
translateCHeader opts = fmap WrapHsDecls . Pipeline.translateCHeader opts

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
