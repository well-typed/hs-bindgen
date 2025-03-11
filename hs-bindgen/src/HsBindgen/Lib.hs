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

    -- * Parsing
  , CHeader -- opaque
  , parseCHeader

    -- * Preprocessor
  , preprocessPure
  , preprocessIO

    -- * Test generation
  , genTests

    -- * Development/debugging
  , dumpCHeader

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
import Text.Show.Pretty qualified as Pretty

import HsBindgen.Backend.PP.Render qualified as Backend.PP
import HsBindgen.Backend.PP.Translation qualified as Backend.PP
import HsBindgen.C.AST qualified as C
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Clang.Args qualified as Args
import HsBindgen.Clang.Paths qualified as Paths
import HsBindgen.ExtBindings qualified as ExtBindings
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Imports (Generic)
import HsBindgen.Resolve qualified as Resolve
import HsBindgen.Hs.Translation qualified as Hs
import HsBindgen.Pipeline qualified as Pipeline
import HsBindgen.Util.Tracer qualified as Tracer

{-------------------------------------------------------------------------------
  Parsing

  An opaque newtype is used because the C and Haskell ASTs are /not/ part of the
  public API.
-------------------------------------------------------------------------------}

newtype CHeader = WrapCHeader {
      unwrapCHeader :: C.Header
    }
  deriving Generic

-- | Parse a C header
parseCHeader :: Pipeline.Opts -> Paths.CHeaderIncludePath -> IO CHeader
parseCHeader opts = fmap (WrapCHeader . snd) . Pipeline.parseCHeader opts

{-------------------------------------------------------------------------------
  Preprocessor
-------------------------------------------------------------------------------}

-- | Generate bindings for the given C header
preprocessPure ::
     Pipeline.Opts
  -> Pipeline.PPOpts
  -> Paths.CHeaderIncludePath
  -> CHeader
  -> String
preprocessPure opts ppOpts headerIncludePath =
    Pipeline.preprocessPure opts ppOpts headerIncludePath . unwrapCHeader

-- | Generate bindings for the given C header
preprocessIO ::
     Pipeline.Opts
  -> Pipeline.PPOpts
  -> Paths.CHeaderIncludePath
  -> Maybe FilePath -- ^ Output file or 'Nothing' for @STDOUT@
  -> CHeader
  -> IO ()
preprocessIO opts ppOpts headerIncludePath fp =
    Pipeline.preprocessIO opts ppOpts headerIncludePath fp . unwrapCHeader

{-------------------------------------------------------------------------------
  Test generation
-------------------------------------------------------------------------------}

genTests ::
     Pipeline.PPOpts
  -> Paths.CHeaderIncludePath
  -> FilePath -- ^ Test suite directory path
  -> CHeader
  -> IO ()
genTests ppOpts headerIncludePath testDir =
    Pipeline.genTests ppOpts headerIncludePath testDir . unwrapCHeader

{-------------------------------------------------------------------------------
  Development/debugging
-------------------------------------------------------------------------------}

dumpCHeader :: CHeader -> IO ()
dumpCHeader = Pretty.dumpIO . unwrapCHeader
