-- | Main entry point for using @hs-bindgen@-as-a-library
--
-- Intended for unqualified import.
--
-- NOTE: Client code should /NOT/ have to import from @clang@.
module HsBindgen.Lib (
    -- * Parsing and translating
    HsDecls -- opaque
  , translateCHeaders

    -- * Preprocessor
  , preprocessPure
  , preprocessIO

    -- * Binding specification generation
  , genExtBindings

    -- * Test generation
  , genTests

    -- * Options
  , module Default
  , ModuleUnique(..)
  , Pipeline.Opts(..)

    -- ** Clang arguments
  , Args.ClangArgs(..)
  , Args.Target(..)
  , Args.TargetEnv(..)
  , Args.targetTriple
  , Args.CStandard(..)

    -- ** External bindings
  , ResolvedBindingSpec
  , Pipeline.loadExtBindings
  , Resolve.ResolveHeaderException(..)
  , emptyExtBindings
  , stdlibExtBindingsYaml

    -- ** Translation options
  , Hs.TranslationOpts(..)
  , Hs.Strategy(..)
  , Hs.HsTypeClass(..)

    -- ** Predicates
  , Predicate.Predicate(..)
  , Predicate.Regex -- opaque

    -- ** Logging
  , Trace.Trace (..)
  , module HsBindgen.Util.Tracer

    -- ** Preprocessor
  , Pipeline.PPOpts(..)
  , Backend.PP.HsModuleOpts(..)
  , Backend.PP.HsRenderOpts(..)

    -- * Paths
  , Paths.CHeaderIncludePath -- opaque
  , Paths.parseCHeaderIncludePath
  , Paths.CIncludePathDir(..)
  , (FilePath.</>)
  , FilePath.joinPath
  ) where

import Data.ByteString (ByteString)
import System.FilePath qualified as FilePath

import Clang.Args qualified as Args
import Clang.Paths qualified as Paths
import HsBindgen.Backend.PP.Render qualified as Backend.PP
import HsBindgen.Backend.PP.Translation qualified as Backend.PP
import HsBindgen.BindingSpec (ResolvedBindingSpec)
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.BindingSpec.Stdlib qualified as Stdlib
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.Translation qualified as Hs
import HsBindgen.Imports
import HsBindgen.Imports as Default (Default (..))
import HsBindgen.ModuleUnique
import HsBindgen.Pipeline qualified as Pipeline
import HsBindgen.Resolve qualified as Resolve
import HsBindgen.Util.Trace qualified as Trace
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
translateCHeaders :: HasCallStack
  => ModuleUnique -> Pipeline.Opts -> [Paths.CHeaderIncludePath] -> IO HsDecls
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

genExtBindings ::
     Pipeline.PPOpts
  -> [Paths.CHeaderIncludePath]
  -> FilePath
  -> HsDecls
  -> IO ()
genExtBindings ppOpts headerIncludePaths fp =
    Pipeline.genExtBindings ppOpts headerIncludePaths fp . unwrapHsDecls

{-------------------------------------------------------------------------------
  External bindings
-------------------------------------------------------------------------------}

emptyExtBindings :: ResolvedBindingSpec
emptyExtBindings = BindingSpec.empty

stdlibExtBindingsYaml :: ByteString
stdlibExtBindingsYaml = BindingSpec.encodeYaml Stdlib.bindings

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
