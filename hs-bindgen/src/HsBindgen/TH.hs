{-# LANGUAGE CPP #-}

-- | Main entry point for using @hs-bindgen@ in TH mode
module HsBindgen.TH (
    -- * Template Haskell API
    Pipeline.genBindings
  , Pipeline.genBindings'

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

    -- ** Binding specifications
  , BindingSpecs.BindingSpecs -- opaque
  , BindingSpecs.emptyBindingSpecs
  , loadBindingSpecs
  , Resolve.ResolveHeaderException(..)

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
  , Tracer.nullTracer
  , Tracer.mkTracerIO
  , Tracer.mkTracerQ
  , Tracer.mkTracer
  , Tracer.contramap

    -- * Paths
  , Paths.CIncludePathDir(..)
  , (FilePath.</>)
  , FilePath.joinPath
  , THSyntax.getPackageRoot
  ) where

import Data.Set (Set)
import Language.Haskell.TH qualified as TH
import System.FilePath qualified as FilePath

import Clang.Args qualified as Args
import Clang.Paths qualified as Paths
import HsBindgen.BindingSpecs qualified as BindingSpecs
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.Translation qualified as Hs
import HsBindgen.Pipeline qualified as Pipeline
import HsBindgen.Resolve qualified as Resolve
import HsBindgen.Util.Tracer qualified as Tracer

#ifdef MIN_VERSION_th_compat
import Language.Haskell.TH.Syntax.Compat qualified as THSyntax
#else
import Language.Haskell.TH.Syntax qualified as THSyntax
#endif

{-------------------------------------------------------------------------------
  Binding specifications
-------------------------------------------------------------------------------}

-- | Load binding specifications
--
-- The format is determined by filename extension.  The following formats are
-- supported:
--
-- * YAML (@.yaml@ extension)
-- * JSON (@.json@ extension)
loadBindingSpecs ::
     Args.ClangArgs
  -> [FilePath]
  -> TH.Q (Set Resolve.ResolveHeaderException, BindingSpecs.BindingSpecs)
loadBindingSpecs args = TH.runIO . BindingSpecs.loadBindingSpecs args
