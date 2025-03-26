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

    -- ** External bindings
  , ExtBindings.ExtBindings -- opaque
  , ExtBindings.emptyExtBindings
  , loadExtBindings
  , Resolve.ResolveHeaderException(..)

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

    -- * Paths
  , Paths.CIncludePathDir(..)
  , (FilePath.</>)
  , FilePath.joinPath
  , THSyntax.getPackageRoot
  ) where

import Language.Haskell.TH qualified as TH
import System.FilePath qualified as FilePath

import Clang.Args qualified as Args
import Clang.Paths qualified as Paths
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.ExtBindings qualified as ExtBindings
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
  External bindings
-------------------------------------------------------------------------------}

-- | Load external bindings from configuration files
--
-- The format is determined by filename extension.  The following formats are
-- supported:
--
-- * YAML (@.yaml@ extension)
-- * JSON (@.json@ extension)
loadExtBindings ::
     Args.ClangArgs
  -> [FilePath]
  -> TH.Q ([Resolve.ResolveHeaderException], ExtBindings.ExtBindings)
loadExtBindings args = TH.runIO . ExtBindings.loadExtBindings args
