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
  , Hs.HsTypeClass(..)

    -- ** Predicates
  , Predicate.Predicate(..)
  , Predicate.Regex -- opaque

    -- ** Logging
  , Trace.Trace (..)
  , module HsBindgen.Util.Tracer

    -- * Paths
  , Paths.CIncludePathDir(..)
  , (FilePath.</>)
  , FilePath.joinPath
  , THSyntax.getPackageRoot
  ) where

import Control.Tracer (Tracer, natTracer)
import Data.Set (Set)
import Language.Haskell.TH qualified as TH
import System.FilePath qualified as FilePath

import Clang.Args qualified as Args
import Clang.Paths qualified as Paths
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Clang.Args (ExtraClangArgsLog)
import HsBindgen.ExtBindings qualified as ExtBindings
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.Translation qualified as Hs
import HsBindgen.Pipeline qualified as Pipeline
import HsBindgen.Resolve qualified as Resolve
import HsBindgen.Util.Trace qualified as Trace
import HsBindgen.Util.Tracer hiding (withTracerFile, withTracerStdOut)

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
     Tracer TH.Q (TraceWithCallStack Trace.Trace)
  -> Args.ClangArgs
  -> [FilePath]
  -> TH.Q (Set Resolve.ResolveHeaderException, ExtBindings.ExtBindings)
loadExtBindings tracer args = TH.runIO . ExtBindings.loadExtBindings tracer' args
  where
    tracer' :: Tracer IO (TraceWithCallStack ExtraClangArgsLog)
    tracer' = useTrace Trace.TraceExtraClangArgs $ natTracer TH.runQ tracer
