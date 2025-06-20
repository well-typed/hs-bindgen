{-# LANGUAGE CPP #-}

-- | Main entry point for using @hs-bindgen@ in TH mode
module HsBindgen.TH (
    -- * Template Haskell API
    Pipeline.hashInclude'
  , Pipeline.hashInclude
  , Pipeline.hashIncludeWith

    -- * Options
  , module Default
  , Pipeline.Opts(..)
  , Pipeline.QuoteIncludeDir(..)
  , Pipeline.HashIncludeOpts(..)

    -- ** Clang arguments
  , Args.ClangArgs(..)
  , Args.Target(..)
  , Args.TargetEnv(..)
  , Args.targetTriple
  , Args.CStandard(..)

    -- ** External bindings
  , ResolvedBindingSpec -- opaque
  , loadExtBindings
  , emptyExtBindings
  , Resolve.ResolveHeaderException(..)

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
import HsBindgen.BindingSpec (ResolvedBindingSpec)
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Clang.Args (ExtraClangArgsLog)
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.Translation qualified as Hs
import HsBindgen.Imports as Default (Default (..))
import HsBindgen.Pipeline qualified as Pipeline
import HsBindgen.Resolve qualified as Resolve
import HsBindgen.Util.Trace qualified as Trace
import HsBindgen.Util.Tracer hiding (withTracerFile)

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
  -> Bool -- ^ Automatically include @stdlib@?
  -> [FilePath]
  -> TH.Q (Set Resolve.ResolveHeaderException, ResolvedBindingSpec)
loadExtBindings tracer args isAutoStdlib =
    TH.runIO . Pipeline.loadExtBindings tracer' args isAutoStdlib
  where
    tracer' :: Tracer IO (TraceWithCallStack ExtraClangArgsLog)
    tracer' = useTrace Trace.TraceExtraClangArgs $ natTracer TH.runQ tracer

emptyExtBindings :: ResolvedBindingSpec
emptyExtBindings = BindingSpec.empty
