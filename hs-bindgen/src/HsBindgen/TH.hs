{-# LANGUAGE CPP #-}

-- | Main entry point for using @hs-bindgen@ in TH mode
module HsBindgen.TH (
    -- * Template Haskell API
    Pipeline.hashInclude'
  , Pipeline.hashInclude
  , Pipeline.hashIncludeWith

    -- * Debugging
    -- ** Header resolution
  , Resolve.ResolveHeaderMsg(..)

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

    -- ** Binding specifications
  , BindingSpec -- opaque
  , loadExtBindingSpecs
  , emptyBindingSpec
  , StdlibBindingSpecConf(..)

    -- ** Translation options
  , Hs.TranslationOpts(..)
  , Hs.Strategy(..)
  , Hs.HsTypeClass(..)

    -- ** Selection predicates
  , Predicate.Predicate(..)
  , Predicate.Regex -- opaque

    -- ** Program slicing
  , Slice.ProgramSlicing(..)

    -- ** Logging
  , TraceMsg(..)
  , module HsBindgen.Util.Tracer

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
import HsBindgen.BindingSpec (ResolvedBindingSpec)
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Frontend.Pass.Slice qualified as Slice
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.Translation qualified as Hs
import HsBindgen.Imports as Default (Default (..))
import HsBindgen.Pipeline (StdlibBindingSpecConf (..))
import HsBindgen.Pipeline qualified as Pipeline
import HsBindgen.Resolve qualified as Resolve
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer hiding (withTracerFile)

#ifdef MIN_VERSION_th_compat
import Language.Haskell.TH.Syntax.Compat qualified as THSyntax
#else
import Language.Haskell.TH.Syntax qualified as THSyntax
#endif

{-------------------------------------------------------------------------------
  Binding specifications
-------------------------------------------------------------------------------}

-- TODO use opaque wrapper
type BindingSpec = ResolvedBindingSpec

-- | Load external binding specifications
--
-- The format is determined by filename extension.  The following formats are
-- supported:
--
-- * YAML (@.yaml@ extension)
-- * JSON (@.json@ extension)
loadExtBindingSpecs ::
     Tracer TH.Q TraceMsg
  -> Args.ClangArgs
  -> StdlibBindingSpecConf
  -> [FilePath]
  -> TH.Q BindingSpec
loadExtBindingSpecs tracer args stdlibConf =
    TH.runIO . Pipeline.loadExtBindingSpecs tracer' args stdlibConf
  where
    tracer' :: Tracer IO TraceMsg
    tracer' = natTracer TH.runQ tracer

emptyBindingSpec :: BindingSpec
emptyBindingSpec = BindingSpec.empty
